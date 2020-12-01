;;; nih.el --- There are knights who say it          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  João Távora

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(require 'websocket)
(require 'jsonrpc)
(require 'cl-lib)


;;;; Basic connection management
(defclass nih--connection (jsonrpc-connection)
  ((repl
    :accessor nih--repl
    :documentation "REPL buffer")
   (target-info
    :reader nih--target-info :initarg :target-info
    :documentation
    "Full target info.  A JSON object returned by /json/list.
May be augmented with nih-specific fields.")
   (socket
    :reader nih--socket
    :initarg :socket
    :documentation "Open WEBSOCKET object"))
  :documentation "Represents a NIH connection.  `jsonrpc-name' is NOT unique.")

(defun nih--target-url (conn)
  (plist-get (nih--target-info conn) :webSocketDebuggerUrl))

(defun nih--pid (conn) (when-let (p (nih--proc conn)) (process-id p)))
(defun nih--command (conn) (when-let (p (nih--proc conn)) (process-command p)))
(defun nih--proc (conn) (plist-get (nih--target-info conn) :nih--proc))
(defun nih--host (conn) (plist-get (nih--target-info conn) :nih--host))
(defun nih--port (conn) (plist-get (nih--target-info conn) :nih--port))

(cl-defmethod jsonrpc-connection-send ((conn nih--connection)
                                       &rest args
                                       &key
                                       _id
                                       method
                                       _params
                                       _result
                                       _error
                                       _partial)
  "Send MESSAGE, a JSON object, to CONNECTION."
  ;; next form clearly something to put in an :around method
  (when method
    (plist-put args :method
               (cond ((keywordp method) (substring (symbol-name method) 1))
                     ((and method (symbolp method)) (symbol-name method)))))
  (let* ((message `(;; CDP isn't technically JSONRPC, so don't send
                    ;; the `:jsonrpc' "2.0" version identifier which
                    ;; trips up node's server, for example.
                    ,@args))
         (json (jsonrpc--json-encode message)))
    (with-slots (socket) conn
      (websocket-send-text socket json))
    ;; also something for a generic :AFTER
    (jsonrpc--log-event conn message 'client)))

(cl-defmethod jsonrpc-running-p ((conn nih--connection))
  (with-slots (socket) conn (websocket-openp socket)))

(cl-defmethod jsonrpc-shutdown ((conn nih--connection))
  (with-slots (socket) conn (websocket-close socket)))

(defvar nih-path nil
  "Directory containing the NIH package.")

(setq nih-path
      (if load-file-name
          (file-name-directory load-file-name)
        (nih--error "fatal: impossible to determine nih-path")))

(defvar nih--connections nil
  "List of live `nih--connection' objects.")

(defvar nih--default-connection nil
  "Currently active NIH connection, returned by `nih-current-connection'.
In buffers dedicated to a specific connection this holds that
connection.  In other buffers this holds the default connection,
which can be cycled with `nih-cycle-connections'.")

(defvar nih--dispatching-connection nil)

(put 'nih--default-connection 'permanent-local t)

(defun nih--current-connection ()
  "Returns currently active NIH connection."
  (or nih--dispatching-connection
      nih--default-connection
      (setq nih--default-connection
            (car nih--connections))))

(defun nih-cycle-connections (interactive)
  "Cycle NIH connection."
  (interactive (list t))
  (setq nih--default-connection
        (or (cdr (member nih--default-connection nih--connections))
            (car nih--connections)))
  (when interactive
    (nih--message "Active connection is now %s" nih--default-connection)))

(defun nih--message (format &rest args)
  "Message out with FORMAT with ARGS."
  (message "[nih] %s" (apply #'format format args)))

(defun nih--error (format &rest args)
  "Error out with FORMAT with ARGS."
  (error "[nih] %s" (apply #'format format args)))

(defconst nih--{} (make-hash-table) "The empty JSON object.")

(defvar nih-connected-hook 'nih-repl-new
  "Hook of functions run when NIH connects.
Each function is called with a single
`nih--connection' object.")

(defvar nih-preserve-buffers nil
  "If nil, kill all buffers when a connection is removed.")

(cl-defmacro nih--when-live-buffer (buf &rest body)
  "Check BUF live, then do BODY in it." (declare (indent 1) (debug t))
  (let ((b (cl-gensym)))
    `(let ((,b ,buf)) (if (buffer-live-p ,b) (with-current-buffer ,b ,@body)))))

(defun nih--on-websocket-close (ws)
  "Teardown NIH WS."
  (let ((conn (cl-find ws
                       nih--connections
                       :key #'nih--socket))
        proc)
    (setq nih--connections
          (delete conn nih--connections))
    (unless nih-preserve-buffers
      (kill-buffer (jsonrpc-events-buffer conn))
      (nih--when-live-buffer (nih--repl conn)
        (nih--repl-teardown "websocket close")))
    (if (eq conn nih--default-connection)
        (set-default 'nih--default-connection (car nih--connections)))
    ;; Now maybe delete the originating inferior process
    (unless (or (not (setq proc (nih--proc conn)))
                (not (process-live-p proc))
                (cl-find proc nih--connections :key #'nih--proc)
                (not (y-or-n-p (format "[nih] Also kill process %s?" proc))))
      (delete-process proc))))

(defun nih--on-websocket-message (ws frame)
  "Called when FRAME received on NIH WS."
  (jsonrpc-connection-receive
   (websocket-client-data ws) ; the connection
   (json-parse-string
    (websocket-frame-text frame)
    :object-type 'plist
    :null-object nil
    :false-object :json-false)))

(defun nih--http-get-json (host port path)
  "Contact HOST on PORT.  Get PATH as parsed JSON."
  (let ((res
         (catch 'done
           (let (proc
                 (timer (run-with-timer 5 nil
                                        (lambda ()
                                          (throw 'done
                                                 '(:error "Timed out"))))))
             (unwind-protect
                 (let* ((buffer
                         (url-retrieve
                          (format "http://%s:%s/%s"
                                  host port
                                  (replace-regexp-in-string "^/" "" path))
                          (lambda (status)
                            (if (plist-get status :error)
                                (throw 'done `(:error ,status)))
                            (goto-char (point-min))
                            (search-forward "\n\n")
                            (throw 'done
                                   (json-parse-buffer
                                    :object-type 'plist))))))
                   (setq proc (get-buffer-process buffer))
                   (while (process-live-p proc)
                     (accept-process-output proc 10)))
               (cancel-timer timer)
               (when proc (delete-process proc))
               `(:error "process probably died"))))))
    (let ((err (plist-get res :error))) 
      (when err (nih--error "Getting %s resulted in %S" path err)))
    res))


;;; M-x nih-list-connections
;;;
(defvar nih--connection-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") 'nih-connection-list-make-default)
    (define-key map (kbd "k") 'nih-connection-list-kill)
    map))

(defun nih-connection-list-make-default (conn)
  (interactive (list (tabulated-list-get-id)))
  (set-default 'nih--default-connection conn)
  (revert-buffer))

(defun nih-connection-list-kill (conn)
  (interactive (list (tabulated-list-get-id)))
  (jsonrpc-shutdown conn)
  (revert-buffer))

(define-derived-mode nih--connection-list-mode tabulated-list-mode
  "nih" "A mode for listing NIH connections.")

(defun nih--connection-list-recalculate ()
  (setq-local tabulated-list-format `[("Default" 8) ("Name" 24 t)
                                      ("PID" 7 t)
                                      ("Host" 10 t) ("Port" 6 t)
                                      ("Command" 8 t)])
  (setq-local tabulated-list-entries
              (mapcar
               (lambda (conn)
                 (list conn
                       `[,(if (eq nih--default-connection conn) "*" " ")
                         (,(nih--target-uniqueish-nickname (nih--target-info conn))
                          action
                          ,#'(lambda (_button)
                               (pop-to-buffer (nih--repl conn))))
                         ,(format "%s" (or (nih--pid conn) "unknown"))
                         ,(format "%s" (nih--host conn))
                         ,(format "%s" (nih--port conn))
                         ,(if-let (cmd (nih--command conn))
                              (mapconcat #'identity cmd " ")
                            "(direct connection)")]))
               (reverse nih--connections))))

(defun nih-list-connections ()
  (interactive)
  (with-current-buffer (get-buffer-create "*NIH connected targets*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (nih--connection-list-mode)
      (add-hook 'tabulated-list-revert-hook
                'nih--connection-list-recalculate nil t)
      (nih--connection-list-recalculate)
      (tabulated-list-init-header)
      (tabulated-list-print)
      (pop-to-buffer (current-buffer)))))


;;; M-x nih & friends
(defvar nih-host-programs nil)
(setq
 nih-host-programs
 '((firefox  :command "firefox-trunk --remote-debugging-port 0 --new-instance"
             :new-target (lambda () (nih--error "unimplemented")))
   (chromium  :command
              (lambda ()
                "chromium-browser --remote-debugging-port=0 --user-data-dir")
              :new-target (lambda () (nih--error "unimplemented")))
   (node :command "node --inspect=0"
         :new-target (lambda () (nih--error "Not supported for node hosts")))))

(defvar nih--command-history nil)

(defun nih--inferior-process-nickname (proc)
  (format "%s@%s:%s"
          (process-get proc 'nih-command-name)
          (car (process-get proc 'nih-host-and-port))
          (cadr (process-get proc 'nih-host-and-port))))

(defun nih--target-nickname (target)
  "TARGET is a plist of the kind returned by /json/list.
Return a short but no necessarily unique name, unless UNIQUE-ISH
is t."
  (or (let* ((desc (plist-get target :description))
             (url (plist-get target :url))
             (url (and url (replace-regexp-in-string
                            "^file://\\(.*\\)" "\\1" url))))
        (or (and (not (string= "" url)) url)
            (and (not (string= "" desc)) desc)
            (plist-get target :title)
            (plist-get target :id)))
      (symbol-name (cl-gensym "js-"))))

(defun nih--target-uniqueish-nickname (target)
  (format "%s[%s]"
          (nih--target-nickname target)
          (substring (plist-get target :id) 0 6)))

(defun nih--get-targets (host port)
  (nih--http-get-json host port "/json/list"))

(defun nih--read-target (host port)
  (let ((targets (append (nih--get-targets host port) nil)))
    (if (cdr targets)
        (cl-find (completing-read
                  "[nih] Target: "
                  (mapcar #'nih--target-uniqueish-nickname targets))
                 targets
                 :key #'nih--target-uniqueish-nickname
                 :test #'string=)
      (car targets))))

(defun nih--locally-started-processes ()
  (cl-remove-if-not
   (lambda (proc) (process-get proc 'nih-host-and-port))
   (process-list)))

(defun nih--read-host-and-port (specify-p)
  "Return a string and a number suitable for `nih-connect-to-target'"
  (let ((procs (nih--locally-started-processes)))
    (if (or specify-p (null procs))
        (list (read-from-minibuffer
               "[nih] Hostname: ")
              (string-to-number
               (read-from-minibuffer
                "[nih] Port number: ")))
      (let* ((proc (if (cdr procs)
                       (cl-find (completing-read
                                 "[nih] Locally started host: "
                                 (mapcar #'nih--inferior-process-nickname
                                         procs))
                                procs :key #'nih--inferior-process-nickname
                                :test #'string=)
                     (car procs))))
        (append (process-get proc 'nih-host-and-port)
                (list proc))))))

(defun nih--read-contact (specify-p)
  "Read a suitable argument for `nih-start-host'."
  (if specify-p
      `(:command
        ,(read-from-minibuffer
          "[nih] Command to start? "
          nil nil nil 'nih--comand-history))
    (cdr
     (assoc
      (completing-read "[nih] Start new host: "
                       nih-host-programs
                       nil t nil)
      nih-host-programs #'string=))))

(defun nih--read-connection ()
  (let ((name (completing-read "[nih] Connection? "
                   (mapcar #'jsonrpc-name nih--connections))))
    (cl-find name nih--connections :key #'jsonrpc-name
             :test #'string=)))

(defun nih--command-from-command-spec (spec)
  (cl-etypecase spec
    (cons spec)
    (string (split-string spec))
    (function (nih--command-from-command-spec
               (funcall spec)))))

(defun nih--kill-if-dead (proc _change)
  (unless nih-preserve-buffers
    (unless (process-live-p proc)
      (kill-buffer (process-buffer proc)))))

(defun nih--start-new-host (contact)
  (let* ((contact (if (stringp contact) `(:command contact) contact))
         (command-and-args
          (nih--command-from-command-spec (plist-get contact :command)))
         (process-environment (append '() process-environment))
         (process-connection-type nil)
         proc host port)
    (with-current-buffer
        (generate-new-buffer
         (format "*NIH inferior: %S*"
                 (mapconcat #'identity command-and-args " ")))
      (let ((inhibit-read-only t)) (erase-buffer))
      (setq proc
            (apply #'start-process
                   (format "nih-%s" (car command-and-args)) (current-buffer)
                   (car command-and-args) (cdr command-and-args)))
      (process-put proc 'nih-inferior-lisp-process t)
      (set-process-sentinel proc #'nih--kill-if-dead)
      (set-process-query-on-exit-flag proc t)
      (cl-loop repeat 5
               while (and (process-live-p proc)
                          (null host))
               do
               (accept-process-output proc 1)
               (goto-char (point-min))
               (when (search-forward-regexp
                      "wss?://\\(localhost\\|127.0.0.1\\):\\([0-9]+\\)/"
                      nil t)
                 (setq host (substring-no-properties (match-string 1))
                       port (string-to-number (match-string 2)))))
      (unless host
        (nih--error "Couldn't start %s" (car command-and-args)))
      (process-put proc 'nih-host-and-port
                   (list host port))
      (process-put proc 'nih-command-name
                   (car command-and-args))
      (nih--message "Started %S, host=%S and port=%S" (current-buffer)
                    host port)
      (list proc host port))))


;;;
(defun nih (action)
  "Start saying NIH to someone.
ACTION is a symbol naming another interactive function."
  (interactive
   (let ((actions
          `(,@(when nih--connections
                `(nih-switch-to-target))
            ,@(when (or current-prefix-arg
                        (nih--locally-started-processes))
                `(nih-connect-to-target
                  nih-connect-to-new-target))
            nih-start-and-connect-to-target)))
     (list
      (if (cdr actions)
          (intern
           (completing-read "[nih] What to do? " actions nil t))
        (car actions)))))
  (call-interactively action))

(defun nih-switch-to-target (connection)
  "Switch to the REPL of CONNECTION."
  (interactive (list (nih--read-connection)))
  (pop-to-buffer (nih--repl connection)))

(defun nih-connect-to-target (target-url &optional full-target-info interactive)
  "Connect to an existing CDP target on TARGET-URL, a string.

Optional FULL-TARGET-INFO is a plist describing the target fully.

Interactively, select a host and port first.  With a prefix
argument prompt for these two things, else select from previously
started \"inferior\" hosts. In the latter case, if there's only
one, use that, otherwise present a choice.  After selecting a
host, select a TARGET.  If there's more than one prompt user to
select from the minibuffer."
  (interactive
   (cl-destructuring-bind (host port &optional proc)
       (nih--read-host-and-port current-prefix-arg)
     (let ((chosen (nih--read-target host port)))
       (list (plist-get chosen :webSocketDebuggerUrl)
             (append chosen
                     (list :nih--proc proc :nih--host host :nih--port port))
             t))))
  (let ((existing
         (cl-find target-url nih--connections
                  :key #'nih--target-url
                  :test #'string=))
        (full-target-info
         (or full-target-info
             (or full-target-info
                 (list :title "Anonymous target"
                       :id (gensym "unknown-websocket-target-id-")
                       :webSocketDebuggerUrl target-url))))
        new-connection
        websocket)
    (cond ((and existing
                interactive
                (y-or-n-p (format "[nih] switch to existing %s instead?"
                                  (jsonrpc-name existing))))
           (pop-to-buffer (nih--ensure-repl-buffer existing)))
          (t
           (catch 'done
             (setq websocket
                   (websocket-open
                    target-url
                    :on-open
                    (lambda (ws)
                      (setq new-connection
                            (make-instance
                             'nih--connection
                             :name (nih--target-nickname full-target-info)
                             :target-info full-target-info
                             :request-dispatcher
                             (lambda (conn method params)
                               (apply 'nih-handle-request conn method params))
                             :notification-dispatcher
                             (lambda (conn method params)
                               (apply 'nih-handle-notification conn method params))
                             :socket ws))
                      (setf (websocket-client-data ws) new-connection)
                      (throw 'done new-connection))
                    :on-message #'nih--on-websocket-message
                    :on-close   #'nih--on-websocket-close))
             (cl-loop with proc = (websocket-conn websocket)
                      repeat 5
                      while (process-live-p proc)
                      do (accept-process-output proc 1)))))
    (unless new-connection
      (nih--error "Couldn't connect to %s" target-url))
    (push new-connection nih--connections)
    (unless nih--default-connection (setq nih--default-connection
                                          new-connection))
    (unwind-protect
        (progn
          (jsonrpc-request new-connection :Runtime.enable nil)
          (jsonrpc-request new-connection :Log.enable nil))
      (run-hook-with-args 'nih-connected-hook new-connection))))

(defun nih-start-target-on-host (_host _port)
  "Connect to a new CDP target on existing HOST and PORT.

To select a host, first consider previously started \"inferior\"
hosts. If there's only one, use that, otherwise present a choice.
With a prefix argument, read HOST and PORT from minibuffer.

After selecting a host, ask it to create a new target and connect
to that."
  (interactive
   (nih--read-host-and-port current-prefix-arg))
  (nih--error "not implemented"))

(defun nih-start-and-connect-to-target (contact interactive)
  "Launch an inferior host, connect to one of its targets.
CONTACT is a string, or a plist in the form of the cdrs of the
elements of `nih-host-programs'."
  (interactive
   (list (nih--read-contact current-prefix-arg) t))
  (cl-destructuring-bind (proc host port)
      (nih--start-new-host contact)
    (let ((target (append
                   (if interactive
                       (nih--read-target host port)
                     (car (nih--get-targets host port)))
                   (list :nih--proc proc
                         :nih--host host
                         :nih--port port))))
      (nih-connect-to-target
       (plist-get target :webSocketDebuggerUrl)
       target
       interactive))))


;;;; Utils
(cl-defmacro nih--dbind (((_interface) &rest syms) object &body body)
  "Destructure OBJECT, binding VARS in BODY.
INTERFACE is a type described in chromedevtools.github.io.
SYMS are keys of that type."
  (declare (indent 2) (debug (sexp sexp &rest form)))
  `(cl-destructuring-bind
       (&key ,@syms &allow-other-keys) ,object
     ,@body))

(defun nih--ensure-keyword (thing)
  (cond ((keywordp thing) thing)
        ((symbolp thing)
         (intern (concat ":" (symbol-name thing))))
        ((stringp thing)
         (intern (concat ":" thing)))
        (t
         (error "Can't make %s a keyword" thing))))

(defmacro nih--properly-supressing-message (&rest body)
  "Geez...  Supress message() calls in BODY."
  (let ((curr (cl-gensym)))
    `(let ((,curr (current-message))
           (message-log-max nil))
       (let ((inhibit-message t)) ,@body)
       (if ,curr (message ,curr)))))


;;;;
(cl-defmethod nih-handle-notification
  (_server method &rest args &key &allow-other-keys)
  "Handle unknown notification"
  (nih--message "Server sent unknown notification method `%s' with args %S"
                method
                args))

(cl-defmethod nih-handle-notification
  (conn (_method (eql Runtime.consoleAPICalled)) &rest all &key type args
        &allow-other-keys)
  (apply #'nih--console-api-call conn (nih--ensure-keyword type) args all))


;;;; Console API loggers

(cl-defgeneric nih--console-api-call (conn method args &key &allow-other-keys))

(cl-defmethod nih--console-api-call (conn (_method (eql :log)) args
                                          &rest whole-call &key &allow-other-keys )
  (with-current-buffer (nih-repl conn)
    (nih--message "Would be inserting %s in %s remembering %s"
                  (plist-get (aref args 0) :value)
                  (current-buffer)
                  whole-call)))


;;; Object formatting
(defun nih--pp-get-remote (remote-object-id)
  (plist-get (jsonrpc-request (nih--current-connection)
                              :Runtime.getProperties
                              (list :objectId remote-object-id
                                    :ownProperties t))
             :result))

(defun nih--insert (&rest strings)
  (if-let (proc (nih--repl-process))
      (dolist (string strings)
        (comint-output-filter (nih--repl-process) string))
    (apply #'insert strings)))

(defun nih--pp-object (remote-object-id
                              _whole
                              arrayp
                              before
                              after)
  (cl-loop
   with remote-object = (nih--pp-get-remote remote-object-id)
   with (n-to-print . maxlen)
   = (cl-loop for d across remote-object
              unless (eq (plist-get d :enumerable) :json-false)
              maximize (length (plist-get d :name)) into maxlen
              and count 1 into n-to-print
              finally (cl-return (cons n-to-print maxlen)))
   for desc across remote-object
   initially (nih--insert before)
   do (nih--dbind ((PropertyDescriptor) name
                   ((:value remote-object)) enumerable)
          desc
        (nih--dbind ((Result.RemoteObject) type objectId subtype)
            remote-object
          (unless (eq enumerable :json-false)
            (cl-decf n-to-print)
            (unless arrayp
              (nih--insert (propertize name 'font-lock-face
                                       'font-lock-function-name-face)
                           (make-string (- maxlen
                                                (length name))
                                             ? )
                           " : "))
            (nih--pp-result objectId
                            (nih--ensure-keyword type)
                            (nih--ensure-keyword subtype)
                            remote-object)
            (when (cl-plusp n-to-print) (nih--insert ",\n")))))
   finally
   (when (cl-plusp n-to-print) (nih--insert "..."))
   (nih--insert after)))

(cl-defgeneric nih--pp-result (remote-object-id type subtype whole)
  "Print description REMOTE-OBJECT-ID of TYPE/SUBTYPE at point.
WHOLE is the whole RemoteObject plist.")

(defvar nih--pp-indent t)

(cl-defmethod nih--pp-result :around (remote-object-id
                                      _type
                                      _subtype
                                      whole)
  (let ((original-buffer (current-buffer))
        (beg (point)))
    (prog1
        (cond (nih--pp-indent
               (with-temp-buffer
                 (js-mode)
                 (let ((nih--pp-indent nil))
                   (cl-call-next-method))
                 (nih--properly-supressing-message
                  (indent-region (point-min) (point-max)))
                 (let ((text (buffer-substring
                              (point-min) (point-max)))
                       (ovs (overlays-in (point-min) (point-max))))
                   (with-current-buffer original-buffer
                     (nih--insert text)
                     (dolist (ov ovs)
                       (move-overlay ov
                                     (+ (1- beg) (overlay-start ov))
                                     (+ (1- beg) (overlay-end ov))
                                     (current-buffer)))))))
              (t (cl-call-next-method)))
      (when remote-object-id
        (make-button beg (point)
                     'face nil
                     'nih--remote-object whole
                     'help-echo
                     (format "mouse-2, RET: Inspect %s %s"
                             (or (plist-get whole :subtype)
                                 (plist-get whole :type))
                             remote-object-id))))))

(cl-defmethod nih--pp-result (remote-object-id
                              (_type (eql :object))
                              (_subtype (eql :array))
                              whole)
  (if remote-object-id
      (nih--pp-object remote-object-id whole t "[" "]")
    (nih--insert "Array")))

(cl-defmethod nih--pp-result (remote-object-id
                              (_type (eql :object))
                              _subtype
                              whole)
  (if remote-object-id
      (nih--pp-object remote-object-id whole nil "{ " " }")
    (nih--insert "Object")))

(cl-defmethod nih--pp-result (_remote-object-id
                              (_type (eql :object))
                              (_subtype (eql :null))
                              _whole)
  (nih--insert (propertize "null" 'font-lock-face 'font-lock-constant-face)))

(cl-defmethod nih--pp-result (_remote-object-id
                              (_type (eql :number))
                              _subtype
                              whole)
  (nih--insert
   (propertize (format "%s" (plist-get whole :value))
               'font-lock-face 'font-lock-type-face)))

(cl-defmethod nih--pp-result (_remote-object-id
                              (_type (eql :string))
                              _subtype
                              whole)
  (nih--insert
   (propertize (format "%S" (plist-get whole :value))
               'font-lock-face 'font-lock-string-face)))

(cl-defmethod nih--pp-result (remote-object-id
                              (_type (eql :function))
                              _subtype
                              _whole)
  (let ((name-prop
         (cl-find "name" (nih--pp-get-remote remote-object-id)
                  :key (lambda (d) (plist-get d :name))
                  :test #'string=)))
    (if name-prop
        (nih--insert (format "<function %s>"
                             (plist-get (plist-get name-prop :value)
                                        :value)))
      (nih--insert "<function>"))))

(cl-defmethod nih--pp-result (_remote-object-id
                              (_type (eql :boolean))
                              _subtype
                              whole)
  (nih--insert
   (propertize (if (eq (plist-get whole :value)
                                    :json-false)
                                "false"
                              "true")
               'font-lock-face 'font-lock-constant-face)))

(cl-defmethod nih--pp-result (_remote-object-id
                              (_type (eql :nil))
                              _subtype
                              _whole)
  (nih--insert (propertize "nil?" 'font-lock-face 'font-lock-constant-face)))

(cl-defmethod nih--pp-result (_remote-object-id
                              (_type (eql :undefined))
                              _subtype
                              _whole)
  (nih--insert (propertize "undefined" 'font-lock-face 'font-lock-constant-face)))

(cl-defmethod nih--pp-result (_remote-object-id
                              (_type (eql :symbol))
                              _subtype
                              _whole)
  (nih--insert "symbol"))



;;;; repl

(require 'js)
(require 'comint)

(defun nih-repl (conn)
  (interactive (list (nih--current-connection)))
  (cl-assert conn "No current connection")
  (let ((repl (nih--repl conn)))
    (if (and repl (buffer-live-p repl)) (pop-to-buffer repl)
      (nih-repl-new conn))))
;; (global-set-key (kbd "C-c C-z") 'nih-repl)

(defvar nih--repl-output-mark nil)

(define-derived-mode nih--repl-mode comint-mode
  nil "A mode for the NIH REPL"
  (cl-loop for (var . value)
           in `((comint-use-prompt-regexp          . nil)
                (comint-inhibit-carriage-motion    . t)
                (comint-input-sender               . nih--repl-input-sender)
                (comint-output-filter-functions    . nil)
                (comint-input-filter-functions     . nil)
                (comint-history-isearch            . dwim)
                (comint-input-ignoredups           . t)
                (comint-prompt-read-only           . t)
                (comint-process-echoes             . nil)
                (indent-line-function              . lisp-indent-line)
                (nih--repl-read-mark               . nil)
                (nih--repl-pending-output          . nil)
                (nih--repl-output-mark             . ,(point-marker))
                (nih--repl-last-prompt-overlay     . ,(make-overlay 0 0 nil nil))
                (mode-line-process                 . nil)
                (parse-sexp-ignore-comments        . t)
                (syntax-propertize-function        . nih--repl-syntax-propertize)
                (comint-scroll-show-maximum-output . nil)
                (comint-scroll-to-bottom-on-input  . nil)
                (comint-scroll-to-bottom-on-output . nil)
                (inhibit-field-text-motion         . nil)
                (buffer-file-coding-system         . utf-8-unix))
           do (set (make-local-variable var) value))
  (set-marker-insertion-type nih--repl-output-mark nil)
  (add-hook 'kill-emacs-hook 'nih--repl-save-histories)
  ;;(set (make-local-variable 'comint-get-old-input) 'ielm-get-old-input)
  (set-syntax-table js-mode-syntax-table)

  ;; Add hooks to isearch-mode placed strategically after the ones
  ;; set by comint.el itself.
  ;;
  ;; (add-hook 'isearch-mode-hook 'nih--repl-setup-comint-isearch t t)
  ;; (add-hook 'isearch-mode-end-hook 'nih--repl-teardown-comint-isearch t t)

  ;; Add a post-command-handler
  ;;
  (add-hook 'kill-buffer-hook 'nih--repl-teardown nil t))

(setq nih--repl-mode-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map comint-mode-map)
        (define-key map (kbd "RET") 'nih-repl-return)
        map))

(defun nih--repl-process ()
  "Internal NIH REPL process just for comint.el purposes.
Mostly a dummy, the only interesting thing is the process mark
which is the place in the buffer where comint will insert
output."
  (get-buffer-process (current-buffer)))

(defun nih--repl-syntax-propertize (beg end)
  "Make everything up to current prompt comment syntax."
  (remove-text-properties beg end '(syntax-table nil))
  (let ((end (min end (nih--repl-safe-mark)))
        (beg beg))
    (when (> end beg)
      (unless (nth 8 (syntax-ppss beg))
        (add-text-properties beg (1+ beg)
                             `(syntax-table ,(string-to-syntax "!"))))
      (add-text-properties (1- end) end
                           `(syntax-table ,(string-to-syntax "!"))))))

(defun nih--repl-teardown (&optional reason)
  "Tear down the NIH REPL.
Called in `kill-buffer-hook' or when the NIH connection is closed
for some reason."
  (remove-hook 'kill-buffer-hook 'nih--repl-teardown t)
  (delete-process (nih--repl-process))
  (nih--repl-insert-note (format "Tearing down becasue %s" reason))
  (nih--repl-save-this-buffers-history))

(defun nih--ensure-repl-buffer (conn)
  "Get or create suitable REPL buffer for CONN."
  (let* (probe
         preferred-name
         (buffer
          (cond
           ;; nih--repl buffer object is live, use it
           ((and (setq probe (nih--repl conn))
                 (buffer-live-p probe))
            probe)
           ;; nih--repl buffer is nil.  This is likely the first-time
           ;; REPL launch.  Check that any live buffer is NOT being
           ;; used for another live connection.  If that checks out,
           ;; reuse it.
           ((and (setq probe
                       (get-buffer
                        (setq preferred-name
                              (format "*NIH REPL: %s*" (jsonrpc-name conn)))))
                 (not (with-current-buffer probe
                        (jsonrpc-running-p nih--default-connection))))
            probe)
           ;; Buffer exists, but someone else is using it. We need to
           ;; find a buffer with a new name.  Use a more specific name
           ;; to try to avoid clashes.
           (probe
            (generate-new-buffer
             (format "*NIH REPL: %s*" (nih--target-uniqueish-nickname
                                       (nih--target-info conn)))))
           ;; Buffer doesn't exist, we can make it safely
           (t
            (get-buffer-create preferred-name)))))
    (with-current-buffer buffer
      (setf (nih--repl conn) buffer)
      (setq-local nih--default-connection conn))
    buffer))

(defun nih--repl-insert-prompt (proc)
  "Insert the prompt into the NIH REPL."
  (unless (bolp) (comint-output-filter proc "\n"))
  (comint-output-filter proc "JS> "))

(defvar nih--in-repl-debug nil) ;; (setq nih--in-repl-debug t)

(defun nih--repl-insert-result (result)
  (nih--dbind ((Result.RemoteObject) type subtype objectId)
      result
    (when (and nih--in-repl-debug objectId)
      (nih--repl-insert-note (and objectId
                                  (jsonrpc-request (nih--current-connection)
                                                   :Runtime.getProperties
                                                   (list :objectId objectId
                                                         :ownProperties t
                                                         :generatePreview t)))))
    (let ((nih--dispatching-connection nih--default-connection))
      (nih--pp-result objectId
                      (nih--ensure-keyword type)
                      (nih--ensure-keyword subtype)
                      result))))

(cl-defun nih--repl-input-sender (proc string &aux success)
  "Send STRING to PROC."
  (unwind-protect
      (cl-destructuring-bind (&key result exceptionDetails)
          (jsonrpc-request (process-get proc 'nih--connection)
                           :Runtime.evaluate
                           `(:expression
                             ,(substring-no-properties
                               (string-trim string))
                             :replMode t
                             :generatePreview t))
        (cond (exceptionDetails
               (nih--dbind ((Result.RemoteObject) text exception)
                   exceptionDetails
                 (when nih--in-repl-debug
                   (nih--repl-insert-note exception))
                 (comint-output-filter
                  proc
                  (propertize (format "%s %s\n" text (plist-get exception
                                                                :description))
                              'font-lock-face 'font-lock-warning-face))))
              (result
               (when nih--in-repl-debug
                 (nih--repl-insert-note result))
               (nih--repl-insert-result result))
              (t
               (nih--error "Unkonwn reply to Runtime.evaluate")))
        (setq success t))
    (unless success
      (nih--repl-insert-note "Something went wrong"))
    (nih--repl-insert-prompt proc)))

(defun nih--repl-read-in-history ()
  "Read in history from the ~/.nih-repl-history file."
  (let ((comint-input-ring-separator "####\n")
        (comint-input-ring-file-name (expand-file-name "~/.nih-repl-history")))
    (comint-read-input-ring)))

(defun nih--repl-save-this-buffers-history ()
  "Save this buffer's history."
  (cl-loop
   with unsaved = (copy-tree comint-input-ring 'vectors-too)
   initially (nih--repl-read-in-history) ; this clobbers `comint-input-ring'
   for i from (1- (ring-length unsaved)) downto 0
   for item = (ring-ref unsaved i)
   for existing-index = (ring-member comint-input-ring item)
   do (cond (existing-index
             (ring-remove comint-input-ring existing-index)
             (ring-insert comint-input-ring item))
            (t
             (ring-insert comint-input-ring item)))
   unless (ring-member comint-input-ring item)
   do (ring-insert comint-input-ring item))
  ;; Now save `comint-input-ring'
  (let ((coding-system-for-write 'utf-8-unix)
        (comint-input-ring-separator "####\n")
        (comint-input-ring-file-name (expand-file-name "~/.nih-repl-history")))
    (comint-write-input-ring)))

(defun nih--repl-save-histories ()
  "Save all NIH REPL histories, except for current buffer's."
  (dolist (buffer (buffer-list))
    (unless (eq buffer (current-buffer))
      (with-current-buffer buffer
        (when (eq major-mode 'nih--repl-mode)
          (nih--repl-save-this-buffers-history))))))

(defun nih--repl-mark ()
  "Returns a marker to the end of the last prompt."
  (let ((proc (nih--repl-process)))
    (unless proc (nih--error "Not in a connected REPL"))
    (process-mark proc)))

(defun nih--repl-safe-mark ()
  "Like `nih--repl-mark', but safe if there's no process."
  (if (nih--repl-process) (nih--repl-mark) (copy-marker (point-max))))

(defmacro nih--repl-commiting-text (props &rest body)
  (declare (debug (sexp &rest form)) (indent 1))
  (let ((start-sym (cl-gensym)))
    `(let ((,start-sym (marker-position (nih--repl-safe-mark)))
           (inhibit-read-only t))
       ,@body
       (add-text-properties ,start-sym (nih--repl-safe-mark)
                            (append '(read-only t front-sticky (read-only))
                                    ,props)))))

(defun nih-repl-return ()
  "Send the current JS statement for evaluation."
  (interactive)
  (comint-send-input))

(defface nih--repl-note-face
  `((t (:inherit font-lock-keyword-face)))
  "Face for the REPL notes."
  :group 'nih)

(defun nih--repl-insert-note (string &optional face)
  "Insert a note into the REPL."
  (let* ((face (or face 'nih--repl-note-face))
         (string (if (stringp string) string (pp-to-string string)))
         (string (replace-regexp-in-string "^" "// " (string-trim string))))
    (nih--repl-commiting-text (when face
                                `(face ,face font-lock-face ,face))
      (cond ((nih--repl-process)
             ;; notes are inserted "synchronously" with the process mark  process
             (comint-output-filter (nih--repl-process)
                                   (format "%s\n" string)))
            (t
             ;; If no process yet, fall back to the simpler strategy.
             (goto-char (point-max))
             (unless (bolp) (newline))
             (insert string "\n"))))))

(defun nih-repl-new (conn)
  "Create and setup a new REPL buffer for CONN.
CONN defaults to the current NIH connection."
  (interactive (list (nih--current-connection)))
  (let* ((buffer (nih--ensure-repl-buffer conn)))
    (with-current-buffer (pop-to-buffer buffer)
      ;; Take this oportunity to save any other REPL histories so that
      ;; the new REPL will see them.
      (nih--repl-save-histories)
      (goto-char (point-max))
      ;; Maybe print some introductory message?
      (nih--repl-mode)
      (let ((proc
             (start-process (format "NIH REPL %s" (jsonrpc-name conn))
                            (current-buffer)
                            nil)))
        (set-process-sentinel proc #'ignore)
        (set-process-query-on-exit-flag proc nil)
        (process-put proc 'nih--connection conn)
        ;; maybe do something that enables the remote REPL?
        (nih--repl-read-in-history)
        (nih--repl-insert-note (format "Welcome to %s!"
                                       (jsonrpc-name conn)))
        (nih--repl-insert-prompt proc)))))

(provide 'nih)
;;; nih.el ends here
