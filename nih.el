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


;;;; Utils
;;;;
(cl-defmacro nih--dbind (((_interface) &rest syms) object &body body)
  "Destructure OBJECT, binding VARS in BODY.
INTERFACE is a type described in chromedevtools.github.io.
SYMS are keys of that type."
  ;; TODO: make this smarter by plugging int the protocol spec somehow
  (declare (indent 2) (debug (sexp form &rest form)))
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

(cl-defmacro nih--when-live-buffer (buf &rest body)
  "Check BUF live, then do BODY in it." (declare (indent 1) (debug t))
  (let ((b (cl-gensym)))
    `(let ((,b ,buf)) (if (buffer-live-p ,b) (with-current-buffer ,b ,@body)))))

(defun nih--message (format &rest args)
  "Message out with FORMAT with ARGS."
  (message "[nih] %s" (apply #'format format args)))

(defun nih--error (format &rest args)
  "Error out with FORMAT with ARGS."
  (error "[nih] %s" (apply #'format format args)))

(defun nih--mouse-call (what)
  "Make an interactive lambda for calling WHAT from mode-line."
  (lambda (event)
    (interactive "e")
    (let ((start (event-start event))) (with-selected-window (posn-window start)
                                         (save-excursion
                                           (goto-char (or (posn-point start)
                                                          (point)))
                                           (call-interactively what)
                                           (force-mode-line-update t))))))

(defun nih--mode-line-props (thing face defs &optional prepend)
  "Helper for function `nih--mode-line-format'.
Uses THING, FACE, DEFS and PREPEND."
  (cl-loop with map = (make-sparse-keymap)
           for (elem . rest) on defs
           for (key def help) = elem
           do (define-key map `[mode-line ,key] (nih--mouse-call def))
           concat (format "%s: %s" key help) into blurb
           when rest concat "\n" into blurb
           finally (return `(:propertize
                             ,thing
                             face ,face
                             keymap ,map help-echo ,(concat prepend blurb)
                             mouse-face mode-line-highlight))))


;;;; Basic connection management
;;;;
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
            (car nih--connections))
      (nih--error "No NIH connection!")))

(defun nih-cycle-connections (interactive)
  "Cycle NIH connection."
  (interactive (list t))
  (setq nih--default-connection
        (or (cdr (member nih--default-connection nih--connections))
            (car nih--connections)))
  (when interactive
    (nih--message "Active connection is now %s" nih--default-connection)))

(defconst nih--{} (make-hash-table) "The empty JSON object.")

(defvar nih-connected-hook 'nih-repl-new
  "Hook of functions run when NIH connects.
Each function is called with a single
`nih--connection' object.")

(defvar nih-preserve-buffers nil
  "If nil, kill all buffers when a connection is removed.")

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


;;;; M-x nih-list-connections
;;;;
(defvar nih--connection-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") 'nih-connection-list-make-default)
    (define-key map (kbd "k") 'nih-connection-list-kill)
    (define-key map (kbd "RET") 'nih-connection-list-repl)
    map))

(defun nih-connection-list-make-default (conn)
  (interactive (list (tabulated-list-get-id)))
  (set-default 'nih--default-connection conn)
  (revert-buffer))

(defun nih-connection-list-kill (conn)
  (interactive (list (tabulated-list-get-id)))
  (jsonrpc-shutdown conn)
  (revert-buffer))

(defun nih-connection-list-repl (conn)
  (interactive (list (tabulated-list-get-id)))
  (nih-repl conn t))

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


;;;; Helpers for M-x nih
;;;;
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

(defvar nih--read-host-and-port-history nil)

(defun nih--read-host-and-port ()
  "Return (HOST NUMBER [PROC]) suitable for `nih-connect-to-target'"
  (let* ((ht (make-hash-table :test #'equal))
         (rht (make-hash-table :test #'equal)))
    (dolist (proc (nih--locally-started-processes))
      (let ((host-and-port (process-get proc 'nih-host-and-port)))
        (setf (gethash (nih--inferior-process-nickname proc) ht)
              (append host-and-port (list proc)))
        (setf (gethash host-and-port rht) t)))
    (dolist (conn nih--connections)
      (let ((host-and-port (list (nih--host conn) (nih--port conn))))
        (unless (gethash host-and-port rht)
          (setf (gethash (format "%s:%s"
                                 (car host-and-port)
                                 (cadr host-and-port))
                         ht)
                host-and-port))))
    (let* ((input (completing-read "[nih] Known host or <hostname:port>: "
                                   ht
                                   nil nil nil
                                   'nih--read-host-and-port-history))
           (match (gethash input ht)))
      (or match
          (and (string-match "\\([^:]+\\):\\([0-9]+\\)" input)
               (list (match-string 1 input)
                     (string-to-number (match-string 2 input))))))))

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


;;;; M-x nih
;;;;
(defun nih (action)
  "Start saying NIH to someone.
ACTION is a symbol naming another interactive function."
  (interactive
   (let ((actions
          `(,@(when nih--connections
                `(nih-switch-to-target
                  nih-connect-to-target))
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
       (nih--read-host-and-port)
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
                (y-or-n-p (format (concat "[nih] Already connected. "
                                          "Switch to existing %s instead?")
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
                      do (accept-process-output proc 1)))
           (unless new-connection
             (nih--error "Couldn't connect to %s" target-url))
           (push new-connection nih--connections)
           (unless nih--default-connection (setq nih--default-connection
                                                 new-connection))
           (unwind-protect
               (progn
                 (jsonrpc-request new-connection :Runtime.enable nil)
                 (ignore-errors
                   (jsonrpc-request new-connection :Console.disable nil))
                 (ignore-errors
                   (jsonrpc-request new-connection :Log.enable nil)))
             (run-hook-with-args 'nih-connected-hook new-connection))))))

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


;;;; CDP Events (aka JSONRPC notifications)
;;;;
(cl-defmethod nih-handle-notification
  (_server method &rest args &key &allow-other-keys)
  "Handle unknown notification"
  (nih--message "Server sent unknown notification method `%s' with args %S"
                method
                args))

(cl-defmethod nih-handle-notification
  (conn (_method (eql Runtime.consoleAPICalled))
        &key type stackTrace timestamp args
        &allow-other-keys)
  (let ((level (alist-get type
                          '(("warning"   . "warning")
                            ("info"      . "info")
                            ("log"       . "info")
                            ("error"     . "error")
                            ("exception" . "error")
                            ("debug"     . "verbose"))
                          nil nil #'equal)))
    (when level
      (nih--repl-log conn
                     :source "javascript"
                     :level level
                     :args args
                     :timestamp timestamp
                     :stackTrace stackTrace))))

(cl-defmethod nih-handle-notification
  (conn (_method (eql Console.messageAdded))
        &key message
        &allow-other-keys)
  (nih--dbind ((Console.ConsoleMessage) source level text url line column)
      message
    (nih--repl-log conn
                   :source source
                   :level level
                   :args text
                   :url url
                   :line line
                   :column column)))

(cl-defmethod nih-handle-notification
  (conn (_method (eql Log.entryAdded)) &rest all &key entry
        &allow-other-keys)
  (nih--dbind ((LogEntry)
               source level text url lineNumber timestamp args
               stackTrace)
      entry
    (nih--repl-log conn
                   :source source
                   :level level
                   :args (or args text)
                   :url url
                   :line lineNumber
                   :timestamp timestamp
                   :stackTrace stackTrace)))

(cl-defmethod nih-handle-notification
  (conn (_method (eql Runtime.executionContextCreated)) &rest all)
  (nih--when-live-buffer (nih--repl conn)
    (nih--repl-insert-note (format "ExecutionContext Created: %S" all)
                           nil t)
    ;; FIXME: maybe do this in the execution context just created?
    (nih--repl-init-object-store)))

(cl-defmethod nih-handle-notification
  (conn (_method (eql Runtime.executionContextDestroyed)) &rest all)
  (nih--when-live-buffer (nih--repl conn)
    (nih--repl-insert-note (format "ExecutionContext Destroyed: %S" all)
                           nil t)))

(cl-defmethod nih-handle-notification
  (conn (_method (eql Runtime.executionContextsCleared)) &rest all)
  (nih--when-live-buffer (nih--repl conn)
    (nih--repl-insert-note (format "All ExecutionContexts cleared %S" all)
                           nil t)))


;;;; Object formatting
;;;;
(defun nih--pp-get-remote (remote-object-id)
  (let ((res (jsonrpc-request (nih--current-connection)
                              :Runtime.getProperties
                              (list :objectId remote-object-id
                                    :ownProperties t))))
    (list (plist-get res :result) (plist-get res :internalProperties))))

(defmacro nih--repl-commiting-text (props &rest body)
  (declare (debug (sexp &rest form)) (indent 1))
  (let ((start-sym (cl-gensym)))
    `(let ((,start-sym (marker-position (nih--repl-safe-mark)))
           (inhibit-read-only t))
       (goto-char ,start-sym)
       ,@body
       (add-text-properties ,start-sym (nih--repl-safe-mark)
                            (append '(read-only t front-sticky (read-only))
                                    ,props)))))

(defvar nih--pp-synchronously t
  "Non-nil if printing synchronously from interactive command.")
(defvar nih--pp-more-properties t
  "Non-nil if non-enumerable and internal properties should be printed.")
(defvar nih--pp-prin1 t "Non-nil, `prin1' is used for strings.")

(defun nih--insert (&rest strings)
  (if-let (proc (and nih--pp-synchronously (nih--repl-process)))
      (nih--repl-commiting-text ()
        (dolist (string strings)
          (comint-output-filter (nih--repl-process) string)))
    (apply #'insert strings)))

(cl-defgeneric nih--pp-delimiters (type subtype &key props preview))

(cl-defmethod nih--pp-delimiters
  ((_type (eql :object)) (_subtype (eql :array))
   &key &allow-other-keys)
  (list "[" nil "]"))

(cl-defmethod nih--pp-delimiters
  ((_type (eql :object)) _subtype &key preview &allow-other-keys)
  (let ((desc (plist-get preview :description)))
    (list "{" (unless (string= desc "Object") desc) "}")))

(cl-defmethod nih--pp-delimiters
  ((_type (eql :function)) _subtype &key props preview)
  (let* ((fdesc (nih--pp-format-function-desc
                 (plist-get preview :description)))
         (fname (or (plist-get preview :name)
                    (cl-some
                     (lambda (p)
                       (and (equal (plist-get p :name) "name")
                            (plist-get (plist-get p :value) :value)))
                     props)))
         (props-p (or nih--pp-more-properties (cl-plusp (length props)))))
    (cl-destructuring-bind (b . a)
        (if props-p `("{" . "}") `("" . ""))
      (list
       b
       (cond ((setq fdesc (nih--pp-format-function-desc
                           (plist-get preview :description)))
              fdesc)
             ((setq fname
                    (or (plist-get preview :name)
                        (cl-some
                         (lambda (p)
                           (and (equal (plist-get p :name) "name")
                                (plist-get (plist-get p :value) :value)))
                         props)))
              (format "function %s" fname))
             (t
              "function"))
       a))))

(defun nih--pp-format-function-desc (desc)
  (let ((pos (cl-position ?{ desc)))
    (when pos
      (string-trim
        (replace-regexp-in-string
         "\n" "" (substring desc 0 pos))))))

(defun nih--pp-obj-label (remote-object)
  (let ((receipt (plist-get remote-object :nih--repl-history-id))
        (desc (plist-get remote-object :description)))
    (if receipt
      (format "@[%s] == %s" receipt desc)
      (format "%s" desc))))

(defun nih--button-help-echo (&rest things)
  (mapconcat #'identity things ", "))

(defun nih--pp-expanded-from-remote (remote-object)
  (nih--dbind ((Result.RemoteObject) type subtype ((:objectId remote-object-id))
               nih--repl-history-id)
      remote-object
    (cl-loop
     with (properties internal-properties) = (nih--pp-get-remote remote-object-id)
     with type = (nih--ensure-keyword type)
     with subtype = (nih--ensure-keyword subtype)
     with relevant = (cond (nih--pp-more-properties
                            (append properties internal-properties nil))
                           (t
                            (append (cl-remove-if (lambda (p)
                                                    (eq (plist-get p :enumerable)
                                                        :json-false))
                                                  properties)
                                    nil)))
     with maxlen = (cl-loop for p in relevant
                            maximize (length (plist-get p :name)))
     with (before meat after) = (nih--pp-delimiters type
                                                    subtype
                                                    :props relevant
                                                    :preview remote-object)
     with end-marker = nil
     for (p . more) on relevant
     initially
     (nih--insert (make-text-button
                   (concat meat (and meat " ") before)
                    nil
                   'mouse-face 'highlight
                   'type 'nih--collapse 'action 'nih--collapse
                   'nih--repl-history-id nih--repl-history-id
                   'nih--remote-object remote-object
                   'nih--object-end (setq end-marker
                                          (copy-marker (point) t))
                   'help-echo (nih--button-help-echo
                               (nih--pp-obj-label remote-object)
                               "mouse-2, RET: Collapse"
                               "M-RET: Return to REPL"
                               "mouse-3: Pop up menu")))
     (nih--insert (if meat "\n  " " "))
     do (nih--dbind ((PropertyDescriptor) name value) p
          (unless (and (not nih--pp-more-properties) (eq subtype :array))
            (nih--insert (make-text-button
                          name nil
                          'font-lock-face 'font-lock-function-name-face
                          'type 'nih--property-name
                          'nih--remote-object value
                          'nih--owner-object remote-object
                          'nih--property-descriptor p
                          'help-echo (nih--button-help-echo
                                      name
                                      "mouse-2, RET: Expand/collapse value"
                                      "e: Edit value "
                                      "M-RET: Return to REPL"
                                      "mouse-3: Pop up menu"))
                         (make-string (- maxlen (length name)) ? )
                         " : "))
          (nih--pp-collapsed value)
          (when more
            (nih--insert ",")
            (if (and (not nih--pp-more-properties) (eq subtype :array))
                (nih--insert " ")
              (nih--insert "\n  "))))
     finally
     (when more (nih--insert "..."))
     (nih--insert " " after)
     (set-marker-insertion-type end-marker nil))))

(defun nih--pp-collapsed-from-preview (preview type subtype)
  (cl-loop with (before meat after) = (nih--pp-delimiters type subtype :preview preview)
           initially
           (when meat (nih--insert meat " "))
           (nih--insert before " ")
           with properties = (plist-get preview :properties)
           with n-to-print = (length properties)
           for desc across properties
           do
           (cl-decf n-to-print)
           (unless (eq subtype :array)
             (nih--insert (propertize
                           (plist-get desc :name)
                           'font-lock-face 'font-lock-function-name-face
                           'face 'font-lock-function-name-face)
                          " : "))
           (nih--pp-collapsed desc)
           (when (cl-plusp n-to-print) (nih--insert ", "))
           finally
           (unless (eq (plist-get preview :overflow)
                       :json-false)
             (nih--insert "..."))
           (nih--insert " " after)))

(defun nih--pp-collapsed (remote-object &optional nobutton)
  (nih--dbind ((Result.RemoteObject) type subtype ((:objectId remote-object-id))
               nih--repl-history-id)
      remote-object
    (let ((start (point))
          (preview (plist-get remote-object :preview))
          (type (nih--ensure-keyword type))
          (subtype (nih--ensure-keyword subtype)))
      (if preview
          (nih--pp-collapsed-from-preview preview type subtype)
        (nih--pp-primitive type subtype remote-object))
      (if (and (not nobutton) remote-object-id)
          (make-text-button start (point)
                            'mouse-face 'highlight
                            'face nil
                            'type 'nih--expand  'action 'nih--expand
                            'nih--repl-history-id nih--repl-history-id
                            'nih--remote-object remote-object
                            'help-echo (nih--button-help-echo
                                        (nih--pp-obj-label remote-object)
                                        "mouse-2, RET: Expand"
                                        "M-RET: Return to REPL"
                                        "mouse-3: Pop up menu"))))))

(cl-defgeneric nih--pp-primitive (type subtype whole)
  "Print primitive value of TYPE/SUBTYPE at point.
WHOLE is either the whole Result.RemoteObject or a
Runtime.PropertyPreview plist.  Anyway, should have `value'.")

(cl-defmethod nih--pp-primitive ((_type (eql :object))
                                 (_subtype (eql :null))
                                 _whole)
  (nih--insert (propertize "null" 'font-lock-face 'font-lock-constant-face)))

(cl-defmethod nih--pp-primitive ((_type (eql :number))
                                 _subtype
                                 whole)
  (nih--insert
   (propertize (format "%s" (plist-get whole :value))
               'font-lock-face 'font-lock-type-face)))

(cl-defmethod nih--pp-primitive ((_type (eql :string))
                                 _subtype
                                 whole)
  (let ((str (plist-get whole :value)))
    (nih--insert
     (if nih--pp-prin1
         (propertize (prin1-to-string str)
                     'font-lock-face 'font-lock-string-face)
       str))))

(cl-defmethod nih--pp-primitive ((_type (eql :boolean))
                                 _subtype
                                 whole)
  (nih--insert
   (propertize (if (eq (plist-get whole :value)
                       :json-false)
                   "false"
                 "true")
               'font-lock-face 'font-lock-constant-face)))

(cl-defmethod nih--pp-primitive ((_type (eql :nil))
                                 _subtype
                                 _whole)
  (nih--insert (propertize "nil?" 'font-lock-face 'font-lock-constant-face)))

(cl-defmethod nih--pp-primitive ((_type (eql :undefined))
                                 _subtype
                                 _whole)
  (nih--insert (propertize "undefined" 'font-lock-face 'font-lock-constant-face)))

(cl-defmethod nih--pp-primitive ((_type (eql :symbol))
                                 _subtype
                                 _whole)
  (nih--insert "symbol"))

(cl-defmethod nih--pp-primitive ((_type (eql :function))
                                 _subtype
                                 whole)
  (let* ((desc (plist-get whole :description))
         (abbrev (nih--pp-format-function-desc desc)))
    (nih--insert (or abbrev desc "function"))))

(cl-defmethod nih--pp-primitive (_type
                                 (_subtype (eql :array))
                                 whole)
  (let* ((desc (plist-get whole :description)))
    (nih--insert (or desc "Array"))))

(cl-defmethod nih--pp-primitive ((_type (eql :object))
                                 _subtype
                                 whole)
  (let* ((desc (plist-get whole :description)))
    (nih--insert (or desc "Object"))))

(cl-defmethod nih--pp-primitive ((_type (eql :object))
                                 (_subtype (eql :array))
                                 whole)
  (let* ((desc (plist-get whole :description)))
    (nih--insert (or desc "Array"))))


;;; Buttons and button actions
;;;
(require 'text-property-search)

(defun nih--next-button (&optional interactive) "Skip to the next button."
  (interactive (list t))
  (let ((match (text-property-search-forward
                'category 'nih (lambda (_ v)
                                 (button-type-subtype-p
                                  (get v 'type) 'nih))
                t)))
    (if (not match) (user-error "[nih] No previous button")
      (goto-char (prop-match-beginning match))
      (when interactive
        (nih--message "%s" (get-text-property (point) 'help-echo)))
      (button-at (point)))))

(defun nih--previous-button (&optional interactive) "Skip to previous button."
  (interactive (list t))
  (if (not (text-property-search-backward
                'category 'nih (lambda (_ v)
                                 (button-type-subtype-p
                                  (get v 'type) 'nih))
                t))
      (user-error "[nih] No previous button")
    (when interactive
      (nih--message "%s" (get-text-property (point) 'help-echo)))
    (button-at (point))))

(define-button-type 'nih 'face nil :supertype 'button)

(defvar nih--button-parent-keymap)
(setq nih--button-parent-keymap
 (let ((map (make-sparse-keymap)))
   (define-key map [down-mouse-3] 'nih--pop-up-object-menu)
   (define-key map ["mouse-3"] 'nih--pop-up-object-menu)
   (define-key map (kbd "M-RET") 'nih--copy-to-repl)
   (define-key map [nih--copy-to-repl] '(menu-item "Copy to REPL" nih--copy-to-repl))
   map))

(define-button-type 'nih--expand :supertype 'nih
  'keymap (let ((map (make-sparse-keymap)))
            (set-keymap-parent map nih--button-parent-keymap)
            (define-key map [nih--expand] '(menu-item "Expand" nih--expand))
            (define-key map [mouse-2] 'nih--expand)
            (define-key map (kbd "RET")     'nih--expand)
            map))

(define-button-type 'nih--collapse :supertype 'nih
  'keymap (let ((map (make-sparse-keymap)))
            (set-keymap-parent map nih--button-parent-keymap)
            (define-key map [nih--expand] '(menu-item "Collapse" nih--collapse))
            (define-key map [mouse-2] 'nih--collapse)
            (define-key map (kbd "RET")     'nih--collapse)
            map))

(define-button-type 'nih--property-name :supertype 'nih
  'keymap (let ((map (make-sparse-keymap)))
            (set-keymap-parent map nih--button-parent-keymap)
            (define-key map (kbd "e") 'nih--edit-value)
            (define-key map (kbd "RET") 'nih--collapse-expand)
            (define-key map [nih--edit-value] '(menu-item "Edit value" nih--edit-value))
            map))

(define-button-type 'nih--log :supertype 'nih
  'keymap (let ((map (make-sparse-keymap)))
            (define-key map [down-mouse-3] 'nih--pop-up-object-menu)
            (define-key map (kbd "v") 'nih--goto-source)
            (define-key map [nih--goto-source] '(menu-item "Got to source"
                                                           nih--goto-source))
            map))

(defvar nih--really-the-button nil)

(cl-defmacro nih--define-button-action (name args &optional doc &body body)
  (declare (debug
            (&define name sexp cl-declarations-or-string def-body))
           (doc-string 3)
           (indent 2))
  (cl-assert (and (consp args) (= 1 (length args))))
  (unless (stringp doc) (setq body (cons doc body)
                              doc nil))
  (let ((pos (cl-gensym "event-")))
    ;; logic mostly stolen from `push-button'
    `(defun ,name (,pos) ,@(when doc `(,doc))
            (interactive
             (list (if (integerp last-command-event) (point) last-command-event)))
            (let ((fn (lambda ,args ,@body)) (pos ,pos))
              (cond (nih--really-the-button
                     (funcall fn nih--really-the-button))
                    ((and (not (integerp pos)) (eventp pos))
                     (let ((posn (event-start pos)))
                       (with-current-buffer (window-buffer (posn-window posn))
                         (let* ((str (posn-string posn))
                                (str-button (and str (get-text-property
                                                      (cdr str) 'button (car str)))))
                           (if str-button
                               ;; mode-line, header-line, or display string event.
                               (funcall fn str-button)
                             (,name (posn-point posn)))))))
                    (t
                     (let ((button (button-at (or pos (point)))))
                       (if button (funcall fn button)
                         (error "on noes no button at %s" pos)))))))))

(nih--define-button-action nih--expand (button)
  (let ((ro (get-text-property button 'nih--remote-object)))
    (goto-char (button-start button))
    (save-excursion
      (let* ((inhibit-read-only t)
             (col (let ((inhibit-field-text-motion t))
                    (- (point) (point-at-bol 1))))
             (nih--pp-synchronously nil))
        (save-restriction
          (narrow-to-region (point) (point))
          (nih--pp-expanded-from-remote ro)
          (goto-char (point-min))
          (forward-line)
          (while (not (eobp))
            (insert (make-string col ? ))
            (forward-line))
          (add-text-properties
           (point-min) (point-max)
           '(read-only t front-sticky (read-only))))
        (delete-region (button-start button) (button-end button))))))

(nih--define-button-action nih--collapse (button)
  (let ((ro (get-text-property button 'nih--remote-object)))
    (goto-char (button-start button))
    (let ((inhibit-read-only t)
          (nih--pp-synchronously nil))
      (delete-region (point) (get-text-property button 'nih--object-end))
      (save-excursion
        (save-restriction
          (narrow-to-region (point) (point))
          (nih--pp-collapsed ro)
          (add-text-properties
           (point-min) (point-max)
           '(read-only t front-sticky (read-only))))))))

(nih--define-button-action nih--collapse-expand (button)
  (let (butt)
    (save-excursion
      (save-restriction
        (narrow-to-region (point) (line-end-position))
        (setq butt (nih--next-button))))
    (if (eq butt button)
        (nih--error "Can't do that with this property")
      (save-excursion
        (funcall (button-get butt 'action) butt)))))

(nih--define-button-action nih--pop-up-object-menu (button)
  (let ((nih--really-the-button button))
    (popup-menu (button-get button 'keymap))))


;;;; REPL
;;;;
(require 'js)
(require 'comint)

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
                ;; (comint-input-sender-no-newline    . t)
                (indent-line-function              . lisp-indent-line)
                (nih--repl-read-mark               . nil)
                (nih--repl-pending-output          . nil)
                (nih--repl-output-mark             . ,(point-marker))
                (nih--repl-last-prompt-overlay     . ,(make-overlay 0 0 nil nil))
                (mode-line-process                 . nil)
                (show-paren-data-function          . nih--show-paren-data-function)
                (parse-sexp-ignore-comments        . t)
                (syntax-propertize-function        . nih--repl-syntax-propertize)
                (comint-scroll-show-maximum-output . nil)
                (comint-scroll-to-bottom-on-input  . nil)
                (comint-scroll-to-bottom-on-output . nil)
                (inhibit-field-text-motion         . nil)
                (buffer-file-coding-system         . utf-8-unix))
           do (set (make-local-variable var) value))
  (nih-mode)
  (set-marker-insertion-type nih--repl-output-mark nil)
  (add-hook 'kill-emacs-hook 'nih--repl-save-histories)
  (add-hook 'eldoc-documentation-functions 'nih--eldoc-preview-function nil t)
  (add-hook 'completion-at-point-functions 'nih--completion-at-point nil t)
  (setq-local company-backends '(company-capf))
  (setq-local eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
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

(defun nih--filtered-def (def &optional away-from-input-p)
  ;; Stolen from `yas-filtered-definition'.  Thanks Noam!
  `(menu-item "" ,def
              :filter ,(lambda (cmd)
                         (let ((c (< (point) (nih--repl-safe-mark))))
                           (if away-from-input-p (and c cmd) (unless c cmd))))))

(setq nih--repl-mode-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map comint-mode-map)
        (define-key map (kbd "RET")   (nih--filtered-def 'nih-repl-return))
        (define-key map (kbd "TAB")   (nih--filtered-def 'nih--next-button t))
        (define-key map [(tab)]       (nih--filtered-def 'nih--next-button t))
        (define-key map [(shift tab)] (nih--filtered-def 'nih--previous-button t))
        (define-key map [backtab]     (nih--filtered-def 'nih--previous-button t))
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

(defun nih--show-paren-data-function ()
  "Make everything up to current prompt comment syntax."
  (unless (< (point) (nih--repl-safe-mark))
    (with-no-warnings (show-paren--default))))

(defun nih--repl-teardown (&optional reason)
  "Tear down the NIH REPL.
Called in `kill-buffer-hook' or when the NIH connection is closed
for some reason."
  (remove-hook 'kill-buffer-hook 'nih--repl-teardown t)
  (delete-process (nih--repl-process))
  (nih--repl-insert-note (format "Tearing down becasue %s" reason))
  (nih--repl-save-this-buffers-history))

(defun nih--ensure-repl-buffer (conn)
  "Set CONNs REPL to a suitably named buffer."
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
  (nih--repl-commiting-text ()
    (unless (bolp) (comint-output-filter proc "\n"))
    (comint-output-filter proc "JS> ")
    (buffer-disable-undo)
    (buffer-enable-undo)))

(defvar nih--in-repl-debug nil) ;; (setq nih--in-repl-debug t)

(defvar-local nih--repl-object-store-id nil)
(defvar nih--repl-object-store-js)
(setq nih--repl-object-store-js
      "var $_;
       $nih = (function(obj) {
                 var history = [];
                 var recall = function(n) {
                    // console.log('recalling', n);
                    return history[n];
                 };
                 var store = function(obj) {
                    // console.log('storing');
                    history.push(obj);
                    return history.length - 1;
                 };
                 return {recall, store};
              }());")

(defun nih--repl-init-object-store ()
  (cl-destructuring-bind (&key result)
      (jsonrpc-request (nih--current-connection)
                       :Runtime.evaluate
                       `(:expression
                         ,nih--repl-object-store-js
                         :replMode t
                         :generatePreview t)
                       :timeout 40)
    (setq-local nih--repl-object-store-id
                (and result
                     (plist-get result :objectId)))
    (unless nih--repl-object-store-id
      (nih--repl-insert-note "Object store failed to initialize!"))))

(defun nih--repl-store-remote-object (result)
  (nih--dbind ((Result.RemoteObject)
               value objectId unserializableValue)
      result
    (let ((store-result
           (jsonrpc-request
            (nih--current-connection)
            :Runtime.callFunctionOn
            (let ((arg (or `(,@(and objectId `(:objectId ,objectId))
                             ,@(and value `(:value ,value))
                             ,@(and unserializableValue `(:unserializableValue
                                                          ,unserializableValue)))
                           nih--{})))
              (list :functionDeclaration
                    "function(obj) {$_ = obj; return this.store(obj);}"
                    :objectId nih--repl-object-store-id
                    :arguments
                    `[,arg])))))
      (plist-get (plist-get store-result :result)
                 :value))))

(defvar nih-repl-insert-collapsed t
  "Non-nil says to complex evaluation collapsed by default.")

(defun nih--insert-remote-object (obj)
  "Synchronously insert OBJ in REPL ."
  (let ((receipt (nih--repl-store-remote-object obj)))
    (let ((obj (append (list :nih--repl-history-id receipt)
                       obj)))
      (if (and (not nih-repl-insert-collapsed)
               (plist-get obj :objectId))
          (nih--pp-expanded-from-remote obj)
        (nih--pp-collapsed obj)))))

(nih--define-button-action nih--copy-to-repl (button)
  (with-current-buffer
      (nih-repl (nih--current-connection))
    (let* ((input (buffer-substring
                   (nih--repl-safe-mark)
                   (point-max)))
           (inhibit-read-only t)
           (obj (button-get button 'nih--remote-object))
           (desc (plist-get obj :description)))
      (delete-region (nih--repl-safe-mark)
                     (point-max))
      (nih--repl-insert-note (format "Returning '%s'" desc))
      (nih--insert-remote-object obj)
      (nih--repl-insert-prompt (nih--repl-process))
      (insert input))))

(nih--define-button-action nih--edit-value (button)
  (let* ((ro (get-text-property button 'nih--owner-object))
         (pd (get-text-property button 'nih--property-descriptor))
         (inhibit-read-only t)
         (nih--pp-synchronously nil)
         (retval
          (jsonrpc-request
           (nih--current-connection)
           :Runtime.callFunctionOn
           (list :functionDeclaration
                 (format "function() {
                             var aux = (%s);
                             this['%s'] = aux;
                             return aux;
                          }"
                         (read-from-minibuffer "[nih] JS expression: ")
                         (plist-get pd :name))
                 :objectId (plist-get ro :objectId)
                 :arguments [])))
         (new-object (plist-get retval :result)) )
    (cond (new-object
           (save-excursion
             (let ((butt
                    (progn (goto-char button)
                           (search-forward " : " (line-end-position))
                           (button-at (point)))))
               (cond ((null butt)
                      (delete-region (point) (1- (line-end-position))))
                     ((eq 'nih--collapse (button-type butt))
                      (delete-region (button-start butt)
                                     (button-get butt 'nih--object-end)))
                     ((eq 'nih--expand (button-type butt))
                      (delete-region (button-start butt)
                                     (button-end butt)))))
             (nih--pp-collapsed new-object)))
          (t (nih--message "probably failed %s" retval)))))

(defun nih--repl-massaged-input (string)
  (replace-regexp-in-string "@\\[\\([0-9]+\\)\\]" "$nih.recall(\\1)" string))

(defun nih--repl-comint-input ()
  (save-excursion (goto-char (nih--repl-mark))
                  (buffer-substring (point) (field-end))))

(cl-defun nih--repl-input-sender (proc string &aux success)
  "Send STRING to PROC."
  (unless (bolp) (nih--insert "\n"))
  (set-marker nih--repl-output-mark (point))
  (buffer-disable-undo)
  (setq string (nih--repl-massaged-input string))
  (unwind-protect
      (cl-destructuring-bind (&key result exceptionDetails)
          (jsonrpc-request (nih--current-connection)
                           :Runtime.evaluate
                           `(:expression
                             ,(substring-no-properties
                               (string-trim string))
                             :replMode t
                             :generatePreview t)
                           :timeout 40)
        (cond (exceptionDetails
               (nih--dbind ((Result.RemoteObject) text exception)
                   exceptionDetails
                 (when nih--in-repl-debug
                   (nih--repl-insert-note exception))
                 (goto-char (nih--repl-safe-mark))
                 (unless (bolp) (nih--insert "\n"))
                 (comint-output-filter
                  proc
                  (propertize (format "%s %s\n" text (plist-get exception
                                                                :description))
                              'font-lock-face 'font-lock-warning-face))))
              (result
               (when nih--in-repl-debug
                 (nih--repl-insert-note result))
               (goto-char (nih--repl-safe-mark))
               (unless (bolp) (nih--insert "\n"))
               (nih--insert-remote-object result))
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

(defun nih-repl-return ()
  "Send the current JS statement for evaluation."
  (interactive)
  (nih--repl-commiting-text ()
    (comint-send-input t)))

(defface nih--repl-note-face
  `((t (:inherit font-lock-keyword-face)))
  "Face for the REPL notes."
  :group 'nih)

(defun nih--repl-insert-output (things &optional ensure-newline)
  (save-excursion
    (let ((things (cl-etypecase things
                    (string (list things))
                    (cons things)
                    (arrayp (append things nil))))
          (inhibit-read-only t)
          (start (progn (goto-char nih--repl-output-mark)
                        (point)))
          (nih--pp-synchronously nil)
          (nih--pp-prin1 nil))
      (cond ((and ensure-newline
                  (not (bolp)))
             ;; We're not at bolp and we should be.  Insert newline behind.
             (nih--insert "\n"))
            ((eq (get-text-property (point) 'field) 'output)
             ;; We're up against synchronously inserted output, insert a
             ;; newline after us.
             (save-excursion (insert "\n"))))
      (cl-loop for (thing . rest) on things
               if (stringp thing) do (nih--insert thing)
               else do (nih--pp-collapsed thing)
               when rest do (nih--insert " "))
      (add-text-properties start (point)
                           '(read-only t front-sticky (read-only)))
      (set-marker nih--repl-output-mark (point))
      (let ((mark (nih--repl-mark)))
        (when (and mark
                   (< mark nih--repl-output-mark))
          (set-marker mark nih--repl-output-mark))))))

(cl-defun nih--repl-log (conn &rest all &key
                              _source
                              level
                              args
                              _url
                              _line
                              _column
                              _timestamp
                              _stackTrace)
  (nih--when-live-buffer (nih--repl conn)
    (let ((face  (alist-get level '(("warning" . 'warning)
                                    ("error"   . 'error)
                                    ("info"    . nil)
                                    ("verbose" . nil))
                            nil nil #'equal)))
      (cond ((stringp args)
             (setq args (list args)))
            ((arrayp args)
             (setq args (append args nil))))
      (when face
        (setq args (cons (make-text-button (format "%s:" level) nil
                                           'font-lock-face face
                                           'type 'nih--log
                                           'nih--log-data all)
                         args)))
      (nih--repl-insert-output args t))))

(defun nih--repl-insert-note (string &optional face async)
  "Insert a note into the REPL.
If ASYNC, the note is asynchronous, i.e. not it doesn't affect
the process mark."
  (let* ((face (or face 'nih--repl-note-face))
         (string (if (stringp string) string (pp-to-string string)))
         (string (replace-regexp-in-string "^" "// " (string-trim string))))
    (cond (async (nih--repl-insert-output
                  (propertize string 'font-lock-face face)
                  t))
          ((nih--repl-process)
           (nih--repl-commiting-text (when face
                                       `(face ,face font-lock-face ,face))
             ;; insert synchronously
             (comint-output-filter (nih--repl-process)
                                   (format "%s\n" string))
             (set-marker nih--repl-output-mark (nih--repl-mark))))
          (t
           ;; If no process yet/anymore, fall back to the simpler strategy.
           (let ((inhibit-read-only t))
             (goto-char (point-max))
             (unless (bolp) (newline))
             (insert string "\n"))))))

(cl-defun nih-repl-new (conn &optional (interactive t))
  "Create and setup a new REPL buffer for CONN.
CONN defaults to the current NIH connection.  Return the buffer.
INTERACTIVE non-nil pops to it."
  (interactive (list (nih--current-connection) t))
  (let* ((buffer (nih--ensure-repl-buffer conn)))
    (with-current-buffer (if interactive (pop-to-buffer buffer)
                           buffer)
      ;; JT@2020-12-03: We used to save other live REPL histories here
      ;; so that the new REPL will see them.  But that would mess up
      ;; the order of our commands, so we have commented it out.
      ;; (ignore-errors (nih--repl-save-histories))
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
        (nih--repl-init-object-store)
        (nih--repl-read-in-history)
        (nih--repl-insert-note (format "Welcome to %s!"
                                       (jsonrpc-name conn)))
        (nih--repl-insert-prompt proc))
      (current-buffer))))

(defun nih-repl (conn &optional interactive)
  "Ensure REPL for CONN.  If INTERACTIVE, pop to it, else return it."
  (interactive (list (nih--current-connection) t))
  (cl-assert conn "No current connection")
  (let ((repl (nih--repl conn)))
    (if (and repl (buffer-live-p repl))
        (if interactive (pop-to-buffer repl) repl)
      (nih-repl-new conn interactive))))
;; (global-set-key (kbd "C-c C-z") 'nih-repl)


;;;; compilation
;;;;
(defvar-local nih--compiled-script-id nil)
(defvar nih--compilation-buffers
  (make-hash-table :test #'equal))

(defun nih-compile-file ()
  (interactive)
  (let* ((original-buffer (current-buffer))
         (conn (nih--current-connection))
         (contents (save-restriction (widen) (buffer-string)))
         (source buffer-file-name)
         (response))
    (with-current-buffer (generate-new-buffer " *nih-temp-compilation*")
      (insert contents)
      (setq response
            (jsonrpc-request conn :Runtime.compileScript
                       (list :expression
                             (buffer-substring-no-properties
                              (point-min) (point-max))
                             :sourceURL source
                             :persistScript t))
            nih--compiled-script-id
            (plist-get response :scriptId))
      (cond (nih--compiled-script-id
             (puthash nih--compiled-script-id
                      (list :compilation-buffer (current-buffer)
                            :original-buffer original-buffer
                            :sourceURL source)
                      nih--compilation-buffers)
             (nih--message "Compiled")
             (setq response
                   (jsonrpc-request conn :Runtime.runScript
                                    (list :scriptId nih--compiled-script-id)))
             (nih--message "Ran compiled script: %s"
                           (plist-get response :result)))
            (t
             (nih--error "Oops %s" (plist-get response
                                              :exceptionDetails)))))))

(defun nih--line-column-to-point (line column)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line line)
      (unless (eobp) ;; if line was excessive leave point at eob
        (let ((tab-width 1))
          (goto-char
           (min (+ (line-beginning-position) column)
                (line-end-position)))))
      (point))))

(nih--define-button-action nih--goto-source (button)
  (let* ((stack (plist-get (button-get button 'nih--log-data)
                          :stackTrace))
         (frames (plist-get stack :callFrames))
         (topmost (ignore-errors (elt frames 0)))
         snippet)
    (nih--dbind ((Runtime.CallFrame) functionName scriptId
                 lineNumber columnNumber)
        topmost
      (cl-destructuring-bind (&key compilation-buffer
                                   original-buffer
                                   sourceURL)
          (gethash scriptId nih--compilation-buffers)
        (with-current-buffer compilation-buffer
          (goto-char (nih--line-column-to-point lineNumber
                                                columnNumber))
          (setq snippet
                (string-trim
                 (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position)))))
        (with-current-buffer (or (and (buffer-live-p original-buffer)
                                      original-buffer)
                                 (find-file-noselect sourceURL))
          (goto-char (nih--line-column-to-point lineNumber
                                                columnNumber))
          (cond ((or (and (goto-char (line-beginning-position))
                          (search-forward snippet nil t))
                     (and (goto-char (line-end-position))
                          (search-backward snippet nil t)))
                 (save-excursion
                   (pop-to-buffer (current-buffer)))
                 (pulse-momentary-highlight-region
                  (line-beginning-position) (line-end-position))
                 (message "Landed on %S, hopefully" functionName))
                (t
                 (nih--error "Can't find source for %S at %s"
                             functionName
                             topmost))))))))


;;;; eldoc/completion
;;;;
(defun nih--trim-js (string)
  (setq string (string-trim string)
        string (replace-regexp-in-string "[ \n\t]+" " " string)))

(defun nih--eldoc-preview-function (callback)
  "Evaluate whatever is transiently in REPL for a preview of results."
  (when (> (point) (nih--repl-safe-mark))
    (let ((last-expr
           (nih--trim-js (nih--repl-comint-input))))
      (jsonrpc-async-request
       (nih--current-connection)
       :Runtime.evaluate
       `(:expression ,(nih--repl-massaged-input last-expr)
                     :replMode t :throwOnSideEffect t :generatePreview t)
       :success-fn (lambda (result)
                     (let ((ro (plist-get result :result)))
                       (funcall
                        callback
                        (format "%s => %s"
                                (truncate-string-to-width last-expr 30
                                                          nil nil "...")
                                (with-temp-buffer
                                  (nih--pp-collapsed ro t)
                                  (buffer-string))))))
       :error-fn (lambda (_err) (funcall callback nil))
       :timeout-fn (lambda (_err) (funcall callback nil)))))
  t)

(defun nih--backward-expression (&optional interactive)
  (interactive (list t))
  (let ((ppss (syntax-ppss)))
    (cond
     ((nth 3 ppss) (goto-char (1+ (nth 8 ppss))))
     (t (backward-sexp 1 interactive)
        (while (and (looking-back "\\([^ \t\n;]\\|\\.[ \t\n;]*\\)"
                                  (- (point) 300))
                    (ignore-errors (backward-sexp) t)))))))

(defun nih--forward-expression (&optional interactive)
  (interactive (list t))
  (let ((ppss (syntax-ppss)))
    (cond
     ((nth 3 ppss) (goto-char (nth 8 ppss)) (forward-sexp) (backward-char 1))
     (t (forward-sexp 1 interactive)
        (while (and (looking-at "\\([^ \t\n.;]\\|[ \t\n;]*\\.\\w\\)")
                    (ignore-errors (forward-sexp) t)))
        (when (looking-at  "[ \t\n;]*\\.")
          (goto-char (match-end 0)))))))

(cl-defun nih--parse-expression-at-point ()
  (save-excursion
    (unless (looking-at "[ \t\n;]")
      (ignore-errors (nih--forward-expression)))
    (list (progn (ignore-errors (nih--backward-expression)) (point))
          (progn (ignore-errors (nih--forward-expression)) (point))
          (and (search-backward-regexp
                "\\.\\w*"
                (line-beginning-position) t)
               (point)))))

(defun nih--completion-at-point ()
  (cl-destructuring-bind (beg end maybe) (nih--parse-expression-at-point)
    (when maybe
      (list
       (1+ maybe) end
       (completion-table-with-cache
        (lambda (_)
          (pulse-momentary-highlight-region beg maybe 'highlight)
          (nih--dbind ((Result.RemoteObject) objectId)
              (plist-get (jsonrpc-request
                          (nih--current-connection)
                          :Runtime.evaluate
                          `(:expression
                            ,(buffer-substring-no-properties beg maybe)
                            :replMode t :throwOnSideEffect t :generatePreview t)
                          :cancel-on-input non-essential)
                         :result)
            (cl-loop with (properties internal) = (nih--pp-get-remote objectId)
                     for prop in (append properties internal nil)
                     collect (propertize (plist-get prop :name)
                                         'nih--completion
                                         (plist-get prop :value))))))
       :annotation-function (lambda (p)
                              (plist-get (get-text-property 0 'nih--completion p)
                                         :type))
       :company-prefix-length (when maybe t) :company-require-match 'never
       :company-doc-buffer (lambda (p)
                             (with-current-buffer (get-buffer-create "*nih doc*")
                               (erase-buffer)
                               (insert (plist-get
                                        (get-text-property 0 'nih--completion p)
                                        :description))
                               (current-buffer)))))))


;;;; nih-mode
;;;;
(define-minor-mode nih-mode
  "Minor mode for all things NIH."
  nil nil nil)

(define-minor-mode nih-editing-mode
  "Minor mode for editing `js-mode' buffers."
  nil nil nil
  (nih-mode 1))

(defvar nih-editing-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") 'nih-compile-file)
    (define-key map (kbd "C-c M-k") 'nih-compile-file)
    (define-key map (kbd "C-c C-c") 'nih-compile-top-level)
    map))

;;;###autoload
(add-hook 'js-mode-hook 'nih-editing-mode)

(defvar nih--mode-line-format `(:eval (nih--mode-line-format)))

(put 'nih--mode-line-format 'risky-local-variable t)

(defgroup nih nil
  "There are knights who say it.  Also CDP Emacs client."
  :prefix "nih-" :group 'applications)

(defface nih-mode-line
  '((t (:inherit font-lock-constant-face :weight bold)))
  "Face for package-name in NIH's mode line.")

(defun nih-events-buffer (conn)
  "Display events buffer for SERVER.
Use current server's or first available Nih events buffer."
  (interactive (list (nih--current-connection)))
  (let ((buffer (jsonrpc-events-buffer conn)))
    (if buffer (display-buffer buffer)
      (nih--error "Can't find an Nih events buffer!"))))

(defun nih--mode-line-format ()
  "Compose NIH's mode-line."
  (let* ((conn (nih--current-connection))
         (pending (and conn (hash-table-count
                             (jsonrpc--request-continuations conn))))
         (nick (and conn (jsonrpc-name conn))))
    (append
     `(,(nih--mode-line-props "nih" 'nih-mode-line nil))
     (when nick
       `(":" ,(nih--mode-line-props
               nick 'nih-mode-line
               '((mouse-1 nih-repl "go to REPL")
                 (mouse-2 nih-events-buffer "go to events buffer")))
         ,@(when (cl-plusp pending)
         `("/" ,(nih--mode-line-props
                 (format "%d" pending) 'warning
                 '(;; (mouse-3 nih-forget-pending-continuations
                   ;;          "forget pending continuations")
                   )))))))))

(add-to-list 'mode-line-misc-info
             `(nih-mode (" [" nih--mode-line-format "] ")))

(provide 'nih)
;;; nih.el ends here
