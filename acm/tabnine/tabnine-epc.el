;;; -*- lexical-binding: t; -*-
;;; epc.el --- A RPC stack for the Emacs Lisp

;; Copyright (C) 2011, 2012, 2013  Masashi Sakurai

;; Author: SAKURAI Masashi <m.sakurai at kiwanami.net>
;; Version: 0.1.1
;; Keywords: lisp, rpc
;; URL: https://github.com/kiwanami/emacs-epc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This program is an asynchronous RPC stack for Emacs.  Using this
;; RPC stack, the Emacs can communicate with the peer process.
;; Because the protocol is S-expression encoding and consists of
;; asynchronous communications, the RPC response is fairly good.
;;
;; Current implementations for the EPC are followings:
;; - epcs.el : Emacs Lisp implementation
;; - RPC::EPC::Service : Perl implementation

;;; Code:

;; Copy from lsp-bridge and emacs-epc

(require 'cl-lib)

;; deferred
(defmacro tabnine-epc-deferred-chain (&rest elements)
  "Anaphoric function chain macro for deferred chains."
  (declare (debug (&rest form))
           (indent 0))
  `(let (it)
     ,@(cl-loop for i in elements
                collect
                `(setq it ,i))
     it))

;; Debug
(defvar tabnine-epc-deferred-debug nil
  "Debug output switch.")

(defvar tabnine-epc-deferred-debug-count 0
  "[internal] Debug output counter.")

(defun tabnine-epc-deferred-log (&rest args)
  "[internal] Debug log function."
  (when tabnine-epc-deferred-debug
    (with-current-buffer (get-buffer-create "*tabnine-epc-deferred-log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "%5i %s\n\n\n" tabnine-epc-deferred-debug-count (apply #'format args)))))
    (cl-incf tabnine-epc-deferred-debug-count)))

(defvar tabnine-epc-deferred-debug-on-signal nil
  "If non nil, the value `debug-on-signal' is substituted this
value in the `condition-case' form in deferred
implementations. Then, Emacs debugger can catch an error occurred
in the asynchronous tasks.")

(defmacro tabnine-epc-deferred-condition-case (var protected-form &rest handlers)
  "[internal] Custom condition-case. See the comment for
`tabnine-epc-deferred-debug-on-signal'."
  (declare (debug condition-case)
           (indent 1))
  `(let ((debug-on-signal
          (or debug-on-signal tabnine-epc-deferred-debug-on-signal)))
     (condition-case ,var
         ,protected-form
       ,@handlers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back end functions of deferred tasks

(defvar tabnine-epc-deferred-tick-time 0.001
  "Waiting time between asynchronous tasks (second).
The shorter waiting time increases the load of Emacs. The end
user can tune this parameter. However, applications should not
modify it because the applications run on various environments.")

(defvar tabnine-epc-deferred-queue nil
  "[internal] The execution queue of deferred objects.
See the functions `tabnine-epc-deferred-post-task' and `tabnine-epc-deferred-worker'.")

(defun tabnine-epc-deferred-post-task (d which &optional arg)
  "[internal] Add a deferred object to the execution queue
`tabnine-epc-deferred-queue' and schedule to execute.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (let ((pack `(,d ,which . ,arg)))
    (push pack tabnine-epc-deferred-queue)
    (tabnine-epc-deferred-log "QUEUE-POST [%s]: %s" (length tabnine-epc-deferred-queue) pack)
    (run-at-time tabnine-epc-deferred-tick-time nil 'tabnine-epc-deferred-worker)
    d))

(defun tabnine-epc-deferred-worker ()
  "[internal] Consume a deferred task.
Mainly this function is called by timer asynchronously."
  (when tabnine-epc-deferred-queue
    (let* ((pack (car (last tabnine-epc-deferred-queue)))
           (d (car pack))
           (which (cadr pack))
           (arg (cddr pack)) value)
      (setq tabnine-epc-deferred-queue (nbutlast tabnine-epc-deferred-queue))
      (condition-case err
          (setq value (tabnine-epc-deferred-exec-task d which arg))
        (error
         (tabnine-epc-deferred-log "ERROR : %s" err)
         (message "deferred error : %s" err)))
      value)))

;; Struct: tabnine-epc-deferred-object
;;
;; callback    : a callback function (default `identity')
;; errorback   : an errorback function (default `tabnine-epc-deferred-resignal')
;; cancel      : a canceling function (default `tabnine-epc-deferred-default-cancel')
;; next        : a next chained deferred object (default nil)
;; status      : if 'ok or 'ng, this deferred has a result (error) value. (default nil)
;; value       : saved value (default nil)
;;
(cl-defstruct tabnine-epc-deferred-object
  (callback 'identity)
  (errorback 'tabnine-epc-deferred-resignal)
  (cancel 'tabnine-epc-deferred-default-cancel)
  next status value)

(defun tabnine-epc-deferred-resignal (err)
  "[internal] Safely resignal ERR as an Emacs condition.

If ERR is a cons (ERROR-SYMBOL . DATA) where ERROR-SYMBOL has an
`error-conditions' property, it is re-signaled unchanged. If ERR
is a string, it is signaled as a generic error using `error'.
Otherwise, ERR is formatted into a string as if by `print' before
raising with `error'."
  (cond ((and (listp err)
              (symbolp (car err))
              (get (car err) 'error-conditions))
         (signal (car err) (cdr err)))
        ((stringp err)
         (error "%s" err))
        (t
         (error "%S" err))))

(defun tabnine-epc-deferred-default-cancel (d)
  "[internal] Default canceling function."
  (tabnine-epc-deferred-log "CANCEL : %s" d)
  (setf (tabnine-epc-deferred-object-callback d) 'identity)
  (setf (tabnine-epc-deferred-object-errorback d) 'tabnine-epc-deferred-resignal)
  (setf (tabnine-epc-deferred-object-next d) nil)
  d)

(defun tabnine-epc-deferred-exec-task (d which &optional arg)
  "[internal] Executing deferred task. If the deferred object has
next deferred task or the return value is a deferred object, this
function adds the task to the execution queue.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (tabnine-epc-deferred-log "EXEC : %s / %s / %s" d which arg)
  (when (null d) (error "tabnine-epc-deferred-exec-task was given a nil."))
  (let ((callback (if (eq which 'ok)
                      (tabnine-epc-deferred-object-callback d)
                    (tabnine-epc-deferred-object-errorback d)))
        (next-deferred (tabnine-epc-deferred-object-next d)))
    (cond
     (callback
      (tabnine-epc-deferred-condition-case err
        (let ((value (funcall callback arg)))
          (cond
           ((tabnine-epc-deferred-object-p value)
            (tabnine-epc-deferred-log "WAIT NEST : %s" value)
            (if next-deferred
                (tabnine-epc-deferred-set-next value next-deferred)
              value))
           (t
            (if next-deferred
                (tabnine-epc-deferred-post-task next-deferred 'ok value)
              (setf (tabnine-epc-deferred-object-status d) 'ok)
              (setf (tabnine-epc-deferred-object-value d) value)
              value))))
        (error
         (cond
          (next-deferred
           (tabnine-epc-deferred-post-task next-deferred 'ng err))
          (t
           (tabnine-epc-deferred-log "ERROR : %S" err)
           (message "deferred error : %S" err)
           (setf (tabnine-epc-deferred-object-status d) 'ng)
           (setf (tabnine-epc-deferred-object-value d) err)
           err)))))
     (t                                 ; <= (null callback)
      (cond
       (next-deferred
        (tabnine-epc-deferred-exec-task next-deferred which arg))
       ((eq which 'ok) arg)
       (t                               ; (eq which 'ng)
        (tabnine-epc-deferred-resignal arg)))))))

(defun tabnine-epc-deferred-set-next (prev next)
  "[internal] Connect deferred objects."
  (setf (tabnine-epc-deferred-object-next prev) next)
  (cond
   ((eq 'ok (tabnine-epc-deferred-object-status prev))
    (setf (tabnine-epc-deferred-object-status prev) nil)
    (let ((ret (tabnine-epc-deferred-exec-task
                next 'ok (tabnine-epc-deferred-object-value prev))))
      (if (tabnine-epc-deferred-object-p ret) ret
        next)))
   ((eq 'ng (tabnine-epc-deferred-object-status prev))
    (setf (tabnine-epc-deferred-object-status prev) nil)
    (let ((ret (tabnine-epc-deferred-exec-task next 'ng (tabnine-epc-deferred-object-value prev))))
      (if (tabnine-epc-deferred-object-p ret) ret
        next)))
   (t
    next)))

(defun tabnine-epc-deferred-new (&optional callback)
  "Create a deferred object."
  (if callback
      (make-tabnine-epc-deferred-object :callback callback)
    (make-tabnine-epc-deferred-object)))

(defun tabnine-epc-deferred-callback (d &optional arg)
  "Start deferred chain with a callback message."
  (tabnine-epc-deferred-exec-task d 'ok arg))

(defun tabnine-epc-deferred-errorback (d &optional arg)
  "Start deferred chain with an errorback message."
  (declare (indent 1))
  (tabnine-epc-deferred-exec-task d 'ng arg))

(defun tabnine-epc-deferred-callback-post (d &optional arg)
  "Add the deferred object to the execution queue."
  (declare (indent 1))
  (tabnine-epc-deferred-post-task d 'ok arg))

(defun tabnine-epc-deferred-next (&optional callback arg)
  "Create a deferred object and schedule executing. This function
is a short cut of following code:
 (tabnine-epc-deferred-callback-post (tabnine-epc-deferred-new callback))."
  (let ((d (if callback
               (make-tabnine-epc-deferred-object :callback callback)
             (make-tabnine-epc-deferred-object))))
    (tabnine-epc-deferred-callback-post d arg)
    d))

(defun tabnine-epc-deferred-nextc (d callback)
  "Create a deferred object with OK callback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-tabnine-epc-deferred-object :callback callback)))
    (tabnine-epc-deferred-set-next d nd)))

(defun tabnine-epc-deferred-error (d callback)
  "Create a deferred object with errorback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-tabnine-epc-deferred-object :errorback callback)))
    (tabnine-epc-deferred-set-next d nd)))

;;==================================================
;; Utility

(defvar tabnine-epc:debug-out nil)
(defvar tabnine-epc:debug-buffer "*epc log*")

(defun tabnine-epc:log-init ()
  (when (get-buffer tabnine-epc:debug-buffer)
    (kill-buffer tabnine-epc:debug-buffer)))

(defun tabnine-epc:log (&rest args)
  (when tabnine-epc:debug-out
    (with-current-buffer
        (get-buffer-create tabnine-epc:debug-buffer)
      (buffer-disable-undo)
      (goto-char (point-max))
      (insert (apply 'format args) "\n"))))

(defun tabnine-epc:make-procbuf (name)
  "[internal] Make a process buffer."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (set (make-local-variable 'kill-buffer-query-functions) nil)
      (erase-buffer) (buffer-disable-undo))
    buf))

;;==================================================
;; Low Level Interface

(defvar tabnine-epc:uid 1)

(defun tabnine-epc:uid ()
  (cl-incf tabnine-epc:uid))

(defvar tabnine-epc:accept-process-timeout 150  "Asynchronous timeout time. (msec)")
(defvar tabnine-epc:accept-process-timeout-count 100 " Startup function waits (`tabnine-epc:accept-process-timeout' * `tabnine-epc:accept-process-timeout-count') msec for the external process getting ready.")

(put 'epc-error 'error-conditions '(error epc-error))
(put 'epc-error 'error-message "EPC Error")

(cl-defstruct tabnine-epc:connection
  "Set of information for network connection and event handling.

name    : Connection name. This name is used for process and buffer names.
process : Connection process object.
buffer  : Working buffer for the incoming data.
channel : Event channels for incoming messages."
  name process buffer channel)

(defun tabnine-epc:connect (host port)
  "[internal] Connect the server, initialize the process and
return tabnine-epc:connection object."
  (tabnine-epc:log ">> Connection start: %s:%s" host port)
  (let* ((connection-id (tabnine-epc:uid))
         (connection-name (format "epc con %s" connection-id))
         (connection-buf (tabnine-epc:make-procbuf (format "*%s*" connection-name)))
         (connection-process
          (open-network-stream connection-name connection-buf host port))
         (channel (list connection-name nil))
         (connection (make-tabnine-epc:connection
                      :name connection-name
                      :process connection-process
                      :buffer connection-buf
                      :channel channel)))
    (tabnine-epc:log ">> Connection establish")
    (set-process-coding-system  connection-process 'binary 'binary)
    (set-process-filter connection-process
                        (lambda (p m)
                          (tabnine-epc:process-filter connection p m)))
    (set-process-sentinel connection-process
                          (lambda (p e)
                            (tabnine-epc:process-sentinel connection p e)))
    (set-process-query-on-exit-flag connection-process nil)
    connection))

(defun tabnine-epc:process-sentinel (connection process msg)
  (tabnine-epc:log "!! Process Sentinel [%s] : %S : %S"
                   (tabnine-epc:connection-name connection) process msg)
  (tabnine-epc:disconnect connection))

(defun tabnine-epc:net-send (connection sexp)
  (let* ((msg (encode-coding-string
               (concat (tabnine-epc:prin1-to-string sexp) "\n") 'utf-8-unix))
         (string (concat (format "%06x" (length msg)) msg))
         (proc (tabnine-epc:connection-process connection)))
    (tabnine-epc:log ">> SEND : [%S]" string)
    (process-send-string proc string)))

(defun tabnine-epc:disconnect (connection)
  (let
      ((process (tabnine-epc:connection-process connection))
       (buf (tabnine-epc:connection-buffer connection))
       (name (tabnine-epc:connection-name connection)))
    (tabnine-epc:log "!! Disconnect [%s]" name)
    (when process
      (set-process-sentinel process nil)
      (delete-process process)
      (when (get-buffer buf) (kill-buffer buf)))
    (tabnine-epc:log "!! Disconnected finished [%s]" name)))

(defun tabnine-epc:process-filter (connection process message)
  (tabnine-epc:log "INCOMING: [%s] [%S]" (tabnine-epc:connection-name connection) message)
  (with-current-buffer (tabnine-epc:connection-buffer connection)
    (goto-char (point-max))
    (insert message)
    (tabnine-epc:process-available-input connection process)))

(defun tabnine-epc-signal-connect (channel event-sym &optional callback)
  "Append an observer for EVENT-SYM of CHANNEL and return a deferred object.
If EVENT-SYM is `t', the observer receives all signals of the channel.
If CALLBACK function is given, the deferred object executes the
CALLBACK function asynchronously. One can connect subsequent
tasks to the returned deferred object."
  (let ((d (if callback
               (tabnine-epc-deferred-new callback)
             (tabnine-epc-deferred-new))))
    (push (cons event-sym d)
          (cddr channel))
    d))

(defun tabnine-epc-signal-send (channel event-sym &rest args)
  "Send a signal to CHANNEL. If ARGS values are given,
observers can get the values by following code:

  (lambda (event)
    (destructuring-bind
     (event-sym (args))
     event ... ))
"
  (let ((observers (cddr channel))
        (event (list event-sym args)))
    (cl-loop for i in observers
             for name = (car i)
             for d = (cdr i)
             if (or (eq event-sym name) (eq t name))
             do (tabnine-epc-deferred-callback-post d event))))

(defun tabnine-epc:process-available-input (connection process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (tabnine-epc:net-have-input-p)
      (let ((event (tabnine-epc:net-read-or-lose process))
            (ok nil))
        (tabnine-epc:log "<< RECV [%S]" event)
        (unwind-protect
            (condition-case err
                (progn
                  (apply 'tabnine-epc-signal-send
                         (cons (tabnine-epc:connection-channel connection) event))
                  (setq ok t))
              ('error (tabnine-epc:log "MsgError: %S / <= %S" err event)))
          (unless ok
            (tabnine-epc:process-available-input connection process)))))))

(defun tabnine-epc:net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (tabnine-epc:net-decode-length))))

(defun tabnine-epc:net-read-or-lose (_process)
  (condition-case error
      (tabnine-epc:net-read)
    (error
     (debug 'error error)
     (error "net-read error: %S" error))))

(defun tabnine-epc:net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (tabnine-epc:net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length)))
    (cl-assert (cl-plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (decode-coding-string
                    (buffer-string) 'utf-8-unix)))
      (delete-region (point-min) end))))

(defun tabnine-epc:net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun tabnine-epc:prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))

;;==================================================
;; High Level Interface

(cl-defstruct tabnine-epc:manager
  "Root object that holds all information related to an EPC activity.

`tabnine-epc:start-epc' returns this object.

title          : instance name for displaying on the `tabnine-epc:controller' UI
server-process : process object for the peer
commands       : a list of (prog . args)
port           : port number
connection     : tabnine-epc:connection instance
methods        : alist of method (name . function)
sessions       : alist of session (id . deferred)
exit-hook      : functions for after shutdown EPC connection"
  title server-process commands port connection methods sessions exit-hooks)

(cl-defstruct tabnine-epc:method
  "Object to hold serving method information.

name       : method name (symbol)   ex: 'test
task       : method function (function with one argument)
arg-specs  : arg-specs (one string) ex: \"(A B C D)\"
docstring  : docstring (one string) ex: \"A test function. Return sum of A,B,C and D\"
"
  name task docstring arg-specs)

(defvar tabnine-epc:live-connections nil
  "[internal] A list of `tabnine-epc:manager' objects those currently connect to the epc peer.
This variable is for debug purpose.")

(defun tabnine-epc:live-connections-add (mngr)
  "[internal] Add the EPC manager object."
  (push mngr tabnine-epc:live-connections))

(defun tabnine-epc:live-connections-delete (mngr)
  "[internal] Remove the EPC manager object."
  (setq tabnine-epc:live-connections (delete mngr tabnine-epc:live-connections)))


(defun tabnine-epc:start-epc (server-prog server-args)
  "Start the epc server program and return an tabnine-epc:manager object.

Start server program SERVER-PROG with command line arguments
SERVER-ARGS.  The server program must print out the port it is
using at the first line of its stdout.  If the server prints out
non-numeric value in the first line or does not print out the
port number in three seconds, it is regarded as start-up
failure."
  (let ((mngr (tabnine-epc:start-server server-prog server-args)))
    (tabnine-epc:init-epc-layer mngr)
    mngr))

(defun tabnine-epc:server-process-name (uid)
  (format "tabnine-epc:server:%s" uid))

(defun tabnine-epc:server-buffer-name (uid)
  (format " *%s*" (tabnine-epc:server-process-name uid)))

(defun tabnine-epc:start-server (server-prog server-args)
  "[internal] Start a peer server and return an tabnine-epc:manager instance which is set up partially."
  (let* ((uid (tabnine-epc:uid))
         (process-name (tabnine-epc:server-process-name uid))
         (process-buffer (get-buffer-create (tabnine-epc:server-buffer-name uid)))
         (process (apply 'start-process
                         process-name process-buffer
                         server-prog server-args))
         (cont 1) port)
    (while cont
      (accept-process-output process 0 tabnine-epc:accept-process-timeout t)
      (let ((port-str (with-current-buffer process-buffer
                        (buffer-string))))
        (cond
         ((string-match "^[ \n\r]*[0-9]+[ \n\r]*$" port-str)
          (setq port (string-to-number port-str)
                cont nil))
         ((< 0 (length port-str))
          (error "Server may raise an error. \
Use \"M-x tabnine-epc:pop-to-last-server-process-buffer RET\" \
to see full traceback:\n%s" port-str))
         ((not (eq 'run (process-status process)))
          (setq cont nil))
         (t
          (cl-incf cont)
          (when (< tabnine-epc:accept-process-timeout-count cont) ; timeout 15 seconds
            (error "Timeout server response."))))))
    (set-process-query-on-exit-flag process nil)
    (make-tabnine-epc:manager :server-process process
                              :commands (cons server-prog server-args)
                              :title (mapconcat 'identity (cons server-prog server-args) " ")
                              :port port
                              :connection (tabnine-epc:connect "localhost" port))))

(defun tabnine-epc:stop-epc (mngr)
  "Disconnect the connection for the server."
  (let* ((proc (tabnine-epc:manager-server-process mngr))
         (buf (and proc (process-buffer proc))))
    (tabnine-epc:disconnect (tabnine-epc:manager-connection mngr))
    (when proc
      (accept-process-output proc 0 tabnine-epc:accept-process-timeout t))
    (when (and proc (equal 'run (process-status proc)))
      (kill-process proc))
    (when buf  (kill-buffer buf))
    (tabnine-epc:live-connections-delete mngr)))

(defun tabnine-epc:start-epc-debug (port)
  "[internal] Return an tabnine-epc:manager instance which is set up partially."
  (tabnine-epc:init-epc-layer
   (make-tabnine-epc:manager :server-process nil
                             :commands (cons "[DEBUG]" nil)
                             :port port
                             :connection (tabnine-epc:connect "localhost" port))))

(defun tabnine-epc:args (args)
  "[internal] If ARGS is an atom, return it. If list, return the cadr of it."
  (cond
   ((atom args) args)
   (t (cadr args))))

(defun tabnine-epc:init-epc-layer (mngr)
  "[internal] Connect to the server program and return an tabnine-epc:connection instance."
  (let*
      ((mngr mngr)
       (conn (tabnine-epc:manager-connection mngr))
       (channel (tabnine-epc:connection-channel conn)))
    ;; dispatch incoming messages with the lexical scope
    (cl-loop for (method . body) in
             `((call
                . (lambda (args)
                    (tabnine-epc:log "SIG CALL: %S" args)
                    (apply 'tabnine-epc:handler-called-method ,mngr (tabnine-epc:args args))))
               (cl-return
                . (lambda (args)
                    (tabnine-epc:log "SIG RET: %S" args)
                    (apply 'tabnine-epc:handler-return ,mngr (tabnine-epc:args args))))
               (return-error
                . (lambda (args)
                    (tabnine-epc:log "SIG RET-ERROR: %S" args)
                    (apply 'tabnine-epc:handler-return-error ,mngr (tabnine-epc:args args))))
               (epc-error
                . (lambda (args)
                    (tabnine-epc:log "SIG EPC-ERROR: %S" args)
                    (apply 'tabnine-epc:handler-epc-error ,mngr (tabnine-epc:args args))))
               (methods
                . (lambda (args)
                    (tabnine-epc:log "SIG METHODS: %S" args)
                    (tabnine-epc:handler-methods ,mngr (caadr args))))
               ) do
             (tabnine-epc-signal-connect channel method body))
    (tabnine-epc:live-connections-add mngr)
    mngr))

(defun tabnine-epc:manager-status-server-process (mngr)
  "[internal] Return the status of the process object for the peer process. If the process is nil, return nil."
  (and mngr
       (tabnine-epc:manager-server-process mngr)
       (process-status (tabnine-epc:manager-server-process mngr))))

(defun tabnine-epc:manager-status-connection-process (mngr)
  "[internal] Return the status of the process object for the connection process."
  (and (tabnine-epc:manager-connection mngr)
       (process-status (tabnine-epc:connection-process
                        (tabnine-epc:manager-connection mngr)))))

(defun tabnine-epc:manager-send (mngr method &rest messages)
  "[internal] low-level message sending."
  (let* ((conn (tabnine-epc:manager-connection mngr)))
    (tabnine-epc:net-send conn (cons method messages))))

(defun tabnine-epc:manager-get-method (mngr method-name)
  "[internal] Return a method object. If not found, return nil."
  (cl-loop for i in (tabnine-epc:manager-methods mngr)
           if (eq method-name (tabnine-epc:method-name i))
           do (cl-return i)))

(defun tabnine-epc:handler-methods (mngr uid)
  "[internal] Return a list of information for registered methods."
  (let ((info
         (cl-loop for i in (tabnine-epc:manager-methods mngr)
                  collect
                  (list
                   (tabnine-epc:method-name i)
                   (or (tabnine-epc:method-arg-specs i) "")
                   (or (tabnine-epc:method-docstring i) "")))))
    (tabnine-epc:manager-send mngr 'return uid info)))

(defun tabnine-epc:handler-called-method (mngr uid name args)
  "[internal] low-level message handler for peer's calling."
  (let ((mngr mngr) (uid uid))
    (let* ((method (tabnine-epc:manager-get-method mngr name)))
      (cond
       ((null method)
        (tabnine-epc:log "ERR: No such method : %s" name)
        (tabnine-epc:manager-send mngr 'epc-error uid (format "EPC-ERROR: No such method : %s" name)))
       (t
        (condition-case err
            (let* ((f (tabnine-epc:method-task method))
                   (ret (apply f args)))
              (cond
               ((tabnine-epc-deferred-object-p ret)
                (tabnine-epc-deferred-nextc ret
                                (lambda (xx) (tabnine-epc:manager-send mngr 'return uid xx))))
               (t (tabnine-epc:manager-send mngr 'return uid ret))))
          (error
           (tabnine-epc:log "ERROR : %S" err)
           (tabnine-epc:manager-send mngr 'return-error uid err))))))))

(defun tabnine-epc:manager-remove-session (mngr uid)
  "[internal] Remove a session from the epc manager object."
  (cl-loop with ret = nil
           for pair in (tabnine-epc:manager-sessions mngr)
           unless (eq uid (car pair))
           do (push pair ret)
           finally
           do (setf (tabnine-epc:manager-sessions mngr) ret)))

(defun tabnine-epc:handler-return (mngr uid args)
  "[internal] low-level message handler for normal returns."
  (let ((pair (assq uid (tabnine-epc:manager-sessions mngr))))
    (cond
     (pair
      (tabnine-epc:log "RET: id:%s [%S]" uid args)
      (tabnine-epc:manager-remove-session mngr uid)
      (tabnine-epc-deferred-callback (cdr pair) args))
     (t ; error
      (tabnine-epc:log "RET: NOT FOUND: id:%s [%S]" uid args)))))

(defun tabnine-epc:handler-return-error (mngr uid args)
  "[internal] low-level message handler for application errors."
  (let ((pair (assq uid (tabnine-epc:manager-sessions mngr))))
    (cond
     (pair
      (tabnine-epc:log "RET-ERR: id:%s [%S]" uid args)
      (tabnine-epc:manager-remove-session mngr uid)
      (tabnine-epc-deferred-errorback (cdr pair) (format "%S" args)))
     (t ; error
      (tabnine-epc:log "RET-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun tabnine-epc:handler-epc-error (mngr uid args)
  "[internal] low-level message handler for epc errors."
  (let ((pair (assq uid (tabnine-epc:manager-sessions mngr))))
    (cond
     (pair
      (tabnine-epc:log "RET-EPC-ERR: id:%s [%S]" uid args)
      (tabnine-epc:manager-remove-session mngr uid)
      (tabnine-epc-deferred-errorback (cdr pair) (list 'epc-error args)))
     (t ; error
      (tabnine-epc:log "RET-EPC-ERR: NOT FOUND: id:%s [%S]" uid args)))))



(defun tabnine-epc:call-deferred (mngr method-name args)
  "Call peer's method with args asynchronously. Return a deferred
object which is called with the result."
  (let ((uid (tabnine-epc:uid))
        (sessions (tabnine-epc:manager-sessions mngr))
        (d (tabnine-epc-deferred-new)))
    (push (cons uid d) sessions)
    (setf (tabnine-epc:manager-sessions mngr) sessions)
    (tabnine-epc:manager-send mngr 'call uid method-name args)
    d))

(defun tabnine-epc:define-method (mngr method-name task &optional arg-specs docstring)
  "Define a method and return a deferred object which is called by the peer."
  (let* ((method (make-tabnine-epc:method
                  :name method-name :task task
                  :arg-specs arg-specs :docstring docstring))
         (methods (cons method (tabnine-epc:manager-methods mngr))))
    (setf (tabnine-epc:manager-methods mngr) methods)
    method))


(defun tabnine-epc:sync (mngr d)
  "Wrap deferred methods with synchronous waiting, and return the result.
If an exception is occurred, this function throws the error."
  (let ((result 'tabnine-epc:nothing))
    (tabnine-epc-deferred-chain
     d
                (tabnine-epc-deferred-nextc it
                                (lambda (x) (setq result x)))
                (tabnine-epc-deferred-error it
                                (lambda (er) (setq result (cons 'error er)))))
    (while (eq result 'tabnine-epc:nothing)
      (save-current-buffer
        (accept-process-output
         (tabnine-epc:connection-process (tabnine-epc:manager-connection mngr))
         0 tabnine-epc:accept-process-timeout t)))
    (if (and (consp result) (eq 'error (car result)))
        (error (cdr result)) result)))

(defun tabnine-epc:call-sync (mngr method-name args)
  "Call peer's method with args synchronously and return the result.
If an exception is occurred, this function throws the error."
  (tabnine-epc:sync mngr (tabnine-epc:call-deferred mngr method-name args)))

(defun tabnine-epc:live-p (mngr)
  "Return non-nil when MNGR is an EPC manager object with a live
connection."
  (let ((proc (ignore-errors
                (tabnine-epc:connection-process (tabnine-epc:manager-connection mngr)))))
    (and (processp proc)
         ;; Same as `process-live-p' in Emacs >= 24:
         (memq (process-status proc) '(run open listen connect stop)))))


(provide 'tabnine-epc)
;;; epc.el ends here
