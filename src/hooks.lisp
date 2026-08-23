;;;; hooks.lisp — lifecycle hooks, port of core/hooks.
;;;;
;;;; fx hooks are an in-process handler registry, not shell commands. The
;;;; four kinds and their contracts are ported from definitions.zig:
;;;;   PreToolUse        — before local tool validation, permissions, and
;;;;                       execution; a handler returns :continue,
;;;;                       (:rewrite-arguments <json-object>) or
;;;;                       (:block <reason>)
;;;;   Stop              — after a terminal assistant candidate before turn
;;;;                       finalization; :allow or (:continue-once <context>)
;;;;                       (honored at most once per turn)
;;;;   PostTurnEnd       — observes terminal turns; side effects only
;;;;   AttentionRequired — observes a foreground turn waiting on the user
;;;;                       (kind :permission or :question)
;;;;
;;;; Where the Zig version wires first-party providers (Herdr,
;;;; notifications), this port loads user-supplied Lisp providers from
;;;; ~/.fx/hooks.lisp and the workspace's .fx/hooks.lisp.

(in-package #:fx.hooks)

(defstruct handler name function)

(defvar *pre-tool-use* '())
(defvar *stop* '())
(defvar *post-turn-end* '())
(defvar *attention-required* '())

(defun clear-hooks ()
  (setf *pre-tool-use* '() *stop* '()
        *post-turn-end* '() *attention-required* '()))

(defun %register (list-symbol name function)
  (set list-symbol
       (append (remove name (symbol-value list-symbol)
                       :key #'handler-name :test #'equal)
               (list (make-handler :name name :function function))))
  name)

(defun register-pre-tool-use (name function)
  "FUNCTION is called with a plist (:tool-name :arguments :step-index) and
returns :continue, (:rewrite-arguments <decoded-json-object>), or
(:block <reason-string>)."
  (%register '*pre-tool-use* name function))

(defun register-stop (name function)
  "FUNCTION is called with (:assistant-text :step-index :can-continue) and
returns :allow or (:continue-once <context-string>)."
  (%register '*stop* name function))

(defun register-post-turn-end (name function)
  "FUNCTION is called with (:final-text :steps); return value is ignored."
  (%register '*post-turn-end* name function))

(defun register-attention-required (name function)
  "FUNCTION is called with (:kind :tool-name :label); return value ignored."
  (%register '*attention-required* name function))

(defun list-hooks ()
  (loop for (kind list) in `(("PreToolUse" ,*pre-tool-use*)
                             ("Stop" ,*stop*)
                             ("PostTurnEnd" ,*post-turn-end*)
                             ("AttentionRequired" ,*attention-required*))
        append (loop for handler in list
                     collect (list kind (handler-name handler)))))

;;; ---------------------------------------------------------------- running

(defun run-pre-tool-use (tool-name arguments step-index)
  "Run PreToolUse handlers in order. A rewrite feeds later handlers; a
block short-circuits. Returns (values outcome payload):
:unchanged / :rewritten <args> / :blocked <reason>. Handler errors are
treated as :continue."
  (let ((current arguments)
        (rewritten nil))
    (dolist (handler *pre-tool-use*)
      (let ((action (handler-case
                        (funcall (handler-function handler)
                                 (list :tool-name tool-name
                                       :arguments current
                                       :step-index step-index))
                      (error () :continue))))
        (cond
          ((and (consp action) (eq (first action) :block))
           (return-from run-pre-tool-use
             (values :blocked (or (second action) "blocked by hook"))))
          ((and (consp action) (eq (first action) :rewrite-arguments))
           (setf current (second action)
                 rewritten t))
          (t nil))))
    (if rewritten
        (values :rewritten current)
        (values :unchanged arguments))))

(defun run-stop (assistant-text step-index can-continue)
  "Run Stop handlers; the first :continue-once wins when CAN-CONTINUE.
Returns (values :allow) or (values :continue-once context)."
  (dolist (handler *stop* (values :allow nil))
    (let ((action (handler-case
                      (funcall (handler-function handler)
                               (list :assistant-text assistant-text
                                     :step-index step-index
                                     :can-continue can-continue))
                    (error () :allow))))
      (when (and can-continue (consp action)
                 (eq (first action) :continue-once)
                 (stringp (second action))
                 (plusp (length (second action))))
        (return (values :continue-once (second action)))))))

(defun run-post-turn-end (final-text steps)
  (dolist (handler *post-turn-end*)
    (handler-case
        (funcall (handler-function handler)
                 (list :final-text final-text :steps steps))
      (error () nil))))

(defun run-attention-required (kind tool-name label)
  (dolist (handler *attention-required*)
    (handler-case
        (funcall (handler-function handler)
                 (list :kind kind :tool-name tool-name :label label))
      (error () nil))))

;;; ------------------------------------------------------------ user hooks

(defun load-user-hooks (&key (workspace (namestring *default-pathname-defaults*)))
  "Load hook providers from ~/.fx/hooks.lisp then <workspace>/.fx/hooks.lisp.
Returns the list of files loaded; load errors are reported, not fatal."
  (let ((loaded '()))
    (dolist (path (list (merge-pathnames "hooks.lisp" (fx.util:fx-dir))
                        (fx.util:expand-path ".fx/hooks.lisp" workspace)))
      (when (probe-file path)
        (handler-case
            (progn (load path) (push (namestring path) loaded))
          (error (e)
            (format *error-output* "~&hooks: could not load ~a: ~a~%"
                    path e)))))
    (nreverse loaded)))
