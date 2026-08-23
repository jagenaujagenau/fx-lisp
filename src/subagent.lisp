;;;; subagent.lisp — child sessions, port of core/subagent (scoped subset).
;;;;
;;;; The Zig subsystem is an asynchronous manager for ordinary fx child
;;;; sessions. This port keeps the same command API — one command branch per
;;;; call: create / inspect / message / lifecycle — with children running the
;;;; regular agent loop on SBCL threads. Creation returns an admitted handle
;;;; without waiting; inspect.wait provides the bounded same-turn wait.
;;;; relationship/configure and milestones are not ported.
;;;;
;;;; Child permission modes inherit the caller and cannot exceed it, and
;;;; children have no terminal, so anything the child's engine does not
;;;; auto-allow is denied rather than prompted.

(in-package #:fx.subagent)

(defparameter *max-depth* 3
  "Maximum nesting depth of child sessions.")

(defvar *depth* 0
  "Current subagent depth; children run with parent depth + 1.")

(defparameter +settled-statuses+ '(:idle :completed :failed :cancelled :closed))
(defparameter +max-inspect-wait-ms+ 600000)

(defstruct child
  id name mode status thread mutex waitqueue
  (queue '()) (messages '()) (generation 0)
  model api-key effort permission-mode engine depth
  failure (output (make-string-output-stream)))

(defvar *children* (make-hash-table :test #'equal))
(defvar *registry-mutex* (sb-thread:make-mutex :name "fx-subagent-registry"))
(defvar *counter* 0)

(defun reset-children ()
  (sb-thread:with-mutex (*registry-mutex*)
    (maphash (lambda (id child)
               (declare (ignore id))
               (let ((thread (child-thread child)))
                 (when (and thread (sb-thread:thread-alive-p thread))
                   (ignore-errors (sb-thread:terminate-thread thread)))))
             *children*)
    (clrhash *children*)
    (setf *counter* 0)))

(defun list-children ()
  (sb-thread:with-mutex (*registry-mutex*)
    (loop for child being the hash-values of *children* collect child)))

(defun %find-child (id)
  (sb-thread:with-mutex (*registry-mutex*)
    (gethash id *children*)))

(defun %fail (fmt &rest args)
  (error 'fx.tools:tool-error
         :message (apply #'format nil fmt args)))

(defun %status-name (status)
  (string-downcase (symbol-name status)))

;;; ----------------------------------------------------------- child worker

(defun %run-child-prompt (child prompt)
  (handler-case
      (multiple-value-bind (text messages)
          (let ((fx.agent:*output-stream* (child-output child))
                (fx.agent:*engine* (child-engine child))
                (fx.agent:*approval-hook*
                  (lambda (name label target)
                    (declare (ignore name label target))
                    nil))
                (*depth* (child-depth child)))
            (fx.agent:run-turn :session nil
                               :model (child-model child)
                               :api-key (child-api-key child)
                               :user-input prompt
                               :messages (child-messages child)))
        (declare (ignore text))
        (sb-thread:with-mutex ((child-mutex child))
          (setf (child-messages child) messages
                (child-failure child) nil)
          (incf (child-generation child)))
        t)
    (error (e)
      (sb-thread:with-mutex ((child-mutex child))
        (setf (child-failure child) (princ-to-string e)))
      nil)))

(defun %worker (child)
  (loop
    (let ((prompt nil))
      (sb-thread:with-mutex ((child-mutex child))
        (loop until (or (child-queue child)
                        (member (child-status child) '(:closed :cancelled)))
              do (setf (child-status child)
                       (if (eq (child-mode child) :one-off)
                           (child-status child)
                           :idle))
                 (sb-thread:condition-broadcast (child-waitqueue child))
                 (when (eq (child-mode child) :one-off)
                   ;; A one_off child with an empty queue is finished.
                   (return-from %worker))
                 (sb-thread:condition-wait (child-waitqueue child)
                                           (child-mutex child)))
        (when (member (child-status child) '(:closed :cancelled))
          (sb-thread:condition-broadcast (child-waitqueue child))
          (return-from %worker))
        (setf prompt (pop (child-queue child))
              (child-status child) :running))
      (let ((ok (%run-child-prompt child prompt)))
        (sb-thread:with-mutex ((child-mutex child))
          (unless (member (child-status child) '(:closed :cancelled))
            (setf (child-status child)
                  (cond ((not ok) :failed)
                        ((eq (child-mode child) :one-off) :completed)
                        (t :idle))))
          (sb-thread:condition-broadcast (child-waitqueue child))
          (when (eq (child-mode child) :one-off)
            (return-from %worker)))))))

;;; ----------------------------------------------------------------- create

(defun %mode-rank (mode)
  (ecase mode (:ask 0) (:auto 1) (:yolo 2)))

(defun %capped-permission-mode (requested parent-mode)
  (let ((requested (or (and requested (fx.permissions:parse-mode requested))
                       parent-mode)))
    (if (> (%mode-rank requested) (%mode-rank parent-mode))
        parent-mode
        requested)))

(defun %create (spec)
  (let ((name (fx.json:jget spec "name"))
        (mode-string (fx.json:jget spec "mode"))
        (prompt (fx.json:jget spec "prompt")))
    (unless (and (stringp name) (plusp (length name)))
      (%fail "create requires a name"))
    (unless (member mode-string '("one_off" "persistent") :test #'equal)
      (%fail "create mode must be one_off or persistent"))
    (when (>= *depth* *max-depth*)
      (%fail "subagent depth limit reached (~d)" *max-depth*))
    (let ((mode (if (equal mode-string "one_off") :one-off :persistent)))
      (when (and (eq mode :one-off)
                 (not (and (stringp prompt) (plusp (length prompt)))))
        (%fail "a one_off child requires a prompt"))
      (let* ((parent-engine (or fx.agent:*engine*
                                (fx.permissions:make-default-engine)))
             (permission-mode (%capped-permission-mode
                               (fx.json:jget spec "permission_mode")
                               (fx.permissions:engine-mode parent-engine)))
             (id (sb-thread:with-mutex (*registry-mutex*)
                   (format nil "sub-~d" (incf *counter*))))
             (child (make-child
                     :id id :name name :mode mode :status :created
                     :mutex (sb-thread:make-mutex :name id)
                     :waitqueue (sb-thread:make-waitqueue :name id)
                     :queue (if (stringp prompt) (list prompt) '())
                     :model (or (fx.json:jget spec "model") fx.agent:*model*)
                     :api-key fx.agent:*api-key*
                     :effort (fx.json:jget spec "effort")
                     :permission-mode permission-mode
                     :engine (fx.permissions:make-engine
                              :mode permission-mode
                              :rules (fx.permissions:load-rules)
                              :workspace (fx.permissions:engine-workspace
                                          parent-engine))
                     :depth (1+ *depth*))))
        (unless (child-api-key child)
          (%fail "no gateway credential available for the child session"))
        (setf (child-thread child)
              (sb-thread:make-thread (lambda () (%worker child))
                                     :name (format nil "fx-subagent-~a" id)))
        (sb-thread:with-mutex (*registry-mutex*)
          (setf (gethash id *children*) child))
        (fx.json:encode-to-string
         (fx.json:make-jobject "id" id "name" name "mode" mode-string
                               "status" "admitted"))))))

;;; ---------------------------------------------------------------- inspect

(defun %wait-settled (child wait)
  "Bounded wait until the child settles. Returns :settled or :wait_timed_out."
  (let* ((timeout-ms (fx.json:jget wait "timeout_ms"))
         (after-generation (fx.json:jget wait "after_generation")))
    (unless (equal (fx.json:jget wait "until") "settled")
      (%fail "inspect.wait until must be \"settled\""))
    (unless (and (integerp timeout-ms) (plusp timeout-ms))
      (%fail "inspect.wait requires timeout_ms"))
    (let ((deadline (+ (get-internal-real-time)
                       (* (min timeout-ms +max-inspect-wait-ms+)
                          (/ internal-time-units-per-second 1000)))))
      (sb-thread:with-mutex ((child-mutex child))
        (loop
          (when (and (member (child-status child) +settled-statuses+)
                     (or (not (integerp after-generation))
                         (> (child-generation child) after-generation)))
            (return :settled))
          (let ((remaining (/ (- deadline (get-internal-real-time))
                              internal-time-units-per-second)))
            (when (<= remaining 0) (return :wait_timed_out))
            (sb-thread:condition-wait (child-waitqueue child)
                                      (child-mutex child)
                                      :timeout (min remaining 1))))))))

(defun %message-preview (message limit)
  (let* ((role (fx.json:jget message "role"))
         (content (fx.json:jget message "content"))
         (calls (fx.json:jget message "tool_calls"))
         (text (cond ((stringp content) content)
                     (calls (format nil "(~d tool call~:p)" (length calls)))
                     (t ""))))
    (format nil "~a: ~a" role (fx.compaction:compact-line text limit))))

(defun %inspect (spec)
  (let* ((id (or (fx.json:jget spec "id") (%fail "inspect requires an id")))
         (child (or (%find-child id) (%fail "unknown subagent id: ~a" id)))
         (sections (or (fx.json:jget spec "sections")
                       (%fail "inspect requires sections")))
         (limit (or (fx.json:jget spec "limit") 5))
         (wait (fx.json:jget spec "wait"))
         (wait-status (when wait
                        (unless (member "status" sections :test #'equal)
                          (%fail "inspect.wait requires the status section"))
                        (%wait-settled child wait)))
         (result (fx.json:make-jobject "id" id)))
    (sb-thread:with-mutex ((child-mutex child))
      (dolist (section sections)
        (cond
          ((equal section "status")
           (fx.json:jput result "status"
                         (fx.json:make-jobject
                          "name" (child-name child)
                          "mode" (if (eq (child-mode child) :one-off)
                                     "one_off" "persistent")
                          "state" (%status-name (child-status child))
                          "generation" (child-generation child)
                          "queued" (length (child-queue child))
                          "failure" (or (child-failure child) :null))))
          ((equal section "messages")
           (fx.json:jput result "messages"
                         (let ((messages (child-messages child)))
                           (mapcar (lambda (m) (%message-preview m 200))
                                   (last messages (min limit
                                                       (length messages)))))))
          ((equal section "configuration")
           (fx.json:jput result "configuration"
                         (fx.json:make-jobject
                          "model" (or (child-model child) :null)
                          "permission_mode" (fx.permissions:mode-label
                                             (child-permission-mode child))
                          "effort" (or (child-effort child) :null))))
          (t
           (fx.json:jput result section "(section not ported)")))))
    (when wait-status
      (fx.json:jput result "wait" (string-downcase (symbol-name wait-status))))
    (fx.json:encode-to-string result)))

;;; ------------------------------------------------------- message/lifecycle

(defun %message (spec)
  (let ((send (fx.json:jget spec "send")))
    (when (fx.json:jget spec "milestone")
      (%fail "milestones are not ported in this build"))
    (unless send (%fail "message requires send"))
    (let* ((id (or (fx.json:jget send "id") (%fail "message.send requires an id")))
           (content (or (fx.json:jget send "content")
                        (%fail "message.send requires content")))
           (child (or (%find-child id) (%fail "unknown subagent id: ~a" id))))
      (unless (eq (child-mode child) :persistent)
        (%fail "message.send targets persistent children; a one_off child takes only its create prompt"))
      (sb-thread:with-mutex ((child-mutex child))
        (when (member (child-status child) '(:closed :cancelled))
          (%fail "child ~a is ~a" id (%status-name (child-status child))))
        (setf (child-queue child)
              (append (child-queue child) (list content)))
        (when (eq (child-status child) :failed)
          (setf (child-status child) :idle))
        (sb-thread:condition-broadcast (child-waitqueue child))
        (fx.json:encode-to-string
         (fx.json:make-jobject "id" id "status" "queued"
                               "queued" (length (child-queue child))))))))

(defun %lifecycle (spec)
  (let* ((id (or (fx.json:jget spec "id") (%fail "lifecycle requires an id")))
         (action (or (fx.json:jget spec "action") (%fail "lifecycle requires an action")))
         (child (or (%find-child id) (%fail "unknown subagent id: ~a" id))))
    (unless (member action '("cancel" "close") :test #'equal)
      (%fail "lifecycle action ~a is not ported (cancel and close are)" action))
    (let ((target (if (equal action "cancel") :cancelled :closed))
          (thread (child-thread child)))
      (sb-thread:with-mutex ((child-mutex child))
        (let ((running (eq (child-status child) :running)))
          (setf (child-status child) target
                (child-queue child) '())
          (sb-thread:condition-broadcast (child-waitqueue child))
          (when (and running thread (sb-thread:thread-alive-p thread))
            ;; A running turn cannot be asked to stop; terminate it.
            (ignore-errors (sb-thread:terminate-thread thread)))))
      (fx.json:encode-to-string
       (fx.json:make-jobject "id" id "status" (%status-name target))))))

;;; ---------------------------------------------------------------- dispatch

(defun handle-command (command)
  (unless (hash-table-p command)
    (%fail "subagent requires a command object"))
  (let ((branches (loop for key in '("create" "inspect" "message"
                                     "relationship" "configure" "lifecycle")
                        when (fx.json:jget command key)
                          collect key)))
    (unless (= 1 (length branches))
      (%fail "select exactly one command branch"))
    (let ((branch (first branches)))
      (cond
        ((equal branch "create") (%create (fx.json:jget command "create")))
        ((equal branch "inspect") (%inspect (fx.json:jget command "inspect")))
        ((equal branch "message") (%message (fx.json:jget command "message")))
        ((equal branch "lifecycle") (%lifecycle (fx.json:jget command "lifecycle")))
        (t (%fail "~a is not ported in this build" branch))))))

;;; ---------------------------------------------------------- tool registry

(fx.tools::define-tool "subagent"
    (:description "Create, inspect, message, or control ordinary fx child sessions through one asynchronous manager API. When to use: delegate independent work, inspect an explicit child, send ordinary content, or change an authorized child. Select exactly one command branch; creation returns an admitted child handle without waiting for completion. When NOT to use: ordinary local work, implicit child discovery, or multiple operations in one call. Inspect only explicit child IDs and requested bounded sections. When the current turn requires the child's settled result, use inspect.wait instead of terminal.exec, shell sleep, or repeated polling. The messages section returns recent committed child conversation; failed status includes the latest retained failure reason. Ordinary content must use message.send."
     :schema (fx.tools::schema
              :properties
              `(("command" "type" "object" "description"
                 "Exactly one branch: create {name, mode (one_off|persistent), prompt, model, permission_mode (ask|auto|yolo, capped at the caller's mode)}; inspect {id, sections [status|messages|configuration], limit, wait {until: \"settled\", timeout_ms, after_generation}}; message {send {id, content}}; lifecycle {id, action (cancel|close)}."))
              :required '("command"))
     :requires-approval nil
     :action-label "Managing")
    (args)
  (handle-command (fx.json:jget args "command")))
