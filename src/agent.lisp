;;;; agent.lisp — the agent loop, port of core/agent/runtime/orchestrator.zig.
;;;;
;;;; One turn: append the user message, then repeatedly call the gateway,
;;;; execute any requested tool calls (with approval for mutating tools),
;;;; and feed results back until the model stops calling tools.

(in-package #:fx.agent)

(defparameter *max-agent-steps* 40
  "Safety cap on gateway round-trips per user turn.")

(defvar *output-stream* *standard-output*)

(defparameter *history-byte-guard* 262144
  "When the serialized history grows past this, force an aggressive
compaction (mirrors the Zig budget trim + forceCompaction backstop).")

(defun %history-bytes (messages)
  (loop for message in messages
        sum (let ((content (fx.json:jget message "content")))
              (if (stringp content) (length content) 64))))

(defun %maybe-compact (messages session)
  "Auto-compact prior history before a new turn; returns the message list."
  (multiple-value-bind (compacted compacted-p removed)
      (fx.compaction:compact-messages messages)
    (when (and (not compacted-p)
               (> (%history-bytes messages) *history-byte-guard*))
      (multiple-value-setq (compacted compacted-p removed)
        (fx.compaction:compact-messages messages :force t)))
    (when compacted-p
      (format *output-stream* "~&(compacted ~d older turn~:p)~%" removed)
      (force-output *output-stream*)
      (when session
        (fx.session:append-event
         session (fx.json:make-jobject "kind" "compaction"
                                       "removed_turns" removed))))
    compacted))

(defvar *approval-hook*
  (lambda (tool-name label target)
    (declare (ignore tool-name label target))
    :once)
  "Called with (tool-name label target) when the permission engine says :ask.
Return :once, :always, or NIL/:deny. The CLI installs an interactive prompt.")

(defvar *engine* nil
  "The fx.permissions engine for the current run; NIL falls back to a
fresh default (ask-mode) engine.")

(defvar *model* nil
  "Model id of the in-flight run-turn, visible to tools (subagent create).")

(defvar *api-key* nil
  "API key of the in-flight run-turn, visible to tools (subagent create).")

(defvar *mode* fx.modes:+default-mode-id+
  "Active mode id; couples a permission mode with a tool policy.")

(defun %current-engine ()
  (or *engine* (setf *engine* (fx.permissions:make-default-engine))))

(defun %tool-label (spec args)
  (format nil "~a ~a"
          (fx.tools:tool-spec-action-label spec)
          (or (fx.json:jget args "path")
              (fx.json:jget args "pattern")
              (fx.json:jget args "command")
              (fx.json:jget args "url")
              (fx.json:jget args "action")
              "")))

(defun %assistant-message (content tool-calls)
  (fx.json:make-jobject
   "role" "assistant"
   "content" (if (plusp (length content)) content :null)
   "tool_calls"
   (if tool-calls
       (loop for tc in tool-calls
             collect (fx.json:make-jobject
                      "id" (getf tc :id)
                      "type" "function"
                      "function" (fx.json:make-jobject
                                  "name" (getf tc :name)
                                  "arguments" (getf tc :arguments))))
       :omit)))

(defun %run-tool-call (tc step-index)
  "Execute one tool call plist; return the tool-result message."
  (let* ((name (getf tc :name))
         (spec (fx.tools:find-tool name))
         (result
           (block run
             (handler-case
                 (let ((args (if (plusp (length (getf tc :arguments)))
                                 (fx.json:decode (getf tc :arguments))
                                 (fx.json:make-jobject))))
                   (unless spec
                     (return-from run (format nil "error: unknown tool ~a" name)))
                   (unless (fx.modes:tool-allowed-p *mode* name)
                     (return-from run
                       (format nil "error: ~a"
                               (fx.modes:denial-message *mode* name))))
                   (multiple-value-bind (hook-outcome payload)
                       (fx.hooks:run-pre-tool-use name args step-index)
                     (case hook-outcome
                       (:blocked
                        (return-from run
                          (format nil "error: blocked by PreToolUse hook: ~a"
                                  payload)))
                       (:rewritten (setf args payload))
                       (t nil)))
                   (let ((label (%tool-label spec args))
                         (engine (%current-engine))
                         (target (fx.permissions:tool-target name args)))
                     (format *output-stream* "~&[~a]~%" label)
                     (force-output *output-stream*)
                     (ecase (fx.permissions:decide
                             engine name target
                             (fx.tools:tool-spec-requires-approval spec))
                       (:deny
                        (return-from run "error: blocked by permission policy"))
                       (:allow)
                       (:ask
                        (fx.hooks:run-attention-required :permission name label)
                        (case (funcall *approval-hook* name label target)
                          (:once)
                          (:always (fx.permissions:add-grant engine name target))
                          (t (return-from run "error: the user denied permission for this action"))))))
                   (fx.tools:execute-tool name args))
               (fx.tools:tool-error (e)
                 (format nil "error: ~a" e))
               (fx.json:json-parse-error ()
                 "error: tool arguments were not valid JSON")
               (error (e)
                 (format nil "error: ~a" e))))))
    (fx.json:make-jobject
     "role" "tool"
     "tool_call_id" (getf tc :id)
     "content" result)))

(defun run-turn (&key session model api-key user-input messages)
  "Run one user turn. MESSAGES is the prior chat history (list of decoded
JSON messages, no system message). Returns (values final-text messages)."
  (let* ((*model* model)
         (*api-key* api-key)
         (system-message
           (fx.json:make-jobject
            "role" "system"
            "content" (let ((skills-section (fx.skills:static-context)))
                        (format nil "~a~%~%~a~@[~%~%~a~]"
                                (fx.prompt:gateway-system-prompt)
                                (fx.prompt:runtime-context)
                                skills-section))))
         (user-message (fx.json:make-jobject "role" "user" "content" user-input))
         (history (append (%maybe-compact messages session)
                          (list user-message)))
         (final-text "")
         (stop-continuation-used nil))
    (when session
      (fx.session:append-event
       session (fx.json:make-jobject "kind" "message" "message" user-message)))
    (loop for step from 1 to *max-agent-steps*
          do (multiple-value-bind (content tool-calls finish-reason usage)
                 (fx.gateway:chat-completion
                  :api-key api-key
                  :model model
                  :messages (cons system-message history)
                  :tools (fx.modes:project-tools
                          (fx.tools:gateway-tool-definitions) *mode*)
                  :on-delta (lambda (text)
                              (write-string text *output-stream*)
                              (force-output *output-stream*)))
               (declare (ignorable finish-reason))
               (when (and session usage)
                 (fx.session:append-event
                  session (fx.json:make-jobject
                           "kind" "usage" "model" model
                           "prompt_tokens" (or (fx.json:jget usage "prompt_tokens") 0)
                           "completion_tokens" (or (fx.json:jget usage "completion_tokens") 0))))
               (let ((assistant (%assistant-message content tool-calls)))
                 (setf history (append history (list assistant)))
                 (when session
                   (fx.session:append-event
                    session (fx.json:make-jobject "kind" "message"
                                                  "message" assistant))))
               (when (plusp (length content))
                 (setf final-text content)
                 (fresh-line *output-stream*))
               (if tool-calls
                   (dolist (tc tool-calls)
                     (let ((result (%run-tool-call tc step)))
                       (setf history (append history (list result)))
                       (when session
                         (fx.session:append-event
                          session (fx.json:make-jobject "kind" "message"
                                                        "message" result)))))
                   ;; Terminal assistant candidate: Stop hooks may request
                   ;; one synthetic continuation before finalization.
                   (multiple-value-bind (stop-action context)
                       (fx.hooks:run-stop content step
                                          (not stop-continuation-used))
                     (if (eq stop-action :continue-once)
                         (let ((continuation
                                 (fx.json:make-jobject "role" "user"
                                                       "content" context)))
                           (setf stop-continuation-used t
                                 history (append history (list continuation)))
                           (when session
                             (fx.session:append-event
                              session (fx.json:make-jobject
                                       "kind" "message"
                                       "message" continuation))))
                         (return)))))
          finally (format *output-stream*
                          "~&(stopped after ~d agent steps)~%" *max-agent-steps*))
    (fx.hooks:run-post-turn-end final-text *max-agent-steps*)
    (values final-text history)))
