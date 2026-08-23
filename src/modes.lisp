;;;; modes.lisp — agent modes, port of core/modes + builtins/modes.zig.
;;;;
;;;; A mode couples a permission mode with a tool policy. The built-ins are
;;;; fx's exact registry: "code" (auto, full tools) and "ask" (ask, full
;;;; tools), default "ask". A read_only tool policy restricts both the
;;;; advertised tool projection and execution, using a name list like the
;;;; Zig ToolSet's read_only_tool_names. A built-in "plan" read-only mode
;;;; demonstrates the policy (the Zig registry keeps only code/ask, but the
;;;; contract and denial path are the same).

(in-package #:fx.modes)

(defstruct mode-spec
  id name description
  (permission-mode :ask)
  (tool-policy :full)
  denial-message)

(defparameter +default-mode-id+ "ask")

(defparameter +read-only-tool-names+
  '("list_files" "glob_files" "grep_files" "read_file" "file_info"
    "skill" "semantic_search" "web_fetch" "web_search"
    "mcp_search_tools" "mcp_select_tool" "mcp_features")
  "Tools a read_only policy may advertise and execute.")

(defvar *modes*
  (list (make-mode-spec :id "code" :name "Code"
                        :description "Write and modify code with full tool access"
                        :permission-mode :auto)
        (make-mode-spec :id "ask" :name "Ask"
                        :description "Request permission before making any changes"
                        :permission-mode :ask)
        (make-mode-spec :id "plan" :name "Plan"
                        :description "Read-only inspection and planning; no mutations"
                        :permission-mode :ask
                        :tool-policy :read-only)))

(defun modes () *modes*)

(defun lookup (id)
  (find id *modes* :key #'mode-spec-id :test #'string=))

(defun register-mode (spec)
  (setf *modes* (append (remove (mode-spec-id spec) *modes*
                                :key #'mode-spec-id :test #'string=)
                        (list spec)))
  spec)

(defun read-only-tool-p (tool-name)
  (member tool-name +read-only-tool-names+ :test #'string=))

(defun tool-allowed-p (mode-id tool-name)
  "Port of Registry.toolAllowed: unknown modes and unknown tools pass."
  (let ((mode (lookup mode-id)))
    (or (null mode)
        (eq (mode-spec-tool-policy mode) :full)
        (and (read-only-tool-p tool-name) t))))

(defun denial-message (mode-id tool-name)
  (let ((mode (lookup mode-id)))
    (or (and mode (mode-spec-denial-message mode))
        (format nil "tool ~a is not available in ~a mode (read-only tool policy)"
                tool-name (or (and mode (mode-spec-name mode)) mode-id)))))

(defun project-tools (definitions mode-id)
  "Filter gateway tool definitions by the mode's tool policy
(port of buildGatewayToolProjection)."
  (let ((mode (lookup mode-id)))
    (if (or (null mode) (eq (mode-spec-tool-policy mode) :full))
        definitions
        (remove-if-not
         (lambda (definition)
           (read-only-tool-p (fx.json:jget* definition "function" "name")))
         definitions))))
