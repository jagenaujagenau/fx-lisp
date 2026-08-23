;;;; package.lisp — package definitions for the fx Common Lisp port.

(defpackage #:fx.json
  (:use #:cl)
  (:export #:decode #:encode #:encode-to-string
           #:json-null #:json-false #:json-true-p
           #:jget #:jget* #:jbool
           #:make-jobject #:jput
           #:json-parse-error))

(defpackage #:fx.util
  (:use #:cl)
  (:export #:home-dir #:fx-dir #:expand-path #:getenv
           #:read-file-string #:write-file-string
           #:join-lines #:split-lines #:string-prefix-p #:string-suffix-p
           #:iso8601-now #:random-id
           #:starts-with-any))

(defpackage #:fx.config
  (:use #:cl #:fx.util)
  (:export #:*default-model* #:*gateway-base-url*
           #:settings-path #:load-settings #:save-settings
           #:resolve-model #:resolve-api-key
           #:missing-credential-message))

(defpackage #:fx.prompt
  (:use #:cl)
  (:export #:gateway-system-prompt #:runtime-context))

(defpackage #:fx.permissions
  (:use #:cl)
  (:export #:engine #:make-engine #:make-default-engine
           #:engine-mode #:engine-rules #:engine-grants #:engine-workspace
           #:decide #:tool-target #:add-grant #:grant-pattern #:grant-allows-p
           #:parse-rules #:evaluate-rules #:load-rules
           #:wildcard-match-p #:target-match-p #:static-command-p
           #:permission-name-for-tool #:url-domain
           #:mode-label #:parse-mode #:status-text #:+yolo-warning+))

(defpackage #:fx.tools
  (:use #:cl #:fx.util)
  (:export #:tool-spec #:tool-spec-name #:tool-spec-description
           #:tool-spec-schema #:tool-spec-requires-approval
           #:tool-spec-action-label #:tool-spec-call
           #:builtin-tools #:find-tool #:gateway-tool-definitions
           #:execute-tool #:tool-error
           #:register-tool #:unregister-tool #:*ask-user-hook*))

(defpackage #:fx.gateway
  (:use #:cl #:fx.util)
  (:export #:chat-completion #:gateway-error
           #:list-models
           #:record-usage #:usage-summary #:reset-usage))

(defpackage #:fx.session
  (:use #:cl #:fx.util)
  (:export #:session #:make-new-session #:session-id #:session-path
           #:append-event #:session-messages #:load-session
           #:latest-session-id #:list-sessions))

(defpackage #:fx.compaction
  (:use #:cl)
  (:export #:compact-messages #:group-turns #:summary-message-p
           #:build-summary-text #:continuation-message-text #:compact-line
           #:max-history-turns #:*default-max-history-turns*))

(defpackage #:fx.hooks
  (:use #:cl)
  (:export #:register-pre-tool-use #:register-stop #:register-post-turn-end
           #:register-attention-required #:clear-hooks #:list-hooks
           #:run-pre-tool-use #:run-stop #:run-post-turn-end
           #:run-attention-required #:load-user-hooks))

(defpackage #:fx.modes
  (:use #:cl)
  (:export #:mode-spec #:make-mode-spec #:mode-spec-id #:mode-spec-name
           #:mode-spec-description #:mode-spec-permission-mode
           #:mode-spec-tool-policy #:mode-spec-denial-message
           #:modes #:lookup #:register-mode #:project-tools
           #:tool-allowed-p #:denial-message #:read-only-tool-p
           #:+default-mode-id+))

(defpackage #:fx.skills
  (:use #:cl)
  (:export #:skills #:find-skill #:discover-skills #:static-context
           #:read-skill-chunk #:parse-skill-file #:resolve-metadata
           #:skill #:skill-name #:skill-description #:skill-location
           #:skill-source
           #:install-from-source #:install-from-directory
           #:normalize-install-source #:looks-like-install-command-p
           #:validate-managed-name #:managed-skills-dir
           #:create-skill-template #:remove-managed-skill))

(defpackage #:fx.agent
  (:use #:cl #:fx.util)
  (:export #:run-turn #:*approval-hook* #:*output-stream*
           #:*max-agent-steps* #:*engine* #:*model* #:*api-key* #:*mode*))

(defpackage #:fx.mcp
  (:use #:cl)
  (:export #:load-config #:parse-config #:config-path #:connect #:server-tools
           #:search-tools #:select-tool #:call-dynamic-tool #:features
           #:dynamic-tool-name #:shutdown-connections #:mcp-error
           #:server-config #:server-config-name #:server-config-command
           #:server-config-args #:server-config-enabled))

(defpackage #:fx.websearch
  (:use #:cl)
  (:export #:execute #:format-output #:worker-model
           #:escape-markdown-title #:escape-markdown-url))

(defpackage #:fx.subagent
  (:use #:cl)
  (:export #:handle-command #:reset-children #:*max-depth* #:*depth*
           #:list-children))

(defpackage #:fx.cli
  (:use #:cl #:fx.util)
  (:export #:main #:repl #:ask))

(defpackage #:fx
  (:use #:cl)
  (:export #:main))
