;;;; fx.asd — ASDF system definition for the Common Lisp port of fx.
;;;;
;;;; fx is a tiny coding agent harness and CLI, originally written in Zig
;;;; (see tmp/fx). This system is a functional port of its core: config,
;;;; auth, gateway client, builtin tools, agent loop, sessions, and CLI.

(asdf:defsystem "fx"
  :description "Tiny, open, embeddable, native coding agent — Common Lisp port."
  :author "fx contributors"
  :license "Apache-2.0"
  :version "0.1.0"
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "json")
                             (:file "util")
                             (:file "config")
                             (:file "prompt")
                             (:file "permissions")
                             (:file "tools")
                             (:file "gateway")
                             (:file "session")
                             (:file "compaction")
                             (:file "hooks")
                             (:file "modes")
                             (:file "skills")
                             (:file "agent")
                             (:file "websearch")
                             (:file "mcp")
                             (:file "subagent")
                             (:file "cli")))))

(asdf:defsystem "fx/tests"
  :description "Tests for the fx Common Lisp port."
  :depends-on ("fx")
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "run-tests")))))
