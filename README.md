<div align="center">

# fx-lisp

**A Common Lisp port of [fx](https://github.com/vercel-labs/fx) — the tiny, open, embeddable coding agent — from ~550 files of Zig to ~4,600 lines of dependency-free SBCL.**

![Common Lisp](https://img.shields.io/badge/Common%20Lisp-SBCL-9B4993?style=for-the-badge&logo=commonlisp&logoColor=white)
![Dependencies](https://img.shields.io/badge/dependencies-none-2ea44f?style=for-the-badge)
![Tests](https://img.shields.io/badge/offline%20checks-112%20passing-2ea44f?style=for-the-badge)

</div>

## What is this?

A functional port of fx's headless agent core: the byte-for-byte system prompt, the full builtin tool set, the streaming Vercel AI Gateway client, permissions, context compaction, skills, MCP, subagents, hooks, modes, session persistence, and the CLI/REPL. No Quicklisp, no external systems — the only runtime requirements are SBCL and `curl` (used as the HTTPS transport).

Where practical the port is verbatim: tool descriptions and JSON schemas, constants and caps, output framing, and permission semantics all match the Zig source. Coverage details are in [What's ported](#whats-ported).

## Quick Start

```bash
brew install sbcl                # the only build dependency

export AI_GATEWAY_API_KEY=...    # or: bin/fx setup
bin/fx                           # interactive REPL
```

Other entry points:

```bash
bin/fx ask "explain build.zig"   # one-shot turn (FX_AUTO_APPROVE=1 to allow mutations)
bin/fx resume [id]               # resume a stored session (default: latest)
bin/fx models                    # list gateway models
bin/fx sessions                  # list stored sessions
```

Inside the REPL: `/help` `/model [id]` `/models` `/mode [id]` `/permissions [ask|auto|yolo]` `/skills` `/mcp` `/hooks` `/status` `/new` `/resume [id]` `/usage` `/compact` `/sessions` `/version` `/exit`.

## Configuration

State mirrors the Zig version, under `~/.fx/`:

| Path | Purpose |
|---|---|
| `~/.fx/settings.json` | model, `permission_mode`, `permission` rules, `max_history_turns`, `web_search_worker_model` |
| `~/.fx/sessions/*.jsonl` | session event log, with a `latest` pointer |
| `~/.fx/api-key.json` | stored gateway key (`bin/fx setup`) |
| `~/.fx/mcp.json` | MCP servers (fx schema, stdio transport) |
| `~/.fx/skills/` + workspace skill roots | discovered skills |
| `~/.fx/hooks.lisp` + workspace `.fx/hooks.lisp` | user hook providers |

Credentials resolve in fx's order: `VERCEL_OIDC_TOKEN` → `AI_GATEWAY_API_KEY` → stored key. `FX_GATEWAY_BASE_URL`, `FX_MODEL`, and `FX_WEB_SEARCH_MODEL` override the gateway, model, and search worker.

## Project Structure

```
fx-lisp/
├── bin/
│   └── fx                  # sh launcher (sbcl --eval chain)
├── src/
│   ├── agent.lisp          # agent loop, approvals, hooks wiring
│   ├── cli.lisp            # command surface + REPL
│   ├── compaction.lisp     # deterministic history compaction
│   ├── config.lisp         # settings + credential resolution
│   ├── gateway.lisp        # streaming SSE client, usage ledger
│   ├── hooks.lisp          # lifecycle hook registry
│   ├── json.lisp           # JSON codec
│   ├── mcp.lisp            # stdio MCP client
│   ├── modes.lisp          # code/ask/plan mode registry
│   ├── package.lisp        # package definitions
│   ├── permissions.lisp    # rules, modes, session grants
│   ├── prompt.lisp         # system prompt (verbatim) + runtime context
│   ├── session.lisp        # JSONL session store
│   ├── skills.lisp         # discovery, catalog, skill + install_skill
│   ├── subagent.lisp       # threaded child-session manager
│   ├── tools.lisp          # builtin tool set
│   ├── util.lisp           # paths, files, strings
│   └── websearch.lisp      # gateway worker web search
├── tests/
│   └── run-tests.lisp      # 112 offline checks
├── README.md
└── fx.asd                  # ASDF systems fx and fx/tests
```

Each file maps to a Zig subsystem:

| File | Ports |
|---|---|
| `src/json.lisp` | JSON codec (objects → hash-tables, `:false`/`:null` sentinels) |
| `src/config.lisp` | `core/config` settings + `core/auth/credentials.zig` source order |
| `src/prompt.lisp` | `builtins/context.zig` system prompt, verbatim, + runtime context |
| `src/permissions.lisp` | `core/permissions` — modes, rules, session grants |
| `src/tools.lisp` | `builtins/tools.zig` + `src/tools/filesystem/*` + `src/tools/web` fetch |
| `src/gateway.lisp` | `gateway/client.zig` — streaming SSE client via a curl subprocess |
| `src/session.lisp` | `core/session` JSONL store with a `latest` pointer |
| `src/compaction.lisp` | `core/session` history compaction (summary turns, `/compact`) |
| `src/hooks.lisp` | `core/hooks` lifecycle hook registry (4 kinds) |
| `src/modes.lisp` | `core/modes` + `builtins/modes.zig` mode registry, tool policy |
| `src/skills.lisp` | `core/skills` + `builtins/skills.zig` discovery, catalog, installer |
| `src/websearch.lisp` | `tools/web/search.zig` + gateway worker search backend |
| `src/mcp.lisp` | `core/mcp` + `builtins/mcp.zig` stdio client, search/select, features |
| `src/agent.lisp` | `core/agent/runtime/orchestrator.zig` agent loop + approvals |
| `src/subagent.lisp` | `core/subagent` child-session manager (threaded subset) |
| `src/cli.lisp` | `core/cli` command surface and REPL |

## What's ported

At a glance: the system prompt (all six sections, byte-for-byte), 17 builtin tools with their original descriptions and schemas, the permission engine, context compaction, threaded subagents, skills with a full installer, gateway-worker web search, stdio MCP, in-process hooks, modes with the read-only tool policy, session resume, and per-model usage accounting.

<details>
<summary><strong>Tools</strong> — the full builtin set with verbatim specs</summary>

`list_files`, `glob_files`, `grep_files`, `read_file`, `write_file`, `edit_file`, `delete_file`, `rename_file`, `copy_file`, `create_folder`, `file_info`, `memory`, `terminal` (the exec-only `terminalExecOnlySpec` variant), `web_fetch`, `web_search`, `semantic_search`, and `ask_user_question`.

- `semantic_search` is fx's lexical concept-keyword ranking: the stop-word list, per-line scoring plus basename bonus, sample lines, ignored-dir list, walk/result caps, and the exact `[search]` output format.
- `web_fetch` enforces `url_policy.zig` (http/https only, length cap, no credentials, private/loopback hosts rejected), pins redirect protocols in curl, and runs a reduced `html_to_markdown` conversion.
- `ask_user_question` uses fx's schema (1–4 multiple-choice questions, 2–6 options each); the REPL prompts with numbered options, and without an interactive user the tool fails with fx's surface-the-blocker guidance. The auto-mode `permission_request_id` branch is not ported.
</details>

<details>
<summary><strong>Permissions</strong> — ask/auto/yolo, rules, session grants</summary>

Rules load from `settings.json` `"permission"` in the same shape as the Zig parser: an action string, or `{"bash": {"git status": "allow", "git push*": "deny"}}`. The last matching rule wins, bash allow-wildcards only match static commands, and deny rules override yolo. Session grants come from the `[a]lways` approval answer — exact command for bash, domain for web_fetch, workspace tree for paths. `/permissions` shows state; `/permissions ask|auto|yolo` persists the mode.
</details>

<details>
<summary><strong>Gateway, sessions, and usage</strong></summary>

Same base URL, auth headers, and credential precedence as fx; the transport is the gateway's OpenAI-compatible `/v1/chat/completions` (SSE streaming with tool-call fragment assembly) instead of the private v3 protocol. Sessions persist as JSONL with a latest pointer; `fx resume [id]` and `/resume` restore full history including tool calls, and `/model` persists the model choice. Every gateway request — agent loop, subagent threads, and web_search workers alike — records per-model tokens into a thread-safe ledger shown by `/usage`, and each turn's usage lands as a `usage` event in the session file.
</details>

<details>
<summary><strong>Context compaction</strong> — deterministic summary turns</summary>

When history exceeds `max_history_turns` (default 8), older turns fold into a deterministic summary — recent user requests, assistant outcomes, tool evidence, capped at 24 lines / 1200 chars — while the last 4 turns stay verbatim, carried with fx's exact continuation framing. Re-compaction nests prior summaries and accumulates counts. `/compact` forces it, and a 256 KB byte guard backstops runaway histories.
</details>

<details>
<summary><strong>Subagents</strong> — threaded children with capped permissions</summary>

The `subagent` tool with one command branch per call: `create` (one_off or persistent children on SBCL threads, each running the full agent loop with its own history and compaction), `inspect` (status/messages/configuration, plus `wait {until: "settled"}` for the bounded same-turn wait), `message.send`, and `lifecycle` cancel/close. Child permission modes inherit the caller and cannot exceed it; rules apply inside children, which deny rather than prompt. Nesting caps at depth 3. Not ported: relationships, configure, milestones, report intervals.
</details>

<details>
<summary><strong>Skills</strong> — discovery, catalog, and the full installer</summary>

Discovery scans fx's exact root list in precedence order — workspace `.fx/skills`, `skills`, `.opencode/skills`, `.codex/skills`, `.claude/skills`, `.agents/skills`, `.claw/skills`, then managed `~/.fx/skills` and the global compatibility roots — parsing SKILL.md YAML frontmatter (inline, quoted, and block scalars), first name wins. The catalog is advertised in the static context (16 KB cap, 1 KB per description), and the `skill` tool reads resources in 20 KB chunks with `next_offset` continuation, a 1 MB file cap, traversal rejection, and stale-location detection.

`install_skill` normalizes every fx source form — pasted `npx`/`bunx skills add` commands, `skills.sh` URLs, `owner/repo@skill` specs, git URLs, GitHub shorthand, local paths — installing local directories directly and everything else via `git clone --depth 1`. Both root-SKILL.md and nested layouts install, managed names are validated, and frontmatter-less SKILL.md files resolve under their directory name. `/skills [list|add|install|show|create|remove|reload|path]` manages them; only the `$` search menu is not ported.
</details>

<details>
<summary><strong>Web search</strong> — nested gateway worker request</summary>

fx runs search as a nested gateway worker request with a server-executed backend; the port keeps that architecture on the OpenAI-compatible endpoint with a search-capable worker model (default `perplexity/sonar`). Allowed/blocked domain filters map to the provider's filter (blocked prefixed `-`), only one filter may be non-empty (fx's exact validation), and output uses fx's verbatim framing: untrusted-content warning, escaped `[title](url)` source list, citation reminder, 100k char cap.
</details>

<details>
<summary><strong>MCP</strong> — stdio transport with dynamic tool selection</summary>

Servers configure in `~/.fx/mcp.json` with fx's schema, start lazily over newline-delimited JSON-RPC with the standard initialize handshake, restart if they die, and tolerate interleaved notifications and server-initiated requests. Dynamic schemas stay out of the main prompt exactly like fx: `mcp_search_tools` searches bounded metadata, `mcp_select_tool` advertises one exact `mcp_<server>_<tool>` for the next model step (execution requires approval), and `mcp_features` covers resources and prompts. `/mcp [list|tools|reload|path]` manages servers. Not ported: HTTP/SSE transports, OAuth, elicitation, completions, subscriptions.
</details>

<details>
<summary><strong>Hooks and modes</strong></summary>

fx's four in-process lifecycle hook kinds with their exact contracts — PreToolUse (continue / rewrite-arguments / block, with rewrites chaining and blocks short-circuiting), Stop (may request one synthetic continuation per turn), and the PostTurnEnd / AttentionRequired observers. Where the Zig wires first-party providers, this port loads user Lisp providers from `~/.fx/hooks.lisp` and the workspace `.fx/hooks.lisp`; `/hooks` lists registrations.

Modes match fx's registry — `code` (auto permissions) and `ask` (default) — plus a read-only `plan` mode exercising the ported read-only tool policy: the advertised tool projection is filtered and execution of anything else is denied with the mode's denial message. `/mode [id]` switches; `register-mode` adds custom ones.
</details>

**Not ported** (out of scope for the core port): the durable terminal/PTY subsystem, the TUI render engine, OAuth logins (Codex/Grok), ACP, NAPI/WASM embeddings, benchmarks, the subagent manager's relationship/configure/milestone surface, and MCP's HTTP/SSE transports, OAuth, elicitation, and completions.

## Tests

```bash
sbcl --noinform --disable-debugger \
  --eval '(require :asdf)' \
  --eval '(asdf:load-asd (truename "fx.asd"))' \
  --eval '(asdf:load-system :fx/tests)' \
  --eval '(fx.tests:run)' --quit
```

112 offline checks cover the JSON codec, glob matcher, every filesystem tool, terminal exec, SSE tool-call assembly, prompt fidelity, the permission engine, web_fetch URL policy, html-to-markdown, compaction, subagent command validation and mode capping, skills, web_search, and the session store.

Beyond the offline suite, end-to-end runs against a mock gateway exercise the agent loop, deny-rule enforcement, auto-compaction across a 12-turn session, threaded subagents, skill-catalog advertisement, and the web_search worker request with domain filters. A live stdio MCP server proves the full search → select → dynamic-call flow plus resources and prompts; hook/mode integration is proven in the live loop (a PreToolUse block reaching the model, a Stop hook triggering exactly one continuation, plan mode denying mutation); and install_skill runs a real `git clone` of a local repository with the installed skill then discovered by the catalog.
