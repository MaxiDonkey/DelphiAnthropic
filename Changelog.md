#### 2026, May 27 version 1.3.2

- Pre-installed [managed agent cards](demos/bin64/VCL_Anthropic/support/VCL_Anthropic-agent-cards.json): Added five ready-to-use cards for the agent selector — three defined inline in JSON (Research Analyst: single-agent claude-opus-4-7 with web_search/web_fetch always allowed; Local Project Review: coordinator + sub-agent code-inspector read-only on the uploaded local project; Supervised Exploration: coordinator + sub-agent explorer with confirmation on each tool) and two referenced by md_path (Safe Code Patch and Sandbox To Local Code Edit...).

- Added streamed async/await variants for Anthropic sessions with progress/cancellation callbacks and TSessionStreamStatus typed status.

- Ajout d’un GetStream SDK générique avec callback de réception, exposé sur les routes Sessions.StreamRaw, afin de supporter les flux SSE via la couche HTTP injectable et monitorée. 


#### 2026, May 19 version 1.3.1

- Typed stream routing:
  - `TEventData` exposes block-aware accumulators alongside the legacy `Text` / `Thought` properties: `AssistantText` (text-only blocks), `ToolCalls: TArray<TToolCallSnapshot>`, `ToolResults: TArray<TToolResultSnapshot>`, plus `CurrentBlockType` / `CurrentBlockIndex` and per-chunk `LastAssistantDelta` / `LastReasoningDelta` / `LastToolInputDelta` / `LastToolResultDelta`.
  - New public records `TToolCallSnapshot` and `TToolResultSnapshot` re-exported through `Anthropic`.

<br>

- Semantic streaming callbacks:
  - `TStreamEventCallBack` adds optional typed slots: `OnAssistantTextDelta`, `OnReasoningDelta`, `OnToolUseStart`, `OnToolUseInputDelta`, `OnToolUseStop`, `OnToolResultStart`, `OnToolResultDelta`, `OnToolResultStop`.
  - `IStreamEventDispatcher` gains matching `Dispatch*` methods.
  - Consumers can subscribe to high-level events (assistant text, reasoning, tool input, tool output) without re-implementing block-type routing on top of raw `TChatStream` events.

<br>

- Stream engine internals:
  - `TEventEngineManager` maintains an `Index → TContentBlockType` map across `content_block_start` / `_stop` events and primes `TEventData.SetActiveBlock` before each `Aggregate`.
  - Legacy dispatch (`OnContentStart` / `OnContentDelta` / `OnContentStop`, `OnMessageStart` / `OnMessageDelta` / `OnMessageStop`, `OnError`) is preserved verbatim and fires first; the typed dispatch runs after.

<br>

- Backward compatibility:
  - All pre-existing fields, properties, methods and callbacks are preserved unchanged. Consumers relying on `TPromiseChatStream.OnProgress` or on the legacy `OnContentDelta` / `TEventData.Text` see identical behavior.
  - Typed callbacks are opt-in: slots left unassigned are no-ops.

<br>

#### 2026, May 17 version 1.3.0

- Functional demo using **Pythia-WebView2** (see demo [folder](demos) and [Pythia-WebView2 project](https://github.com/MaxiDonkey/Pythia-webView2)) 

<br>

- Managed Agents API (new):
  - End-to-end support for the new Anthropic agent orchestration surface, exposed through the `IAnthropic` client as dedicated routes (`Agents`, `Environments`, `Sessions`, `Vaults`, `MemoryStores`).
  - `Agents`: create, retrieve, list, update, archive, version listing, and multi-agent composition (sub-agents) with Skills binding.
  - `Environments`: create, retrieve, list, update, delete, archive — defines the container in which a session runs.
  - `Sessions` (with nested sub-routes): create / retrieve / list / update / delete / archive, plus `Events` (list, send, raw SSE stream), `Resources` (GitHub repository with branch/commit checkout, Files API mount, memory-store attachment with access mode and instructions), `Threads` (list / retrieve / raw stream) and `Threads.Events` (list, raw stream).
  - `Vaults` + nested `Credentials` route (create, retrieve, list, update, delete, archive, validate).
  - `MemoryStores` + nested `Memories` and `MemoryVersions` routes (create, retrieve, list, update, delete; redact a memory version).
  - Async/await variants (`AsyncAwaitXxx` returning `TPromise<T>`) for every managed-agent operation.

<br>

- Webhooks (new):
  - New unit dedicated to verifying and consuming Anthropic webhook deliveries (`TWebhookVerifier`, `TWebhookEvent`, `TWebhookEventData`).
  - HMAC signature verification with constant-time comparison and configurable freshness window (`MaxAgeSeconds`, default 300 s).
  - Strongly typed event surface (`TWebhookEventType`, `TWebhookResourceKind`) covering session, session-thread, vault and vault-credential lifecycles, with `TryGetEventType` / `IsSessionEvent` / `IsVaultCredentialEvent` helpers.
  - `Unwrap` / `Verify` / `VerifyOrRaise` overloads accepting either raw `TBytes` or `string` payloads.

<br>

- Updated server tool types (Anthropic refresh):
  - Advisor tool `advisor_20260301` (beta header `advisor-tool-2026-03-01` is inferred).
  - Web search tool `web_search_20260209`.
  - Web fetch tool `web_fetch_20260209`.
  - Code execution tool `code_execution_20260120`.
  - Beta-header inference (`Anthropic.Headers.Beta`) distinguishes GA tool `type` values from beta header tokens and gates remaining betas per endpoint and payload shape.

<br>

- Custom tool definitions:
  - Added typed `InputExamples(...)` and `EagerInputStreaming(...)` helpers for custom tools.
  - `EagerInputStreaming(True)` now infers the `fine-grained-tool-streaming-2025-05-14` beta header.

<br>

- New content block params and helpers:
  - `TCompactionBlockParam` and `TContainerUploadBlockParam` exposed at the request level.
  - `TContentsHelper.AddCompaction` (raw / with encrypted content / from object) and `TContentsHelper.AddContainerUpload` (from file id / from object) added to simplify message composition.

<br>

- Models metadata expansion:
  - `TModel` now surfaces structured capability metadata: `TModelCapabilities`, `TModelCapabilitySupport`, `TModelThinkingCapability`, `TModelThinkingTypes`, `TModelEffortCapability`, `TModelContextManagementCapability`.
  - `Models.Retrieve` / `Models.List` consumers can now inspect supported thinking modes, effort tiers and context-management features directly from the API response.

<br>

- Chat responses:
  - New `TStopDetails` object on chat responses, exposing structured stop information alongside `stop_reason` / `stop_sequence`.

<br>

- Beta-header coverage update:
  - Added recognition for `managed-agents-2026-04-01`, `task-budgets-2026-03-13`, `fine-grained-tool-streaming-2025-05-14`, `user-profiles-2026-03-24`, `compact-2026-01-12`, `extended-cache-ttl-2025-04-11`, `fast-mode-2026-02-01`, `interleaved-thinking-2025-05-14`, `dev-full-thinking-2025-05-14`, `mcp-client-2025-04-04`, `model-context-window-exceeded-2025-08-26`, `pdfs-2024-09-25`, `prompt-caching-2024-07-31`, `token-counting-2024-11-01`, `token-efficient-tools-2025-02-19`.
  - Removed superseded tokens (`advanced-tool-use-2025-11-20`, `tool-search-tool-2025-10-19`, `web-fetch-2025-09-10`).

<br>

#### 2026, February 19 version 1.2.0

- Anthropic parity refresh (as of 2026-02-07):
Comprehensive wrapper revision to ensure API surface, behaviors, and defaults remain consistent with the Anthropic offering as of February 7, 2026.

<br>

- Model updates:
  - Add support for Claude 4.6 models (e.g. Sonnet 4.6 / Opus 4.6) across the wrapper surface (messages, streaming, tools, structured outputs).
  - Updated model selection guidance and recommended defaults to reflect the upstream lineup.

<br>

- Async/await support (Promises):
  - Added promise-based asynchronous API enabling async/await workflows across core operations (requests, streaming, tool calls, and batch endpoints where applicable).
  - Unified cancellation / timeout semantics for async execution paths.

<br>

- Skills support (built-in + custom):
  - Built-in skills: end-to-end support for skill-enabled workflows (tool choice / automatic vs forced invocation, consistent event mapping in streaming).
  - Custom skills: support for registering and invoking custom tools/skills (schema-driven parameters, deterministic call/return mapping, streaming compatibility).

<br>

Fast mode / low-latency execution:
- Added Fast mode toggle (where available upstream), enabling latency-optimized execution paths.
- Documentation updated with guidance on when to prefer Fast vs Standard execution.

<br>

- Structured outputs / formatted responses:
  - Updated formatted output handling to match the latest Anthropic behavior (schema-driven responses, validation strategy, error semantics).
  - Improved streaming integration for structured outputs (event ordering and completion semantics aligned with upstream).

<br>

- Surface alignment and consistency pass:
  - Harmonized request/response shapes, option names, and defaults.
  - Consistency pass on error handling and edge-case semantics (validation, missing fields, streaming termination, etc.)

<br>

- Docs and examples update:
  - Updated documentation and examples to reflect the 4.6 models, skills, fast mode, async/await, and structured output changes.
  - Minor cleanup to keep wrapper usage aligned with upstream conventions.

<br>

#### 2025, January 7 version 1.1.0

- Add Batch Delete API.
- Add PDF support.
- Integrating Model Service APIs.
- Integrating Token counting APIs.
- Revision of the Simplified Unit Declaration.
- Adding Tools to Simplify the Tutorial.
- Code Fixes - Issues (8 to 14).
- README.md Revision.
