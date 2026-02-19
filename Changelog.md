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
