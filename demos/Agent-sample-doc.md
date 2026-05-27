# Discovering the agent cards

**Demo code using**: Five cards ship with the `VCL_Anthropic` demo. Run them in order — each one grants the agent a bit more power.

| # | Card                       | What it shows                                  |
|---|----------------------------|------------------------------------------------|
| 1 | Research Analyst           | single agent, web tools, no project needed     |
| 2 | Local Project Review       | sub-agent, read-only on your project           |
| 3 | Supervised Exploration     | same, but **asks** before each tool call       |
| 4 | Safe Code Patch            | proposes a unified diff, writes nothing        |
| 5 | Sandbox To Local Code Edit | edits a sandbox copy, patches your disk after **your** confirmation |

## Where the cards live

- **Registry (UI side):**
  [`bin64/VCL_Anthropic/support/VCL_Anthropic-agent-cards.json`](bin64/VCL_Anthropic/support/VCL_Anthropic-agent-cards.json)
- **Inline JSON** for cards 1–3 (compact, fits in `content`).
- **Markdown definitions** for cards 4–5 (richer, referenced via `md_path`):
  - [`safe-code-patch-agent.md`](bin64/VCL_Anthropic/safe-code-patch-agent.md?plain=1)
  - [`sandbox-to-local-code-edit-agent.md`](bin64/VCL_Anthropic/sandbox-to-local-code-edit-agent.md?plain=1)

> Both links use GitHub's `?plain=1` raw view so the YAML front matter — which
> declares the agent's topology, tools and policies — stays visible. Without
> it, the rendered Markdown hides that header.

<br>

<p align="center">
  <img src="../docs/images/screenshots/card-agent-selector.png?raw=true" width="500"/>
</p>

---

## Card 1 — Research Analyst

Single agent, `claude-opus-4-7`. `web_search` and `web_fetch` are
`always_allow`; nothing else.

**No project needed.**

**Try this:**
> Produce a short, sourced briefing on the public state of Anthropic's
> Managed Agents API as of today. Group findings by theme, cite every claim
> with a URL, and flag anything you could not verify.

You should see interleaved tool calls and assistant text, with URL citations
in the final answer. No confirmation pops up.

<p align="center">
  <img src="../docs/images/screenshots/Agent-prompt_1.png?raw=true" width="500"/>
</p>

---

## Card 2 — Local Project Review

Coordinator + sub-agent **Code Inspector**. Read-only: `read`, `glob`, `grep`
(`always_allow`). No write, no `bash`. Inspector reads at most 12 files.

**Setup:** click the **Project** button on the input bubble and pick a small
local folder. It is uploaded under `/mnt/session/uploads/workspace/project`.

**Try this:**
> Review the selected project. Identify up to five concrete issues (correctness,
> safety, clarity), citing the exact file path for each. Group your answer as
> Findings / Severity / Recommendations.

You get three sections, each finding tied to a real path. Nothing is written.

<p align="center">
  <img src="../docs/images/screenshots/Agent-prompt_2.png?raw=true" width="500"/>
</p>

---

## Card 3 — Supervised Exploration

Same shape as card 2, but the sub-agent's tools are `always_ask` — Pythia
prompts you before each call. The sub-agent has a tight budget: one `glob`,
up to three `read`, at most one `grep`.

**Setup:** same as card 2 (a small selected project).

**Try this:**
> Give me a one-paragraph summary of what this project is, plus a short list
> of its most notable files. Use the smallest number of tool calls.

The point here is to *experience* the confirmation dialogs (labels come from
[`Demo.Anthropic.Strs.pas`](Demo.Anthropic.Strs.pas)). Deny one to see the
clean *"Interrupted by the user."* path.

<p align="center">
  <img src="../docs/images/screenshots/Agent-prompt_3.png?raw=true" width="500"/>
</p>

---

## Card 4 — Safe Code Patch

Multi-agent (Markdown-defined): **Code Locator** finds the spot, **Patch
Author** drafts a diff. Neither has `edit`. Coordinator reviews and returns
the diff in a fixed format.

**Setup:** any small selected project.

**Try this:**
> In the selected project, propose the smallest safe patch that improves
> one specific user-visible string for clarity. Pick the string yourself,
> explain the choice, then return the unified diff between the
> `PYTHIA_UNIFIED_DIFF_BEGIN/END` markers. Do not modify any file.

**Required answer shape** (three sections, in this order):

1. **Patch Summary** — files affected, intent.
2. **Unified Diff** between `PYTHIA_UNIFIED_DIFF_BEGIN` / `…_END`, in
   standard unified diff syntax with local relative paths in the `---` /
   `+++` headers.
3. **Validation Notes** — why the patch is narrow, suggested test or
   manual check.

Your disk is untouched: this card *proposes*, nothing more.

<p align="center">
  <img src="../docs/images/screenshots/Agent-prompt_4.png?raw=true" width="500"/>
</p>

---

## Card 5 — Sandbox To Local Code Edit

The full round-trip. **Code Locator** finds, **Sandbox Editor** edits the
sandbox copy (its `edit` tool is `always_ask`), the coordinator returns
**two** machine-readable blocks. Pythia spots them and asks *you* before
touching your local file.

**The boundary the card teaches:**
1. Your folder is uploaded to the sandbox.
2. Agents edit only the sandbox copy.
3. The answer returns a manifest + a unified diff.
4. Pythia — not the agent — applies the diff locally, after your confirmation.

The coordinator must never claim your disk was modified.

### Validation test (patches the demo's own source)

The point of this test is to let the agent **find the work itself** — no file
hint, no symbol name, just an intent and a contract.

**Setup — choose one of these two options:**

> ⚠️ The local apply step really modifies the file on disk. There is no
> built-in undo.

- **Recommended — work on a copy.** Duplicate `demos\VCL\pythia-anthropic`
  somewhere outside the repo (e.g. `D:\sandbox\pythia-anthropic-copy`) and
  point the **Project** button at that copy. You can throw the copy away
  afterwards.
- **Direct — work on the demo source itself.** Pick
  `demos\VCL\pythia-anthropic` directly. Commit or back up the folder
  first; otherwise the change is permanent for your working tree.

**Try this:**
> In the selected project, find the error message shown when an agent card
> is missing or invalid (the one that mentions both *not found* and
> *invalid*).
>
> Goal:
> - modify only that message so it states the card can be defined either
>   via `content` or via `md_path`;
> - touch a single file;
> - do not change any business behavior;
> - do not reformat code.
>
> Expected work:
> 1. locate the relevant file;
> 2. modify only the sandbox copy with the `edit` tool if needed;
> 3. return the Local Apply Manifest between the
>    `PYTHIA_LOCAL_APPLY_MANIFEST_BEGIN` / `PYTHIA_LOCAL_APPLY_MANIFEST_END`
>    markers;
> 4. return the Unified Diff between the
>    `PYTHIA_UNIFIED_DIFF_BEGIN` / `PYTHIA_UNIFIED_DIFF_END` markers;
> 5. state the manual check to run after local application.

**Required answer shape** (four sections, in this order):

1. **Cloud Edit Summary** — sandbox files changed, intent, local files
   expected to change.
2. **Local Apply Manifest** between `PYTHIA_LOCAL_APPLY_MANIFEST_BEGIN` /
   `…_END`. Key-value text (not JSON) with at least `root_hint`, `files`,
   and for each file `sandbox_path`, `local_relative_path`, `change_type`,
   `requires_user_confirmation`.
3. **Unified Diff** between `PYTHIA_UNIFIED_DIFF_BEGIN` / `…_END`.
4. **Validation Notes** — why the edit is narrow, suggested local check.

> **Path gotcha:** `local_relative_path` is relative to `root_hint`. If the
> sandbox path is `/mnt/session/uploads/workspace/project/Foo.pas`, the
> local relative path is `Foo.pas`, **not** `project/Foo.pas`.

**What you should see, in order:**
1. `code-locator` globs/greps the project and reports back where the error
   message lives.
2. `sandbox-editor` asks before its `edit` call → you allow it.
3. The final answer contains both marker blocks above, with a real file
   path filled in and the new wording referencing `content` and `md_path`.
4. Pythia detects the markers and asks for a local-apply confirmation.
5. After you confirm, the matching `.pas` file on disk carries the new
   message; to see it live, temporarily break the cards registry (e.g.
   rename an agent `id` so it no longer matches the selection) and relaunch
   the demo — the new wording appears.

<p align="center">
  <img src="../docs/images/screenshots/Agent-prompt_5.png?raw=true" width="500"/>
</p>

---

## Where to dig in the code

| Layer                  | Units                                                                              |
|------------------------|------------------------------------------------------------------------------------|
| Card model & loading   | `Agent.Cards`, `Agent.Markdown`, `Agent.Fingerprint`                               |
| Cloud lifecycle        | `Agent.Registry`, `Agent.Provisioning`, `Agent.Cleanup`                            |
| Local project bridge   | `Agent.Folder` (upload), `Agent.LocalApply` (apply manifest + diff)                |
| Run-time flow          | `Session.Transport`, `Session.Events`, `Session.Flow`, `Agent.TurnDisplay`         |

All units live under [`demos/VCL/pythia-anthropic`](.) with the
`Demo.Anthropic.` prefix.

---

## Caveats

> [!WARNING]
> **Occasional cloud-side `503` errors.** Anthropic's Managed Agents API is
> still in beta and the platform appears to be capacity-constrained at
> times. A run may fail with a `503` returned by the cloud. Unlike the
> regular Pythia error path, that failure is delivered *inside a tool
> result block* rather than as a top-level Pythia error notification, so
> it does not pop up the usual error dialog. What you will see instead is
> the turn ending early with the error embedded in the agent's trace —
> control is returned to you, the chip stays in place, and you can simply
> resend the prompt. This is a transient platform issue, not a bug in the
> card or in the demo.

<br>

> [!NOTE]
> **Scope of this document.** The five cards covered here exercise only
> the core Managed Agents surface: built-in tools (`web_search`,
> `web_fetch`, `read`, `glob`, `grep`, `edit`), single-agent and
> multi-agent topologies, sandbox uploads, and the local apply round-trip.
> MCP servers, skills, Vault and memories already work in Pythia outside
> the agent context and are documented separately. What is **not** covered
> here is their use *from inside an agent card* — wiring an MCP server or
> a skill into a managed agent's tool list, or letting an agent reach
> Vault and memories during a run. That integration will be the subject
> of a separate document.
