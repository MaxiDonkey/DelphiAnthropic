# Managed Agents [beta]

The ***Managed Agents API*** is the server-orchestrated counterpart of the Messages API. Anthropic operates the runtime — an **Agent** definition bound to an **Environment** runs inside a **Session** container — while the client drives the loop through *events*, attaches *resources*, and consumes results either synchronously, through raw SSE streams, or asynchronously via webhooks.

This guide is the entry point for the whole Managed Agents surface. Each sub-resource has its own dedicated guide.

- [Overview](#overview)
- [The four building blocks](#the-four-building-blocks)
- [Lifecycle at a glance](#lifecycle-at-a-glance)
- [Beta gating](#beta-gating)
- [Sub-resource guides](#sub-resource-guides)
- [References](#references)

___

<br>

## Overview

The Managed Agents API differs from the Messages API in three key ways:

1. **Long-lived state** — an Agent + Environment + Session lives beyond a single request and accumulates conversation, code-execution state, and attached resources.
2. **Server orchestration** — the client does not loop over `tool_use` / `tool_result` blocks turn by turn; instead, it sends *events* into a session and observes the resulting event stream.
3. **First-class resource model** — GitHub repositories, Files API entries, and Memory Stores are explicitly attached to a session rather than re-uploaded per turn.

>[!IMPORTANT]
> Managed Agents is a beta feature. All routes require the `managed-agents-2026-04-01` beta header (handled automatically by the wrapper through `Anthropic.Headers.Beta`).

<br>

## The four building blocks

| Resource | Role | Wrapper route |
|---|---|---|
| **Agent** | A reusable agent definition (system prompt, tools, Skills binding, default model). Versioned. | `Client.Agents` |
| **Environment** | The execution container template (base image, mounts, network policy). | `Client.Environments` |
| **Session** | A live instance: one Agent + one Environment, with events, threads and resources. | `Client.Sessions` |
| **Vault** | A workspace-scoped store for credentials reused by sessions (GitHub tokens, API keys). | `Client.Vaults` |

A fifth resource — **Memory Stores** — provides cross-session persistent memory that can be mounted as a session resource.

<br>

## Lifecycle at a glance

```text
1. Define an Agent             →  Client.Agents.Create(...)
2. Define an Environment       →  Client.Environments.Create(...)
3. (Optional) Store credentials in a Vault
                               →  Client.Vaults.Credentials.Create(...)
4. (Optional) Create a Memory Store
                               →  Client.MemoryStores.Create(...)
5. Open a Session              →  Client.Sessions.Create(... Agent + Environment + Resources ...)
6. Drive the session
   - Send events               →  Client.Sessions.Events.Send(...)
   - Stream raw events         →  Client.Sessions.Events.StreamRaw(...)
   - Inspect threads           →  Client.Sessions.Threads.*
7. (Optional) Be notified asynchronously via a Webhook delivery
                               →  see Webhooks guide
8. Archive or delete           →  Client.Sessions.Archive(...) / .Delete(...)
```

<br>

## Beta gating

Managed Agents routes are gated by the `managed-agents-2026-04-01` beta header. The wrapper sets it automatically when:

- a route under `agents/`, `environments/`, `sessions/`, `vaults/` or `memory_stores/` is called;
- a session payload references an Agent, an Environment, or a Memory Store.

You normally do not need to set the header yourself. If you do (e.g. testing a different revision), use `.Beta(['managed-agents-2026-04-01'])` on the relevant `*Params` object.

<br>

## Sub-resource guides

- [Agents](managed-agents-agents.md)
- [Environments](managed-agents-environments.md)
- [Sessions (Events / Resources / Threads / Thread Events)](managed-agents-sessions.md)
- [Vaults & Credentials](managed-agents-vaults.md)
- [Memory Stores (Memories / Memory Versions)](managed-agents-memory-stores.md)
- [Webhooks](webhooks.md)

<br>

## References

- Source units: [`Anthropic.Agents.pas`](../source/Anthropic.Agents.pas), [`Anthropic.Environment.pas`](../source/Anthropic.Environment.pas), [`Anthropic.Sessions.pas`](../source/Anthropic.Sessions.pas), [`Anthropic.Vaults.pas`](../source/Anthropic.Vaults.pas), [`Anthropic.MemoryStore.pas`](../source/Anthropic.MemoryStore.pas)
- Public API documentation: [platform.claude.com](https://platform.claude.com/docs/en/api/overview)
