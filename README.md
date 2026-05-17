# DelphiAnthropic – Claude API Wrapper for Delphi

![Delphi async/await supported](https://img.shields.io/badge/Delphi%20async%2Fawait-supported-blue)
![GitHub](https://img.shields.io/badge/IDE%20Version-Delphi%2010.4/11/12-ffffba)
![GitHub](https://img.shields.io/badge/platform-all%20platforms-baffc9)

___

### New
- [changelog v1.3](Changelog.md)
- [Functional demo using **Pythia-WebView2**](demos)
- [**Managed Agents API**](guides/managed-agents.md#managed-agents-beta)
- [**Webhooks**](guides/webhooks.md#webhooks) — signature verification (HMAC, constant-time) and strongly typed event dispatch
- Updated server tool types: `advisor_20260301`, `web_search_20260209`, `web_fetch_20260209`, `code_execution_20260120`
- [adaptive thinking](guides/thinking.md#adaptive-reasoning)
- [fast mode](guides/fast-mode.md#fast-mode-research-preview)

___


## Two simple illustrative examples of synchronous text generation

>[!TIP]
>To obtain an Anthropic API key, refer to https://platform.claude.com/account/keys

<br>

- Non-streamed example:

  ```pascal
  // uses Anthropic, Anthropic.Types, Anthropic.Helpers;
  // Client: IAnthropic;

  var Client := TAnthropicFactory.CreateInstance('ANTHROPIC_API_KEY');

  //JSON payload
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      Params
        .Model('claude-opus-4-7')
        .Messages( Generation.MessageParts
            .User('From which version of Delphi were multi-line strings introduced?')
        )
        .MaxTokens(1024);
    end;

  //Synchronous example
  var Chat := Client.Chat.Create(Payload);

  try
    for var Block in Chat.Content do
      if Block.&Type = TContentBlockType.text then
        Memo1.Lines.Text := Memo1.Text + Block.Text;
  finally
    Chat.Free;
  end;

  ```

<br>

- Streamed example (SSE):
  
  ```pascal
  // uses Anthropic, Anthropic.Types, Anthropic.Helpers;
  // Client: IAnthropic;

  //JSON payload
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model('claude-opus-4-7')
          .Messages( MessageParts
              .User('Explain the discrete topology')
          )
          .Thinking( CreateThinkingConfig('adaptive') )
          .Stream;
    end;

  // Streaming callback
  var StreamEvent: TChatEvent :=
    procedure (var Event: TChatStream; IsDone: Boolean; var Cancel: Boolean)
    begin
      if not IsDone then
        if Event.Block.&Type = TContentBlockType.text then
          Memo1.Text := Memo1.Text + Event.Block.Text;
      Application.ProcessMessages;
    end;

  //Synchronous example
  Client.Chat.CreateStream(Payload, StreamEvent);
  ```

<br>

Summary
- [Introduction](#introduction)
- [Philosophy and Scope](#philosophy-and-scope)
- [Documentation – Overview](#documentation--overview)
- [Going Further](#going-further)
- [Functional Demo](#functional-demo)
- [Functional Coverage](#functional-coverage)
- [Project Status](#project-status)
- [License](#license)

## Introduction

>Built with Delphi 12 Community Edition (v12.1 Patch 1) <br>
>The wrapper itself is MIT-licensed. <br>
>You can compile and test it free of charge with Delphi CE.

<br>

**DelphiAnthropic** is a native **Delphi wrapper for the Anthropic Claude API**, providing structured access to the Messages API, including:
- synchronous and asynchronous execution
- SSE streaming
- tools and function calling
- structured outputs (JSON schema)
- multimodal inputs (image, PDF)
- advanced reasoning modes (adaptive / extended thinking)
- the **Managed Agents** surface (Agents, Environments, Sessions, Vaults, Memory Stores)
- **Webhook** verification and consumption

<br>

>[!IMPORTANT]
> This is an unofficial library. <br>
> Anthropic does not provide an official Delphi SDK for Claude. <br>
> This project is a Delphi implementation over the [public API](https://platform.claude.com/docs/en/api/overview)

<br>

## Philosophy and Scope

Anthropic now exposes two complementary surfaces:
- ***the Messages API***, single and unified, where the client orchestrates each turn;
- ***the Managed Agents API***, where Anthropic operates a long-lived runtime (Agent + Environment + Session) and the client drives it through events and resources.

**Agent Skills** extend Claude through implicit, model-selected execution. `Tools` are explicitly invoked and fully client-orchestrated. **Managed Agents** sit one level above: they bind an agent definition, an environment and a session container together, with their own event stream and resource model.

This wrapper therefore focuses on:
- **faithful mapping** of both the Messages API and the Managed Agents surface
- **explicit modeling** of execution modes (client-orchestrated vs server-orchestrated)
- **clear separation between always-on API features and features gated by explicit Anthropic beta headers**
- **Delphi-first ergonomics**, not JSON-first usage


### Core execution modes

- **Standard generation**
  - blocking or promise-based
  - full response returned at once
  - suitable for background processing or batch workflows

- **SSE streaming**
  - synchronous or asynchronous
  - session-level or event-level callbacks
  - fine-grained interception of Claude SSE events

- **Tool-driven workflows**
  - function calling
  - server-side tools (beta)
  - client-side orchestration
  - strict schema validation for agent safety

- **Server-orchestrated agents (Managed Agents)**
  - long-lived `Agent` / `Environment` / `Session` lifecycle
  - resource attachments (GitHub repository, Files API entries, Memory Stores)
  - typed event surface (session events, thread events) with raw SSE access
  - credential management through `Vaults`
  - asynchronous, webhook-driven completion notifications

These distinctions are applied consistently at the API level and in the documentation.

<br>

## Documentation – Overview

The documentation is organized as **focused Markdown guides**, each covering one major capability.

### Main entry points
- Content generation
  - [non-streamed generation](guides/content-generation.md#content-generation-non-streamed)
  - [SSE streaming](guides/content-generation-sse.md#content-generation-sse-streaming)
  - promises and orchestration ([non-streamed](guides/content-generation.md#3-promises-and-orchestration) and [SSE](guides/content-generation-sse.md#5-promises-and-orchestration-chaining-asynchronous-operations))
- [Document & image understanding](guides/document-understanding.md#document-understanding)
  - [image inputs](guides/document-understanding.md#image-understanding)
  - [PDF analysis](guides/document-understanding.md#document-pdf-understanding)
  - [Files API integration](guides/files-api.md#using-a-file-in-messages)
- [Tools](guides/tools.md#tools)
  - [function calling](guides/tools-function-calling.md#function-calling)
  - [tool orchestration](guides/tools.md#using-tools-with-claude)
  - [text editor](guides/tools-text-editor.md#text-editor) 
  - [bash tools](guides/tools.md#bash-tool)
  - [web search](guides/tools.md#web-search-tool)
  - [***[beta]*** tools](guides/tools-beta.md)
- [Reasoning](guides/thinking.md#thinking) & control
  - [adaptive thinking](guides/thinking.md#adaptive-reasoning)
  - [extended thinking](guides/thinking.md#extended-reasoning)
  - [effort control](guides/thinking.md#effort)
  - [fast mode](guides/fast-mode.md#fast-mode-research-preview)
- [Agent Skills](guides/agent-skills.md#skills-beta)
  - [Custom Skills – API & Versioning](guides/agent-skills-custom.md)
- [Managed Agents](guides/managed-agents.md#managed-agents-beta)
  - [Agents](guides/managed-agents-agents.md#agents-beta)
  - [Environments](guides/managed-agents-environments.md#environments-beta)
  - [Sessions (Events / Resources / Threads / Thread Events)](guides/managed-agents-sessions.md#sessions-beta)
  - [Vaults & Credentials](guides/managed-agents-vaults.md#vaults--credentials-beta)
  - [Memory Stores](guides/managed-agents-memory-stores.md#memory-stores-beta)

Each section includes Delphi-first examples, not raw JSON.

>[!WARNING]
> In Anthropic terminology, `beta` means ***"feature gated by explicit beta headers"***, not a prerelease SDK version.

<br>

## Going Further

Advanced or cross-cutting topics are documented separately to keep the core readable:
- [structured outputs](guides/structured-outputs.md#structured-outputs)
- [Batch processing](guides/batch-processing.md#batch-processing)
- [Prompt caching](guides/prompt-caching.md#prompt-caching) (5 min / 1 hour)
- [Token counting](guides/token-counting.md#token-counting)
- [Citations](guides/citations.md#citations)
- [Models API](guides/models.md#models) — now exposes structured capability metadata (thinking modes, effort tiers, context-management features)
- [Files API](guides/files-api.md#files-api) (CRUD)
- [Webhooks](guides/webhooks.md#webhooks) — signature verification + typed event dispatch
- [Tips and Tricks](guides/tips-and-tricks.md#tips-and-tricks)

Each topic has its own Markdown document, directly linked from the guides.

<br> 

## Functional Demo

This repository includes a working VCL demo in the [demos](demos) folder, built on top of [Pythia-WebView2](https://github.com/MaxiDonkey/Pythia-webView2), used here as the host application for the wrapper.

This demo matters for users of the wrapper because it shows **DelphiAnthropic running inside a real application flow**, not only through isolated code snippets. It demonstrates how the `IAnthropic` client is connected to a UI-oriented conversation layer, with asynchronous SSE streaming, request/response JSON traceability, file upload and download through the Files API, reasoning display, web search, code execution, skills and MCP configuration.

One key part of the demo is the context reconstruction layer (`Demo.Anthropic.Context.pas`). Instead of replaying only a flat history of user prompts and assistant answers, it rebuilds richer message context from the stored JSON request and streamed JSON response: text blocks, reasoning blocks, tool calls and matching tool results, MCP exchanges, web-search results and reusable container state for skills/code execution. This gives the next request a more faithful view of what actually happened in previous turns, while deliberately avoiding stale or ephemeral file references.

The guides keep the didactic path: each API surface is explained independently, with focused Delphi examples. The demo is the complementary reference: it shows how those capabilities cooperate end-to-end in a functional Delphi application, and it provides a practical starting point for validating your API key, runtime setup and optional MCP configuration. See also the demo-specific setup notes in [demos/README.md](demos/README.md).

<br>

## Functional Coverage

| Domain / Feature                                   | Supported | Anthropic API (Beta) |
|--------------------------------------------------|:---------:|:--------------------:|
| Text generation                                   | ● | |
| Multimodal (image, PDF input)                     | ● | |
| SSE / streaming                                   | ● | |
| Persistent conversations                          | ● | |
| Agent skills                                      | ● | ● |
| Batch processing                                  | ● | |
| Structured outputs (JSON / strict tools)          | ● | |
| Function / tool calling                           | ● | |
| Programmatic tool calling                         | ● | ● |
| Tool search (dynamic discovery)                   | ● | ● |
| Fine-grained tool streaming                       | ● | |
| Code execution (Python sandbox)                   | ● | ● |
| Computer use                                      | ● | ● |
| Memory (cross-conversation)                       | ● | ● |
| Web search                                        | ● | |
| Web fetch (URL / PDF content)                     | ● | ● |
| Citations                                         | ● | |
| Search results grounding                          | ● | |
| Large context window (1M tokens)                  | ● | ● |
| Adaptive thinking                                 | ● | |
| Extended thinking                                 | ● | |
| Effort control                                    | ● | |
| Context compaction                                | ● | ● |
| Context editing                                   | ● | ● |
| Prompt caching (5 min)                            | ● | |
| Prompt caching (1 hour)                           | ● | |
| Token counting                                    | ● | |
| File management (Files API)                       | ● | ● |
| Data residency (inference geo)                    | ● | |
| Fast mode (research preview)                      | ● | ● |
| Managed Agents — Agents                           | ● | ● |
| Managed Agents — Environments                     | ● | ● |
| Managed Agents — Sessions (Events / Resources / Threads) | ● | ● |
| Managed Agents — Vaults & Credentials             | ● | ● |
| Managed Agents — Memory Stores                    | ● | ● |
| Webhooks (HMAC verification + typed events)       | ● | ● |
| Advisor tool                                      | ● | ● |

>- Supported: support provided by `DelphiAnthropic`
>- Anthropic API (Beta): Feature available only via the Anthropic API (beta).

<br>

## Project Status
- The Anthropic API is evolving rapidly
- Several advanced capabilities are still beta
- The wrapper follows a pragmatic approach:
  - expose what exists
  - clearly label what is beta
  - avoid duplicating the official documentation
  - prefer correctness over abstraction 

<br>

## License

This project is licensed under the [MIT](https://choosealicense.com/licenses/mit/) License.
