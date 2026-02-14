# DelphiAnthropic – Claude API Wrapper for Delphi

![Delphi async/await supported](https://img.shields.io/badge/Delphi%20async%2Fawait-supported-blue)
![GitHub](https://img.shields.io/badge/IDE%20Version-Delphi%2010.4/11/12-ffffba)
![GitHub](https://img.shields.io/badge/platform-all%20platforms-baffc9)

___

### New
- changelog v1.2
- [adaptive thinking](guides/thinking.md#adaptive-reasoning)
- [fast mode](guides/fast-mode.md#fast-mode-research-preview)
- compaction

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

  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      Params
        .Model('claude-sonnet-4-6')
        .Messages( Generation.MessageParts
            .User('From which version of Delphi were multi-line strings introduced?')
        )
        .MaxTokens(1024);
    end;

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

  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      Params
        .Model('claude-opus-4-6')
        .Messages( Generation.MessageParts
            .User('Explain the discrete topology')
        )
        .Thinking( CreateThinkingConfig('adaptive') )
        .Stream;
    end;

  var StreamEvent: TChatEvent :=
    procedure (var Event: TChatStream; IsDone: Boolean; var Cancel: Boolean)
    begin
      if not IsDone then
        if Event.Block.&Type = TContentBlockType.text then
          Memo1.Text := Memo1.Text + Event.Block.Text;
      Application.ProcessMessages;
    end;

  Client.Chat.CreateStream(Payload, StreamEvent);
  ```

<br>

Summary
- [Introduction](#introduction)
- [Philosophy and Scope](#philosophy-and-scope)
- [Documentation – Overview](#documentation--overview)
- [Going Further](#going-further)
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

<br>

>[!IMPORTANT]
> This is an unofficial library. <br>
> Anthropic does not provide an official Delphi SDK for Claude. <br>
> This project is a Delphi implementation over the [public API](https://platform.claude.com/docs/en/api/overview)

<br>

## Philosophy and Scope

Anthropic exposes ***a single, unified Messages API***.

This wrapper therefore focuses on:
- **faithful mapping** of the Messages API
- **explicit modeling** of execution modes
- **clear separation between stable and beta features**
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
  - [function calling](tools-function-calling.md#function-calling)
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

Each section includes Delphi-first examples, not raw JSON.

<br>

## Going Further

Advanced or cross-cutting topics are documented separately to keep the core readable:
- [structured outputs](guides/structured-outputs.md#structured-outputs)
- [Batch processing](guides/batch-processing.md#batch-processing)
- [Prompt caching](guides/prompt-caching.md#prompt-caching) (5 min / 1 hour)
- [Token counting](guides/token-counting.md#token-counting)
- [Citations](guides/citations.md#citations)
- [Models API](guides/models.md#models)
- [Files API](guides/files-api.md#files-api) (CRUD)

Each topic has its own Markdown document, directly linked from the guides.

<br> 

## Functional Coverage

| Domain / Feature                                   | Supported | Anthropic API (Beta) |
|--------------------------------------------------|:---------:|:--------------------:|
| Text generation                                   | ● | |
| Multimodal (image, PDF input)                     | ● | |
| SSE / streaming                                  | ● | |
| Persistent conversations                          | ● | |
| Agents                                           | ● | |
| Agent skills                                     | ● | ● |
| Batch processing                                 | ● | |
| Structured outputs (JSON / strict tools)          | ● | |
| Function / tool calling                           | ● | |
| Programmatic tool calling                         | ● | ● |
| Tool search (dynamic discovery)                   | ● | ● |
| Fine-grained tool streaming                       | ● | |
| Code execution (Python sandbox)                   | ● | ● |
| Computer use                                     | ● | ● |
| Memory (cross-conversation)                       | ● | ● |
| Web search                                       | ● | |
| Web fetch (URL / PDF content)                     | ● | ● |
| Citations                                        | ● | |
| Search results grounding                          | ● | |
| Large context window (1M tokens)                  | ● | |
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