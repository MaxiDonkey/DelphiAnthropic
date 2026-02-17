# Web Fetch Tool [beta]

The **Web Fetch API** is a controlled mechanism that allows a model to ingest the **full content of explicitly provided web sources** in order to analyze them accurately within a secure framework.

- [Overview of the Web Fetch API](#overview-of-the-web-fetch-api)
- [Quick Start](#quick-start)
- [Combining Web Search and Web Fetch](#combining-web-search-and-web-fetch)
- [Key Takeaways](#key-takeaways)

___

<br>

## Overview of the Web Fetch API

The Web Fetch API enables a Claude model to **retrieve and process the complete text** of a static web page or a PDF document, using URLs that are **already present in the conversation context**
It is not designed to discover sources or browse the web, but to **load identified documents** for in-depth analysis *auditing, summarization, extraction, critique)*

**Key characteristics:**
- targeted fetching (no free browsing),
- direct integration into the model context,
- optional but traceable citations,
- synchronous execution within a single generation turn.

<br>

## Quick Start

Getting started requires three minimal steps:
1. **Enable the beta flag** `web-fetch-2025-09-10`
2. **Explicitly include the URL** in the user message
3. **Declare the** `web_fetch` **tool** in the API request

Minimal execution flow:
- the user requests analysis of a known URL,
- the model decides to trigger a fetch,
- the content is retrieved (text or PDF),
- the analysis is generated immediately.
 
Recommended best practices from the start:
- set `max_uses`,
- limit `max_content_tokens`,
- restrict domains when operating in sensitive environments.

### Usage code

```pascal
  var ModelName := 'claude-opus-4-6';
  var MaxTokens := 1024;
  var Prompt := 'Please analyze the content at https://platform.claude.com/docs/en/agents-and-tools/tool-use/web-fetch-tool#url-validation';

  //JSON payload creation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Beta(['web-fetch-2025-09-10'])
          .Model(ModelName)
          .MaxTokens(MaxTokens)
          .Messages( MessageParts
              .User( Prompt )
          )
          .Tools( ToolParts
              .Add( Tool.Beta.CreateWebFetchTool20250910
                  .MaxUses(5)
              )
          );
    end;

  // Asynchronous example
  var Promise := Client.Chat.AsyncAwaitCreate(Payload);

  Promise
    .&Then(
      procedure (Value: TChat)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  // Synchronous example
//  var Value := Client.Chat.Create(Payload);
//
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

## Combining Web Search and Web Fetch

The two tools are complementary, not interchangeable.
- **Web Search**: exploration phase <br>
  → identify, filter, and rank relevant sources.

- **Web Fetch**: exploitation phase <br>
  → retrieve the full content of a selected source.

Typical pipeline:
1. Web Search identifies several candidate documents
2. The model selects the most relevant one
3. Web Fetch retrieves the full text
4. Detailed analysis is produced with precise citations

This separation provides:
- tighter cost control,
- better traceability,
- reduced security and data-leak risks.

<br>

## Key Takeaways

Core invariants of the API:
- **Security first** <br>
  The model can never invent or reconstruct URLs.
- **Fetch ≠ Search** <br>
  The API loads content; it does not discover it.
- **Indirect cost model** <br>
  No functional surcharge, but potentially high token usage.
- **Implicit caching** <br>
  Fetched content may be reused across turns.
- **Product responsibility** <br>
  Citations are optional at the API level but legally critical at the UI level.
- **Ideal use cases** <br>
  Analysis of known documents: specs, research papers, contracts, technical documentation.
