# Token counting

Token counting makes it possible to estimate in advance the input size of a message sent to Claude, in order to optimize costs, rate limits, and model selection.

Token counting acts as a universal preview layer, shared across all models and content types, enabling pre-execution reasoning about feasibility, cost, and model invocation strategy.

- [Supported models](#supported-models)
- [Implementing token counting by processing type](#implementing-token-counting-by-processing-type)
- [Quick Selection Guide](#quick-selection-guide)
- [Practical Notes](#practical-notes)

___

<br>

## Supported models

The token counting mechanism is supported by ***all active Claude models***.
In other words, regardless of the selected model (Opus, Sonnet, etc.), the API provides a reliable estimate of the number of input tokens before the message is actually sent.

**Key points to remember:**
- Full compatibility across the current Claude ecosystem
- Estimation is independent from actual billing
- System-added tokens may be included in the count but are not billed

<br>

## Implementing token counting by processing type

Token counting relies on ***the same input format as the Messages API***, ensuring full consistency between estimation and execution. The mechanism automatically adapts depending on the type of message:

- JSON Payload creation
  - [1. Basic message (text only)](#1-basic-message-text-only)
  - [2. Message with tools](#2-message-with-tools)
  - [3. Message with images](#3-message-with-images)
  - [4. Message with extended thinking](#4-message-with-extended-thinking)
  - [5. Message with PDF documents](#5-message-with-pdf-documents)

___

<br>


### 1. Basic message (text only)

- Inputs: system prompt + user/assistant messages
- Use case: standard scenarios, simple routing, prompt length optimization

<br>

- JSON Payload creation
  ```pascal
    var ModelName := 'claude-opus-4-6';
    var SystemPrompt := 'You are a scientist';
    var Prompt := 'Hello, Claude';

    //JSON payload creation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Model(ModelName)
            .System(SystemPrompt)
            .Messages( MessageParts
                .User( Prompt )
            );
      end;
  ```

<br>

### 2. Message with tools

- Tool definitions (schemas, descriptions) are included in the token count
- The cost applies only to the initial sampling call, not to subsequent server-side tool calls
- Token impact can be significant due to JSON schemas

<br>

- JSON Payload creation
  ```pascal
    var ModelName := 'claude-opus-4-6';
    var Prompt := 'What is the weather in San Francisco?';

    // Schema Payload creation using TSchemaParams class
    var GetWeather := TSchemaParams.New
      .&Type('object')
      .Properties( TJSONObject.Create
         .AddPair('location', TJSONObject.Create
             .AddPair('type', 'string')
             .AddPair('description', 'The city and state, e.g. San Francisco, CA')
         )
      )
      .Required(['location']);

    //JSON payload creation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Model(ModelName)
            .Tools( ToolParts
                .Add( Tool.CreateToolCustom
                    .Name('get_weather')
                    .Description('Get the current weather in a given location')
                    .InputSchema(GetWeather)
                )
            )
            .Messages( MessageParts
                .User( Prompt )
            );
      end;
  ```

<br>

### 3. Message with images

- Base64-encoded images are counted
- Token usage strongly depends on image size and visual complexity
- Text and image content are aggregated within a single structured message
 
<br>

- JSON Payload creation
  ```pascal
    var Document := '..\media\Invoice.png';
    var Base64 := TMediaCodec.EncodeBase64(Document);
    var MimeType := TMediaCodec.GetMimeType(Document);

    var ModelName := 'claude-opus-4-6';
    var Prompt := 'Describe this image';

    //JSON payload generation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Model(ModelName)
            .Messages( MessageParts
                .User( ContentParts
                    .AddImage(Base64, MimeType)
                    .AddText(Prompt)
                )
            );
      end;  
  ```

<br>

### 4. Message with extended thinking

- Thinking blocks from previous assistant turns are ignored
- The thinking block of the current turn is counted
- This allows precise anticipation of the impact of internal reasoning on the context window

<br>

- JSON Payload creation
  ```pascal
    var ModelName := 'claude-sonnet-4-5';
    var BudgetTokens := 10000;
    var Prompt := 'Are there an infinite number of prime numbers such that n mod 4 == 3?';

    //JSON payload generation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Model(ModelName)
            .Thinking( CreateThinkingConfig('enabled')
                .BudgetTokens(BudgetTokens)
            )
            .Messages( MessageParts
                .User( Prompt )
            );
      end;
  ```

<br>

### 5. Message with PDF documents

- PDFs are handled as content blocks
- The same rules and limitations apply as with the Messages API
- Token counting includes both the document and the associated text instructions 

<br>

- JSON Payload creation
  ```pascal
    var Document := '..\media\File_Search_file.pdf';
    var Base64 := TMediaCodec.EncodeBase64(Document);

    var ModelName := 'claude-opus-4-6';
    var Prompt := 'Please summarize this document.';

    //JSON payload generation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Model(ModelName)
            .Messages( MessageParts
                .User( ContentParts
                    .AddPDF(Base64)
                    .AddText(Prompt)
                )
            );
      end;
  ```

<br>

## Quick Selection Guide

Use token counting strategically depending on your objective:
- **Prompt size control** → Count tokens early when designing long or templated prompts
- **Model routing** → Compare token estimates to decide between Opus vs. Sonnet for cost/performance trade-offs
- **Multimodal inputs** → Always pre-count when using images or PDFs, as token impact can grow quickly
- **Tool-heavy workflows** → Pre-evaluate tool schemas to avoid unexpected token inflation
- **Extended reasoning** → Budget explicitly when enabling extended thinking to preserve context headroom

<br>

## Practical Notes

- Token counts are estimates, not exact guarantees; small deviations may occur at execution time
- System-added optimization tokens may appear in counts but are never billed
- Token counting and message creation have independent rate limits
- Cached prompts do not apply during token counting
- For large documents or images, token usage often dominates over plain text
- Treat token counting as a design-time diagnostic tool, not just a cost check

