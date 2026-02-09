# Content generation (non-streamed)

- [Overview](#overview)
- [Synchronous text generation](#1-synchronous-text-generation)
- [Asynchronous text generation (promise-based)](#2-asynchronous-text-generation-promise-based)
- [Promises and orchestration](#3-promises-and-orchestration)
- [Quick selection guide](#quick-selection-guide)
- [Practical notes](#practical-notes)

___

<br>

## Overview

The `Client.Chat.Create` and `Client.Chat.AsyncAwaitCreate` methods request an Anthropic-Claude model to generate a single, non-streamed response using the **Messages API** style.

#### Unlike SSE streaming:
- the model produces its full output in one response,
- no intermediate chunks or events are emitted,
- the resulting `TChat` object contains the complete generated content.

<br>

>[!NOTE]
>**Tutorial support units**
>
>The `Display(...)` helper procedures used in the examples are provided by `Anthropic.Tutorial.VCL` or `Anthropic.Tutorial.FMX`, depending on the target platform.
>
>These units exist solely to keep tutorial code concise and readable. They are not part of the core API.
>In a real application, you are expected to replace them with your own UI, logging, or domain-specific logic.


<br>

## 1) Synchronous text generation

#### When to use it
Use the synchronous API when:
- you want the simplest possible usage,
- blocking the current thread is acceptable,
- you are running in a background thread or a console-style workflow.

#### How it works 
- `ParamProc` configures the `TChatParams` payload (model, messages, tools, and request parameters).
- `Create(ParamProc)` performs a blocking HTTP call.
- The returned `TChat` must be freed by the caller.

  
   ```pascal
     // uses Anthropic, Anthropic.Types, Anthropic.Helpers; 
     // Client: IAnthropic;

     var Client := TAnthropicFactory.CreateInstance('My_Anthropic_Key');

     var ModelName := 'claude-sonnet-4-5';
     var Prompt := 'What should I search for to find the latest developments in renewable energy?';
     var MaxTokens := 1000;

     //JSON payload
     var Payload: TChatParamProc :=
        procedure (Params: TChatParams)
        begin
          Params
            .Model(ModelName)
            .Messages( Generation.MessageParts
                .User( Prompt )
            )
            .MaxTokens(MaxTokens);
        end;

     //Synchronous example
     var Generated := Client.Chat.Create(Payload);

     //Display generated content
     try
       for var Content in Generated.Content do
         begin
           if Content.&Type = TContentBlockType.text then
             begin
               Memo2.Lines.Text := Memo2.Text + Content.Text;
             end
         end;
     finally
       Generated.Free;
     end;
   ```

#### Key points
- The call blocks until the model response is fully received.
- Memory ownership is explicit: you must free `TChat`.
- Errors are raised as exceptions.

<br>

## 2) Asynchronous text generation (promise-based)
#### Why use the asynchronous variant
The asynchronous API:
- avoids blocking the calling thread,
- integrates naturally with UI applications,
- enables clean composition through promises.

#### How it works
- `AsyncAwaitCreate(ParamProc)` immediately returns a `TPromise<TChat>`.
- The request runs in the background.
- `&Then` is invoked on success.
- `&Catch` centralizes error handling.

  ```pascal
    // uses Anthropic, Anthropic.Types, Anthropic.Helpers; 
    // Client: IAnthropic;

    //  In the asynchronous case, increase the scope of Client by declaring it in FormCreate.
    //  var Client := TAnthropicFactory.CreateInstance(My_Anthropic_Key);

    var Document := '..\media\Invoice.png';
    var base64 := TMediaCodec.EncodeBase64(Document);
    var mimeType := TMediaCodec.GetMimeType(Document);

    var ModelName := 'claude-sonnet-4-5';
    var MaxTokens := 1000;
    var Prompt := 'Describe the image';

    //JSON payload generation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Model(ModelName)
            .Messages( MessageParts
                .User( ContentParts
                    .AddImage(base64, mimeType)
                    .AddText(Prompt)
                )
            )
            .MaxTokens(MaxTokens);
      end;

    // Asynchronous generation (promise-based)
    var Promise := Client.Chat.AsyncAwaitCreate(Payload);

    // Simple processing or orchestration of promise
    Promise
      .&Then<string>(
        function (Value: TChat): string
        begin
          for var Content in Value.Content do
            begin
              if Content.&Type = TContentBlockType.text then
                begin
                  Memo2.Lines.Text := Memo2.Text + Content.Text;
                end
            end;
        end)
      .&Catch(
        procedure (E: Exception)
        begin
          Memo2.Lines.Text := Memo2.Text + E.Message;
        end);
   ```

#### Key points (callbacks variant)
- The callback context (`TPromiseChat`) is optional: use it when you need start/success/error hooks.
- `Sender` follows the same idea as in the tutorial streaming examples: it is a contextual object for UI/log helpers.
- The promise chain remains the primary composition mechanism (`&Then` / `&Catch`).

<br>

## 3) Promises and orchestration
Because `AsyncAwaitCreate` returns a `TPromise<TChat>`, it can be used as a building block for asynchronous orchestration.

#### You can:
- post-process results,
- trigger dependent operations,
- chain multiple generations sequentially,
- centralize error handling.

<br>

- Example: chaining two non-streamed generation requests
  - First promise parameters

    ```pascal
      //uses 
      //  Anthropic, Anthropic.Types, Anthropic.Helpers, Anthropic.Async.Promise, 
      //  Anthropic.Tutorial.VCL or Anthropic.Tutorial.FMX; 

      var ModelName := 'claude-sonnet-4-5';
      var MaxTokens := 2048;
      var Prompt := 'Write a short story about a magic backpack.';

      //JSON payload generation
      var Payload: TChatParamProc :=
        procedure (Params: TChatParams)
        begin
          with Generation do
            Params
              .Model(ModelName)
              .Messages( MessageParts
                  .User( Prompt )
              )
              .MaxTokens(MaxTokens);
        end;

      var Callbacks: TFunc<TPromiseChat> :=
        function: TPromiseChat
        begin
          Result.Sender := TutorialHub;
          Result.OnStart := Start;
          Result.OnSuccess := DisplayChat;
          Result.OnError := DisplayChat;
        end;
    ```


   - Linking two promises 

     ```pascal
      // Asynchronous generation (promise-based)
      var Promise := Client.Chat.AsyncAwaitCreate(Payload, Callbacks);

      Promise
       .&Then(
         function (First: TChat): TPromise<TChat>
         var
           Answer: string;
         begin
           Answer := First.Content[0].Text;

           // Second promise
           Result := Client.Chat.AsyncAwaitCreate(
             procedure (Params: TChatParams)
             begin
               with Generation do
                 Params
                   .Model(ModelName)
                   .Messages( MessageParts
                       .User( 'Summarize the following answer in 5 bullet points:'#10 + Answer )
                   )
                   .MaxTokens(MaxTokens);
             end
           );
         end)
       .&Then(
         procedure (Second: TChat)
         begin
           Display(TutorialHub, '----- SUMMARY');
           Display(TutorialHub, Second);
         end)
       .&Catch(
         procedure (E: Exception)
         begin
           Display(TutorialHub, E.Message);
         end);
     ```

This approach avoids nested callbacks and produces a linear, readable control flow.

>[!IMPORTANT]
>Using `TPromise<TChat>` requires adding the `Anthropic.Async.Promise` unit to the `uses` clause. 

<br>

>[!NOTE]
>**Tutorial support units**
>
>The `Display(...)` helper procedures used in the examples are provided by `Anthropic.Tutorial.VCL` or `Anthropic.Tutorial.FMX`, depending on the target platform.
>
>These units exist solely to keep tutorial code concise and readable. They are not part of the core API.
>In a real application, you are expected to replace them with your own UI, logging, or domain-specific logic.

<br>

## Quick selection guide
- Blocking call, simplest usage → `Client.Chat.Create(Payload)`
- UI-friendly, non-blocking → `Client.Chat.AsyncAwaitCreate(Payload)`
- Chained async workflows → `AsyncAwaitCreate` + `&Then`

<br>

## Practical notes
- These methods produce non-streamed responses only.
- Use streaming APIs if you need partial results or real-time rendering.
- `TChatParams` can be reused across sync and async variants.
