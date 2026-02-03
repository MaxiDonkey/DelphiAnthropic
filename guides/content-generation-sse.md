# Content generation SSE (Streaming) 

- [Terminology](#terminology)
- [Synchronous streaming (direct stream consumption)](#1-synchronous-streaming-direct-stream-consumption)
- [“Mute” asynchronous streaming (aggregation only)](#2-mute-asynchronous-streaming-aggregation-only)
- [Asynchronous streaming with session callbacks](#3-asynchronous-streaming-with-session-callbacks)
- [Asynchronous streaming with event callbacks](#4-asynchronous-streaming-with-event-callbacks)
- [Promises and orchestration: chaining asynchronous operations](#5-promises-and-orchestration-chaining-asynchronous-operations)
- [Quick selection guide](#quick-selection-guide)
- [Practical notes](#practical-notes)

___

This section presents several ways to consume an SSE stream returned by `Client.Chat`, with an intentional progression:
1. **Synchronous SSE:** the most direct approach, ideal to get to the point.
2. **“Mute” asynchronous SSE:** asynchronous, with no real-time tracking (final aggregated result only). 
3. **Asynchronous SSE with session callbacks:** simple tracking (`start/progress/cancel`).
4. **Asynchronous SSE with event callbacks:** fine-grained, event-by-event interception.
5. **Promises and orchestration:** composition with &Then / &Catch to chain asynchronous operations.

<br>

>[!NOTE] 
>**(tutorial support units)**
>
>The `Display...` / `DisplayStream...` helper methods used in the examples are provided by `Anthropic.Tutorial.VCL` or `Anthropic.Tutorial.FMX` (depending on the target platform) to keep examples simple and readable.
>They are not part of the core API—you can replace them with your own UI/logging routines.

<br>

## Terminology

- **Session callbacks** Session-level stream tracking: start, progress (aggregated chunks), cancellation, completion.
<br>

- **Event callbacks** Fine-grained interception by SSE event type: `message_start_event`, `content_block_delta`, `content_block_stop`, etc. 

<br>


## 1) Synchronous streaming (direct stream consumption)  

### When to use it
When you want a straightforward, immediate consumption of the SSE stream—no promise and no Then/Catch composition.

### How it works
- `Payload` builds the request (model, input, options, streaming).
- `Event` is called for each decoded SSE chunk.
- When the final SSE event is received (`message_stop` followed by stream termination), the stream is finished. 

  ```pascal
  //uses Anthropic, Anthropic.Types, Anthropic.Helpers, Anthropic.Tutorial.VCL (*or Anthropic.Tutorial.FMX*);

  //  Increase the scope of Client by declaring it in FormCreate.
  //  var Client := TAnthropicFactory.CreateInstance(My_Anthropic_Key);

  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 16000;
  var ThinkingBudget := 6000;
  var SystemPrompt := 'You are an expert in mathematics topology';
  var Prompt := 'Can we find accumulation points in a discrete topology?';

  //JSON payload generation
  var Payload: TProc<TChatParams> :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        params
          .Model(ModelName)
          .MaxTokens(MaxTokens)
          .System(SystemPrompt)
          .Messages( MessageParts
            .User( Prompt )
          )
          .Thinking( CreateThinkingConfig(True)
            .BudgetTokens(ThinkingBudget)
          )
          .Stream;
    end;   

  // Streaming callback
  var StreamEvent: TChatEvent :=
     procedure (var Chat: TChatStream; IsDone: Boolean; var Cancel: Boolean)
     begin
       if not IsDone then
         DisplayStream(TutorialHub, Chat);
       Application.ProcessMessages;
     end;

  //Synchronous example
  Client.Chat.CreateStream(Payload, StreamEvent);
  ```

<br>


## 2) “Mute” asynchronous streaming (aggregation only)

Compared to the synchronous mode, this variant frees the calling thread (promise-based) but provides no real-time tracking: you only get the final aggregated result.

Important: this example is asynchronous (promise).
- “Mute” means no live callbacks, not “synchronous”.

  ```pascal
  //uses Anthropic, Anthropic.Types, Anthropic.Helpers, Anthropic.Tutorial.VCL (*or Anthropic.Tutorial.FMX*);

  //  In the asynchronous case, increase the scope of Client by declaring it in FormCreate.
  //  var Client := TAnthropicFactory.CreateInstance(My_Anthropic_Key);

  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 1000;
  var Prompt := 'From which version of Delphi were multi-line strings introduced?';

  //JSON payload generation
  var Payload: TProc<TChatParams> :=
  procedure (Params: TChatParams)
  begin
    Params
      .Model(ModelName)
      .MaxTokens(MaxTokens)
      .Messages( Generation.MessageParts
          .User( Prompt )
        )
      .Stream;
  end;

  // Asynchronous promise example (mute: no live callbacks)
  var Promise := Client.Chat.AsyncAwaitCreateStream(Payload);

  Promise
    .&Then<TEventData>(
      function (Value: TEventData): TEventData
      begin
        Result := Value;
        ShowMessage(Value.Id);
        ShowMessage(Value.Text);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
  ``` 

Consequence
- No `OnProgress`: no incremental UI updates.
- The stream is consumed/aggregated internally → `TEventData` is available at completion.

<br>


## 3) Asynchronous streaming with session callbacks

Compared to the “mute” mode, this variant adds progressive tracking through session callbacks (`OnStart`, `OnProgress`, cancellation...), while still returning a promise for the final aggregated result.

**Recommended split**
- Session context (`TPromiseChatStream`): tracking logic.
- Payload (`TProc<TChatParams>`): JSON request construction.

  ```pascal
  //uses Anthropic, Anthropic.Types, Anthropic.Helpers, Anthropic.Tutorial.VCL (*or Anthropic.Tutorial.FMX*);

  //  In the asynchronous case, increase the scope of Client by declaring it in FormCreate.
  //  var Client := TAnthropicFactory.CreateInstance(My_Anthropic_Key);

  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 16000;
  var ThinkingBudget := 6000;
  var Prompt := 'From which version of Delphi were multi-line strings introduced?';

  //JSON payload generation
  var Payload: TProc<TChatParams> :=
  procedure (Params: TChatParams)
  begin
    with Generation do
      Params
        .Model(ModelName)
        .MaxTokens(MaxTokens)
        .Messages( MessageParts
            .User( Prompt )
          )
        .Thinking( CreateThinkingConfig(True)
            .BudgetTokens(ThinkingBudget)
          )
        .Stream;
  end;

  var SessionCallbacks: TFunc<TPromiseChatStream> :=
      function : TPromiseChatStream
      begin
        Result.Sender := TutorialHub;
        Result.OnStart := Start;
        Result.OnProgress := DisplayStream;
        Result.OnDoCancel := DoCancellation;
        Result.OnCancellation := DoCancellationStream;
      end;

  //Asynchronous promise example
  var Promise := Client.Chat.AsyncAwaitCreateStream(Payload, SessionCallbacks);

  Promise
    .&Then(
      procedure (Value: TEventData)
      begin
        ShowMessage(Value.Thought);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
  ```

>[!WARNING] 
>**(Sender / TutorialHub dependency)**
>
> In these examples, `Result.Sender := TutorialHub;` is used because the tutorial relies on the `TTutorialHub` support class provided by `Anthropic.Tutorial.VCL` or `Anthropic.Tutorial.FMX`. This object is passed as the callback Sender so the helper procedures (such as `Display`... / `DisplayStream`...) can access the tutorial UI/log context.
>
> When integrating the library into your own project, you are not required to use `TTutorialHub`. You can set `Sender` to any object that makes sense for your application (for example, your main form, a view-model, a controller, or a custom context class), and implement your own display/log/cancellation handlers accordingly.
>
>You may also leave `Sender` as `nil`.

<br>


## 4) Asynchronous streaming with event callbacks

- Compared to session callbacks (which track the stream “as a whole”), this variant enables fine-grained interception per SSE event type (`message_start_event`, `content_block_delta`, `message_stop`, ...), which is ideal for precise control (UI, logging, instrumentation).

  ```pascal
  //uses Anthropic, Anthropic.Types, Anthropic.Helpers, Anthropic.Tutorial.VCL (*or Anthropic.Tutorial.FMX*);

  //  In the asynchronous case, increase the scope of Client by declaring it in FormCreate.
  //  var Client := TAnthropicFactory.CreateInstance(My_Anthropic_Key);

  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 16000;
  var ThinkingBudget := 6000;
  var Prompt := 'From which version of Delphi were multi-line strings introduced?';

  //JSON payload generation
  var Payload: TProc<TChatParams> :=
  procedure (Params: TChatParams)
  begin
    with Generation do
      Params
        .Model(ModelName)
        .MaxTokens(MaxTokens)
        .Messages( MessageParts
            .User( Prompt )
          )
        .Thinking( CreateThinkingConfig(True)
            .BudgetTokens(ThinkingBudget)
          )
        .Stream;
  end;

  var EventCallbacks := TEventEngineManagerFactory.CreateInstance(
    function : TStreamEventCallBack
    begin
      Result.Sender := TutorialHub;
      Result.OnMessageStart := DisplayMessageStart;
      Result.OnMessageDelta := DisplayMessageDelta;
      Result.OnMessageStop := DisplayMessageStop;
      Result.OnContentStart := DisplayContentStart;
      Result.OnContentDelta := DisplayContentDelta;
      Result.OnContentStop := DisplayContentStop;
      Result.OnError := DisplayStreamError;
    end);

  //Asynchronous promise example
  var Promise := Client.Chat.AsyncAwaitCreateStream(Payload, EventCallbacks);

  Promise
    .&Then(
      procedure (Value: TEventData)
      begin
        ShowMessage(Value.Thought);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  ```

<br>


## 5) Promises and orchestration: chaining asynchronous operations

The `AsyncAwait`... variants return a `TPromise<TEventData>`. Beyond streaming, this enables a readable processing pipeline:
- **`&Then`:** post-processing after resolution.
- **`&Catch`:** centralized error handling.
- Returning a new promise from a `&Then`: chaining asynchronous operations (pipeline).

<br>

- Example: chaining two asynchronous operations

  ```pascal
  // Asynchronous generation (promise-based)
  var Promise := Client.Chat.AsyncAwaitCreateStream(Payload, EventCallbacks);

  Promise
    .&Then(
      function (First: TEventData): TPromise<TEventData>
      begin
        // Second promise
        Result := Client.Chat.AsyncAwaitCreateStream(
          procedure (Params: TChatParams)
          begin
             with Generation do
               Params
                 .Model(ModelName)
                 .MaxTokens(MaxTokens)
                 .Messages( MessageParts
                     .User( 'Summarize the following answer: ' + First.Text )
                   )
                 .Stream;
          end);
      end)
    .&Then(
      procedure (Second: TEventData)
      begin
        ShowMessage('----- SUMMARY'#10 + Second.Text);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
  ``` 

>[!NOTE]
> Using `TPromise<TEventData>` requires adding the `Anthropic.Async.Promise` unit to the `uses` clause.


<br>

## Quick selection guide
- **Get to the point / quick debugging** → (1) Synchronous
- **Final result only** → (2) Async “mute”
- **Simple incremental rendering** → (3) Session callbacks
- **Fine-grained SSE event handling** → (4) Event callbacks
- **Chaining / orchestration** → (5) Promises + `&Then`

<br>

## Practical notes
- Make sure to call `.Stream` in your payload (otherwise you are not using SSE).
- Do not keep chunk objects beyond callbacks—copy required fields.
- Cancellation is cooperative:
  - driven via `OnDoCancel / OnCancellation`, and/or
  - relayed by the dispatcher through `IEventEngineManager`.
