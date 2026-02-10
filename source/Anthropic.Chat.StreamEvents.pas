unit Anthropic.Chat.StreamEvents;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  REST.JsonReflect, REST.Json.Types,
  Anthropic.API, Anthropic.API.Params, Anthropic.Types, Anthropic.Chat.Responses,
  Anthropic.Exceptions, Anthropic.API.JsonSafeReader, Anthropic.Async.Support,
  Anthropic.Async.Promise, Anthropic.Chat.Beta;

type
  {$REGION 'error'}

  TErrorMessage = class
  private
    FType: string;
    FMessage: string;
  public
    property &Type: string read FType write FType;
    property Message: string read FMessage write FMessage;
  end;

  TRawErrorEvent = class
  private
    [JsonReflectAttribute(ctString, rtString, TEventTypeInterceptor)]
    FType: TEventType;
    FError: TErrorMessage;
  public
    property &Type: TEventType read FType write FType;
    property Error: TErrorMessage read FError write FError;

    destructor Destroy; override;
  end;

  {$ENDREGION}

  {$REGION 'message_start_event'}

  TRawMessageStartEvent = class
  private
    [JsonReflectAttribute(ctString, rtString, TEventTypeInterceptor)]
    FType: TEventType;
    FMessage: TMessage;
  public
    property &Type: TEventType read FType write FType;
    property Message: TMessage read FMessage write FMessage;

    destructor Destroy; override;
  end;

  {$ENDREGION}

  {$REGION 'content_block_start'}

  TRawContentBlockStartEvent = class
  private
    [JsonReflectAttribute(ctString, rtString, TEventTypeInterceptor)]
    FType: TEventType;
    FIndex: Int64;
    [JsonNameAttribute('content_block')]
    FContentBlock: TContentBlock;
  public
    property &Type: TEventType read FType write FType;
    property Index: Int64 read FIndex write FIndex;
    property ContentBlock: TContentBlock read FContentBlock write FContentBlock;

    destructor Destroy; override;
  end;

  {$ENDREGION}

  {$REGION 'content_block_delta'}

  TRawContentBlockDelta = class
  private
    [JsonReflectAttribute(ctString, rtString, TDeltaTypeInterceptor)]
    FType: TDeltaType;
    FIndex: Int64;
    FText: string;
    [JsonNameAttribute('partial_json')]
    FPartialJson: string;
    FThinking: string;
    FSignature: string;
    FCitationsDelta: TTextCitation;
  public
    property &Type: TDeltaType read FType write FType;
    property Index: Int64 read Findex write Findex;
    property Text: string read FText write FText;
    property PartialJson: string read FPartialJson write FPartialJson;
    property Signature: string read FSignature write FSignature;
    property Thinking: string read FThinking write FThinking;
    property CitationsDelta: TTextCitation read FCitationsDelta write FCitationsDelta;

    destructor Destroy; override;
  end;

  TRawContentBlockDeltaEvent = class
  private
    [JsonReflectAttribute(ctString, rtString, TEventTypeInterceptor)]
    FType: TEventType;
    FIndex: Int64;
    FDelta: TRawContentBlockDelta;
  public
    property &Type: TEventType read FType write FType;
    property Index: Int64 read FIndex write FIndex;
    property Delta: TRawContentBlockDelta read FDelta write FDelta;

    destructor Destroy; override;
  end;

  {$ENDREGION}

  {$REGION 'content_block_stop'}

  TRawContentBlockStopEvent = class
  private
    [JsonReflectAttribute(ctString, rtString, TEventTypeInterceptor)]
    FType: TEventType;
    FIndex: Int64;
  public
    property &Type: TEventType read FType write FType;
    property Index: Int64 read FIndex write FIndex;
  end;

  {$ENDREGION}

  {$REGION 'message_delta'}

  TDelta = class
  private
    [JsonReflectAttribute(ctString, rtString, TStopReasonInterceptor)]
    [JsonNameAttribute('stop_reason')]
    FStopReason: TStopReason;
    [JsonNameAttribute('stop_sequence')]
    FStopSequence: string;
  public
    property StopReason: TStopReason read FStopReason write FStopReason;
    property StopSequence: string read FStopSequence write FStopSequence;
  end;

  TRawMessageDeltaEvent = class
  private
    [JsonReflectAttribute(ctString, rtString, TEventTypeInterceptor)]
    FType: TEventType;
    FDelta: TDelta;
    FUsage: TUsage;
  public
    property &Type: TEventType read FType write FType;
    property Delta: TDelta read FDelta write FDelta;
    property Usage: TUsage read FUsage write FUsage;

    Destructor Destroy; override;
  end;

  {$ENDREGION}

  {$REGION 'message_stop'}

  TRawMessageStopEvent = class
  private
    [JsonReflectAttribute(ctString, rtString, TEventTypeInterceptor)]
    FType: TEventType;
  public
    property &Type: TEventType read FType write FType;
  end;

  {$ENDREGION}

  TStreamEvent = class(TJSONFingerprint)
  private
    [JSONMarshalled(False)] FError: TRawErrorEvent;
    [JSONMarshalled(False)] FMessageStart: TRawMessageStartEvent;
    [JSONMarshalled(False)] FMessageDelta: TRawMessageDeltaEvent;
    [JSONMarshalled(False)] FMessageStop: TRawMessageStopEvent;
    [JSONMarshalled(False)] FContentBlockStart: TRawContentBlockStartEvent;
    [JSONMarshalled(False)] FContentBlockDelta: TRawContentBlockDeltaEvent;
    [JSONMarshalled(False)] FContentBlockStop: TRawContentBlockStopEvent;
    [JSONMarshalled(False)] FEventType: TEventType;
    function GetContentBlock: TContentBlock;

  protected
    /// <summary>
    /// Builds and routes a stream event to its strongly typed internal representation.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method is responsible for interpreting the raw streaming JSON payload and constructing
    /// the appropriate event-specific object graph (for example, content_block_start, content_block_delta,
    /// message_start, message_delta, and message_stop).
    /// </para>
    /// <para>
    /// • It is typically invoked as part of the post-deserialization lifecycle and relies on the
    /// JSONResponse property to parse and bind the event payload to the corresponding internal fields.
    /// </para>
    /// <para>
    /// • This method is not intended to be called directly by user code and should be considered an
    /// internal implementation detail of the streaming deserialization process.
    /// </para>
    procedure StreamEventBuilder; override;

    /// <summary>
    /// Executes internal post-deserialization processing for the current instance.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method serves as a protected lifecycle hook that is invoked after the JSON payload has
    /// been fully deserialized, bound, and the JSONResponse property assigned.
    /// </para>
    /// <para>
    /// • It allows derived classes to perform additional internal initialization steps such as invoking
    /// ContentUpdate, StreamEventBuilder, or other class-specific post-processing logic.
    /// </para>
    /// <para>
    /// • This method is not intended to be called directly by user code and should be considered an
    /// internal extension point of the deserialization infrastructure.
    /// </para>
    /// </remarks>
    procedure AfterDeserialize; override;

  public
    property EventType: TEventType read FEventType write FEventType;

    /// <summary>
    /// Represents an error event received from the Anthropic streaming API.
    /// </summary>
    /// <remarks>
    /// This property is populated when the stream event type is "error". It contains the structured error
    /// information returned by the API. Generic JSON shape: {"type":"error","error":{...}} Error events
    /// may be sent instead of a normal HTTP error response when using streaming mode (Server-Sent Events).
    /// </remarks>
    property Error: TRawErrorEvent read FError write FError;

    /// <summary>
    /// Represents a message start event received from the Anthropic streaming API.
    /// </summary>
    /// <remarks>
    /// This property is populated when the stream event type is "message_start". It contains the initial
    /// Message object with empty content. Generic JSON shape: {"type":"message_start","message":{...}}
    /// </remarks>
    property MessageStart: TRawMessageStartEvent read FMessageStart write FMessageStart;

    /// <summary>
    /// Represents a message delta event received from the Anthropic streaming API.
    /// </summary>
    /// <remarks>
    /// This property is populated when the stream event type is "message_delta". It contains incremental
    /// updates applied to the top-level Message object, such as stop reason or usage information.
    /// Generic JSON shape: {"type":"message_delta","delta":{...}}
    /// </remarks>
    property MessageDelta: TRawMessageDeltaEvent read FMessageDelta write FMessageDelta;

    /// <summary>
    /// Represents a message stop event received from the Anthropic streaming API.
    /// </summary>
    /// <remarks>
    /// This property is populated when the stream event type is "message_stop". It indicates that the
    /// current streamed message has completed and no further events will be emitted for this message.
    /// Generic JSON shape: {"type":"message_stop"}
    /// </remarks>
    property MessageStop: TRawMessageStopEvent read FMessageStop write FMessageStop;

    /// <summary>
    /// Represents a content block start event received from the Anthropic streaming API.
    /// </summary>
    /// <remarks>
    /// This property is populated when the stream event type is "content_block_start". It marks the
    /// beginning of a new content block at the specified index within the message content array.
    /// Generic JSON shape: {"type":"content_block_start","index":0,"content_block":{...}}
    /// </remarks>
    property ContentBlockStart: TRawContentBlockStartEvent read FContentBlockStart write FContentBlockStart;

    /// <summary>
    /// Represents a content block delta event received from the Anthropic streaming API.
    /// </summary>
    /// <remarks>
    /// This property is populated when the stream event type is "content_block_delta". It contains
    /// incremental updates applied to the content block at the specified index.
    /// Generic JSON shape: {"type":"content_block_delta","index":0,"delta":{...}}
    /// </remarks>
    property ContentBlockDelta: TRawContentBlockDeltaEvent read FContentBlockDelta write FContentBlockDelta;

    /// <summary>
    /// Represents a content block stop event received from the Anthropic streaming API.
    /// </summary>
    /// <remarks>
    /// This property is populated when the stream event type is "content_block_stop". It marks the
    /// completion of the content block at the specified index.
    /// Generic JSON shape: {"type":"content_block_stop","index":0}
    /// </remarks>
    property ContentBlockStop: TRawContentBlockStopEvent read FContentBlockStop write FContentBlockStop;

    /// <summary>
    /// Gets the content block associated with the current stream event.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • <c>ContentBlock</c> exposes the <c>TContentBlock</c> instance carried by the current stream event,
    /// when the event type represents a content-related update.
    /// </para>
    /// <para>
    /// • Depending on the underlying stream event kind, this property may represent a content block
    /// start, delta, or stop, or may be <c>nil</c> if the event does not carry content data.
    /// </para>
    /// <para>
    /// • The returned value is derived from the event payload and should be treated as read-only and
    /// transient, reflecting the incremental nature of streaming responses.
    /// </para>
    /// <para>
    /// • Use this property when handling <c>content_block_*</c> events to access the structured content
    /// emitted by the streaming chat endpoint.
    /// </para>
    /// </remarks>
    property ContentBlock: TContentBlock read GetContentBlock;

    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents a single event emitted by the streaming chat endpoint.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TChatStream</c> is a chat-specific specialization of <c>TStreamEvent</c> that does not introduce
  /// additional members and exists primarily as a strongly typed marker for chat streaming.
  /// </para>
  /// <para>
  /// • It provides a self-descriptive event type for generic streaming helpers, such as callback- or
  /// promise-based consumers, while preserving a clean and explicit public API.
  /// </para>
  /// <para>
  /// • All event parsing, payload access, and typed projections (for example <c>message_start</c>,
  /// <c>message_delta</c>, <c>message_stop</c>, <c>content_block_start</c>, <c>content_block_delta</c>,
  /// <c>content_block_stop</c>, and <c>error</c>) are inherited from <c>TStreamEvent</c>.
  /// </para>
  /// <para>
  /// • Use <c>TChatStream</c> as the streamed item type when consuming chat responses incrementally,
  /// typically via <c>TAsyncStreamCallback&lt;TChatStream&gt;</c> or equivalent abstractions.
  /// </para>
  /// </remarks>
  TChatStream = class(TStreamEvent);

  /// <summary>
  /// Defines an asynchronous callback handler for streaming chat events.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TAsynChatStream</c> is a type alias for <c>TAsynStreamCallBack&lt;TChatStream&gt;</c>, specialized
  /// for consuming chat-related stream events.
  /// </para>
  /// <para>
  /// • It binds the generic asynchronous streaming infrastructure to the chat domain by fixing the
  /// streamed event type to <c>TChatStream</c>.
  /// </para>
  /// <para>
  /// • This alias improves readability and intent expression in public APIs while preserving the full
  /// behavior and lifecycle semantics of <c>TAsynStreamCallBack&lt;TChatStream&gt;</c>.
  /// </para>
  /// <para>
  /// • Use <c>TAsynChatStream</c> when registering callbacks that handle incremental chat responses
  /// emitted by the streaming chat endpoint.
  /// </para>
  /// </remarks>
  TAsynChatStream = TAsynStreamCallBack<TChatStream>;

  /// <summary>
  /// Defines a promise-based callback handler for streaming chat events.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TPromiseChatStream</c> is a type alias for <c>TPromiseStreamCallBack&lt;TChatStream&gt;</c>,
  /// specialized for consuming chat-related stream events.
  /// </para>
  /// <para>
  /// • It adapts the generic promise-based streaming infrastructure to the chat domain by fixing the
  /// streamed event type to <c>TChatStream</c>.
  /// </para>
  /// <para>
  /// • This alias improves API clarity by explicitly expressing promise-based consumption semantics
  /// while preserving the complete behavior of <c>TPromiseStreamCallBack&lt;TChatStream&gt;</c>.
  /// </para>
  /// <para>
  /// • Use <c>TPromiseChatStream</c> when consuming incremental chat responses through promise-oriented
  /// abstractions instead of direct callbacks.
  /// </para>
  /// </remarks>
  TPromiseChatStream = TPromiseStreamCallBack<TChatStream>;

  /// <summary>
  /// Represents a callback procedure used during the reception of responses from a chat request in streaming mode.
  /// </summary>
  /// <param name="Chat">
  /// The <c>TChat</c> object containing the current information about the response generated by the model.
  /// If this value is <c>nil</c>, it indicates that the data stream is complete.
  /// </param>
  /// <param name="IsDone">
  /// A boolean flag indicating whether the streaming process is complete.
  /// If <c>True</c>, it means the model has finished sending all response data.
  /// </param>
  /// <param name="Cancel">
  /// A boolean flag that can be set to <c>True</c> within the callback to cancel the streaming process.
  /// If set to <c>True</c>, the streaming will be terminated immediately.
  /// </param>
  /// <remarks>
  /// This callback is invoked multiple times during the reception of the response data from the model.
  /// It allows for real-time processing of received messages and interaction with the user interface or other systems
  /// based on the state of the data stream.
  /// When the <c>IsDone</c> parameter is <c>True</c>, it indicates that the model has finished responding,
  /// and the <c>Chat</c> parameter will be <c>nil</c>.
  /// </remarks>
  TChatEvent = reference to procedure(var Chat: TChatStream; IsDone: Boolean; var Cancel: Boolean);

  TSessionCallbacks = TFunc<TPromiseChat>;

  TSessionCallbacksStream = TFunc<TPromiseChatStream>;

implementation

{ TRawErrorEvent }

destructor TRawErrorEvent.Destroy;
begin
  if Assigned(FError) then
    FError.Free;
  inherited;
end;

{ TRawMessageStartEvent }

destructor TRawMessageStartEvent.Destroy;
begin
  if Assigned(FMessage) then
    FMessage.Free;
  inherited;
end;

{ TRawContentBlockStartEvent }

destructor TRawContentBlockStartEvent.Destroy;
begin
  if Assigned(FContentBlock) then
    FContentBlock.Free;
  inherited;
end;

{ TRawContentBlockDeltaEvent }

destructor TRawContentBlockDeltaEvent.Destroy;
begin
  if Assigned(FDelta) then
    FDelta.Free;
  inherited;
end;

{ TRawContentBlockDelta }

destructor TRawContentBlockDelta.Destroy;
begin
  if Assigned(FCitationsDelta) then
    FCitationsDelta.Free;
  inherited;
end;

{ TRawMessageDeltaEvent }

destructor TRawMessageDeltaEvent.Destroy;
begin
  if Assigned(FDelta) then
    FDelta.Free;
  if Assigned(FUsage) then
    FUsage.Free;
  inherited;
end;

{ TStreamEvent }

procedure TStreamEvent.AfterDeserialize;
begin
  inherited;
  StreamEventBuilder;
end;

constructor TStreamEvent.Create;
begin
  inherited Create;
  FError := TRawErrorEvent.Create;
  FMessageStart := TRawMessageStartEvent.Create;
  FMessageDelta := TRawMessageDeltaEvent.Create;
  FMessageStop := TRawMessageStopEvent.Create;
  FContentBlockStart := TRawContentBlockStartEvent.Create;
  FContentBlockDelta := TRawContentBlockDeltaEvent.Create;
  FContentBlockStop := TRawContentBlockStopEvent.Create;
end;

destructor TStreamEvent.Destroy;
begin
  if Assigned(FError) then
    FError.Free;
  if Assigned(FMessageStart) then
    FMessageStart.Free;
  if Assigned(FMessageDelta) then
    FMessageDelta.Free;
  if Assigned(FMessageStop) then
    FMessageStop.Free;
  if Assigned(FContentBlockStart) then
    FContentBlockStart.Free;
  if Assigned(FContentBlockDelta) then
    FContentBlockDelta.Free;
  if Assigned(FContentBlockStop) then
    FContentBlockStop.Free;
  inherited;
end;

function TStreamEvent.GetContentBlock: TContentBlock;
begin
  Result := nil;

  if Assigned(ContentBlockStart) and Assigned(ContentBlockStart.ContentBlock) then
    Exit(ContentBlockStart.ContentBlock);
end;

procedure TStreamEvent.StreamEventBuilder;
{$REGION 'Dev note'}
(*
    # Simulating "union types" for Anthropic streaming events (Delphi)

    The Anthropic streaming API sends multiple JSON shapes over the same channel.
    Each SSE frame contains a "type" discriminator (e.g. "message_start", "content_block_delta", etc.)
    and a payload whose structure depends on that type. In languages with native union / sum types
    (TypeScript, Rust, etc.), this is modeled as a discriminated union.

    Delphi does not provide a convenient, native way to express a "union of classes" where one variable
    can safely represent one of many unrelated DTO classes based on a discriminator. To keep the public
    API simple and strongly typed, this unit implements a manual discriminated-union pattern.

    Approach:
    ---------

    • TStreamEvent holds one field per possible event DTO (Error, MessageStart, ContentBlockStart, ...).
    • StreamEventBuilder reads JSONResponse["type"], maps it to TEventType, and deserializes JSONResponse
      into the matching DTO class.
    • The consumer code uses EventType (typically in a case statement) to select the relevant property.
      This keeps event handling explicit and avoids RTTI/reflection-heavy designs.


    Why this design:
    ----------------

    • Strong typing at the call site: handlers can access TRawMessageStartEvent, TRawContentBlockDeltaEvent,
      etc., without manual JSON parsing.
    • Pragmatic safety: properties exist on the object, avoiding nil access crashes if a developer
      accidentally references the wrong branch (the field is at least instantiated, even if not the
      active branch for the current event).
    • Minimal boilerplate: no complex generic factories or variant records with interface indirection.


    Notes:
    ------

    • The active branch for the current JSON payload is indicated by EventType.
    • "ping" events intentionally carry no additional payload.
    • This pattern is effectively a "discriminated union" implemented via composition + explicit routing.
    • New event types may be introduced by the API; unknown types should be handled gracefully upstream.
*)
{$ENDREGION}
begin
  inherited;

  var LocalType := TJsonReader.Parse(JSONResponse).AsString('type');

  EventType := TEventType.Parse(LocalType);
  case EventType of
    TEventType.ping: ;

    (* {"type": "error", "error": {...}} *)
    TEventType.error:
      begin
        FError.Free;
        FError := TApiDeserializer.Parse<TRawErrorEvent>(JSONResponse);
      end;

    (* {"type": "message_start", "message": {...}} *)
    TEventType.message_start:
      begin
        FMessageStart.Free;
        FMessageStart := TApiDeserializer.Parse<TRawMessageStartEvent>(JSONResponse);
      end;

    (* {"type": "content_block_start", "index": 0, "content_block": {...}} *)
    TEventType.content_block_start:
      begin
        FContentBlockStart.Free;
        FContentBlockStart := TApiDeserializer.Parse<TRawContentBlockStartEvent>(JSONResponse);

        {--- Post-hydration of polymorphic "content_block.content" (string/object/array)
             Safe: no-op if content_block.content is absent (e.g. server_tool_use) }
        var Root := TJsonReader.Parse(JSONResponse);

        if Assigned(FContentBlockStart) and Assigned(FContentBlockStart.ContentBlock) then
          FContentBlockStart.ContentBlock.HydratePolymorphicContent(Root, 'content_block');
      end;

    (* {"type":"content_block_delta","index":0,"delta":{"type":"text_delta",...}} *)
    TEventType.content_block_delta:
      begin
        FContentBlockDelta.Free;
        FContentBlockDelta := TApiDeserializer.Parse<TRawContentBlockDeltaEvent>(JSONResponse);
      end;

    (* {"type": "content_block_stop", "index": 0} *)
    TEventType.content_block_stop:
      begin
        FContentBlockStop.Free;
        FContentBlockStop := TApiDeserializer.Parse<TRawContentBlockStopEvent>(JSONResponse);
      end;

    (* {"type": "message_delta", "delta": {"stop_reason": "end_turn"...}, "usage": {...}} *)
    TEventType.message_delta:
      begin
        FMessageDelta.Free;
        FMessageDelta := TApiDeserializer.Parse<TRawMessageDeltaEvent>(JSONResponse);
      end;

    (* {"type": "message_stop"} *)
    TEventType.message_stop:
      begin
        FMessageStop.Free;
        FMessageStop := TApiDeserializer.Parse<TRawMessageStopEvent>(JSONResponse);
      end;
  end;
end;

end.
