unit Anthropic.Chat;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Threading,
  Anthropic.API, Anthropic.Types,
  Anthropic.Chat.Request, Anthropic.Chat.Responses,
  Anthropic.Chat.StreamEvents, Anthropic.Chat.StreamEngine, Anthropic.Chat.StreamCallbacks,
  Anthropic.Async.Support, Anthropic.Async.Params, Anthropic.Async.Promise;

type
  TAbstractSupport = class(TAnthropicAPIRoute)
  protected
    function Create(const ParamProc: TProc<TChatParams>): TChat; virtual; abstract;

    function CreateStream(
      const ParamProc: TProc<TChatParams>;
      const Event: TChatEvent;
      const StreamEvents: IEventEngineManager = nil): Boolean; virtual; abstract;

    function TokenCount(const ParamProc: TProc<TChatParams>): TTokenCount; virtual; abstract;
  end;

  TAsynchronousSupport = class(TAbstractSupport)
  protected
    procedure AsynCreate(ParamProc: TProc<TChatParams>; CallBacks: TFunc<TAsynChat>);

    procedure AsynCreateStream(ParamProc: TProc<TChatParams>;
      CallBacks: TFunc<TAsynChatStream>;
      const StreamEvents: IEventEngineManager = nil);

    procedure AsynTokenCount(ParamProc: TProc<TChatParams>; CallBacks: TFunc<TAsynTokenCount>);

    function AsyncAwaitCreateStream(
      const ParamProc: TProc<TChatParams>;
      const Callbacks: TFunc<TPromiseChatStream> = nil;
      const StreamEvents: IEventEngineManager = nil): TPromise<TEventData>; overload;
  end;

  TChatRoute = class(TAsynchronousSupport)
    /// <summary>
    /// Creates a chat message using the provided chat parameters.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method performs a blocking request to the chat messages endpoint and returns a fully
    /// materialized <c>TChat</c> instance.
    /// </para>
    /// <para>
    /// • The <c>ParamProc</c> callback is used to configure the <c>TChatParams</c> payload, including
    /// model selection, input messages, tools, and generation settings.
    /// </para>
    /// <para>
    /// • On success, the returned object contains the assistant message content, stop information, and
    /// usage metrics as provided by the API.
    /// </para>
    /// <para>
    /// • For non-blocking usage, prefer <c>AsynCreate</c> / <c>AsyncAwaitCreate</c> or the stream-based
    /// variants, depending on the desired consumption model.
    /// </para>
    /// </remarks>
    function Create(const ParamProc: TProc<TChatParams>): TChat; override;

    /// <summary>
    /// Creates a streaming chat request using the provided chat parameters.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method performs a blocking streaming request to the chat messages endpoint and emits
    /// incremental chat events as they are received.
    /// </para>
    /// <para>
    /// • The <c>ParamProc</c> callback is used to configure the <c>TChatParams</c> payload, including
    /// model selection, input messages, tools, and streaming options.
    /// </para>
    /// <para>
    /// • The <c>Event</c> callback is invoked for each <c>TChatStream</c> event produced by the server,
    /// including intermediate updates and the final completion signal.
    /// </para>
    /// <para>
    /// • The optional <c>StreamEvents</c> manager can be used to aggregate, dispatch, or coordinate
    /// stream events across multiple consumers.
    /// </para>
    /// <para>
    /// • The method returns <c>True</c> if the streaming request was successfully initiated and
    /// completed without transport-level errors; exceptions are raised on failure.
    /// </para>
    /// </remarks>
    function CreateStream(
      const ParamProc: TProc<TChatParams>;
      const Event: TChatEvent;
      const StreamEvents: IEventEngineManager = nil): Boolean; override;

    /// <summary>
    /// Computes the token usage for a chat request without generating a response.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method performs a blocking request to the token counting endpoint associated with chat
    /// messages and returns a <c>TTokenCount</c> result.
    /// </para>
    /// <para>
    /// • The <c>ParamProc</c> callback is used to configure the <c>TChatParams</c> payload, including
    /// input messages, system prompts, tools, and context management options.
    /// </para>
    /// <para>
    /// • No chat content is generated; the request is evaluated solely to determine token usage for
    /// the provided parameters.
    /// </para>
    /// <para>
    /// • For non-blocking usage, prefer <c>AsynTokenCount</c> or <c>AsyncAwaitTokenCount</c>.
    /// </para>
    /// </remarks>
    function TokenCount(const ParamProc: TProc<TChatParams>): TTokenCount; override;

    /// <summary>
    /// Creates a chat message using an async/await promise-based pattern.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method wraps the asynchronous callback-based chat creation workflow into a
    /// <c>TPromise&lt;TChat&gt;</c>, enabling async/await-style consumption.
    /// </para>
    /// <para>
    /// • The <c>ParamProc</c> callback is used to configure the <c>TChatParams</c> payload, in the same
    /// manner as synchronous and callback-based creation methods.
    /// </para>
    /// <para>
    /// • The optional <c>Callbacks</c> factory allows interception of lifecycle events such as start,
    /// progress, success, error, and cancellation, while still returning a promise.
    /// </para>
    /// <para>
    /// • The returned promise resolves with a fully materialized <c>TChat</c> instance or is rejected
    /// if an error or cancellation occurs.
    /// </para>
    /// </remarks>
    function AsyncAwaitCreate(
      const ParamProc: TProc<TChatParams>;
      const Callbacks: TFunc<TPromiseChat> = nil): TPromise<TChat>;

    /// <summary>
    /// Creates a streaming chat request using an async/await promise-based pattern.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method wraps the streaming chat workflow into a <c>TPromise&lt;TEventData&gt;</c>,
    /// allowing asynchronous consumption with async/await semantics.
    /// </para>
    /// <para>
    /// • The <c>ParamProc</c> callback is used to configure the <c>TChatParams</c> payload, including
    /// model selection, input messages, tools, and streaming options.
    /// </para>
    /// <para>
    /// • The optional <c>Callbacks</c> factory allows observation of stream lifecycle events while the
    /// aggregated stream data is returned through the promise result.
    /// </para>
    /// <para>
    /// • The returned promise resolves with the aggregated <c>TEventData</c> once the stream completes,
    /// or is rejected if an error or cancellation occurs.
    /// </para>
    /// </remarks>
    function AsyncAwaitCreateStream(
      const ParamProc: TProc<TChatParams>;
      const Callbacks: TFunc<TPromiseChatStream> = nil): TPromise<TEventData>; overload;

    /// <summary>
    /// Creates a streaming chat request using an async/await promise-based pattern and an event engine manager.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This overload wraps the streaming chat workflow into a <c>TPromise&lt;TEventData&gt;</c> and
    /// delegates stream aggregation and dispatching to the provided <c>IEventEngineManager</c>.
    /// </para>
    /// <para>
    /// • The <c>ParamProc</c> callback is used to configure the <c>TChatParams</c> payload, including
    /// model selection, input messages, tools, and streaming options.
    /// </para>
    /// <para>
    /// • The optional <c>StreamEvents</c> manager coordinates incremental stream events and produces
    /// the aggregated <c>TEventData</c> that the promise resolves with upon completion.
    /// </para>
    /// <para>
    /// • The returned promise resolves when the stream completes, or is rejected if an error or
    /// cancellation occurs.
    /// </para>
    /// </remarks>
    function AsyncAwaitCreateStream(
      const ParamProc: TProc<TChatParams>;
      const StreamEvents: IEventEngineManager): TPromise<TEventData>; overload;

    /// <summary>
    /// Computes token usage for a chat request using an async/await promise-based pattern.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method wraps the asynchronous token counting workflow into a
    /// <c>TPromise&lt;TTokenCount&gt;</c>, enabling async/await-style consumption.
    /// </para>
    /// <para>
    /// • The <c>ParamProc</c> callback is used to configure the <c>TChatParams</c> payload, including
    /// input messages, system prompts, tools, and context management options.
    /// </para>
    /// <para>
    /// • The optional <c>Callbacks</c> factory allows observation of lifecycle events such as start,
    /// success, error, and cancellation while still returning a promise.
    /// </para>
    /// <para>
    /// • The returned promise resolves with a <c>TTokenCount</c> result or is rejected if an error or
    /// cancellation occurs.
    /// </para>
    /// </remarks>
    function AsyncAwaitTokenCount(
      const ParamProc: TProc<TChatParams>;
      const Callbacks: TFunc<TPromiseTokenCount> = nil): TPromise<TTokenCount>;
  end;

implementation

uses
  Anthropic.API.SSEDecoder, Anthropic.API.Streams, Anthropic.API.JsonSafeReader;

{ TAsynchronousSupport }

function TAsynchronousSupport.AsyncAwaitCreateStream(
  const ParamProc: TProc<TChatParams>;
  const Callbacks: TFunc<TPromiseChatStream>;
  const StreamEvents: IEventEngineManager): TPromise<TEventData>;
begin
  Result := TPromise<TEventData>.Create(
    procedure(Resolve: TProc<TEventData>; Reject: TProc<Exception>)
    begin
      var Buffer := TEventData.Empty;

      AsynCreateStream(ParamProc,
        function : TAsynChatStream
        begin
          if Assigned(Callbacks) then
            Result.Sender := Callbacks.Sender;

          if Assigned(Callbacks) and Assigned(Callbacks.OnStart) then
            Result.OnStart := Callbacks.OnStart;

          Result.OnProgress :=
            procedure (Sender: TObject; Event: TChatStream)
            begin
              Buffer.Aggregate(Event, procedure
                begin
                  var Error := EmptyStr;
                  if Assigned(Callbacks) and Assigned(Callbacks.OnError) then
                    Error := Callbacks.OnError(Sender, Error);
                  Reject(Exception.Create(Error));
                end);

              if Assigned(Callbacks) and Assigned(Callbacks.OnProgress) then
                Callbacks.OnProgress(Sender, Event);
            end;

          Result.OnSuccess :=
            procedure (Sender: TObject)
            begin
              Resolve(Buffer);
            end;

          Result.OnError :=
            procedure (Sender: TObject; Error: string)
            begin
              if Assigned(Callbacks) and Assigned(Callbacks.OnError) then
                Error := Callbacks.OnError(Sender, Error);
              Reject(Exception.Create(Error));
            end;

          Result.OnDoCancel :=
            function : Boolean
            begin
              if Assigned(Callbacks) and Assigned(Callbacks.OnDoCancel) then
                Result := Callbacks.OnDoCancel()
              else
                Result := False;
            end;

          Result.OnCancellation :=
            procedure (Sender: TObject)
            begin
              var Error := 'aborted';
              if Assigned(Callbacks) and Assigned(Callbacks.OnCancellation) then
                Error := Callbacks.OnCancellation(Sender);
              Reject(Exception.Create(Error));
            end;
        end,
        StreamEvents);
    end);
end;

procedure TAsynchronousSupport.AsynCreate(ParamProc: TProc<TChatParams>;
  CallBacks: TFunc<TAsynChat>);
begin
  with TAsynCallBackExec<TAsynChat, TChat>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TChat
      begin
        Result := Self.Create(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynCreateStream(
  ParamProc: TProc<TChatParams>;
  CallBacks: TFunc<TAsynChatStream>;
  const StreamEvents: IEventEngineManager);
var
  Sender: TObject;
  OnCancellation: TProc<TObject>;
  OnDoCancel: TFunc<Boolean>;
begin
  var CallBackParams := TUseParamsFactory<TAsynChatStream>.CreateInstance(CallBacks);

  var OnStart := CallBackParams.Param.OnStart;
  var OnSuccess := CallBackParams.Param.OnSuccess;
  var OnProgress := CallBackParams.Param.OnProgress;
  var OnError := CallBackParams.Param.OnError;
  var CancelTag := 0;

  if Assigned(StreamEvents) then
    begin
      Sender := StreamEvents.GetStreamEventDispatcher.CallBacks.Sender;
      OnCancellation := StreamEvents.GetStreamEventDispatcher.CallBacks.OnCancellation;
      OnDoCancel := StreamEvents.GetStreamEventDispatcher.CallBacks.OnDoCancel;
    end
  else
    begin
      Sender := CallBackParams.Param.Sender;
      OnCancellation := CallBackParams.Param.OnCancellation;
      OnDoCancel := CallBackParams.Param.OnDoCancel;
    end;

  var Task: ITask := TTask.Create(
        procedure()
        begin
            {--- Pass the instance of the current class in case no value was specified. }
            if not Assigned(Sender) then
              Sender := Self;

            {--- Trigger OnStart callback }
            if Assigned(OnStart) then
              TThread.Queue(nil, procedure
                begin
                  OnStart(Sender);
                end);

            try
              var Stop := False;

              {--- Processing }
              CreateStream(ParamProc,
                procedure (var EventCalled: TChatStream; IsDone: Boolean; var Cancel: Boolean)
                begin
                  {--- Check that the process has not been canceled }
                  if Assigned(OnDoCancel) then
                    TThread.Queue(nil, procedure
                      begin
                        Stop := OnDoCancel();
                      end);

                  if Stop then
                    begin
                      {--- Trigger when processus was stopped }
                      if (CancelTag = 0) and Assigned(OnCancellation) then
                        TThread.Queue(nil, procedure
                          begin
                            OnCancellation(Sender)
                          end);

                      Inc(CancelTag);
                      Cancel := True;
                      Exit;
                    end;

                  if not IsDone and Assigned(EventCalled) then
                    begin
                      var LocalEvent := EventCalled;
                      EventCalled := nil;

                      {--- Triggered when processus is progressing }
                      if Assigned(OnProgress) then
                        TThread.Synchronize(nil, procedure
                          begin
                            try
                              OnProgress(Sender, LocalEvent);
                            finally
                              {--- Makes sure to release the instance containing the data obtained
                                   following processing}
                              LocalEvent.Free;
                            end;
                          end)
                      else
                       LocalEvent.Free;
                    end
                  else
                  if IsDone then
                    begin
                      {--- Trigger OnEnd callback when the process is done }
                      if Assigned(OnSuccess) then
                        TThread.Queue(nil, procedure
                          begin
                            OnSuccess(Sender);
                          end);
                    end;
                end,
              StreamEvents);

            except
              on E: Exception do
                begin
                  var Error := AcquireExceptionObject;
                  try
                    var ErrorMsg := (Error as Exception).Message;

                    {--- Trigger OnError callback if the process has failed }
                    if Assigned(OnError) then
                      TThread.Queue(nil, procedure
                        begin
                          OnError(Sender, ErrorMsg);
                        end);
                  finally
                    {--- Ensures that the instance of the caught exception is released}
                    Error.Free;
                  end;
                end;
            end;
        end);
  Task.Start;
end;

procedure TAsynchronousSupport.AsynTokenCount(ParamProc: TProc<TChatParams>;
  CallBacks: TFunc<TAsynTokenCount>);
begin
  with TAsynCallBackExec<TAsynTokenCount, TTokenCount>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TTokenCount
      begin
        Result := Self.TokenCount(ParamProc);
      end);
  finally
    Free;
  end;
end;

{ TChatRoute }

function TChatRoute.AsyncAwaitCreate(const ParamProc: TProc<TChatParams>;
  const Callbacks: TFunc<TPromiseChat>): TPromise<TChat>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TChat>(
    procedure(const CallbackParams: TFunc<TAsynChat>)
    begin
      Self.AsynCreate(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TChatRoute.AsyncAwaitCreateStream(const ParamProc: TProc<TChatParams>;
  const Callbacks: TFunc<TPromiseChatStream>): TPromise<TEventData>;
begin
  Result := Self.AsyncAwaitCreateStream(ParamProc, Callbacks, nil);
end;

function TChatRoute.AsyncAwaitCreateStream(const ParamProc: TProc<TChatParams>;
  const StreamEvents: IEventEngineManager): TPromise<TEventData>;
begin
  Result := Self.AsyncAwaitCreateStream(ParamProc, nil, StreamEvents);
end;

function TChatRoute.AsyncAwaitTokenCount(const ParamProc: TProc<TChatParams>;
  const Callbacks: TFunc<TPromiseTokenCount>): TPromise<TTokenCount>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TTokenCount>(
    procedure(const CallbackParams: TFunc<TAsynTokenCount>)
    begin
      Self.AsynTokenCount(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TChatRoute.Create(const ParamProc: TProc<TChatParams>): TChat;
begin
  Result := API.Post<TChat, TChatParams>('messages', ParamProc);
end;

function TChatRoute.CreateStream(
  const ParamProc: TProc<TChatParams>;
  const Event: TChatEvent;
  const StreamEvents: IEventEngineManager): Boolean;
var
  Response: TLockedMemoryStream;
  RetPos: Int64;
  Decoder: TSSEDecoder;
  DoneSent: Boolean;
  AbortFlag: Boolean;
  LocalEvent: IEventEngineManager;
  Buffer: TEventData;
begin
  if StreamEvents = nil then
    begin
      LocalEvent := TEventEngineManagerFactory.CreateInstance(
        function : TStreamEventCallBack begin end);
    end
  else
    LocalEvent := StreamEvents;

  Response := TLockedMemoryStream.Create;
  try
    RetPos := 0;
    DoneSent := False;
    AbortFlag := False;

    Decoder := TSSEDecoder.Create(
      procedure(const Data: string; var AAbort: Boolean)
      var
        Line: string;
        IsDone: Boolean;
        Content: TChatStream;
        EventType: string;
      begin
        Content := nil;

        if AAbort or DoneSent then
          Exit;

        Line := Data.Trim;

        {--- Reading the type from the raw JSON }
        EventType := TJsonReader.Parse(Line).AsString('type');
        if EventType.IsEmpty then
          Exit;

        IsDone := (EventType = 'message_stop');

        Content := nil;
        try
          Content := TApiDeserializer.Parse<TChatStream>(Line);

          if isDone then
            DoneSent := True;
        except
          Content := nil;
        end;

        try
          LocalEvent.AggregateStreamEvents(Content, Buffer);
          Event(Content, IsDone, AAbort);
        finally
          Content.Free;
        end;
      end
    );

    var Drain :=
      procedure(var Abort: Boolean)
      var
        Bytes: TBytes;
        Snap: Int64;
      begin
        Snap := RetPos;
        try
          while Response.ExtractDelta(RetPos, Bytes) do
            begin
              if Length(Bytes) = 0 then
                Continue;

              Decoder.Feed(Bytes, Abort);

              if Abort then
                Exit;
            end;
        except
          RetPos := Snap;
          raise;
        end;
      end;

    try
      Result := API.Post<TChatParams>(
        'messages',
        ParamProc,
        Response,
        procedure(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var AAbort: Boolean)
        begin
          if DoneSent then
            begin
              AAbort := True;
              Exit;
            end;

          Drain(AAbort);

          if AAbort then
            AbortFlag := True;
        end
      );
    finally
      if not DoneSent and not AbortFlag then
        begin
          Drain(AbortFlag);
          Decoder.Flush(AbortFlag);
        end;

      var LoacalContent: TChatStream := nil;
      if not DoneSent and not AbortFlag then
        begin
          DoneSent := True;
          Event(LoacalContent, True, AbortFlag);
        end;

      Decoder.Free;
      Drain := nil;
    end;

  finally
    Response.Free;
  end;
end;

function TChatRoute.TokenCount(const ParamProc: TProc<TChatParams>): TTokenCount;
begin
  Result := API.Post<TTokenCount, TChatParams>('messages/count_tokens', ParamProc);
end;

end.
