unit Demo.Anthropic.Session.Transport;

interface

uses
  System.SysUtils, System.Classes,
  Anthropic;

type
  TSessionStreamEventProc = reference to procedure(const EventData: string);

  TSessionStreamTransport = class
  private
    FClient: IAnthropic;
    FWorker: TThread;
  public
    constructor Create(const AClient: IAnthropic);
    destructor Destroy; override;

    /// <summary>
    /// Opens the session event stream through the Anthropic SDK on a worker
    /// thread. <c>OnEvent</c> is invoked once per SSE event with its raw
    /// "data:" payload; <c>OnDone</c> is invoked once at the end with an empty
    /// string on success or the error message on failure.
    /// </summary>
    procedure Start(const SessionId: string;
      const OnEvent: TSessionStreamEventProc;
      const OnDone: TProc<string>);

    /// <summary>
    /// Requests cancellation of the in-flight stream. The HTTP read is
    /// aborted at the next received-data tick and OnDone fires normally.
    /// </summary>
    procedure Abort;
  end;

implementation

{$REGION 'Dev note'}
(*

  Demo-local stream bridge for Managed Agents session events.

  Why this exists
  ---------------
  The HTTP transport, SSE decoding, beta headers, monitoring and injected
  HTTP-client settings belong to the Anthropic SDK. The client timeout policy
  is configured when the demo creates the Anthropic service.

  Mechanism
  ---------
  This unit only owns the demo-level orchestration: it starts a worker thread,
  calls Sessions.Events.StreamEvents, maps SDK stream callbacks to the Pythia
  flow callbacks, and exposes Abort as a simple flag checked by the SDK stream.
  The worker is an explicit TThread descendant rather than an anonymous thread,
  so its lifetime is owned by the transport and can be joined during teardown.

  Threading
  ---------
  Start spawns a worker thread; OnEvent and OnDone fire ON THAT THREAD. The
  caller (Demo.Anthropic.Session.Flow) is responsible for marshalling any UI
  work to the main thread. The transport instance must outlive the stream:
  free it only after OnDone has fired during normal execution. Destroy cancels
  silently so teardown cannot post a late completion callback.

*)
{$ENDREGION}

type
  TSessionStreamWorker = class(TThread)
  private
    FClient: IAnthropic;
    FSessionId: string;
    FOnEvent: TSessionStreamEventProc;
    FOnDone: TProc<string>;
    FNotifyDone: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(const AClient: IAnthropic; const ASessionId: string;
      const AOnEvent: TSessionStreamEventProc; const AOnDone: TProc<string>);
    procedure Cancel(const NotifyDone: Boolean);
  end;

{ TSessionStreamWorker }

constructor TSessionStreamWorker.Create(const AClient: IAnthropic;
  const ASessionId: string; const AOnEvent: TSessionStreamEventProc;
  const AOnDone: TProc<string>);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FClient := AClient;
  FSessionId := ASessionId;
  FOnEvent := AOnEvent;
  FOnDone := AOnDone;
  FNotifyDone := True;
end;

procedure TSessionStreamWorker.Cancel(const NotifyDone: Boolean);
begin
  if not NotifyDone then
    FNotifyDone := False;
  Terminate;
end;

procedure TSessionStreamWorker.Execute;
begin
  var ErrorMsg := '';
  try
    FClient.Sessions.Events.StreamEvents(
      FSessionId,
      procedure(const Data: string; var Abort: Boolean)
      begin
        if Terminated then
          begin
            Abort := True;
            Exit;
          end;

        if Assigned(FOnEvent) then
          FOnEvent(Data);

        if Terminated then
          Abort := True;
      end,
      procedure(const Sender: TObject; AContentLength, AReadCount: Int64;
        var AAbort: Boolean)
      begin
        if Terminated then
          AAbort := True;
      end);
  except
    on E: Exception do
      if not Terminated then
        ErrorMsg := E.Message;
  end;

  if FNotifyDone and Assigned(FOnDone) then
    FOnDone(ErrorMsg);
end;

{ TSessionStreamTransport }

constructor TSessionStreamTransport.Create(const AClient: IAnthropic);
begin
  inherited Create;
  FClient := AClient;
end;

destructor TSessionStreamTransport.Destroy;
begin
  if Assigned(FWorker) then
    begin
      TSessionStreamWorker(FWorker).Cancel(False);
      FWorker.WaitFor;
      FWorker.Free;
    end;
  inherited;
end;

procedure TSessionStreamTransport.Abort;
begin
  if Assigned(FWorker) then
    TSessionStreamWorker(FWorker).Cancel(True);
end;

procedure TSessionStreamTransport.Start(const SessionId: string;
  const OnEvent: TSessionStreamEventProc; const OnDone: TProc<string>);
begin
  if not Assigned(FClient) then
    raise Exception.Create('Anthropic client is required for session streaming.');

  if Assigned(FWorker) then
    raise Exception.Create('Session stream transport is already started.');

  FWorker := TSessionStreamWorker.Create(FClient, SessionId, OnEvent, OnDone);
  FWorker.Start;
end;

end.
