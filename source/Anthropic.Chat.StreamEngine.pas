unit Anthropic.Chat.StreamEngine;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiGemini
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes,
  REST.JsonReflect, REST.Json.Types,
  Anthropic.API.Params, Anthropic.Types, Anthropic.Chat.Responses,
  Anthropic.Async.Support, Anthropic.Async.Promise,
  Anthropic.Chat.StreamEvents, Anthropic.Chat.StreamCallbacks;

type
  {$REGION 'Interfaces'}

  IStreamEventHandler = interface
    function CanHandle(EventType: TEventType): Boolean;

    function Handle(const Chunk: TChatStream; var Buffer: TEventData): Boolean;
  end;

  IEventEngineManager = interface
    ['{5F887D14-1654-40CA-8903-43165CF95AC3}']
    function AggregateStreamEvents(const Chunk: TChatStream; var Buffer: TEventData): Boolean;

    function GetStreamEventDispatcher: IStreamEventDispatcher;
  end;

  TEventEngineManagerFactory = class
    class function CreateInstance(const CallBacks: TFunc<TStreamEventCallBack> = nil): IEventEngineManager;
  end;

  {$ENDREGION}

  {$REGION 'Execution engine'}

  TEventExecutionEngine = class
  private
    FHandlers: TArray<IStreamEventHandler>;
  public
    procedure RegisterHandler(AHandler: IStreamEventHandler);
    function AggregateStreamEvents(const Chunk: TChatStream;
      var Buffer: TEventData): Boolean;
  end;

  TEventEngineManager = class(TInterfacedObject, IEventEngineManager)
  private
    FEngine: TEventExecutionEngine;
    FDispatcher: IStreamEventDispatcher;
    procedure EventExecutionEngineInitialize;
    function GetStreamEventDispatcher: IStreamEventDispatcher;
  public
    constructor Create(const ADispatcher: IStreamEventDispatcher = nil);
    function AggregateStreamEvents(const Chunk: TChatStream; var Buffer: TEventData): Boolean;
    destructor Destroy; override;
  end;

  {$ENDREGION}

  {$REGION 'Handlers events'}

  TMessageStart = class(TInterfacedObject, IStreamEventHandler)
    function CanHandle(EventType: TEventType): Boolean;
    function Handle(const Chunk: TChatStream; var Buffer: TEventData): Boolean;
  end;

  TMessageDelta = class(TInterfacedObject, IStreamEventHandler)
    function CanHandle(EventType: TEventType): Boolean;
    function Handle(const Chunk: TChatStream; var Buffer: TEventData): Boolean;
  end;

  TMessageStop = class(TInterfacedObject, IStreamEventHandler)
    function CanHandle(EventType: TEventType): Boolean;
    function Handle(const Chunk: TChatStream; var Buffer: TEventData): Boolean;
  end;

  TContentStart = class(TInterfacedObject, IStreamEventHandler)
    function CanHandle(EventType: TEventType): Boolean;
    function Handle(const Chunk: TChatStream; var Buffer: TEventData): Boolean;
  end;

  TContentDelta = class(TInterfacedObject, IStreamEventHandler)
    function CanHandle(EventType: TEventType): Boolean;
    function Handle(const Chunk: TChatStream; var Buffer: TEventData): Boolean;
  end;

  TContentStop = class(TInterfacedObject, IStreamEventHandler)
    function CanHandle(EventType: TEventType): Boolean;
    function Handle(const Chunk: TChatStream; var Buffer: TEventData): Boolean;
  end;

  TErrorEvent = class(TInterfacedObject, IStreamEventHandler)
    function CanHandle(EventType: TEventType): Boolean;
    function Handle(const Chunk: TChatStream; var Buffer: TEventData): Boolean;
  end;

  {$ENDREGION}


implementation

{ TEventEngineManagerFactory }

class function TEventEngineManagerFactory.CreateInstance(
  const CallBacks: TFunc<TStreamEventCallBack>): IEventEngineManager;
begin
  Result := TEventEngineManager.Create(TStreamEventDispatcher.Create(CallBacks));
end;

{ TEventExecutionEngine }

function TEventExecutionEngine.AggregateStreamEvents(const Chunk: TChatStream;
  var Buffer: TEventData): Boolean;
begin
  if not Assigned(Chunk) then
    Exit(True);

  var EventType := Chunk.EventType;

  for var Handler in FHandlers do
    begin
      if Handler.CanHandle(EventType) then
        begin
          Result := Handler.Handle(Chunk, Buffer);
          Exit;
        end;
    end;

  Result := True;
end;

procedure TEventExecutionEngine.RegisterHandler(AHandler: IStreamEventHandler);
begin
  FHandlers := FHandlers + [AHandler];
end;

{ TEventEngineManager }

function TEventEngineManager.AggregateStreamEvents(const Chunk: TChatStream;
  var Buffer: TEventData): Boolean;
begin
  if not Assigned(Chunk) then
    Exit(True);

  Result := FEngine.AggregateStreamEvents(Chunk, Buffer);

  if Assigned(FDispatcher) then
    FDispatcher.DispatchEvent(Chunk.EventType, Buffer);
end;

constructor TEventEngineManager.Create(
  const ADispatcher: IStreamEventDispatcher);
begin
  inherited Create;
  FDispatcher := ADispatcher;
  EventExecutionEngineInitialize;
end;

destructor TEventEngineManager.Destroy;
begin
  FEngine.Free;
  inherited;
end;

procedure TEventEngineManager.EventExecutionEngineInitialize;
begin
  {--- NOTE: TEventEngineManager is a singleton }
  FEngine := TEventExecutionEngine.Create;
  FEngine.RegisterHandler(TMessageStart.Create);
  FEngine.RegisterHandler(TMessageDelta.Create);
  FEngine.RegisterHandler(TMessageStop.Create);
  FEngine.RegisterHandler(TContentStart.Create);
  FEngine.RegisterHandler(TContentDelta.Create);
  FEngine.RegisterHandler(TContentStop.Create);
  FEngine.RegisterHandler(TErrorEvent.Create);
end;

function TEventEngineManager.GetStreamEventDispatcher: IStreamEventDispatcher;
begin
  Result := FDispatcher;
end;

{ TErrorEvent }

function TErrorEvent.CanHandle(EventType: TEventType): Boolean;
begin
  Result := EventType = TEventType.error;
end;

function TErrorEvent.Handle(const Chunk: TChatStream;
  var Buffer: TEventData): Boolean;
begin
  Result := False;
  Buffer.Aggregate(Chunk);
end;

{ TMessageStart }

function TMessageStart.CanHandle(EventType: TEventType): Boolean;
begin
  Result := EventType = TEventType.message_start;
end;

function TMessageStart.Handle(const Chunk: TChatStream;
  var Buffer: TEventData): Boolean;
begin
  Result := True;

  Buffer := Default(TEventData);
  Buffer.Aggregate(Chunk);
end;

{ TMessageDelta }

function TMessageDelta.CanHandle(EventType: TEventType): Boolean;
begin
  Result := EventType = TEventType.message_delta;
end;

function TMessageDelta.Handle(const Chunk: TChatStream;
  var Buffer: TEventData): Boolean;
begin
  Result := True;
  Buffer.Aggregate(Chunk);
end;

{ TMessageStop }

function TMessageStop.CanHandle(EventType: TEventType): Boolean;
begin
  Result := EventType = TEventType.message_stop;
end;

function TMessageStop.Handle(const Chunk: TChatStream;
  var Buffer: TEventData): Boolean;
begin
  Result := True;
  Buffer.Aggregate(Chunk);
end;

{ TContentStart }

function TContentStart.CanHandle(EventType: TEventType): Boolean;
begin
  Result := EventType = TEventType.content_block_start;
end;

function TContentStart.Handle(const Chunk: TChatStream;
  var Buffer: TEventData): Boolean;
begin
  Result := True;
  Buffer.Aggregate(Chunk);
end;

{ TContentDelta }

function TContentDelta.CanHandle(EventType: TEventType): Boolean;
begin
  Result := EventType = TEventType.content_block_delta;
end;

function TContentDelta.Handle(const Chunk: TChatStream;
  var Buffer: TEventData): Boolean;
begin
  var CanContinue := False;

  Buffer.Aggregate(Chunk, procedure
    begin
      CanContinue := False;
    end);

  Result := CanContinue;
end;

{ TContentStop }

function TContentStop.CanHandle(EventType: TEventType): Boolean;
begin
  Result := EventType = TEventType.content_block_stop;
end;

function TContentStop.Handle(const Chunk: TChatStream;
  var Buffer: TEventData): Boolean;
begin
  Result := True;
  Buffer.Aggregate(Chunk);
end;

end.
