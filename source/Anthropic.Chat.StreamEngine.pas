unit Anthropic.Chat.StreamEngine;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
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
    FBlockTypes: TDictionary<Int64, TContentBlockType>;
    procedure EventExecutionEngineInitialize;
    function GetStreamEventDispatcher: IStreamEventDispatcher;

    {--- Typed routing helpers }
    function PrepareTypedRouting(const Chunk: TChatStream;
      var Buffer: TEventData; out ABlockType: TContentBlockType;
      out AIndex: Int64): Boolean;
    procedure DispatchTyped(AEventType: TEventType;
      ABlockType: TContentBlockType; const Buffer: TEventData);
    function FindToolCallByIndex(const Buffer: TEventData;
      const AIndex: Int64; out ASnapshot: TToolCallSnapshot): Boolean;
    function FindToolResultByIndex(const Buffer: TEventData;
      const AIndex: Int64; out ASnapshot: TToolResultSnapshot): Boolean;
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

const
  TOOL_USE_BLOCKS: set of TContentBlockType = [
    TContentBlockType.tool_use,
    TContentBlockType.server_tool_use,
    TContentBlockType.mcp_tool_use
  ];

  TOOL_RESULT_BLOCKS: set of TContentBlockType = [
    TContentBlockType.web_search_tool_result,
    TContentBlockType.web_fetch_tool_result,
    TContentBlockType.advisor_tool_result,
    TContentBlockType.code_execution_tool_result,
    TContentBlockType.bash_code_execution_tool_result,
    TContentBlockType.text_editor_code_execution_tool_result,
    TContentBlockType.tool_search_tool_result,
    TContentBlockType.mcp_tool_result
  ];

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
var
  BlockType: TContentBlockType;
  BlockIndex: Int64;
  HasTypedRouting: Boolean;
begin
  if not Assigned(Chunk) then
    Exit(True);

  {--- Reset the per-stream Index → BlockType map on message_start so a
       reused manager does not carry stale entries between turns. }
  if Chunk.EventType = TEventType.message_start then
    FBlockTypes.Clear;

  {--- Pre-handler typed routing: inform Buffer of the active block so its
       Aggregate method routes deltas to the typed fields. }
  HasTypedRouting := PrepareTypedRouting(Chunk, Buffer, BlockType, BlockIndex);

  {--- Run the legacy handler chain (text/thought aggregation, etc.). }
  Result := FEngine.AggregateStreamEvents(Chunk, Buffer);

  {--- Legacy dispatch (OnContentStart / OnContentDelta / OnContentStop, etc.).
       Identical to pre-Step-2 behavior. }
  if Assigned(FDispatcher) then
    FDispatcher.DispatchEvent(Chunk.EventType, Buffer);

  {--- Typed dispatch (OnAssistantTextDelta / OnToolUseStart / …). Only fires
       when the active block type is known. Callbacks that were not provided
       by the consumer are no-ops at the dispatcher level. }
  if Assigned(FDispatcher) and HasTypedRouting then
    DispatchTyped(Chunk.EventType, BlockType, Buffer);

  {--- Drop the entry after content_block_stop so closed indices do not leak. }
  if (Chunk.EventType = TEventType.content_block_stop) and HasTypedRouting then
    FBlockTypes.Remove(BlockIndex);
end;

constructor TEventEngineManager.Create(
  const ADispatcher: IStreamEventDispatcher);
begin
  inherited Create;
  FDispatcher := ADispatcher;
  FBlockTypes := TDictionary<Int64, TContentBlockType>.Create;
  EventExecutionEngineInitialize;
end;

destructor TEventEngineManager.Destroy;
begin
  FEngine.Free;
  FBlockTypes.Free;
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

function TEventEngineManager.PrepareTypedRouting(const Chunk: TChatStream;
  var Buffer: TEventData; out ABlockType: TContentBlockType;
  out AIndex: Int64): Boolean;
begin
  Result := False;
  ABlockType := Default(TContentBlockType);
  AIndex := 0;

  case Chunk.EventType of
    TEventType.content_block_start:
      begin
        if not Assigned(Chunk.ContentBlockStart)
           or not Assigned(Chunk.ContentBlockStart.ContentBlock) then
          Exit;
        AIndex := Chunk.ContentBlockStart.Index;
        ABlockType := Chunk.ContentBlockStart.ContentBlock.&Type;
        FBlockTypes.AddOrSetValue(AIndex, ABlockType);
        Buffer.SetActiveBlock(ABlockType, AIndex);
        Result := True;
      end;

    TEventType.content_block_delta:
      begin
        if not Assigned(Chunk.ContentBlockDelta) then
          Exit;
        AIndex := Chunk.ContentBlockDelta.Index;
        if not FBlockTypes.TryGetValue(AIndex, ABlockType) then
          Exit;
        Buffer.SetActiveBlock(ABlockType, AIndex);
        Result := True;
      end;

    TEventType.content_block_stop:
      begin
        if not Assigned(Chunk.ContentBlockStop) then
          Exit;
        AIndex := Chunk.ContentBlockStop.Index;
        if not FBlockTypes.TryGetValue(AIndex, ABlockType) then
          Exit;
        Buffer.SetActiveBlock(ABlockType, AIndex);
        Result := True;
      end;
  end;
end;

procedure TEventEngineManager.DispatchTyped(AEventType: TEventType;
  ABlockType: TContentBlockType; const Buffer: TEventData);
var
  ToolCall: TToolCallSnapshot;
  ToolResult: TToolResultSnapshot;
begin
  case AEventType of
    TEventType.content_block_start:
      begin
        if ABlockType in TOOL_USE_BLOCKS then
          begin
            if FindToolCallByIndex(Buffer, Buffer.CurrentBlockIndex, ToolCall) then
              FDispatcher.DispatchToolUseStart(Buffer, ToolCall);
          end
        else
        if ABlockType in TOOL_RESULT_BLOCKS then
          begin
            if FindToolResultByIndex(Buffer, Buffer.CurrentBlockIndex, ToolResult) then
              FDispatcher.DispatchToolResultStart(Buffer, ToolResult);
          end;
      end;

    TEventType.content_block_delta:
      case ABlockType of
        TContentBlockType.text:
          if Buffer.LastAssistantDelta <> '' then
            FDispatcher.DispatchAssistantTextDelta(Buffer, Buffer.LastAssistantDelta);

        TContentBlockType.thinking:
          if Buffer.LastReasoningDelta <> '' then
            FDispatcher.DispatchReasoningDelta(Buffer, Buffer.LastReasoningDelta);
      else
        if ABlockType in TOOL_USE_BLOCKS then
          begin
            if Buffer.LastToolInputDelta <> '' then
              FDispatcher.DispatchToolUseInputDelta(Buffer, Buffer.LastToolInputDelta);
          end
        else
        if ABlockType in TOOL_RESULT_BLOCKS then
          begin
            if Buffer.LastToolResultDelta <> '' then
              FDispatcher.DispatchToolResultDelta(Buffer, Buffer.LastToolResultDelta);
          end;
      end;

    TEventType.content_block_stop:
      begin
        if ABlockType in TOOL_USE_BLOCKS then
          begin
            if FindToolCallByIndex(Buffer, Buffer.CurrentBlockIndex, ToolCall) then
              FDispatcher.DispatchToolUseStop(Buffer, ToolCall);
          end
        else
        if ABlockType in TOOL_RESULT_BLOCKS then
          begin
            if FindToolResultByIndex(Buffer, Buffer.CurrentBlockIndex, ToolResult) then
              FDispatcher.DispatchToolResultStop(Buffer, ToolResult);
          end;
      end;
  end;
end;

function TEventEngineManager.FindToolCallByIndex(const Buffer: TEventData;
  const AIndex: Int64; out ASnapshot: TToolCallSnapshot): Boolean;
begin
  for var Item in Buffer.ToolCalls do
    if Item.Index = AIndex then
      begin
        ASnapshot := Item;
        Exit(True);
      end;
  Result := False;
end;

function TEventEngineManager.FindToolResultByIndex(const Buffer: TEventData;
  const AIndex: Int64; out ASnapshot: TToolResultSnapshot): Boolean;
begin
  for var Item in Buffer.ToolResults do
    if Item.Index = AIndex then
      begin
        ASnapshot := Item;
        Exit(True);
      end;
  Result := False;
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
  var CanContinue := True;

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
