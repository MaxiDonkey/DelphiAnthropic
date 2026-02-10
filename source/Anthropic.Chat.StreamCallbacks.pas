unit Anthropic.Chat.StreamCallbacks;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.Threading,
  Anthropic.Types, Anthropic.Chat.StreamEvents;

type
  TEventData = record
  strict private
    class var FIndex: Integer;
  private
    FDelta: string;
    FId: string;
    FInputJson: string;
    FModel: string;
    FSignature: string;
    FStopReason: TStopReason;
    FStopSequence: string;
    FText: string;
    FThought: string;
    FRawJson: string;
  public
    class function Empty: TEventData; static;
    function Aggregate(const AChunk: TStreamEvent; const ErrorProc: TProc = nil): TEventData;
    property Delta: string read FDelta write FDelta;
    property Id: string read FId write FId;
    property InputJson: string read FInputJson write FInputJson;
    property Model: string read FModel write FModel;
    property RawJson: string read FRawJson write FRawJson;
    property Signature: string read FSignature write FSignature;
    property StopReason: TStopReason read FStopReason write FStopReason;
    property StopSequence: string read FStopSequence write FStopSequence;
    property Text: string read FText write FText;
    property Thought: string read FThought write FThought;
    class property Index: Integer read FIndex write FIndex;
  end;

  TStreamEventCallBack = record
  private
    FSender: TObject;
    FOnMessageStart: TProc<TObject, TEventData>;
    FOnMessageDelta: TProc<TObject, TEventData>;
    FOnMessageStop: TProc<TObject, TEventData>;
    FOnContentStart: TProc<TObject, TEventData>;
    FOnContentDelta: TProc<TObject, TEventData>;
    FOnContentStop: TProc<TObject, TEventData>;
    FOnError: TProc<TObject, TEventData>;
    FOnCancellation: TProc<TObject>;
    FOnDoCancel: TFunc<Boolean>;
  public
    property Sender: TObject read FSender write FSender;
    property OnMessageStart: TProc<TObject, TEventData> read FOnMessageStart write FOnMessageStart;
    property OnMessageDelta: TProc<TObject, TEventData> read FOnMessageDelta write FOnMessageDelta;
    property OnMessageStop: TProc<TObject, TEventData> read FOnMessageStop write FOnMessageStop;
    property OnContentStart: TProc<TObject, TEventData> read FOnContentStart write FOnContentStart;
    property OnContentDelta: TProc<TObject, TEventData> read FOnContentDelta write FOnContentDelta;
    property OnContentStop: TProc<TObject, TEventData> read FOnContentStop write FOnContentStop;
    property OnError: TProc<TObject, TEventData> read FOnError write FOnError;
    property OnCancellation: TProc<TObject> read FOnCancellation write FOnCancellation;
    property OnDoCancel: TFunc<Boolean> read FOnDoCancel write FOnDoCancel;
  end;

  IStreamEventDispatcher = interface
    ['{4B0DE7D4-7E57-4A9E-8B7A-49E7E7C9B1B7}']
    function GetCallBacks: TStreamEventCallBack;

    procedure DispatchEvent(EventType: TEventType; const Buffer: TEventData);

    property CallBacks: TStreamEventCallBack read GetCallBacks;
  end;

  TStreamEventDispatcher = class(TInterfacedObject, IStreamEventDispatcher)
  private
    FCallBacks: TStreamEventCallBack;

    procedure Invoke(const Proc: TProc<TObject, TEventData>; const Buffer: TEventData);
  public
    constructor Create(const CallBacks: TFunc<TStreamEventCallBack> = nil);

    function GetCallBacks: TStreamEventCallBack;

    procedure DispatchEvent(EventType: TEventType; const Buffer: TEventData);
  end;

implementation

{ TEventData }

function TEventData.Aggregate(const AChunk: TStreamEvent;
  const ErrorProc: TProc): TEventData;
begin
  if not Assigned(AChunk) then
    Exit;

  RawJson := AChunk.JSONResponse;

  case AChunk.EventType of
    TEventType.message_start:
      begin
        Id := AChunk.MessageStart.Message.Id;
        Model := AChunk.MessageStart.Message.Model;
        StopReason := AChunk.MessageStart.Message.StopReason;
        StopSequence := AChunk.MessageStart.Message.StopSequence;
      end;

    TEventType.content_block_start:
      begin
        Index := AChunk.ContentBlockStart.Index;
      end;

    TEventType.content_block_stop:
      begin
        Text := Text + sLineBreak;
        Thought := Thought + sLineBreak;
        Signature := Signature + sLineBreak;
        InputJson := InputJson + sLineBreak;
      end;

    TEventType.content_block_delta:
      case AChunk.ContentBlockDelta.Delta.&Type of
        TDeltaType.text_delta:
          begin
            Delta := AChunk.ContentBlockDelta.Delta.Text;
            Text := Text + Delta;
          end;

        TDeltaType.thinking_delta:
          begin
            Thought := Thought + AChunk.ContentBlockDelta.Delta.Thinking;
          end;

        TDeltaType.signature_delta:
          begin
            Signature := Signature + AChunk.ContentBlockDelta.Delta.Signature;
          end;

        TDeltaType.input_json_delta:
          begin
            InputJson := InputJson + AChunk.ContentBlockDelta.Delta.PartialJson;
          end;
      end;

    TEventType.error:
      begin
        if Assigned(ErrorProc) then
          ErrorProc();
      end;
  end;
end;

class function TEventData.Empty: TEventData;
begin
  Result := Default(TEventData);
  FIndex := 0;
end;

{ TStreamEventDispatcher }

constructor TStreamEventDispatcher.Create(
  const CallBacks: TFunc<TStreamEventCallBack>);
begin
  inherited Create;
  if Assigned(CallBacks) then
    FCallBacks := CallBacks()
  else
    FCallBacks := Default(TStreamEventCallBack);
end;

procedure TStreamEventDispatcher.DispatchEvent(EventType: TEventType;
  const Buffer: TEventData);
begin
  case EventType of
    TEventType.message_start:
      Invoke(FCallBacks.FOnMessageStart, Buffer);

    TEventType.message_delta:
      Invoke(FCallBacks.OnMessageDelta, Buffer);

    TEventType.message_stop:
      Invoke(FCallBacks.OnMessageStop, Buffer);

    TEventType.content_block_start:
      Invoke(FCallBacks.OnContentStart, Buffer);

    TEventType.content_block_delta:
      Invoke(FCallBacks.OnContentDelta, Buffer);

    TEventType.content_block_stop:
      Invoke(FCallBacks.OnContentStop, Buffer);

    TEventType.error:
      Invoke(FCallBacks.OnError, Buffer);
    else ;
  end;
end;

function TStreamEventDispatcher.GetCallBacks: TStreamEventCallBack;
begin
  Result := FCallBacks;
end;

procedure TStreamEventDispatcher.Invoke(const Proc: TProc<TObject, TEventData>;
  const Buffer: TEventData);
var
  LocalSender: TObject;
  LocalProc: TProc<TObject, TEventData>;
begin
  LocalSender := FCallBacks.Sender;
  LocalProc := Proc;

  if not Assigned(LocalSender) then
    LocalSender := Self;

  if Assigned(LocalProc) then
    begin
      var  Task: ITask := TTask.Run(
      procedure()
      begin
        TThread.Synchronize(nil, procedure
          begin
            LocalProc(LocalSender, Buffer);
          end)
      end);
    end;
end;

end.
