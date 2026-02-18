unit Anthropic.Context.Helper;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.JSON, System.Threading,
  System.Generics.Collections,
  Anthropic.Types, Anthropic.API.Params, Anthropic.Chat.Request, Anthropic.Chat.Responses,
  Anthropic.Chat.StreamEvents, Anthropic.API.JsonSafeReader, Anthropic.API, Anthropic.Exceptions,
  Anthropic.Helpers,

  VCL.Dialogs;

type
  TPayloadReader = record
  private
    FJsonReader: TJsonReader;
  public
    class function Parse(const Value: string): TPayloadReader; static;
    function TextMessageRetrieve: string;
    property Reader: TJsonReader read FJsonReader;
  end;

  TToolResponse = record
  private
    FType: TContentBlockType;
    FId: string;
    FName: string;
    FInput: string;
    FText: string;
  public
    property &Type: TContentBlockType read FType write FType;
    property Id: string read FId write FId;
    property Name: string read FName write FName;
    property Input: string read FInput write FInput;
    property Text: string read FText write FText;
  end;

  TTurns = class;

  TTurnItem = record
  private
    FPayload: string;
    FResponse: string;
    FToolResponse: TArray<TToolResponse>;
    FIndex: Integer;
    FTurns: TTurns;
    function GetPriorPrompt: string;
    function GetTool(const Index: Integer): TToolResponse;
  public
    class function Empty: TTurnItem; static;
    function Parse<T: class, constructor>: T;
    function Next: TTurnItem;
    function Prior: TTurnItem;
    property Index: Integer read FIndex;
    property Payload: string read FPayload write FPayload;
    property PriorPrompt: string read GetPriorPrompt;
    property Response: string read FResponse write FResponse;
    property ToolResponse: TArray<TToolResponse> read FToolResponse write FToolResponse;
    property Turns: TTurns read FTurns;
    property Tool[const Index: Integer]: TToolResponse read GetTool;
  end;

  IMessageBuilder = interface
    ['{1C886848-358D-4957-8B61-0FBBCFA37624}']
    function BuildContextFromHistory(const ToolName: string; const Prompt: string): TArray<TMessageParam>;
  end;

  ITurns = interface(IMessageBuilder)
    ['{42AC632B-A2C5-4F6F-B1C6-2FC0EA7DA796}']
    function GetCount: Integer;
    function GetFirst: TTurnItem;
    function GetItem(Index: Integer): TTurnItem;
    function GetLast: TTurnItem;

    function AddItem(const APayload: string): TTurnItem; overload;
    function AddItem(const APayload: string; const AResponse: string): TTurnItem; overload;
    function AddItem(const Value: TChat): TTurnItem; overload;
    function SetResponse(const Index: Integer; const AResponse: string): TTurnItem; overload;
    function SetResponse(const Index: Integer; const Value: TChat): TTurnItem; overload;

    property Count: Integer read GetCount;
    property First: TTurnItem read GetFirst;
    property Item[Index: Integer]: TTurnItem read GetItem;
    property Last: TTurnItem read GetLast;
  end;

  TTurns = class(TInterfacedObject, ITurns)
  private
    FDictionary: TDictionary<integer, TTurnItem>;
    FNextIndex: Integer;
    FCurrentItem: TTurnItem;
    function GetCount: Integer;
    function GetFirst: TTurnItem;
    function GetItem(Index: Integer): TTurnItem;
    function GetLast: TTurnItem;
    procedure ToolsAggregation(const Value: TChat; var TurnItem: TTurnItem);
  public
    constructor Create;
    destructor Destroy; override;

    function AddItem(const APayload: string): TTurnItem; overload;
    function AddItem(const APayload: string; const AResponse: string): TTurnItem; overload;
    function AddItem(const Value: TChat): TTurnItem; overload;
    function SetResponse(const Index: Integer; const AResponse: string): TTurnItem; overload;
    function SetResponse(const Index: Integer; const Value: TChat): TTurnItem; overload;

    function BuildContextFromHistory(const ToolName: string; const Prompt: string): TArray<TMessageParam>;

    property Count: Integer read GetCount;
    property CurrentItem: TTurnItem read FCurrentItem write FCurrentItem;
    property First: TTurnItem read GetFirst;
    property Item[Index: Integer]: TTurnItem read GetItem;
    property Last: TTurnItem read GetLast;

    class function CreateInstance(const APayload: string = ''): ITurns;
  end;

implementation

{ TPayloadReader }

class function TPayloadReader.Parse(const Value: string): TPayloadReader;
begin
  Result := Default(TPayloadReader);
  Result.FJsonReader := TJsonReader.Parse(Value);
end;

function TPayloadReader.TextMessageRetrieve: string;
const
  S_MESSAGE = 'messages';
  S_ROLE = '.role';
  S_CONTENT = '.content';
begin
  var HighIndex := Reader.Count(S_MESSAGE);
  for var I := HighIndex-1 downto 0 do
    begin
      var Messages := Format('%s[%d]', [S_MESSAGE, I]);
      var Content := Messages + S_CONTENT;

      if (Reader.AsString(Messages + S_ROLE) = 'user') then
        begin
          if Reader.IsStringNode(Content) then
            begin
              Result := Reader.AsString(Content);
              Exit;
            end
          else
          if Reader.IsArrayNode(Content) then
            begin
              var HighContent := Reader.Count(Content);
              for var J := HighContent - 1 to 0 do
                begin
                  var Contents := Format('%s[%d]', [Content, J]);
                  var ContentValue := Contents + S_CONTENT;

                  Result := Reader.AsString(ContentValue);
              Exit;
                end;
            end;
        end;
    end;
end;

{ TTurns }

function TTurns.AddItem(const APayload: string): TTurnItem;
begin
  Result.FTurns := Self;
  Result.Payload := APayload;
  Result.FIndex := FNextIndex;
  FDictionary.Add(Result.Index, Result);
  Inc(FNextIndex);
end;

function TTurns.SetResponse(const Index: Integer; const AResponse: string): TTurnItem;
begin
  Result := Item[Index];
  Result.Response := AResponse;
  FDictionary[Index] := Result;
end;

function TTurns.AddItem(const APayload, AResponse: string): TTurnItem;
begin
  Result.FTurns := Self;
  Result.Payload := APayload;
  Result.Response := AResponse;
  Result.FIndex := FNextIndex;
  FDictionary.Add(Result.Index, Result);
  Inc(FNextIndex);
end;

function TTurns.AddItem(const Value: TChat): TTurnItem;
begin
  Result := AddItem(Value.JSONPayload);
  Result := SetResponse(Result.Index, Value);
end;

constructor TTurns.Create;
begin
  inherited Create;
  FDictionary := TDictionary<integer, TTurnItem>.Create;
  FNextIndex := 0;
  CurrentItem := Default(TTurnItem);

end;

class function TTurns.CreateInstance(const APayload: string): ITurns;
begin
  Result := TTurns.Create;
  if not APayload.Trim.IsEmpty then
    Result.AddItem(APayload);
end;

destructor TTurns.Destroy;
begin
  FDictionary.Free;
  inherited;
end;

function TTurns.GetCount: Integer;
begin
  Result := FDictionary.Count;
end;

function TTurns.GetFirst: TTurnItem;
begin
  Result := Item[0];
end;

function TTurns.GetItem(Index: Integer): TTurnItem;
begin
  if not FDictionary.TryGetValue(Index, Result) then
    raise EArgumentOutOfRangeException.CreateFmt('Invalid index %d', [Index]);
end;

function TTurns.GetLast: TTurnItem;
begin
  Result := Item[GetCount - 1];
end;

function TTurns.BuildContextFromHistory(const ToolName,
  Prompt: string): TArray<TMessageParam>;
var
  ToolId: string;
begin
  Result := [];
  if Count < 1 then
    Exit;

  var Messages := Generation.MessageParts;

    for var I := 0 to Count - 1 do
      begin
        var CurrentTurn := Item[I];
        var CurrentTool := CurrentTurn.Tool[0];

        if I = 0 then
          begin
            Messages := Messages
                .User(CurrentTurn.PriorPrompt)
                .Assistant( Generation.ContentParts
                  .AddText( CurrentTool.Text)
                    .Add( Generation.Context.CreateToolUse
                        .Id( CurrentTool.Id )
                        .Name( ToolName  )
                        .Input( CurrentTool.Input )
                    )
                );
            ToolId := CurrentTool.Id;
          end
        else
        if I <= Count - 1 then
          Messages := Messages
             .User( Generation.ContentParts
                .Add( Generation.Context.CreateToolResult
                    .ToolUseId( ToolId )
                    .Content( CurrentTurn.PriorPrompt )
                )
             )
             .Assistant( Generation.ContentParts
              .AddText( CurrentTool.Text)
                .Add( Generation.Context.CreateToolUse
                    .Id( ToolId )
                    .Name( ToolName  )
                    .Input( CurrentTool.Input )
                )
            );

        if I = Count - 1 then
          Messages := Messages
             .User( Generation.ContentParts
                .Add( Generation.Context.CreateToolResult
                    .ToolUseId( ToolId )
                    .Content( Prompt )
                )
            )
      end;

  Result := Messages;
end;

function TTurns.SetResponse(const Index: Integer;
  const Value: TChat): TTurnItem;
begin
  if not Assigned(Value) then
    EAnthropicException.Create('Response: TChat value non assigned');

  Result := Item[Index];
  Result.Response := Value.JSONResponse;
  ToolsAggregation(Value, Result);

  FDictionary[Index] := Result;
end;

procedure TTurns.ToolsAggregation(const Value: TChat; var TurnItem: TTurnItem);
begin
  if not Assigned(Value) or (Value.StopReason <> TStopReason.tool_use ) then
    Exit;

  var ContentText := '';

  for var Item in Value.Content do
    begin
      if Item.&Type = TContentBlockType.text then
        begin
          ContentText := Item.Text;
        end
      else
      if Item.&Type = TContentBlockType.tool_use then
        begin
          var ToolResponse := Default(TToolResponse);
          ToolResponse.&Type := Item.&Type;
          ToolResponse.Id := Item.Id;
          ToolResponse.Name := Item.Name;
          ToolResponse.Input := Item.Input;
          ToolResponse.Text := ContentText;
          ContentText := '';
          TurnItem.FToolResponse := TurnItem.FToolResponse + [ToolResponse];
        end;
    end;
end;

{ TTurnItem }

class function TTurnItem.Empty: TTurnItem;
begin
  Result := Default(TTurnItem);
  Result.FIndex := -1;
end;

function TTurnItem.GetPriorPrompt: string;
begin
  var Payload := Turns.Item[FIndex].Payload;
  Result := TPayloadReader.Parse(Payload).TextMessageRetrieve;
end;

function TTurnItem.GetTool(const Index: Integer): TToolResponse;
begin
  if (Index < 0) or (Index > Length(ToolResponse) - 1) then
    raise EArgumentOutOfRangeException.CreateFmt('Invalid index %d', [Index]);

  Result := ToolResponse[Index];
end;

function TTurnItem.Next: TTurnItem;
begin
  if Turns.Count > 0 then
    begin
      if Index < Turns.Count - 1 then
        Result := Turns.Item[Index + 1]
      else
        Result := Turns.Item[Turns.Count - 1]
    end
  else
    Result := Empty;
end;

function TTurnItem.Parse<T>: T;
begin
  Result := TApiDeserializer.Parse<T>(Response, Payload);
end;

function TTurnItem.Prior: TTurnItem;
begin
  if Turns.Count > 0 then
    begin
      if Index > 0 then
        Result := Turns.Item[Index - 1]
      else
        Result := Turns.Item[0]
    end
  else
    Result := Empty;
end;

end.
