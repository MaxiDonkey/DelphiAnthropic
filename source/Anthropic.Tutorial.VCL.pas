unit Anthropic.Tutorial.VCL;

{ Tutorial Support Unit

   WARNING:
     This module is intended solely to illustrate the examples provided in the
     README.md file of the repository :
          https://github.com/MaxiDonkey/DelphiAnthropic
     Under no circumstances should the methods described below be used outside
     of the examples presented on the repository's page.
}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.UITypes, System.Threading, System.JSON,
  Anthropic, Anthropic.Types, Anthropic.API.Params, Anthropic.Helpers, Anthropic.Context.Helper;

type
  TToolProc = procedure (const Value: string) of object;

  TVCLTutorialHub = class
  private
    FClient: IAnthropic;
    FMemo1: TMemo;
    FMemo2: TMemo;
    FMemo3: TMemo;
    FMemo4: TMemo;
    FButton: TButton;
    FTool: IFunctionCore;
    FToolCall: TToolProc;
    FCancel: Boolean;
    FToolTurns: ITurns;
    FFileName: string;
    FBatchId: string;
    procedure OnButtonClick(Sender: TObject);
    procedure SetButton(const Value: TButton);
    procedure SetMemo1(const Value: TMemo);
    procedure SetMemo2(const Value: TMemo);
    procedure SetMemo3(const Value: TMemo);
    procedure SetMemo4(const Value: TMemo);
    procedure SetJSONRequest(const Value: string);
    procedure SetJSONResponse(const Value: string);
  public
    property Client: IAnthropic read FClient write FClient;
    property Memo1: TMemo read FMemo1 write SetMemo1;
    property Memo2: TMemo read FMemo2 write SetMemo2;
    property Memo3: TMemo read FMemo3 write SetMemo3;
    property Memo4: TMemo read FMemo4 write SetMemo4;

    property Button: TButton read FButton write SetButton;
    property Cancel: Boolean read FCancel write FCancel;
    property Tool: IFunctionCore read FTool write FTool;
    property ToolCall: TToolProc read FToolCall write FToolCall;
    property JSONRequest: string write SetJSONRequest;
    property JSONResponse: string write SetJSONResponse;

    property ToolTurns: ITurns read FToolTurns write FToolTurns;

    procedure JSONUIClear;
    procedure ShowCancel;
    procedure HideCancel;

    function WeatherRetrieve(const Value: string): string;
    procedure WeatherReporter(const Value: string);

    constructor Create(const AClient: IAnthropic;
      const AMemo1, AMemo2, AMemo3, AMemo4: TMemo;
      const AButton: TButton);
  end;

  procedure Cancellation(Sender: TObject);
  function DoCancellation: Boolean;
  function DoCancellationStream(Sender: TObject): string;
  procedure Start(Sender: TObject);

  procedure DisplaySync(Sender: TObject; Value: string); overload;
  procedure DisplayStreamSync(Sender: TObject; Value: string); overload;

  procedure Display(Sender: TObject); overload;
  procedure Display(Sender: TObject; Value: string); overload;
  procedure Display(Sender: TObject; Value: TArray<string>); overload;

  procedure Display(Sender: TObject; Value: TChat); overload;
  procedure Display(Sender: TObject; Value: TModel); overload;
  procedure Display(Sender: TObject; Value: TModels); overload;
  procedure Display(Sender: TObject; Value: TUsage); overload;
  procedure Display(Sender: TObject; Value: TBatchList); overload;
  procedure Display(Sender: TObject; Value: TBatch); overload;
  procedure Display(Sender: TObject; Value: TBatchDelete); overload;
  procedure Display(Sender: TObject; Value: TStringList); overload;
  procedure Display(Sender: TObject; Value: IBatcheResults); overload;
  procedure Display(Sender: TObject; Value: TTokenCount); overload;
  procedure Display(Sender: TObject; Value: TFile); overload;
  procedure Display(Sender: TObject; Value: TFileList); overload;
  procedure Display(Sender: TObject; Value: TFileDeleted); overload;
  procedure Display(Sender: TObject; Value: TTurnItem); overload;
  procedure Display(Sender: TObject; Value: TSkill); overload;
  procedure Display(Sender: TObject; Value: TSkillList); overload;
  procedure Display(Sender: TObject; Value: TSkillDeleted); overload;
  procedure Display(Sender: TObject; Value: TSkillVersion); overload;
  procedure Display(Sender: TObject; Value: TSkillVersionList); overload;

  function DisplayChat(Sender: TObject; Value: TChat): string; overload;
  function DisplayChat(Sender: TObject; Value: string): string; overload;

  procedure DisplayStream(Sender: TObject; Value: string); overload;
  procedure DisplayStream(Sender: TObject; Value: TChatStream); overload;

  procedure DisplayChunk(Value: string); overload;
  procedure DisplayChunk(Value: TChatStream); overload;

  function DisplayPromise(Sender: TObject; Value: string): string; overload;
  function DisplayPromise(Sender: TObject; Value: TChat): string; overload;

  procedure DisplayUsage(Sender: TObject; Value: TChat);

  procedure DisplayMessageStart(Sender: TObject; Value: TEventData);
  procedure DisplayMessageDelta(Sender: TObject; Value: TEventData);
  procedure DisplayMessageStop(Sender: TObject; Value: TEventData);
  procedure DisplayContentStart(Sender: TObject; Value: TEventData);
  procedure DisplayContentDelta(Sender: TObject; Value: TEventData);
  procedure DisplayContentStop(Sender: TObject; Value: TEventData);
  procedure DisplayStreamError(Sender: TObject; Value: TEventData);

  function F(const Name, Value: string): string; overload;
  function F(const Name: string; const Value: TArray<string>): string; overload;
  function F(const Name: string; const Value: boolean): string; overload;
  function F(const Name: string; const State: Boolean; const Value: Double): string; overload;

var
  /// <summary>
  /// A global instance of the <see cref="TVCLTutorialHub"/> class used as the main tutorial hub.
  /// </summary>
  /// <remarks>
  /// This variable serves as the central hub for managing tutorial components, such as memos, buttons, and pages.
  /// It is initialized dynamically during the application's runtime, and its memory is automatically released during
  /// the application's finalization phase.
  /// </remarks>
  TutorialHub: TVCLTutorialHub = nil;

implementation

procedure Cancellation(Sender: TObject);
begin
  Display(Sender, 'The operation was cancelled');
  Display(Sender);
  TutorialHub.Cancel := False;
end;

function DoCancellation: Boolean;
begin
  Result := TutorialHub.Cancel;
end;

function DoCancellationStream(Sender: TObject): string;
begin
  Result := 'aborted';
end;

procedure Start(Sender: TObject);
begin
  TutorialHub.Cancel := False;
  Display(Sender, 'Please wait...');
  Display(Sender);
end;

procedure DisplaySync(Sender: TObject; Value: string);
var
  M: TMemo;
begin
  if Sender is TMemo then
    M := TMemo(Sender) else
    M := (Sender as TVCLTutorialHub).Memo1;

  var S := Value.Split([#10]);
  if Length(S) = 0 then
    begin
      M.Lines.Add(Value)
    end
  else
    begin
      for var Item in S do
        M.Lines.Add(Item);
    end;

  M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
end;

procedure Display(Sender: TObject; Value: string);
begin
  DisplaySync(Sender, Value);

//  var Task: ITask := TTask.Create(
//  procedure()
//  begin
//    TThread.Synchronize(nil, procedure
//      begin
//        DisplaySync(Sender, Value);
//      end)
//  end);
//
//  Task.Start;
end;

procedure Display(Sender: TObject; Value: TArray<string>);
begin
  var index := 0;
  for var Item in Value do
    begin
      if not Item.IsEmpty then
        begin
          if index = 0 then
            Display(Sender, Item) else
            Display(Sender, '    ' + Item);
        end;
      Inc(index);
    end;
end;

procedure Display(Sender: TObject);
begin
  Display(Sender, '');
end;

procedure Display(Sender: TObject; Value: TChat);
begin
  if not Assigned(Value) then
    Exit;

  TutorialHub.JSONResponse := Value.JSONResponse;

  for var Item in Value.Content do
    begin
      if Item.&Type = TContentBlockType.text then
        begin
          Display(Sender, Item.Text);
          //DisplayUsage(Sender, Value);
        end
      else
      if Item.&Type = TContentBlockType.web_search_tool_result then
        begin
          for var Content in Item.ToolContent.WebSearchToolResultBlock.Content do
            Display(Sender, Content.Url);
          //DisplayUsage(Sender, Value);
        end
      else
      if Item.&Type = TContentBlockType.tool_use then
        begin
          Display(Sender, F('Type', Item.&Type.ToString));
          Display(Sender, F('Name', Item.Name));
          Display(Sender, F('input', Item.Input));
          //DisplayUsage(Sender, Value);

          if Item.Name = 'get_weather' then
            TutorialHub.ToolCall(Item.Input);
        end;
    end;
  Display(Sender);
end;

procedure Display(Sender: TObject; Value: TModel);
begin
  if not Value.JSONResponse.IsEmpty then
    TutorialHub.JSONResponse := Value.JSONResponse;
  Display(Sender, [
    Value.Id,
    F('• Type', Value.&Type),
    F('• DisplayName', Value.DisplayName),
    F('• CreatedAt', Value.CreatedAt)
  ]);
  Display(Sender, EmptyStr);
end;

procedure Display(Sender: TObject; Value: TModels);
begin
  if Length(Value.Data) = 0 then
    begin
      Display(Sender, 'No model found');
      Exit;
    end;
  TutorialHub.JSONResponse := Value.JSONResponse;
  for var Item in Value.Data do
    begin
      Display(Sender, Item);
    end;
  Display(Sender);
end;

procedure Display(Sender: TObject; Value: TUsage);
begin
  Display(Sender, [F('• input_tokens', [Value.InputTokens.ToString,
      F('• output_tokens', Value.OutputTokens.ToString),
      F('• cache_creation_input_tokens', Value.CacheCreationInputTokens.ToString),
      F('• cache_read_input_tokens', Value.CacheReadInputTokens.ToString)
   ])]);
  Display(Sender)
end;

procedure Display(Sender: TObject; Value: TBatch);
begin
  if not Value.JSONResponse.IsEmpty then
    TutorialHub.JSONResponse := Value.JSONResponse;

  Display(Sender, [EmptyStr,
    Value.Id,
    F('• Type', Value.&Type),
    F('• Processing_status', Value.ProcessingStatus.ToString),
    F('• CreatedAt', Value.CreatedAt),
    F('• ExpiresAt', Value.ExpiresAt),
    F('• CancelInitiatedAt', Value.CancelInitiatedAt),
    F('• ResultsUrl', Value.ResultsUrl),
    F('• Processing', Value.RequestCounts.Processing.ToString),
    F('• Succeeded', Value.RequestCounts.Succeeded.ToString),
    F('• Errored', Value.RequestCounts.Errored.ToString),
    F('• Canceled', Value.RequestCounts.Canceled.ToString),
    F('• Expired', Value.RequestCounts.Expired.ToString)
  ]);
  Display(Sender, EmptyStr);
end;

procedure Display(Sender: TObject; Value: TBatchDelete);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  Display(Sender, F('Id', [
     Value.Id,
     F('Type', Value.&Type)
  ]));
  Display(Sender);
end;

procedure Display(Sender: TObject; Value: TBatchList);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;

  Display(Sender, F('• HasMore', BoolToStr(Value.HasMore, True)));
  Display(Sender, F('• FirstId', Value.FirstId));
  Display(Sender, F('• LastId', Value.LastId));
  Display(Sender, EmptyStr);

  for var Item in Value.Data do
    begin
      Display(Sender, [EmptyStr,
        F('Id', [
          Item.Id,
          F('• processingStatus', Item.ProcessingStatus.ToString)
        ])
      ]);
    end;
  Display(Sender);
end;

procedure Display(Sender: TObject; Value: TStringList);
begin
  with Value.GetEnumerator do
  try
    while MoveNext do
      Display(Sender, Current);
  finally
    Free;
    Display(Sender);
  end;
end;

procedure Display(Sender: TObject; Value: IBatcheResults);
begin
  for var Item in Value.Batches do
    Display(Sender, F(Item.CustomId, Item.Result.Message.Content[0].Text));
  Display(Sender, EmptyStr);
end;

procedure Display(Sender: TObject; Value: TTokenCount);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  Display(Sender, F('• Input_tokens', Value.InputTokens.ToString));
end;

procedure Display(Sender: TObject; Value: TFile);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  Display(Sender, [
    Value.Id,
    F('• filename', Value.Filename),
    F('• mimeType', Value.MimeType),
    F('• sizeBytes', Value.SizeBytes.ToString),
    F('• downloadable', BoolToStr(Value.Downloadable, True))
  ]);
  Display(Sender);
end;

procedure Display(Sender: TObject; Value: TFileList);
begin
  if not Value.JSONResponse.IsEmpty then
    TutorialHub.JSONResponse := Value.JSONResponse;
  for var Item in Value.Data do
    Display(Sender, Item);
end;

procedure Display(Sender: TObject; Value: TFileDeleted);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  Display(Sender, Value.Id + ' deleted');
end;

procedure Display(Sender: TObject; Value: TTurnItem);
begin
  Display(Sender, F('• count', Value.Turns.Count.ToString));
  Display(Sender, F('• index', Value.Index.ToString));
  for var Item in Value.ToolResponse do
    begin
      Display(Sender, F('  • type', Item.&Type.ToString));
      Display(Sender, F('  • id', Item.Id));
      Display(Sender, F('  • name', Item.Name));
      Display(Sender, F('  • text', Item.Text.Trim));
      Display(Sender, F('  • input', Item.Input));
    end;
  Display(Sender);
end;

procedure Display(Sender: TObject; Value: TSkill);
begin
  if not Value.JSONResponse.IsEmpty then
    TutorialHub.JSONResponse := Value.JSONResponse;
  Display(Sender, Value.Id);
  Display(Sender, F('  • created_at', Value.CreatedAt));
  Display(Sender, F('  • display_title', Value.DisplayTitle));
  Display(Sender, F('  • LatestVersion', Value.LatestVersion));
  Display(Sender, F('  • source', Value.Source));
  Display(Sender, F('  • type', Value.&Type));
//  Display(Sender, F('  • updated_at', Value.UpdatedAt));

  Display(Sender, '');
end;

procedure Display(Sender: TObject; Value: TSkillList);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  for var Item in Value.Data do
    Display(Sender, Item);
end;

procedure Display(Sender: TObject; Value: TSkillDeleted);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  Display(TutorialHub, Value.Id + ' skill deleted');
end;

procedure Display(Sender: TObject; Value: TSkillVersion); overload;
begin
  if not Value.JSONResponse.IsEmpty then
    TutorialHub.JSONResponse := Value.JSONResponse;
  Display(Sender, Value.Id);
  Display(Sender, F('  • created_at', Value.CreatedAt));
  Display(Sender, F('  • description', Value.Description.Trim));
  Display(Sender, F('  • directory', Value.Directory));
  Display(Sender, F('  • name', Value.Name));
  Display(Sender, F('  • skill_id', Value.SkillId));
  Display(Sender, F('  • type', Value.&Type));
  Display(Sender, F('  • version', Value.Version));

  Display(Sender, '');
end;

procedure Display(Sender: TObject; Value: TSkillVersionList);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  for var Item in Value.Data do
    Display(Sender, Item);
end;

function DisplayChat(Sender: TObject; Value: TChat): string;
begin
  Display(Sender, Value);
end;

function DisplayChat(Sender: TObject; Value: string): string;
begin
  Display(Sender, Value);
end;

procedure DisplayStreamSync(Sender: TObject; Value: string);
var
  M: TMemo;
  CurrentLine: string;
  Lines: TArray<string>;
begin
  if Sender is TMemo then
    M := TMemo(Sender) else
    M := (Sender as TVCLTutorialHub).Memo1;

  var OldSelStart := M.SelStart;
  var ShouldScroll := (OldSelStart = M.GetTextLen);

  M.Lines.BeginUpdate;
  try
    Lines := Value.Split([#10]);
    if Length(Lines) > 0 then
    begin
      if M.Lines.Count > 0 then
        CurrentLine := M.Lines[M.Lines.Count - 1]
      else
        CurrentLine := '';

      CurrentLine := CurrentLine + Lines[0];

      if M.Lines.Count > 0 then
        M.Lines[M.Lines.Count - 1] := CurrentLine
      else
        M.Lines.Add(CurrentLine);

      for var i := 1 to High(Lines) do
        M.Lines.Add(Lines[i]);
    end;
  finally
    M.Lines.EndUpdate;
  end;

  if ShouldScroll then
  begin
    M.SelStart := M.GetTextLen;
    M.SelLength := 0;
    M.Perform(EM_SCROLLCARET, 0, 0);
  end;
end;

procedure DisplayStream(Sender: TObject; Value: string);
begin
  DisplayStreamSync(Sender, Value);
//  var Task: ITask := TTask.Create(
//  procedure()
//  begin
//    TThread.Synchronize(nil, procedure
//      begin
//        DisplayStreamSync(Sender, Value);
//      end)
//  end);
//
//  Task.Start;
end;

procedure DisplayStream(Sender: TObject; Value: TChatStream);
begin
  if not Assigned(Value) then
    Exit;

  if Value.EventType = TEventType.content_block_delta then
    DisplayStream(Sender, Value.ContentBlockDelta.Delta.Text);

  DisplayChunk(Value);
end;

procedure DisplayChunk(Value: string);
begin
  if Value.Trim.IsEmpty then
    Exit;

  var JSONValue := TJSONObject.ParseJSONValue(Value);
    try
      Display(TutorialHub.Memo4, JSONValue.ToString);
    finally
      JSONValue.Free;
    end;
end;

procedure DisplayChunk(Value: TChatStream);
begin
  DisplayChunk(Value.JSONResponse);
end;

function DisplayPromise(Sender: TObject; Value: string): string;
begin
  Display(Sender, Value);
end;

function DisplayPromise(Sender: TObject; Value: TChat): string;
begin
  Display(Sender, Value);
end;

procedure DisplayUsage(Sender: TObject; Value: TChat);
begin
  Display(Sender, Value.Usage);
end;

procedure DisplayMessageStart(Sender: TObject; Value: TEventData);
begin

end;

procedure DisplayMessageDelta(Sender: TObject; Value: TEventData);
begin

end;

procedure DisplayMessageStop(Sender: TObject; Value: TEventData);
begin

end;

procedure DisplayContentStart(Sender: TObject; Value: TEventData);
begin

end;

procedure DisplayContentDelta(Sender: TObject; Value: TEventData);
begin
  DisplayStream(Sender, Value.Delta);
end;

procedure DisplayContentStop(Sender: TObject; Value: TEventData);
begin

end;

procedure DisplayStreamError(Sender: TObject; Value: TEventData);
begin
  Display(Sender, 'error');
end;

function F(const Name, Value: string): string;
begin
  if not Value.IsEmpty then
    Result := Format('%s: %s', [Name, Value])
end;

function F(const Name: string; const Value: TArray<string>): string;
begin
  var index := 0;
  for var Item in Value do
    begin
      if index = 0 then
        Result := Format('%s: %s', [Name, Item]) else
        Result := Result + '    ' + Item;
      Inc(index);
    end;
end;

function F(const Name: string; const Value: boolean): string;
begin
  Result := Format('%s: %s', [Name, BoolToStr(Value, True)])
end;

function F(const Name: string; const State: Boolean; const Value: Double): string;
begin
  Result := Format('%s (%s): %s%%', [Name, BoolToStr(State, True), (Value * 100).ToString(ffNumber, 3, 2)])
end;

{ TVCLTutorialHub }

constructor TVCLTutorialHub.Create(
  const AClient: IAnthropic;
  const AMemo1, AMemo2, AMemo3, AMemo4: TMemo;
  const AButton: TButton);
begin
  inherited Create;
  FClient := AClient;
  Memo1 := AMemo1;
  Memo2 := AMemo2;
  Memo3 := AMemo3;
  Memo4 := AMemo4;
  Button := AButton;

  ToolCall := WeatherReporter;
end;

procedure TVCLTutorialHub.HideCancel;
begin
  FButton.Visible := False;
end;

procedure TVCLTutorialHub.JSONUIClear;
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;
  Memo3.Lines.Clear;
  Memo4.Lines.Clear;
end;

procedure TVCLTutorialHub.OnButtonClick(Sender: TObject);
begin
  Cancel := True;
end;

procedure TVCLTutorialHub.SetButton(const Value: TButton);
begin
  FButton := Value;
  FButton.OnClick := OnButtonClick;
  FButton.Caption := 'Cancel';
end;

procedure TVCLTutorialHub.SetJSONRequest(const Value: string);
begin
  var Task: ITask := TTask.Create(
  procedure()
  begin
    TThread.Synchronize(nil, procedure
      begin
        Memo3.Lines.Text := Value;
        Memo3.SelStart := 0;
      end)
  end);

  Task.Start;
end;

procedure TVCLTutorialHub.SetJSONResponse(const Value: string);
begin
  var Task: ITask := TTask.Create(
  procedure()
  begin
    TThread.Synchronize(nil, procedure
      begin
        Memo4.Lines.Text := Value;
        Memo4.SelStart := 0;
      end)
  end);

  Task.Start;
end;

procedure TVCLTutorialHub.SetMemo1(const Value: TMemo);
begin
  FMemo1 := Value;
  FMemo1.ScrollBars := TScrollStyle.ssVertical;
end;

procedure TVCLTutorialHub.SetMemo2(const Value: TMemo);
begin
  FMemo2 := Value;
  FMemo2.ScrollBars := TScrollStyle.ssVertical;
end;

procedure TVCLTutorialHub.SetMemo3(const Value: TMemo);
begin
  FMemo3 := Value;
  FMemo3.ScrollBars := TScrollStyle.ssBoth;
end;

procedure TVCLTutorialHub.SetMemo4(const Value: TMemo);
begin
  FMemo4 := Value;
  FMemo4.ScrollBars := TScrollStyle.ssBoth;
end;

procedure TVCLTutorialHub.ShowCancel;
begin
  FButton.Visible := True;
end;

procedure TVCLTutorialHub.WeatherReporter(const Value: string);
begin
  var ModelName := 'claude-opus-4-6';
  var MaxTokens := 1024;
  var Prompt := 'Announce the day''s weather forecast';

  if Tool = nil then
    Prompt := Prompt + sLineBreak + WeatherRetrieve(Value)
  else
    Prompt := Prompt + sLineBreak + Tool.Execute(Value);

  Display(TutorialHub, #10'Second step, please wait...'#10);
  TutorialHub.Memo2.Lines.Text := Prompt;

  //JSON payload creation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .MaxTokens(MaxTokens)
          .Messages( MessageParts
              .User( Prompt)
          );

      TutorialHub.JSONRequest := Params.ToFormat();
    end;

  // Asynchronous example
  var Promise := Client.Chat.AsyncAwaitCreate(Payload);

  Promise
    .&Then(
      procedure (Value: TChat)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

function TVCLTutorialHub.WeatherRetrieve(const Value: string): string;
begin
  var Location := TJsonReader.Parse(Value).AsString('location');

  if Location.ToLower.Contains('san francisco') then
    begin
      var Json := TJSONObject.Create;
      try
      Result := Json
        .AddPair('Location', 'San Francisco, CA')
        .AddPair('temperature', '16°C')
        .AddPair('forecast', 'rainy, low visibility but sunny in the late afternoon or early evening')
        .ToJSON;
      finally
        Json.Free;
      end;
    end;
end;

initialization
finalization
  if Assigned(TutorialHub) then
    TutorialHub.Free;
end.
