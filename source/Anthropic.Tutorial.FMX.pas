unit Anthropic.Tutorial.FMX;

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
  System.SysUtils, System.Classes, Winapi.Messages, FMX.Types, FMX.StdCtrls, FMX.ExtCtrls,
  FMX.Controls, FMX.Forms, Winapi.Windows, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Media, FMX.Objects, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, System.UITypes,
  System.Types, System.JSON, system.Threading,
  Anthropic, Anthropic.Types, Anthropic.API.Params;

type
  TToolProc = procedure (const Value: string) of object;

  TFMXTutorialHub = class
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

    procedure JSONUIClear;
    procedure ShowCancel;
    procedure HideCancel;

    constructor Create(
      const AClient: IAnthropic;
      const AMemo1, AMemo2, AMemo3, AMemo4: TMemo;
      const AButton: TButton);
  end;

  procedure Cancellation(Sender: TObject);
  function DoCancellation: Boolean;
  function DoCancellationStream(Sender: TObject): string;
  procedure Start(Sender: TObject);

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
  TutorialHub: TFMXTutorialHub = nil;

implementation

uses
  System.DateUtils;

function UnixIntToDateTime(const Value: Int64): TDateTime;
begin
  Result := TTimeZone.Local.ToLocalTime(UnixToDateTime(Value));
end;

function UnixDateTimeToString(const Value: Int64): string;
begin
  Result := DateTimeToStr(UnixIntToDateTime(Value))
end;

procedure Cancellation(Sender: TObject);
begin
  Display(Sender, 'The operation was cancelled' + sLineBreak);
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
  Display(Sender, 'Please wait...');
  Display(Sender);
end;

procedure Display(Sender: TObject; Value: string);
var
  M: TMemo;
begin
  if Sender is TMemo then
    M := Sender as TMemo else
    M := (Sender as TFMXTutorialHub).Memo1;
  M.Lines.Add(Value);
  M.ViewportPosition := PointF(M.ViewportPosition.X, M.Content.Height - M.Height);
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
  Display(Sender, sLineBreak);
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
        end
      else
      if Item.&Type = TContentBlockType.web_search_tool_result then
        begin
//          Display(Sender, Item.Content);

          for var Content in Item.ToolContent.WebSearchToolResultBlock.Content do
            Display(Sender, Content.Url);
        end
      else
      if Item.&Type = TContentBlockType.tool_use then
        begin
          Display(Sender, F('Type', Item.&Type.ToString));
          Display(Sender, F('Name', Item.Name));
          Display(Sender, F('input', Item.Input));
          if Assigned(TutorialHub.ToolCall) then
            TutorialHub.ToolCall(TutorialHub.Tool.Execute(Item.Input));
        end;
    end;
  Display(Sender);
  DisplayUsage(Sender, Value);
end;

procedure Display(Sender: TObject; Value: TModel);
begin
  if not Value.JSONResponse.IsEmpty then
    TutorialHub.JSONResponse := Value.JSONResponse;
  Display(Sender, [
    Value.Id,
    F('Type', Value.&Type),
    F('DisplayName', Value.DisplayName),
    F('CreatedAt', Value.CreatedAt)
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
  Display(Sender, F('Input_tokens', Value.InputTokens.ToString));
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

procedure DisplayStream(Sender: TObject; Value: string);
var
  M: TMemo;
begin
  if Sender is TMemo then
    M := Sender as TMemo
  else
    M := (Sender as TFMXTutorialHub).Memo1;

  M.Lines.BeginUpdate;
  try
    M.Lines.Text := M.Text + Value;
  finally
    M.Lines.EndUpdate;
  end;
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
  DisplayChunk(Value.RawJson);
end;

procedure DisplayMessageDelta(Sender: TObject; Value: TEventData);
begin
  DisplayChunk(Value.RawJson);
end;

procedure DisplayMessageStop(Sender: TObject; Value: TEventData);
begin
  DisplayChunk(Value.RawJson);
end;

procedure DisplayContentStart(Sender: TObject; Value: TEventData);
begin
  DisplayChunk(Value.RawJson);
end;

procedure DisplayContentDelta(Sender: TObject; Value: TEventData);
begin
  DisplayStream(Sender, Value.Delta);
  DisplayChunk(Value.RawJson);
end;

procedure DisplayContentStop(Sender: TObject; Value: TEventData);
begin
  DisplayChunk(Value.RawJson);
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
  Result := Format('%s (%s): %s%%', [Name, BoolToStr(State, True), (Value * 100).ToString(ffNumber, 3, 3)])
end;

function CodeBefore: string;
begin
  with TStringWriter.Create do
  try
    WriteLine('def is_odd(n):');
    WriteLine('  return n % 2 == 1');
    WriteLine('def test_is_odd():');
    Result := ToString;
  finally
    Free;
  end;
end;

function CodeAfter: string;
begin
  with TStringWriter.Create do
  try
    WriteLine('n = int(input(''Enter a number: ''))');
    WriteLine('print(fibonacci(n))');
    Result := ToString;
  finally
    Free;
  end;
end;

{ TFMXTutorialHub }

constructor TFMXTutorialHub.Create(
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
end;

procedure TFMXTutorialHub.HideCancel;
begin
  FButton.Visible := False;
end;

procedure TFMXTutorialHub.JSONUIClear;
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;
  Memo3.Lines.Clear;
  Memo4.Lines.Clear;
end;

procedure TFMXTutorialHub.OnButtonClick(Sender: TObject);
begin
  Cancel := True;
  HideCancel;
end;

procedure TFMXTutorialHub.SetButton(const Value: TButton);
begin
  FButton := Value;
  FButton.OnClick := OnButtonClick;
  FButton.Text := 'Cancel';
end;

procedure TFMXTutorialHub.SetJSONRequest(const Value: string);
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

procedure TFMXTutorialHub.SetJSONResponse(const Value: string);
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

procedure TFMXTutorialHub.SetMemo1(const Value: TMemo);
begin
  FMemo1 := Value;
  FMemo1.TextSettings.WordWrap := True;
end;

procedure TFMXTutorialHub.SetMemo2(const Value: TMemo);
begin
  FMemo2 := Value;
  FMemo2.TextSettings.WordWrap := False;
end;

procedure TFMXTutorialHub.SetMemo3(const Value: TMemo);
begin
  FMemo3 := Value;
  FMemo3.TextSettings.WordWrap := False;
end;

procedure TFMXTutorialHub.SetMemo4(const Value: TMemo);
begin
  FMemo4 := Value;
  FMemo4.TextSettings.WordWrap := False;
end;

procedure TFMXTutorialHub.ShowCancel;
begin
  FButton.Visible := True;
end;

initialization
finalization
  if Assigned(TutorialHub) then
    TutorialHub.Free;
end.
