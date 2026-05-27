unit Demo.Anthropic.DisplayBlocks;

interface

uses
  System.SysUtils, System.JSON,
  Anthropic, Anthropic.Types, Anthropic.Chat.StreamEvents, Anthropic.Chat.StreamCallbacks,
  Anthropic.API.JsonSafeReader,
  WVPythia.Chat.DisplayBlocks;

type
  /// <summary>
  /// Anthropic adapter over the vendor-neutral Pythia display block
  /// aggregator. Only the methods that consume Anthropic stream snapshots
  /// stay here.
  /// </summary>
  IAnthropicDisplayBlockAggregator = interface(IPythiaDisplayBlockAggregator)
    ['{4FD13E85-8D8E-4E0C-9A17-D6F9197FB35D}']
    procedure RegisterToolUseStop(const Snapshot: TToolCallSnapshot;
      const DisplayTitle: string);
    procedure RegisterToolResultStop(const Snapshot: TToolResultSnapshot);
  end;

  TAnthropicDisplayBlockAggregator = class(
    TPythiaDisplayBlockAggregator, IAnthropicDisplayBlockAggregator)
  public
    procedure RegisterToolUseStop(const Snapshot: TToolCallSnapshot;
      const DisplayTitle: string);
    procedure RegisterToolResultStop(const Snapshot: TToolResultSnapshot);
  end;

  TToolDisplayTitle = record
  private
    class function BaseTitle(const ABlockType: TContentBlockType;
      const AToolName: string): string; static;
    class function HumanizeIdentifier(const Value: string): string; static;
    class function ResolveDisplayName(const AInternalName: string): string; static;
    class function TryReadInputJsonString(
      const Reader: TJsonReader;
      const FieldName: string;
      out Value: string): Boolean; static;
  public
    class function FromInput(
      const ABlockType: TContentBlockType;
      const AToolName, AInputJson: string): string; static;
    class function FromBlockType(
      const ABlockType: TContentBlockType): string; static;
  end;

  TToolUseDisplayDetails = record
  private
    class procedure AppendDetailLine(
      const Builder: TStringBuilder;
      const LabelName, Value: string); static;
    class procedure AppendInputJson(
      const Builder: TStringBuilder;
      const LabelName, Value: string); static;
    class function FormatJson(const Value: string): string; static;
    class function HumanizeIdentifier(const Value: string): string; static;
    class function IsComputerTool(
      const Snapshot: TToolCallSnapshot): Boolean; static;
    class function BuildComputerDetails(
      const Snapshot: TToolCallSnapshot;
      out Details: string): Boolean; static;
    class function BuildMCPToolUseDetails(
      const Snapshot: TToolCallSnapshot;
      out Details: string): Boolean; static;
  public
    class function TryFromSnapshot(
      const Snapshot: TToolCallSnapshot;
      out Details: string): Boolean; static;
  end;

  TToolResultDisplayDetails = record
  private
    class procedure AppendDetailLine(
      const Builder: TStringBuilder;
      const LabelName, Value: string); static;
    class function BuildToolErrorSummary(
      const AFailureLabel, AErrorCode, AErrorMessage: string): string; overload; static;
    class function BuildToolErrorSummary(
      const AFailureLabel, ARawContent: string): string; overload; static;
    class function BuildWebFetchDetails(
      const Block: TContentBlock;
      out Details: string): Boolean; static;
    class function BuildWebSearchDetails(
      const Block: TContentBlock;
      out Details: string): Boolean; static;
    class function BuildToolSearchDetails(
      const Block: TContentBlock;
      out Details: string): Boolean; static;
    class function BuildMCPToolDetails(
      const Block: TContentBlock;
      out Details: string): Boolean; static;
    class function HumanizeIdentifier(const Value: string): string; static;
  public
    class function TryFromEvent(
      const Event: TChatStream;
      out Details: string): Boolean; static;
  end;

implementation

{$REGION 'Dev note'}
(*

  Anthropic-flavored display rendering helpers for the Messages API streaming
  path (Demo.Anthropic.Services). Three concerns live here, kept together
  because they all turn raw Anthropic stream snapshots into UI-ready text:

    1. TAnthropicDisplayBlockAggregator
       A thin subclass of TPythiaDisplayBlockAggregator that adds two
       Anthropic-shaped hooks: RegisterToolUseStop and RegisterToolResultStop.
       They translate the SDK snapshots (TToolCallSnapshot, TToolResultSnapshot)
       into the vendor-neutral AppendToolUse / MarkToolError / CloseCurrent
       calls the rest of Pythia already understands. The Messages flow holds
       this aggregator as IAnthropicDisplayBlockAggregator, captures it from
       every stream closure, then hands its CloneDisplayBlocks snapshot to
       TFinalizeData on completion for persistent replay.

       This is the Messages-API counterpart of TAnthropicAgentTurnDisplay
       (Demo.Anthropic.Agent.TurnDisplay), which plays the equivalent role
       for the Managed Agents session flow.

    2. TToolDisplayTitle
       Resolves a human-readable label from an Anthropic block_type + tool
       name + (optional) input JSON. The internal Anthropic identifiers
       (bash_code_execution, text_editor_code_execution, code_execution,
       web_search, web_fetch, computer, mcp_tool, ...) are first mapped
       through a small TOOL_DISPLAY_NAMES table to user-facing names
       ("Shell command", "File editor", "Python execution", ...). Both
       exact match AND "<name>_<suffix>" prefix match are honored so
       versioned or paired tool kinds (code_execution_20260120,
       code_execution_tool_result, ...) resolve to the same label.

       When the input JSON is available (FromInput), the title is enriched
       with the salient argument: command/path for the shell and editor,
       query for web_search, url for web_fetch, action for computer, tool
       name for mcp_tool_use. The rendered title reads
       "Shell command - ls /tmp" rather than the raw "bash_code_execution".

    3. TToolUseDisplayDetails / TToolResultDisplayDetails
       Build structured detail text blocks for the tool entries.
         - TryFromSnapshot covers the input side for computer and MCP tools
           (the variants where the bare title is not informative enough).
         - TryFromEvent covers the result side for web_search, web_fetch,
           tool_search and mcp_tool_result, with a shared
           BuildToolErrorSummary fallback that parses error_code /
           error_message out of Block.RawContent.
       Both formatters return False when nothing useful can be rendered,
       so the caller can skip the extra UI line without checking for empty
       strings.

  Timing
  ------
  Anthropic emits tool input as content_block_delta chunks and only
  finalizes the assembled JSON at content_block_stop. Title resolution
  (FromInput) therefore runs from OnToolUseStop in the service layer, not
  at block_start; rendering earlier would label every tool call with the
  raw block-type identifier.

  Locality
  --------
  Display labels live in this unit on purpose. Keeping them out of
  Demo.Anthropic.Services avoids sprinkling user-facing strings through
  business logic and gives a single place to update when Anthropic ships a
  new tool kind or renames an existing one - extend TOOL_DISPLAY_NAMES,
  add the matching case branch in FromInput or TryFromEvent, and the live
  UI plus the persisted history both pick up the new label.

*)
{$ENDREGION}

type
  TToolDisplayNameMapItem = record
    InternalName: string;
    DisplayName: string;
  end;

{ TAnthropicDisplayBlockAggregator }

procedure TAnthropicDisplayBlockAggregator.RegisterToolUseStop(
  const Snapshot: TToolCallSnapshot;
  const DisplayTitle: string);
begin
  {--- Anthropic exposes the complete tool input only at block_stop. This
       adapter resolves the title from the Anthropic snapshot, then feeds the
       vendor-neutral Pythia aggregator. }
  var Title := DisplayTitle.Trim;
  if Title.IsEmpty then
    Title := Snapshot.ToolName.Trim;
  if Title.IsEmpty then
    Title := Snapshot.BlockType.ToString;

  AppendToolUse(Snapshot.ToolId, Title);
end;

procedure TAnthropicDisplayBlockAggregator.RegisterToolResultStop(
  const Snapshot: TToolResultSnapshot);
begin
  {--- Anthropic reports the final error state at block_stop. Pythia only
       needs to know which tool block must become an error block. }
  if Snapshot.IsError then
    MarkToolError(Snapshot.ToolUseId);

  CloseCurrent;
end;

{ TToolDisplayTitle }

class function TToolDisplayTitle.BaseTitle(
  const ABlockType: TContentBlockType;
  const AToolName: string): string;
begin
  Result := ResolveDisplayName(AToolName);
  if Result.IsEmpty then
    Result := ResolveDisplayName(ABlockType.ToString);
  if Result.IsEmpty then
    Result := AToolName.Trim;
  if Result.IsEmpty then
    Result := ABlockType.ToString;
end;

class function TToolDisplayTitle.HumanizeIdentifier(
  const Value: string): string;
begin
  Result := Value.Trim.Replace('_', ' ');
end;

class function TToolDisplayTitle.ResolveDisplayName(
  const AInternalName: string): string;
const
  TOOL_DISPLAY_NAMES: array[0..14] of TToolDisplayNameMapItem = (
    (InternalName: 'bash_code_execution'; DisplayName: 'Shell command'),
    (InternalName: 'bash_code_execution_tool_result'; DisplayName: 'Shell command'),
    (InternalName: 'text_editor_code_execution'; DisplayName: 'File editor'),
    (InternalName: 'text_editor_code_execution_tool_result'; DisplayName: 'File editor'),
    (InternalName: 'code_execution'; DisplayName: 'Python execution'),
    (InternalName: 'python_code_execution'; DisplayName: 'Python execution'),
    (InternalName: 'code_execution_tool_result'; DisplayName: 'Python execution'),
    (InternalName: 'web_search'; DisplayName: 'Web search'),
    (InternalName: 'web_search_tool_result'; DisplayName: 'Web search'),
    (InternalName: 'web_fetch'; DisplayName: 'Web fetch'),
    (InternalName: 'web_fetch_tool_result'; DisplayName: 'Web fetch'),
    (InternalName: 'computer'; DisplayName: 'Computer control'),
    (InternalName: 'tool_search'; DisplayName: 'Tool search'),
    (InternalName: 'tool_search_tool_result'; DisplayName: 'Tool search'),
    (InternalName: 'mcp_tool'; DisplayName: 'MCP tool')
  );
begin
  var Normalized := AInternalName.Trim.ToLowerInvariant;
  if Normalized.IsEmpty then
    Exit('');

  for var Item in TOOL_DISPLAY_NAMES do
    if (Normalized = Item.InternalName) or
       Normalized.StartsWith(Item.InternalName + '_') then
      Exit(Item.DisplayName);

  Result := '';
end;

class function TToolDisplayTitle.TryReadInputJsonString(
  const Reader: TJsonReader;
  const FieldName: string;
  out Value: string): Boolean;
begin
  Value := Reader.AsString(FieldName);
  Result := not Value.Trim.IsEmpty;
end;

class function TToolDisplayTitle.FromBlockType(
  const ABlockType: TContentBlockType): string;
begin
  Result := BaseTitle(ABlockType, '');
end;

class function TToolDisplayTitle.FromInput(
  const ABlockType: TContentBlockType;
  const AToolName, AInputJson: string): string;
var
  Command: string;
  Path: string;
  Query: string;
  Url: string;
  Action: string;
begin
  {--- The input JSON is complete only at content_block_stop. This helper keeps
       title extraction close to stream display concerns while avoiding
       localized labels in the vendor service. }
  var ToolTitle := BaseTitle(ABlockType, AToolName);

  Result := ToolTitle;

  if AInputJson.Trim.IsEmpty then
    Exit;

  var Reader := TJsonReader.Parse(AInputJson);
  if not Reader.IsValid then
    Exit;

  case ABlockType of
    TContentBlockType.server_tool_use,
    TContentBlockType.tool_use:
      begin
        {--- bash uses "command", text_editor uses "command" + "path",
             web_search uses "query", web_fetch uses "url", computer uses
             "action". Try the richest combinations first. }
        if AToolName.Trim.ToLowerInvariant.StartsWith('computer') and
           TToolDisplayTitle.TryReadInputJsonString(Reader, 'action', Action) then
          begin
            Result := Format('%s - %s', [ToolTitle, HumanizeIdentifier(Action)]);
            Exit;
          end;

        if TToolDisplayTitle.TryReadInputJsonString(Reader, 'command', Command) then
          begin
            if TToolDisplayTitle.TryReadInputJsonString(Reader, 'path', Path) then
              Result := Format('%s - %s %s', [ToolTitle, Command, Path])
            else
              Result := Format('%s - %s', [ToolTitle, Command]);
            Exit;
          end;

        if TToolDisplayTitle.TryReadInputJsonString(Reader, 'path', Path) then
          begin
            Result := Format('%s - %s', [ToolTitle, Path]);
            Exit;
          end;

        if TToolDisplayTitle.TryReadInputJsonString(Reader, 'query', Query) then
          begin
            Result := Format('%s - %s', [ToolTitle, Query]);
            Exit;
          end;

        if TToolDisplayTitle.TryReadInputJsonString(Reader, 'url', Url) then
          begin
            Result := Format('%s - %s', [ToolTitle, Url]);
            Exit;
          end;
      end;

    TContentBlockType.mcp_tool_use:
      begin
        if TToolDisplayTitle.TryReadInputJsonString(Reader, 'name', Command) or
           TToolDisplayTitle.TryReadInputJsonString(Reader, 'tool', Command) then
          begin
            Result := Format('%s - %s', [ToolTitle, Command]);
            Exit;
          end;
      end;
  end;
end;

{ TToolUseDisplayDetails }

class procedure TToolUseDisplayDetails.AppendDetailLine(
  const Builder: TStringBuilder;
  const LabelName, Value: string);
begin
  var Text := Value.Trim;
  if Text.IsEmpty then
    Exit;

  Builder.Append(LabelName).Append(': ').AppendLine(Text);
end;

class procedure TToolUseDisplayDetails.AppendInputJson(
  const Builder: TStringBuilder;
  const LabelName, Value: string);
begin
  var Text := FormatJson(Value);
  if Text.IsEmpty or SameText(Text, '{}') then
    Exit;

  Builder.Append(LabelName).Append(':').AppendLine;
  Builder.AppendLine(Text);
end;

class function TToolUseDisplayDetails.FormatJson(
  const Value: string): string;
begin
  Result := Value.Trim;
  if Result.IsEmpty then
    Exit;

  var Reader := TJsonReader.Parse(Result);
  if Reader.IsValid then
    Result := Reader.Format(2).Trim;
end;

class function TToolUseDisplayDetails.HumanizeIdentifier(
  const Value: string): string;
begin
  Result := Value.Trim.Replace('_', ' ');
end;

class function TToolUseDisplayDetails.IsComputerTool(
  const Snapshot: TToolCallSnapshot): Boolean;
begin
  var ToolName := Snapshot.ToolName.Trim.ToLowerInvariant;
  Result := (ToolName = 'computer') or ToolName.StartsWith('computer_');
end;

class function TToolUseDisplayDetails.BuildComputerDetails(
  const Snapshot: TToolCallSnapshot;
  out Details: string): Boolean;
begin
  Details := '';
  if not IsComputerTool(Snapshot) then
    Exit(False);

  var Reader := TJsonReader.Parse(Snapshot.InputJson);
  if not Reader.IsValid then
    Exit(False);

  var Action := HumanizeIdentifier(Reader.AsString('action'));
  var Coordinate := Reader.AsString('coordinate').Trim;
  var StartCoordinate := Reader.AsString('start_coordinate').Trim;
  var InputText := Reader.AsString('text').Trim;
  var Key := Reader.AsString('key').Trim;
  var ScrollDirection := HumanizeIdentifier(Reader.AsString('scroll_direction'));
  var ScrollAmount := Reader.AsString('scroll_amount').Trim;
  var Duration := Reader.AsString('duration').Trim;

  var Builder := TStringBuilder.Create;
  try
    Builder.AppendLine('Computer action details:');
    AppendDetailLine(Builder, 'Action', Action);
    AppendDetailLine(Builder, 'Coordinate', Coordinate);
    AppendDetailLine(Builder, 'Start coordinate', StartCoordinate);
    AppendDetailLine(Builder, 'Text', InputText);
    AppendDetailLine(Builder, 'Key', Key);
    AppendDetailLine(Builder, 'Scroll direction', ScrollDirection);
    AppendDetailLine(Builder, 'Scroll amount', ScrollAmount);
    AppendDetailLine(Builder, 'Duration', Duration);

    Details := Builder.ToString.Trim;
    Result := not SameText(Details, 'Computer action details:');
  finally
    Builder.Free;
  end;
end;

class function TToolUseDisplayDetails.BuildMCPToolUseDetails(
  const Snapshot: TToolCallSnapshot;
  out Details: string): Boolean;
begin
  Details := '';
  if Snapshot.BlockType <> TContentBlockType.mcp_tool_use then
    Exit(False);

  var Builder := TStringBuilder.Create;
  try
    Builder.AppendLine('MCP tool call details:');
    AppendDetailLine(Builder, 'Server', Snapshot.ServerName);
    AppendDetailLine(Builder, 'Tool', Snapshot.ToolName);
    AppendInputJson(Builder, 'Input', Snapshot.InputJson);

    Details := Builder.ToString.Trim;
    Result := not SameText(Details, 'MCP tool call details:');
  finally
    Builder.Free;
  end;
end;

class function TToolUseDisplayDetails.TryFromSnapshot(
  const Snapshot: TToolCallSnapshot;
  out Details: string): Boolean;
begin
  if BuildComputerDetails(Snapshot, Details) then
    Exit(True);

  Result := BuildMCPToolUseDetails(Snapshot, Details);
end;

{ TToolResultDisplayDetails }

class procedure TToolResultDisplayDetails.AppendDetailLine(
  const Builder: TStringBuilder;
  const LabelName, Value: string);
begin
  var Text := Value.Trim;
  if Text.IsEmpty then
    Exit;

  Builder.Append(LabelName).Append(': ').AppendLine(Text);
end;

class function TToolResultDisplayDetails.BuildToolErrorSummary(
  const AFailureLabel, AErrorCode, AErrorMessage: string): string;
var
  ErrorText: string;
begin
  var Code := HumanizeIdentifier(AErrorCode);
  var Message := AErrorMessage.Trim;

  if Code.IsEmpty and Message.IsEmpty then
    Exit('');

  if Code.IsEmpty then
    ErrorText := Message
  else
    if Message.IsEmpty then
      ErrorText := Code
    else
      ErrorText := Format('%s - %s', [Code, Message]);

  Result := Format('%s: %s', [AFailureLabel, ErrorText]);
end;

class function TToolResultDisplayDetails.BuildToolErrorSummary(
  const AFailureLabel, ARawContent: string): string;
begin
  Result := '';

  var Reader := TJsonReader.Parse(ARawContent);
  if not Reader.IsValid then
    Exit;

  var ErrorCode := Reader.AsString('error_code').Trim;
  if ErrorCode.IsEmpty then
    ErrorCode := Reader.AsString('content.error_code').Trim;

  var ErrorMessage := Reader.AsString('error_message').Trim;
  if ErrorMessage.IsEmpty then
    ErrorMessage := Reader.AsString('message').Trim;
  if ErrorMessage.IsEmpty then
    ErrorMessage := Reader.AsString('content.error_message').Trim;

  Result := BuildToolErrorSummary(AFailureLabel, ErrorCode, ErrorMessage);
end;

class function TToolResultDisplayDetails.BuildWebFetchDetails(
  const Block: TContentBlock;
  out Details: string): Boolean;
begin
  Details := '';

  if not Assigned(Block.ToolContent) then
    Exit(False);

  var FetchBlock := Block.ToolContent.WebFetchToolResultBlock;
  if (not Assigned(FetchBlock)) or (not FetchBlock.HasValue) then
    begin
      Details := BuildToolErrorSummary('Fetch failed', Block.RawContent);
      Exit(not Details.IsEmpty);
    end;

  var Content := FetchBlock.Content;
  if not Assigned(Content) then
    Exit(False);

  if not Content.ErrorCode.Trim.IsEmpty then
    begin
      Details := BuildToolErrorSummary('Fetch failed', Content.ErrorCode, '');
      Exit(not Details.IsEmpty);
    end;

  var Title := '';
  var MediaType := '';

  if Assigned(Content.Content) then
    begin
      Title := Content.Content.Title.Trim;
      if Assigned(Content.Content.Source) then
        MediaType := Content.Content.Source.MediaType.Trim;
    end;

  var Url := Content.Url.Trim;
  var RetrievedAt := Content.RetrievedAt.Trim;

  var Builder := TStringBuilder.Create;
  try
    Builder.AppendLine('Fetched web content:');
    var HasDetails := False;

    if not Title.IsEmpty then
      begin
        AppendDetailLine(Builder, 'Title', Title);
        HasDetails := True;
      end;

    if not Url.IsEmpty then
      begin
        AppendDetailLine(Builder, 'URL', Url);
        HasDetails := True;
      end;

    if not RetrievedAt.IsEmpty then
      begin
        AppendDetailLine(Builder, 'Retrieved at', RetrievedAt);
        HasDetails := True;
      end;

    if not MediaType.IsEmpty then
      begin
        AppendDetailLine(Builder, 'Media type', MediaType);
        HasDetails := True;
      end;

    Details := Builder.ToString.Trim;
    Result := HasDetails and not Details.IsEmpty;
  finally
    Builder.Free;
  end;
end;

class function TToolResultDisplayDetails.BuildWebSearchDetails(
  const Block: TContentBlock;
  out Details: string): Boolean;
begin
  Details := '';

  if not Assigned(Block.ToolContent) then
    Exit(False);

  var SearchBlock := Block.ToolContent.WebSearchToolResultBlock;
  if (not Assigned(SearchBlock)) or (not SearchBlock.HasValue) then
    begin
      Details := BuildToolErrorSummary('Search failed', Block.RawContent);
      Exit(not Details.IsEmpty);
    end;

  var ContentCount := Length(SearchBlock.Content);
  if ContentCount = 0 then
    Exit(False);

  var Builder := TStringBuilder.Create;
  try
    if ContentCount = 1 then
      Builder.AppendLine('Found 1 web result:')
    else
      Builder.AppendLine(Format('Found %d web results:', [ContentCount]));

    var ResultIndex := 0;
    for var Item in SearchBlock.Content do
      begin
        if not Assigned(Item) then
          Continue;

        var Title := Item.Title.Trim;
        var Url := Item.Url.Trim;
        var PageAge := Item.PageAge.Trim;

        if Title.IsEmpty and Url.IsEmpty then
          Continue;

        Inc(ResultIndex);
        Builder.Append(Format('%d. ', [ResultIndex]));

        if not Title.IsEmpty then
          Builder.Append(Title)
        else
          Builder.Append(Url);

        if not Url.IsEmpty and not SameText(Url, Title) then
          Builder.AppendLine.Append('   ').Append(Url);

        if not PageAge.IsEmpty then
          Builder.AppendLine.Append('   Page age: ').Append(PageAge);

        Builder.AppendLine;
      end;

    Details := Builder.ToString.Trim;
    Result := not Details.IsEmpty;
  finally
    Builder.Free;
  end;
end;

class function TToolResultDisplayDetails.BuildToolSearchDetails(
  const Block: TContentBlock;
  out Details: string): Boolean;
begin
  Details := '';

  if not Assigned(Block.ToolContent) then
    Exit(False);

  var SearchBlock := Block.ToolContent.ToolSearchToolResultBlock;
  if (not Assigned(SearchBlock)) or (not SearchBlock.HasValue) then
    begin
      Details := BuildToolErrorSummary('Tool search failed', Block.RawContent);
      Exit(not Details.IsEmpty);
    end;

  var Content := SearchBlock.Content;
  if not Assigned(Content) then
    Exit(False);

  if (not Content.ErrorCode.Trim.IsEmpty) or
     (not Content.ErrorMessage.Trim.IsEmpty) then
    begin
      Details := BuildToolErrorSummary(
        'Tool search failed',
        Content.ErrorCode,
        Content.ErrorMessage);
      Exit(not Details.IsEmpty);
    end;

  var ContentCount := Length(Content.ToolReferences);
  if ContentCount = 0 then
    begin
      Details := 'No matching tools found.';
      Exit(True);
    end;

  var Builder := TStringBuilder.Create;
  try
    if ContentCount = 1 then
      Builder.AppendLine('Found 1 tool reference:')
    else
      Builder.AppendLine(Format('Found %d tool references:', [ContentCount]));

    var ResultIndex := 0;
    for var Item in Content.ToolReferences do
      begin
        if not Assigned(Item) then
          Continue;

        var ToolName := Item.ToolName.Trim;
        if ToolName.IsEmpty then
          Continue;

        Inc(ResultIndex);
        Builder.AppendLine(Format('%d. %s', [ResultIndex, ToolName]));
      end;

    Details := Builder.ToString.Trim;
    if Details.IsEmpty or (ResultIndex = 0) then
      Details := 'No matching tools found.';
    Result := not Details.IsEmpty;
  finally
    Builder.Free;
  end;
end;

class function TToolResultDisplayDetails.BuildMCPToolDetails(
  const Block: TContentBlock;
  out Details: string): Boolean;
begin
  Details := '';

  if not Assigned(Block.ToolContent) then
    Exit(False);

  var MCPBlock := Block.ToolContent.MCPToolResultBlock;
  if (not Assigned(MCPBlock)) or (not MCPBlock.HasValue) then
    begin
      if Block.IsError then
        Details := BuildToolErrorSummary('MCP tool failed', Block.RawContent);
      Exit(not Details.IsEmpty);
    end;

  var Builder := TStringBuilder.Create;
  try
    if Block.IsError then
      Builder.AppendLine('MCP tool failed:')
    else
      Builder.AppendLine('MCP tool returned:');

    if SameText(MCPBlock.&Type, 'text') then
      begin
        var TextValue := MCPBlock.StringContent.Trim;
        if not TextValue.IsEmpty then
          AppendDetailLine(Builder, 'Text', TextValue);
      end
    else
      begin
        var ContentCount := Length(MCPBlock.Content);
        if ContentCount > 0 then
          AppendDetailLine(Builder, 'Content blocks', IntToStr(ContentCount));

        var TextBlockCount := 0;
        var CitationCount := 0;
        for var Item in MCPBlock.Content do
          begin
            if not Assigned(Item) then
              Continue;

            Inc(CitationCount, Length(Item.Citations));
            var TextValue := Item.Text.Trim;
            if TextValue.IsEmpty then
              Continue;

            Inc(TextBlockCount);
            Builder.AppendLine(Format('%d. %s', [TextBlockCount, TextValue]));
          end;

        if CitationCount > 0 then
          AppendDetailLine(Builder, 'Citations', IntToStr(CitationCount));
      end;

    Details := Builder.ToString.Trim;
    if SameText(Details, 'MCP tool failed:') then
      Details := 'MCP tool failed.';

    Result := not Details.IsEmpty and
      not SameText(Details, 'MCP tool returned:') and
      not SameText(Details, 'MCP tool failed:');
  finally
    Builder.Free;
  end;
end;

class function TToolResultDisplayDetails.HumanizeIdentifier(
  const Value: string): string;
begin
  Result := Value.Trim.Replace('_', ' ');
end;

class function TToolResultDisplayDetails.TryFromEvent(
  const Event: TChatStream;
  out Details: string): Boolean;
begin
  Details := '';

  if (not Assigned(Event)) or
     (Event.EventType <> TEventType.content_block_start) then
    Exit(False);

  var Block := Event.ContentBlock;
  if not Assigned(Block) then
    Exit(False);

  case Block.&Type of
    TContentBlockType.web_search_tool_result:
      Exit(BuildWebSearchDetails(Block, Details));

    TContentBlockType.web_fetch_tool_result:
      Exit(BuildWebFetchDetails(Block, Details));

    TContentBlockType.tool_search_tool_result:
      Exit(BuildToolSearchDetails(Block, Details));

    TContentBlockType.mcp_tool_result:
      Exit(BuildMCPToolDetails(Block, Details));
  else
    Result := False;
  end;
end;

end.
