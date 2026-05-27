unit Demo.Anthropic.Agent.Markdown;

interface

uses
  System.SysUtils,
  Demo.Anthropic.Agent.Cards;

type
  TAgentMarkdownReader = record
  public
    /// <summary>
    /// Reads a Markdown-backed agent definition. The supported format is a
    /// small YAML frontmatter subset followed by Markdown sections:
    /// "# Coordinator" and "# Subagent: ref".
    /// </summary>
    class function TryReadFile(const FileName, CardId: string;
      out Def: TAgentCardDefinition): Boolean; overload; static;
    class function TryReadFile(const FileName, CardId,
      EnvelopeVersion: string;
      out Def: TAgentCardDefinition): Boolean; overload; static;
  end;

implementation

{$REGION 'Dev note'}
(*

  Markdown-backed agent-card parsing for the pythia-anthropic VCL demo.

  Some demo agents are easier to maintain as Markdown files than as escaped
  JSON strings inside the card catalog. This unit reads those files and turns
  a small frontmatter subset plus Markdown sections into the same
  TAgentCardDefinition shape produced by Demo.Anthropic.Agent.Cards.

  The frontmatter parser is intentionally tiny: it supports the fields used by
  the bundled cards, simple nested blocks, booleans, inline lists and indented
  list items. It is not meant to be a full YAML implementation.

  The Markdown body supplies long prompts. Multi-agent cards use "# Coordinator"
  and "# Subagent: ref" headings to bind prompt text to the coordinator and
  named sub-agents. Single-agent cards use "# Agent" when present, otherwise
  the whole body becomes the agent system prompt.

  Versioning is validated against the card envelope when one is provided, so a
  stale md_path target cannot silently reuse the wrong cloud agent definition.

*)
{$ENDREGION}

uses
  System.Classes, System.IOUtils,
  WVPythia.TextFile.Helper;

type
  TYamlMini = record
  private
    class function SplitLines(const Text: string): TArray<string>; static;
    class function IndentOf(const Line: string): Integer; static;
    class function IsBlank(const Line: string): Boolean; static;
    class function IsListItem(const Line: string): Boolean; static;
    class function Unquote(const Value: string): string; static;
    class function SplitKeyValue(const Text: string; out Key, Value: string): Boolean; static;
    class function LineHasKey(const Line, Key: string): Boolean; static;
    class function BlockEnd(const Lines: TArray<string>; const StartIndex,
      MaxIndex, Indent: Integer): Integer; static;
    class function FindBlock(const Lines: TArray<string>; const StartIndex,
      EndIndex, Indent: Integer; const Key: string; out BlockStart,
      BlockFinish: Integer): Boolean; static;
    class function ReadScalar(const Lines: TArray<string>; const StartIndex,
      EndIndex, Indent: Integer; const Key: string; const Default: string = ''): string; static;
    class function ParseBool(const Value: string; const Default: Boolean): Boolean; static;
    class function ReadStringList(const Lines: TArray<string>; const StartIndex,
      EndIndex, Indent: Integer; const Key: string): TArray<string>; static;
    class function ParseInlineList(const Value: string): TArray<string>; static;
    class function ParseTools(const Lines: TArray<string>; const StartIndex,
      EndIndex, ToolsIndent: Integer): TAgentToolsDef; static;
    class function ParseKind(const Value: string): TAgentCardKind; static;
  public
    class function ExtractFrontmatterAndBody(const Markdown: string;
      out Frontmatter, Body: string): Boolean; static;
    class function ParseDefinition(const FileName, Markdown, CardId: string;
      out Def: TAgentCardDefinition): Boolean; static;
  end;

  TMarkdownSections = record
  private
    class function JoinTrimmed(const Lines: TArray<string>; const FirstIndex,
      LastIndex: Integer): string; static;
    class function SplitLines(const Text: string): TArray<string>; static;
    class function ExtractHeading(const Body, Heading: string): string; static;
  public
    class function Coordinator(const Body: string): string; static;
    class function Subagent(const Body, Ref, Name: string): string; static;
  end;

{ TYamlMini }

class function TYamlMini.SplitLines(const Text: string): TArray<string>;
begin
  var Normalized := Text.Replace(#13#10, #10).Replace(#13, #10);
  Normalized := Normalized.Replace(#10, sLineBreak);

  var Lines := TStringList.Create;
  try
    Lines.Text := Normalized;
    SetLength(Result, Lines.Count);
    for var I := 0 to Lines.Count - 1 do
      Result[I] := Lines[I];
  finally
    Lines.Free;
  end;
end;

class function TYamlMini.IndentOf(const Line: string): Integer;
begin
  Result := 0;
  while (Result < Line.Length) and (Line[Result + 1] = ' ') do
    Inc(Result);
end;

class function TYamlMini.IsBlank(const Line: string): Boolean;
begin
  Result := Line.Trim.IsEmpty;
end;

class function TYamlMini.IsListItem(const Line: string): Boolean;
begin
  Result := Line.TrimLeft.StartsWith('- ');
end;

class function TYamlMini.Unquote(const Value: string): string;
begin
  Result := Value.Trim;
  if Result.Length < 2 then
    Exit;

  if ((Result[1] = '"') and (Result[Result.Length] = '"')) or
     ((Result[1] = '''') and (Result[Result.Length] = '''')) then
    Result := Result.Substring(1, Result.Length - 2);

  Result := Result.Replace('\"', '"');
end;

class function TYamlMini.SplitKeyValue(const Text: string; out Key,
  Value: string): Boolean;
begin
  Key := '';
  Value := '';

  var P := Text.IndexOf(':');
  if P < 0 then
    Exit(False);

  Key := Text.Substring(0, P).Trim;
  Value := Unquote(Text.Substring(P + 1));
  Result := not Key.IsEmpty;
end;

class function TYamlMini.LineHasKey(const Line, Key: string): Boolean;
begin
  var K, V: string;
  Result := SplitKeyValue(Line.Trim, K, V) and SameText(K, Key);
end;

class function TYamlMini.BlockEnd(const Lines: TArray<string>;
  const StartIndex, MaxIndex, Indent: Integer): Integer;
begin
  Result := MaxIndex;
  for var I := StartIndex + 1 to MaxIndex do
    if (not IsBlank(Lines[I])) and (IndentOf(Lines[I]) <= Indent) then
      Exit(I - 1);
end;

class function TYamlMini.FindBlock(const Lines: TArray<string>;
  const StartIndex, EndIndex, Indent: Integer; const Key: string;
  out BlockStart, BlockFinish: Integer): Boolean;
begin
  Result := False;
  BlockStart := -1;
  BlockFinish := -1;

  for var I := StartIndex to EndIndex do
    if (IndentOf(Lines[I]) = Indent) and LineHasKey(Lines[I], Key) then
      begin
        BlockStart := I;
        BlockFinish := BlockEnd(Lines, I, EndIndex, Indent);
        Exit(True);
      end;
end;

class function TYamlMini.ReadScalar(const Lines: TArray<string>;
  const StartIndex, EndIndex, Indent: Integer; const Key,
  Default: string): string;
begin
  for var I := StartIndex to EndIndex do
    if (IndentOf(Lines[I]) = Indent) and LineHasKey(Lines[I], Key) then
      begin
        var K, V: string;
        SplitKeyValue(Lines[I].Trim, K, V);
        Exit(V);
      end;

  Result := Default;
end;

class function TYamlMini.ParseBool(const Value: string;
  const Default: Boolean): Boolean;
begin
  if SameText(Value, 'true') then
    Exit(True);

  if SameText(Value, 'false') then
    Exit(False);

  Result := Default;
end;

class function TYamlMini.ParseInlineList(const Value: string): TArray<string>;
begin
  Result := [];

  var S := Value.Trim;
  if (S.Length < 2) or (S[1] <> '[') or (S[S.Length] <> ']') then
    Exit;

  S := S.Substring(1, S.Length - 2);
  for var Item in S.Split([',']) do
    begin
      var V := Unquote(Item);
      if not V.Trim.IsEmpty then
        Result := Result + [V.Trim];
    end;
end;

class function TYamlMini.ReadStringList(const Lines: TArray<string>;
  const StartIndex, EndIndex, Indent: Integer; const Key: string): TArray<string>;
begin
  Result := [];

  for var I := StartIndex to EndIndex do
    if (IndentOf(Lines[I]) = Indent) and LineHasKey(Lines[I], Key) then
      begin
        var K, V: string;
        SplitKeyValue(Lines[I].Trim, K, V);

        if not V.Trim.IsEmpty then
          Exit(ParseInlineList(V));

        var ListEnd := BlockEnd(Lines, I, EndIndex, Indent);
        for var J := I + 1 to ListEnd do
          if (IndentOf(Lines[J]) > Indent) and IsListItem(Lines[J]) then
            begin
              var Item := Lines[J].TrimLeft.Substring(2).Trim;
              if not Item.IsEmpty then
                Result := Result + [Unquote(Item)];
            end;
        Exit;
      end;
end;

class function TYamlMini.ParseTools(const Lines: TArray<string>;
  const StartIndex, EndIndex, ToolsIndent: Integer): TAgentToolsDef;
begin
  Result := Default(TAgentToolsDef);

  var ToolsStart, ToolsEnd: Integer;
  if not FindBlock(Lines, StartIndex, EndIndex, ToolsIndent, 'tools',
    ToolsStart, ToolsEnd) then
    Exit;

  var BuiltinStart, BuiltinEnd: Integer;
  if not FindBlock(Lines, ToolsStart + 1, ToolsEnd, ToolsIndent + 2, 'builtin',
    BuiltinStart, BuiltinEnd) then
    Exit;

  Result.Builtin.Defined := True;

  var DefaultStart, DefaultEnd: Integer;
  if FindBlock(Lines, BuiltinStart + 1, BuiltinEnd, ToolsIndent + 4, 'default',
     DefaultStart, DefaultEnd) then
    begin
      Result.Builtin.DefaultEnabled := ParseBool(
        ReadScalar(Lines, DefaultStart + 1, DefaultEnd, ToolsIndent + 6,
          'enabled', 'false'), False);
      Result.Builtin.DefaultPolicy :=
        ReadScalar(Lines, DefaultStart + 1, DefaultEnd, ToolsIndent + 6,
          'policy');
    end;

  var ConfigsStart, ConfigsEnd: Integer;
  if not FindBlock(Lines, BuiltinStart + 1, BuiltinEnd, ToolsIndent + 4,
    'configs', ConfigsStart, ConfigsEnd) then
    Exit;

  var I := ConfigsStart + 1;
  while I <= ConfigsEnd do
    begin
      if (IndentOf(Lines[I]) = ToolsIndent + 6) and IsListItem(Lines[I]) then
        begin
          var ItemStart := I;
          var ItemEnd := ConfigsEnd;
          for var J := I + 1 to ConfigsEnd do
            if (IndentOf(Lines[J]) = ToolsIndent + 6) and IsListItem(Lines[J]) then
              begin
                ItemEnd := J - 1;
                Break;
              end;

          var Cfg := Default(TBuiltinToolConfig);
          var First := Lines[I].TrimLeft.Substring(2).Trim;
          if not First.IsEmpty then
            begin
              var K, V: string;
              if SplitKeyValue(First, K, V) and SameText(K, 'name') then
                Cfg.Name := V;
            end;

          Cfg.Enabled := ParseBool(
            ReadScalar(Lines, ItemStart + 1, ItemEnd, ToolsIndent + 8,
              'enabled', 'false'), False);
          Cfg.Policy :=
            ReadScalar(Lines, ItemStart + 1, ItemEnd, ToolsIndent + 8,
              'policy');

          if not Cfg.Name.Trim.IsEmpty then
            Result.Builtin.Configs := Result.Builtin.Configs + [Cfg];

          I := ItemEnd + 1;
        end
      else
        Inc(I);
    end;
end;

class function TYamlMini.ParseKind(const Value: string): TAgentCardKind;
begin
  if SameText(Value, 'single') then
    Exit(ackSingle);

  if SameText(Value, 'multiagent') then
    Exit(ackMultiagent);

  Result := ackUnknown;
end;

class function TYamlMini.ExtractFrontmatterAndBody(const Markdown: string;
  out Frontmatter, Body: string): Boolean;
begin
  Frontmatter := '';
  Body := '';
  Result := False;

  var Lines := SplitLines(Markdown);
  if (Length(Lines) < 3) or (Lines[0].Trim <> '---') then
    Exit;

  var EndIndex := -1;
  for var I := 1 to High(Lines) do
    if Lines[I].Trim = '---' then
      begin
        EndIndex := I;
        Break;
      end;

  if EndIndex < 0 then
    Exit;

  var Fm := TStringList.Create;
  var Bd := TStringList.Create;
  try
    for var I := 1 to EndIndex - 1 do
      Fm.Add(Lines[I]);

    for var I := EndIndex + 1 to High(Lines) do
      Bd.Add(Lines[I]);

    Frontmatter := Fm.Text.Trim;
    Body := Bd.Text.Trim;
    Result := not Frontmatter.IsEmpty;
  finally
    Fm.Free;
    Bd.Free;
  end;
end;

class function TYamlMini.ParseDefinition(const FileName, Markdown,
  CardId: string; out Def: TAgentCardDefinition): Boolean;
begin
  Def := Default(TAgentCardDefinition);
  Result := False;

  var Frontmatter, Body: string;
  if not ExtractFrontmatterAndBody(Markdown, Frontmatter, Body) then
    Exit;

  var Lines := SplitLines(Frontmatter);
  if Length(Lines) = 0 then
    Exit;

  var RootEnd := High(Lines);
  var SourceId := CardId.Trim;
  if SourceId.IsEmpty then
    SourceId := ReadScalar(Lines, 0, RootEnd, 0, 'id');
  if SourceId.IsEmpty then
    Exit;

  Def.CardId := SourceId;
  Def.Version := ReadScalar(Lines, 0, RootEnd, 0, 'version', '0.0.0-dev');
  Def.Kind := ParseKind(ReadScalar(Lines, 0, RootEnd, 0, 'kind'));
  if Def.Kind = ackUnknown then
    Exit;

  var EnvStart, EnvEnd: Integer;
  if FindBlock(Lines, 0, RootEnd, 0, 'environment', EnvStart, EnvEnd) then
    begin
      Def.Environment.Name := ReadScalar(Lines, EnvStart + 1, EnvEnd, 2, 'name');
      Def.Environment.Description :=
        ReadScalar(Lines, EnvStart + 1, EnvEnd, 2, 'description');
    end;

  var SessionStart, SessionEnd: Integer;
  if FindBlock(Lines, 0, RootEnd, 0, 'session', SessionStart, SessionEnd) then
    Def.Session.Title := ReadScalar(Lines, SessionStart + 1, SessionEnd, 2, 'title');

  case Def.Kind of
    ackSingle:
      begin
        var AgentStart, AgentEnd: Integer;
        if FindBlock(Lines, 0, RootEnd, 0, 'agent', AgentStart, AgentEnd) then
          begin
            Def.Agent.Name :=
              ReadScalar(Lines, AgentStart + 1, AgentEnd, 2, 'name',
                ReadScalar(Lines, 0, RootEnd, 0, 'name'));
            Def.Agent.Model := ReadScalar(Lines, AgentStart + 1, AgentEnd, 2, 'model');
            Def.Agent.Tools := ParseTools(Lines, AgentStart + 1, AgentEnd, 2);
          end
        else
          Def.Agent.Name := ReadScalar(Lines, 0, RootEnd, 0, 'name');

        Def.Agent.System := TMarkdownSections.ExtractHeading(Body, 'Agent');
        if Def.Agent.System.Trim.IsEmpty then
          Def.Agent.System := Body.Trim;
      end;

    ackMultiagent:
      begin
        var CoordStart, CoordEnd: Integer;
        if FindBlock(Lines, 0, RootEnd, 0, 'coordinator', CoordStart, CoordEnd) then
          begin
            Def.Coordinator.Name :=
              ReadScalar(Lines, CoordStart + 1, CoordEnd, 2, 'name');
            Def.Coordinator.Model :=
              ReadScalar(Lines, CoordStart + 1, CoordEnd, 2, 'model');
            Def.Coordinator.Roster :=
              ReadStringList(Lines, CoordStart + 1, CoordEnd, 2, 'roster');
            Def.Coordinator.Tools := ParseTools(Lines, CoordStart + 1, CoordEnd, 2);
            Def.Coordinator.System := TMarkdownSections.Coordinator(Body);
          end;

        var SubsStart, SubsEnd: Integer;
        if FindBlock(Lines, 0, RootEnd, 0, 'subagents', SubsStart, SubsEnd) then
          begin
            var I := SubsStart + 1;
            while I <= SubsEnd do
              begin
                if (IndentOf(Lines[I]) = 2) and IsListItem(Lines[I]) then
                  begin
                    var ItemStart := I;
                    var ItemEnd := SubsEnd;
                    for var J := I + 1 to SubsEnd do
                      if (IndentOf(Lines[J]) = 2) and IsListItem(Lines[J]) then
                        begin
                          ItemEnd := J - 1;
                          Break;
                        end;

                    var Sub := Default(TSubAgentDef);
                    var First := Lines[I].TrimLeft.Substring(2).Trim;
                    if not First.IsEmpty then
                      begin
                        var K, V: string;
                        if SplitKeyValue(First, K, V) and SameText(K, 'ref') then
                          Sub.Ref := V;
                      end;

                    Sub.Ref := ReadScalar(Lines, ItemStart + 1, ItemEnd, 4, 'ref',
                      Sub.Ref);
                    Sub.Name := ReadScalar(Lines, ItemStart + 1, ItemEnd, 4, 'name');
                    Sub.Model := ReadScalar(Lines, ItemStart + 1, ItemEnd, 4, 'model');
                    Sub.Tools := ParseTools(Lines, ItemStart + 1, ItemEnd, 4);
                    Sub.System := TMarkdownSections.Subagent(Body, Sub.Ref, Sub.Name);

                    if not Sub.Ref.Trim.IsEmpty then
                      Def.SubAgents := Def.SubAgents + [Sub];

                    I := ItemEnd + 1;
                  end
                else
                  Inc(I);
              end;
          end;
      end;
  end;

  Def.Valid := True;
  Result := True;
end;

{ TMarkdownSections }

class function TMarkdownSections.SplitLines(const Text: string): TArray<string>;
begin
  Result := TYamlMini.SplitLines(Text);
end;

class function TMarkdownSections.JoinTrimmed(const Lines: TArray<string>;
  const FirstIndex, LastIndex: Integer): string;
begin
  var SL := TStringList.Create;
  try
    for var I := FirstIndex to LastIndex do
      SL.Add(Lines[I]);

    Result := SL.Text.Trim;
  finally
    SL.Free;
  end;
end;

class function TMarkdownSections.ExtractHeading(const Body,
  Heading: string): string;
begin
  Result := '';

  var Lines := SplitLines(Body);
  var Wanted := '# ' + Heading.Trim;
  var StartIndex := -1;

  for var I := 0 to High(Lines) do
    if SameText(Lines[I].Trim, Wanted) then
      begin
        StartIndex := I;
        Break;
      end;

  if StartIndex < 0 then
    Exit;

  var EndIndex := High(Lines);
  for var I := StartIndex + 1 to High(Lines) do
    if Lines[I].Trim.StartsWith('# ') then
      begin
        EndIndex := I - 1;
        Break;
      end;

  Result := JoinTrimmed(Lines, StartIndex + 1, EndIndex);
end;

class function TMarkdownSections.Coordinator(const Body: string): string;
begin
  Result := ExtractHeading(Body, 'Coordinator');
end;

class function TMarkdownSections.Subagent(const Body, Ref,
  Name: string): string;
begin
  Result := '';

  if not Ref.Trim.IsEmpty then
    Result := ExtractHeading(Body, 'Subagent: ' + Ref.Trim);

  if Result.Trim.IsEmpty and not Name.Trim.IsEmpty then
    Result := ExtractHeading(Body, 'Subagent: ' + Name.Trim);
end;

{ TAgentMarkdownReader }

class function TAgentMarkdownReader.TryReadFile(const FileName,
  CardId: string; out Def: TAgentCardDefinition): Boolean;
begin
  Result := TryReadFile(FileName, CardId, '', Def);
end;

class function TAgentMarkdownReader.TryReadFile(const FileName, CardId,
  EnvelopeVersion: string; out Def: TAgentCardDefinition): Boolean;
begin
  Def := Default(TAgentCardDefinition);
  Result := False;

  if FileName.Trim.IsEmpty or not TFile.Exists(FileName) then
    Exit;

  var Markdown := TFileIOHelper.LoadFromFile(FileName);
  Result := TYamlMini.ParseDefinition(FileName, Markdown, CardId, Def);
  if not Result then
    Exit;

  if EnvelopeVersion.Trim.IsEmpty then
    Exit;

  if SameText(Def.Version, '0.0.0-dev') then
    Def.Version := EnvelopeVersion.Trim
  else
  if not SameText(Def.Version, EnvelopeVersion.Trim) then
    begin
      Def := Default(TAgentCardDefinition);
      Result := False;
    end;
end;

end.
