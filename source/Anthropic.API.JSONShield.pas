unit Anthropic.API.JSONShield;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Anthropic.API.Consts;

type
  /// <summary>
  /// Provides a mechanism for converting or transforming specific JSON fields in a string
  /// before deserialization. Typically used to handle scenarios where certain fields
  /// contain complex or nested structures that need to be converted into valid JSON.
  /// </summary>
  /// <remarks>
  /// <para>
  /// Implementations of this interface can adjust the JSON string prior to the deserialization
  /// process to address inconsistencies, remove or replace invalid characters, or convert
  /// problematic JSON fields into formats that a JSON deserializer can handle properly.
  /// </para>
  /// <para>
  /// For example, the implementation may detect fields named "metadata" and convert their
  /// content from a raw, non-standard structure into a valid JSON string by replacing certain
  /// delimiters or escape characters.
  /// </para>
  /// </remarks>
  ICustomFieldsPrepare = interface
    ['{B09FEEBA-747E-4E6E-B916-ECBEC2467415}']
    /// <summary>
    /// Converts or transforms specified fields in the provided JSON string to ensure
    /// deserialization compatibility.
    /// </summary>
    /// <param name="Value">
    /// The raw JSON string containing fields that may require transformation.
    /// </param>
    /// <returns>
    /// A revised JSON string after applying the necessary field conversions or
    /// transformations.
    /// </returns>
    function Convert(const Value: string): string; overload;
  end;

  /// <summary>
  /// Implements the <c>ICustomFieldsPrepare</c> interface to transform specific JSON fields
  /// before deserialization. The class applies rules for handling particular fields that may
  /// contain nested objects or invalid characters, ensuring the resulting JSON is valid and
  /// ready for further processing.
  /// </summary>
  /// <remarks>
  /// <para>
  /// This class is most commonly used to modify JSON fields such as "metadata" that might
  /// otherwise cause errors during deserialization. It replaces problematic delimiters and
  /// characters (like quotation marks or braces) within specified field blocks, enabling
  /// compliant JSON output.
  /// </para>
  /// <para>
  /// <c>TDeserializationPrepare</c> scans the JSON string for patterns and adjusts the content
  /// accordingly, preventing typical parsing exceptions arising from malformed or unexpected
  /// inline structures.
  /// </para>
  /// </remarks>
  TDeserializationPrepare = class(TInterfacedObject, ICustomFieldsPrepare)
  public
    /// <summary>
    /// Scans and modifies the input JSON string to replace fields that contain nested
    /// objects or invalid characters, ensuring the JSON is suitable for deserialization.
    /// </summary>
    /// <param name="Value">
    /// The original JSON string needing transformation.
    /// </param>
    /// <returns>
    /// A revised JSON string after applying all necessary field modifications.
    /// </returns>
    /// <remarks>
    /// <para>
    /// The conversion logic is determined by which fields or patterns are defined
    /// within the implementation. Currently, the class targets fields labeled
    /// <c>"metadata"</c>, converting them from non-standard structures into proper
    /// JSON-friendly strings.
    /// </para>
    /// </remarks>
    function Convert(const Value: string): string;

    /// <summary>
    /// Factory method for creating an instance of the <c>TDeserializationPrepare</c> class.
    /// Returns an interface reference to <c>ICustomFieldsPrepare</c>.
    /// </summary>
    /// <returns>
    /// A newly constructed <c>TDeserializationPrepare</c> object as <c>ICustomFieldsPrepare</c>.
    /// </returns>
    /// <remarks>
    /// This method hides the constructor, enforcing interface-based usage.
    /// </remarks>
    class function CreateInstance: ICustomFieldsPrepare;
  end;

  TJsonPolyUnshield = record
  public
    class function Restore(const Arg: string): string; static;
  end;

implementation

type
  TJsonPolyShield = record
  public
    const S_QUOTE  = Char($E000);
    const S_BSLASH = Char($E001);
    const S_KIND_O = Char($E010);
    const S_KIND_A = Char($E011);

    class function Prepare(const Json: string; const Keys: array of string): string; static;
    class function Unshield(const Shielded: string): string; static;

  private
    class function ReadJsonString(const S: string; var I: Integer; out Text: string): Boolean; static;
    class procedure SkipSpaces(const S: string; var I: Integer); static;
    class function FindBlockEnd(const S: string; StartIdx: Integer): Integer; static;
    class function InSet(const Key: string; const SetKeys: TDictionary<string, Byte>): Boolean; static;
  end;

{ TJsonPolyShield }

class function TJsonPolyShield.InSet(const Key: string; const SetKeys: TDictionary<string, Byte>): Boolean;
begin
  Result := SetKeys.ContainsKey(Key);
end;

class procedure TJsonPolyShield.SkipSpaces(const S: string; var I: Integer);
begin
  while (I <= S.Length) and (S[I] <= ' ') do
    Inc(I);
end;

{--- Reads a JSON string from S[I] = '"', returns the minimally decoded text.
     Here we only want the key, so we retrieve the raw content, handling \" and \\
     to avoid premature termination.}
class function TJsonPolyShield.ReadJsonString(const S: string; var I: Integer; out Text: string): Boolean;
begin
  Result := False;
  Text := EmptyStr;

  if (I > S.Length) or (S[I] <> '"') then
    Exit;

  var StringBuilder := TStringBuilder.Create;
  try
    {--- skips the " }
    Inc(I);
    var esc := False;

    while I <= S.Length do
      begin
        if esc then
          begin
            {--- We keep the character as is; for the key, that's enough. }
            StringBuilder.Append(S[I]);
            esc := False;
          end
        else
          begin
            if S[I] = '\' then
              esc := True
            else if S[I] = '"' then
              begin
                {--- consumes the " }
                Inc(I);
                Text := StringBuilder.ToString;
                Exit(True);
              end
            else
              StringBuilder.Append(S[I]);
          end;
        Inc(I);
      end;

    {--- no closure }
    Exit(False);
  finally
    StringBuilder.Free;
  end;
end;

class function TJsonPolyShield.FindBlockEnd(const S: string; StartIdx: Integer): Integer;
begin
  var index := StartIdx;
  var inStr := False;
  var esc := False;
  var braceDepth := 0;
  var bracketDepth := 0;

  case S[index] of
    '{': braceDepth := 1;
    '[': bracketDepth := 1;
  else
    raise Exception.Create('StartIdx must point to { or [');
  end;

  Inc(index);

  while index <= S.Length do
    begin
      if inStr then
        begin
          if esc then
            esc := False
          else
            begin
              if S[index] = '\' then esc := True
              else if S[index] = '"' then inStr := False;
            end;
        end
      else
        begin
          case S[index] of
            '"': inStr := True;
            '{': Inc(braceDepth);
            '}': Dec(braceDepth);
            '[': Inc(bracketDepth);
            ']': Dec(bracketDepth);
          end;

          if (braceDepth = 0) and (bracketDepth = 0) then
            Exit(index);
        end;

      Inc(index);
    end;

  raise Exception.Create('Invalid JSON string (unclosed block)');
end;

class function TJsonPolyShield.Prepare(const Json: string; const Keys: array of string): string;
var
  Dictionary: TDictionary<string, Byte>;
  key: string;
  kind: Char;
begin
  Dictionary := TDictionary<string, Byte>.Create;
  try
    {--- Whitelist }
    for var Item in Keys do
      if not Item.IsEmpty then
        Dictionary.AddOrSetValue(Item, 0);

    var StringBuilder := TStringBuilder.Create(Json.Length + 64);
    try
      var i := 1;
      var n := Json.Length;

      while i <= n do
        begin
          {--- Raw copy until eventually encountering a string (beginning of a potential key) }
          if Json[i] <> '"' then
            begin
              StringBuilder.Append(Json[i]);
              Inc(i);
              Continue;
            end;

          {--- Attempt: read a JSON string (potential key) }
          var keyStart := i;
          var afterKey := i;
          if not ReadJsonString(Json, afterKey, key) then
            begin
              {--- The string is malformed; we copy the rest as is. }
              StringBuilder.Append(Copy(Json, i, n - i + 1));
              Break;
            end;

          {--- We don't know if it's a key or a value. We look after the string: }
          var j := afterKey;
          SkipSpaces(Json, j);

          if (j <= n) and (Json[j] = ':') and InSet(key, Dictionary) then
            begin
              {--- It's a whitelisted key: we look at the value}
              Inc(j); // --> jumps ':'
              SkipSpaces(Json, j);

              if (j <= n) and CharInSet(Json[j], ['{', '[']) then
                begin
                  {--- Copy '"key"' + spaces + ':' + spaces exactly as they were }
                  StringBuilder.Append(Copy(Json, keyStart, j - keyStart));

                  (* Shield of the block {..} or [..] => " + kind + shielded content + " *)
                  if Json[j] = '{' then
                    kind := S_KIND_O
                  else
                    kind := S_KIND_A;

                  var blockEnd := FindBlockEnd(Json, j);

                  StringBuilder.Append('"');
                  StringBuilder.Append(kind);

                  {--- Internal content = Json[j+1 .. blockEnd-1], shielding " and \ + " }
                  for var p := j + 1 to blockEnd - 1 do
                    begin
                      case Json[p] of
                        '"': StringBuilder.Append(S_QUOTE);
                        '\': StringBuilder.Append(S_BSLASH);
                      else
                        StringBuilder.Append(Json[p]);
                      end;
                    end;

                  StringBuilder.Append('"');

                  i := blockEnd + 1;
                  Continue;
                end;
            end;

          {--- Otherwise: we copy the string exactly as it was }
          StringBuilder.Append(Copy(Json, keyStart, afterKey - keyStart));
          i := afterKey;
        end;

      Result := StringBuilder.ToString;
    finally
      StringBuilder.Free;
    end;
  finally
    Dictionary.Free;
  end;
end;

class function TJsonPolyShield.Unshield(const Shielded: string): string;
begin
  Result := Shielded
    .Replace(string(S_QUOTE), '"')
    .Replace(string(S_BSLASH), '\');
end;

{ TJsonPolyUnshield }

class function TJsonPolyUnshield.Restore(const Arg: string): string;
var
  kind: Char;
begin
  var Buffer := Arg.Replace(#10, '').Trim;
  Buffer := TJsonPolyShield.Unshield(Buffer);

  {--- Normalization (optional) }
  while Buffer.Contains(', ') do
    Buffer := Buffer.Replace(', ', ',');

  if not Buffer.IsEmpty and ((Buffer[1] = TJsonPolyShield.S_KIND_O) or (Buffer[1] = TJsonPolyShield.S_KIND_A)) then
    begin
      kind := Buffer[1];
      Delete(Buffer, 1, 1);
    end
  else
    Exit(Buffer);

  if kind = TJsonPolyShield.S_KIND_A then
    Result := Format('[%s]', [Buffer])
  else
    Result := Format('{%s}', [Buffer]);

  Result := Result.Replace(',', ', ');
end;

{ TDeserializationPrepare }

class function TDeserializationPrepare.CreateInstance: ICustomFieldsPrepare;
begin
  Result := TDeserializationPrepare.Create;
end;

function TDeserializationPrepare.Convert(const Value: string): string;
begin
  Result := TJsonPolyShield.Prepare(Value, PROTECTED_FIELD);
end;

end.
