unit Anthropic.JSONL;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.JSON;

type
  TJSONLHelper = record
  private
  public
    class function LoadStrArrayFromFile(const FilePath: string): TArray<string>; static;
    class function LoadFromFile(const FilePath: string;const AsJSONArray: Boolean = True): string; static;
  end;

implementation

{ TJSONLHelper }

class function TJSONLHelper.LoadStrArrayFromFile(
  const FilePath: string): TArray<string>;
begin
  if not FileExists(FilePath) or not ExtractFileExt(FilePath).ToLower.StartsWith('.jsonl') then
    raise Exception.CreateFmt('File %s not found or is not jsonl type.', [FilePath]);

  var StreamReader := TStreamReader.Create(FilePath, TEncoding.UTF8);
  try
    try
      while not StreamReader.EndOfStream do
        begin
          var Line := StreamReader.ReadLine;
          if Line.Trim.IsEmpty then
            Continue;

          var JSONVlue := TJSONObject.ParseJSONValue(Line);
          if not Assigned(JSONVlue) then
            raise Exception.CreateFmt('Error: Malformed JSON data.'#10'%s', [Line]);

          JSONVlue.Free;
          Result := Result + [Line];
        end;
    except
      raise;
    end;
  finally
    StreamReader.Free;
  end;
end;

class function TJSONLHelper.LoadFromFile(const FilePath: string;
  const AsJSONArray: Boolean): string;
begin
  var StrArray := LoadStrArrayFromFile(FilePath);

  if AsJSONArray then
    Exit(Format('['#10'%s'#10']', [string.Join(','#10, StrArray)]));


  Result := string.Join(#10, StrArray);
end;

end.
