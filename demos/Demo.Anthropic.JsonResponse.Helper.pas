unit Demo.Anthropic.JsonResponse.Helper;

interface

uses
  System.SysUtils, System.JSON,
  System.Classes,
  WVPythia.JSON.SafeReader;

type
  TAnthropicJsonResponseHelper = record
  private
    class procedure AppendNormalisedJson(
      const ABuilder: TStringBuilder;
      const AReader: TJsonReader); static;

  public
    class function NormalizeJsonResponse(
      const AJsonResponse: string): string; static;
  end;

implementation

{ TAnthropicJsonResponseHelper }

class procedure TAnthropicJsonResponseHelper.AppendNormalisedJson(
  const ABuilder: TStringBuilder;
  const AReader: TJsonReader);
begin
  var Json := AReader.ToJson.Trim;

  if Json.IsEmpty then
    Exit;

  if ABuilder.Length > 0 then
    ABuilder.Append(sLineBreak);

  ABuilder.Append(Json);
end;

class function TAnthropicJsonResponseHelper.NormalizeJsonResponse(
  const AJsonResponse: string): string;
begin
  Result := '';

  if AJsonResponse.Trim.IsEmpty then
    Exit;

  var Builder := TStringBuilder.Create;
  try
    var PendingJson := '';

    for var RawLine in AJsonResponse.Split([sLineBreak]) do
      begin
        if RawLine.Trim.IsEmpty and PendingJson.Trim.IsEmpty then
          Continue;

        var CandidateJson := '';

        if PendingJson.IsEmpty then
          CandidateJson := RawLine
        else
          CandidateJson := PendingJson + RawLine;

        var Reader := TJsonReader.Parse(CandidateJson);

        if Reader.IsValid then
          begin
            AppendNormalisedJson(
              Builder,
              Reader);

            PendingJson := '';
          end
        else
          PendingJson := CandidateJson;
      end;

    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

end.
