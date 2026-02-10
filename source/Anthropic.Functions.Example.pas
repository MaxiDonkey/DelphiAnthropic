unit Anthropic.Functions.Example;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, Anthropic.Functions.Core, MistralAI.Schema, MistralAI.Types;

type
  TWeatherReportFunction = class(TFunctionCore)
  protected
    function GetDescription: string; override;
    function GetName: string; override;
    function GetInputSchema: string; override;
  public
    function Execute(const Arguments: string): string; override;
    class function CreateInstance(const CacheControl: Boolean = False): IFunctionCore;
  end;

implementation

uses
  System.StrUtils, System.JSON;

{ TWeatherReportFunction }

class function TWeatherReportFunction.CreateInstance(
  const CacheControl: Boolean): IFunctionCore;
begin
  Result := TWeatherReportFunction.create;
  Result.CacheControl := CacheControl;
end;

function TWeatherReportFunction.Execute(const Arguments: string): string;

  procedure AddToReport(const Value: TJSONObject;
    Temperature: Integer; Forecast: TArray<string>);
  begin
    Value.AddPair('temperature', TJSONNumber.Create(Temperature));
    Value.AddPair('forecast', TJSONArray.Create(Forecast[0], Forecast[1]));
  end;

begin
  Result := EmptyStr;
  var Location := EmptyStr;

  {--- Parse arguments to retrieve parameters }
  var JSON := TJSONObject.ParseJSONValue(Arguments) as TJSONObject;
  try
    if Assigned(JSON) then
    try
      Location := JSON.GetValue('location', '');
    finally
      JSON.Free;
    end;
  except
    Location := EmptyStr;
  end;

  {--- Stop the treatment if location is empty }
  if Location.IsEmpty then
    Exit;

  {--- Build the response }
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('location', Location);
    case IndexStr(AnsiLowerCase(Location), [
      'san francisco', 'san francisco, ca',
      'paris', 'paris, fr', 'paris, france']) of
      0,1 :
        AddToReport(JSON, 21, [
          'sunny',
          'windy']);

      2,3,4 :
        AddToReport(JSON, 14, [
          'rainy',
          'low visibility but sunny in the late afternoon or early evening']);
    end;
    Result := JSON.ToJSON;
  finally
    JSON.Free;
  end;
end; {Execute}


function TWeatherReportFunction.GetDescription: string;
begin
  Result := 'Get the current weather in a given location';
end;

function TWeatherReportFunction.GetName: string;
begin
  Result := 'get_weather';
end;

function TWeatherReportFunction.GetInputSchema: string;
begin
//  Result :=
//    '{'+
//    '"type": "object",'+
//    '"properties": {'+
//         '"location": {'+
//             '"type": "string",'+
//             '"description": "The city and department, e.g. Marseille, 13"'+
//         '},'+
//         '"unit": {'+
//             '"type": "string",'+
//             '"enum": ["celsius", "fahrenheit"]'+
//         '}'+
//     '},'+
//     '"required": ["location"]'+
//  '}';

  {--- If we use the TSchemaParams class defined in the MistralAI.Schema.pas unit }
  var Schema := TSchemaParams.New(
    procedure (var Params: TSchemaParams)
    begin
      Params.&Type(TSchemaType.Object);
      Params.Properties('properties',
        procedure (var Params: TSchemaParams)
        begin
          Params.Properties('location',
            procedure (var Params: TSchemaParams)
            begin
              Params.&Type(TSchemaType.String);
              Params.Description('The city and state, e.g. San Francisco, CA');
            end);
          Params.Properties('unit',
            procedure (var Params: TSchemaParams)
            begin
              Params.&Type(TSchemaType.String);
              Params.Enum(['celsius', 'fahrenheit']);
            end);
        end);
      Params.Required(['location', 'unit']);
    end);
  Result := Schema.ToJsonString(True);
end;

end.
