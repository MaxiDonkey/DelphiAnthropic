unit Anthropic.Functions.Example;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, Anthropic.Functions.Core, Anthropic.Schema, Anthropic.Types,
  Anthropic.API.JsonSafeReader;

type
  TWeatherReportFunction = class(TFunctionCore)
  protected
    function GetDescription: string; override;
    function GetName: string; override;
    function GetInputSchema: string; override;
  public
    function Execute(const Arguments: string): string; override;
    class function CreateInstance: IFunctionCore;
  end;

implementation

uses
  System.StrUtils, System.JSON;

{ TWeatherReportFunction }

class function TWeatherReportFunction.CreateInstance;
begin
  Result := TWeatherReportFunction.create;
end;

function TWeatherReportFunction.Execute(const Arguments: string): string;

  procedure AddToReport(const Value: TJSONObject;
    Temperature: Integer; Forecast: TArray<string>);
  begin
    Value.AddPair('temperature', TJSONNumber.Create(Temperature));
    Value.AddPair('forecast', TJSONArray.Create(Forecast[0], Forecast[1]));
  end;

begin
  var Location := TJsonReader.Parse(Arguments).AsString('location');

  {--- Stop the treatment if location is empty }
  if Location.IsEmpty then
    Exit('');

  {--- Build the response }
  var JSON := TJSONObject.Create;
  try
    JSON.AddPair('location', Location);

    if Location.ToLower.Contains('san francisco') then
      begin
        AddToReport(JSON, 21, [
          'sunny',
          'windy']);
      end
    else
    if Location.ToLower.Contains('paris') then
      begin
        AddToReport(JSON, 6, [
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
//    '''
//    {
//      "type": "object",
//      "properties": {
//           "location": {
//               "type": "string",
//               "description": "The city and department, e.g. Marseille, 13"
//           },
//           "unit": {
//               "type": "string",
//               "enum": ["celsius", "fahrenheit"]
//           }
//       },
//       "required": ["location"]
//    }
//    ''';

  {--- If we use the TSchemaParams class defined in the Anthropic.Schema.pas unit }
  var Schema := TSchemaParams.Create
        .&Type('object')
        .Properties( TJSONObject.Create
           .AddPair('location', TJSONObject.Create
               .AddPair('type', 'string')
               .AddPair('description', 'The city and state, e.g. San Francisco, CA')
           )
           .AddPair('unit', TJSONObject.Create
               .AddPair('type', 'string')
               .AddPair('enum', TJSONArray.Create
                   .Add('celsius')
                   .Add('fahrenheit'))
           )
        )
        .Required(['location']);

  Result := Schema.ToJsonString(True);
end;

end.
