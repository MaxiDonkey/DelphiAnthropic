unit Anthropic.Functions.Core;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.JSON;

type
  /// <summary>
  /// Interface defining the core structure and functionality of a function in the system.
  /// </summary>
  /// <remarks>
  /// This interface outlines the basic properties and methods that any function implementation must include.
  /// </remarks>
  IFunctionCore = interface
    ['{D318078A-BA04-4131-A0E9-C95F0330C984}']
    /// <summary>
    /// Retrieves the description of the function.
    /// </summary>
    function GetDescription: string;

    /// <summary>
    /// Retrieves the name of the function.
    /// </summary>
    function GetName: string;

    /// <summary>
    /// Retrieves the InputSchema required by the function, represented as a JSON schema.
    /// </summary>
    function GetInputSchema: string;

    /// <summary>
    /// Executes the function with the provided arguments and returns the result as a string.
    /// </summary>
    /// <param name="Arguments">The arguments passed to the function in JSON format.</param>
    /// <returns>The result of the function execution as a string.</returns>
    function Execute(const Arguments: string): string;

     /// <summary>
    /// Converts the TFunctionCore instance to a JSON object that contains its type and representation.
    /// </summary>
    /// <returns>A JSON object representing the function instance.</returns>
    function ToJson: TJSONObject;

    /// <summary>
    /// Creates a string representation of the TFunctionCore instance in JSON format, including its description, name, and InputSchema.
    /// </summary>
    /// <returns>A string representation of the function in JSON format.</returns>
    function ToString: string;

    /// <summary>
    /// A brief description of the function's purpose, used by the model to determine when and how to call the function.
    /// </summary>
    property Description: string read GetDescription;

    /// <summary>
    /// The unique identifier of the function that will be called. It must only contain characters from a-z, A-Z, 0-9, underscores, or dashes, and should not exceed 64 characters in length.
    /// </summary>
    property Name: string read GetName;

    //// <summary>
    /// The InputSchema required by the function, specified as a JSON schema. If no InputSchema are required, use the schema: {"type": "object", "properties": {}}.
    /// </summary>
    property InputSchema: string read GetInputSchema;

  end;

  /// <summary>
  /// Abstract base class for implementing core function behavior.
  /// </summary>
  /// <remarks>
  /// This class provides basic implementations for some methods and defines the structure that derived classes must follow.
  /// </remarks>
  TFunctionCore = class abstract(TinterfacedObject, IFunctionCore)
  protected
    FCacheControl: Boolean;
    /// <summary>
    /// Retrieves the description of the function. Derived classes must implement this method.
    /// </summary>
    function GetDescription: string; virtual; abstract;

     /// <summary>
    /// Retrieves the name of the function. Derived classes must implement this method.
    /// </summary>
    function GetName: string; virtual; abstract;

    /// <summary>
    /// Retrieves the InputSchema required by the function, represented as a JSON schema. Derived classes must implement this method.
    /// </summary>
    function GetInputSchema: string; virtual; abstract;

  public
    /// <summary>
    /// Executes the function with the provided arguments and returns the result as a string. Derived classes must implement this method.
    /// </summary>
    /// <param name="Arguments">The arguments passed to the function in JSON format.</param>
    /// <returns>The result of the function execution as a string.</returns>
    function Execute(const Arguments: string): string; virtual; abstract;

    /// <summary>
    /// Converts the TFunctionCore instance to a JSON object that contains its type and representation.
    /// </summary>
    /// <returns>A JSON object representing the function instance.</returns>
    function ToJson: TJSONObject;

    /// <summary>
    /// Creates a string representation of the TFunctionCore instance in JSON format, including its description, name, and InputSchema.
    /// </summary>
    /// <returns>A string representation of the function in JSON format.</returns>
    function ToString: string; override;

    /// <summary>
    /// A brief description of the function's purpose, used by the model to determine when and how to call the function.
    /// </summary>
    property Description: string read GetDescription;

    /// <summary>
    /// The unique identifier of the function that will be called. It must only contain characters from a-z, A-Z, 0-9, underscores, or dashes, and should not exceed 64 characters in length.
    /// </summary>
    property Name: string read GetName;

    /// <summary>
    /// The InputSchema required by the function, specified as a JSON schema. If no InputSchema are required, use the schema: {"type": "object", "properties": {}}.
    /// </summary>
    property InputSchema: string read GetInputSchema;

  end;

implementation

{ TFunctionCore }

function TFunctionCore.ToJson: TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Result.AddPair('name', Name);
    Result.AddPair('description', Description);
    Result.AddPair('input_schema', TJSONObject.ParseJSONValue(InputSchema));
    if FCacheControl then
      begin
        {--- Add cache_control object }
        var JSONCacheValue := TJSONObject.Create(TJSONPair.Create('type', 'ephemeral'));
        Result.AddPair('cache_control', JSONCacheValue);
      end;
  except
    on E: Exception do
      begin
        Result.Free;
        raise;
      end;
  end;
end;

function TFunctionCore.ToString: string;
begin
  with TStringWriter.Create do
    try
      Write('"name": "%s"', [Name]);
      Write(',"description": "%s"', [Description]);
      Write(',"input_schema": %s', [InputSchema]);
      if FCacheControl then
        Write(',"cache_control": {"type": "ephemeral"}');
      Result := Format('{%s}', [ToString]);
    finally
      Free;
    end;

  {--- Format output }
  var JSON := TJSONObject.ParseJSONValue(Result);
  try
    Result := JSON.Format();
  finally
    JSON.Free;
  end;
end;

end.
