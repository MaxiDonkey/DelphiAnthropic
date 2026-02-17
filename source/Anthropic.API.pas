unit Anthropic.API;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.Net.HttpClient, System.Net.URLClient,
  System.Net.Mime, System.JSON, System.Generics.Collections, System.NetEncoding,
  Anthropic.API.Params, Anthropic.Errors,
  Anthropic.Exceptions, Anthropic.HttpClientInterface, Anthropic.HttpClientAPI,
  Anthropic.API.JSONShield, Anthropic.Api.JsonFingerprintBinder,
  Anthropic.Headers.Beta, Anthropic.Monitoring, Anthropic.API.JsonSafeReader;

type
  TAnthropicSettings = class
  const
    URL_BASE = 'https://api.anthropic.com/v1';

  private
    FToken: string;
    FBaseUrl: string;
    FOrganization: string;
    FCustomHeaders: TNetHeaders;
    procedure SetBaseUrl(const Value: string);
    procedure SetCustomHeaders(const Value: TNetHeaders);
    procedure SetOrganization(const Value: string);
    procedure SetToken(const Value: string);

  public
    constructor Create; overload;

    /// <summary>
    /// The API key used for authentication.
    /// </summary>
    property Token: string read FToken write SetToken;

    /// <summary>
    /// Gets or sets the base URL for all API requests.
    /// </summary>
    /// <remarks>
    /// This value defines the root endpoint used to build request URLs
    /// (for example, <c>https://api.anthropic.com/v1</c>). It is combined with
    /// relative paths to form the final request URL.
    /// </remarks>
    property BaseUrl: string read FBaseUrl write SetBaseUrl;

    /// <summary>
    /// The organization identifier used for the API.
    /// </summary>
    property Organization: string read FOrganization write SetOrganization;

    /// <summary>
    /// Custom headers to include in API requests.
    /// </summary>
    property CustomHeaders: TNetHeaders read FCustomHeaders write SetCustomHeaders;
  end;

  TApiHttpHandler = class(TAnthropicSettings)
  private
    FHttpClient: IHttpClientAPI;

  protected
    /// <summary>
    /// Validates that the API settings required to issue requests are present.
    /// </summary>
    /// <remarks>
    /// This routine checks the configuration held by <see cref="TAnthropicSettings"/> before performing
    /// an HTTP request. It is typically invoked by the underlying HTTP client implementation prior to
    /// sending a request.
    /// <para>
    /// • Validation rule: <see cref="TAnthropicSettings.Token"/> must be non-empty.
    /// </para>
    /// <para>
    /// • Validation rule: <see cref="TAnthropicSettings.BaseUrl"/> must be non-empty.
    /// </para>
    /// </remarks>
    /// <exception cref="EAnthropicExceptionAPI">
    /// Raised when a required setting is missing or empty (for example, an empty token or base URL).
    /// </exception>
    procedure VerifyApiSettings;

    /// <summary>
    /// Creates and returns a new HTTP client instance configured for Anthropic API requests.
    /// </summary>
    /// <returns>
    /// A newly created instance implementing <see cref="IHttpClientAPI"/> that is ready to issue requests
    /// using the current API settings.
    /// </returns>
    /// <remarks>
    /// <para>
    /// • The returned client is created via <c>THttpClientAPI.CreateInstance(VerifyApiSettings)</c>,
    /// so API settings validation (token/base URL) can be enforced by the underlying implementation.
    /// </para>
    /// <para>
    /// • If <see cref="HttpClient"/> is assigned, this method copies runtime configuration to the new instance
    /// (timeouts and proxy settings): <c>SendTimeOut</c>, <c>ConnectionTimeout</c>, <c>ResponseTimeout</c>,
    /// and <c>ProxySettings</c>.
    /// </para>
    /// <para>
    /// • This method always returns a fresh instance; it does not reuse <see cref="HttpClient"/>.
    /// <see cref="HttpClient"/> is treated as a template for configuration values.
    /// </para>
    /// </remarks>
    /// <exception cref="EAnthropicExceptionAPI">
    /// Raised when required API settings are missing or empty (for example, an empty token or base URL),
    /// depending on when the underlying HTTP client invokes the provided validation callback.
    /// </exception>
    function NewHttpClient: IHttpClientAPI; virtual;

  public
    constructor Create;

    /// <summary>
    /// The HTTP client used to send requests to the API.
    /// </summary>
    /// <value>
    /// An instance of a class implementing <c>IHttpClientAPI</c>.
    /// </value>
    property HttpClient: IHttpClientAPI read FHttpClient write FHttpClient;
  end;

  TApiDeserializer = class(TApiHttpHandler)
  strict private
    class var FMetadataManager: ICustomFieldsPrepare;
    class var FMetadataAsObject: Boolean;

  protected
    /// <summary>
    /// Parses the error data from the API response.
    /// </summary>
    /// <param name="Code">
    /// The HTTP status code returned by the API.
    /// </param>
    /// <param name="ResponseText">
    /// The response body containing error details.
    /// </param>
    /// <exception cref="GeminiExceptionAPI">
    /// Raised if the error response cannot be parsed or contains invalid data.
    /// </exception>
    procedure DeserializeErrorData(const Code: Int64; const ResponseText: string); virtual;

    /// <summary>
    /// Raises an exception corresponding to the API error code.
    /// </summary>
    /// <param name="Code">
    /// The HTTP status code returned by the API.
    /// </param>
    /// <param name="Error">
    /// The deserialized error object containing error details.
    /// </param>
    procedure RaiseError(Code: Int64; Error: TErrorCore);

    /// <summary>
    /// Deserializes an HTTP response payload into a strongly typed Delphi object, or raises
    /// a structured exception when the response represents an API error.
    /// </summary>
    /// <typeparam name="T">
    /// The target type to deserialize into. Must be a class type with a parameterless constructor.
    /// </typeparam>
    /// <param name="Code">
    /// The HTTP status code returned by the server.
    /// </param>
    /// <param name="ResponseText">
    /// The response body as a JSON string (success payload or error payload).
    /// </param>
    /// <param name="DisabledShield">
    /// When <c>True</c>, disables metadata preprocessing and performs a direct JSON-to-object
    /// conversion (see <c>Parse{T}</c>). When <c>False</c> (default), parsing follows the global
    /// metadata configuration (<c>MetadataAsObject</c>/<c>MetadataManager</c>).
    /// </param>
    /// <returns>
    /// A deserialized instance of <typeparamref name="T"/> when <paramref name="Code"/> indicates success (2xx).
    /// <para>
    /// • If <typeparamref name="T"/> inherits from <c>TJSONFingerprint</c>, the original JSON payload is
    /// normalized (formatted) and stored in <c>JSONResponse</c>, then propagated to nested fingerprint
    /// instances in the object graph.
    /// </para>
    /// </returns>
    /// <remarks>
    /// <para>
    /// • Success path: for HTTP status codes in the range 200..299, this method maps
    /// <paramref name="ResponseText"/> into <typeparamref name="T"/> by calling <c>Parse{T}</c>.
    /// </para>
    /// <para>
    /// • Error path: for any non-2xx code, this method delegates to <c>DeserializeErrorData</c>,
    /// which attempts to parse the API error payload and raises an appropriate <c>GeminiException</c>
    /// subtype. This method does not return normally in that case.
    /// </para>
    /// <para>
    /// • This method does not validate transport-level concerns (timeouts, connectivity). It only
    /// interprets the HTTP status code and JSON payload already obtained by the caller.
    /// </para>
    /// </remarks>
    /// <exception cref="GeminiException">
    /// Raised when the server returns a structured error payload that can be parsed and mapped to a known error type.
    /// </exception>
    /// <exception cref="EGeminiExceptionAPI">
    /// Raised when the server returns an error payload that is not parseable as a structured error object.
    /// </exception>
    /// <exception cref="EInvalidResponse">
    /// Raised when the JSON success payload cannot be mapped to <typeparamref name="T"/> under the active
    /// parsing mode (for example metadata preprocessing requirements not satisfied).
    /// </exception>
    function Deserialize<T: class, constructor>(const Code: Int64;
      const Payload: string;
      const ResponseText: string;
      DisabledShield: Boolean = False): T;

  public
    class constructor Create;

    /// <summary>
    /// Gets or sets whether the deserializer treats the "metadata" payload as a JSON object.
    /// </summary>
    /// <remarks>
    /// When set to <c>True</c>, deserialization expects metadata fields to be represented as proper JSON objects
    /// and mapped to the corresponding Delphi types (for example, a dedicated metadata class with matching fields).
    /// <para>
    /// • When set to <c>False</c> (default), metadata fields are treated as raw JSON text and preprocessed through
    /// <see cref="MetadataManager"/> before the final object mapping occurs. This mode is intended for scenarios
    /// where the metadata schema is variable across response types and cannot be bound reliably to a single class.
    /// </para>
    /// <para>
    /// • This setting is process-wide (static) and affects all calls that use <see cref="Parse{T}(string)"/> and
    /// <see cref="Deserialize{T}(Int64,string)"/> within this unit.
    /// </para>
    /// </remarks>
    class property MetadataAsObject: Boolean read FMetadataAsObject write FMetadataAsObject;

    /// <summary>
    /// Gets or sets the global metadata preprocessor used during JSON deserialization.
    /// </summary>
    /// <remarks>
    /// This property holds an implementation of <c>ICustomFieldsPrepare</c> responsible for preparing and/or
    /// transforming JSON payloads before they are mapped to Delphi objects.
    /// <para>
    /// • When <see cref="MetadataAsObject"/> is <c>False</c> (default), the deserializer invokes
    /// <c>MetadataManager.Convert(...)</c> to normalize metadata fields that may contain variable or untyped
    /// structures, enabling stable deserialization without requiring a dedicated metadata class.
    /// </para>
    /// <para>
    /// • When <see cref="MetadataAsObject"/> is <c>True</c>, the metadata preprocessor is typically not required
    /// because metadata is expected to be represented as proper JSON objects and mapped directly to Delphi types.
    /// </para>
    /// <para>
    /// • This setting is process-wide (static). Assigning a new manager affects all subsequent calls to
    /// <see cref="Parse{T}(string)"/> and <see cref="Deserialize{T}(Int64,string)"/> within this unit.
    /// </para>
    /// <para>
    /// • If set to <c>nil</c>, and <see cref="MetadataAsObject"/> is <c>False</c>, deserialization may fail for
    /// responses that rely on metadata preprocessing.
    /// </para>
    /// </remarks>
    class property MetadataManager: ICustomFieldsPrepare read FMetadataManager write FMetadataManager;

    /// <summary>
    /// Parses a JSON payload and maps it to a strongly typed Delphi object.
    /// </summary>
    /// <typeparam name="T">
    /// The target type to deserialize into. Must be a class type with a parameterless constructor.
    /// </typeparam>
    /// <param name="Value">
    /// The JSON payload to parse.
    /// </param>
    /// <param name="DisabledShield">
    /// When <c>True</c>, performs a direct JSON-to-object conversion without applying the metadata
    /// preprocessing pipeline. When <c>False</c> (default), parsing follows the global metadata
    /// configuration (<see cref="MetadataAsObject"/> / <see cref="MetadataManager"/>).
    /// </param>
    /// <returns>
    /// An instance of <typeparamref name="T"/> populated from <paramref name="Value"/>.
    /// <para>
    /// • If <typeparamref name="T"/> inherits from <c>TJSONFingerprint</c>, the original JSON payload
    /// is normalized (formatted) and stored in <c>JSONResponse</c>, then propagated to nested
    /// <c>TJSONFingerprint</c> instances in the object graph.
    /// </para>
    /// </returns>
    /// <remarks>
    /// Parsing behavior depends on the global deserialization configuration:
    /// <para>
    /// • If <paramref name="NullConversion"/> is <c>True</c>, this method calls <c>TJson.JsonToObject&lt;T&gt;</c>
    /// directly on <paramref name="Value"/> (no metadata conversion).
    /// </para>
    /// <para>
    /// • Otherwise, when <see cref="MetadataAsObject"/> is <c>True</c>, metadata fields are expected to
    /// be proper JSON objects and parsing is direct.
    /// </para>
    /// <para>
    /// • When <see cref="MetadataAsObject"/> is <c>False</c>, <see cref="MetadataManager"/> is used to
    /// preprocess/normalize metadata fields before mapping to <typeparamref name="T"/>. If
    /// <see cref="MetadataManager"/> is <c>nil</c> in this mode, parsing raises an invalid-response
    /// exception.
    /// </para>
    /// <para>
    /// This method is a pure deserialization utility: it does not interpret HTTP status codes.
    /// Error payload handling is performed by higher-level routines (for example
    /// <c>Deserialize{T}</c>/<c>DeserializeErrorData</c>).
    /// </para>
    /// </remarks>
    /// <exception cref="EInvalidResponse">
    /// Raised when <see cref="MetadataAsObject"/> is <c>False</c> and <see cref="MetadataManager"/> is <c>nil</c>,
    /// or when the JSON payload cannot be mapped to <typeparamref name="T"/> under the active mode.
    /// </exception>
    class function Parse<T: class, constructor>(const Value: string;
      const Payload: string;
      DisabledShield: Boolean = False): T; overload;

    class function Parse<T: class, constructor>(const Value: string;
      DisabledShield: Boolean = False): T; overload;
  end;

  TAnthropicAPI = class(TApiDeserializer)
  protected
    function GetHeaders(
      const Path: string;
      const Json: TJSONObject;
      const BetaValues: TArray<string>;
      const ContentType: string = 'application/json';
      const Accept: string = ''): TNetHeaders;

    function GetRequestURL(const Endpoint: string): string;

    function JsonObjectRemoveBeta(const InObj: TJSONObject; out BetaValues: TArray<string>): TJSONObject;

    function MockJsonFile(const FieldName: string; Response: TStream): string;

  public
    function Delete<TResult: class, constructor>(
      const Path: string): TResult; overload;

    function Get<TResult: class, constructor>(const Path: string): TResult; overload;

    function Get(const Path: string): string; overload;

    function Get<TResult: class, constructor; TParams: TUrlParam>(
      const Path: string;
      const ParamProc: TProc<TParams>): TResult; overload;

    function GetFile(
      const Path: string;
      const Response: TStream): Integer;

    function GetMedia<TResult: class, constructor>(const Endpoint: string;
      const JSONFieldName: string):TResult;

    function Post<TParams: TJSONParam>(
      const Path: string;
      const ParamProc: TProc<TParams>;
      const Response: TStringStream;
      Event: TReceiveDataCallback): Boolean; overload;

    function Post<TResult: class, constructor; TParams: TJSONParam>(
       const Path: string;
       const ParamProc: TProc<TParams>;
       const NullConversion: Boolean = False): TResult; overload;

    function Post<TResult: class, constructor>(
      const Path: string;
      const ParamJSON: TJSONObject): TResult; overload;

    function Post<TResult: class, constructor>(
      const Path: string): TResult; overload;

    function Post<TParams: TJSONParam>(
      const Path: string;
      const ParamProc: TProc<TParams>;
      const Response: TStream;
      Event: TReceiveDataCallback): Boolean; overload;

    function PostForm<TResult: class, constructor; TParams: TMultipartFormData, constructor>(
      const Path: string;
      const ParamProc: TProc<TParams>): TResult; overload;

    constructor Create; overload;
  end;

  TAnthropicAPIRoute = class
  private
    FAPI: TAnthropicAPI;
    procedure SetAPI(const Value: TAnthropicAPI);
  public
    property API: TAnthropicAPI read FAPI write SetAPI;
    constructor CreateRoute(AAPI: TAnthropicAPI); reintroduce;
  end;

implementation

uses
  REST.Json;

{ TAnthropicAPI }

constructor TAnthropicAPI.Create;
begin
  inherited Create;

end;

function TAnthropicAPI.Post<TParams>(
  const Path: string;
  const ParamProc: TProc<TParams>;
  const Response: TStream;
  Event: TReceiveDataCallback): Boolean;
var
  P: TParams;
  Code: Integer;
  BetaValues: TArray<string>;
begin
  Monitoring.Inc;
  P := TParams.Create;
  try
    if Assigned(ParamProc) then
      ParamProc(P);

    var ParamsBetaLess := JsonObjectRemoveBeta(P.JSON, BetaValues);
    try
      var Http := NewHttpClient;
      Code := Http.Post(
        GetRequestURL(Path),
        ParamsBetaLess,
        Response,
        GetHeaders(Path, P.JSON, BetaValues),
        Event
      );

      case Code of
        200..299:
          Exit(True);
      else
        begin
          Result := False;
          var Recieved := TStringStream.Create('', TEncoding.UTF8);
          try
            Response.Position := 0;
            Recieved.LoadFromStream(Response);
            DeserializeErrorData(Code, Recieved.DataString);
          finally
            Recieved.Free;
          end;
        end;
      end;
    finally
      ParamsBetaLess.Free;
    end;
  finally
    P.Free;
    Monitoring.Dec;
  end;
end;

function TAnthropicAPI.Post<TResult, TParams>(const Path: string;
  const ParamProc: TProc<TParams>;
  const NullConversion: Boolean): TResult;
var
  Response: TStringStream;
  Params: TParams;
  Code: Integer;
  BetaValues: TArray<string>;
  JSONPayload: string;
begin
  Monitoring.Inc;
  Response := TStringStream.Create('', TEncoding.UTF8);
  Params := TParams.Create;
  try
    if Assigned(ParamProc) then
      begin
        ParamProc(Params);
        JSONPayload := Params.ToJsonString();
      end;

    var ParamsBetaLess := JsonObjectRemoveBeta(Params.JSON, BetaValues);
    try
      var Http := NewHttpClient;
      Code := Http.Post(
        GetRequestURL(Path),
        ParamsBetaLess,
        Response,
        GetHeaders(Path, Params.JSON, BetaValues));

      Result := Deserialize<TResult>(Code, JSONPayload, Response.DataString, NullConversion);
    finally
      ParamsBetaLess.Free;
    end;

  finally
    Params.Free;
    Response.Free;
    Monitoring.Dec;
  end;
end;

function TAnthropicAPI.Post<TResult>(
  const Path: string;
  const ParamJSON: TJSONObject): TResult;
var
  Response: TStringStream;
  Code: Integer;
begin
  Monitoring.Inc;
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    var Http := NewHttpClient;
    Code := Http.Post(
      GetRequestURL(Path),
      ParamJSON,
      Response,
      GetHeaders(Path, ParamJSON, [])
    );

    Result := Deserialize<TResult>(Code, '', Response.DataString);
  finally
    Response.Free;
    Monitoring.Dec;
  end;
end;

function TAnthropicAPI.Post<TParams>(
  const Path: string;
  const ParamProc: TProc<TParams>;
  const Response: TStringStream;
  Event: TReceiveDataCallback): Boolean;
var
  Params: TParams;
  Code: Integer;
  BetaValues: TArray<string>;
begin
  Monitoring.Inc;
  Params := TParams.Create;

  try
    if Assigned(ParamProc) then
      ParamProc(Params);

    var ParamsBetaLess := JsonObjectRemoveBeta(Params.JSON, BetaValues);
    try
      var Http := NewHttpClient;
      Code := Http.Post(
        GetRequestURL(Path),
        ParamsBetaLess,
        Response,
        GetHeaders(Path, Params.JSON, BetaValues),
        Event
      );

      case Code of
        200..299:
          Result := True;
      else
        begin
          Result := False;
          var Recieved := TStringStream.Create;
          try
            Response.Position := 0;
            Recieved.LoadFromStream(Response);
            DeserializeErrorData(Code, Recieved.DataString);
          finally
            Recieved.Free;
          end;
        end;
      end;
    finally
      ParamsBetaLess.Free;
    end;
  finally
    Params.Free;
    Monitoring.Dec;
  end;
end;

function TAnthropicAPI.Post<TResult>(const Path: string): TResult;
var
  Response: TStringStream;
  Code: Integer;
begin
  Monitoring.Inc;
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    var Http := NewHttpClient;
    Code := Http.Post(
      GetRequestURL(Path),
      Response,
      GetHeaders(Path, nil, [], '')
    );

    Result := Deserialize<TResult>(Code, '', Response.DataString);
  finally
    Response.Free;
    Monitoring.Dec;
  end;
end;

function TAnthropicAPI.Delete<TResult>(const Path: string): TResult;
var
  Response: TStringStream;
  Code: Integer;
begin
  Monitoring.Inc;
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    var Http := NewHttpClient;
    Code := Http.Delete(
      GetRequestURL(Path),
      Response,
      GetHeaders(Path, nil, [], '')
    );

    Result := Deserialize<TResult>(Code, '', Response.DataString);
  finally
    Response.Free;
    Monitoring.Dec;
  end;
end;

function TAnthropicAPI.PostForm<TResult, TParams>(
  const Path: string;
  const ParamProc: TProc<TParams>): TResult;
var
  Response: TStringStream;
  Params: TParams;
  Code: Integer;
begin
  Monitoring.Inc;
  Response := TStringStream.Create('', TEncoding.UTF8);
  Params := TParams.Create;
  try
    if Assigned(ParamProc) then
      ParamProc(Params);

    var Http := NewHttpClient;
    Code := Http.Post(
      GetRequestURL(Path),
      Params,
      Response,
      GetHeaders(Path, nil, [], '')
    );

    Result := Deserialize<TResult>(Code, '', Response.DataString);
  finally
    Params.Free;
    Response.Free;
    Monitoring.Dec;
  end;
end;

function TAnthropicAPI.Get(const Path: string): string;
var
  Response: TStringStream;
  Code: Integer;
begin
  Monitoring.Inc;
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    var Http := NewHttpClient;
    Code := Http.Get(
      GetRequestURL(Path),
      Response,
      GetHeaders(Path, nil, [], '')
    );

    case Code of
      200..299: ; //Success
    end;
    Result := Response.DataString;
  finally
    Response.Free;
    Monitoring.Dec;
  end;
end;

function TAnthropicAPI.Get<TResult, TParams>(
  const Path: string;
  const ParamProc: TProc<TParams>): TResult;
var
  Response: TStringStream;
  Code: Integer;
  Params: TParams;
begin
  Monitoring.Inc;
  Response := TStringStream.Create('', TEncoding.UTF8);
  Params := TParams.Create;
  try
    if Assigned(ParamProc) then
      ParamProc(Params);

    var Http := NewHttpClient;
    Code := Http.Get(
      GetRequestURL(Path) + Params.ToQueryString,
      Response,
      GetHeaders(Path, nil, [], '')
    );

    Result := Deserialize<TResult>(Code, '', Response.DataString);
  finally
    Response.Free;
    Params.Free;
    Monitoring.Dec;
  end;
end;

function TAnthropicAPI.Get<TResult>(const Path: string): TResult;
var
  Response: TStringStream;
  Code: Integer;
begin
  Monitoring.Inc;
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    var Http := NewHttpClient;
    Code := Http.Get(
      GetRequestURL(Path),
      Response,
      GetHeaders(Path, nil, [], '')
    );

    Result := Deserialize<TResult>(Code, '', Response.DataString);
  finally
    Response.Free;
    Monitoring.Dec;
  end;
end;

function TAnthropicAPI.GetFile(
  const Path: string;
  const Response: TStream): Integer;
begin
  Monitoring.Inc;
  try
    var Http := NewHttpClient;
    Result := Http.Get(
      GetRequestURL(Path),
      Response,
      GetHeaders(Path, nil, [], '', 'application/octet-stream')
    );

    case Result of
      200..299:
        ; {success}
    else
      var Recieved := TStringStream.Create;
      try
        Response.Position := 0;
        Recieved.LoadFromStream(Response);
        DeserializeErrorData(Result, Recieved.DataString);
      finally
        Recieved.Free;
      end;
    end;
  finally
    Monitoring.Dec;
  end;
end;

function TAnthropicAPI.GetHeaders(
  const Path: string;
  const Json: TJSONObject;
  const BetaValues: TArray<string>;
  const ContentType: string;
  const Accept: string): TNetHeaders;
var
  JsonPayload: string;

  function JoinComma(const Value: TArray<string>): string;
  begin
    if Length(Value) = 0 then
      Exit(EmptyStr);

    Result := Value[0];
    for var I := 1 to High(Value) do
      Result := Result + ',' + Value[I];
  end;

begin
  Result := [TNetHeader.Create('x-api-key', FToken)] + FCustomHeaders;
  Result := Result + [TNetHeader.Create('anthropic-version', '2023-06-01')];

  if not ContentType.IsEmpty then
    Result := Result + [TNetHeader.Create('content-Type', ContentType)];

  if not Accept.IsEmpty then
    Result := Result + [TNetHeader.Create('accept', Accept)];

  if Json = nil then
    JsonPayload := EmptyStr
  else
    JsonPayload := Json.ToJSON;

  if Length(BetaValues) = 0 then
    Result := Result + TBetaHeaderManager.Build(Path, JsonPayload)
  else
    Result := Result + [TNetHeader.Create('anthropic-beta', JoinComma(BetaValues))];
end;

function TAnthropicAPI.GetMedia<TResult>(const Endpoint,
  JSONFieldName: string): TResult;
begin
  Monitoring.Inc;
  var Stream := TMemoryStream.Create;
  try
    var Code := GetFile(Endpoint, Stream);
    Result := Deserialize<TResult>(Code, '', MockJsonFile(JSONFieldName, Stream));
  finally
    Stream.Free;
    Monitoring.Dec;
  end;
end;

function TAnthropicAPI.GetRequestURL(const Endpoint: string): string;
begin
  Result := FBaseUrl.TrimRight(['/']) + '/' + Endpoint.TrimLeft(['/']);
end;

function TAnthropicAPI.JsonObjectRemoveBeta(const InObj: TJSONObject;
  out BetaValues: TArray<string>): TJSONObject;
begin
  Result := nil;
  BetaValues := nil;

  if InObj = nil then
    Exit;

  var S := InObj.ToJSON;

  var Reader := TJsonReader.Parse(S);
  if not Reader.IsValid then
    Exit(nil);

  var V := Reader.Value('beta');
  if (V <> nil) and (V is TJSONArray) then
    begin
      var Arr := TJSONArray(V);
      SetLength(BetaValues, Arr.Count);
      for var I := 0 to Arr.Count - 1 do
        begin
          if Arr.Items[I] is TJSONString then
            BetaValues[I] := TJSONString(Arr.Items[I]).Value
          else
          if Arr.Items[I] <> nil then
            BetaValues[I] := Arr.Items[I].Value;
        end;
    end
  else
    BetaValues := nil;

  Reader.Remove('beta');

  V := TJSONObject.ParseJSONValue(Reader.Format(0));
  if (V <> nil) and (V is TJSONObject) then
    Result := TJSONObject(V)
  else
    V.Free;
end;

function TAnthropicAPI.MockJsonFile(const FieldName: string;
  Response: TStream): string;
var
  Bytes: TBytes;
  B64: string;
  Obj: TJSONObject;
begin
  Response.Position := 0;
  SetLength(Bytes, Response.Size);
  if Length(Bytes) > 0 then
    Response.ReadBuffer(Bytes[0], Length(Bytes));

  B64 := TNetEncoding.Base64.EncodeBytesToString(Bytes);

  Obj := TJSONObject.Create;
  try
    Obj.AddPair(FieldName, B64);     // JSON escaping handled
    Result := Obj.ToString;
  finally
    Obj.Free;
  end;
end;

{ TAnthropicAPIRoute }

constructor TAnthropicAPIRoute.CreateRoute(AAPI: TAnthropicAPI);
begin
  inherited Create;
  FAPI := AAPI;
end;

procedure TAnthropicAPIRoute.SetAPI(const Value: TAnthropicAPI);
begin
  FAPI := Value;
end;

{ TAnthropicSettings }

constructor TAnthropicSettings.Create;
begin
   inherited Create;
  FToken := EmptyStr;
  FBaseUrl := URL_BASE;
  FCustomHeaders := [];
end;

procedure TAnthropicSettings.SetBaseUrl(const Value: string);
begin
  FBaseUrl := Value;
end;

procedure TAnthropicSettings.SetCustomHeaders(const Value: TNetHeaders);
begin
  FCustomHeaders := Value;
end;

procedure TAnthropicSettings.SetOrganization(const Value: string);
begin
  FOrganization := Value;
end;

procedure TAnthropicSettings.SetToken(const Value: string);
begin
  FToken := Value;
end;

{ TApiHttpHandler }

constructor TApiHttpHandler.Create;
begin
  inherited Create;

  {--- Config TEMPLATE, exposed via IAnthropic.HttpClient }
  FHttpClient := THttpClientAPI.CreateInstance(VerifyApiSettings);
end;

function TApiHttpHandler.NewHttpClient: IHttpClientAPI;
begin
  Result := THttpClientAPI.CreateInstance(VerifyApiSettings);

  if Assigned(FHttpClient) then
    begin
      Result.SendTimeOut        := FHttpClient.SendTimeOut;
      Result.ConnectionTimeout  := FHttpClient.ConnectionTimeout;
      Result.ResponseTimeout    := FHttpClient.ResponseTimeout;
      Result.ProxySettings      := FHttpClient.ProxySettings;
    end;
end;

procedure TApiHttpHandler.VerifyApiSettings;
begin
  if FToken.IsEmpty or FBaseUrl.IsEmpty then
    raise EAnthropicExceptionAPI.Create('Invalid API key or base URL.');
end;

{ TApiDeserializer }

class constructor TApiDeserializer.Create;
begin
  FMetadataManager := TDeserializationPrepare.CreateInstance;
  FMetadataAsObject := False;
end;

function TApiDeserializer.Deserialize<T>(const Code: Int64;
  const Payload: string;
  const ResponseText: string;
  DisabledShield: Boolean): T;
begin
  Result := nil;
  case Code of
    200..299:
      try
        Result := Parse<T>(ResponseText, Payload, DisabledShield);
      except
        raise;
      end;
    else
      DeserializeErrorData(Code, ResponseText);
  end;
end;

procedure TApiDeserializer.DeserializeErrorData(const Code: Int64;
  const ResponseText: string);
var
  Error: TError;
begin
  Error := nil;
  try
    try
      Error := TJson.JsonToObject<TError>(ResponseText);
    except
      Error := nil;
    end;
    if Assigned(Error) then
      begin
        RaiseError(Code, Error);
      end
    else
      raise EAnthropicExceptionAPI.CreateFmt(
        'Server returned error code %d but response was not parseable: %s', [Code, ResponseText]);
  finally
    if Assigned(Error) then
      Error.Free;
  end;
end;

class function TApiDeserializer.Parse<T>(const Value: string;
  DisabledShield: Boolean): T;
begin
  Result := Parse<T>(Value, '', DisabledShield);
end;

class function TApiDeserializer.Parse<T>(const Value: string;
  const Payload: string;
  DisabledShield: Boolean): T;
{$REGION 'Dev note'}
  (*
    • If MetadataManager are to be treated as objects, a dedicated TMetadata class is required, containing
      all properties corresponding to the specified JSON fields.

    • However, if MetadataManager are not treated as objects, they will be temporarily handled as a string
      and subsequently converted back into a valid JSON string during deserialization using the
      Revert method of the interceptor.

    By default, MetadataManager are treated as strings rather than objects to handle cases where multiple
    classes to be deserialized may contain variable data structures. Refer to the global variable
    MetadataAsObject.

    • JSON fingerprint propagation:
      If the target type inherits from TJSONFingerprint, the original JSON payload is normalized
      (formatted) and stored into JSONResponse. The formatted JSON is then propagated to all
      TJSONFingerprint instances found in the resulting object graph via
      TJSONFingerprintBinder.Bind(Result, Formatted). The binder traverses RTTI fields only (no
      properties evaluated) and is cycle-safe.

    • Exception safety:
      Post-processing (formatting/binding) may raise (e.g., truncation handler in DEBUG). On any
      exception after allocation, the partially created instance is freed before re-raising to
      prevent leaks.
  *)
{$ENDREGION}
var
  Obj: TObject;
begin
  Result := Default(T);
  try
    if DisabledShield then
      begin
        Result := TJson.JsonToObject<T>(Value);
      end
    else
      case MetadataAsObject of
        True:
          Result := TJson.JsonToObject<T>(Value);
        else
          begin
            if MetadataManager = nil then
              raise EAnthropicInvalidResponse.Create('MetadataManager is nil while MetadataAsObject=False');
            try
              Result := TJson.JsonToObject<T>(MetadataManager.Convert(Value));
            except

            end;
          end;
      end;

    {--- Add JSON response if class inherits from TJSONFingerprint class. }
    if Assigned(Result) and (Result is TJSONFingerprint) then
      begin
        var JSONValue := TJSONObject.ParseJSONValue(Value);
        try
          var Formatted := JSONValue.Format();

          (Result as TJSONFingerprint).JSONResponse := Formatted;
          TJSONFingerprintBinder.Bind(Result, Formatted);

          (Result as TJSONFingerprint).JSONPayload := Payload;

          (Result as TJSONFingerprint).InternalFinalizeDeserialize;
        finally
          JSONValue.Free;
        end;
      end;
  except
    Obj := TObject(Result);
    if Obj <> nil then
      Obj.Free;
//    raise;
  end;
end;

procedure TApiDeserializer.RaiseError(Code: Int64; Error: TErrorCore);
begin
    case Code of
      400: raise EAnthropicInvalidRequest.Create(Code, Error);
      401: raise EAnthropicAuthentication.Create(Code, Error);
      403: raise EAnthropicPermissionDenied.Create(Code, Error);
      404: raise EAnthropicNotFound.Create(Code, Error);
      413: raise EAnthropicRequestTooLarge.Create(Code, Error);
      429: raise EAnthropicRateLimited.Create(Code, Error);
      500: raise EAnthropicApiInternalError.Create(Code, Error);
      529: raise EAnthropicOverloaded.Create(Code, Error);
    else
      raise EAnthropicException.Create(Code, Error);
    end;
end;

end.

