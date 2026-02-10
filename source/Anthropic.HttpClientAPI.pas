unit Anthropic.HttpClientAPI;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.NetConsts,
  System.Net.HttpClient, System.Net.URLClient, System.Net.Mime, System.JSON,
  Anthropic.HttpClientInterface;

type
  /// <summary>
  /// Default implementation of IHttpClientAPI using Delphi THTTPClient.
  /// </summary>
  THttpClientAPI = class(TInterfacedObject, IHttpClientAPI)
  private
    FHttpClient: THTTPClient;
    FCheckSettingsProc: TProc;

    procedure CheckAPISettings; inline;

    procedure SetSendTimeOut(const Value: Integer);
    function GetSendTimeOut: Integer;

    function GetConnectionTimeout: Integer;
    procedure SetConnectionTimeout(const Value: Integer);

    function GetResponseTimeout: Integer;
    procedure SetResponseTimeout(const Value: Integer);

    function GetProxySettings: TProxySettings;
    procedure SetProxySettings(const Value: TProxySettings);
  public
    constructor Create(const CheckProc: TProc);
    class function CreateInstance(const CheckProc: TProc): IHttpClientAPI;
    destructor Destroy; override;

    { IHttpClientAPI }
    function Get(const URL: string; Response: TStringStream; const Headers: TNetHeaders): Integer; overload;
    function Get(const URL: string; const Response: TStream; const Headers: TNetHeaders): Integer; overload;

    function Delete(const URL: string; Response: TStringStream; const Headers: TNetHeaders): Integer;

    function Post(const URL: string; Response: TStringStream; const Headers: TNetHeaders): Integer; overload;
    function Post(const URL: string; Body: TMultipartFormData; Response: TStringStream; const Headers: TNetHeaders): Integer; overload;

    function Post(const URL: string; Body: TJSONObject; Response: TStringStream;
      const Headers: TNetHeaders; OnReceiveData: TReceiveDataCallback = nil): Integer; overload;

    function Post(const URL: string; Body: TJSONObject; Response: TStream;
      const Headers: TNetHeaders; OnReceiveData: TReceiveDataCallback = nil): Integer; overload;

    function Post(const URL: string; Body: TStream; Response: TStringStream; const Headers: TNetHeaders): Integer; overload;

    function PostHeaders(const URL: string; Body: TJSONObject; const Headers: TNetHeaders;
      out ResponseHeaders: TNetHeaders): Integer;

    function Patch(const URL: string; Body: TJSONObject; Response: TStringStream;
      const Headers: TNetHeaders; OnReceiveData: TReceiveDataCallback = nil): Integer;

    { IHttpClientParam }
    property SendTimeOut: Integer read GetSendTimeOut write SetSendTimeOut;
    property ConnectionTimeout: Integer read GetConnectionTimeout write SetConnectionTimeout;
    property ResponseTimeout: Integer read GetResponseTimeout write SetResponseTimeout;
    property ProxySettings: TProxySettings read GetProxySettings write SetProxySettings;
  end;

implementation

{ THttpClientAPI }

procedure THttpClientAPI.CheckAPISettings;
begin
  if Assigned(FCheckSettingsProc) then
    FCheckSettingsProc;
end;

constructor THttpClientAPI.Create(const CheckProc: TProc);
begin
  inherited Create;
  FHttpClient := THTTPClient.Create;
  FHttpClient.AcceptCharSet := 'utf-8';
  FCheckSettingsProc := CheckProc;
end;

class function THttpClientAPI.CreateInstance(const CheckProc: TProc): IHttpClientAPI;
begin
  Result := THttpClientAPI.Create(CheckProc);
end;

destructor THttpClientAPI.Destroy;
begin
  FHttpClient.Free;
  inherited;
end;

function THttpClientAPI.Get(const URL: string; Response: TStringStream; const Headers: TNetHeaders): Integer;
begin
  CheckAPISettings;
  Result := FHttpClient.Get(URL, Response, Headers).StatusCode;
end;

function THttpClientAPI.Get(const URL: string; const Response: TStream; const Headers: TNetHeaders): Integer;
begin
  CheckAPISettings;
  Result := FHttpClient.Get(URL, Response, Headers).StatusCode;
end;

function THttpClientAPI.Delete(const URL: string; Response: TStringStream; const Headers: TNetHeaders): Integer;
begin
  CheckAPISettings;
  Result := FHttpClient.Delete(URL, Response, Headers).StatusCode;
end;

function THttpClientAPI.Post(const URL: string; Response: TStringStream; const Headers: TNetHeaders): Integer;
begin
  CheckAPISettings;
  var EmptyBody: TStringStream := nil;
  Result := FHttpClient.Post(URL, EmptyBody, Response, Headers).StatusCode;
end;

function THttpClientAPI.Post(const URL: string; Body: TMultipartFormData; Response: TStringStream; const Headers: TNetHeaders): Integer;
begin
  CheckAPISettings;
  Result := FHttpClient.Post(URL, Body, Response, Headers).StatusCode;
end;

function THttpClientAPI.Post(const URL: string; Body: TJSONObject; Response: TStringStream;
  const Headers: TNetHeaders; OnReceiveData: TReceiveDataCallback): Integer;
var
  Req: TStringStream;
begin
  CheckAPISettings;

  Req := TStringStream.Create('', TEncoding.UTF8);
  FHttpClient.ReceiveDataCallBack := OnReceiveData;
  try
    if Assigned(Body) then
      Req.WriteString(Body.ToJSON);
    Req.Position := 0;
    Result := FHttpClient.Post(URL, Req, Response, Headers).StatusCode;
  finally
    FHttpClient.ReceiveDataCallBack := nil;
    Req.Free;
  end;
end;

function THttpClientAPI.Post(const URL: string; Body: TJSONObject; Response: TStream;
  const Headers: TNetHeaders; OnReceiveData: TReceiveDataCallback): Integer;
var
  Bytes: TBytes;
  Req: TMemoryStream;
begin
  CheckAPISettings;

  {--- Keep request encoding explicit UTF-8 }
  if Assigned(Body) then
    Bytes := TEncoding.UTF8.GetBytes(Body.ToJSON)
  else
    SetLength(Bytes, 0);

  Req := TMemoryStream.Create;
  try
    if Length(Bytes) > 0 then
      Req.WriteBuffer(Bytes, Length(Bytes));
    Req.Position := 0;

    FHttpClient.ReceiveDataCallBack := OnReceiveData;
    try
      Result := FHttpClient.Post(URL, Req, Response, Headers).StatusCode;
    finally
      FHttpClient.ReceiveDataCallBack := nil;
    end;
  finally
    Req.Free;
  end;
end;

function THttpClientAPI.Post(const URL: string; Body: TStream; Response: TStringStream; const Headers: TNetHeaders): Integer;
begin
  CheckAPISettings;
  Result := FHttpClient.Post(URL, Body, Response, Headers).StatusCode;
end;

function THttpClientAPI.PostHeaders(const URL: string; Body: TJSONObject; const Headers: TNetHeaders;
  out ResponseHeaders: TNetHeaders): Integer;
var
  Req: TStringStream;
  Resp: IHTTPResponse;
begin
  CheckAPISettings;

  Req := nil;
  try
    if Assigned(Body) then
      begin
        Req := TStringStream.Create('', TEncoding.UTF8);
        Req.WriteString(Body.ToJSON);
        Req.Position := 0;
      end;

    Resp := FHttpClient.Post(URL, Req, nil, Headers);
    Result := Resp.StatusCode;

    if (Result >= 200) and (Result < 300) then
      ResponseHeaders := Resp.Headers
    else
      ResponseHeaders := [];
  finally
    Req.Free;
  end;
end;

function THttpClientAPI.Patch(const URL: string; Body: TJSONObject; Response: TStringStream;
  const Headers: TNetHeaders; OnReceiveData: TReceiveDataCallback): Integer;
var
  Req: TStringStream;
begin
  CheckAPISettings;

  Req := TStringStream.Create('', TEncoding.UTF8);
  FHttpClient.ReceiveDataCallBack := OnReceiveData;
  try
    if Assigned(Body) then
      Req.WriteString(Body.ToJSON);
    Req.Position := 0;
    Result := FHttpClient.Patch(URL, Req, Response, Headers).StatusCode;
  finally
    FHttpClient.ReceiveDataCallBack := nil;
    Req.Free;
  end;
end;

function THttpClientAPI.GetSendTimeOut: Integer;
begin
  Result := FHttpClient.SendTimeout;
end;

procedure THttpClientAPI.SetSendTimeOut(const Value: Integer);
begin
  FHttpClient.SendTimeout := Value;
end;

function THttpClientAPI.GetConnectionTimeout: Integer;
begin
  Result := FHttpClient.ConnectionTimeout;
end;

procedure THttpClientAPI.SetConnectionTimeout(const Value: Integer);
begin
  FHttpClient.ConnectionTimeout := Value;
end;

function THttpClientAPI.GetResponseTimeout: Integer;
begin
  Result := FHttpClient.ResponseTimeout;
end;

procedure THttpClientAPI.SetResponseTimeout(const Value: Integer);
begin
  FHttpClient.ResponseTimeout := Value;
end;

function THttpClientAPI.GetProxySettings: TProxySettings;
begin
  Result := FHttpClient.ProxySettings;
end;

procedure THttpClientAPI.SetProxySettings(const Value: TProxySettings);
begin
  FHttpClient.ProxySettings := Value;
end;

end.

