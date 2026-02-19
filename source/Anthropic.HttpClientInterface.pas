unit Anthropic.HttpClientInterface;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes,
  System.Net.HttpClient, System.Net.URLClient,
  System.Net.Mime, System.JSON;

type
  /// <summary>
  /// Interface for configuring HTTP client parameters (timeouts, proxy).
  /// </summary>
  IHttpClientParam = interface
    ['{0270CF81-6035-4F23-AB96-E71ACF5BFA17}']
    procedure SetSendTimeOut(const Value: Integer);
    function GetSendTimeOut: Integer;

    function GetConnectionTimeout: Integer;
    procedure SetConnectionTimeout(const Value: Integer);

    function GetResponseTimeout: Integer;
    procedure SetResponseTimeout(const Value: Integer);

    function GetProxySettings: TProxySettings;
    procedure SetProxySettings(const Value: TProxySettings);

    property SendTimeOut: Integer read GetSendTimeOut write SetSendTimeOut;
    property ConnectionTimeout: Integer read GetConnectionTimeout write SetConnectionTimeout;
    property ResponseTimeout: Integer read GetResponseTimeout write SetResponseTimeout;
    property ProxySettings: TProxySettings read GetProxySettings write SetProxySettings;
  end;

  /// <summary>
  /// Abstraction of the HTTP client used by Anthropic.API
  /// </summary>
  IHttpClientAPI = interface(IHttpClientParam)
    ['{7AA66FD7-FB96-4E63-946F-383666C00F38}']

    {--- GET }
    function Get(const URL: string; Response: TStringStream; const Headers: TNetHeaders): Integer; overload;
    function Get(const URL: string; const Response: TStream; const Headers: TNetHeaders): Integer; overload;

    {--- DELETE }
    function Delete(const URL: string; Response: TStringStream; const Headers: TNetHeaders): Integer;

    {--- POST (no body) }
    function Post(const URL: string; Response: TStringStream; const Headers: TNetHeaders): Integer; overload;

    {--- POST (multipart) }
    function Post(const URL: string; Body: TMultipartFormData; Response: TStringStream; const Headers: TNetHeaders): Integer; overload;

    {--- POST (JSON + stream callback, text response) }
    function Post(const URL: string; Body: TJSONObject; Response: TStringStream;
      const Headers: TNetHeaders; OnReceiveData: TReceiveDataCallback = nil): Integer; overload;

    {--- POST (JSON + stream callback, binary/stream response) }
    function Post(const URL: string; Body: TJSONObject; Response: TStream;
      const Headers: TNetHeaders; OnReceiveData: TReceiveDataCallback = nil): Integer; overload;

    {--- POST (raw stream body; used by UploadRaw) }
    function Post(const URL: string; Body: TStream; Response: TStringStream; const Headers: TNetHeaders): Integer; overload;

    {--- POST (capture response headers; body optional; response body ignored) }
    function PostHeaders(const URL: string; Body: TJSONObject; const Headers: TNetHeaders;
      out ResponseHeaders: TNetHeaders): Integer;

    {--- PATCH (JSON + optional stream callback) }
    function Patch(const URL: string; Body: TJSONObject; Response: TStringStream;
      const Headers: TNetHeaders; OnReceiveData: TReceiveDataCallback = nil): Integer;
  end;

implementation

end.

