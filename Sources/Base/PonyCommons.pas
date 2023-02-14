{=============================================================================
 Unit :
 Framework : Pony
 Author : Frédéric Libaud
 Description :
 Features:

 =============================================================================
 History
 -----------------------------------------------------------------------------
 08/02/2022 - Forking code from Horse framework
 =============================================================================}
unit PonyCommons;

{$ifdef FPC}
  {$mode objfpc}{$H+}
  {$MODESWITCH TypeHelpers}
{$endif}

interface

uses
{$ifdef FPC}
  Classes,
  SysUtils,
  StrUtils,
  RegExpr
{$else}
  System.Classes,
  System.SysUtils,
  System.RegularExpressions
{$endif}
  ;

type
{$ifdef FPC}
  TMethodType = (mtAny, mtGet, mtPut, mtPost, mtHead, mtDelete, mtPatch);
{$endif}

{$SCOPEDENUMS ON}
  THTTPStatus = (
    Continue = 100,
    SwitchingProtocols = 101,
    Processing = 102,
    OK = 200,
    Created = 201,
    Accepted = 202,
    NonAuthoritativeInformation = 203,
    NoContent = 204,
    ResetContent = 205,
    PartialContent = 206,
    MultiStatus = 207,
    AlreadyReported = 208,
    IMUsed = 226,
    MultipleChoices = 300,
    MovedPermanently = 301,
    Found = 302,
    SeeOther = 303,
    NotModified = 304,
    UseProxy = 305,
    TemporaryRedirect = 307,
    PermanentRedirect = 308,
    BadRequest = 400,
    Unauthorized = 401,
    PaymentRequired = 402,
    Forbidden = 403,
    NotFound = 404,
    MethodNotAllowed = 405,
    NotAcceptable = 406,
    ProxyAuthenticationRequired = 407,
    RequestTimeout = 408,
    Conflict = 409,
    Gone = 410,
    LengthRequired = 411,
    PreconditionFailed = 412,
    PayloadTooLarge = 413,
    RequestURITooLong = 414,
    UnsupportedMediaType = 415,
    RequestedRangeNotSatisfiable = 416,
    ExpectationFailed = 417,
    Imateapot = 418,
    MisdirectedRequest = 421,
    UnprocessableEntity = 422,
    Locked = 423,
    FailedDependency = 424,
    UpgradeRequired = 426,
    PreconditionRequired = 428,
    TooManyRequests = 429,
    RequestHeaderFieldsTooLarge = 431,
    ConnectionClosedWithoutResponse = 444,
    UnavailableForLegalReasons = 451,
    ClientClosedRequest = 499,
    InternalServerError = 500,
    NotImplemented = 501,
    BadGateway = 502,
    ServiceUnavailable = 503,
    GatewayTimeout = 504,
    HTTPVersionNotSupported = 505,
    VariantAlsoNegotiates = 506,
    InsufficientStorage = 507,
    LoopDetected = 508,
    NotExtended = 510,
    NetworkAuthenticationRequired = 511,
    NetworkConnectTimeoutError = 599);

  TMimeTypes = (
    MultiPartFormData,
    ApplicationXWWWFormURLEncoded,
    ApplicationJSON,
    ApplicationOctetStream,
    ApplicationXML,
    ApplicationJavaScript,
    ApplicationPDF,
    ApplicationTypeScript,
    ApplicationZIP,
    TextPlain,
    TextCSS,
    TextCSV,
    TextHTML,
    ImageJPEG,
    ImagePNG,
    ImageGIF,
    Download);

  TMessageType = (Default, Error, Warning, Information);
  TLhsBracketsType = (Equal, NotEqual, LessThan, LessThanOrEqual, GreaterThan, GreaterThanOrEqual, Range, Like);
{$SCOPEDENUMS OFF}

  TLhsBrackets = set of TLhsBracketsType;

  THTTPStatusHelper = {$ifdef FPC} type {$else} record {$endif} helper for THTTPStatus
    function ToInteger: Integer;
  end;

  TMimeTypesHelper = {$ifdef FPC} type {$else} record {$endif} helper
  for TMimeTypes
    function ToString: string;
  end;

  TLhsBracketsTypeHelper = {$ifdef FPC} type {$else} record {$endif} helper for TLhsBracketsType
    function ToString: string;
  end;

{$ifdef FPC}
function StringCommandToMethodType(const ACommand: string): TMethodType;
{$endif}

function MatchRoute(const AText: string; const AValues: array of string): Boolean;

implementation

{$ifdef FPC}
function StringCommandToMethodType(const ACommand: string): TMethodType;
begin
  case AnsiIndexText(ACommand, ['ANY', 'DELETE', 'GET', 'HEAD', 'PATCH', 'POST', 'PUT']) of
    0:
      Result:= TMethodType.mtAny;
    1:
      Result:= TMethodType.mtDelete;
    2:
      Result:= TMethodType.mtGet;
    3:
      Result:= TMethodType.mtHead;
    4:
      Result:= TMethodType.mtPatch;
    5:
      Result:= TMethodType.mtPost;
    6:
      Result:= TMethodType.mtPut;
  end;
end;
{$endif}

function MatchRoute(const AText: string; const AValues: array of string): Boolean;
  function ReplaceParams(const AValue: string): string;
  var
    LPart: string;
    LSplitedPath: {$ifdef FPC}specialize{$endif} TArray<string>;
  begin
    Result:= AValue;
    LSplitedPath:= AValue.Split(['/']);
    for LPart in LSplitedPath do
    begin
      if LPart.StartsWith(':') then
        Result:= StringReplace(Result, LPart, '([^/]*)', []);
    end;
    Result:= Trim(Result);
    if not(Result.EndsWith('/')) then
      Result:= Result + '/';
  end;

var
{$ifdef FPC}
  LRegexObj: TRegExpr;
{$endif}
  I: Integer;
  LText, LExpression: string;
begin
  Result:= False;
{$ifdef FPC}
  LRegexObj:= TRegExpr.Create;
  try
{$endif}
    LText:= Trim(AText);
    if not(LText.EndsWith('/')) then
      LText:= LText + '/';
    for I:= Low(AValues) to High(AValues) do
    begin
      LExpression:= '^(' + ReplaceParams(AValues[I]) + ')$';
{$ifdef FPC}
      LRegexObj.Expression:= LExpression;
      if LRegexObj.Exec(LText) then
{$else}
      if TRegEx.IsMatch(LText, LExpression) then
{$endif}
      begin
        Result:= True;
        Exit;
      end;
    end;
{$ifdef FPC}
  finally
    LRegexObj.Free;
  end;
{$endif}
end;

{ TLhsBracketsTypeHelper }

function TLhsBracketsTypeHelper.ToString: string;
begin
  case Self of
    TLhsBracketsType.Equal:
      Result:= '[eq]';
    TLhsBracketsType.NotEqual:
      Result:= '[ne]';
    TLhsBracketsType.LessThan:
      Result:= '[lt]';
    TLhsBracketsType.LessThanOrEqual:
      Result:= '[lte]';
    TLhsBracketsType.GreaterThan:
      Result:= '[gt]';
    TLhsBracketsType.GreaterThanOrEqual:
      Result:= '[gte]';
    TLhsBracketsType.Range:
      Result:= '[range]';
    TLhsBracketsType.Like:
      Result:= '[like]';
  end;
end;

{ THTTPStatusHelper }

function THTTPStatusHelper.ToInteger: Integer;
begin
  Result:= Ord(Self);
end;

{ TMimeTypesHelper }

function TMimeTypesHelper.ToString: string;
begin
  case Self of
    TMimeTypes.MultiPartFormData:
      Result:= 'multipart/form-data';
    TMimeTypes.ApplicationXWWWFormURLEncoded:
      Result:= 'application/x-www-form-urlencoded';
    TMimeTypes.ApplicationJSON:
      Result:= 'application/json';
    TMimeTypes.ApplicationOctetStream:
      Result:= 'application/octet-stream';
    TMimeTypes.ApplicationXML:
      Result:= 'application/xml';
    TMimeTypes.ApplicationJavaScript:
      Result:= 'application/javascript';
    TMimeTypes.ApplicationPDF:
      Result:= 'application/pdf';
    TMimeTypes.ApplicationTypeScript:
      Result:= 'application/typescript';
    TMimeTypes.ApplicationZIP:
      Result:= 'application/zip';
    TMimeTypes.TextPlain:
      Result:= 'text/plain';
    TMimeTypes.TextCSS:
      Result:= 'text/css';
    TMimeTypes.TextCSV:
      Result:= 'text/csv';
    TMimeTypes.TextHTML:
      Result:= 'text/html';
    TMimeTypes.ImageJPEG:
      Result:= 'image/jpeg';
    TMimeTypes.ImagePNG:
      Result:= 'image/png';
    TMimeTypes.ImageGIF:
      Result:= 'image/gif';
    TMimeTypes.Download:
      Result:= 'application/x-download';
  end;
end;

end.
