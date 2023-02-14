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
unit PonyResponse;

//{$ifdef FPC}
//  {$MODE DELPHI}{$H+}
//{$endif}
{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
{$ifdef FPC}
  SysUtils, Classes, fpHTTP, HTTPDefs,
{$else}
  System.SysUtils, System.Classes, Web.HTTPApp,
  {$IF CompilerVersion > 32.0}
    Web.ReqMulti,
  {$endif}
{$endif}
  PonyResponseInterfaces, PonyCommons, PonyCoreFiles, PonyMime;

type
  TPonyResponse = class (IPonyResponse)
  private
    _Refcount: integer;
    oWebResponse: {$ifdef FPC}TResponse{$else}TWebResponse{$endif};
    oContent: TObject;
  public
    // Constructor's and destructor's
    constructor Create(const aWebResponse: {$ifdef FPC}TResponse{$else}TWebResponse{$endif});
    destructor Destroy; override;
    // Interfaces Implementation Method's
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$else}const{$endif} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
    function _AddRef: longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
    function _Release: longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
    // Method's
    function Send(const aContent: string): IPonyResponse; overload;
    function Send(const aStream: TStream): IPonyResponse; overload;
    { Todo : Generic implementation }
    //{$ifdef FPC}generic{$endif} function Send<T{: class}>(aContent: T): IPonyResponse; overload;
    function RedirectTo(const ALocation: string): IPonyResponse; overload;
    function RedirectTo(const ALocation: string; const AStatus: THTTPStatus): IPonyResponse; overload;
    function Status(const aStatus: Integer): IPonyResponse; overload;
    function Status(const aStatus: THTTPStatus): IPonyResponse; overload;
    function SendFile(const aFileStream: TStream; const aFileName: string = ''; const AContentType: string = ''): IPonyResponse; overload;
    function SendFile(const aFileName: string; const aContentType: string = ''): IPonyResponse; overload;
    function Download(const aFileStream: TStream; const aFileName: string; const AContentType: string = ''): IPonyResponse; overload;
    function Download(const aFileName: string; const aContentType: string = ''): IPonyResponse; overload;
    function Render(const aFileStream: TStream; const aFileName: string): IPonyResponse; overload;
    function Render(const aFileName: string): IPonyResponse; overload;
    function Status: Integer; overload;
    function AddHeader(const aName, AValue: string): IPonyResponse;
    function Content: TObject; overload;
    function Content(const aContent: TObject): IPonyResponse; overload;
    function ContentType(const aContentType: string): IPonyResponse;
    function RawWebResponse: {$ifdef FPC}TResponse{$else}TWebResponse{$endif};
  end;

implementation

constructor TPonyResponse.Create(const AWebResponse: {$ifdef FPC}TResponse{$else}TWebResponse{$endif});
begin
  oWebResponse:= AWebResponse;
  {$ifdef FPC}oWebResponse.Code{$else}FWebResponse.StatusCode{$endif}:= THTTPStatus.Ok.ToInteger;
  {$ifdef FPC}
  oWebResponse.FreeContentStream:= True;
  {$endif}
end;

destructor TPonyResponse.Destroy;
begin
  if Assigned(oContent) then
    oContent.Free;
  inherited;
end;

// Interfaces Implementation Method's

function TPonyResponse.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$else}const{$endif} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  if GetInterface(IID,Obj) then
	Result:= S_OK
  else
	Result:= E_NOINTERFACE;
end;

function TPonyResponse._AddRef : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  Result:= InterlockedIncrement(_RefCount);
end;

function TPonyResponse._Release : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  Result:= InterlockedDecrement(_RefCount);
  {if Result = 0 then
	Self.Destroy;}
end;

// Public Method's

function TPonyResponse.AddHeader(const aName, AValue: string): IPonyResponse;
begin
  oWebResponse.SetCustomHeader(aName, AValue);
  Result:= Self;
end;

function TPonyResponse.Content(const aContent: TObject): IPonyResponse;
begin
  Result:= Self;
  oContent:= aContent;
end;

function TPonyResponse.Content: TObject;
begin
  Result:= oContent;
end;

function TPonyResponse.ContentType(const aContentType: string): IPonyResponse;
begin
  oWebResponse.ContentType:= aContentType;
  Result:= Self;
end;

function TPonyResponse.RawWebResponse: {$ifdef FPC}TResponse{$else}TWebResponse{$endif};
begin
  Result:= oWebResponse;
end;

function TPonyResponse.Send(const AContent: string): IPonyResponse;
begin
  oWebResponse.Content:= AContent;
  Result:= Self;
end;

function TPonyResponse.Send(const aStream: TStream): IPonyResponse;
begin
  oWebResponse.ContentStream:= aStream;
  Result:= Self;
end;

{ Todo : implementation }
{{$ifdef FPC} generic{$endif}function TPonyResponse.Send<T>(aContent: T): IPonyResponse;
begin
  oContent:= AContent;
  Result:= Self;
end;}

function TPonyResponse.RedirectTo(const ALocation: string): IPonyResponse;
begin
  oWebResponse.SetCustomHeader('Location', ALocation);
  Result:= Status(THTTPStatus.SeeOther);
end;

function TPonyResponse.RedirectTo(const ALocation: string; const AStatus: THTTPStatus): IPonyResponse;
begin
  oWebResponse.SetCustomHeader('Location', ALocation);
  Result:= Status(AStatus);
end;

function TPonyResponse.Status(const aStatus: THTTPStatus): IPonyResponse;
begin
  {$ifdef FPC}oWebResponse.Code{$else}FWebResponse.StatusCode{$endif}:= aStatus.ToInteger;
  Result:= Self;
end;

function TPonyResponse.SendFile(const aFileStream: TStream; const AFileName: string; const AContentType: string): IPonyResponse;
var
  LFileName: string;
begin
  Result:= Self;
  aFileStream.Position:= 0;
  LFileName:= ExtractFileName(AFileName);

  oWebResponse.FreeContentStream:= False;
  oWebResponse.ContentLength:= aFileStream.Size;
  oWebResponse.ContentStream:= aFileStream;
  oWebResponse.SetCustomHeader('Content-Disposition', Format('inline; filename="%s"', [LFileName]));

  oWebResponse.ContentType:= AContentType;
  if (AContentType = EmptyStr) then
    oWebResponse.ContentType:= PonyMime.TPonyMimeTypes.GetFileType(LFileName);

  {$ifdef FPC}
  oWebResponse.SendContent;
  {$else}
  FWebResponse.SendResponse;
  {$endif}
end;

function TPonyResponse.SendFile(const aFileName: string; const AContentType: string): IPonyResponse;
var
  LFile: TPonyCoreFile;
  LContentType: string;
begin
  Result:= Self;

  LFile:= TPonyCoreFile.Create(aFileName);
  LFile.FreeContentStream:= True;
  try
    LContentType:= AContentType;
    if (AContentType = EmptyStr) then
      LContentType:= LFile.ContentType;
    SendFile(LFile.ContentStream, LFile.Name, LContentType);
  finally
    LFile.Free;
  end;
end;

function TPonyResponse.Download(const aFileStream: TStream; const AFileName: string; const AContentType: string): IPonyResponse;
var
  LFileName: string;
begin
  Result:= Self;
  aFileStream.Position:= 0;
  LFileName:= ExtractFileName(AFileName);

  oWebResponse.FreeContentStream:= False;
  oWebResponse.ContentLength:= aFileStream.Size;
  oWebResponse.ContentStream:= aFileStream;
  oWebResponse.SetCustomHeader('Content-Disposition', Format('attachment; filename="%s"', [LFileName]));

  oWebResponse.ContentType:= AContentType;
  if (AContentType = EmptyStr) then
    oWebResponse.ContentType:= PonyMime.TPonyMimeTypes.GetFileType(LFileName);

  {$ifdef FPC}
  oWebResponse.SendContent;
  {$else}
  FWebResponse.SendResponse;
  {$endif}
end;

function TPonyResponse.Download(const aFileName: string; const AContentType: string): IPonyResponse;
var
  LFile: TPonyCoreFile;
  LContentType: string;
begin
  Result:= Self;

  LFile:= TPonyCoreFile.Create(aFileName);
  LFile.FreeContentStream:= True;
  try
    LContentType:= AContentType;
    if (AContentType = EmptyStr) then
      LContentType:= LFile.ContentType;
    Download(LFile.ContentStream, LFile.Name, LContentType);
  finally
    LFile.Free;
  end;
end;

function TPonyResponse.Render(const aFileStream: TStream;
                              const aFileName: string): IPonyResponse;
begin
  Result:= Self;
  SendFile(aFileStream, aFileName, PonyCommons.TMimeTypes.TextHTML.ToString);
end;

function TPonyResponse.Render(const aFileName: string): IPonyResponse;
begin
  Result:= Self;
  SendFile(aFileName, PonyCommons.TMimeTypes.TextHTML.ToString);
end;

function TPonyResponse.Status: Integer;
begin
  Result:= {$ifdef FPC}oWebResponse.Code{$else}FWebResponse.StatusCode{$endif};
end;

function TPonyResponse.Status(const aStatus: Integer): IPonyResponse;
begin
  {$ifdef FPC}oWebResponse.Code{$else}FWebResponse.StatusCode{$endif}:= aStatus;
  Result:= Self;
end;

end.
