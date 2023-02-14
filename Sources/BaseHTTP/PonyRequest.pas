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
unit PonyRequest;

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
  PonyRequestInterfaces, PonyParamsInterfaces, PonyCoreParam, PonyCoreParamHeader, PonyCommons, PonySession;

type
  TPonyRequest = class (IPonyRequest)
  private
    // Members
    _RefCount: integer;
    FWebRequest: {$ifdef FPC}TRequest{$else}TWebRequest{$endif};
    FHeaders: TPonyCoreParam;
    FQuery: TPonyCoreParam;
    FParams: TPonyCoreParam;
    FContentFields: TPonyCoreParam;
    FCookie: TPonyCoreParam;
    FBody: TObject;
    FSession: TObject;
    FSessions: TPonySessions;
    // Method's
    procedure InitializeQuery;
    procedure InitializeParams;
    procedure InitializeContentFields;
    procedure InitializeCookie;
    function IsMultipartForm: Boolean;
    function IsFormURLEncoded: Boolean;
    function CanLoadContentFields: Boolean;
    // Access Method's
    function GetSessions: TPonySessions;
  public
    constructor Create(const AWebRequest: {$ifdef FPC}TRequest{$else}TWebRequest{$endif});
    destructor Destroy; override;
    // Interfaces Implementation Method's
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$else}const{$endif} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
    // Overrided Method's from Inherited interfaces
    function Body: string; overload;
    function BodyAsObject: TObject; overload;
    function Body(const ABody: TObject): IPonyRequest; overload;
    function Session(const ASession: TObject): IPonyRequest; overload;
    function Headers: IPonyCoreParam;
    function Query: IPonyCoreParam;
    function Params: IPonyCoreParam;
    function Cookie: IPonyCoreParam;
    function ContentFields: IPonyCoreParam;
    function MethodType: TMethodType;
    function RawWebRequest: {$ifdef FPC}TRequest{$else}TWebRequest{$endif};
    // Generics Overrided Method's from Inherited interfaces
    //{$ifdef FPC}generic{$endif} function Body<T: class>: T; overload;
    //{$ifdef FPC}generic{$endif}function Session<T: class>: T; overload;
    // Properties
    property Sessions: TPonySessions read GetSessions;
  end;

implementation

uses
  PonyTypes;

{ TPonyRequest implementation }

// Constructor's and destructor's

constructor TPonyRequest.Create(const AWebRequest: {$ifdef FPC}TRequest{$else}TWebRequest{$endif});
begin
  FWebRequest:= AWebRequest;
  FSessions:= TPonySessions.Create;
end;

destructor TPonyRequest.Destroy;
begin
  if Assigned(FHeaders) then
    FreeAndNil(FHeaders);
  if Assigned(FQuery) then
    FreeAndNil(FQuery);
  if Assigned(FParams) then
    FreeAndNil(FParams);
  if Assigned(FContentFields) then
    FreeAndNil(FContentFields);
  if Assigned(FCookie) then
    FreeAndNil(FCookie);
  if Assigned(FBody) then
    FBody.Free;
  if Assigned(FSessions) then
    FSessions.Free;
  inherited;
end;

// Interfaces Implementation Method's

function TPonyRequest.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$else}const{$endif} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  if GetInterface(IID,Obj) then
	Result:= S_OK
  else
	Result:= E_NOINTERFACE;
end;

function TPonyRequest._AddRef : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  Result:= InterlockedIncrement(_RefCount);
end;

function TPonyRequest._Release : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  Result:= InterlockedDecrement(_RefCount);
  {if Result = 0 then
	Self.Destroy;}
end;

// private Access Method's

function TPonyRequest.GetSessions: TPonySessions;
begin
  Result:= FSessions;
end;

// Public Method's

// Overrided Method's from Inherited Interfaces

function TPonyRequest.Body: string;
begin
  Result:= FWebRequest.Content;
end;

function TPonyRequest.BodyAsObject: TObject;
begin
  Result:= FBody;
end;

function TPonyRequest.Body(const ABody: TObject): IPonyRequest;
begin
  Result:= Self;
  FBody:= ABody;
end;

function TPonyRequest.CanLoadContentFields: Boolean;
begin
  Result:= IsMultipartForm or IsFormURLEncoded;
end;

function TPonyRequest.ContentFields: IPonyCoreParam;
begin
  if not Assigned(FContentFields) then
    InitializeContentFields;
  Result:= FContentFields;
end;

function TPonyRequest.Cookie: IPonyCoreParam;
begin
  if not Assigned(FCookie) then
    InitializeCookie;
  Result:= FCookie;
end;

function TPonyRequest.Headers: IPonyCoreParam;
var
  LParam: TPonyList;
begin
  if not Assigned(FHeaders) then
  begin
    LParam:= TPonyCoreParamHeader.GetHeaders(FWebRequest);
    FHeaders:= TPonyCoreParam.Create(LParam).Required(False) as TPonyCoreParam;
  end;
  result:= FHeaders;
end;

procedure TPonyRequest.InitializeContentFields;
const
  CONTENT_DISPOSITION = 'Content-Disposition: form-data; name=';
var
  I: Integer;
  LName: String;
  LValue: String;
begin
  FContentFields:= TPonyCoreParam.Create(TPonyList.Create).Required(False) as TPonyCoreParam;
  if (not CanLoadContentFields) then
    Exit;

  for I:= 0 to Pred(FWebRequest.Files.Count) do
    FContentFields.AddStream(FWebRequest.Files[I].FieldName, FWebRequest.Files[I].Stream);

  for I:= 0 to Pred(FWebRequest.ContentFields.Count) do
  begin
    if IsMultipartForm then
    begin
      {$ifdef FPC}
        LName:= FWebRequest.ContentFields.Names[I];
        LValue:= FWebRequest.ContentFields.ValueFromIndex[I];
      {$else}
      {$IF CompilerVersion <= 31.0}
        if FWebRequest.ContentFields[I].StartsWith(CONTENT_DISPOSITION) then
        begin
          LName:= FWebRequest.ContentFields[I]
                      .Replace(CONTENT_DISPOSITION, EmptyStr)
                      .Replace('"', EmptyStr);
          LValue:= FWebRequest.ContentFields[I + 1];
        end;
      {$else}
        LName:= FWebRequest.ContentFields.Names[I];
        LValue:= FWebRequest.ContentFields.ValueFromIndex[I];
      {$endif}
      {$endif}
    end
    else
    begin
      LName:= FWebRequest.ContentFields.Names[I];
      LValue:= FWebRequest.ContentFields.ValueFromIndex[I];
    end;

    if LName <> EmptyStr then
      FContentFields.Dictionary.AddOrSetValue(LName, LValue);

    LName:= EmptyStr;
    LValue:= EmptyStr;
  end;
end;

procedure TPonyRequest.InitializeCookie;
const
  KEY = 0;
  VALUE = 1;
var
  LParam: {$ifdef FPC}specialize{$endif} TArray<string>;
  LItem: string;
begin
  FCookie:= TPonyCoreParam.Create(TPonyList.Create).Required(False) as TPonyCoreParam;
  for LItem in FWebRequest.CookieFields do
  begin
    LParam:= LItem.Split(['=']);
    FCookie.Dictionary.AddOrSetValue(LParam[KEY], LParam[VALUE]);
  end;
end;

procedure TPonyRequest.InitializeParams;
begin
  FParams:= TPonyCoreParam.Create(TPonyList.Create).Required(True) as TPonyCoreParam;
end;

procedure TPonyRequest.InitializeQuery;
var
  LItem, LKey, LValue: string;
  LEqualFirstPos: Integer;
begin
  FQuery:= TPonyCoreParam.Create(TPonyList.Create).Required(False) as TPonyCoreParam;
  for LItem in FWebRequest.QueryFields do
  begin
    LEqualFirstPos:= Pos('=', Litem);
    LKey:= Copy(Litem, 1, LEqualFirstPos - 1);
    LValue:= Copy(Litem, LEqualFirstPos + 1, Length(LItem));
    FQuery.Dictionary.AddOrSetValue(LKey, LValue);
  end;
end;

function TPonyRequest.IsFormURLEncoded: Boolean;
var
  LContentType: String;
  LFormUrlEncoded: String;
begin
  LContentType:= FWebRequest.ContentType;
  LFormUrlEncoded:= TMimeTypes.ApplicationXWWWFormURLEncoded.ToString;
  Result:= StrLIComp(PChar(LContentType), PChar(LFormUrlEncoded), Length(LFormUrlEncoded)) = 0;
end;

function TPonyRequest.IsMultipartForm: Boolean;
var
  LContentType: String;
  LFormData: String;
begin
  LContentType:= FWebRequest.ContentType;
  LFormData:= TMimeTypes.MultiPartFormData.ToString;
  Result:= StrLIComp(PChar(LContentType), PChar(LFormData), Length(PChar(LFormData))) = 0;
end;

function TPonyRequest.MethodType: TMethodType;
begin
  Result:= {$ifdef FPC}StringCommandToMethodType(FWebRequest.Method);{$else}FWebRequest.MethodType;{$endif}
end;

function TPonyRequest.Params: IPonyCoreParam;
begin
  if not Assigned(FParams) then
    InitializeParams;
  Result:= FParams;
end;

function TPonyRequest.Query: IPonyCoreParam;
begin
  if not Assigned(FQuery) then
    InitializeQuery;
  Result:= FQuery;
end;

function TPonyRequest.RawWebRequest: {$ifdef FPC}TRequest{$else}TWebRequest{$endif};
begin
  Result:= FWebRequest;
end;

function TPonyRequest.Session(const ASession: TObject): IPonyRequest;
begin
  Result:= Self;
  FSession:= ASession;
end;

// Public Generics Overrided Method's from Inherited Interfaces

//{$ifdef FPC}generic{$endif} function TPonyRequest.Body<T>: T;
//begin
//  Result:= T(FBody);
//end;
//
//{$ifdef FPC}generic{$endif} function TPonyRequest.Session<T>: T;
//begin
//  Result:= T(FSession);
//end;

{ End of TPonyRequest implementation }

end.
