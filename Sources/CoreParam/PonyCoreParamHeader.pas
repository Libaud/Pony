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
unit PonyCoreParamHeader;

//{$ifdef FPC}
//  {$MODE DELPHI}{$H+}
//{$endif}
{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
{$ifdef FPC}
  SysUtils, Classes, Generics.Collections, fpHTTP, fphttpserver, httpprotocol, HTTPDefs,
{$else}
  System.Classes, System.SysUtils, System.Generics.Collections,
  Web.HTTPApp, IdCustomHTTPServer, IdHeaderList,
  PonyRtti,
{$IF DEFINED(Pony_APACHE)}
  Web.ApacheHTTP, Web.HTTPD24,
{$endif}
{$endif}
  PonyCoreParam, PonyCommons, PonyRttiHelper, PonyTypes;

type
  TPonyCoreParamHeader = class
  private
{$ifdef FPC}
    class function GetHeadersList(const AWebRequest: TRequest): TStrings;
{$else}
    class function GetHeadersList(const AWebRequest: TWebRequest): TStrings;
{$endif}
  public
    class function GetHeaders(const AWebRequest: {$ifdef FPC}TRequest{$else}TWebRequest{$endif}): TPonyList;
  end;

implementation

class function TPonyCoreParamHeader.GetHeaders(const AWebRequest: {$ifdef FPC}TRequest{$else}TWebRequest{$endif}): TPonyList;
var
  I: Integer;
  LName, LValue: string;
  LHeaders: TStrings;
begin
  Result:= TPonyList.create;
  try
    LHeaders:= GetHeadersList(AWebRequest);
    try
      for I:= 0 to Pred(LHeaders.Count) do
      begin
        LName:= LHeaders.Names[I];
        LValue:= LHeaders.Values[LName];
        Result.AddOrSetValue(LName, Trim(LValue));
      end;
{$ifdef FPC}
      for I:= Integer(Low(THeader)) to Integer(High(THeader)) do
      begin
        LName:= HTTPHeaderNames[THeader(I)];
        LValue:= AWebRequest.GetHeader(THeader(I));
        if not LValue.Trim.IsEmpty then
          Result.AddOrSetValue(LName, LValue);
      end;
{$endif}
    finally
      LHeaders.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

{$ifdef FPC}
class function TPonyCoreParamHeader.GetHeadersList(const AWebRequest: TRequest): TStrings;
var
  LRequest: TFPHTTPConnectionRequest;
begin
  Result:= TStringList.create;
  try
    if AWebRequest is TFPHTTPConnectionRequest then
    begin
      LRequest:= TFPHTTPConnectionRequest(AWebRequest);
      Result.NameValueSeparator:= '=';
      Result.Text:= LRequest.CustomHeaders.Text;
    end;
  except
    Result.Free;
    raise;
  end;
end;
{$else}
class function TPonyCoreParamHeader.GetHeadersList(const AWebRequest: TWebRequest): TStrings;
{$IF DEFINED(Pony_APACHE)}
type
  Papr_table_entry_t = ^apr_table_entry_t;

var
  LHeadersArray: papr_array_header_t;
  LHeadersEntry: Papr_table_entry_t;
  I: Integer;
{$ELSEIF NOT DEFINED(Pony_ISAPI)}
var
  LRequest: TIdHTTPRequestInfo;
  LObject: TObject;
{$endif}
begin
  Result:= TStringList.create;
  try
    Result.NameValueSeparator:= ':';

{$IF DEFINED(Pony_ISAPI)}
    Result.Text:= AWebRequest.GetFieldByName('ALL_RAW');
{$ELSEIF DEFINED(Pony_APACHE)}
    LHeadersArray:= papr_array_header_t(Prequest_rec(TApacheRequest(AWebRequest).HTTPDRequest)^.headers_in);
    LHeadersEntry:= Papr_table_entry_t(LHeadersArray^.elts);

    for I:= 0 to Pred(LHeadersArray^.nelts) do
    begin
      Result.Add(string(LHeadersEntry^.key) + Result.NameValueSeparator + string(LHeadersEntry^.val));
      Inc(LHeadersEntry);
    end;
{$else}
    LObject:= TPonyRtti.GetInstance.GetType(AWebRequest.ClassType).FieldValueAsObject(AWebRequest, 'FRequestInfo');
    if (Assigned(LObject)) and (LObject is TIdHTTPRequestInfo) then
    begin
      LRequest:= TIdHTTPRequestInfo(LObject);
      Result.Text:= LRequest.RawHeaders.Text;
    end
{$endif}
  except
    Result.Free;
    raise;
  end;
end;
{$endif}

end.
