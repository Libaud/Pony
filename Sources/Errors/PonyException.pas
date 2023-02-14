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
unit PonyException;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
{$ifdef FPC}
  SysUtils,
{$else}
  System.SysUtils,
{$endif}
  PonyCommons;

type
  EPonyException = class(Exception)
  strict private
    FError: string;
    FStatus: THTTPStatus;
    FType: TMessageType;
    FTitle: string;
    FCode: Integer;
    FHint: string;
    FUnit: string;
  public
    constructor Create; reintroduce;
    function Error(const AValue: string): EPonyException; overload;
    function Error: string; overload;
    function Status(const AValue: THTTPStatus): EPonyException; overload;
    function Status: THTTPStatus; overload;
    function &Type(const AValue: TMessageType): EPonyException; overload;
    function &Type: TMessageType; overload;
    function Title(const AValue: string): EPonyException; overload;
    function Title: string; overload;
    function Code(const AValue: Integer): EPonyException; overload;
    function Code: Integer; overload;
    function Hint(const AValue: string): EPonyException; overload;
    function Hint: string; overload;
    function &Unit(const AValue: string): EPonyException; overload;
    function &Unit: string; overload;
    function ToJSON: string; virtual;
    class function New: EPonyException;
  end;

implementation

uses
  {$ifdef FPC}
  fpjson, jsonparser, TypInfo;
  {$else}
  System.JSON, System.TypInfo;
  {$endif}

constructor EPonyException.Create;
begin
  FError:= EmptyStr;
  FStatus:= THTTPStatus.InternalServerError;
  FCode:= 0;
end;

class function EPonyException.New: EPonyException;
begin
  Result:= EPonyException.Create;
end;

function EPonyException.Code(const AValue: Integer): EPonyException;
begin
  FCode:= AValue;
  Result:= Self;
end;

function EPonyException.&Type: TMessageType;
begin
  Result:= FType;
end;

function EPonyException.&Type(const AValue: TMessageType): EPonyException;
begin
  FType:= AValue;
  Result:= Self;
end;

function EPonyException.Code: Integer;
begin
  Result:= FCode;
end;

function EPonyException.&Unit: string;
begin
  Result:= FUnit;
end;

function EPonyException.&Unit(const AValue: string): EPonyException;
begin
  FUnit:= AValue;
  Result:= Self;
end;

function EPonyException.Error: string;
begin
  Result:= FError;
end;

function EPonyException.Error(const AValue: string): EPonyException;
begin
  FError:= AValue;
  Result:= Self;
end;

function EPonyException.Status: THTTPStatus;
begin
  Result:= FStatus;
end;

function EPonyException.Status(const AValue: THTTPStatus): EPonyException;
begin
  FStatus:= AValue;
  Result:= Self;
end;

function EPonyException.Title(const AValue: string): EPonyException;
begin
  FTitle:= AValue;
  Result:= Self;
end;

function EPonyException.Title: string;
begin
  Result:= FTitle;
end;

function EPonyException.Hint(const AValue: string): EPonyException;
begin
  FHint:= AValue;
  Result:= Self;
end;

function EPonyException.Hint: string;
begin
  Result:= FHint;
end;

function EPonyException.ToJSON: string;
var
  LJSON: TJSONObject;
begin
  LJSON:= TJSONObject.Create;
  try
    if (FType <> TMessageType.Default) then
    begin
      LJSON.{$ifdef FPC}Add{$else}AddPair{$endif}('type', GetEnumName(TypeInfo(TMessageType), Integer(FType)));
    end;
    if not FTitle.Trim.IsEmpty then
    begin
      LJSON.{$ifdef FPC}Add{$else}AddPair{$endif}('title', FTitle);
    end;
    if FCode <> 0 then
    begin
      LJSON.{$ifdef FPC}Add{$else}AddPair{$endif}('code', {$ifdef FPC}TJSONIntegerNumber{$else}TJSONNumber{$endif}.Create(FCode));
    end;
    LJSON.{$ifdef FPC}Add{$else}AddPair{$endif}('error', FError);
    if not FHint.Trim.IsEmpty then
    begin
      LJSON.{$ifdef FPC}Add{$else}AddPair{$endif}('hint', FHint);
    end;
    if not FUnit.Trim.IsEmpty then
    begin
      LJSON.{$ifdef FPC}Add{$else}AddPair{$endif}('unit', FUnit);
    end;
    Result:= {$ifdef FPC}LJSON.AsJSON{$else}{$IF CompilerVersion > 27.0}LJSON.ToJSON{$else}LJSON.ToString{$endif}{$endif};
  finally
    LJSON.Free;
  end;
end;

end.
