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
unit PonyRtti;

//{$ifdef FPC}
//  {$MODE DELPHI}{$H+}
//{$endif}
{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
{$ifdef FPC}
  SysUtils, RTTI,
{$else}
  System.SysUtils, System.Rtti,
{$endif}
  PonyCommons;

type
  TPonyRtti = class
  private
    class var FPonyRtti: TPonyRtti;
    FContext: TRttiContext;
  protected
    class function GetDefaultPonyRtti: TPonyRtti;
  public
    function GetType(const AClass: TClass): TRttiType;
    constructor Create; virtual;
    class destructor UnInitialize; {$IFNDEF FPC}virtual;{$endif}
    class function GetInstance: TPonyRtti;
  end;

implementation

constructor TPonyRtti.Create;
begin
  if FPonyRtti <> nil then
    raise Exception.Create('The Pony Rtti instance has already been created');
  FContext:= TRttiContext.Create;
  FPonyRtti:= Self;
end;

class function TPonyRtti.GetDefaultPonyRtti: TPonyRtti;
begin
  if FPonyRtti = nil then
    FPonyRtti:= TPonyRtti.Create;
  Result:= FPonyRtti;
end;

class function TPonyRtti.GetInstance: TPonyRtti;
begin
  Result:= GetDefaultPonyRtti;
end;

function TPonyRtti.GetType(const AClass: TClass): TRttiType;
begin
  Result:= FContext.GetType(AClass);
end;

class destructor TPonyRtti.UnInitialize;
begin
  if FPonyRtti <> nil then
    FreeAndNil(FPonyRtti);
end;

end.

