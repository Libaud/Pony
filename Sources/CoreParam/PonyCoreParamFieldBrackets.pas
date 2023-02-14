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
unit PonyCoreParamFieldBrackets;

//{$ifdef FPC}
//  {$MODE DELPHI}{$H+}
//{$endif}
{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
{$ifdef FPC}
  SysUtils, Classes,
{$else}
  System.SysUtils, System.Classes,
{$endif}
  PonyParamsInterfaces, PonyCommons;

type
  TPonyCoreParamFieldLhsBrackets = class (IPonyCoreParamFieldLhsBrackets)
      private
        // Members
        _RefCount: integer;
        FEq: string;
        FNe: string;
        FLt: string;
        FLte: string;
        FGt: string;
        FGte: string;
        FRange: string;
        FLike: string;
        FTypes: TLhsBrackets;
        // Method's
        // Access Method's
        function geteq: string;
        function getne: string;
        function getlt: string;
        function getlte: string;
        function getgt: string;
        function getgte: string;
        function getrange: string;
        function getlike: string;
        function GetTypes: TLhsBrackets;
        procedure SetTypes(aTypes: TLhsBrackets);
      public
        // Interfaces Implementation Method's
        function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$else}const{$endif} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
        function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
        function _Release : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
        // Method's
        procedure SetValue(const AType: TLhsBracketsType; const AValue: string);
        function GetValue(const AType: TLhsBracketsType): string;
        // Properties
        property Eq: string read geteq;
        property Ne: string read getne;
        property Lt: string read getlt;
        property Lte: string read getlte;
        property Gt: string read getgt;
        property Gte: string read getgte;
        property Range: string read getrange;
        property Like: string read getlike;
        property Types: TLhsBrackets read FTypes write FTypes;
    end;

implementation

// Interfaces Implementation Method's

function TPonyCoreParamFieldLhsBrackets.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$else}const{$endif} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  if GetInterface(IID,Obj) then
	Result:= S_OK
  else
	Result:= E_NOINTERFACE;
end;

function TPonyCoreParamFieldLhsBrackets._AddRef : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  Result:= InterlockedIncrement(_RefCount);
end;

function TPonyCoreParamFieldLhsBrackets._Release : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  Result:= InterlockedDecrement(_RefCount);
  {if Result = 0 then
	Self.Destroy;}
end;

// Private Method's

// Private Access Method's

function TPonyCoreParamFieldLhsBrackets.geteq: string;
begin
  Result:= feq;
end;

function TPonyCoreParamFieldLhsBrackets.getne: string;
begin
  Result:= fne;
end;

function TPonyCoreParamFieldLhsBrackets.getlt: string;
begin
  Result:= flt;
end;

function TPonyCoreParamFieldLhsBrackets.getlte: string;
begin
  Result:= flte;
end;

function TPonyCoreParamFieldLhsBrackets.getgt: string;
begin
  Result:= fgt;
end;

function TPonyCoreParamFieldLhsBrackets.getgte: string;
begin
  Result:= fgte;
end;

function TPonyCoreParamFieldLhsBrackets.getrange: string;
begin
  Result:= frange;
end;

function TPonyCoreParamFieldLhsBrackets.getlike: string;
begin
  Result:= flike;
end;

function TPonyCoreParamFieldLhsBrackets.GetTypes: TLhsBrackets;
begin
  Result:= ftypes;
end;

procedure TPonyCoreParamFieldLhsBrackets.SetTypes(aTypes: TLhsBrackets);
begin
  FTypes:= aTypes;
end;

// Public Method's

procedure TPonyCoreParamFieldLhsBrackets.SetValue(const AType: TLhsBracketsType; const AValue: string);
begin
  case AType of
    TLhsBracketsType.Equal:
      FEq:= AValue;
    TLhsBracketsType.NotEqual:
      FNe:= AValue;
    TLhsBracketsType.LessThan:
      FLt:= AValue;
    TLhsBracketsType.LessThanOrEqual:
      FLte:= AValue;
    TLhsBracketsType.GreaterThan:
      FGt:= AValue;
    TLhsBracketsType.GreaterThanOrEqual:
      FGte:= AValue;
    TLhsBracketsType.Range:
      FRange:= AValue;
    TLhsBracketsType.Like:
      FLike:= AValue;
  end;
end;

function TPonyCoreParamFieldLhsBrackets.GetValue(const AType: TLhsBracketsType): string;
begin
  case AType of
    TLhsBracketsType.Equal:
      Result:= FEq;
    TLhsBracketsType.NotEqual:
      Result:= FNe;
    TLhsBracketsType.LessThan:
      Result:= FLt;
    TLhsBracketsType.LessThanOrEqual:
      Result:= FLte;
    TLhsBracketsType.GreaterThan:
      Result:= FGt;
    TLhsBracketsType.GreaterThanOrEqual:
      Result:= FGte;
    TLhsBracketsType.Range:
      Result:= FRange;
    TLhsBracketsType.Like:
      Result:= FLike;
  end;
end;

end.

