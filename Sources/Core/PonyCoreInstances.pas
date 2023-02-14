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
unit PonyCoreInstances;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  {$ifdef FPC}
  Classes, SysUtils,
  {$else}
  Classes, System.SysUtils,
  {$endif}
  PonyInterfaces, PonyRouterTreeInterfaces,
  PonyModuleInterfaces;

type
  TPonyCoreInstance = {$ifdef FPC}class{$else}record{$endif} (IPonyCoreInsance)
      private
        _RefCount: integer;
        FSelfInstance: IPonyCore;
        FDefaultPonyCoreInstance: IPonyCore;
        FPonyRouterTree: IPonyRouterTree;
        function GetSelfInstance: IPonyCore;
        function GetDefaultPonyCoreInstance: IPonyCore;
        function GetPonyRouterTree: IPonyRouterTree;
      public
        // Interfaces Implementation Method's
        function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$else}const{$endif} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
        function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
        function _Release : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
        function ToPony: IPonyCore;
        constructor Create(const aSelfInstance, aDefaultPonyCoreInstance: IPonyCore; const aPonyRouterTree: IPonyRouterTree);
    end;

implementation

{ TPonyCoreInstance implementation }

constructor TPonyCoreInstance.Create(const aSelfInstance, aDefaultPonyCoreInstance: IPonyCore; const aPonyRouterTree: IPonyRouterTree);
begin
  FSelfInstance:= ASelfInstance;
  FDefaultPonyCoreInstance:= ADefaultPonyCoreInstance;
  FPonyRouterTree:= APonyRouterTree;
end;

// Interfaces Implementation Method's

function TPonyCoreInstance.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$else}const{$endif} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  if GetInterface(IID,Obj) then
	Result:= S_OK
  else
	Result:= E_NOINTERFACE;
end;

function TPonyCoreInstance._AddRef : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  Result:= InterlockedIncrement(_RefCount);
end;

function TPonyCoreInstance._Release : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  Result:= InterlockedDecrement(_RefCount);
  {if Result = 0 then
	Self.Destroy;}
end;


function TPonyCoreInstance.ToPony: IPonyCore;
begin
  Result:= GetSelfInstance;
  Result.DefaultPony:= GetDefaultPonyCoreInstance;
  Result.Routes:= GetPonyRouterTree;
end;

function TPonyCoreInstance.GetDefaultPonyCoreInstance: IPonyCore;
begin
  Result:= FDefaultPonyCoreInstance;
end;

function TPonyCoreInstance.GetPonyRouterTree: IPonyRouterTree;
begin
  Result:= FPonyRouterTree;
end;

function TPonyCoreInstance.GetSelfInstance: IPonyCore;
begin
  Result:= FSelfInstance;
end;

{ End of TPonyCoreInstance implementation }

end.

