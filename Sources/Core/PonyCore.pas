{=============================================================================
 Unit : PonyCore
 Framework : Pony
 Author : Frédéric Libaud
 Description : PonyCore implementation
 Features:

 =============================================================================
 History
 -----------------------------------------------------------------------------
 08/02/2022 - Forking code from Horse framework
 =============================================================================}
unit PonyCore;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  {$ifdef FPC}
  Classes, SysUtils, Generics.Collections,
  {$else}
  Classes, System.SysUtils, System.Generics.Collections,
  {$endif}
  PonyCoreRouterTree, PonyCommons, PonyConstants, PonyCallback, PonyCoreGroupContract,
  PonyCoreRouteContract, PonyInterfaces, PonyRouterTreeInterfaces, PonyModuleInterfaces,
  PonyTypes;

type
  {TPonyCore = class;

  PPonyCore = ^TPonyCore;

  PPonyModule = ^TPonyModule;}

  TPonyCore = class (TPersistent, IPonyCore)
  private
    _RefCount: integer;
    FRoutes: TPonyRouterTree;
    FCallbacks: {$ifdef FPC}specialize{$endif} TList<TPonyCallback>;
    FDefaultPony: IPonyCore;
    _Port: TPort;
    // Methods
    function TrimPath(const aPath: string): string;
    function RegisterRoute(const aHTTPType: TMethodType; const aPath: string; const aCallback: TPonyObjCallback): TPonyCore; overload;
    function RegisterRoute(const aHTTPType: TMethodType; const aPath: string; const aCallback: TPonyCallback): TPonyCore; overload;

    function InternalRoute(const aPath: string): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<TPonyCore>;
    function InternalGroup: {$ifdef FPC}specialize{$endif} IPonyCoreGroup<TPonyCore>;
    function InternalGetRoutes: TPonyRouterTree;
    procedure InternalSetRoutes(const aValue: TPonyRouterTree);
    function MakePonyModule: IPonyCoreInsance;

    function GetCallback(const aCallbackRequest: TPonyCallbackRequestResponse): TPonyCallback; {$ifdef FPC}virtual;{$endif} overload;
    function GetCallback(const aCallbackRequest: TPonyCallbackRequest): TPonyCallback; {$ifdef FPC}virtual;{$endif} overload;
    {$IFNDEF FPC}
    class function GetCallback(const aCallbackResponse: TPonyCallbackResponse): TPonyCallback; overload;
    {$endif}

    function GetCallbacks: {$ifdef FPC}specialize{$endif} TArray<TPonyCallback>;
    function RegisterCallbacksRoute(const aMethod: TMethodType; const aPath: string): TPonyCore;
    // Access Method's
    // Inherieted Access Method's from Interfaces
    function GetPort: TPort;
    procedure SetPort(const aPort: TPort);
    function GetRoutes: IPonyRouterTree; {$ifdef DELPHI}static;{$endif}
    procedure SetRoutes(const aRoutes: IPonyRouterTree); {$ifdef DELPHI}static;{$endif}
    function GetDefaultPony: IPonyCore;
    procedure SetDefaultPony(aDefaultPony: IPonyCore);
  public
    constructor Create; virtual;
    destructor UnInitialize; {$IFNDEF FPC}virtual;{$endif}
    // Method's
    // Interfaces Implementation Method's
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$else}const{$endif} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
    // Class Method's
    function AddCallback(const aCallback: TPonyCallback): IPonyCore;
    function AddCallbacks(const aCallbacks: {$ifdef FPC}specialize{$endif} TArray<TPonyCallback>): IPonyCore;

    function All(const aPath: string; const aCallback: TPonyCallback): IPonyCore; {$ifndef FPC}overload;{$endif}
    function All(const aPath: string; const aCallback: TPonyCallbackRequestResponse): IPonyCore; {$ifndef FPC}overload;{$endif}
    function All(const aPath: string; const aCallback: TPonyCallbackRequest): IPonyCore; {$ifndef FPC}overload;{$endif}
    {$IFNDEF FPC}
    function All(const APath: string; const ACallback: TPonyCallbackResponse): TPonyCore; {$ifndef FPC}overload;{$endif}
    {$IFEND}

    {$IF (defined(fpc) or (CompilerVersion > 27.0))}
    function Delete(const aPath: string; const aCallback: TPonyObjCallback): IPonyCore; {$ifndef FPC}overload;{$endif}
    function Delete(const aPath: string; const aCallback: TPonyCallback): IPonyCore; {$ifndef FPC}overload;{$endif}
    function Delete(const aPath: string; const aCallback: TPonyCallbackRequestResponse): IPonyCore; {$ifndef FPC}overload;{$endif}
    function Delete(const aPath: string; const aCallback: TPonyCallbackRequest): IPonyCore; {$ifndef FPC}overload;{$endif}
    {$IFNDEF FPC}
    function Delete(const aPath: string; const aCallback: TPonyCallbackResponse): IPonyCore; {$ifndef FPC}overload;{$endif}
    {$IFEND}
    {$IFEND}


    function Get(const aPath: string; const aCallback: TPonyObjCallback): IPonyCore; {$ifndef FPC}overload;{$endif}
    function Get(const aPath: string; const aCallback: TPonyCallback): IPonyCore; {$ifndef FPC}overload;{$endif}
    function Get(const aPath: string; const aCallback: TPonyCallbackRequestResponse): IPonyCore; {$ifndef FPC}overload;{$endif}
    function Get(const aPath: string; const aCallback: TPonyCallbackRequest): IPonyCore; {$ifndef FPC}overload;{$endif}
    {$IFNDEF FPC}
    function Get(const aPath: string; const aCallback: TPonyCallbackResponse): IPonyCore; {$ifndef FPC}overload;{$endif}
    {$IFEND}

    class function GetInstance: TPonyCore;

    function Group: {$ifdef FPC}specialize{$endif} IPonyCoreGroup<TPonyCore>;

    function Head(const aPath: string; const aCallback: TPonyCallback): IPonyCore; {$ifndef FPC}overload;{$endif}
    function Head(const aPath: string; const aCallback: TPonyCallbackRequestResponse): IPonyCore; {$ifndef FPC}overload;{$endif}
    function Head(const aPath: string; const aCallback: TPonyCallbackRequest): IPonyCore; {$ifndef FPC}overload;{$endif}
    {$IFNDEF FPC}
    function Head(const aPath: string; const aCallback: TPonyCallbackResponse): IPonyCore; {$ifndef FPC}overload;{$endif}
    {$IFEND}

    {$IF (DEFINED(FPC) or (CompilerVersion > 27.0))}
    function Patch(const aPath: string; const aCallback: TPonyCallback): IPonyCore; {$ifndef FPC}overload;{$endif}
    function Patch(const aPath: string; const aCallback: TPonyCallbackRequestResponse): IPonyCore; {$ifndef FPC}overload;{$endif}
    function Patch(const aPath: string; const aCallback: TPonyCallbackRequest): IPonyCore; {$ifndef FPC}overload;{$endif}
    {$IFNDEF FPC}
    function Patch(const aPath: string; const aCallback: TPonyCallbackResponse): IPonyCore; {$ifdef FPC}overload;{$endif}
    {$IFEND}
    {$IFEND}

    function Post(const aPath: string; const aCallback: TPonyObjCallback): IPonyCore; {$ifdef FPC}overload;{$endif}
    function Post(const aPath: string; const aCallback: TPonyCallback): IPonyCore; {$ifdef FPC}overload;{$endif}
    function Post(const aPath: string; const aCallback: TPonyCallbackRequestResponse): IPonyCore; {$ifdef FPC}overload;{$endif}
    function Post(const aPath: string; const aCallback: TPonyCallbackRequest): IPonyCore; {$ifdef FPC}overload;{$endif}
    {$IFNDEF FPC}
    function Post(const aPath: string; const aCallback: TPonyCallbackResponse): IPonyCore; overload;
    {$IFEND}

    function Put(const aPath: string; const aCallback: TPonyObjCallback): IPonyCore; {$ifdef FPC}overload;{$endif}
    function Put(const aPath: string; const aCallback: TPonyCallback): IPonyCore; {$ifdef FPC}overload;{$endif}
    function Put(const aPath: string; const aCallback: TPonyCallbackRequestResponse): IPonyCore; {$ifdef FPC}overload;{$endif}
    function Put(const aPath: string; const aCallback: TPonyCallbackRequest): IPonyCore; {$ifdef FPC}overload;{$endif}
    {$IFNDEF FPC}
    function Put(const aPath: string; const aCallback: TPonyCallbackResponse): IPonyCore; {$ifdef FPC}overload;{$endif}
    {$IFEND}

    function Route(const aPath: string): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<TPonyCore>;

    function ToModule: IPonyCoreInsance;

    function Use(const aPath: string; const aCallback: TPonyCallback): IPonyCore; {$ifdef FPC}overload;{$endif}
    function Use(const aCallback: TPonyCallback): IPonyCore; {$ifdef FPC}virtual;{$endif} {$ifdef FPC}overload;{$endif}
    function Use(const aPath: string; const aCallbacks: array of TPonyCallback): IPonyCore; {$ifdef FPC}overload;{$endif}
    function Use(const aCallbacks: array of TPonyCallback): IPonyCore; {$ifdef FPC}overload;{$endif}

    function Version: string;

    // Properties
    property Port: TPort read GetPort
                         write SetPort;
    property Routes: IPonyRouterTree read GetRoutes
                                     write SetRoutes;
    property DefaultPony: IPonyCore read GetDefaultPony
                                    write SetDefaultPony;
  end;

implementation

uses
  PonyCoreInstances, PonyCoreRoute, PonyCoreGroup;

var
  oPony: TPonyCore;

{ TPonyCore implementation }

// Constructor's and destructor's

constructor TPonyCore.Create;
begin
  if FDefaultPony <> nil then
    raise Exception.Create('The Pony instance has already been created');
  if FRoutes = nil then
    FRoutes:= TPonyRouterTree.Create;
  FDefaultPony:= Self
end;

destructor TPonyCore.UnInitialize;
begin
  if FDefaultPony <> nil then
    FreeAndNil(FDefaultPony);
  if FRoutes <> nil then
    FreeAndNil(FRoutes);
  if FCallbacks <> nil then
    FreeAndNil(FCallbacks);
end;

// Interfaces Implementation Method's

function TPonyCore.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$else}const{$endif} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  if GetInterface(IID,Obj) then
	Result:= S_OK
  else
	Result:= E_NOINTERFACE;
end;

function TPonyCore._AddRef : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  Result:= InterlockedIncrement(_RefCount);
end;

function TPonyCore._Release : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  Result:= InterlockedDecrement(_RefCount);
  {if Result = 0 then
	Self.Destroy;}
end;

// Private Method's

function TPonyCore.RegisterRoute(const aHTTPType: TMethodType; const aPath: string; const aCallback: TPonyObjCallback): TPonyCore;
var
  LDefaultPony: TPonyCore;
begin
  LDefaultPony:= GetInstance;
  Result:= LDefaultPony;
  LDefaultPony.GetRoutes.RegisterRoute(aHTTPType, TrimPath(aPath), aCallback);
end;

function TPonyCore.RegisterRoute(const aHTTPType: TMethodType; const aPath: string; const aCallback: TPonyCallback): TPonyCore;
var
  LDefaultPony: TPonyCore;
begin
  LDefaultPony:= GetInstance;
  Result:= LDefaultPony;
  LDefaultPony.GetRoutes.RegisterRoute(aHTTPType, TrimPath(aPath), aCallback);
end;

// Private Access Method's

function TPonyCore.GetPort: TPort;
begin
  Result:= _Port;
end;

procedure TPonyCore.SetPort(const aPort: TPort);
begin
  _Port:= aPort;
end;

function TPonyCore.GetRoutes: IPonyRouterTree;
begin
  Result:= GetInstance.InternalGetRoutes;
end;

procedure TPonyCore.SetRoutes(const aRoutes: IPonyRouterTree);
begin
  GetInstance.InternalSetRoutes(aRoutes as TPonyRouterTree);
end;

function TPonyCore.GetCallback(const aCallbackRequest: TPonyCallbackRequest): TPonyCallback;
begin
  Result :=
    {$IFDEF FPC}
     TPonyCallback(ACallbackRequest);
    {$else}
    procedure(Req: TPonyRequest; Res: TPonyResponse; Next: TProc)
    begin
      Res.Status(THTTPStatus.NoContent);
      ACallbackRequest(Req);
    end;
    {$IFEND}
end;

function TPonyCore.GetCallbacks: {$ifdef FPC}specialize{$endif} TArray<TPonyCallback>;
begin
  Result:= [];
  if Assigned(FCallbacks) then
  begin
    Result:= FCallbacks.ToArray;
    FCallbacks.Clear;
  end;
end;

function TPonyCore.RegisterCallbacksRoute(const aMethod: TMethodType; const aPath: string): TPonyCore;
var
  LCallback: TPonyCallback;
begin
  Result:= GetInstance;
  for LCallback in GetCallbacks do
    RegisterRoute(aMethod, aPath, LCallback);
end;

function TPonyCore.GetDefaultPony: IPonyCore;
begin
  Result:= FDefaultPony;
end;

procedure TPonyCore.SetDefaultPony(aDefaultPony: IPonyCore);
begin
  FDefaultPony:= aDefaultPony;
end;

// Public Method's

function TPonyCore.AddCallback(const aCallback: TPonyCallback): IPonyCore;
begin
  Result:= GetInstance;
  if FCallbacks = nil then
    FCallbacks:= {$ifdef FPC}specialize{$endif} TList<TPonyCallback>.Create;
  FCallbacks.Add(ACallback);
end;

function TPonyCore.AddCallbacks(const aCallbacks: {$ifdef FPC}specialize{$endif} TArray<TPonyCallback>): IPonyCore;
var
  LCallback: TPonyCallback;
begin
  for LCallback in ACallbacks do
    AddCallback(LCallback);
  Result:= GetInstance;
end;

class function TPonyCore.GetInstance: TPonyCore;
begin
  {if FDefaultPony = nil then
    FDefaultPony:= TPonyCore.Create;
  Result:= FDefaultPony as TPonyCore;}
end;

function TPonyCore.Group: {$ifdef FPC}specialize{$endif} IPonyCoreGroup<TPonyCore>;
begin
  Result:= GetInstance.InternalGroup();
end;

function TPonyCore.Route(const aPath: string): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<TPonyCore>;
begin
  Result:= GetInstance.InternalRoute(APath);
end;

function TPonyCore.ToModule: IPonyCoreInsance;
begin
  Result:= GetInstance.MakePonyModule;
end;

function TPonyCore.TrimPath(const aPath: string): string;
begin
  Result:= '/' + APath.Trim(['/']);
end;

function TPonyCore.InternalGetRoutes: TPonyRouterTree;
begin
  Result:= FRoutes;
end;

function TPonyCore.InternalGroup: {$ifdef FPC}specialize{$endif} IPonyCoreGroup<TPonyCore>;
begin
  Result:= {$ifdef FPC}specialize{$endif} TPonyCoreGroup<TPonyCore>.Create;
end;

function TPonyCore.InternalRoute(const aPath: string): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<TPonyCore>;
begin
  Result:= {$ifdef FPC}specialize{$endif} TPonyCoreRoute<TPonyCore>.Create(APath);
end;

procedure TPonyCore.InternalSetRoutes(const aValue: TPonyRouterTree);
begin
  FRoutes:= AValue;
end;

function TPonyCore.MakePonyModule: IPonyCoreInsance;
begin
   Result:= TPonyCoreInstance.Create(Self, FDefaultPony, FRoutes);
end;

{$IF (defined(fpc) or (CompilerVersion > 27.0))}
function TPonyCore.Delete(const aPath: string; const aCallback: TPonyCallbackRequestResponse): IPonyCore;
begin
  Result:= Delete(APath, GetCallback(ACallback));
end;

function TPonyCore.Delete(const aPath: string; const aCallback: TPonyCallbackRequest): IPonyCore;
begin
  Result:= Delete(APath, GetCallback(ACallback));
end;

{$IFNDEF FPC}
function TPonyCore.Delete(const aPath: string; const aCallback: TPonyCallbackResponse): IPonyCore;
begin
  Result:= Delete(APath, GetCallback(ACallback));
end;
{$IFEND}

function TPonyCore.Delete(const aPath: string; const aCallback: TPonyObjCallback): IPonyCore;
begin
  Result:= RegisterCallbacksRoute(mtDelete, APath);
  RegisterRoute(mtDelete, APath, ACallback);
end;

function TPonyCore.Delete(const aPath: string; const aCallback: TPonyCallback): IPonyCore;
begin
  Result:= RegisterCallbacksRoute(mtDelete, APath);
  RegisterRoute(mtDelete, APath, ACallback);
end;
{$IFEND}

function TPonyCore.Head(const aPath: string; const aCallback: TPonyCallback): IPonyCore;
begin
  Result:= RegisterCallbacksRoute(mtHead, APath);
  RegisterRoute(mtHead, APath, ACallback);
end;

function TPonyCore.Get(const aPath: string; const aCallback: TPonyObjCallback): IPonyCore;
begin
  Result:= RegisterCallbacksRoute(mtGet, APath);
  RegisterRoute(mtGet, APath, ACallback);
end;

function TPonyCore.Get(const aPath: string; const aCallback: TPonyCallback): IPonyCore;
begin
  Result:= RegisterCallbacksRoute(mtGet, APath);
  RegisterRoute(mtGet, APath, ACallback);
end;

function TPonyCore.Post(const aPath: string; const aCallback: TPonyObjCallback): IPonyCore;
begin
  Result:= RegisterCallbacksRoute(mtPost, APath);
  RegisterRoute(mtPost, aPath, aCallback);
end;

function TPonyCore.Post(const aPath: string; const aCallback: TPonyCallback): IPonyCore;
begin
  Result:= RegisterCallbacksRoute(mtPost, APath);
  RegisterRoute(mtPost, aPath, aCallback);
end;

{$IF (defined(fpc) or (CompilerVersion > 27.0))}
function TPonyCore.Patch(const aPath: string; const aCallback: TPonyCallbackRequestResponse): IPonyCore;
begin
  Result:= Patch(APath, GetCallback(aCallback));
end;

function TPonyCore.Patch(const aPath: string; const aCallback: TPonyCallbackRequest): IPonyCore;
begin
  Result:= Patch(APath, GetCallback(aCallback));
end;

{$IFNDEF FPC}
function TPonyCore.Patch(const APath: string; const ACallback: TPonyCallbackResponse): IPonyCore;
begin
  Result:= Patch(APath, GetCallback(ACallback));
end;
{$IFEND}

function TPonyCore.Patch(const aPath: string; const aCallback: TPonyCallback): IPonyCore;
begin
  Result:= RegisterCallbacksRoute(mtPatch, APath);
  RegisterRoute(mtPatch, aPath, aCallback);
end;
{$IFEND}

function TPonyCore.Put(const aPath: string; const aCallback: TPonyObjCallback): IPonyCore;
begin
  Result:= RegisterCallbacksRoute(mtPut, aPath);
  RegisterRoute(mtPut, aPath, aCallback);
end;

function TPonyCore.Put(const aPath: string; const aCallback: TPonyCallback): IPonyCore;
begin
  Result:= RegisterCallbacksRoute(mtPut, aPath);
  RegisterRoute(mtPut, aPath, aCallback);
end;

function TPonyCore.All(const aPath: string; const aCallback: TPonyCallback): IPonyCore;
var
  LMethodType: TMethodType;
begin
  for LMethodType:= Low(TMethodType) to High(TMethodType) do
  begin
    Result:= RegisterCallbacksRoute(LMethodType, aPath);
    RegisterRoute(LMethodType, aPath, aCallback);
  end;
end;

function TPonyCore.All(const aPath: string; const aCallback: TPonyCallbackRequestResponse): IPonyCore;
begin
  Result:= All(aPath, GetCallback(aCallback));
end;

function TPonyCore.All(const aPath: string; const aCallback: TPonyCallbackRequest): IPonyCore;
begin
  Result:= All(aPath, GetCallback(aCallback));
end;

{$IFNDEF FPC}
function TPonyCore.All(const aPath: string; const aCallback: TPonyCallbackResponse): IPonyCore;
begin
  Result:= All(aPath, GetCallback(aCallback));
end;
{$IFEND}

function TPonyCore.Get(const aPath: string; const aCallback: TPonyCallbackRequest): IPonyCore;
begin
  Result:= Get(aPath, GetCallback(aCallback));
end;

{$IFNDEF FPC}
function TPonyCore.Get(const APath: string; const ACallback: TPonyCallbackResponse): IPonyCore;
begin
  Result:= Get(aPath, GetCallback(aCallback));
end;
{$IFEND}

function TPonyCore.Get(const aPath: string; const aCallback: TPonyCallbackRequestResponse): IPonyCore;
begin
  Result:= Get(aPath, GetCallback(aCallback));
end;

function TPonyCore.GetCallback(const aCallbackRequest: TPonyCallbackRequestResponse): TPonyCallback;
begin
  Result :=
    {$IFDEF FPC}
     TPonyCallback(aCallbackRequest);
    {$else}
    procedure(Req: TPonyRequest; Res: TPonyResponse; Next: TProc)
    begin
      ACallbackRequest(Req, Res);
    end;
    {$IFEND}
end;

{$IFNDEF FPC}
function TPonyCore.GetCallback(const aCallbackResponse: TPonyCallbackResponse): TPonyCallback;
begin
  Result :=
    procedure(Req: TPonyRequest; Res: TPonyResponse; Next: TProc)
    begin
      ACallbackResponse(Res);
    end;
end;
{$IFEND}

function TPonyCore.Head(const aPath: string; const aCallback: TPonyCallbackRequestResponse): IPonyCore;
begin
  Result:= Head(aPath, GetCallback(aCallback));
end;

function TPonyCore.Head(const aPath: string; const aCallback: TPonyCallbackRequest): IPonyCore;
begin
  Result:= Head(APath, GetCallback(aCallback));
end;

{$IFNDEF FPC}
function TPonyCore.Head(const aPath: string; const aCallback: TPonyCallbackResponse): IPonyCore;
begin
  Result:= Head(aPath, GetCallback(aCallback));
end;
{$IFEND}

function TPonyCore.Post(const aPath: string; const aCallback: TPonyCallbackRequestResponse): IPonyCore;
begin
  Result:= Post(aPath, GetCallback(aCallback));
end;

function TPonyCore.Post(const APath: string; const ACallback: TPonyCallbackRequest): IPonyCore;
begin
  Result:= Post(APath, GetCallback(ACallback));
end;

{$IFNDEF FPC}
function TPonyCore.Post(const APath: string; const ACallback: TPonyCallbackResponse): TPonyCore;
begin
  Result:= Post(APath, GetCallback(ACallback));
end;
{$IFEND}

function TPonyCore.Put(const APath: string; const ACallback: TPonyCallbackRequestResponse): IPonyCore;
begin
  Result:= Put(APath, GetCallback(ACallback));
end;

function TPonyCore.Put(const APath: string; const ACallback: TPonyCallbackRequest): IPonyCore;
begin
  Result:= Put(APath, GetCallback(ACallback));
end;

{$IFNDEF FPC}
function TPonyCore.Put(const APath: string; const ACallback: TPonyCallbackResponse): IPonyCore;
begin
  Result:= Put(APath, GetCallback(ACallback));
end;
{$IFEND}

function TPonyCore.Use(const ACallbacks: array of TPonyCallback): IPonyCore;
var
  LCallback: TPonyCallback;
begin
  Result:= GetInstance;
  for LCallback in ACallbacks do
    Use(LCallback);
end;

function TPonyCore.Use(const APath: string; const ACallbacks: array of TPonyCallback): IPonyCore;
var
  LCallback: TPonyCallback;
begin
  Result:= GetInstance;
  for LCallback in ACallbacks do
    Use(APath, LCallback);
end;

function TPonyCore.Use(const ACallback: TPonyCallback): IPonyCore;
begin
  Result:= GetInstance;
  Result.Routes.RegisterMiddleware('/', ACallback);
end;

function TPonyCore.Use(const APath: string; const ACallback: TPonyCallback): IPonyCore;
begin
  Result:= GetInstance;
  Result.Routes.RegisterMiddleware(TrimPath(APath), ACallback);
end;

function TPonyCore.Version: string;
begin
  Result:= Pony_VERSION;
end;

{ End of TPonyCore implementation }

end.
