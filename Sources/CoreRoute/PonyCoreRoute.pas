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
unit PonyCoreRoute;

//{$ifdef FPC}
//  {$MODE DELPHI}{$H+}
//{$endif}
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
  PonyCoreRouteContract{, PonyCoreRouterTree}, PonyCallback, PonyInterfaces;

type
  {$ifdef FPC}generic{$endif}TPonyCoreRoute<T: class> = class(TInterfacedObject, {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>)
  private
    FPath: string;
    FPonyCore: IPonyCore;
  public
    constructor Create(const APath: string);

    function This: {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
    function AddCallback(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
    function AddCallbacks(const ACallbacks: {$ifdef FPC}specialize{$endif} TArray<TPonyCallback>): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;

    function All(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>; overload;
    function All(const AMiddleware, ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>; overload;
    function All(const ACallbacks: array of TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>; overload;
    function All(const ACallbacks: array of TPonyCallback; const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;

    function Get(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>; overload;
    function Get(const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>; overload;
    function Get(const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>; overload;
    {$IFNDEF FPC}
    function Get(const ACallback: TPonyCallbackResponse): IPonyCoreRoute<T>; overload;
    {$IFEND}

    function Put(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>; overload;
    function Put(const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
    function Put(const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>; overload;
    {$IFNDEF FPC}
    function Put(const ACallback: TPonyCallbackResponse): IPonyCoreRoute<T>; overload;
    {$IFEND}

    function Head(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>; overload;
    function Head(const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>; overload;
    function Head(const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>; overload;
    {$IFNDEF FPC}
    function Head(const ACallback: TPonyCallbackResponse): IPonyCoreRoute<T>; overload;
    {$IFEND}

    function Post(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>; overload;
    function Post(const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>; overload;
    function Post(const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>; overload;
    {$IFNDEF FPC}
    function Post(const ACallback: TPonyCallbackResponse): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>; overload;
    {$IFEND}

    {$IF (DEFINED(FPC) OR (CompilerVersion > 27.0))}
    function Patch(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>; overload;
    function Delete(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>; overload;

      function Patch(const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>; overload;
      function Patch(const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>; overload;
      {$IFNDEF FPC}
      function Patch(const ACallback: TPonyCallbackResponse): IPonyCoreRoute<T>; overload;
      {$IFEND}

      function Delete(const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>; overload;
      function Delete(const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>; overload;
      {$IFNDEF FPC}
      function Delete(const ACallback: TPonyCallbackResponse): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>; overload;
      {$IFEND}
    {$IFEND}

    function &End: T;
  end;

implementation

{uses
  PonyCore, PonyInterfaces;}

constructor TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Create(const APath: string);
begin
  FPath:= APath;
  //FPonyCore:= TPonyCore.GetInstance;
  FPonyCore:= oPonyCore;
end;

function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.This: {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  Result:= Self;
end;

function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.All(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  Result:= Self;
  FPonyCore.Use(FPath, ACallback);
end;

function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.All(const AMiddleware, ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  Result:= Self;
  FPonyCore.Use(FPath, [AMiddleware, ACallback]);
end;

function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.All(const ACallbacks: array of TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  Result:= Self;
  FPonyCore.Use(FPath, ACallbacks);
end;

function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.&End: T;
begin
  Result:= FPonyCore as T;
end;

function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.AddCallback(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  Result:= Self;
  FPonyCore.AddCallback(ACallback);
end;

function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.All(const ACallbacks: array of TPonyCallback; const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  Result:= Self;
  FPonyCore.Use(FPath, ACallbacks);
  FPonyCore.Use(FPath, [ACallback]);
end;

{$IF (DEFINED(FPC) OR (CompilerVersion > 27.0))}
function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Delete(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  Result:= Self;
  FPonyCore.Delete(FPath, ACallback);
end;

function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Patch(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  Result:= Self;
  FPonyCore.Patch(FPath, ACallback);
end;

  function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Delete(const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
  begin
    FPonyCore.Delete(FPath, ACallback);
    Result:= Self;
  end;

  function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Delete(const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
  begin
    FPonyCore.Delete(FPath, ACallback);
    Result:= Self;
  end;

  {$IFNDEF FPC}
  function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Delete(const ACallback: TPonyCallbackResponse): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
  begin
    FPonyCore.Delete(FPath, ACallback);
    Result:= Self;
  end;
  {$IFEND}

  function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Patch(const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
  begin
    FPonyCore.Patch(FPath, ACallback);
    Result:= Self;
  end;

  function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Patch(const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
  begin
    FPonyCore.Patch(FPath, ACallback);
    Result:= Self;
  end;

  {$IFNDEF FPC}
  function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Patch(const ACallback: TPonyCallbackResponse): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
  begin
    FPonyCore.Patch(FPath, ACallback);
    Result:= Self;
  end;
  {$IFEND}
{$IFEND}

function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Get(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  Result:= Self;
  FPonyCore.Get(FPath, ACallback);
end;

function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Head(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  Result:= Self;
  FPonyCore.Head(FPath, ACallback);
end;

function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Post(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  Result:= Self;
  FPonyCore.Post(FPath, ACallback);
end;

function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Put(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  Result:= Self;
  FPonyCore.Put(FPath, ACallback);
end;

function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.AddCallbacks(const ACallbacks: {$ifdef FPC}specialize{$endif} TArray<TPonyCallback>): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
var
  LCallback: TPonyCallback;
begin
  for LCallback in ACallbacks do
    AddCallback(LCallback);
  Result:= Self;
end;

function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Get(const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  FPonyCore.Get(FPath, ACallback);
  Result:= Self;
end;

function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Get(const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  FPonyCore.Get(FPath, ACallback);
  Result:= Self;
end;

{$IFNDEF FPC}
function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Get(const ACallback: TPonyCallbackResponse): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  FPonyCore.Get(FPath, ACallback);
  Result:= Self;
end;
{$IFEND}

function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Head(const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  FPonyCore.Head(FPath, ACallback);
  Result:= Self;
end;

function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Head(const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  FPonyCore.Head(FPath, ACallback);
  Result:= Self;
end;

{$IFNDEF FPC}
function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Head(const ACallback: TPonyCallbackResponse): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  FPonyCore.Head(FPath, ACallback);
  Result:= Self;
end;
{$IFEND}

function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Post(const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  FPonyCore.Post(FPath, ACallback);
  Result:= Self;
end;

function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Post(const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  FPonyCore.Post(FPath, ACallback);
  Result:= Self;
end;

{$IFNDEF FPC}
function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Post(const ACallback: TPonyCallbackResponse): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  FPonyCore.Post(FPath, ACallback);
  Result:= Self;
end;
{$IFEND}

{$IFNDEF FPC}
function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Put(const ACallback: TPonyCallbackResponse): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  FPonyCore.Put(FPath, ACallback);
  Result:= Self;
end;
{$IFEND}

function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Put(const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  FPonyCore.Put(FPath, ACallback);
  Result:= Self;
end;

function TPonyCoreRoute{$ifndef FPC}<T>{$endif}.Put(const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  FPonyCore.Put(FPath, ACallback);
  Result:= Self;
end;

end.
