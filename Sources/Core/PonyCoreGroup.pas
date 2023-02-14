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
unit PonyCoreGroup;

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
  PonyCoreGroupContract, PonyCoreRouteContract, PonyCallback;

type
  {$ifdef FPC}generic{$endif} TPonyCoreGroup<T: class> = class(TInterfacedObject, {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>)
  private
    FPonyCore: TObject;
    FPrefix: string;
    function NormalizePath(const APath: string): string;
  public
    constructor Create;

    function Prefix(const APrefix: string): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
    function Route(const APath: string): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;

    function AddCallback(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
    function AddCallbacks(const ACallbacks: {$ifdef FPC}specialize{$endif} TArray<TPonyCallback>): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;

    function Use(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
    function Use(const AMiddleware, ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
    function Use(const ACallbacks: array of TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
    function Use(const ACallbacks: array of TPonyCallback; const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;

    function Get(const APath: string; const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
    function Get(const APath: string; const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
    function Get(const APath: string; const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
    {$IFNDEF FPC}
    function Get(const APath: string; const ACallback: TPonyCallbackResponse): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
    {$IFEND}

    function Put(const APath: string; const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
    function Put(const APath: string; const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
    function Put(const APath: string; const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
    {$IFNDEF FPC}
    function Put(const APath: string; const ACallback: TPonyCallbackResponse): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
    {$IFEND}

    function Head(const APath: string; const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
    function Head(const APath: string; const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
    function Head(const APath: string; const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
    {$IFNDEF FPC}
    function Head(const APath: string; const ACallback: TPonyCallbackResponse): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
    {$IFEND}

    function Post(const APath: string; const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
    function Post(const APath: string; const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
    function Post(const APath: string; const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
    {$IFNDEF FPC}
    function Post(const APath: string; const ACallback: TPonyCallbackResponse): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
    {$IFEND}

    {$IF (defined(fpc) or (CompilerVersion > 27.0))}
    function Patch(const APath: string; const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
    function Delete(const APath: string; const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;

      function Patch(const APath: string; const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
      function Patch(const APath: string; const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
      {$IFNDEF FPC}
      function Patch(const APath: string; const ACallback: TPonyCallbackResponse): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
      {$IFEND}

      function Delete(const APath: string; const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
      function Delete(const APath: string; const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
      {$IFNDEF FPC}
      function Delete(const APath: string; const ACallback: TPonyCallbackResponse): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>; overload;
      {$IFEND}
    {$IFEND}

    function &End: T;
  end;

implementation

uses
  PonyCore{, Pony};

function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.&End: T;
begin
  Result:= FPonyCore as T;
end;

function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Get(const APath: string; const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
begin
  TPonyCore(FPonyCore).Get(NormalizePath(APath), ACallback);
  Result:= Self;
end;

function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Get(const APath: string; const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
begin
  TPonyCore(FPonyCore).Get(NormalizePath(APath), ACallback);
  Result:= Self;
end;

{$IFNDEF FPC}
function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Get(const APath: string; const ACallback: TPonyCallbackResponse): IPonyCoreGroup<T>;
begin
  TPonyCore(FPonyCore).Get(NormalizePath(APath), ACallback);
  Result:= Self;
end;
{$IFEND}

function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Post(const APath: string; const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
begin
  TPonyCore(FPonyCore).Post(NormalizePath(APath), ACallback);
  Result:= Self;
end;

function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Post(const APath: string; const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
begin
  TPonyCore(FPonyCore).Post(NormalizePath(APath), ACallback);
  Result:= Self;
end;

{$IFNDEF FPC}
function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Post(const APath: string; const ACallback: TPonyCallbackResponse): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
begin
  TPonyCore(FPonyCore).Post(NormalizePath(APath), ACallback);
  Result:= Self;
end;
{$endif}

{$IFNDEF FPC}
function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Put(const APath: string; const ACallback: TPonyCallbackResponse): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
begin
  TPonyCore(FPonyCore).Put(NormalizePath(APath), ACallback);
  Result:= Self;
end;
{$IFEND}

function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Put(const APath: string; const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
begin
  TPonyCore(FPonyCore).Put(NormalizePath(APath), ACallback);
  Result:= Self;
end;

function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Put(const APath: string; const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
begin
  TPonyCore(FPonyCore).Put(NormalizePath(APath), ACallback);
  Result:= Self;
end;

{$IFNDEF FPC}
function TPonyCoreGroup<T>.Head(const APath: string; const ACallback: TPonyCallbackResponse): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
begin
  TPonyCore(FPonyCore).Head(NormalizePath(APath), ACallback);
  Result:= Self;
end;
{$IFEND}

function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Head(const APath: string; const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
begin
  TPonyCore(FPonyCore).Head(NormalizePath(APath), ACallback);
  Result:= Self;
end;

function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Head(const APath: string; const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
begin
  TPonyCore(FPonyCore).Head(NormalizePath(APath), ACallback);
  Result:= Self;
end;

function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.AddCallback(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
begin
  Result:= Self;
  TPonyCore(FPonyCore).AddCallback(ACallback);
end;

function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.AddCallbacks(const ACallbacks: {$ifdef FPC}specialize{$endif} TArray<TPonyCallback>): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
var
  LCallback: TPonyCallback;
begin
  for LCallback in ACallbacks do
    AddCallback(LCallback);
  Result:= Self;
end;

constructor TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Create;
begin
  { Todo : implementation }
  FPonyCore:= TPonyCore.GetInstance;
end;

function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Prefix(const APrefix: string): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
begin
  Result:= Self;
  FPrefix:= '/' + APrefix.Trim(['/']);
end;

function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Route(const APath: string): {$ifdef FPC}specialize{$endif} IPonyCoreRoute<T>;
begin
  Result:= TPonyCore(FPonyCore).Route(NormalizePath(APath)) {as IPonyCoreRoute<T>};
end;

function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Use(const AMiddleware, ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
begin
  Result:= Self;
  TPonyCore(FPonyCore).Use(NormalizePath('/'), [AMiddleware, ACallback]);
end;

function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Use(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
begin
  Result:= Self;
  TPonyCore(FPonyCore).Use(NormalizePath('/'), ACallback);
end;

function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Use(const ACallbacks: array of TPonyCallback; const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
begin
  Result:= Self;
  TPonyCore(FPonyCore).Use(NormalizePath('/'), ACallbacks);
  TPonyCore(FPonyCore).Use(NormalizePath('/'), [ACallback]);
end;

function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Use(const ACallbacks: array of TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
begin
  Result:= Self;
  TPonyCore(FPonyCore).Use(NormalizePath('/'), ACallbacks);
end;

function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Get(const APath: string; const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
begin
  Result:= Self;
  TPonyCore(FPonyCore).Get(NormalizePath(APath), ACallback);
end;

function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Put(const APath: string; const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
begin
  Result:= Self;
  TPonyCore(FPonyCore).Put(NormalizePath(APath), ACallback);
end;

{$IF (defined(fpc) or (CompilerVersion > 27.0))}
function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Patch(const APath: string; const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
begin
  Result:= Self;
  TPonyCore(FPonyCore).Patch(NormalizePath(APath), ACallback);
end;

function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Delete(const APath: string; const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
begin
  Result:= Self;
  TPonyCore(FPonyCore).Delete(NormalizePath(APath), ACallback);
end;

  {$IFNDEF FPC}
  function TPonyCoreGroup<T>.Patch(const APath: string; const ACallback: TPonyCallbackResponse): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
  begin
    TPonyCore(FPonyCore).Patch(NormalizePath(APath), ACallback);
    Result:= Self;
  end;
  {$IFEND}

  function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Patch(const APath: string; const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
  begin
    TPonyCore(FPonyCore).Patch(NormalizePath(APath), ACallback);
    Result:= Self;
  end;

  function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Patch(const APath: string; const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
  begin
    TPonyCore(FPonyCore).Patch(NormalizePath(APath), ACallback);
    Result:= Self;
  end;

  {$IFNDEF FPC}
  function TPonyCoreGroup<T>.Delete(const APath: string; const ACallback: TPonyCallbackResponse): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
  begin
    TPonyCore(FPonyCore).Delete(NormalizePath(APath), ACallback);
    Result:= Self;
  end;
  {$IFEND}

  function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Delete(const APath: string; const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
  begin
    TPonyCore(FPonyCore).Delete(NormalizePath(APath), ACallback);
    Result:= Self;
  end;

  function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Delete(const APath: string; const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
  begin
    TPonyCore(FPonyCore).Delete(NormalizePath(APath), ACallback);
    Result:= Self;
  end;
{$IFEND}

function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.NormalizePath(const APath: string): string;
begin
  Result:= FPrefix + '/' + APath.Trim(['/']);
end;

function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Head(const APath: string; const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
begin
  Result:= Self;
  TPonyCore(FPonyCore).Head(NormalizePath(APath), ACallback);
end;

function TPonyCoreGroup{$ifndef FPC}<T>{$endif}.Post(const APath: string; const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif} IPonyCoreGroup<T>;
begin
  Result:= Self;
  TPonyCore(FPonyCore).Post(NormalizePath(APath), ACallback);
end;

end.
