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
unit PonyCoreGroupContract;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  PonyCoreRouteContract{, PonyCoreRouterTree}, PonyCallback;

type
  {$ifdef FPC}generic{$endif} IPonyCoreGroup<T: class> = interface
    ['{5EB734D6-6944-473E-9C79-506647E2F5E8}']
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

end.
