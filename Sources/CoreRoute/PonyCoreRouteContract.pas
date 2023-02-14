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
unit PonyCoreRouteContract;

//{$ifdef FPC}
//  {$MODE DELPHI}{$H+}
//{$endif}
{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  {PonyCoreRouterTree,} PonyCallback;

type
  {$ifdef FPC}generic{$endif} IPonyCoreRoute<T: class> = interface
    ['{8D593D98-44B3-4FD2-A21B-BA29F784B3AA}']
    function AddCallback(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>;
    function AddCallbacks(const ACallbacks: {$ifdef FPC}specialize{$endif}TArray<TPonyCallback>): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>;

    function All(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
    function All(const AMiddleware, ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
    function All(const ACallbacks: array of TPonyCallback): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
    function All(const ACallbacks: array of TPonyCallback; const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;

    function Get(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
    function Get(const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
    function Get(const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
    {$IFNDEF FPC}
    function Get(const ACallback: TPonyCallbackResponse): IPonyCoreRoute<T>; overload;
    {$IFEND}

    function Put(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
    function Put(const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
    function Put(const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
    {$IFNDEF FPC}
    function Put(const ACallback: TPonyCallbackResponse): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
    {$IFEND}

    function Head(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
    function Head(const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
    function Head(const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
    {$IFNDEF FPC}
    function Head(const ACallback: TPonyCallbackResponse): IPonyCoreRoute<T>; overload;
    {$IFEND}

    function Post(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
    function Post(const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
    function Post(const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
    {$IFNDEF FPC}
    function Post(const ACallback: TPonyCallbackResponse): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
    {$IFEND}

    {$IF (DEFINED(FPC) OR (CompilerVersion > 27.0))}
    function Patch(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
    function Delete(const ACallback: TPonyCallback): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;

      function Patch(const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
      function Patch(const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
      {$IFNDEF FPC}
      function Patch(const ACallback: TPonyCallbackResponse): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
      {$IFEND}

      function Delete(const ACallback: TPonyCallbackRequestResponse): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
      function Delete(const ACallback: TPonyCallbackRequest): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
      {$IFNDEF FPC}
      function Delete(const ACallback: TPonyCallbackResponse): {$ifdef FPC}specialize{$endif}IPonyCoreRoute<T>; overload;
      {$IFEND}
    {$IFEND}

    function &End: T;
  end;

implementation

end.
