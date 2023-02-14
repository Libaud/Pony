{=============================================================================
 Unit : PonyInterfaces
 Framework : Pony
 Author : Frédéric Libaud
 Description : PonyCore abstract for pony Framework
 Features:

 =============================================================================
 History
 -----------------------------------------------------------------------------
 08/02/2022 - Forking code from Horse framework
 =============================================================================}
unit PonyInterfaces;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, PonyTypes, PonyCallback, PonyRouterTreeInterfaces;

type
  IPonyCore = interface
      function AddCallback(const aCallback: TPonyCallback): IPonyCore;
      function AddCallbacks(const aCallbacks: {$ifdef FPC}specialize{$endif} TArray<TPonyCallback>): IPonyCore;
      //function Group: IPonyCoreGroup<TPonyCore>;
      //function Route(const APath: string): IPonyCoreRoute<TPonyCore>;
      function All(const aPath: string; const aCallback: TPonyCallback): IPonyCore; {$ifdef FPC}overload;{$endif}
      function All(const aPath: string; const aCallback: TPonyCallbackRequestResponse): IPonyCore; {$ifdef FPC}overload;{$endif}
      function All(const aPath: string; const aCallback: TPonyCallbackRequest): IPonyCore; {$ifdef FPC}overload;{$endif}
      {$IFNDEF FPC}
      function All(const aPath: string; const ACallback: TPonyCallbackResponse): IPonyCore; {$ifdef FPC}overload;{$endif}
      {$IFEND}

      {$IF (defined(fpc) or (CompilerVersion > 27.0))}
      function Delete(const aPath: string; const aCallback: TPonyCallback): IPonyCore; {$ifdef FPC}overload;{$endif}
      function Delete(const aPath: string; const aCallback: TPonyCallbackRequestResponse): IPonyCore; {$ifdef FPC}overload;{$endif}
      function Delete(const aPath: string; const aCallback: TPonyCallbackRequest): IPonyCore; {$ifdef FPC}overload;{$endif}
      {$IFNDEF FPC}
      function Delete(const aPath: string; const aCallback: TPonyCallbackResponse): IPonyCore; {$ifdef FPC}overload;{$endif}
      {$IFEND}
      {$IFEND}
      function Get(const aPath: string; const ACallback: TPonyCallback): IPonyCore; {$ifdef FPC}overload;{$endif}
      function Get(const aPath: string; const ACallback: TPonyCallbackRequestResponse): IPonyCore; {$ifdef FPC}overload;{$endif}
      function Get(const aPath: string; const ACallback: TPonyCallbackRequest): IPonyCore; {$ifdef FPC}overload;{$endif}
      {$IFNDEF FPC}
      function Get(const aPath: string; const ACallback: TPonyCallbackResponse): TPonyCore; {$ifdef FPC}overload;{$endif}
      {$IFEND}

      function Put(const aPath: string; const aCallback: TPonyCallback): IPonyCore; {$ifdef FPC}overload;{$endif}
      function Put(const aPath: string; const aCallback: TPonyCallbackRequestResponse): IPonyCore; {$ifdef FPC}overload;{$endif}
      function Put(const aPath: string; const aCallback: TPonyCallbackRequest): IPonyCore; {$ifdef FPC}overload;{$endif}
      {$IFNDEF FPC}
      function Put(const aPath: string; const aCallback: TPonyCallbackResponse): TPonyCore; {$ifdef FPC}overload;{$endif}
      {$IFEND}
      {$IF (DEFINED(FPC) or (CompilerVersion > 27.0))}
      function Patch(const aPath: string; const aCallback: TPonyCallback): IPonyCore; {$ifdef FPC}overload;{$endif}
      function Patch(const aPath: string; const aCallback: TPonyCallbackRequestResponse): IPonyCore; {$ifdef FPC}overload;{$endif}
      function Patch(const aPath: string; const aCallback: TPonyCallbackRequest): IPonyCore; {$ifdef FPC}overload;{$endif}
      {$IFNDEF FPC}
      function Patch(const aPath: string; const aCallback: TPonyCallbackResponse): IPonyCore; {$ifdef FPC}overload;{$endif}
      {$IFEND}
      {$IFEND}
      function Head(const aPath: string; const aCallback: TPonyCallback): IPonyCore; {$ifdef FPC}overload;{$endif}
      function Head(const aPath: string; const aCallback: TPonyCallbackRequestResponse): IPonyCore; {$ifdef FPC}overload;{$endif}
      function Head(const aPath: string; const aCallback: TPonyCallbackRequest): IPonyCore; {$ifdef FPC}overload;{$endif}
      {$IFNDEF FPC}
      function Head(const aPath: string; const aCallback: TPonyCallbackResponse): IPonyCore; {$ifdef FPC}overload;{$endif}
      {$IFEND}
      function Post(const aPath: string; const aCallback: TPonyCallback): IPonyCore; {$ifdef FPC}overload;{$endif}
      function Post(const aPath: string; const aCallback: TPonyCallbackRequestResponse): IPonyCore; {$ifdef FPC}overload;{$endif}
      function Post(const aPath: string; const aCallback: TPonyCallbackRequest): IPonyCore; {$ifdef FPC}overload;{$endif}
      {$IFNDEF FPC}
      function Post(const aPath: string; const aCallback: TPonyCallbackResponse): IPonyCore; {$ifdef FPC}overload;{$endif}
      {$IFEND}

      function Use(const aPath: string; const aCallback: TPonyCallback): IPonyCore; {$ifdef FPC}overload;{$endif}
      function Use(const aCallback: TPonyCallback): IPonyCore; {$ifdef FPC}overload;{$endif}
      function Use(const APath: string; const aCallbacks: array of TPonyCallback): IPonyCore; {$ifdef FPC}overload;{$endif}
      function Use(const aCallbacks: array of TPonyCallback): IPonyCore; {$ifdef FPC}overload;{$endif}
      // Access Method's
      function GetPort: TPort;
      procedure setPort(const aPort: TPort);
      function GetRoutes: IPonyRouterTree;
      procedure SetRoutes(const aRoutes: IPonyRouterTree);
      function GetDefaultPony: IPonyCore;
      procedure SetDefaultPony(aDefaultPony: IPonyCore);
      // properties
      property Port: TPort read GetPort
                           write SetPort;
      property Routes: IPonyRouterTree read GetRoutes
                                       write SetRoutes;
      property DefaultPony: IPonyCore read GetDefaultPony
                                      write SetDefaultPony;
    end;

var
  oPonyCore: IPonyCore;

implementation

end.

