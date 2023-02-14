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
unit PonyRouterTreeInterfaces;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, PonyCommons, PonyCallback, PonyRequestInterfaces, PonyResponseInterfaces;

type
  IPonyRouterTree = interface
    function CreateRouter(const APath: string): IPonyRouterTree;
    function GetPrefix: string;
    procedure Prefix(const APrefix: string);
    procedure RegisterRoute(const AHTTPType: TMethodType; const APath: string; const ACallback: TPonyObjCallback);
    procedure RegisterRoute(const AHTTPType: TMethodType; const APath: string; const ACallback: TPonyCallback);
    procedure RegisterMiddleware(const APath: string; const AMiddleware: TPonyCallback); overload;
    procedure RegisterMiddleware(const AMiddleware: TPonyCallback); overload;
    function Execute(const ARequest: IPonyRequest; const AResponse: IPonyResponse): Boolean;
  end;

implementation

end.

