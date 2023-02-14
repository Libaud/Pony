{=============================================================================
 Unit : PonyFastCGI
 Framework : Pony
 Author : Frédéric Libaud
 Description : Pony provider implementation for Pony Framework
 Features:

 =============================================================================
 History
 -----------------------------------------------------------------------------
 08/02/2022 - Forking code from Horse framework
 =============================================================================}
unit PonyFastCGI;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  SysUtils, Classes, fpFCGI, httpdefs, fpHTTP, PonyProviderAbstract, PonyConstants, PonyProc;

type
  {$ifdef FPC}generic{$endif} TPonyProvider<T: class> = class({$ifdef FPC}specialize{$endif} TPonyProviderAbstract<T>)
  private
    // Members
    class var FPort: Integer;
    class var FHost: string;
    class var FRunning: Boolean;
    class var FFastCGIApplication: TFCGIApplication;
    // Method's
    class function GetDefaultFastCGIApplication: TFCGIApplication;
    class function FastCGIApplicationIsNil: Boolean;
    class procedure SetPort(const AValue: Integer); static;
    class procedure SetHost(const AValue: string); static;
    class function GetPort: Integer; static;
    class function GetDefaultPort: Integer; static;
    class function GetDefaultHost: string; static;
    class function GetHost: string; static;
    class procedure InternalListen; virtual;
    class procedure DoGetModule(Sender: TObject; aRequest: TRequest; var ModuleClass: TCustomHTTPModuleClass);
  public
    // Method's
    class procedure Listen; overload; override;
    class procedure Listen(const APort: Integer; const AHost: string = '0.0.0.0'; const ACallback: {$ifdef FPC}specialize{$endif} TProcWT<T> = nil); reintroduce; overload; static;
    class procedure Listen(const APort: Integer; const ACallback: {$ifdef FPC}specialize{$endif} TProcWT<T>); reintroduce; overload; static;
    class procedure Listen(const AHost: string; const ACallback: {$ifdef FPC}specialize{$endif} TProcWT<T> = nil); reintroduce; overload; static;
    class procedure Listen(const ACallback: {$ifdef FPC}specialize{$endif} TProcWT<T>); reintroduce; overload; static;
    // Properties
    class property Host: string read GetHost write SetHost;
    class property Port: Integer read GetPort write SetPort;
  end;

implementation

uses
  PonyModules;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetDefaultFastCGIApplication: TFCGIApplication;
begin
  if FastCGIApplicationIsNil then
    FFastCGIApplication:= Application;
  Result:= FFastCGIApplication;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.FastCGIApplicationIsNil: Boolean;
begin
  Result:= FFastCGIApplication = nil;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetDefaultHost: string;
begin
  Result:= DEFAULT_HOST;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetDefaultPort: Integer;
begin
  Result:= -1;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetHost: string;
begin
  Result:= FHost;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetPort: Integer;
begin
  Result:= FPort;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.InternalListen;
var
  LFastCGIApplication: TFCGIApplication;
begin
  inherited;
  if FHost.IsEmpty then
    FHost:= GetDefaultHost;
  LFastCGIApplication:= GetDefaultFastCGIApplication;
  LFastCGIApplication.AllowDefaultModule:= True;
  LFastCGIApplication.OnGetModule:= {$ifdef FPC}@{$endif}DoGetModule;
  LFastCGIApplication.Port:= FPort;
  LFastCGIApplication.LegacyRouting:= True;
  LFastCGIApplication.Address:= FHost;
  LFastCGIApplication.Initialize;
  FRunning:= True;
  DoOnListen;
  LFastCGIApplication.Run;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.DoGetModule(Sender: TObject; ARequest: TRequest; var ModuleClass: TCustomHTTPModuleClass);
begin
  ModuleClass:= TPonyWebModule;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen;
begin
  InternalListen;;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen(const APort: Integer; const AHost: string; const ACallback: {$ifdef FPC}specialize{$endif} TProcWT<T>);
begin
  SetPort(APort);
  SetHost(AHost);
  SetOnListen(ACallback);
  InternalListen;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen(const AHost: string; const ACallback: {$ifdef FPC}specialize{$endif} TProcWT<T>);
begin
  Listen(FPort, AHost, ACallback);
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen(const ACallback: {$ifdef FPC}specialize{$endif} TProcWT<T>);
begin
  Listen(FPort, FHost, ACallback);
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen(const APort: Integer; const ACallback: {$ifdef FPC}specialize{$endif} TProcWT<T>);
begin
  Listen(APort, FHost, ACallback);
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.SetHost(const AValue: string);
begin
  FHost:= AValue;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.SetPort(const AValue: Integer);
begin
  FPort:= AValue;
end;

end.
