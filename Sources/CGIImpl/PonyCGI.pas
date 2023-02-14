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
unit PonyCGI;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,
  {$ifndef FPC}

  {$else}
  fpCGI, fphttp, httpdefs, PonyProc,
  {$endif}
  PonyProviderAbstract;


type
  { generic TPonyProvider implementation of TPonyProviderAbstract }

  {$ifdef FPC}generic{$endif}TPonyProvider<T: class> = class({$ifdef FPC}specialize{$endif} TPonyProviderAbstract<T>)
  private
    // Members
    {$ifdef FPC}
    class var FCGIApplication: TCGIApplication;
    {$endif}
    // Method's
    {$ifdef FPC}
    class function GetDefaultCGIApplication: TCGIApplication;
    class function CGIApplicationIsNil: Boolean;
    class procedure DoGetModule(Sender: TObject; ARequest: TRequest; var ModuleClass: TCustomHTTPModuleClass);
    {$else}
    {$endif}
    class procedure InternalListen; {$ifdef FPC}virtual{$else}static{$endif};
  public
    {$ifdef FPC}
    constructor Create; reintroduce; overload;
    {$endif}
    class procedure Listen(const ACallback: {$ifdef FPC}specialize{$endif} TProcWT<T>); reintroduce; overload; static;
    class procedure Listen; overload; override;
  end;

  { generic TPonyProvider }

{$ifdef FPC}
var
  ShowCleanUpErrors: Boolean = False;
{$endif}

implementation

uses
  {$ifdef FPC}
  PonyModules
  {$else}
  Web.WebBroker, Web.CGIApp
  {$endif};

{$ifdef FPC}
class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetDefaultCGIApplication: TCGIApplication;
begin
  if CGIApplicationIsNil then
    FCGIApplication:= Application;
  Result:= FCGIApplication;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.CGIApplicationIsNil: Boolean;
begin
  Result:= FCGIApplication = nil;
end;

constructor TPonyProvider{$ifndef FPC}<T>{$endif}.Create;
begin
  inherited Create;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.DoGetModule(Sender: TObject; ARequest: TRequest; var ModuleClass: TCustomHTTPModuleClass);
begin
  ModuleClass:= TPonyWebModule;
end;
{$endif}

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.InternalListen;
{$ifdef FPC}
var
  LCGIApplication: TCGIApplication;
{$endif}
begin
  {$ifdef FPC}
  inherited;
  LCGIApplication:= GetDefaultCGIApplication;
  LCGIApplication.AllowDefaultModule:= True;
  LCGIApplication.OnGetModule:= @DoGetModule;
  LCGIApplication.LegacyRouting:= True;
  LCGIApplication.Initialize;
  DoOnListen;
  LCGIApplication.Run;
  {$else}
  Application.Initialize;
  Application.WebModuleClass:= WebModuleClass;
  DoOnListen;
  Application.Run;
  {$endif}
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen;
begin
  {$ifndef FPC}
  inherited;
  {$endif}
  InternalListen;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen(const ACallback: {$ifdef FPC}specialize{$endif} TProcWT<T>);
begin
  {$ifndef FPC}
  inherited;
  {$endif}
  SetOnListen(ACallback);
  InternalListen;
end;

end.

