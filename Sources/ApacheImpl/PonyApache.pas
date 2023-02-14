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
unit PonyApache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$ifdef FPC}
  {$ifdef UNIX}
    cthreads,
  {$endif}
    fphttp, httpdefs, httpd24, fpApache24, custapache24, PonyProviderAbstract,
    PonyConstants, PonyProc;
  {$else}
  PonyProviderAbstract, System.SysUtils, Web.HTTPD24Impl;
  {$endif}

type
  {$ifdef FPC}generic{$endif} TPonyProvider<T: class> = class({$ifdef FPC}specialize{$endif} TPonyProviderAbstract<T>)
      private
        // Members
        class var FHandlerName: string;
        class var FDefaultModule: {$ifdef FPC}
                                  pmodule
                                  {$else}
                                  Pointer
                                  {$endif};
        {$ifdef FPC}
        class var FApacheApplication: TCustomApacheApplication;
        class var FModuleName: string;
        {$endif}
        // Method's
        class procedure InternalListen; {$ifdef FPC}virtual{$else}static{$endif};
        {$ifdef FPC}
        class function GetDefaultApacheApplication: TCustomApacheApplication;
        class function ApacheApplicationIsNil: Boolean;
        class procedure DoGetModule(Sender: TObject; ARequest: TRequest; var pmoduleClass: TCustomHTTPModuleClass);
        {$endif}
        // Access Method's
        class function GetHandlerName: string; static;
        class procedure SetHandlerName(const AValue: string); static;
        class function GetDefaultModule: {$ifdef FPC}pmodule{$else}Pointer{$endif}; static;
        class procedure SetDefaultModule(const AValue: {$ifdef FPC}pmodule{$else}Pointer{$endif}); static;
        {$ifdef FPC}
        class procedure SetModuleName(const AValue: string); static;
        class function GetModuleName: string; static;
        {$endif}
      public
        // Method's
        class procedure Listen; overload; override;
        class procedure Listen(const ACallback: {$ifdef FPC}specialize{$endif} TProcWT<T>); reintroduce; overload; static;
        // Properties
        class property HandlerName: string read GetHandlerName
                                           write SetHandlerName;
        class property DefaultModule: {$ifdef FPC}pmodule{$else}pointer{$endif} read GetDefaultModule
                                                                                write SetDefaultModule;
        {$ifdef FPC}
        class property ModuleName: string read GetModuleName
                                          write SetModuleName;
        {$endif}
    end;

implementation

uses
  {$ifndef FPC}
  Web.WebBroker, Web.ApacheApp,
  {$IFDEF MSWINDOWS}
  Winapi.ActiveX, System.Win.ComObj,
  {$endif}
  {$endif}
  PonyModules;

{ TPonyProvider implementation }

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.InternalListen;
{$ifdef FPC}
var
  LApacheApplication: TCustomApacheApplication;
{$endif}
begin
  {$ifdef FPC}
  inherited;
  LApacheApplication:= GetDefaultApacheApplication;
  LApacheApplication.ModuleName:= FModuleName;
  LApacheApplication.HandlerName:= FHandlerName;
  LApacheApplication.SetModuleRecord(FDefaultModule^);
  LApacheApplication.AllowDefaultModule:= True;
  LApacheApplication.OnGetModule:= {$ifdef FPC}@{$endif}DoGetModule;
  LApacheApplication.LegacyRouting:= True;
  DoOnListen;
  LApacheApplication.Initialize;
  {$else}
  {$IFDEF MSWINDOWS}
    CoInitFlags:= COINIT_MULTITHREADED;
  {$endif}
    Web.ApacheApp.InitApplication(FDefaultModule, UTF8String(FHandlerName));
    Application.Initialize;
    Application.WebModuleClass:= WebModuleClass;
    DoOnListen;
    Application.Run;
  {$endif}
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetDefaultApacheApplication: TCustomApacheApplication;
begin
  if ApacheApplicationIsNil then
    FApacheApplication:= Application;
  Result:= FApacheApplication;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.ApacheApplicationIsNil: Boolean;
begin
  Result:= FApacheApplication = nil;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.DoGetModule(Sender: TObject; ARequest: TRequest; var pmoduleClass: TCustomHTTPModuleClass);
begin
  pmoduleClass:= TPonyWebModule;
end;

// Private Access Method's

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetHandlerName: string;
begin
  Result:= FHandlerName;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.SetHandlerName(const AValue: string);
begin
  FHandlerName:= AValue;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetDefaultModule: {$ifdef FPC}pmodule{$else}Pointer{$endif};
begin
  Result:= FDefaultModule;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.SetDefaultModule(const AValue: {$ifdef FPC}pmodule{$else}Pointer{$endif});
begin
  FDefaultModule:= AValue;
end;

{$ifdef FPC}
class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetModuleName: string;
begin
  Result:= FModuleName;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.SetModuleName(const AValue: string);
begin
  FModuleName:= AValue;
end;
{$endif}

// Public Method's

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

