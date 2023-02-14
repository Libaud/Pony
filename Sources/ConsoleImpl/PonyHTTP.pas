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
unit PonyHTTP;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$ifdef FPC}
  httpdefs, fpHTTP, fphttpapp, PonyProviderAbstract, PonyConstants, PonyProc
  {$else}
  PonyProviderAbstract, PonyConstants, PonyProviderIOHandleSSLContract,
  PonyProviderIOHandleSSL, IdHTTPWebBrokerBridge, IdSSLOpenSSL, IdSSL, IdContext,
  System.Classes, System.SyncObjs, System.SysUtils
  {$endif};

type
  {$ifdef FPC}generic{$endif} TPonyProvider<T: class> = class({$ifdef FPC}specialize{$endif} TPonyProviderAbstract<T>)
      private
        // Members
        class var FPort: Integer;
        class var FHost: string;
        class var FRunning: Boolean;
        class var FListenQueue: Integer;
        {$ifdef FPC}
        class var FHTTPApplication: THTTPApplication;
        {$else}
        class var FEvent: TEvent;
        class var FMaxConnections: Integer;
        class var FKeepConnectionAlive: Boolean;
        class var FIdHTTPWebBrokerBridge: TIdHTTPWebBrokerBridge;
        class var FPonyProviderIOHandleSSL: IPonyProviderIOHandleSSL;
        {$endif}
        // Method's
        class procedure InternalListen; virtual;
        {$ifdef FPC}
        class function HTTPApplicationIsNil: Boolean;
        class procedure DoGetModule(Sender: TObject; ARequest: TRequest; var ModuleClass: TCustomHTTPModuleClass);
        {$else}
        class function HTTPWebBrokerIsNil: Boolean;
        class procedure OnAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
        class procedure OnQuerySSLPort(APort: Word; var VUseSSL: Boolean);
        class procedure InternalStopListen; virtual;
        class procedure InitServerIOHandlerSSLOpenSSL(const AIdHTTPWebBrokerBridge: TIdHTTPWebBrokerBridge; const APonyProviderIOHandleSSL: IPonyProviderIOHandleSSL);
        {$endif}
        // Access Method's
        class function GetDefaultPort: Integer; static;
        class function GetDefaultHost: string; static;
        class function GetPort: Integer; static;
        class procedure SetPort(const AValue: Integer); static;
        class function GetHost: string; static;
        class procedure SetHost(const AValue: string); static;
        class function GetListenQueue: Integer; static;
        class procedure SetListenQueue(const AValue: Integer); static;
        {$ifdef FPC}
        class function GetDefaultHTTPApplication: THTTPApplication;
        {$else}
        class procedure SetMaxConnections(const AValue: Integer); static;
        class procedure SetIOHandleSSL(const AValue: IPonyProviderIOHandleSSL); static;
        class procedure SetKeepConnectionAlive(const AValue: Boolean); static;
        class function GetMaxConnections: Integer; static;
        class function GetDefaultPort: Integer; static;
        class function GetDefaultHost: string; static;
        class function GetIOHandleSSL: IPonyProviderIOHandleSSL; static;
        class function GetKeepConnectionAlive: Boolean; static;
        class function GetDefaultHTTPWebBroker: TIdHTTPWebBrokerBridge;
        class function GetDefaultPonyProviderIOHandleSSL: IPonyProviderIOHandleSSL;
        class function GetDefaultEvent: TEvent;
        {$endif}
      public
        class function IsRunning: Boolean;
        class procedure Listen; overload; override;
        {$ifdef FPC}
        class procedure Listen(const APort: Integer;
                               const AHost: string = '0.0.0.0';
                               const ACallback: {$ifdef FPC}specialize{$endif} TProcWT<T> = nil); reintroduce; overload; static;
        class procedure Listen(const APort: Integer;
                               const ACallback: {$ifdef FPC}specialize{$endif} TProcWT<T>); reintroduce; overload; static;
        class procedure Listen(const AHost: string;
                               const ACallback: {$ifdef FPC}specialize{$endif} TProcWT<T> = nil); reintroduce; overload; static;
        class procedure Listen(const ACallback: {$ifdef FPC}specialize{$endif} TProcWT<T>); reintroduce; overload; static;
        {$else}
        class procedure StopListen; override;
        class procedure Listen(const APort: Integer;
                               const AHost: string = '0.0.0.0';
                               const ACallbackListen: {$ifdef FPC}specialize{$endif} TProcWT<T> = nil;
                               const ACallbackStopListen: TProcWT<T> = nil); reintroduce; overload; static;
        class procedure Listen(const APort: Integer;
                               const ACallbackListen: {$ifdef FPC}specialize{$endif} TProcWT<T>;
                               const ACallbackStopListen: {$ifdef FPC}specialize{$endif} TProcWT<T> = nil); reintroduce; overload; static;
        class procedure Listen(const AHost: string;
                               const ACallbackListen: {$ifdef FPC}specialize{$endif} TProcWT<T> = nil;
                               const ACallbackStopListen: {$ifdef FPC}specialize{$endif} TProcWT<T> = nil); reintroduce; overload; static;
        class procedure Listen(const ACallbackListen: {$ifdef FPC}specialize{$endif} TProcWT<T>;
                               const ACallbackStopListen: {$ifdef FPC}specialize{$endif} TProcWT<T> = nil); reintroduce; overload; static;
        class destructor UnInitialize;
        {$endif}
        // Properties
        class property Host: string read GetHost
                                    write SetHost;
        class property Port: Integer read GetPort
                                     write SetPort;
        class property ListenQueue: Integer read GetListenQueue
                                            write SetListenQueue;
        {$ifndef FPC}
        class property MaxConnections: Integer read GetMaxConnections
                                               write SetMaxConnections;
        class property KeepConnectionAlive: Boolean read GetKeepConnectionAlive
                                                    write SetKeepConnectionAlive;
        class property IOHandleSSL: IPonyProviderIOHandleSSL read GetIOHandleSSL
                                                             write SetIOHandleSSL;
        {$endif}
    end;

implementation


uses
  {$ifndef FPC}
  Web.WebReq, IdCustomTCPServer,
  {$endif}
  PonyModules;

{ TPonyProvider implementation }

// Constructor's and destructor's

// Private Method's

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.InternalListen;
var
  {$ifdef FPC}
  LHTTPApplication: THTTPApplication;
  {$else}
  LAttach: string;
  LIdHTTPWebBrokerBridge: TIdHTTPWebBrokerBridge;
  {$endif}
begin
  {$ifdef FPC}
  inherited;
  if FPort <= 0 then
    FPort:= GetDefaultPort;
  if FHost.IsEmpty then
    FHost:= GetDefaultHost;
  if FListenQueue = 0 then
    FListenQueue:= 15;
  LHTTPApplication:= GetDefaultHTTPApplication;
  LHTTPApplication.AllowDefaultModule:= True;
  LHTTPApplication.OnGetModule:= @DoGetModule;
  LHTTPApplication.Threaded:= True;
  LHTTPApplication.QueueSize:= FListenQueue;
  LHTTPApplication.Port:= FPort;
  LHTTPApplication.LegacyRouting:= True;
  LHTTPApplication.Address:= FHost;
  LHTTPApplication.Initialize;
  FRunning:= True;
  DoOnListen;
  LHTTPApplication.Run;
  {$else}
  inherited;
  if FPort <= 0 then
    FPort:= GetDefaultPort;

  if FHost.IsEmpty then
    FHost:= GetDefaultHost;

  LIdHTTPWebBrokerBridge:= GetDefaultHTTPWebBroker;
  WebRequestHandler.WebModuleClass:= WebModuleClass;
  try
    if FMaxConnections > 0 then
    begin
      WebRequestHandler.MaxConnections:= FMaxConnections;
      GetDefaultHTTPWebBroker.MaxConnections:= FMaxConnections;
    end;

    if FListenQueue = 0 then
      FListenQueue:= IdListenQueueDefault;

    if FPonyProviderIOHandleSSL <> nil then
      InitServerIOHandlerSSLOpenSSL(LIdHTTPWebBrokerBridge, GetDefaultPonyProviderIOHandleSSL);
    LIdHTTPWebBrokerBridge.ListenQueue:= FListenQueue;

    LIdHTTPWebBrokerBridge.Bindings.Clear;
    if FHost <> GetDefaultHost then
    begin
      LIdHTTPWebBrokerBridge.Bindings.Add;
      LIdHTTPWebBrokerBridge.Bindings.Items[0].IP:= FHost;
      LIdHTTPWebBrokerBridge.Bindings.Items[0].Port:= FPort;
    end;

    LIdHTTPWebBrokerBridge.KeepAlive:= FKeepConnectionAlive;
    LIdHTTPWebBrokerBridge.DefaultPort:= FPort;
    LIdHTTPWebBrokerBridge.Active:= True;
    LIdHTTPWebBrokerBridge.StartListening;
    FRunning:= True;
    DoOnListen;

    if IsConsole then
    begin
      while FRunning do
        GetDefaultEvent.WaitFor();
    end
  except
    on E: Exception do
    begin
      if IsConsole then
      begin
        Writeln(E.ClassName, ': ', E.Message);
        Read(LAttach);
      end
      else
      {$IF CompilerVersion >= 32.0}
        raise AcquireExceptionObject;
      {$else}
        raise;
      {$endif}
    end;
  end;
  {$endif}
end;

// FPC Method's

{$ifdef FPC}

class function TPonyProvider{$ifndef FPC}<T>{$endif}.HTTPApplicationIsNil: Boolean;
begin
  Result:= FHTTPApplication = nil;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.DoGetModule(Sender: TObject; ARequest: TRequest; var ModuleClass: TCustomHTTPModuleClass);
begin
  ModuleClass:=  TPonyWebModule;
end;

{$endif}

// Delphi Method's

{$ifndef FPC}

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.InternalStopListen;
begin
  if not HTTPWebBrokerIsNil then
  begin
    GetDefaultHTTPWebBroker.StopListening;
    GetDefaultHTTPWebBroker.Active:= False;
    DoOnStopListen;
    FRunning:= False;
    if FEvent <> nil then
      GetDefaultEvent.SetEvent;
  end
  else
    raise Exception.Create('Pony not listen');
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.InitServerIOHandlerSSLOpenSSL(const AIdHTTPWebBrokerBridge: TIdHTTPWebBrokerBridge; const APonyProviderIOHandleSSL: IPonyProviderIOHandleSSL);
var
  LIOHandleSSL: TIdServerIOHandlerSSLOpenSSL;
begin
  LIOHandleSSL:= TIdServerIOHandlerSSLOpenSSL.Create(AIdHTTPWebBrokerBridge);
  LIOHandleSSL.SSLOptions.CertFile:= APonyProviderIOHandleSSL.CertFile;
  LIOHandleSSL.SSLOptions.RootCertFile:= APonyProviderIOHandleSSL.RootCertFile;
  LIOHandleSSL.SSLOptions.KeyFile:= APonyProviderIOHandleSSL.KeyFile;
  LIOHandleSSL.SSLOptions.Method:= APonyProviderIOHandleSSL.Method;
  LIOHandleSSL.SSLOptions.SSLVersions:= APonyProviderIOHandleSSL.SSLVersions;
  LIOHandleSSL.SSLOptions.CipherList:= APonyProviderIOHandleSSL.CipherList;
  LIOHandleSSL.SSLOptions.DHParamsFile:= APonyProviderIOHandleSSL.DHParamsFile;
  LIOHandleSSL.OnGetPassword:= APonyProviderIOHandleSSL.OnGetPassword;
  AIdHTTPWebBrokerBridge.IOHandler:= LIOHandleSSL;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.HTTPWebBrokerIsNil: Boolean;
begin
  Result:= FIdHTTPWebBrokerBridge = nil;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.OnQuerySSLPort(APort: Word; var VUseSSL: Boolean);
begin
  VUseSSL:= (FPonyProviderIOHandleSSL <> nil) and (FPonyProviderIOHandleSSL.Active);
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.OnAuthentication(AContext: TIdContext;
                                                                       const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
begin
  VHandled:= True;
end;

{$endif}

// Private Access Method's

// Common Private Access Method's

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetDefaultHost: string;
begin
  Result:= DEFAULT_HOST;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetDefaultPort: Integer;
begin
  Result:= DEFAULT_PORT;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetPort: Integer;
begin
  Result:= FPort;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.SetPort(const AValue: Integer);
begin
  FPort:= AValue;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetHost: string;
begin
  Result:= FHost;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.SetHost(const AValue: string);
begin
  FHost:= AValue.Trim;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetListenQueue: Integer;
begin
  Result:= FListenQueue;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.SetListenQueue(const AValue: Integer);
begin
  FListenQueue:= AValue;
end;

// FPC Private Access Method's

{$ifdef FPC}

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetDefaultHTTPApplication: THTTPApplication;
begin
  if HTTPApplicationIsNil then
    FHTTPApplication:= Application;
  Result:= FHTTPApplication;
end;

{$endif}

// Delphi Private Access Method's

{$ifndef FPC}

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.SetKeepConnectionAlive(const AValue: Boolean);
begin
  FKeepConnectionAlive:= AValue;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.SetIOHandleSSL(const AValue: IPonyProviderIOHandleSSL);
begin
  FPonyProviderIOHandleSSL:= AValue;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.SetMaxConnections(const AValue: Integer);
begin
  FMaxConnections:= AValue;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetDefaultHTTPWebBroker: TIdHTTPWebBrokerBridge;
begin
  if HTTPWebBrokerIsNil then
  begin
    FIdHTTPWebBrokerBridge:= TIdHTTPWebBrokerBridge.Create(nil);
    FIdHTTPWebBrokerBridge.OnParseAuthentication:= OnAuthentication;
    FIdHTTPWebBrokerBridge.OnQuerySSLPort:= OnQuerySSLPort;
  end;
  Result:= FIdHTTPWebBrokerBridge;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetDefaultPonyProviderIOHandleSSL: IPonyProviderIOHandleSSL;
begin
  if FPonyProviderIOHandleSSL = nil then
    FPonyProviderIOHandleSSL:= TPonyProviderIOHandleSSL.New;
  Result:= FPonyProviderIOHandleSSL;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetKeepConnectionAlive: Boolean;
begin
  Result:= FKeepConnectionAlive;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetIOHandleSSL: IPonyProviderIOHandleSSL;
begin
  Result:= GetDefaultPonyProviderIOHandleSSL;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetMaxConnections: Integer;
begin
  Result:= FMaxConnections;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetDefaultEvent: TEvent;
begin
  if FEvent = nil then
    FEvent:= TEvent.Create;
  Result:= FEvent;
end;

{$endif}

// Public Method's

// Common Public Method's

class function TPonyProvider{$ifndef FPC}<T>{$endif}.IsRunning: Boolean;
begin
  Result:= FRunning;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen;
begin
  InternalListen;
end;

// FPC Public Method's

{$ifdef FPC}

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen(const APort: Integer;
                                                             const AHost: string;
                                                             const ACallback: {$ifdef FPC}specialize{$endif} TProcWT<T>);
begin
  SetPort(APort);
  SetHost(AHost);
  SetOnListen(ACallback);
  InternalListen;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen(const AHost: string;
                                                             const ACallback: {$ifdef FPC}specialize{$endif} TProcWT<T>);
begin
  Listen(FPort, AHost, ACallback);
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen(const ACallback: {$ifdef FPC}specialize{$endif} TProcWT<T>);
begin
  Listen(FPort, FHost, ACallback);
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen(const APort: Integer;
                                                             const ACallback: {$ifdef FPC}specialize{$endif} TProcWT<T>);
begin
  Listen(APort, FHost, ACallback);
end;

{$endif}

// Delphi Public Method's

{$ifndef FPC}

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.StopListen;
begin
  InternalStopListen;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen(const APort: Integer;
                                                             const AHost: string;
                                                             const ACallbackListen, ACallbackStopListen: {$ifdef FPC}specialize{$endif} TProcWT<T>);
begin
  SetPort(APort);
  SetHost(AHost);
  SetOnListen(ACallbackListen);
  SetOnStopListen(ACallbackStopListen);
  InternalListen;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen(const AHost: string;
                                                             const ACallbackListen, ACallbackStopListen: {$ifdef FPC}specialize{$endif} TProcWT<T>);
begin
  Listen(FPort, AHost, ACallbackListen, ACallbackStopListen);
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen(const ACallbackListen, ACallbackStopListen: {$ifdef FPC}specialize{$endif} TProcWT<T>);
begin
  Listen(FPort, FHost, ACallbackListen, ACallbackStopListen);
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen(const APort: Integer;
                                                             const ACallbackListen, ACallbackStopListen: {$ifdef FPC}specialize{$endif} TProcWT<T>);
begin
  Listen(APort, FHost, ACallbackListen, ACallbackStopListen);
end;

class destructor TPonyProvider{$ifndef FPC}<T>{$endif}.UnInitialize;
begin
  FreeAndNil(FIdHTTPWebBrokerBridge);
  if FEvent <> nil then
    FreeAndNil(FEvent);
end;

{$endif}

{ End of TPonyProvider implementation }

end.

