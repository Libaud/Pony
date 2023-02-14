unit PonySSL;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, SyncObjs,
  {$ifdef FPC}
  {$else}
  IdHTTPWebBrokerBridge, IdSSLOpenSSL, IdContext,
  System.SyncObjs,
  {$endif}
  IdHTTPWebBrokerBridge, IdSSLOpenSSL, IdContext,
  PonySSLIOHandler, PonySSLIOHandlerContract, PonyProviderAbstract, PonyConstants;

type
  {$ifdef FPC}generic{$endif} TPonyProvider<T: class> = class({$ifdef FPC}specialize{$endif} TPonyProviderAbstract<T>)
  private
    // Members
    class var FPort: Integer;
    class var FHost: string;
    class var FRunning: Boolean;
    class var FEvent: TEvent;
    class var FMaxConnections: Integer;
    class var FListenQueue: Integer;
    class var FKeepConnectionAlive: Boolean;
    class var FIdHTTPWebBrokerBridge: TIdHTTPWebBrokerBridge;
    class var FHorseProviderIOHandleSSL: IPonyProviderIOHandleSSL;
    // Method's
    class function HTTPWebBrokerIsNil: Boolean;
    class procedure OnAuthentication(AContext: TIdContext;
                                     const AAuthType, AAuthData: string;
                                     var VUsername, VPassword: string;
                                     var VHandled: Boolean);
    class procedure OnQuerySSLPort(APort: Word; var VUseSSL: Boolean);
    class procedure InternalListen; virtual;
    class procedure InternalStopListen; virtual;
    class procedure InitServerIOHandlerSSLOpenSSL(const AIdHTTPWebBrokerBridge: TIdHTTPWebBrokerBridge;
                                                  const AHorseProviderIOHandleSSL: IPonyProviderIOHandleSSL);
    // Access Method's
    class function GetDefaultHTTPWebBroker: TIdHTTPWebBrokerBridge;
    class function GetDefaultHorseProviderIOHandleSSL: IPonyProviderIOHandleSSL;
    class function GetDefaultEvent: TEvent;
    class procedure SetListenQueue(const AValue: Integer); static;
    class procedure SetMaxConnections(const AValue: Integer); static;
    class procedure SetPort(const AValue: Integer); static;
    class procedure SetIOHandleSSL(const AValue: IPonyProviderIOHandleSSL); static;
    class procedure SetHost(const AValue: string); static;
    class procedure SetKeepConnectionAlive(const AValue: Boolean); static;
    class function GetKeepConnectionAlive: Boolean; static;
    class function GetListenQueue: Integer; static;
    class function GetMaxConnections: Integer; static;
    class function GetPort: Integer; static;
    class function GetDefaultPort: Integer; static;
    class function GetDefaultHost: string; static;
    class function GetIOHandleSSL: IPonyProviderIOHandleSSL; static;
    class function GetHost: string; static;
  public
    class destructor UnInitialize;
    // Method's
    class procedure StopListen; override;
    class procedure Listen; overload; override;
    class procedure Listen(const APort: Integer;
                           const AHost: string = '0.0.0.0';
                           const ACallbackListen: {$ifdef FPC}specialize{$endif} TProc<T> = nil;
                           const ACallbackStopListen: {$ifdef FPC}specialize{$endif} TProc<T> = nil); reintroduce; overload; static;
    class procedure Listen(const APort: Integer;
                           const ACallbackListen: {$ifdef FPC}specialize{$endif} TProc<T>;
                           const ACallbackStopListen: {$ifdef FPC}specialize{$endif} TProc<T> = nil); reintroduce; overload; static;
    class procedure Listen(const AHost: string;
                           const ACallbackListen: {$ifdef FPC}specialize{$endif} TProc<T> = nil;
                           const ACallbackStopListen: {$ifdef FPC}specialize{$endif} TProc<T> = nil); reintroduce; overload; static;
    class procedure Listen(const ACallbackListen: {$ifdef FPC}specialize{$endif} TProc<T>;
                           const ACallbackStopListen: {$ifdef FPC}specialize{$endif} TProc<T> = nil); reintroduce; overload; static;
    class function IsRunning: Boolean;
    // Properties
    class property Host: string read GetHost
                                write SetHost;
    class property Port: Integer read GetPort
                                 write SetPort;
    class property MaxConnections: Integer read GetMaxConnections
                                           write SetMaxConnections;
    class property ListenQueue: Integer read GetListenQueue
                                        write SetListenQueue;
    class property KeepConnectionAlive: Boolean read GetKeepConnectionAlive
                                                write SetKeepConnectionAlive;
    class property IOHandleSSL: IPonyProviderIOHandleSSL read GetIOHandleSSL
                                                          write SetIOHandleSSL;
  end;

implementation

uses
  Web.WebReq, Horse.WebModule, IdCustomTCPServer;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.IsRunning: Boolean;
begin
  Result := FRunning;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetDefaultHTTPWebBroker: TIdHTTPWebBrokerBridge;
begin
  if HTTPWebBrokerIsNil then
  begin
    FIdHTTPWebBrokerBridge := TIdHTTPWebBrokerBridge.Create(nil);
    FIdHTTPWebBrokerBridge.OnParseAuthentication := OnAuthentication;
    FIdHTTPWebBrokerBridge.OnQuerySSLPort := OnQuerySSLPort;
  end;
  Result := FIdHTTPWebBrokerBridge;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetKeepConnectionAlive: Boolean;
begin
  Result := FKeepConnectionAlive;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.SetKeepConnectionAlive(const AValue: Boolean);
begin
  FKeepConnectionAlive := AValue;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.HTTPWebBrokerIsNil: Boolean;
begin
  Result := FIdHTTPWebBrokerBridge = nil;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.OnQuerySSLPort(APort: Word; var VUseSSL: Boolean);
begin
  VUseSSL := (FHorseProviderIOHandleSSL <> nil) and (FHorseProviderIOHandleSSL.Active);
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetDefaultEvent: TEvent;
begin
  if FEvent = nil then
    FEvent := TEvent.Create;
  Result := FEvent;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetDefaultHorseProviderIOHandleSSL: IPonyProviderIOHandleSSL;
begin
  if FHorseProviderIOHandleSSL = nil then
    FHorseProviderIOHandleSSL := THorseProviderIOHandleSSL.New;
  Result := FHorseProviderIOHandleSSL;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetDefaultHost: string;
begin
  Result := DEFAULT_HOST;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetDefaultPort: Integer;
begin
  Result := DEFAULT_PORT;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetHost: string;
begin
  Result := FHost;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetIOHandleSSL: IPonyProviderIOHandleSSL;
begin
  Result := GetDefaultHorseProviderIOHandleSSL;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetListenQueue: Integer;
begin
  Result := FListenQueue;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetMaxConnections: Integer;
begin
  Result := FMaxConnections;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetPort: Integer;
begin
  Result := FPort;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.InitServerIOHandlerSSLOpenSSL(const AIdHTTPWebBrokerBridge: TIdHTTPWebBrokerBridge; const AHorseProviderIOHandleSSL: IPonyProviderIOHandleSSL);
var
  LIOHandleSSL: TIdServerIOHandlerSSLOpenSSL;
begin
  LIOHandleSSL := TIdServerIOHandlerSSLOpenSSL.Create(AIdHTTPWebBrokerBridge);
  LIOHandleSSL.SSLOptions.CertFile := AHorseProviderIOHandleSSL.CertFile;
  LIOHandleSSL.SSLOptions.RootCertFile := AHorseProviderIOHandleSSL.RootCertFile;
  LIOHandleSSL.SSLOptions.KeyFile := AHorseProviderIOHandleSSL.KeyFile;
  LIOHandleSSL.SSLOptions.Method := AHorseProviderIOHandleSSL.Method;
  LIOHandleSSL.SSLOptions.SSLVersions := AHorseProviderIOHandleSSL.SSLVersions;
  LIOHandleSSL.OnGetPassword := AHorseProviderIOHandleSSL.OnGetPassword;
  AIdHTTPWebBrokerBridge.IOHandler := LIOHandleSSL;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.InternalListen;
var
  LAttach: string;
  LIdHTTPWebBrokerBridge: TIdHTTPWebBrokerBridge;
begin
  inherited;
  if FPort <= 0 then
    FPort := GetDefaultPort;
  if FHost.IsEmpty then
    FHost := GetDefaultHost;

  LIdHTTPWebBrokerBridge := GetDefaultHTTPWebBroker;
  WebRequestHandler.WebModuleClass := WebModuleClass;
  try
    if FMaxConnections > 0 then
    begin
      WebRequestHandler.MaxConnections := FMaxConnections;
      GetDefaultHTTPWebBroker.MaxConnections := FMaxConnections;
    end;

    if FListenQueue = 0 then
      FListenQueue := IdListenQueueDefault;

    if FHorseProviderIOHandleSSL <> nil then
      InitServerIOHandlerSSLOpenSSL(LIdHTTPWebBrokerBridge, GetDefaultHorseProviderIOHandleSSL);

    LIdHTTPWebBrokerBridge.ListenQueue := FListenQueue;

    LIdHTTPWebBrokerBridge.Bindings.Clear;
    if FHost <> GetDefaultHost then
    begin
      LIdHTTPWebBrokerBridge.Bindings.Add;
      LIdHTTPWebBrokerBridge.Bindings.Items[0].IP := FHost;
      LIdHTTPWebBrokerBridge.Bindings.Items[0].Port := FPort;
    end;

    LIdHTTPWebBrokerBridge.KeepAlive := FKeepConnectionAlive;
    LIdHTTPWebBrokerBridge.DefaultPort := FPort;
    LIdHTTPWebBrokerBridge.Active := True;
    LIdHTTPWebBrokerBridge.StartListening;
    FRunning := True;
    DoOnListen;
  except
    raise;
  end;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.InternalStopListen;
begin
  if not HTTPWebBrokerIsNil then
  begin
    GetDefaultHTTPWebBroker.StopListening;
    GetDefaultHTTPWebBroker.Active := False;
    DoOnStopListen;
    FRunning := False;
    if FEvent <> nil then
      GetDefaultEvent.SetEvent;
  end
  else
    raise Exception.Create('Horse not listen');
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.StopListen;
begin
  InternalStopListen;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen;
begin
  InternalListen;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen(const APort: Integer;
                                                             const AHost: string;
                                                             const ACallbackListen, ACallbackStopListen: {$ifdef FPC}specialize{$endif} TProc<T>);
begin
  SetPort(APort);
  SetHost(AHost);
  SetOnListen(ACallbackListen);
  SetOnStopListen(ACallbackStopListen);
  InternalListen;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen(const AHost: string;
                                                             const ACallbackListen, ACallbackStopListen: {$ifdef FPC}specialize{$endif} TProc<T>);
begin
  Listen(FPort, AHost, ACallbackListen, ACallbackStopListen);
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen(const ACallbackListen, ACallbackStopListen: {$ifdef FPC}specialize{$endif} TProc<T>);
begin
  Listen(FPort, FHost, ACallbackListen, ACallbackStopListen);
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen(const APort: Integer;
                                                             const ACallbackListen, ACallbackStopListen: {$ifdef FPC}specialize{$endif} TProc<T>);
begin
  Listen(APort, FHost, ACallbackListen, ACallbackStopListen);
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.OnAuthentication(AContext: TIdContext; const AAuthType, AAuthData: string; var VUsername, VPassword: string; var VHandled: Boolean);
begin
  VHandled := True;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.SetHost(const AValue: string);
begin
  FHost := AValue.Trim;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.SetIOHandleSSL(const AValue: IPonyProviderIOHandleSSL);
begin
  FHorseProviderIOHandleSSL := AValue;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.SetListenQueue(const AValue: Integer);
begin
  FListenQueue := AValue;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.SetMaxConnections(const AValue: Integer);
begin
  FMaxConnections := AValue;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.SetPort(const AValue: Integer);
begin
  FPort := AValue;
end;

class destructor TPonyProvider{$ifndef FPC}<T>{$endif}.UnInitialize;
begin
  FreeAndNil(FIdHTTPWebBrokerBridge);
  if FEvent <> nil then
    FreeAndNil(FEvent);
end;

end.

