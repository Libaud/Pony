{=============================================================================
 Unit : PonyDaemon
 Framework : Pony
 Author : Frédéric Libaud
 Description : Daemon implementation of Pony framework
 Features:
 - Purpose a deamon implementation for REST
 =============================================================================
 History
 -----------------------------------------------------------------------------
 08/02/2022 - Forking code from Horse framework
 =============================================================================}
unit PonyDaemon;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,
  {$ifdef FPC}
  httpdefs, fpHTTP, fphttpserver, PonyRequest, PonyResponse, PonyCore,
  PonyProc, PonyCommons, PonyException,
  {$else}
  PonyProvider.IOHandleSSL.Contract, IdHTTPWebBrokerBridge, IdSSLOpenSSL, IdContext,
  PonyProviderIOHandleSSL, System.SyncObjs, Posix.SysTypes,
  {$endif}
  PonyProviderAbstract, PonyConstants  ;

type
  {$ifdef FPC}

  { THTTPServerThread }

  THTTPServerThread = class(TThread)
  private
    FStartServer: Boolean;
    FHost: string;
    FPort: Integer;
    FListenQueue: Word;
    FServer: TFPHTTPServer;
    FPony: TPonyCore;
  public
    constructor Create(const ACreateSuspended: Boolean; const AStackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    procedure StartServer;
    procedure StopServer;
    property Port: Integer read FPort write FPort;
    property Host: String read FHost write FHost;
    property ListenQueue: Word read FListenQueue write FListenQueue;
    procedure Execute; override;
    procedure OnRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
  end;

  { THTTPServerThread }

  {$endif}

  { TPonyProvider<T: class> }

  {$ifdef FPC}generic{$endif} TPonyProvider<T: class> = class({$ifdef FPC}specialize{$endif} TPonyProviderAbstract<T>)
  private
    // Members
    class var FPort: Integer;
    class var FHost: string;
    class var FListenQueue: Integer;
    {$ifdef FPC}
    class var FRunning: Boolean;
    class var FHTTPServerThread: THTTPServerThread;
    {$else}
    class var FMaxConnections: Integer;
    class var FKeepConnectionAlive: Boolean;
    class var FIdHTTPWebBrokerBridge: TIdHTTPWebBrokerBridge;
    class var FPonyProviderIOHandleSSL: IPonyProviderIOHandleSSL;
    {$endif}
    // Method's
    class procedure InternalListen; virtual;
    class procedure InternalStopListen; virtual;
    {$ifdef FPC}
    class function HTTPServerThreadIsNil: Boolean;
    {$else}
    class function HTTPWebBrokerIsNil: Boolean;
    class procedure OnAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
    class procedure OnQuerySSLPort(APort: Word; var VUseSSL: Boolean);
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
    class function GetDefaultHTTPServerThread: THTTPServerThread;
    {$else}
    class function GetDefaultHTTPWebBroker: TIdHTTPWebBrokerBridge;
    class function GetDefaultPonyProviderIOHandleSSL: IPonyProviderIOHandleSSL;
    class function GetMaxConnections: Integer; static;
    class procedure SetMaxConnections(const AValue: Integer); static;
    class function GetIOHandleSSL: IPonyProviderIOHandleSSL; static;
    class procedure SetIOHandleSSL(const AValue: IPonyProviderIOHandleSSL); static;
    class function GetKeepConnectionAlive: Boolean; static;
    class procedure SetKeepConnectionAlive(const AValue: Boolean); static;
    {$endif}
  public
    class procedure StopListen; override;
    class procedure Listen; overload; override;
    class procedure Listen(const APort: Integer; const AHost: string = '0.0.0.0'; const ACallbackListen: {$ifdef FPC}specialize{$endif} TProcWT<T> = nil; const ACallbackStopListen: {$ifdef FPC}specialize{$endif} TProcWT<T> = nil); reintroduce; overload; static;
    class procedure Listen(const APort: Integer; const ACallbackListen: {$ifdef FPC}specialize{$endif} TProcWT<T>; const ACallbackStopListen: {$ifdef FPC}specialize{$endif} TProcWT<T> = nil); reintroduce; overload; static;
    class procedure Listen(const AHost: string; const ACallbackListen: {$ifdef FPC}specialize{$endif} TProcWT<T> = nil; const ACallbackStopListen: {$ifdef FPC}specialize{$endif} TProcWT<T> = nil); reintroduce; overload; static;
    class procedure Listen(const ACallbackListen: {$ifdef FPC}specialize{$endif} TProcWT<T>; const ACallbackStopListen: {$ifdef FPC}specialize{$endif} TProcWT<T> = nil); reintroduce; overload; static;
    class destructor UnInitialize;
    {$ifdef FPC}
    class function IsRunning: Boolean;
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

  { TPonyProvider<T: class> }

{$ifndef FPC}
var
  FEvent: TEvent;
  FRunning: Boolean;
  FPID: pid_t;
  FId: Integer;

const
  EXIT_FAILURE = 1;
  EXIT_SUCCESS = 0;

procedure HandleSignals(SigNum: Integer); cdecl;
{$endif}

implementation

uses
  {$ifdef FPC}
  PonyExceptionInterrupted,
  {$else}
  Web.WebReq, PonyWebModule, IdCustomTCPServer, Posix.Stdlib, Posix.SysStat, Posix.Unistd, Posix.Signal, Posix.Fcntl,
  ThirdParty.Posix.Syslog,
  {$endif}
  PonyModules;

{$ifndef FPC}
procedure HandleSignals(SigNum: Integer); cdecl;
begin
  case SigNum of
    SIGTERM:
      begin
        FRunning:= False;
        FEvent.SetEvent;
      end;
    SIGHUP:
      begin
        Syslog(LOG_NOTICE, 'daemon: reloading config');
      end;
  end;
end;
{$endif}

{ THTTPServerThread implementation }

// Constructor's and destructor's

constructor THTTPServerThread.Create(const ACreateSuspended: Boolean; const AStackSize: SizeUInt = DefaultStackSize);
begin
  inherited Create(ACreateSuspended, AStackSize);
  FreeOnTerminate:= True;
  FStartServer:= False;
  FServer:= TFPHttpServer.Create(Nil);
  FServer.OnRequest:= @OnRequest;
  FPony:= TPonyCore.GetInstance;
end;

destructor THTTPServerThread.Destroy;
begin
  if Assigned(FServer) then
    FServer.Active:= False;
  FreeAndNil(FServer);
  inherited Destroy;
end;

//

procedure THTTPServerThread.OnRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
var
  oRequest: TPonyRequest;
  oResponse: TPonyResponse;
begin
  oRequest:= TPonyRequest.Create(ARequest);
  oResponse:= TPonyResponse.Create(AResponse);
  try
    try
      if not FPony.Routes.Execute(oRequest, oResponse) then
      begin
        AResponse.Content:= 'Not Found';
        AResponse.Code:= THTTPStatus.NotFound.ToInteger;
      end;
    except
      on E: Exception do
        if not E.InheritsFrom(EPonyCallbackInterrupted) then
          raise;
    end;
  finally
    if (oRequest.BodyAsObject = oResponse.Content) then
      oResponse.Content(nil);
    oRequest.Free;
    oResponse.Free;
  end;
end;

procedure THTTPServerThread.StartServer;
begin
  Start;
  FStartServer:= True;
end;

procedure THTTPServerThread.StopServer;
begin
  FStartServer:= False;
  FServer.Active:= FStartServer;
end;

procedure THTTPServerThread.Execute;
begin
  while not Terminated do
  begin
    if FStartServer then
    begin
      FServer.HostName:= FHost;
      FServer.Port:= FPort;
      FServer.Threaded:= True;
      FServer.QueueSize:= FListenQueue;
      FServer.Active:= True;
    end;
  end;
end;

{ End of THTTPServerThread implementation }

{ TPonyProvider<T: class> }

class destructor TPonyProvider{$ifndef FPC}<T>{$endif}.UnInitialize;
begin
  {$ifdef FPC}
  FreeAndNil(FHTTPServerThread);
  {$else}
  FreeAndNil(FIdHTTPWebBrokerBridge);
  {$endif}
end;

// Private Method's

{$ifdef FPC}
class function TPonyProvider{$ifndef FPC}<T>{$endif}.HTTPServerThreadIsNil: Boolean;
begin
  Result:= FHTTPServerThread = nil;
end;
{$endif}

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.InternalListen;
var
  {$ifdef FPC}
  LHTTPServerThread: THTTPServerThread;
  {$else}
  LIdx: Integer;
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
  LHTTPServerThread:= GetDefaultHTTPServerThread;
  LHTTPServerThread.Port:= FPort;
  LHTTPServerThread.Host:= FHost;
  LHTTPServerThread.ListenQueue:= FListenQueue;
  LHTTPServerThread.StartServer;
  FRunning:= True;
  DoOnListen;
  {$else}
  inherited;
  FEvent:= TEvent.Create;
  try
    openlog(nil, LOG_PID or LOG_NDELAY, LOG_DAEMON);

    if getppid() > 1 then
    begin
      FPID:= fork();
      if FPID < 0 then
        raise Exception.Create('Error forking the process');

      if FPID > 0 then
        Halt(EXIT_SUCCESS);

      if setsid() < 0 then
        raise Exception.Create('Impossible to create an independent session');

      Signal(SIGCHLD, TSignalHandler(SIG_IGN));
      Signal(SIGHUP, HandleSignals);
      Signal(SIGTERM, HandleSignals);

      FPID:= fork();
      if FPID < 0 then
        raise Exception.Create('Error forking the process');

      if FPID > 0 then
        Halt(EXIT_SUCCESS);

      for LIdx:= sysconf(_SC_OPEN_MAX) downto 0 do
        __close(LIdx);

      FId:= __open('/dev/null', O_RDWR);
      dup(FId);
      dup(FId);

      umask(027);

      chdir('/');
    end;

    try
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

        Syslog(LOG_INFO, Format(START_RUNNING, [FHost, FPort]));
      except
        on E: Exception do
          Syslog(LOG_ERR, E.ClassName + ': ' + E.Message);
      end;

      while FRunning do
        FEvent.WaitFor();

      ExitCode:= EXIT_SUCCESS;
    except
      on E: Exception do
      begin
        Syslog(LOG_ERR, 'Error: ' + E.Message);
        ExitCode:= EXIT_FAILURE;
      end;
    end;

    Syslog(LOG_NOTICE, 'daemon stopped');
    closelog();
  finally
    FEvent.Free;
  end;
  {$endif}
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.InternalStopListen;
begin
  {$ifdef FPC}
  if not HTTPServerThreadIsNil then
  begin
    GetDefaultHTTPServerThread.StopServer;
    DoOnStopListen;
    FRunning:= False;
  end
  else
    raise Exception.Create('Pony not listen');
  {$else}
  if not HTTPWebBrokerIsNil then
  begin
    GetDefaultHTTPWebBroker.StopListening;
    GetDefaultHTTPWebBroker.Active:= False;
    DoOnStopListen;
    FRunning:= False;
    if FEvent <> nil then
      FEvent.SetEvent;
  end
  else
    raise Exception.Create('Pony not listen');
  {$endif}
end;

{$ifndef FPC}
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
  LIOHandleSSL.OnGetPassword:= APonyProviderIOHandleSSL.OnGetPassword;
  AIdHTTPWebBrokerBridge.IOHandler:= LIOHandleSSL;
end;
{$endif}

{$ifndef FPC}
class function TPonyProvider{$ifndef FPC}<T>{$endif}.HTTPWebBrokerIsNil: Boolean;
begin
  Result:= FIdHTTPWebBrokerBridge = nil;
end;
{$endif}

{$ifndef FPC}
class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.OnQuerySSLPort(APort: Word; var VUseSSL: Boolean);
begin
  VUseSSL:= (FPonyProviderIOHandleSSL <> nil) and (FPonyProviderIOHandleSSL.Active);
end;
{$endif}

{$ifndef FPC}
class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.OnAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
begin
  VHandled:= True;
end;
{$endif}

// Private Access Method's

// Common Private Access Method's

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetDefaultPort: Integer;
begin
  Result:= DEFAULT_PORT;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetDefaultHost: string;
begin
  Result:= DEFAULT_HOST;
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

// FPC dedicated Access Method's

{$ifdef FPC}

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetDefaultHTTPServerThread: THTTPServerThread;
begin
  if HTTPServerThreadIsNil then
    FHTTPServerThread:= THTTPServerThread.Create(True);
  Result:= FHTTPServerThread;
end;

{$endif}

// Delphi Dedicated Access Method's

{$ifndef FPC}

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

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetMaxConnections: Integer;
begin
  Result:= FMaxConnections;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.SetMaxConnections(const AValue: Integer);
begin
  FMaxConnections:= AValue;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetKeepConnectionAlive: Boolean;
begin
  Result:= FKeepConnectionAlive;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.SetKeepConnectionAlive(const AValue: Boolean);
begin
  FKeepConnectionAlive:= AValue;
end;

class function TPonyProvider{$ifndef FPC}<T>{$endif}.GetIOHandleSSL: IPonyProviderIOHandleSSL;
begin
  Result:= GetDefaultPonyProviderIOHandleSSL;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.SetIOHandleSSL(const AValue: IPonyProviderIOHandleSSL);
begin
  FPonyProviderIOHandleSSL:= AValue;
end;

{$endif}

// Public Method's

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.StopListen;
begin
  InternalStopListen;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen;
begin
  InternalListen;;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen(const APort: Integer; const AHost: string; const ACallbackListen, ACallbackStopListen: {$ifdef FPC}specialize{$endif} TProcWT<T>);
begin
  SetPort(APort);
  SetHost(AHost);
  SetOnListen(ACallbackListen);
  SetOnStopListen(ACallbackStopListen);
  InternalListen;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen(const AHost: string; const ACallbackListen, ACallbackStopListen: {$ifdef FPC}specialize{$endif} TProcWT<T>);
begin
  Listen(FPort, AHost, ACallbackListen, ACallbackStopListen);
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen(const ACallbackListen, ACallbackStopListen: {$ifdef FPC}specialize{$endif} TProcWT<T>);
begin
  Listen(FPort, FHost, ACallbackListen, ACallbackStopListen);
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen(const APort: Integer; const ACallbackListen, ACallbackStopListen: {$ifdef FPC}specialize{$endif} TProcWT<T>);
begin
  Listen(APort, FHost, ACallbackListen, ACallbackStopListen);
end;

{$ifdef FPC}
class function TPonyProvider{$ifndef FPC}<T>{$endif}.IsRunning: Boolean;
begin
  Result:= FRunning;
end;
{$endif}

{ End of TPonyProvider<T: class> implementation }

end.

