unit PonyHTTP;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

uses
  SysUtils, Classes, httpdefs, fpHTTP, fphttpapp, PonyProviderAbstract, PonyConstants, PonyProc;

type
  TPonyProvider<T: class> = class(TPonyProviderAbstract<T>)
  private
    class var FPort: Integer;
    class var FHost: string;
    class var FRunning: Boolean;
    class var FListenQueue: Integer;
    class var FHTTPApplication: THTTPApplication;
    class function GetDefaultHTTPApplication: THTTPApplication;
    class function HTTPApplicationIsNil: Boolean;
    class procedure SetListenQueue(const AValue: Integer); static;
    class procedure SetPort(const AValue: Integer); static;
    class procedure SetHost(const AValue: string); static;
    class function GetListenQueue: Integer; static;
    class function GetPort: Integer; static;
    class function GetDefaultPort: Integer; static;
    class function GetDefaultHost: string; static;
    class function GetHost: string; static;
    class procedure InternalListen; virtual;
    class procedure DoGetModule(Sender: TObject; ARequest: TRequest; var ModuleClass: TCustomHTTPModuleClass);
  public
    class property Host: string read GetHost write SetHost;
    class property Port: Integer read GetPort write SetPort;
    class property ListenQueue: Integer read GetListenQueue write SetListenQueue;
    class procedure Listen; overload; override;
    class procedure Listen(const APort: Integer; const AHost: string = '0.0.0.0'; const ACallback: TProc<T> = nil); reintroduce; overload; static;
    class procedure Listen(const APort: Integer; const ACallback: TProc<T>); reintroduce; overload; static;
    class procedure Listen(const AHost: string; const ACallback: TProc<T> = nil); reintroduce; overload; static;
    class procedure Listen(const ACallback: TProc<T>); reintroduce; overload; static;
    class function IsRunning: Boolean;
  end;

implementation

uses
  PonyModules;

class function TPonyProvider<T>.GetDefaultHTTPApplication: THTTPApplication;
begin
  if HTTPApplicationIsNil then
    FHTTPApplication := Application;
  Result := FHTTPApplication;
end;

class function TPonyProvider<T>.HTTPApplicationIsNil: Boolean;
begin
  Result := FHTTPApplication = nil;
end;

class function TPonyProvider<T>.GetDefaultHost: string;
begin
  Result := DEFAULT_HOST;
end;

class function TPonyProvider<T>.GetDefaultPort: Integer;
begin
  Result := DEFAULT_PORT;
end;

class function TPonyProvider<T>.GetHost: string;
begin
  Result := FHost;
end;

class function TPonyProvider<T>.GetListenQueue: Integer;
begin
  Result := FListenQueue;
end;

class function TPonyProvider<T>.GetPort: Integer;
begin
  Result := FPort;
end;

class procedure TPonyProvider<T>.InternalListen;
var
  LHTTPApplication: THTTPApplication;
begin
  inherited;
  if FPort <= 0 then
    FPort := GetDefaultPort;
  if FHost.IsEmpty then
    FHost := GetDefaultHost;
  if FListenQueue = 0 then
    FListenQueue := 15;
  LHTTPApplication := GetDefaultHTTPApplication;
  LHTTPApplication.AllowDefaultModule := True;
  LHTTPApplication.OnGetModule := DoGetModule;
  LHTTPApplication.Threaded := True;
  LHTTPApplication.QueueSize := FListenQueue;
  LHTTPApplication.Port := FPort;
  LHTTPApplication.LegacyRouting := True;
  LHTTPApplication.Address := FHost;
  LHTTPApplication.Initialize;
  FRunning := True;
  DoOnListen;
  LHTTPApplication.Run;
end;

class procedure TPonyProvider<T>.DoGetModule(Sender: TObject; ARequest: TRequest; var ModuleClass: TCustomHTTPModuleClass);
begin
  ModuleClass :=  TPonyWebModule;
end;

class function TPonyProvider<T>.IsRunning: Boolean;
begin
  Result := FRunning;
end;

class procedure TPonyProvider<T>.Listen;
begin
  InternalListen;;
end;

class procedure TPonyProvider<T>.Listen(const APort: Integer; const AHost: string; const ACallback: TProc<T>);
begin
  SetPort(APort);
  SetHost(AHost);
  SetOnListen(ACallback);
  InternalListen;
end;

class procedure TPonyProvider<T>.Listen(const AHost: string; const ACallback: TProc<T>);
begin
  Listen(FPort, AHost, ACallback);
end;

class procedure TPonyProvider<T>.Listen(const ACallback: TProc<T>);
begin
  Listen(FPort, FHost, ACallback);
end;

class procedure TPonyProvider<T>.Listen(const APort: Integer; const ACallback: TProc<T>);
begin
  Listen(APort, FHost, ACallback);
end;

class procedure TPonyProvider<T>.SetHost(const AValue: string);
begin
  FHost := AValue;
end;

class procedure TPonyProvider<T>.SetListenQueue(const AValue: Integer);
begin
  FListenQueue := AValue;
end;

class procedure TPonyProvider<T>.SetPort(const AValue: Integer);
begin
  FPort := AValue;
end;

end.

