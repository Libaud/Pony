unit PonySSLIOHandler;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,
  IdSSLOpenSSL, PonySSLIOHandlerContract;

type
  TPonyProviderIOHandleSSL = class(TInterfacedObject, IPonyProviderIOHandleSSL)
  private
    FKeyFile: string;
    FRootCertFile: string;
    FCertFile: string;
    FDHParamsFile: string;
    FCipherList: string;
    FMethod: TIdSSLVersion;
    FSSLVersions: TIdSSLVersions;
    FOnGetPassword: TPasswordEvent;
    FActive: Boolean;
    function Active: Boolean; overload;
    function Active(const AValue: Boolean): IPonyProviderIOHandleSSL; overload;
    function CertFile: string; overload;
    function CertFile(const AValue: string): IPonyProviderIOHandleSSL; overload;
    function RootCertFile: string; overload;
    function RootCertFile(const AValue: string): IPonyProviderIOHandleSSL; overload;
    function KeyFile: string; overload;
    function KeyFile(const AValue: string): IPonyProviderIOHandleSSL; overload;
    function Method: TIdSSLVersion; overload;
    function Method(const AValue: TIdSSLVersion): IPonyProviderIOHandleSSL; overload;
    function SSLVersions: TIdSSLVersions; overload;
    function SSLVersions(const AValue: TIdSSLVersions): IPonyProviderIOHandleSSL; overload;
    function DHParamsFile: string; overload;
    function DHParamsFile(const AValue: string): IPonyProviderIOHandleSSL; overload;
    function CipherList: string; overload;
    function CipherList(const AValue: string): IPonyProviderIOHandleSSL; overload;
    function OnGetPassword: TPasswordEvent; overload;
    function OnGetPassword(const AValue: TPasswordEvent): IPonyProviderIOHandleSSL; overload;
  public
    constructor Create;
    class function New: IPonyProviderIOHandleSSL;
  end;

implementation

function TPonyProviderIOHandleSSL.Active(const AValue: Boolean): IPonyProviderIOHandleSSL;
begin
  FActive := AValue;
  Result := Self;
end;

function TPonyProviderIOHandleSSL.Active: Boolean;
begin
  Result := FActive;
end;

function TPonyProviderIOHandleSSL.CertFile(const AValue: string): IPonyProviderIOHandleSSL;
begin
  FCertFile := AValue;
  Result := Self;
end;

function TPonyProviderIOHandleSSL.CipherList: string;
begin
  Result := FCipherList;
end;

function TPonyProviderIOHandleSSL.CipherList(const AValue: string): IPonyProviderIOHandleSSL;
begin
  FCipherList := AValue;
  Result := Self;
end;

function TPonyProviderIOHandleSSL.CertFile: string;
begin
  Result := FCertFile;
end;

constructor TPonyProviderIOHandleSSL.Create;
begin
  FActive := True;
  FMethod := DEF_SSLVERSION;
  FSSLVersions := DEF_SSLVERSIONS;
end;

function TPonyProviderIOHandleSSL.DHParamsFile: string;
begin
  Result := FDHParamsFile;
end;

function TPonyProviderIOHandleSSL.DHParamsFile(const AValue: string): IPonyProviderIOHandleSSL;
begin
  FDHParamsFile := AValue;
  Result := Self;
end;

function TPonyProviderIOHandleSSL.KeyFile(const AValue: string): IPonyProviderIOHandleSSL;
begin
  FKeyFile := AValue;
  Result := Self;
end;

function TPonyProviderIOHandleSSL.KeyFile: string;
begin
  Result := FKeyFile;
end;

function TPonyProviderIOHandleSSL.Method(const AValue: TIdSSLVersion): IPonyProviderIOHandleSSL;
begin
  FMethod := AValue;
  Result := Self;
end;

function TPonyProviderIOHandleSSL.Method: TIdSSLVersion;
begin
  Result := FMethod;
end;

class function TPonyProviderIOHandleSSL.New: IPonyProviderIOHandleSSL;
begin
  Result := TPonyProviderIOHandleSSL.Create;
end;

function TPonyProviderIOHandleSSL.OnGetPassword(const AValue: TPasswordEvent): IPonyProviderIOHandleSSL;
begin
  FOnGetPassword := AValue;
  Result := Self;
end;

function TPonyProviderIOHandleSSL.OnGetPassword: TPasswordEvent;
begin
  Result := FOnGetPassword;
end;

function TPonyProviderIOHandleSSL.RootCertFile(const AValue: string): IPonyProviderIOHandleSSL;
begin
  FRootCertFile := AValue;
  Result := Self;
end;

function TPonyProviderIOHandleSSL.RootCertFile: string;
begin
  Result := FRootCertFile;
end;

function TPonyProviderIOHandleSSL.SSLVersions: TIdSSLVersions;
begin
  Result := FSSLVersions;
end;

function TPonyProviderIOHandleSSL.SSLVersions(const AValue: TIdSSLVersions): IPonyProviderIOHandleSSL;
begin
  FSSLVersions := AValue;
  Result := Self;
end;

end.

