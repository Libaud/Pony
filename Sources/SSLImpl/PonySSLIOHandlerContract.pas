unit PonySSLIOHandlerContract;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, IdSSLOpenSSL;

type
  IPonyProviderIOHandleSSL = interface
    ['{E24EB539-B1B9-4E27-88AA-238A3F47BC11}']
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
  end;

implementation

end.

