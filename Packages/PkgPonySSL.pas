{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PkgPonySSL;

{$warn 5023 off : no warning about unused units}
interface

uses
  PonySSLHandler, PonySSLHandlerInterfaces, TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PkgPonySSL', @Register);
end.
