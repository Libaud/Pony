{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PkgPonySSLImpl;

{$warn 5023 off : no warning about unused units}
interface

uses
  Pony, PonySSL, PonySSLIOHandlerContract, PonySSLIOHandler, TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PkgPonySSLImpl', @Register);
end.
