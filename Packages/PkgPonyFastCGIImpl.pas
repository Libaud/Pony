{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PkgPonyFastCGIImpl;

{$warn 5023 off : no warning about unused units}
interface

uses
  Pony, PonyFastCGI, TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PkgPonyFastCGIImpl', @Register);
end.
