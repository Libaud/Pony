{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PkgPonyProvidersBase;

{$warn 5023 off : no warning about unused units}
interface

uses
  PonyProviderAbstract, TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PkgPonyProvidersBase', @Register);
end.
