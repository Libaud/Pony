{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PkgPonyImplementationBase;

{$warn 5023 off : no warning about unused units}
interface

uses
  PonyProviderAbstract, TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PkgPonyImplementationBase', @Register);
end.
