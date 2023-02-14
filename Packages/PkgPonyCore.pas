{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PkgPonyCore;

{$warn 5023 off : no warning about unused units}
interface

uses
  PonyCore, PonyCoreGroup, PonyCoreGroupContract, PonyConstants, 
  PonyCoreInstances, TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PkgPonyCore', @Register);
end.
