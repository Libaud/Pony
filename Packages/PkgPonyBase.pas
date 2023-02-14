{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PkgPonyBase;

{$warn 5023 off : no warning about unused units}
interface

uses
  PonyCommons, PonyCallback, PonyInterfaces, PonyCoreFiles, PonyMime, 
  PonyProc, PonySession, PonyTypes, PonyRequestInterfaces, 
  PonyParamsInterfaces, PonyResponseInterfaces, PonyRouterTreeInterfaces, 
  PonyModuleInterfaces, TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PkgPonyBase', @Register);
end.
