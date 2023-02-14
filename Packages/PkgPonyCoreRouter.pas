{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PkgPonyCoreRouter;

{$warn 5023 off : no warning about unused units}
interface

uses
  PonyCoreRouterTree, PonyCoreRouterTreeNextCaller, TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PkgPonyCoreRouter', @Register);
end.
