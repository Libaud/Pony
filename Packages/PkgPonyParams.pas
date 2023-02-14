{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PkgPonyParams;

{$warn 5023 off : no warning about unused units}
interface

uses
  PonyCoreParam, PonyCoreParamConfig, PonyCoreParamField, 
  PonyCoreParamFieldBrackets, PonyCoreParamHeader, TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PkgPonyParams', @Register);
end.
