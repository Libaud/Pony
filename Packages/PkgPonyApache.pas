{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PkgPonyApache;

{$warn 5023 off : no warning about unused units}
interface

uses
  Pony, PonyApache, TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PkgPonyApache', @Register);
end.
