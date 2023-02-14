{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit PkgPonyRTTI;

{$warn 5023 off : no warning about unused units}
interface

uses
  PonyRtti, PonyRttiHelper, TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PkgPonyRTTI', @Register);
end.
