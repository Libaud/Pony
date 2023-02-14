{=============================================================================
 Unit :
 Framework : Pony
 Author : Frédéric Libaud
 Description :
 Features:

 =============================================================================
 History
 -----------------------------------------------------------------------------
 08/02/2022 - Forking code from Horse framework
 =============================================================================}
unit PonyModuleInterfaces;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PonyInterfaces;

type
  IPonyCoreInsance = interface
    function ToPony: IPonyCore;
  end;

implementation

end.

