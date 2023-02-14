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
unit Pony;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PonyDaemon;

type
  TPony = class;

  TPonyProvider = {$ifdef FPC}specialize{$endif} TPonyProvider<TPony>;

  TPony = class(TPonyProvider);

implementation

end.

