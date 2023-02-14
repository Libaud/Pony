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

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, PonyHTTP;

type
  TPony = class;

  TPonyProvider = {$ifdef FPC}specialize{$endif} TPonyProvider<TPony>;

  TPony = class(TPonyProvider);

implementation

end.

