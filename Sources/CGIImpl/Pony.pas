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
  Classes, SysUtils, PonyCGI;

type
  TPony = class;

  TPonyProvider =
  {$ifdef FPC}
    {$ifdef FPC}specialize{$endif} TPonyProvider<TPony>;
  {$endif}

  TPony = class(TPonyProvider);

implementation

end.

