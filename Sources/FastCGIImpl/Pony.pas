{=============================================================================
 Unit : Pony
 Framework : Pony
 Author : Frédéric Libaud
 Description : Pony Fast CGI implementation for Pony framework
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
  Classes, SysUtils, PonyFastCGI;

type
  TPony = class;

  TPonyProvider = {$ifdef FPC}specialize{$endif} TPonyProvider<TPony>;

  TPony = class(TPonyProvider);

implementation

end.

