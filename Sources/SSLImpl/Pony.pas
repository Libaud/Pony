unit Pony;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, PonySSL;

type
  TPony = class;

  TPonyProvider = {$ifdef FPC}specialize{$endif} TPonyProvider<TPony>;

  TPony = class(TPonyProvider);

implementation

end.

