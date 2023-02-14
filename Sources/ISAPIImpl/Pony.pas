unit Pony;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PonyISAPI;

type
  TPony = class;

  TPonyProvider =
  {$ifdef FPC}
    {$ifdef FPC}specialize{$endif} TPonyProvider<TPony>;
  {$endif}

  TPony = class(TPonyProvider);


implementation

end.

