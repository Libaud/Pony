{=============================================================================
 Unit :
 Framework : Pony
 Author :  Frédéric Libaud
 Description :
 Features:

 =============================================================================
 History
 -----------------------------------------------------------------------------
 08/02/2022 - Forking code from Horse framework
 =============================================================================}
unit PonyTypes;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  {$ifdef FPC}
    SysUtils, Classes, DateUtils, Generics.Collections, fpHTTP, fphttpserver, HTTPDefs
  {$else}
    System.SysUtils, System.Classes, System.DateUtils, System.Generics.Collections
  {$endif};

type
  TPonyList = {$ifdef FPC}specialize{$endif} TDictionary<string, string>;

  TPort = word;

implementation

end.

