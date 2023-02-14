{=============================================================================
 Prject :
 Author : Frédéric Libaud
 Description :
 Features:

 =============================================================================
 History
 -----------------------------------------------------------------------------
 08/02/2022 - Forking code from Horse framework
 =============================================================================}

{$MODE Delphi}

{$R *.res}

uses
  Pony, PonyRequestInterfaces, PonyResponseInterfaces, PonyProc;

procedure Treat(aReq: THorseRequest; aRes: THorseResponse; aNext: {$ifdef FPC}TNextProc{$else}TProc{$endif});
begin
  aRes.Send('pong');
end;

var
  oPony: TPony;

begin
  oPony:= TPony.Create;
  try
    oPony.Get('/ping',
    oPony.Listen;
  finally
    oPony.Destroy;
  end;
end.

