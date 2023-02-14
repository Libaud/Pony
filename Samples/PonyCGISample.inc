{=============================================================================
 Project :
 Author : Frédéric Libaud
 Description :
 Features:

 =============================================================================
 History
 -----------------------------------------------------------------------------
 08/02/2022 - Forking code from Horse framework
 =============================================================================}

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

{$ifdef DELPHI}
{$APPTYPE CONSOLE}
{$R *.res}
{$endif}


uses
  Pony, PonyRequestInterfaces, PonyResponseInterfaces, PonyProc;

procedure Treat(aReq: IPonyRequest; aRes: IPonyResponse; aNext: TNextProc);
begin
  aRes.Send('pong');
end;

var
  oPony: TPony;

begin
  oPony:= TPony.Create;
  try
    oPony.Get('/ping', @Treat);
    oPony.Listen;
  finally
    oPony.Destroy;
  end;
end.

