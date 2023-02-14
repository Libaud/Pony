{=============================================================================
 Prject : Pony Console Sample
 Author : Frédéric Libaud
 Description : Pony framework implementation sample for Console application
 Features:

 =============================================================================
 History
 -----------------------------------------------------------------------------
 08/02/2022 - Forking code from Horse framework
 =============================================================================}
{$ifdef FPC}
{$mode objfpc}{$H+}
{$else}
{$APPTYPE CONSOLE}
{$R *.res}
{$endif}

uses
  Classes, SysUtils,
  Pony, PonyRequestInterfaces, PonyResponseInterfaces, PonyProc;

procedure Treat(aReq: IPonyRequest; aRes: IPonyResponse; aNext: {$ifdef FPC}TNextProc{$else}TProc{$endif});
begin
  aRes.Send('pong');
end;

procedure WriteOut(aPony: TPony);
begin
  Writeln(Format('Server is runing on %s:%d', [aPony.Host, aPony.Port]));
  Readln;
end;

var
  oPony: TPony;

begin
  oPony:= TPony.Create;
  try
    {$ifndef FPC}
    oPony.KeepConnectionAlive := True;
    {$endif}
    {$IFDEF MSWINDOWS}
    IsConsole := False;
    ReportMemoryLeaksOnShutdown := True;
    {$ENDIF}
    oPony.Get('/ping', @Treat);
    oPony.Listen(9000, @WriteOut);
  finally
  end;
end.
