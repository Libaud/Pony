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
program PonyDaemonSample;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$else}
{$APPTYPE CONSOLE}
{$R *.res}
{$endif}

uses
  Classes, SysUtils,
  {$ifdef FPC}
  {$IFDEF UNIX}
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}
  {$ENDIF}
  {$else}
  {$endif}
  Pony, PonyRequestInterfaces, PonyResponseInterfaces, PonyProc;

var
  sCMD: string;
  bTerminated: Boolean;
  oPony: TPony;

procedure GetPing(aReq: IPonyRequest; aRes: IPonyResponse; aNext: {$ifdef FPC}TNextProc{$else}TProc{$endif});
begin
  aRes.Send('pong');
end;

procedure OnListen(aPony: TPony);
begin
  if aPony.IsRunning then
    Writeln(Format('Server is runing on %s:%d', [aPony.Host, aPony.Port]));
  if not oPony.IsRunning then
    Writeln('Server stopped');
end;

begin
  oPony:= TPony.Create;
  try
    oPony.Get('/ping', @GetPing);
    {$ifdef FPC}
    bTerminated := False;
    WriteLn('COMMANDS: START, STOP, TERMINATE');
    while not bTerminated do
    begin
      ReadLn(sCMD);
      case sCMD.ToUpper() of
        'START': oPony.Listen(9000, @OnListen);
        'STOP': oPony.StopListen;
        'TERMINATE' : bTerminated := True;
      end;
    end;
    {$else}
    oPony.Listen;
    {$endif}
  finally
    oPony.Destroy;
  end;
end.
