{=============================================================================
 Project : Pony SSL Sample
 Author : Frédéric Libaud
 Description : Pony framwework implementation for SSL
 Features:

 =============================================================================
 History
 -----------------------------------------------------------------------------
 08/02/2022 - Forking code from Horse framework
 =============================================================================}

{$ifdef FPC}
{$MODE Delphi}
{$endif}

uses
  Classes, SysUtils, Forms, Interfaces
  {$ifdef FPC}
  {$else}
  {$endif},
  Main;

{$ifndef FPC}
{$R *.res}
{$endif}

begin
{$IFDEF MSWINDOWS}
  IsConsole := False;
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
