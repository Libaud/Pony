{=============================================================================
 Project : Pony windows Service Sample
 Author : Frédéric Libaud
 Description : Pony framework implementation for Windows Service
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
  {$ifdef FPC}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
  {$ENDIF}{$ENDIF}
  DaemonApp, lazdaemonapp, DaemonManager, DaemonMain;
  {$else}
  Vcl.SvcMgr
  {$endif};

{$ifndef FPC}
{$R *.RES}
{$endif}

begin
  // Windows 2003 Server requires StartServiceCtrlDispatcher to be
  // called before CoRegisterClassObject, which can be called indirectly
  // by Application.Initialize. TServiceApplication.DelayInitialize allows
  // Application.Initialize to be called from TService.Main (after
  // StartServiceCtrlDispatcher has been called).

  // Delayed initialization of the Application object may affect
  // events which then occur prior to initialization, such as
  // TService.OnCreate. It is only recommended if the ServiceApplication
  // registers a class object with OLE and is intended for use with
  // Windows 2003 Server.

  // Application.DelayInitialize := True;
  {$ifdef FPC}
  Application.Initialize;
  Application.Run;
  {$else}
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TMainService, MainService);
  Application.Run;
  {$endif}
end.


