{=============================================================================
 Project : Pony Apache Framework Implementation Sample
 Author : Frédéric Libaud
 Description :
 Features:

 =============================================================================
 History
 -----------------------------------------------------------------------------
 08/02/2022 - Forking code from Horse framework
 =============================================================================}
{$ifdef FPC}
{$DEFINE Apache2_4}
{$DEFINE FPCAPACHE_2_4}
{$mode objfpc}{$H+}
{$endif}

{$ifndef FPC}
{$R *.res}
{$endif}

(*
 httpd.conf entries:
 LoadModule apache_horse_module modules/Apache.dll
 <Location /apache_horse>
    SetHandler apache_horse_module-handle
 </Location>
 To use the feature:
 http://localhost/apache_horse/ping
 These entries assume that the output directory for this project is the apache/modules directory.
 httpd.conf entries should be different if the project is changed in these ways:
   1. The TApacheModuleData variable name is changed.
   2. The project is renamed.
   3. The output directory is not the apache/modules directory.
   4. The dynamic library extension depends on a platform. Use .dll on Windows and .so on Linux.
*)

// Declare exported variable so that Apache can access this module.


uses
  {$ifdef FPC}
  {$ifdef unix}
  cthreads,
  {$endif}
  httpd24, fpApache24, custapache24,
  {$else}
  Web.HTTPD24Impl,
  {$endif}
  Pony, PonyRequestInterfaces, PonyResponseInterfaces, PonyProc;

{$ifdef FPC}
const
  ModuleName = 'apache_horse_module';
{$endif}

var
  {$ifdef FPC}
  ApacheModuleData : module; {$ifdef unix} public name ModuleName;{$endif unix}
  {$else}
  ApacheModuleData: TApacheModuleData;
  {$endif}
  oPony: TPony;

exports
  {$ifdef FPC}
  ApacheModuleData name ModuleName;
  {$else}
  ApacheModuleData name 'apache_horse_module';
  {$endif}

procedure GetPing(aReq: IPonyRequest; aRes: IPonyResponse; aNext: {$ifdef FPC}TNextProc{$else}TProc{$endif});
begin
  aRes.Send('pong');
end;

begin
  oPony:= TPony.Create;
  try
    oPony.DefaultModule := @ApacheModuleData;
    oPony.HandlerName := 'apache_pony_module-handle';
    {$ifdef FPC}
    oPony.ModuleName := ModuleName;
    {$endif}
    oPony.Get('/ping', @GetPing);
    oPony.Listen;
  finally
    oPony.Destroy;
  end;
end.

