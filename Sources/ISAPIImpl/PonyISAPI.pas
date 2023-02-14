unit PonyISAPI;

{$ifndef WINDOWS}
 Exclusively for Windows because IIS is not working elsewhere
{$endif}

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,
  {$ifdef WINDOWS}
  {$ifdef FPC}
  {$else}
  Web.Win.ISAPIApp,
  {$endif}
  {$endif}
  PonyProviderAbstract, PonyProc;

type
  {$ifdef FPC}generic{$endif} TPonyProvider<T: class> = class({$ifdef FPC}specialize{$endif} TPonyProviderAbstract<T>)
  private
    class procedure InternalListen; static;
  public
    class procedure Listen; overload; override;
    class procedure Listen(const ACallback: {$ifdef FPC}specialize{$endif} TProcWT<T>); reintroduce; overload; static;
  end;

implementation

uses
  {$ifdef WINDOWS}
  {$ifdef FPC}
  {$else}
  Web.WebBroker, System.Win.ComObj, Winapi.ActiveX,
  {$endif}
  {$endif}
  PonyModules;

exports
  { Todo : implementation }
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.InternalListen;
begin
  CoInitFlags := COINIT_MULTITHREADED;
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  DoOnListen;
  Application.Run;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen;
begin
  inherited;
  InternalListen;
end;

class procedure TPonyProvider{$ifndef FPC}<T>{$endif}.Listen(const ACallback: {$ifdef FPC}specialize{$endif} TProcWT<T>);
begin
  inherited;
  SetOnListen(ACallback);
  InternalListen;
end;

end.

