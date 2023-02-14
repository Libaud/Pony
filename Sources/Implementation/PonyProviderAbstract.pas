{=============================================================================
 Unit : PonyProviderAbstract
 Framework : Pony
 Author : Frédéric Libaud
 Description : Abstract implementation for Pony Provider's
 Features:

 =============================================================================
 History
 -----------------------------------------------------------------------------
 08/02/2022 - Forking code from Horse framework
 =============================================================================}
unit PonyProviderAbstract;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
{$ifdef FPC}
  SysUtils, PonyProc,
{$else}
  System.SysUtils,
{$endif}
  PonyCore;

type
  {$ifdef FPC}generic{$endif} TPonyProviderAbstract<T: class{{$ifdef FPC}, constructor{$endif}}> = class(TPonyCore)
  private
    class var FOnListen: {$ifdef FPC}specialize{$endif} TProcWT<T>;
    class var FOnStopListen: {$ifdef FPC}specialize{$endif} TProcWT<T>;
    class function GetOnStopListen: {$ifdef FPC}specialize{$endif} TProcWT<T>; static;
  protected
    class function GetOnListen: {$ifdef FPC}specialize{$endif} TProcWT<T>; static;
    class procedure SetOnListen(const AValue: {$ifdef FPC}specialize{$endif} TProcWT<T>); static;
    class procedure SetOnStopListen(const AValue: {$ifdef FPC}specialize{$endif} TProcWT<T>); static;
    class procedure DoOnListen;
    class procedure DoOnStopListen;
  public
    class property OnListen: {$ifdef FPC}specialize{$endif} TProcWT<T> read GetOnListen
                                                                     write SetOnListen;
    class property OnStopListen: {$ifdef FPC}specialize{$endif} TProcWT<T> read GetOnStopListen
                                                                         write SetOnStopListen;
    class procedure Listen; virtual; abstract;
    class procedure StopListen; virtual;
  end;

implementation

class procedure TPonyProviderAbstract{$ifndef FPC}<T>{$endif}.DoOnListen;
begin
  if Assigned(FOnListen) then
    FOnListen({$ifdef FPC}T(GetInstance){$else}GetInstance{$endif});
end;

class procedure TPonyProviderAbstract{$ifndef FPC}<T>{$endif}.DoOnStopListen;
begin
  if Assigned(FOnStopListen) then
    FOnStopListen({$ifdef FPC}T(GetInstance){$else}GetInstance{$endif});
end;

class function TPonyProviderAbstract{$ifndef FPC}<T>{$endif}.GetOnListen: {$ifdef FPC}specialize{$endif} TProcWT<T>;
begin
  Result:= FOnListen;
end;

class function TPonyProviderAbstract{$ifndef FPC}<T>{$endif}.GetOnStopListen: {$ifdef FPC}specialize{$endif} TProcWT<T>;
begin
  Result:= FOnStopListen;
end;

class procedure TPonyProviderAbstract{$ifndef FPC}<T>{$endif}.SetOnListen(const AValue: {$ifdef FPC}specialize{$endif} TProcWT<T>);
begin
  FOnListen:= AValue;
end;

class procedure TPonyProviderAbstract{$ifndef FPC}<T>{$endif}.SetOnStopListen(const AValue: {$ifdef FPC}specialize{$endif} TProcWT<T>);
begin
  FOnStopListen:= AValue;
end;

class procedure TPonyProviderAbstract{$ifndef FPC}<T>{$endif}.StopListen;
begin
  raise Exception.Create('StopListen not implemented');
end;

end.
