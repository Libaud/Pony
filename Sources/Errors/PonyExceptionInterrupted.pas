{=============================================================================
 Unit :
 Framework : Pony
 Author : Frédéric Libaud
 Description :
 Features:

 =============================================================================
 History
 -----------------------------------------------------------------------------
 08/02/2022 - Forking code from Horse framework
 =============================================================================}
unit PonyExceptionInterrupted;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
{$ifdef FPC}
  SysUtils;
{$else}
  System.SysUtils;
{$endif}

type
  EPonyCallbackInterrupted = class(Exception)
    constructor Create; reintroduce;
  end;

implementation

constructor EPonyCallbackInterrupted.Create;
begin
  inherited Create(EmptyStr);
end;

end.
