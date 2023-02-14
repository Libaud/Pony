
{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

uses
  Classes
  { you can add units after this }
  , Pony, PonyRequestInterfaces, PonyResponseInterfaces, PonyProc;

procedure Treat(aReq: IPonyRequest; aRes: IPonyResponse; aNext: TNextProc);
begin
  aRes.Send('pong');
end;

var
  oPony: TPony;

begin
  oPony:= TPony.Create;
  try
    with oPony do
    begin
     Get('/ping', {$ifdef FPC}@{$endif}Treat);
     Listen;
    end;
  finally
    oPony.Destroy;
  end;
  {TPony.Get('/ping', {$ifdef FPC}@{$endif}Treat);
  TPony.Listen;}
end.


