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
unit PonyCoreParamConfig;

//{$ifdef FPC}
//  {$MODE DELPHI}{$H+}
//{$endif}
{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

type
  TPonyCoreParamConfig = class
  private
    class var FInstance: TPonyCoreParamConfig;
    FRequiredMessage: string;
    FInvalidFormatMessage: string;
    FDateFormat: string;
    FTimeFormat: string;
    FReturnUTC: Boolean;
    FTrueValue: string;
    FCheckLhsBrackets: Boolean;
  public
    // Constructor's and destructor's
    constructor Create;
    // Method's
    function RequiredMessage(const AValue: string): TPonyCoreParamConfig; overload;
    function RequiredMessage: string; overload;
    function InvalidFormatMessage(const AValue: string): TPonyCoreParamConfig; overload;
    function InvalidFormatMessage: string; overload;
    function DateFormat(const AValue: string): TPonyCoreParamConfig; overload;
    function DateFormat: string; overload;
    function TimeFormat(const AValue: string): TPonyCoreParamConfig; overload;
    function TimeFormat: string; overload;
    function ReturnUTC(const AValue: Boolean): TPonyCoreParamConfig; overload;
    function ReturnUTC: Boolean; overload;
    function TrueValue(const AValue: string): TPonyCoreParamConfig; overload;
    function TrueValue: string; overload;
    function CheckLhsBrackets(const AValue: Boolean): TPonyCoreParamConfig; overload;
    function CheckLhsBrackets: Boolean; overload;
    // Class Method's
    class function GetInstance: TPonyCoreParamConfig;
    class destructor UnInitialize;
  end;

implementation

uses
{$ifdef FPC}
  SysUtils;
{$else}
  System.SysUtils;
{$endif}

constructor TPonyCoreParamConfig.Create;
begin
  FReturnUTC:= True;
  FDateFormat:= 'yyyy-MM-dd';
  FTimeFormat:= 'hh:mm:ss';
  FTrueValue:= 'true';
  FRequiredMessage:= 'The %s param is required.';
  FInvalidFormatMessage:= 'The %0:s param ''%1:s'' is not valid a %2:s type.';
  FCheckLhsBrackets:= False;
end;

function TPonyCoreParamConfig.DateFormat(const AValue: string): TPonyCoreParamConfig;
begin
  Result:= Self;
  FDateFormat:= AValue;
end;

function TPonyCoreParamConfig.DateFormat: string;
begin
  Result:= FDateFormat;
end;

class function TPonyCoreParamConfig.GetInstance: TPonyCoreParamConfig;
begin
  if not Assigned(FInstance) then
    FInstance:= TPonyCoreParamConfig.Create;
  Result:= FInstance;
end;

function TPonyCoreParamConfig.InvalidFormatMessage: string;
begin
  Result:= FInvalidFormatMessage;
end;

function TPonyCoreParamConfig.InvalidFormatMessage(const AValue: string): TPonyCoreParamConfig;
begin
  Result:= Self;
  FInvalidFormatMessage:= AValue;
end;

function TPonyCoreParamConfig.RequiredMessage(const AValue: string): TPonyCoreParamConfig;
begin
  Result:= Self;
  FRequiredMessage:= AValue;
end;

function TPonyCoreParamConfig.RequiredMessage: string;
begin
  Result:= FRequiredMessage;
end;

function TPonyCoreParamConfig.ReturnUTC(const AValue: Boolean): TPonyCoreParamConfig;
begin
  Result:= Self;
  FReturnUTC:= AValue;
end;

function TPonyCoreParamConfig.ReturnUTC: Boolean;
begin
  Result:= FReturnUTC;
end;

function TPonyCoreParamConfig.TimeFormat: string;
begin
  Result:= FTimeFormat;
end;

function TPonyCoreParamConfig.TimeFormat(const AValue: string): TPonyCoreParamConfig;
begin
  Result:= Self;
  FTimeFormat:= AValue;
end;

function TPonyCoreParamConfig.TrueValue(const AValue: string): TPonyCoreParamConfig;
begin
  Result:= Self;
  FTrueValue:= AValue;
end;

function TPonyCoreParamConfig.TrueValue: string;
begin
  Result:= FTrueValue;
end;

function TPonyCoreParamConfig.CheckLhsBrackets(const AValue: Boolean
  ): TPonyCoreParamConfig;
begin
  Result:= Self;
  FCheckLhsBrackets:= AValue;
end;

function TPonyCoreParamConfig.CheckLhsBrackets: Boolean;
begin
  Result:= FCheckLhsBrackets;
end;

class destructor TPonyCoreParamConfig.UnInitialize;
begin
  if Assigned(FInstance) then
    FreeAndNil(FInstance);
end;

end.
