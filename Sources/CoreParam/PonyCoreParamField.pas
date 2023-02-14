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
unit PonyCoreParamField;

//{$ifdef FPC}
//  {$MODE DELPHI}{$H+}
//{$endif}
{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
{$ifdef FPC}
  SysUtils, Classes, DateUtils, Generics.Collections,
{$else}
  System.SysUtils, System.Classes, System.DateUtils, System.Generics.Collections,
{$endif}
  PonyParamsInterfaces, PonyException, PonyCommons, PonyCoreParamFieldBrackets,
  PonyCoreParamConfig;

type
  TPonyCoreParamField = class (IPonyCoreParamField)
  private
    // Members
    _RefCount: integer;
    _Contains: Boolean;
    _FieldName: string;
    _Required: Boolean;
    _RequiredMessage: string;
    _InvalidFormatMessage: string;
    _DateFormat: string;
    _TimeFormat: string;
    _ReturnUTC: Boolean;
    _TrueValue: string;
    _Value: string;
    _Stream: TStream;
    _LhsBrackets: TPonyCoreParamFieldLhsBrackets;
    // Method's
    function GetFormatSettings: TFormatSettings;
    procedure RaisePonyException(const AMessage: string); overload;
    procedure RaisePonyException(const AMessage: string; const Args: array of const); overload;
    function TryISO8601ToDate(const AValue: string; out Value: TDateTime): Boolean;
    procedure InitializeLhsBrackets(const AParams: {$ifdef FPC}specialize{$endif} TDictionary<string, string>; const AFieldName: string);
    // Access Method's
    function GetLhsBrackets: IPonyCoreParamFieldLhsBrackets;
  public
    // Constructor's and destructor's
    constructor Create(const AParams: {$ifdef FPC}specialize{$endif} TDictionary<string, string>; const AFieldName: string); overload;
    constructor Create(const AStream: TStream; const AFieldName: string); overload;
    destructor Destroy; override;
    // Interfaces Implementation Method's
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$else}const{$endif} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
    function _AddRef: longint; {$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
    function _Release: longint; {$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
    // Method's
    function DateFormat(const AValue: string): IPonyCoreParamField;
    function InvalidFormatMessage(const AValue: string): IPonyCoreParamField;
    function Required: IPonyCoreParamField; overload;
    function Required(const AValue: Boolean): IPonyCoreParamField; overload;
    function RequiredMessage(const AValue: string): IPonyCoreParamField;
    function ReturnUTC(const AValue: Boolean): IPonyCoreParamField;
    function TimeFormat(const AValue: string): IPonyCoreParamField;
    function TrueValue(const AValue: string): IPonyCoreParamField;
    procedure SaveToFile(const AFileName: String);
    function AsBoolean: Boolean;
    function AsCurrency: Currency;
    function AsDate: TDateTime;
    function AsDateTime: TDateTime;
    function AsExtended: Extended;
    function AsFloat: Double;
    function AsInteger: Integer;
    function AsInt64: Int64;
    function AsISO8601DateTime: TDateTime;
    function AsStream: TStream;
    function AsString: string;
    function AsTime: TTime;
    // Properties
    property LhsBrackets: IPonyCoreParamFieldLhsBrackets read GetLhsBrackets;
  end;

implementation

{ TPonyCoreParamField implementation }

// Constructor's and destructor's

constructor TPonyCoreParamField.Create(const AStream: TStream; const AFieldName: string);
begin
  _Contains:= True;
  _FieldName:= AFieldName;
  _Value:= EmptyStr;
  _Required:= False;
  _Stream:= AStream;
end;

constructor TPonyCoreParamField.Create(const AParams: {$ifdef FPC}specialize{$endif} TDictionary<string, string>; const AFieldName: string);
var
  LKey: string;
begin
  _Contains:= False;
  _FieldName:= AFieldName;
  _Value:= EmptyStr;
  _Required:= False;

  for LKey in AParams.Keys do
  begin
    if AnsiCompareText(LKey, _FieldName) = 0 then
    begin
      _Contains:= True;
      Break;
    end;
  end;

  if _Contains then
    _Value:= AParams.Items[LKey];

  InitializeLhsBrackets(AParams, AFieldName);
end;

destructor TPonyCoreParamField.Destroy;
begin
  _LhsBrackets.Free;
  inherited Destroy;
end;

// Interfaces Implementation Method's

function TPonyCoreParamField.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$else}const{$endif} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  if GetInterface(IID,Obj) then
	Result:= S_OK
  else
	Result:= E_NOINTERFACE;
end;

function TPonyCoreParamField._AddRef : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  Result:= InterlockedIncrement(_RefCount);
end;

function TPonyCoreParamField._Release : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  Result:= InterlockedDecrement(_RefCount);
  {if Result = 0 then
	Self.Destroy;}
end;

function TPonyCoreParamField.GetLhsBrackets: IPonyCoreParamFieldLhsBrackets;
begin
  Result:= _LhsBrackets;
end;

// Public Method's

function TPonyCoreParamField.AsBoolean: Boolean;
var
  LStrParam: string;
begin
  Result:= False;
  LStrParam:= AsString;
  if LStrParam <> EmptyStr then
    Result:= LowerCase(LStrParam) = LowerCase(_TrueValue);
end;

function TPonyCoreParamField.AsCurrency: Currency;
begin
  Result:= AsFloat;
end;

function TPonyCoreParamField.AsDate: TDateTime;
var
  LStrParam: string;
  LFormat: TFormatSettings;
begin
  Result:= 0;
  LStrParam:= AsString;
  try
    if LStrParam <> EmptyStr then
    begin
      LFormat:= GetFormatSettings;
      Result:= StrToDate(Copy(LStrParam, 1, Length(_DateFormat)), LFormat);
    end;
  except
    on E: EConvertError do
      RaisePonyException(_InvalidFormatMessage, [_FieldName, LStrParam, 'date']);
  end;
end;

function TPonyCoreParamField.AsDateTime: TDateTime;
var
  LStrParam: string;
  LFormat: TFormatSettings;
begin
  Result:= 0;
  LStrParam:= AsString;
  try
    if LStrParam <> EmptyStr then
    begin
      LFormat:= GetFormatSettings;
      Result:= StrToDateTime(LStrParam, LFormat);
    end;
  except
    on E: EConvertError do
      RaisePonyException(_InvalidFormatMessage, [_FieldName, LStrParam, 'datetime']);
  end;
end;

function TPonyCoreParamField.AsExtended: Extended;
begin
  Result:= AsFloat;
end;

function TPonyCoreParamField.AsFloat: Double;
var
  LStrParam: string;
begin
  Result:= 0;
  LStrParam:= AsString;
  try
    if LStrParam <> EmptyStr then
    begin
      LStrParam:= LStrParam.Replace(',', FormatSettings.DecimalSeparator).Replace('.', FormatSettings.DecimalSeparator);
      Result:= StrToFloat(LStrParam);
    end;
  except
    on E: EConvertError do
      RaisePonyException(_InvalidFormatMessage, [_FieldName, LStrParam, 'numeric']);
  end;
end;

function TPonyCoreParamField.AsInt64: Int64;
var
  LStrParam: string;
begin
  Result:= 0;
  LStrParam:= AsString;
  try
    if LStrParam <> EmptyStr then
      Result:= StrToInt64(LStrParam);
  except
    on E: EConvertError do
      RaisePonyException(_InvalidFormatMessage, [_FieldName, LStrParam, 'int64']);
  end;
end;

function TPonyCoreParamField.AsInteger: Integer;
var
  LStrParam: string;
begin
  Result:= 0;
  LStrParam:= AsString;
  try
    if LStrParam <> EmptyStr then
      Result:= StrToInt(LStrParam);
  except
    on E: EConvertError do
      RaisePonyException(_InvalidFormatMessage, [_FieldName, LStrParam, 'integer']);
  end;
end;

function TPonyCoreParamField.AsISO8601DateTime: TDateTime;
var
  LStrParam: string;
begin
  Result:= 0;
  LStrParam:= AsString;
  if LStrParam <> EmptyStr then
  begin
    if not TryISO8601ToDate(LStrParam, Result) then
      RaisePonyException(_InvalidFormatMessage, [_FieldName, LStrParam, 'ISO8601 date']);
  end;
end;

function TPonyCoreParamField.AsStream: TStream;
begin
  Result:= nil;
  if _Contains then
  begin
    Result:= _Stream;
    if Assigned(Result) then
      Result.Position:= 0;
  end
  else
  if _Required then
    RaisePonyException(_RequiredMessage, [_FieldName]);
end;

function TPonyCoreParamField.AsString: string;
begin
  Result:= EmptyStr;
  if _Contains then
    Result:= _Value
  else
  if _Required then
    RaisePonyException(_RequiredMessage, [_FieldName]);
end;

function TPonyCoreParamField.AsTime: TTime;
var
  LStrParam: string;
  LFormat: TFormatSettings;
begin
  Result:= 0;
  LStrParam:= AsString;
  try
    if LStrParam <> EmptyStr then
    begin
      LFormat:= GetFormatSettings;
      Result:= StrToTime(Copy(LStrParam, 1, Length(_TimeFormat)), LFormat);
    end;
  except
    on E: EConvertError do
      RaisePonyException(_InvalidFormatMessage, [_FieldName, LStrParam, 'time']);
  end;
end;

function TPonyCoreParamField.DateFormat(const AValue: string): IPonyCoreParamField;
begin
  Result:= Self;
  _DateFormat:= AValue;
end;

function TPonyCoreParamField.GetFormatSettings: TFormatSettings;
begin
{$ifdef FPC}
  Result:= DefaultFormatSettings;
{$else}
  Result:= TFormatSettings.Create;
{$endif}
  if _DateFormat.IndexOf('-') > 0 then
    Result.DateSeparator:= '-';
  Result.ShortDateFormat:= _DateFormat;
  Result.ShortTimeFormat:= _TimeFormat;
end;

procedure TPonyCoreParamField.InitializeLhsBrackets(const AParams: {$ifdef FPC}specialize{$endif} TDictionary<string, string>; const AFieldName: string);
var
  LLhsBracketType: TLhsBracketsType;
begin
  _LhsBrackets:= TPonyCoreParamFieldLhsBrackets.Create;

  if TPonyCoreParamConfig.GetInstance.CheckLhsBrackets then
  begin
    for LLhsBracketType:= Low(TLhsBracketsType) to High(TLhsBracketsType) do
    begin
      if AParams.ContainsKey(_FieldName + LLhsBracketType.ToString) then
      begin
        _LhsBrackets.SetValue(LLhsBracketType, AParams.Items[_FieldName+LLhsBracketType.ToString]);
        _LhsBrackets.Types:= _LhsBrackets.Types + [LLhsBracketType];
      end;
    end;
  end;
end;

function TPonyCoreParamField.InvalidFormatMessage(const AValue: string): IPonyCoreParamField;
begin
  Result:= Self;
  _InvalidFormatMessage:= AValue;
end;

procedure TPonyCoreParamField.RaisePonyException(const AMessage: string; const Args: array of const);
begin
  RaisePonyException(Format(AMessage, Args));
end;

procedure TPonyCoreParamField.RaisePonyException(const AMessage: string);
var
  LException: EPonyException;
begin
  LException:= EPonyException.New.Status(THTTPStatus.BadRequest).Error(AMessage);
  LException.Message:= AMessage;
  raise LException;
end;

function TPonyCoreParamField.Required(const AValue: Boolean): IPonyCoreParamField;
begin
  Result:= Self;
  _Required:= AValue;
end;

function TPonyCoreParamField.Required: IPonyCoreParamField;
begin
  Result:= Self;
  _Required:= True;
end;

function TPonyCoreParamField.RequiredMessage(const AValue: string): IPonyCoreParamField;
begin
  Result:= Self;
  _RequiredMessage:= AValue;
end;

function TPonyCoreParamField.ReturnUTC(const AValue: Boolean): IPonyCoreParamField;
begin
  Result:= Self;
  _ReturnUTC:= AValue;
end;

procedure TPonyCoreParamField.SaveToFile(const AFileName: String);
var
  LMemoryStream: TMemoryStream;
begin
  if AsStream = nil then
    Exit;

  LMemoryStream:= TMemoryStream.Create;
  try
    LMemoryStream.LoadFromStream(AsStream);
    LMemoryStream.Position:= 0;
    LMemoryStream.SaveToFile(AFileName);
  finally
    LMemoryStream.Free;
  end;
end;

function TPonyCoreParamField.TimeFormat(const AValue: string): IPonyCoreParamField;
begin
  Result:= Self;
  _TimeFormat:= AValue;
end;

function TPonyCoreParamField.TrueValue(const AValue: string): IPonyCoreParamField;
begin
  Result:= Self;
  _TrueValue:= AValue;
end;

function TPonyCoreParamField.TryISO8601ToDate(const AValue: string; out Value: TDateTime): Boolean;
begin
  Value:= ISO8601ToDate(AValue, _ReturnUTC);
  Result:= True;
end;

{ End of TPonyCoreParamField implementation }

end.
