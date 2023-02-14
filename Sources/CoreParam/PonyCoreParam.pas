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
unit PonyCoreParam;

//{$ifdef FPC}
//  {$MODE DELPHI}{$H+}
//{$endif}
{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
{$ifdef FPC}
  SysUtils, Classes, DateUtils, Generics.Collections, fpHTTP, HTTPDefs,
{$else}
  System.SysUtils, System.Classes, System.DateUtils, System.Generics.Collections,
{$endif}
  PonyParamsInterfaces, PonyCoreParamField, PonyTypes;

type
  TPonyCoreParam = class (IPonyCoreParam)
  private
    // Members
    _RefCount: integer;
    FParams: TPonyList;
    FFiles: {$ifdef FPC}specialize{$endif} TDictionary<String, TStream>;
    FFields: {$ifdef FPC}specialize{$endif} TDictionary<string, TPonyCoreParamField>;
    FContent: TStrings;
    FRequired: Boolean;
    function GetItem(const AKey: string): string;
    function GetDictionary: TPonyList;
    function GetCount: Integer;
    function GetContent: TStrings;
    function AsString(const AKey: string): string;
    procedure ClearFields;

    function NewField(const AKey: String): IPonyCoreParamField;
  public
    // Interfaces Implementation Method's
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$else}const{$endif} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
    function Required(const AValue: Boolean): IPonyCoreParam;
    function Field(const AKey: string): IPonyCoreParamField;
    function ContainsKey(const AKey: string): Boolean;
    function ContainsValue(const AValue: string): Boolean;
    function ToArray: {$ifdef FPC}specialize{$endif} TArray< {$ifdef FPC}specialize{$endif} TPair<string, string> >;
    function TryGetValue(const AKey: string; var AValue: string): Boolean;
    property Content: TStrings read GetContent;
    property Count: Integer read GetCount;
    property Items[const AKey: string]: string read GetItem; default;
    property Dictionary: TPonyList read GetDictionary;

    function AddStream(const AKey: string; const AContent: TStream): IPonyCoreParam;
    constructor Create(const AParams: TPonyList);
    destructor Destroy; override;
  end;

implementation

uses
  PonyCoreParamConfig;

// Interfaces Implementation Method's

function TPonyCoreParam.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$else}const{$endif} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  if GetInterface(IID,Obj) then
	Result:= S_OK
  else
	Result:= E_NOINTERFACE;
end;

function TPonyCoreParam._AddRef : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  Result:= InterlockedIncrement(_RefCount);
end;

function TPonyCoreParam._Release : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  Result:= InterlockedDecrement(_RefCount);
  {if Result = 0 then
	Self.Destroy;}
end;

function TPonyCoreParam.ContainsKey(const AKey: string): Boolean;
var
  LKey: string;
begin
  Result:= False;
  for LKey in FParams.Keys do
  begin
    if AnsiCompareText(LKey, AKey) = 0 then
      Exit(True);
  end;
end;

function TPonyCoreParam.ContainsValue(const AValue: string): Boolean;
begin
  Result:= FParams.ContainsValue(AValue);
end;

constructor TPonyCoreParam.Create(const AParams: TPonyList);
begin
  FParams:= AParams;
  FRequired:= False;
end;

destructor TPonyCoreParam.Destroy;
begin
  FParams.Free;
  FContent.Free;
  ClearFields;
  if Assigned(FFiles) then
    FFiles.Free;
  inherited;
end;

function TPonyCoreParam.Field(const AKey: string): IPonyCoreParamField;
var
  LFieldName: string;
begin
  if not Assigned(FFields) then
    FFields:= {$ifdef FPC}specialize{$endif} TDictionary<string, TPonyCoreParamField>.Create;

  LFieldName:= AKey.ToLower;
  if FFields.ContainsKey(LFieldName) then
    Exit(FFields.Items[LFieldName]);

  Result:= NewField(AKey);
  try
    Result
      .Required(FRequired)
      .DateFormat(TPonyCoreParamConfig.GetInstance.DateFormat)
      .InvalidFormatMessage(TPonyCoreParamConfig.GetInstance.InvalidFormatMessage)
      .RequiredMessage(TPonyCoreParamConfig.GetInstance.RequiredMessage)
      .ReturnUTC(TPonyCoreParamConfig.GetInstance.ReturnUTC)
      .TimeFormat(TPonyCoreParamConfig.GetInstance.TimeFormat)
      .TrueValue(TPonyCoreParamConfig.GetInstance.TrueValue);

    FFields.AddOrSetValue(LFieldName, Result as TPonyCoreParamField);
  except
    (Result as TPonyCoreParamField).Free;
    raise;
  end;
end;

function TPonyCoreParam.AddStream(const AKey: string; const AContent: TStream): IPonyCoreParam;
begin
  Result:= Self;
  if not Assigned(FFiles) then
    FFiles:= {$ifdef FPC}specialize{$endif} TDictionary<String, TStream>.Create;

  FFiles.AddOrSetValue(AKey, AContent);
end;

function TPonyCoreParam.AsString(const AKey: string): string;
var
  LKey: string;
begin
  Result:= EmptyStr;
  for LKey in FParams.Keys do
  begin
    if AnsiCompareText(LKey, AKey) = 0 then
      Exit(FParams.Items[LKey]);
  end;
end;

procedure TPonyCoreParam.ClearFields;
var
  LKey: string;
begin
  if Assigned(FFields) then
  begin
    for LKey in FFields.Keys do
      FFields.Items[LKey].Free;

    FFields.Free;
  end;
end;

function TPonyCoreParam.GetContent: TStrings;
var
  LKey: string;
begin
  if not Assigned(FContent) then
  begin
    FContent:= TstringList.Create;
    for LKey in FParams.Keys do
      FContent.Add(Format('%s=%s', [LKey, FParams[LKey]]));
  end;
  Result:= FContent;
end;

function TPonyCoreParam.GetCount: Integer;
begin
  Result:= FParams.Count;
end;

function TPonyCoreParam.GetItem(const AKey: string): string;
var
  LKey: string;
begin
  for LKey in FParams.Keys do
  begin
    if AnsiCompareText(LKey, AKey) = 0 then
      Exit(FParams[LKey]);
  end;
  Result:= EmptyStr;
end;

function TPonyCoreParam.NewField(const AKey: String): IPonyCoreParamField;
var
  LKey: String;
begin
  if Assigned(FFiles) then
  begin
    for LKey in FFiles.Keys do
    begin
      if AnsiSameText(LKey, AKey) then
      begin
        Result:= TPonyCoreParamField.Create(FFiles.Items[LKey], AKey);
        Exit;
      end;
    end;
  end;

  Result:= TPonyCoreParamField.create(FParams, AKey);
end;

function TPonyCoreParam.Required(const AValue: Boolean): IPonyCoreParam;
begin
  Result:= Self;
  FRequired:= AValue;
end;

function TPonyCoreParam.GetDictionary: TPonyList;
begin
  Result:= FParams;
end;

function TPonyCoreParam.ToArray: {$ifdef FPC}specialize{$endif} TArray< {$ifdef FPC}specialize{$endif} TPair<string, string> >;
begin
  Result:= FParams.ToArray;
end;

function TPonyCoreParam.TryGetValue(const AKey: string; var AValue: string): Boolean;
begin
  Result:= ContainsKey(AKey);
  if Result then
    AValue:= AsString(AKey);
end;

end.
