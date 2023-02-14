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
unit PonySession;

//{$ifdef FPC}
//  {$MODE DELPHI}{$H+}
//{$endif}
{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
{$ifdef FPC}
  SysUtils, Generics.Collections;
{$else}
  System.SysUtils, System.Generics.Collections;
{$endif}

type
  TSession = class
  end;

  TSessionClass = class of TSession;

  TPonySessions = class
  private
    FSessions: {$ifdef FPC}specialize{$endif} TObjectDictionary<TSessionClass, TSession>;
    function GetSession(const ASessionClass: TSessionClass): TSession; overload;
    function GetObject(const ASessionClass: TSessionClass): TObject; overload;
  public
    {$ifdef FPC}generic{$endif} function TryGetSession<T: class>(out ASession: T): Boolean;
    function Contains(const ASessionClass: TSessionClass): Boolean;
    function SetSession(const ASessionClass: TSessionClass; const AInstance: TSession): TPonySessions;
    property Session[const ASessionClass: TSessionClass]: TSession read GetSession;
    property &Object[const ASessionClass: TSessionClass]: TObject read GetObject;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TPonySessions.Create;
begin
  FSessions:= {$ifdef FPC}specialize{$endif} TObjectDictionary<TSessionClass, TSession>.Create([doOwnsValues]);
end;

destructor TPonySessions.Destroy;
begin
  FSessions.Free;
  inherited Destroy;
end;

function TPonySessions.GetObject(const ASessionClass: TSessionClass): TObject;
begin
  Result:= FSessions.Items[ASessionClass];
end;

function TPonySessions.GetSession(const ASessionClass: TSessionClass): TSession;
begin
  Result:= FSessions.Items[ASessionClass];
end;

function TPonySessions.SetSession(const ASessionClass: TSessionClass; const AInstance: TSession): TPonySessions;
begin
  Result:= Self;
  if not ASessionClass.InheritsFrom(AInstance.ClassType) then
    raise Exception.CreateFmt('SessionClass differs from of instance[%s].', [AInstance.ClassType.ClassName]);
  FSessions.AddOrSetValue(ASessionClass, AInstance);
end;

function TPonySessions.Contains(const ASessionClass: TSessionClass): Boolean;
begin
  Result:= FSessions.ContainsKey(ASessionClass);
end;

{$ifdef FPC}generic{$endif} function TPonySessions.TryGetSession<T>(out ASession: T): Boolean;
begin
  Result:= FSessions.TryGetValue(TSessionClass(T), TSession(ASession));
end;

end.
