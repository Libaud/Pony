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
unit PonyRttiHelper;

interface

uses
{$ifdef FPC}
  RTTI;
{$else}
  System.Rtti;
{$endif}

type
  TPonyRttiTypeHelper = class helper for TRttiType
  public
    {$IF NOT DEFINED(FPC)}
    function FieldValueAsObject(const AInstance: Pointer; const AFieldName: string): TObject;
    {$endif}
  end;

implementation

{$IF NOT DEFINED(FPC)}
function TPonyRttiTypeHelper.FieldValueAsObject(const AInstance: Pointer; const AFieldName: string): TObject;
var
  LField: TRttiField;
begin
  Result:= nil;
  LField:= GetField(AFieldName);
  if Assigned(LField) then
    Result:= LField.GetValue(AInstance).AsObject;
end;
{$endif}

end.
