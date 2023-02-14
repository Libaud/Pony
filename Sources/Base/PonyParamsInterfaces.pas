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
unit PonyParamsInterfaces;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,
  {$ifdef FPC}
  Generics.Collections,
  {$else}
  System.Generics.Collections,
  {$endif}
  PonyCommons, PonyTypes;

type
  IPonyCoreParamFieldLhsBrackets = interface
      // Method's
      procedure SetValue(const AType: TLhsBracketsType; const AValue: string);
      function GetValue(const AType: TLhsBracketsType): string;
      // Access Method's
      function geteq: string;
      function getne: string;
      function getlt: string;
      function getlte: string;
      function getgt: string;
      function getgte: string;
      function getrange: string;
      function getlike: string;
      function GetTypes: TLhsBrackets;
      procedure SetTypes(aTypes: TLhsBrackets);
      // Properties
      property Eq: string read geteq;
      property Ne: string read getne;
      property Lt: string read getlt;
      property Lte: string read getlte;
      property Gt: string read getgt;
      property Gte: string read getgte;
      property Range: string read getrange;
      property Like: string read getlike;
      property Types: TLhsBrackets read GetTypes
                                   write SetTypes;
    end;

  IPonyCoreParamField = interface
      // Method's
      function DateFormat(const AValue: string): IPonyCoreParamField;
      function InvalidFormatMessage(const AValue: string): IPonyCoreParamField;
      function Required: IPonyCoreParamField;
      function Required(const AValue: Boolean): IPonyCoreParamField;
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
      // Access Method's
      function GetLhsBrackets: IPonyCoreParamFieldLhsBrackets;
      // properties
      property LhsBrackets:IPonyCoreParamFieldLhsBrackets read GetLhsBrackets;
    end;

  IPonyCoreParam = interface
      // Method's
      function Required(const AValue: Boolean): IPonyCoreParam;
      function Field(const AKey: string): IPonyCoreParamField;
      function ContainsKey(const AKey: string): Boolean;
      function ContainsValue(const AValue: string): Boolean;
      function ToArray: {$ifdef FPC}specialize{$endif} TArray< {$ifdef FPC}specialize{$endif} TPair<string, string> >;
      function TryGetValue(const AKey: string; var AValue: string): Boolean;
      function AddStream(const AKey: string; const AContent: TStream): IPonyCoreParam;
      // Access Method's
      function GetContent: TStrings;
      function GetCount: integer;
      function GetItem(const aKey: string): string;
      function GetDictionary: TPonylist;
      // Properties
      property Content: TStrings read GetContent;
      property Count: Integer read GetCount;
      property Items[const AKey: string]: string read GetItem;
      property Dictionary: TPonyList read GetDictionary;
    end;

implementation

end.

