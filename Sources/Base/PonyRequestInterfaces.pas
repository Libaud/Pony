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
unit PonyRequestInterfaces;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,
  {$ifdef FPC}
  Generics.Collections, HTTPDefs,
  {$else}
  System.Generics.Collections, Web.HTTPApp,
  {$endif}
  PonyParamsInterfaces, PonySession, PonyCommons;

type
  IPonyRequest = interface
      // Method's
      function Body: string; overload;
      function Body(const ABody: TObject): IPonyRequest; overload;
      function Session(const ASession: TObject): IPonyRequest;
      function Headers: IPonyCoreParam;
      function Query: IPonyCoreParam;
      function Params: IPonyCoreParam;
      function Cookie: IPonyCoreParam;
      function ContentFields: IPonyCoreParam;
      function MethodType: TMethodType;
      function RawWebRequest: {$ifdef FPC}TRequest{$else}TWebRequest{$endif};
      // Generics Method's
      //{$ifdef FPC}generic{$endif} function Body<T: class>: T;
      //{$ifdef FPC}generic{$endif} function Session<T: class>: T;
      // Access Method's
      function GetSessions: TPonySessions;
      // Properties
      property Sessions: TPonySessions read GetSessions;
    end;

implementation

end.

