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
unit PonyResponseInterfaces;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,
  {$ifdef FPC}
  HTTPDefs,
  {$else}
  Web.HTTPApp,
  {$endif}
  PonyCommons;

type
  IPonyResponse = interface
      function Send(const aContent: string): IPonyResponse; overload;
      function Send(const aStream: TStream): IPonyResponse; overload;
      //{$ifdef FPC}generic{$endif} function Send<T{: class}>(aContent: T): IPonyResponse; overload;
      function RedirectTo(const aLocation: string): IPonyResponse;
      function RedirectTo(const aLocation: string; const aStatus: THTTPStatus): IPonyResponse;
      function Status(const aStatus: Integer): IPonyResponse;
      function Status(const aStatus: THTTPStatus): IPonyResponse;
      function SendFile(const aFileStream: TStream; const aFileName: string = ''; const aContentType: string = ''): IPonyResponse;
      function SendFile(const aFileName: string; const aContentType: string = ''): IPonyResponse;
      function Download(const aFileStream: TStream; const aFileName: string; const aContentType: string = ''): IPonyResponse;
      function Download(const aFileName: string; const aContentType: string = ''): IPonyResponse;
      function Render(const aFileStream: TStream; const aFileName: string): IPonyResponse;
      function Render(const aFileName: string): IPonyResponse;
      function Status: Integer;
      function AddHeader(const aName, AValue: string): IPonyResponse;
      function Content: TObject;
      function Content(const aContent: TObject): IPonyResponse;
      function ContentType(const aContentType: string): IPonyResponse;
      function RawWebResponse: {$ifdef FPC}TResponse{$else}TWebResponse{$endif};
    end;

implementation

end.

