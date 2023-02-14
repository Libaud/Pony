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
unit PonyCallback;

//{$ifdef FPC}
//  {$MODE DELPHI}{$H+}
//{$endif}
{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  {$ifdef FPC}
  Generics.Collections, fpHTTP,
  {$else}
  Web.HTTPApp, System.Generics.Collections,
  {$endif}
  PonyRequestInterfaces, PonyResponseInterfaces, PonyProc, PonyCommons;

type
{$ifdef FPC}
  TPonyCallbackRequest = procedure(aReq: IPonyRequest);
  TPonyCallbackResponse = procedure(aRes: IPonyResponse);
  TPonyCallbackRequestResponse = procedure(aReq: IPonyRequest; aRes: IPonyResponse);
  TPonyObjCallback = procedure(aReq: IPonyRequest; aRes: IPonyResponse; aNext: TNextProc) of object;
  TPonyCallback = procedure(aReq: IPonyRequest; aRes: IPonyResponse; aNext: TNextProc);
  TCallNextPath = function(var aPath: {$ifdef FPC}specialize{$endif}TQueue<string>;
                           const aHTTPType: TMethodType;
                           const aRequest: IPonyRequest;
                           const AResponse: IPonyResponse): Boolean of object;
{$else}
  TPonyCallbackRequest = reference to procedure(aReq: IPonyRequest);
  TPonyCallbackResponse = reference to procedure(aRes: IPonyResponse);
  TPonyCallbackRequestResponse = reference to procedure(aReq: IPonyRequest; aRes: IPonyResponse);
  TPonyCallback = reference to procedure(aReq: IPonyRequest; aRes: IPonyResponse; aNext: TNextProc);
  TCallNextPath = reference to function(var aPath: TQueue<string>; const aHTTPType: TMethodType; const aRequest: IPonyRequest; const aResponse: IPonyResponse): Boolean;
{$endif}

implementation

end.
