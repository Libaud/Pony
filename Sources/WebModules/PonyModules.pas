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
unit PonyModules;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
{$ifdef FPC}
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb,
{$else}
  System.SysUtils, System.Classes, Web.HTTPApp,
{$endif}
  PonyCore{, PonyCommons};

type
  {$ifdef FPC}
  TPonyWebModule = class(TFPWebModule)
        procedure DoOnRequest(aRequest: TRequest; aResponse: TResponse; var aHandled: Boolean); override;
      {$else}
      TPonyWebModule = class(TWebModule)
      {$endif}
        procedure HandlerAction(const aSender: TObject; const aRequest: {$ifdef FPC}TRequest{$else}TWebRequest{$endif}; const aResponse: {$ifdef FPC}TResponse{$else}TWebResponse{$endif}; var vHandled: Boolean);
      private
        FPony: TPonyCore;
        class var FInstance: TPonyWebModule;
      public
        property Pony: TPonyCore read FPony write FPony;
        constructor Create(AOwner: TComponent); override;
        class function GetInstance: TPonyWebModule;
    end;

var
{$ifdef FPC}
  PonyWebModule: TPonyWebModule;
{$else}
  WebModuleClass: TComponentClass = TPonyWebModule;
{$endif}

implementation

uses PonyRequest, PonyResponse{, PonyException}, PonyExceptionInterrupted;

{$ifdef FPC}
  {$ifdef LAZARUSIDE}
  {$R *.lfm}
  {$else}
  {$R *.frm}
  {$endif}
{$else}
  {$R *.frm}
{$endif}

class function TPonyWebModule.GetInstance: TPonyWebModule;
begin
  Result:= FInstance;
end;

constructor TPonyWebModule.Create(AOwner: TComponent);
begin
{$ifdef FPC}
  inherited CreateNew(AOwner, 0);
{$else}
  inherited;
{$endif}
  FPony:= TPonyCore.GetInstance;
  FInstance:= Self;
end;

{$ifdef FPC}
procedure TPonyWebModule.DoOnRequest(aRequest: {$ifdef FPC}TRequest{$else}  TWebRequest {$endif}; aResponse: {$ifdef FPC}TResponse{$else}  TWebResponse {$endif}; var aHandled: Boolean);
begin
  HandlerAction(Self, aRequest, aResponse, aHandled);
end;
{$endif}

procedure TPonyWebModule.HandlerAction(const aSender: TObject; const aRequest: {$ifdef FPC}TRequest{$else}TWebRequest{$endif};
                                       const aResponse: {$ifdef FPC}TResponse{$else}TWebResponse{$endif}; var vHandled: Boolean);
var
  oRequest: TPonyRequest;
  oResponse: TPonyResponse;
begin
  vHandled:= True;
  oRequest:= TPonyRequest.Create(aRequest);
  oResponse:= TPonyResponse.Create(aResponse);
  try
    try
      FPony.Routes.Execute(oRequest, oResponse)
    except
      on E: Exception do
        if not E.InheritsFrom(EPonyCallbackInterrupted) then
          raise;
    end;
  finally
    { Todo : implementation }
    {if oRequest.Body<TObject> = oResponse.Content then}
    if oRequest.BodyAsObject = oResponse.Content then
      oResponse.Content(nil);
    oRequest.Free;
    oResponse.Free;
  end;
end;

{$ifdef FPC}
initialization
  RegisterHTTPModule(TPonyWebModule);
{$endif}

end.
