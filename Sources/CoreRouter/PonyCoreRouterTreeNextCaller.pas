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
unit PonyCoreRouterTreeNextCaller;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
{$ifdef FPC}
  SysUtils, Generics.Collections, fpHTTP, httpprotocol,
{$else}
  System.NetEncoding, System.SysUtils, Web.HTTPApp, System.Generics.Collections,
{$endif}
  PonyCommons, PonyRequest, PonyResponse, PonyCallback;

type
  { TNextCaller }

  TNextCaller = class
  private
    FIndex: Integer;
    FIndexCallback: Integer;
    FPath: {$ifdef FPC}specialize{$endif} TQueue<string>;
    FHTTPType: TMethodType;
    FRequest: TPonyRequest;
    FResponse: TPonyResponse;
    FMiddleware: {$ifdef FPC}specialize{$endif} TList<TPonyCallback>;
    FCallBack: {$ifdef FPC}specialize{$endif} TObjectDictionary<TMethodType, {$ifdef FPC}specialize{$endif} TList<TPonyCallback>>;
    FCallNextPath: TCallNextPath;
    FIsGroup: Boolean;
    FTag: string;
    FIsParamsKey: Boolean;
    FFound: ^Boolean;
  public
    // Method's
    function Init: TNextCaller;
    function SetCallback(const aCallback: {$ifdef FPC}specialize{$endif} TObjectDictionary<TMethodType, {$ifdef FPC}specialize{$endif} TList<TPonyCallback>>): TNextCaller;
    function SetPath(const APath: {$ifdef FPC}specialize{$endif} TQueue<string>): TNextCaller;
    function SetHTTPType(const AHTTPType: TMethodType): TNextCaller;
    function SetRequest(const ARequest: TPonyRequest): TNextCaller;
    function SetResponse(const AResponse: TPonyResponse): TNextCaller;
    function SetIsGroup(const AIsGroup: Boolean): TNextCaller;
    function SetMiddleware(const AMiddleware: {$ifdef FPC}specialize{$endif} TList<TPonyCallback>): TNextCaller;
    function SetTag(const ATag: string): TNextCaller;
    function SetIsParamsKey(const AIsParamsKey: Boolean): TNextCaller;
    function SetOnCallNextPath(const ACallNextPath: TCallNextPath): TNextCaller;
    function SetFound(var AFound: Boolean): TNextCaller;
    procedure Next;
  end;

  { TNextCaller }

implementation

uses
  PonyException, PonyExceptionInterrupted;

{ TNextCaller }

function TNextCaller.Init: TNextCaller;
var
  LCurrent: string;
begin
  Result:= Self;
  if not FIsGroup then
    LCurrent:= FPath.Dequeue;
  FIndex:= -1;
  FIndexCallback:= -1;
  if FIsParamsKey then
    FRequest.Params.Dictionary.Add(FTag, {$ifdef FPC}HTTPDecode(LCurrent){$else}TNetEncoding.URL.Decode(LCurrent){$endif});
end;

procedure TNextCaller.Next;
var
  oCallbacks: {$ifdef FPC}specialize{$endif} TList<TPonyCallback>;
begin
  inc(FIndex);
  if (FMiddleware.Count > FIndex) then
  begin
    FFound^:= True;
    Self.FMiddleware.Items[FIndex](FRequest, FResponse, {$ifdef FPC}@{$endif}Next);
    if (FMiddleware.Count > FIndex) then
      Next;
  end
  else
  if (FPath.Count = 0) and assigned(FCallBack) then
  begin
    inc(FIndexCallback);
    if FCallBack.TryGetValue(FHTTPType, oCallbacks) then
    begin
      if (oCallbacks.Count > FIndexCallback) then
      begin
        try
          FFound^:= True;
          oCallbacks.Items[FIndexCallback](FRequest, FResponse, {$ifdef FPC}@{$endif}Next);
        except
          on E: Exception do
          begin
            if (not(E is EPonyCallbackInterrupted)) and
               (not(E is EPonyException)) and
               (FResponse.Status < Integer(THTTPStatus.BadRequest))
            then
              FResponse.Send('Internal Application Error').Status(THTTPStatus.InternalServerError);
            raise;
          end;
        end;
        Next;
      end;
    end
    else
    begin
      if FCallBack.Count > 0 then
      begin
        FFound^:= True;
        FResponse.Send('Method Not Allowed').Status(THTTPStatus.MethodNotAllowed);
      end
      else
        FResponse.Send('Not Found').Status(THTTPStatus.NotFound)
    end;
  end
  else
    FFound^:= FCallNextPath(FPath, FHTTPType, FRequest, FResponse);

  if not FFound^ then
    FResponse.Send('Not Found').Status(THTTPStatus.NotFound);
end;

function TNextCaller.SetCallback(const aCallback: {$ifdef FPC}specialize{$endif} TObjectDictionary < TMethodType, {$ifdef FPC}specialize{$endif} TList < TPonyCallback >> ): TNextCaller;
begin
  FCallBack:= aCallback;
  Result:= Self;
end;

function TNextCaller.SetFound(var AFound: Boolean): TNextCaller;
begin
  FFound:= @AFound;
  Result:= Self;
end;

function TNextCaller.SetHTTPType(const AHTTPType: TMethodType): TNextCaller;
begin
  FHTTPType:= AHTTPType;
  Result:= Self;
end;

function TNextCaller.SetIsGroup(const AIsGroup: Boolean): TNextCaller;
begin
  FIsGroup:= AIsGroup;
  Result:= Self;
end;

function TNextCaller.SetIsParamsKey(const AIsParamsKey: Boolean): TNextCaller;
begin
  FIsParamsKey:= AIsParamsKey;
  Result:= Self;
end;

function TNextCaller.SetMiddleware(const AMiddleware: {$ifdef FPC}specialize{$endif} TList<TPonyCallback>): TNextCaller;
begin
  FMiddleware:= AMiddleware;
  Result:= Self;
end;

function TNextCaller.SetOnCallNextPath(const ACallNextPath: TCallNextPath): TNextCaller;
begin
  FCallNextPath:= ACallNextPath;
  Result:= Self;
end;

function TNextCaller.SetPath(const APath: {$ifdef FPC}specialize{$endif} TQueue<string>): TNextCaller;
begin
  FPath:= APath;
  Result:= Self;
end;

function TNextCaller.SetRequest(const ARequest: TPonyRequest): TNextCaller;
begin
  FRequest:= ARequest;
  Result:= Self;
end;

function TNextCaller.SetResponse(const AResponse: TPonyResponse): TNextCaller;
begin
  FResponse:= AResponse;
  Result:= Self;
end;

function TNextCaller.SetTag(const ATag: string): TNextCaller;
begin
  FTag:= ATag;
  Result:= Self;
end;

{ End of TNextCaller implementation }

end.
