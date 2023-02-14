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
unit PonyCoreRouterTree;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  {$ifdef FPC}
  SysUtils, Generics.Collections, fpHTTP, httpprotocol, RegExpr,
  {$else}
  System.SysUtils, System.NetEncoding, Web.HTTPApp, System.Generics.Collections, System.RegularExpressions,
  {$endif}
  PonyRequest, PonyResponse, {PonyProc,} PonyCommons, PonyCallback, PonyRouterTreeInterfaces,
  PonyRequestInterfaces, PonyResponseInterfaces;

type
  PPonyRouterTree = ^TPonyRouterTree;

  TPonyRouterTree = class(IPonyRouterTree)
  strict private
    // Member's
    FPrefix: string;
    FIsInitialized: Boolean;
    // Method's
    function GetQueuePath(APath: string; const AUsePrefix: Boolean = True): {$ifdef FPC}specialize{$endif} TQueue<string>;
    function ForcePath(const APath: string): TPonyRouterTree;
  private
    // Member's
    _RefCount: integer;
    FPart: string;
    FTag: string;
    FIsParamsKey: Boolean;
    FRouterRegex: string;
    FIsRouterRegex: Boolean;
    FMiddleware: {$ifdef FPC}specialize{$endif} TList<TPonyCallback>;
    FRegexedKeys: {$ifdef FPC}specialize{$endif} TList<string>;
    oObjCallBacks: {$ifdef FPC}specialize{$endif} TObjectDictionary<TMethodType, {$ifdef FPC}specialize{$endif} TList<TPonyObjCallback>>;
    oCallBacks: {$ifdef FPC}specialize{$endif} TObjectDictionary<TMethodType, {$ifdef FPC}specialize{$endif} TList<TPonyCallback>>;
    FRoute: {$ifdef FPC}specialize{$endif} TObjectDictionary<string, TPonyRouterTree>;
    // Method's
    procedure RegisterInternal(const aHTTPType: TMethodType;
                               var vPath: {$ifdef FPC}specialize{$endif} TQueue<string>;
                               const aCallback: TPonyObjCallback);
    procedure RegisterInternal(const aHTTPType: TMethodType;
                               var vPath: {$ifdef FPC}specialize{$endif} TQueue<string>;
                               const aCallback: TPonyCallback);
    procedure RegisterMiddlewareInternal(var vPath: {$ifdef FPC}specialize{$endif} TQueue<string>;
                                         const aMiddleware: TPonyCallback);
    function ExecuteInternal(const aPath: {$ifdef FPC}specialize{$endif} TQueue<string>;
                             const aHTTPType: TMethodType;
                             const aRequest: TPonyRequest;
                             const aResponse: TPonyResponse;
                             const aIsGroup: Boolean = False): Boolean;
    function CallNextPath(var vPath: {$ifdef FPC}specialize{$endif} TQueue<string>;
                          const AHTTPType: TMethodType;
                          const ARequest: IPonyRequest;
                          const AResponse: IPonyResponse): Boolean;
    function HasNext(const vMethod: TMethodType;
                     const aPaths: {$ifdef FPC}specialize{$endif} TArray<string>;
                     aIndex: Integer = 0): Boolean;
  public
    // Constructor's and destructor's
    constructor Create;
    destructor Destroy; override;
    // Interfaces Implementation Method's
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$else}const{$endif} iid : tguid;
                            out obj) : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
    // Method's
    function CreateRouter(const aPath: string): IPonyRouterTree;
    function GetPrefix: string;
    procedure Prefix(const aPrefix: string);
    procedure RegisterRoute(const aHTTPType: TMethodType;
                            const aPath: string;
                            const ACallback: TPonyObjCallback);
    procedure RegisterRoute(const aHTTPType: TMethodType;
                            const aPath: string;
                            const aCallback: TPonyCallback);
    procedure RegisterMiddleware(const aPath: string;
                                 const aMiddleware: TPonyCallback); overload;
    procedure RegisterMiddleware(const aMiddleware: TPonyCallback); overload;
    function Execute(const aRequest: IPonyRequest;
                     const aResponse: IPonyResponse): Boolean;
  end;

implementation

uses
  {PonyException,} PonyCoreRouterTreeNextCaller;

{ TPonyRouterTree implementation }

// Constructor's and destructor's

constructor TPonyRouterTree.Create;
begin
  FMiddleware:= {$ifdef FPC}specialize{$endif} TList<TPonyCallback>.Create;
  FRoute:= {$ifdef FPC}specialize{$endif} TObjectDictionary<string, TPonyRouterTree>.Create([doOwnsValues]);
  FRegexedKeys:= {$ifdef FPC}specialize{$endif} TList<string>.Create;
  oObjCallBacks:= {$ifdef FPC}specialize{$endif} TObjectDictionary < TMethodType, {$ifdef FPC}specialize{$endif} TList < TPonyObjCallback >>.Create([doOwnsValues]);
  oCallBacks:= {$ifdef FPC}specialize{$endif} TObjectDictionary < TMethodType, {$ifdef FPC}specialize{$endif} TList < TPonyCallback >>.Create([doOwnsValues]);
  FPrefix:= '';
  FIsRouterRegex:= False;
end;

destructor TPonyRouterTree.Destroy;
begin
  FMiddleware.Free;
  FreeAndNil(FRoute);
  FRegexedKeys.Clear;
  FRegexedKeys.Free;
  oCallBacks.Free;
  oObjCallBacks.Destroy;
  inherited;
end;

// Interfaces Implementation Method's

function TPonyRouterTree.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$else}const{$endif} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  if GetInterface(IID,Obj) then
	Result:= S_OK
  else
	Result:= E_NOINTERFACE;
end;

function TPonyRouterTree._AddRef : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  Result:= InterlockedIncrement(_RefCount);
end;

function TPonyRouterTree._Release : longint;{$IFNDEF WINDOWS}cdecl{$else}stdcall{$endif};
begin
  Result:= InterlockedDecrement(_RefCount);
  {if Result = 0 then
	Self.Destroy;}
end;

// Private Method's

// Strict Private Method's

function TPonyRouterTree.GetQueuePath(APath: string; const AUsePrefix: Boolean = True): {$ifdef FPC}specialize{$endif} TQueue<string>;
var
  LPart: string;
  LSplitedPath: {$ifdef FPC}specialize{$endif} TArray<string>;
begin
  Result:= {$ifdef FPC}specialize{$endif} TQueue<string>.Create;
  if AUsePrefix then
    if not APath.StartsWith('/') then
      APath:= (FPrefix + '/' + APath)
    else
      APath:= (FPrefix + APath);
  LSplitedPath:= APath.Split(['/']);
  for LPart in LSplitedPath do
  begin
    if (Result.Count > 0) and LPart.IsEmpty then
      Continue;
    Result.Enqueue(LPart);
  end;
end;

function TPonyRouterTree.ForcePath(const APath: string): TPonyRouterTree;
begin
  if not FRoute.TryGetValue(APath, Result) then
  begin
    Result:= TPonyRouterTree.Create;
    FRoute.Add(APath, Result);
  end;
end;

// Private Method's

procedure TPonyRouterTree.RegisterInternal(const aHTTPType: TMethodType;
                                           var vPath: {$ifdef FPC}specialize{$endif} TQueue<string>;
                                           const aCallback: TPonyObjCallback);
var
  LNextPart: string;
  LCallbacks: {$ifdef FPC}specialize{$endif} TList<TPonyObjCallback>;
  LForceRouter: TPonyRouterTree;
begin
  if not FIsInitialized then
  begin
    FPart:= vPath.Dequeue;

    FIsParamsKey:= FPart.StartsWith(':');
    FTag:= FPart.Substring(1, Length(FPart) - 1);

    FIsRouterRegex:= FPart.StartsWith('(') and FPart.EndsWith(')');
    FRouterRegex:= FPart;

    FIsInitialized:= True;
  end
  else
    vPath.Dequeue;

  if vPath.Count = 0 then
  begin
    if not oObjCallBacks.TryGetValue(aHTTPType, LCallbacks) then
    begin
      LCallbacks:= {$ifdef FPC}specialize{$endif} TList<TPonyObjCallback>.Create;
      oObjCallBacks.Add(aHTTPType, LCallbacks);
    end;
    LCallbacks.Add(aCallback)
  end;
  if vPath.Count > 0 then
  begin
    LNextPart:= vPath.Peek;

    LForceRouter:= ForcePath(LNextPart);

    LForceRouter.RegisterInternal(aHTTPType, vPath, aCallback);
    if LForceRouter.FIsParamsKey or LForceRouter.FIsRouterRegex then
      FRegexedKeys.Add(LNextPart);
  end;
end;

procedure TPonyRouterTree.RegisterInternal(const aHTTPType: TMethodType; var vPath: {$ifdef FPC}specialize{$endif} TQueue<string>; const aCallback: TPonyCallback);
var
  LNextPart: string;
  LCallbacks: {$ifdef FPC}specialize{$endif} TList<TPonyCallback>;
  LForceRouter: TPonyRouterTree;
begin
  if not FIsInitialized then
  begin
    FPart:= vPath.Dequeue;
    FIsParamsKey:= FPart.StartsWith(':');
    FTag:= FPart.Substring(1, Length(FPart) - 1);
    FIsRouterRegex:= FPart.StartsWith('(') and FPart.EndsWith(')');
    FRouterRegex:= FPart;
    FIsInitialized:= True;
  end
  else
    vPath.Dequeue;
  if vPath.Count = 0 then
  begin
    if not oCallBacks.TryGetValue(aHTTPType, LCallbacks) then
    begin
      LCallbacks:= {$ifdef FPC}specialize{$endif} TList<TPonyCallback>.Create;
      oCallBacks.Add(aHTTPType, LCallbacks);
    end;
    LCallbacks.Add(aCallback)
  end;
  if vPath.Count > 0 then
  begin
    LNextPart:= vPath.Peek;
    LForceRouter:= ForcePath(LNextPart);
    LForceRouter.RegisterInternal(aHTTPType, vPath, aCallback);
    if LForceRouter.FIsParamsKey or LForceRouter.FIsRouterRegex then
      FRegexedKeys.Add(LNextPart);
  end;
end;

function TPonyRouterTree.ExecuteInternal(const aPath: {$ifdef FPC}specialize{$endif} TQueue<string>; const aHTTPType: TMethodType; const aRequest: TPonyRequest;
                                         const aResponse: TPonyResponse; const aIsGroup: Boolean = False): Boolean;
var
  oNextCaller: TNextCaller;
  _Found: Boolean;
begin
  _Found:= False;
  oNextCaller:= TNextCaller.Create;
  try
    oNextCaller.SetCallback(oCallBacks);
    oNextCaller.SetPath(aPath);
    oNextCaller.SetHTTPType(aHTTPType);
    oNextCaller.SetRequest(aRequest);
    oNextCaller.SetResponse(aResponse);
    oNextCaller.SetIsGroup(aIsGroup);
    oNextCaller.SetMiddleware(FMiddleware);
    oNextCaller.SetTag(FTag);
    oNextCaller.SetIsParamsKey(FIsParamsKey);
    oNextCaller.SetOnCallNextPath(@CallNextPath);
    oNextCaller.SetFound(_Found);
    oNextCaller.Init;
    oNextCaller.Next;
  finally
    oNextCaller.Free;
    Result:= _Found;
  end;
end;

procedure TPonyRouterTree.RegisterMiddlewareInternal(var vPath: {$ifdef FPC}specialize{$endif} TQueue<string>; const aMiddleware: TPonyCallback);
begin
  vPath.Dequeue;
  if vPath.Count = 0 then
    FMiddleware.Add(aMiddleware)
  else
    ForcePath(vPath.Peek).RegisterMiddlewareInternal(vPath, aMiddleware);
end;

// Protected Method's

// Public Method's

procedure TPonyRouterTree.RegisterRoute(const aHTTPType: TMethodType; const aPath: string; const ACallback: TPonyObjCallback);
var
  LPathChain: {$ifdef FPC}specialize{$endif} TQueue<string>;
begin
  LPathChain:= GetQueuePath(aPath);
  try
    RegisterInternal(aHTTPType, LPathChain, ACallback);
  finally
    LPathChain.Free;
  end;
end;

procedure TPonyRouterTree.RegisterRoute(const aHTTPType: TMethodType; const aPath: string; const aCallback: TPonyCallback);
var
  LPathChain: {$ifdef FPC}specialize{$endif} TQueue<string>;
begin
  LPathChain:= GetQueuePath(aPath);
  try
    RegisterInternal(aHTTPType, LPathChain, aCallback);
  finally
    LPathChain.Free;
  end;
end;

function TPonyRouterTree.CallNextPath(var vPath: {$ifdef FPC}specialize{$endif} TQueue<string>;
                                      const AHTTPType: TMethodType;
                                      const ARequest: IPonyRequest;
                                      const AResponse: IPonyResponse): Boolean;
var
  LCurrent, LKey: string;
  LAcceptable: TPonyRouterTree;
  LFound, LIsGroup: Boolean;
  LPathOrigin: {$ifdef FPC}specialize{$endif} TQueue<string>;
begin
  LIsGroup:= False;
  LPathOrigin:= vPath;
  LCurrent:= vPath.Peek;
  LFound:= FRoute.TryGetValue(LCurrent, LAcceptable);
  if (not LFound) then
  begin
    LFound:= FRoute.TryGetValue(EmptyStr, LAcceptable);
    if (LFound) then
      vPath:= LPathOrigin;
    LIsGroup:= LFound;
  end;
  if (not LFound) and (FRegexedKeys.Count > 0) then
  begin
    for LKey in FRegexedKeys do
    begin
      FRoute.TryGetValue(LKey, LAcceptable);
      if LAcceptable.HasNext(AHTTPType, vPath.ToArray) then
      begin
        LFound:= LAcceptable.ExecuteInternal(vPath, AHTTPType, ARequest as TPonyRequest, AResponse as TPonyResponse);
        Break;
      end;
    end;
  end
  else if LFound then
    LFound:= LAcceptable.ExecuteInternal(vPath, AHTTPType, ARequest as TPonyRequest, AResponse as TPonyResponse, LIsGroup);
  Result:= LFound;
end;

function TPonyRouterTree.Execute(const aRequest: IPonyRequest; const aResponse: IPonyResponse): Boolean;
var
  LQueue: {$ifdef FPC}specialize{$endif} TQueue<string>;
begin
  LQueue:= GetQueuePath({$ifdef FPC}aRequest.RawWebRequest.PathInfo{$else}ARequest.RawWebRequest.RawPathInfo{$endif}, False);
  try
    Result:= ExecuteInternal(LQueue, {$ifdef FPC}
                                      StringCommandToMethodType(aRequest.RawWebRequest.Method)
                                      {$else}
                                      ARequest.RawWebRequest.MethodType
                                      {$endif},
                                      aRequest as TPonyRequest, aResponse as TPonyResponse);
  finally
    LQueue.Free;
  end;
end;

function TPonyRouterTree.CreateRouter(const aPath: string): IPonyRouterTree;
begin
  Result:= ForcePath(aPath);
end;

procedure TPonyRouterTree.Prefix(const aPrefix: string);
begin
  FPrefix:= '/' + aPrefix.Trim(['/']);
end;

function TPonyRouterTree.GetPrefix: string;
begin
  Result:= FPrefix;
end;

function TPonyRouterTree.HasNext(const vMethod: TMethodType; const aPaths: {$ifdef FPC}specialize{$endif} TArray<string>; aIndex: Integer = 0): Boolean;
var
  LNext, LKey: string;
  LNextRoute: TPonyRouterTree;
begin
  Result:= False;
  if (Length(aPaths) <= aIndex) then
    Exit(False);
  if (Length(aPaths) - 1 = aIndex) and ((aPaths[aIndex] = FPart) or (FIsParamsKey)) then
    Exit(oCallBacks.ContainsKey(vMethod) or (vMethod = mtAny));

  {$IFNDEF FPC}
  if FIsRouterRegex then
  begin
    Result:= TRegEx.IsMatch(APaths[AIndex], Format('^%s$', [FRouterRegex]));
    Exit;
  end;
  {$endif}

  LNext:= aPaths[aIndex + 1];
  Inc(aIndex);
  if FRoute.TryGetValue(LNext, LNextRoute) then
  begin
    Result:= LNextRoute.HasNext(vMethod, aPaths, aIndex);
  end
  else
  begin
    for LKey in FRegexedKeys do
    begin
      if FRoute.Items[LKey].HasNext(vMethod, aPaths, aIndex) then
        Exit(True);
    end;
  end;
end;

procedure TPonyRouterTree.RegisterMiddleware(const aMiddleware: TPonyCallback);
begin
  FMiddleware.Add(aMiddleware);
end;

procedure TPonyRouterTree.RegisterMiddleware(const aPath: string; const aMiddleware: TPonyCallback);
var
  LPathChain: {$ifdef FPC}specialize{$endif} TQueue<string>;
begin
  LPathChain:= GetQueuePath(aPath);
  try
    RegisterMiddlewareInternal(LPathChain, aMiddleware);
  finally
    LPathChain.Free;
  end;
end;

end.
