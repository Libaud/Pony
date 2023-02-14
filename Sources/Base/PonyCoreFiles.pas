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
unit PonyCoreFiles;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
{$ifdef FPC}
  SysUtils, Classes
{$else}
  System.SysUtils, System.Classes
{$endif};

type
  TPonyCoreFile = class
  private
    _FileName: string;
    _Name: string;
    oFileStream: TStream;
    _FreeContentStream: Boolean;
    _ContentType: string;
  public
    constructor Create(const aFileName: string);
    destructor Destroy; override;
    function ContentType: string;
    function ContentStream: TStream;
    function Size: Int64;
    property Name: string read _Name;
    property FreeContentStream: Boolean read _FreeContentStream
                                        write _FreeContentStream;
  end;

implementation

uses PonyMime;

constructor TPonyCoreFile.Create(const aFileName: string);
begin
  if aFileName = EmptyStr then
    raise Exception.Create('Invalid FileName');

  if not FileExists(aFileName) then
    raise Exception.Create('File not exist');

  _FileName:= aFileName;
  _Name:= ExtractFileName(_FileName);
  _FreeContentStream:= True;
  _ContentType:= TPonyMimeTypes.GetFileType(_FileName);
end;

destructor TPonyCoreFile.Destroy;
begin
  if _FreeContentStream then
    oFileStream.Free;
  inherited;
end;

function TPonyCoreFile.ContentType: string;
begin
  Result:= _ContentType;
end;

function TPonyCoreFile.ContentStream: TStream;
begin
  if not Assigned(oFileStream) then
    oFileStream:= TFileStream.Create(_FileName, fmOpenRead or fmShareDenyWrite);
  Result:= oFileStream;
end;

function TPonyCoreFile.Size: Int64;
begin
  Result:= ContentStream.Size;
end;

end.
