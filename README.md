<a href="https://github.com/Libaud/Pony">Pony</a> is a fork of Horse framework that would be more easier to implement Web (daemon/services, CGI/Fast...) application with connection with REST Full object for example.
It respects the philosophies of components/generics architecture in Delphi and Free Pascal (VCL or LCL implementation). The logic of implementation of packages with RAD dev tools as Delphi and others. The object is to made more easy and efficient rapid web application development. Use the base of architecture of Horse with amelioration. Packages structuration, easy package use in projects and so on.

## ⚙️ Installation

Download the framework package and decompress it in a dedicated directory. We preconize to use an other dedicated directory for compiled files (*.o, *.ppu...) for your developpement packages, frameworks dans so.


## ⚡️ Quickstart Delphi/Lazarus or Typhon

uses
  Pony, PonyRequestInterfaces, PonyResponseInterfaces, PonyProc;

procedure Treat(aReq: IPonyRequest; aRes: IPonyResponse; aNext: TNextProc);
begin
  aRes.Send('pong');
end;

var
  oPony: TPony;

begin
  oPony:= TPony.Create;
  try
    oPony.Get('/ping', @Treat);
    oPony.Listen;
  finally
    oPony.Destroy;
  end;
end.

```


##  License

`Pony` is free and open-source software licensed under the [MIT License](https://github.com/Libaud/pony/). 
