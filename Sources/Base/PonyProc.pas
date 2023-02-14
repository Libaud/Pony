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
unit PonyProc;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

{$IF NOT DEFINED(FPC)}
uses
  System.SysUtils;
{$endif}

type
  TNextProc = {$ifdef FPC}
              procedure of object
              {$else}
              System.SysUtils.TProc
              {$endif};

  TProc = {$ifdef FPC}
          procedure()
          {$else}
          System.SysUtils.TProc
          {$endif};

  {$ifdef FPC}generic{$endif} TProcWT<T> = procedure(aArg: T);

implementation

end.
