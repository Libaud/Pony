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
unit PonyConstants;

//{$ifdef FPC}
//  {$MODE DELPHI}{$H+}
//{$endif}
{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

const
  DEFAULT_HOST = '0.0.0.0';
  DEFAULT_PORT = 9000;
  START_RUNNING = 'Server is runing on %s:%d';
  Pony_VERSION = '3.0.2';

implementation

end.
