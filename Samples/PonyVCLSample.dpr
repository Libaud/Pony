{=============================================================================
 Prject :
 Author : Frédéric Libaud
 Description :
 Features:

 =============================================================================
 History
 -----------------------------------------------------------------------------
 08/02/2022 - Forking code from Horse framework
 =============================================================================}
program PonyVCLSample;

{$MODE Delphi}

uses
  Vcl.Forms,
  Main.Form in 'src\Main.Form.pas' {FrmVCL};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmVCL, FrmVCL);
  Application.Run;
end.
