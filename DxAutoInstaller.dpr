program DxAutoInstaller;

{$APPTYPE CONSOLE}
{$R *.dres}

uses
  Windows,
  SysUtils,
  Vcl.Forms,
  MainFrm in 'Sources\MainFrm.pas' {MainForm},
  DxComponent in 'Sources\DxComponent.pas',
  DxComponentFactory in 'Sources\DxComponentFactory.pas',
  DxIDE in 'Sources\DxIDE.pas',
  DxInstaller in 'Sources\DxInstaller.pas',
  DxProfile in 'Sources\DxProfile.pas',
  DxProgress in 'Sources\DxProgress.pas' {DxProgressForm},
  DxQuantumTreeList in 'Sources\DxQuantumTreeList.pas' {DxResourceModule: TDataModule},
  DxUtils in 'Sources\DxUtils.pas',
  DxConsoleInstaller in 'Sources\DxConsoleInstaller.pas',
  DxBDSVersionsConsts in 'Sources\DxBDSVersionsConsts.pas',
  DxConsoleSwitchesConsts in 'Sources\DxConsoleSwitchesConsts.pas',
  DxInstallerExceptions in 'Sources\DxInstallerExceptions.pas';

{$R *.res}

begin
  if IsConsoleMode then
  try
    ExitCode := ExecuteConsoleInstallation;
  except
    on E: DxCiException do
    begin
      WriteLn(E.Message);
      ExitCode := E.HelpContext;
    end;
    on E: Exception do
    begin
      WriteLn(E.Message);
      ExitCode := 96;
    end
  End
  else
  begin
    ShowWindow(GetConsoleWindow, SW_HIDE);
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.Title := 'DxAutoInstaller';
    Application.CreateForm(TMainForm, MainForm);
  Application.Run;
  end;
end.
