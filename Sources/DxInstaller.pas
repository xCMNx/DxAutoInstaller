{*******************************************************}
{                                                       }
{          DxAutoInstaller Installer Classes            }
{                                                       }
{        http://www.delphier.com/DxAutoIntaller         }
{        Copyright(c) 2014 by faceker@gmail.com         }
{                                                       }
{*******************************************************}

unit DxInstaller;

interface

uses
  Forms,
  Classes,
  SysUtils,
  DxIDE,
  DxComponent,
  DxProfile,
  DxConsoleSwitchesConsts;

type
  TDxInstallOption = (dxioAddBrowsingPath, dxioNativeLookAndFeel, dxioCompileWin64Library, dxioInstallToCppBuilder, dxioMakeDebugDcu, dxioChangePackagePostfix, dxioAddPackageSuffix);
  TDxInstallOptions = set of TDxInstallOption;

  TDxThirdPartyComponent = (dxtpcIBX, dxtpcTeeChart, dxtpcFireDAC, dxtpcBDE);
  TDxThirdPartyComponents = set of TDxThirdPartyComponent;

  TDxInstallerState = (dxisNormal, dxisRunning, dxisStopped, dxisError);
  TDxInstallerAction = procedure(const IDEArray: TDxIDEArray) of object;

  TDxUpdateProgressEvent = procedure(IDE: TDxIDE; Component: TDxComponentProfile; const Task, Target: String) of object;
  TDxUpdateProgressStateEvent = procedure(const StateText: String) of object;
  IProgressBar = interface
    ['{C6CB1064-4F5F-4EBD-B4CC-ABD3AF3ACF92}']
    procedure SetMax(const Value: Integer);
    function GetMax: Integer;
    procedure SetPos(const Value: Integer);
    function GetPos: Integer;
    procedure StepIt;
    procedure SetSize(const Value: Double);
    function GetSize: Double;
    property Size: Double read GetSize write SetSize;
    property Max: Integer read GetMax write SetMax;
    property Pos: Integer read GetPos write SetPos;
  end;
  TDxProgressStartEvent = function(const Max: Integer; const Size: Double = 0.5): IProgressBar of object;
  TDxInstallerOption = (dxioConsoleMode, dxioCompileOnly);
  TDxInstallerOptions = set of TDxInstallerOption;

  TDxInstaller = class
  const
    DxEnvironmentVariableName = 'DXVCL';
  private
    FIDEs: TDxIDEs;
    FProfile: TDxProfile;
    FInstallFileDir: String;
    FComponents: array of TDxComponentList;
    FOptions: array of TDxInstallOptions;
    FThirdPartyComponents: array of TDxThirdPartyComponents;
    FState: TDxInstallerState;
    FRCTemplate: string;
    FOnUpdateProgress: TDxUpdateProgressEvent;
    FOnUpdateProgressState: TDxUpdateProgressStateEvent;
    FOnProgressStart: TDxProgressStartEvent;
    FComponentsVersion: Cardinal;
    FFindSourcePackage: boolean;
    FProgressBar: IProgressBar;
    FConsoleMode: boolean;
    FCompileOnly: boolean;
    FPackagePostfix: String;
    FInstallLibraryDir: String;
    FBPLPath: String;
    FDCPPath: String;
    procedure SetInstallFileDir(const Value: String);
    function GetComponents(IDE: TDxIDE): TDxComponentList;
    function GetOptions(IDE: TDxIDE): TDxInstallOptions;
    procedure SetOptions(IDE: TDxIDE; const Value: TDxInstallOptions);
    function GetThirdPartyComponents(IDE: TDxIDE): TDxThirdPartyComponents;
    procedure SetThirdPartyComponents(IDE: TDxIDE; const Value: TDxThirdPartyComponents);
    procedure DetectionThirdPartyComponents(IDE: TDxIDE);
    procedure SetState(const Value: TDxInstallerState);
    procedure Install(IDE: TDxIDE); overload;
    procedure InstallPackage(IDE: TDxIDE; const IDEPlatform: TDxIDEPlatform; Component: TDxComponent; Package: TDxPackage);
    procedure Uninstall(IDE: TDxIDE); overload;
    procedure UninstallPackage(IDE: TDxIDE; const IDEPlatform: TDxIDEPlatform; Component: TDxComponentProfile; const PackageBaseName: String);
    procedure UpdateProgress(IDE: TDxIDE; Component: TDxComponentProfile; const Task, Target: String);
    procedure UpdateProgressState(const StateText: String);
    function CreateProgress(const Max: Integer; const Size: Double = 0.5): IProgressBar;
    procedure CheckStoppedState();
    procedure SetFindSourcePackage(const Value: boolean);
    procedure RefreshComponentsAndOptions;
    function GetBplPath(IDE: TDxIDE; const IDEPlatform: TDxIDEPlatform): String;
    function GetDclPath(IDE: TDxIDE; const IDEPlatform: TDxIDEPlatform): String;
  public
    constructor Create(const Options: TDxInstallerOptions = []);
    destructor Destroy; override;
    function GetIdeComponentsHierarchy(IDE: TDxIDE): string;
    property IDEs: TDxIDEs read FIDEs;
    property Profile: TDxProfile read FProfile;
    property InstallFileDir: String read FInstallFileDir write SetInstallFileDir;
    property Components[IDE: TDxIDE]: TDxComponentList read GetComponents;
    property Options[IDE: TDxIDE]: TDxInstallOptions read GetOptions write SetOptions;
    property ThirdPartyComponents[IDE: TDxIDE]: TDxThirdPartyComponents read GetThirdPartyComponents write SetThirdPartyComponents;
    property State: TDxInstallerState read FState;
    property OnUpdateProgress: TDxUpdateProgressEvent read FOnUpdateProgress write FOnUpdateProgress;
    property OnUpdateProgressState: TDxUpdateProgressStateEvent read FOnUpdateProgressState write FOnUpdateProgressState;
    property OnOnStartProgress: TDxProgressStartEvent read FOnProgressStart write FOnProgressStart;
    property ComponentsVersion: Cardinal read FComponentsVersion;
    property FindSourcePackage: boolean read FFindSourcePackage write SetFindSourcePackage;
    property ConsoleMode: Boolean read FConsoleMode;
    property PackagePostfix: String read FPackagePostfix write FPackagePostfix;
    property InstallLibraryDir: String read FInstallLibraryDir write FInstallLibraryDir;
    property BPLPath: String read FBPLPath write FBPLPath;
    property DCPPath: String read FDCPPath write FDCPPath;
    property CompileOnly: boolean read FCompileOnly write FCompileOnly;
    procedure Install(const IDEArray: TDxIDEArray); overload;
    procedure Uninstall(const IDEArray: TDxIDEArray); overload;
    procedure Stop();
    procedure SearchNewPackages(List: TStringList);
    function GetInstallLibraryDir(const InstallFileDir: String; IDE: TDxIDE; const IDEPlatform: TDxIDEPlatform = Win32): String;
    function GetInstallSourcesDir(const InstallFileDir: String): String;
  end;

const
  DxInstallOptionNames: array[TDxInstallOption] of String = ('Add Browsing Path', 'Use Native Look and Feel as Default', 'Compile Win64 Library', 'Install to C++Builder', 'Make debug .dcu', 'Replace IDE version postfix to ' + UNIQUE_POSTFIX, 'Add to package file name IDE suffix');


procedure ApplyAppParams(const Installer: TDxInstaller);

implementation

uses
  DxComponentFactory,
  DxUtils,
  IOUtils,
  JclSysUtils,
  DxInstallerExceptions;

type
  TDummyProgress = class(TinterfacedObject, IProgressBar)
    procedure SetMax(const Value: Integer);
    procedure SetPos(const Value: Integer);
    procedure StepIt;
    procedure SetSize(const Value: Double);
    function GetSize: Double;
    function GetPos: Integer;
    function GetMax: Integer;
  end;

procedure ApplyAppParams(const Installer: TDxInstaller);
var
  TempSwithValue: String;
begin
  Installer.FindSourcePackage := dxFindCmdLineSwitch(dxisFindSourcePackage) or dxFindCmdLineSwitch(dxisReg) or dxFindCmdLineSwitch(dxisUnReg);
  if dxFindCmdLineSwitch(dxisSources, TempSwithValue) then
    Installer.InstallFileDir := TempSwithValue;
  if dxFindCmdLineSwitch(dxisPackagePostfix, TempSwithValue) then
  begin
    TempSwithValue := Trim(TempSwithValue);
    if TempSwithValue = EmptyStr then
      raise DxCiException.CreateHelp('Package postfix can not be empty.', dxiecEmptyPostfix);
    Installer.PackagePostfix := TempSwithValue;
  end;
  if dxFindCmdLineSwitch(dxisInstallDir, TempSwithValue) then
    Installer.InstallLibraryDir := TempSwithValue;
  if dxFindCmdLineSwitch(dxisBPLOutDir, TempSwithValue) then
    Installer.BPLPath := TempSwithValue;
  if dxFindCmdLineSwitch(dxisDCPOutDir, TempSwithValue) then
    Installer.DCPPath := TempSwithValue;
  Installer.CompileOnly := dxFindCmdLineSwitch(dxisCompileOnly);
end;

{ TDxInstaller }

constructor TDxInstaller.Create(const Options: TDxInstallerOptions);
const TEMP_RC_NAME = 'template.rc';
var
  I: Integer;
begin
  inherited Create;
  FIDEs := TDxIDEs.Create;
  for I := 0 to FIDEs.Count - 1 do FIDEs[I].OutputCallback := UpdateProgressState;
  FProfile := TDxProfile.Create;
  FInstallFileDir := EmptyStr;
  SetLength(FComponents, FIDEs.Count);
  SetLength(FOptions, FIDEs.Count);
  SetLength(FThirdPartyComponents, FIDEs.Count);
  for I := 0 to FIDEs.Count - 1 do DetectionThirdPartyComponents(FIDEs[I]);
  FState := dxisNormal;
  if not FileExists(TEMP_RC_NAME) then
    ExportResourceToFile(TEMP_RC_NAME, 'TEMP_RC', 'TXT');
  FRCTemplate := TFile.ReadAllText(TEMP_RC_NAME);
  FConsoleMode := dxioConsoleMode in Options;
  FCompileOnly := dxioCompileOnly in Options;
  FPackagePostfix := UNIQUE_POSTFIX;
end;

function TDxInstaller.CreateProgress(const Max: Integer; const Size: Double): IProgressBar;
begin
  if Assigned(FOnProgressStart) then
    Exit(FOnProgressStart(Max, Size));
  Result := TDummyProgress.Create;
end;

destructor TDxInstaller.Destroy;
var
  I: Integer;
begin
  for I := Low(FComponents) to High(FComponents) do FreeAndNil(FComponents[I]);
  FProfile.Free;
  FIDEs.Free;
  inherited;
end;

procedure TDxInstaller.DetectionThirdPartyComponents(IDE: TDxIDE);
var
  I: Integer;
  FileName: String;
  Components: TDxThirdPartyComponents;
begin
  Components := [];
  for I := 0 to IDE.IdePackages.Count - 1 do begin
    FileName := IDE.IdePackages.PackageFileNames[I];
    if (not(dxtpcIBX in Components)) and (Pos('\dclib', FileName) > 0) then Include(Components, dxtpcIBX)
    else if (not(dxtpcTeeChart in Components)) and (Pos('\dcltee', FileName) > 0) then Include(Components, dxtpcTeeChart)
    else if (not(dxtpcFireDAC in Components)) and ((Pos('\dclFireDAC', FileName) > 0) or (Pos('\AnyDAC_', FileName) > 0)) then Include(Components, dxtpcFireDAC)
    else if (not(dxtpcBDE in Components)) and (Pos('\dclbde', FileName) > 0) then Include(Components, dxtpcBDE);
  end;
  ThirdPartyComponents[IDE] := Components;
end;

function TDxInstaller.GetIdeComponentsHierarchy(IDE: TDxIDE): string;
begin
  if InstallFileDir = EmptyStr then Exit(EmptyStr);
  Result := GetComponentsHierarhyJson(FComponents[IDEs.IndexOf(IDE)]);
end;

function TDxInstaller.GetInstallLibraryDir(const InstallFileDir: String; IDE: TDxIDE; const IDEPlatform: TDxIDEPlatform): String;
begin
  Result := InstallLibraryDir;
  if Result = EmptyStr then
    Result := IncludeTrailingPathDelimiter(InstallFileDir) + DEF_INSTALL_DIR;
  if IDE <> nil then begin
    Result := Result + '\' + TDxProfile.GetIDEVersionNumberStr(IDE);
    if IDEPlatform = Win64 then Result := Result + '\' + DxIDEPlatformNames[IDEPlatform];
  end;
end;

function TDxInstaller.GetInstallSourcesDir(const InstallFileDir: String): String;
begin
  Result := GetInstallLibraryDir(InstallFileDir, nil) + '\Sources';
end;

function TDxInstaller.GetBplPath(IDE: TDxIDE; const IDEPlatform: TDxIDEPlatform): String;
begin
  Result := FBPLPath;
  if Result = EmptyStr then
    Result := IDE.BPLOutputPath[IDEPlatform];
end;

function TDxInstaller.GetComponents(IDE: TDxIDE): TDxComponentList;
begin
  Result := FComponents[IDEs.IndexOf(IDE)];
end;

function TDxInstaller.GetDclPath(IDE: TDxIDE; const IDEPlatform: TDxIDEPlatform): String;
begin
  Result := FDCPPath;
  if Result = EmptyStr then
    Result := IDE.DCPOutputPath[IDEPlatform];
end;

function TDxInstaller.GetOptions(IDE: TDxIDE): TDxInstallOptions;
begin
  Result := FOptions[IDEs.IndexOf(IDE)];
end;

procedure TDxInstaller.SetOptions(IDE: TDxIDE; const Value: TDxInstallOptions);
var
  Options: TDxInstallOptions;
begin
  Options := Value;
  if (dxioCompileWin64Library in Options) and (not IsSupportWin64(IDE)) then Exclude(Options, dxioCompileWin64Library);
  if (dxioInstallToCppBuilder in Options) and (not IsSupportCppBuilder(IDE)) then Exclude(Options, dxioInstallToCppBuilder);
  FOptions[IDEs.IndexOf(IDE)] := Options;
end;


function TDxInstaller.GetThirdPartyComponents(IDE: TDxIDE): TDxThirdPartyComponents;
begin
  Result := FThirdPartyComponents[IDEs.IndexOf(IDE)];
end;

procedure TDxInstaller.SetThirdPartyComponents(IDE: TDxIDE; const Value: TDxThirdPartyComponents);
begin
  FThirdPartyComponents[IDEs.IndexOf(IDE)] := Value;
end;

procedure TDxInstaller.SearchNewPackages(List: TStringList);
var
  Packages, DPKFileList: TStringList;
  Component: TDxComponentProfile;
  S, FileName: String;
begin
  List.Clear;
  if InstallFileDir = EmptyStr then Exit;

  Packages := TStringList.Create;
  DPKFileList := TStringList.Create;
  try
    for Component in Profile.Components do begin
      Packages.AddStrings(Component.RequiredPackages);
      Packages.AddStrings(Component.OptionalPackages);
      Packages.AddStrings(Component.OutdatedPackages);
    end;
    BuildFileList(IncludeTrailingPathDelimiter(InstallFileDir) + '*.dpk', DPKFileList, faAnyFile, True, True);
    for S in DPKFileList do begin
      FileName := TDxPackage.ExtractPackageName(ChangeFileExt(ExtractFileName(S), ''));
      if Packages.IndexOf(FileName) < 0 then List.Add(S);
    end;
  finally
    Packages.Free;
    DPKFileList.Free;
  end;
end;

procedure TDxInstaller.SetFindSourcePackage(const Value: boolean);
begin
  FFindSourcePackage := Value;
  RefreshComponentsAndoptions;
end;

procedure TDxInstaller.SetInstallFileDir(const Value: String);
var
  I: Integer;
begin
  FInstallFileDir := Value;
  FComponentsVersion := Profile.GetDxBuildNumber(FInstallFileDir);
  RefreshComponentsAndoptions;
  for I := 0 to IDEs.Count - 1 do Options[IDEs[I]] := [dxioAddBrowsingPath, dxioNativeLookAndFeel];
end;

procedure TDxInstaller.SetState(const Value: TDxInstallerState);
begin
  if State = Value then Exit;
  if (Value = dxisStopped) and (State = dxisNormal) then Exit;
  FState := Value;
  case State of
    dxisNormal:   UpdateProgressState('Finished!');
    dxisStopped:  UpdateProgressState('Stopped.');
    dxisError:    UpdateProgressState('Error.');
  end;
  if State = dxisError then SetState(dxisRunning); // On Ignore Error.
end;

procedure TDxInstaller.Stop;
begin
  SetState(dxisStopped);
end;

procedure TDxInstaller.CheckStoppedState;
begin
  if not FConsoleMode then
    Application.ProcessMessages;
  if State = dxisStopped then begin
    SetState(dxisNormal);
    Abort;
  end;
end;

procedure TDxInstaller.Install(const IDEArray: TDxIDEArray);
var
  IDE: TDxIDE;
begin
  SetState(dxisRunning);
  FProgressBar := CreateProgress(Length(IDEArray) * 2, 100);
  try
    for IDE in IDEArray do Install(IDE);
    SetState(dxisNormal);
  finally
    FProgressBar := nil;
  end;
end;

procedure TDxInstaller.Uninstall(const IDEArray: TDxIDEArray);
var
  IDE: TDxIDE;
begin
  SetState(dxisRunning);
  FProgressBar := CreateProgress(Length(IDEArray), 100);
  try
    for IDE in IDEArray do Uninstall(IDE);
    SetState(dxisNormal);
  finally
    FProgressBar := nil;
  end;
end;

procedure TDxInstaller.Install(IDE: TDxIDE);
var
  Comp: TDxComponent;
  Package: TDxPackage;
  SourcesFileDir, InstallSourcesDir: String;
  dxBuildNumber: Cardinal;
  I: Integer;

  procedure AddLibrarySearchPath(const Dir: String; const IDEPlatform: TDxIDEPlatform);
  begin
    IDE.AddToLibrarySearchPath(Dir, IDEPlatform);
    if (dxioInstallToCppBuilder in Options[IDE]) and IsRADStudio(IDE) then TDxBDSIDE(IDE).AddToCppLibraryPath(Dir, IDEPlatform);
  end;
  procedure AddLibraryBrowsingPath(const Dir: String; const IDEPlatform: TDxIDEPlatform);
  begin
    IDE.AddToLibraryBrowsingPath(Dir, IDEPlatform);
    if (dxioInstallToCppBuilder in Options[IDE]) and IsRADStudio(IDE) then TDxBDSIDE(IDE).AddToCppBrowsingPath(Dir, IDEPlatform);
  end;
  procedure CopyFiles(const ASourcesFileDir: String);
  begin
    CopyFilesToDirectory(ASourcesFileDir + '*.*', InstallSourcesDir);
    CopyFilesToDirectory(ASourcesFileDir + '*.dfm;*.res', GetInstallLibraryDir(InstallFileDir, IDE, Win32));
    if dxioCompileWin64Library in Options[IDE] then
      CopyFilesToDirectory(ASourcesFileDir + '*.dfm;*.res', GetInstallLibraryDir(InstallFileDir, IDE, Win64));
  end;
var
  pb: IProgressBar;
begin
  Uninstall(IDE);
  i := 0;
  for Comp in Components[IDE] do
  begin
    if Comp.State <> dxcsInstall then Continue;
    for Package in Comp.Packages do
      inc(i, 2);
  end;
  pb := CreateProgress(i);
  dxBuildNumber := Profile.GetDxBuildNumber(InstallFileDir);
  InstallSourcesDir := GetInstallSourcesDir(InstallFileDir);
  for Comp in Components[IDE] do begin
    if Comp.State <> dxcsInstall then Continue;
    SourcesFileDir := IncludeTrailingPathDelimiter(Profile.GetComponentSourcesDir(InstallFileDir, Comp.Profile.ComponentName));
    UpdateProgress(IDE, Comp.Profile, 'Copying', 'Sources Files');
    UpdateProgressState('Copying Files: ' + SourcesFileDir + '*.*');
    CopyFiles(SourcesFileDir);
    // Fix for version >= 18.2.x
    if (dxBuildNumber >= 20180200) and (Comp.Profile.ComponentName = 'ExpressLibrary') then begin
      for I := 0 to Profile.Components.Count - 1 do
        if DirectoryExists(Profile.GetComponentSourcesDir(InstallFileDir, Profile.Components[I].ComponentName)) and
          not DirectoryExists(Profile.GetComponentPackagesDir(InstallFileDir, Profile.Components[I].ComponentName)) then
            CopyFiles(IncludeTrailingPathDelimiter(Profile.GetComponentSourcesDir(InstallFileDir, Profile.Components[I].ComponentName)));
      CopyFiles(IncludeTrailingPathDelimiter(Profile.GetComponentSourcesDir(InstallFileDir, 'ExpressPageControl')));
    end;
    for Package in Comp.Packages do if Package.Required then begin
      InstallPackage(IDE, Win32, Comp, Package);
      pb.StepIt;
      InstallPackage(IDE, Win64, Comp, Package);
      pb.StepIt;
    end;
  end;

  for Comp in Components[IDE] do begin
    if Comp.State <> dxcsInstall then Continue;
    for Package in Comp.Packages do if not Package.Required then begin
      InstallPackage(IDE, Win32, Comp, Package);
      pb.StepIt;
      InstallPackage(IDE, Win64, Comp, Package);
      pb.StepIt;
    end;
  end;
  FProgressBar.StepIt;

  if FCompileOnly then
    Exit;
  AddLibrarySearchPath(GetInstallLibraryDir(InstallFileDir, IDE, Win32), Win32);
  if dxioCompileWin64Library in Options[IDE] then AddLibrarySearchPath(GetInstallLibraryDir(InstallFileDir, IDE, Win64), Win64);
  if dxioAddBrowsingPath in Options[IDE] then begin
    AddLibraryBrowsingPath(InstallSourcesDir, Win32);
    if dxioCompileWin64Library in Options[IDE] then AddLibraryBrowsingPath(InstallSourcesDir, Win64);
  end else begin
    AddLibrarySearchPath(installSourcesDir, Win32);
    if dxioCompileWin64Library In Options[IDE] then AddLibrarySearchPath(InstallSourcesDir, Win64)
  end;

  SetIDEOverrideEnvironmentVariable(IDE, DxEnvironmentVariableName, InstallFileDir);
end;

procedure TDxInstaller.InstallPackage(IDE: TDxIDE; const IDEPlatform: TDxIDEPlatform; Component: TDxComponent; Package: TDxPackage);
const
  TEMP_RC_NAME = '~vi.rc';
var
  BPLPath, DCPPath: String;
  I: Integer;
  R: Boolean;
  ExtraOptions: String;
  InstallSourcesDir, InstallLibraryDir: String;
  DestPackageName: String;
  ResFileName: String;
  RCFileName: String;
  Code: string;
  ExecOptions: TJclExecuteCmdProcessOptions;
begin
  CheckStoppedState;
  if not Package.Exists then Exit;
  case Package.Category of
    dxpcIBX:      if not (dxtpcIBX in ThirdPartyComponents[IDE]) then Exit;
    dxpcTeeChart: if not (dxtpcTeeChart in ThirdPartyComponents[IDE]) then Exit;
    dxpcFireDAC:  if not (dxtpcFireDAC in ThirdPartyComponents[IDE]) then Exit;
    dxpcBDE:      if not (dxtpcBDE in ThirdPartyComponents[IDE]) or (IDEPlatform = Win64) then Exit;
  end;
  if (IDEPlatform = Win64) and (not (dxioCompileWin64Library in Options[IDE])) then Exit;
  if (IDEPlatform = Win64) and (Package.Usage = dxpuDesigntimeOnly) then Exit;
  if not Package.Required then
    for I := 0 to Package.DependentComponents.Count - 1 do
      if Package.DependentComponents[I].State <> dxcsInstall then Exit;

  UpdateProgress(IDE, Component.Profile, 'Install Package', DxIDEPlatformNames[IDEPlatform] + ' > ' + Package.FullFileName);
  if IDEPlatform = Win32 then IDE.DCC := IDE.DCC32 else
  if IDEPlatform = Win64 then TDxBDSIDE(IDE).DCC := TDxBDSIDE(IDE).DCC64;

  BPLPath := GetBplPath(IDE, IDEPlatform);
  DCPPath := GetDclPath(IDE, IDEPlatform);
  InstallSourcesDir := GetInstallSourcesDir(InstallFileDir);
  InstallLibraryDir := GetInstallLibraryDir(InstallFileDir, IDE, IDEPlatform);
  ForceDirectories(BPLPath);
  ForceDirectories(DCPPath);
  // -$D- Debug Information;
  // -$L- Local Debug Symbols;
  // -$Y- Symbol Reference Info;
  // -Q   Quiet Compile;
  // -U   Unit Search Paths;
  // -R   Resource Search Paths;
  // -B   Build All Units;
  // -NU  Unit .dcu Output Directory; XE2+
  // -N0  Unit .dcu Output Directory; XE2-
  // -A   Unit Alias;
  // -NS  Namespaces Search Paths;

  // -D   Define Conditionals;

  // -JL  Generate package .lib, .bpi, and all .hpp files for C++;
  // -NB  Unit .bpi Output Directory - DCP Path;
  // -NH  Unit .hpp Output Directory;
  // -NO  Unit .obj Output Directory - DCP Path;
  if dxioMakeDebugDcu in Options[IDE] then
    ExtraOptions := ' -$D+ -$L+ -$Y+ -$O- -DDXDEBUGMODE'
  else
    ExtraOptions := ' -$D- -$L- -$Y-';
  ExtraOptions := ExtraOptions +
    ' -Q ' +
    Format(' -U"%s" -U"%s" -R"%s" ', [DCPPath, InstallSourcesDir, InstallSourcesDir]) +
    Format(' -B -NU"%s" -N0"%s" ', [InstallLibraryDir, InstallLibraryDir]) +
    '-AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE ' +
    '-NSWinapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Data;Datasnap;Web;Soap;IBX;VclTee; ';

  try
    if Package.Postfix <> Package.SourcePostfix then
    begin
      UpdateProgressState(Format('Copy and modify source package from %s to %s...', [Package.SourcePostfix, Package.Postfix]));
      code := TFile.ReadAllText(Package.FullSourceFileName);
      code := code
        .Replace(TPath.GetFileNameWithoutExtension(Package.FullFileName), TPath.GetFileNameWithoutExtension(Package.SourceFullName))
        .Replace(Package.SourcePostfix, Package.Postfix)
      ;
      TFile.WriteAllText(Package.FullFileName, code);
    end;

    RCFileName := EmptyStr;
    ResFileName := TPath.ChangeExtension(Package.FullFileName, '.res');
    if not TFile.Exists(ResFileName) then
    begin
      UpdateProgressState('Resource file not found, generating new one...');
      RCFileName := TPath.GetTempPath;
      DeleteFile(RCFileName);
      RCFileName := TPath.Combine(RCFileName, TEMP_RC_NAME);
      i := FComponentsVersion mod 10000;
      code := FRCTemplate
        .Replace('%BUILD_YEAR%', IntToStr(FComponentsVersion div 10000))
        .Replace('%BUILD_MONTH%', IntToStr(i div 100))
        .Replace('%BUILD_DAY%', IntToStr(i mod 100))
        .Replace('%MAJOR%', IntToStr(FComponentsVersion div 10000 mod 100))
        .Replace('%MINOR%', IntToStr(i div 100))
        .Replace('%BUILD%', IntToStr(i mod 100))
        .Replace('%DESCRIPTION%', Package.Description.Replace('"', '`'))
        .Replace('%PACKAGE_NAME%', Package.FullName.Replace('"', '`') + BPLExtName)
      ;
      TFile.WriteAllText(RCFileName, code);
      ExecOptions := TJclExecuteCmdProcessOptions.Create(Format('%sbrcc32.exe "%s" "-fo%s"', [IncludeTrailingPathDelimiter(IDE.BinFolderName), RCFileName, ResFileName]));
      try
        ExecOptions.AutoConvertOem := false;
        ExecOptions.RawOutput := false;
        ExecOptions.MergeError := true;
        ExecOptions.RawError := false;
        ExecOptions.StartupVisibility := svHide;

        UpdateProgressState(ExecOptions.CommandLine);
        ExecuteCmdProcess(ExecOptions);
        UpdateProgressState(ExecOptions.Output);
      finally
        ExecOptions.Free;
      end;
      DeleteFile(RCFileName);
    end;

    if dxioNativeLookAndFeel in Options[IDE] then ExtraOptions := ExtraOptions + ' -DUSENATIVELOOKANDFEELASDEFAULT ';
    if dxioInstallToCppBuilder in Options[IDE] then
      ExtraOptions := ExtraOptions + Format(' -JL -NB"%s" -NH"%s" -NO"%s" ', [DCPPath, InstallLibraryDir, DCPPath]);

    DestPackageName := Package.FullFileName;
    if dxioChangePackagePostfix in Options[IDE] then
    begin
      UpdateProgressState(Format('Modify source package and replace IDE postfix from %s to %s...', [Package.Postfix, PackagePostfix]));
      DestPackageName := IncludeTrailingPathDelimiter(ExtractFilePath(Package.FullFileName)) + Package.Name + PackagePostfix + ExtractFileExt(Package.FullFileName);
      if not SameText(DestPackageName, Package.FullFileName) then
      begin
        code := TFile.ReadAllText(Package.FullFileName);
        code := code.Replace(Package.Postfix, PackagePostfix);
        code := code.Replace('{$R *.res}', Format('{$R ''%s''}', [ResFileName]));
        TFile.WriteAllText(DestPackageName, code);
      end;
    end;
    if dxioAddPackageSuffix in Options[IDE] then
    begin
      //ExtraOptions := Format('%s --lib-suffix %s', [ExtraOptions, ide.PackageVersionNumberStr]);
      UpdateProgressState('Modify source package insert suffix auto directive...');
      code := TFile.ReadAllText(DestPackageName);
      if DestPackageName = Package.FullFileName then
        DestPackageName := DestPackageName + '.tmp';
      i := pos('{$LIBSUFFIX', code);
      if i > 0 then
        code[i + 1] := ' ';
      //code := Format('{$LIBSUFFIX ''%s''}'#13#10'%s', [ide.PackageVersionNumberStr, code]);
      code := Format('{$LIBSUFFIX ''%d''}'#13#10'%s', [ide.PackageVersionNumber, code]);
      TFile.WriteAllText(DestPackageName, code);
    end;

    R := IDE.CompileDelphiPackageEx(DestPackageName, BPLPath, DCPPath, ExtraOptions);
    if R then begin
      // Fix issue that the skin names not listed since v18.2.x. dxSkinXxxxx.bpl should be placed in the library install directory.
      if SameText(Package.Name, 'dxSkin') then
        CopyFile(IncludeTrailingPathDelimiter(BPLPath) + Package.FullName + BPLExtName, IncludeTrailingPathDelimiter(InstallLibraryDir) + Package.FullName + BPLExtName, True);

      if not FCompileOnly and (IDEPlatform = Win32) and (Package.Usage <> dxpuRuntimeOnly) then
        R := IDE.RegisterPackage(DestPackageName, BPLPath, Package.Description);
    end;
    if not R then SetState(dxisError);
  finally
    if DestPackageName <> Package.FullFileName then
      DeleteFile(DestPackageName);
    if Package.Postfix <> Package.SourcePostfix then
      DeleteFile(Package.FullFileName);
    if RCFileName <> EmptyStr then
    begin
      DeleteFile(RCFileName);
      DeleteFile(ResFileName);
    end;
  end;
end;

procedure TDxInstaller.RefreshComponentsAndoptions;
var
  Factory: TDxComponentFactory;
  I: Integer;
begin
  if FInstallFileDir = EmptyStr then
    Exit;
  Factory := TDxComponentFactory.Create(Self, FFindSourcePackage);
  try
    for I := 0 to IDEs.Count - 1 do begin
      if FComponents[I] = nil then FComponents[I] := TDxComponentList.Create;
      Factory.BuildComponentList(IDEs[I], FComponents[I]);
    end;
  finally
    Factory.Free;
  end;
end;

procedure TDxInstaller.Uninstall(IDE: TDxIDE);
var
  Comp: TDxComponentProfile;
  InstallFileDir, InstallLibraryDir, InstallSourcesDir: String;
  pb: IProgressBar;

  procedure RemoveLibraryPath(const IDEPlatform: TDxIDEPlatform);
  begin
    IDE.RemoveFromLibrarySearchPath(InstallLibraryDir, IDEPlatform);
    IDE.RemoveFromLibrarySearchPath(InstallSourcesDir, IDEPlatform);
    IDE.RemoveFromLibraryBrowsingPath(InstallSourcesDir, IDEPlatform);
    if IsRADStudio(IDE) and IsSupportCppBuilder(IDE) then begin
      TDxBDSIDE(IDE).RemoveFromCppLibraryPath(InstallLibraryDir, IDEPlatform);
      TDxBDSIDE(IDE).RemoveFromCppLibraryPath(InstallSourcesDir, IDEPlatform);
      TDxBDSIDE(IDE).RemoveFromCppBrowsingPath(InstallSourcesDir, IDEPlatform);
    end;
  end;

  procedure UninstallPackages(List: TStringList);
  var
    Package: String;
  begin
    for Package in List do begin
      UninstallPackage(IDE, Win32, Comp, Package);
      pb.StepIt;
      UninstallPackage(IDE, Win64, Comp, Package);
      pb.StepIt;
    end;
  end;
var
  i: integer;
begin
  i := 4;
  for Comp in Profile.Components do
    inc(i, (Comp.RequiredPackages.Count + Comp.OptionalPackages.Count + Comp.OutdatedPackages.Count) * 2);
  pb := CreateProgress(i + 1);
  try
    for Comp in Profile.Components do begin
      UninstallPackages(Comp.RequiredPackages);
      UninstallPackages(Comp.OptionalPackages);
      UninstallPackages(Comp.OutdatedPackages);
    end;

    InstallFileDir := GetIDEOverrideEnvironmentVariable(IDE, DxEnvironmentVariableName);
    if InstallFileDir = EmptyStr then Exit;
    InstallLibraryDir := GetInstallLibraryDir(InstallFileDir, IDE);
    InstallSourcesDir := GetInstallSourcesDir(InstallFileDir);
    UpdateProgress(IDE, nil, 'Deleting', 'Installation Files');
    UpdateProgressState('Deleting Directory: ' + InstallLibraryDir);
    DxUtils.DeleteDirectory(InstallLibraryDir);

    InstallLibraryDir := GetInstallLibraryDir(InstallFileDir, nil);
    if IsEmptyDirectory(InstallLibraryDir, InstallSourcesDir) then begin
      UpdateProgressState('Deleting Directory: ' + InstallSourcesDir);
      DxUtils.DeleteDirectory(InstallSourcesDir);
      RemoveDir(InstallLibraryDir);
    end;
    pb.StepIt;

    if FCompileOnly then
      Exit;
    RemoveLibraryPath(Win32);
    if IsSupportWin64(IDE) then begin
      InstallLibraryDir := GetInstallLibraryDir(InstallFileDir, IDE, Win64);
      RemoveLibraryPath(Win64);
    end;
    
    if not FCompileOnly then
      SetIDEOverrideEnvironmentVariable(IDE, DxEnvironmentVariableName, EmptyStr);
  finally
    FProgressBar.StepIt;
  end;
end;

procedure TDxInstaller.UninstallPackage(IDE: TDxIDE; const IDEPlatform: TDxIDEPlatform; Component: TDxComponentProfile; const PackageBaseName: String);
var
  BPLPath, DCPPath, PackageName, FileName: String;
begin
  CheckStoppedState;
  if (IDEPlatform = Win64) and (not IsSupportWin64(IDE)) then Exit;

  BPLPath := IncludeTrailingPathDelimiter(GetBplPath(IDE, IDEPlatform));
  DCPPath := IncludeTrailingPathDelimiter(GetDclPath(IDE, IDEPlatform));
  PackageName := Profile.GetPackageName(PackageBaseName, IDE);

  FileName := BPLPath + PackageBaseName + FPackagePostfix + BPLExtName;
  if not FCompileOnly then
  begin
    UpdateProgress(IDE, Component, 'Uninstall Package', FileName);
    IDE.UnregisterPackage(FileName);
  end;

  FileName := ChangeFileExt(FileName, '.*');
  UpdateProgressState('Deleting BPL Files: ' + FileName);
  DeleteFiles(FileName);

  FileName := DCPPath + PackageBaseName + FPackagePostfix + '.*';
  UpdateProgressState('Deleting DCP Files: ' + FileName);
  DeleteFiles(FileName);

  FileName := BPLPath + PackageName + BPLExtName;
  if not FCompileOnly then
  begin
    UpdateProgress(IDE, Component, 'Uninstall Package', FileName);
    IDE.UnregisterPackage(FileName);
  end;

  FileName := ChangeFileExt(FileName, '.*');
  UpdateProgressState('Deleting BPL Files: ' + FileName);
  DeleteFiles(FileName);

  FileName := DCPPath + PackageName + '.*';
  UpdateProgressState('Deleting DCP Files: ' + FileName);
  DeleteFiles(FileName);
end;

procedure TDxInstaller.UpdateProgress(IDE: TDxIDE; Component: TDxComponentProfile; const Task, Target: String);
begin
  if Assigned(FOnUpdateProgress) then FOnUpdateProgress(IDE, Component, Task, Target);
end;

procedure TDxInstaller.UpdateProgressState(const StateText: String);
begin
  if Assigned(FOnUpdateProgressState) then FOnUpdateProgressState(StateText)
end;

{ TDummyProgress }

function TDummyProgress.GetMax: Integer;
begin
  Result := 0;
end;

function TDummyProgress.GetPos: Integer;
begin
  Result := 0;
end;

function TDummyProgress.GetSize: Double;
begin
  Result := 0;
end;

procedure TDummyProgress.SetMax(const Value: Integer);
begin
  /// do nothing
end;

procedure TDummyProgress.SetPos(const Value: Integer);
begin
  /// do nothing
end;

procedure TDummyProgress.SetSize(const Value: Double);
begin
  /// do nothing
end;

procedure TDummyProgress.StepIt;
begin
  /// do nothing
end;

end.
