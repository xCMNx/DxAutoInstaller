unit DxConsoleInstaller;

interface

uses
  Windows,
  SysUtils,
  Generics.Collections,
  Diagnostics,
  IOUtils,
  Classes,
  DxConsoleSwitchesConsts;

type
  TSwitchValues = TDictionary<TDxInstallerSwitch, String>;

function IsConsoleMode: boolean;
function ExecuteConsoleInstallation: Integer;

implementation

uses
  DxInstaller,
  DxIDE,
  DxBDSVersionsConsts,
  DxProfile,
  DxUtils,
  DxComponent,
  DxProgress,
  DxInstallerExceptions,
  JclIDEUtils;

type
  TProgress = class(TProgressBase)
  protected
    FMax: Integer;
    FPos: Integer;
    FSize: Double;
  public
    constructor Create(const Max: integer; onEvent: TOnProgressBarEvent; Size: double = 0);
    procedure SetMax(const Value: Integer); override;
    function GetMax: Integer; override;
    procedure SetPos(const Value: Integer); override;
    procedure SetSize(const Value: Double); override;
    function GetSize: Double; override;
    function GetPos: Integer; override;
  end;
  TIDEnames = array [0..7] of String;
  TLogOutput = class
    Installer: TDxInstaller;
    LastTarget: String;
    Bars: TList<Weakref<IProgressBar>>;
    Stopwatch: TStopwatch;
    Title: String;
    LastInfo: TConsoleScreenBufferInfo;
    ConHandle: THandle;
    constructor Create;
    destructor Destroy; override;
    procedure Update;
    procedure OnProgresEvent(progress: IProgressBar; event: TProgressBaseBarEvent);
    procedure UpdateProgress(IDE: TDxIDE; Component: TDxComponentProfile; const Task, Target: String);
    procedure UpdateProgressState(const StateText: String);
    function CreateProgress(const Max: Integer; const Size: Double = 0.5): IProgressBar;
    procedure ClearLine;
  end;
  SmallRectHelper = record helper for TSmallRect
    function GetWidth: integer;
  end;
  CONSOLE_SCREEN_BUFFER_INFO_Helper = record helper for _CONSOLE_SCREEN_BUFFER_INFO
    function GetWidth: integer;
  end;

function IsConsoleMode: boolean;
var
  Switch: TDxInstallerSwitch;
begin
  for Switch := Low(TDxInstallerSwitch) to High(TDxInstallerSwitch) do
    if DxFindCmdLineSwitch(Switch) and DxSwitches[Switch].InfoArg then
      Exit(true);
  Result := false;
end;

function GetSwitchValue(const Switch: TDxInstallerSwitch): String;
begin
  if DxFindCmdLineSwitch(Switch, Result) then
    Exit;
  if DxSwitches[Switch].ErrorCode <> 0 then
    raise DxCiException.Create(Switch);
end;

function GetIdeNames(const IDE: TDxIde): TIDEnames;
begin
  Result[0] := IDE.Name;
  Result[1] := IDE.IDEVersionNumberStr;
  Result[2] := TDxProfile.GetIDEVersionNumberStr(IDE);
  Result[3] := IntToStr(IDE.IDEVersionNumber);
  Result[4] := IntToStr(IDE.IDEVersionNumber)+'.0';
  if IDE.IDEVersionNumber > HIGH(BDSVersions) then
    Exit;
  Result[5] := 'VER' + BDSVersions[IDE.IDEVersionNumber].CoreIdeVersion;
  Result[6] := BDSVersions[IDE.IDEVersionNumber].VersionStr;
  Result[7] := BDSVersionNames[IDE.IDEVersionNumber];
end;

procedure ListIDEs(Installer: TDxInstaller);
var
  i: Integer;
begin
  WriteLn('Installed IDE names:');
  for i := Installer.IDEs.Count - 1 downto 0 do
    WriteLn(#9, String.Join('/', GetIdeNames(Installer.IDEs[i])));
end;


function GetIde(const Param: string; Installer: TDxInstaller): TDxIde;
var
  i: Integer;
  Names: TIDEnames;
  Name: String;
begin
  for i := Installer.IDEs.Count - 1 downto 0 do
  begin
    Result := Installer.IDEs[i];
    Names := GetIdeNames(Result);
    for Name in Names do
    begin
      if SameText(Name, Param) then
        Exit;
    end;
  end;
  raise DxCiException.Create(dxisIDE);
end;

procedure PrintHelp;
  procedure PrintSwitchesDescription(const Optional: boolean);
  var
    Switch: TDxInstallerSwitch;
  begin
    for Switch := Low(TDxInstallerSwitch) to High(TDxInstallerSwitch) do
      if (DxSwitches[Switch].ErrorCode = 0) = Optional then
        WriteLn(#9, DxSwitches[Switch].ToString);
  end;
begin
  WriteLn('Requiered:');
  PrintSwitchesDescription(false);
  WriteLn('Optional:');
  PrintSwitchesDescription(true);
end;

procedure PrintSwitches(const SwitchValues: TSwitchValues);
var
  Switch: TPair<TDxInstallerSwitch, String>;
begin
  WriteLn('Parameters:');
  for Switch in SwitchValues do
  begin
    Write(#9, DxSwitches[Switch.Key].ToString);
    if Switch.Value <> EmptyStr then
      Write(' = ', Switch.Value);
    WriteLn;
  end;
end;

function GetStringFileInfo(const fPInfoBuffer: PChar; const Name: string): string;
var
  CommandBuf: array[0..255] of char;
  Ptr: Pointer;
  Len: UINT;
begin
  Result := '';
  StrPCopy(CommandBuf, '\StringFileInfo\0\' + Name);
  if VerQueryValue(fPInfoBuffer, CommandBuf, Ptr, Len) then
    Result := PChar(Ptr);
end;

Type
  TVersionInfoProp = (
    vipComments
    ,vipCompanyName
    ,vipFileDescription
    ,vipFileVersion
    ,vipInternalName
    ,vipLegalCopyright
    ,vipLegalTrademarks
    ,vipOriginalFileName
    ,vipPrivateBuild
    ,vipProductName
    ,vipProductVersion
    ,vipSpecialBuild
  );
  TVersionInfo = TDictionary<TVersionInfoProp, String>;
const
  VerNames: array [TVersionInfoProp] of string = (
    'Comments'
    ,'CompanyName'
    ,'FileDescription'
    ,'FileVersion'
    ,'InternalName'
    ,'LegalCopyright'
    ,'LegalTrademarks'
    ,'OriginalFileName'
    ,'PrivateBuild'
    ,'ProductName'
    ,'ProductVersion'
    ,'SpecialBuild'
  );

function getVersionInfo(const FileName: string; const downcase: boolean = false): TVersionInfo;
Type
  TTransRec = record
    Lang: Word;
    CharSet: Word;
  end;
  PTransRec = ^TTransRec;
var
  InfoBuffer: String;
  InfoHandle: DWORD;
  Ptr: Pointer;
  Len: DWORD;
  prop: TVersionInfoProp;
  propStrPrefix: string;
begin
  Result := TVersionInfo.Create;
  try
    SetLength(InfoBuffer, GetFileVersionInfoSize(PChar(FileName), InfoHandle));
    if Length(InfoBuffer) = 0 then
      Exit;
    if not GetFileVersionInfo(PChar(FileName), InfoHandle, Length(InfoBuffer), PChar(InfoBuffer)) then
      Exit;
    if not VerQueryValue(PChar(InfoBuffer), '\VarFileInfo\Translation', Ptr, Len) then
      Exit;
    propStrPrefix := Format('\StringFileInfo\%4.4x%4.4x\', [PTransRec(Ptr).Lang, PTransRec(Ptr).CharSet]);
    for prop:= Low(TVersionInfoProp) to High(TVersionInfoProp) do
    begin
      if VerQueryValue(PChar(InfoBuffer), PChar(propStrPrefix + VerNames[prop]), Ptr, Len) then
        if downcase then
          Result.AddOrSetValue(prop, LowerCase(PChar(Ptr)))
        else
          Result.AddOrSetValue(prop, PChar(Ptr));
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure registerComponent(IDE:TDxIde; Components: TDxComponentList; src: string);
Type
  TTransRec = record
    Lang: Word;
    CharSet: Word;
  end;
  PTransRec = ^TTransRec;
var
  Component: TDxComponent;
  Package: TDxPackage;
  verisonInfo: TVersionInfo;
  InstallList: TStrings;
  Name: string;
  FileName: string;
begin
  InstallList := TStringList.Create;
  try
    for Component in Components do
    begin
      for Package in Component.Packages do
      begin
        if Package.Usage = dxpuRuntimeOnly then
          Continue;
        InstallList.Values[LowerCase(Package.FullName)] := Package.Description;
      end;
    end;
    for FileName in TDirectory.GetFiles(src, '*.bpl') do
    try
      verisonInfo := getVersionInfo(FileName);
      if verisonInfo.TryGetValue(vipOriginalFileName, name) and (InstallList.IndexOfName(LowerCase(Tpath.GetFileNameWithoutExtension(name))) > -1) then
        IDE.RegisterPackage(FileName, InstallList.Values[Name])
    finally
      FreeAndNil(verisonInfo);
    end;
  finally
    InstallList.Free;
  end;
end;

procedure unregisterComponent(IDE:TDxIde);
const
  DX_NAME = 'developer express';
var
  i: integer;
  packages: TJclBorRADToolIdePackages;
  fileName: string;
  verisonInfo: TVersionInfo;
  value: string;
begin
  packages := IDE.IdePackages;
  for i := packages.Count - 1 downto 0 do
  try
    fileName := packages.PackageFileNames[i];
    verisonInfo := getVersionInfo(FileName, true);
    if (verisonInfo.TryGetValue(vipCompanyName, value) and (pos(DX_NAME, value) > 0))
      or (verisonInfo.TryGetValue(vipLegalCopyright, value) and (pos(DX_NAME, value) > 0))
      or (verisonInfo.TryGetValue(vipFileDescription, value) and (pos(DX_NAME, value) > 0)) then
      IDE.UnregisterPackage(fileName);
  finally
    FreeAndNil(verisonInfo);
  end;
end;


function ExecuteConsoleInstallation: Integer;

  function CheckInstallablecomponents(const Components: TDxComponentList): boolean;
  var
    Component: TDxComponent;
  begin
    Result := false;
    for Component in Components do
    begin
      if Component.Installable then
      begin
        Component.State := dxcsInstall;
        Result := true;
      end
      else
        Component.State := dxcsNotInstall;
    end;
  end;

var
  SwitchValues: TSwitchValues;
  Installer: TDxInstaller;
  IDE: TDxIde;
  Output: TLogOutput;
  Options: TDxInstallOptions;
  Switch: TDxInstallerSwitch;
  SwitchValue: string;
begin
  Result := 0;
{$IFDEF DEBUG}
  WriteLn(GetCommandLine);
{$ENDIF}
  if dxFindCmdLineSwitch(dxisVersion) then
  Begin
    WriteLn(GetVersionStr);
    Exit;
  End;
  if dxFindCmdLineSwitch(dxisDxVersion) then
  Begin
    WriteLn(TDxProfile.GetDxBuildNumberAsVersion(TDxProfile.GetDxBuildNumber(GetSwitchValue(dxisSources))));
    Exit;
  End;

  Output := TLogOutput.Create;
  Installer := TDxInstaller.Create([dxioConsoleMode]);
  try
    if dxFindCmdLineSwitch(dxisHelp) or dxFindCmdLineSwitch(dxisHelp2) then
    Begin
      PrintHelp;
      ListIDEs(Installer);
      Exit;
    End;

    Installer.OnUpdateProgress := Output.UpdateProgress;
    Installer.OnUpdateProgressState := Output.UpdateProgressState;
    Output.Installer := Installer;
    Installer.OnOnStartProgress := Output.CreateProgress;

    SwitchValues := TSwitchValues.Create;
    try
      for Switch := Low(TDxInstallerSwitch) to High(TDxInstallerSwitch) do
        if DxFindCmdLineSwitch(Switch, SwitchValue) then
          SwitchValues.Add(Switch, SwitchValue);
      if not SwitchValues.TryGetValue(dxisIDE, SwitchValue) then
        raise DxCiException.Create(dxisIDE);
      IDE := GetIde(SwitchValue, Installer);
      if SwitchValues.ContainsKey(dxisIDEBpl32) then
      begin
        Write(IDE.BPLOutputPath[bpWin32]);
        Exit;
      end;
      if SwitchValues.ContainsKey(dxisIDEBpl64) then
      begin
        Write(IDE.BPLOutputPath[bpWin64]);
        Exit;
      end;

      PrintSwitches(SwitchValues);
      ListIDEs(Installer);
      WriteLn('Selected IDE: ', IDE.Name);

      if IDE.AnyInstanceRunning then
        raise DxCiException.CreateFmtHelp('Close all running %s instances.', [IDE.Name], dxiecIDEInstanceStarted);
      if SwitchValues.ContainsKey(dxisUnReg) then
      begin
        Installer.OnUpdateProgress(IDE, nil, 'Unregister components', '');
        unregisterComponent(IDE);
        Exit;
      end;

      ApplyAppParams(Installer);

      if SwitchValues.ContainsKey(dxisUninstall) then
      begin
        Installer.OnUpdateProgress(IDE, nil, 'Uninstall', '');
        Installer.Uninstall([IDE]);
        Exit;
      end;

      if not (SwitchValues.TryGetValue(dxisSources, SwitchValue) and DirectoryExists(SwitchValue)) then
        raise DxCiException.Create(dxisSources);

      if SwitchValues.TryGetValue(dxisJsonExport, SwitchValue) then
      begin
        TFile.WriteAllText(SwitchValue, Installer.GetIdeComponentsHierarchy(IDE));
        Exit;
      end;

      if not CheckInstallablecomponents(Installer.Components[IDE]) then
        raise DxCiException.CreateFmtHelp('DevExpress installable packages for %s is not supported.', [IDE.Name], dxIDENotSupported);

      Options := [];
      if not SwitchValues.ContainsKey(dxisNoBrowsingPath) then
        Include(Options, dxioAddBrowsingPath);
      if not SwitchValues.ContainsKey(dxisNoNativeLookAndFeel) then
        Include(Options, dxioNativeLookAndFeel);
      if SwitchValues.ContainsKey(dxisWin64) then
        Include(Options, dxioCompileWin64Library);
      if SwitchValues.ContainsKey(dxisCpp) then
        Include(Options, dxioInstallToCppBuilder);
      if SwitchValues.ContainsKey(dxisDebugDcu) then
        Include(Options, dxioMakeDebugDcu);
      if SwitchValues.ContainsKey(dxisPrefix) then
        Include(Options, dxioDoNotIncludeIDEVersionInPackageName);
      Installer.Options[IDE] := Options;

      if SwitchValues.TryGetValue(dxisReg, SwitchValue) then
      begin
        Installer.OnUpdateProgress(IDE, nil, 'Register components', '');
        registerComponent(IDE, Installer.Components[IDE], SwitchValue);
      end
      else
        Installer.Install([IDE]);
    finally
      SwitchValues.Free;
    end;
  finally
    Output.Free;
    Installer.Free;
  end;
end;

{ TLogOutput }

procedure TLogOutput.ClearLine;
begin
//  SetConsoleCursorPosition(ConHandle, LastInfo.dwCursorPosition);
  Write(#13, StringOfChar(' ', LastInfo.GetWidth));
  GetConsoleScreenBufferInfo(ConHandle, LastInfo);
end;

constructor TLogOutput.Create;
begin
  Stopwatch := TStopwatch.StartNew;
  Bars := TList<Weakref<IProgressBar>>.Create;
  ConHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleScreenBufferInfo(ConHandle, LastInfo);
end;

function TLogOutput.CreateProgress(const Max: Integer; const Size: Double): IProgressBar;
begin
  Result := TProgress.Create(Max, OnProgresEvent, Size);
end;

destructor TLogOutput.Destroy;
begin
  FreeAndNil(Bars);
  inherited;
end;

procedure TLogOutput.OnProgresEvent(progress: IProgressBar; event: TProgressBaseBarEvent);
begin
  case event of
    pbeCreate:
      Bars.Add(progress);
    pbeTerminate:
      Bars.Remove(progress);
  end;
  Update;
end;

procedure TLogOutput.Update;
var
  Progress: IProgressBar;
  Str: string;
begin
  ClearLine;
  str := Stopwatch.TimeString + ' | ';
  for Progress in Bars do
    if Progress.Max > 1 then
      str := str + Format('%d/%d(%d%%) | ', [Progress.Pos, Progress.Max, Trunc(Progress.Pos * (1 / Progress.Max) * 100)]);
  str := str + Title;
  if Length(str) > LastInfo.GetWidth  then
  begin
    SetLength(str, LastInfo.GetWidth - 5);
    Str := Str + '...';
  end;
  Write(#13, Str, #13);
end;

procedure TLogOutput.UpdateProgress(IDE: TDxIDE; Component: TDxComponentProfile; const Task, Target: String);
begin
  ClearLine;
  if LastTarget <> Target then
  begin
    WriteLn;
    LastTarget := Target;
  end;
  Title := GetProgressTitle(IDE, Component, Task, Target);
  Update;
end;

procedure TLogOutput.UpdateProgressState(const StateText: String);
begin
  ClearLine;
  WriteLn(#13, StateText);
  //GetConsoleScreenBufferInfo(ConHandle, LastInfo);
  Update;
  if Installer.State = dxisError then
//    Installer.Stop;
    raise DxCiException.CreateHelp('Compiler error.', 200);
end;

{ TProgress }

constructor TProgress.Create(const Max: integer; onEvent: TOnProgressBarEvent; Size: double);
begin
  FPos := 0;
  inherited Create(Max, onEvent, Size);
end;

function TProgress.GetMax: Integer;
begin
  Result := FMax;
end;

function TProgress.GetPos: Integer;
begin
  Result := FPos;
end;

function TProgress.GetSize: Double;
begin
  Result := FSize;
end;

procedure TProgress.SetMax(const Value: Integer);
begin
  FMax := Value;
  inherited;
end;

procedure TProgress.SetPos(const Value: Integer);
begin
  FPos := Value;
  inherited;
end;

procedure TProgress.SetSize(const Value: Double);
begin
  FSize := Value;
  inherited;
end;

{ SmallRectHelper }

function SmallRectHelper.GetWidth: integer;
begin
  Result := Right - Left;
end;

{ CONSOLE_SCREEN_BUFFER_INFO_Helper }

function CONSOLE_SCREEN_BUFFER_INFO_Helper.GetWidth: integer;
begin
  Result := srWindow.GetWidth;
end;

end.