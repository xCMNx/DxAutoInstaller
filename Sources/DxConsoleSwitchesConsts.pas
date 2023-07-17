unit DxConsoleSwitchesConsts;

interface

type
  TDxInstallerSwitchRec = record
    Name, Description: string;
    ErrorCode: Integer;
    ErrorDescription: String;
    InfoArg: boolean;
    function ToString: String; overload;
  end;
  TDxInstallerSwitch = (
    dxisSillent
    ,dxisHelp
    ,dxisHelp2
    ,dxisVersion
    ,dxisDxVersion
    ,dxisUninstall
    ,dxisReg
    ,dxisUnReg
    ,dxisCompileOnly
    ,dxisJsonExport
    ,dxisPackagePrefix
    ,dxisInstallDir
    ,dxisBPLOutDir
    ,dxisDCPOutDir
    ,dxisIDE
    ,dxisIDEBpl32
    ,dxisIDEBpl64
    ,dxisSources
    ,dxisFindSourcePackage
    ,dxisNoNativeLookAndFeel
    ,dxisNoBrowsingPath
    ,dxisWin64
    ,dxisCpp
    ,dxisDebugDcu
    ,dxisPrefix
  );
  TDxInstallerSwitches = set of TDxInstallerSwitch;

const
  UNIQUE_PREFIX = 'PKG';
  DEF_INSTALL_DIR = 'Library';
  DxSwitches: array [TDxInstallerSwitch] of TDxInstallerSwitchRec = (
    (
      Name: 'sillent';
      Description: 'Run DevExpress installation in console mode';
      InfoArg: true;
    )
    ,(
      Name: '?';
      Description: 'Print help';
      InfoArg: true;
    )
    ,(
      Name: 'h';
      Description: 'Print help';
      InfoArg: true;
    )
    ,(
      Name: 'v';
      Description: 'Print version';
      InfoArg: true;
    )
    ,(
      Name: 'dxv';
      Description: 'Print DevExpress component version';
      InfoArg: true;
    )
    ,(
      Name: 'u';
      Description: 'Execute uninstall only';
      InfoArg: true;
    )
    ,(
      Name: 'r+';
      Description: 'Register compiled packages';
      InfoArg: true;
    )
    ,(
      Name: 'r-';
      Description: 'Unregister compiled packages';
      InfoArg: true;
    )
    ,(
      Name: 'c';
      Description: 'Compile only';
    )
    ,(
      Name: 'je';
      Description: 'Export hierarchy JSON';
      InfoArg: true;
    )
    ,(
      Name: 'pp';
      Description: 'Set package prefix for option -p';
    )
    ,(
      Name: 'oi';
      Description: 'Set where installation files will be saved, default ' + DEF_INSTALL_DIR;
    )
    ,(
      Name: 'ob';
      Description: 'Set where BPL files will be saved, default in IDE BPL direcory';
    )
    ,(
      Name: 'od';
      Description: 'Set where DCP files will be saved, default in IDE DCP direcory';
    )
    ,(
      Name: 'ide';
      Description: 'IDE name to install for';
      ErrorCode: 1;
      ErrorDescription: 'IDE name is not specified or IDE not found';
    )
    ,(
      Name: 'ideBpl32';
      Description: 'Print ide BPL directory for win32 packages';
      InfoArg: true;
    )
    ,(
      Name: 'ideBpl64';
      Description: 'Print ide BPL directory for win64 packages';
      InfoArg: true;
    )
    ,(
      Name: 'src';
      Description: 'Source directory of DevExpress';
      ErrorCode: 2;
      ErrorDescription: 'Source directory is not specified or does not exists';
    )
    ,(
      Name: 'f';
      Description: 'Try to find source package';
    )
    ,(
      Name: 'n-';
      Description: 'Do not use Native Look and Feel as Default';
    )
    ,(
      Name: 'b-';
      Description: 'Do not add Browsing Path';
    )
    ,(
      Name: 'win64';
      Description: 'Compile Win64 Library';
    )
    ,(
      Name: 'cpp';
      Description: 'Install to C++Builder';
    )
    ,(
      Name: 'd';
      Description: 'Make debug .dcu';
    )
    ,(
      Name: 'p';
      Description: 'Replace IDE version prefix to ' + UNIQUE_PREFIX + ', can be changed by -pp option';
    )
  );

function DxFindCmdLineSwitch(const Switch: TDxInstallerSwitch): boolean; overload;
function DxFindCmdLineSwitch(const Switch: TDxInstallerSwitch; var Value: string): boolean; overload;

implementation

uses
  SysUtils;

function DxFindCmdLineSwitch(const Switch: TDxInstallerSwitch): boolean;
var
  tmp: string;
begin
  Result := DxFindCmdLineSwitch(Switch, tmp);
end;

function DxFindCmdLineSwitch(const Switch: TDxInstallerSwitch; var Value: string): boolean;
begin
  Result := FindCmdLineSwitch(DxSwitches[Switch].Name, Value, true, [clstValueNextParam]);
end;

{ TDxInstallerSwitchRec }

function TDxInstallerSwitchRec.ToString: String;
begin
  Result := '-' + Name + ': ' + Description;
end;

end.
