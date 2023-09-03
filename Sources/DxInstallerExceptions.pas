unit DxInstallerExceptions;

interface

uses
  SysUtils,
  DxConsoleSwitchesConsts;

type
  DxInstallerErrorCode = (
    dxiecResurvedFirst = 1
    ,dxIDENotSupported = 69
    ,dxiecIDEInstanceStarted = 100
    ,dxiecEmptyPostfix
  );
  DxCiException = class(Exception)
  public
    constructor Create(const Switch: TDxInstallerSwitchRec); overload;
    constructor Create(const Switch: TDxInstallerSwitch); overload;
    constructor CreateHelp(const Msg: string; ErrorCode: DxInstallerErrorCode); overload;
    constructor CreateFmtHelp(const Msg: string; const Args: array of const;
      ErrorCode: DxInstallerErrorCode);
  end;

implementation

{ DxCiException }

constructor DxCiException.Create(const Switch: TDxInstallerSwitch);
begin
  Create(DxSwitches[Switch]);
end;

constructor DxCiException.CreateFmtHelp(const Msg: string; const Args: array of const; ErrorCode: DxInstallerErrorCode);
begin
  inherited CreateFmtHelp(Msg, Args, integer(ErrorCode));
end;

constructor DxCiException.CreateHelp(const Msg: string; ErrorCode: DxInstallerErrorCode);
begin
  inherited CreateHelp(Msg, integer(ErrorCode));
end;

constructor DxCiException.Create(const Switch: TDxInstallerSwitchRec);
begin
  inherited CreateFmtHelp('Parameter: %s; Error: %s; ErrorCode: %d', [Switch.Name, Switch.ErrorDescription, Switch.ErrorCode], Switch.ErrorCode);
end;

end.
