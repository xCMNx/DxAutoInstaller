{*******************************************************}
{                                                       }
{          DxAutoInstaller Progress Classes             }
{                                                       }
{        http://www.delphier.com/DxAutoIntaller         }
{        Copyright(c) 2014 by faceker@gmail.com         }
{                                                       }
{*******************************************************}

unit DxProgress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, DxIDE, DxProfile, DxInstaller, ActnList,
  ImgList, cxGraphics, DxUtils, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxProgressBar, System.ImageList, cxImageList,
  System.Actions, Vcl.ComCtrls, Generics.Collections, Diagnostics;

type
  TStopwatchHelper = record helper for TStopwatch
    function TimeString: string;
  end;
  TProgressBaseBarEvent = (pbeCreate, pbeInvalidate, pbePosChanged, pbeMaxChanged, pbeSizeChanged, pbeTerminate);
  IProgressBarControl = interface(IProgressBar)
    ['{99843BC2-0B80-48F3-8403-DE0296E5EBCA}']
    function GetControl: TControl;
    property Control: TControl read GetControl;
  end;
  Weakref<T: IInterface> = record
    [weak]
    ref: T;
    class operator Implicit(const value: T): Weakref<T>; inline;
    class operator Implicit(const value: Weakref<T>): T;
    class operator Implicit(const value: IInterface): Weakref<T>; inline;
    class operator Implicit(const value: Weakref<T>): IInterface;
    function IsEmpty: Boolean;
  end;
  TDxProgressForm = class(TDxForm)
    PanLogs: TPanel;
    ProgressLogs: TMemo;
    ProgressTitle: TLabel;
    Actions: TActionList;
    ActionStop: TAction;
    ActionClose: TAction;
    Images: TcxImageList;
    BtnAction: TButton;
    PanProgress: TPanel;
    tmPassed: TTimer;
    TimeLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionStopExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure tmPassedTimer(Sender: TObject);
    procedure PanProgressResize(Sender: TObject);
  private
    { Private declarations }
    FInstaller: TDxInstaller;
    FTarget: String;
    FTargetLogs: TStringList;
    FBars: TList<Weakref<IProgressBarControl>>;
    FStopwatch: TStopwatch;
    procedure ArrangeProgressBars;
    procedure OnProgresEvent(progress: IProgressBar; event: TProgressBaseBarEvent);
  public
    { Public declarations }
    property Installer: TDxInstaller read FInstaller write FInstaller;
    procedure Initial();
    procedure UpdateProgress(IDE: TDxIDE; Component: TDxComponentProfile; const Task, Target: String);
    procedure UpdateProgressState(const StateText: String);
    function CreateProgress(const Max: Integer; const Size: Double = 0.5): IProgressBar;
  end;
type
  TOnProgressBarEvent = procedure(progress: IProgressBar; event: TProgressBaseBarEvent) of object;
  TProgressBase = class(TinterfacedObject, IProgressBar)
  protected
    FOnEvent: TOnProgressBarEvent;
  protected
    procedure DoEvent(event: TProgressBaseBarEvent);
  public
    constructor Create(const Max: integer; onEvent: TOnProgressBarEvent; Size: double = 0);
    destructor Destroy; override;
    procedure SetMax(const Value: Integer); virtual;
    function GetMax: Integer; virtual; abstract;
    procedure SetPos(const Value: Integer); virtual;
    function GetPos: Integer; virtual; abstract;
    procedure StepIt; virtual;
    procedure SetSize(const Value: Double); virtual;
    function GetSize: Double; virtual; abstract;
  end;

function GetProgressTitle(IDE: TDxIDE; Component: TDxComponentProfile; const Task, Target: String): string;

implementation

{$R *.dfm}

type
  TProgress = class(TProgressBase, IProgressBarControl)
  protected
    FProgress: TCxProgressBar;
    FSize: Double;
  public
    constructor Create(const Owner: TWinControl; const Max: integer; onEvent: TOnProgressBarEvent; Size: double = 0);
    destructor Destroy; override;
    procedure SetMax(const Value: Integer); override;
    function GetMax: Integer; override;
    procedure SetPos(const Value: Integer); override;
    function GetPos: Integer; override;
    procedure SetSize(const Value: Double); override;
    function GetSize: Double; override;
    function GetControl: TControl;
    property Control: TControl read GetControl;
  end;

function GetProgressTitle(IDE: TDxIDE; Component: TDxComponentProfile; const Task, Target: String): string;
begin
  Result := IDE.Name;
  if Component <> nil then
    Result := Result + ' > ' + Component.ComponentName;
  if Task <> EmptyStr then
    Result := Result + ' > ' + Task;
  if Target <> EmptyStr then
    Result := Result + ' > ' + Target;
end;

{ TDxProgressForm }

procedure TDxProgressForm.ActionCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TDxProgressForm.ActionStopExecute(Sender: TObject);
begin
  tmPassed.Enabled := false;
  Installer.Stop;
end;

procedure TDxProgressForm.ArrangeProgressBars;
var
  i, l: Integer;
  Bar: Weakref<IProgressBarControl>;
  BarSizeCoef: Double;
begin
  if FBars.Count = 0 then
    Exit;
  if (FBars.Count = 1) then
  begin
    if FBars[0].ref.Control.Visible then
      FBars[0].ref.Control.Width := PanProgress.ClientWidth;
    Exit;
  end;
  BarSizeCoef := 0;
  i := PanProgress.ClientWidth;
  for Bar in FBars do
  begin
    if not Bar.ref.Control.Visible then
      Continue;
    if Bar.ref.Size < 1 then
      BarSizeCoef := BarSizeCoef + Bar.ref.Size
    else
    begin
      Bar.ref.Control.Width := Trunc(Bar.ref.Size);
      Dec(i, Bar.ref.Control.Width);
    end;
  end;
  if BarSizeCoef = 0 then
    BarSizeCoef := 1;
  BarSizeCoef := i * (1 / BarSizeCoef);
  for Bar in FBars do
  begin
    if not Bar.ref.Control.Visible then
      Continue;
    if Bar.ref.Size >= 1 then
      Continue;
    Bar.ref.Control.Width := Round(BarSizeCoef * Bar.ref.Size);
  end;
  l := 0;
  for i := 0 to PanProgress.ComponentCount - 1 do
  begin
    (PanProgress.Components[i] as TControl).Left := l;
    Inc(l, (PanProgress.Components[i] as TControl).Width);
  end;
end;

function TDxProgressForm.CreateProgress(const Max: Integer; const Size: Double): IProgressBar;
begin
  Result := TProgress.Create(PanProgress, Max, OnProgresEvent, Size);
end;

procedure TDxProgressForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := Installer.State = dxisNormal;
end;

procedure TDxProgressForm.FormCreate(Sender: TObject);
begin
  SetWindowLong(Handle, GWL_EXSTYLE, WS_EX_APPWINDOW);
  Caption := Application.Title;
  FInstaller := nil;
  FTarget := EmptyStr;
  FTargetLogs := TStringList.Create;
  FBars := TList<Weakref<IProgressBarControl>>.Create;
end;

procedure TDxProgressForm.FormDestroy(Sender: TObject);
begin
  FTargetLogs.Free;
  FBars.Free;
end;

procedure TDxProgressForm.Initial;
begin
  ProgressLogs.Clear;
  BtnAction.Action := ActionClose;
  FStopwatch := FStopwatch.StartNew;
  tmPassed.Enabled := true;
  Show;
end;

procedure TDxProgressForm.OnProgresEvent(progress: IProgressBar; event: TProgressBaseBarEvent);
var
  bar: IProgressBarControl;
begin
  bar := progress as IProgressBarControl;
  case event of
    pbeCreate:
    begin
      FBars.Add(bar);
      bar.Control.Top := 0;
      bar.Control.Height := PanProgress.ClientHeight;
    end;
    pbeTerminate:
      FBars.Remove(bar);
  end;
  if not bar.Control.Visible then
    bar.Control.Width := 0;
  ArrangeProgressBars;
  Application.ProcessMessages;
end;

procedure TDxProgressForm.PanProgressResize(Sender: TObject);
begin
  ArrangeProgressBars;
end;

procedure TDxProgressForm.tmPassedTimer(Sender: TObject);
begin
  TimeLabel.Caption := FStopWatch.TimeString;
end;

procedure TDxProgressForm.UpdateProgress(IDE: TDxIDE; Component: TDxComponentProfile; const Task, Target: String);
begin
  if Installer = nil then Exit;
  if Installer.State = dxisRunning then BtnAction.Action := ActionStop;
  ProgressTitle.Caption := GetProgressTitle(IDE, Component, Task, Target);
  if FTarget <> Target then begin
    FTargetLogs.Clear;
    ProgressLogs.Lines.Add(StringOfChar('-', 100));
  end;
  FTarget := Target;
end;

procedure TDxProgressForm.UpdateProgressState(const StateText: String);
begin
  if Installer = nil then Exit;
  ProgressLogs.Lines.Add(StateText);
  FTargetLogs.Add(StateText);
  case Installer.State of
    dxisNormal:  begin
                   tmPassed.Enabled := false;
                   BtnAction.Action := ActionClose;
                   ProgressTitle.Caption := StateText;
                   Close;
                   ShowModal;
                 end;
    dxisError:   if Application.MessageBox(PChar(FTargetLogs.Text + CRLF + 'An error has occurred, do you want to continue?'),
                                           'Confirmation', MB_ICONQUESTION + MB_OKCANCEL) = IDCANCEL then ActionStop.Execute;
  end;
end;

{ TProgressBase }

constructor TProgressBase.Create(const Max: integer; onEvent: TOnProgressBarEvent; Size: double = 0);
begin
  SetMax(Max);
  SetSize(Size);
  FOnEvent := onEvent;
  DoEvent(pbeCreate);
end;

destructor TProgressBase.Destroy;
begin
  DoEvent(pbeTerminate);
  inherited;
end;

procedure TProgressBase.DoEvent(event: TProgressBaseBarEvent);
begin
  if Assigned(FOnEvent) then
    FOnEvent(self, event);
end;

procedure TProgressBase.SetMax(const Value: Integer);
begin
  DoEvent(pbeMaxChanged);
end;

procedure TProgressBase.SetPos(const Value: Integer);
begin
  DoEvent(pbePosChanged);
end;

procedure TProgressBase.SetSize(const Value: Double);
begin
  DoEvent(pbeSizeChanged);
end;

procedure TProgressBase.StepIt;
begin
  SetPos(GetPos + 1);
end;

{ Weakref<T> }

class operator Weakref<T>.Implicit(const value: Weakref<T>): T;
begin
  Result := value.ref;
end;

class operator Weakref<T>.Implicit(const value: Weakref<T>): IInterface;
begin
  Result := value.ref;
end;

class operator Weakref<T>.Implicit(const value: IInterface): Weakref<T>;
begin
  Result.ref := T(value);
end;

function Weakref<T>.IsEmpty: Boolean;
begin
  Result := not Assigned(ref);
end;

class operator Weakref<T>.Implicit(const value: T): Weakref<T>;
begin
  Result.ref := value;
end;

{ TProgress }

constructor TProgress.Create(const Owner: TWinControl; const Max: integer; onEvent: TOnProgressBarEvent; Size: double);
begin
  FProgress := TCxProgressBar.Create(Owner);
  FSize := Size;
  FProgress.Parent := Owner;
  inherited Create(Max, onEvent, Size);
end;

destructor TProgress.Destroy;
begin
  inherited;
  FreeAndNil(FProgress);
end;

function TProgress.GetControl: TControl;
begin
  Result := FProgress;
end;

function TProgress.GetMax: Integer;
begin
  Result := Trunc(FProgress.Properties.Max);
end;

function TProgress.GetPos: Integer;
begin
  Result := Trunc(FProgress.Position);
end;

function TProgress.GetSize: Double;
begin
  Result := FSize;
end;

procedure TProgress.SetMax(const Value: Integer);
begin
  FProgress.Properties.Max := Value;
  FProgress.Visible := Value > 1;
  inherited;
end;

procedure TProgress.SetPos(const Value: Integer);
begin
  FProgress.Position := Value;
  inherited;
end;

procedure TProgress.SetSize(const Value: Double);
begin
  FSize := Value;
  inherited;
end;

{ TStopwatchHelper }

function TStopwatchHelper.TimeString: string;
begin
  Result := Elapsed.ToString.Split(['.'])[0];
end;

end.
