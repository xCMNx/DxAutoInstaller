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
  TProgressBarEvent = (pbeCreate, pbeInvalidate, pbeMaxChanged, pbeSizeChanged, pbeTerminate);
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
    procedure OnProgresEvent(progress: IProgressBarControl; event: TProgressBarEvent);
  public
    { Public declarations }
    property Installer: TDxInstaller read FInstaller write FInstaller;
    procedure Initial();
    procedure UpdateProgress(IDE: TDxIDE; Component: TDxComponentProfile; const Task, Target: String);
    procedure UpdateProgressState(const StateText: String);
    function CreateProgress(const Max: Integer; const Size: Double = 0.5): IProgressBar;
  end;

implementation

{$R *.dfm}

type
  TOnProgressBarEvent = procedure(progress: IProgressBarControl; event: TProgressBarEvent) of object;
  TProgress = class(TinterfacedObject, IProgressBar, IProgressBarControl)
  protected
    FProgress: TCxProgressBar;
    FOnEvent: TOnProgressBarEvent;
    FSize: Double;
  protected
    procedure DoEvent(event: TProgressBarEvent);
  public
    constructor Create(const Owner: TWinControl; const Max: integer; onEvent: TOnProgressBarEvent; Size: double = 0);
    destructor Destroy; override;
    procedure SetMax(const Value: Integer);
    function GetMax: Integer;
    procedure SetPos(const Value: Integer);
    procedure StepIt;
    procedure SetSize(const Value: Double);
    function GetSize: Double;
    function GetControl: TControl;
    property Control: TControl read GetControl;
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

procedure TDxProgressForm.OnProgresEvent(progress: IProgressBarControl; event: TProgressBarEvent);
begin
  case event of
    pbeCreate:
    begin
      FBars.Add(progress);
      progress.Control.Top := 0;
      progress.Control.Height := PanProgress.ClientHeight;
    end;
    pbeTerminate:
      FBars.Remove(progress);
  end;
  if not progress.Control.Visible then
    progress.Control.Width := 0;
  ArrangeProgressBars;
  Application.ProcessMessages;
end;

procedure TDxProgressForm.PanProgressResize(Sender: TObject);
begin
  ArrangeProgressBars;
end;

procedure TDxProgressForm.tmPassedTimer(Sender: TObject);
begin
  TimeLabel.Caption := FStopWatch.Elapsed.ToString.Split(['.'])[0];
end;

procedure TDxProgressForm.UpdateProgress(IDE: TDxIDE; Component: TDxComponentProfile; const Task, Target: String);
begin
  if Installer = nil then Exit;
  if Installer.State = dxisRunning then BtnAction.Action := ActionStop;
  ProgressTitle.Caption := IDE.Name;
  if Component <> nil then ProgressTitle.Caption := ProgressTitle.Caption + ' > ' + Component.ComponentName;
  if Task <> EmptyStr then ProgressTitle.Caption := ProgressTitle.Caption + ' > ' + Task;
  if FTarget <> Target then begin
    FTargetLogs.Clear;
    ProgressLogs.Lines.Add(StringOfChar('-', 100));
  end;
  FTarget := Target;
  if Target <> EmptyStr then ProgressTitle.Caption := ProgressTitle.Caption + ' > ' + Target;
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

{ TProgress }

constructor TProgress.Create(const Owner: TWinControl; const Max: integer; onEvent: TOnProgressBarEvent; Size: double = 0);
begin
  FProgress := TCxProgressBar.Create(Owner);
  FSize := Size;
  FProgress.Parent := Owner;
  SetMax(Max);
  FOnEvent := onEvent;
  DoEvent(pbeCreate);
end;

destructor TProgress.Destroy;
begin
  DoEvent(pbeTerminate);
  FreeAndNil(FProgress);
  inherited;
end;

procedure TProgress.DoEvent(event: TProgressBarEvent);
begin
  if Assigned(FOnEvent) then
    FOnEvent(self, event);
end;

function TProgress.GetControl: TControl;
begin
  Result := FProgress;
end;

function TProgress.GetMax: Integer;
begin
  Result := Trunc(FProgress.Properties.Max);
end;

function TProgress.GetSize: Double;
begin
  Result := FSize;
end;

procedure TProgress.SetMax(const Value: Integer);
begin
  FProgress.Properties.Max := Value;
  FProgress.Visible := Value > 1;
  DoEvent(pbeMaxChanged);
end;

procedure TProgress.SetPos(const Value: Integer);
begin
  FProgress.Position := Value;
end;

procedure TProgress.SetSize(const Value: Double);
begin
  FSize := Value;
  DoEvent(pbeSizeChanged);
end;

procedure TProgress.StepIt;
begin
  FProgress.Position := FProgress.Position + 1;
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

end.
