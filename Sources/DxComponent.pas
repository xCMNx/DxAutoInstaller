{*******************************************************}
{                                                       }
{          DxAutoInstaller Component Classes            }
{                                                       }
{        http://www.delphier.com/DxAutoIntaller         }
{        Copyright(c) 2014 by faceker@gmail.com         }
{                                                       }
{*******************************************************}

unit DxComponent;

interface

uses
  SysUtils, Classes, Generics.Collections, DxProfile, IOUtils;

type
  TDxComponent = class;
  TDxComponentList = TObjectList<TDxComponent>;

  TDxPackageCategory = (dxpcNormal, dxpcIBX, dxpcTeeChart, dxpcFireDAC, dxpcBDE);
  TDxPackageUsage = (dxpuDesigntimeOnly, dxpuRuntimeOnly, dxpuDesigntimeAndRuntime);

  TDxPackage = class
  private
    FName: String;
    FPath: String;
    FPrefix: String;
    FCategory: TdxPackageCategory;
    FSourcePrefix: String;
    FDescription: String;
    FUsage: TDxPackageUsage;
    FRequires: TStringList;
    FExists: Boolean;
    FRequired: Boolean;
    FDependentComponents: TDxComponentList;
    FContains: TStringList;
    procedure ReadOptions();
    function GetFullName: string;
    function GetSourceFullName: string;
    function GetFullFileName: string;
    function GetFullSourceFileName: string;
  public
    constructor Create(const Path, Name, Prefix: String; const SourcePrefix: String = '');
    destructor Destroy; override;
    property FullSourceFileName: String read GetFullSourceFileName;
    property FullFileName: String read GetFullFileName;
    property Prefix: String read FPrefix;
    property SourcePrefix: String read FSourcePrefix;
    property FullName: String read GetFullName;
    property SourceFullName: String read GetSourceFullName;
    property Name: String read FName;
    property Category: TDxPackageCategory read FCategory;
    property Description: String read FDescription;
    property Usage: TDxPackageUsage read FUsage;
    property Requires: TStringList read FRequires;
    property Contains: TStringList read FContains;
    property Exists: Boolean read FExists;
    property Required: Boolean read FRequired write FRequired;
    property DependentComponents: TDxComponentList read FDependentComponents;
    class procedure GetPackageNameAndPrefix(const FullName: String; out Name, Prefix: String);
    class function ExtractPackageName(const Name: String): String;
  end;

  TDxPackageList = TObjectList<TDxPackage>;
  TDxComponentState = (dxcsInstall, dxcsNotInstall, dxcsNotFound, dxcsNotSupported, dxcsMissing);

  TDxComponent = class
  private
    FProfile: TDxComponentProfile;
    FPackages: TDxPackageList;
    FParentComponents: TDxComponentList;
    FSubComponents: TDxComponentList;
    FState: TDxComponentState;
    procedure SetState(const Value: TDxComponentState);
    function GetCanInstall: boolean;
  public
    constructor Create(ComponentProfile: TDxComponentProfile);
    destructor Destroy; override;
    property Profile: TDxComponentProfile read FProfile;
    property Packages: TDxPackageList read FPackages;
    property ParentComponents: TDxComponentList read FParentComponents write FParentComponents;
    property SubComponents: TDxComponentList read FSubComponents write FSubComponents;
    property State: TDxComponentState read FState write SetState;
    function GetExistsPackageCount(): Integer;
    function IsMissingDependents(): Boolean;
    property Installable: Boolean read GetCanInstall;
  end;

const
  DPKDescriptionOptionIdent     = '{$DESCRIPTION ''';
  DPKDesigntimeOnlyOptionIdent  = '{$DESIGNONLY';
  DPKRuntimeOnlyOptionIdent     = '{$RUNONLY';
  DPKRequiresOptionIdent        = 'requires';
  DPKContainsOptionIdent        = 'contains';
  DPKExtName = '.dpk';

  dxcsEditModes = [dxcsInstall, dxcsNotInstall];

function GetComponentsHierarhyJson(const Components: TDxComponentList): String;

implementation

uses
  JSON, REST.JSON;

Type
  TDxPackageJson = class
    Name: String;
    Description: String;
    Category: TdxPackageCategory;
    Usage: TDxPackageUsage;
    Requires: TArray<String>;
    Required: Boolean;
    Contains: TArray<String>;
    DependentComponents: TArray<String>;
  public
    constructor Create(Package: TDxPackage);
  end;
  TDxComponentJson = class
    Name: String;
    Packages: TArray<TDxPackageJson>;
    ParentComponents: TArray<String>;
    SubComponents: TArray<String>;
  public
    constructor Create(Component: TDxComponent);
    destructor Destroy; override;
  end;
  TSerializationContainer = class
    List: TArray<TDxComponentJson>;
  public
    constructor Create(Components: TDxComponentList);
    destructor Destroy; override;
  end;

{ TDxPackageJson }

function ConponentsToNames(const Components: TDxComponentList): TArray<String>;
var
  i: integer;
begin
  SetLength(Result, Components.Count);
  for i := Components.Count - 1 downto 0 do
    Result[i] := Components[i].Profile.ComponentName;
end;

constructor TDxPackageJson.Create(Package: TDxPackage);
begin
  Name := Package.Name;
  Description := Package.Description;
  Category := Package.Category;
  Usage := Package.Usage;
  Requires := Package.Requires.ToStringArray;
  Required := Package.Required;
  Contains := Package.Contains.ToStringArray;
  DependentComponents := ConponentsToNames(Package.DependentComponents);
end;

{ TDxComponentJson }

function PackagesToArray(Packages: TDxPackageList): TArray<TDxPackageJson>;
var
  i: integer;
begin
  SetLength(Result, Packages.Count);
  for i := Packages.Count - 1 downto 0 do
    Result[i] := TDxPackageJson.Create(Packages[i]);
end;

constructor TDxComponentJson.Create(Component: TDxComponent);
begin
  Name := Component.Profile.ComponentName;
  Packages := PackagesToArray(Component.FPackages);
  ParentComponents := ConponentsToNames(Component.FParentComponents);
  SubComponents := ConponentsToNames(Component.FSubComponents);
end;

destructor TDxComponentJson.Destroy;
var
  p: TDxPackageJson;
begin
  for p in Packages do
    p.Free;
  inherited;
end;

{ TSerializationContainer }

constructor TSerializationContainer.Create(Components: TDxComponentList);
var
  i: integer;
begin
  SetLength(List, Components.Count);
  for i := Components.Count - 1 downto 0 do
    List[i] := TDxComponentJson.Create(Components[i]);
end;

destructor TSerializationContainer.Destroy;
var
  c: TDxComponentJson;
begin
  for c in List do
    c.Free;
  inherited;
end;

function GetComponentsHierarhyJson(const Components: TDxComponentList): String;
var
  f: TSerializationContainer;
  j: TJsonObject;
begin
  f:= TSerializationContainer.Create(Components);
  try
    j := TJson.ObjectToJsonObject(f);
    try
      Result := j.Pairs[0].JsonValue.Format(2);
    finally
      j.Free;
    end;
  finally
    f.Free;
  end;
end;

{ TDxPackage }

constructor TDxPackage.Create(const Path, Name, Prefix, SourcePrefix: String);
begin
  inherited Create;
  FName := Name;
  FPrefix := Prefix;
  FPath := Path;
  if SourcePrefix = EmptyStr then
    FSourcePrefix := FPrefix
  else
    FSourcePrefix := SourcePrefix;
  if Pos('IBX', FName) > 0 then FCategory := dxpcIBX
    else if Pos('TeeChart', FName)> 0 then FCategory := dxpcTeeChart
    else if Pos('FireDAC', FName) > 0 then FCategory := dxpcFireDAC
    else if Pos('BDE', FName) > 0 then FCategory := dxpcBDE
    else FCategory := dxpcNormal;
  FDescription := '';
  FUsage := dxpuDesigntimeAndRuntime;
  FRequires := TStringList.Create;
  FExists := FileExists(FullSourceFileName);
  FRequired := True;
  FDependentComponents := TDxComponentList.Create(False);
  FContains := TStringList.Create;;
  ReadOptions;
end;

destructor TDxPackage.Destroy;
begin
  FRequires.Free;
  FDependentComponents.Free;
  FContains.Free;
  inherited;
end;

class function TDxPackage.ExtractPackageName(const Name: String): String;
var
  I: Integer;
begin
  Result := Name;
  for I := Length(Result) downto 1 do
    if CharInSet(Result[I], ['0'..'9']) then
      Delete(Result, I, 1)
    else
      Break;
  if SameText(Result[Length(Result)], 'D') then
    Result := Copy(Result, 1, Length(Result) - 1)
  else
    if SameText(Copy(Result, Length(Result) - 1, 2), 'RS') then
      Result := Copy(Result, 1, Length(Result) - 2);
end;

function TDxPackage.GetFullFileName: string;
begin
  Result := FPath + '\' + FullName + DPKExtName;
end;

function TDxPackage.GetFullName: string;
begin
  Result := FName + FPrefix;
end;

function TDxPackage.GetFullSourceFileName: string;
begin
  Result := FPath + '\' + SourceFullName + DPKExtName;
end;

class procedure TDxPackage.GetPackageNameAndPrefix(const FullName: String; out Name, Prefix: String);
begin
  Name := ExtractPackageName(FullName);
  Prefix := Copy(FullName, Length(Name) + 1, Length(FullName));
end;

function TDxPackage.GetSourceFullName: string;
begin
  Result := FName + FSourcePrefix;
end;

procedure TDxPackage.ReadOptions;
type
  TSection = (sNone, sRequieres, sContains);
var
  DPK: TStringList;
  S: String;
  section: TSection;
  targetList: TStringList;
begin
  if not Exists then Exit;
  DPK := TStringList.Create;
  try
    DPK.LoadFromFile(FullSourceFileName);
    section := sNone;
    targetList := nil;
    for S in DPK do begin
      if (section in [sRequieres, sContains]) then
      begin
        if (Trim(S) = EmptyStr) or (Pos('{$', S) > 0) then
          Continue;
        if Pos(',', S) > 0 then
          targetList.Add(ExtractPackageName(Trim(StringReplace(S, ',', '', []))))
        else
        begin
          targetList.Add(ExtractPackageName(Trim(StringReplace(S, ';', '', []))));
          section := sNone;
        end;
      end else
      begin
        if Pos(DPKDescriptionOptionIdent, S) > 0 then
          FDescription := Copy(S, Length(DPKDescriptionOptionIdent) + 1, Length(S) - Length(DPKDescriptionOptionIdent) - 2)
        else if Pos(DPKDesigntimeOnlyOptionIdent, S) > 0
          then FUsage := dxpuDesigntimeOnly
          else if Pos(DPKRuntimeOnlyOptionIdent, S) > 0 then
            FUsage := dxpuRuntimeOnly
          else if Trim(S) = DPKRequiresOptionIdent then
          begin
            section := sRequieres;
            targetList := FRequires;
          end
          else if Trim(S) = DPKContainsOptionIdent then
          begin
            section := sContains;
            targetList := FContains;
          end;
      end;
    end;
  finally
    DPK.Free;
  end;
end;

{ TDxComponent }

constructor TDxComponent.Create(ComponentProfile: TDxComponentProfile);
begin
  inherited Create;
  FProfile := ComponentProfile;
  FPackages := TDxPackageList.Create();
  FParentComponents := TDxComponentList.Create(False);
  FSubComponents := TDxComponentList.Create(False);
  FState := dxcsInstall;
end;

destructor TDxComponent.Destroy;
begin
  FPackages.Free;
  FParentComponents.Free;
  FSubComponents.Free;
  inherited;
end;

function TDxComponent.GetCanInstall: boolean;
var
  Package: TDxPackage;
  Component: TDxComponent;
  Cnt: Integer;
begin
  Result := false;
  for Component in FParentComponents do
    if not Component.Installable then
      Exit;
  Cnt := 0;
  for Package in Packages do
  begin
    if Package.Exists then
      Inc(Cnt)
    else if Package.Required then
      Exit;
  end;
  Result := Cnt > 0;
end;

function TDxComponent.GetExistsPackageCount: Integer;
var
  Package: TDxPackage;
begin
  Result := 0;
  for Package in Packages do if Package.Exists then Inc(Result);
end;

function TDxComponent.IsMissingDependents: Boolean;
var
  Comp: TDxComponent;
begin
  Result := False;
  for Comp in ParentComponents do
    if not (Comp.State in dxcsEditModes) then begin
      Result := True;
      Break;
    end;
end;

procedure TDxComponent.SetState(const Value: TDxComponentState);
var
  Comp: TDxComponent;
begin
  if State = Value then Exit;
  if not (State in dxcsEditModes) then Exit;
  case Value of
    dxcsInstall:    for Comp in ParentComponents do Comp.State := dxcsInstall;
    dxcsNotInstall: for Comp in SubComponents do Comp.State := dxcsNotInstall;
  end;
  FState := Value;
end;

end.
