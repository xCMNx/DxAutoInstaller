{*******************************************************}
{                                                       }
{       DxAutoInstaller Component Factory Classes       }
{                                                       }
{        http://www.delphier.com/DxAutoIntaller         }
{        Copyright(c) 2014 by faceker@gmail.com         }
{                                                       }
{*******************************************************}

unit DxComponentFactory;

interface

uses
  SysUtils, Classes, DxComponent, DxInstaller, DxProfile, DxIDE;

type
  TDxComponentFactory = class
  private
    FInstaller: TDxInstaller;
    FFindSourcePackage: Boolean;
    function CreateComponent(IDE: TDxIDE; ComponentProfile: TDxComponentProfile): TDxComponent;
    procedure CreatePackageList(Component: TDxComponent; IDE: TDxIDE; IsRequiredPackages: Boolean);
  public
    constructor Create(Installer: TDxInstaller; FindSourcePackage: Boolean);
    property Installer: TDxInstaller read FInstaller;
    procedure BuildComponentList(IDE: TDxIDE; List: TDxComponentList);
  end;


implementation

{ TDxComponentFactory }

constructor TDxComponentFactory.Create(Installer: TDxInstaller; FindSourcePackage: Boolean);
begin
  inherited Create;
  FInstaller := Installer;
  FFindSourcePackage := FindSourcePackage;
end;

procedure TDxComponentFactory.BuildComponentList(IDE: TDxIDE; List:TDxComponentList);
var
  CompProfile: TDxComponentProfile;
  Comp, C: TDxComponent;
  Package, P: TDxPackage;
begin
  List.Clear;
  for CompProfile in Installer.Profile.Components do List.Add(CreateComponent(IDE, CompProfile));

  for Comp in List do begin
    for Package in Comp.Packages do begin
      // Set Package Dependents;
      for C in List do if C <> Comp then
        for P in C.Packages do
          if Package.Requires.IndexOf(P.Name) >= 0 then begin
            Package.DependentComponents.Add(C);
            // Set Component Dependents;
            if Package.Required then begin
              if Comp.ParentComponents.IndexOf(C) < 0 then begin
                Comp.ParentComponents.Add(C);
                if C.SubComponents.IndexOf(Comp) < 0 then C.SubComponents.Add(Comp);
              end;
            end;
            Break;
          end;
    end;
    // Set Component State;
    if not DirectoryExists(TDxProfile.GetComponentDir(Installer.InstallFileDir, Comp.Profile.ComponentName)) then
      Comp.State := dxcsNotFound
    else if Comp.GetExistsPackageCount = 0 then
      Comp.State := dxcsNotSupported
    else if Comp.IsMissingDependents then
      Comp.State := dxcsMissing;
  end;
end;

function TDxComponentFactory.CreateComponent(IDE: TDxIDE; ComponentProfile: TDxComponentProfile): TDxComponent;
begin
  Result := TDxComponent.Create(ComponentProfile);
  CreatePackageList(Result, IDE, True);
  CreatePackageList(Result, IDE, False);
end;

procedure TDxComponentFactory.CreatePackageList(Component: TDxComponent; IDE: TDxIDE; IsRequiredPackages: Boolean);
var
  List: TStringList;
  PackageName, FileName: String;
  Package: TDxPackage;
  ComponentPath, Postfix, SourcePostfix: String;
  i: Byte;
begin
  if IsRequiredPackages then List := Component.Profile.RequiredPackages else List := Component.Profile.OptionalPackages;
  ComponentPath := TDxProfile.GetComponentPackagesDir(Installer.InstallFileDir, Component.Profile.ComponentName);
  for PackageName in List do begin
    Postfix := TDxProfile.GetIDEVersionNumberStr(IDE);
    SourcePostfix := EmptyStr;
    FileName := ComponentPath + '\' + PackageName + Postfix + IDE.PackageSourceFileExtension;
    if not FileExists(FileName) then
    begin
      if FFindSourcePackage then
      begin
        i := 1;
        while (IDE.IDEPackageVersionNumber + i < 29) or (IDE.IDEPackageVersionNumber - i > 1) do
        begin
          SourcePostfix := TDxProfile.GetPackagePostfix(IDE.IDEPackageVersionNumber + i);
          FileName := ComponentPath + '\' +  PackageName + SourcePostfix + IDE.PackageSourceFileExtension;
          if FileExists(FileName) then
            break;
          SourcePostfix := TDxProfile.GetPackagePostfix(IDE.IDEPackageVersionNumber - i);
          FileName := ComponentPath + '\' +  PackageName + SourcePostfix + IDE.PackageSourceFileExtension;
          if FileExists(FileName) then
            break;
          Inc(i);
        end;
      end;
      if not FileExists(FileName) then
        Continue;
    end;
    Package := TDxPackage.Create(ComponentPath, PackageName, Postfix, SourcePostfix);
    Package.Required := IsRequiredPackages;
    Component.Packages.Add(Package);
  end;
end;


end.
