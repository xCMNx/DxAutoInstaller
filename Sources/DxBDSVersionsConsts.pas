/// FROM JclIDEUtils
unit DxBDSVersionsConsts;

interface

type
  TBDSVersionInfo = record
    Name: PResStringRec;
    VersionStr: string;
    DCCVersion: Single;
    IDEPkgVersion: Integer;
    PkgVersion: Integer;
    CoreIdeVersion: string;
    Supported: Boolean;
  end;

resourcestring
  RsDelphiName          = 'Delphi';
  RsBCBName             = 'C++Builder';
  RsCSharpName          = 'C#Builder';
  RsBDSName             = 'Borland Developer Studio';
  RsRSName              = 'RAD Studio';

const
  BDSVersions: array [1..22] of TBDSVersionInfo = (
    (
      Name: @RsCSharpName;
      VersionStr: '1.0';
      DCCVersion: 0.0;
      IDEPkgVersion: 1;
      PkgVersion: 1;
      CoreIdeVersion: '71';
      Supported: True),
    (
      Name: @RsDelphiName;
      VersionStr: '8';
      DCCVersion: 15.0; // Delphi 8 used the Delphi 7 compiler
      IDEPkgVersion: 8;
      PkgVersion: 7;
      CoreIdeVersion: '71';
      Supported: True),
    (
      Name: @RsDelphiName;
      VersionStr: '2005';
      DCCVersion: 17.0;
      IDEPkgVersion: 9;
      PkgVersion: 9;
      CoreIdeVersion: '90';
      Supported: True),
    (
      Name: @RsBDSName;
      VersionStr: '2006';
      DCCVersion: 18.0;
      IDEPkgVersion: 10;
      PkgVersion: 10;
      CoreIdeVersion: '100';
      Supported: True),
    (
      Name: @RsRSName;
      VersionStr: '2007';
      DCCVersion: 18.5;
      IDEPkgVersion: 11; // Delphi 2007 IDE is 11 but runtime are 10
      PkgVersion: 10;
      CoreIdeVersion: '100';
      Supported: True),
    (
      Name: @RsRSName;
      VersionStr: '2009';
      DCCVersion: 20.0; // Delphi.NET 2009 is 19.0
      IDEPkgVersion: 12;
      PkgVersion: 12;
      CoreIdeVersion: '120';
      Supported: True),
    (
      Name: @RsRSName;
      VersionStr: '2010';
      DCCVersion: 21.0;
      IDEPkgVersion: 14;
      PkgVersion: 14;
      CoreIdeVersion: '140';
      Supported: True),
    (
      Name: @RsRSName;
      VersionStr: 'XE';
      DCCVersion: 22.0;
      IDEPkgVersion: 15;
      PkgVersion: 15;
      CoreIdeVersion: '150';
      Supported: True),
    (
      Name: @RsRSName;
      VersionStr: 'XE2';
      DCCVersion: 23.0;
      IDEPkgVersion: 16;
      PkgVersion: 16;
      CoreIdeVersion: '160';
      Supported: True),
    (
      Name: @RsRSName;
      VersionStr: 'XE3';
      DCCVersion: 24.0;
      IDEPkgVersion: 17;
      PkgVersion: 17;
      CoreIdeVersion: '170';
      Supported: True),
    (
      Name: @RsRSName;
      VersionStr: 'XE4';
      DCCVersion: 25.0;
      IDEPkgVersion: 18;
      PkgVersion: 18;
      CoreIdeVersion: '180';
      Supported: True),
    (
      Name: @RsRSName;
      VersionStr: 'XE5';
      DCCVersion: 26.0;
      IDEPkgVersion: 19;
      PkgVersion: 19;
      CoreIdeVersion: '190';
      Supported: True),
    (
      Name: nil; // "Appmethod"
      VersionStr: '';
      DCCVersion: 0.0;
      IDEPkgVersion: 0;
      PkgVersion: 0;
      CoreIdeVersion: '';
      Supported: False),
    (
      Name: @RsRSName;
      VersionStr: 'XE6';
      DCCVersion: 27.0;
      IDEPkgVersion: 20;
      PkgVersion: 20;
      CoreIdeVersion: '200';
      Supported: True),
    (
      Name: @RsRSName;
      VersionStr: 'XE7';
      DCCVersion: 28.0;
      IDEPkgVersion: 21;
      PkgVersion: 21;
      CoreIdeVersion: '210';
      Supported: True),
    (
      Name: @RsRSName;
      VersionStr: 'XE8';
      DCCVersion: 29.0;
      IDEPkgVersion: 22;
      PkgVersion: 22;
      CoreIdeVersion: '220';
      Supported: True),
    (
      Name: @RsRSName;
      VersionStr: '10';
      DCCVersion: 30.0;
      IDEPkgVersion: 23;
      PkgVersion: 23;
      CoreIdeVersion: '230';
      Supported: True),
    (
      Name: @RsRSName;
      VersionStr: '10.1';
      DCCVersion: 31.0;
      IDEPkgVersion: 24;
      PkgVersion: 24;
      CoreIdeVersion: '240';
      Supported: True),
    (
      Name: @RsRSName;
      VersionStr: '10.2';
      DCCVersion: 32.0;
      IDEPkgVersion: 25;
      PkgVersion: 25;
      CoreIdeVersion: '250';
      Supported: True),
    (
      Name: @RsRSName;
      VersionStr: '10.3';
      DCCVersion: 33.0;
      IDEPkgVersion: 26;
      PkgVersion: 26;
      CoreIdeVersion: '260';
      Supported: True),
    (
      Name: @RsRSName;
      VersionStr: '10.4';
      DCCVersion: 34.0;
      IDEPkgVersion: 27;
      PkgVersion: 27;
      CoreIdeVersion: '270';
      Supported: True),
    (
      Name: @RsRSName;
      VersionStr: '11';
      DCCVersion: 35.0;
      IDEPkgVersion: 28;
      PkgVersion: 28;
      CoreIdeVersion: '280';
      Supported: True)
  );
  BDSVersionNames: array [Low(BDSVersions)..High(BDSVersions)] of String = (
    '1.0',
    '8',
    '2005',
    '2006',
    '2007',
    '2009',
    '2010',
    'XE',
    'XE2',
    'XE3',
    'XE4',
    'XE5',
    '',
    'XE6',
    'XE7',
    'XE8',
    'Seatle',
    'Berlin',
    'Tokyo',
    'Rio',
    'Sydney',
    'Alexandria'
  );


implementation

end.
