unit UGeneralRegistry;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  TRegKeyInfo = record
    NumberSubKeys: Integer;
    MaxSubKeyLength: Integer;
    NumberValues: Integer;
    MaxValueLength: Integer;
    MaxDataLength: Integer;
    CreationTime: TDateTime;
    ModificationTime: TDateTime;
  end;

  TRegValueType = (vtUnknown, vtInteger, vtString, vtBinary, vtFloat, vtBoolean);

  TRegValueInfo = record
    ValueType: TRegValueType;
    Size: Integer;
  end;

  { TBaseRegistry }

  TBaseRegistry = class
  private
    procedure SetCurrentKey(const AValue: string);
    procedure SetCurrentRoot(const AValue: string);
  protected
    FCurrentRoot: string;
    FCurrentKey: string;
  public
    function KeyExists(const Name: string): Boolean; virtual; abstract;
    function ValueExists(const Name: string): Boolean; virtual; abstract;
    procedure OpenKey(const Key: string; CreateNew: Boolean); virtual; abstract;
    procedure CloseKey; virtual; abstract;
    function DeleteKey(const Name: string; Recursive: Boolean = False): Boolean; virtual; abstract;
    function DeleteValue(const Name: string): Boolean; virtual; abstract;
    function RenameValue(const OldName, NewName: string): Boolean; virtual; abstract;
    function ReadBool(const Name: string): Boolean; virtual; abstract;
    function ReadDateTime(const Name: string): TDateTime; virtual; abstract;
    function ReadFloat(const Name: string): Double; virtual; abstract;
    function ReadInteger(const Name: string): Integer; virtual; abstract;
    function ReadString(const Name: string): string; virtual; abstract;
    function ReadBinaryData(const Name: string; var Buffer; BufSize: Integer): Integer; virtual; abstract;
    procedure WriteBool(const Name: string; Value: Boolean); virtual; abstract;
    procedure WriteDateTime(const Name: string; Value: TDateTime); virtual; abstract;
    procedure WriteFloat(const Name: string; Value: Double); virtual; abstract;
    procedure WriteInteger(const Name: string; Value: Integer); virtual; abstract;
    procedure WriteString(const Name: string; Value: string); virtual; abstract;
    procedure WriteBinaryData(const Name: string; var Buffer; BufSize: Integer); virtual; abstract;
    property CurrentRoot: string read FCurrentRoot write SetCurrentRoot;
    property CurrentKey: string read FCurrentKey write SetCurrentKey;
  end;

  EBackendNotDefined = class(Exception);

  { TGeneralRegistry }

  TGeneralRegistry = class(TComponent)
  private
    FBackend: TBaseRegistry;
    procedure CheckBackend; inline;
    function GetCurrentKey: string;
    function GetCurrentRoot: string;
    procedure SetBackend(const AValue: TBaseRegistry);
    procedure SetCurrentKey(const AValue: string);
    procedure SetCurrentRoot(const AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CloseKey;
    function KeyExists(const Name: string): Boolean;
    function ValueExists(const Name: string): Boolean;
    function DeleteKey(const Name: string; Recursive: Boolean = False): Boolean;
    function DeleteValue(const Name: string): Boolean;
    function RenameValue(const OldName, NewName: string): Boolean;
    procedure OpenKey(const Key: string; CreateNew: Boolean);
    function ReadBool(const Name: string): Boolean;
    function ReadDateTime(const Name: string): TDateTime;
    function ReadFloat(const Name: string): Double;
    function ReadInteger(const Name: string): Integer;
    function ReadString(const Name: string): string;
    function ReadBinaryData(const Name: string; var Buffer; BufSize: Integer): Integer;
    procedure WriteBool(const Name: string; Value: Boolean);
    procedure WriteDateTime(const Name: string; Value: TDateTime);
    procedure WriteFloat(const Name: string; Value: Double);
    procedure WriteInteger(const Name: string; Value: Integer);
    procedure WriteString(const Name: string; Value: string);
    procedure WriteBinaryData(const Name: string; var Buffer; BufSize: Integer);
    property Backend: TBaseRegistry read FBackend write SetBackend;
    property CurrentRoot: string read GetCurrentRoot write SetCurrentRoot;
    property CurrentKey: string read GetCurrentKey write SetCurrentKey;
  end;

  { TXMLRegistry }

  TXMLRegistry = class(TBaseRegistry)
    procedure OpenKey(const Key: string; CreateNew: Boolean); override;
  end;

  { TIniRegistry }

  TIniRegistry = class(TBaseRegistry)
    IniFile: TIniFile;
    constructor Create;
    destructor Destroy; override;
  end;

  TMemoryRegistry = class(TBaseRegistry)
  end;

  TWinRegistry = class(TBaseRegistry)

  end;

procedure Register;


implementation

resourcestring
  SBackendNotDefined = 'Backend not defined';

procedure Register;
begin
  RegisterComponents('Samples', [TGeneralRegistry]);
end;

procedure TBaseRegistry.SetCurrentKey(const AValue: string);
begin
  if FCurrentKey=AValue then Exit;
  FCurrentKey:=AValue;
end;

procedure TBaseRegistry.SetCurrentRoot(const AValue: string);
begin
  if FCurrentRoot=AValue then Exit;
  FCurrentRoot:=AValue;
end;

{ TBaseRegistry }

{ TIniRegistry }

constructor TIniRegistry.Create;
begin
  //IniFile := TIniFile.Create;
end;

destructor TIniRegistry.Destroy;
begin
  //IniFile.Free;
  inherited Destroy;
end;

{ TXMLRegistry }

procedure TXMLRegistry.OpenKey(const Key: string; CreateNew: Boolean);
begin

end;

{ TGeneralRegistry }

procedure TGeneralRegistry.CheckBackend;
begin
  if not Assigned(Backend) then
    raise EBackendNotDefined.Create(SBackendNotDefined);
end;

function TGeneralRegistry.GetCurrentKey: string;
begin
  CheckBackend;
  Result := Backend.CurrentKey;
end;

function TGeneralRegistry.GetCurrentRoot: string;
begin
  CheckBackend;
  Result := Backend.CurrentRoot;
end;

procedure TGeneralRegistry.SetBackend(const AValue: TBaseRegistry);
begin
  if FBackend = AValue then Exit;
  if Assigned(FBackend) then FreeAndNil(FBackend);
  FBackend := AValue;
end;

procedure TGeneralRegistry.SetCurrentKey(const AValue: string);
begin
  CheckBackend;
  Backend.CurrentKey := AValue;
end;

procedure TGeneralRegistry.SetCurrentRoot(const AValue: string);
begin
  CheckBackend;
  Backend.CurrentKey := AValue;
end;

procedure TGeneralRegistry.CloseKey;
begin
  CheckBackend;
  Backend.CloseKey;
end;

function TGeneralRegistry.KeyExists(const Name: string): Boolean;
begin
  CheckBackend;
  Result := Backend.KeyExists(Name);
end;

function TGeneralRegistry.ValueExists(const Name: string): Boolean;
begin
  CheckBackend;
  Result := Backend.ValueExists(Name);
end;

function TGeneralRegistry.DeleteValue(const Name: string): Boolean;
begin
  CheckBackend;
  Result := Backend.DeleteValue(Name);
end;

function TGeneralRegistry.RenameValue(const OldName, NewName: string): Boolean;
begin
  CheckBackend;
  Result := Backend.RenameValue(OldName, NewName);
end;

function TGeneralRegistry.DeleteKey(const Name: string; Recursive: Boolean = False): Boolean;
begin
  CheckBackend;
  Result := Backend.DeleteKey(Name, Recursive);
end;

procedure TGeneralRegistry.OpenKey(const Key: string; CreateNew: Boolean);
begin
  CheckBackend;
  Backend.OpenKey(Key, CreateNew);
end;

function TGeneralRegistry.ReadBool(const Name: string): Boolean;
begin
  CheckBackend;
  Result := Backend.ReadBool(Name);
end;

function TGeneralRegistry.ReadDateTime(const Name: string): TDateTime;
begin
  CheckBackend;
  Result := Backend.ReadDateTime(Name);
end;

function TGeneralRegistry.ReadFloat(const Name: string): Double;
begin
  CheckBackend;
  Result := Backend.ReadFloat(Name);
end;

function TGeneralRegistry.ReadInteger(const Name: string): Integer;
begin
  CheckBackend;
  Result := ReadInteger(Name);
end;

function TGeneralRegistry.ReadString(const Name: string): string;
begin
  CheckBackend;
  Result := Backend.ReadString(Name);
end;

function TGeneralRegistry.ReadBinaryData(const Name: string; var Buffer;
  BufSize: Integer): Integer;
begin
  CheckBackend;
  Result := Backend.ReadBinaryData(Name, Buffer, BufSize);
end;

procedure TGeneralRegistry.WriteBool(const Name: string; Value: Boolean);
begin
  CheckBackend;
  Backend.WriteBool(Name, Value);
end;

procedure TGeneralRegistry.WriteDateTime(const Name: string; Value: TDateTime);
begin
  CheckBackend;
  Backend.WriteDateTime(Name, Value);
end;

procedure TGeneralRegistry.WriteFloat(const Name: string; Value: Double);
begin
  CheckBackend;
  Backend.WriteFloat(Name, Value);
end;

procedure TGeneralRegistry.WriteInteger(const Name: string; Value: Integer);
begin
  CheckBackend;
  Backend.WriteInteger(Name, Value);
end;

procedure TGeneralRegistry.WriteString(const Name: string; Value: string);
begin
  CheckBackend;
  Backend.WriteString(Name, Value);
end;

procedure TGeneralRegistry.WriteBinaryData(const Name: string; var Buffer;
  BufSize: Integer);
begin
  CheckBackend;
  Backend.WriteBinaryData(Name, Buffer, BufSize);
end;

constructor TGeneralRegistry.Create(AOwner: TComponent);
begin
  {$IFDEF Windows}
  Backend := TWinRegistry.Create;
  //TWinRegistry(Backend).RootKey := HKEY_CURRENT_USER;
  //TWinRegistry(Backend).BaseKey := 'Software\' + CompanyName + '\' +
  //  ApplicationName;
  {$ENDIF}
  {$IFDEF Linux}
  Backend := TXMLRegistry.Create;
  //TXMLRegistry(Backend).FileName := HomeDir + DirectorySeparator + ApplicationName +
  //  ExtensionSeparator + '.xml';
  {$ENDIF}
  inherited Create(AOwner);
end;

destructor TGeneralRegistry.Destroy;
begin
  if Assigned(Backend) then Backend.Free;
  inherited Destroy;
end;

end.

