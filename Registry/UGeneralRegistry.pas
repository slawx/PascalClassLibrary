unit UGeneralRegistry;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, XMLRead, XMLWrite, DOM
  {$IFDEF Windows}
  , WinRegistry
  {$ENDIF};

type
  TRegistryRoot = (rrUnknown, rrApplicationUser, rrApplicationGlobal,
    rrSystemUser, rrSystemGlobal);

  TRegKeyInfo = record
    NumberSubKeys: Integer;
    MaxSubKeyLength: Integer;
    NumberValues: Integer;
    MaxValueLength: Integer;
    MaxDataLength: Integer;
    CreationTime: TDateTime;
    ModificationTime: TDateTime;
  end;

  TRegValueType = (vtUnknown, vtInteger, vtString, vtBinary, vtFloat, vtBoolean,
    vtText);

  TRegValueInfo = record
    ValueType: TRegValueType;
    Size: Integer;
  end;

  TRegKey = record
    Root: NativeInt;
    Path: string;
  end;

  { TBaseRegistry }

  TBaseRegistry = class
  private
  protected
    FCurrentRoot: NativeInt;
    FCurrentKey: string;
    procedure SetCurrentKey(const AValue: string); virtual; abstract;
    procedure SetCurrentRoot(const AValue: NativeInt); virtual; abstract;
  public
    function KeyExists(const Name: string): Boolean; virtual; abstract;
    function ValueExists(const Name: string): Boolean; virtual; abstract;
    function GetKeyInfo(var Value: TRegKeyInfo): Boolean; virtual; abstract;
    function GetValueInfo(const Name: string; var Value: TRegValueInfo): Boolean; virtual; abstract;
    function GetValueType(const Name: string): TRegValueType; virtual; abstract;
    function GetValueSize(const Name: string): Integer; virtual; abstract;
    function OpenKey(const Key: string; CreateNew: Boolean): Boolean; virtual; abstract;
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
    function HasSubKeys: Boolean; virtual; abstract;
    procedure GetKeyNames(Strings: TStrings); virtual; abstract;
    procedure GetValueNames(Strings: TStrings); virtual; abstract;
    property CurrentRoot: NativeInt read FCurrentRoot write SetCurrentRoot;
    property CurrentKey: string read FCurrentKey write SetCurrentKey;
  end;

  EBackendNotDefined = class(Exception);

  { TGeneralRegistry }

  TGeneralRegistry = class(TComponent)
  private
    FBackend: TBaseRegistry;
    procedure CheckBackend; inline;
    function GetCurrentKey: string;
    function GetCurrentRoot: NativeInt;
    procedure SetBackend(const AValue: TBaseRegistry);
    procedure SetCurrentKey(const AValue: string);
    procedure SetCurrentRoot(const AValue: NativeInt);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CloseKey;
    function KeyExists(const Name: string): Boolean;
    function ValueExists(const Name: string): Boolean;
    function DeleteKey(const Name: string; Recursive: Boolean = False): Boolean;
    function DeleteValue(const Name: string): Boolean;
    function RenameValue(const OldName, NewName: string): Boolean;
    function GetKeyInfo(out Value: TRegKeyInfo): Boolean;
    function GetValueInfo(const Name: string; out Value: TRegValueInfo): Boolean;
    function GetValueType(const Name: string): TRegValueType;
    function GetValueSize(const Name: string): Integer;
    function OpenKey(const Key: string; CreateNew: Boolean): Boolean;
    function ReadBool(const Name: string): Boolean;
    function ReadDateTime(const Name: string): TDateTime;
    function ReadFloat(const Name: string): Double;
    function ReadInteger(const Name: string): Integer;
    function ReadString(const Name: string): string;
    function ReadBinaryData(const Name: string; var Buffer; BufSize: Integer): Integer;
    function ReadBoolDefault(const Name: string; const DefaultValue: Boolean): Boolean;
    function ReadDateTimeDefault(const Name: string; const DefaultValue: TDateTime): TDateTime;
    function ReadFloatDefault(const Name: string; const DefaultValue: Double): Double;
    function ReadIntegerDefault(const Name: string; const DefaultValue: Integer): Integer;
    function ReadStringDefault(const Name: string; const DefaultValue: string): string;
    procedure WriteBool(const Name: string; Value: Boolean);
    procedure WriteDateTime(const Name: string; Value: TDateTime);
    procedure WriteFloat(const Name: string; Value: Double);
    procedure WriteInteger(const Name: string; Value: Integer);
    procedure WriteString(const Name: string; Value: string);
    procedure WriteBinaryData(const Name: string; var Buffer; BufSize: Integer);
    function HasSubKeys: Boolean;
    procedure GetKeyNames(Strings: TStrings);
    procedure GetValueNames(Strings: TStrings);
    property Backend: TBaseRegistry read FBackend write SetBackend;
    property CurrentRoot: NativeInt read GetCurrentRoot write SetCurrentRoot;
    property CurrentKey: string read GetCurrentKey write SetCurrentKey;
  end;

  { TXMLRegistry }

  TXMLRegistry = class(TBaseRegistry)
    XMLDocument: TXMLDocument;
    function OpenKey(const Key: string; CreateNew: Boolean): Boolean; override;
    constructor Create;
    destructor Destroy; override;
  end;

  { TIniRegistry }

  TIniRegistry = class(TBaseRegistry)
    IniFile: TIniFile;
    constructor Create;
    destructor Destroy; override;
  end;

  TMemoryRegistry = class(TBaseRegistry)
  end;

  {$IFDEF Windows}

  { TWinRegistry }

  TWinRegistry = class(TBaseRegistry)
  protected
    procedure SetCurrentKey(const AValue: string); override;
    procedure SetCurrentRoot(const AValue: NativeInt); override;
  public
    BasePath: string;
    Registry: TRegistry;
    function KeyExists(const Name: string): Boolean; override;
    function ValueExists(const Name: string): Boolean; override;
    function GetKeyInfo(var Value: TRegKeyInfo): Boolean; override;
    function GetValueInfo(const Name: string; var Value: TRegValueInfo): Boolean; override;
    function GetValueType(const Name: string): TRegValueType; override;
    function GetValueSize(const Name: string): Integer; override;
    function OpenKey(const Key: string; CreateNew: Boolean): Boolean; override;
    procedure CloseKey; override;
    function DeleteKey(const Name: string; Recursive: Boolean = False): Boolean; override;
    function DeleteValue(const Name: string): Boolean; override;
    function RenameValue(const OldName, NewName: string): Boolean; override;
    function ReadBool(const Name: string): Boolean; override;
    function ReadDateTime(const Name: string): TDateTime; override;
    function ReadFloat(const Name: string): Double; override;
    function ReadInteger(const Name: string): Integer; override;
    function ReadString(const Name: string): string; override;
    function ReadBinaryData(const Name: string; var Buffer; BufSize: Integer): Integer; override;
    procedure WriteBool(const Name: string; Value: Boolean); override;
    procedure WriteDateTime(const Name: string; Value: TDateTime); override;
    procedure WriteFloat(const Name: string; Value: Double); override;
    procedure WriteInteger(const Name: string; Value: Integer); override;
    procedure WriteString(const Name: string; Value: string); override;
    procedure WriteBinaryData(const Name: string; var Buffer; BufSize: Integer); override;
    function HasSubKeys: Boolean; override;
    procedure GetKeyNames(Strings: TStrings); override;
    procedure GetValueNames(Strings: TStrings); override;
    constructor Create;
    destructor Destroy; override;
  end;
  {$ENDIF}

const
  RegValueTypeName: array[TRegValueType] of string = ('Unknown', 'Integer', 'String',
    'Binary', 'Float', 'Boolean', 'Text');

procedure Register;


implementation

resourcestring
  SBackendNotDefined = 'Backend not defined';

procedure Register;
begin
  RegisterComponents('Samples', [TGeneralRegistry]);
end;

{ TBaseRegistry }

{$IFDEF Windows}
{ TWinRegistry }

procedure TWinRegistry.SetCurrentKey(const AValue: string);
begin
  if FCurrentKey = AValue then Exit;
  OpenKey(AValue, False);
end;

procedure TWinRegistry.SetCurrentRoot(const AValue: NativeInt);
begin
  if FCurrentRoot = AValue then Exit;
  FCurrentRoot := AValue;
  if AValue = Integer(rrApplicationUser) then begin
    Registry.RootKey := HKEY_CURRENT_USER;
    BasePath := '\Software\Company\Product';
  end;
  if AValue = Integer(rrApplicationGlobal) then begin
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    BasePath := '\Software\Company\Product';
  end;
end;

function TWinRegistry.KeyExists(const Name: string): Boolean;
begin
  Result := Registry.KeyExists(Name);
end;

function TWinRegistry.ValueExists(const Name: string): Boolean;
begin
  Result := Registry.ValueExists(Name);
end;

function TWinRegistry.GetKeyInfo(var Value: TRegKeyInfo): Boolean;
var
  KeyInfo: WinRegistry.TRegKeyInfo;
begin
  Result := Registry.GetKeyInfo(KeyInfo);
  if Result then begin
    Value.CreationTime := KeyInfo.FileTime;
    Value.ModificationTime := KeyInfo.FileTime;
    Value.MaxDataLength := KeyInfo.MaxDataLen;
    Value.MaxSubKeyLength := KeyInfo.MaxSubKeyLen;
    Value.MaxValueLength := KeyInfo.MaxValueLen;
    Value.NumberSubKeys := KeyInfo.NumSubKeys;
    Value.NumberValues := KeyInfo.NumValues;
  end;
end;

function TWinRegistry.GetValueInfo(const Name: string; var Value: TRegValueInfo): Boolean;
var
  ValueInfo: WinRegistry.TRegDataInfo;
begin
  Result := Registry.GetDataInfo(Name, ValueInfo);
  if Result then begin
    Value.Size := ValueInfo.DataSize;
    case ValueInfo.RegData of
      rdUnknown: Value.ValueType := vtUnknown;
      rdString: Value.ValueType := vtString;
      rdExpandString: Value.ValueType := vtText;
      rdBinary: Value.ValueType := vtBinary;
      rdInteger: Value.ValueType := vtInteger;
    end;
  end;
end;

function TWinRegistry.GetValueType(const Name: string): TRegValueType;
var
  ValueInfo: TRegValueInfo;
begin
  if GetValueInfo(Name, ValueInfo) then
    Result := ValueInfo.ValueType else Result := vtUnknown;
end;

function TWinRegistry.GetValueSize(const Name: string): Integer;
begin
  Result := Registry.GetDataSize(Name);
end;

function TWinRegistry.OpenKey(const Key: string; CreateNew: Boolean): Boolean;
begin
  Result := Registry.OpenKey(Key, CreateNew);
  FCurrentKey := Key;
end;

procedure TWinRegistry.CloseKey;
begin
  Registry.CloseKey;
end;

function TWinRegistry.DeleteKey(const Name: string; Recursive: Boolean
  ): Boolean;
var
  SubKeys: TStringList;
  I: Integer;
begin
  try
    SubKeys := TStringList.Create;
    if Recursive and OpenKey(Name, False) and HasSubKeys then begin
      GetKeyNames(SubKeys);
      for I := 0 to SubKeys.Count - 1 do
        DeleteKey(Name + '\' + SubKeys[I], True);
    end;
    Result := Registry.DeleteKey(Name);
  finally
    SubKeys.Free;
  end;
end;

function TWinRegistry.DeleteValue(const Name: string): Boolean;
begin
  Result := Registry.DeleteValue(Name);
end;

function TWinRegistry.RenameValue(const OldName, NewName: string): Boolean;
begin
  Result := True;
  Registry.RenameValue(OldName, NewName);
end;

function TWinRegistry.ReadBool(const Name: string): Boolean;
begin
  Result := Registry.ReadBool(Name);
end;

function TWinRegistry.ReadDateTime(const Name: string): TDateTime;
begin
  Result := Registry.ReadDateTime(Name);
end;

function TWinRegistry.ReadFloat(const Name: string): Double;
begin
  Result := Registry.ReadFloat(Name);
end;

function TWinRegistry.ReadInteger(const Name: string): Integer;
begin
  Result := Registry.ReadInteger(Name);
end;

function TWinRegistry.ReadString(const Name: string): string;
begin
  Result := Registry.ReadString(Name);
end;

function TWinRegistry.ReadBinaryData(const Name: string; var Buffer;
  BufSize: Integer): Integer;
begin
  Result := Registry.ReadBinaryData(Name, Buffer, BufSize);
end;

procedure TWinRegistry.WriteBool(const Name: string; Value: Boolean);
begin
  Registry.WriteBool(Name, Value);
end;

procedure TWinRegistry.WriteDateTime(const Name: string; Value: TDateTime);
begin
  Registry.WriteDateTime(Name, Value);
end;

procedure TWinRegistry.WriteFloat(const Name: string; Value: Double);
begin
  Registry.WriteFloat(Name, Value);
end;

procedure TWinRegistry.WriteInteger(const Name: string; Value: Integer);
begin
  Registry.WriteInteger(Name, Value);
end;

procedure TWinRegistry.WriteString(const Name: string; Value: string);
begin
  Registry.WriteString(Name, Value);
end;

procedure TWinRegistry.WriteBinaryData(const Name: string; var Buffer;
  BufSize: Integer);
begin
  Registry.WriteBinaryData(Name, Buffer, BufSize);
end;

function TWinRegistry.HasSubKeys: Boolean;
begin
  Result := Registry.HasSubKeys;
end;

procedure TWinRegistry.GetKeyNames(Strings: TStrings);
begin
  Registry.GetKeyNames(Strings);
end;

procedure TWinRegistry.GetValueNames(Strings: TStrings);
begin
  Registry.GetValueNames(Strings);
end;

constructor TWinRegistry.Create;
begin
  Registry := TRegistry.Create;
end;

destructor TWinRegistry.Destroy;
begin
  Registry.Free;
  inherited Destroy;
end;
{$ENDIF}

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

function TXMLRegistry.OpenKey(const Key: string; CreateNew: Boolean): Boolean;
begin

end;

constructor TXMLRegistry.Create;
begin
  XMLDocument := TXMLDocument.Create;
end;

destructor TXMLRegistry.Destroy;
begin
  XMLDocument.Free;
  inherited Destroy;
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

function TGeneralRegistry.GetCurrentRoot: NativeInt;
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

procedure TGeneralRegistry.SetCurrentRoot(const AValue: NativeInt);
begin
  CheckBackend;
  Backend.CurrentRoot := AValue;
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

function TGeneralRegistry.GetKeyInfo(out Value: TRegKeyInfo): Boolean;
begin
  CheckBackend;
  Result := Backend.GetKeyInfo(Value);
end;

function TGeneralRegistry.GetValueInfo(const Name: string; out Value: TRegValueInfo): Boolean;
begin
  CheckBackend;
  Result := Backend.GetValueInfo(Name, Value);
end;

function TGeneralRegistry.GetValueType(const Name: string): TRegValueType;
begin
  CheckBackend;
  Result := Backend.GetValueType(Name);
end;

function TGeneralRegistry.GetValueSize(const Name: string): Integer;
begin
  CheckBackend;
  Result := Backend.GetValueSize(Name);
end;

function TGeneralRegistry.DeleteKey(const Name: string; Recursive: Boolean = False): Boolean;
begin
  CheckBackend;
  Result := Backend.DeleteKey(Name, Recursive);
end;

function TGeneralRegistry.OpenKey(const Key: string; CreateNew: Boolean): Boolean;
begin
  CheckBackend;
  Result := Backend.OpenKey(Key, CreateNew);
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
  Result := Backend.ReadInteger(Name);
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

function TGeneralRegistry.ReadBoolDefault(const Name: string;
  const DefaultValue: Boolean): Boolean;
begin
  if ValueExists(Name) then Result := ReadBool(Name)
    else Result := DefaultValue;
end;

function TGeneralRegistry.ReadDateTimeDefault(const Name: string;
  const DefaultValue: TDateTime): TDateTime;
begin
  if ValueExists(Name) then Result := ReadDateTime(Name)
    else Result := DefaultValue;
end;

function TGeneralRegistry.ReadFloatDefault(const Name: string;
  const DefaultValue: Double): Double;
begin
  if ValueExists(Name) then Result := ReadFloat(Name)
    else Result := DefaultValue;
end;

function TGeneralRegistry.ReadIntegerDefault(const Name: string;
  const DefaultValue: Integer): Integer;
begin
  if ValueExists(Name) then Result := ReadInteger(Name)
    else Result := DefaultValue;
end;

function TGeneralRegistry.ReadStringDefault(const Name: string;
  const DefaultValue: string): string;
begin
  if ValueExists(Name) then Result := ReadString(Name)
    else Result := DefaultValue;
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

function TGeneralRegistry.HasSubKeys: Boolean;
begin
  CheckBackend;
  Result := Backend.HasSubKeys;
end;

procedure TGeneralRegistry.GetKeyNames(Strings: TStrings);
begin
  CheckBackend;
  Backend.GetKeyNames(Strings);
end;

procedure TGeneralRegistry.GetValueNames(Strings: TStrings);
begin
  CheckBackend;
  Backend.GetValueNames(Strings);
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

