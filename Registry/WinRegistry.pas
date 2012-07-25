// FPC windows registry implementation with removed linux xmlregistry imitation

unit WinRegistry;

{$mode objfpc}
{$H+}

interface

uses
  Windows, Classes, SysUtils, IniFiles;

type
  LPDWORD = ^DWord;
  LPVOID  = Pointer;
  WINBOOL = LongBool;
  LPCSTR  = PChar;
  LPSTR   = Pchar;
  LONG    = LongInt;
  LPBYTE  = ^Byte;

  ACCESS_MASK = DWORD;
       REGSAM = ACCESS_MASK;

  SECURITY_ATTRIBUTES = record
    nLength : DWORD;
    lpSecurityDescriptor : LPVOID;
    bInheritHandle : WINBOOL;
  end;
  LPSECURITY_ATTRIBUTES = ^SECURITY_ATTRIBUTES;


  HKEY = THandle;
  PHKEY = ^HKEY;



Const
  HKEY_CLASSES_ROOT     = HKEY($80000000);
  HKEY_CURRENT_USER     = HKEY($80000001);
  HKEY_LOCAL_MACHINE    = HKEY($80000002);
  HKEY_USERS            = HKEY($80000003);
  HKEY_PERFORMANCE_DATA = HKEY($80000004);
  HKEY_CURRENT_CONFIG   = HKEY($80000005);
  HKEY_DYN_DATA         = HKEY($80000006);

  KEY_ALL_ACCESS         = $F003F;
  KEY_CREATE_LINK        = 32;
  KEY_CREATE_SUB_KEY     = 4;
  KEY_ENUMERATE_SUB_KEYS = 8;
  KEY_EXECUTE            = $20019;
  KEY_NOTIFY             = 16;
  KEY_QUERY_VALUE        = 1;
  KEY_READ               = $20019;
  KEY_SET_VALUE          = 2;
  KEY_WRITE              = $20006;

  REG_BINARY                     = 3;
  REG_DWORD                      = 4;
  REG_DWORD_LITTLE_ENDIAN        = 4;
  REG_DWORD_BIG_ENDIAN           = 5;
  REG_EXPAND_SZ                  = 2;
  REG_FULL_RESOURCE_DESCRIPTOR   = 9;
  REG_LINK                       = 6;
  REG_MULTI_SZ                   = 7;
  REG_NONE                       = 0;
  REG_RESOURCE_LIST              = 8;
  REG_RESOURCE_REQUIREMENTS_LIST = 10;
  REG_SZ                         = 1;

  REG_OPTION_VOLATILE            = 1;
  REG_OPTION_NON_VOLATILE        = 0;
  REG_CREATED_NEW_KEY            = 1;
  REG_OPENED_EXISTING_KEY        = 2;

  ERROR_SUCCESS = 0;



type
  ERegistryException = class(Exception);

  TRegKeyInfo = record
    NumSubKeys: Integer;
    MaxSubKeyLen: Integer;
    NumValues: Integer;
    MaxValueLen: Integer;
    MaxDataLen: Integer;
    FileTime: TDateTime;
  end;

  TRegDataType = (rdUnknown, rdString, rdExpandString, rdBinary, rdInteger);

  TRegDataInfo = record
    RegData: TRegDataType;
    DataSize: Integer;
  end;

{ ---------------------------------------------------------------------
    TRegistry
  ---------------------------------------------------------------------}

  TRegistry = class(TObject)
  private
    FStringSizeIncludesNull : Boolean;
    FSysData : Pointer;
    fAccess: LongWord;
    fCurrentKey: HKEY;
    fRootKey: HKEY;
    fLazyWrite: Boolean;
    fCurrentPath: string;
    procedure SetRootKey(Value: HKEY);
    Procedure SysRegCreate;
    Procedure SysRegFree;
    Function  SysGetData(const Name: String; Buffer: Pointer; BufSize: Integer; var RegData: TRegDataType): Integer;
    Function  SysPutData(const Name: string; Buffer: Pointer; BufSize: Integer; RegData: TRegDataType) : Boolean;
    Function  SysCreateKey(const Key: String): Boolean;
  protected
    function GetBaseKey(Relative: Boolean): HKey;
    function GetData(const Name: string; Buffer: Pointer;
                  BufSize: Integer; var RegData: TRegDataType): Integer;
    function GetKey(const Key: string): HKEY;
    procedure ChangeKey(Value: HKey; const Path: string);
    procedure PutData(const Name: string; Buffer: Pointer;
                  BufSize: Integer; RegData: TRegDataType);
    procedure SetCurrentKey(Value: HKEY);
  public
    constructor Create; overload;
    constructor Create(aaccess:longword); overload;
    destructor Destroy; override;

    function CreateKey(const Key: string): Boolean;
    function DeleteKey(const Key: string): Boolean;
    function DeleteValue(const Name: string): Boolean;
    function GetDataInfo(const ValueName: string; var Value: TRegDataInfo): Boolean;
    function GetDataSize(const ValueName: string): Integer;
    function GetDataType(const ValueName: string): TRegDataType;
    function GetKeyInfo(var Value: TRegKeyInfo): Boolean;
    function HasSubKeys: Boolean;
    function KeyExists(const Key: string): Boolean;
    function LoadKey(const Key, FileName: string): Boolean;
    function OpenKey(const Key: string; CanCreate: Boolean): Boolean;
    function OpenKeyReadOnly(const Key: string): Boolean;
    function ReadCurrency(const Name: string): Currency;
    function ReadBinaryData(const Name: string; var Buffer; BufSize: Integer): Integer;
    function ReadBool(const Name: string): Boolean;
    function ReadDate(const Name: string): TDateTime;
    function ReadDateTime(const Name: string): TDateTime;
    function ReadFloat(const Name: string): Double;
    function ReadInteger(const Name: string): Integer;
    function ReadString(const Name: string): string;
    function ReadTime(const Name: string): TDateTime;
    function RegistryConnect(const UNCName: string): Boolean;
    function ReplaceKey(const Key, FileName, BackUpFileName: string): Boolean;
    function RestoreKey(const Key, FileName: string): Boolean;
    function SaveKey(const Key, FileName: string): Boolean;
    function UnLoadKey(const Key: string): Boolean;
    function ValueExists(const Name: string): Boolean;

    procedure CloseKey;
    procedure CloseKey(key:HKEY);
    procedure GetKeyNames(Strings: TStrings);
    procedure GetValueNames(Strings: TStrings);
    procedure MoveKey(const OldName, NewName: string; Delete: Boolean);
    procedure RenameValue(const OldName, NewName: string);
    procedure WriteCurrency(const Name: string; Value: Currency);
    procedure WriteBinaryData(const Name: string; var Buffer; BufSize: Integer);
    procedure WriteBool(const Name: string; Value: Boolean);
    procedure WriteDate(const Name: string; Value: TDateTime);
    procedure WriteDateTime(const Name: string; Value: TDateTime);
    procedure WriteFloat(const Name: string; Value: Double);
    procedure WriteInteger(const Name: string; Value: Integer);
    procedure WriteString(const Name, Value: string);
    procedure WriteExpandString(const Name, Value: string);
    procedure WriteTime(const Name: string; Value: TDateTime);

    property Access: LongWord read fAccess write fAccess;
    property CurrentKey: HKEY read fCurrentKey;
    property CurrentPath: string read fCurrentPath;
    property LazyWrite: Boolean read fLazyWrite write fLazyWrite;
    property RootKey: HKEY read fRootKey write SetRootKey;
    Property StringSizeIncludesNull : Boolean read FStringSizeIncludesNull;
  end;

{ ---------------------------------------------------------------------
    TRegIniFile
  ---------------------------------------------------------------------}
  TRegIniFile = class(TRegistry)
  private
    fFileName          : String;
    fPath              : String;
    fPreferStringValues: Boolean;
  public
    constructor Create(const FN: string); overload;
    constructor Create(const FN: string;aaccess:longword); overload;
    function ReadString(const Section, Ident, Default: string): string;
    function ReadInteger(const Section, Ident: string;
                Default: Longint): Longint;
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean;

    procedure WriteString(const Section, Ident, Value: String);
    procedure WriteInteger(const Section, Ident: string; Value: Longint);
    procedure WriteBool(const Section, Ident: string; Value: Boolean);
    procedure ReadSection(const Section: string; Strings: TStrings);
    procedure ReadSections(Strings: TStrings);
    procedure ReadSectionValues(const Section: string; Strings: TStrings);
    procedure EraseSection(const Section: string);
    procedure DeleteKey(const Section, Ident: String);

    property FileName: String read fFileName;
    property PreferStringValues: Boolean read fPreferStringValues
                write fPreferStringValues;
  end;

{ ---------------------------------------------------------------------
    TRegIniFile
  ---------------------------------------------------------------------}


  TRegistryIniFile = class(TCustomIniFile)
  private
    FRegIniFile: TRegIniFile;
  public
    constructor Create(const AFileName: string); overload;
    constructor Create(const AFileName: string; AAccess: LongWord); overload;
    function ReadDate(const Section, Name: string; Default: TDateTime): TDateTime; override;
    function ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime; override;
    function ReadInteger(const Section, Name: string; Default: Longint): Longint; override;
    function ReadFloat(const Section, Name: string; Default: Double): Double; override;
    function ReadString(const Section, Name, Default: string): string; override;
    function ReadTime(const Section, Name: string; Default: TDateTime): TDateTime; override;
    function ReadBinaryStream(const Section, Name: string; Value: TStream): Integer; override;
    procedure WriteDate(const Section, Name: string; Value: TDateTime); override;
    procedure WriteDateTime(const Section, Name: string; Value: TDateTime); override;
    procedure WriteFloat(const Section, Name: string; Value: Double); override;
    procedure WriteInteger(const Section, Name: string; Value: Longint); override;
    procedure WriteString(const Section, Name, Value: String); override;
    procedure WriteTime(const Section, Name: string; Value: TDateTime); override;
    procedure WriteBinaryStream(const Section, Name: string; Value: TStream); override;
    procedure ReadSection(const Section: string; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); override;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
    procedure EraseSection(const Section: string); override;
    procedure DeleteKey(const Section, Name: String); override;
    procedure UpdateFile; override;
    property RegIniFile: TRegIniFile read FRegIniFile;
  end;

ResourceString
  SInvalidRegType   = 'Invalid registry data type: "%s"';
  SRegCreateFailed  = 'Failed to create key: "%s"';
  SRegSetDataFailed = 'Failed to set data for value "%s"';
  SRegGetDataFailed = 'Failed to get data for value "%s"';

var
  GlobalXMLFile : Boolean = False;

implementation

{ ---------------------------------------------------------------------
    Include implementation-dependent code
  ---------------------------------------------------------------------}

{******************************************************************************
                                  TRegistry
 ******************************************************************************}

Procedure TRegistry.SysRegCreate;
begin
  FStringSizeIncludesNull:=True;
end;

Procedure TRegistry.SysRegfree;

begin
end;

Function PrepKey(Const S : String) : pChar;

begin
  Result:=PChar(S);
  If Result^='\' then
    Inc(Result);
end;

Function RelativeKey(Const S : String) : Boolean;

begin
  Result:=(S='') or (S[1]<>'\')
end;


function TRegistry.sysCreateKey(const Key: String): Boolean;
Var
  P: PChar;
  Disposition: Dword;
  Handle: HKEY;
  SecurityAttributes: Pointer; //LPSECURITY_ATTRIBUTES;

begin
  SecurityAttributes := Nil;
  P:=PrepKey(Key);
  Result:=RegCreateKeyExA(GetBaseKey(RelativeKey(Key)),
                         P,
                         0,
                         '',
                         REG_OPTION_NON_VOLATILE,
                         KEY_ALL_ACCESS,
                         SecurityAttributes,
                         Handle,
                         @Disposition) = ERROR_SUCCESS;
  RegCloseKey(Handle);
end;

function TRegistry.DeleteKey(const Key: String): Boolean;

Var
  P: PChar;
begin
  P:=PRepKey(Key);
  Result:=RegDeleteKeyA(GetBaseKey(RelativeKey(Key)),P)=ERROR_SUCCESS;
end;

function TRegistry.DeleteValue(const Name: String): Boolean;
begin
  Result := RegDeleteValueA(fCurrentKey, @Name[1]) = ERROR_SUCCESS;
end;

function TRegistry.SysGetData(const Name: String; Buffer: Pointer;
          BufSize: Integer; var RegData: TRegDataType): Integer;
Var
  P: PChar;
  RD : DWord;

begin
  P := PChar(Name);
  If RegQueryValueExA(fCurrentKey,P,Nil,
                      @RD,Buffer,lpdword(@BufSize))<>ERROR_SUCCESS Then
    Result:=-1
  else
    begin
    If (RD=REG_SZ) then
      RegData:=rdString
    else if (RD=REG_EXPAND_SZ) then
      Regdata:=rdExpandString
    else if (RD=REG_DWORD) then
      RegData:=rdInteger
    else if (RD=REG_BINARY) then
      RegData:=rdBinary
    else
      RegData:=rdUnknown;
    Result:=BufSize;
    end;
end;

function TRegistry.GetDataInfo(const ValueName: String; var Value: TRegDataInfo): Boolean;

Var
  P: PChar;

begin
  P:=PChar(ValueName);
  With Value do
    Result:=RegQueryValueExA(fCurrentKey,P,Nil,lpdword(@RegData),Nil,lpdword(@DataSize))=ERROR_SUCCESS;
  If Not Result Then
    begin
    Value.RegData := rdUnknown;
    Value.DataSize := 0
    end
end;


function TRegistry.GetKey(const Key: String): HKEY;
var
  S : string;
  Rel : Boolean;
begin
  Result:=0;
  S:=Key;
  Rel:=RelativeKey(S);
  if not(Rel) then
    Delete(S,1,1);
{$ifdef WinCE}
  RegOpenKeyEx(GetBaseKey(Rel),PWideChar(WideString(S)),0,FAccess,Result);
{$else WinCE}
  RegOpenKeyEx(GetBaseKey(Rel),PChar(S),0,FAccess,Result);
{$endif WinCE}
end;


function TRegistry.GetKeyInfo(var Value: TRegKeyInfo): Boolean;
var
  winFileTime: Windows.FILETIME;
  sysTime: TSystemTime;
begin
  FillChar(Value, SizeOf(Value), 0);
  With Value do
    Result:=RegQueryInfoKeyA(CurrentKey,nil,nil,nil,lpdword(@NumSubKeys),
              lpdword(@MaxSubKeyLen),nil,lpdword(@NumValues),lpdword(@MaxValueLen),
              lpdword(@MaxDataLen),nil,@winFileTime)=ERROR_SUCCESS;
  if Result then
  begin
    FileTimeToSystemTime(@winFileTime, @sysTime);
    Value.FileTime := SystemTimeToDateTime(sysTime);
  end;
end;


function TRegistry.KeyExists(const Key: string): Boolean;
var
  KeyHandle : HKEY;
  OldAccess : LONG;
begin
  Result:=false;
  OldAccess:=FAccess;
  try
    FAccess:=KEY_QUERY_VALUE or KEY_ENUMERATE_SUB_KEYS or STANDARD_RIGHTS_READ;
    KeyHandle:=GetKey(Key);
    if KeyHandle<>0 then
      begin
        RegCloseKey(KeyHandle);
        Result:=true;
      end;
  finally
    FAccess:=OldAccess;
  end;
end;


function TRegistry.LoadKey(const Key, FileName: string): Boolean;
begin
  Result := False;
end;


function TRegistry.OpenKey(const Key: string; CanCreate: Boolean): Boolean;

Var
  P: PChar;
  Handle: HKEY;
  Disposition: Integer;
  SecurityAttributes: Pointer; //LPSECURITY_ATTRIBUTES;

begin
  SecurityAttributes := Nil;
  P:=PrepKey(Key);
  If CanCreate then
    begin
    Handle:=0;
    Result:=RegCreateKeyExA(GetBaseKey(RelativeKey(Key)),P,0,'',

                           REG_OPTION_NON_VOLATILE,
                           fAccess,SecurityAttributes,Handle,
                           pdword(@Disposition))=ERROR_SUCCESS

    end
  else
    Result:=RegOpenKeyExA(GetBaseKey(RelativeKey(Key)),
                         P,0,fAccess,Handle)=ERROR_SUCCESS;
  If Result then
    fCurrentKey:=Handle;
end;

function TRegistry.OpenKeyReadOnly(const Key: string): Boolean;

Var
  P: PChar;
  Handle: HKEY;

begin
  P:=PrepKey(Key);
  Result := RegOpenKeyExA(GetBaseKey(RelativeKey(Key)),P,0,KEY_READ,Handle) = 0;
  If Result Then
    fCurrentKey := Handle;
end;

function TRegistry.RegistryConnect(const UNCName: string): Boolean;
begin
  Result := False;
end;

function TRegistry.ReplaceKey(const Key, FileName, BackUpFileName: string): Boolean;
begin
  Result := False;
end;

function TRegistry.RestoreKey(const Key, FileName: string): Boolean;
begin
  Result := False;
end;

function TRegistry.SaveKey(const Key, FileName: string): Boolean;
begin
  Result := False;
end;

function TRegistry.UnLoadKey(const Key: string): Boolean;
begin
  Result := false;
end;

function TRegistry.ValueExists(const Name: string): Boolean;

var
  Info : TRegDataInfo;

begin
  Result:=GetDataInfo(Name,Info);
end;

procedure TRegistry.CloseKey;
begin
  If (CurrentKey<>0) then
    begin
    if LazyWrite then
      RegCloseKey(CurrentKey)
    else
      RegFlushKey(CurrentKey);
    fCurrentKey:=0;
    end
end;

procedure TRegistry.CloseKey(key:HKEY);
begin
  RegCloseKey(CurrentKey)
end;

procedure TRegistry.ChangeKey(Value: HKey; const Path: String);
begin
  CloseKey;
  FCurrentKey:=Value;
  FCurrentPath:=Path;
end;

procedure TRegistry.GetKeyNames(Strings: TStrings);

Var
  L : Cardinal;
  I: Integer;
  Info: TRegKeyInfo;
  P : PChar;

begin
   Strings.Clear;
   if GetKeyInfo(Info) then
     begin
     L:=Info.MaxSubKeyLen+1;
     GetMem(P,L);
     Try
       for I:=0 to Info.NumSubKeys-1 do
         begin
         L:=Info.MaxSubKeyLen+1;
         RegEnumKeyExA(CurrentKey,I,P,L,Nil,Nil,Nil,Nil);
         Strings.Add(StrPas(P));
         end;
     Finally
       FreeMem(P);
     end;
     end;
end;

procedure TRegistry.GetValueNames(Strings: TStrings);

Var
  L : Cardinal;
  I: Integer;
  Info: TRegKeyInfo;
  P : PChar;

begin
   Strings.Clear;
   if GetKeyInfo(Info) then
     begin
     L:=Info.MaxValueLen+1;
     GetMem(P,L);
     Try
       for I:=0 to Info.NumValues-1 do
         begin
         L:=Info.MaxValueLen+1;
         RegEnumValueA(CurrentKey,I,P,L,Nil,Nil,Nil,Nil);
         Strings.Add(StrPas(P));
         end;
     Finally
       FreeMem(P);
     end;
     end;

end;

Function TRegistry.SysPutData(const Name: string; Buffer: Pointer;
  BufSize: Integer; RegData: TRegDataType) : Boolean;

Var
  P: PChar;
  RegDataType: DWORD;

begin
  Case RegData of
    rdUnknown      : RegDataType:=REG_NONE;
    rdString       : RegDataType:=REG_SZ;
    rdExpandString : RegDataType:=REG_EXPAND_SZ;
    rdInteger      : RegDataType:=REG_DWORD;
    rdBinary       : RegDataType:=REG_BINARY;
  end;
  P:=PChar(Name);
  Result:=RegSetValueExA(fCurrentKey,P,0,RegDataType,Buffer,BufSize)=ERROR_SUCCESS;
end;

procedure TRegistry.RenameValue(const OldName, NewName: string);

var
  L: Integer;
  InfoO,InfoN : TRegDataInfo;
  D : TRegDataType;
  P: PChar;

begin
  If GetDataInfo(OldName,InfoO) and Not GetDataInfo(NewName,InfoN) then
    begin
    L:=InfoO.DataSize;
    if L>0 then
      begin
      GetMem(P,L);
      try
        L:=GetData(OldName,P,L,D);
        If SysPutData(NewName,P,L,D) then
          DeleteValue(OldName);
      finally
        FreeMem(P);
      end;
      end;
    end;
end;

procedure TRegistry.SetCurrentKey(Value: HKEY);
begin
  fCurrentKey := Value;
end;

procedure TRegistry.SetRootKey(Value: HKEY);
begin
  fRootKey := Value;
end;

{ ---------------------------------------------------------------------
    Generic, implementation-independent code.
  ---------------------------------------------------------------------}


Constructor TRegistry.Create;

begin
  inherited Create;
  FAccess     := KEY_ALL_ACCESS;
  FRootKey    := HKEY_CURRENT_USER;
  FLazyWrite  := True;
  FCurrentKey := 0;
  SysRegCreate;
end;

Constructor TRegistry.Create(aaccess:longword);

begin
  Create;
  FAccess     := aaccess;
end;

Destructor TRegistry.Destroy;
begin
  CloseKey;
  SysRegFree;
  inherited Destroy;
end;

function TRegistry.CreateKey(const Key: String): Boolean;

begin
  Result:=SysCreateKey(Key);
  If Not Result Then
    Raise ERegistryException.CreateFmt(SRegCreateFailed, [Key]);
end;

function TRegistry.GetBaseKey(Relative: Boolean): HKey;
begin
  If Relative and (CurrentKey<>0) Then
    Result := CurrentKey
  else
    Result := RootKey;
end;

function TRegistry.GetData(const Name: String; Buffer: Pointer;
          BufSize: Integer; var RegData: TRegDataType): Integer;
begin
  Result:=SysGetData(Name,Buffer,BufSize,RegData);
  If (Result=-1) then
    Raise ERegistryException.CreateFmt(SRegGetDataFailed, [Name]);
end;

procedure TRegistry.PutData(const Name: string; Buffer: Pointer;
  BufSize: Integer; RegData: TRegDataType);

begin
  If Not SysPutData(Name,Buffer,BufSize,RegData) then
    Raise ERegistryException.CreateFmt(SRegSetDataFailed, [Name]);
end;


function TRegistry.GetDataSize(const ValueName: String): Integer;

Var
  Info: TRegDataInfo;

begin
  If GetDataInfo(ValueName,Info) Then
    Result := Info.DataSize
  else
    Result := -1;
end;

function TRegistry.GetDataType(const ValueName: string): TRegDataType;

Var
  Info: TRegDataInfo;

begin
  GetDataInfo(ValueName, Info);
  Result:=Info.RegData;
end;

Function TRegistry.HasSubKeys: Boolean;

Var
  Info : TRegKeyInfo;

begin
  Result:=GetKeyInfo(Info);
  If Result then
    Result:=(Info.NumSubKeys>0);
end;

function TRegistry.ReadBinaryData(const Name: string; var Buffer; BufSize: Integer): Integer;

Var
  RegDataType: TRegDataType;

begin
  Result := GetData(Name, @Buffer, BufSize, RegDataType);
  If (RegDataType<>rdBinary) Then
    Raise ERegistryException.CreateFmt(SInvalidRegType, [Name]);
end;

function TRegistry.ReadInteger(const Name: string): Integer;

Var
  RegDataType: TRegDataType;

begin
  GetData(Name, @Result, SizeOf(Integer), RegDataType);
  If RegDataType<>rdInteger Then
    Raise ERegistryException.CreateFmt(SInvalidRegType, [Name]);
end;

function TRegistry.ReadBool(const Name: string): Boolean;

begin
  Result:=ReadInteger(Name)<>0;
end;

function TRegistry.ReadCurrency(const Name: string): Currency;

Var
  RegDataType: TRegDataType;

begin
  ReadBinaryData(Name, Result, SizeOf(Currency));
end;

function TRegistry.ReadDate(const Name: string): TDateTime;

begin
  ReadBinaryData(Name, Result, SizeOf(TDateTime));
  Result:=Trunc(Result);
end;

function TRegistry.ReadDateTime(const Name: string): TDateTime;
Var
  RegDataType: TRegDataType;

begin
  ReadBinaryData(Name, Result, SizeOf(TDateTime));
end;

function TRegistry.ReadFloat(const Name: string): Double;

begin
  ReadBinaryData(Name,Result,SizeOf(Double));
end;

function TRegistry.ReadString(const Name: string): string;

Var
  Info : TRegDataInfo;

begin
  GetDataInfo(Name,Info);
  if info.datasize>0 then
    begin
     If Not (Info.RegData in [rdString,rdExpandString]) then
       Raise ERegistryException.CreateFmt(SInvalidRegType, [Name]);
     SetLength(Result,Info.DataSize);
     If StringSizeIncludesNull then
       SetLength(Result, Info.DataSize-1)
     else
       SetLength(Result, Info.DataSize);
     GetData(Name,PChar(Result),Info.DataSize,Info.RegData);
   end
  else
    result:='';
end;

function TRegistry.ReadTime(const Name: string): TDateTime;

begin
  ReadBinaryData(Name, Result, SizeOf(TDateTime));
  Result:=Frac(Result);
end;

procedure TRegistry.WriteBinaryData(const Name: string; var Buffer; BufSize: Integer);
begin
  PutData(Name, @Buffer, BufSize, rdBinary);
end;

procedure TRegistry.WriteBool(const Name: string; Value: Boolean);

begin
  WriteInteger(Name,Ord(Value));
end;

procedure TRegistry.WriteCurrency(const Name: string; Value: Currency);
begin
  WriteBinaryData(Name, Value, SizeOf(Currency));
end;

procedure TRegistry.WriteDate(const Name: string; Value: TDateTime);
begin
  WriteBinarydata(Name, Value, SizeOf(TDateTime));
end;

procedure TRegistry.WriteTime(const Name: string; Value: TDateTime);
begin
  WriteBinaryData(Name, Value, SizeOf(TDateTime));
end;

procedure TRegistry.WriteDateTime(const Name: string; Value: TDateTime);
begin
  WriteBinaryData(Name, Value, SizeOf(TDateTime));
end;

procedure TRegistry.WriteExpandString(const Name, Value: string);

begin
  PutData(Name, PChar(Value), Length(Value),rdExpandString);
end;

procedure TRegistry.WriteFloat(const Name: string; Value: Double);
begin
  WriteBinaryData(Name, Value, SizeOf(Double));
end;

procedure TRegistry.WriteInteger(const Name: string; Value: Integer);
begin
  PutData(Name, @Value, SizeOf(Integer), rdInteger);
end;

procedure TRegistry.WriteString(const Name, Value: string);

begin
  PutData(Name, PChar(Value), Length(Value), rdString);
end;

procedure TRegistry.MoveKey(const OldName, NewName: string; Delete: Boolean);
begin

end;

{ ---------------------------------------------------------------------
    Include TRegIniFile implementation
  ---------------------------------------------------------------------}

  {******************************************************************************
                                  TRegIniFile
   ******************************************************************************}

  constructor TRegIniFile.Create(const FN: String);
  begin
    inherited Create;
    fFileName := FN;
    if fFileName<>'' then
     fPath := fFileName + '\'
    else
     fPath := '';
  end;

  constructor TRegIniFile.Create(const FN: String;aaccess:longword);
  begin
    inherited Create(aaccess);
    fFileName := FN;
    if fFileName<>'' then
     fPath := fFileName + '\'
    else
     fPath := '';
  end;

  procedure TRegIniFile.DeleteKey(const Section, Ident: String);
  begin
  	if not OpenKey(fPath+Section,true) then Exit;
  	try
  	 DeleteValue(Ident);
  	finally
  	 CloseKey;
  	end;
  end;

  procedure TRegIniFile.EraseSection(const Section: string);
  begin
   inherited DeleteKey(fPath+Section);
  end;

  procedure TRegIniFile.ReadSection(const Section: string; Strings: TStrings);
  begin
  	if not OpenKey(fPath+Section,false) then Exit;
  	try
  	 GetValueNames(Strings);
  	finally
  	 CloseKey;
  	end;
  end;

  procedure TRegIniFile.ReadSections(Strings: TStrings);
  begin
  	if not OpenKey(fFileName,false) then Exit;
  	try
  	 GetKeyNames(Strings);
  	finally
  	 CloseKey;
  	end;
  end;

  procedure TRegIniFile.ReadSectionValues(const Section: string; Strings: TStrings);
  var
   ValList : TStringList;
   V : String;
   i : Integer;
  begin
  	if not OpenKey(fPath+Section,false) then Exit;
  	ValList := TStringList.Create;
  	try
  	 GetValueNames(ValList);
  	 for i:=0 to ValList.Count-1 do
  	 begin
  	   V := inherited ReadString(ValList.Strings[i]);
  	   Strings.Add(ValList.Strings[i] + '=' + V);
  	 end;
  	finally
  	 ValList.Free;
  	 CloseKey;
  	end;
  end;

  procedure TRegIniFile.WriteBool(const Section, Ident: string; Value: Boolean);
  begin
  	if not OpenKey(fPath+Section,true) then Exit;
  	try
      if not fPreferStringValues then
    	  inherited WriteBool(Ident,Value)
      else begin
        if ValueExists(Ident) and (GetDataType(Ident)=rdInteger) then
      	  inherited WriteBool(Ident,Value)
        else
          inherited WriteString(Ident,BoolToStr(Value));
      end;
    finally
  	  CloseKey;
  	end;
  end;

  procedure TRegIniFile.WriteInteger(const Section, Ident: string; Value: LongInt);
  begin
    if not OpenKey(fPath+Section,true) then Exit;
    try
      if not fPreferStringValues then
        inherited WriteInteger(Ident,Value)
      else begin
        if ValueExists(Ident) and (GetDataType(Ident)=rdInteger) then
      	  inherited WriteInteger(Ident,Value)
        else
          inherited WriteString(Ident,IntToStr(Value));
      end;
    finally
      CloseKey;
    end;
  end;

  procedure TRegIniFile.WriteString(const Section, Ident, Value: String);
  begin
    if not OpenKey(fPath+Section,true) then Exit;
    try
     inherited WriteString(Ident,Value);
    finally
     CloseKey;
    end;
  end;

  function TRegIniFile.ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
  begin
  	Result := Default;
  	if not OpenKey(fPath+Section,false) then Exit;
  	try
      if ValueExists(Ident) then
        if (not fPreferStringValues) or (GetDataType(Ident)=rdInteger) then
    	    Result := inherited ReadBool(Ident)
        else
          Result := StrToBool(inherited ReadString(Ident));
  	finally
  	  CloseKey;
  	end;
  end;

  function TRegIniFile.ReadInteger(const Section, Ident: string; Default: LongInt): LongInt;
  begin
    Result := Default;
    if not OpenKey(fPath+Section,false) then Exit;
    try
      if ValueExists(Ident) then
        if (not fPreferStringValues) or (GetDataType(Ident)=rdInteger) then
          Result := inherited ReadInteger(Ident)
        else
          Result := StrToInt(inherited ReadString(Ident));
    finally
      CloseKey;
    end;
  end;

  function TRegIniFile.ReadString(const Section, Ident, Default: String): String;
  begin
    Result := Default;
    if not OpenKey(fPath+Section,false) then Exit;
    try
      if ValueExists(Ident) then
        Result := inherited ReadString(Ident);
    finally
      CloseKey;
    end;
  end;

{ TRegistryIniFile }

// interface from
// http://www.koders.com/delphi/fid65C1FFAEF89B0CDC4B93FF94C1819686CA6141FC.aspx
constructor TRegistryIniFile.Create(const AFileName: string;
  AAccess: LongWord);
begin
  inherited create(AFilename);
  FRegInifile:=TreginiFile.Create(AFileName,AAccess);
end;

constructor TRegistryIniFile.Create(const AFileName: string);
begin
  Create(AFileName,KEY_ALL_ACCESS);
end;

procedure TRegistryIniFile.DeleteKey(const Section, Name: String);
begin
  FRegIniFile.Deletekey(section,name);
end;

procedure TRegistryIniFile.EraseSection(const Section: string);
begin
  FRegIniFile.EraseSection(section);
end;

function TRegistryIniFile.ReadBinaryStream(const Section, Name: string;
  Value: TStream): Integer;
begin
  result:=-1; // unimplemented
 // 
end;

function TRegistryIniFile.ReadDate(const Section, Name: string;
  Default: TDateTime): TDateTime;
var sectkey,curkey : HKey;
begin 
  with FRegInifile do
    begin
      sectkey:=getkey(Section);
      if sectkey<>0 then
        begin           
          try // allocation ok
            curkey:=FRegIniFile.CurrentKey;               
            SetCurrentKey(sectKey);
            try             // save current key
              if ValueExists(Name) THen 
                result:=FRegIniFile.ReadDate(Name)
              else
                result:=default;
            finally
              SetCurrentKey(CurKey);
              end;
          finally
            closekey(sectkey);
            end;
        end
       else
         result:=default; 
    end;          
end;

function TRegistryIniFile.ReadDateTime(const Section, Name: string;
  Default: TDateTime): TDateTime;
var sectkey,curkey : HKey;  
begin
  with FRegInifile do
    begin
      sectkey:=getkey(Section);
      if sectkey<>0 then
        begin           
          try // allocation ok
            curkey:=FRegIniFile.CurrentKey;               
            SetCurrentKey(sectKey);
            try             // save current key
              if ValueExists(Name) THen 
                result:=FRegIniFile.ReadDateTime(Name)
              else
                result:=default;
            finally
              SetCurrentKey(CurKey);
              end;
          finally
            closekey(sectkey);
            end;
        end
       else
         result:=default; 
    end;          
end;

function TRegistryIniFile.ReadFloat(const Section, Name: string;
  Default: Double): Double;
var sectkey,curkey : HKey;  
begin
  with FRegInifile do
    begin
      sectkey:=getkey(Section);
      if sectkey<>0 then
        begin           
          try // allocation ok
            curkey:=FRegIniFile.CurrentKey;               
            SetCurrentKey(sectKey);
            try             // save current key
              if ValueExists(Name) THen 
                result:=FRegIniFile.ReadFloat(Name)
              else
                result:=default;
            finally
              SetCurrentKey(CurKey);
              end;
          finally
            closekey(sectkey);
            end;
        end
       else
         result:=default; 
    end;          
end;

function TRegistryIniFile.ReadInteger(const Section, Name: string;
  Default: Integer): Longint;
var sectkey,curkey : HKey;  
begin
  with FRegInifile do
    begin
      sectkey:=getkey(Section);
      if sectkey<>0 then
        begin           
          try // allocation ok
            curkey:=FRegIniFile.CurrentKey;               
            SetCurrentKey(sectKey);
            try             // save current key
              if ValueExists(Name) THen 
                result:=FRegIniFile.ReadInteger(section,Name,default)
              else
                result:=default;
            finally
              SetCurrentKey(CurKey);
              end;
          finally
            closekey(sectkey);
            end;
        end
       else
         result:=default; 
    end;          
end;

procedure TRegistryIniFile.ReadSection(const Section: string;
  Strings: TStrings);
begin
  FRegIniFile.ReadSection(Section,strings);
end;

procedure TRegistryIniFile.ReadSections(Strings: TStrings);
begin
  FRegIniFile.ReadSections(strings);
end;

procedure TRegistryIniFile.ReadSectionValues(const Section: string;
  Strings: TStrings);
begin
  FRegIniFile.ReadSectionValues(Section,strings);
end;

function TRegistryIniFile.ReadString(const Section, Name,
  Default: string): string;
var sectkey,curkey : HKey;  
begin
  with FRegInifile do
    begin
      sectkey:=getkey(Section);
      if sectkey<>0 then
        begin           
          try // allocation ok
            curkey:=FRegIniFile.CurrentKey;               
            SetCurrentKey(sectKey);
            try             // save current key
              if ValueExists(Name) THen 
                result:=FRegIniFile.ReadString(section,Name,default)
              else
                result:=default;
            finally
              SetCurrentKey(CurKey);
              end;
          finally
            closekey(sectkey);
            end;
        end
       else
         result:=default; 
    end;          
end;

function TRegistryIniFile.ReadTime(const Section, Name: string;
  Default: TDateTime): TDateTime;
var sectkey,curkey : HKey;  
begin
  with FRegInifile do
    begin
      sectkey:=getkey(Section);
      if sectkey<>0 then
        begin           
          try // allocation ok
            curkey:=FRegIniFile.CurrentKey;               
            SetCurrentKey(sectKey);
            try             // save current key
              if ValueExists(Name) THen 
                result:=FRegIniFile.ReadTime(Name)
              else
                result:=default;
            finally
              SetCurrentKey(CurKey);
              end;
          finally
            closekey(sectkey);
            end;
        end
       else
         result:=default; 
    end;          
end;

procedure TRegistryIniFile.UpdateFile;
begin
//  FRegIniFile.UpdateFile; ??
end;

procedure TRegistryIniFile.WriteBinaryStream(const Section, Name: string;
  Value: TStream);
begin
 // ??
end;

procedure TRegistryIniFile.WriteDate(const Section, Name: string;
  Value: TDateTime);
var sectkey,curkey : HKey;  
begin
  with FRegInifile do
    begin
      sectkey:=getkey(Section);
      if sectkey<>0 then
        begin           
          try // allocation ok
            curkey:=FRegIniFile.CurrentKey;               
            SetCurrentKey(sectKey);
            try             // save current key
              FRegIniFile.WriteDate(name,value)
            finally
              SetCurrentKey(CurKey);
              end;
          finally
            closekey(sectkey);
            end;
        end
    end;          
end;

procedure TRegistryIniFile.WriteDateTime(const Section, Name: string;
  Value: TDateTime);
var sectkey,curkey : HKey;  
begin
  with FRegInifile do
    begin
      sectkey:=getkey(Section);
      if sectkey<>0 then
        begin           
          try // allocation ok
            curkey:=FRegIniFile.CurrentKey;               
            SetCurrentKey(sectKey);
            try             // save current key
              FRegIniFile.WriteDateTime(Name,value)
            finally
              SetCurrentKey(CurKey);
              end;
          finally
            closekey(sectkey);
            end;
        end
    end;
end;

procedure TRegistryIniFile.WriteFloat(const Section, Name: string;
  Value: Double);
var sectkey,curkey : HKey;  
begin
  with FRegInifile do
    begin
      sectkey:=getkey(Section);
      if sectkey<>0 then
        begin           
          try // allocation ok
            curkey:=FRegIniFile.CurrentKey;               
            SetCurrentKey(sectKey);
            try             // save current key
              FRegIniFile.WriteFloat(Name,value)
            finally
              SetCurrentKey(CurKey);
              end;
          finally
            closekey(sectkey);
            end;
        end
    end;          
end;

procedure TRegistryIniFile.WriteInteger(const Section, Name: string;
  Value: Integer);
var sectkey,curkey : HKey;  
begin
  with FRegInifile do
    begin
      sectkey:=getkey(Section);
      if sectkey<>0 then
        begin           
          try // allocation ok
            curkey:=FRegIniFile.CurrentKey;               
            SetCurrentKey(sectKey);
            try             // save current key
              FRegIniFile.WriteInteger(section,Name,value)
            finally
              SetCurrentKey(CurKey);
              end;
          finally
            closekey(sectkey);
            end;
        end
    end;          

end;

procedure TRegistryIniFile.WriteString(const Section, Name, Value: String);
var sectkey,curkey : HKey;  
begin
  with FRegInifile do
    begin
      sectkey:=getkey(Section);
      if sectkey<>0 then
        begin           
          try // allocation ok
            curkey:=FRegIniFile.CurrentKey;               
            SetCurrentKey(sectKey);
            try             // save current key
              FRegIniFile.WriteString(section,Name,value)
            finally
              SetCurrentKey(CurKey);
              end;
          finally
            closekey(sectkey);
            end;
        end
    end;
end;

procedure TRegistryIniFile.WriteTime(const Section, Name: string;
  Value: TDateTime);
var sectkey,curkey : HKey;
begin
  with FRegInifile do
    begin
      sectkey:=getkey(Section);
      if sectkey<>0 then
        begin
          try // allocation ok
            curkey:=FRegIniFile.CurrentKey;
            SetCurrentKey(sectKey);
            try             // save current key
              FRegIniFile.WriteTime(Name,value)
            finally
              SetCurrentKey(CurKey);
              end;
          finally
            closekey(sectkey);
            end;
        end
    end;
end;

end.
