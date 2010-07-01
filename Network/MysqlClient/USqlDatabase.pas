unit USqlDatabase;

{$mode Delphi}{$H+}
// Upraveno: 16.12.2009

interface

uses
  SysUtils, Classes, Dialogs, mysql50, TypInfo;

type
  EQueryError = Exception;

  TClientCapabilities = (_CLIENT_LONG_PASSWORD, _CLIENT_FOUND_ROWS,
    _CLIENT_LONG_FLAG, _CLIENT_CONNECT_WITH_DB, _CLIENT_NO_SCHEMA,
    _CLIENT_COMPRESS, _CLIENT_ODBC, _CLIENT_LOCAL_FILES, _CLIENT_IGNORE_SPACE,
    _CLIENT_INTERACTIVE, _CLIENT_SSL, _CLIENT_IGNORE_SIGPIPE, _CLIENT_TRANSACTIONS);

  TSetClientCapabilities = set of TClientCapabilities;

  TAssociativeArray = class(TStringList)
  private
    function GetValues(Index: string): string;
    function GetValuesAtIndex(Index: Integer): string;
    procedure SetValues(Index: string; const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    function GetAllValues: string;
    procedure AddKeyValue(Key, Value: string);
    property ValuesAtIndex[Index: Integer]: string read GetValuesAtIndex;
    property Values[Index: string]: string read GetValues write SetValues; default;
  end;

  TDbRows = class(TList)
  private
    function GetData(Index: Integer): TAssociativeArray;
    procedure SetData(Index: Integer; const Value: TAssociativeArray);
  public
    property Data[Index: Integer]: TAssociativeArray read GetData write SetData; default;
    destructor Destroy; override;
  end;

  TSqlDatabase = class
    procedure mySQLClient1ConnectError(Sender: TObject; Msg: String);
  private
    FSession: PMYSQL;
    FConnected: Boolean;
    FDatabase: string;
    function GetConnected: Boolean;
    function GetLastErrorMessage: string;
    function GetLastErrorNumber: Integer;
    function CheckError: Boolean;
    function GetCharset: string;
    procedure SetDatabase(const Value: string);
    { Private declarations }
  public
    Hostname: string;
    UserName: string;
    Password: string;
    Encoding: string;
    Table: string;
    RepeatLastAction: Boolean;
    LastQuery: string;
    procedure CreateDatabase;
    procedure CreateTable(Name: string);
    procedure CreateColumn(Table, ColumnName: string; ColumnType: TTypeKind);
    function Query(Data: string): TDbRows;
    function Select(ATable: string; Filter: string = '*'; Condition: string = '1'): TDbRows;
    procedure Delete(ATable: string; Condition: string = '1');
    procedure Insert(ATable: string; Data: TAssociativeArray);
    procedure Update(ATable: string; Data: TAssociativeArray; Condition: string = '1');
    procedure Replace(ATable: string; Data: TAssociativeArray);
    procedure Connect;
    procedure Disconnect;
    function LastInsertId: Integer;
    property LastErrorMessage: string read GetLastErrorMessage;
    property LastErrorNumber: Integer read GetLastErrorNumber;
    property Connected: Boolean read GetConnected;
    constructor Create;
    destructor Destroy; override;
    property Charset: string read GetCharset;
    property Database: string read FDatabase write SetDatabase;
  end;

  function MySQLFloatToStr(F: Real): string;
  function MySQLStrToFloat(S: string): Real;

implementation

uses DateUtils, Math;

const
  CLIENT_LONG_PASSWORD = 1;      // new more secure passwords
  CLIENT_FOUND_ROWS = 2;         // Found instead of affected rows
  CLIENT_LONG_FLAG = 4;          // Get all column flags
  CLIENT_CONNECT_WITH_DB = 8;    // One can specify db on connect
  CLIENT_NO_SCHEMA = 16;         // Don't allow database.table.column
  CLIENT_COMPRESS = 32;          // Can use compression protcol
  CLIENT_ODBC = 64;              // Odbc client
  CLIENT_LOCAL_FILES = 128;      // Can use LOAD DATA LOCAL
  CLIENT_IGNORE_SPACE = 256;     // Ignore spaces before '('
  CLIENT_INTERACTIVE = 1024;     // This is an interactive client
  CLIENT_SSL = 2048;             // Switch to SSL after handshake
  CLIENT_IGNORE_SIGPIPE = 4096;  // IGNORE sigpipes
  CLIENT_TRANSACTIONS = 8192;    // Client knows about transactions

{ TDataModule2 }

function MySQLFloatToStr(F: Real): string;
var
  S: string;
begin
  S := FloatToStr(F);
  if Pos(',', S) > 0 then S[Pos(',',S)] := '.';
  Result := S;
end;

function MySQLStrToFloat(S: string): Real;
begin
  if Pos('.', S) > 0 then S[Pos('.',S)] := ',';
  Result := StrToFloat(S);
end;

procedure TSqlDatabase.Connect;
var
  NewSession: PMYSQL;
  Rows: TDbRows;
begin
  RepeatLastAction := False;
//  mySQLClient1.Connect;
  FSession := mysql_init(FSession);
//  FSession.charset := 'latin2';
  NewSession := mysql_real_connect(FSession, PChar(HostName), PChar(UserName),
    PChar(Password), PChar(Database), 3306, nil, CLIENT_LONG_PASSWORD + CLIENT_CONNECT_WITH_DB);
  if Assigned(NewSession) then begin
    FConnected := True;
    FSession := NewSession;
  end else FConnected := False;
  CheckError;
  Rows := Query('SET NAMES ' + Encoding);
  Rows.Free;
end;

procedure TSqlDatabase.Insert(ATable: string; Data: TAssociativeArray);
var
  DbNames: string;
  DbValues: string;
  I: Integer;
  Value: string;
  DbResult: TDbRows;
begin
  Table := ATable;
  DbNames := '';
  DbValues := '';
  for I := 0 to Data.Count - 1 do begin
    Value := Data.ValuesAtIndex[I];
    StringReplace(Value, '"', '\"', [rfReplaceAll]);
    if Value = 'NOW()' then DbValues := DbValues + ',' + Value
    else DbValues := DbValues + ',"' + Value + '"';
    DbNames := DbNames + ',`' + Data.Names[I] + '`';
  end;
  System.Delete(DbNames, 1, 1);
  System.Delete(DbValues, 1, 1);
  DbResult := Query('INSERT INTO `' + Table + '` (' + DbNames + ') VALUES (' + DbValues + ')');
  DbResult.Free;
end;

function TSqlDatabase.Query(Data: string): TDbRows;
var
  I, II: Integer;
  DbResult: PMYSQL_RES;
  DbRow: MYSQL_ROW;
begin
  //DebugLog('SqlDatabase query: '+Data);
  RepeatLastAction := False;
  LastQuery := Data;
  //if not Connected then NastaveniPripojeni.ShowModal;
  Result := TDbRows.Create;
  //repeat
  mysql_query(FSession, PChar(Data));
  //until not
  CheckError;
  //if not CheckError then
  begin
    DbResult := mysql_store_result(FSession);
    if Assigned(DbResult) then begin
      Result.Count := mysql_num_rows(DbResult);
      for I := 0 to Result.Count - 1 do begin
        DbRow := mysql_fetch_row(DbResult);
        Result[I] := TAssociativeArray.Create;
        with Result[I] do begin
          for II := 0 to mysql_num_fields(DbResult) - 1 do begin
            Add(mysql_fetch_field_direct(DbResult, II)^.Name +
              NameValueSeparator + PChar((DbRow + II)^));
          end;
        end;
      end;
    end;
  end;
  mysql_free_result(DbResult);
  (*
  if Assigned(DatabaseIntegrity) then
  with DatabaseIntegrity do if not Checking then begin
    Check;
    DebugLog('Database integrity: Unreferenced='+IntToStr(Unreferenced)+' BadReferences='+IntToStr(BadReferences));
  end;
  *)
end;

procedure TSqlDatabase.Replace(ATable: string; Data: TAssociativeArray);
var
  DbNames: string;
  DbValues: string;
  Value: string;
  I: Integer;
  DbResult: TDbRows;
begin
  Table := ATable;
  DbNames := '';
  DbValues := '';
  for I := 0 to Data.Count - 1 do begin
    Value := Data.ValuesAtIndex[I];
    StringReplace(Value, '"', '\"', [rfReplaceAll]);
    if Value = 'NOW()' then DbValues := DbValues + ',' + Value
    else DbValues := DbValues + ',"' + Value + '"';
    DbNames := DbNames + ',`' + Data.Names[I] + '`';
  end;
  System.Delete(DbNames, 1, 1);
  System.Delete(DbValues, 1, 1);
  DbResult := Query('REPLACE INTO `' + Table + '` (' + DbNames + ') VALUES (' + DbValues + ')');
  DbResult.Free;
end;

function TSqlDatabase.Select(ATable: string; Filter: string = '*'; Condition: string = '1'): TDbRows;
begin
  Table := ATable;
  Result := Query('SELECT ' + Filter + ' FROM `' + Table + '` WHERE '+Condition);
end;

procedure TSqlDatabase.Update(ATable: string; Data: TAssociativeArray; Condition: string = '1');
var
  DbValues: string;
  Value: string;
  I: Integer;
  DbResult: TDbRows;
begin
  Table := ATable;
  DbValues := '';
  for I := 0 to Data.Count - 1 do begin
    Value := Data.ValuesAtIndex[I];
    StringReplace(Value, '"', '\"', [rfReplaceAll]);
    if Value = 'NOW()' then DbValues := DbValues + ',' + Value
    else DbValues := DbValues + ',' + Data.Names[I] + '=' + '"' + Value + '"';
  end;
  System.Delete(DbValues, 1, 1);
  DbResult := Query('UPDATE `' + Table + '` SET (' + DbValues + ') WHERE ' + Condition);
  DbResult.Free;
end;

procedure TSqlDatabase.mySQLClient1ConnectError(Sender: TObject; Msg: String);
begin
//  LastError := Msg + '('+IntToStr(mySQLClient1.LastErrorNumber)+')';
end;

{ TAssocArray }

procedure TAssociativeArray.AddKeyValue(Key, Value: string);
begin
  Add(Key + NameValueSeparator + Value);
end;

constructor TAssociativeArray.Create;
begin
  NameValueSeparator := '|';
end;

destructor TAssociativeArray.Destroy;
begin
  inherited;
end;

function TAssociativeArray.GetAllValues: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do begin
    Result := Result + Names[I] + '=' + ValuesAtIndex[I] + ',';
  end;
end;

function TAssociativeArray.GetValues(Index: string): string;
begin
  Result := inherited Values[Index];
end;

function TAssociativeArray.GetValuesAtIndex(Index: Integer): string;
begin
  Result := inherited Values[Names[Index]];
end;

procedure TSqlDatabase.Delete(ATable: string; Condition: string = '1');
begin
  Table := ATable;
  Query('DELETE FROM `' + Table + '` WHERE ' + Condition);
end;

function TSqlDatabase.GetConnected: Boolean;
begin
  Result := FConnected;
end;

procedure TSqlDatabase.Disconnect;
begin
  mysql_close(FSession);
  FConnected := False;
end;

constructor TSqlDatabase.Create;
begin
  inherited;
  FSession := nil;
  Encoding := 'utf8';
end;

procedure TAssociativeArray.SetValues(Index: string; const Value: string);
begin
  inherited Values[Index] := Value;
end;

{ TDbRows }

destructor TDbRows.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Data[I].Free;
  inherited;
end;

function TDbRows.GetData(Index: Integer): TAssociativeArray;
begin
  Result := Items[Index];
end;

procedure TDbRows.SetData(Index: Integer; const Value: TAssociativeArray);
begin
  Items[Index] := Value;
end;

function TSqlDatabase.LastInsertId: Integer;
begin
  Result := mysql_insert_id(FSession);
end;

function TSqlDatabase.GetLastErrorMessage: string;
begin
  Result := mysql_error(FSession);
end;

function TSqlDatabase.GetLastErrorNumber: Integer;
begin
  Result := mysql_errno(FSession);
end;

function TSqlDatabase.CheckError: Boolean;
begin
  Result := LastErrorNumber <> 0;
  if Result then
    raise EQueryError.Create('Database query error: "' + LastErrorMessage + '"');
end;

procedure TSqlDatabase.CreateDatabase;
var
  TempDatabase: string;
begin
  TempDatabase := Database;
  Database := 'mysql';
  Connect;
  Query('CREATE DATABASE ' + TempDatabase);
  Disconnect;
  Database := TempDatabase;
end;

procedure TSqlDatabase.CreateTable(Name: string);
var
  DbRows: TDbRows;
begin
  DbRows := Query('CREATE TABLE `' + Name + '`' +
  ' (`Id` INT NOT NULL AUTO_INCREMENT, PRIMARY KEY (`Id`));');
  DbRows.Destroy;
end;

procedure TSqlDatabase.CreateColumn(Table, ColumnName: string;
  ColumnType: TTypeKind);
const
  ColTypes: array[0..17] of string = ('', 'INT', 'CHAR', 'INT', 'DOUBLE',
  'VARCHAR(255)', 'SET', 'INT', '', '', 'TEXT', 'TEXT', '', '', '', '', '', '');
begin
  Query('ALTER TABLE `' + Table + '` ADD `' + ColumnName + '` ' +
    ColTypes[Integer(ColumnType)] + ' NOT NULL');
end;

destructor TSqlDatabase.Destroy;
begin
  if Connected then Disconnect;
end;

function TSqlDatabase.GetCharset: string;
begin
  Result := mysql_character_set_name(FSession);
end;

procedure TSqlDatabase.SetDatabase(const Value: string);
begin
  FDatabase := Value;
  if FConnected then mysql_select_db(FSession, PChar(FDatabase));
end;

end.


