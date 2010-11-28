unit USqlDatabase;

{$mode Delphi}{$H+}

// Upraveno: 28.10.2010

interface

uses
  SysUtils, Classes, Dialogs, mysql50, TypInfo, SpecializedObjectList,
  SpecializedDictionary, SpecializedList;

type
  EQueryError = class(Exception);

  TClientCapabilities = (_CLIENT_LONG_PASSWORD, _CLIENT_FOUND_ROWS,
    _CLIENT_LONG_FLAG, _CLIENT_CONNECT_WITH_DB, _CLIENT_NO_SCHEMA,
    _CLIENT_COMPRESS, _CLIENT_ODBC, _CLIENT_LOCAL_FILES, _CLIENT_IGNORE_SPACE,
    _CLIENT_INTERACTIVE, _CLIENT_SSL, _CLIENT_IGNORE_SIGPIPE, _CLIENT_TRANSACTIONS);

  TSetClientCapabilities = set of TClientCapabilities;

  TDbRows = class(TListObject)
  private
    function GetData(Index: Integer): TDictionaryStringString;
    procedure SetData(Index: Integer; const Value: TDictionaryStringString);
  public
    property Data[Index: Integer]: TDictionaryStringString read GetData write SetData; default;
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
    function GetCharset: string;
    procedure SetDatabase(const Value: string);
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
    procedure Query(DbRows: TDbRows; Data: string);
    procedure Select(DbRows: TDbRows; ATable: string; Filter: string = '*'; Condition: string = '1');
    procedure Delete(ATable: string; Condition: string = '1');
    procedure Insert(ATable: string; Data: TDictionaryStringString);
    procedure Update(ATable: string; Data: TDictionaryStringString; Condition: string = '1');
    procedure Replace(ATable: string; Data: TDictionaryStringString);
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
  function SQLToDateTime(Value: string): TDateTime;
  function DateTimeToSQL(Value: TDateTime): string;

implementation

uses
  DateUtils, Math;

resourcestring
  SDatabaseQueryError = 'Database query error: "%s"';

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

function MySQLFloatToStr(F: Real): string;
var
  S: string;
begin
  S := FloatToStr(F);
  if Pos(',', S) > 0 then S[Pos(',', S)] := '.';
  Result := S;
end;

function MySQLStrToFloat(S: string): Real;
begin
  if Pos('.', S) > 0 then S[Pos('.', S)] := ',';
  Result := StrToFloat(S);
end;

function StrToStr(Value: string): string;
begin
  Result := Value;
end;

function SQLToDateTime(Value: string): TDateTime;
var
  Parts: TListString;
  DateParts: TListString;
  TimeParts: TListString;
begin
  try
    Parts := TListString.Create;
    DateParts := TListString.Create;
    TimeParts := TListString.Create;

    Parts.Explode(Value, ' ', StrToStr);
    DateParts.Explode(Parts[0], '-', StrToStr);
    Result := EncodeDate(StrToInt(DateParts[0]), StrToInt(DateParts[1]),
      StrToInt(DateParts[2]));
    if Parts.Count > 1 then begin
      TimeParts.Explode(Parts[1], ':', StrToStr);
      Result := Result + EncodeTime(StrToInt(TimeParts[0]), StrToInt(TimeParts[1]),
        StrToInt(TimeParts[2]), 0);
    end;
  finally
    DateParts.Free;
    TimeParts.Free;
    Parts.Free;
  end;
end;

function DateTimeToSQL(Value: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh.nn.ss', Value);
end;

{ TSqlDatabase }

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

  if LastErrorNumber <> 0 then
    raise EQueryError.Create(Format(SDatabaseQueryError, [LastErrorMessage]));

  try
    Rows := TDbRows.Create;
    Query(Rows, 'SET NAMES ' + Encoding);
  finally
    Rows.Free;
  end;
end;

procedure TSqlDatabase.Insert(ATable: string; Data: TDictionaryStringString);
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
    Value := Data.Items[I].Value;
    StringReplace(Value, '"', '\"', [rfReplaceAll]);
    if Value = 'NOW()' then DbValues := DbValues + ',' + Value
    else DbValues := DbValues + ',"' + Value + '"';
    DbNames := DbNames + ',`' + Data.Keys[I] + '`';
  end;
  System.Delete(DbNames, 1, 1);
  System.Delete(DbValues, 1, 1);
  try
    DbResult := TDbRows.Create;
    Query(DbResult, 'INSERT INTO `' + Table + '` (' + DbNames + ') VALUES (' + DbValues + ')');
  finally
    DbResult.Free;
  end;
end;

procedure TSqlDatabase.Query(DbRows: TDbRows; Data: string);
var
  I, II: Integer;
  DbResult: PMYSQL_RES;
  DbRow: MYSQL_ROW;
begin
  DbRows.Clear;
  //DebugLog('SqlDatabase query: '+Data);
  RepeatLastAction := False;
  LastQuery := Data;
  mysql_query(FSession, PChar(Data));
  if LastErrorNumber <> 0 then begin
    raise EQueryError.Create(Format(SDatabaseQueryError, [LastErrorMessage]));
  end;

  DbResult := mysql_store_result(FSession);
  if Assigned(DbResult) then begin
    DbRows.Count := mysql_num_rows(DbResult);
    for I := 0 to DbRows.Count - 1 do begin
      DbRow := mysql_fetch_row(DbResult);
      DbRows[I] := TDictionaryStringString.Create;
      with DbRows[I] do begin
        for II := 0 to mysql_num_fields(DbResult) - 1 do begin
          Add(mysql_fetch_field_direct(DbResult, II)^.Name,
            PChar((DbRow + II)^));
          end;
        end;
      end;
  end;
  mysql_free_result(DbResult);
end;

procedure TSqlDatabase.Replace(ATable: string; Data: TDictionaryStringString);
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
    Value := Data.Items[I].Value;
    StringReplace(Value, '"', '\"', [rfReplaceAll]);
    if Value = 'NOW()' then DbValues := DbValues + ',' + Value
    else DbValues := DbValues + ',"' + Value + '"';
    DbNames := DbNames + ',`' + Data.Keys[I] + '`';
  end;
  System.Delete(DbNames, 1, 1);
  System.Delete(DbValues, 1, 1);
  try
    DbResult := TDbRows.Create;
    Query(DbResult, 'REPLACE INTO `' + Table + '` (' + DbNames + ') VALUES (' + DbValues + ')');
  finally
    DbResult.Free;
  end;
end;

procedure TSqlDatabase.Select(DbRows: TDbRows; ATable: string; Filter: string = '*'; Condition: string = '1');
begin
  Table := ATable;
  Query(DbRows, 'SELECT ' + Filter + ' FROM `' + Table + '` WHERE ' + Condition);
end;

procedure TSqlDatabase.Update(ATable: string; Data: TDictionaryStringString; Condition: string = '1');
var
  DbValues: string;
  Value: string;
  I: Integer;
  DbResult: TDbRows;
begin
  Table := ATable;
  DbValues := '';
  for I := 0 to Data.Count - 1 do begin
    Value := Data.Items[I].Value;
    StringReplace(Value, '"', '\"', [rfReplaceAll]);
    if Value = 'NOW()' then DbValues := DbValues + ',' + Value
    else DbValues := DbValues + ',' + Data.Keys[I] + '=' + '"' + Value + '"';
  end;
  System.Delete(DbValues, 1, 1);
  try
    DbResult := TDbRows.Create;
    Query(DbResult, 'UPDATE `' + Table + '` SET (' + DbValues + ') WHERE ' + Condition);
  finally
    DbResult.Free;
  end;
end;

procedure TSqlDatabase.mySQLClient1ConnectError(Sender: TObject; Msg: String);
begin
//  LastError := Msg + '('+IntToStr(mySQLClient1.LastErrorNumber)+')';
end;

procedure TSqlDatabase.Delete(ATable: string; Condition: string = '1');
var
  DbResult: TDbRows;
begin
  Table := ATable;
  try
    DbResult := TDbRows.Create;
    Query(DbResult, 'DELETE FROM `' + Table + '` WHERE ' + Condition);
  finally
    DbResult.Free;
  end;
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

procedure TSqlDatabase.CreateDatabase;
var
  TempDatabase: string;
  DbRows: TDbRows;
begin
  TempDatabase := Database;
  Database := 'mysql';
  Connect;
  try
    DbRows := TDbRows.Create;
    Query(DbRows, 'CREATE DATABASE ' + TempDatabase);
  finally
    DbRows.Free;
  end;
  Disconnect;
  Database := TempDatabase;
end;

procedure TSqlDatabase.CreateTable(Name: string);
var
  DbRows: TDbRows;
begin
  try
    DbRows := TDbRows.Create;
    Query(DbRows, 'CREATE TABLE `' + Name + '`' +
    ' (`Id` INT NOT NULL AUTO_INCREMENT, PRIMARY KEY (`Id`));');
  finally
    DbRows.Free;
  end;
end;

procedure TSqlDatabase.CreateColumn(Table, ColumnName: string;
  ColumnType: TTypeKind);
const
  ColTypes: array[0..17] of string = ('', 'INT', 'CHAR', 'INT', 'DOUBLE',
  'VARCHAR(255)', 'SET', 'INT', '', '', 'TEXT', 'TEXT', '', '', '', '', '', '');
var
  DbRows: TDbRows;
begin
  try
    DbRows := TDbRows.Create;
    Query(DbRows, 'ALTER TABLE `' + Table + '` ADD `' + ColumnName + '` ' +
      ColTypes[Integer(ColumnType)] + ' NOT NULL');
  finally
    DbRows.Free;
  end;
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

{ TDbRows }

destructor TDbRows.Destroy;
begin
  inherited;
end;

function TDbRows.GetData(Index: Integer): TDictionaryStringString;
begin
  Result := TDictionaryStringString(Items[Index]);
end;

procedure TDbRows.SetData(Index: Integer; const Value: TDictionaryStringString);
begin
  Items[Index] := Value;
end;

end.


