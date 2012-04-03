unit UPDClientMySQL;

{$mode delphi}

interface

uses
  Classes, SysUtils, USqlDatabase, UPDClient, SpecializedDictionary;

type

  { TPDClientMySQL }

  TPDClientMySQL = class(TPDClient)
  protected
    procedure InitSystemTypes; override;
    function GetConnected: Boolean; override;
    procedure Init; override;
  public
    Database: TSqlDatabase;
    procedure ObjectLoad(AObject: TObjectProxy); override;
    procedure ObjectSave(AObject: TObjectProxy); override;
    procedure ObjectDelete(AObject: TObjectProxy); override;
    procedure ListLoad(AList: TListProxy); override;
    procedure ListSave(AList: TListProxy); override;
    procedure TypeDefine(AType: TPDType); override;
    procedure TypeUndefine(AType: TPDType); override;
    function TypeIsDefined(AType: TPDType): Boolean; override;
    procedure Install;
    procedure Uninstall;
    constructor Create; override;
    destructor Destroy; override;
    procedure Connect; override;
    procedure Disconnect; override;
  end;

implementation


resourcestring
  SMissingBaseType = 'Missing base typ for %s';
  SUndefinedType = 'Undefinned type %s';
  SCantLoadObjectWithoutId = 'Can''t load object without id';


{ TPDClientMySQL }

procedure TPDClientMySQL.InitSystemTypes;
begin
  inherited InitSystemTypes;
  Types.AddType('Integer', 'int(11)');
  Types.AddType('String', 'varchar(255)');
  Types.AddType('RelationOne', 'int(11)');
  Types.AddType('Double', 'double');
  Types.AddType('DateTime', 'datetime');
  Types.AddType('Date', 'date');
  Types.AddType('Time', 'time');
  Types.AddType('Text', 'text');
  Types.AddType('Boolean', 'bool');
end;

function TPDClientMySQL.GetConnected: Boolean;
begin
  Result := Database.Connected;
end;

procedure TPDClientMySQL.ObjectLoad(AObject: TObjectProxy);
var
  DbRows: TDbRows;
  NewObject: TObjectProxy;
  Table: string;
begin
  if AObject.Id = 0 then raise Exception.Create(SCantLoadObjectWithoutId);
  try
    DbRows := TDbRows.Create;
    Table := '`' + AObject.ObjectName + '`';
    if AObject.SchemaName <> '' then Table := '`' + AObject.SchemaName + '`.' + Table;
    Database.Query(DbRows, 'SELECT * FROM ' + Table +
      ' WHERE `Id`=' + IntToStr(AObject.Id));
    AObject.Properties.Assign(TDictionaryStringString(DbRows[0]));
  finally
    DbRows.Free;
  end;
end;

procedure TPDClientMySQL.ObjectSave(AObject: TObjectProxy);
var
  DbRows: TDbRows;
  NewObject: TObjectProxy;
  Table: string;
begin
  try
    DbRows := TDbRows.Create;
    Database.Replace(AObject.ObjectName, AObject.Properties, AObject.SchemaName);
    if AObject.Id = 0 then AObject.Id := Database.LastInsertId;
  finally
    DbRows.Free;
  end;
end;

procedure TPDClientMySQL.ObjectDelete(AObject: TObjectProxy);
begin
  Database.Delete(AObject.ObjectName, 'Id=' + IntToStr(AObject.Id),
    AObject.SchemaName);
end;

procedure TPDClientMySQL.ListLoad(AList: TListProxy);
var
  DbRows: TDbRows;
  Filter: string;
  DbCondition: string;
  I: Integer;
  NewObject: TObjectProxy;
  Table: string;
begin
  try
    DbRows := TDbRows.Create;
    if AList.ColummsFilterUse then begin
      Filter := '';
      for I := 0 to AList.ColumnsFilter.Count - 1 do
        Filter := Filter + '`' + AList.ColumnsFilter[I] + '`, ';
      Delete(Filter, Length(Filter) - 1, 2);
    end else Filter := '*';
    if AList.Condition <> '' then DbCondition := ' WHERE ' + AList.Condition
      else DbCondition := '';
    Table := '`' + AList.ObjectName + '`';
    if AList.SchemaName <> '' then Table := '`' + AList.SchemaName + '`.' + Table;
    Database.Query(DbRows, 'SELECT ' + Filter + ' FROM ' + Table + DbCondition);
    AList.Objects.Clear;
    for I := 0 to DbRows.Count - 1 do begin
      NewObject := TObjectProxy.Create;
      NewObject.Client := AList.Client;
      NewObject.ObjectName := AList.ObjectName;
      NewObject.SchemaName := AList.SchemaName;
      NewObject.Properties.Assign(TDictionaryStringString(DbRows[I]));
      AList.Objects.Add(NewObject);
    end;
  finally
    DbRows.Free;
  end;
end;

procedure TPDClientMySQL.ListSave(AList: TListProxy);
begin

end;

procedure TPDClientMySQL.TypeDefine(AType: TPDType);
var
  DbRows: TDbRows;
  I: Integer;
  Query: string;
  RefType: TPDType;
begin
  try
    DbRows := TDbRows.Create;
    Query := 'CREATE TABLE IF NOT EXISTS `' + AType.Name + '` ( ' +
      '`Id` int(11) NOT NULL AUTO_INCREMENT,';
    for I := 0 to AType.Properties.Count - 1 do
    with AType.Properties do begin
      RefType := Types.SearchByName(Items[I].Value);
      if not Assigned(RefType) then
        raise Exception.Create(Format(SUndefinedType, [Items[I].Value]));
      if RefType.DbType = '' then
        raise Exception.Create(Format(SMissingBaseType, [RefType.Name]));

      Query := Query + '`' + Items[I].Key + '` ' + RefType.DbType + ' NULL,';
    end;
    Query := Query + 'PRIMARY KEY (`Id`)' +
      ') ENGINE=InnoDB  DEFAULT CHARSET=utf8';
    Database.Query(DbRows, Query);
  finally
    DbRows.Free;
  end;
end;

procedure TPDClientMySQL.TypeUndefine(AType: TPDType);
var
  DbRows: TDbRows;
  I: Integer;
  Query: string;
  RefType: TPDType;
begin
  try
    DbRows := TDbRows.Create;
    Query := 'DROP TABLE IF EXISTS `' + AType.Name + '`';
    Database.Query(DbRows, Query);
  finally
    DbRows.Free;
  end;
end;

function TPDClientMySQL.TypeIsDefined(AType: TPDType): Boolean;
var
  NewProxy: TListProxy;
begin
  try
    NewProxy := TListProxy.Create;
    NewProxy.Client := Self;
    NewProxy.SchemaName := 'information_schema';
    NewProxy.ObjectName := 'TABLES';
    NewProxy.Condition := '(TABLE_SCHEMA = "' + Schema +
      '") AND (TABLE_NAME = "' + AType.Name + '")';
    NewProxy.Load;
    Result := NewProxy.Objects.Count > 0;
  finally
    NewProxy.Free;
  end;
end;

procedure TPDClientMySQL.Install;
begin
(*  if Tables.IndexOf(InformationTable) = -1 then begin
    Database.Query(DbRows, 'CREATE TABLE IF NOT EXISTS `' + InformationTable + '` ( ' +
'`Version` varchar(255) NOT NULL,' +
'`LastUpdateTime` datetime NOT NULL' +
') ENGINE=InnoDB DEFAULT CHARSET=utf8;');
    Database.Query(DbRows, 'INSERT INTO `' + InformationTable + '` (`Version`, `LastUpdateTime`) VALUES ' +
'("0.1", "0000-00-00 00:00:00");');
  end;
  Database.Select(DbRows, InformationTable);
  StructureVersion := DbRows[0].Values['Version'];*)
end;

procedure TPDClientMySQL.Uninstall;
begin

end;

procedure TPDClientMySQL.Init;
begin
  inherited;
end;

constructor TPDClientMySQL.Create;
begin
  inherited;
  Database := TSqlDatabase.Create(nil);
end;

destructor TPDClientMySQL.Destroy;
begin
  FreeAndNil(Database);
  inherited Destroy;
end;

procedure TPDClientMySQL.Connect;
begin
  Database.Port := Port;
  Database.UserName := User;
  Database.Password := Password;
  Database.HostName := Host;
  Database.Database := Schema;
  Database.Connect;
  Init;
end;

procedure TPDClientMySQL.Disconnect;
begin
  Database.Disconnect;
end;

end.

