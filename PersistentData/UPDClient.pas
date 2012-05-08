unit UPDClient;

{$mode delphi}

interface

uses
  Classes, SysUtils, SpecializedList, SpecializedDictionary;

const
  SystemVersionObject = 'SystemVersion';

type
  EClientNotSet = class(Exception);

  TPDClient = class;

  TOrderDirection = (odNone, odAscending, odDescending);

  { TObjectProxy }

  TObjectProxy = class
    Id: Integer;
    Properties: TDictionaryStringString;
    Client: TPDClient;
    ObjectName: string;
    Path: string;
    procedure Load;
    procedure Save;
    procedure Delete;
    constructor Create;
    destructor Destroy; override;
  end;

  { TListProxy }

  TListProxy = class
    Client: TPDClient;
    OrderColumn: string;
    OrderDirection: TOrderDirection;
    OrderUse: Boolean;
    PageItemFirst: Integer;
    PageItemCount: Integer;
    PageUse: Boolean;
    ColumnsFilter: TListString;
    ColummsFilterUse: Boolean;
    Condition: string;
    ObjectName: string;
    Path: string;
    Objects: TListObject; // TListObject<TObjectProxy>
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    procedure Load; virtual;
    procedure Save; virtual;
  end;

  { TPDType }

  TPDType = class
    Client: TPDClient;
    Name: string;
    DbType: string;
    Properties: TDictionaryStringString;
    function IsDefined: Boolean;
    procedure Define;
    procedure Undefine;
    constructor Create;
    destructor Destroy; override;
  end;

  { TPDTypeList }

  TPDTypeList = class(TListObject)
    Client: TPDClient;
    function AddType(Name: string; DbType: string = ''): TPDType;
    function SearchByName(Name: string): TPDType;
  end;

  { TPDClient }

  TPDClient = class(TComponent)
  private
    FSchema: string;
    procedure SetConnected(AValue: Boolean);
  protected
    procedure InitSystemTypes; virtual;
    procedure Init; virtual;
    function GetConnected: Boolean; virtual;
    function GetConnectionString: string; virtual;
    procedure SetConnectionString(AValue: string); virtual;
  public
    Types: TPDTypeList;
    Version: string;
    BackendName: string;
    procedure ObjectLoad(AObject: TObjectProxy); virtual; abstract;
    procedure ObjectSave(AObject: TObjectProxy); virtual; abstract;
    procedure ObjectDelete(AObject: TObjectProxy); virtual; abstract;
    procedure ListLoad(AList: TListProxy); virtual; abstract;
    procedure ListSave(AList: TListProxy); virtual; abstract;
    function TypeIsDefined(AType: TPDType): Boolean; virtual; abstract;
    procedure TypeDefine(AType: TPDType); virtual; abstract;
    procedure TypeUndefine(AType: TPDType); virtual; abstract;
    procedure CheckTypes;
    function TypeExists(Name: string): Boolean; virtual; abstract;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure Install; virtual;
    procedure Uninstall; virtual;
    procedure Update; virtual;
  published
    property Schema: string read FSchema write FSchema;
    property Connected: Boolean read GetConnected write SetConnected;
    property ConnectionString: string read GetConnectionString
      write SetConnectionString;
  end;

  TPDClientClass = class of TPDClient;

implementation

resourcestring
  SClientNotSet = 'Client not set';
  SNotSupported = 'Not supported';
  SVersionMismatch = 'Version mismatch, client: %0:s, server: %1:s. Please upgrade database.';


{ TPDTypeList }

function TPDTypeList.AddType(Name: string; DbType: string = ''): TPDType;
begin
  Result := TPDType(AddNew(TPDType.Create));
  Result.Client := Client;
  Result.Name := Name;
  Result.DbType := DbType;
end;

function TPDTypeList.SearchByName(Name: string): TPDType;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and (TPDType(Items[I]).Name <> Name) do Inc(I);
  if I < Count then Result := TPDType(Items[I])
    else Result := nil;
end;

function TPDType.IsDefined: Boolean;
begin
  if Assigned(Client) then Result := Client.TypeIsDefined(Self)
    else raise EClientNotSet.Create(SClientNotSet);
end;

procedure TPDType.Define;
begin
  if Assigned(Client) then Client.TypeDefine(Self)
    else raise EClientNotSet.Create(SClientNotSet);
end;

procedure TPDType.Undefine;
begin
  if Assigned(Client) then Client.TypeUndefine(Self)
    else raise EClientNotSet.Create(SClientNotSet);
end;

constructor TPDType.Create;
begin
  Properties := TDictionaryStringString.Create;
end;

destructor TPDType.Destroy;
begin
  Properties.Free;
  inherited Destroy;
end;

{ TObjectProxy }

procedure TObjectProxy.Load;
begin
  if Assigned(Client) then Client.ObjectLoad(Self)
    else raise EClientNotSet.Create(SClientNotSet);
end;

procedure TObjectProxy.Save;
begin
  if Assigned(Client) then Client.ObjectSave(Self)
    else raise EClientNotSet.Create(SClientNotSet);
end;

procedure TObjectProxy.Delete;
begin
  if Assigned(Client) then Client.ObjectDelete(Self)
    else raise EClientNotSet.Create(SClientNotSet);
end;

constructor TObjectProxy.Create;
begin
  Properties := TDictionaryStringString.Create;
end;

destructor TObjectProxy.Destroy;
begin
  Properties.Free;
  inherited Destroy;
end;

{ TListProxy }

procedure TListProxy.Clear;
begin
  PageUse := False;
  ColummsFilterUse := False;
  OrderUse := False;
  Objects.Free;
end;

constructor TListProxy.Create;
begin
  ColumnsFilter := TListString.Create;
  Objects := TListObject.Create;
end;

destructor TListProxy.Destroy;
begin
  Objects.Free;
  ColumnsFilter.Free;
  inherited Destroy;
end;

procedure TListProxy.Load;
begin
  if Assigned(Client) then Client.ListLoad(Self)
    else raise EClientNotSet.Create(SClientNotSet);
end;

procedure TListProxy.Save;
begin
  if Assigned(Client) then Client.ListSave(Self)
    else raise EClientNotSet.Create(SClientNotSet);
end;

{ TPDClient }

function TPDClient.GetConnectionString: string;
begin
  Result := '';
end;

procedure TPDClient.SetConnectionString(AValue: string);
begin

end;

procedure TPDClient.SetConnected(AValue: Boolean);
begin
  if AValue then Connect else Disconnect;
end;

procedure TPDClient.InitSystemTypes;
begin
end;

procedure TPDClient.Init;
var
  NewProxy: TListProxy;
  NewType: TPDType;
  NewObject: TObjectProxy;
  DbVersion: string;
begin
  NewProxy := TListProxy.Create;
  NewProxy.Client := Self;
  NewProxy.Path := 'information_schema';
  NewProxy.ObjectName := 'TABLES';
  NewProxy.Condition := '(TABLE_SCHEMA = "' + Schema +
    '") AND (TABLE_NAME = "' + SystemVersionObject + '")';
  NewProxy.Load;
  if NewProxy.Objects.Count > 0 then begin
    NewObject := TObjectProxy.Create;
    NewObject.Client := Self;
    NewObject.Path := Schema;
    NewObject.ObjectName := SystemVersionObject;
    NewObject.Id := 1;
    NewObject.Load;

    DbVersion := NewObject.Properties.Values['Version'];
    if Version <> DbVersion then
      raise Exception.Create(Format(SVersionMismatch, [Version, DbVersion]));
  end else begin
    NewType := TPDType.Create;
    NewType.Client := Self;
    NewType.Name := SystemVersionObject;
    NewType.Properties.Add('Version', 'String');
    NewType.Properties.Add('Time', 'DateTime');
    NewType.Define;

    NewObject := TObjectProxy.Create;
    NewObject.Client := Self;
    NewObject.Path := Schema;
    NewObject.ObjectName := SystemVersionObject;
    NewObject.Properties.Add('Version', Version);
    NewObject.Properties.Add('Time', 'NOW()');
    NewObject.Save;

    Install;
  end;
end;

function TPDClient.GetConnected: Boolean;
begin
  Result := False;
end;

procedure TPDClient.CheckTypes;
var
  StructureVersion: string;
  Data: TDictionaryStringString;
  ObjectId: Integer;
  Tables: TListString;
  I: Integer;
  NewProxy: TListProxy;
begin
  try
    Tables := TListString.Create;
    Data := TDictionaryStringString.Create;

    NewProxy := TListProxy.Create;
    NewProxy.Client := Self;
    NewProxy.Path := 'information_schema';
    NewProxy.ObjectName := 'TABLES';
    NewProxy.Condition := 'TABLE_SCHEMA = "' + Schema + '"';
    NewProxy.Load;
    //Database.Query(DbRows, 'SHOW TABLES');
    Tables.Count := NewProxy.Objects.Count;
    for I := 0 to NewProxy.Objects.Count - 1 do
      Tables[I] := TObjectProxy(NewProxy.Objects[I]).Properties.Values['TABLE_NAME'];

    for I := 0 to Types.Count - 1 do
    with TPDType(Types[I]) do begin
      if (DbType = '') and (Tables.IndexOf(Name) = -1) then begin
        Define;
      end;
    end;
  finally
    NewProxy.Free;
    Data.Free;
    Tables.Free;
  end;
end;

constructor TPDClient.Create(AOwner: TComponent);
begin
  inherited;
  Types := TPDTypeList.Create;
  Types.Client := Self;
  InitSystemTypes;
end;

destructor TPDClient.Destroy;
begin
  Types.Free;
  inherited Destroy;
end;

procedure TPDClient.Connect;
begin
end;

procedure TPDClient.Disconnect;
begin
end;

procedure TPDClient.Install;
begin

end;

procedure TPDClient.Uninstall;
begin

end;

procedure TPDClient.Update;
begin

end;

end.
