unit UPDClientMemory;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, UPDClient, SpecializedDictionary, SpecializedList;

type

  { TPDClientMemory }

  TPDClientMemory = class(TPDClient)
  protected
    FLastObjectId: Integer;
    procedure InitSystemTypes; override;
    function GetConnected: Boolean; override;
    procedure Init; override;
    function GetNewObjectId: Integer;
    function SearchObject(Id: Integer): TObjectProxy;
  public
    Objects: TListObject;
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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect; override;
    procedure Disconnect; override;
  end;

resourcestring
  SObjectNotFound = 'Object with id %s not found';

implementation

{ TPDClientMemory }

procedure TPDClientMemory.InitSystemTypes;
begin
  inherited InitSystemTypes;
end;

function TPDClientMemory.GetConnected: Boolean;
begin
  Result:=inherited GetConnected;
end;

procedure TPDClientMemory.Init;
begin
  inherited Init;
end;

function TPDClientMemory.GetNewObjectId: Integer;
begin
  Inc(FLastObjectId);
  Result := FLastObjectId;
end;

function TPDClientMemory.SearchObject(Id: Integer): TObjectProxy;
var
  I: Integer;
begin
  I := 0;
  while (I < Objects.Count) and
    (TObjectProxy(Objects[I]).Id <> Id) do Inc(I);
  if I < Objects.Count then Result := TObjectProxy(Objects[I])
    else Result := nil;
end;

procedure TPDClientMemory.ObjectLoad(AObject: TObjectProxy);
var
  Obj: TObjectProxy;
begin
  if AObject.Id = 0 then raise Exception.Create(SCantLoadObjectWithoutId);
  Obj := SearchObject(AObject.Id);
  if Assigned(Obj) then AObject.Assign(Obj)
    else raise Exception.CreateFmt(SObjectNotFound, [AObject.Id]);
end;

procedure TPDClientMemory.ObjectSave(AObject: TObjectProxy);
var
  I: Integer;
  Obj: TObjectProxy;
begin
  if AObject.Id = 0 then raise Exception.Create(SCantLoadObjectWithoutId);
  Obj := SearchObject(AObject.Id);
  if Assigned(Obj) then Obj.Assign(AObject)
    else begin
      AObject.Id := GetNewObjectId;
      Obj := TObjectProxy(Objects.AddNew(TObjectProxy.Create));
      Obj.Assign(AObject);
    end;
end;

procedure TPDClientMemory.ObjectDelete(AObject: TObjectProxy);
var
  Obj: TObjectProxy;
begin
  Obj := SearchObject(AObject.Id);
  if Assigned(Obj) then Objects.Delete(Objects.IndexOf(Obj))
    else raise Exception.CreateFmt(SObjectNotFound, [AObject.Id])
end;

procedure TPDClientMemory.ListLoad(AList: TListProxy);
var
  Filter: string;
  DbCondition: string;
  I: Integer;
  P: Integer;
  NewObject: TObjectProxy;
  Table: string;
begin
  AList.Objects.Clear;
  for I := 0 to Objects.Count - 1 do
  with TObjectProxy(Objects[I]) do begin
    if 1 = 1 then begin
      NewObject := TObjectProxy.Create;
      NewObject.Properties.Assign(Properties);
      NewObject.Client := AList.Client;
      NewObject.ObjectName := AList.ObjectName;
      NewObject.Path := AList.Path;
      AList.Objects.Add(NewObject);

      if AList.ColummsFilterUse then begin
        for P := 0 to Properties.Count - 1 do
        if AList.ColumnsFilter.IndexOf(Properties.Keys[I]) <> -1 then
          NewObject.Properties.Add(Properties.Keys[I], Properties[I].Value);
      end else NewObject.Properties.Assign(Properties);
    end;
  end;
  if AList.OrderUse then begin

  end;
  if AList.PageUse then begin
  end;
end;

procedure TPDClientMemory.ListSave(AList: TListProxy);
begin

end;

procedure TPDClientMemory.TypeDefine(AType: TPDType);
begin

end;

procedure TPDClientMemory.TypeUndefine(AType: TPDType);
begin

end;

function TPDClientMemory.TypeIsDefined(AType: TPDType): Boolean;
begin

end;

procedure TPDClientMemory.Install;
begin

end;

procedure TPDClientMemory.Uninstall;
begin

end;

constructor TPDClientMemory.Create(AOwner: TComponent);
begin
  inherited;
  Objects := TListObject.Create;
  BackendName := 'Memory';
end;

destructor TPDClientMemory.Destroy;
begin
  Objects.Free;
  inherited Destroy;
end;

procedure TPDClientMemory.Connect;
begin
  inherited Connect;
end;

procedure TPDClientMemory.Disconnect;
begin
  inherited Disconnect;
end;

end.

