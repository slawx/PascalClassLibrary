unit UPermission;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, UWebObject, UCommon, USqlDatabase, UStringListEx;

type
  { TPermissionACL }

  TPermissionACL = class
    procedure Add(ARO, ACO: Integer);
    procedure Copy(SourceARO, DestARO: Integer);
    procedure Delete(ARO: Integer);
  end;

  { TPermissionACO }

  TPermissionACO = class(TWebObject)
    ACL: TPermissionACL;
    procedure Delete(ACO: Integer);
    procedure Add(Module, Action, Item: Integer);
  end;

  { TPermissionARO }

  TPermissionARO = class(TWebObject)
    ACL: TPermissionACL;
    procedure Delete(ARO: Integer);
    procedure Add(Group, User: Integer);
  end;

  { TPermission }

  TPermission = class(TWebObject)
  private
  public
    constructor Create;
    destructor Destroy; override;
    procedure RebuildCache;
    function AppendFilter(SourceSQL: string; User, Action, ObjectId: Integer): string;
    function GetARO(User, Group: Integer): Integer;
    function GetACO(AObject, Row: Integer): Integer;
    function Check(ARO, ACO, Action: Integer): Boolean;
    function GetAROListForUser(User: Integer): TStringListEx;
    function GetAROListForGroup(Group: Integer): TStringListEx;
  end;

implementation      


{ TPermission }

constructor TPermission.Create;
begin

end;

destructor TPermission.Destroy;
begin
  inherited Destroy;
end;

procedure TPermission.RebuildCache;
begin

end;

function TPermission.AppendFilter(SourceSQL: string; User, Action, ObjectId: Integer): string;
var
  AROList: TStringListEx;
begin
  AROList := GetAROListForUser(User);
  Result := 'SELECT `T`.* FROM (' + SourceSQL + ') AS `T` JOIN `PermissionACL` ON `PermissionACL`.`ARO` IN (' +
    AROList.Implode(',') + ') AND `PermissionACL`.`Action` = ' + IntToStr(Action) +
    ' JOIN `PermissionACO` ON `PermissionACO`.`Id` = `PermissionACL`.`ACO` AND `PermissionACO`.`Item` = `T`.`Id` AND `PermissionACO`.`Object` = ' +
    IntToStr(ObjectId) + ' GROUP BY `T`.`Id`';
  AROList.Destroy;
end;

function TPermission.GetARO(User, Group: Integer): Integer;
begin

end;

function TPermission.GetACO(AObject, Row: Integer): Integer;
begin

end;

function TPermission.Check(ARO, ACO, Action: Integer): Boolean;
//var
//  ItemFilter: string;
begin
//  if Item <> 0 then ItemFilter := ' AND (Item=' + IntToStr(Item) + ')'
//    else ItemFilter := ' AND (Item IS NULL)';

end;

function TPermission.GetAROListForUser(User: Integer): TStringListEx;
var
  DbRows: TDbRows;
  I: Integer;
  GroupItems: TStringListEx;
begin
  Result := TStringListEx.Create;
  DbRows := Database.Query('SELECT * FROM `PermissionARO` WHERE `User`=' + IntToStr(User));
  for I := 0 to DbRows.Count - 1 do begin
    Result.Add(DbRows[I].Values['Id']);
  end;
  DbRows.Destroy;

  // Append group items
  DbRows := Database.Query('SELECT * FROM `UserGroupAssignment` WHERE `User`=' + IntToStr(User));
  for I := 0 to DbRows.Count - 1 do begin
    GroupItems := GetAROListForGroup(StrToInt(DbRows[I].Values['ParentGroup']));
    Result.AddStrings(GroupItems);
    GroupItems.Destroy;
  end;
  DbRows.Destroy;
end;

function TPermission.GetAROListForGroup(Group: Integer): TStringListEx;
var
  DbRows: TDbRows;
  I: Integer;
  GroupItems: TStringListEx;
begin
  Result := TStringListEx.Create;
  DbRows := Database.Query('SELECT * FROM `PermissionARO` WHERE `Group`=' + IntToStr(Group));
  for I := 0 to DbRows.Count - 1 do begin
    Result.Add(DbRows[I].Values['Id']);
  end;
  DbRows.Destroy;

  // Append subgroup items
  DbRows := Database.Query('SELECT * FROM `UserGroupAssignment` WHERE `Group`=' + IntToStr(Group));
  for I := 0 to DbRows.Count - 1 do begin
    GroupItems := GetAROListForGroup(StrToInt(DbRows[I].Values['ParentGroup']));
    Result.AddStrings(GroupItems);
    GroupItems.Destroy;
  end;
  DbRows.Destroy;
end;

{ TPermissionACL }

procedure TPermissionACL.Add(ARO, ACO: Integer);
begin

end;

procedure TPermissionACL.Copy(SourceARO, DestARO: Integer);
begin

end;

procedure TPermissionACL.Delete(ARO: Integer);
begin

end;

{ TPermissionARO }

procedure TPermissionARO.Delete(ARO: Integer);
begin
end;

procedure TPermissionARO.Add(Group, User: Integer);
begin

end;

{ TPermissionACO }

procedure TPermissionACO.Delete(ACO: Integer);
begin
end;

procedure TPermissionACO.Add(Module, Action, Item: Integer);
begin

end;

end.
