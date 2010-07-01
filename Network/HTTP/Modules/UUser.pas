unit UUser;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, UWebObject, synacode, USqlDatabase, UCommon;

type
  EDuplicateItem = Exception;
  ENotFound = Exception;

  { TWebUser }

  TWebUser = class(TWebObject)
    procedure Delete(Id: Integer);
    procedure Add(Name, Password, Email: string);
    function GetIdByName(Name: string): Integer;
    function GetIdByNamePassword(Name: string; PassWord: string): Integer;
  end;
  
  { TWebOnlineUser }

  TWebOnlineUser = class(TWebObject)
    Id: Integer;
    User: Integer;
    procedure Update;
    procedure Login(User: Integer);
    procedure Logout;
  end;

implementation

{ TOnlineUser }

procedure TWebOnlineUser.Update;
var
  DbRows: TDbRows;
  Id: Integer;
begin
  DbRows := Database.Query('SELECT * FROM `UserOnline` WHERE `SessionId`="' +
    HandlerData.Request.Cookies.Values['SessionId'] + '"');
  if DbRows.Count > 0 then begin
    // Update exited
    Id := StrToInt(DbRows[0].Values['Id']);
    DbRows.Destroy;
    DbRows := Database.Query('UPDATE `UserOnline` SET `ActivityTime` = NOW() WHERE `Id`=' + IntToStr(Id));
  end else begin
    // Create new record
    DbRows.Destroy;
    DbRows := Database.Query('INSERT INTO `UserOnline` (`User`, `ActivityTime`, `SessionId`) ' +
      'VALUES (1, NOW(), "' + HandlerData.Request.Cookies.Values['SessionId'] + '")');
    Id := Database.LastInsertId;
  end;
  DbRows.Destroy;
end;

procedure TWebOnlineUser.Login(User: Integer);
var
  DbRows: TDbRows;
begin
  Logout;
  DbRows := Database.Query('UPDATE `UserOnline` SET `User` = ' + IntToStr(User) + ', `LoginTime` = NOW() WHERE `SessionId`="' +
    HandlerData.Request.Cookies.Values['SessionId'] + '"');
  DbRows.Destroy;
  Self.User := User;
end;

procedure TWebOnlineUser.Logout;
var
  DbRows: TDbRows;
begin
  if Id = 1 then Update;
  if User <> 1 then begin
    DbRows := Database.Query('UPDATE `UserOnline` SET `User` = 1 WHERE `SessionId`="' +
      HandlerData.Request.Cookies.Values['SessionId'] + '"');
    DbRows.Destroy;
    User := 1;
  end;
end;

{ TUser }

procedure TWebUser.Delete(Id: Integer);
begin
  Database.Query('DELETE FROM `User` WHERE `Id`=' + IntToStr(Id));
end;

procedure TWebUser.Add(Name, Password, Email: string);
var
  Salt: string;
  DbRows: TDbRows;
begin
  DbRows := Database.Query('SELECT `Id` FROM `User` WHERE `Name`="' + Name + '"');
  try
    if DbRows.Count = 0 then begin
      Salt := EncodeBase64(Copy(BinToHexString(SHA1(FloatToStr(Now))), 1, 8));
      Database.Query('INSERT INTO `User` (`Name`, `Password`, `Salt`, `Email`, `RegistrationTime`) VALUES ("' +
        Name + '", SHA1(CONCAT("' + Password + '", "' + Salt + '")), "' + Salt +
        '", "' + Email + '", NOW())');
    end else raise EDuplicateItem.Create('User name already used');
  finally
    DbRows.Destroy;
  end;
end;

function TWebUser.GetIdByName(Name: string): Integer;
var
  DbRows: TDbRows;
begin
  DbRows := Database.Query('SELECT `Id` FROM `User` WHERE `Name`="' + Name + '"');
  try
    if DbRows.Count = 1 then Result := StrToInt(DbRows[0].ValuesAtIndex[0])
      else raise ENotFound.Create('User "' + Name + '" not found');
  finally
    DBRows.Destroy;
  end;
end;

function TWebUser.GetIdByNamePassword(Name: string; PassWord: string): Integer;
var
  DbRows: TDbRows;
begin
  DbRows := Database.Query('SELECT `Id` FROM `User` WHERE `Name`="' + Name + '" AND ' +
    '`Password` = SHA1(CONCAT("' + Password + '", Salt))');
  try
    if DbRows.Count = 1 then Result := StrToInt(DbRows[0].ValuesAtIndex[0])
      else raise ENotFound.Create('User "' + Name + '" not found');
  finally
    DBRows.Destroy;
  end;
end;

end.

