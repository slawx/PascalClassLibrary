unit UUser;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, synacode, USqlDatabase, UCommon, UHTTPServer;

const
  AnonymousUserId = 1;

type
  EDuplicateItem = class(Exception);
  ENotFound = class(Exception);

  { TWebUser }

  TWebUser = class
    FullName: string;
    Database: TSqlDatabase;
    HandlerData: THTTPHandlerData;
    procedure Delete(Id: Integer);
    procedure Add(Name, Password, Email: string);
    function GetIdByName(Name: string): Integer;
    function GetIdByNamePassword(Name: string; PassWord: string): Integer;
  end;

  { TWebOnlineUser }

  TWebOnlineUser = class
    Database: TSqlDatabase;
    HandlerData: THTTPHandlerData;
    Id: Integer;
    User: Integer;
    procedure Update;
    procedure Login(User: Integer);
    procedure Logout;
  end;

implementation

resourcestring
  SDuplicateUserItem = 'User name already used.';

{ TOnlineUser }

procedure TWebOnlineUser.Update;
var
  DbRows: TDbRows;
  Id: Integer;
begin
  try
    DbRows := TDbRows.Create;
    Database.Query(DbRows, 'SELECT * FROM `UserOnline` WHERE `SessionId`="' +
      HandlerData.Request.Cookies.Values['SessionId'] + '"');
    if DbRows.Count > 0 then begin
      // Update exited
      Id := StrToInt(DbRows[0].Values['Id']);
      User := StrToInt(DbRows[0].Values['User']);
      Database.Query(DbRows, 'UPDATE `UserOnline` SET `ActivityTime` = NOW() WHERE `Id`=' + IntToStr(Id));
    end else begin
      // Create new record
      Database.Query(DbRows, 'INSERT INTO `UserOnline` (`User`, `ActivityTime`, `SessionId`) ' +
        'VALUES (1, NOW(), "' + HandlerData.Request.Cookies.Values['SessionId'] + '")');
      Id := Database.LastInsertId;
      User := 1;
    end;
  finally
    DbRows.Free;
  end;
end;

procedure TWebOnlineUser.Login(User: Integer);
var
  DbRows: TDbRows;
begin
  Logout;
  try
    DbRows := TDbRows.Create;
    Database.Query(DbRows, 'UPDATE `UserOnline` SET `User` = ' + IntToStr(User) + ', `LoginTime` = NOW() WHERE `SessionId`="' +
      HandlerData.Request.Cookies.Values['SessionId'] + '"');
  finally
    DbRows.Free;
  end;
  Self.User := User;
end;

procedure TWebOnlineUser.Logout;
var
  DbRows: TDbRows;
begin
  if Id = AnonymousUserId then Update;
  if User <> AnonymousUserId then begin
    try
      DbRows := TDbRows.Create;
      Database.Query(DbRows, 'UPDATE `UserOnline` SET `User` = ' + IntToStr(AnonymousUserId) + ' WHERE `SessionId`="' +
        HandlerData.Request.Cookies.Values['SessionId'] + '"');
    finally
      DbRows.Free;
    end;
    User := AnonymousUserId;
  end;
end;

{ TUser }

procedure TWebUser.Delete(Id: Integer);
var
  DbRows: TDbRows;
begin
  try
    DbRows := TDbRows.Create;
    Database.Query(DbRows, 'DELETE FROM `User` WHERE `Id`=' + IntToStr(Id));
  finally
    DbRows.Free;
  end;
end;

procedure TWebUser.Add(Name, Password, Email: string);
var
  Salt: string;
  DbRows: TDbRows;
begin
  try
    DbRows := TDbRows.Create;
    Database.Query(DbRows, 'SELECT `Id` FROM `User` WHERE `Name`="' + Name + '"');
    if DbRows.Count = 0 then begin
      Salt := EncodeBase64(Copy(BinToHexString(SHA1(FloatToStr(Now))), 1, 8));
      Database.Query(DbRows, 'INSERT INTO `User` (`Name`, `Password`, `Salt`, `Email`, `RegistrationTime`) VALUES ("' +
        Name + '", SHA1(CONCAT("' + Password + '", "' + Salt + '")), "' + Salt +
        '", "' + Email + '", NOW())');
    end else raise EDuplicateItem.Create(SDuplicateUserItem);
  finally
    DbRows.Free;
  end;
end;

function TWebUser.GetIdByName(Name: string): Integer;
var
  DbRows: TDbRows;
begin
  try
    DbRows := TDbRows.Create;
    Database.Query(DbRows, 'SELECT `Id` FROM `User` WHERE `Name`="' + Name + '"');
    if DbRows.Count = 1 then Result := StrToInt(DbRows[0].Items[0].Value)
      else raise ENotFound.Create('User "' + Name + '" not found');
  finally
    DBRows.Free;
  end;
end;

function TWebUser.GetIdByNamePassword(Name: string; PassWord: string): Integer;
var
  DbRows: TDbRows;
begin
  try
    DbRows := TDbRows.Create;
    Database.Query(DbRows, 'SELECT `Id` FROM `User` WHERE `Name`="' + Name + '" AND ' +
      '`Password` = SHA1(CONCAT("' + Password + '", Salt))');
    if DbRows.Count = 1 then Result := StrToInt(DbRows[0].Items[0].Value)
      else raise ENotFound.Create('User "' + Name + '" not found');
  finally
    DBRows.Free;
  end;
end;

end.

