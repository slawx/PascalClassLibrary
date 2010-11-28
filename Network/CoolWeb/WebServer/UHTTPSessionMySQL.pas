unit UHTTPSessionMySQL;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, UHTTPServer, syncobjs, synacode, UCommon, USqlDatabase;

type

  { TFileHTTPSessionStorage }

  THTTPSessionStorageMySQL = class(THTTPSessionStorage)
  private
    Lock: TCriticalSection;
    function GetNewSessionId: string;
    procedure GetSessionId(HandlerData: THTTPHandlerData);
  public
    Timeout: Integer; // in seconds
    SqlDatabase: TSqlDatabase;
    SessionIdCookieName: string;
    Sessions: TStringList;
    procedure Load(HandlerData: THTTPHandlerData); override;
    procedure Save(HandlerData: THTTPHandlerData); override;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ THTTPSession }

function THTTPSessionStorageMySQL.GetNewSessionId: string;
var
  DbRows: TDbRows;
  Found: Boolean;
begin
  repeat
    Result := BinToHexString(SHA1(FloatToStr(Now)));
    try
      DbRows := TDbRows.Create;
      SqlDatabase.Query(DbRows, 'SELECT * FROM `HTTPSession` WHERE `Identification`="' +
        Result + '"');
      Found := DbRows.Count > 0;
    finally
      DbRows.Free;
    end;
  until not Found;
end;

procedure THTTPSessionStorageMySQL.GetSessionId(HandlerData: THTTPHandlerData);
begin
  with HandlerData do begin
    if Request.Cookies.IndexOfName(SessionIdCookieName) <> -1 then begin
      SessionId := Request.Cookies.Values[SessionIdCookieName];
    end else begin
      SessionId := GetNewSessionId;
    end;
  end;
end;

procedure THTTPSessionStorageMySQL.Load(HandlerData: THTTPHandlerData);
var
  DbRows: TDbRows;
begin
  GetSessionId(HandlerData);
  try
    Lock.Acquire;
    DbRows := TDbRows.Create;
    SqlDatabase.Query(DbRows, 'DELETE FROM `HTTPSession` WHERE `Time` < DATE_SUB(NOW(), INTERVAL ' +
      IntToStr(Timeout) +' SECOND)');
    SqlDatabase.Query(DbRows, 'SELECT * FROM `HTTPSession` WHERE `Identification`="' +
      HandlerData.SessionId + '"');
    if DbRows.Count > 0 then begin
      HandlerData.Session.Text := DbRows[0].Values['Variables'];
    end else begin
      HandlerData.SessionId := GetNewSessionId;
    end;
  finally
    DbRows.Free;
    Lock.Release;
  end;
  inherited;
end;

procedure THTTPSessionStorageMySQL.Save(HandlerData: THTTPHandlerData);
var
  DbRows: TDbRows;
  DbRows2: TDbRows;
begin
  try
    Lock.Acquire;
    DbRows := TDbRows.Create;
    DbRows2 := TDbRows.Create;
    SqlDatabase.Query(DbRows, 'SELECT * FROM `HTTPSession` WHERE `Identification`="' +
      HandlerData.SessionId + '"');
    if DbRows.Count > 0 then
      SqlDatabase.Query(DbRows2, 'UPDATE `HTTPSession` SET `Variables`="' + HandlerData.Session.Text
        + '", `Time` = NOW() WHERE `Identification`="' + HandlerData.SessionId + '"')
    else SqlDatabase.Query(DbRows2, 'INSERT INTO `HTTPSession` (`Time`,  `Variables`, `Identification`) VALUES (' +
    'NOW(), "' + HandlerData.Session.Text + '", "' + HandlerData.SessionId + '")');
    HandlerData.Response.Cookies.Values[SessionIdCookieName] := HandlerData.SessionId;
  finally
    DbRows2.Free;
    DbRows.Free;
    Lock.Release;
  end;
  inherited;
end;

constructor THTTPSessionStorageMySQL.Create;
begin
  inherited Create;
  Lock := TCriticalSection.Create;
  Sessions := TStringList.Create;
  SessionIdCookieName := 'SessionId';
  Timeout := 3 * 3600;
end;

destructor THTTPSessionStorageMySQL.Destroy;
begin
  Sessions.Free;
  Lock.Free;
  inherited Destroy;
end;

end.
