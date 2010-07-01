unit UHTTPSessionMySQL;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, UHTTPServer, syncobjs, synacode, UCommon, USqlDatabase;

type

  { TFileHTTPSessionStorage }

  TMySQLHTTPSessionStorage = class(THTTPSessionStorage)
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

function TMySQLHTTPSessionStorage.GetNewSessionId: string;
var
  DbRows: TDbRows;
begin
  DbRows := nil;
  Result := BinToHexString(SHA1(FloatToStr(Now)));
  repeat
    if Assigned(DbRows) then DbRows.Destroy;
    DbRows := SqlDatabase.Query('SELECT * FROM Session WHERE Identification="' +
      Result + '"');
    if DbRows.Count > 0 then Result := BinToHexString(SHA1(FloatToStr(Now)));
  until DbRows.Count > 0;
  DbRows.Destroy;
end;

procedure TMySQLHTTPSessionStorage.GetSessionId(HandlerData: THTTPHandlerData);
begin
  with HandlerData do begin
    if Request.Cookies.IndexOfName(SessionIdCookieName) <> -1 then begin
      SessionId := Request.Cookies.Values[SessionIdCookieName];
    end else begin
      SessionId := GetNewSessionId;
      Response.Cookies.Values[SessionIdCookieName] := SessionId;
    end;
  end;
end;

procedure TMySQLHTTPSessionStorage.Load(HandlerData: THTTPHandlerData);
var
  DbRows: TDbRows;
begin
  GetSessionId(HandlerData);
  try
    Lock.Acquire;
    DbRows := SqlDatabase.Query('SELECT * FROM Session WHERE Identification="' +
      HandlerData.SessionId + '"');
    if DbRows.Count > 0 then begin
      HandlerData.Session.Text := DbRows[0].Values['Variables'];
    end else begin
      HandlerData.SessionId := GetNewSessionId;
    end;
    DbRows.Destroy;
  finally
    Lock.Release;
  end;
  inherited;
end;

procedure TMySQLHTTPSessionStorage.Save(HandlerData: THTTPHandlerData);
var
  DbRows: TDbRows;
  DbRows2: TDbRows;
begin
  try
    Lock.Acquire;
    DbRows := SqlDatabase.Query('SELECT * FROM Session WHERE Identification="' +
      HandlerData.SessionId + '"');
    if DbRows.Count > 0 then
      DbRows2 := SqlDatabase.Query('UPDATE Session SET Variables="' + HandlerData.Session.Text
        + '" WHERE Identification="' + HandlerData.SessionId + '"')
    else DbRows2 := SqlDatabase.Query('REPLACE Session SET Variables="' + HandlerData.Session.Text
        + '" WHERE Identification="' + HandlerData.SessionId + '"');
    DbRows2.Destroy;
    DbRows.Destroy;
    HandlerData.Response.Cookies.Values[SessionIdCookieName] := HandlerData.SessionId;
  finally
    Lock.Release;
  end;
  inherited;
end;

constructor TMySQLHTTPSessionStorage.Create;
begin
  inherited Create;
  Lock := TCriticalSection.Create;
  Sessions := TStringList.Create;
  SessionIdCookieName := 'SessionId';
  SqlDatabase := TSqlDatabase.Create;
  Timeout := 3600;
end;

destructor TMySQLHTTPSessionStorage.Destroy;
begin
  SqlDatabase.Destroy;
  Sessions.Destroy;
  Lock.Destroy;
  inherited Destroy;
end;

end.
