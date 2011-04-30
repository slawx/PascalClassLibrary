unit UHTTPSessionFile;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, UHTTPServer, syncobjs, synacode, UCommon;

type

  { THTTPSessionStorageFile }

  THTTPSessionStorageFile = class(THTTPSessionStorage)
  private
    FDirectory: string;
    FSessionIdCookieName: string;
    FTimeout: Integer;
    Lock: TCriticalSection;
    function GetNewSessionId: string;
    procedure GetSessionId(HandlerData: THTTPHandlerData);
  public
    Sessions: TStringList;
    procedure Load(HandlerData: THTTPHandlerData); override;
    procedure Save(HandlerData: THTTPHandlerData); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Timeout: Integer read FTimeout write FTimeout; // in seconds
    property Directory: string read FDirectory write FDirectory;
    property SessionIdCookieName: string read FSessionIdCookieName
      write FSessionIdCookieName;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CoolWeb', [THTTPSessionStorageFile]);
end;


{ THTTPSession }

function THTTPSessionStorageFile.GetNewSessionId: string;
begin
  Result := BinToHexString(SHA1(FloatToStr(Now)));
  while FileExists(Directory + '/' + Result) do
    Result := BinToHexString(SHA1(FloatToStr(Now)));
end;

procedure THTTPSessionStorageFile.GetSessionId(HandlerData: THTTPHandlerData);
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

procedure THTTPSessionStorageFile.Load(HandlerData: THTTPHandlerData);
var
  SessionFile: string;
begin
  GetSessionId(HandlerData);
  try
    Lock.Acquire;
    SessionFile := Directory + '/' + HandlerData.SessionId;
    if FileExists(SessionFile) then
      HandlerData.Session.LoadFromFile(SessionFile)
      else HandlerData.SessionId := GetNewSessionId;
  finally
    Lock.Release;
  end;
  inherited;
end;

procedure THTTPSessionStorageFile.Save(HandlerData: THTTPHandlerData);
var
  SessionFile: string;
begin
  try
    Lock.Acquire;
    SessionFile := Directory + '/' + HandlerData.SessionId;
    ForceDirectories(Directory);
    if DirectoryExists(Directory) then begin
      DeleteFile(SessionFile);
      HandlerData.Session.SaveToFile(SessionFile)
    end else raise Exception.Create('Can''t create session storage directory.');

    HandlerData.Response.Cookies.Values[SessionIdCookieName] := HandlerData.SessionId;
  finally
    Lock.Release;
  end;
  inherited;
end;

constructor THTTPSessionStorageFile.Create(AOwner: TComponent);
begin
  inherited;
  Lock := TCriticalSection.Create;
  Sessions := TStringList.Create;
  SessionIdCookieName := 'SessionId';
  Directory := 'Session';
  Timeout := 3600;
end;

destructor THTTPSessionStorageFile.Destroy;
begin
  Sessions.Destroy;
  Lock.Destroy;
  inherited Destroy;
end;

end.
