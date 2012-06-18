unit UWebApp;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, CustApp, SpecializedList, UWebPage, UHTTPSessionFile,
  UHTTPServer, Forms, FileUtil;

type
  THTTPServerType = (stCGI, stTCP, stTurboPower);

  TRegistredPage = class
    Name: string;
    Page: TWebPage;
  end;

  { TPageList }

  TPageList = class(TListObject)
    RootDir: string;
    function FindByName(Name: string): TRegistredPage;
    procedure RegisterPage(PageClass: TWebPageClass; out Reference; Path: string);
    function ProducePage(HandlerData: THTTPHandlerData): Boolean;
  end;

  { TWebApp }

  TWebApp = class(TComponent)
  private
    FOnPageProduce: TOnProduceEvent;
    FOnInitialize: TNotifyEvent;
    FServerType: THTTPServerType;
    function DumpExceptionCallStack(E: Exception): string;
    procedure HTTPServerRequest(HandlerData: THTTPHandlerData);
    procedure SetServerType(AValue: THTTPServerType);
  public
    HTTPServer: THTTPServer;
    HTTPSessionStorageFile: THTTPSessionStorageFile;
    LogException: Boolean;
    procedure ShowException(E: Exception);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run;
  published
    property OnPageProduce: TOnProduceEvent read FOnPageProduce write FOnPageProduce;
    property OnInitialize: TNotifyEvent read FOnInitialize write FOnInitialize;
    property ServerType: THTTPServerType read FServerType write SetServerType;
  end;


procedure Register;

implementation

uses
  UHTTPServerCGI, UHTTPServerTCP, UHTTPServerTurboPower;


procedure Register;
begin
  RegisterComponents('CoolWeb', [TWebApp]);
end;


{ TPageList }

function TPageList.FindByName(Name: string): TRegistredPage;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and (TRegistredPage(Items[I]).Name <> Name) do Inc(I);
  if I < Count then Result := TRegistredPage(Items[I])
    else Result := nil;
end;

{ TWebApp }

procedure TWebApp.Run;
begin
  if Assigned(FOnInitialize) then FOnInitialize(Self);
  HTTPServer.Run;
  if (ServerType = stCGI) or (ServerType = stTCP) then
    Application.Terminate;
end;

function TWebApp.DumpExceptionCallStack(E: Exception): string;
var
  I: Integer;
  Frames: PPointer;
  Report: string;
begin
  Report := 'Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(PointerArray(Frames)[I]);
  Result := Report;
end;

procedure TPageList.RegisterPage(PageClass: TWebPageClass; out Reference;
  Path: string);
var
  NewPage: TRegistredPage;
  Instance: TWebPage;
begin
  NewPage := TRegistredPage(AddNew(TRegistredPage.Create));
//  NewPage.Page := PageClass.Create(Self);
  NewPage.Page := PageClass.Create(nil);
  NewPage.Name := Path;
  TWebPage(Reference) := NewPage.Page;
end;

function TPageList.ProducePage(HandlerData: THTTPHandlerData): Boolean;
var
  Page: TRegistredPage;
  PageName: string;
begin
  with HandlerData do begin
    if Request.QueryParts.Count > 0 then PageName := Request.QueryParts[0]
      else PageName := '';
    Page := FindByName(PageName);
    if Assigned(Page) then begin
      Page.Page.OnProduce(HandlerData);
      Result := True;
    end else Result := False;
  end;
end;

procedure TWebApp.HTTPServerRequest(HandlerData: THTTPHandlerData);
begin
  if Assigned(FOnPageProduce) then
    FOnPageProduce(HandlerData);
end;

procedure TWebApp.SetServerType(AValue: THTTPServerType);
begin
  if FServerType = AValue then Exit;
  FServerType := AValue;
  HTTPServer.Free;
  case FServerType of
    stCGI: HTTPServer := THTTPServerCGI.Create(nil);
    stTCP: HTTPServer := THTTPServerTCP.Create(nil);
    stTurboPower: HTTPServer := THTTPServerTurboPower.Create(nil);
  end;
  HTTPServer.OnRequest := HTTPServerRequest;
end;

procedure TWebApp.ShowException(E: Exception);
var
  hstdout: ^Text;
begin
  if not LogException then begin
    hstdout := @stdout;
    WriteLn(hstdout^, 'Content-type: text/html');
    WriteLn(hstdout^);
    Writeln(hstdout^, 'An unhandled exception occurred: ' + E.Message + '<br>');
    WriteLn(hstdout^, StringReplace(DumpExceptionCallStack(E), LineEnding, '<br>', [rfReplaceAll]));
  end else begin
    hstdout := @stdout;
    WriteLn(hstdout^, 'Content-type: text/html');
    WriteLn(hstdout^);
    WriteLn(hstdout^, 'Error occured during page generation.');
    hstdout := @stderr;
    Writeln(hstdout^, 'An unhandled exception occurred: ' + E.Message + '<br>');
    WriteLn(hstdout^, DumpExceptionCallStack(E));
  end;
end;

constructor TWebApp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HTTPServer := THTTPServerCGI.Create(nil);
  HTTPServer.OnRequest := HTTPServerRequest;
end;

destructor TWebApp.Destroy;
begin
  HTTPServer.Free;
  inherited Destroy;
end;


initialization

finalization

end.

