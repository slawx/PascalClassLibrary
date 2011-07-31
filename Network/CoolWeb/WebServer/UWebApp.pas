unit UWebApp;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, CustApp, SpecializedList, UWebPage, UHTTPSessionFile,
  UHTTPServer, UHTTPServerCGI;

type
  TRegistredPage = class
    Name: string;
    Page: TWebPage;
  end;

  { TRegistredPageList }

  TRegistredPageList = class(TListObject)
    function FindByName(Name: string): TRegistredPage;
  end;

  { TWebApp }

  TWebApp = class(TCustomApplication)
  private
    FOnInitialize: TNotifyEvent;
    procedure DoRun; override;
    function DumpExceptionCallStack(E: Exception): string;
    procedure HTTPServerRequest(HandlerData: THTTPHandlerData);
  public
    Pages: TRegistredPageList;
    HTTPServer: THTTPServer;
    HTTPSessionStorageFile: THTTPSessionStorageFile;
    LogException: Boolean;
    procedure ShowException(E: Exception); override;
    procedure RegisterPage(PageClass: TWebPageClass; out Reference; Path: string);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnInitialize: TNotifyEvent read FOnInitialize write FOnInitialize;
  end;


procedure Register;

var
  Application: TWebApp;


implementation

resourcestring
  SPageNotFound = 'Page not found';


procedure Register;
begin
  RegisterClass(TWebApp);
end;


{ TRegistredPageList }

function TRegistredPageList.FindByName(Name: string): TRegistredPage;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and (TRegistredPage(Items[I]).Name <> Name) do Inc(I);
  if I < Count then Result := TRegistredPage(Items[I])
    else Result := nil;
end;

{ TWebApp }

procedure TWebApp.DoRun;
begin
  try
    if Assigned(FOnInitialize) then FOnInitialize(Self);
    HTTPServer.Run;
  finally
    Terminate;
  end;
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

procedure TWebApp.RegisterPage(PageClass: TWebPageClass; out Reference;
  Path: string);
var
  NewPage: TRegistredPage;
  Instance: TWebPage;
begin
  NewPage := TRegistredPage(Pages.AddNew(TRegistredPage.Create));
//  NewPage.Page := PageClass.Create(Self);
  NewPage.Page := PageClass.Create(nil);
  NewPage.Name := Path;
  TWebPage(Reference) := NewPage.Page;
end;

procedure TWebApp.HTTPServerRequest(HandlerData: THTTPHandlerData);
var
  Page: TRegistredPage;
  PageName: string;
begin
  with HandlerData do begin
    //Request.QueryParts.Count := 2;
    //Request.QueryParts[0] := 'uzivatel';
    //Request.QueryParts[1] := 'prihlaseni';

    if Request.QueryParts.Count > 0 then PageName := Request.QueryParts[0]
      else PageName := '';
    Page := Pages.FindByName(PageName);
    if Assigned(Page) then begin
      Page.Page.OnProduce(HandlerData);
    end else Response.Stream.WriteString(SPageNotFound);
  end;
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
    WriteLn(hstdout^, StringReplace(DumpExceptionCallStack(E), LineEnding, '<br>', [rfReplaceAll]));
  end;
end;

constructor TWebApp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Pages := TRegistredPageList.Create;
  HTTPServer := THTTPServerCGI.Create(nil);
  HTTPServer.OnRequest := HTTPServerRequest;
end;

destructor TWebApp.Destroy;
begin
  Pages.Free;
  HTTPServer.Free;
  inherited Destroy;
end;


initialization

Application := TWebApp.Create(nil);


finalization

Application.Free;

end.

