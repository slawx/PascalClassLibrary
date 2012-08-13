unit UUpdateChecker;

{$mode delphi}{$H+}

interface

uses
  {$IFDEF Windows}Windows, ShellApi, {$ENDIF}Forms, Classes, SysUtils, httpsend, DOM, XMLWrite, XMLRead, UXMLUtils,
  FileUtil, Dialogs, Process, Blcksock, UFormDownloadProgress;

type
  TVersionInfo = record
    Id: Integer;
    Version: string;
    SourceURL: string;
    ReleaseTime: TDateTime;
    ReleaseNotes: string;
  end;

  { TUpdateChecker }

  TUpdateChecker = class(TComponent)
  private
    FBranchId: Integer;
    FVersionInfo: TVersionInfo;
    HTTPSender: THTTPSend;
    FOnTerminate: TNotifyEvent;
    FVersionInfoURL: string;
    function DownloadHTTP(URL, TargetFile: string): Boolean;
    function InstallerFileName: string;
    function IsSystemAdmin: Boolean;
    procedure SockStatus(Sender: TObject; Reason: THookSocketReason;
    const Value: String);
  public
    FormDownloadProgress: TFormDownloadProgress;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadVersionInfo: Boolean;
    { Download source file using HTTP protocol and save it to temp folder }
    procedure Download;
    procedure Install;
    property VersionInfo: TVersionInfo read FVersionInfo write FVersionInfo;
  published
    property VersionInfoURL: string read FVersionInfoURL write FVersionInfoURL;
    property BranchId: Integer read FBranchId write FBranchId;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
  end;

procedure Register;

resourcestring
  SWrongFileFormat = 'Wrong file format';
  SCantExecuteFile = 'Can''t execute installer "%s"';
  SDownloadProgress = 'Download progress';
  SFile = 'File:';
  SProgress = 'Progress:';

implementation

{$IFDEF Windows}
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5)) ;

const
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('Samples', [TUpdateChecker]);
end;

{ TUpdateChecker }

function TUpdateChecker.LoadVersionInfo: Boolean;
var
  Content: string;
  URL: string;
  XmlDocument: TXMLDocument;
  Node1: TDOMNode;
  Node2: TDOMNode;
  Node3: TDOMNode;
begin
  FVersionInfo.Version := '';
  FVersionInfo.Id := 0;
  FVersionInfo.SourceURL := '';
  with HTTPSender do begin
    Clear;
    URL := VersionInfoURL + '?BranchId=' + IntToStr(BranchId) +
    '&Id&Version&SourceURL&ReleaseTime&Limit=1';
    if HTTPMethod('GET', URL) then begin
      Document.Position := 0;
      try
      ReadXMLFile(XmlDocument, Document);
      if XmlDocument.DocumentElement.NodeName <> 'SourceList' then
        raise Exception.Create(SWrongFileFormat);
      Node1 := XmlDocument.DocumentElement.FindNode('Items');
      if Assigned(Node1) then begin
        Node2 := Node1.FirstChild;
        while Assigned(Node2) and (Node2.NodeName = 'Source') do begin
          Node3 := Node2.FindNode('Version');
          if Assigned(Node3) then
            FVersionInfo.Version := UTF8Encode(string(Node3.TextContent));
          Node3 := Node2.FindNode('Id');
          if Assigned(Node3) then
            FVersionInfo.Id := StrToInt(Node3.TextContent);
          Node3 := Node2.FindNode('SourceURL');
          if Assigned(Node3) then
            FVersionInfo.SourceURL := UTF8Encode(string(Node3.TextContent));
          Node3 := Node2.FindNode('ReleaseTime');
          if Assigned(Node3) then
            FVersionInfo.ReleaseTime := XMLTimeToDateTime(Node3.TextContent);
          Node2 := Node2.NextSibling;
        end;
      end;
      finally
        XmlDocument.Free;
      end;
    end;
  end;
  Result := (FVersionInfo.Version <> '') and (VersionInfo.Id <> 0) and
    (VersionInfo.SourceURL <> '');
end;

procedure TUpdateChecker.Download;
begin
  if FVersionInfo.SourceURL <> '' then begin
    HTTPSender.Clear;
    try
      FormDownloadProgress.Show;
      FormDownloadProgress.ProgressBar1.Max := 0;
      FormDownloadProgress.LabelFileName.Caption := VersionInfo.SourceURL;
      HTTPSender.Sock.OnStatus := SockStatus;
      if HTTPSender.HTTPMethod('GET', FVersionInfo.SourceURL) then
        HTTPSender.Document.SaveToFile(InstallerFileName);
    finally
      FormDownloadProgress.Hide;
      HTTPSender.Sock.OnStatus := nil;
    end;
  end;
end;

procedure TUpdateChecker.Install;
var
  Process: TProcess;
begin
  if FileExistsUTF8(InstallerFileName) then begin
    if not IsSystemAdmin then
      try
        Process := TProcess.Create(nil);
        Process.CommandLine := 'runas ' + InstallerFileName;
        Process.Options := Process.Options + [];
        Process.Execute;
      finally
        Process.Free;
      end
      //ShellExecute(0, PChar('runas'), PChar(InstallerFileName),
      //  0, 0, SW_SHOWNORMAL)
    else
    try
      Process := TProcess.Create(nil);
      Process.CommandLine := InstallerFileName;
      Process.Options := Process.Options + [];
      Process.Execute;
    finally
      Process.Free;
    end;
    if Assigned(FOnTerminate) then FOnTerminate(Self);
  end else ShowMessage(Format(SCantExecuteFile, [InstallerFileName]));
end;

function TUpdateChecker.DownloadHTTP(URL, TargetFile: string): Boolean;
// Download file; retry if necessary.
// Deals with SourceForge download links
// Could use Synapse HttpGetBinary, but that doesn't deal
// with result codes (i.e. it happily downloads a 404 error document)
const
  MaxRetries = 3;
var
  HTTPGetResult: Boolean;
  HTTPSender: THTTPSend;
  RetryAttempt: Integer;
begin
  Result := False;
  RetryAttempt := 1;
  //Optional: mangling of Sourceforge file download URLs; see below.
  //URL:=SourceForgeURL(URL); //Deal with sourceforge URLs
    try
      // Try to get the file
      HTTPGetResult := HTTPSender.HTTPMethod('GET', URL);
      while (not HTTPGetResult) and (RetryAttempt < MaxRetries) do
      begin
        Sleep(500 * RetryAttempt);
        HTTPGetResult := HTTPSender.HTTPMethod('GET', URL);
        Inc(RetryAttempt);
      end;
      // If we have an answer from the server, LoadVersionInfo if the file
      // was sent to us.
      case HTTPSender.Resultcode of
        100..299:
          begin
            with TFileStream.Create(TargetFile,fmCreate or fmOpenWrite) do
            try
              Seek(0, soFromBeginning);
              CopyFrom(HTTPSender.Document, 0);
            finally
              Free;
            end;
            Result := True;
          end; //informational, success
        300..399: Result := False; //redirection. Not implemented, but could be.
        400..499: Result := False; //client error; 404 not found etc
        500..599: Result := False; //internal server error
        else Result := False; //unknown code
      end;
    except
      // We don't care for the reason for this error; the download failed.
      Result := False;
    end;
end;

function TUpdateChecker.InstallerFileName: string;
begin
  Result := UTF8Encode(GetTempDir) + DirectorySeparator +
    ExtractFileName(FVersionInfo.SourceURL);
end;

constructor TUpdateChecker.Create(AOwner: TComponent);
begin
  inherited;
  FVersionInfoURL := 'http://localhost/VersionInfo.xml';
  HTTPSender := THTTPSend.Create;
  FormDownloadProgress := TFormDownloadProgress.Create(nil);
end;

destructor TUpdateChecker.Destroy;
begin
  FormDownloadProgress.Free;
  HTTPSender.Free;
  inherited;
end;

{$IFDEF Windows}
function TUpdateChecker.IsSystemAdmin: Boolean;
var
  hAccessToken: THandle;
  ptgGroups: PTokenGroups;
  dwInfoBufferSize: DWORD;
  psidAdministrators: PSID;
  g: Integer;
  bSuccess: BOOL;
begin
  Result := False;
  bSuccess := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, hAccessToken) ;
  if not bSuccess then
  begin
    if GetLastError = ERROR_NO_TOKEN then
    bSuccess := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hAccessToken) ;
  end;


  if bSuccess then
  begin
    GetMem(ptgGroups, 1024) ;

    bSuccess := GetTokenInformation(hAccessToken, TokenGroups, ptgGroups, 1024, dwInfoBufferSize) ;

    CloseHandle(hAccessToken) ;

    if bSuccess then
    begin
      AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, psidAdministrators) ;

      for g := 0 to ptgGroups^.GroupCount - 1 do
        if EqualSid(psidAdministrators, ptgGroups^.Groups[g].Sid) then
        begin
          Result := True;
          Break;
        end;

      FreeSid(psidAdministrators) ;
    end;

    FreeMem(ptgGroups) ;
  end;
end;
{$ELSE}
function TUpdateChecker.IsSystemAdmin: Boolean;
begin
  Result := False;
end;

{$ENDIF}

procedure TUpdateChecker.SockStatus(Sender: TObject; Reason: THookSocketReason;
  const Value: String);
var
  Num: Integer;
begin
  if (Reason = HR_SocketCreate) then begin
    FormDownloadProgress.ProgressBar1.Position := Num;
    Application.ProcessMessages;
  end;
  if (Reason = HR_ReadCount) and TryStrToInt(Value, Num) then begin
    if HTTPSender.DownloadSize <> 0 then
      FormDownloadProgress.ProgressBar1.Max := HTTPSender.DownloadSize;
    with FormDownloadProgress.ProgressBar1 do begin
      Position := Position + Num;
      FormDownloadProgress.LabelProgress.Caption :=
        IntToStr(Position) + ' / ' + IntToStr(Max);
    end;
    Application.ProcessMessages;
  end;
end;

end.
