unit UUpdateChecker;

{$mode delphi}{$H+}

interface

uses
  {$IFDEF Windows}Windows, ShellApi, {$ENDIF}Forms, Classes, SysUtils,
  httpsend, DOM, XMLWrite, XMLRead, UXMLUtils,
  FileUtil, Dialogs, Process, Blcksock, UFormDownloadProgress, Controls;

type
  TVersionInfo = record
    Id: Integer;
    Version: string;
    SourceURL: string;
    ReleaseTime: TDateTime;
    ReleaseNotes: string;
  end;

  TVersionInfoItem = (viiId, viiVersion, viiSourceURL, viiReleaseTime,
    viiReleaseNotes);
  TVersionInfoItems = set of TVersionInfoItem;

  { TUpdateChecker }

  TUpdateChecker = class(TComponent)
  private
    FBranchId: Integer;
    FShowReleaseNotes: Boolean;
    FVersionInfo: TVersionInfo;
    HTTPSender: THTTPSend;
    FOnTerminate: TNotifyEvent;
    FVersionInfoURL: string;
    InstallerFileName: string;
    function DownloadHTTP(URL, TargetFile: string): Boolean;
    function IsSystemAdmin: Boolean;
    procedure SockStatus(Sender: TObject; Reason: THookSocketReason;
      const Value: String);
    function StripTags(XMLText: string): string;
    function GetFile(URI: string; Content: TMemoryStream): Boolean;
  public
    FormDownloadProgress: TFormDownloadProgress;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadVersionInfo(Items: TVersionInfoItems = []): Boolean;
    { Download source file using HTTP protocol and save it to temp folder }
    procedure Download;
    procedure Install;
    procedure Check(CurrentReleaseDate: TDateTime);
    property VersionInfo: TVersionInfo read FVersionInfo write FVersionInfo;
  published
    property VersionInfoURL: string read FVersionInfoURL write FVersionInfoURL;
    property BranchId: Integer read FBranchId write FBranchId;
    property ShowReleaseNotes: Boolean read FShowReleaseNotes write FShowReleaseNotes;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
  end;

procedure Register;

resourcestring
  SWrongFileFormat = 'Wrong file format';
  SCantExecuteFile = 'Can''t execute installer "%s"';
  SDownloadProgress = 'Download progress';
  SFile = 'File:';
  SProgress = 'Progress:';
  SYouHaveLatestVersion = 'You have latest version';
  SNewVersionAvailable = 'New version available: %s. Do you want to download and install it now?';
  SErrorCheckingNewVersion = 'New version check failed.';
  SCheckUpdates = 'Check updates';
  SChangesInNewVersion = 'Changes in new version:';
  SWhatsNew = 'What''s new?';
  SYes = 'Update';
  SNo = 'Later';

implementation

uses
  UFormNewVersionOffer;

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

function RunAsAdmin(const Handle: Hwnd; const Path, Params: string): Boolean;
var
  sei: TShellExecuteInfoA;
begin
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize := SizeOf(sei);
  sei.Wnd := Handle;
  sei.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  sei.lpVerb := 'runas';
  sei.lpFile := PAnsiChar(Path);
  sei.lpParameters := PAnsiChar(Params);
  sei.nShow := SW_SHOWNORMAL;
  Result := ShellExecuteExA(@sei);
end;

{ TUpdateChecker }

function TUpdateChecker.LoadVersionInfo(Items: TVersionInfoItems = []): Boolean;
var
  URL: string;
  XmlDocument: TXMLDocument;
  Node1: TDOMNode;
  Node2: TDOMNode;
  Node3: TDOMNode;
  Content: TMemoryStream;
begin
  Result := False;
  FVersionInfo.Version := '';
  FVersionInfo.Id := 0;
  FVersionInfo.SourceURL := '';
  URL := VersionInfoURL;
  if Pos('://', VersionInfoURL) > 0 then begin
    URL := URL + '?BranchId=' + IntToStr(BranchId) +
    '&Limit=1';
    if viiVersion in Items then URL := URL + '&Version';
    if viiReleaseNotes in Items then URL := URL + '&ReleaseNotes';
    if viiReleaseTime in Items then URL := URL + '&ReleaseTime';
    if viiSourceURL in Items then URL := URL + '&SourceURL';
    if viiId in Items then URL := URL + '&Id';
  end;
  try
    Content := TMemoryStream.Create;
    if GetFile(URL, Content) then begin
      try
      ReadXMLFile(XmlDocument, Content);
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
          Node3 := Node2.FindNode('ReleaseNotes');
          if Assigned(Node3) then
            FVersionInfo.ReleaseNotes := UTF8Encode(string(Node3.TextContent));
          Node2 := Node2.NextSibling;
        end;
      end;
      Result := True;
      finally
        XmlDocument.Free;
      end;
    end;
  finally
    Content.Free;
  end;
end;

procedure TUpdateChecker.Download;
begin
  if FVersionInfo.SourceURL <> '' then begin
    if FileExistsUTF8(FVersionInfo.SourceURL) then
      InstallerFileName := FVersionInfo.SourceURL
    else begin
      InstallerFileName := UTF8Encode(GetTempDir) + DirectorySeparator +
        ExtractFileName(FVersionInfo.SourceURL);
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
end;

procedure TUpdateChecker.Install;
var
  Process: TProcess;
begin
  if FileExistsUTF8(InstallerFileName) then begin
    if not IsSystemAdmin then begin
      RunAsAdmin(FormNewVersionOffer.Handle, InstallerFileName, '');
      (*try
        Process := TProcess.Create(nil);
        Process.CommandLine := 'runas ' + InstallerFileName;
        Process.Options := Process.Options + [];
        //Process.Execute;
      finally
        Process.Free;
      end*)
    end else
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

procedure TUpdateChecker.Check(CurrentReleaseDate: TDateTime);
begin
  if LoadVersionInfo([viiReleaseTime]) then begin
    if VersionInfo.ReleaseTime > CurrentReleaseDate then begin
      LoadVersionInfo([viiVersion, viiReleaseTime, viiReleaseNotes, viiSourceURL]);
      try
        FormNewVersionOffer := TFormNewVersionOffer.Create(nil);
        FormNewVersionOffer.LabelQuestion.Caption := Format(SNewVersionAvailable, [VersionInfo.Version]);
        FormNewVersionOffer.MemoReleaseNotes.Lines.Text := Trim(StripTags(VersionInfo.ReleaseNotes));
        if ShowReleaseNotes then FormNewVersionOffer.BitBtnWhatsNew.Click;
        if FormNewVersionOffer.ShowModal = mrYes then begin
          Download;
          Install;
        end;
      finally
        FormNewVersionOffer.Free;
      end;
    end else ShowMessage(SYouHaveLatestVersion);
  end else ShowMessage(SErrorCheckingNewVersion);
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
  if (Reason = HR_SocketCreate) and TryStrToInt(Value, Num) then begin
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

function TUpdateChecker.StripTags(XMLText: string): string;
begin
  Result := '';
  while Pos('<', XMLText) > 0 do begin
    Result := Result + Copy(XMLText, 1, Pos('<', XMLText) - 1);
    Delete(XMLText, 1, Pos('<', XMLText));
    Delete(XMLText, 1, Pos('>', XMLText));
  end;
  Result := Result + XMLText;
end;

function TUpdateChecker.GetFile(URI: string; Content: TMemoryStream): Boolean;
var
  Buffer: array of Byte;
  FileStream: TFileStream;
begin
  Result := False;
  Content.Size := 0;
  if FileExistsUTF8(URI) then
  try
    FileStream := TFileStream.Create(URI, fmOpenRead);
    Content.CopyFrom(FileStream, FileStream.Size);
    Content.Position := 0;
    Result := True;
  finally
    FileStream.Free;
  end else
  if (Copy(URI, 1, 7) = 'http://') or (Copy(URI, 1, 8) = 'https://') then
  with THTTPSend.Create do
  try
    Clear;
    if HTTPMethod('GET', URI) then begin
      Document.Position := 0;
      Content.CopyFrom(Document, Document.Size);
      Content.Position := 0;
      Result := True;
    end;
  finally
    Free;
  end;
end;

end.

