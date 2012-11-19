{$IFDEF INTERFACE}

{$DEFINE TGListIndex := TGFileListIndex}
{$DEFINE TGListItem := TGFileListItem}
{$DEFINE TGList := TGFileListList}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

  // TGFileList<TGFileListIndex, TGFileListItem>
  TGFileList = class(TGList)
  private
    FFileName: string;
    FMode: Word;
    FHandle: THandle;
    procedure SetFileName(const Value: string);
    procedure SetMode(const Value: Word);
    function GetOpenned: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    property FileName: string read FFileName write SetFileName;
    property Mode: Word read FMode write SetMode;
    property Openned: Boolean read GetOpenned;
  end;

{$UNDEF INTERFACE}
{$ENDIF}


{$IFDEF IMPLEMENTATION}

{$DEFINE TGListIndex := TGFileListIndex}
{$DEFINE TGListItem := TGFileListItem}
{$DEFINE TGList := TGFileListList}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

constructor TGFileList.Create;
begin
  inherited;
  FHandle := feInvalidHandle;
end;

destructor TGFileList.Destroy;
begin
  Close;
  inherited;
end;

procedure TGFileList.Open;
begin
  If (Mode and fmCreate) > 0 then
    FHandle := FileCreate(FFileName, FMode, 438)
  else
    FHandle := FileOpen(FFileName, FMode);
end;

procedure TGFileList.Close;
begin
  if FHandle <> feInvalidHandle then FileClose(FHandle);
end;

procedure TGFileList.SetFileName(const Value: string);
begin
  if FFileName = Value then Exit;
  FFileName := Value;
  if FHandle <> feInvalidHandle then begin
    Close;
    Open;
  end;
end;

procedure TGFileList.SetMode(const Value: Word);
begin
  if FMode = Value then Exit;
  FMode := Value;
  if FHandle <> feInvalidHandle then begin
    Close;
    Open;
  end;
end;

function TGFileList.GetOpenned: Boolean;
begin
  Result := FHandle <> feInvalidHandle;
end;

{$UNDEF IMPLEMENTATION}
{$ENDIF}

