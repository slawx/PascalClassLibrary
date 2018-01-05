unit GenericFileList;

{$mode delphi}

interface

type
  TGFileList<T> = class(TGList<T>)
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


implementation

constructor TGFileList<T>.Create;
begin
  inherited;
  FHandle := feInvalidHandle;
end;

destructor TGFileList<T>.Destroy;
begin
  Close;
  inherited;
end;

procedure TGFileList<T>.Open;
begin
  If (Mode and fmCreate) > 0 then
    FHandle := FileCreate(FFileName, FMode, 438)
  else
    FHandle := FileOpen(FFileName, FMode);
end;

procedure TGFileList<T>.Close;
begin
  if FHandle <> feInvalidHandle then FileClose(FHandle);
end;

procedure TGFileList<T>.SetFileName(const Value: string);
begin
  if FFileName = Value then Exit;
  FFileName := Value;
  if FHandle <> feInvalidHandle then begin
    Close;
    Open;
  end;
end;

procedure TGFileList<T>.SetMode(const Value: Word);
begin
  if FMode = Value then Exit;
  FMode := Value;
  if FHandle <> feInvalidHandle then begin
    Close;
    Open;
  end;
end;

function TGFileListTGFileList.GetOpenned: Boolean;
begin
  Result := FHandle <> feInvalidHandle;
end;

end.
