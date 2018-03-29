unit UDataFile;

{$mode delphi}

interface

uses
  Classes, SysUtils, Contnrs;

type
  { TDataFile }

  TDataFile = class
  private
    FFileName: string;
    FModified: Boolean;
    FOnDestroy: TNotifyEvent;
    FOnModify: TNotifyEvent;
    procedure SetFileName(AValue: string);
    procedure SetModified(AValue: Boolean);
  public
    function GetFileExt: string; virtual;
    function GetFileDialogFilter: string; virtual;
    procedure LoadFromFile(FileName: string); virtual;
    procedure SaveToFile(FileName: string); virtual;
    constructor Create; virtual;
    destructor Destroy; override;
    property FileName: string read FFileName write SetFileName;
    property Modified: Boolean read FModified write SetModified;
    property OnModify: TNotifyEvent read FOnModify write FOnModify;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  end;

  TDataFileClass = class of TDataFile;

  TDataFiles = class(TObjectList)
  end;


resourcestring
  SAllFiles = 'All files';

implementation

resourcestring
  SDataFileName = 'File';
  SDataFiles = 'Data files';


{ TDataFile }

procedure TDataFile.SetModified(AValue: Boolean);
begin
  if FModified = AValue then Exit;
  FModified := AValue;
end;

function TDataFile.GetFileExt: string;
begin
  Result := '.dat';
end;

function TDataFile.GetFileDialogFilter: string;
begin
  Result := SDataFiles + ' (' + GetFileExt + ')|*' + GetFileExt + '|' +
    SAllFiles + '|*.*';
end;

procedure TDataFile.LoadFromFile(FileName: string);
begin
  FModified := False;
  Self.FileName := FileName;
end;

procedure TDataFile.SaveToFile(FileName: string);
begin
  Self.FileName := FileName;
  FModified := False;
end;

constructor TDataFile.Create;
begin
  FileName := SDataFileName + GetFileExt;
end;

destructor TDataFile.Destroy;
begin
  if Assigned(FOnDestroy) then FOnDestroy(Self);
  inherited Destroy;
end;

procedure TDataFile.SetFileName(AValue: string);
begin
  if FFileName = AValue then Exit;
  FFileName := AValue;
end;


end.

