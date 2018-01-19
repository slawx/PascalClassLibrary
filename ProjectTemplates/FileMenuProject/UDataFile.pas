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
    FOnModify: TNotifyEvent;
    procedure SetFileName(AValue: string);
    procedure SetModified(AValue: Boolean);
  public
    function GetFileExt: string; virtual;
    procedure LoadFromFile(FileName: string); virtual;
    procedure SaveToFile(FileName: string); virtual;
    constructor Create; virtual;
    property FileName: string read FFileName write SetFileName;
    property Modified: Boolean read FModified write SetModified;
    property OnModify: TNotifyEvent read FOnModify write FOnModify;
  end;

  TDataFiles = class(TObjectList)
  end;


implementation

resourcestring
  SDataFileName = 'File';

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

procedure TDataFile.SetFileName(AValue: string);
begin
  if FFileName = AValue then Exit;
  FFileName := AValue;
end;


end.

