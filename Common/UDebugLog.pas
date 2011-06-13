unit UDebugLog; 

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil;

type
  TDebugLogAddEvent = procedure (Group: string; Text: string) of object;

  TDebugLogItem = class
    Time: TDateTime;
    Group: string;
    Text: string;
  end;

  TNewItemEvent = procedure (NewItem: TDebugLogItem) of object;

  { TDebugLog }

  TDebugLog = class(TComponent)
  private
    FFileName: string;
    FOnNewItem: TNewItemEvent;
  public
    WriteToFileEnable: Boolean;
    Items: TThreadList;
    MaxCount: Integer;
    procedure Add(Group: string; Text: string);
    procedure WriteToFile(Text: string);
    property OnNewItem: TNewItemEvent read FOnNewItem write FOnNewItem;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FileName: string read FFileName write FFileName;
  end;

var
  DebugLog: TDebugLog;

implementation

{ TDebugLog }

procedure TDebugLog.Add(Group: string; Text: string);
var
  I: Integer;
  List: TList;
  NewItem: TDebugLogItem;
begin
  NewItem := TDebugLogItem.Create;
  NewItem.Time := Now;
  NewItem.Group := Group;
  NewItem.Text := Text;

  List := Items.LockList;
  List.Insert(0, NewItem);
  if List.Count > MaxCount then begin
    TDebugLogItem(List.Items[List.Count - 1]).Free;
    List.Delete(List.Count - 1);
  end;
  Items.UnlockList;

  if WriteToFileEnable then begin
    if Group <> '' then Group := Group + '[' + Group + '] ';
    WriteToFile(Group + Text);
  end;
  if Assigned(FOnNewItem) then
    FOnNewItem(NewItem);
end;

procedure TDebugLog.WriteToFile(Text: string);
var
  LogFile: TFileStream;
begin
  try
    if FileExistsUTF8(FileName) then LogFile := TFileStream.Create(FileName, fmOpenWrite)
      else LogFile := TFileStream.Create(FileName, fmCreate);
    LogFile.Seek(0, soFromEnd);
    Text := FormatDateTime('hh:nn:ss.zzz', Now) + ': ' + Text + LineEnding;
    LogFile.WriteBuffer(Text[1], Length(Text));
  finally
    LogFile.Free;
  end;
end;

constructor TDebugLog.Create(AOwner: TComponent);
begin
  inherited;
  Items := TThreadList.Create;
  MaxCount := 100;
  FileName := 'DebugLog.txt';
  WriteToFileEnable := False;
end;

destructor TDebugLog.Destroy;
var
  List: TList;
  I: Integer;
begin
  List := Items.LockList;
  for I := 0 to List.Count - 1 do
    TDebugLogItem(List[I]).Free;
  Items.Free;
  inherited Destroy;
end;

end.

