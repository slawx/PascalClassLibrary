unit ULastOpenedList;

{$mode delphi}

interface

uses
  Classes, SysUtils, Registry, URegistry, Menus;

type

  { TLastOpenedList }

  TLastOpenedList = class(TStringList)
  private
    FOnChange: TNotifyEvent;
  public
    MaxCount: Integer;
    ClickAction: TNotifyEvent;
    constructor Create;
    destructor Destroy; override;
    procedure LoadToMenuItem(MenuItem: TMenuItem);
    procedure LoadFromRegistry(Root: HKEY; Key: string);
    procedure SaveToRegistry(Root: HKEY; Key: string);
    procedure AddItem(FileName: string);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TLastOpenedList }

constructor TLastOpenedList.Create;
begin
  inherited;
  MaxCount := 5;
end;

destructor TLastOpenedList.Destroy;
begin
  inherited;
end;

procedure TLastOpenedList.LoadToMenuItem(MenuItem: TMenuItem);
var
  NewMenuItem: TMenuItem;
  I: Integer;
begin
  if Assigned(MenuItem) then begin
    MenuItem.Clear;
    for I := 0 to Count - 1 do begin
      NewMenuItem := TMenuItem.Create(MenuItem);
      NewMenuItem.Caption := Strings[I];
      NewMenuItem.OnClick := ClickAction;
      MenuItem.Add(NewMenuItem);
    end;
  end;
end;

procedure TLastOpenedList.LoadFromRegistry(Root: HKEY; Key: string);
var
  I: Integer;
  Registry: TRegistryEx;
  FileName: string;
begin
  Registry := TRegistryEx.Create;
  with Registry do
  try
    RootKey := Root;
    OpenKey(Key, True);
    Clear;
    I := 0;
    while ValueExists('File' + IntToStr(I)) and (I < MaxCount) do begin
      FileName := UTF8Encode(ReadStringWithDefault('File' + IntToStr(I), ''));
      if Trim(FileName) <> '' then inherited Add(FileName);
      Inc(I);
    end;
    if Assigned(FOnChange) then
      FOnChange(Self);
  finally
    Free;
  end;
end;

procedure TLastOpenedList.SaveToRegistry(Root: HKEY; Key: string);
var
  I: Integer;
  Registry: TRegistryEx;
begin
  Registry := TRegistryEx.Create;
  with Registry do
  try
    RootKey := Root;
    OpenKey(Key, True);
    for I := 0 to Count - 1 do
      WriteString('File' + IntToStr(I), UTF8Decode(Strings[I]));
  finally
    Free;
  end;
end;

procedure TLastOpenedList.AddItem(FileName:string);
begin
  if IndexOf(FileName) <> -1 then Delete(IndexOf(FileName));
  Insert(0, FileName);
  while Count > MaxCount do
    Delete(Count - 1);

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.

