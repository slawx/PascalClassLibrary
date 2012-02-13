unit ULastOpenedList;

{$mode delphi}

interface

uses
  Classes, SysUtils, Registry, URegistry, Menus;

type

  { TLastOpenedList }

  TLastOpenedList = class(TStringList)
  public
    MaxCount: Integer;
    MenuItem: TMenuItem;
    ClickAction: TNotifyEvent;
    constructor Create;
    destructor Destroy; override;
    procedure ReloadMenu;
    procedure LoadFromRegistry(Root: HKEY; Key: string);
    procedure SaveToRegistry(Root: HKEY; Key: string);
    procedure AddItem(FileName: string);
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

procedure TLastOpenedList.ReloadMenu;
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
begin
  Registry := TRegistryEx.Create;
  with Registry do
  try
    RootKey := Root;
    OpenKey(Key + '\LastOpenedFiles', True);
    Clear;
    I := 0;
    while ValueExists('File' + IntToStr(I)) and (I < MaxCount) do begin
      inherited Add(UTF8Encode(ReadStringWithDefault('File' + IntToStr(I), '')));
      Inc(I);
    end;
    ReloadMenu;
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
    OpenKey(Key + '\LastOpenedFiles', True);
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

  ReloadMenu;
end;

end.
