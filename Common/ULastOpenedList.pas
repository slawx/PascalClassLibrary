unit ULastOpenedList;

{$mode delphi}

interface

uses
  Classes, SysUtils, Registry, URegistry, Menus;

type

  { TLastOpenedList }

  TLastOpenedList = class(TComponent)
  private
    FMaxCount: Integer;
    FOnChange: TNotifyEvent;
    procedure SetMaxCount(AValue: Integer);
    procedure LimitMaxCount;
  public
    Items: TStringList;
    ClickAction: TNotifyEvent;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadToMenuItem(MenuItem: TMenuItem);
    procedure LoadFromRegistry(Root: HKEY; Key: string);
    procedure SaveToRegistry(Root: HKEY; Key: string);
    procedure AddItem(FileName: string);
    property MaxCount: Integer read FMaxCount write SetMaxCount;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TLastOpenedList }

procedure TLastOpenedList.SetMaxCount(AValue: Integer);
begin
  if FMaxCount = AValue then Exit;
  FMaxCount := AValue;
  if FMaxCount < 0 then FMaxCount := 0;
  LimitMaxCount;
end;

procedure TLastOpenedList.LimitMaxCount;
begin
  while Items.Count > MaxCount do
    Items.Delete(Items.Count - 1);
end;

constructor TLastOpenedList.Create(AOwner: TComponent);
begin
  inherited;
  Items := TStringList.Create;
  MaxCount := 10;
end;

destructor TLastOpenedList.Destroy;
begin
  Items.Free;
  inherited;
end;

procedure TLastOpenedList.LoadToMenuItem(MenuItem: TMenuItem);
var
  NewMenuItem: TMenuItem;
  I: Integer;
begin
  if Assigned(MenuItem) then begin
    MenuItem.Clear;
    for I := 0 to Items.Count - 1 do begin
      NewMenuItem := TMenuItem.Create(MenuItem);
      NewMenuItem.Caption := Items[I];
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
    Items.Clear;
    I := 0;
    while ValueExists('File' + IntToStr(I)) and (I < MaxCount) do begin
      FileName := UTF8Encode(ReadStringWithDefault('File' + IntToStr(I), ''));
      if Trim(FileName) <> '' then Items.Add(FileName);
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
    for I := 0 to Items.Count - 1 do
      WriteString('File' + IntToStr(I), UTF8Decode(Items[I]));
  finally
    Free;
  end;
end;

procedure TLastOpenedList.AddItem(FileName:string);
begin
  if Items.IndexOf(FileName) <> -1 then Items.Delete(Items.IndexOf(FileName));
  Items.Insert(0, FileName);
  LimitMaxCount;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.

