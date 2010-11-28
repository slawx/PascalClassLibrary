unit UPool;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, SpecializedObjectList;

type

  { TThreadedPoolItem }

  TThreadedPoolItem = class
    Used: Boolean;
    Item: TObject;
    constructor Create;
    destructor Destroy; override;
  end;

  { TThreadedPool }

  TThreadedPool = class(TListObject)
  private
    Lock: TCriticalSection;
    ReleaseEvent: TEvent;
    FTotalCount: Integer;
    function IndexOfObjectItem(Item: TObject): Integer;
    procedure SetTotalCount(const AValue: Integer);
  public
    UsedCount: Integer;
    function GetUnused: TThreadedPoolItem;
    function Acquire: TObject;
    procedure Release(Item: TObject);
    constructor Create;
    destructor Destroy; override;
    property TotalCount: Integer read FTotalCount write SetTotalCount;
  end;

implementation

{ TThreadedPool }

function TThreadedPool.GetUnused: TThreadedPoolItem;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and (TThreadedPoolItem(Items[I]).Used) do
    Inc(I);
  if I < Count then Result := TThreadedPoolItem(Items[I])
    else Result := nil;
end;

function TThreadedPool.IndexOfObjectItem(Item: TObject): Integer;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and (TThreadedPoolItem(Items[I]).Item <> Item) do
    Inc(I);
  if I < Count then Result := I
    else Result := -1;
end;

procedure TThreadedPool.SetTotalCount(const AValue: Integer);
var
  I: Integer;
begin
  if AValue > FTotalCount then begin
    for I := FTotalCount to AValue - 1 do
      Add(TThreadedPoolItem.Create);
  end else
  if AValue < FTotalCount then begin
    for I := AValue to FTotalCount - 1 do
      TThreadedPoolItem(Items[I]).Destroy;
  end;
  FTotalCount := AValue;
end;

function TThreadedPool.Acquire: TObject;
var
  Item: TThreadedPoolItem;
begin
  try
    Lock.Acquire;
    while UsedCount = TotalCount do begin
      Lock.Release;
      ReleaseEvent.WaitFor(1000);
      Lock.Acquire;
    end;
    Item := GetUnused;
    Item.Used := True;
    Result := Item.Item;
    Inc(UsedCount);
  finally
    Lock.Release;
  end;
end;

procedure TThreadedPool.Release(Item: TObject);
var
  Index: Integer;
begin
  try
    Lock.Acquire;
    Index := IndexOfObjectItem(Item);
    with TThreadedPoolItem(Items[Index]) do begin
      Used := False;
    end;
    Dec(UsedCount);
    if UsedCount < TotalCount then ReleaseEvent.SetEvent;
  finally
    Lock.Release;
  end;
end;

constructor TThreadedPool.Create;
begin
  inherited;
  Lock := TCriticalSection.Create;
  ReleaseEvent := TEvent.Create(nil, False, False, '');
end;

destructor TThreadedPool.Destroy;
begin
  ReleaseEvent.Destroy;
  Lock.Destroy;
  inherited Destroy;
end;

{ TThreadedPoolItem }

constructor TThreadedPoolItem.Create;
begin
  Item := nil;
end;

destructor TThreadedPoolItem.Destroy;
begin
  if Assigned(Item) then Item.Destroy;
  inherited Destroy;
end;

end.

