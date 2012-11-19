{$IFDEF INTERFACE}

{$DEFINE TGListIndex := TGListObjectIndex}
{$DEFINE TGListItem := TGListObjectItem}
{$DEFINE TGList := TGListObjectList}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

  // TGListObject<TListObjectIndex, TListObjectItem> = class(TGList)
  TGListObject = class(TGList)
  protected
    procedure Put(Index: TGListIndex; const AValue: TGListItem); override;
    procedure SetCount(const AValue: TGListIndex); override;
  public
    OwnsObjects: Boolean;
    function AddNew(NewObject: TGListItem = nil): TGListItem;
    function InsertNew(Index: TGListIndex; NewObject: TGListItem = nil): TGListItem;
    procedure Delete(Index: TGListObjectIndex); override;
    procedure Assign(Source: TGList); override;
    constructor Create; override;
    destructor Destroy; override;
  end;

{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION_USES}

  {$DEFINE IMPLEMENTATION_USES}
  {$I 'GenericList.inc'}

{$UNDEF IMPLEMENTATION_USES}
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{$DEFINE TGListIndex := TGListObjectIndex}
{$DEFINE TGListItem := TGListObjectItem}
{$DEFINE TGList := TGListObjectList}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

{ TGListObject }

function TGListObject.AddNew(NewObject: TGListItem = nil): TGListItem;
begin
  if Assigned(NewObject) then Result := NewObject
    else Result := TGListItem.Create;
  Add(Result);
end;

function TGListObject.InsertNew(Index: TGListIndex;
  NewObject: TGListItem = nil): TGListItem;
begin
  if Assigned(NewObject) then Result := NewObject
    else Result := TGListItem.Create;
  Insert(Index, Result);
end;

procedure TGListObject.Assign(Source: TGList);
begin
  Clear;
  OwnsObjects := False;
  inherited;
end;

procedure TGListObject.Put(Index: TGListIndex; const AValue: TGListItem);
begin
  if OwnsObjects and (FItems[Index] <> AValue) then FItems[Index].Free;
  inherited Put(Index, AValue);
end;

procedure TGListObject.Delete(Index: TGListObjectIndex);
begin
  if OwnsObjects then FItems[Index].Free;
  inherited Delete(Index);
end;

procedure TGListObject.SetCount(const AValue: TGListIndex);
var
  I: TGListObjectIndex;
begin
  if OwnsObjects then begin
    I := FCount - 1;
    while I >= AValue do begin
      FItems[I].Free;
      I := I - 1;
    end;
  end;
  I := FCount;
  inherited;
  // Nil newly allocated items
  while I < AValue do begin
    FItems[I] := nil;
    I := I + 1;
  end;
end;

constructor TGListObject.Create;
begin
  inherited;
  OwnsObjects := True;
end;

destructor TGListObject.Destroy;
begin
  Clear;
  inherited;
end;

{$UNDEF IMPLEMENTATION}
{$ENDIF}
