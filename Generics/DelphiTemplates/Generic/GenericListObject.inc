{$IFDEF INTERFACE}

TGListIndex = TGListObjectIndex;
TGListItem = TGListObjectItem;
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

  // TGListObject<TListObjectIndex, TListObjectItem> = class(TGList)
  TGListObject = class(TGList)
  private
    procedure Put(Index: TGListIndex; const AValue: TGListItem); override;
  public
    OwnsObjects: Boolean;
    function AddNew(NewObject: TGListItem = nil): TGListItem;
    function AddInsertNew(Index: TGListIndex; NewObject: TGListItem = nil): TGListItem;
    procedure Delete(Index: TGListObjectIndex); override;
    procedure Clear; override;
    procedure Assign(Source: TGList); override;
    constructor Create;
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

{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

{ TGListObject }

function TGListObject.AddNew(NewObject: TGListItem = nil): TGListItem;
begin
  if Assigned(NewObject) then Result := NewObject
    else Result := TGListItem.Create;
  Add(Result);
end;

function TGListObject.AddInsertNew(Index: TGListIndex;
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
  if OwnsObjects then FItems[Index].Free;
  inherited Put(Index, AValue);
end;

procedure TGListObject.Delete(Index: TGListObjectIndex);
begin
  if OwnsObjects then FItems[Index].Free;
  inherited Delete(Index);
end;

procedure TGListObject.Clear;
var
  I: TGListObjectIndex;
begin
  if OwnsObjects then begin
    I := 0;
    while I < Count do begin
      FItems[I].Free;
      I := I + 1;
    end;
  end;
  inherited Clear;
end;

constructor TGListObject.Create;
begin
  inherited;
  OwnsObjects := True;
end;

destructor TGListObject.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{$UNDEF IMPLEMENTATION}
{$ENDIF}
