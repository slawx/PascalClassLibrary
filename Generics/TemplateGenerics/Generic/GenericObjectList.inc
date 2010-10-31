{$IFDEF INTERFACE}

  TListIndex = TObjectListIndex;
  TListItem = TObjectListItem;
  {$DEFINE INTERFACE}
  {$INCLUDE 'GenericList.inc'}

  // TGObjectList<TObjectListIndex, TObjectListItem> = class(TGList)
  TGObjectList = class(TGList)
  private
    procedure Put(Index: TListIndex; const AValue: TListItem); override;
  public
    OwnsObjects: Boolean;
    procedure Delete(Index: TListIndex); override;
    procedure Clear; override;
    constructor Create;
    destructor Destroy; override;
  end;
{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{$DEFINE IMPLEMENTATION}
{$INCLUDE 'GenericList.inc'}

{ TGObjectList }

procedure TGObjectList.Put(Index: TObjectListIndex; const AValue: TObjectListItem);
begin
  if OwnsObjects then FItems[Index].Free;
  inherited Put(Index, AValue);
end;

procedure TGObjectList.Delete(Index: TObjectListIndex);
begin
  if OwnsObjects then FItems[Index].Free;
  inherited Delete(Index);
end;

procedure TGObjectList.Clear;
var
  I: TObjectListIndex;
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

constructor TGObjectList.Create;
begin
  inherited;
  OwnsObjects := True;
end;

destructor TGObjectList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{$ENDIF}
