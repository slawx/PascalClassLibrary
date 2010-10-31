{$IFDEF INTERFACE}

{$DEFINE TGListIndex := TGObjectListIndex}
{$DEFINE TGListItem := TGObjectListItem}
{$DEFINE TGList := TGObjectListList}
{$DEFINE TGListSortCompare := TGObjectListSortCompare}
{$DEFINE TGListStringConverter := TGObjectListStringConverter}
{$DEFINE INTERFACE}
{$INCLUDE 'GenericList.inc'}

  // TGObjectList<TObjectListIndex, TObjectListItem> = class(TGList)
  TGObjectList = class(TGList)
  private
    procedure Put(Index: TGListIndex; const AValue: TGListItem); override;
  public
    OwnsObjects: Boolean;
    procedure Delete(Index: TGObjectListIndex); override;
    procedure Clear; override;
    constructor Create;
    destructor Destroy; override;
  end;

{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION_USES}

  {$DEFINE IMPLEMENTATION_USES}
  {$INCLUDE '..\Generic\GenericList.inc'}

{$UNDEF IMPLEMENTATION_USES}
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{$DEFINE TGListIndex := TGObjectListIndex}
{$DEFINE TGListItem := TGObjectListItem}
{$DEFINE TGList := TGObjectListList}
{$DEFINE TGListSortCompare := TGObjectListSortCompare}
{$DEFINE TGListStringConverter := TGObjectListStringConverter}
{$DEFINE IMPLEMENTATION}
{$INCLUDE 'GenericList.inc'}

{ TGObjectList }

procedure TGObjectList.Put(Index: TGListIndex; const AValue: TGListItem);
begin
  if OwnsObjects then FItems[Index].Free;
  inherited Put(Index, AValue);
end;

procedure TGObjectList.Delete(Index: TGObjectListIndex);
begin
  if OwnsObjects then FItems[Index].Free;
  inherited Delete(Index);
end;

procedure TGObjectList.Clear;
var
  I: TGObjectListIndex;
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

{$UNDEF IMPLEMENTATION}
{$ENDIF}