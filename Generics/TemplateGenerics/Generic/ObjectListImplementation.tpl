{$INCLUDE 'ListImplementation.tpl'}

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
