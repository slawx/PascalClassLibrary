uses
  RtlConsts;

// Used instead of System.Move form because of error: Identifier "System" not found
procedure SystemMove(const Source; var Dest; Count: SizeInt);
begin
  Move(Source, Dest, Count);
end;

{ TGList }

function TGList.GetCapacity: TListIndex;
begin
  Result := Length(FItems);
end;

procedure TGList.SetCapacity(const AValue: TListIndex);
begin
  SetLength(FItems, AValue);
end;

function TGList.Get(Index: TListIndex): TListItem;
begin
  Result := FItems[Index];
end;

function TGList.GetCount: TListIndex;
begin
  Result := FCount;
end;

procedure TGList.Put(Index: TListIndex; const AValue: TListItem);
begin
  FItems[Index] := AValue;
end;

procedure TGList.SetCount(const AValue: TListIndex);
begin
  SetLength(FItems, AValue);
  FCount := AValue;
end;

procedure TGList.QuickSort(L, R: TListIndex; Compare: TGListSortCompare);
var
  I, J: TListIndex;
  P, Q: TListItem;
begin
 repeat
   I := L;
   J := R;
   P := FItems[ (L + R) div 2 ];
   repeat
     while Compare(P, FItems[I]) > 0 do
       I := I + 1;
     while Compare(P, FItems[J]) < 0 do
       J := J - 1;
     If I <= J then
     begin
       Q := FItems[I];
       FItems[I] := FItems[J];
       FItems[J] := Q;
       I := I + 1;
       J := J - 1;
     end;
   until I > J;
   if L < J then
     QuickSort(L, J, Compare);
   L := I;
  until I >= R;
end;

procedure TGList.Assign(List: TGList);
var
  I: Integer;
begin
  Count := List.Count;
  I := 0;
  while I < Count do begin
    Items[I] := List[I];
    I := I + 1;
  end;
end;

procedure TGList.Expand;
var
  IncSize: TListIndex;
begin
  if FCount = Capacity then begin
    IncSize := 4;
    if Capacity > 3 then IncSize := IncSize + 4;
    if Capacity > 8 then IncSize := IncSize + 8;
    if Capacity > 63 then IncSize := IncSize + Capacity shr 2;
    Capacity := Capacity + IncSize;
  end;
end;

function TGList.Extract(Item: TListItem): TListItem;
var
  I: TListIndex;
begin
  I := IndexOf(Item);
  if I >= 0 then begin
    Result := Item;
    Delete(I);
  end else
    raise EListError.CreateFmt(SListIndexError, [0]);
end;

function TGList.First: TListItem;
begin
  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    Result := Items[0];
end;

function TGList.IndexOf(Item: TListItem): TListIndex;
begin
  Result := 0;
  while (Result < FCount) and CompareMem(Addr(FItems[Result]), Addr(Item), SizeOf(TListItem)) do
    Result := Result + 1;
  if Result = FCount then Result := -1;
end;

procedure TGList.Insert(Index: TListIndex; Item: TListItem);
begin
  if (Index < 0) or (Index > FCount ) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  if FCount = Capacity then Expand;
  if Index < FCount then
    SystemMove(FItems[Index], FItems[Index + 1], (FCount - Index) * SizeOf(TListItem));
  FItems[Index] := Item;
  FCount := FCount + 1;
end;

procedure TGList.InsertList(Index: TListIndex; List: TGList);
var
  I: TListIndex;
begin
  I := 0;
  while (I < List.Count) do begin
    Insert(Index + I, List[I]);
    I := I + 1;
  end;
end;

function TGList.Last: TListItem;
begin
  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    Result := Items[FCount - 1];
end;

procedure TGList.Move(CurIndex, NewIndex: TListIndex);
var
  Temp: TListItem;
begin
  if ((CurIndex < 0) or (CurIndex > Count - 1)) then
    raise EListError.CreateFmt(SListIndexError, [CurIndex]);
  if ((NewIndex < 0) or (NewIndex > Count -1)) then
    raise EListError.CreateFmt(SlistIndexError, [NewIndex]);
  Temp := FItems[CurIndex];
  Delete(CurIndex);
  Insert(NewIndex, Temp);
end;

procedure TGList.MoveItems(CurIndex, NewIndex, Count: TListIndex);
var
  S: Integer;
  D: Integer;
begin
  if CurIndex < NewIndex then begin
    S := CurIndex + Count - 1;
    D := NewIndex + Count - 1;
    while S >= CurIndex do begin
      Move(S, D);
      S := S - 1;
      D := D - 1;
    end;
  end else
  if CurIndex > NewIndex then begin
    S := CurIndex;
    D := NewIndex;
    while S < (CurIndex + Count) do begin
      Move(S, D);
      S := S + 1;
      D := D + 1;
    end;
  end;
end;

function TGList.Remove(Item: TListItem): TListIndex;
begin
  Result := IndexOf(Item);
  if Result <> -1 then
    Delete(Result);
end;

(*function TGList.Equals(Obj: TObject): Boolean;
var
  I: TListIndex;
begin
  Result := Count = (Obj as TGList).Count;
  if Result then begin
    I := 0;
    while I < Count do begin
      if Items[I] <> (Obj as TGList)[I] then begin
        Result := False;
        Break;
      end;
      I := I + 1;
    end;
  end;
end;*)

procedure TGList.Reverse;
var
  I: TListIndex;
begin
  I := 0;
  while I < (Count div 2) do begin
    Exchange(I, Count - 1 - I);
    I := I + 1;
  end;
end;

procedure TGList.Sort(Compare: TGListSortCompare);
begin
  if FCount > 1 then
    QuickSort(0, FCount - 1, Compare);
end;

procedure TGList.AddArray(Values: array of TListItem);
var
  I: TListIndex;
begin
  I := 0;
  while I <= High(Values) do begin
    Add(Values[I]);
    I := I + 1;
  end;
end;

function TGList.Implode(Separator: string; Converter: TGListStringConverter): string;
var
  I: TListIndex;
begin
  Result := '';
  I := 0;
  while I < Count do begin
    Result := Result + Converter(Items[I]);
    if I < (Count - 1) then
      Result := Result + Separator;
    I := I + 1;
  end;
end;

function TGList.Add(Item: TListItem): TListIndex;
begin
  if FCount = Capacity then
    Self.Expand;
  FItems[FCount] := Item;
  Result := FCount;
  FCount := FCount + 1;
end;

procedure TGList.AddList(List: TGList);
var
  I: TListIndex;
begin
  I := 0;
  while I < List.Count do begin
    Add(List[I]);
    I := I + 1;
  end;
end;

procedure TGList.Clear;
begin
  Count := 0;
  Capacity := 0;
end;

procedure TGList.Delete(Index: TListIndex);
begin
  if (Index < 0) or (Index >= FCount) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  FCount := FCount - 1;
  SystemMove(FItems[Index + 1], FItems[Index], (FCount - Index) * SizeOf(TListItem));
  // Shrink the list if appropriate
  if (Capacity > 256) and (FCount < Capacity shr 2) then
  begin
    Capacity := Capacity shr 1;
  end;
end;

procedure TGList.DeleteItems(Index, Count: TListIndex);
var
  I: TListIndex;
begin
  I := Index;
  while I < (Index + Count) do begin
    Delete(I);
    I := I + 1;
  end;
end;

procedure TGList.Exchange(Index1, Index2: TListIndex);
var
  Temp: TListItem;
begin
  if ((Index1 >= FCount) or (Index1 < 0)) then
    raise EListError.CreateFmt(SListIndexError, [Index1]);
  if ((Index2 >= FCount) or (Index2 < 0)) then
    raise EListError.CreateFmt(SListIndexError, [Index2]);
  Temp := FItems[Index1];
  FItems[Index1] := FItems[Index2];
  FItems[Index2] := Temp;
end;