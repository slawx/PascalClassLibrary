{$IFDEF INTERFACE}

  TGList = class;

  TGListSortCompare = function(const Item1, Item2: TGListItem): Integer of object;
  TGListToStringConverter = function(Item: TGListItem): string;
  TGListFromStringConverter = function(Text: string): TGListItem;
  TGListItemArray = array of TGListItem;

  // TGList<TGListIndex, TGListItem> = class
  TGList = class
  private
    FItems: array of TGListItem;
    FCount: TGListIndex;
    function Get(Index: TGListIndex): TGListItem;
    function GetCapacity: TGListIndex;
    function GetLast: TGListItem;
    function GetFirst: TGListItem;
    procedure SetCapacity(const AValue: TGListIndex);
    procedure SetCapacityOptimized(const NewCapacity: TGListIndex);
    procedure SetLast(AValue: TGListItem);
    procedure SetFirst(AValue: TGListItem);
    procedure Put(Index: TGListIndex; const AValue: TGListItem); virtual;
    procedure SetCount(const AValue: TGListIndex); virtual;
    procedure QuickSort(L, R : TGListIndex; Compare: TGListSortCompare);
  public
    function Add(Item: TGListItem): TGListIndex;
    procedure AddArray(Values: array of TGListItem);
    procedure AddList(List: TGList);
    procedure Assign(Source: TGList); virtual;
    procedure Clear; virtual;
    procedure Delete(Index: TGListIndex); virtual;
    procedure DeleteItems(Index, Count: TGListIndex);
    function EqualTo(List: TGList): Boolean;
    procedure Exchange(Index1, Index2: TGListIndex);
    procedure Explode(Text, Separator: string; Converter: TGListFromStringConverter; SlicesCount: Integer = -1);
    function Extract(Item: TGListItem): TGListItem;
    property First: TGListItem read GetFirst write SetFirst;
    procedure Fill(Start, Count: TGListIndex; Value: TGListItem);
    function GetArray: TGListItemArray;
    function Implode(Separator: string; Converter: TGListToStringConverter): string;
    function IndexOf(Item: TGListItem; Start: TGListIndex = 0): TGListIndex;
    function IndexOfList(List: TGList; Start: TGListIndex = 0): TGListIndex;
    procedure Insert(Index: TGListIndex; Item: TGListItem);
    procedure InsertList(Index: TGListIndex; List: TGList);
    procedure InsertArray(Index: TGListIndex; Values: array of TGListItem);
    procedure Move(CurIndex, NewIndex: TGListIndex);
    procedure MoveItems(CurIndex, NewIndex, Count: TGListIndex);
    function Remove(Item: TGListItem): TGListIndex;
    procedure Reverse;
    procedure Sort(Compare: TGListSortCompare);
    procedure SetArray(Values: TGListItemArray);
    property Count: TGListIndex read FCount write SetCount;
    property Capacity: TGListIndex read GetCapacity write SetCapacity;
    property Items[Index: TGListIndex]: TGListItem read Get write Put; default;
    property Last: TGListItem read GetLast write SetLast;
  end;
  
{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION_USES}

uses
  RtlConsts;

{$UNDEF IMPLEMENTATION_USES}
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TGList }

function TGList.GetCapacity: TGListIndex;
begin
  Result := Length(FItems);
end;

procedure TGList.SetCapacity(const AValue: TGListIndex);
begin
  if (AValue < FCount) then
    raise EListError.CreateFmt(SListCapacityError, [AValue]);
  SetLength(FItems, AValue);
end;

procedure TGList.SetCapacityOptimized(const NewCapacity: TGListIndex);
var
  IncSize: TGListIndex;
begin
  if NewCapacity > Capacity then begin
    IncSize := NewCapacity - Capacity;
    // Expand
    if FCount = Capacity then begin
      IncSize := 4;
      if Capacity > 3 then IncSize := IncSize + 4;
      if Capacity > 8 then IncSize := IncSize + 8;
      if Capacity > 63 then IncSize := IncSize + Capacity shr 2; // Grow by one quarter
      Capacity := Capacity + IncSize;
    end;
  end else
  if NewCapacity < Capacity then begin
    // Contract
    if (Capacity > 256) and (FCount < Capacity shr 2) then
    begin
      Capacity := Capacity shr 1;
    end;
  end;
end;

function TGList.Get(Index: TGListIndex): TGListItem;
begin
  Result := FItems[Index];
end;

procedure TGList.Put(Index: TGListIndex; const AValue: TGListItem);
begin
  FItems[Index] := AValue;
end;

procedure TGList.SetCount(const AValue: TGListIndex);
begin
  if (AValue < 0) then
    raise EListError.CreateFmt(SListCountError, [AValue]);
  if AValue > Capacity then SetCapacityOptimized(AValue); // Before FCount change
  FCount := AValue;
  if AValue < Capacity then SetCapacityOptimized(AValue); // After FCount change
end;

function TGList.GetArray: TGListItemArray;
var
  I: Integer;
begin
  SetLength(Result, Count);
  I := 0;
  while I < Count do begin
    Result[I] := FItems[I];
    I := I + 1;
  end;
end;

procedure TGList.QuickSort(L, R: TGListIndex; Compare: TGListSortCompare);
var
  I, J: TGListIndex;
  P, Q: TGListItem;
begin
 repeat
   I := L;
   J := R;
   P := FItems[(L + R) div 2];
   repeat
     while Compare(P, FItems[I]) > 0 do
       I := I + 1;
     while Compare(P, FItems[J]) < 0 do
       J := J - 1;
     if I <= J then
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

procedure TGList.Assign(Source: TGList);
var
  I: TGListIndex;
begin
  Count := Source.Count;
  I := 0;
  while I < Count do begin
    Items[I] := Source[I];
    I := I + 1;
  end;
end;

function TGList.Extract(Item: TGListItem): TGListItem;
var
  I: TGListIndex;
begin
  I := IndexOf(Item);
  if I >= 0 then begin
    Result := Item;
    Delete(I);
  end else
    raise EListError.CreateFmt(SListIndexError, [0]);
end;

function TGList.IndexOf(Item: TGListItem; Start: TGListIndex): TGListIndex;
begin
  Result := Start;
  while (Result < FCount) and
  not CompareMem(Addr(FItems[Result]), Addr(Item), SizeOf(TGListItem)) do
    Result := Result + 1;
  if Result = FCount then Result := -1;
end;

procedure TGList.Insert(Index: TGListIndex; Item: TGListItem);
begin
  if (Index < 0) or (Index > FCount ) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  if FCount = Capacity then SetCapacityOptimized(Capacity + 1);
  if Index < FCount then
    System.Move(FItems[Index], FItems[Index + 1], (FCount - Index) * SizeOf(TGListItem));
  FItems[Index] := Item;
  FCount := FCount + 1;
end;

procedure TGList.InsertList(Index: TGListIndex; List: TGList);
var
  I: TGListIndex;
begin
  I := 0;
  while (I < List.Count) do begin
    Insert(Index + I, List[I]);
    I := I + 1;
  end;
end;

function TGList.IndexOfList(List: TGList; Start: TGListIndex): TGListIndex;
var
  I: TGListIndex;
begin
  if List.Count > 0 then begin
    Result := IndexOf(List[0], Start);
    if Result <> -1 then begin
      I := 1;
      while I < List.Count do begin
        if not CompareMem(Addr(FItems[Result + I]), Addr(List.FItems[I]), SizeOf(TGListItem)) then begin
          Result := -1;
          Break;
        end;
        I := I + 1;
      end;
    end;
  end else Result := -1;
end;

function TGList.GetLast: TGListItem;
begin
  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    Result := Items[FCount - 1];
end;

procedure TGList.SetLast(AValue: TGListItem);
begin
  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    Items[FCount - 1] := AValue;
end;

function TGList.GetFirst: TGListItem;
begin
  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    Result := Items[0];
end;

procedure TGList.SetFirst(AValue: TGListItem);
begin
  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    Items[0] := AValue;
end;

procedure TGList.Move(CurIndex, NewIndex: TGListIndex);
var
  Temp: TGListItem;
begin
  if ((CurIndex < 0) or (CurIndex > Count - 1)) then
    raise EListError.CreateFmt(SListIndexError, [CurIndex]);
  if ((NewIndex < 0) or (NewIndex > Count -1)) then
    raise EListError.CreateFmt(SlistIndexError, [NewIndex]);
  Temp := FItems[CurIndex];
  if NewIndex > CurIndex then begin
    System.Move(FItems[CurIndex + 1], FItems[CurIndex], (NewIndex - CurIndex) * SizeOf(TGListItem));
  end else
  if NewIndex < CurIndex then begin
    System.Move(FItems[NewIndex], FItems[NewIndex + 1], (CurIndex - NewIndex) * SizeOf(TGListItem));
  end;
  FItems[NewIndex] := Temp;
  //Delete(CurIndex);
  //Insert(NewIndex, Temp);
end;

procedure TGList.MoveItems(CurIndex, NewIndex, Count: TGListIndex);
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

function TGList.Remove(Item: TGListItem): TGListIndex;
begin
  Result := IndexOf(Item);
  if Result <> -1 then
    Delete(Result);
end;

function TGList.EqualTo(List: TGList): Boolean;
var
  I: TGListIndex;
begin
  Result := Count = List.Count;
  if Result then begin
    I := 0;
    while I < Count do begin
      if not CompareMem(Addr(FItems[I]), Addr(List.FItems[I]), SizeOf(TGListItem)) then begin
        Result := False;
        Break;
      end;
      I := I + 1;
    end;
  end;
end;

procedure TGList.Reverse;
var
  I: TGListIndex;
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

procedure TGList.AddArray(Values: array of TGListItem);
var
  I: TGListIndex;
begin
  I := 0;
  while I <= High(Values) do begin
    Add(Values[I]);
    I := I + 1;
  end;
end;

procedure TGList.SetArray(Values: TGListItemArray);
var
  I: TGListIndex;
begin
  Clear;
  I := 0;
  while I <= High(Values) do begin
    Add(Values[I]);
    I := I + 1;
  end;
end;

procedure TGList.InsertArray(Index: TGListIndex; Values: array of TGListItem);
var
  I: TGListIndex;
begin
  I := 0;
  while I <= High(Values) do begin
    Insert(Index + I, Values[I]);
    I := I + 1;
  end;
end;

function TGList.Implode(Separator: string; Converter: TGListToStringConverter): string;
var
  I: TGListIndex;
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

procedure TGList.Explode(Text, Separator: string; Converter: TGListFromStringConverter; SlicesCount: Integer = -1);
begin
  Clear;
  while (Pos(Separator, Text) > 0) and
  ((Count < (SlicesCount - 1)) or (SlicesCount = -1)) do begin
    Add(Converter(Copy(Text, 1, Pos(Separator, Text) - 1)));
    System.Delete(Text, 1, Pos(Separator, Text) + Length(Separator) - 1);
  end;
  Add(Converter(Text));
end;

function TGList.Add(Item: TGListItem): TGListIndex;
begin
  Count := Count + 1;
  Result := FCount - 1;
  FItems[Result] := Item;
end;

procedure TGList.AddList(List: TGList);
var
  I: TGListIndex;
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

procedure TGList.Delete(Index: TGListIndex);
begin
  if (Index < 0) or (Index >= FCount) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  FCount := FCount - 1;
  System.Move(FItems[Index + 1], FItems[Index], (FCount - Index) * SizeOf(TGListItem));
  SetCapacityOptimized(Capacity - 1);
end;

procedure TGList.DeleteItems(Index, Count: TGListIndex);
var
  I: TGListIndex;
begin
  I := Index;
  while I < (Index + Count) do begin
    Delete(Index);
    I := I + 1;
  end;
end;

procedure TGList.Fill(Start, Count: TGListIndex; Value: TGListItem);
var
  I: TGListIndex;
begin
  I := Start;
  while I < Count do begin
    Items[I] := Value;
    I := I + 1;
  end;
end;

procedure TGList.Exchange(Index1, Index2: TGListIndex);
var
  Temp: TGListItem;
begin
  if ((Index1 >= FCount) or (Index1 < 0)) then
    raise EListError.CreateFmt(SListIndexError, [Index1]);
  if ((Index2 >= FCount) or (Index2 < 0)) then
    raise EListError.CreateFmt(SListIndexError, [Index2]);
  Temp := FItems[Index1];
  FItems[Index1] := FItems[Index2];
  FItems[Index2] := Temp;
end;

{$UNDEF IMPLEMENTATION}
{$ENDIF}
