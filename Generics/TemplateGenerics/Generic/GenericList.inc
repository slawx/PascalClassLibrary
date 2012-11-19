{$IFDEF INTERFACE}

  // TGList implemented using templates
  // - item operations (Add, Insert, ReplaceArray, Get, Set, IndexOf,
  //   Extract, Delete, Exchange)
  // - item range operations (DeleteItems, InsertItems, ReplaceItems,
  //   Move, Fill)
  // - other TGList operations (AddList, InsertList,
  //   ReplaceList, GetList, IndexOfList)
  // - dynamic array operations (AddArray, InsertArray,
  //   ReplaceArray, GetArray, IndexOfArray)
  // - all items operations (Clear, Reverse, Sort)

  //TGAbstractList = class

  //end;

  // TGList<TGListIndex, TGListItem> = class
  TGList = class//(TGAbstractList)
  public
    type
      PItem = ^TGListItem;
      TSortCompare = function(Item1, Item2: TGListItem): Integer of object;
      TToStringConverter = function(Item: TGListItem): string;
      TFromStringConverter = function(Text: string): TGListItem;
      TItemArray = array of TGListItem;
  private
    FItems: array of TGListItem;
    FCount: TGListIndex;
    FUpdateCount: Integer;
    FOnUpdate: TNotifyEvent;
    function Get(Index: TGListIndex): TGListItem;
    function GetCapacity: TGListIndex;
    function GetLast: TGListItem;
    function GetFirst: TGListItem;
    procedure SetCapacity(const AValue: TGListIndex);
    procedure SetCapacityOptimized(const NewCapacity: TGListIndex);
    procedure SetLast(AValue: TGListItem);
    procedure SetFirst(AValue: TGListItem);
    procedure QuickSort(L, R : TGListIndex; Compare: TSortCompare);
    procedure DoUpdate;
  protected
    procedure Put(Index: TGListIndex; const AValue: TGListItem); virtual;
    procedure SetCount(const AValue: TGListIndex); virtual;
  public
    function CompareMem(P1, P2: Pointer; Length: cardinal): Boolean; inline;
    function Add(Item: TGListItem): TGListIndex;
    procedure AddArray(Values: array of TGListItem);
    procedure AddList(List: TGList);
    procedure AddListPart(List: TGList; ItemIndex, ItemCount: TGListIndex);
    procedure Assign(Source: TGList); virtual;
    constructor Create; virtual;
    procedure Clear; virtual;
    procedure Delete(Index: TGListIndex); virtual;
    procedure DeleteItems(Index, Count: TGListIndex);
    function EqualTo(List: TGList): Boolean;
    procedure Exchange(Index1, Index2: TGListIndex);
    procedure Explode(Text, Separator: string; Converter: TFromStringConverter; SlicesCount: Integer = -1);
    function Extract(Item: TGListItem): TGListItem;
    property First: TGListItem read GetFirst write SetFirst;
    procedure Fill(Start, Count: TGListIndex; Value: TGListItem);
    function GetArray(Index, ACount: TGListIndex): TItemArray;
    procedure GetList(List: TGList; Index, ACount: TGListIndex);
    procedure GetBuffer(Index: TGListIndex; var Buffer; Count: TGListIndex);
    function Implode(Separator: string; Converter: TToStringConverter): string;
    function IndexOf(Item: TGListItem; Start: TGListIndex = 0): TGListIndex; virtual;
    function IndexOfList(List: TGList; Start: TGListIndex = 0): TGListIndex;
    function IndexOfArray(Values: array of TGListItem; Start: TGListIndex = 0): TGListIndex;
    procedure Insert(Index: TGListIndex; Item: TGListItem);
    procedure InsertList(Index: TGListIndex; List: TGList);
    procedure InsertArray(Index: TGListIndex; Values: array of TGListItem);
    procedure InsertCount(Index: TGListIndex; ACount: TGListIndex);
    procedure Move(CurIndex, NewIndex: TGListIndex);
    procedure MoveItems(CurIndex, NewIndex, Count: TGListIndex);
    function Remove(Item: TGListItem): TGListIndex;
    procedure Reverse;
    procedure ReplaceArray(Index: TGListIndex; Values: array of TGListItem);
    procedure ReplaceList(Index: TGListIndex; Source: TGList);
    procedure ReplaceListPart(Index: TGListIndex; Source: TGList;
      SourceIndex, SourceCount: TGListIndex);
    procedure ReplaceBuffer(Index: TGListIndex; var Buffer; Count: TGListIndex);
    procedure Sort(Compare: TSortCompare);
    procedure SetArray(Values: array of TGListItem);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Update;
    property Count: TGListIndex read FCount write SetCount;
    property Capacity: TGListIndex read GetCapacity write SetCapacity;
    property Items[Index: TGListIndex]: TGListItem read Get write Put; default;
    property Last: TGListItem read GetLast write SetLast;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
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

constructor TGList.Create;
begin
  FCount := 0;
  FUpdateCount := 0;
end;

procedure TGList.GetBuffer(Index: TGListIndex; var Buffer; Count: TGListIndex);
var
  P: PItem;
  I: TGListIndex;
begin
  if (Index + Count) > FCount then
    raise EListError.CreateFmt(SListIndexError, [Index + Count]);
  P := PItem(@Buffer);
  I := 0;
  while I < Count do begin
    P^ := Items[Index + I];
    Inc(P, 1);
    I := I + 1;
  end;
end;

procedure TGList.ReplaceBuffer(Index: TGListIndex; var Buffer; Count: TGListIndex);
var
  P: PItem;
  I: TGListIndex;
begin
  if (Index + Count) > FCount then
    raise EListError.CreateFmt(SListIndexError, [Index + Count]);
  P := PItem(@Buffer);
  I := 0;
  while I < Count do begin
    Items[Index + I] := P^;
    Inc(P, 1);
    I := I + 1;
  end;
end;

procedure TGList.ReplaceArray(Index: TGListIndex; Values: array of TGListItem);
var
  I: TGListIndex;
begin
  I := 0;
  while I < Length(Values) do begin
    Items[Index + I] := Values[I];
    I := I + 1;
  end;
  Update;
end;

procedure TGList.ReplaceList(Index: TGListIndex; Source: TGList);
var
  I: TGListIndex;
begin
  I := 0;
  while I < Source.Count do begin
    Items[Index + I] := Source[I];
    I := I + 1;
  end;
  Update;
end;

procedure TGList.ReplaceListPart(Index: TGListIndex; Source: TGList;
  SourceIndex, SourceCount: TGListIndex);
var
  I: TGListIndex;
begin
  I := 0;
  while I < SourceCount do begin
    Items[Index + I] := Source[SourceIndex + I];
    I := I + 1;
  end;
  Update;
end;

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
    if IncSize = 1 then begin
      IncSize := 4;
      if Capacity > 3 then IncSize := IncSize + 4;
      if Capacity > 8 then IncSize := IncSize + 8;
      if Capacity > 63 then IncSize := IncSize + Capacity shr 2; // Grow by one quarter
    end;
    Capacity := Capacity + IncSize;
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
  if (Index < 0) or (Index >= Count) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  Result := FItems[Index];
end;

procedure TGList.Put(Index: TGListIndex; const AValue: TGListItem);
begin
  if (Index < 0) or (Index >= Count) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
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

function TGList.GetArray(Index, ACount: TGListIndex): TItemArray;
var
  I: Integer;
begin
  SetLength(Result, ACount);
  I := 0;
  while I < Count do begin
    Result[I] := FItems[Index + I];
    I := I + 1;
  end;
end;

procedure TGList.GetList(List: TGList; Index, ACount: TGListIndex);
begin
  List.Clear;
  List.AddListPart(Self, Index, ACount);
end;

procedure TGList.QuickSort(L, R: TGListIndex; Compare: TSortCompare);
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
    FItems[I] := Source[I];
    I := I + 1;
  end;
  Update;
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

function TGList.CompareMem(P1, P2: Pointer; Length: cardinal): Boolean;
var
  I: Cardinal;
begin
  Result := True;
  I := 0;
  if (P1) <> (P2) then
    while Result and (I < Length) do
    begin
      Result := PByte(P1)^ = PByte(P2)^;
      Inc(I);
      Inc(pchar(P1));
      Inc(pchar(P2));
    end;
end;

function TGList.IndexOf(Item: TGListItem; Start: TGListIndex): TGListIndex;
begin
  Result := Start;
  while (Result < FCount) and
//  not CompareMem(@FItems[Result], @Item, SizeOf(TGListItem)) do
  not (CompareByte(FItems[Result], Item, SizeOf(TGListItem)) = 0) do
    Result := Result + 1;
  if Result = FCount then Result := -1;
end;

procedure TGList.Insert(Index: TGListIndex; Item: TGListItem);
begin
  if (Index < 0) or (Index > FCount) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  try
    BeginUpdate;
    InsertCount(Index, 1);
    FItems[Index] := Item;
  finally
    EndUpdate;
  end;
end;

procedure TGList.InsertList(Index: TGListIndex; List: TGList);
begin
  if (Index < 0) or (Index > FCount) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  InsertCount(Index, List.Count);
  ReplaceList(Index, List);
end;

procedure TGList.InsertArray(Index: TGListIndex; Values: array of TGListItem);
begin
  if (Index < 0) or (Index > FCount) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  InsertCount(Index, Length(Values));
  ReplaceArray(Index, Values);
end;

procedure TGList.InsertCount(Index: TGListIndex; ACount: TGListIndex);
begin
  if (Index < 0) or (Index > FCount) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  Count := Count + ACount;
  if Index < FCount then
    System.Move(FItems[Index], FItems[Index + ACount], (FCount - ACount - Index) * SizeOf(TGListItem));
  Update;
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

function TGList.IndexOfArray(Values: array of TGListItem; Start: TGListIndex): TGListIndex;
var
  I: TGListIndex;
begin
  if Length(Values) > 0 then begin
    Result := IndexOf(Values[0], Start);
    if Result <> -1 then begin
      I := 1;
      while I < Length(Values) do begin
        if not CompareMem(Addr(FItems[Result + I]), Addr(Values[I]), SizeOf(TGListItem)) then begin
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
    Result := FItems[FCount - 1];
end;

procedure TGList.SetLast(AValue: TGListItem);
begin
  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    FItems[FCount - 1] := AValue;
end;

function TGList.GetFirst: TGListItem;
begin
  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    Result := FItems[0];
end;

procedure TGList.SetFirst(AValue: TGListItem);
begin
  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    FItems[0] := AValue;
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
  Update;
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
  Update;
end;

function TGList.Remove(Item: TGListItem): TGListIndex;
begin
  Result := IndexOf(Item);
  if Result <> -1 then
    Delete(Result)
    else raise Exception.CreateFmt(SItemNotFound, [0]);
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
  Update;
end;

procedure TGList.Sort(Compare: TSortCompare);
begin
  if FCount > 1 then
    QuickSort(0, FCount - 1, Compare);
  Update;
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
  Update;
end;

procedure TGList.SetArray(Values: array of TGListItem);
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

procedure TGList.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TGList.EndUpdate;
begin
  if FUpdateCount > 0 then Dec(FUpdateCount);
  if FUpdateCount = 0 then DoUpdate;
end;

procedure TGList.DoUpdate;
begin
  if Assigned(FOnUpdate) then FOnUpdate(Self);
end;

procedure TGList.Update;
begin
  if FUpdateCount = 0 then DoUpdate;
end;

function TGList.Implode(Separator: string; Converter: TToStringConverter): string;
var
  I: TGListIndex;
begin
  Result := '';
  I := 0;
  while I < Count do begin
    Result := Result + Converter(FItems[I]);
    if I < (Count - 1) then
      Result := Result + Separator;
    I := I + 1;
  end;
end;

procedure TGList.Explode(Text, Separator: string; Converter: TFromStringConverter; SlicesCount: Integer = -1);
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
  Update;
end;

procedure TGList.AddList(List: TGList);
var
  I: TGListIndex;
  J: TGListIndex;
begin
  I := Count;
  J := 0;
  Count := Count + List.Count;
  while I < Count do begin
    Items[I] := List[J];
    I := I + 1;
    J := J + 1;
  end;
  Update;
end;

procedure TGList.AddListPart(List: TGList; ItemIndex, ItemCount: TGListIndex);
var
  I: TGListIndex;
  J: TGListIndex;
begin
  I := Count;
  J := ItemIndex;
  Count := Count + ItemCount;
  while I < Count do begin
    Items[I] := List[J];
    I := I + 1;
    J := J + 1;
  end;
  Update;
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
  Update;
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
  Update;
end;

procedure TGList.Fill(Start, Count: TGListIndex; Value: TGListItem);
var
  I: TGListIndex;
begin
  I := Start;
  while I < Count do begin
    FItems[I] := Value;
    I := I + 1;
  end;
  Update;
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
  Update;
end;

{$UNDEF IMPLEMENTATION}
{$ENDIF}
