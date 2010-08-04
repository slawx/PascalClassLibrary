unit GenericList;

{$mode delphi}

interface

uses
  Classes, SysUtils, fgl;

type
  //TFPList

  // Generic type inheritance is not supported by compiler
  // TIndexType have to be ordinal type

  { TGenericList }

  generic TGenericList<TIndexType, TItemType> = class
  type public
    TListSortCompare = function(const Item1, Item2: TItemType): Integer;
  var private
    function GetCapacity: TIndexType;
    procedure SetCapacity(const AValue: TIndexType);
  private
    FItems: array of TItemType;
    FCount: TIndexType;
    function Get(Index: TIndexType): TItemType;
    function GetCount: TIndexType;
    procedure Put(Index: TIndexType; const AValue: TItemType);
    procedure SetCount(const AValue: TIndexType);
    procedure QuickSort(L, R : TIndexType; Compare: TListSortCompare);
    property Capacity: TIndexType read GetCapacity write SetCapacity;
  public
    function Add(Item: TItemType): TIndexType;
    procedure Clear;
    procedure Delete(Index: TIndexType);
    procedure Exchange(Index1, Index2: TIndexType);
    procedure Expand;
    function Extract(Item: TItemType): TItemType;
    function First: TItemType;
    function IndexOf(Item: TItemType): TIndexType;
    procedure Insert(Index: TIndexType; Item: TItemType);
    function Last: TItemType;
    procedure Move(CurIndex, NewIndex: TIndexType);
    function Remove(Item: TItemType): TIndexType;
    procedure Sort(Compare: TListSortCompare);
    property Items[Index: TIndexType]: TItemType read Get write Put; default;
    property Count: TIndexType read GetCount write SetCount;
  end;

  // TPointerList
  TPointerList = specialize TGenericList<Integer, Pointer>;
  TBigPointerList = specialize TGenericList<Int64, Pointer>;
  TSmallPointerList = specialize TGenericList<SmallInt, Pointer>;
  TTinyPointerList = specialize TGenericList<ShortInt, Pointer>;

  // TObjectList
  TObjectList = specialize TGenericList<Integer, TObject>;
  TBigObjectList = specialize TGenericList<Int64, TObject>;
  TSmallObjectList = specialize TGenericList<SmallInt, TObject>;
  TTinyObjectList = specialize TGenericList<ShortInt, TObject>;

  // TIntegerList
  TIntegerList = specialize TGenericList<Integer, Integer>;
  TBigIntegerList = specialize TGenericList<Int64, Integer>;
  TSmallIntegerList = specialize TGenericList<SmallInt, Integer>;
  TTinyIntegerList = specialize TGenericList<ShortInt, Integer>;

  // TStringList
  TStringList = specialize TGenericList<Integer, string>;
  TBigStringList = specialize TGenericList<Int64, string>;
  TSmallStringList = specialize TGenericList<SmallInt, string>;
  TTinyStringList = specialize TGenericList<ShortInt, string>;

  // TDoubleList
  TDoubleList = specialize TGenericList<Integer, Double>;
  TBigDoubleList = specialize TGenericList<Int64, Double>;
  TSmallDoubleList = specialize TGenericList<SmallInt, Double>;
  TTinyDoubleList = specialize TGenericList<ShortInt, Double>;

implementation

uses
  RtlConsts;

{ TGenericList }

function TGenericList.GetCapacity: TIndexType;
begin
  Result := Length(FItems);
end;

procedure TGenericList.SetCapacity(const AValue: TIndexType);
begin
  SetLength(FItems, AValue);
end;

function TGenericList.Get(Index: TIndexType): TItemType;
begin
  Result := FItems[Index];
end;

function TGenericList.GetCount: TIndexType;
begin
  Result := FCount;
end;

procedure TGenericList.Put(Index: TIndexType; const AValue: TItemType);
begin
  FItems[Index] := AValue;
end;

procedure TGenericList.SetCount(const AValue: TIndexType);
begin
  SetLength(FItems, AValue);
  FCount := AValue;
end;

procedure TGenericList.QuickSort(L, R: TIndexType; Compare: TListSortCompare);
var
  I, J: TIndexType;
  P, Q: TItemType;
begin
 repeat
   I := L;
   J := R;
   P := FItems[ (L + R) div 2 ];
   repeat
     while Compare(P, FItems[i]) > 0 do
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

procedure TGenericList.Expand;
var
  IncSize: TIndexType;
begin
  if FCount = Capacity then begin
    IncSize := 4;
    if Capacity > 3 then IncSize := IncSize + 4;
    if Capacity > 8 then IncSize := IncSize + 8;
    if Capacity > 127 then IncSize := IncSize + Capacity shr 2;
    Capacity := Capacity + IncSize;
  end;
end;

function TGenericList.Extract(Item: TItemType): TItemType;
var
  I: Integer;
begin
  I := IndexOf(Item);
  if I >= 0 then begin
    Result := Item;
    Delete(I);
  end else
    raise EListError.CreateFmt(SListIndexError, [0]);
end;

function TGenericList.First: TItemType;
begin
  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    Result := Items[0];
end;

function TGenericList.IndexOf(Item: TItemType): TIndexType;
begin
  Result := 0;
  while (Result < FCount) and (FItems[Result] <> Item) do
    Result := Result + 1;
  if Result = FCount then Result := -1;
end;

procedure TGenericList.Insert(Index: TIndexType; Item: TItemType);
begin
  if (Index < 0) or (Index > FCount ) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  iF FCount = Capacity then Expand;
  if Index < FCount then
    System.Move(FItems[Index], FItems[Index + 1], (FCount - Index) * SizeOf(TItemType));
  FItems[Index] := Item;
  FCount := FCount + 1;
end;

function TGenericList.Last: TItemType;
begin
  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    Result := Items[FCount - 1];
end;

procedure TGenericList.Move(CurIndex, NewIndex: TIndexType);
var
  Temp: TItemType;
begin
  if ((CurIndex < 0) or (CurIndex > Count - 1)) then
    raise EListError.CreateFmt(SListIndexError, [CurIndex]);
  if ((NewIndex < 0) or (NewIndex > Count -1)) then
    raise EListError.CreateFmt(SlistIndexError, [NewIndex]);
  Temp := FItems[CurIndex];
  Delete(CurIndex);
  Insert(NewIndex, Temp);
end;

function TGenericList.Remove(Item: TItemType): TIndexType;
begin
  Result := IndexOf(Item);
  if Result <> -1 then
    Delete(Result);
end;

procedure TGenericList.Sort(Compare: TListSortCompare);
begin
  if FCount > 1 then
    QuickSort(0, FCount - 1, Compare);
end;

function TGenericList.Add(Item: TItemType): TIndexType;
begin
  if FCount = Capacity then
    Self.Expand;
  FItems[FCount] := Item;
  Result := FCount;
  FCount := FCount + 1;
end;

procedure TGenericList.Clear;
begin
  Count := 0;
  Capacity := 0;
end;

procedure TGenericList.Delete(Index: TIndexType);
begin
  if (Index < 0) or (Index >= FCount) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  FCount := FCount - 1;
  System.Move(FItems[Index + 1], FItems[Index], (FCount - Index) * SizeOf(TItemType));
  // Shrink the list if appropriate
  if (Capacity > 256) and (FCount < Capacity shr 2) then
  begin
    Capacity := Capacity shr 1;
  end;
end;

procedure TGenericList.Exchange(Index1, Index2: TIndexType);
var
  Temp: TItemType;
begin
  if ((Index1 >= FCount) or (Index1 < 0)) then
    raise EListError.CreateFmt(SListIndexError, [Index1]);
  if ((Index2 >= FCount) or (Index2 < 0)) then
    raise EListError.CreateFmt(SListIndexError, [Index2]);
  Temp := FItems[Index1];
  FItems[Index1] := FItems[Index2];
  FItems[Index2] := Temp;
end;

end.


