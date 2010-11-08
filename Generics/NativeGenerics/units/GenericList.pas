unit GenericList;

{$mode delphi}

interface

uses
  Classes, SysUtils, fgl;

resourcestring
  SNotImplemented = 'Not implemented';

type
  // List could implement many different features which increases
  // time and memory requirements and whole complexity
  // - optimization of memory allocation using Capacity
  // - max item count limitation
  // - ownership of items, object freeing
  // - sorting
  // - single item operations
  // - multiple items operatins, interlist manipulation
  // - action notifications (add, delete, extract, ...)
  // - current position, iteration
  // - custom mass operations (for-each, for-in)

  // Generic type inheritance is not supported by compiler
  // TIndexType have to be ordinal type. compiler doesn't support constraints
  // All lists is prefixed with G (GList) as compiler doesn't support namespaces

  { TGList }

  generic IA<T> = interface
    procedure SetId(Id: T);
  end;

  generic TGList<TIndexType, TItemType> = class
  type public
    TGListSortCompare = function(const Item1, Item2: TItemType): Integer;
    //TGListNotification = (lnAdded, lnExtracted, lnDeleted);
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
    procedure QuickSort(L, R : TIndexType; Compare: TGListSortCompare);
    property Capacity: TIndexType read GetCapacity write SetCapacity;
  public
    // All items
    procedure Reverse;
    procedure Clear;
    procedure Expand;
    procedure Sort(Compare: TGListSortCompare);
    function Implode(Separator: string): string;
    // Many items
    procedure MoveItems(CurIndex, NewIndex, Count: TIndexType);
    // One item
    function Add(Item: TItemType): TIndexType;
    procedure Delete(Index: TIndexType);
    procedure Exchange(Index1, Index2: TIndexType);
    function Extract(Item: TItemType): TItemType;
    function First: TItemType;
    function IndexOf(Item: TItemType): TIndexType;
    procedure Insert(Index: TIndexType; Item: TItemType);
    function Last: TItemType;
    procedure Move(CurIndex, NewIndex: TIndexType);
    function Remove(Item: TItemType): TIndexType;
    property Items[Index: TIndexType]: TItemType read Get write Put; default;
    // List
    function AddList(var Source): TIndexType;
    procedure Assign(var List);
    procedure DeleteItems(Index, Count: TIndexType);
    function Equals(Obj: TObject): Boolean; override;
    function ExtractList(Item: TItemType; Count: TIndexType): TItemType;
    procedure InsertList(Index: TIndexType; var Source);
    // Other
    property Count: TIndexType read GetCount write SetCount;
    // Additional
    procedure SetArray(Values: array of TItemType);
  end;

  // TPointerList
  TPointerGList = specialize TGList<Integer, Pointer>;
  TBigPointerGList = specialize TGList<Int64, Pointer>;
  TSmallPointerGList = specialize TGList<SmallInt, Pointer>;
  TTinyPointerGList = specialize TGList<ShortInt, Pointer>;

  // TObjectList
  TObjectGList = specialize TGList<Integer, TObject>;
  TBigObjectGList = specialize TGList<Int64, TObject>;
  TSmallObjectGList = specialize TGList<SmallInt, TObject>;
  TTinyObjectGList = specialize TGList<ShortInt, TObject>;

  // TIntegerList
  TIntegerGList = specialize TGList<Integer, Integer>;
  TBigIntegerGList = specialize TGList<Int64, Integer>;
  TSmallIntegerGList = specialize TGList<SmallInt, Integer>;
  TTinyIntegerGList = specialize TGList<ShortInt, Integer>;

  // TStringList
  TStringGList = specialize TGList<Integer, string>;
  TBigStringGList = specialize TGList<Int64, string>;
  TSmallStringGList = specialize TGList<SmallInt, string>;
  TTinyStringGList = specialize TGList<ShortInt, string>;

  // TDoubleList
  TDoubleGList = specialize TGList<Integer, Double>;
  TBigDoubleGList = specialize TGList<Int64, Double>;
  TSmallDoubleGList = specialize TGList<SmallInt, Double>;
  TTinyDoubleGList = specialize TGList<ShortInt, Double>;

  procedure SystemMove(const Source; var Dest; Count: SizeInt);

implementation

uses
  RtlConsts;

// Used instead of System.Move form because of error: Identifier "System" not found
procedure SystemMove(const Source; var Dest; Count: SizeInt);
begin
  Move(Source, Dest, Count);
end;

{ TGList }

function TGList.GetCapacity: TIndexType;
begin
  Result := Length(FItems);
end;

procedure TGList.SetCapacity(const AValue: TIndexType);
begin
  SetLength(FItems, AValue);
end;

function TGList.Get(Index: TIndexType): TItemType;
begin
  Result := FItems[Index];
end;

function TGList.GetCount: TIndexType;
begin
  Result := FCount;
end;

procedure TGList.Put(Index: TIndexType; const AValue: TItemType);
begin
  FItems[Index] := AValue;
end;

procedure TGList.SetCount(const AValue: TIndexType);
begin
  SetLength(FItems, AValue);
  FCount := AValue;
end;

procedure TGList.QuickSort(L, R: TIndexType; Compare: TGListSortCompare);
var
  I, J: TIndexType;
  P, Q: TItemType;
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

procedure TGList.Assign(var List);
var
  I: Integer;
begin
  (*Count := List.Count;
  I := 0;
  while I < Count do begin
    Items[I] := List[I];
  end;*)
  raise Exception.Create(SNotImplemented);
end;

procedure TGList.Expand;
var
  IncSize: TIndexType;
begin
  if FCount = Capacity then begin
    IncSize := 4;
    if Capacity > 3 then IncSize := IncSize + 4;
    if Capacity > 8 then IncSize := IncSize + 8;
    if Capacity > 63 then IncSize := IncSize + Capacity shr 2;
    Capacity := Capacity + IncSize;
  end;
end;

function TGList.Extract(Item: TItemType): TItemType;
var
  I: TIndexType;
begin
  I := IndexOf(Item);
  if I >= 0 then begin
    Result := Item;
    Delete(I);
  end else
    raise EListError.CreateFmt(SListIndexError, [0]);
end;

function TGList.ExtractList(Item: TItemType; Count: TIndexType): TItemType;
begin
  raise Exception.Create(SNotImplemented);
end;

function TGList.First: TItemType;
begin
  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    Result := Items[0];
end;

function TGList.IndexOf(Item: TItemType): TIndexType;
begin
  Result := 0;
  while (Result < FCount) and (FItems[Result] <> Item) do
    Result := Result + 1;
  if Result = FCount then Result := -1;
end;

procedure TGList.Insert(Index: TIndexType; Item: TItemType);
begin
  if (Index < 0) or (Index > FCount ) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  if FCount = Capacity then Expand;
  if Index < FCount then
    SystemMove(FItems[Index], FItems[Index + 1], (FCount - Index) * SizeOf(TItemType));
  FItems[Index] := Item;
  FCount := FCount + 1;
end;

procedure TGList.InsertList(Index: TIndexType; var Source);
var
  I: TIndexType;
begin
  (*I := 0;
  while (I < Source.Count) do begin
    Insert(Index + I, Source[I]);
    I := I + 1;
  end;
  *)
  raise Exception.Create(SNotImplemented);
end;

function TGList.Last: TItemType;
begin
  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    Result := Items[FCount - 1];
end;

procedure TGList.Move(CurIndex, NewIndex: TIndexType);
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

procedure TGList.MoveItems(CurIndex, NewIndex, Count: TIndexType);
begin
  raise Exception.Create(SNotImplemented);
end;

function TGList.Remove(Item: TItemType): TIndexType;
begin
  Result := IndexOf(Item);
  if Result <> -1 then
    Delete(Result);
end;

function TGList.Equals(Obj: TObject): Boolean;
var
  I: TIndexType;
begin
(*  Result := Count = List.Count;
  if Result then begin
    I := 0;
    while I < Count do
      if Items[I] <> List[I] then Result := False;
  end;
  *)
  // Not implemented
  raise Exception.Create(SNotImplemented);
  Result := False;
end;

procedure TGList.Reverse;
begin
  raise Exception.Create(SNotImplemented);
end;

procedure TGList.Sort(Compare: TGListSortCompare);
begin
  if FCount > 1 then
    QuickSort(0, FCount - 1, Compare);
end;

procedure TGList.SetArray(Values: array of TItemType);
var
  I: TIndexType;
begin
  Clear;
  I := 0;
  while I <= High(Values) do begin
    Add(Values[I]);
    I := I + 1;
  end;
end;

function TGList.Implode(Separator: string): string;
var
  I: TIndexType;
begin
  Result := '';
  I := 0;
  while I < Count do begin
//    Result := Result + string(Items[I]);
    if I < (Count - 1) then
      Result := Result + Separator;
    I := I + 1;
  end;
end;

function TGList.Add(Item: TItemType): TIndexType;
begin
  if FCount = Capacity then
    Self.Expand;
  FItems[FCount] := Item;
  Result := FCount;
  FCount := FCount + 1;
end;

function TGList.AddList(var Source): TIndexType;
var
  I: TIndexType;
begin
(*  I := 0;
  while I < Source.Count do begin
    Add(Source[I]);
    I := I + 1;
  end;
  *)
 // Not implemented
 Result := 0;
 raise Exception.Create(SNotImplemented);
end;

procedure TGList.Clear;
begin
  Count := 0;
  Capacity := 0;
end;

procedure TGList.Delete(Index: TIndexType);
begin
  if (Index < 0) or (Index >= FCount) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  FCount := FCount - 1;
  SystemMove(FItems[Index + 1], FItems[Index], (FCount - Index) * SizeOf(TItemType));
  // Shrink the list if appropriate
  if (Capacity > 256) and (FCount < Capacity shr 2) then
  begin
    Capacity := Capacity shr 1;
  end;
end;

procedure TGList.DeleteItems(Index, Count: TIndexType);
var
  I: TIndexType;
begin
  I := 0;
  while I < Count do begin
    Delete(I);
    I := I + 1;
  end;
end;

procedure TGList.Exchange(Index1, Index2: TIndexType);
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


