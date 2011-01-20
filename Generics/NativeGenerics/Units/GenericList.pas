unit GenericList;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes;

type
  TGList<TItem> = class
  private
  type
    TGListIndex = Integer;
    TSortCompare = function(Item1, Item2: TItem): Integer of object;
    TToStringConverter = function(Item: TItem): string;
    TFromStringConverter = function(Text: string): TItem;
    TItemArray = array of TItem;
  var
    FItems: array of TItem;
    FCount: TGListIndex;
    function Get(Index: TGListIndex): TItem;
    function GetCapacity: TGListIndex;
    function GetLast: TItem;
    function GetFirst: TItem;
    procedure SetCapacity(const AValue: TGListIndex);
    procedure SetCapacityOptimized(const NewCapacity: TGListIndex);
    procedure SetLast(AValue: TItem);
    procedure SetFirst(AValue: TItem);
    procedure Put(Index: TGListIndex; const AValue: TItem); virtual;
    procedure SetCount(const AValue: TGListIndex); virtual;
    procedure QuickSort(L, R : TGListIndex; Compare: TSortCompare);
  public
    function Add(Item: TItem): TGListIndex;
    procedure AddArray(Values: array of TItem);
    procedure AddList(List: TGList);
    procedure Assign(Source: TGList); virtual;
    procedure Clear; virtual;
    procedure Delete(Index: TGListIndex); virtual;
    procedure DeleteItems(Index, Count: TGListIndex);
    function EqualTo(List: TGList): Boolean;
    procedure Exchange(Index1, Index2: TGListIndex);
    procedure Explode(Text, Separator: string; Converter: TFromStringConverter; SlicesCount: Integer = -1);
    function Extract(Item: TItem): TItem;
    property First: TItem read GetFirst write SetFirst;
    procedure Fill(Start, Count: TGListIndex; Value: TItem);
    function GetArray: TItemArray;
    function Implode(Separator: string; Converter: TToStringConverter): string;
    function IndexOf(Item: TItem; Start: TGListIndex = 0): TGListIndex;
    function IndexOfList(List: TGList; Start: TGListIndex = 0): TGListIndex;
    procedure Insert(Index: TGListIndex; Item: TItem);
    procedure InsertList(Index: TGListIndex; List: TGList);
    procedure InsertArray(Index: TGListIndex; Values: array of TItem);
    procedure Move(CurIndex, NewIndex: TGListIndex);
    procedure MoveItems(CurIndex, NewIndex, Count: TGListIndex);
    function Remove(Item: TItem): TGListIndex;
    procedure Reverse;
    procedure Sort(Compare: TSortCompare);
    procedure SetArray(Values: TItemArray);
    property Count: TGListIndex read FCount write SetCount;
    property Capacity: TGListIndex read GetCapacity write SetCapacity;
    property Items[Index: TGListIndex]: TItem read Get write Put; default;
    property Last: TItem read GetLast write SetLast;
  end;

  TGListObject<TItem> = class(TGList<TItem>)
  private
    procedure Put(Index: Integer; const AValue: TItem); override;
  public
    OwnsObjects: Boolean;
    procedure Delete(Index: Integer); override;
    procedure Clear; override;
    procedure Assign(Source: TGList<TItem>); override;
    constructor Create;
    destructor Destroy; override;
  end;

  TGListString<TItem> = class(TGList<TItem>)
  private
  public
    procedure Delete(Index: Integer); override;
    procedure Clear; override;
    procedure Assign(Source: TGList<TItem>); override;
    constructor Create;
    destructor Destroy; override;
  end;


implementation

uses
  RtlConsts;

{ TGList }

function TGList<TItem>.GetCapacity: TGListIndex;
begin
  Result := Length(FItems);
end;

procedure TGList<TItem>.SetCapacity(const AValue: TGListIndex);
begin
  if (AValue < FCount) then
    raise EListError.CreateFmt(SListCapacityError, [AValue]);
  SetLength(FItems, AValue);
end;

procedure TGList<TItem>.SetCapacityOptimized(const NewCapacity: TGListIndex);
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

function TGList<TItem>.Get(Index: TGListIndex): TItem;
begin
  if (Index < 0) or (Index >= Count) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  Result := FItems[Index];
end;

procedure TGList<TItem>.Put(Index: TGListIndex; const AValue: TItem);
begin
  if (Index < 0) or (Index >= Count) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  FItems[Index] := AValue;
end;

procedure TGList<TItem>.SetCount(const AValue: TGListIndex);
begin
  if (AValue < 0) then
    raise EListError.CreateFmt(SListCountError, [AValue]);
  if AValue > Capacity then SetCapacityOptimized(AValue); // Before FCount change
  FCount := AValue;
  if AValue < Capacity then SetCapacityOptimized(AValue); // After FCount change
end;

function TGList<TItem>.GetArray: TItemArray;
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

procedure TGList<TItem>.QuickSort(L, R: TGListIndex; Compare: TSortCompare);
var
  I, J: TGListIndex;
  P, Q: TItem;
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

procedure TGList<TItem>.Assign(Source: TGList);
var
  I: TGListIndex;
begin
  Count := Source.Count;
  I := 0;
  while I < Count do begin
    FItems[I] := Source[I];
    I := I + 1;
  end;
end;

function TGList<TItem>.Extract(Item: TItem): TItem;
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

function TGList<TItem>.IndexOf(Item: TItem; Start: TGListIndex): TGListIndex;
begin
  Result := Start;
  while (Result < FCount) and
  not CompareMem(Addr(FItems[Result]), Addr(Item), SizeOf(TItem)) do
    Result := Result + 1;
  if Result = FCount then Result := -1;
end;

procedure TGList<TItem>.Insert(Index: TGListIndex; Item: TItem);
begin
  if (Index < 0) or (Index > FCount ) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  if FCount = Capacity then SetCapacityOptimized(Capacity + 1);
  if Index < FCount then
    System.Move(FItems[Index], FItems[Index + 1], (FCount - Index) * SizeOf(TItem));
  FItems[Index] := Item;
  FCount := FCount + 1;
end;

procedure TGList<TItem>.InsertList(Index: TGListIndex; List: TGList);
var
  I: TGListIndex;
begin
  I := 0;
  while (I < List.Count) do begin
    Insert(Index + I, List[I]);
    I := I + 1;
  end;
end;

function TGList<TItem>.IndexOfList(List: TGList; Start: TGListIndex): TGListIndex;
var
  I: TGListIndex;
begin
  if List.Count > 0 then begin
    Result := IndexOf(List[0], Start);
    if Result <> -1 then begin
      I := 1;
      while I < List.Count do begin
        if not CompareMem(Addr(FItems[Result + I]), Addr(List.FItems[I]), SizeOf(TItem)) then begin
          Result := -1;
          Break;
        end;
        I := I + 1;
      end;
    end;
  end else Result := -1;
end;

function TGList<TItem>.GetLast: TItem;
begin
  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    Result := FItems[FCount - 1];
end;

procedure TGList<TItem>.SetLast(AValue: TItem);
begin
  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    FItems[FCount - 1] := AValue;
end;

function TGList<TItem>.GetFirst: TItem;
begin
  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    Result := FItems[0];
end;

procedure TGList<TItem>.SetFirst(AValue: TItem);
begin
  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    FItems[0] := AValue;
end;

procedure TGList<TItem>.Move(CurIndex, NewIndex: TGListIndex);
var
  Temp: TItem;
begin
  if ((CurIndex < 0) or (CurIndex > Count - 1)) then
    raise EListError.CreateFmt(SListIndexError, [CurIndex]);
  if ((NewIndex < 0) or (NewIndex > Count -1)) then
    raise EListError.CreateFmt(SlistIndexError, [NewIndex]);
  Temp := FItems[CurIndex];
  if NewIndex > CurIndex then begin
    System.Move(FItems[CurIndex + 1], FItems[CurIndex], (NewIndex - CurIndex) * SizeOf(TItem));
  end else
  if NewIndex < CurIndex then begin
    System.Move(FItems[NewIndex], FItems[NewIndex + 1], (CurIndex - NewIndex) * SizeOf(TItem));
  end;
  FItems[NewIndex] := Temp;
  //Delete(CurIndex);
  //Insert(NewIndex, Temp);
end;

procedure TGList<TItem>.MoveItems(CurIndex, NewIndex, Count: TGListIndex);
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

function TGList<TItem>.Remove(Item: TItem): TGListIndex;
begin
  Result := IndexOf(Item);
  if Result <> -1 then
    Delete(Result);
end;

function TGList<TItem>.EqualTo(List: TGList): Boolean;
var
  I: TGListIndex;
begin
  Result := Count = List.Count;
  if Result then begin
    I := 0;
    while I < Count do begin
      if not CompareMem(Addr(FItems[I]), Addr(List.FItems[I]), SizeOf(TItem)) then begin
        Result := False;
        Break;
      end;
      I := I + 1;
    end;
  end;
end;

procedure TGList<TItem>.Reverse;
var
  I: TGListIndex;
begin
  I := 0;
  while I < (Count div 2) do begin
    Exchange(I, Count - 1 - I);
    I := I + 1;
  end;
end;

procedure TGList<TItem>.Sort(Compare: TSortCompare);
begin
  if FCount > 1 then
    QuickSort(0, FCount - 1, Compare);
end;

procedure TGList<TItem>.AddArray(Values: array of TItem);
var
  I: TGListIndex;
begin
  I := 0;
  while I <= High(Values) do begin
    Add(Values[I]);
    I := I + 1;
  end;
end;

procedure TGList<TItem>.SetArray(Values: TItemArray);
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

procedure TGList<TItem>.InsertArray(Index: TGListIndex; Values: array of TItem);
var
  I: TGListIndex;
begin
  I := 0;
  while I <= High(Values) do begin
    Insert(Index + I, Values[I]);
    I := I + 1;
  end;
end;

function TGList<TItem>.Implode(Separator: string; Converter: TToStringConverter): string;
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

procedure TGList<TItem>.Explode(Text, Separator: string; Converter: TFromStringConverter; SlicesCount: Integer = -1);
begin
  Clear;
  while (Pos(Separator, Text) > 0) and
  ((Count < (SlicesCount - 1)) or (SlicesCount = -1)) do begin
    Add(Converter(Copy(Text, 1, Pos(Separator, Text) - 1)));
    System.Delete(Text, 1, Pos(Separator, Text) + Length(Separator) - 1);
  end;
  Add(Converter(Text));
end;

function TGList<TItem>.Add(Item: TItem): TGListIndex;
begin
  Count := Count + 1;
  Result := FCount - 1;
  FItems[Result] := Item;
end;

procedure TGList<TItem>.AddList(List: TGList);
var
  I: TGListIndex;
begin
  I := 0;
  while I < List.Count do begin
    Add(List[I]);
    I := I + 1;
  end;
end;

procedure TGList<TItem>.Clear;
begin
  Count := 0;
  Capacity := 0;
end;

procedure TGList<TItem>.Delete(Index: TGListIndex);
begin
  if (Index < 0) or (Index >= FCount) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  FCount := FCount - 1;
  System.Move(FItems[Index + 1], FItems[Index], (FCount - Index) * SizeOf(TItem));
  SetCapacityOptimized(Capacity - 1);
end;

procedure TGList<TItem>.DeleteItems(Index, Count: TGListIndex);
var
  I: TGListIndex;
begin
  I := Index;
  while I < (Index + Count) do begin
    Delete(Index);
    I := I + 1;
  end;
end;

procedure TGList<TItem>.Fill(Start, Count: TGListIndex; Value: TItem);
var
  I: TGListIndex;
begin
  I := Start;
  while I < Count do begin
    FItems[I] := Value;
    I := I + 1;
  end;
end;

procedure TGList<TItem>.Exchange(Index1, Index2: TGListIndex);
var
  Temp: TItem;
begin
  if ((Index1 >= FCount) or (Index1 < 0)) then
    raise EListError.CreateFmt(SListIndexError, [Index1]);
  if ((Index2 >= FCount) or (Index2 < 0)) then
    raise EListError.CreateFmt(SListIndexError, [Index2]);
  Temp := FItems[Index1];
  FItems[Index1] := FItems[Index2];
  FItems[Index2] := Temp;
end;

{ TGListObject }

procedure TGListObject<TItem>.Assign(Source: TGList<TItem>);
begin
  Clear;
  OwnsObjects := False;
  inherited;
end;

procedure TGListObject<TItem>.Put(Index: Integer; const AValue: TItem);
begin
  if OwnsObjects then FItems[Index].Free;
  inherited Put(Index, AValue);
end;

procedure TGListObject<TItem>.Delete(Index: Integer);
begin
  if OwnsObjects then FItems[Index].Free;
  inherited Delete(Index);
end;

procedure TGListObject<TItem>.Clear;
var
  I: Integer;
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

constructor TGListObject<TItem>.Create;
begin
  inherited;
  OwnsObjects := True;
end;

destructor TGListObject<TItem>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{ TGListString }

procedure TGListString<TItem>.Assign(Source: TGList<TItem>);
begin
  Clear;
  inherited;
end;

procedure TGListString<TItem>.Delete(Index: Integer);
begin
  FItems[Index] := '';
  inherited Delete(Index);
end;

procedure TGListString<TItem>.Clear;
var
  I: Integer;
begin
  I := 0;
  while I < Count do begin
    FItems[I] := '';
    I := I + 1;
  end;
  inherited Clear;
end;

constructor TGListString<TItem>.Create;
begin
  inherited;
end;

destructor TGListString<TItem>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

end.
