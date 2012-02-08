unit GenericList;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes;

type

  { TGAbstractList }

  TGAbstractList<TItem> = class
  public
  type
    TIndex = NativeInt;
    TSortCompare = function(Item1, Item2: TItem): Integer of object;
    TToStringConverter = function(Item: TItem): string;
    TFromStringConverter = function(Text: string): TItem;
    TItemArray = array of TItem;
  private
    function GetLast: TItem; virtual; abstract;
    procedure SetLast(AValue: TItem); virtual; abstract;
    function GetFirst: TItem; virtual; abstract;
    procedure SetFirst(AValue: TItem); virtual; abstract;
    function GetCount: TIndex; virtual; abstract;
    procedure SetCount(const AValue: TIndex); virtual; abstract;
    procedure SetCapacity(const AValue: TIndex); virtual; abstract;
    function GetCapacity: TIndex; virtual; abstract;
    function Get(Index: TIndex): TItem; virtual; abstract;
    procedure Put(Index: TIndex; const AValue: TItem); virtual; abstract;
  public
    constructor Create; virtual;
    property Count: TIndex read GetCount write SetCount;
    property Capacity: TIndex read GetCapacity write SetCapacity;
    property Items[Index: TIndex]: TItem read Get write Put; default;
    property First: TItem read GetFirst write SetFirst;
    property Last: TItem read GetLast write SetLast;
  end;

  { TGList }

  TGList<TItem> = class(TGAbstractList<TItem>)
  private
    FCount: TIndex;
    FItems: array of TItem;
    function Get(Index: TIndex): TItem; override;
    function GetCapacity: TIndex; override;
    function GetFirst: TItem; override;
    function GetLast: TItem; override;
    function GetCount: TIndex; override;
    procedure SetCapacity(const AValue: TIndex); override;
    procedure SetCapacityOptimized(const NewCapacity: TIndex);
    procedure SetCount(const AValue: TIndex); override;
    procedure SetFirst(AValue: TItem); override;
    procedure SetLast(AValue: TItem); override;
    procedure Put(Index: TIndex; const AValue: TItem); override;
    procedure QuickSort(L, R : TIndex; Compare: TSortCompare);
  public
    function Add(Item: TItem): TIndex;
    procedure AddArray(Values: array of TItem);
    procedure AddList(List: TGList<TItem>);
    procedure AddListPart(List: TGList<TItem>; ItemIndex, ItemCount: TIndex);
    procedure Assign(Source: TGList<TItem>); virtual;
    procedure Clear; virtual;
    procedure Delete(Index: TIndex); virtual;
    procedure DeleteItems(Index, Count: TIndex);
    function EqualTo(List: TGList<TItem>): Boolean;
    procedure Exchange(Index1, Index2: TIndex);
    procedure Explode(Text, Separator: string; Converter: TFromStringConverter; SlicesCount: Integer = -1);
    function Extract(Item: TItem): TItem;
    procedure Fill(Start, Count: TIndex; Value: TItem);
    function GetArray(Index, ACount: TIndex): TItemArray;
    procedure GetList(List: TGList<TItem>; Index, ACount: TIndex);
    function Implode(Separator: string; Converter: TToStringConverter): string;
    function IndexOf(Item: TItem; Start: TIndex = 0): TIndex;
    function IndexOfList(List: TGList<TItem>; Start: TIndex = 0): TIndex;
    procedure Insert(Index: TIndex; Item: TItem);
    procedure InsertList(Index: TIndex; List: TGList<TItem>);
    procedure InsertArray(Index: TIndex; Values: array of TItem);
    procedure InsertCount(Index: TIndex; ACount: TIndex);
    procedure Move(CurIndex, NewIndex: TIndex);
    procedure MoveItems(CurIndex, NewIndex, Count: TIndex);
    function Remove(Item: TItem): TIndex;
    procedure Reverse;
    procedure ReplaceArray(Index: TIndex; Values: array of TItem);
    procedure ReplaceList(Index: TIndex; Source: TGList<TItem>);
    procedure ReplaceListPart(Index: TIndex; Source: TGList<TItem>;
      SourceIndex, SourceCount: TIndex);
    procedure Sort(Compare: TSortCompare);
    procedure SetArray(Values: array of TItem);
  end;

  TListObject<TItem> = class(TGList<TItem>)
  private
    procedure Put(Index: Integer; const AValue: TItem); override;
  public
    OwnsObjects: Boolean;
    procedure Delete(Index: Integer); override;
    procedure Clear; override;
    procedure Assign(Source: TGList<TItem>); override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TListString<TItem> = class(TGList<TItem>)
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

{ TGList<TItem> }

function TGList<TItem>.GetCapacity: TIndex;
begin
  Result := Length(FItems);
end;

procedure TGList<TItem>.SetCapacity(const AValue: TIndex);
begin
  if (AValue < FCount) then
    raise EListError.CreateFmt(SListCapacityError, [AValue]);
  SetLength(FItems, AValue);
end;

procedure TGList<TItem>.SetCapacityOptimized(const NewCapacity: TIndex);
var
  IncSize: TIndex;
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

function TGList<TItem>.Get(Index: TIndex): TItem;
begin
  if (Index < 0) or (Index >= Count) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  Result := FItems[Index];
end;

procedure TGList<TItem>.Put(Index: TIndex; const AValue: TItem);
begin
  if (Index < 0) or (Index >= Count) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  FItems[Index] := AValue;
end;

procedure TGList<TItem>.SetCount(const AValue: TIndex);
begin
  if (AValue < 0) then
    raise EListError.CreateFmt(SListCountError, [AValue]);
  if AValue > Capacity then SetCapacityOptimized(AValue); // Before FCount change
  FCount := AValue;
  if AValue < Capacity then SetCapacityOptimized(AValue); // After FCount change
end;

function TGList<TItem>.GetArray(Index, ACount: TIndex): TItemArray;
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

procedure TGList<TItem>.GetList(List: TGList; Index, ACount: TIndex);
begin
 List.Clear;
 List.AddListPart(Self, Index, ACount);
end;

procedure TGList<TItem>.QuickSort(L, R: TIndex; Compare: TSortCompare);
var
  I, J: TIndex;
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

procedure TGList<TItem>.Assign(Source: TGList<TItem>);
var
  I: TIndex;
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
  I: TIndex;
begin
  I := IndexOf(Item);
  if I >= 0 then begin
    Result := Item;
    Delete(I);
  end else
    raise EListError.CreateFmt(SListIndexError, [0]);
end;

function TGList<TItem>.IndexOf(Item: TItem; Start: TIndex): TIndex;
begin
  Result := Start;
  while (Result < FCount) and
  not CompareMem(Addr(FItems[Result]), Addr(Item), SizeOf(TItem)) do
    Result := Result + 1;
  if Result = FCount then Result := -1;
end;

procedure TGList<TItem>.Insert(Index: TIndex; Item: TItem);
begin
  if (Index < 0) or (Index > FCount) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  InsertCount(Index, 1);
  FItems[Index] := Item;
end;

procedure TGList<TItem>.InsertList(Index: TIndex; List: TGList<TItem>);
begin
  if (Index < 0) or (Index > FCount) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  InsertCount(Index, List.Count);
  ReplaceList(Index, List);
end;

function TGList<TItem>.IndexOfList(List: TGList<TItem>; Start: TIndex): TIndex;
var
  I: TIndex;
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

function TGList<TItem>.GetCount: TIndex;
begin
  Result := FCount;
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

procedure TGList<TItem>.Move(CurIndex, NewIndex: TIndex);
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

procedure TGList<TItem>.MoveItems(CurIndex, NewIndex, Count: TIndex);
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

function TGList<TItem>.Remove(Item: TItem): TIndex;
begin
  Result := IndexOf(Item);
  if Result <> -1 then
    Delete(Result);
end;

function TGList<TItem>.EqualTo(List: TGList<TItem>): Boolean;
var
  I: TIndex;
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
  I: TIndex;
begin
  I := 0;
  while I < (Count div 2) do begin
    Exchange(I, Count - 1 - I);
    I := I + 1;
  end;
end;

procedure TGList<TItem>.ReplaceArray(Index: TIndex;
  Values: array of TItem);
var
  I: TIndex;
begin
  I := 0;
  while I < Length(Values) do begin
    Items[Index + I] := Values[I];
    I := I + 1;
  end;
end;

procedure TGList<TItem>.ReplaceList(Index: TIndex; Source: TGList<TItem>);
var
  I: TIndex;
begin
  I := 0;
  while I < Source.Count do begin
    Items[Index + I] := Source[I];
    I := I + 1;
  end;
end;

procedure TGList<TItem>.ReplaceListPart(Index: TIndex; Source: TGList<TItem>;
  SourceIndex, SourceCount: TIndex);
var
  I: TIndex;
begin
  I := 0;
  while I < SourceCount do begin
    Items[Index + I] := Source[SourceIndex + I];
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
  I: TIndex;
begin
  I := 0;
  while I <= High(Values) do begin
    Add(Values[I]);
    I := I + 1;
  end;
end;

procedure TGList<TItem>.SetArray(Values: array of TItem);
var
  I: TIndex;
begin
  Clear;
  I := 0;
  while I <= High(Values) do begin
    Add(Values[I]);
    I := I + 1;
  end;
end;

procedure TGList<TItem>.InsertArray(Index: TIndex; Values: array of TItem);
begin
  if (Index < 0) or (Index > FCount) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  InsertCount(Index, Length(Values));
  ReplaceArray(Index, Values);
end;

procedure TGList<TItem>.InsertCount(Index: TIndex; ACount: TIndex);
begin
  if (Index < 0) or (Index > FCount) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  Count := Count + ACount;
  if Index < FCount then
    System.Move(FItems[Index], FItems[Index + ACount], (FCount - ACount - Index) * SizeOf(TItem));
end;

function TGList<TItem>.Implode(Separator: string; Converter: TToStringConverter): string;
var
  I: TIndex;
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

function TGList<TItem>.Add(Item: TItem): TIndex;
begin
  Count := Count + 1;
  Result := FCount - 1;
  FItems[Result] := Item;
end;

procedure TGList<TItem>.AddList(List: TGList<TItem>);
var
  I: TIndex;
begin
  I := 0;
  while I < List.Count do begin
    Add(List[I]);
    I := I + 1;
  end;
end;

procedure TGList<TItem>.AddListPart(List: TGList; ItemIndex, ItemCount: TIndex);
var
  I: TIndex;
  J: TIndex;
begin
  I := Count;
  J := ItemIndex;
  Count := Count + ItemCount;
  while I < Count do begin
    Items[I] := List[J];
    I := I + 1;
    J := J + 1;
  end;
end;

procedure TGList<TItem>.Clear;
begin
  Count := 0;
  Capacity := 0;
end;

procedure TGList<TItem>.Delete(Index: TIndex);
begin
  if (Index < 0) or (Index >= FCount) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  FCount := FCount - 1;
  System.Move(FItems[Index + 1], FItems[Index], (FCount - Index) * SizeOf(TItem));
  SetCapacityOptimized(Capacity - 1);
end;

procedure TGList<TItem>.DeleteItems(Index, Count: TIndex);
var
  I: TIndex;
begin
  I := Index;
  while I < (Index + Count) do begin
    Delete(Index);
    I := I + 1;
  end;
end;

procedure TGList<TItem>.Fill(Start, Count: TIndex; Value: TItem);
var
  I: TIndex;
begin
  I := Start;
  while I < Count do begin
    FItems[I] := Value;
    I := I + 1;
  end;
end;

procedure TGList<TItem>.Exchange(Index1, Index2: TIndex);
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

{ TListObject }

procedure TListObject<TItem>.Assign(Source: TGList<TItem>);
begin
  Clear;
  OwnsObjects := False;
  inherited;
end;

procedure TListObject<TItem>.Put(Index: Integer; const AValue: TItem);
begin
  if OwnsObjects then FItems[Index].Free;
  inherited Put(Index, AValue);
end;

procedure TListObject<TItem>.Delete(Index: Integer);
begin
  if OwnsObjects then FItems[Index].Free;
  inherited Delete(Index);
end;

procedure TListObject<TItem>.Clear;
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

constructor TListObject<TItem>.Create;
begin
  inherited;
  OwnsObjects := True;
end;

destructor TListObject<TItem>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{ TListString }

procedure TListString<TItem>.Assign(Source: TGList<TItem>);
begin
  Clear;
  inherited;
end;

procedure TListString<TItem>.Delete(Index: Integer);
begin
  FItems[Index] := '';
  inherited Delete(Index);
end;

procedure TListString<TItem>.Clear;
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

constructor TListString<TItem>.Create;
begin
  inherited;
end;

destructor TListString<TItem>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{ TGAbstractList<TItem> }

constructor TGAbstractList<TItem>.Create;
begin
end;

end.
