unit GenericList;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
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

  TGList<T> = class//(TGAbstractList)
  public
    type
      PItem = ^T;
      TSortCompare = function(Item1, Item2: T): Integer of object;
      TToStringConverter = function(Item: T): string;
      TFromStringConverter = function(Text: string): T;
      TItemArray = array of T;
  private
    FItems: array of T;
    FCount: Integer;
    FUpdateCount: Integer;
    FOnUpdate: TNotifyEvent;
    function Get(Index: Integer): T;
    function GetCapacity: Integer;
    function GetLast: T;
    function GetFirst: T;
    procedure SetCapacity(const AValue: Integer);
    procedure SetCapacityOptimized(const NewCapacity: Integer);
    procedure SetLast(AValue: T);
    procedure SetFirst(AValue: T);
    procedure QuickSort(L, R : Integer; Compare: TSortCompare);
    procedure DoUpdate;
  protected
    procedure Put(Index: Integer; const AValue: T); virtual;
    procedure SetCount(const AValue: Integer); virtual;
  public
    function CompareMem(P1, P2: Pointer; Length: cardinal): Boolean; inline;
    function Add(Item: T): Integer;
    procedure AddArray(Values: array of T);
    procedure AddList(List: TGList<T>);
    procedure AddListPart(List: TGList<T>; ItemIndex, ItemCount: Integer);
    procedure Assign(Source: TGList<T>); virtual;
    constructor Create; virtual;
    procedure Clear; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure DeleteItems(Index, Count: Integer);
    function EqualTo(List: TGList<T>): Boolean;
    procedure Exchange(Index1, Index2: Integer);
    procedure Explode(Text, Separator: string; Converter: TFromStringConverter; SlicesCount: Integer = -1);
    function Extract(Item: T): T;
    property First: T read GetFirst write SetFirst;
    procedure Fill(Start, Count: Integer; Value: T);
    function GetArray(Index, ACount: Integer): TItemArray;
    procedure GetList(List: TGList<T>; Index, ACount: Integer);
    procedure GetBuffer(Index: Integer; var Buffer; Count: Integer);
    function Implode(Separator: string; Converter: TToStringConverter): string;
    function IndexOf(Item: T; Start: Integer = 0): Integer; virtual;
    function IndexOfList(List: TGList<T>; Start: Integer = 0): Integer;
    function IndexOfArray(Values: array of T; Start: Integer = 0): Integer;
    procedure Insert(Index: Integer; Item: T);
    procedure InsertList(Index: Integer; List: TGList<T>);
    procedure InsertArray(Index: Integer; Values: array of T);
    procedure InsertCount(Index: Integer; ACount: Integer);
    procedure Move(CurIndex, NewIndex: Integer);
    procedure MoveItems(CurIndex, NewIndex, Count: Integer);
    function Remove(Item: T): Integer;
    procedure Reverse;
    procedure ReplaceArray(Index: Integer; Values: array of T);
    procedure ReplaceList(Index: Integer; Source: TGList<T>);
    procedure ReplaceListPart(Index: Integer; Source: TGList<T>;
      SourceIndex, SourceCount: Integer);
    procedure ReplaceBuffer(Index: Integer; var Buffer; Count: Integer);
    procedure Sort(Compare: TSortCompare);
    procedure SetArray(Values: array of T);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Update;
    property Count: Integer read FCount write SetCount;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Items[Index: Integer]: T read Get write Put; default;
    property Last: T read GetLast write SetLast;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;

  TGListObject<T> = class(TGList<T>)
  protected
    procedure Put(Index: Integer; const AValue: T); override;
    procedure SetCount(const AValue: Integer); override;
  public
    OwnsObjects: Boolean;
    function AddNew(NewObject: T = nil): T;
    function InsertNew(Index: Integer; NewObject: T = nil): T;
    procedure Delete(Index: Integer); override;
    procedure Assign(Source: TGList<T>); override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TGListString<T> = class(TGList<T>)
  public
    procedure Delete(Index: Integer); override;
    procedure Clear; override;
    procedure Assign(Source: TGList<T>); override;
    function IndexOf(Item: T; Start: Integer = 0): Integer; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TGAbstractList<T> = class
  private
    FOnUpdate: TNotifyEvent;
    function Get(const Index: Integer): T; virtual; abstract;
    function GetCapacity: Integer; virtual; abstract;
    function GetCount: Integer; virtual; abstract;
    function GetFirst: T; virtual; abstract;
    function GetLast: T; virtual; abstract;
    procedure Put(const Index: Integer; const AValue: T); virtual; abstract;
    procedure SetCapacity(const AValue: Integer); virtual; abstract;
    procedure SetCount(const AValue: Integer); virtual; abstract;
    procedure SetFirst(const AValue: T); virtual; abstract;
    procedure SetLast(const AValue: T); virtual; abstract;
  public
    type
      PItem = ^T;
    property Count: Integer read GetCount write SetCount;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Items[Index: Integer]: T read Get write Put; default;
    property First: T read GetFirst write SetFirst;
    property Last: T read GetLast write SetLast;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;


implementation

uses
  RtlConsts;


{ TGList }

constructor TGList<T>.Create;
begin
  FCount := 0;
  FUpdateCount := 0;
end;

procedure TGList<T>.GetBuffer(Index: Integer; var Buffer; Count: Integer);
var
  P: PItem;
  I: Integer;
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

procedure TGList<T>.ReplaceBuffer(Index: Integer; var Buffer; Count: Integer);
var
  P: PItem;
  I: Integer;
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

procedure TGList<T>.ReplaceArray(Index: Integer; Values: array of T);
var
  I: Integer;
begin
  I := 0;
  while I < Length(Values) do begin
    Items[Index + I] := Values[I];
    I := I + 1;
  end;
  Update;
end;

procedure TGList<T>.ReplaceList(Index: Integer; Source: TGList<T>);
var
  I: Integer;
begin
  I := 0;
  while I < Source.Count do begin
    Items[Index + I] := Source[I];
    I := I + 1;
  end;
  Update;
end;

procedure TGList<T>.ReplaceListPart(Index: Integer; Source: TGList<T>;
  SourceIndex, SourceCount: Integer);
var
  I: Integer;
begin
  I := 0;
  while I < SourceCount do begin
    Items[Index + I] := Source[SourceIndex + I];
    I := I + 1;
  end;
  Update;
end;

function TGList<T>.GetCapacity: Integer;
begin
  Result := Length(FItems);
end;

procedure TGList<T>.SetCapacity(const AValue: Integer);
begin
  if (AValue < FCount) then
    raise EListError.CreateFmt(SListCapacityError, [AValue]);
  SetLength(FItems, AValue);
end;

procedure TGList<T>.SetCapacityOptimized(const NewCapacity: Integer);
var
  IncSize: Integer;
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

function TGList<T>.Get(Index: Integer): T;
begin
  if (Index < 0) or (Index >= Count) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  Result := FItems[Index];
end;

procedure TGList<T>.Put(Index: Integer; const AValue: T);
begin
  if (Index < 0) or (Index >= Count) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  FItems[Index] := AValue;
end;

procedure TGList<T>.SetCount(const AValue: Integer);
begin
  if (AValue < 0) then
    raise EListError.CreateFmt(SListCountError, [AValue]);
  if AValue > Capacity then SetCapacityOptimized(AValue); // Before FCount change
  FCount := AValue;
  if AValue < Capacity then SetCapacityOptimized(AValue); // After FCount change
end;

function TGList<T>.GetArray(Index, ACount: Integer): TItemArray;
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

procedure TGList<T>.GetList(List: TGList<T>; Index, ACount: Integer);
begin
  List.Clear;
  List.AddListPart(Self, Index, ACount);
end;

procedure TGList<T>.QuickSort(L, R: Integer; Compare: TSortCompare);
var
  I, J: Integer;
  P, Q: T;
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

procedure TGList<T>.Assign(Source: TGList<T>);
var
  I: Integer;
begin
  Count := Source.Count;
  I := 0;
  while I < Count do begin
    FItems[I] := Source[I];
    I := I + 1;
  end;
  Update;
end;

function TGList<T>.Extract(Item: T): T;
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

function TGList<T>.CompareMem(P1, P2: Pointer; Length: cardinal): Boolean;
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

function TGList<T>.IndexOf(Item: T; Start: Integer): Integer;
var
  List: PItem;
begin
  Result := 0;
  List := @(FItems[Start]);
  while (Result < FCount) and
//  not CompareMem(@FItems[Result], @Item, SizeOf(T)) do
  (CompareByte(List^, Item, SizeOf(T)) <> 0) do begin
    Inc(List);
    Inc(Result);
  end;
  if Result = FCount then Result := -1;
end;

procedure TGList<T>.Insert(Index: Integer; Item: T);
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

procedure TGList<T>.InsertList(Index: Integer; List: TGList<T>);
begin
  if (Index < 0) or (Index > FCount) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  InsertCount(Index, List.Count);
  ReplaceList(Index, List);
end;

procedure TGList<T>.InsertArray(Index: Integer; Values: array of T);
begin
  if (Index < 0) or (Index > FCount) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  InsertCount(Index, Length(Values));
  ReplaceArray(Index, Values);
end;

procedure TGList<T>.InsertCount(Index: Integer; ACount: Integer);
begin
  if (Index < 0) or (Index > FCount) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  Count := Count + ACount;
  if Index < FCount then
    System.Move(FItems[Index], FItems[Index + ACount], (FCount - ACount - Index) * SizeOf(T));
  Update;
end;

function TGList<T>.IndexOfList(List: TGList<T>; Start: Integer): Integer;
var
  I: Integer;
begin
  if List.Count > 0 then begin
    Result := IndexOf(List[0], Start);
    if Result <> -1 then begin
      I := 1;
      while I < List.Count do begin
        if not CompareMem(Addr(FItems[Result + I]), Addr(List.FItems[I]), SizeOf(T)) then begin
          Result := -1;
          Break;
        end;
        I := I + 1;
      end;
    end;
  end else Result := -1;
end;

function TGList<T>.IndexOfArray(Values: array of T; Start: Integer): Integer;
var
  I: Integer;
begin
  if Length(Values) > 0 then begin
    Result := IndexOf(Values[0], Start);
    if Result <> -1 then begin
      I := 1;
      while I < Length(Values) do begin
        if not CompareMem(Addr(FItems[Result + I]), Addr(Values[I]), SizeOf(T)) then begin
          Result := -1;
          Break;
        end;
        I := I + 1;
      end;
    end;
  end else Result := -1;
end;

function TGList<T>.GetLast: T;
begin
  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    Result := FItems[FCount - 1];
end;

procedure TGList<T>.SetLast(AValue: T);
begin
  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    FItems[FCount - 1] := AValue;
end;

function TGList<T>.GetFirst: T;
begin
  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    Result := FItems[0];
end;

procedure TGList<T>.SetFirst(AValue: T);
begin
  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    FItems[0] := AValue;
end;

procedure TGList<T>.Move(CurIndex, NewIndex: Integer);
var
  Temp: T;
begin
  if ((CurIndex < 0) or (CurIndex > Count - 1)) then
    raise EListError.CreateFmt(SListIndexError, [CurIndex]);
  if ((NewIndex < 0) or (NewIndex > Count -1)) then
    raise EListError.CreateFmt(SlistIndexError, [NewIndex]);
  Temp := FItems[CurIndex];
  if NewIndex > CurIndex then begin
    System.Move(FItems[CurIndex + 1], FItems[CurIndex], (NewIndex - CurIndex) * SizeOf(T));
  end else
  if NewIndex < CurIndex then begin
    System.Move(FItems[NewIndex], FItems[NewIndex + 1], (CurIndex - NewIndex) * SizeOf(T));
  end;
  FItems[NewIndex] := Temp;
  //Delete(CurIndex);
  //Insert(NewIndex, Temp);
  Update;
end;

procedure TGList<T>.MoveItems(CurIndex, NewIndex, Count: Integer);
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

function TGList<T>.Remove(Item: T): Integer;
begin
  Result := IndexOf(Item);
  if Result <> -1 then
    Delete(Result)
    else raise Exception.CreateFmt(SItemNotFound, [0]);
end;

function TGList<T>.EqualTo(List: TGList<T>): Boolean;
var
  I: Integer;
begin
  Result := Count = List.Count;
  if Result then begin
    I := 0;
    while I < Count do begin
      if not CompareMem(Addr(FItems[I]), Addr(List.FItems[I]), SizeOf(T)) then begin
        Result := False;
        Break;
      end;
      I := I + 1;
    end;
  end;
end;

procedure TGList<T>.Reverse;
var
  I: Integer;
begin
  I := 0;
  while I < (Count div 2) do begin
    Exchange(I, Count - 1 - I);
    I := I + 1;
  end;
  Update;
end;

procedure TGList<T>.Sort(Compare: TSortCompare);
begin
  if FCount > 1 then
    QuickSort(0, FCount - 1, Compare);
  Update;
end;

procedure TGList<T>.AddArray(Values: array of T);
var
  I: Integer;
begin
  I := 0;
  while I <= High(Values) do begin
    Add(Values[I]);
    I := I + 1;
  end;
  Update;
end;

procedure TGList<T>.SetArray(Values: array of T);
var
  I: Integer;
begin
  Clear;
  I := 0;
  while I <= High(Values) do begin
    Add(Values[I]);
    I := I + 1;
  end;
end;

procedure TGList<T>.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TGList<T>.EndUpdate;
begin
  if FUpdateCount > 0 then Dec(FUpdateCount);
  if FUpdateCount = 0 then DoUpdate;
end;

procedure TGList<T>.DoUpdate;
begin
  if Assigned(FOnUpdate) then FOnUpdate(Self);
end;

procedure TGList<T>.Update;
begin
  if FUpdateCount = 0 then DoUpdate;
end;

function TGList<T>.Implode(Separator: string; Converter: TToStringConverter): string;
var
  I: Integer;
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

procedure TGList<T>.Explode(Text, Separator: string; Converter: TFromStringConverter; SlicesCount: Integer = -1);
begin
  Clear;
  while (Pos(Separator, Text) > 0) and
  ((Count < (SlicesCount - 1)) or (SlicesCount = -1)) do begin
    Add(Converter(Copy(Text, 1, Pos(Separator, Text) - 1)));
    System.Delete(Text, 1, Pos(Separator, Text) + Length(Separator) - 1);
  end;
  Add(Converter(Text));
end;

function TGList<T>.Add(Item: T): Integer;
begin
  Count := Count + 1;
  Result := FCount - 1;
  FItems[Result] := Item;
  Update;
end;

procedure TGList<T>.AddList(List: TGList<T>);
var
  I: Integer;
  J: Integer;
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

procedure TGList<T>.AddListPart(List: TGList<T>; ItemIndex, ItemCount: Integer);
var
  I: Integer;
  J: Integer;
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

procedure TGList<T>.Clear;
begin
  Count := 0;
  Capacity := 0;
end;

procedure TGList<T>.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  FCount := FCount - 1;
  System.Move(FItems[Index + 1], FItems[Index], (FCount - Index) * SizeOf(T));
  SetCapacityOptimized(Capacity - 1);
  Update;
end;

procedure TGList<T>.DeleteItems(Index, Count: Integer);
var
  I: Integer;
begin
  I := Index;
  while I < (Index + Count) do begin
    Delete(Index);
    I := I + 1;
  end;
  Update;
end;

procedure TGList<T>.Fill(Start, Count: Integer; Value: T);
var
  I: Integer;
begin
  I := Start;
  while I < Count do begin
    FItems[I] := Value;
    I := I + 1;
  end;
  Update;
end;

procedure TGList<T>.Exchange(Index1, Index2: Integer);
var
  Temp: T;
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

procedure TGListString<T>.Assign(Source: TGList<T>);
begin
  Clear;
  inherited;
end;

procedure TGListString<T>.Delete(Index: Integer);
begin
  FItems[Index] := '';
  inherited Delete(Index);
end;

procedure TGListString<T>.Clear;
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

function TGListString<T>.IndexOf(Item: T; Start: Integer): Integer;
begin
  Result := Start;
  while (Result < Count) and
  (CompareStr(FItems[Result], Item) <> 0) do
    Result := Result + 1;
  if Result = FCount then Result := -1;
end;

constructor TGListString<T>.Create;
begin
  inherited;
end;

destructor TGListString<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;


{ TGListObject }

function TGListObject<T>.AddNew(NewObject: T = nil): T;
begin
  if Assigned(NewObject) then Result := NewObject
    else Result := T.Create;
  Add(Result);
end;

function TGListObject<T>.InsertNew(Index: Integer;
  NewObject: T = nil): T;
begin
  if Assigned(NewObject) then Result := NewObject
    else Result := T.Create;
  Insert(Index, Result);
end;

procedure TGListObject<T>.Assign(Source: TGList<T>);
begin
  Clear;
  OwnsObjects := False;
  inherited;
end;

procedure TGListObject<T>.Put(Index: Integer; const AValue: T);
begin
  if OwnsObjects and (FItems[Index] <> AValue) then FItems[Index].Free;
  inherited Put(Index, AValue);
end;

procedure TGListObject<T>.Delete(Index: Integer);
begin
  if OwnsObjects then FItems[Index].Free;
  inherited Delete(Index);
end;

procedure TGListObject<T>.SetCount(const AValue: Integer);
var
  I: Integer;
begin
  if OwnsObjects then begin
    I := FCount - 1;
    while I >= AValue do begin
      FItems[I].Free;
      I := I - 1;
    end;
  end;
  I := FCount;
  inherited;
  // Nil newly allocated items
  while I < AValue do begin
    FItems[I] := nil;
    I := I + 1;
  end;
end;

constructor TGListObject<T>.Create;
begin
  inherited;
  OwnsObjects := True;
end;

destructor TGListObject<T>.Destroy;
begin
  Clear;
  inherited;
end;


end.
