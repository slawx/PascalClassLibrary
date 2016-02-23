unit GenericList;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes;

type

  { TGAbstractList }

  TGAbstractList<TItem> = class
  private
    FOnUpdate: TNotifyEvent;
    FUpdateCount: NativeInt;
  public
  type
    TIndex = NativeInt;
    PItem = ^TItem;
    TSortCompare = function(Item1, Item2: TItem): Integer of object;
    TToStringConverter = function(Item: TItem): string;
    TFromStringConverter = function(Text: string): TItem;
    TItemArray = array of TItem;
  protected
    function GetLast: TItem; virtual;
    procedure SetLast(const AValue: TItem); virtual;
    function GetFirst: TItem; virtual;
    procedure SetFirst(const AValue: TItem); virtual;
    function GetCount: TIndex; virtual; abstract;
    procedure SetCount(const AValue: TIndex); virtual; abstract;
    function Get(Index: TIndex): TItem; virtual; abstract;
    procedure Put(Index: TIndex; const AValue: TItem); virtual; abstract;
    function GetInternal(Index: TIndex): TItem; virtual; abstract;
    procedure PutInternal(Index: TIndex; const AValue: TItem); virtual; abstract;
    procedure QuickSort(L, R : TIndex; Compare: TSortCompare);
    property ItemsInternal[Index: TIndex]: TItem read GetInternal
      write PutInternal;
  public
    function Add(const Item: TItem): TIndex; virtual;
    procedure AddArray(Values: array of TItem);
    procedure AddList(List: TGAbstractList<TItem>);
    procedure AddListPart(List: TGAbstractList<TItem>; ItemIndex, ItemCount: TIndex);
    procedure Assign(Source: TGAbstractList<TItem>); virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Update;
    constructor Create; virtual;
    procedure Clear; virtual;
    function CompareItem(const Item1, Item2: TItem): Boolean; virtual; abstract;
    procedure CopyItems(CurIndex, NewIndex, ACount: TIndex); virtual;
    procedure Delete(const Index: TIndex); virtual;
    procedure DeleteItems(const Index, ACount: TIndex); virtual;
    function EqualTo(List: TGAbstractList<TItem>): Boolean; virtual;
    procedure Exchange(const Index1, Index2: TIndex); virtual;
    procedure Explode(Text, Separator: string; Converter: TFromStringConverter; SlicesCount: Integer = -1);
    function Extract(Item: TItem): TItem; virtual;
    procedure Fill(Start, ACount: TIndex; Value: TItem); virtual;
    function GetArray(Index, ACount: TIndex): TItemArray; virtual;
    procedure GetList(List: TGAbstractList<TItem>; Index, ACount: TIndex); virtual;
    procedure GetBuffer(Index: TIndex; var Buffer; ACount: TIndex); virtual;
    function IndexOfList(List: TGAbstractList<TItem>; Start: TIndex = 0): TIndex; virtual;
    procedure Insert(const Index: TIndex; Item: TItem); virtual;
    procedure InsertCount(const Index: TIndex; ACount: TIndex); virtual;
    procedure InsertList(const Index: TIndex; List: TGAbstractList<TItem>); virtual;
    procedure InsertArray(const Index: TIndex; Values: array of TItem); virtual;
    function IndexOf(Item: TItem; Start: TIndex = 0): TIndex; virtual;
    function Implode(Separator: string; Converter: TToStringConverter): string;
    procedure Move(const CurIndex, NewIndex: TIndex); virtual;
    procedure MoveItems(CurIndex, NewIndex, ACount: TIndex); virtual;
    procedure ReplaceArray(const Index: TIndex; Values: array of TItem); virtual;
    procedure ReplaceList(const Index: TIndex; Source: TGAbstractList<TItem>); virtual;
    procedure ReplaceListPart(const Index: TIndex; Source: TGAbstractList<TItem>;
      SourceIndex, SourceCount: TIndex); virtual;
    procedure ReplaceBuffer(const Index: TIndex; var Buffer; ACount: TIndex);
    function Remove(const Item: TItem): TIndex;
    procedure Reverse;
    procedure Sort(Compare: TSortCompare); virtual;
    procedure SetArray(Values: array of TItem); virtual;
    property Count: TIndex read GetCount write SetCount;
    property Items[Index: TIndex]: TItem read Get write Put; default;
    property First: TItem read GetFirst write SetFirst;
    property Last: TItem read GetLast write SetLast;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;

  { TGList }

  TGList<TItem> = class(TGAbstractList<TItem>)
  private
    FCount: TIndex;
    FItems: array of TItem;
  protected
    function Get(Index: TIndex): TItem; override;
    function GetInternal(Index: TIndex): TItem; override;
    function GetCount: TIndex; override;
    function GetCapacity: TIndex; virtual;
    procedure SetCount(const AValue: TIndex); override;
    procedure SetCapacity(const AValue: TIndex); virtual;
    procedure SetCapacityOptimized(const NewCapacity: TIndex);
    procedure Put(Index: TIndex; const AValue: TItem); override;
    procedure PutInternal(Index: TIndex; const AValue: TItem); override;
  public
    procedure Fill(Start, Count: TIndex; Value: TItem); override;
    procedure Clear; override;
    procedure ReplaceList(const Index: TIndex; Source: TGAbstractList<TItem>); override;
    function CompareItem(const Item1, Item2: TItem): Boolean; override;
    procedure CopyItems(CurIndex, NewIndex, ACount: TIndex); override;
    property Capacity: TIndex read GetCapacity write SetCapacity;
  end;

  { TGObjectList }

  TGObjectList<TItem> = class(TGList<TItem>)
  protected
    procedure Put(Index: TIndex; const AValue: TItem); override;
  public
    OwnsObjects: Boolean;
    procedure SetCount(const AValue: TIndex); override;
    function AddNew(NewObject: TItem = nil): TItem;
    procedure Delete(const Index: TIndex); override;
    procedure Clear; override;
    procedure Assign(Source: TGAbstractList<TItem>); override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TGStringList<TItem> = class(TGList<TItem>)
  private
  public
    procedure Delete(const Index: TIndex); override;
    procedure Clear; override;
    procedure Assign(Source: TGAbstractList<TItem>); override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TGFileList }

  TGFileList<TItem> = class(TGList<TItem>)
  private
    FFileName: string;
    FHandle: THandle;
    FMode: Word;
    function GetOpenned: Boolean;
    procedure SetFileName(AValue: string);
    procedure SetMode(AValue: Word);
  protected
    function GetCount: TIndex; override;
    procedure SetCount(const AValue: TIndex); override;
    procedure SetCapacity(const AValue: TIndex); override;
    function GetCapacity: TIndex; override;
    function Get(Index: TIndex): TItem; override;
    procedure Put(Index: TIndex; const AValue: TItem); override;
  public
    procedure Open;
    procedure Close;
    constructor Create; override;
    destructor Destroy; override;
    property FileName: string read FFileName write SetFileName;
    property Mode: Word read FMode write SetMode;
    property Openned: Boolean read GetOpenned;
  end;

  { TListByte }

  TListByte = class(TGList<Byte>)
    procedure WriteToStream(Stream: TStream);
    procedure WriteToStreamPart(Stream: TStream; ItemIndex, ItemCount: TIndex);
    procedure ReplaceStream(Stream: TStream);
    procedure ReplaceStreamPart(Stream: TStream; ItemIndex, ItemCount: TIndex);
    procedure AddStream(Stream: TStream);
    procedure AddStreamPart(Stream: TStream; ItemCount: TIndex);
    procedure WriteBuffer(var Buffer; Count: Integer);
    procedure ReadBuffer(var Buffer; Count: Integer);
  end;
  TListInteger = TGList<Integer>;
  TListString = TGStringList<string>;
  TListObject = TGObjectList<TObject>;

  { TListNotifyEvent }

  TListNotifyEvent = class(TGList<TNotifyEvent>)
    procedure CallAll(Sender: TObject);
  end;
  TBaseEvent = procedure of object;

  { TListSimpleEvent }

  TListSimpleEvent = class(TGList<TBaseEvent>)
    procedure CallAll;
  end;
  TListBoolean = TGList<Boolean>;
  TListDouble = TGList<Double>;

function StrToStr(Value: string): string;



resourcestring
  SListIndexError               = 'List index (%d) out of bounds';
  SListCapacityError            = 'List capacity (%d) exceeded.';
  SListCountError               = 'List count (%d) out of bounds.';

implementation

{ TGList<TItem> }

function TGList<TItem>.Get(Index: TIndex): TItem;
begin
  if (Index < 0) or (Index >= Count) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  Result := ItemsInternal[Index];
end;

function TGList<TItem>.GetInternal(Index: TIndex): TItem;
begin
  Result := FItems[Index];
end;

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

procedure TGList<TItem>.Put(Index: TIndex; const AValue: TItem);
begin
  if (Index < 0) or (Index >= Count) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  ItemsInternal[Index] := AValue;
end;

procedure TGList<TItem>.PutInternal(Index: TIndex; const AValue: TItem);
begin
  FItems[Index] := AValue;
end;

procedure TGList<TItem>.Fill(Start, Count: TIndex; Value: TItem);
begin
  (*if SizeOf(Value) = 1 then System.FillByte(FItems[Start], Count, Byte(Value))
  else if SizeOf(Value) = 2 then System.FillWord(FItems[Start], Count, Word(Value))
  else if SizeOf(Value) = 4 then System.FillDWord(FItems[Start], Count, DWord(Value))
  else if SizeOf(Value) = 8 then System.FillQWord(FItems[Start], Count, QWord(Value))
  else*) inherited;
end;

procedure TGList<TItem>.Clear;
begin
  inherited;
  Capacity := 0;
end;

procedure TGList<TItem>.ReplaceList(const Index: TIndex; Source: TGAbstractList<TItem>
  );
begin
  if (Source.Count > 0) and (Source is TGList<TItem>) then begin
    System.Move(PByte(TGList<TItem>(Source).FItems)^, FItems[Index], Source.Count * SizeOf(TItem));
  end else inherited;
end;

procedure TGList<TItem>.SetCount(const AValue: TIndex);
begin
  if (AValue < 0) then
    raise EListError.CreateFmt(SListCountError, [AValue]);
  if AValue > Capacity then SetCapacityOptimized(AValue); // Before FCount change

  if AValue > FCount then // Clear allocated space
    FillChar(FItems[FCount], (AValue - FCount) * SizeOf(TItem), 0);
  FCount := AValue;
  if AValue < Capacity then SetCapacityOptimized(AValue); // After FCount change
end;

function TGList<TItem>.CompareItem(const Item1, Item2: TItem): Boolean;
begin
  Result := CompareByte(Item1, Item2, SizeOf(TItem)) = 0;
end;

function TGList<TItem>.GetCount: TIndex;
begin
  Result := FCount;
end;

procedure TGList<TItem>.CopyItems(CurIndex, NewIndex, ACount: TIndex);
begin
  if ACount > 0 then
    System.Move(FItems[CurIndex], FItems[NewIndex], ACount * SizeOf(TItem));
end;

{ TGObjectList }

procedure TGObjectList<TItem>.Assign(Source: TGAbstractList<TItem>);
begin
  Clear;
  OwnsObjects := False;
  inherited;
end;

procedure TGObjectList<TItem>.Put(Index: TIndex; const AValue: TItem);
begin
  if OwnsObjects and Assigned(FItems[Index]) then FItems[Index].Free;
  inherited Put(Index, AValue);
end;

procedure TGObjectList<TItem>.SetCount(const AValue: TIndex);
begin
  if AValue < FCount then
    Fill(AValue, FCount - AValue, nil);
  inherited SetCount(AValue);
end;

function TGObjectList<TItem>.AddNew(NewObject: TItem): TItem;
begin
  if Assigned(NewObject) then Result := NewObject
    else Result := TItem.Create;
  Add(Result);
end;

procedure TGObjectList<TItem>.Delete(const Index: TIndex);
begin
  (*if OwnsObjects then begin
    FItems[Index].Free;
    FItems[Index] := nil;
  end;*)
  inherited Delete(Index);
end;

procedure TGObjectList<TItem>.Clear;
begin
  Fill(0, Count, nil);
  inherited Clear;
end;

constructor TGObjectList<TItem>.Create;
begin
  inherited;
  OwnsObjects := True;
end;

destructor TGObjectList<TItem>.Destroy;
begin
  Clear;
  inherited;
end;

{ TGStringList }

procedure TGStringList<TItem>.Assign(Source: TGAbstractList<TItem>);
begin
  Clear;
  inherited;
end;

procedure TGStringList<TItem>.Delete(const Index: TIndex);
begin
  FItems[Index] := '';
  inherited Delete(Index);
end;

procedure TGStringList<TItem>.Clear;
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

constructor TGStringList<TItem>.Create;
begin
  inherited;
end;

destructor TGStringList<TItem>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{ TGAbstractList<TItem> }

function TGAbstractList<TItem>.GetLast: TItem;
begin
  if Count = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    Result := Items[Count - 1];
end;

procedure TGAbstractList<TItem>.SetLast(const AValue: TItem);
begin
  if Count = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    Items[Count - 1] := AValue;
end;

function TGAbstractList<TItem>.GetFirst: TItem;
begin
  if Count = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    Result := Items[0];
end;

procedure TGAbstractList<TItem>.SetFirst(const AValue: TItem);
begin
  if Count = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    Items[0] := AValue;
end;

procedure TGAbstractList<TItem>.QuickSort(L, R: TIndex; Compare: TSortCompare);
var
  I, J: TIndex;
  P, Q: TItem;
begin
  repeat
    I := L;
    J := R;
    P := ItemsInternal[(L + R) div 2];
    repeat
      while Compare(P, ItemsInternal[I]) > 0 do
        I := I + 1;
      while Compare(P, ItemsInternal[J]) < 0 do
        J := J - 1;
      if I <= J then
      begin
        Q := ItemsInternal[I];
        ItemsInternal[I] := ItemsInternal[J];
        ItemsInternal[J] := Q;
        I := I + 1;
        J := J - 1;
      end;
    until I > J;
    if L < J then
      QuickSort(L, J, Compare);
    L := I;
  until I >= R;
end;

constructor TGAbstractList<TItem>.Create;
begin
end;

procedure TGAbstractList<TItem>.Clear;
begin
  Count := 0;
end;

procedure TGAbstractList<TItem>.Delete(const Index: TIndex);
begin
  DeleteItems(Index, 1);
end;

procedure TGAbstractList<TItem>.DeleteItems(const Index, ACount: TIndex);
begin
  if (Index < 0) or (Index >= Count) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  MoveItems(Index + ACount, Index, Count - Index - ACount);
  //SetCapacityOptimized(Capacity - ACount);

  Count := Count - ACount;
end;

function TGAbstractList<TItem>.EqualTo(List: TGAbstractList<TItem>): Boolean;
var
  I: TIndex;
begin
  Result := Count = List.Count;
  if Result then begin
    I := 0;
    while I < Count do begin
      if not CompareItem(Items[I], List.Items[I]) then begin
        Result := False;
        Break;
      end;
      I := I + 1;
    end;
  end;
end;

procedure TGAbstractList<TItem>.Exchange(const Index1, Index2: TIndex);
var
  Temp: TItem;
begin
  if ((Index1 >= Count) or (Index1 < 0)) then
    raise EListError.CreateFmt(SListIndexError, [Index1]);
  if ((Index2 >= Count) or (Index2 < 0)) then
    raise EListError.CreateFmt(SListIndexError, [Index2]);
  Temp := ItemsInternal[Index1];
  ItemsInternal[Index1] := ItemsInternal[Index2];
  ItemsInternal[Index2] := Temp;
end;

procedure TGAbstractList<TItem>.Explode(Text, Separator: string;
  Converter: TFromStringConverter; SlicesCount: Integer);
begin
  Clear;
  while (Pos(Separator, Text) > 0) and
  ((Count < (SlicesCount - 1)) or (SlicesCount = -1)) do begin
    Add(Converter(Copy(Text, 1, Pos(Separator, Text) - 1)));
    System.Delete(Text, 1, Pos(Separator, Text) + Length(Separator) - 1);
  end;
  Add(Converter(Text));
end;

function TGAbstractList<TItem>.Extract(Item: TItem): TItem;
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

procedure TGAbstractList<TItem>.Fill(Start, ACount: TIndex; Value: TItem);
var
  I: TIndex;
begin
  I := Start;
  while I < (Start + ACount) do begin
    Items[I] := Value;
    I := I + 1;
  end;
end;

function TGAbstractList<TItem>.GetArray(Index, ACount: TIndex): TItemArray;
var
  I: Integer;
begin
  SetLength(Result, ACount);
  I := 0;
  while I < Count do begin
    Result[I] := Items[Index + I];
    I := I + 1;
  end;
end;

procedure TGAbstractList<TItem>.GetList(List: TGAbstractList<TItem>; Index, ACount: TIndex);
begin
  List.Clear;
  List.AddListPart(Self, Index, ACount);
end;

procedure TGAbstractList<TItem>.GetBuffer(Index: TIndex; var Buffer;
  ACount: TIndex);
var
  P: PItem;
  I: TIndex;
begin
  if (Index + ACount) > Count then
    raise EListError.CreateFmt(SListIndexError, [Index + ACount]);
  P := PItem(@Buffer);
  I := 0;
  while I < ACount do begin
    P^ := Items[Index + I];
    Inc(P, 1);
    I := I + 1;
  end;
end;

function TGAbstractList<TItem>.IndexOfList(List: TGAbstractList<TItem>; Start: TIndex
  ): TIndex;
var
  I: TIndex;
begin
  if List.Count > 0 then begin
    Result := IndexOf(List[0], Start);
    if Result <> -1 then begin
      I := 1;
      while I < List.Count do begin
        if not CompareItem(Items[Result + I], List.Items[I]) then begin
          Result := -1;
          Break;
        end;
        I := I + 1;
      end;
    end;
  end else Result := -1;
end;

function TGAbstractList<TItem>.IndexOf(Item: TItem; Start: TIndex): TIndex;
begin
  Result := Start;
  while (Result < Count) and (not CompareItem(Items[Result], Item)) do
    Result := Result + 1;
  if Result = Count then Result := -1;
end;

procedure TGAbstractList<TItem>.Insert(const Index: TIndex; Item: TItem);
begin
  if (Index > Count) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  InsertCount(Index, 1);
  ItemsInternal[Index] := Item;
end;

procedure TGAbstractList<TItem>.InsertList(const Index: TIndex; List: TGAbstractList<TItem>);
begin
  if (Index < 0) or (Index > Count) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  InsertCount(Index, List.Count);
  ReplaceList(Index, List);
end;

procedure TGAbstractList<TItem>.InsertArray(const Index: TIndex;
  Values: array of TItem);
begin
  if (Index < 0) or (Index > Count) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  InsertCount(Index, Length(Values));
  ReplaceArray(Index, Values);
end;

procedure TGAbstractList<TItem>.InsertCount(const Index: TIndex; ACount: TIndex);
begin
  if (Index < 0) or (Index > Count) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  Count := Count + ACount;
  CopyItems(Index, Index + ACount, Count - ACount - Index);
end;

function TGAbstractList<TItem>.Implode(Separator: string;
  Converter: TToStringConverter): string;
var
  I: TIndex;
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

procedure TGAbstractList<TItem>.CopyItems(CurIndex, NewIndex, ACount: TIndex);
var
  I: Integer;
begin
  if CurIndex > NewIndex then begin
    I := 0;
    while I < ACount do begin
      ItemsInternal[NewIndex] := ItemsInternal[CurIndex];
      CurIndex := CurIndex + 1;
      NewIndex := NewIndex + 1;
      I := I + 1;
    end;
  end else begin
    I := ACount - 1;
    NewIndex := NewIndex + ACount - 1;
    CurIndex := CurIndex + ACount - 1;
    while I >= 0 do begin
      ItemsInternal[NewIndex] := ItemsInternal[CurIndex];
      NewIndex := NewIndex - 1;
      CurIndex := CurIndex - 1;
      I := I - 1;
    end;
  end;
end;

procedure TGAbstractList<TItem>.Move(const CurIndex, NewIndex: TIndex);
begin
  MoveItems(CurIndex, NewIndex, 1);
end;

procedure TGAbstractList<TItem>.MoveItems(CurIndex, NewIndex,
  ACount: TIndex);
var
//  I: Integer;
  Temp: TGList<TItem>;
begin
  if (ACount > 0) and (NewIndex <> CurIndex) then
  try
    Temp := TGList<TItem>.Create;
    if NewIndex > CurIndex then begin
      Temp.AddListPart(Self, CurIndex, ACount);
      CopyItems(CurIndex + ACount, CurIndex, NewIndex - CurIndex);
      ReplaceList(NewIndex, Temp);
    end else
    if NewIndex < CurIndex then begin
       Temp.AddListPart(Self, CurIndex, ACount);
      CopyItems(NewIndex, NewIndex + ACount, CurIndex - NewIndex);
      ReplaceList(NewIndex, Temp);
    end;
  finally
    Temp.Free;
  end;
end;

procedure TGAbstractList<TItem>.ReplaceArray(const Index: TIndex;
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

procedure TGAbstractList<TItem>.ReplaceList(const Index: TIndex; Source: TGAbstractList<TItem>
  );
var
  I: TIndex;
begin
  I := 0;
  while I < Source.Count do begin
    Items[Index + I] := Source[I];
    I := I + 1;
  end;
end;

procedure TGAbstractList<TItem>.ReplaceListPart(const Index: TIndex;
  Source: TGAbstractList<TItem>; SourceIndex, SourceCount: TIndex);
var
  I: TIndex;
begin
  I := 0;
  while I < SourceCount do begin
    Items[Index + I] := Source[SourceIndex + I];
    I := I + 1;
  end;
end;

procedure TGAbstractList<TItem>.ReplaceBuffer(const Index: TIndex; var Buffer;
  ACount: TIndex);
var
  P: PItem;
  I: TIndex;
begin
  if (Index + ACount) > Count then
    raise EListError.CreateFmt(SListIndexError, [Index + ACount]);
  P := PItem(@Buffer);
  I := 0;
  while I < ACount do begin
    Items[Index + I] := P^;
    Inc(P, 1);
    I := I + 1;
  end;
end;

function TGAbstractList<TItem>.Remove(const Item: TItem): TIndex;
begin
  Result := IndexOf(Item);
  if Result <> -1 then
    Delete(Result);
end;

procedure TGAbstractList<TItem>.Reverse;
var
  I: TIndex;
begin
  I := 0;
  while I < (Count div 2) do begin
    Exchange(I, Count - 1 - I);
    I := I + 1;
  end;
end;

procedure TGAbstractList<TItem>.Sort(Compare: TSortCompare);
begin
  if Count > 1 then
    QuickSort(0, Count - 1, Compare);
end;

procedure TGAbstractList<TItem>.SetArray(Values: array of TItem);
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

function TGAbstractList<TItem>.Add(const Item: TItem): TIndex;
begin
  Count := Count + 1;
  Result := Count - 1;
  ItemsInternal[Result] := Item;
end;

procedure TGAbstractList<TItem>.AddArray(Values: array of TItem);
var
  I: TIndex;
begin
  I := 0;
  while I <= High(Values) do begin
    Add(Values[I]);
    I := I + 1;
  end;
end;

procedure TGAbstractList<TItem>.AddList(List: TGAbstractList<TItem>);
begin
  Count := Count + List.Count;
  ReplaceList(Count - List.Count, List);
end;

procedure TGAbstractList<TItem>.AddListPart(List: TGAbstractList<TItem>; ItemIndex,
  ItemCount: TIndex);
begin
  Count := Count + ItemCount;
  ReplaceListPart(Count - ItemCount, List, ItemIndex, ItemCount);
end;

procedure TGAbstractList<TItem>.Assign(Source: TGAbstractList<TItem>);
begin
  Count := Source.Count;
  if Count > 0 then ReplaceList(0, Source);
end;

procedure TGAbstractList<TItem>.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TGAbstractList<TItem>.EndUpdate;
begin
  Dec(FUpdateCount);
  Update;
end;

procedure TGAbstractList<TItem>.Update;
begin
  if Assigned(FOnUpdate) and (FUpdateCount = 0) then FOnUpdate(Self);
end;

{ TGFileList<TItem> }

procedure TGFileList<TItem>.SetFileName(AValue: string);
begin
  if FFileName = AValue then Exit;
  FFileName := AValue;
  if Openned then begin
    Close;
    Open;
  end;
end;

function TGFileList<TItem>.GetOpenned: Boolean;
begin
  Result := FHandle <> feInvalidHandle;
end;

procedure TGFileList<TItem>.SetMode(AValue: Word);
begin
  if FMode = AValue then Exit;
  FMode := AValue;
  if Openned then begin
    Close;
    Open;
  end;
end;

function TGFileList<TItem>.GetCount: TIndex;
var
  OldPos: TIndex;
begin
  OldPos := FileSeek(FHandle, 0, 1);
  Result := FileSeek(FHandle, 0, 2);
  FileSeek(FHandle, OldPos, 0);
end;

procedure TGFileList<TItem>.SetCount(const AValue: TIndex);
begin
  FileTruncate(FHandle, AValue);
end;

procedure TGFileList<TItem>.SetCapacity(const AValue: TIndex);
begin
  inherited SetCapacity(AValue);
end;

function TGFileList<TItem>.GetCapacity: TIndex;
begin
  Result := inherited GetCapacity;
end;

function TGFileList<TItem>.Get(Index: TIndex): TItem;
begin
  FileSeek(FHandle, Index, 0);
  FileRead(FHandle, Result, SizeOf(Result));
end;

procedure TGFileList<TItem>.Put(Index: TIndex; const AValue: TItem);
begin
  FileSeek(FHandle, Index, 0);
  FileWrite(FHandle, AValue, SizeOf(AValue));
end;

procedure TGFileList<TItem>.Open;
begin
  Close;
  if Mode = fmCreate then FHandle := FileCreate(FileName, Mode)
    else FileOpen(FileName, Mode);
end;

procedure TGFileList<TItem>.Close;
begin
  if FHandle <> feInvalidHandle then FileClose(FHandle);
end;

constructor TGFileList<TItem>.Create;
begin
  FHandle := feInvalidHandle;
end;

destructor TGFileList<TItem>.Destroy;
begin
  Close;
  inherited;
end;

function StrToStr(Value: string): string;
begin
  Result := Value;
end;

{ TListSimpleEvent }

procedure TListSimpleEvent.CallAll;
var
  I: TIndex;
begin
  I := 0;
  while (I < Count) do begin
    Items[I]();
    I := I + 1;
  end;
end;

{ TListNotifyEvent }

procedure TListNotifyEvent.CallAll(Sender: TObject);
var
  I: TIndex;
begin
  I := 0;
  while (I < Count) do begin
    Items[I](Sender);
    I := I + 1;
  end;
end;

{ TListByte }

procedure TListByte.WriteToStream(Stream: TStream);
var
  I: Integer;
begin
  Stream.Position := 0;
  I := 0;
  while I < Count do begin
    Stream.WriteByte(Items[I]);
    I := I + 1;
  end;
end;

procedure TListByte.WriteToStreamPart(Stream: TStream; ItemIndex, ItemCount: TIndex);
var
  I: Integer;
begin
  I := ItemIndex;
  while I < ItemCount do begin
    Stream.WriteByte(Items[I]);
    I := I + 1;
  end;
end;

procedure TListByte.ReplaceStream(Stream: TStream);
var
  I: Integer;
begin
  Stream.Position := 0;
  I := 0;
  while I < Count do begin
    Items[I] := Stream.ReadByte;
    I := I + 1;
  end;
end;

procedure TListByte.ReplaceStreamPart(Stream: TStream; ItemIndex,
  ItemCount: TIndex);
var
  I: Integer;
begin
  I := ItemIndex;
  while I < ItemCount do begin
    Items[I] := Stream.ReadByte;
    I := I + 1;
  end;
end;

procedure TListByte.AddStream(Stream: TStream);
var
  I: Integer;
begin
  Stream.Position := 0;
  I := Count;
  Count := Count + Stream.Size;
  while I < Count do begin
    Items[I] := Stream.ReadByte;
    I := I + 1;
  end;
end;

procedure TListByte.AddStreamPart(Stream: TStream; ItemCount: TIndex);
var
  I: Integer;
begin
  I := Count;
  Count := Count + ItemCount;
  while I < Count do begin
    Items[I] := Stream.ReadByte;
    I := I + 1;
  end;
end;

procedure TListByte.WriteBuffer(var Buffer; Count: Integer);
begin

end;

procedure TListByte.ReadBuffer(var Buffer; Count: Integer);
begin

end;

end.
