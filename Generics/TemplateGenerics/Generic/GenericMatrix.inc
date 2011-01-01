{$IFDEF INTERFACE}

  TGMatrix = class;

  TGMatrixSortCompare = function(const Item1, Item2: TGMatrixItem): Integer of object;
  TGMatrixToStringConverter = function(Item: TGMatrixItem): string;
  TGMatrixFromStringConverter = function(Text: string): TGMatrixItem;

  TGMatrixIndex = record
    X: TGMatrixIndexX;
    Y: TGMatrixIndexY;
  end;

  // TGMatrix<TGMatrixIndex, TGMatrixIndex, TGMatrixItem> = class
  TGMatrix = class
  private
    FItems: array of array of TGMatrixItem;
    FCount: TGMatrixIndex;
    function Get(Y: TGMatrixIndexY; X: TGMatrixIndexX): TGMatrixItem;
    function GetCapacity: TGMatrixIndex;
    function GetLast: TGMatrixItem;
    function GetFirst: TGMatrixItem;
    procedure SetCapacity(const AValue: TGMatrixIndex);
    procedure SetLast(AValue: TGMatrixItem);
    procedure SetFirst(AValue: TGMatrixItem);
    procedure Put(Y: TGMatrixIndexY; X: TGMatrixIndexX; const AValue: TGMatrixItem); virtual;
    procedure SetCount(const AValue: TGMatrixIndex);
  public
    function Add(Item: TGMatrixItem): TGMatrixIndex;
    procedure AddArray(Values: array of TGMatrixItem);
    procedure AddList(List: TGMatrix);
    procedure Assign(Source: TGMatrix);
    procedure Clear; virtual;
    procedure Contract;
    function CreateIndex(Y: TGMatrixIndexY; X: TGMatrixIndexX): TGMatrixIndex;
    procedure Delete(Index: TGMatrixIndex); virtual;
    procedure DeleteItems(Index, Count: TGMatrixIndex);
    function EqualTo(List: TGMatrix): Boolean;
    procedure Expand;
    function Extract(Item: TGMatrixItem): TGMatrixItem;
    procedure Exchange(Index1, Index2: TGMatrixIndex);
    property First: TGMatrixItem read GetFirst write SetFirst;
    procedure Fill(Start, Count: TGMatrixIndex; Value: TGMatrixItem);
    function Implode(Separator: string; Converter: TGMatrixToStringConverter): string;
    procedure Explode(Text, Separator: string; Converter: TGMatrixFromStringConverter; SlicesCount: Integer = -1);
    function IndexOf(Item: TGMatrixItem; Start: TGMatrixIndex = 0): TGMatrixIndex;
    function IndexOfList(List: TGMatrix; Start: TGMatrixIndex = 0): TGMatrixIndex;
    procedure Insert(Index: TGMatrixIndex; Item: TGMatrixItem);
    procedure InsertList(Index: TGMatrixIndex; List: TGMatrix);
    procedure InsertArray(Index: TGMatrixIndex; Values: array of TGMatrixItem);
    procedure Move(CurIndex, NewIndex: TGMatrixIndex);
    procedure MoveItems(CurIndex, NewIndex, Count: TGMatrixIndex);
    function Remove(Item: TGMatrixItem): TGMatrixIndex;
    procedure Reverse;
    procedure Sort(Compare: TGMatrixSortCompare);
    procedure SetArray(Values: array of TGMatrixItem);
    property Count: TGMatrixIndex read FCount write SetCount;
    property Capacity: TGMatrixIndex read GetCapacity write SetCapacity;
    property Items[Y: TGMatrixIndexY; X: TGMatrixIndexX]: TGMatrixItem read Get write Put; default;
    property Last: TGMatrixItem read GetLast write SetLast;
  end;

{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION_USES}

uses
  RtlConsts;

resourcestring
  SMatrixIndexError = 'Matrix index error [%d, %d]';

{$UNDEF IMPLEMENTATION_USES}
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TGMatrix }

function TGMatrix.CreateIndex(Y: TGMatrixIndexY; X: TGMatrixIndexX): TGMatrixIndex;
begin
  Result.X := X;
  Result.Y := Y;
end;

function TGMatrix.GetCapacity: TGMatrixIndex;
begin
  Result.Y := Length(FItems);
  if Result.Y > 0 then Result.X := Length(FItems[0]) else Result.X := 0;
end;

procedure TGMatrix.SetCapacity(const AValue: TGMatrixIndex);
var
  Y: TGMatrixIndexY;
begin
  if (Capacity.X <> AValue.X) and (Capacity.Y <> AValue.Y) then begin
    SetLength(FItems, AValue.Y);
    Y := 0;
    while Y < Length(FItems) do begin
      SetLength(FItems[Y], AValue.X);
      Y := Y + 1;
    end;
  end;
end;

function TGMatrix.Get(Y: TGMatrixIndexY; X: TGMatrixIndexX): TGMatrixItem;
begin
  Result := FItems[Y, X];
end;

procedure TGMatrix.Put(Y: TGMatrixIndexY; X: TGMatrixIndexX; const AValue: TGMatrixItem);
begin
  FItems[Y, X] := AValue;
end;

procedure TGMatrix.SetCount(const AValue: TGMatrixIndex);
begin
  Capacity := AValue;;
  FCount := AValue;
end;

procedure TGMatrix.Assign(Source: TGMatrix);
var
  X: TGMatrixIndexX;
  Y: TGMatrixIndexY;
begin
  Count := Source.Count;
  Y := 0;
  while Y < Count.Y do begin
    while X < Count.X do begin
      Items[Y, X] := Source[Y, X];
      X := X + 1;
    end;
    Y := Y + 1;
  end;
end;

procedure TGMatrix.Expand;
var
  IncSize: TGMatrixIndex;
  NewCapacity: TGMatrixIndex;
begin
  if (FCount.X = Capacity.X) then begin
    IncSize.X := 4;
    if Capacity.X > 3 then IncSize.X := IncSize.X + 4;
    if Capacity.X > 8 then IncSize.X := IncSize.X + 8;
    if Capacity.X > 63 then IncSize.X := IncSize.X + Capacity.X shr 2;
    NewCapacity.X := Capacity.X + IncSize.X;
  end;
  if (FCount.Y = Capacity.Y) then begin
    IncSize.Y := 4;
    if Capacity.Y > 3 then IncSize.Y := IncSize.Y + 4;
    if Capacity.Y > 8 then IncSize.Y := IncSize.Y + 8;
    if Capacity.Y > 63 then IncSize.Y := IncSize.Y + Capacity.Y shr 2;
    NewCapacity.Y := Capacity.Y + IncSize.Y;
  end;
  Capacity := NewCapacity;
end;

procedure TGMatrix.Contract;
var
  NewCapacity: TGMatrixIndex;
begin
  if (Capacity.X > 256) and (FCount.X < Capacity.X shr 2) then
  begin
    NewCapacity.X := Capacity.X shr 1;
  end;
  if (Capacity.Y > 256) and (FCount.Y < Capacity.Y shr 2) then
  begin
    NewCapacity.Y := Capacity.Y shr 1;
  end;
  Capacity := NewCapacity;
end;

function TGMatrix.Extract(Item: TGMatrixItem): TGMatrixItem;
var
  I: TGMatrixIndex;
begin
(*  I := IndexOf(Item);
  if I >= 0 then begin
    Result := Item;
    Delete(I);
  end else
    raise EListError.CreateFmt(SListIndexError, [0]);
    *)
end;

function TGMatrix.IndexOf(Item: TGMatrixItem; Start: TGMatrixIndex): TGMatrixIndex;
begin
(*  Result := Start;
  while (Result < FCount) and
  not CompareMem(Addr(FItems[Result]), Addr(Item), SizeOf(TGMatrixItem)) do
    Result := Result + 1;
  if Result = FCount then Result := -1;
  *)
end;

procedure TGMatrix.Insert(Index: TGMatrixIndex; Item: TGMatrixItem);
begin
(*  if (Index < 0) or (Index > FCount ) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  if FCount = Capacity then Expand;
  if Index < FCount then
    System.Move(FItems[Index], FItems[Index + 1], (FCount - Index) * SizeOf(TGMatrixItem));
  FItems[Index] := Item;
  FCount := FCount + 1;
  *)
end;

procedure TGMatrix.InsertList(Index: TGMatrixIndex; List: TGMatrix);
var
  I: TGMatrixIndex;
begin
(*  I := 0;
  while (I < List.Count) do begin
    Insert(Index + I, List[I]);
    I := I + 1;
  end;
  *)
end;

function TGMatrix.IndexOfList(List: TGMatrix; Start: TGMatrixIndex): TGMatrixIndex;
var
  I: TGMatrixIndex;
begin
(*  if List.Count > 0 then begin
    Result := IndexOf(List[0], Start);
    if Result <> -1 then begin
      I := 1;
      while I < List.Count do begin
        if not CompareMem(Addr(FItems[Result + I]), Addr(List.FItems[I]), SizeOf(TGMatrixItem)) then begin
          Result := -1;
          Break;
        end;
        I := I + 1;
      end;
    end;
  end else Result := -1;
  *)
end;

function TGMatrix.GetLast: TGMatrixItem;
begin
(*  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    Result := Items[FCount - 1];
    *)
end;

procedure TGMatrix.SetLast(AValue: TGMatrixItem);
begin
(*  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    Items[FCount - 1] := AValue;
    *)
end;

function TGMatrix.GetFirst: TGMatrixItem;
begin
(*  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    Result := Items[0];
    *)
end;

procedure TGMatrix.SetFirst(AValue: TGMatrixItem);
begin
(*  if FCount = 0 then
    raise EListError.CreateFmt(SListIndexError, [0])
  else
    Items[0] := AValue;
    *)
end;

procedure TGMatrix.Move(CurIndex, NewIndex: TGMatrixIndex);
var
  Temp: TGMatrixItem;
begin
(*  if ((CurIndex < 0) or (CurIndex > Count - 1)) then
    raise EListError.CreateFmt(SListIndexError, [CurIndex]);
  if ((NewIndex < 0) or (NewIndex > Count -1)) then
    raise EListError.CreateFmt(SlistIndexError, [NewIndex]);
  Temp := FItems[CurIndex];
  if NewIndex > CurIndex then begin
    System.Move(FItems[CurIndex + 1], FItems[CurIndex], (NewIndex - CurIndex) * SizeOf(TGMatrixItem));
  end else
  if NewIndex < CurIndex then begin
    System.Move(FItems[NewIndex], FItems[NewIndex + 1], (CurIndex - NewIndex) * SizeOf(TGMatrixItem));
  end;
  FItems[NewIndex] := Temp;
  //Delete(CurIndex);
  //Insert(NewIndex, Temp);*)
end;

procedure TGMatrix.MoveItems(CurIndex, NewIndex, Count: TGMatrixIndex);
var
  S: Integer;
  D: Integer;
begin
(*  if CurIndex < NewIndex then begin
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
  end;*)
end;

function TGMatrix.Remove(Item: TGMatrixItem): TGMatrixIndex;
begin
(*  Result := IndexOf(Item);
  if Result <> -1 then
    Delete(Result); *)
end;

function TGMatrix.EqualTo(List: TGMatrix): Boolean;
var
  I: TGMatrixIndex;
begin
(*  Result := Count = List.Count;
  if Result then begin
    I := 0;
    while I < Count do begin
      if not CompareMem(Addr(FItems[I]), Addr(List.FItems[I]), SizeOf(TGMatrixItem)) then begin
        Result := False;
        Break;
      end;
      I := I + 1;
    end;
  end; *)
end;

procedure TGMatrix.Reverse;
var
  I: TGMatrixIndex;
begin
(*  I := 0;
  while I < (Count div 2) do begin
    Exchange(I, Count - 1 - I);
    I := I + 1;
  end; *)
end;

procedure TGMatrix.Sort(Compare: TGMatrixSortCompare);
begin
(*  if FCount > 1 then
    QuickSort(0, FCount - 1, Compare); *)
end;

procedure TGMatrix.AddArray(Values: array of TGMatrixItem);
var
  I: TGMatrixIndex;
begin
(*  I := 0;
  while I <= High(Values) do begin
    Add(Values[I]);
    I := I + 1;
  end; *)
end;

procedure TGMatrix.SetArray(Values: array of TGMatrixItem);
var
  I: TGMatrixIndex;
begin
(*  Clear;
  I := 0;
  while I <= High(Values) do begin
    Add(Values[I]);
    I := I + 1;
  end; *)
end;

procedure TGMatrix.InsertArray(Index: TGMatrixIndex; Values: array of TGMatrixItem);
var
  I: TGMatrixIndex;
begin
(*  I := 0;
  while I <= High(Values) do begin
    Insert(Index + I, Values[I]);
    I := I + 1;
  end; *)
end;

function TGMatrix.Implode(Separator: string; Converter: TGMatrixToStringConverter): string;
var
  I: TGMatrixIndex;
begin
(*  Result := '';
  I := 0;
  while I < Count do begin
    Result := Result + Converter(Items[I]);
    if I < (Count - 1) then
      Result := Result + Separator;
    I := I + 1;
  end; *)
end;

procedure TGMatrix.Explode(Text, Separator: string; Converter: TGMatrixFromStringConverter; SlicesCount: Integer = -1);
begin
(*  Clear;
  while (Pos(Separator, Text) > 0) and
  ((Count < (SlicesCount - 1)) or (SlicesCount = -1)) do begin
    Add(Converter(Copy(Text, 1, Pos(Separator, Text) - 1)));
    System.Delete(Text, 1, Pos(Separator, Text) + Length(Separator) - 1);
  end;
  Add(Converter(Text)); *)
end;

function TGMatrix.Add(Item: TGMatrixItem): TGMatrixIndex;
begin
(*  if FCount = Capacity then
    Self.Expand;
  FItems[FCount] := Item;
  Result := FCount;
  FCount := FCount + 1; *)
end;

procedure TGMatrix.AddList(List: TGMatrix);
var
  I: TGMatrixIndex;
begin
(*  I := 0;
  while I < List.Count do begin
    Add(List[I]);
    I := I + 1;
  end; *)
end;

procedure TGMatrix.Clear;
begin
  Count := CreateIndex(0, 0);
  Capacity := CreateIndex(0, 0);
end;

procedure TGMatrix.Delete(Index: TGMatrixIndex);
begin
(*  if (Index < 0) or (Index >= FCount) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  FCount := FCount - 1;
  System.Move(FItems[Index + 1], FItems[Index], (FCount - Index) * SizeOf(TGMatrixItem));
  Contract;
  *)
end;

procedure TGMatrix.DeleteItems(Index, Count: TGMatrixIndex);
var
  I: TGMatrixIndex;
begin
(*  I := Index;
  while I < (Index + Count) do begin
    Delete(Index);
    I := I + 1;
  end;
  *)
end;

procedure TGMatrix.Fill(Start, Count: TGMatrixIndex; Value: TGMatrixItem);
var
  X: TGMatrixIndexX;
  Y: TGMatrixIndexY;
begin
  Y := Start.Y;
  while Y < Count.Y do begin
    X := Start.X;
    while X < Count.X do begin
      Items[Y, X] := Value;
      X := X + 1;
    end;
    Y := Y + 1;
  end;
end;

procedure TGMatrix.Exchange(Index1, Index2: TGMatrixIndex);
var
  Temp: TGMatrixItem;
begin
  if ((Index1.Y >= FCount.Y) or (Index1.Y < 0)) then
    raise EListError.CreateFmt(SMatrixIndexError, [Index1.Y, Index1.X]);
  if ((Index2.Y >= FCount.Y) or (Index2.Y < 0)) then
    raise EListError.CreateFmt(SMatrixIndexError, [Index2.Y, Index2.X]);
  Temp := FItems[Index1.Y, Index1.X];
  FItems[Index1.Y, Index1.X] := FItems[Index2.Y, Index1.X];
  FItems[Index2.Y, Index2.X] := Temp;
end;

{$UNDEF IMPLEMENTATION}
{$ENDIF}
