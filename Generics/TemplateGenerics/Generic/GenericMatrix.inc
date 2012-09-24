// Work in progress...

{$IFDEF INTERFACE}

  // TGMatrix<TGMatrixIndex, TGMatrixIndex, TGMatrixItem> = class
  TGMatrix = class
  public
    type
      TSortCompare = function(const Item1, Item2: TGMatrixItem): Integer of object;
      TToStringConverter = function(Item: TGMatrixItem): string;
      TFromStringConverter = function(Text: string): TGMatrixItem;
      TRow = array of TGMatrixItem;
      TMerge = function(Item1, Item2: TGMatrixItem): TGMatrixItem of object;

      TIndex = record
        X: TGMatrixIndexX;
        Y: TGMatrixIndexY;
      end;
  private
    FItems: array of array of TGMatrixItem;
    FCount: TIndex;
    function GetItemXY(X: TGMatrixIndexX; Y: TGMatrixIndexY): TGMatrixItem;
    function GetItem(Index: TIndex): TGMatrixItem;
    function GetCapacity: TIndex;
    function GetLast: TGMatrixItem;
    function GetFirst: TGMatrixItem;
    procedure SetCapacity(const AValue: TIndex);
    procedure SetLast(AValue: TGMatrixItem);
    procedure SetFirst(AValue: TGMatrixItem);
    procedure PutItemXY(X: TGMatrixIndexX; Y: TGMatrixIndexY; const AValue: TGMatrixItem); virtual;
    procedure PutItem(Index: TIndex; const AValue: TGMatrixItem); virtual;
    procedure SetCount(const AValue: TIndex);
  public
    function Add(Item: TGMatrixItem): TIndex;
    procedure AddMatrix(Values: array of TRow);
    procedure AddList(List: TGMatrix);
    procedure Assign(Source: TGMatrix);
    procedure Clear; virtual;
    procedure Contract;
    function CreateIndex(X: TGMatrixIndexY; Y: TGMatrixIndexX): TIndex;
    procedure Delete(Index: TIndex); virtual;
    procedure DeleteItems(Index, Count: TIndex);
    function EqualTo(List: TGMatrix): Boolean;
    procedure Expand;
    function Extract(Item: TGMatrixItem): TGMatrixItem;
    procedure Exchange(Index1, Index2: TIndex);
    property First: TGMatrixItem read GetFirst write SetFirst;
    procedure FillAll(Value: TGMatrixItem);
    procedure Fill(Start, Count: TIndex; Value: TGMatrixItem);
    function Implode(RowSeparator, ColSeparator: string; Converter: TToStringConverter): string;
    procedure Explode(Text, Separator: string; Converter: TFromStringConverter; SlicesCount: Integer = -1);
    function IndexOf(Item: TGMatrixItem; Start: TIndex = 0): TIndex;
    function IndexOfList(List: TGMatrix; Start: TIndex = 0): TIndex;
    procedure Insert(Index: TIndex; Item: TGMatrixItem);
    procedure InsertList(Index: TIndex; List: TGMatrix);
    procedure InsertArray(Index: TIndex; Values: array of TGMatrixItem);
    procedure Move(CurIndex, NewIndex: TIndex);
    procedure MoveItems(CurIndex, NewIndex, Count: TIndex);
    procedure Merge(Index: TIndex; Source: TGMatrix; Proc: TMerge);
    procedure Replace(Index: TIndex; Source: TGMatrix);
    function Remove(Item: TGMatrixItem): TIndex;
    procedure Reverse;
    procedure ReverseHorizontal;
    procedure ReverseVertical;
    procedure Sort(Compare: TSortCompare);
    procedure SetArray(Values: array of TGMatrixItem);
    property Count: TIndex read FCount write SetCount;
    property Capacity: TIndex read GetCapacity write SetCapacity;
    property ItemsXY[X: TGMatrixIndexX; Y: TGMatrixIndexY]: TGMatrixItem
      read GetItemXY write PutItemXY; default;
    property Items[Index: TIndex]: TGMatrixItem
      read GetItem write PutItem;
    property Last: TGMatrixItem read GetLast write SetLast;
  end;

{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION_USES}

uses
  RtlConsts;

resourcestring
  SMatrixIndexError = 'Matrix index error [X: %d, Y: %d]';

{$UNDEF IMPLEMENTATION_USES}
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TGMatrix }

procedure TGMatrix.Replace(Index: TIndex; Source: TGMatrix);
var
  X: TGMatrixIndexX;
  Y: TGMatrixIndexY;
begin
  Y := 0;
  while Y < Source.Count.Y do begin
    X := 0;
    while X < Source.Count.X do begin
      ItemsXY[Index.X + X, Index.Y + Y] := Source.ItemsXY[X, Y];
      X := X + 1;
    end;
    Y := Y + 1;
  end;
end;

procedure TGMatrix.Merge(Index: TIndex; Source: TGMatrix; Proc: TMerge);
var
  X: TGMatrixIndexX;
  Y: TGMatrixIndexY;
begin
  Y := 0;
  while Y < Source.Count.Y do begin
    X := 0;
    while X < Source.Count.X do begin
      ItemsXY[Index.X + X, Index.Y + Y] := Proc(ItemsXY[Index.X + X, Index.Y + Y], Source.ItemsXY[X, Y]);
      X := X + 1;
    end;
    Y := Y + 1;
  end;
end;

function TGMatrix.CreateIndex(X: TGMatrixIndexY; Y: TGMatrixIndexX): TIndex;
begin
  Result.X := X;
  Result.Y := Y;
end;

function TGMatrix.GetCapacity: TIndex;
begin
  Result.Y := Length(FItems);
  if Result.Y > 0 then Result.X := Length(FItems[0]) else Result.X := 0;
end;

procedure TGMatrix.SetCapacity(const AValue: TIndex);
var
  Y: TGMatrixIndexY;
begin
  if (Capacity.X <> AValue.X) and (Capacity.Y <> AValue.Y) then begin
(*    SetLength(FItems, AValue.Y);
    Y := 0;
    while Y < Length(FItems) do begin
      SetLength(FItems[Y], AValue.X);
      Y := Y + 1;
    end;
  end;
  *)
    SetLength(FItems, AValue.Y, AValue.X);
  end;
end;

function TGMatrix.GetItemXY(X: TGMatrixIndexX; Y: TGMatrixIndexY): TGMatrixItem;
begin
  if (X < 0) or (X >= Count.X) or
    (Y < 0) or (Y >= Count.Y) then
    raise EListError.CreateFmt(SMatrixIndexError, [X, Y]);
  Result := FItems[Y, X];
end;

function TGMatrix.GetItem(Index: TIndex): TGMatrixItem;
begin
  if (Index.X < 0) or (Index.X >= Count.X) or
    (Index.Y < 0) or (Index.Y >= Count.Y) then
    raise EListError.CreateFmt(SMatrixIndexError, [Index.X, Index.Y]);
  Result := FItems[Index.Y, Index.X];
end;

procedure TGMatrix.PutItemXY(X: TGMatrixIndexX; Y: TGMatrixIndexY; const AValue: TGMatrixItem);
begin
  if (X < 0) or (X >= Count.X) or
    (Y < 0) or (Y >= Count.Y) then
    raise EListError.CreateFmt(SMatrixIndexError, [X, Y]);
  FItems[Y, X] := AValue;
end;

procedure TGMatrix.PutItem(Index: TIndex; const AValue: TGMatrixItem);
begin
  if (Index.X < 0) or (Index.X >= Count.X) or
    (Index.Y < 0) or (Index.Y >= Count.Y) then
    raise EListError.CreateFmt(SMatrixIndexError, [Index.X, Index.Y]);
  FItems[Index.Y, Index.X] := AValue;
end;

procedure TGMatrix.SetCount(const AValue: TIndex);
begin
  Capacity := AValue;
  FCount := AValue;
end;

procedure TGMatrix.Assign(Source: TGMatrix);
var
  Index: TIndex;
begin
  Count := Source.Count;
  Index.Y := 0;
  while Index.Y < Count.Y do begin
    Index.X := 0;
    while Index.X < Count.X do begin
      Items[Index] := Source.Items[Index];
      Index.X := Index.X + 1;
    end;
    Index.Y := Index.Y + 1;
  end;
end;

procedure TGMatrix.Expand;
var
  IncSize: TIndex;
  NewCapacity: TIndex;
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
  NewCapacity: TIndex;
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
  I: TIndex;
begin
(*  I := IndexOf(Item);
  if I >= 0 then begin
    Result := Item;
    Delete(I);
  end else
    raise EListError.CreateFmt(SListIndexError, [0]);
    *)
end;

function TGMatrix.IndexOf(Item: TGMatrixItem; Start: TIndex): TIndex;
begin
(*  Result := Start;
  while (Result < FCount) and
  not CompareMem(Addr(FItems[Result]), Addr(Item), SizeOf(TGMatrixItem)) do
    Result := Result + 1;
  if Result = FCount then Result := -1;
  *)
end;

procedure TGMatrix.Insert(Index: TIndex; Item: TGMatrixItem);
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

procedure TGMatrix.InsertList(Index: TIndex; List: TGMatrix);
var
  I: TIndex;
begin
(*  I := 0;
  while (I < List.Count) do begin
    Insert(Index + I, List[I]);
    I := I + 1;
  end;
  *)
end;

function TGMatrix.IndexOfList(List: TGMatrix; Start: TIndex): TIndex;
var
  I: TIndex;
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

procedure TGMatrix.Move(CurIndex, NewIndex: TIndex);
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

procedure TGMatrix.MoveItems(CurIndex, NewIndex, Count: TIndex);
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

function TGMatrix.Remove(Item: TGMatrixItem): TIndex;
begin
(*  Result := IndexOf(Item);
  if Result <> -1 then
    Delete(Result); *)
end;

function TGMatrix.EqualTo(List: TGMatrix): Boolean;
var
  I: TIndex;
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
  X: TGMatrixIndexX;
  Y: TGMatrixIndexY;
begin
  Y := 0;
  while Y < (Count.Y - 1) do begin
    X := 1 + Y;
    while X < Count.X do begin
      Exchange(CreateIndex(X, Y), CreateIndex(Y, X));
      X := X + 1;
    end;
    Y := Y + 1;
  end;
end;

procedure TGMatrix.ReverseHorizontal;
var
  X: TGMatrixIndexX;
  Y: TGMatrixIndexY;
begin
  Y := 0;
  while Y < Count.Y do begin
    X := 0;
    while X < (Count.X div 2) do begin
      Exchange(CreateIndex(X, Y), CreateIndex(Count.X - 1 - X, Y));
      X := X + 1;
    end;
    Y := Y + 1;
  end;
end;

procedure TGMatrix.ReverseVertical;
var
  X: TGMatrixIndexX;
  Y: TGMatrixIndexY;
begin
  X := 0;
  while X < Count.X do begin
    Y := 0;
    while Y < (Count.Y div 2) do begin
      Exchange(CreateIndex(X, Y), CreateIndex(X, Count.Y - 1 - Y));
      Y := Y + 1;
    end;
    X := X + 1;
  end;
end;

procedure TGMatrix.Sort(Compare: TSortCompare);
begin
(*  if FCount > 1 then
    QuickSort(0, FCount - 1, Compare); *)
end;

procedure TGMatrix.AddMatrix(Values: array of TRow);
var
  I: TIndex;
begin
(*  I := 0;
  while I <= High(Values) do begin
    Add(Values[I]);
    I := I + 1;
  end; *)
end;

procedure TGMatrix.SetArray(Values: array of TGMatrixItem);
var
  I: TIndex;
begin
(*  Clear;
  I := 0;
  while I <= High(Values) do begin
    Add(Values[I]);
    I := I + 1;
  end; *)
end;

procedure TGMatrix.InsertArray(Index: TIndex; Values: array of TGMatrixItem);
var
  I: TIndex;
begin
(*  I := 0;
  while I <= High(Values) do begin
    Insert(Index + I, Values[I]);
    I := I + 1;
  end; *)
end;

function TGMatrix.Implode(RowSeparator, ColSeparator: string; Converter: TToStringConverter): string;
var
  Y: TGMatrixIndexY;
  X: TGMatrixIndexX;
begin
  Result := '';
  Y := 0;
  while Y < Count.Y do begin
    X := 0;
    while X < Count.X do begin
      Result := Result + Converter(ItemsXY[X, Y]);
      if X < (Count.X - 1) then
        Result := Result + ColSeparator;
      X := X + 1;
    end;
    if Y < (Count.Y - 1) then
      Result := Result + RowSeparator;
    Y := Y + 1;
  end;
end;

procedure TGMatrix.Explode(Text, Separator: string; Converter: TFromStringConverter; SlicesCount: Integer = -1);
begin
(*  Clear;
  while (Pos(Separator, Text) > 0) and
  ((Count < (SlicesCount - 1)) or (SlicesCount = -1)) do begin
    Add(Converter(Copy(Text, 1, Pos(Separator, Text) - 1)));
    System.Delete(Text, 1, Pos(Separator, Text) + Length(Separator) - 1);
  end;
  Add(Converter(Text)); *)
end;

function TGMatrix.Add(Item: TGMatrixItem): TIndex;
begin
(*  if FCount = Capacity then
    Self.Expand;
  FItems[FCount] := Item;
  Result := FCount;
  FCount := FCount + 1; *)
end;

procedure TGMatrix.AddList(List: TGMatrix);
var
  I: TIndex;
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

procedure TGMatrix.Delete(Index: TIndex);
begin
(*  if (Index < 0) or (Index >= FCount) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  FCount := FCount - 1;
  System.Move(FItems[Index + 1], FItems[Index], (FCount - Index) * SizeOf(TGMatrixItem));
  Contract;
  *)
end;

procedure TGMatrix.DeleteItems(Index, Count: TIndex);
var
  I: TIndex;
begin
(*  I := Index;
  while I < (Index + Count) do begin
    Delete(Index);
    I := I + 1;
  end;
  *)
end;

procedure TGMatrix.Fill(Start, Count: TIndex; Value: TGMatrixItem);
var
  X: TGMatrixIndexX;
  Y: TGMatrixIndexY;
begin
  Y := Start.Y;
  while Y < Count.Y do begin
    X := Start.X;
    while X < Count.X do begin
      ItemsXY[X, Y] := Value;
      X := X + 1;
    end;
    Y := Y + 1;
  end;
end;

procedure TGMatrix.FillAll(Value: TGMatrixItem);
begin
  Fill(CreateIndex(0, 0), CreateIndex(Count.X - 1, Count.Y - 1), Value);
end;

procedure TGMatrix.Exchange(Index1, Index2: TIndex);
var
  Temp: TGMatrixItem;
begin
  if (Index1.X < 0) or (Index1.X >= Count.X) or
    (Index1.Y < 0) or (Index1.Y >= Count.Y) then
    raise EListError.CreateFmt(SMatrixIndexError, [Index1.X, Index1.Y]);
  if (Index2.X < 0) or (Index2.X >= Count.X) or
    (Index2.Y < 0) or (Index2.Y >= Count.Y) then
    raise EListError.CreateFmt(SMatrixIndexError, [Index2.X, Index2.Y]);
  Temp := FItems[Index1.Y, Index1.X];
  FItems[Index1.Y, Index1.X] := FItems[Index2.Y, Index2.X];
  FItems[Index2.Y, Index2.X] := Temp;
end;

{$UNDEF IMPLEMENTATION}
{$ENDIF}
