{$IFDEF INTERFACE}

  TGDictionary = class;

  TGPair = record
    Key: TDictionaryKey;
    Value: TDictionaryValue;
  end;

  TListIndex = TDictionaryIndex;
  TListItem = TGPair;
  {$DEFINE INTERFACE}
  {$INCLUDE 'GenericList.inc'}

  // TGDictionary<TDictionaryIndex, TDictionaryKey, TDictionaryValue> = class(TGList)
  TGDictionary = class(TGList)
  private
    function GetKey(Index: TDictionaryIndex): TDictionaryKey;
    function GetValue(Key: TDictionaryKey): TDictionaryValue;
    procedure PutKey(Index: TDictionaryIndex; const AValue: TDictionaryKey);
    procedure PutValue(Key: TDictionaryKey; const AValue: TDictionaryValue);
  public
    function SearchKey(Key: TDictionaryKey): TDictionaryIndex;
    procedure Add(Key: TDictionaryKey; Value: TDictionaryValue);
    property Values[Index: TDictionaryKey]: TDictionaryValue
      read GetValue write PutValue;
    property Keys[Index: TDictionaryIndex]: TDictionaryKey
      read GetKey write PutKey;
  end;
{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION}


{$DEFINE IMPLEMENTATION}
{$INCLUDE 'GenericList.inc'}

function TGDictionary.GetKey(Index: TDictionaryIndex): TDictionaryKey;
begin
  Result := Items[Index].Key;
end;

function TGDictionary.GetValue(Key: TDictionaryKey): TDictionaryValue;
begin
  Result := Items[SearchKey(Key)].Value;
end;

procedure TGDictionary.PutKey(Index: TDictionaryIndex;
  const AValue: TDictionaryKey);
var
  Item: TGPair;
begin
  //Items[Index].Key := AValue;
  Item := Items[Index];
  Item.Key := AValue;
  Items[Index] := Item;
end;

procedure TGDictionary.PutValue(Key: TDictionaryKey;
  const AValue: TDictionaryValue);
var
  Item: TGPair;
  Index: TDictionaryIndex;
begin
  //Items[SearchKey(Index)].Value := AValue;
  Index := SearchKey(Key);
  Item := Items[Index];
  Item.Value := AValue;
  Items[Index] := Item;
end;

function TGDictionary.SearchKey(Key: TDictionaryKey): TDictionaryIndex;
begin
  Result := 0;
  while Result < Count do begin
    if Items[Result].Key = Key then begin
      Break;
    end;
    Result := Result + 1;
  end;
end;

procedure TGDictionary.Add(Key: TDictionaryKey; Value: TDictionaryValue);
var
  NewPair: TGPair;
begin
  NewPair.Key := Key;
  NewPair.Value := Value;
  inherited Add(NewPair);
end;

{$ENDIF}
