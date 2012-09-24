{$IFDEF INTERFACE}

  TGDictionary = class;

  TGPair = record
    Key: TGPairKey;
    Value: TGPairValue;
  end;

{$DEFINE TGListIndex := TGDictionaryIndex}
{$DEFINE TGListItem := TGPair}
{$DEFINE TGList := TGDictionaryList}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

  // TGDictionary<TGDictionaryIndex, TGPair<TGPairKey, TGPairValue>> = class(TGList)
  TGDictionary = class(TGList)
  private
    function GetKey(Index: TGDictionaryIndex): TGPairKey;
    function GetValue(Key: TGPairKey): TGPairValue;
    procedure PutKey(Index: TGDictionaryIndex; const AValue: TGPairKey);
    procedure PutValue(Key: TGPairKey; const AValue: TGPairValue);
  public
    function SearchKey(Key: TGPairKey): TGDictionaryIndex;
    procedure Add(Key: TGPairKey; Value: TGPairValue);
    property Values[Index: TGPairKey]: TGPairValue
      read GetValue write PutValue;
    property Keys[Index: TGDictionaryIndex]: TGPairKey
      read GetKey write PutKey;
  end;

{$UNDEF INTERFACE}
{$ENDIF}


{$IFDEF IMPLEMENTATION_USES}
{$I '..\Generic\GenericList.inc'}
{$UNDEF IMPLEMENTATION_USES}
{$ENDIF}


{$IFDEF IMPLEMENTATION}

{$DEFINE TGListIndex := TGDictionaryIndex}
{$DEFINE TGListItem := TGPair}
{$DEFINE TGList := TGDictionaryList}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

function TGDictionary.GetKey(Index: TGDictionaryIndex): TGPairKey;
begin
  Result := Items[Index].Key;
end;

function TGDictionary.GetValue(Key: TGPairKey): TGPairValue;
begin
  Result := Items[SearchKey(Key)].Value;
end;

procedure TGDictionary.PutKey(Index: TGDictionaryIndex;
  const AValue: TGPairKey);
var
  Item: TGPair;
begin
  //Items[Index].Key := AValue;
  Item := Items[Index];
  Item.Key := AValue;
  Items[Index] := Item;
end;

procedure TGDictionary.PutValue(Key: TGPairKey;
  const AValue: TGPairValue);
var
  Item: TGPair;
  Index: TGDictionaryIndex;
begin
  //Items[SearchKey(Index)].Value := AValue;
  Index := SearchKey(Key);
  Item := Items[Index];
  Item.Value := AValue;
  Items[Index] := Item;
end;

function TGDictionary.SearchKey(Key: TGPairKey): TGDictionaryIndex;
begin
  Result := 0;
  while Result < Count do begin
    if Items[Result].Key = Key then begin
      Break;
    end;
    Result := Result + 1;
  end;
end;

procedure TGDictionary.Add(Key: TGPairKey; Value: TGPairValue);
var
  NewPair: TGPair;
begin
  NewPair.Key := Key;
  NewPair.Value := Value;
  inherited Add(NewPair);
end;

{$UNDEF IMPLEMENTATION}
{$ENDIF}
