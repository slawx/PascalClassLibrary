
{$INCLUDE 'ListImplementation.tpl'}

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