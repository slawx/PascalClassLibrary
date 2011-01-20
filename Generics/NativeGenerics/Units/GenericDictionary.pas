unit GenericDictionary;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, GenericList;

type
  TGPair<TKey, TValue> = record
    Key: TKey;
    Value: TValue;
  end;

  TGDictionary<TPair> = class(TGList<TPair>)
  private
  type
    TGDictionaryIndex = Integer;
  var
    function GetKey(Index: TGDictionaryIndex): TPair.TKey;
    function GetValue(Key: TPair.TKey): TPair.TValue;
    procedure PutKey(Index: TGDictionaryIndex; const AValue: TPair.TKey);
    procedure PutValue(Key: TPair.TKey; const AValue: TPair.TValue);
  public
    function SearchKey(Key: TPair.TKey): TGDictionaryIndex;
    procedure Add(Key: TPair.TKey; Value: TPair.TValue);
    property Values[Index: TPair.TKey]: TPair.TValue
      read GetValue write PutValue;
    property Keys[Index: TGDictionaryIndex]: TPair.TKey
      read GetKey write PutKey;
  end;

implementation


function TGDictionary<TPair>.GetKey(Index: TGDictionaryIndex): TPair.TKey;
begin
  Result := Items[Index].Key;
end;

function TGDictionary<TPair>.GetValue(Key: TPair.TKey): TPair.TValue;
begin
  Result := Items[SearchKey(Key)].Value;
end;

procedure TGDictionary<TPair>.PutKey(Index: TGDictionaryIndex;
  const AValue: TPair.TKey);
var
  Item: TPair;
begin
  //Items[Index].Key := AValue;
  Item := Items[Index];
  Item.Key := AValue;
  Items[Index] := Item;
end;

procedure TGDictionary<TPair>.PutValue(Key: TPair.TKey;
  const AValue: TPair.TValue);
var
  Item: TPair;
  Index: TGDictionaryIndex;
begin
  //Items[SearchKey(Index)].Value := AValue;
  Index := SearchKey(Key);
  Item := Items[Index];
  Item.Value := AValue;
  Items[Index] := Item;
end;

function TGDictionary<TPair>.SearchKey(Key: TPair.TKey): TGDictionaryIndex;
begin
  Result := 0;
  while Result < Count do begin
    if Items[Result].Key = Key then begin
      Break;
    end;
    Result := Result + 1;
  end;
end;

procedure TGDictionary<TPair>.Add(Key: TPair.TKey; Value: TPair.TValue);
var
  NewPair: TPair;
begin
  NewPair.Key := Key;
  NewPair.Value := Value;
  inherited Add(NewPair);
end;

end.
