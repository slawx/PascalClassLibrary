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

  TGDictionary<TKey, TValue> = class
  private
  type
    TIndex = NativeInt;
    TDictionaryPair = TGPair<TKey, TValue>;
  var
    FList: TGList<TDictionaryPair>;
    function GetKey(Index: TIndex): TKey;
    function GetValue(Key: TKey): TValue;
    procedure PutKey(Index: TIndex; const AValue: TKey);
    procedure PutValue(Key: TKey; const AValue: TValue);
  public
    constructor Create;
    destructor Destroy; override;
    function SearchKey(Key: TKey): TIndex;
    procedure Add(Key: TKey; Value: TValue);
    property Values[Index: TKey]: TValue
      read GetValue write PutValue;
    property Keys[Index: TIndex]: TKey
      read GetKey write PutKey;
    property List: TGList<TDictionaryPair> read FList;
  end;

implementation


constructor TGDictionary<TKey, TValue>.Create;
begin
  FList := TGList<TDictionaryPair>.Create;
end;

destructor TGDictionary<TKey, TValue>.Destroy;
begin
  FList.Free;
end;

function TGDictionary<TKey, TValue>.GetKey(Index: TIndex): TKey;
begin
  Result := FList.Items[Index].Key;
end;

function TGDictionary<TKey, TValue>.GetValue(Key: TKey): TValue;
begin
  Result := FList.Items[SearchKey(Key)].Value;
end;

procedure TGDictionary<TKey, TValue>.PutKey(Index: TIndex;
  const AValue: TKey);
var
  Item: TDictionaryPair;
begin
  //Items[Index].Key := AValue;
  Item := FList.Items[Index];
  Item.Key := AValue;
  FList.Items[Index] := Item;
end;

procedure TGDictionary<TKey, TValue>.PutValue(Key: TKey;
  const AValue: TValue);
var
  Item: TDictionaryPair;
  Index: TIndex;
begin
  //Items[SearchKey(Index)].Value := AValue;
  Index := SearchKey(Key);
  Item := FList.Items[Index];
  Item.Value := AValue;
  FList.Items[Index] := Item;
end;

function TGDictionary<TKey, TValue>.SearchKey(Key: TKey): TIndex;
begin
  Result := 0;
  while Result < FList.Count do begin
    if FList.Items[Result].Key = Key then begin
      Break;
    end;
    Result := Result + 1;
  end;
end;

procedure TGDictionary<TKey, TValue>.Add(Key: TKey; Value: TValue);
var
  NewPair: TDictionaryPair;
begin
  NewPair.Key := Key;
  NewPair.Value := Value;
  FList.Add(NewPair);
end;

end.
