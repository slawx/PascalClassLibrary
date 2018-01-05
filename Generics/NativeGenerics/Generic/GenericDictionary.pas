unit GenericDictionary;

{$mode delphi}

interface

uses
  GenericList, fgl;

type
  TGPair<TKey, TValue> = record
    Key: TKey;
    Value: TValue;
  end;

  // Construction of complext generic types not supported by FPC:
  //   TGDictionay<TKey, TValue> = class(TGList<TGPair<TKey, TValue>>)
  TGDictionary<TKey, TValue> = class
  private
    type
      TPair = TGPair<TKey, TValue>;
    var
    FList: TGList<TPair>;
    function GetCount: Integer;
    function GetKey(Index: Integer): TKey;
    function GetValue(Key: TKey): TValue;
    procedure SetCount(const AValue: Integer);
    procedure PutKey(Index: Integer; const AValue: TKey);
    procedure PutValue(Key: TKey; const AValue: TValue);
  public
    function SearchKey(Key: TKey): Integer;
    procedure Add(Key: TKey; Value: TValue);
    constructor Create;
    destructor Destroy; override;
    property Values[Index: TKey]: TValue
      read GetValue write PutValue;
    property Keys[Index: Integer]: TKey
      read GetKey write PutKey;
    property List: TGList<TPair> read FList;
    property Count: Integer read GetCount write SetCount;
  end;


implementation

constructor TGDictionary<TKey, TValue>.Create;
begin
  FList := TGList<TPair>.Create;
end;

destructor TGDictionary<TKey, TValue>.Destroy;
begin
  FList.Free;
end;

function TGDictionary<TKey, TValue>.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TGDictionary<TKey, TValue>.SetCount(const AValue: Integer);
begin
  FList.Count := AValue;
end;

function TGDictionary<TKey, TValue>.GetKey(Index: Integer): TKey;
begin
  Result := FList.Items[Index].Key;
end;

function TGDictionary<TKey, TValue>.GetValue(Key: TKey): TValue;
begin
  Result := FList.Items[SearchKey(Key)].Value;
end;

procedure TGDictionary<TKey, TValue>.PutKey(Index: Integer;
  const AValue: TKey);
var
  Item: TPair;
begin
  //Items[Index].Key := AValue;
  Item := FList.Items[Index];
  Item.Key := AValue;
  FList.Items[Index] := Item;
end;

procedure TGDictionary<TKey, TValue>.PutValue(Key: TKey;
  const AValue: TValue);
var
  Item: TPair;
  Index: Integer;
begin
  //Items[SearchKey(Index)].Value := AValue;
  Index := SearchKey(Key);
  Item := FList.Items[Index];
  Item.Value := AValue;
  FList.Items[Index] := Item;
end;

function TGDictionary<TKey, TValue>.SearchKey(Key: TKey): Integer;
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
  NewPair: TPair;
begin
  NewPair.Key := Key;
  NewPair.Value := Value;
  FList.Add(NewPair);
end;

end.
