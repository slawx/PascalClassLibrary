
  TGDictionary = class;

  TGPair = record
    Key: TDictionaryKey;
    Value: TDictionaryValue;
  end;

  TListIndex = TDictionaryIndex;
  TListItem = TGPair;
  {$INCLUDE 'ListInterface.tpl'}

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