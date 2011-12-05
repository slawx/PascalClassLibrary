unit SpecializedDictionary;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
{$MACRO ON}

// TDictionaryStringString<Integer, TPair<string, string>>
{$DEFINE TGDictionaryIndex := Integer}
{$DEFINE TGPair := TPairStringString}
{$DEFINE TGPairKey := string}
{$DEFINE TGPairValue := string}
{$DEFINE TGDictionary := TDictionaryStringString}
{$DEFINE TGDictionaryList := TDictionaryStringStringList}
{$DEFINE TGDictionarySortCompare := TDictionaryStringStringSortCompare}
{$DEFINE TGDictionaryToStringConverter := TDictionaryStringStringToStringConverter}
{$DEFINE TGDictionaryFromStringConverter := TDictionaryStringStringFromStringConverter}
{$DEFINE TGDictionaryItemArray := TDictionaryStringStringItemArray}
{$DEFINE INTERFACE}
{$I 'GenericDictionary.inc'}

// TDictionaryIntegerString<Integer, TPair<Integer, string>>
{$DEFINE TGDictionaryIndex := Integer}
{$DEFINE TGPair := TPairIntegerString}
{$DEFINE TGPairKey := Integer}
{$DEFINE TGPairValue := string}
{$DEFINE TGDictionary := TDictionaryIntegerString}
{$DEFINE TGDictionaryList := TDictionaryIntegerStringList}
{$DEFINE TGDictionarySortCompare := TDictionaryIntegerStringSortCompare}
{$DEFINE TGDictionaryToStringConverter := TDictionaryIntegerStringToStringConverter}
{$DEFINE TGDictionaryFromStringConverter := TDictionaryIntegerStringFromStringConverter}
{$DEFINE TGDictionaryItemArray := TDictionaryIntegerStringItemArray}
{$DEFINE INTERFACE}
{$I 'GenericDictionary.inc'}


implementation

{$DEFINE IMPLEMENTATION_USES}
{$I '..\Generic\GenericDictionary.inc'}


// TDictionaryStringString<Integer, TPair<string, string>>
{$DEFINE TGDictionaryIndex := Integer}
{$DEFINE TGPair := TPairStringString}
{$DEFINE TGPairKey := string}
{$DEFINE TGPairValue := string}
{$DEFINE TGDictionary := TDictionaryStringString}
{$DEFINE TGDictionaryList := TDictionaryStringStringList}
{$DEFINE TGDictionarySortCompare := TDictionaryStringStringSortCompare}
{$DEFINE TGDictionaryToStringConverter := TDictionaryStringStringToStringConverter}
{$DEFINE TGDictionaryFromStringConverter := TDictionaryStringStringFromStringConverter}
{$DEFINE TGDictionaryItemArray := TDictionaryStringStringItemArray}
{$DEFINE IMPLEMENTATION}
{$I 'GenericDictionary.inc'}

// TDictionaryIntegerString<Integer, TPair<Integer, string>>
{$DEFINE TGDictionaryIndex := Integer}
{$DEFINE TGPair := TPairIntegerString}
{$DEFINE TGPairKey := Integer}
{$DEFINE TGPairValue := string}
{$DEFINE TGDictionary := TDictionaryIntegerString}
{$DEFINE TGDictionaryList := TDictionaryIntegerStringList}
{$DEFINE TGDictionarySortCompare := TDictionaryIntegerStringSortCompare}
{$DEFINE TGDictionaryToStringConverter := TDictionaryIntegerStringToStringConverter}
{$DEFINE TGDictionaryFromStringConverter := TDictionaryIntegerStringFromStringConverter}
{$DEFINE TGDictionaryItemArray := TDictionaryIntegerStringItemArray}
{$DEFINE IMPLEMENTATION}
{$I 'GenericDictionary.inc'}


end.
