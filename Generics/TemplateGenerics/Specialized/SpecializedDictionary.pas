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
{$DEFINE TGDictionaryList := TDictionaryList}
{$DEFINE TGDictionarySortCompare := TDictionarySortCompareInteger}
{$DEFINE TGDictionaryToStringConverter := TDictionaryToStringConverterInteger}
{$DEFINE TGDictionaryFromStringConverter := TDictionaryFromStringConverterInteger}
{$DEFINE TGDictionaryItemArray := TDictionaryStringItemArray}
{$DEFINE INTERFACE}
{$I 'GenericDictionary.inc'}

implementation

// TDictionaryStringString<Integer, TPair<string, string>>
{$DEFINE TGDictionaryIndex := Integer}
{$DEFINE TGPair := TPairStringString}
{$DEFINE TGPairKey := string}
{$DEFINE TGPairValue := string}
{$DEFINE TGDictionary := TDictionaryStringString}
{$DEFINE TGDictionaryList := TDictionaryList}
{$DEFINE TGDictionarySortCompare := TDictionarySortCompareInteger}
{$DEFINE TGDictionaryToStringConverter := TDictionaryToStringConverterInteger}
{$DEFINE TGDictionaryFromStringConverter := TDictionaryFromStringConverterInteger}
{$DEFINE TGDictionaryItemArray := TDictionaryStringItemArray}
{$DEFINE IMPLEMENTATION}
{$I 'GenericDictionary.inc'}

end.
