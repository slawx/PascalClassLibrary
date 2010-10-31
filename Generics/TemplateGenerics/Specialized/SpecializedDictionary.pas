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
{$DEFINE TGDictionaryStringConverter := TDictionaryStringConverterInteger}
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericDictionary.inc'}

implementation

// TDictionaryStringString<Integer, TPair<string, string>>
{$DEFINE TGDictionaryIndex := Integer}
{$DEFINE TGPair := TPairStringString}
{$DEFINE TGPairKey := string}
{$DEFINE TGPairValue := string}
{$DEFINE TGDictionary := TDictionaryStringString}
{$DEFINE TGDictionaryList := TDictionaryList}
{$DEFINE TGDictionarySortCompare := TDictionarySortCompareInteger}
{$DEFINE TGDictionaryStringConverter := TDictionaryStringConverterInteger}
{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericDictionary.inc'}

end.
