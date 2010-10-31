unit DictionaryStringString;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TDictionaryIndex = Integer;
  TDictionaryKey = string;
  TDictionaryValue = string;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericDictionary.inc'}

type
  TPairStringString = TGPair;

  TDictionaryStringString = class(TGDictionary)
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericDictionary.inc'}


end.
