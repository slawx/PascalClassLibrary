unit DictionaryStringString;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TDictionaryIndex = Integer;
  TDictionaryKey = string;
  TDictionaryValue = string;
{$INCLUDE '..\Generic\DictionaryInterface.tpl'}

type
  TPairStringString = TGPair;

  TDictionaryStringString = class(TGDictionary)
  end;

implementation

{$INCLUDE '..\Generic\DictionaryImplementation.tpl'}


end.
