unit DictionaryString;

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
  TPairString = TGPair;

  TDictionaryString = class(TGDictionary)
  end;

implementation

{$INCLUDE '..\Generic\DictionaryImplementation.tpl'}


end.
