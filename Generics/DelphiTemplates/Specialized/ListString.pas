unit ListString;

interface

uses
  Classes, SysUtils;

type
  // TListString<Integer, String>
  TGListIndex = Integer;
  TGListItem = String;
  {$DEFINE INTERFACE}
  {$I 'GenericList.inc'}

  TListString = class(TGList)
  end;

function StrToStr(Value: string): string;

implementation

{$DEFINE IMPLEMENTATION_USES}
{$I 'GenericList.inc'}

function StrToStr(Value: string): string;
begin
  Result := Value;
end;

{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}


end.
