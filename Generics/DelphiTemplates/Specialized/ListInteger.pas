unit ListInteger;

interface

uses
  Classes, SysUtils;

type

  // TListInteger<Integer, Integer>
  TGListIndex = Integer;
  TGListItem = Integer;
  {$DEFINE INTERFACE}
  {$I 'GenericList.inc'}

  TListInteger = class(TGList)
  end;

implementation

{$DEFINE IMPLEMENTATION_USES}
{$I 'GenericList.inc'}

{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}


end.
