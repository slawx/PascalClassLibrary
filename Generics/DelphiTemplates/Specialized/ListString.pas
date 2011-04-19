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

implementation

{$DEFINE IMPLEMENTATION_USES}
{$I 'GenericList.inc'}

{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}


end.
