unit ListObject;

interface

uses
  Classes, SysUtils;

type

  // TListInteger<Integer, Integer>
  TGListObjectIndex = Integer;
  TGListObjectItem = TObject;
  {$DEFINE INTERFACE}
  {$I 'GenericListObject.inc'}

  TListObject = class(TGListObject)
  end;

implementation

{$DEFINE IMPLEMENTATION_USES}
{$I 'GenericListObject.inc'}

{$DEFINE IMPLEMENTATION}
{$I 'GenericListObject.inc'}


end.
