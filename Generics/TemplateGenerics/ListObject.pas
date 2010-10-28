unit ListObject;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TListIndex = Integer;
  TListItem = TObject;
{$INCLUDE 'ListGenericInterface.tpl'}

type

  { TObjectList }

  TObjectList = class(TGList)
    //OwnObjects: Boolean;
    destructor Destroy; override;
  end;

implementation

{$INCLUDE 'ListGenericImplementation.tpl'}


{ TObjectList }

destructor TObjectList.Destroy;
begin
  inherited Destroy;
end;

end.
