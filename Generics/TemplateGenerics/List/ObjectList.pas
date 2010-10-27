unit ObjectList;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TListIndex = Integer;
  TListItem = TObject;
{$INCLUDE 'GenericListInterface.tpl'}

type

  { TObjectList }

  TObjectList = class(TGList)
    //OwnObjects: Boolean;
    destructor Destroy; override;
  end;

implementation

{$INCLUDE 'GenericListImplementation.tpl'}


{ TObjectList }

destructor TObjectList.Destroy;
begin
  inherited Destroy;
end;

end.
