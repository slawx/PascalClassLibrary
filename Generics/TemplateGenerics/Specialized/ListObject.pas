unit ListObject;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TListIndex = Integer;
  TListItem = TObject;
{$INCLUDE '..\Generic\ListInterface.tpl'}

type

  { TObjectList }

  TObjectList = class(TGList)
    //OwnObjects: Boolean;
    destructor Destroy; override;
  end;

implementation

{$INCLUDE '..\Generic\ListImplementation.tpl'}


{ TObjectList }

destructor TObjectList.Destroy;
begin
  inherited Destroy;
end;

end.
