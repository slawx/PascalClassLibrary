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

  { TListObject }

  TListObject = class(TGList)
  private
    procedure Put(Index: TListIndex; const AValue: TListItem); override;
  public
    OwnObjects: Boolean;
    procedure Delete(Index: TListIndex); override;
    procedure Clear; override;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{$INCLUDE '..\Generic\ListImplementation.tpl'}


{ TObjectList }

procedure TListObject.Put(Index: TListIndex; const AValue: TListItem);
begin
  if OwnObjects then FItems[Index].Free;
  inherited Put(Index, AValue);
end;

procedure TListObject.Delete(Index: TListIndex);
begin
  if OwnObjects then FItems[Index].Free;
  inherited Delete(Index);
end;

procedure TListObject.Clear;
var
  I: TListIndex;
begin
  if OwnObjects then begin
    I := 0;
    while I < Count do begin
      FItems[I].Free;
      I := I + 1;
    end;
  end;
  inherited Clear;
end;

constructor TListObject.Create;
begin
  inherited;
  OwnObjects := True;
end;

destructor TListObject.Destroy;
begin
  Clear;
  inherited Destroy;
end;

end.
