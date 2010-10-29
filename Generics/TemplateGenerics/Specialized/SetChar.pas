unit SetChar;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TSetIndex = Integer;
  TSetItem = Char;
{$INCLUDE '..\Generic\SetInterface.tpl'}

type

  { TSetChar }

  TSetChar = class(TGSet)
  end;

implementation

{$INCLUDE '..\Generic\SetImplementation.tpl'}


function TGSet.IsIn(Item: TSetItem): Boolean;
begin
  Result := FList.IndexOf(Item) <> -1;
end;

constructor TGSet.Create;
begin
  FList := TGList.Create;
end;

destructor TGSet.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

end.
