unit StackObject;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TStackIndex = Integer;
  TStackItem = TObject;
{$INCLUDE '..\Generic\StackInterface.tpl'}

type

  { TObjectStack }

  TObjectStack = class(TGStack)
    //OwnObjects: Boolean;
    destructor Destroy; override;
  end;

implementation

{$INCLUDE '..\Generic\StackImplementation.tpl'}


{ TObjectStack }

destructor TObjectStack.Destroy;
begin
  inherited Destroy;
end;

end.
