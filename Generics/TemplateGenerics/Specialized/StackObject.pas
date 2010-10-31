unit StackObject;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TStackIndex = Integer;
  TStackItem = TObject;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericStack.inc'}

type

  { TObjectStack }

  TObjectStack = class(TGStack)
    //OwnObjects: Boolean;
    destructor Destroy; override;
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericStack.inc'}

{ TObjectStack }

destructor TObjectStack.Destroy;
begin
  inherited Destroy;
end;

end.
