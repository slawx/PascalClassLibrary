unit UObjectByte;

interface

uses
  Classes, SysUtils, UObjectTypeBase, UObjectBoolean;

type
  TByte = class(TInterfacedObject, IAssignable, IOrderable, IComparable)
  public
    Value: Byte;
    procedure Assign(Source: TInterfacedObject);
    function HigherThen(Operand: IOrderable): TBoolean;
    function LowerThan(Operand: IOrderable): TBoolean;
    function Max(Operand1, Operand2: IOrderable): IOrderable;
    function Min(Operand1, Operand2: IOrderable): IOrderable;
    function EqualTo(Operand: IComparable): TBoolean;
    function Predecessor: IOrderable;
    function Successor: IOrderable;
    function Low: IOrderable;
    function High: IOrderable;
  end;

implementation

{ TByte }

procedure TByte.Assign(Source: TInterfacedObject);
begin
  if Source is TByte then Value := (Source as TByte).Value
  else raise EConvertError.Create('');
end;

function TByte.EqualTo(Operand: IComparable): TBoolean;
begin

end;

function TByte.High: IOrderable;
begin

end;

function TByte.HigherThen(Operand: IOrderable): TBoolean;
begin

end;

function TByte.Low: IOrderable;
begin

end;

function TByte.LowerThan(Operand: IOrderable): TBoolean;
begin

end;

function TByte.Max(Operand1, Operand2: IOrderable): IOrderable;
begin

end;

function TByte.Min(Operand1, Operand2: IOrderable): IOrderable;
begin

end;

function TByte.Predecessor: IOrderable;
begin

end;

function TByte.Successor: IOrderable;
begin

end;

end.
