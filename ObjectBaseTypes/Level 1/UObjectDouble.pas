unit UObjectDouble;

interface

uses
  UObjectTypeBase, UObjectBoolean;

type
  TDouble = class(TInterfacedObject, IAssignable, IOrderable, IComparable)
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

{ TDouble }

procedure TDouble.Assign(Source: TInterfacedObject);
begin

end;

function TDouble.EqualTo(Operand: IComparable): TBoolean;
begin

end;

function TDouble.High: IOrderable;
begin

end;

function TDouble.HigherThen(Operand: IOrderable): TBoolean;
begin

end;

function TDouble.Low: IOrderable;
begin

end;

function TDouble.LowerThan(Operand: IOrderable): TBoolean;
begin

end;

function TDouble.Max(Operand1, Operand2: IOrderable): IOrderable;
begin

end;

function TDouble.Min(Operand1, Operand2: IOrderable): IOrderable;
begin

end;

function TDouble.Predecessor: IOrderable;
begin

end;

function TDouble.Successor: IOrderable;
begin

end;

end.
