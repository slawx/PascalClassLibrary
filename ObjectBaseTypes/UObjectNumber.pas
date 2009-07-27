unit UObjectNumber;

interface

uses
  UObjectTypeBase, UObjectBoolean;

type
  IOrderable = interface
    function HigherThen(Operand: IOrderable): TBoolean;
    function LowerThan(Operand: IOrderable): TBoolean;
    function Max(Operand1, Operand2: IOrderable): IOrderable;
    function Min(Operand1, Operand2: IOrderable): IOrderable;
  end;

  IOrdinal = interface
    function Ordinal: IOrdinal;
    function Predecessor: IOrdinal;
    function Successor: IOrdinal;
    function Low: IOrdinal;
    function High: IOrdinal;
  end;

implementation

end.
