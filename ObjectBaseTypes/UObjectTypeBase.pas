unit UObjectTypeBase;

interface

uses
  Classes, SysUtils;

type
  ENotImplemented = class(Exception);

  IAssignable = interface
    procedure Assign(Source: TInterfacedObject);
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
