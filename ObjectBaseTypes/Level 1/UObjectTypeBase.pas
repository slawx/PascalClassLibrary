unit UObjectTypeBase;

interface

uses
  Classes, SysUtils;

type
  ENotImplemented = class(Exception);

  IAssignable = interface
    procedure Assign(Source: TInterfacedObject);
  end;

  ISortable = interface
    procedure Sort;
  end;

  IStreamable = interface
    procedure GetStream(Stream: TStream);
    procedure SetStrem(Stream: TStream);
  end;

implementation

end.
