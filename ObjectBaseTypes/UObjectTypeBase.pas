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
    function GetStream: TStream;
    procedure SetStrem(Stream: TStream);
  end;

implementation

end.
