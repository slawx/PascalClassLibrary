unit GenericTree; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

implementation

type
  generic TGPoint<TDimensionType> = class 
    Coordinates: array[] of TDimensionType;
  end;

end.

