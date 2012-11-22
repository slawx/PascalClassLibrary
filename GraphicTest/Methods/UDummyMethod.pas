unit UDummyMethod;

{$mode delphi}

interface

uses
  Classes, SysUtils, UDrawMethod, UFastBitmap;

type
  { TDummyMethod }

  TDummyMethod = class(TDrawMethod)
    constructor Create; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;


implementation

{ TDummyMethod }

constructor TDummyMethod.Create;
begin
  inherited Create;
  Caption := 'Dummy';
end;

procedure TDummyMethod.DrawFrame(FastBitmap: TFastBitmap);
begin
end;


end.

