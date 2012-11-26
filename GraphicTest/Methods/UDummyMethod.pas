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
  Description.Add('This method doesn''t draw anything. It''s purpose is to measure ' +
    'and compare speed of system and event handling.');
end;

procedure TDummyMethod.DrawFrame(FastBitmap: TFastBitmap);
begin
end;


end.

