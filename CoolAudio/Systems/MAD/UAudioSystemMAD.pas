unit UAudioSystemMAD;

{$I UCoolAudioConfig.inc}
{$mode delphi}{$H+}

interface

{$IFDEF AudioSystemMAD}
uses
  Classes, SysUtils, mad, UAudioSystem;

type
  TAudioSystemMAD = class(TAudioSystem)
  public
  end;

  { TPlayerMAD }

  TPlayerMAD = class(TPlayer)
  public
    procedure Play; override;
    procedure Pause; override;
    procedure Stop; override;
  end;

{$ENDIF}

implementation

{$IFDEF AudioSystemMAD}

{ TPlayerMAD }

procedure TPlayerMAD.Play;
begin
  inherited Play;

end;

procedure TPlayerMAD.Pause;
begin
  inherited Pause;
end;

procedure TPlayerMAD.Stop;
begin
  inherited Stop;
end;

{$ENDIF}

end.

