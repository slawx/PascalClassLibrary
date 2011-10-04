unit UAudioSystemWindows;

{$mode objfpc}{$H+}

interface

{$IFDEF Windows}
uses
  Classes, SysUtils, UAudioSystem, MMSystem;

type

  { TPlayerWindows }

  TPlayerWindows = class(TPlayer)
  private
    procedure Play; override;
    procedure Pause; override;
    procedure Stop; override;
  end;
{$ENDIF}

implementation

{$IFDEF Windows}

{ TPlayerWindows }

procedure TPlayerWindows.Play;
begin
  PlaySound();
  sndPlaySound(FFileName, SND_ASYNC);
end;

procedure TPlayerWindows.Pause;
begin
  inherited Pause;
end;

procedure TPlayerWindows.Stop;
begin
  sndPlaySound(nil, 0);
end;

{$ENDIF}

end.
