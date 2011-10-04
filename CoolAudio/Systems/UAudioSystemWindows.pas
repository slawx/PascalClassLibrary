unit UAudioSystemWindows;

{$mode objfpc}{$H+}

interface

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

implementation

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

end.

