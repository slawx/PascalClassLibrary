unit UAudioSystemMPlayer;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, UAudioSystem;

type
  { TAudioSystemMPlayer }

  TAudioSystemMPlayer = class(TAudioSystem)
  private
    procedure SetOutputMode(AValue: TOutputDriver); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TPlayerMPlayer }

  TPlayerMPlayer = class(TPlayer)
  private
    FVolume: Real;
    function GetLength: TDateTime; override;
    function GetPosition: TDateTime; override;
    function GetVolume: Real; override;
    function GetMuted: Boolean; override;
    procedure SetPosition(AValue: TDateTime); override;
    procedure SetVolume(AValue: Real); override;
    procedure SetMuted(AValue: Boolean); override;
  public
    procedure Play; override;
    procedure Pause; override;
    procedure Stop; override;
  end;


implementation

{ TAudioSystemMPlayer }

procedure TAudioSystemMPlayer.SetOutputMode(AValue: TOutputDriver);
begin
  inherited SetOutputMode(AValue);
end;

constructor TAudioSystemMPlayer.Create;
begin
  inherited Create;
end;

destructor TAudioSystemMPlayer.Destroy;
begin
  inherited Destroy;
end;

{ TPlayerMPlayer }

function TPlayerMPlayer.GetLength: TDateTime;
begin
  Result:=inherited GetLength;
end;

function TPlayerMPlayer.GetPosition: TDateTime;
begin
  Result:=inherited GetPosition;
end;

function TPlayerMPlayer.GetVolume: Real;
begin
  Result:=inherited GetVolume;
end;

function TPlayerMPlayer.GetMuted: Boolean;
begin
  Result:=inherited GetMuted;
end;

procedure TPlayerMPlayer.SetPosition(AValue: TDateTime);
begin
  inherited SetPosition(AValue);
end;

procedure TPlayerMPlayer.SetVolume(AValue: Real);
begin
  inherited SetVolume(AValue);
end;

procedure TPlayerMPlayer.SetMuted(AValue: Boolean);
begin
  inherited SetMuted(AValue);
end;

procedure TPlayerMPlayer.Play;
begin
  inherited Play;
end;

procedure TPlayerMPlayer.Pause;
begin
  inherited Pause;
end;

procedure TPlayerMPlayer.Stop;
begin
  inherited Stop;
end;

end.

