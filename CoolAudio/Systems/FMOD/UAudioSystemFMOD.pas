unit UAudioSystemFMOD;

{$I UCoolAudioConfig.inc}
{$mode delphi}{$H+}

interface

{$IFDEF AudioSystemFMOD}
uses
  Classes, SysUtils, fmoddyn, fmodtypes, UAudioSystem, DateUtils;

type

  { TAudioSystemFMOD }

  TAudioSystemFMOD = class(TAudioSystem)
  private
    procedure SetOutputMode(AValue: TOutputDriver); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TPlayerFMOD }

  TPlayerFMOD = class(TPlayer)
  private
    FHandle: PFSoundStream;
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

{$ENDIF}


implementation

{$IFDEF AudioSystemFMOD}

{ TAudioSystemFMOD }

procedure TAudioSystemFMOD.SetOutputMode(AValue: TOutputDriver);
begin
  inherited SetOutputMode(AValue);
  {$ifdef linux}
    if FOutputDriver = omOSS then begin
      if not FSOUND_SetOutput(FSOUND_OUTPUT_OSS) then EOpenOutputFailed.Create(SOpenOutputFailed);
    end else
    if FOutputDriver = omALSA then begin
      if not FSOUND_SetOutput(FSOUND_OUTPUT_ALSA) then raise EOpenOutputFailed.Create(SOpenOutputFailed);
    end else raise EOpenOutputFailed.Create(SOpenOutputFailed);
  {$endif}
end;

constructor TAudioSystemFMOD.Create(AOwner: TComponent);
begin
  inherited;
  fmod_load('');
  OutputMode := omALSA;
  FSOUND_Init(44100, 32, 0);
end;

destructor TAudioSystemFMOD.Destroy;
begin
  FMOD_Unload;
  inherited Destroy;
end;

{ TPlayerFMOD }

function TPlayerFMOD.GetLength: TDateTime;
begin
  Result := FVolume;
end;

function TPlayerFMOD.GetPosition: TDateTime;
begin

end;

function TPlayerFMOD.GetVolume: Real;
begin
  Result := FSOUND_GetVolume(0) / 256;
end;

function TPlayerFMOD.GetMuted: Boolean;
begin
  Result := FSOUND_GetMute(0);
end;

procedure TPlayerFMOD.SetPosition(AValue: TDateTime);
begin
  if FPlaying then FSOUND_Stream_SetPosition(FHandle, Trunc(AValue / OneMillisecond));
end;

procedure TPlayerFMOD.SetVolume(AValue: Real);
begin
  FSOUND_SetVolume(0, Trunc(AValue * 256));
end;

procedure TPlayerFMOD.SetMuted(AValue: Boolean);
begin
  FSOUND_SetMute(0, AValue)
end;

procedure TPlayerFMOD.Play;
begin
  //FHandle := FSOUND_Stream_Open(tmpp, FSOUND_NONBLOCKING, 0, 0);
  FPlaying := True;
end;

procedure TPlayerFMOD.Pause;
begin
  if FPlaying then
    FSOUND_Setpaused(0, not FSOUND_Getpaused(0));
end;

procedure TPlayerFMOD.Stop;
begin
  if FPlaying then begin
    FSOUND_Stream_Stop(FHandle);
    FSOUND_Stream_Close(FHandle);
    FPlaying := False;
  end;
  FSOUND_Close;
end;

{$ENDIF}

end.

