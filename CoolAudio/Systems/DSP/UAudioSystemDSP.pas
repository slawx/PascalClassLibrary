unit UAudioSystemDSP;

{$mode delphi}{$H+}

interface

//{$IFDEF AudioSystemDSP}
uses
  Classes, SysUtils, UAudioSystem, BaseUnix;

type

  { TAudioSystemDSP }

  TAudioSystemDSP = class(TAudioSystem)
  private
    FDeviceId: Integer;
  public
    procedure OpenDevice;
  end;

  { TPlayerDSP }

  TPlayerDSP = class(TPlayer)
  public
    procedure Play; override;
    procedure Pause; override;
    procedure Stop; override;
  end;

//{$ENDIF}

implementation

{ TAudioSystemDSP }

procedure TAudioSystemDSP.OpenDevice;
var
  fd, Stereo, Temp, Error: Longint;
begin
  fd := fpOpen('/dev/dsp', O_WRONLY, 0);
  if fd < 0 then
    PlayError(SErrOpeningDevice);
  Stereo := 0;
  if not (FpIOCtl(fd, SNDCTL_DSP_STEREO, @Stereo) <> -1) then
    PlayError(SErrSettingStereo);
  if not (FpIOCtl(fd, SNDCTL_DSP_RESET, nil) <> -1) then
    PlayError(SErrResettingDevice);
  Temp := 16;
  if not (FpIOCtl(fd, SOUND_PCM_WRITE_BITS, @Temp) <> -1) then
    PlayError(SErrSetWriteBits);
  if not (FpIOCtl(fd, SOUND_PCM_WRITE_CHANNELS, @Channels) <> -1) then
    PlayError(SErrSetChannels);
  if not (FpIOCtl(fd, SOUND_PCM_WRITE_RATE, @SampleRate) <> -1) then
    PlayError(SErrSetSampleRate);
  if not (FpIOCtl(fd, SNDCTL_DSP_SYNC, nil) <> -1) then
    PlayError(SErrSetSyncMode);
  FDeviceId := Fd;
end;

//{$IFDEF AudioSystemDSP}

{ TPlayerMAD }

procedure TPlayerDSP.Play;
begin
  inherited Play;

end;

procedure TPlayerDSP.Pause;
begin
  inherited Pause;
end;

procedure TPlayerDSP.Stop;
begin
  inherited Stop;
end;

//{$ENDIF}


end.

