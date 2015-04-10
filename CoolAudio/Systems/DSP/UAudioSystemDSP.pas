unit UAudioSystemDSP;

{$I UCoolAudioConfig.inc}
{$mode delphi}{$H+}

interface

{$IFDEF AudioSystemDSP}
uses
  Classes, SysUtils, UAudioSystem, BaseUnix, UWavFile, ExtCtrls;

type

  { TAudioSystemDSP }

  TAudioSystemDSP = class(TAudioSystem)
  private
    FDeviceId: Integer;
  public
    Channels: Integer;
    SampleRate: Integer; // Hz
    BitsPerSample: Integer;
    function GetMediaPlayerDriverClass: TMediaPlayerDriverClass; override;
    procedure OpenDevice;
    procedure CloseDevice;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TPlayerDSP }

  TPlayerDSP = class(TMediaPlayerDriver)
  private
    FTimer: TTimer;
    procedure TimerExecute(Sender: TObject);
  public
    WavFile: TWAVFile;
    procedure Play; override;
    procedure Pause; override;
    procedure Stop; override;
    procedure Open; override;
    procedure Close; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

const
  BUFFERLEN = 1024;

const
  { Values obtained from a C program - These are complex (!) C macros }
  SNDCTL_DSP_STEREO = -1073459197;
  SNDCTL_DSP_RESET = 20480;
  SNDCTL_DSP_SYNC = 20481;
  SOUND_PCM_WRITE_BITS = -1073459195;
  SOUND_PCM_WRITE_CHANNELS = -1073459194;
  SOUND_PCM_WRITE_RATE = -1073459198;

resourcestring
  SPlaying = 'Playing : ';
  SErrChannels = 'Error : Number of channels not supported: ';
  SErrOpeningDevice = 'Could not open sound device';
  SErrSettingStereo = 'Could not set stereo';
  SErrResettingDevice = 'Could not reset DSP device';
  SErrSetWriteBits = 'Could not set write bits to 16';
  SErrSetChannels = 'Could not set channels';
  SErrSetSampleRate = 'Could not set sync mode';
  SErrSetSyncMode = 'Could not set sync mode';

{$ENDIF}

implementation

{$IFDEF AudioSystemDSP}

{ TAudioSystemDSP }

function TAudioSystemDSP.GetMediaPlayerDriverClass: TMediaPlayerDriverClass;
begin
  Result := TPlayerDSP;
end;

procedure TAudioSystemDSP.OpenDevice;
var
  Stereo: Longint;
begin
  Write('Openning /dev/dsp');
  FDeviceId := fpOpen('/dev/dsp', O_WRONLY, 0);
  if FDeviceId < 0 then
    raise Exception.Create(SErrOpeningDevice);
  Stereo := 0;
  if not (FpIOCtl(FDeviceId, SNDCTL_DSP_STEREO, @Stereo) <> -1) then
    raise Exception.Create(SErrSettingStereo);
  if not (FpIOCtl(FDeviceId, SNDCTL_DSP_RESET, nil) <> -1) then
    raise Exception.Create(SErrResettingDevice);
  if not (FpIOCtl(FDeviceId, SOUND_PCM_WRITE_BITS, @BitsPerSample) <> -1) then
    raise Exception.Create(SErrSetWriteBits);
  if not (FpIOCtl(FDeviceId, SOUND_PCM_WRITE_CHANNELS, @Channels) <> -1) then
    raise Exception.Create(SErrSetChannels);
  if not (FpIOCtl(FDeviceId, SOUND_PCM_WRITE_RATE, @SampleRate) <> -1) then
    raise Exception.Create(SErrSetSampleRate);
  if not (FpIOCtl(FDeviceId, SNDCTL_DSP_SYNC, nil) <> -1) then
    raise Exception.Create(SErrSetSyncMode);
  WriteLn(' device: ' + IntToStr(FDeviceId));
end;

procedure TAudioSystemDSP.CloseDevice;
begin
  if FDeviceId >= 0 then FpClose(FDeviceId);
end;

constructor TAudioSystemDSP.Create(AOwner: TComponent);
begin
  inherited;
  BitsPerSample := 16;
  Channels := 2;
  SampleRate := 44000;
end;

destructor TAudioSystemDSP.Destroy;
begin
  inherited Destroy;
end;

{ TPlayerMAD }

procedure TPlayerDSP.TimerExecute(Sender: TObject);
var
  Buffer: array of Byte;
  Size: Integer;
begin
  SetLength(Buffer, 10000);
  Size := WavFile.SourceFile.Read(PByte(Buffer)^, System.Length(Buffer));
  Write('Buf: ' + IntToStr(Size) + ', ');
  fpWrite(TAudioSystemDSP(AudioSystem).FDeviceId, PByte(Buffer)^, Size);
  FTimer.Interval := 100;
end;

procedure TPlayerDSP.Play;
begin
  WriteLn('Stop');
  Stop;
  TAudioSystemDSP(AudioSystem).SampleRate := WavFile.SampleRate;
  TAudioSystemDSP(AudioSystem).Channels := WavFile.ChannelModeID;
  TAudioSystemDSP(AudioSystem).BitsPerSample := WavFile.BitsPerSample;
  TAudioSystemDSP(AudioSystem).OpenDevice;
  WriteLn('seek');
  WavFile.SourceFile.Seek(SizeOf(TWAVRecord), soFromBeginning);
  while WavFile.SourceFile.Position < WavFile.SourceFile.Size do
    TimerExecute(nil);
  WriteLn('int');
  FTimer.Interval := 10;
  FTimer.OnTimer := TimerExecute;
  WriteLn('ena');
  //FTimer.Enabled := True;
  WriteLn('true');
end;

procedure TPlayerDSP.Pause;
begin
  if FActive then FTimer.Enabled := not FTimer.Enabled;
end;

procedure TPlayerDSP.Stop;
begin
  inherited Stop;
  FTimer.Enabled := False;
  TAudioSystemDSP(AudioSystem).CloseDevice;
end;

procedure TPlayerDSP.Open;
begin
  inherited;
  WavFile.OpenFile(FFileName);
end;

procedure TPlayerDSP.Close;
begin
  inherited Close;
end;

constructor TPlayerDSP.Create;
begin
  inherited;
  WavFile := TWAVFile.Create;
  FTimer := TTimer.Create(nil);
end;

destructor TPlayerDSP.Destroy;
begin
  FreeAndNil(FTimer);
  FreeAndNil(WavFile);
  inherited Destroy;
end;

{$ENDIF}

end.
