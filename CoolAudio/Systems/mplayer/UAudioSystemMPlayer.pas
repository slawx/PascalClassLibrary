// MPlayer slave command list: http://www.mplayerhq.hu/DOCS/tech/slave.txt

unit UAudioSystemMPlayer;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, UAudioSystem, Process, Math, Dialogs, DateUtils;

const
{$ifdef Unix}
  MPlayerExecutableName = 'mplayer';
{$endif}
{$ifdef Windows}
  MPlayerExecutableName = 'mplayer.exe';
{$endif}

type
  { TAudioSystemMPlayer }

  TAudioSystemMPlayer = class(TAudioSystem)
  private
    FPath: string;
    procedure SetOutputMode(AValue: TOutputDriver); override;
  public
    function FindPath: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Path: string read FPath write FPath;
  end;

  { TPlayerMPlayer }

  TPlayerMPlayer = class(TPlayer)
  private
    FProcess: TProcess;
    FVolume: Real;
    function GetProcessOutput: string;
    procedure SendCommand(Command: string);
    function GetLength: TDateTime; override;
    function GetPosition: TDateTime; override;
    function GetVolume: Real; override;
    function GetMuted: Boolean; override;
    procedure SetPosition(AValue: TDateTime); override;
    procedure SetVolume(AValue: Real); override;
    procedure SetMuted(AValue: Boolean); override;
    procedure SetFileName(AValue: string); override;
  public
    procedure Play; override;
    procedure Pause; override;
    procedure Stop; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

resourcestring
  SMPlayerNotFound = 'MPlayer executable not found. Make sure it is properly installed in binary path';
  SSendCommandException = 'Exception occured during sending command to MPlayer';
  SErrorReadingOutput = 'Exception while reading MPlayer output';
  SCantStopProcess = 'Can''t stop Mplayer process';

implementation

function StrToFloatPoint(Value: string): Extended;
var
  FPointSeparator: TFormatSettings;
begin
  // Format seetings to convert a string to a float
  FPointSeparator := DefaultFormatSettings;
  FPointSeparator.DecimalSeparator := '.';
  FPointSeparator.ThousandSeparator := '#';// disable the thousand separator
  Result := StrToFloat(Value, FPointSeparator);
end;

function FloatPointToStr(Value: Extended): string;
var
  FPointSeparator: TFormatSettings;
begin
  // Format seetings to convert a string to a float
  FPointSeparator := DefaultFormatSettings;
  FPointSeparator.DecimalSeparator := '.';
  FPointSeparator.ThousandSeparator := '#';// disable the thousand separator
  Result := FloatToStr(Value, FPointSeparator);
end;

{ TAudioSystemMPlayer }

procedure TAudioSystemMPlayer.SetOutputMode(AValue: TOutputDriver);
begin
  inherited SetOutputMode(AValue);
end;

function TAudioSystemMPlayer.FindPath: string;
var
  tmps: string;
  tmppath: string;
  I: Integer;
begin
  Result := '';
  {$ifdef Darwin}
  {$else}
  // Searches for MPlayer in the PATH
  tmps := GetEnvironmentVariable('PATH');
  repeat
    I := Pos(PathSeparator, tmps);
    if I = 0 then I := Length(tmps);
    tmppath := IncludeTrailingPathDelimiter(Copy(tmps, 0, I - 1)) + MPlayerExecutableName;
    if FileExists(tmppath) then Result := tmppath
      else Delete(tmps, 1, I);
  until (Length(tmps) <= 1) or (Result <> '');
  {$endif}
  if Result = '' then raise Exception.Create(SMPlayerNotFound);
end;

constructor TAudioSystemMPlayer.Create(AOwner: TComponent);
begin
  inherited;
  FPath := FindPath;
end;

destructor TAudioSystemMPlayer.Destroy;
begin
  inherited Destroy;
end;

{ TPlayerMPlayer }

procedure TPlayerMPlayer.SendCommand(Command: string);
begin
  Command := Command + #10; // MPLayer always needs #10 as Lineending, no matter if win32 or linux
  try
    if FProcess.Running then FProcess.Input.Write(Command[1], System.Length(Command));
  except
    raise Exception.Create(SSendCommandException);
  end;
end;

function TPlayerMPlayer.GetLength: TDateTime;
var
  tmps: string;
  I: Integer;
  Time: Real;
begin
  if FPlaying and fProcess.Running then begin
    repeat
      SendCommand('get_time_length');
      Sleep(5);
      tmps := GetProcessOutput;
    until Pos('LENGTH', tmps) > 0;
    I := LastDelimiter('=', tmps);
    if I > 0 then begin
      Time := StrToFloatPoint(Copy(tmps, I + 1, System.Length(tmps)));
      Result := Time * OneSecond;
    end;
  end;
end;

function TPlayerMPlayer.GetPosition: TDateTime;
var
  tmps: string;
  I: Integer;
  Time: Real;
begin
  if FProcess.Running then begin
    I := 0;
    repeat
      SendCommand('get_property time_pos');
      Sleep(8);
      tmps := GetProcessOutput;
      Inc(I);
    until (Pos('time_pos', tmps) > 0) or (I >= 3);
    I := LastDelimiter('=', tmps);
    if I > 0 then begin
      Time := StrToFloatPoint(Copy(tmps, I + 1, System.Length(tmps)));
      Result := Time * OneSecond;
    end else Result := -1;
  end else Result := -1;
end;

function TPlayerMPlayer.GetVolume: Real;
begin
end;

function TPlayerMPlayer.GetMuted: Boolean;
var
  tmps, s: string;
  I: Integer;
begin
  if FPlaying and FProcess.Running then begin
    repeat
      SendCommand('get_property mute');
      Sleep(5);
      tmps := GetProcessOutput;
    until Pos('mute', tmps) > 0;
    i := LastDelimiter('=', tmps);
    if i > 0 then begin
      s := Copy(tmps, i + 1, System.Length(tmps) - i);
      Result := s = 'yes';
    end;
  end;
end;

procedure TPlayerMPlayer.SetPosition(AValue: TDateTime);
begin
  if FPlaying and FProcess.Running then begin
    SendCommand('set_property time_pos ' + FloatPointToStr(AValue / OneSecond));
  end;
end;

procedure TPlayerMPlayer.SetVolume(AValue: Real);
begin
  if FVolume = AValue then Exit;
  FVolume := AValue;
  if FPlaying and FProcess.Running then begin
    if AValue < 0 then AValue := 0;
    if AValue > 1 then AValue := 1;
    SendCommand('set_property volume ' + IntToStr(Round(AValue * 100)) + '/1');
  end;
end;

procedure TPlayerMPlayer.SetMuted(AValue: Boolean);
begin
  if FPlaying and FProcess.Running then
    SendCommand('mute');
end;

procedure TPlayerMPlayer.SetFileName(AValue: string);
begin
  inherited SetFileName(AValue);
end;

function IntTodB(I, Ref: Longint): Integer;
var
  dB: Real;
begin
  if I = 0 then db := 0.001 else dB := I;
  dB := 20 * log10(dB / ref);
  Result := Round(dB);
end;

procedure TPlayerMPlayer.Play;
var
  MPOptions: String;
  Vol: Real;
begin
  if FPlaying then Stop;
  //FProcess := TProcess.Create(nil);
  MPOptions := '-slave -quiet -softvol';
  if AudioSystem.OutputMode = omAlsa then MPOptions := MPOptions + ' -ao alsa';
  if AudioSystem.OutputMode = omOSS then MPOptions := MPOptions + ' -ao oss';
  if AudioSystem.OutputMode = omWin32 then MPOptions := MPOptions + ' -ao win32';
  if AudioSystem.OutputMode = omDirectX then MPOptions := MPOptions + ' -ao dsound';

  //MPOptions := '-af volume=' + IntToStr(IntTodB(Round(FVolume * 100), 100)) + ' ' + MPOptions;// -volume xx only supported with patched mplayer;

  FProcess.CommandLine := TAudioSystemMPlayer(AudioSystem).FPath + ' ' + MPOptions + ' "' + UTF8Decode(FFileName) + '"';
  FProcess.Options := FProcess.Options + [poUsePipes, poDefaultErrorMode, poStderrToOutPut, poNoConsole];
  //InputBox('', '', FProcess.CommandLine);
  FProcess.Execute;

  if FProcess.Running then begin
    FPlaying := True;
  end;
end;

procedure TPlayerMPlayer.Pause;
begin
  if FPlaying then begin
    SendCommand('pause');
    Sleep(10);
    //FPaused := not FPaused;
  end;
end;

procedure TPlayerMPlayer.Stop;
begin
  if FPlaying then begin
    SendCommand('quit');
    Sleep(15);
    if FProcess.Running then begin
      Sleep(50);
      if FProcess.Running then
        if not FProcess.Terminate(0) then
          raise Exception.Create(SCantStopProcess);
    end;
  end;
  FPlaying := False;
end;

constructor TPlayerMPlayer.Create(AOwner: TComponent);
begin
  inherited;
  FProcess := TProcess.Create(nil);
end;

destructor TPlayerMPlayer.Destroy;
begin
  Stop;
  FProcess.Free;
  inherited Destroy;
end;

function TPlayerMPlayer.GetProcessOutput: string;
var
  AStringList: TStringList;
begin
  try
    AStringList:=TStringList.Create;
    try
      if FProcess.Running then AStringList.LoadFromStream(FProcess.Output);
      if AStringList.Count > 0 then
        Result := AStringList.Strings[0]
        else Result := '';
    except
      Result := '';
      raise Exception.Create(SErrorReadingOutput);
    end;
  finally
    AStringList.Free;
  end;
end;

end.

