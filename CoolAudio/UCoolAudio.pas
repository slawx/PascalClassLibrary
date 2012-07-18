unit UCoolAudio; 

{$I UCoolAudioConfig.inc}

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils,
  {$IFDEF AudioSystemWindows}UAudioSystemWindows,{$ENDIF}
  {$IFDEF AudioSystemMPlayer}UAudioSystemMPlayer,{$ENDIF}
  {$IFDEF AudioSystemFMOD}UAudioSystemFMOD,{$ENDIF}
  {$IFDEF AudioSystemMAD}UAudioSystemMAD,{$ENDIF}
  {$IFDEF AudioSystemDSP}UAudioSystemDSP,{$ENDIF}
  UAudioSystem;

var
  AudioSystemManager: TAudioSystemManager;

procedure Register;


implementation

uses
  UPlayList;

procedure Register;
begin
  RegisterComponents('CoolAudio', [TMediaPlayer, TPlayList]);
end;

initialization

AudioSystemManager := TAudioSystemManager.Create(nil);
{$IFDEF AudioSystemWindows}AudioSystemManager.Register('Windows', TAudioSystemWindows);{$ENDIF}
{$IFDEF AudioSystemMPlayer}AudioSystemManager.Register('MPlayer', TAudioSystemMPlayer);{$ENDIF}
{$IFDEF AudioSystemFMOD}AudioSystemManager.Register('FMOD', TAudioSystemFMOD);{$ENDIF}
{$IFDEF AudioSystemMAD}AudioSystemManager.Register('MAD', TAudioSystemMAD);{$ENDIF}
{$IFDEF AudioSystemDSP}AudioSystemManager.Register('DSP', TAudioSystemDSP);{$ENDIF}

finalization

AudioSystemManager.Free;

end.

