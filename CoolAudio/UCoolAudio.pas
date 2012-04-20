unit UCoolAudio; 

{$mode Delphi}{$H+}

{$IFDEF Windows}
  {$DEFINE AudioSystemWindows}
{$ENDIF}
//{$DEFINE AudioSystemMPlayer}
//{$DEFINE AudioSystemFMOD}
//{$DEFINE AudioSystemDSP}
{$IFDEF Linux}
  //{$DEFINE AudioSystemMAD}
{$ENDIF}

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

implementation

initialization

AudioSystemManager := TAudioSystemManager.Create(nil);
{$IFDEF AudioSystemWindows}
AudioSystemManager.Register('Windows', TAudioSystemWindows, TPlayerWindows);
{$ENDIF}
{$IFDEF AudioSystemMPlayer}
AudioSystemManager.Register('MPlayer', TAudioSystemMPlayer, TPlayerMPlayer);
{$ENDIF}
{$IFDEF AudioSystemFMOD}
AudioSystemManager.Register('FMOD', TAudioSystemFMOD, TPlayerFMOD);
{$ENDIF}
{$IFDEF AudioSystemMAD}
AudioSystemManager.Register('MAD', TAudioSystemMAD, TPlayerMAD);
{$ENDIF}
{$IFDEF AudioSystemDSP}
AudioSystemManager.Register('DSP', TAudioSystemDSP, TPlayerDSP);
{$ENDIF}

finalization

AudioSystemManager.Free;

end.

