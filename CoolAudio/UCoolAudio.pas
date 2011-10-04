unit UCoolAudio; 

{$mode Delphi}{$H+}

{$IFDEF Windows}
  {$DEFINE AudioSystemWindows}
{$ENDIF}
{$DEFINE AudioSystemMPlayer}
{$DEFINE AudioSystemFMOD}

interface

uses
  Classes, SysUtils,
  {$IFDEF AudioSystemWindows}
  UAudioSystemWindows,
  {$ENDIF}
  {$IFDEF AudioSystemMPlayer}
  UAudioSystemMPlayer,
  {$ENDIF}
  {$IFDEF AudioSystemFMOD}
  UAudioSystemFMOD,
  {$ENDIF}
  UAudioSystem;

var
  AudioSystemManager: TAudioSystemManager;

implementation

initialization

AudioSystemManager := TAudioSystemManager.Create;
{$IFDEF AudioSystemWindows}
AudioSystemManager.Register('Windows', TAudioSystemWindows, TPlayerWindows);
{$ENDIF}
{$IFDEF AudioSystemMPlayer}
AudioSystemManager.Register('MPlayer', TAudioSystemMPlayer, TPlayerMPlayer);
{$ENDIF}
{$IFDEF AudioSystemFMOD}
AudioSystemManager.Register('FMOD', TAudioSystemFMOD, TPlayerFMOD);
{$ENDIF}

finalization

AudioSystemManager.Free;

end.

