{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CoolAudio;

interface

uses
  UCoolAudio, UPlaylist, UAudioSystem, UAudioSystemDSP, UAudioSystemFMOD, 
  UAudioSystemMAD, UAudioSystemWindows, UAudioSystemMPlayer, UWavFile, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UCoolAudio', @UCoolAudio.Register);
end;

initialization
  RegisterPackage('CoolAudio', @Register);
end.
