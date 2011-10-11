{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CoolAudio; 

interface

uses
  fmod, fmoddyn, fmoderrors, fmodpresets, fmodtypes, UCoolAudio, UPlaylist, 
  UAudioSystemFMOD, UAudioSystemMPlayer, UAudioSystemWindows, UAudioSystem, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('CoolAudio', @Register); 
end.
