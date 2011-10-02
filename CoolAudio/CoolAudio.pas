{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CoolAudio; 

interface

uses
  fmod, fmoddyn, fmoderrors, fmodpresets, fmodtypes, UCoolAudio, 
  UAudioSystemFMOD, UPlaylist, UAudioSystemMPlayer, UAudioSystem, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('CoolAudio', @Register); 
end.
