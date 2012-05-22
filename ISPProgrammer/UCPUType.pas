unit UCPUType;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TCPUType = (ctUnknown, ctAVR8, ctAVR32, ctAT82S8253, ctAT89S8252, ctAT89S52,
    ctDS89C450, ctRC3000, ctARM);

var
  CPUTypeText: array[TCPUType] of string;

resourcestring
  SCPUTypeUnknown = 'Unknown';
  SCPUTypeAVR8 = 'AVR8';
  SCPUTypeAVR32 = 'AVR32';
  SCPUTypeAT89S8252 = 'AT89S8252';
  SCPUTypeAT82S8253 = 'AT82S8253';
  SCPUTypeAT89S52 = 'AT89S52';
  SCPUTypeDS89C450 = 'DS89C450';
  SCPUTypeRC3000 = 'RC3000';
  SCPUTypeARM = 'ARM';


procedure UpdateTranslation;

implementation

procedure UpdateTranslation;
begin
  CPUTypeText[ctUnknown] := SCPUTypeUnknown;
  CPUTypeText[ctAVR8] := SCPUTypeAVR8;
  CPUTypeText[ctAVR32] := SCPUTypeAVR32;
  CPUTypeText[ctDS89C450] := SCPUTypeDS89C450;
  CPUTypeText[ctAT89S8252] := SCPUTypeAT89S8252;
  CPUTypeText[ctAT82S8253] := SCPUTypeAT82S8253;
  CPUTypeText[ctAT89S52] := SCPUTypeAT89S52;
  CPUTypeText[ctRC3000] := SCPUTypeRC3000;
  CPUTypeText[ctARM] := SCPUTypeARM;
end;

end.

