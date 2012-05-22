unit UProgrammerType;

{$mode delphi}

interface

uses
  Classes, SysUtils, UISPProgrammer, UPresto, UISPprog, UDallasProgrammer,
  URFUProgrammer;

type
  TProgrammerType = (ptDallas, ptISPprog, ptPresto, ptRFU);
  TProgrammerTypeSet = set of TProgrammerType;
  TISPProgrammerClass = class of TISPProgrammer;

var
  ProgrammerTypeText: array[TProgrammerType] of string;

var
  ProgrammerTypeClass: array[TProgrammerType] of TISPProgrammerClass;

procedure UpdateTranslation;

resourcestring
  SProgrammerDallas = 'Dallas ISP (RS-232)';
  SProgrammerISPprog = 'ISPprog (LPT)';
  SProgrammerPRESTO = 'PRESTO (USB)';
  SProgrammerRFU = 'RFU (RS-232)';


implementation

procedure UpdateTranslation;
begin
  ProgrammerTypeText[ptDallas] := SProgrammerDallas;
  ProgrammerTypeText[ptISPprog] := SProgrammerISPprog;
  ProgrammerTypeText[ptPresto] := SProgrammerPRESTO;
  ProgrammerTypeText[ptRFU] := SProgrammerRFU;
end;

initialization

ProgrammerTypeClass[ptDallas] := TDallasProgrammer;
{$IFDEF Windows}
ProgrammerTypeClass[ptISPprog] := TISPProg;
ProgrammerTypeClass[ptPresto] := TPrestoProgrammer;
{$ENDIF}
ProgrammerTypeClass[ptRFU] := TRFUProgrammer;

end.
