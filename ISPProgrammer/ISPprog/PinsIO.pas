unit PinsIO;

{$MODE Delphi}

interface

procedure RstOn;
procedure RstOff;
procedure StrobeOn;
procedure StrobeOff;
procedure ChipselectOn;
procedure ChipselectOff;
function  ReadRst:boolean;
procedure ClkLo;
procedure ClkHi;
procedure Send0;
procedure Send1;
procedure LedOn;
procedure LedOff;
function  ReadBit:boolean;
procedure PinoutChanged;

implementation

uses Globals, PortsIO;

var
  last_data, last_control:byte;

procedure SetDataBit(nr:byte);
begin
  last_data:=last_data or (1 shl nr);
  OutPort(BASE + DataPort, last_data);
end;

procedure ClearDataBit(nr:byte);
begin
  last_data:=last_data and (255-(1 shl nr));
  OutPort(BASE + DataPort, last_data);
end;

function GetDataBit(nr:byte):boolean;
begin
  Result:=((last_data and (1 shl nr)) <> 0);
end;

procedure SetControlBit(nr:byte);
begin
  last_control:=last_control or (1 shl nr);
  OutPort(BASE + ControlPort, last_control);
end;

procedure ClearControlBit(nr:byte);
begin
  last_control:=last_control and (255-(1 shl nr));
  OutPort(BASE + ControlPort, last_control);
end;

function GetControlBit(nr:byte):boolean;
begin
  Result:=((last_control and (1 shl nr)) <> 0);
end;

function GetStatusBit(nr:byte):boolean;
begin
  Result:=((InPort(BASE + StatusPort) and (1 shl nr)) <> 0);
end;

procedure SetLPTBit(nr:byte);
begin
  case nr of
    LPT_OUT_STROBE:   ClearControlBit(0); // inverted
    LPT_OUT_D0:       SetDataBit(0);
    LPT_OUT_D1:       SetDataBit(1);
    LPT_OUT_D2:       SetDataBit(2);
    LPT_OUT_D3:       SetDataBit(3);
    LPT_OUT_D4:       SetDataBit(4);
    LPT_OUT_D5:       SetDataBit(5);
    LPT_OUT_D6:       SetDataBit(6);
    LPT_OUT_D7:       SetDataBit(7);
    LPT_OUT_AUTOLF:   ClearControlBit(1); // inverted
    LPT_OUT_INIT:     SetControlBit(2);
    LPT_OUT_SELECTIN: ClearControlBit(3); // inverted
  end;
end;

procedure ClearLPTBit(nr:byte);
begin
  case nr of
    LPT_OUT_STROBE:   SetControlBit(0);   // inverted
    LPT_OUT_D0:       ClearDataBit(0);
    LPT_OUT_D1:       ClearDataBit(1);
    LPT_OUT_D2:       ClearDataBit(2);
    LPT_OUT_D3:       ClearDataBit(3);
    LPT_OUT_D4:       ClearDataBit(4);
    LPT_OUT_D5:       ClearDataBit(5);
    LPT_OUT_D6:       ClearDataBit(6);
    LPT_OUT_D7:       ClearDataBit(7);
    LPT_OUT_AUTOLF:   SetControlBit(1);   // inverted
    LPT_OUT_INIT:     ClearControlBit(2);
    LPT_OUT_SELECTIN: SetControlBit(3);   // inverted
  end;
end;

function GetLPTBit(nr:byte):boolean;
begin
  case nr of
    LPT_IN_ACK:      Result:=GetStatusBit(6);
    LPT_IN_BUSY:     Result:=not GetStatusBit(7); // inverted
    LPT_IN_PAPEREND: Result:=GetStatusBit(5);
    LPT_IN_SELECT:   Result:=GetStatusBit(4);
    LPT_IN_ERROR:    Result:=GetStatusBit(3);
    else Result:=false;
  end;
end;

procedure RstOn;
var v:boolean;
begin
  v:=(proctype = PROC_TYPE_AVR) xor (not pinout.resetinv);
  if v then
    SetLPTBit(pinout.reset)
  else
    ClearLPTBit(pinout.reset);
end;

procedure RstOff;
var v:boolean;
begin
  v:=(proctype = PROC_TYPE_AVR) xor pinout.resetinv;
  if v then
    SetLPTBit(pinout.reset)
  else
    ClearLPTBit(pinout.reset);
end;

procedure StrobeOn;
begin
  ClearLPTBit(pinout.strobe1);
  ClearLPTBit(pinout.strobe2);
end;

procedure StrobeOff;
begin
  SetLPTBit(pinout.strobe1);
  SetLPTBit(pinout.strobe2);
end;

procedure ChipselectOn;
begin
  ClearLPTBit(pinout.reset);
end;

procedure ChipselectOff;
begin
  SetLPTBit(pinout.reset);
end;

function ReadRst:boolean;
begin
  case pinout.reset of
    LPT_OUT_STROBE:   Result:=not GetControlBit(0);   // inverted
    LPT_OUT_D0:       Result:=GetDataBit(0);
    LPT_OUT_D1:       Result:=GetDataBit(1);
    LPT_OUT_D2:       Result:=GetDataBit(2);
    LPT_OUT_D3:       Result:=GetDataBit(3);
    LPT_OUT_D4:       Result:=GetDataBit(4);
    LPT_OUT_D5:       Result:=GetDataBit(5);
    LPT_OUT_D6:       Result:=GetDataBit(6);
    LPT_OUT_D7:       Result:=GetDataBit(7);
    LPT_OUT_AUTOLF:   Result:=not GetControlBit(1);   // inverted
    LPT_OUT_INIT:     Result:=GetControlBit(2);
    LPT_OUT_SELECTIN: Result:=not GetControlBit(3);   // inverted
    else Result:=false;
  end;
end;

procedure ClkLo;
begin
  ClearLPTBit(pinout.sck);
end;

procedure ClkHi;
begin
  SetLPTBit(pinout.sck);
end;

procedure Send0;
begin
  ClearLPTBit(pinout.mosi);
end;

procedure Send1;
begin
  SetLPTBit(pinout.mosi);
end;

procedure LedOn;
begin
  ClearLPTBit(pinout.led);
end;

procedure LedOff;
begin
  SetLPTBit(pinout.led);
end;

function ReadBit:boolean;
begin
  Result:=GetLPTBit(pinout.miso);
end;

procedure PinoutChanged;
begin
  last_data:=InPort(BASE + DataPort);
  last_control:=InPort(BASE + ControlPort);
end;

end.
