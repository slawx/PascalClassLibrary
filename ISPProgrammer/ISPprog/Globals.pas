unit Globals;

{$MODE Delphi}

interface

{$INCLUDE Options.inc}

uses Dialogs, Forms;

type
  TLPTpinout = packed record
    strobe1:byte; // out
    strobe2:byte; // out
    reset:byte;   // out
    mosi:byte;    // out
    sck:byte;     // out
    miso:byte;    // in
    led:byte;     // out
    resetinv:boolean;      // is RESET inverted
  end;
  PLPTpinout = ^TLPTpinout;

const
  DataPort    = 0;         // offset from BASE
  StatusPort  = 1;         // offset from BASE
  ControlPort = 2;         // offset from BASE

  // processor/memory types
  PROC_TYPE_AVR   = 0;     // AT90, ATmega, ATtiny
  PROC_TYPE_NEW51 = 1;     // AT89[L]S51/52
  PROC_TYPE_OLD51 = 2;     // AT89S53/8252
  PROC_TYPE_S8253 = 3;     // AT89S8253
  PROC_TYPE_S2051 = 4;     // AT89S2051/4051
  PROC_TYPE_DATAFLASH = 5; // AT25/26/45DB
  PROC_TYPE_SERIALFLASH = 6; // AT25F
  PROC_TYPE_I2C_BUS = 7;   // I2C memories

  // ISP operations
  OP_READSIG = 1;          // read signature
  OP_ERASE = 2;            // erase device
  OP_BLANKCHECKFLASH = 4;  // blank check Flash
  OP_BLANKCHECKEEPROM = 8; // blank check EEPROM
  OP_READFLASH = $10;      // read Flash
  OP_WRITEFLASH = $20;     // write Flash
  OP_VERIFYFLASH = $40;    // verify Flash
  OP_READEEPROM = $80;     // read EEPROM
  OP_WRITEEEPROM = $100;   // write EEPROM
  OP_VERIFYEEPROM = $200;  // verify EEPROM
  OP_READUSERSIG = $400;   // read user signature
  OP_WRITEUSERSIG = $800;  // write user signature
  OP_VERIFYUSERSIG = $1000;// verify user signature
  OP_READFUSE = $2000;     // read fuse bits
  OP_WRITEFUSE = $4000;    // write fuse bits
  OP_VERIFYFUSE = $8000;   // verify fuse bits
  OP_READLOCK = $10000;    // read lock bits
  OP_WRITELOCK = $20000;   // write lock bits
  OP_VERIFYLOCK = $40000;  // verify lock bits

  // LPT outputs
  LPT_OUT_STROBE = 0;      // pin 1
  LPT_OUT_D0 = 1;          // pin 2
  LPT_OUT_D1 = 2;          // pin 3
  LPT_OUT_D2 = 3;          // pin 4
  LPT_OUT_D3 = 4;          // pin 5
  LPT_OUT_D4 = 5;          // pin 6
  LPT_OUT_D5 = 6;          // pin 7
  LPT_OUT_D6 = 7;          // pin 8
  LPT_OUT_D7 = 8;          // pin 9
  LPT_OUT_AUTOLF = 9;      // pin 14
  LPT_OUT_INIT = 10;       // pin 16
  LPT_OUT_SELECTIN = 11;   // pin 17
  LPT_OUT_NOTUSED = 12;

  // LPT inputs
  LPT_IN_ACK = 0;          // pin 10
  LPT_IN_BUSY = 1;         // pin 11
  LPT_IN_PAPEREND = 2;     // pin 12
  LPT_IN_SELECT = 3;       // pin 13
  LPT_IN_ERROR = 4;        // pin 15

  // cable pinouts
  LPT_PINOUT_ADD    = 0;
  LPT_PINOUT_AECISP = 1;
  LPT_PINOUT_ALTERA = 2;
  LPT_PINOUT_ATPROG = 3;
  LPT_PINOUT_SIPROG = 4;
  LPT_PINOUT_STK200 = 5;
  LPT_PINOUT_UISP   = 6;
  LPT_PINOUT_CUSTOM = 7;

  ISP_PINOUT_ADD:TLPTpinout = (
    strobe1:LPT_OUT_STROBE;
    strobe2:LPT_OUT_NOTUSED;
    reset:LPT_OUT_AUTOLF;
    mosi:LPT_OUT_INIT;
    sck:LPT_OUT_SELECTIN;
    miso:LPT_IN_ACK;
    led:LPT_OUT_NOTUSED;
    resetinv:false);

  ISP_PINOUT_AECISP:TLPTpinout = (
    strobe1:LPT_OUT_NOTUSED;
    strobe2:LPT_OUT_NOTUSED;
    reset:LPT_OUT_D4;
    mosi:LPT_OUT_D5;
    sck:LPT_OUT_D6;
    miso:LPT_IN_ACK;
    led:LPT_OUT_NOTUSED;
    resetinv:false);

  ISP_PINOUT_ALTERA:TLPTpinout = (
    strobe1:LPT_OUT_AUTOLF;
    strobe2:LPT_OUT_NOTUSED;
    reset:LPT_OUT_D1;
    mosi:LPT_OUT_D6;
    sck:LPT_OUT_D0;
    miso:LPT_IN_BUSY;
    led:LPT_OUT_NOTUSED;
    resetinv:false);

  ISP_PINOUT_ATPROG:TLPTpinout = (
    strobe1:LPT_OUT_NOTUSED;
    strobe2:LPT_OUT_NOTUSED;
    reset:LPT_OUT_D3;
    mosi:LPT_OUT_D0;
    sck:LPT_OUT_D1;
    miso:LPT_IN_ACK;
    led:LPT_OUT_NOTUSED;
    resetinv:false);

  ISP_PINOUT_SIPROG:TLPTpinout = (
    strobe1:LPT_OUT_D3;
    strobe2:LPT_OUT_D2;
    reset:LPT_OUT_D7;
    mosi:LPT_OUT_D5;
    sck:LPT_OUT_D4;
    miso:LPT_IN_ACK;
    led:LPT_OUT_NOTUSED;
    resetinv:false);

  ISP_PINOUT_STK200:TLPTpinout = (
    strobe1:LPT_OUT_D3;
    strobe2:LPT_OUT_D2;
    reset:LPT_OUT_D7;
    mosi:LPT_OUT_D5;
    sck:LPT_OUT_D4;
    miso:LPT_IN_ACK;
    led:LPT_OUT_D6;
    resetinv:false);

  ISP_PINOUT_UISP:TLPTpinout = (
    strobe1:LPT_OUT_NOTUSED;
    strobe2:LPT_OUT_NOTUSED;
    reset:LPT_OUT_INIT;
    mosi:LPT_OUT_D0;
    sck:LPT_OUT_STROBE;
    miso:LPT_IN_BUSY;
    led:LPT_OUT_NOTUSED;
    resetinv:false);

  devicenr:byte = 0;

var
  proctype:byte;              // processor type (AVR/51)
  lptno:integer;              // LPT port number
  BASE:word;                  // LPT port base address
  pinout:TLPTpinout;          // LPT->ISP cable pinout
  pinout_num:integer;         // pinout type id
  flashsize:integer;          // Flash size in bytes
  eepromsize:integer;         // EEPROM size in bytes
  usersigsize:integer;        // User Signature size in bytes
  MCUfreq:integer;            // clock freq [Hz]
  tCLK_AVR:integer;           // clock period in ns
  tCLK_8252:integer;          // 40 clock periods in ns
  forcedev:boolean;           // force device with name forcename
  forcename:string;           // forced device name
  autoop:longword;            // auto operations mask

{$IFDEF Windows}

function MessageDlgCenter(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; Form:TForm): Integer;

function GetProgramVersionString:string;
{$ENDIF}

implementation

{$IFDEF Windows}

uses //Settings,
  SysUtils, LCLIntf, Windows;

function MessageDlgCenter(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; Form:TForm): Integer;
begin
  with CreateMessageDialog(Msg, DlgType, Buttons) do
  try
    HelpContext := HelpCtx;
    HelpFile := '';
    Left:=Form.Left + (Form.Width - Width) div 2;
    Top:=Form.Top + (Form.Height - Height) div 2;
    Position:=poDesigned;
    Result:=ShowModal;
  finally
    Free;
  end;

end;

function GetProgramVersionString:string;
var
  filename:array[0..MAX_PATH-1] of char;
  zero:DWORD;
  infolen:DWORD;
  infobuf:pointer;
  verlen:UINT;
  ptr:pointer;
  verptr:PVSFixedFileInfo;
begin
  Result:='';
  if GetModuleFileName(0, filename, MAX_PATH) > 0 then
    if StrLen(filename) > 0 then
    begin
      zero:=0;
      infolen:=GetFileVersionInfoSize(filename, zero);
      if infolen > 0 then
      begin
        infobuf:=AllocMem(infolen);
        if infobuf<>nil then
        begin
          ptr:=nil;
          verlen:=0;
          if GetFileVersionInfo(filename, 0, infolen, infobuf) then
	    if VerQueryValue(infobuf, '\', ptr, verlen) then
            begin
              verptr:=ptr;
              Result:=
                IntToStr(HiWord(verptr^.dwFileVersionMS)) + '.' +
                IntToStr(LoWord(verptr^.dwFileVersionMS)) + '.' +
                IntToStr(HiWord(verptr^.dwFileVersionLS)) + '.' +
                IntToStr(LoWord(verptr^.dwFileVersionLS));
            end;
          FreeMem(infobuf);
        end;
      end;
    end;
end;

{$ENDIF}

initialization
  //ReadSettingsFromRegistry(false, false, true);

finalization
  //WriteSettingsToRegistry(false, false, true);
end.
