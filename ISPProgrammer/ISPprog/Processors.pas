unit Processors;

{$MODE Delphi}

interface

{$INCLUDE Options.inc}

uses Globals;

type
  TLockBits = array[0..7] of string[5];
  TSignature = record
    b0, b1, b2:byte;   {Signature bytes}
    name:string[16];
    proctype:byte;     {Processor type: PROC_TYPE_xxx}
    fsize:integer;     {Flash size in bytes}
    esize:integer;     {EEPROM size in bytes}
    usigsize:integer;  {User Signature size in bytes}
    fpage:byte;        {Flash page bits}
    fpagesize:integer; {Flash page size in bytes, usually 2^fpage}
    epage:byte;        {EEPROM page bits}
    epagesize:integer; {EEPROM page size in bytes, usually 2^epage}
    osccal:byte;       {Number of oscillator calibration bytes}
    algo:byte;         {Programming algorithm}
    algo_erase:byte;   {Erasing algorithm}
    algo_lb:byte;      {Lock bits and fuses reading/programming algorithm}
    algo_busy:byte;    {Busy check algorithm}
    prog_time:integer; {Time in ms to wait after programming}
    lockbits:array[0..7] of string[5];     {LSB to MSB}
    fusebitslo:array[0..7] of string[16];  {LSB to MSB}
    fusebitshi:array[0..7] of string[16];  {LSB to MSB}
    fusebitsext:array[0..7] of string[16]; {LSB to MSB}
  end;

const
  ALGO_STD = 1;
  ALGO_MEGA = 2;
  ALGO_DATAFLASH = 3;
  ALGO_SERIALFLASH = 4;
{$IFDEF I2C_SUPPORT}
  ALGO_AT24_EEPROM = 5;
{$ENDIF}

  ALGO_ERASE_STD = 0;
  ALGO_ERASE_TWICE = 1;

  ALGO_LB_NONE = 0;
  ALGO_LB_STD = 1;
  ALGO_LB_TINY = 2;
  ALGO_LB_2323 = 3;
  ALGO_LB_2333 = 4;
  ALGO_LB_MEGA = 5;
  ALGO_LB_89x = 6;
  ALGO_LB_89S51 = 7;
  ALGO_LB_89S8253 = 8;
  ALGO_LB_89S2051 = 9;

  ALGO_BUSY_WAIT = 1;
  ALGO_BUSY_POLL_00FF = 2;
  ALGO_BUSY_POLL_FF = 3;
  ALGO_BUSY_POLL_NMSB = 4;
  ALGO_BUSY_POLL_RDYBSY = 5;
  ALGO_BUSY_DATAFLASH = 6;
  ALGO_BUSY_SERIALFLASH = 7;
{$IFDEF I2C_SUPPORT}
  ALGO_BUSY_AT24_EEPROM = 8;
{$ENDIF}

  MAX_FLASH_SIZE = 17301504;   // AT45CS1282 (16.5MB)
  MAX_EEPROM_SIZE = 4096;
  MAX_USERSIG_SIZE = 32;

  SIGNCOUNT = 2
    + 7  {AT89}
    + 18 {AT90}
    + 16 {ATtiny}
    + 38 {ATmega}
    + 21 {AT45}
    + 7  {AT25}
    + 4  {AT25F}
    + 1  {AT26}
{$IFDEF I2C_SUPPORT}
    + 11 {AT24}
{$ENDIF}
    ;

  Signatures:array [1..SIGNCOUNT] of TSignature =
  (
    (b0:$00; b1:$00; b2:$00;
    name:'AT89S53/8252';
    proctype:PROC_TYPE_OLD51;
    fsize:12288; esize:2048; usigsize:0; fpage:0; fpagesize:0; epage:0; epagesize:0;
    osccal:0;
    algo:ALGO_STD;
    algo_erase:ALGO_ERASE_STD;
    algo_lb:ALGO_LB_89x;
    algo_busy:ALGO_BUSY_POLL_NMSB;
    prog_time:16;
    lockbits:('','','','','','LB3','LB2','LB1');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','',''))
,
    (b0:$00; b1:$01; b2:$02;
    name:'CHIP LOCKED';
    proctype:0;
    fsize:0; esize:0; usigsize:0; fpage:0; fpagesize:0; epage:0; epagesize:0;
    osccal:0;
    algo:0;
    algo_erase:0;
    algo_lb:0;
    algo_busy:0;
    prog_time:0;
    lockbits:('','','','','','','','');
    fusebitslo:('','','','','','','','');
    fusebitshi:('','','','','','','','');
    fusebitsext:('','','','','','','',''))
,
{$I Devices_AT89.inc}
,
{$I Devices_AT90.inc}
,
{$I Devices_ATtiny.inc}
,
{$I Devices_ATmega.inc}
,
{$I Devices_AT45.inc}
,
{$I Devices_AT25.inc}
,
{$I Devices_AT25F.inc}
,
{$I Devices_AT26.inc}

{$IFDEF I2C_SUPPORT}
  ,
  {$I Devices_AT24.inc}
{$ENDIF}

  );

  DEVICE_UNKNOWN = 0;
  DEVICE_AT89Sxx = 1;    // AT89S53/8252
  DEVICE_LOCKED = 2;

function FindSignature (s0:byte; s1:byte; s2:byte):integer;
function FindName (name:string):integer;

implementation

function FindSignature (s0:byte; s1:byte; s2:byte):integer;
var
  n:integer;
begin
  for n:=2 to SIGNCOUNT do
    with Signatures[n] do
      if (b0=s0) and (b1=s1) and (b2=s2) then
      begin
        Result:=n;
        Exit;
      end;
  Result:=-1;
end;

function FindName (name:string):integer;
var
  n:integer;
begin
  for n:=1 to SIGNCOUNT do
    if (n <> DEVICE_LOCKED) and (Signatures[n].name = name) then
    begin
      Result:=n;
      Exit;
    end;
  Result:=-1;
end;

end.
