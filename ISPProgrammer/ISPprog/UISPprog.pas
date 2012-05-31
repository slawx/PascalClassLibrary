unit UISPprog;

{$mode delphi}

interface

{$IFDEF Windows}
uses
  Windows, Classes, SysUtils, UISPProgrammer, ISP, UJobProgressView, Globals,
  PinsIO, Delays, Processors, MemBuffer, SerialFlash, ISP_Flash, Forms,
  URegistry, Registry, PortsIO, CfgMgr, UCPUType;

type

  { TISPProg }

  TISPProg = class(TISPProgrammer)
  private
    ChipSignature: string;
    procedure BeginWork;
    procedure EndWork;
    procedure ResetOff;
  protected
    procedure SetCPUType(AValue: TCPUType); override;
    procedure SetActive(AValue: Boolean); override;
  public
    constructor Create; override;
    procedure Write(Job: TJob); override;
    procedure Read(Job: TJob); override;
    procedure Verify(Job: TJob); override;
    procedure Erase; override;
    procedure Reset; override;
    function ReadIdentification: string; override;
    procedure LoadFromRegistry(Root: HKEY; Key: string); override;
    procedure SaveToRegistry(Root: HKEY; Key: string); override;
  end;

  TLPTinfo = packed record
    num:DWORD;
    baseaddr:DWORD;
  end;
  PLPTinfo = ^TLPTinfo;

function GetLPTAddr(dev:PDEVINST; baseaddr:PDWORD):boolean;
function EnumLPT(lpttab:PLPTinfo; maxports:integer):integer;

var
  lpttable: packed array[1..99] of TLPTinfo;

{$ENDIF}

implementation

{$IFDEF Windows}

uses
  InpOut32;

const
  TempHexFile = 'Dump.hex';

resourcestring
  SReady = 'Ready';
  SProgramOK = 'Program OK';
  SNoDataInFile = 'No data in file';
  SUnknownDevice = 'Can''t program locked or not known device.';
  SCantEraseNotKnownDevice = 'Can''t erase not known device.';
  SWriteError = 'Write error at address %s, byte written: %s, byte read: %s';
  SEraseTimeout = 'Erasing timeout.';
  SEraseError = 'Erasing error.';
  SDeviceErased = 'Device erased.';
  SNotKnown = 'NOT KNOWN (%s, %s, %s)';
  SWait = 'Wait...';

const
  lptcom: TGUID = '{4d36e978-e325-11ce-bfc1-08002be10318}';


function GetLPTAddr(dev:PDEVINST; baseaddr:PDWORD):boolean;
var
  res, res2:DWORD;
  conf:LOG_CONF;
  rsrc:RES_DES;
  buf:array[0..255] of char;
begin
  Result:=false;
  res := CM_Get_First_Log_Conf(@conf, dev^, ALLOC_LOG_CONF);
  if (res <> CR_SUCCESS) or (conf = 0) then Exit;
  res := CM_Get_Next_Res_Des(@rsrc, conf, ResType_IO, nil, 0);
  if res <> CR_SUCCESS then Exit;
  res2 := CM_Get_Res_Des_Data(rsrc, @buf, sizeof(buf), 0);
  if res2 <> CR_SUCCESS then Exit;
  baseaddr^ := PIO_RESOURCE(@buf)^.IO_Header.IOD_Alloc_Base and $ffff;
  CM_Free_Log_Conf_Handle(conf);
  Result:=true;
end;

function EnumLPT(lpttab:PLPTinfo; maxports:integer):integer;
var
  i:integer;
  h:HDEVINFO;
  devinfo_data:SP_DEVINFO_DATA;
  res: BOOL;
  dev_name:packed array[0..255] of char;
  p1,p2:PChar;
  lptnum, lptcnt, code:integer;
begin
  Result := -1;
  h := SetupDiGetClassDevs(@lptcom, nil, 0, DIGCF_PRESENT or DIGCF_PROFILE);
  if h = INVALID_HANDLE_VALUE then Exit;
  i := 0;
  lptcnt := 0;
  repeat
    devinfo_data.cbSize := sizeof(SP_DEVINFO_DATA);
    res := SetupDiEnumDeviceInfo(h, i, @devinfo_data);
    if res then
    begin
      if SetupDiGetDeviceRegistryProperty(h, @devinfo_data, SPDRP_FRIENDLYNAME,
        nil, @dev_name, sizeof(dev_name), nil) then
      begin
        p1 := StrPos(dev_name, '(LPT');
        if p1 <> nil then
        begin
          p2 := StrPos(p1, ')');
          if (p2 <> nil) and (p2 > p1) and (p2 - p1 - 5 <= 1) then
          begin
            p2^ := #0;
            Val(StrPas(p1+4), lptnum, code);
            if (code = 0) and (lptnum >= 1) and (lptnum <= 99) then
            begin
              lpttab^.num := lptnum;
              if GetLPTAddr(@devinfo_data.DevInst, @lpttab^.baseaddr) then
              begin
                Inc(lpttab);
                Inc(lptcnt);
              end;
            end;
          end;
        end;
      end;
    end;
    Inc(i);
  until (not res) or (i = maxports);
  SetupDiDestroyDeviceInfoList(h);
  Result:=lptcnt;
end;


{ TISPProg }

procedure TISPProg.Write(Job: TJob);
var
  res: string;
  radr, veradr, pagemask, pagesize, minadr, maxadr: integer;
  b, b1: byte;
  buff: array[0..1055] of byte;
  Dump: TStringList;
begin
  (*if FlashPgmLabel.Caption = '' then
  begin
    StatusBar1.SimpleText:='Specify Flash file name first';
    Exit;
  end;
  StatusBar1.SimpleText:='';
  Application.ProcessMessages;*)
  Active := True;

  if (devicenr = DEVICE_UNKNOWN) or (devicenr = DEVICE_LOCKED) or (flashsize = 0) then
  begin
    raise Exception.Create(SUnknownDevice);
    //Exit;
  end;
    ClearBuffer(BUF_FLASH);
  minadr := 0;
  maxadr := 0;

  // Save to temp file
  try
    Dump := TStringList.Create;
    HexFile.SaveToStringList(Dump);
    Dump.SaveToFile(UTF8Decode(GetTempDir(True) + DirectorySeparator + TempHexFile));
  finally
    Dump.Free;
  end;

  res := LoadFile(BUF_FLASH, FILE_TYPE_INTELHEX,
    UTF8Decode(GetTempDir(True) + DirectorySeparator + TempHexFile),
    flashsize, minadr, maxadr);
  if res <> '' then
  begin
    raise Exception.Create(res);
    //Exit;
  end;
  if (minadr >= flashsize) or (maxadr < 0) then
  begin
    raise Exception.Create(SNoDataInFile);
    //Exit;
  end;
  LedOn;
  //Application.ProcessMessages;

  if (Signatures[devicenr].algo = ALGO_SERIALFLASH) then
    SerialflashUnprotectAll;

  if (proctype = PROC_TYPE_S8253) or (proctype = PROC_TYPE_S2051) or (proctype = PROC_TYPE_NEW51) or
    ((proctype = PROC_TYPE_AVR) and (Signatures[devicenr].fpagesize <> 0)) then
  begin
    // AT89S8253, AT89S2051/4051, AT89S51/52, AVRs with page programming
    pagesize:=Signatures[devicenr].fpagesize;
    pagemask:=pagesize - 1;
    minadr:=minadr and (not pagemask);  // round down to page size
    maxadr:=maxadr or pagemask;         // round up to page size
    radr:=minadr;
    Job.Progress.Max := maxadr - minadr + 1;
    while radr <= maxadr do begin
      if (proctype = PROC_TYPE_AVR) and (flashsize > 1024*128) then
      begin
        // ATmega2560/2561
        if (radr = minadr) or ((radr and $1ffff) = 0) then
          ISPLoadExtendedAddress(radr);
      end;
      ISPWriteFlashPage (radr, @flashbuffer[radr]);
      ISPReadFlashPage (radr, @buff);
      for veradr:=radr to radr + pagesize - 1 do
      begin
        b:=flashbuffer[veradr];
        b1:=buff[veradr - radr];
        if b <> b1 then
        begin
          raise Exception.Create(Format(SWriteError, [IntToHex(veradr, 8), IntToHex(b, 2), IntToHex(b1, 2)]));
          //LedOff;
        end;
      end;
      radr := radr + pagesize;
      Job.Progress.Value := radr - minadr;
      if Job.Terminate then Break;
    end;
    Log(SProgramOK);
    LedOff;
    Exit;
  end
  else if (proctype = PROC_TYPE_DATAFLASH) or (proctype = PROC_TYPE_SERIALFLASH) then
  begin
    // AT45DBxx DataFlash
    pagesize:=Signatures[devicenr].fpagesize;
    minadr:=(minadr div pagesize) * pagesize;                  // round down to page size
    maxadr:=(maxadr div pagesize) * pagesize + (pagesize - 1); // round up to page size
    radr:=minadr;
    while radr <= maxadr do
    begin
      ISPWriteFlashPage (radr, @flashbuffer[radr]);
      ISPReadFlashPage (radr, @buff);
      for veradr:=radr to radr + pagesize - 1 do
      begin
        b := flashbuffer[veradr];
        b1 := buff[veradr - radr];
        if b <> b1 then
        begin
          raise Exception.Create(Format(SWriteError, [IntToHex(veradr, 8), IntToHex(b, 2), IntToHex(b1, 2)]));
          //LedOff;
          //Exit;
        end;
      end;
      radr := radr + pagesize;
      Job.Progress.Value := radr - minadr;
    end;
    Log(SProgramOK);
    LedOff;
    Exit;
  end;

  for radr := minadr to maxadr do
  begin
    if flashbuffer[radr] <> $ff then
    begin
      b := flashbuffer[radr];
      ISPWriteFlash(radr, b);
      b1 := ISPReadFlash(radr);
      if b <> b1 then
      begin
        raise Exception.Create(Format(SWriteError, [IntToHex(radr, 8), IntToHex(b, 2), IntToHex(b1, 2)]));
        //LedOff;
        //Exit;
      end;
    end;
    Job.Progress.Value := radr - minadr;
  end;
  LedOff;
  Log(SProgramOK);
end;

procedure TISPProg.Read;
begin
  inherited;
end;

procedure TISPProg.Verify(Job: TJob);
begin
  inherited;
end;

procedure TISPProg.Erase;
var
  res:integer;
{$IFDEF I2C_SUPPORT}
  buff: array[0..255] of char;
{$ENDIF}
begin
  //StatusBar1.SimpleText:='';
  //Application.ProcessMessages;
  Active := True;

  if devicenr = DEVICE_UNKNOWN then
  begin
    Log(SCantEraseNotKnownDevice);
    Exit;
  end;
  Log(SWait);
  LedOn;
  //Application.ProcessMessages;
{$IFDEF I2C_SUPPORT}
  if proctype = PROC_TYPE_I2C_BUS then
  begin
    res := isplib_erase_all;
    LedOff;
    if res < 0 then
    begin
      isplib_error_desc(res, buff, 256);
      MessageDlgCenter(string(buff), mtError, [mbOK], 0, Self);
      StatusBar1.SimpleText := SReady;
      Exit;
    end;
  end
  else
{$ENDIF}
  begin
    res:=ISPErase;
    LedOff;
  end;
  BeginWork;
  case res of
    ISP.ERROR_TIMEOUT:
      raise Exception.Create(SEraseTimeout);
    ISP.ERROR_PROGRAM,
    ISP.ERROR_STOP:
      raise Exception.Create(SEraseError);
    else
      Log(SDeviceErased);
  end;
end;

procedure TISPProg.Reset;
begin
  Active := True;

  WaitMS(100);
  StrobeOn;
  RstOn;
  //ReadResetStatus;
  //Application.ProcessMessages;
  WaitMS(100);
  RstOff;
  StrobeOff;
  //ReadResetStatus;
  EndWork;
end;

procedure TISPProg.LoadFromRegistry(Root: HKEY; Key: string);
begin
  with TRegistryEx.Create do
  try
    RootKey := Root;
    OpenKey(Key + '\ISPProgrammer\ISPprog', True);
    pinout_num := ReadIntegerWithDefault('pinout_num', 4);
    pinout.strobe1 := ReadIntegerWithDefault('pinout_strobe1', 4);
    pinout.strobe2 := ReadIntegerWithDefault('pinout_strobe2', 3);
    pinout.reset := ReadIntegerWithDefault('pinout_reset', 8);
    pinout.mosi := ReadIntegerWithDefault('pinout_mosi', 6);
    pinout.sck := ReadIntegerWithDefault('pinout_sck', 5);
    pinout.miso := ReadIntegerWithDefault('pinout_miso', 0);
    pinout.led := ReadIntegerWithDefault('pinout_led', 12);
    pinout.resetinv := ReadBoolWithDefault('pinout_resetinv', False);
    lptno := ReadIntegerWithDefault('lptno', 1);
    BASE := ReadIntegerWithDefault('BASE', 888);
    tCLK_8252 := ReadIntegerWithDefault('tCLK_8252', 3616);
    tCLK_AVR := ReadIntegerWithDefault('tCLK_AVR', 90);
    MCUfreq := ReadIntegerWithDefault('MCUfreq', 11059200);
    PinoutChanged;
  finally
    Free;
  end;
end;

procedure TISPProg.SaveToRegistry(Root: HKEY; Key: string);
begin
  with TRegistryEx.Create do
  try
    RootKey := Root;
    OpenKey(Key + '\ISPProgrammer\ISPprog', True);
    WriteInteger('pinout_num', pinout_num);
    WriteInteger('pinout_strobe1', pinout.strobe1);
    WriteInteger('pinout_strobe2', pinout.strobe2);
    WriteInteger('pinout_reset', pinout.reset);
    WriteInteger('pinout_mosi', pinout.mosi);
    WriteInteger('pinout_sck', pinout.sck);
    WriteInteger('pinout_miso', pinout.miso);
    WriteInteger('pinout_led', pinout.led);
    WriteBool('pinout_resetinv', pinout.resetinv);
    WriteInteger('lptno', lptno);
    WriteInteger('BASE', BASE);
    WriteInteger('tCLK_8252', tCLK_8252);
    WriteInteger('tCLK_AVR', tCLK_AVR);
    WriteInteger('MCUfreq', MCUfreq);
  finally
    Free;
  end;
end;

procedure TISPProg.BeginWork;
{$IFDEF I2C_SUPPORT}
var
  buff: array[0..255] of char;
  res: integer;
{$ENDIF}
begin
  if (proctype = PROC_TYPE_DATAFLASH) or (proctype = PROC_TYPE_SERIALFLASH) then
  begin
    StrobeOn;
    ChipselectOff;
    ClkLo;
    WaitMS(100);
  end
{$IFDEF I2C_SUPPORT}
  else if proctype = PROC_TYPE_I2C_BUS then
  begin
    res:=isplib_begin_work;
    if res < 0 then
    begin
      isplib_error_desc(res, buff, 256);
      MessageDlgCenter(string(buff), mtError, [mbOK], 0, Self);
      Exit;
    end
  end
{$ENDIF}
  else
  begin
    StrobeOn;
    RstOff;
    ClkLo;
    WaitMS(100);
    RstOn;
    WaitMS(500);
    ISPEnable;
    //ReadResetStatus;
  end;
  ReadIdentification;
end;

procedure TISPProg.EndWork;
begin
{$IFDEF I2C_SUPPORT}
  if proctype = PROC_TYPE_I2C_BUS then
    isplib_end_work;
{$ENDIF}
  LedOff;
  Log(SReady);
  ChipSignature := '';
  devicenr:=0;
  //EnableButtons;
end;

function TISPProg.ReadIdentification: string;
var
{$IFDEF I2C_SUPPORT}
  buff:array[0..255] of char;
  res:integer;
{$ENDIF}
  n:integer;
  s:array[0..2] of byte;
  z:string;
  label error;
begin
  Active := True;

  if proctype = PROC_TYPE_OLD51 then // 89S8252 or 89S53
  begin
    flashsize := 12*1024;
    eepromsize := 2048;
    usersigsize := 0;
    ChipSignature := 'AT89Sxx, Flash: up to 12 KB, EEPROM: up to 2 KB';
    devicenr := DEVICE_AT89Sxx;
    //working:=true;
    //EnableButtons;
    Exit;
  end;

{$IFDEF I2C_SUPPORT}
  if proctype = PROC_TYPE_I2C_BUS then
  begin
    // scan I2C bus
    res:=isplib_scan(buff, 256);
    if res < 0 then
    begin
      isplib_error_desc(res, buff, 256);
      MessageDlgCenter(string(buff), mtError, [mbOK], 0, Self);
    end
    else
    begin
      // <res> devices found on I2C bus
      if res = 0 then
        z:='No devices'
      else if res = 1 then
        z:='1 device'
      else
        z:=IntToStr(res) + ' devices';
      z:=z + ' found on I2C bus.';
      if res > 0 then
      begin
        z:=z + #13#10 + 'Device address:';
        for n:=1 to res do
          z:=z + ' 0x' + IntToHex(byte(buff[n - 1]), 2);
      end;
      MessageDlgCenter(z, mtInformation, [mbOK], 0, Self);
    end;
    if res <= 0 then
      goto error;
  end;
{$ENDIF}

  if forcedev then
  begin
    n := FindName(forcename);
    if (n >= 0) then
    begin
      s[0] := Signatures[n].b0;
      s[1] := Signatures[n].b1;
      s[2] := Signatures[n].b2;
    end
  end
  else
  begin
    ISPReadSign(@s);
    n := FindSignature(s[0], s[1], s[2]);
  end;

{$IFDEF I2C_SUPPORT}
  if (n >= 0) and (proctype = PROC_TYPE_I2C_BUS) then
  begin
    // scan I2C bus
    res := isplib_scan(buff, 256);
    if res < 0 then
    begin
      isplib_error_desc(res, buff, 256);
      MessageDlgCenter(string(buff), mtError, [mbOK], 0, Self);
      n := -1;
    end
    else
    begin
      // <res> devices found on I2C bus
      if res = 0 then
        z := 'No devices'
      else if res = 1 then
        z := '1 device'
      else
        z := IntToStr(res) + ' devices';
      z := z + ' found on I2C bus.';
      if res > 0 then
      begin
        z := z + #13#10 + 'Device number   address';
        for n := 1 to res do
          z := z + #13#10 + IntToStr(n) + '   0x' + IntToHex(byte(buff[n - 1]), 2);
      end;
      MessageDlgCenter(z, mtInformation, [mbOK], 0, Self);
      n := -1;
    end;
  end;
{$ENDIF}

  if (n >= 0) then
    with Signatures[n] do
      begin
        z := name;
        if (fsize > 0) then
        begin
          if (fsize mod 1048576 = 0) then
            z := z+', Flash: ' + IntToStr(fsize div 1048576) + ' MB'
          else
            z := z + ', Flash: ' + IntToStr(fsize div 1024) + ' KB';
        end;
        if (esize > 0) then
          z := z + ', EEPROM: ' + IntToStr(esize) + ' bytes';
        if (usigsize > 0) then
          z := z + ', User Signature: ' + IntToStr(usigsize) + ' bytes';
        devicenr := n;
        flashsize := fsize;
        eepromsize := esize;
        usersigsize := usigsize;
        ChipSignature := z;
        //working:=true;
        //EnableButtons;
        Exit;
      end;

error:
{$IFDEF I2C_SUPPORT}
  if proctype = PROC_TYPE_I2C_BUS then
    ChipSignatureLabel.Caption := ''
  else
{$ENDIF}
    ChipSignature := Format(SNotKnown, [IntToHex(s[0], 2),
      IntToHex(s[1], 2), IntToHex(s[2], 2)]);

  devicenr := 0;
  flashsize := 0;
  eepromsize := 0;
  usersigsize := 0;
  //Working := False;
  //EnableButtons;
  Result := ChipSignature;
end;

procedure TISPProg.ResetOff;
begin
  RstOff;
  StrobeOff;
  //ReadResetStatus;
  EndWork;
end;

procedure TISPProg.SetCPUType(AValue: TCPUType);
begin
  if AValue = ctAT82S8253 then proctype := PROC_TYPE_S8253
  else if AValue = ctAT89S52 then proctype := PROC_TYPE_NEW51
  else proctype := PROC_TYPE_AVR;
end;

procedure TISPProg.SetActive(AValue: Boolean);
begin
  if Active = AValue then Exit;
  inherited;
  if AValue then begin
    BeginWork;
  end else begin
    EndWork;
  end;
end;

constructor TISPProg.Create;
begin
  inherited Create;
  Capabilities := [ipcErase, ipcWrite, ipcReset];
  CPUType := ctAT82S8253;
  InpOut32.LoadLibraries;
  PortsIO.Init;
end;

{$ENDIF}

end.

