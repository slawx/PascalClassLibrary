unit UPresto;

{$mode delphi}

interface

{$IFDEF Windows}

uses
  Classes, SysUtils, UISPProgrammer, UPrestoDLL,
  UStreamHelper, Globals, Processors, Delays, UCPUType, Registry,
  UJobProgressView;

type

  { TPrestoProgrammer }

  TPrestoProgrammer = class(TISPProgrammer)
  private
    Presto: TPresto;
    procedure ISPEnable;
    procedure ISPReadSign(Data: TStream);
    procedure WaitForReadyFlash(Address: Integer; Value: Byte);
    procedure ISPReadFlashPage(Address: Integer; Buffer: TStream);
    procedure ISPWriteFlashPage(Address: Integer; Buffer: TStream);
    function ISPReadFlash(Address: Integer): Byte;
    procedure ISPWriteFlash(Address: Integer; Data: Byte);
  protected
    procedure SetCPUType(AValue: TCPUType); override;
    procedure SetActive(AValue: Boolean); override;
  public
    Identification: string;
    procedure LoadFromRegistry(Root: HKEY; Key: string); override;
    procedure SaveToRegistry(Root: HKEY; Key: string); override;
    function ReadIdentification: string; override;
    procedure Write(Job: TJob); override;
    procedure Verify(Job: TJob); override;
    procedure Erase; override;
    procedure Reset; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

{$ENDIF}

implementation

{$IFDEF Windows}

resourcestring
  SUnknownDevice = 'Can''t program locked or not known device.';
  SLowFlashSize = 'Device flash memory is smaller than writed block size';
  SNoDataInFile = 'No data in file';
  SProgramOK = 'Program OK';
  SWriteError = 'Write error at address %s, byte written: %s, byte read: %s';
  SNotKnown = 'NOT KNOWN (%s, %s, %s)';
  SIdentification = 'Identification: ';

{ TPrestoProgrammer }

procedure TPrestoProgrammer.LoadFromRegistry(Root: HKEY; Key: string);
begin

end;

procedure TPrestoProgrammer.SaveToRegistry(Root: HKEY; Key: string);
begin

end;

procedure TPrestoProgrammer.ISPReadSign(Data: TStream);
var
  WriteData: TStreamHelper;
  I: Byte;
begin
  Active := True;

  if proctype = PROC_TYPE_S8253 then begin
    Data.Size := 0;
    try
      WriteData := TStreamHelper.Create;
      for I := 0 to 2 do begin
        WriteData.Size := 0;
        WriteData.WriteByte($28);
        WriteData.WriteByte($00);
        WriteData.WriteByte($30 + I);
        Presto.WriteBlock(WriteData);
        //Sleep(10);
        Data.WriteByte(Presto.ReadByte);
      end;
    finally
      WriteData.Free;
    end;
  end else
  if proctype = PROC_TYPE_NEW51 then begin
    Data.Size := 0;
    try
      WriteData := TStreamHelper.Create;
      for I := 0 to 2 do begin
        WriteData.Size := 0;
        WriteData.WriteByte($28);
        WriteData.WriteByte(I);
        WriteData.WriteByte($0);
        Presto.WriteBlock(WriteData);
        //Sleep(10);
        Data.WriteByte(Presto.ReadByte);
      end;
    finally
      WriteData.Free;
    end;
  end else begin
    Data.WriteByte($ff);
    Data.WriteByte($ff);
    Data.WriteByte($ff);
  end;
end;

function TPrestoProgrammer.ReadIdentification: string;
var
{$IFDEF I2C_SUPPORT}
  buff:array[0..255] of char;
  res:integer;
{$ENDIF}
  n:integer;
  s: TStreamHelper;
  z:string;
  label error;
begin
  Active := True;

  try
  s := TStreamHelper.Create;

  if proctype = PROC_TYPE_OLD51 then // 89S8252 or 89S53
  begin
    flashsize := 12*1024;
    eepromsize := 2048;
    usersigsize := 0;
    Result := 'AT89Sxx, Flash: up to 12 KB, EEPROM: up to 2 KB';
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
    ISPReadSign(s);
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
        for n:=1 to res do
          z := z + #13#10 + IntToStr(n) + '   0x' + IntToHex(byte(buff[n - 1]), 2);
      end;
      MessageDlgCenter(z, mtInformation, [mbOK], 0, Self);
      n:=-1;
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
            z := z + ', Flash: ' + IntToStr(fsize div 1048576) + ' MB'
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
        Result := z;
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
    Result := Format(SNotKnown, [IntToHex(s[0], 2), IntToHex(s[1], 2),
      IntToHex(s[2], 2)]);

//  if n = -1 then
//    raise Format(SNotKnown, [IntToHex(s[0], 2), IntToHex(s[1], 2),
//      IntToHex(s[2], 2)]);
  devicenr := 0;
  flashsize := 0;
  eepromsize := 0;
  usersigsize := 0;
  //Working := False;
  //EnableButtons;
  finally
    S.Free;
  end;
end;

procedure TPrestoProgrammer.Write(Job: TJob);
var
  Address, I, PageSize, MinAdr, MaxAdr: Integer;
  b, b1: byte;
  WritePage, ReadPage: TStreamHelper;
  Flash: TStreamHelper;
begin
  Active := True;

  try
    WritePage := TStreamHelper.Create;
    ReadPage := TStreamHelper.Create;
    Flash := TStreamHelper.Create;

    if (devicenr = DEVICE_UNKNOWN) or (devicenr = DEVICE_LOCKED) or (FlashSize = 0) then
    begin
      Log(SUnknownDevice);
      raise Exception.Create(SUnknownDevice);
    end;
    //ClearBuffer(BUF_FLASH);
    minadr := 0;
    maxadr := 0;

    if (minadr >= flashsize) or (maxadr < 0) then
    begin
      raise Exception.Create(SNoDataInFile);
    end;

    HexFile.WriteContinuousBlock(Flash);
    maxadr := Flash.Size;

    // AT89S8253, AT89S2051/4051, AT89S51/52, AVRs with page programming
    PageSize := Signatures[devicenr].fpagesize;
    Job.Progress.Max := MaxAdr;
    WritePage.Size := PageSize;
    ReadPage.Size := PageSize;
    if Flash.Size > Signatures[devicenr].fsize then
      raise Exception.Create(SLowFlashSize);

    Address := 0;
    while Address < MaxAdr do begin
      if True then //Address < (Trunc(MaxAdr / PageSize) * PageSize) then
      begin
        // Write page
        Flash.Position := Address;
        WritePage.Position := 0;
        WritePage.WriteStreamPart(Flash, PageSize);
        while WritePage.Position < PageSize do WritePage.WriteByte($ff); // fill rest of page

        ISPWriteFlashPage(Address, WritePage);
        Sleep(10);
        ISPReadFlashPage(Address, ReadPage);
        WritePage.Position := 0;
        ReadPage.Position := 0;
        for I := 0 to PageSize - 1 do begin
          b := WritePage.ReadByte;
          b1 := ReadPage.ReadByte;
          if b <> b1 then begin
            raise Exception.Create(Format(SWriteError,
              [IntToHex(Address + I, 8), IntToHex(b, 2), IntToHex(b1, 2)]));
            //LedOff;
            //Exit;
          end;
        end;
        Address := Address + PageSize;
      end else begin
        // Write rest
        Flash.Position := Address;
        B := Flash.ReadByte;
        if B <> $ff then begin
          ISPWriteFlash(Address, b);
          //Sleep(10);
          b1 := ISPReadFlash(Address);
          if b <> b1 then begin
            raise Exception.Create(Format(SWriteError, [IntToHex(Address, 8),
              IntToHex(b, 2), IntToHex(b1, 2)]));
              //LedOff;
              //Exit;
          end;
        end;
        Inc(Address);
      end;
      Job.Progress.Value := Address;
      if Job.Terminate then Break;
    end;
    Log(SProgramOK);
  finally
    Flash.Free;
    WritePage.Free;
    ReadPage.Free;
  end;
end;

procedure TPrestoProgrammer.Verify(Job: TJob);
begin
  inherited;
end;

procedure TPrestoProgrammer.Erase;
var
  WriteData: TStreamHelper;
begin
  Active := True;

  try
    WriteData := TStreamHelper.Create;
    WriteData.WriteByte($AC);
    WriteData.WriteByte($80);
    WriteData.WriteByte($00);
    WriteData.WriteByte($00);
    Presto.WriteBlock(WriteData);
    Sleep(500);
    //Active := False;
  finally
    WriteData.Free;
  end;
end;

procedure TPrestoProgrammer.Reset;
begin
  Active := True;
  Presto.SetPin(spP, psHigh);
  Sleep(100);
  Presto.SetPin(spP, psHighZ);
end;

constructor TPrestoProgrammer.Create;
begin
  inherited Create;
  Capabilities := [ipcErase, ipcWrite, ipcReset];
  Presto := TPresto.Create;
  Presto.SPIMode := smMode3;
  Presto.Speed := psClk32;
  //Presto.Speed := psClk4;
  Presto.SPIDataOrder := doMSBFirst;
  CPUType := ctAT82S8253;
end;

destructor TPrestoProgrammer.Destroy;
begin
  Presto.Free;
  inherited Destroy;
end;

procedure TPrestoProgrammer.ISPReadFlashPage(Address: Integer; Buffer: TStream);
var
  PageSize, PageMask, raddr: integer;
  Data: TStreamHelper;
begin
  try
    Data := TStreamHelper.Create;
    PageSize := Signatures[devicenr].fpagesize;
    PageMask := PageSize - 1;
    Data.WriteByte($30);
    Data.WriteByte(Hi(word(Address)));
    if proctype <> PROC_TYPE_NEW51 then
      Data.WriteByte(Lo(word(Address)) and (PageMask xor $ff));
    Presto.WriteBlock(Data);
    Sleep(10);
    Presto.ReadBlock(Buffer);
  finally
    Data.Free;
  end;
end;

procedure TPrestoProgrammer.ISPWriteFlashPage(Address: Integer; Buffer: TStream);
var
  pagesize, pagemask: integer;
  Data: TStreamHelper;
begin
  try
    Data := TStreamHelper.Create;
    pagesize := Signatures[devicenr].fpagesize;
    pagemask := pagesize - 1;
    Data.WriteByte($50);
    Data.WriteByte(Hi(word(Address)));
    if proctype <> PROC_TYPE_NEW51 then
      Data.WriteByte(Lo(word(Address)) and (pagemask xor $ff));
    Presto.WriteBlock(Data);
    Sleep(10);

    Buffer.Position := 0;
    while Buffer.Position < Buffer.Size do begin
      Presto.WriteByte(Buffer.ReadByte);
      if proctype = PROC_TYPE_NEW51 then Sleep(1); // AT89S52 nestíhá
    end;
    Sleep(Signatures[devicenr].prog_time);
  finally
    Data.Free;
  end;
end;

function TPrestoProgrammer.ISPReadFlash(Address: Integer): Byte;
var
  WriteData: TStreamHelper;
begin
  try
    WriteData := TStreamHelper.Create;
    WriteData.WriteByte($20);
    WriteData.WriteByte(Hi(word(Address)));
    WriteData.WriteByte(Lo(word(Address)));
    //WriteData.WriteByte(0);
    Presto.WriteBlock(WriteData);
    //Sleep(10);
    Result := Presto.ReadByte;
  finally
    WriteData.Free;
  end;
end;

procedure TPrestoProgrammer.ISPWriteFlash(Address: Integer; Data: Byte);
var
  WriteData: TStreamHelper;
begin
  try
    WriteData := TStreamHelper.Create;
    WriteData.WriteByte($40);
    WriteData.WriteByte(Hi(word(Address)));
    WriteData.WriteByte(Lo(word(Address)));
    WriteData.WriteByte(Data);
    Presto.WriteBlock(WriteData);
    WaitForReadyFlash(Address, Data);
  finally
    WriteData.Free;
  end;
end;

procedure TPrestoProgrammer.SetCPUType(AValue: TCPUType);
begin
  if AValue = ctAT82S8253 then begin
    proctype := PROC_TYPE_S8253;
    Presto.SPIMode := smMode1;
  end else
  if AValue = ctAT89S52 then begin
    proctype := PROC_TYPE_NEW51;
    Presto.SPIMode := smMode3;
  end else proctype := PROC_TYPE_AVR;
end;

procedure TPrestoProgrammer.WaitForReadyFlash(Address: Integer; Value: Byte);
var
  b: byte;
const
  t1:Int64 = 0;
begin
  if value <> $ff then begin
    Sleep(1);
    Tic(t1);
    repeat
      b := ISPReadFlash(Address);
    until (b = value) or (TocMS(t1) > 100);
  end else
    Sleep(Signatures[devicenr].prog_time);
end;

procedure TPrestoProgrammer.SetActive(AValue: Boolean);
begin
  if Active = AValue then Exit;
  inherited;
  if AValue then begin
    Presto.Open;
    Presto.ActiveLED := True;
    Presto.WriteByte(0);
    Presto.SetPin(spP, psHigh);
    ISPEnable;
    Identification := ReadIdentification;
    Log(SIdentification + Identification);
  end else begin
    //Presto.SetPin(spP, psHighZ);
    Presto.Close;
  end;
end;

procedure TPrestoProgrammer.ISPEnable;
var
  WriteData: TStreamHelper;
begin
  try
    WriteData := TStreamHelper.Create;
    WriteData.WriteByte($AC);
    WriteData.WriteByte($53);
    WriteData.WriteByte($00);
    WriteData.WriteByte($00);
    Presto.WriteBlock(WriteData);
    Sleep(30);
  finally
    WriteData.Free;
  end;
end;

{$ENDIF}

end.

