unit MemBuffer;

{$MODE Delphi}

interface

uses Processors;

const
  BUF_FLASH = 0;
  BUF_EEPROM = 1;
  BUF_USERSIG = 2;
  FILE_TYPE_INTELHEX = 0;
  FILE_TYPE_BINARY = 1;

procedure ClearBuffer(bufid:byte);
function LoadFile(bufid:byte; filetype:integer; fname:string; buflen:integer;
  var minaddr, maxaddr:integer):string;
function SaveFile(bufid:byte; filetype:integer; fname:string; buflen:integer):string;
function LoadIntelHex(bufid:byte; fname:string; buflen:integer;
  var minaddr, maxaddr:integer):string;
function SaveIntelHex(bufid:byte; fname:string; buflen:integer):string;
function LoadBinaryFile(bufid:byte; fname:string; buflen:integer;
  var minaddr, maxaddr:integer):string;
function SaveBinaryFile(bufid:byte; fname:string; buflen:integer):string;
function HighestUsed(bufid:byte; buflen:integer):integer;
function LowestUsed(bufid:byte; buflen:integer):integer;
function IsBlockEmpty(bufid:byte; startadr:integer; len:integer):boolean;
function IsIntelHex(fname:string; bufid:byte):boolean;
function IsIntelHexFilename(fname:string):boolean;

var
  flashbuffer:array[0..MAX_FLASH_SIZE-1] of byte;
  eeprombuffer:array[0..MAX_EEPROM_SIZE-1] of byte;
  usersigbuffer:array[0..MAX_USERSIG_SIZE-1] of byte;

implementation

uses SysUtils;

procedure ClearBuffer(bufid:byte);
begin
  if bufid = BUF_FLASH then
    FillChar(flashbuffer, sizeof(flashbuffer), $FF)
  else if bufid = BUF_EEPROM then
    FillChar(eeprombuffer, sizeof(eeprombuffer), $FF)
  else if bufid = BUF_USERSIG then
    FillChar(usersigbuffer, sizeof(usersigbuffer), $FF);
end;

function LoadFile(bufid:byte; filetype:integer; fname:string; buflen:integer;
  var minaddr, maxaddr:integer):string;
begin
  if filetype = FILE_TYPE_INTELHEX then
    Result:=LoadIntelHex(bufid, fname, buflen, minaddr, maxaddr)
  else
    Result:=LoadBinaryFile(bufid, fname, buflen, minaddr, maxaddr);
end;

function SaveFile(bufid:byte; filetype:integer; fname:string; buflen:integer):string;
begin
  if filetype = FILE_TYPE_INTELHEX then
    Result:=SaveIntelHex(bufid, fname, buflen)
  else
    Result:=SaveBinaryFile(bufid, fname, buflen);
end;

function LoadIntelHex(bufid:byte; fname:string; buflen:integer;
  var minaddr, maxaddr:integer):string;
var
  line:string;
  radr,segadr,aktline,code,p_cr,p_lf:integer;
  b,rlen,rtyp,x,n:byte;
  f:textfile;
begin
  if buflen = 0 then
  begin
    Result:='Can''t load file';
    Exit;
  end;
  AssignFile(f, fname);
  {$I-} Reset(f); {$I+}
  if IOResult <> 0 then
  begin
    Result:='Can''t open file '+fname;
    Exit;
  end;
  minaddr:=High(integer);
  maxaddr:=Low(integer);
  segadr:=0;
  aktline:=0;
  rtyp:=1;
  repeat
    Readln(f, line);
    repeat
      Inc(aktline);
      if (line <> '') then
      begin
        if (line[1] <> ':') then
        begin
          CloseFile(f);
          Result:='Error in file '+fname+', line '+IntToStr(aktline);
          Exit;
        end;
        Val('$' + Copy(line, 2, 2), rlen, code);
        if code <> 0 then
        begin
          CloseFile(f);
          Result:='Error in file '+fname+', line '+IntToStr(aktline);
          Exit;
        end;
        Val('$' + Copy(line, 4, 4), radr, code);
        if code <> 0 then
        begin
          CloseFile(f);
          Result:='Error in file '+fname+', line '+IntToStr(aktline);
          Exit;
        end;
        Val('$' + Copy(line, 8, 2), rtyp, code);
        if code <> 0 then
        begin
          CloseFile(f);
          Result:='Error in file '+fname+', line '+IntToStr(aktline);
          Exit;
        end;
        if rtyp = 2 then
        begin
          // Extended Segment Address Record
          Val('$' + Copy(line, 10, 2), b, code);
          if code <> 0 then
          begin
            CloseFile(f);
            Result:='Error in file '+fname+', line '+IntToStr(aktline);
            Exit;
          end;
          segadr:=longword(b) shl 12;
          Val('$' + Copy(line, 12, 2), b, code);
          if code <> 0 then
          begin
            CloseFile(f);
            Result:='Error in file '+fname+', line '+IntToStr(aktline);
            Exit;
          end;
          segadr:=longword(segadr) + (longword(b) shl 4);
        end
        else if rtyp = 4 then
        begin
          // Extended Linear Address Record
          Val('$' + Copy(line, 10, 2), b, code);
          if code <> 0 then
          begin
            CloseFile(f);
            Result:='Error in file '+fname+', line '+IntToStr(aktline);
            Exit;
          end;
          segadr:=longword(b) shl 24;
          Val('$' + Copy(line, 12, 2), b, code);
          if code <> 0 then
          begin
            CloseFile(f);
            Result:='Error in file '+fname+', line '+IntToStr(aktline);
            Exit;
          end;
          segadr:=longword(segadr) + (longword(b) shl 16);
        end
        else if rtyp = 0 then
        begin
          // Data Record
          radr:=radr + segadr;  // add Extended Segment Address
          x:=10;
          for n:=1 to rlen do
          begin
            Val('$' + Copy(line, x, 2), b, code);
            if code <> 0 then
            begin
              CloseFile(f);
              Result:='Error in file '+fname+', line '+IntToStr(aktline);
              Exit;
            end;
            if radr < buflen then
            begin
              if bufid = BUF_FLASH then
                flashbuffer[radr]:=b
              else if bufid = BUF_EEPROM then
                eeprombuffer[radr]:=b
              else if bufid = BUF_USERSIG then
                usersigbuffer[radr]:=b;
              // search lowest used address
              if (minaddr > radr) then
                minaddr:=radr;
              // search highest used address
              if (maxaddr < radr) then
                maxaddr:=radr;
            end;
            Inc(x, 2);
            Inc(radr);
          end;
        end;
      end;
      p_cr:=Pos(#13, line);
      if p_cr <> 0 then
        Delete(line, 1, p_cr);
      p_lf:=Pos(#10, line);
      if p_lf <> 0 then
        Delete(line, 1, p_lf);
    until (line='') or ((p_cr=0) and (p_lf=0));
  until (rtyp=1) or Eof(f);
  CloseFile(f);
  Result:='';
end;

function SaveIntelHex(bufid:byte; fname:string; buflen:integer):string;
var
  line:string;
  f:textfile;
  radr,d1:integer;
  b,n,rsum,rlen:byte;
  use_segaddr,use_linaddr:boolean;
begin
  if buflen = 0 then
  begin
    Result:='Can''t save file';
    Exit;
  end;
  AssignFile(f, fname);
  {$I-} Rewrite(f); {$I+}
  if IOResult <> 0 then
  begin
    Result:='Can''t create file ' + fname;
    Exit;
  end;
  use_segaddr:=(buflen > 65536) and (buflen <= 1048576);
  use_linaddr:=(buflen > 1048576);
  radr:=0;
  d1:=buflen;
  repeat
    if (radr > 0) and ((radr and $ffff) = 0) then
    begin
      if (use_segaddr) then
      begin
        // Extended Segment Address Record (data size over 64 KB and up to 1 MB)
        rsum:=4 + Hi(word(radr shr 4)) + Lo(word(radr shr 4));
        line:=':02000002' + IntToHex(radr shr 4, 4) + IntToHex(byte(0 - rsum), 2);
        Writeln(f, line);
      end
      else
      if (use_linaddr) then
      begin
        // Extended Linear Address Record (data size over 1 MB)
        rsum:=6 + Hi(word(radr shr 16)) + Lo(word(radr shr 16));
        line:=':02000004' + IntToHex(radr shr 16, 4) + IntToHex(byte(0 - rsum), 2);
        Writeln(f, line);
      end;
    end;
    if d1 > 16 then
      rlen:=16
    else
      rlen:=d1;
    line:=':' + IntToHex(rlen, 2) + IntToHex(radr and $ffff, 4) + '00';
    rsum:=rlen + Hi(word(radr)) + Lo(word(radr));
    for n:=0 to rlen - 1 do
    begin
      if bufid = BUF_FLASH then
        b:=flashbuffer[radr]
      else if bufid = BUF_EEPROM then
        b:=eeprombuffer[radr]
      else if bufid = BUF_USERSIG then
        b:=usersigbuffer[radr]
      else
        b:=$ff;
      line:=line + IntToHex(b, 2);
      rsum:=rsum + b;
      Inc(radr);
      Dec(d1);
    end;
    line:=line + IntToHex(byte(0 - rsum), 2);
    Writeln(f, line);
  until d1 = 0;
  Writeln(f, ':00000001FF');
  CloseFile(f);
  Result:='';
end;

function LoadBinaryFile(bufid:byte; fname:string; buflen:integer;
  var minaddr, maxaddr:integer):string;
var
  f:file;
const
  numrd:integer = 0;
begin
  if buflen = 0 then
  begin
    Result:='Can''t load file';
    Exit;
  end;
  AssignFile(f, fname);
  {$I-} Reset(f, 1); {$I+}
  if IOResult <> 0 then
  begin
    Result:='Can''t open file ' + fname;
    Exit;
  end;
  minaddr:=0;
  if bufid = BUF_FLASH then
    BlockRead(f, flashbuffer, buflen, numrd)
  else if bufid = BUF_EEPROM then
    BlockRead(f, eeprombuffer, buflen, numrd)
  else if bufid = BUF_USERSIG then
    BlockRead(f, usersigbuffer, buflen, numrd);
  CloseFile(f);
  if numrd <= 0 then
  begin
    Result:='File ' + fname + ' is empty';
    Exit;
  end;
  maxaddr:=numrd - 1;
  Result:='';
end;

function SaveBinaryFile(bufid:byte; fname:string; buflen:integer):string;
var
  f:file;
const
  numwr:integer = 0;
begin
  if buflen = 0 then
  begin
    Result:='Can''t save file';
    Exit;
  end;
  AssignFile(f, fname);
  {$I-} Rewrite(f, 1); {$I+}
  if IOResult <> 0 then
  begin
    Result:='Can''t create file ' + fname;
    Exit;
  end;
  if bufid = BUF_FLASH then
    BlockWrite(f, flashbuffer, buflen, numwr)
  else if bufid = BUF_EEPROM then
    BlockWrite(f, eeprombuffer, buflen, numwr)
  else if bufid = BUF_USERSIG then
    BlockWrite(f, usersigbuffer, buflen, numwr);
  CloseFile(f);
  if numwr <> buflen then
  begin
    Result:='Can''t write file ' + fname;
    Exit;
  end;
  Result:='';
end;

function HighestUsed(bufid:byte; buflen:integer):integer;
var i:integer;
begin
  if bufid = BUF_FLASH then
  begin
    for i:=buflen - 1 downto 0 do
      if flashbuffer[i] <> $ff then
      begin
        Result:=i;
        Exit;
      end;
      Result:=-1;
  end
  else if bufid = BUF_EEPROM then
  begin
    for i:=buflen - 1 downto 0 do
      if eeprombuffer[i] <> $ff then
      begin
        Result:=i;
        Exit;
      end;
      Result:=-1;
  end
  else if bufid = BUF_USERSIG then
  begin
    for i:=buflen - 1 downto 0 do
      if usersigbuffer[i] <> $ff then
      begin
        Result:=i;
        Exit;
      end;
      Result:=-1;
  end
  else
    Result:=-1;
end;

function LowestUsed(bufid:byte; buflen:integer):integer;
var i:integer;
begin
  if bufid = BUF_FLASH then
  begin
    for i:=0 to buflen - 1 do
      if flashbuffer[i] <> $ff then
      begin
        Result:=i;
        Exit;
      end;
      Result:=-1;
  end
  else if bufid = BUF_EEPROM then
  begin
    for i:=0 to buflen - 1 do
      if eeprombuffer[i] <> $ff then
      begin
        Result:=i;
        Exit;
      end;
      Result:=-1;
  end
  else if bufid = BUF_USERSIG then
  begin
    for i:=0 to buflen - 1 do
      if usersigbuffer[i] <> $ff then
      begin
        Result:=i;
        Exit;
      end;
      Result:=-1;
  end
  else
    Result:=-1;
end;

function IsBlockEmpty(bufid:byte; startadr:integer; len:integer):boolean;
var i:integer;
begin
  Result:=false;
  if bufid = BUF_FLASH then
  begin
    for i:=startadr to startadr + len - 1 do
      if flashbuffer[i] <> $ff then
        Exit;
  end
  else if bufid = BUF_EEPROM then
  begin
    for i:=startadr to startadr + len - 1 do
      if eeprombuffer[i] <> $ff then
        Exit;
  end
  else if bufid = BUF_USERSIG then
  begin
    for i:=startadr to startadr + len - 1 do
      if usersigbuffer[i] <> $ff then
        Exit;
  end;
  Result:=true;
end;

function IsIntelHex(fname:string; bufid:byte):boolean;
var buflen, minaddr, maxaddr:integer;
begin
  Result:=false;
  if bufid = BUF_FLASH then
    buflen:=MAX_FLASH_SIZE
  else if bufid = BUF_EEPROM then
    buflen:=MAX_EEPROM_SIZE
  else if bufid = BUF_USERSIG then
    buflen:=MAX_USERSIG_SIZE
  else
    Exit;
  maxaddr:=0; minaddr:=0;
  if LoadIntelHex(bufid, fname, buflen, minaddr, maxaddr) = '' then
    Result:=true;
end;

function IsIntelHexFilename(fname:string):boolean;
var ext:string;
begin
  ext:=LowerCase(ExtractFileExt(fname));
  Result:=((ext = 'hex') or (ext = 'eep') or (ext = 'rom') or (ext = 'ihx'));
end;

end.
