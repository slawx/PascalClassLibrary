unit PortsIO;

{$MODE Delphi}

interface

{$INCLUDE Options.inc}

function  InPort(adr:word):byte;
procedure OutPort(adr:word;y:byte);
procedure Init;

implementation

uses
  LCLIntf, SysUtils {$IFDEF WINDOWS},Windows{$ENDIF}
{$IFDEF USE_INPOUT32}
  , InpOut32
{$ENDIF}
  ;

var
  Initialized: Boolean;

function InPort_direct(adr:word):byte; assembler;
asm
  mov  dx,adr
  in   al,dx
end;

procedure OutPort_direct(adr:word;y:byte);
begin
  asm
    mov  dx,adr
    mov  al,y
    out  dx,al
  end;
end;

function InPort(adr:word):byte;
begin
  {$IFDEF WINDOWS}
{$IFDEF USE_INPOUT32}
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result:=Inp32(adr) and $ff
  else
    Result:=InPort_direct(adr);
{$ENDIF}

{$IFDEF USE_GIVEIO}
  Result:=InPort_direct(adr);
{$ENDIF}
{$ENDIF}
end;

procedure OutPort(adr:word;y:byte);
begin
  {$IFDEF WINDOWS}
{$IFDEF USE_INPOUT32}
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Out32(adr,y)
  else
    OutPort_direct(adr,y);
{$ENDIF}

{$IFDEF USE_GIVEIO}
  OutPort_direct(adr,y);
{$ENDIF}
{$ENDIF}
end;

{$IFDEF USE_GIVEIO}
function InitializeGiveIo:boolean;
var
  hDriver:THandle;
begin
  hDriver := CreateFile('\\.\giveio', GENERIC_READ, 0, nil, OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL, 0);
  if hDriver = INVALID_HANDLE_VALUE then
    Result:=false
  else
  begin
    if hDriver <> 0 then
      CloseHandle(hDriver);
    Result:=true;
  end;
end;
{$ENDIF}

procedure Init;
begin
  {$IFDEF WINDOWS}
  if not Initialized then begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
{$IFDEF USE_GIVEIO}
    if not InitializeGiveIo then
    begin
      MessageBox(0,
        'Can''t find giveio.sys driver.' + #13#10#13#10 +
        'The giveio.sys driver must be installed' + #13#10 +
        'before running ISP Programmer.' + #13#10#13#10 +
        'Uninstall and install ISP Programmer again.',
        'ISP Programmer', MB_OK or MB_ICONERROR);
      ExitProcess(1);
    end;
{$ENDIF}

{$IFDEF USE_INPOUT32}
    if not IsInpOutDriverOpen then
    begin
      MessageBox(0,
        'Can''t initialize InpOut32 driver.' + #13#10#13#10 +
        'The InpOut32 driver must be installed' + #13#10 +
        'before running ISP Programmer.' + #13#10#13#10 +
        'Run ISP Programmer with administrator rights' + #13#10 +
        'or uninstall and install ISP Programmer again.',
        'ISP Programmer', MB_OK or MB_ICONERROR);
      ExitProcess(1);
    end;
{$ENDIF}
  end;
  Initialized := True;
  end;
  {$ENDIF}
end;

initialization

Initialized := False;

end.
