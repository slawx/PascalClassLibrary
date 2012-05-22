unit InpOut32;

{$MODE Delphi}

interface

{$INCLUDE Options.inc}

{$IFDEF USE_INPOUT32}

uses LCLIntf, {$IFDEF Windows}Windows,{$ENDIF} SysUtils;

//Functions exported from DLL.
//For easy inclusion is user projects.
//Original InpOut32 function support
var
  {$IFDEF Windows}
  Out32: procedure (PortAddress:SHORT; data:SHORT); stdcall;
  Inp32: function(PortAddress:SHORT):SHORT ; stdcall;

//My extra functions for making life easy
IsInpOutDriverOpen: function:BOOL ; stdcall; //Returns TRUE if the InpOut driver was opened successfully
IsXP64Bit: function: BOOL; stdcall; //Returns TRUE if the OS is 64bit (x64) Windows.
{$ENDIF}

LibraryLoaded: Boolean;
DLLHandle: HModule;

procedure LoadLibraries;
procedure FreeLibraries;

{$ENDIF}

implementation

{$IFDEF USE_INPOUT32}

procedure LoadLibraries;
begin
  {$IFDEF Windows}
  if not LibraryLoaded then begin
    DLLHandle := LoadLibrary('inpout32.dll');
    if DLLHandle <> 0 then begin
      @Out32 := GetProcAddress(DLLHandle, 'Out32');
      @Inp32 := GetProcAddress(DLLHandle, 'Inp32');
      @IsInpOutDriverOpen := GetProcAddress(DLLHandle, 'IsInpOutDriverOpen');
      @IsXP64Bit := GetProcAddress(DLLHandle, 'IsXP64Bit');
    end else raise Exception.Create('Missing library inpout32.dll');
    LibraryLoaded := True;
  end;
  {$ENDIF}
end;

procedure FreeLibraries;
begin
  {$IFDEF Windows}
  if DLLHandle <> 0 then FreeLibrary(DLLHandle);
  LibraryLoaded := False;
  {$ENDIF}
end;

initialization

LibraryLoaded := False;
//LoadLibraries;

finalization

if LibraryLoaded then FreeLibraries;

{$ENDIF}

end.