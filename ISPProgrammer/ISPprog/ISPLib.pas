unit ISPLib;

{$MODE Delphi}

interface

{$INCLUDE Options.inc}

uses LCLIntf;

{$IFDEF I2C_SUPPORT}
function  isplib_begin_work:integer; stdcall;
function  isplib_end_work:integer; stdcall;
function  isplib_scan (buff:PAnsiChar; bufflen:DWORD):integer; stdcall;
function  isplib_check:integer; stdcall;
procedure isplib_error_desc(code:integer; buff:PAnsiChar; len:DWORD); stdcall;
function  isplib_erase_all:integer; stdcall;
{$ENDIF}

implementation

{$IFDEF I2C_SUPPORT}
const
  isplib_dll = 'isplib.dll';

function  isplib_begin_work; external isplib_dll;
function  isplib_end_work; external isplib_dll;
function  isplib_scan; external isplib_dll;
function  isplib_check; external isplib_dll;
procedure isplib_error_desc; external isplib_dll;
function  isplib_erase_all; external isplib_dll;
{$ENDIF}

end.

