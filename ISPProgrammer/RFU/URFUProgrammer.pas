unit URFUProgrammer;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, UISPProgrammer, UCPUType, Process;

type

  { TRFUProgrammer }

  TRFUProgrammer = class(TISPProgrammer)
    procedure Write; override;
    procedure Verify; override;
    procedure Erase; override;
    procedure Reset; override;
    constructor Create; override;
    destructor Destroy; override;
  end;


implementation

{ TRFUProgrammer }

procedure TRFUProgrammer.Write;
begin
  inherited Write;
  with TProcess.Create(nil) do
  try
    CommandLine := 'clRFU.exe ' + FileName + ' -cl coldload.bin -pb pilot.bin';
    CurrentDirectory := ExtractFileDir(Application.ExeName) +
      DirectorySeparator + 'Programmer' + DirectorySeparator + 'RFU';
    Options := [];
    Execute;
  finally
    Free;
  end;
end;

procedure TRFUProgrammer.Verify;
begin
  inherited Verify;
end;

procedure TRFUProgrammer.Erase;
begin
  inherited Erase;
end;

procedure TRFUProgrammer.Reset;
begin
  inherited Reset;
end;

constructor TRFUProgrammer.Create;
begin
  inherited Create;
  Capabilities := [ipcWrite];
  CPUType := ctRC3000;
end;

destructor TRFUProgrammer.Destroy;
begin
  inherited Destroy;
end;

end.

