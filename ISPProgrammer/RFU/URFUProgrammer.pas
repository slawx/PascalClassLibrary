unit URFUProgrammer;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, UISPProgrammer, UCPUType, Process, UJobProgressView;

type

  { TRFUProgrammer }

  TRFUProgrammer = class(TISPProgrammer)
    procedure Write(Job: TJob); override;
    procedure Verify(Job: TJob); override;
    procedure Erase; override;
    procedure Reset; override;
    constructor Create; override;
    destructor Destroy; override;
  end;


implementation

{ TRFUProgrammer }

procedure TRFUProgrammer.Write(Job: TJob);
begin
  inherited;
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

procedure TRFUProgrammer.Verify(Job: TJob);
begin
  inherited;
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

