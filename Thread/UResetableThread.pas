unit UResetableThread;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, syncobjs;

type
  TResetableThread = class;

  { TResetableThreadExecute }

  TResetableThreadExecute = class(TThread)
    Parent: TResetableThread;
    procedure Execute; override;
  end;

  { TResetableThread }

  TResetableThread = class
  private
    StartEvent: TEvent;
    StopEvent: TEvent;
  public
    Thread: TResetableThreadExecute;
    Stopped: Boolean;
    procedure Execute; virtual; abstract;
    procedure Stop;
    procedure Start;
    procedure WaitForStart;
    procedure WaitForStop;
    constructor Create;
    destructor Destroy; override;
  end;
  
implementation

{ TResetableThread }

procedure TResetableThread.Stop;
begin
  Stopped := True;
end;

procedure TResetableThread.Start;
begin
  if Stopped then StartEvent.SetEvent;
end;

procedure TResetableThread.WaitForStart;
var
  WaitResult: TWaitResult;
begin
  repeat
    WaitResult := StartEvent.WaitFor(1000);
  until WaitResult <> wrTimeout;
  if WaitResult = wrError then
    raise Exception.Create('WaitFor error');
end;

procedure TResetableThread.WaitForStop;
begin
  while StopEvent.WaitFor(1000) = wrTimeout do ;
end;

constructor TResetableThread.Create;
begin
  Stopped := True;
  StartEvent := TEvent.Create(nil, False, False, '');
  StopEvent := TEvent.Create(nil, False, False, '');
  Thread := TResetableThreadExecute.Create(True);
  Thread.Parent := Self;
  Thread.Resume;
end;

destructor TResetableThread.Destroy;
begin
  Stop;
  WaitForStop;
  Thread.Destroy;
  StartEvent.Destroy;
  StopEvent.Destroy;
  inherited Destroy;
end;

{ TResetableThreadExecute }

procedure TResetableThreadExecute.Execute;
begin
  while not Terminated do begin
    Parent.WaitForStart;
    Parent.Stopped := False;
    Parent.Execute;
    Parent.Stopped := True;
    Parent.StopEvent.SetEvent;
  end;
end;

end.

