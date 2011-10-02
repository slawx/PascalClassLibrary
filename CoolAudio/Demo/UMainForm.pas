unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, UAudioSystem, UAudioSystemFMOD, UAudioSystemMPlayer;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    ButtonStop: TButton;
    ButtonPlay: TButton;
    ButtonPause: TButton;
    Edit1: TEdit;
    TrackBar1: TTrackBar;
    procedure ButtonPauseClick(Sender: TObject);
    procedure ButtonPlayClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    Player: TPlayer;
    AudioSystem: TAudioSystem;
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  AudioSystem := TFMODAudioSystem.Create;
  Player := TFMODPlayer.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Player.Free;
  AudioSystem.Free;
end;

procedure TMainForm.ButtonPlayClick(Sender: TObject);
begin
  Player.Play;
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  Player.Stop;
end;

procedure TMainForm.ButtonPauseClick(Sender: TObject);
begin
  Player.Pause;
end;

end.

