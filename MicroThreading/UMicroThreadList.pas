unit UMicroThreadList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls;

type

  { TMicroThreadListForm }

  TMicroThreadListForm = class(TForm)
    ListView1: TListView;
    TimerRedraw: TTimer;
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure TimerRedrawTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

uses
  UMicroThreading;

{ TMicroThreadListForm }

procedure TMicroThreadListForm.TimerRedrawTimer(Sender: TObject);
begin
  if ListView1.Items.Count <> MainScheduler.MicroThreadCount then
    ListView1.Items.Count := MainScheduler.MicroThreadCount;
  ListView1.Items[-1];
  ListView1.Refresh;
end;

procedure TMicroThreadListForm.ListView1Data(Sender: TObject; Item: TListItem
  );
begin
  try
    MainScheduler.MicroThreadsLock.Acquire;
    if Item.Index < MainScheduler.MicroThreads.Count then
    with TMicroThread(MainScheduler.MicroThreads[Item.Index]) do begin
      Item.Caption := IntToStr(Id);
      Item.SubItems.Add('');
      Item.SubItems.Add(IntToStr(Priority));
      Item.SubItems.Add(MicroThreadStateText[State]);
      Item.SubItems.Add(MicroThreadBlockStateText[BlockState]);
      Item.SubItems.Add(FloatToStr(ExecutionTime));
      Item.SubItems.Add(IntToStr(ExecutionCount));
      Item.SubItems.Add(IntToStr(Trunc(Completion * 100)) + '%');
      Item.SubItems.Add(IntToStr(StackUsed));
      Item.SubItems.Add(Name);
    end;
  finally
    MainScheduler.MicroThreadsLock.Release;
  end;
end;

procedure TMicroThreadListForm.FormShow(Sender: TObject);
begin
  TimerRedraw.Enabled := True;
end;

procedure TMicroThreadListForm.FormHide(Sender: TObject);
begin
  TimerRedraw.Enabled := False;
end;


end.

