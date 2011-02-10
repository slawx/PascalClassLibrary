unit UMicroThreadList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Menus, DateUtils, UPlatform, UMicroThreadCallStack;

type

  { TMicroThreadListForm }

  TMicroThreadListForm = class(TForm)
  published
    Label10: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MenuItemCallStack: TMenuItem;
    PopupMenu1: TPopupMenu;
    TimerRedraw: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    ListView1: TListView;
    ListView2: TListView;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure ListView2Data(Sender: TObject; Item: TListItem);
    procedure MenuItemCallStackClick(Sender: TObject);
    procedure TimerRedrawTimer(Sender: TObject);
  private
    CallStackForm: TCallStackForm;
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

uses
  UMicroThreading;

{ TMicroThreadListForm }

procedure TMicroThreadListForm.TimerRedrawTimer(Sender: TObject);
var
  ThreadCount: Integer;
begin
  if ListView1.Items.Count <> MainScheduler.MicroThreadCount then
    ListView1.Items.Count := MainScheduler.MicroThreadCount;
  ListView1.Items[-1];
  ListView1.Refresh;

  ThreadCount := MainScheduler.ThreadPoolCount;
  if MainScheduler.UseMainThread then Inc(ThreadCount);
  if ListView2.Items.Count <> ThreadCount then
    ListView2.Items.Count := ThreadCount;
  ListView2.Items[-1];
  ListView2.Refresh;

  Label6.Caption := IntToStr(GetLogicalProcessorCount);
  Label9.Caption := IntToStr(MainScheduler.ThreadPoolCount);
  Label10.Caption := IntToStr(MainScheduler.MicroThreadCount);
  Label2.Caption := FloatToStr(MainScheduler.MainThreadOutsideDuration / OneMillisecond) + ' ms';
end;

procedure TMicroThreadListForm.ListView1Data(Sender: TObject; Item: TListItem
  );
begin
  try
    MainScheduler.MicroThreadsLock.Acquire;
    if Item.Index < MainScheduler.MicroThreads.Count then
    with TMicroThread(MainScheduler.MicroThreads[Item.Index]) do begin
      Item.Caption := IntToStr(Id);
      Item.Data := TMicroThread(MainScheduler.MicroThreads[Item.Index]);
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

procedure TMicroThreadListForm.ListView2Data(Sender: TObject; Item: TListItem);
var
  Increment: Integer;
begin
  if MainScheduler.UseMainThread then Increment := 1
    else Increment := 0;

  if Item.Index < (MainScheduler.ThreadPoolCount + Increment) then begin
    if MainScheduler.UseMainThread and (Item.Index = 0) then begin
      Item.Caption := IntToStr(MainThreadID);
      Item.SubItems.Add('');
      Item.SubItems.Add(IntToStr(MainScheduler.MainThreadManager.GetCurrentMicroThreadId));
      Item.SubItems.Add(FloatToStr(MainScheduler.MainThreadManager.LoopDuration / OneMillisecond) + ' ms');
    end else
    try
      MainScheduler.ThreadPoolLock.Acquire;
      with TMicroThreadThread(MainScheduler.ThreadPool[Item.Index - Increment]) do begin
        Item.Caption := IntToStr(ThreadID);
        Item.SubItems.Add(MicroThreadThreadStateText[State]);
        Item.SubItems.Add(IntToStr(Manager.GetCurrentMicroThreadId));
        Item.SubItems.Add(FloatToStr(Manager.LoopDuration / OneMillisecond) + ' ms');
      end;
    finally
      MainScheduler.ThreadPoolLock.Release;
    end;
  end;
end;

procedure TMicroThreadListForm.MenuItemCallStackClick(Sender: TObject);
begin
  if Assigned(ListView1.Selected) then
  with TMicroThread(ListView1.Selected.Data) do begin
    //Suspend;
    CallStackForm.Show(BasePointer);
    //Resume;
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

procedure TMicroThreadListForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  TimerRedraw.Enabled := False;
end;

procedure TMicroThreadListForm.FormCreate(Sender: TObject);
begin
  CallStackForm := TCallStackForm.Create(nil);
end;

procedure TMicroThreadListForm.FormDestroy(Sender: TObject);
begin
  CallStackForm.Free;
end;


end.

