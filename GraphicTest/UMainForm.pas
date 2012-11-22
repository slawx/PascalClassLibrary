unit UMainForm;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, DateUtils, UPlatform, LCLType, IntfGraphics, fpImage,
  Math, GraphType, Contnrs, LclIntf, Spin, UFastBitmap, UDrawMethod;

const
  SceneFrameCount = 100;

type


  { TMainForm }

  TMainForm = class(TForm)
    ButtonStop: TButton;
    ButtonBenchmark: TButton;
    ButtonSingleTest: TButton;
    CheckBoxEraseBackground: TCheckBox;
    CheckBoxDoubleBuffered: TCheckBox;
    FloatSpinEdit1: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ListViewMethods: TListView;
    SpinEditWidth: TSpinEdit;
    SpinEditHeight: TSpinEdit;
    Timer1: TTimer;
    procedure ButtonBenchmarkClick(Sender: TObject);
    procedure ButtonSingleTestClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure CheckBoxDoubleBufferedChange(Sender: TObject);
    procedure CheckBoxEraseBackgroundChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ListViewMethodsData(Sender: TObject; Item: TListItem);
    procedure ListViewMethodsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure SpinEditHeightChange(Sender: TObject);
    procedure SpinEditWidthChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    MethodIndex: Integer;
    SingleTestActive: Boolean;
    AllTestActive: Boolean;
    procedure GenerateSceneFrames;
    procedure UpdateMethodList;
    procedure UpdateInterface;
    procedure UpdateFrameSize;
  public
    FrameSize: TPoint;
    DrawMethods: TObjectList; // TObjectList<TDrawMethod>
    Scenes: TObjectList; // TObjectList<TFastBitmap>
    SceneIndex: Integer;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  UDrawForm;


{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  NewDrawMethod: TDrawMethod;
  I: Integer;
begin
  Scenes := TObjectList.Create;

  FrameSize := Point(320, 240);
  Randomize;

  DrawMethods := TObjectList.Create;
  for I := 0 to High(DrawMethodClasses) do begin
    NewDrawMethod := DrawMethodClasses[I].Create;
    DrawMethods.Add(NewDrawMethod);
  end;
end;

procedure TMainForm.ButtonSingleTestClick(Sender: TObject);
var
  StepStartTime: TDateTime;
begin
  try
    SingleTestActive := True;
    UpdateInterface;
    Timer1.Enabled := True;
    MethodIndex := ListViewMethods.Selected.Index;
    Timer1.Enabled := True;
    if MethodIndex >= 0 then
    with TDrawMethod(DrawMethods[MethodIndex]) do begin
      Init(DrawForm, FrameSize);
      Application.ProcessMessages;
      repeat
        StepStartTime := NowPrecise;
        DrawFrameTiming(TFastBitmap(Scenes[SceneIndex]));
        SceneIndex := (SceneIndex + 1) mod Scenes.Count;
        Application.ProcessMessages;
        StepDuration := NowPrecise - StepStartTime;
      until not SingleTestActive;
      Done;
    end;
  finally
    Timer1.Enabled := False;
    SingleTestActive := False;
    UpdateInterface;
  end;
end;

procedure TMainForm.ButtonBenchmarkClick(Sender: TObject);
var
  I: Integer;
  StartTime: TDateTime;
  StepStartTime: TDateTime;
begin
  try
    AllTestActive := True;
    UpdateInterface;
    Timer1.Enabled := True;
    with ListViewMethods, Items do
    for I := 0 to DrawMethods.Count - 1 do
    with TDrawMethod(DrawMethods[I]) do begin
      Init(DrawForm, FrameSize);
      MethodIndex := I;
      StartTime := NowPrecise;
      repeat
        StepStartTime := NowPrecise;
        DrawFrameTiming(TFastBitmap(Scenes[SceneIndex]));
        SceneIndex := (SceneIndex + 1) mod Scenes.Count;
        Application.ProcessMessages;
        StepDuration := NowPrecise - StepStartTime;
      until ((NowPrecise - StartTime) > OneSecond * FloatSpinEdit1.Value) or not AllTestActive;
      Done;
    end;
  finally
    Timer1.Enabled := False;
    AllTestActive := False;
    UpdateInterface;
  end;
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  SingleTestActive := False;
  AllTestActive := False;
end;

procedure TMainForm.CheckBoxDoubleBufferedChange(Sender: TObject);
begin
  DrawForm.DoubleBuffered := CheckBoxDoubleBuffered.Checked;
end;

procedure TMainForm.CheckBoxEraseBackgroundChange(Sender: TObject);
begin
  DrawForm.EraseBackgroundEnabled := CheckBoxEraseBackground.Checked;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ButtonStopClick(Self);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ListViewMethods.Clear;
  FreeAndNil(DrawMethods);
  FreeAndNil(Scenes);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  UpdateFrameSize;
  UpdateMethodList;
  UpdateInterface;
  DrawForm.Show;
end;

procedure TMainForm.ListViewMethodsData(Sender: TObject; Item: TListItem);
begin
  if (Item.Index >= 0) and (Item.Index < DrawMethods.Count) then
  with TDrawMethod(DrawMethods[Item.Index]) do begin
    Item.Caption := Caption;
    Item.SubItems.Add(FloatToStr(RoundTo(FrameDuration / OneMillisecond, -3)));
    if FrameDuration > 0 then
      Item.SubItems.Add(FloatToStr(RoundTo(1 / (FrameDuration / OneSecond), -3)))
      else Item.SubItems.Add('0');
    Item.SubItems.Add(FloatToStr(RoundTo(StepDuration / OneMillisecond, -3)));
    if FrameDuration > 0 then
      Item.SubItems.Add(FloatToStr(RoundTo(1 / (StepDuration / OneSecond), -3)))
      else Item.SubItems.Add('0');
  end;
end;

procedure TMainForm.ListViewMethodsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  UpdateInterface;
end;

procedure TMainForm.SpinEditHeightChange(Sender: TObject);
begin
  FrameSize.Y := SpinEditHeight.Value;
  UpdateFrameSize;
end;

procedure TMainForm.SpinEditWidthChange(Sender: TObject);
begin
  FrameSize.X := SpinEditWidth.Value;
  UpdateFrameSize;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  UpdateMethodList;
end;

procedure TMainForm.GenerateSceneFrames;
var
  I: Integer;
  NewScene: TFastBitmap;
begin
  Scenes.Clear;
  for I := 0 to SceneFrameCount - 1 do begin
    NewScene := TFastBitmap.Create;
    NewScene.Size := FrameSize;
    NewScene.RandomImage;
    Scenes.Add(NewScene);
  end;
end;

procedure TMainForm.UpdateMethodList;
begin
  ListViewMethods.Items.Count := DrawMethods.Count;
  ListViewMethods.Refresh;
end;

procedure TMainForm.UpdateInterface;
begin
  ButtonSingleTest.Enabled := not SingleTestActive and not AllTestActive and Assigned(ListViewMethods.Selected);
  ButtonBenchmark.Enabled := not AllTestActive and not SingleTestActive;
  ButtonStop.Enabled := SingleTestActive or AllTestActive;
  SpinEditWidth.MaxValue := Screen.DesktopWidth;
  SpinEditHeight.MaxValue := Screen.DesktopHeight;
  CheckBoxDoubleBuffered.Checked := DrawForm.DoubleBuffered;
  CheckBoxEraseBackground.Checked := DrawForm.EraseBackgroundEnabled;
end;

procedure TMainForm.UpdateFrameSize;
begin
  DrawForm.ClientWidth := FrameSize.X;
  DrawForm.ClientHeight := FrameSize.Y;
  GenerateSceneFrames;
end;

end.

