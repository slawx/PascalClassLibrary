unit UMainForm;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynHighlighterPas, SynMemo, Forms, Controls,
  Graphics, Dialogs, ComCtrls, ExtCtrls, StdCtrls, DateUtils, UPlatform,
  LCLType, IntfGraphics, fpImage, Math, GraphType, Contnrs, LclIntf, Spin,
  UFastBitmap, UDrawMethod;

const
  SceneFrameCount = 100;

type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonBenchmark: TButton;
    ButtonSingleTest: TButton;
    ButtonStop: TButton;
    CheckBoxDoubleBuffered: TCheckBox;
    CheckBoxEraseBackground: TCheckBox;
    FloatSpinEdit1: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ListViewMethods: TListView;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    SpinEditHeight: TSpinEdit;
    SpinEditWidth: TSpinEdit;
    Splitter1: TSplitter;
    SynMemo1: TSynMemo;
    SynPasSyn1: TSynPasSyn;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TimerUpdateList: TTimer;
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
    procedure TimerUpdateListTimer(Sender: TObject);
  private
    MethodIndex: Integer;
    SingleTestActive: Boolean;
    AllTestActive: Boolean;
    TestTerminated: Boolean;
    TestTimeout: Real;
    procedure GenerateSceneFrames;
    procedure TestMethod(Method: TDrawMethod);
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
  UDrawForm, ULazIntfImageColorsCopy, ULazIntfImageColorsNoCopy, UCanvasPixels,
  UCanvasPixelsUpdateLock, UBGRABitmapPaintBox, UBitmapRawImageDataPaintBox,
  UBitmapRawImageData, UBitmapRawImageDataMove, UDummyMethod, UOpenGLMethod,
  UOpenGLPBOMethod;

const
  DrawMethodClasses: array[0..{$IFDEF opengl}10{$ELSE}8{$ENDIF}] of TDrawMethodClass = (
    TCanvasPixels, TCanvasPixelsUpdateLock, TLazIntfImageColorsCopy,
    TLazIntfImageColorsNoCopy, TBitmapRawImageData, TBitmapRawImageDataPaintBox,
    TBitmapRawImageDataMove, TBGRABitmapPaintBox{$IFDEF opengl}, TOpenGLMethod, TOpenGLPBOMethod{$ENDIF}
    ,TDummyMethod);



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

  PageControl1.TabIndex := 0;
end;

procedure TMainForm.TestMethod(Method: TDrawMethod);
var
  StepStartTime: TDateTime;
  StartTime: TDateTime;
begin
  with Method do begin
    Init(DrawForm, FrameSize);
    TestTerminated := False;
    //Application.ProcessMessages;
    StartTime := NowPrecise;
    repeat
      StepStartTime := NowPrecise;
      DrawFrameTiming(TFastBitmap(Scenes[SceneIndex]));
      Application.ProcessMessages;
      StepDuration := NowPrecise - StepStartTime;
      SceneIndex := (SceneIndex + 1) mod Scenes.Count;
      Inc(FrameCounter);
    until TestTerminated or
      ((TestTimeout > 0) and ((NowPrecise - StartTime) > OneSecond * TestTimeout));
    Done;
  end;
end;

procedure TMainForm.ButtonSingleTestClick(Sender: TObject);
begin
  if Assigned(ListViewMethods.Selected) then
  try
    SingleTestActive := True;
    UpdateInterface;
    TimerUpdateList.Enabled := True;
    MethodIndex := ListViewMethods.Selected.Index;
    TestTimeout := -1;
    if MethodIndex >= 0 then
      TestMethod(TDrawMethod(DrawMethods[MethodIndex]));
  finally
    TimerUpdateList.Enabled := False;
    SingleTestActive := False;
    UpdateInterface;
  end;
end;

procedure TMainForm.ButtonBenchmarkClick(Sender: TObject);
var
  I: Integer;
begin
  try
    AllTestActive := True;
    UpdateInterface;
    TimerUpdateList.Enabled := True;
    TestTerminated := False;
    TestTimeout := FloatSpinEdit1.Value;
    with ListViewMethods, Items do
    for I := 0 to DrawMethods.Count - 1 do
    with TDrawMethod(DrawMethods[I]) do begin
      TestMethod(TDrawMethod(DrawMethods[I]));
    end;
  finally
    TimerUpdateList.Enabled := False;
    AllTestActive := False;
    UpdateInterface;
  end;
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  TestTerminated := True;
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
    Item.SubItems.Add(FloatToStr(RoundTo(FPS, -3)));
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
var
  FileName: string;
begin
  UpdateInterface;
  if Assigned(ListViewMethods.Selected) then begin
    FileName := 'Methods' + DirectorySeparator + 'U' +
    Copy(TDrawMethod(DrawMethods[ListViewMethods.Selected.Index]).ClassName, 2, High(Integer)) + '.pas';

    if FileExistsUTF8(FileName) then
      SynMemo1.Lines.LoadFromFile(FileName)
      else SynMemo1.Lines.Clear;
    Memo1.Lines.Assign(TDrawMethod(DrawMethods[ListViewMethods.Selected.Index]).Description);
  end;
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

procedure TMainForm.TimerUpdateListTimer(Sender: TObject);
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

