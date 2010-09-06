unit UExceptionForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CustomLineInfo, Spin, ComCtrls, Contnrs, UStackTrace;

type

  { TMainForm }

  { TExceptionForm }

  TExceptionForm = class(TForm)
    ButtonKill: TButton;
    ButtonClose: TButton;
    ListView1: TListView;
    MemoExceptionInfo: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonKillClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
    procedure LoadStackTraceToListView(StackTrace: TStackTrace);
  end;

var
  ExceptionForm: TExceptionForm;

implementation

{$R *.lfm}

procedure TExceptionForm.FormShow(Sender: TObject);
begin
end;

procedure TExceptionForm.FormCreate(Sender: TObject);
begin

end;

procedure TExceptionForm.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TExceptionForm.ButtonKillClick(Sender: TObject);
begin
  Halt;
end;

procedure TExceptionForm.FormDestroy(Sender: TObject);
begin
end;

procedure TExceptionForm.LoadStackTraceToListView(StackTrace: TStackTrace);
var
  I: Integer;
  NewItem: TListItem;
begin
  with ListView1, Items do begin
    BeginUpdate;
    Clear;
    for I := 0 to StackTrace.Count - 1 do
    with TStackFrameInfo(StackTrace[I]) do begin
      NewItem := Add;
      with NewItem do begin
        Caption := IntToStr(Index);
        SubItems.Add(IntToHex(Address, 8));
        SubItems.Add(FunctionClassName);
        SubItems.Add(FunctionName);
        SubItems.Add(Source);
        SubItems.Add(IntToStr(LineNumber));
      end;
    end;
    EndUpdate;
  end;
end;

initialization

ExceptionForm := TExceptionForm.Create(nil);

finalization

FreeAndNil(ExceptionForm);

end.

