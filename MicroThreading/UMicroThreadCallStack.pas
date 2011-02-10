unit UMicroThreadCallStack;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  UStackTrace;

type

  { TCallStackForm }

  TCallStackForm = class(TForm)
    ListView1: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    StackTrace: TStackTrace;
    procedure LoadStackTraceToListView(StackTrace: TStackTrace);
  public
    procedure Show(BasePointer: Pointer);
  end; 

implementation

{$R *.lfm}

procedure TCallStackForm.FormCreate(Sender: TObject);
begin
  StackTrace := TStackTrace.Create;
end;

procedure TCallStackForm.FormDestroy(Sender: TObject);
begin
  StackTrace.Free;
end;

procedure TCallStackForm.LoadStackTraceToListView(StackTrace: TStackTrace);
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
        SubItems.Add(IntToStr(LineNumber));
        SubItems.Add(FunctionClassName);
        SubItems.Add(FunctionName);
        SubItems.Add(Source);
      end;
    end;
    EndUpdate;
  end;
end;

procedure TCallStackForm.Show(BasePointer: Pointer);
begin
  StackTrace.GetCallStack(BasePointer);
  LoadStackTraceToListView(StackTrace);
  ShowModal;
end;

end.

