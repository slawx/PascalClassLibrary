unit UFrameLog;

interface

uses
  Classes, UPin, UMemoryStreamEx, Dialogs, SysUtils, UTextFileStream;

type
  TFrameLog = class
  private
    FFileName: string;
    TextFile: TTextFileStream;
    FActive: Boolean;
    procedure ReceiveExt(Stream: TStream);
    procedure ReceiveInt(Stream: TStream);
    procedure SetFileName(const Value: string);
    procedure SetActive(const Value: Boolean);
  public
    DataPinExt: TPin;
    DataPinInt: TPin;
    destructor Destroy; override;
    constructor Create;
    property Active: Boolean read FActive write SetActive;
    property FileName: string read FFileName write SetFileName;
  end;

implementation

{ TFrameLog }

constructor TFrameLog.Create;
begin
  inherited;
  DataPinExt := TPin.Create;
  DataPinExt.OnReceive := ReceiveExt;
  DataPinInt := TPin.Create;
  DataPinInt.OnReceive := ReceiveInt;
  FileName := 'FrameLog.txt';
  FActive := False;
  TextFile := nil;
end;

destructor TFrameLog.Destroy;
begin
  TextFile.Free;
  DataPinExt.Free;
  DataPinInt.Free;
  inherited;
end;

procedure TFrameLog.ReceiveExt(Stream: TStream);
var
  I: Integer;
  Output: string;
begin
  if Assigned(TextFile) then begin
    Output := 'Rx: ';
    for I := 0 to Stream.Size - 1 do
      Output := Output + IntToHex(TMemoryStreamEx(Stream).ReadByte, 2) + ' ';
    TextFile.WriteLn(Output);
  end;
  DataPinInt.Send(Stream);
end;

procedure TFrameLog.ReceiveInt(Stream: TStream);
var
  I: Integer;
  Output: string;
begin
  if Assigned(TextFile) then begin
    Output := 'Tx: ';
    for I := 0 to Stream.Size - 1 do
      Output := Output + IntToHex(TMemoryStreamEx(Stream).ReadByte, 2) + ' ';
    TextFile.WriteLn(Output);
  end;
  DataPinExt.Send(Stream);
end;

procedure TFrameLog.SetActive(const Value: Boolean);
begin
  if FActive and (not Value) then TextFile.Free
  else if (not FActive) and Value then TextFile := TTextFileStream.Create(FileName, fmCreate);
  FActive := Value;
end;

procedure TFrameLog.SetFileName(const Value: string);
begin
  FFileName := Value;
  if FActive then begin
    TextFile.Free;
    TextFile := TTextFileStream.Create(FileName, fmCreate);
  end;
end;

end.
