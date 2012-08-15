unit UCommMark;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, UCommPin, SpecializedList;

type

  { TCommMark }

  TCommMark = class
  private
    FActive: Boolean;
    FMarkIndex: Integer;
    FFrameData: TListByte;
    procedure RawDataReceive(Sender: TCommPin; Stream: TListByte);
    procedure RawSetStatus(Sender: TCommPin; Status: Integer);
    procedure FrameDataReceive(Sender: TCommPin; Stream: TListByte);
    procedure FrameSetStatus(Sender: TCommPin; Status: Integer);
    procedure SetActive(AValue: Boolean);
  public
    PinRaw: TCommPin;
    PinFrame: TCommPin;
    Mark: TListByte;
    procedure Reset;
    constructor Create;
    destructor Destroy; override;
    property Active: Boolean read FActive write SetActive;
  end;

implementation

{ TCommMark }

procedure TCommMark.RawDataReceive(Sender: TCommPin; Stream: TListByte);
var
  I: Integer;
begin
  for I := 0 to Stream.Count - 1 do
  if (Mark.Count > 0) and (Stream[I] = Mark[FMarkIndex]) then begin
    Inc(FMarkIndex);
    if FMarkIndex >= Mark.Count then begin
      PinFrame.Send(FFrameData);
      FFrameData.Count := 0;
      FMarkIndex := 0;
    end;
  end else begin
    FFrameData.Count := FFrameData.Count + 1;
    FFrameData[FFrameData.Count - 1] := Stream[I];
    FMarkIndex := 0;
    if Mark.Count = 0 then begin
      PinFrame.Send(FFrameData);
      FFrameData.Count := 0;
    end;
  end;
end;

procedure TCommMark.RawSetStatus(Sender: TCommPin; Status: Integer);
begin
  PinFrame.Status := Status;
end;

procedure TCommMark.FrameDataReceive(Sender: TCommPin; Stream: TListByte);
begin
  PinRaw.Send(Stream);
  PinRaw.Send(Mark);
end;

procedure TCommMark.FrameSetStatus(Sender: TCommPin; Status: Integer);
begin
  PinRaw.Status := Status;
end;

procedure TCommMark.SetActive(AValue: Boolean);
begin
  if FActive = AValue then Exit;
  FActive := AValue;
  if FActive then Reset;
end;

procedure TCommMark.Reset;
begin
  FMarkIndex := 0;
  FFrameData.Clear;
end;

constructor TCommMark.Create;
begin
  PinRaw := TCommPin.Create;
  PinRaw.OnReceive := RawDataReceive;
  PinRaw.OnSetSatus := RawSetStatus;
  PinFrame := TCommPin.Create;
  PinFrame.OnReceive := FrameDataReceive;
  PinFrame.OnSetSatus := FrameSetStatus;
  Mark := TListByte.Create;
  FMarkIndex := 0;
  FFrameData := TListByte.Create;
end;

destructor TCommMark.Destroy;
begin
  FFrameData.Free;
  Mark.Free;
  PinRaw.Free;
  PinFrame.Free;
  inherited Destroy;
end;

end.

