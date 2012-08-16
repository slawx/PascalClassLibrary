unit UCommFrame;

{$mode Delphi}{$H+}

interface

uses
  Classes, UStreamHelper, Dialogs, SysUtils, SpecializedList, UBinarySerializer,
  UCommPin;

type
  TFrameState = (fsOutside, fsStart, fsInside, fsEnd);

  { TCommFrame }

  TCommFrame = class
  private
    LastCharIsSpecialChar: Boolean;
    ReceiveBuffer: TBinarySerializer;
    FrameState: TFrameState;
    FFrameErrorCount: Integer;
    FCRCErrorCount: Integer;
    function GetStreamCRC8(Stream: TListByte): Byte;
    procedure RawDataReceive(Sender: TCommPin; Stream: TListByte);
    procedure RawSetStatus(Sender: TCommPin; Status: Integer);
    procedure FrameDataReceive(Sender: TCommPin; Stream: TListByte);
    procedure FrameSetStatus(Sender: TCommPin; Status: Integer);
  public
    RawDataPin: TCommPin;
    FrameDataPin: TCommPin;
    PacketLoss: Real;
    SpecialChar: Byte;
    ControlCodeFrameStart: Byte;
    ControlCodeFrameEnd: Byte;
    ControlCodeSpecialChar: Byte;
    function ComputeRawSize(DataStream: TListByte): Integer;
    constructor Create;
    destructor Destroy; override;
    property FrameErrorCount: Integer read FFrameErrorCount;
    property CRCErrorCount: Integer read FCRCErrorCount;
  end;


implementation

{ TCommFrame }

constructor TCommFrame.Create;
begin
  ReceiveBuffer := TBinarySerializer.Create;
  ReceiveBuffer.List := TListByte.Create;
  ReceiveBuffer.OwnsList := True;
  RawDataPin := TCommPin.Create;
  RawDataPin.OnReceive := RawDataReceive;
  FrameDataPin := TCommPin.Create;
  FrameDataPin.OnReceive := FrameDataReceive;
  PacketLoss := 0;
  SpecialChar := $fe;
  ControlCodeFrameStart := $fd;
  ControlCodeFrameEnd := $fc;
  ControlCodeSpecialChar := $fb;
end;

destructor TCommFrame.Destroy;
begin
  FreeAndNil(RawDataPin);
  FreeAndNil(FrameDataPin);
  ReceiveBuffer.Free;
  inherited;
end;

procedure TCommFrame.FrameDataReceive(Sender: TCommPin; Stream: TListByte);
var
  RawData: TBinarySerializer;
  I: Integer;
  Character: Byte;
  CRC: Byte;
begin
  // Write CRC code to end of frame
  CRC := GetStreamCRC8(Stream);

  // Byte stuffing
  try
    RawData := TBinarySerializer.Create;
    RawData.List := TListByte.Create;
    RawData.OwnsList := True;
    RawData.WriteByte(SpecialChar);
    RawData.WriteByte(ControlCodeFrameStart);
    for I := 0 to Stream.Count - 1 do begin
      Character := Stream[I];
      if Character = SpecialChar then begin
        RawData.WriteByte(SpecialChar);
        RawData.WriteByte(ControlCodeSpecialChar);
      end else RawData.WriteByte(Character);
    end;

    Character := CRC;
    if Character = SpecialChar then begin
      RawData.WriteByte(SpecialChar);
      RawData.WriteByte(ControlCodeSpecialChar);
    end else RawData.WriteByte(Character);

    RawData.WriteByte(SpecialChar);
    RawData.WriteByte(ControlCodeFrameEnd);
    if Random >= PacketLoss then
      RawDataPin.Send(RawData.List);
  finally
    RawData.Free;
  end;
end;

procedure TCommFrame.FrameSetStatus(Sender: TCommPin; Status: Integer);
begin
  RawDataPin.Status := Status;
end;

function TCommFrame.ComputeRawSize(DataStream: TListByte): Integer;
var
  I: Integer;
begin
  Result := 5; // FrameStart + CRC + FrameEnd
  for I := 0 to DataStream.Count - 1 do
    if DataStream[I] = SpecialChar then Inc(Result, 2)
      else Inc(Result, 1);
end;

procedure TCommFrame.RawDataReceive(Sender: TCommPin; Stream: TListByte);
var
  Character: Byte;
  CRC: Byte;
  ExpectedCRC: Byte;
  I: Integer;
begin
  for I := 0 to Stream.Count - 1 do begin
    Character := Stream[I];
    if LastCharIsSpecialChar then begin
      if Character = ControlCodeSpecialChar then begin
          ReceiveBuffer.WriteByte(SpecialChar)
        end else
        if Character = ControlCodeFrameStart then begin
          if FrameState = fsInside then
            Inc(FFrameErrorCount);
          ReceiveBuffer.List.Count := 0;
          ReceiveBuffer.Position := 0;
          FrameState := fsInside;
        end else
        if Character = ControlCodeFrameEnd then begin
          if FrameState = fsInside then begin
            // Check CRC
            if ReceiveBuffer.List.Count > 0 then begin
              ReceiveBuffer.Position := ReceiveBuffer.List.Count - 1;
              CRC := ReceiveBuffer.ReadByte;
              ReceiveBuffer.List.Count := ReceiveBuffer.List.Count - 1;
              ExpectedCRC := GetStreamCRC8(ReceiveBuffer.List);

              if ExpectedCRC <> CRC then Inc(FCRCErrorCount)
                else begin
                  //if Random >= PacketLoss then
                    FrameDataPin.Send(ReceiveBuffer.List);
                end;
            end else Inc(FCRCErrorCount);
          end else Inc(FFrameErrorCount);
          FrameState := fsOutside;
        end;
        LastCharIsSpecialChar := False;
      end else begin
        if Character = SpecialChar then LastCharIsSpecialChar := True
        else ReceiveBuffer.WriteByte(Character);
      end;
    end;
end;

procedure TCommFrame.RawSetStatus(Sender: TCommPin; Status: Integer);
begin
  FrameDataPin.Status := Status;
end;

function TCommFrame.GetStreamCRC8(Stream: TListByte): Byte;
var
  I: Integer;
  B: Integer;
  Pom: Byte;
const
  Polynom: Byte = $18;
begin
  Pom := 0;
  Result := 0;
  for I := 0 to Stream.Count - 1 do begin
    Pom := Stream[I];
    for B := 0 to 7 do begin
      if ((Result xor Pom) and 1) = 1 then
        Result := ((Result xor Polynom) shr 1) or $80
        else Result := Result shr 1;
      Pom := (Pom shr 1) or ((Pom shl 7) and $80); // Rotace vpravo
    end;
  end;
end;


end.
