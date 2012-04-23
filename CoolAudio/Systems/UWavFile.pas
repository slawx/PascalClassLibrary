{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library (Freeware)                                              }
{ Class TWAVFile - for extracting information from WAV file header            }
{                                                                             }
{ Copyright (c) 2001 by Jurgen Faul                                           }
{ E-mail: jfaul@gmx.de                                                        }
{ http://jfaul.de/atl                                                         }
{                                                                             }
{ Version 1.0 (31th July 2001)                                                }
{   - Info: channel mode, sample rate, bits per sample, file size, duration   }
{                                                                             }
{ *************************************************************************** }

unit UWavFile;

interface

uses
  Classes, SysUtils;

const
  { Used with ChannelMode property }
  CHANNEL_MODE_MONO = 1;                              { Index for mono mode }
  CHANNEL_MODE_STEREO = 2;                            { Index for stereo mode }

  { Channel mode names }
  CHANNEL_MODE: array [0..2] of string = ('Unknown', 'Mono', 'Stereo');

type
  { Real structure of WAV file header }
  TWAVRecord = record
    { RIFF file header }
    RIFFHeader: array [1..4] of Char;                        { Must be "RIFF" }
    FileSize: Integer;                           { Must be "RealFileSize - 8" }
    WAVEHeader: array [1..4] of Char;                        { Must be "WAVE" }
    { Format information }
    FormatHeader: array [1..4] of Char;                      { Must be "fmt " }
    FormatSize: Integer;                               { Must be 16 (decimal) }
    FormatCode: Word;                                             { Must be 1 }
    ChannelNumber: Word;                                 { Number of channels }
    SampleRate: Integer;                                   { Sample rate (hz) }
    BytesPerSecond: Integer;                               { Bytes per second }
    BytesPerSample: Word;                                  { Bytes per Sample }
    BitsPerSample: Word;                                    { Bits per sample }
    { Data area }
    DataHeader: array [1..4] of Char;                        { Must be "data" }
    DataSize: Integer;                                            { Data size }
  end;

  { TWAVFile }

  TWAVFile = class
    private
      FValid: Boolean;
      FChannelModeID: Byte;
      FSampleRate: Word;
      FBitsPerSample: Byte;
      FFileSize: Cardinal;
      procedure ResetData;
      function GetChannelMode: string;
      function GetDuration: Double;
      function HeaderIsValid: Boolean;
    public
      SourceFile: TFileStream;
      WAVData: TWAVRecord;
      constructor Create;                                     { Create object }
      function OpenFile(const FileName: string): Boolean;   { Load header }
      procedure CloseFile;
      property Valid: Boolean read FValid;             { True if header valid }
      property ChannelModeID: Byte read FChannelModeID;   { Channel mode code }
      property ChannelMode: string read GetChannelMode;  { Channel mode name }
      property SampleRate: Word read FSampleRate;          { Sample rate (hz) }
      property BitsPerSample: Byte read FBitsPerSample;     { Bits per sample }
      property FileSize: Cardinal read FFileSize;         { File size (bytes) }
      property Duration: Double read GetDuration;       { Duration (seconds) }
  end;


implementation


{ ********************* Auxiliary functions & procedures ******************** }

function TWAVFile.HeaderIsValid: Boolean;
begin
  Result := True;
  if WAVData.RIFFHeader <> 'RIFF' then Result := False;
  if WAVData.WAVEHeader <> 'WAVE' then Result := False;
  if WAVData.FormatHeader <> 'fmt ' then Result := False;
  if WAVData.FormatSize <> 16 then Result := False;
  if WAVData.FormatCode <> 1 then Result := False;
  if WAVData.DataHeader <> 'data' then Result := False;
  if (WAVData.ChannelNumber <> CHANNEL_MODE_MONO) and
    (WAVData.ChannelNumber <> CHANNEL_MODE_STEREO) then Result := False;
end;

{ ********************** Private functions & procedures ********************* }

procedure TWAVFile.ResetData;
begin
  FValid := false;
  FChannelModeID := 0;
  FSampleRate := 0;
  FBitsPerSample := 0;
  FFileSize := 0;
end;

function TWAVFile.GetChannelMode: string;
begin
  Result := CHANNEL_MODE[FChannelModeID];
end;

function TWAVFile.GetDuration: Double;
begin
  if FValid then
    Result := (FFileSize - 44) * 8 /
      FSampleRate / FBitsPerSample / FChannelModeID
  else
    Result := 0;
end;

constructor TWAVFile.Create;
begin
  inherited;
  ResetData;
end;

function TWAVFile.OpenFile(const FileName: string): Boolean;
var
  Transferred: Integer;
begin
  Result := True;
  SourceFile := TFileStream.Create(FileName, fmOpenRead);
  Transferred := SourceFile.Read(WAVData, 44);
  if Transferred < 44 then Result := False;

  ResetData;
  if Result and HeaderIsValid then
  begin
    FValid := True;
    FChannelModeID := WAVData.ChannelNumber;
    FSampleRate := WAVData.SampleRate;
    FBitsPerSample := WAVData.BitsPerSample;
    FFileSize := WAVData.FileSize + 8;
  end;
end;

procedure TWAVFile.CloseFile;
begin
  ResetData;
  FreeAndNil(SourceFile);
end;

end.
