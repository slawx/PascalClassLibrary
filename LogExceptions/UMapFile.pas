unit UMapFile;

interface

uses
  SysUtils, Classes, Windows, Dialogs;

type
  TMapFile = class
  private
    FMapFileName: String;
    FSegmentData, FAdressData, FLineData: TStringList;
    FMapFileBase: DWORD;
    FExceptAddress: DWORD;
    FExceptLineNumber: Integer;
    FExceptMethodName: String;
    FExceptUnitName: String;
    FExceptionAnalyzed: Boolean;
    procedure SetMapFileName(const Value: String);
    procedure LoadMapFile;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadExceptionData(Address: Pointer = nil);

    property MapFileName: String read FMapFileName write SetMapFileName;
    property MapFileBase: DWORD read FMapFileBase write FMapFileBase;
    property ExceptUnitName: String read FExceptUnitName;
    property ExceptMethodName: String read FExceptMethodName;
    property ExceptLineNumber: Integer read FExceptLineNumber;
    property ExceptAddress: DWORD read FExceptAddress;
    property ExceptionAnalyzed: Boolean read FExceptionAnalyzed;
  end;

implementation

{ TMapFile }

constructor TMapFile.Create;
begin
  inherited Create;
  FSegmentData := TStringList.Create;
  FAdressData := TStringList.Create;
  FLineData := TStringList.Create;
  FMapFileName := '';
  FMapFileBase := $00401000;
  FExceptAddress := 0;
  FExceptLineNumber := 0;
  FExceptMethodName := '';
  FExceptUnitName := '';
  FExceptionAnalyzed := False;
end;

destructor TMapFile.Destroy;
begin
  FreeAndNil(FSegmentData);
  FreeAndNil(FAdressData);
  FreeAndNil(FLineData);
  inherited Destroy;
end;

procedure TMapFile.LoadExceptionData(Address: Pointer);
var
  UnitLineDataFound: Boolean;
  I, J, LastLine: Integer;
  Start, Stop, ProcAddr, LineAddr: DWORD;
  Line: String;
begin
  // reset
  FExceptAddress := 0;
  FExceptLineNumber := 0;
  FExceptMethodName := '';
  FExceptUnitName := '';
  FExceptionAnalyzed := False;

  // load address
  if Address = nil then
    Address := ExceptAddr;
  if Address = nil then
    Exit;

  // load and adjust exception address
  FExceptAddress := DWORD(Address) - FMapFileBase;

  // find unit of exception
  I := 0;
  while (I < FSegmentData.Count) and (FSegmentData[I] <> '') do
  begin
    try
      // check whether address is within unit address limits
      Start := DWORD(StrToInt('0x' + Copy(FSegmentData[I], 7, 8)));
      Stop := Start + DWORD(StrToInt('0x' + Copy(FSegmentData[I], 16, 8)));
      if (Start <= FExceptAddress) and (FExceptAddress < Stop) then
      begin
        Start := Pos('M=', FSegmentData[I]) + 2;
        Stop := Pos('ACBP=', FSegmentData[I]);
        if (Start > 0) and (Stop > 0) then
          FExceptUnitName :=
            Trim(Copy(FSegmentData[I], Start, Stop - Start - 1));
      end;
    except
    end;
    Inc(I);
  end;

  // find function of exception
  I := 0;
  while I < FAdressData.Count do
  begin
    try
      ProcAddr := DWORD(StrToInt('0x' + Copy(FAdressData[I], 7, 8)));
      if ProcAddr >= FExceptAddress then
      begin
        if ProcAddr = FExceptAddress then
          Line := FAdressData[I]
        else
          Line := FAdressData[Pred(I)];
        FExceptMethodName := Trim(Copy(Line, 22, Length(Line)));
        Break;
      end;
    except
    end;
    Inc(I);
  end;

  // find line number of exception
  I := 0;
  UnitLineDataFound := False;
  // search for unit section
  while I < FLineData.Count do
  begin
    if Pos(FExceptUnitName, FLineData[I]) <> 0 then
    begin
      UnitLineDataFound := True;
      Break;
    end;
    Inc(I);
  end;
  if UnitLineDataFound then
  begin
    // search for line number
    LastLine := 0;
    LineAddr := 0;
    Inc(I, 2);
    while I < FLineData.Count do
    begin
      //ShowMessage(FLineData[I]);
      if Pos('Line numbers for', FLineData[I]) <> 0 then
        Break;
      if FLineData[I] <> '' then 
      try
        for J := 0 to 3 do
        begin
          LineAddr := StrToInt('0x' + Copy(FLineData[I], J * 20 + 13, 8));
          if LineAddr > FExceptAddress then
            Break;
          LastLine := StrToInt(Trim(Copy(FLineData[I], J * 20 + 1, 6)));
          if LineAddr = FExceptAddress then
            Break;
        end;
      except
      end;
      Inc(I);
    end;
    if LineAddr >= FExceptAddress then
      FExceptLineNumber := LastLine;
  end;

  FExceptionAnalyzed := True;
end;

procedure TMapFile.LoadMapFile;
var
  I: Integer;
begin
  FSegmentData.Clear;
  FAdressData.Clear;
  FLineData.Clear;
  if FileExists(FMapFileName) then
    with TStringList.Create do
    try
      LoadFromFile(FMapFileName);
      // find start of detailed segment block
      I := 0;
      while I < Count do
        if Pos('Detailed map of segments', Strings[I]) <> 0 then
          Break
        else
          Inc(I);
      Inc(I, 2);

      // copy all lines to segment data, until name-address block starts
      while I < Count do
        if Pos('Address         Publics by Name', Strings[I]) <> 0 then
          Break
        else begin
          FSegmentData.Add(Strings[I]);
          Inc(I);
        end;

      // find start of value-address block
      while I < Count do
        if Pos('Address         Publics by Value', Strings[I]) <> 0 then
          Break
        else
          Inc(I);
      Inc(I, 3);

      // copy all lines to address data, until line number block starts
      while I < Count do
        if Pos('Line numbers for', Strings[I]) <> 0 then
          Break
        else begin
          FAdressData.Add(Strings[I]);
          Inc(I);
        end;

      // copy all remaining lines to line data
      while I < Count do
      begin
        FLineData.Add(Strings[I]);
        Inc(I);
      end;
    finally
      Free;
    end;
end;

procedure TMapFile.SetMapFileName(const Value: String);
begin
  if FMapFileName <> Value then
  begin
    FMapFileName := Value;
    LoadMapFile;
  end;
end;

end. 