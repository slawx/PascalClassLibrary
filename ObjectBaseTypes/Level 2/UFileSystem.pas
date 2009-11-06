unit UFileSystem;

interface

uses
  Types, SysUtils;

type
  TDirectory = class;

  TFilePath = class
  private
    FPath: string;
    function GetDirectory: string;
    function GetDrive: string;
    function GetName: string;
    procedure SetDirectory(const Value: string);
    procedure SetDrive(const Value: string);
    procedure SetExtension(const Value: string);
    procedure SetName(const Value: string);
    function GetExtension: string;
  public
    procedure Expand;
    procedure ExtractRelativePath(const BaseName, DestName: string);
    property Path: string read FPath write FPath;
    property Drive: string read GetDrive write SetDrive;
    property Directory: string read GetDirectory write SetDirectory;
    property Name: string read GetName write SetName;
    property Extension: string read GetExtension write SetExtension;
  end;

  TFile = class
  private
    Parent: TDirectory;
    FFile: File;
    function GetFileSize: Int64;
    procedure Remove;
  public
    AccessTime: TDateTime;
    CreateTime: TDateTime;
    ModifyTime: TDateTime;
    Name: string;
    property Size: Int64 read GetFileSize;
    function Exists: Boolean;
    procedure Rename(NewName: string);
    constructor Create;
  end;

  TFileSearch = class
    SearchRec: TSearchRec;
    function FindFirst: TFile;
    function FindNext: TFile;
    destructor Destroy; override;
  end;

  TFileSystem = class;

  TDirectory = class
  private
    FileSystem: TFileSystem;
    Parent: TDirectory;
    FFullPath: string;
    function GetFullPath: string;
  public
    Name: string;
    function CreateFile(Name: string): TFile;
    function CreateDirectory(Name: string): TDirectory;
    procedure RemoveDirectory(Directory: TDirectory);
    function FindFirstFile(Filter: string): TFileSearch;
    property FullPath: string read GetFullPath;
    function Exists: Boolean;
  end;

  TFileSystem = class
  private
    function GetSize: Int64;
    function GetFreeSpace: Int64;
  public
    Drive: Byte;
    function GetLetter: Char;
    function GetRootDirectory: TDirectory;
    function GetCurrentDirectory: TDirectory;
    procedure SetCurrentDirectory(Directory: TDirectory);
    property FreeSpace: Int64 read GetFreeSpace;
    property Size: Int64 read GetSize;
  end;

implementation

{ TFileSystem }

function TFileSystem.GetCurrentDirectory: TDirectory;
begin
  Result := TDirectory.Create;
  Result.FileSystem := Self;
  Result.FFullPath := GetCurrentDir;
end;

function TFileSystem.GetFreeSpace: Int64;
begin
  Result := DiskFree(Drive);
end;

function TFileSystem.GetLetter: Char;
begin
  Result := Chr(Ord('A') + Drive - 1);
end;

function TFileSystem.GetRootDirectory: TDirectory;
begin
  Result := TDirectory.Create;
  Result.FileSystem := Self;
  Result.FFullPath := GetLetter + ':\';
end;

function TFileSystem.GetSize: Int64;
begin
  Result := DiskSize(Drive);
end;

procedure TFileSystem.SetCurrentDirectory(Directory: TDirectory);
begin
  SetCurrentDir(Directory.FFullPath);
end;

{ TDirectory }

function TDirectory.CreateDirectory(Name: string): TDirectory;
begin
  Result := TDirectory.Create;
  Result.Parent := Self;
  Result.Name := Name;
  SysUtils.CreateDir(Result.FFullPath);
end;

function TDirectory.CreateFile(Name: string): TFile;
begin
  Result := TFile.Create;
  Result.Parent := Self;
  Result.Name := Name;
  Result.Create;
end;

function TDirectory.Exists: Boolean;
begin
  Result := DirectoryExists(FullPath);
end;

function TDirectory.FindFirstFile(Filter: string): TFileSearch;
begin
  Result := TFileSearch.Create;
  SysUtils.FindFirst(FullPath + filter, $2f, Result.SearchRec);
end;

function TDirectory.GetFullPath: string;
begin

end;

procedure TDirectory.RemoveDirectory(Directory: TDirectory);
begin
  SysUtils.RemoveDir(Directory.FFullPath);
end;

{ TFile }

constructor TFile.Create;
begin
end;

function TFile.Exists: Boolean;
begin
  Result := FileExists(Name);
end;

function TFile.GetFileSize: Int64;
begin
  Result := FileSize(FFile);
end;

procedure TFile.Remove;
begin
  Truncate(FFile);
end;

procedure TFile.Rename(NewName: string);
begin
  RenameFile(Name, NewName);
end;

{ TFileSearch }

destructor TFileSearch.Destroy;
begin
  FindClose(SearchRec);
  inherited;
end;

function TFileSearch.FindFirst: TFile;
begin

end;

function TFileSearch.FindNext: TFile;
begin
  SysUtils.FindNext(SearchRec);
end;

{ TFilePath }

procedure TFilePath.Expand;
begin
  FPath := ExpandFileName(FPath);
end;

procedure TFilePath.ExtractRelativePath(const BaseName, DestName: string);
begin
  Path := SysUtils.ExtractRelativePath(BaseName, DestName);
end;

function TFilePath.GetDirectory: string;
begin
  Result := ExtractFilePath(Path);
end;

function TFilePath.GetDrive: string;
begin
  Result := ExtractFileDrive(Path);
end;

function TFilePath.GetExtension: string;
begin
  Result := ExtractFileExt(Path);
end;

function TFilePath.GetName: string;
begin
  Result := ExtractFileName(Path);
end;

procedure TFilePath.SetDirectory(const Value: string);
begin

end;

procedure TFilePath.SetDrive(const Value: string);
begin

end;

procedure TFilePath.SetExtension(const Value: string);
begin

end;

procedure TFilePath.SetName(const Value: string);
begin

end;

end.
