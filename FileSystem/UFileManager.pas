// In development

unit UFileManager;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDirectory = class;
  TFileSystem = class;

  { TFile }

  TFile = class
  private
    function GetAbsoluteName: string;
    function GetSize: Integer;
    procedure SetSize(AValue: Integer);
  public
    Path: TDirectory;
    Name: string;
    Extension: string;
    function ReadInteger: Integer;
    procedure WriteInteger(const Value: Integer);
    function Exists: Boolean;
    procedure Delete;
    procedure Rename(const Name: string);
    property Size: Integer read GetSize write SetSize;
    property AbsoluteName: string read GetAbsoluteName;
  end;

  { TDirectory }

  TDirectory = class
  private
    function GetAbsoluteName: string;
  public
    FileSystem: TFileSystem;
    Path: string;
    procedure MoveTo(Target: TDirectory);
    procedure CopyTo(Target: TDirectory);
    procedure Rename(const Name: string);
    procedure Delete;
    function Exists: Boolean;
    function GetFile(const Name: string): TFile;
    property AbsoluteName: string read GetAbsoluteName;
  end;

  { TFileSystem }

  TFileSystem = class
    Identification: string;
    Name: string;
    Format: string;
    function OpenDirectory(Path: string): TDirectory;
  end;

  { TDiskManager }

  TDiskManager = class
    function OpenFileSystem(Identification: string): TFileSystem;
  end;



implementation

{ TFileSystem }

function TFileSystem.OpenDirectory(Path: string): TDirectory;
begin
  Result := TDirectory.Create;
  Result.FileSystem := Self;
  Result.Path := Path;
end;

{ TDiskManager }

function TDiskManager.OpenFileSystem(Identification: string): TFileSystem;
begin

end;

{ TDirectory }

function TDirectory.GetAbsoluteName: string;
begin
  Result := FileSystem.Identification + DriveSeparator + Path;
end;

procedure TDirectory.MoveTo(Target: TDirectory);
begin

end;

procedure TDirectory.CopyTo(Target: TDirectory);
begin

end;

procedure TDirectory.Rename(const Name: string);
begin
  //RenameDir(Self.Path, Name);
end;

procedure TDirectory.Delete;
begin
  RemoveDir(Path);
end;

function TDirectory.Exists: Boolean;
begin
  Result := DirectoryExists(AbsoluteName);
end;

function TDirectory.GetFile(const Name: string): TFile;
begin
  Result := TFile.Create;
  Result.Path := Self;
  Result.Name := Name;
end;

{ TFile }

function TFile.GetAbsoluteName: string;
begin
  Result := Path.AbsoluteName + DirectorySeparator + Name +
    ExtensionSeparator + Extension;
end;

function TFile.GetSize: Integer;
begin

end;

procedure TFile.SetSize(AValue: Integer);
begin

end;

function TFile.ReadInteger: Integer;
begin

end;

procedure TFile.WriteInteger(const Value: Integer);
begin

end;

function TFile.Exists: Boolean;
begin
  Result := FileExists(AbsoluteName);
end;

procedure TFile.Delete;
begin
  DeleteFile(AbsoluteName);
end;

procedure TFile.Rename(const Name: string);
begin
  RenameFile(AbsoluteName, Name);
end;

end.

