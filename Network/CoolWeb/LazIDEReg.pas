unit LazIDEReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazIDEIntf, ProjectIntf, FormEditingIntf, UWebPage, Forms, Controls;

type
   { TFileDescWebPage }

  TFileDescWebPage = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

  TCGIApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;


resourcestring
  SWebPageTitle = 'CoolWeb page';
  SWebPageDescription = '';
  SCGIAppTitle = 'CoolWeb application';
  SCGIAppDescription = '';

var
  FileDescriptorWebPage: TFileDescWebPage;
  CGIAppDescriptor: TCGIApplicationDescriptor;


procedure Register;

implementation

procedure Register;
begin
  FileDescriptorWebPage := TFileDescWebPage.Create;
  RegisterProjectFileDescriptor(FileDescriptorWebPage);
  CGIAppDescriptor := TCGIApplicationDescriptor.Create;
  RegisterProjectDescriptor(CGIAppDescriptor);
  FormEditingHook.RegisterDesignerBaseClass(TWebPage);
end;

{ TFileDescWebPage }

constructor TFileDescWebPage.Create;
begin
  inherited Create;
  Name := 'CoolWeb page';
  ResourceClass := TWebPage;
  UseCreateFormStatements := False;
end;

function TFileDescWebPage.GetInterfaceUsesSection: string;
begin
  Result := inherited GetInterfaceUsesSection;
  Result := Result + ', UWebPage, UHTTPServer';
end;

function TFileDescWebPage.GetLocalizedName: string;
begin
  Result := SWebPageTitle;
end;

function TFileDescWebPage.GetLocalizedDescription: string;
begin
  Result := SWebPageDescription;
end;


{ TCGIApplicationDescriptor }

constructor TCGIApplicationDescriptor.Create;
begin
  inherited Create;
  Name := 'CoolWeb CGI Application';
end;

function TCGIApplicationDescriptor.GetLocalizedName: string;
begin
  Result := SCGIAppTitle;
end;

function TCGIApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result := SCGIAppDescription;
end;

function TCGIApplicationDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  le: string;
  NewSource: TStringList;
  MainFile: TLazProjectFile;
begin
  inherited InitProject(AProject);

  MainFile := AProject.CreateProjectFile('cgiapp1.lpr');
  MainFile.IsPartOfProject := True;
  AProject.AddFile(MainFile, False);
  AProject.MainFileID := 0;

  // Create program source
  try
    NewSource := TStringList.Create;
    with NewSource do begin
      Add('program CGIApp1;');
      Add('');
      Add('{$mode objfpc}{$H+}');
      Add('');
      Add('uses');
      Add('  Interfaces, Forms, UWebApp, UWebPage;');
      Add('');
      Add('var');
      Add('  Application: TWebApp;');
      Add('begin');
      Add('  Application := TWebApp.Create(nil);');
      Add('  with Application do');
      Add('  try');
      Add('    Initialize;');
      Add('    RegisterPage(TWebPage1, WebPage1, '''');');
      Add('    Run;');
      Add('  finally');
      Add('    Free;');
      Add('  end;');
      Add('end.');
      Add('');
    end;
    AProject.MainFile.SetSourceText(NewSource.Text);
  finally
    NewSource.Free;
  end;

  // add
  AProject.AddPackageDependency('FCL');
  AProject.AddPackageDependency('LCL');
  AProject.AddPackageDependency('CoolWeb');

  // compiler options
  AProject.LazCompilerOptions.UseLineInfoUnit := True;
  Result := mrOK;
end;

function TCGIApplicationDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  LazarusIDE.DoNewEditorFile(FileDescriptorWebPage, '', '',
    [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);
  Result := mrOK;
end;


end.

