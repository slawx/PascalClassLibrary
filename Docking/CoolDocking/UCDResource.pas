unit UCDResource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls;

type

  { TDataModule2 }

  TDataModule2 = class(TDataModule)
  published
    ImageList1: TImageList;
    { private declarations }
  public
    { public declarations }
  end; 

var
  DataModule2: TDataModule2; 

implementation

{$R *.lfm}

initialization

DataModule2 := TDataModule2.Create(nil);

finalization

DataModule2.Free;

end.

