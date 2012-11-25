unit UnitBGRAImageManipulationDemo;

{ ============================================================================
  BGRAImageManipulation Demon Unit

  Copyright (C) 2011 - Emerson Cavalcanti

  This program is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  ============================================================================
  Description:

  TFormBGRAImageManipulationDemo is a sample form to use the component
  TBGRAImageManipulation.

  ============================================================================
  History:

  2011-05-06 - Emerson Cavalcanti
             - Initial version

  2011-06-01 - Emerson Cavalcanti
             - Relayout of form.
             - Add control to toggle the option 'Keep Aspect Ratio' in
               component

  2011-06-18 - Emerson Cavalcanti
             - Relayout of form for expand component on resize.
             - Add control to rotate image

  ============================================================================
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtDlgs, ComCtrls, ExtCtrls, BGRAImageManipulation, BGRABitmap,
  BGRABitmapTypes, BGRAPanel, BGRAButton{, BGRATrackBar};

type

  { TFormBGRAImageManipulationDemo }

  TFormBGRAImageManipulationDemo = class(TForm)
    Background:        TBGRAPanel;
    btnShape:          TBGRAButton;
    btnOpenPicture:    TBGRAButton;
    BGRAImageManipulation: TBGRAImageManipulation;
    btnGetAspectRatioFromImage: TBGRAButton;
    btnSavePicture:    TBGRAButton;
    btnSetAspectRatio: TBGRAButton;
    btnRotateLeft:     TBGRAButton;
    btnRotateRight:    TBGRAButton;
    edAspectRatio:     TEdit;
    KeepAspectRatio:   TCheckBox;
    lbAspectRatio:     TLabel;
    lbOptions:         TLabel;
    lbCompression:     TLabel;
    OpenPictureDialog: TOpenPictureDialog;
    SavePictureDialog: TSavePictureDialog;
    RateCompression:   TTrackBar;
    procedure btnGetAspectRatioFromImageClick(Sender: TObject);
    procedure btnOpenPictureClick(Sender: TObject);
    procedure btnRotateLeftClick(Sender: TObject);
    procedure btnRotateRightClick(Sender: TObject);
    procedure btnSavePictureClick(Sender: TObject);
    procedure btnSetAspectRatioClick(Sender: TObject);
    procedure KeepAspectRatioClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormBGRAImageManipulationDemo: TFormBGRAImageManipulationDemo;

implementation

{$R *.lfm}

{ TFormBGRAImageManipulationDemo }

procedure TFormBGRAImageManipulationDemo.btnOpenPictureClick(Sender: TObject);
var
  Bitmap: TBGRABitmap;
begin
  // To put a new image in the component, you will simply need execute open
  // picture dialog to locate an image...
  if OpenPictureDialog.Execute then
  begin
    // ...and create a new TBGRABitmap and load to it
    Bitmap := TBGRABitmap.Create;
    Bitmap.LoadFromFile(OpenPictureDialog.FileName);
    // Finally, associate the image into component
    BGRAImageManipulation.Bitmap := Bitmap;
    Bitmap.Free;
  end;
end;

procedure TFormBGRAImageManipulationDemo.btnRotateLeftClick(Sender: TObject);
begin
  BGRAImageManipulation.rotateLeft;
end;

procedure TFormBGRAImageManipulationDemo.btnRotateRightClick(Sender: TObject);
begin
  BGRAImageManipulation.rotateRight;
end;

procedure TFormBGRAImageManipulationDemo.btnGetAspectRatioFromImageClick(
  Sender: TObject);
begin
  if not (BGRAImageManipulation.Empty) then
  begin
    edAspectRatio.Text := BGRAImageManipulation.getAspectRatioFromImage(
      BGRAImageManipulation.Bitmap);
  end;
end;

procedure TFormBGRAImageManipulationDemo.btnSavePictureClick(Sender: TObject);
var
  JpegImage: TJpegImage;
begin
  // This example save image compress in JPEG format

  // Execute our Save Picture Dialog
  SavePictureDialog.Filter := 'JPEG Image File (*.jpg, *.jpeg)';
  if SavePictureDialog.Execute then
  begin
    try
      // Compress
      JpegImage := TJpegImage.Create;
      JpegImage.Assign(BGRAImageManipulation.getBitmap);
      JpegImage.CompressionQuality := RateCompression.Position;

      // And save to file
      JpegImage.SaveToFile(SavePictureDialog.FileName);
    finally
      JpegImage.Free;
    end;
  end;
end;

procedure TFormBGRAImageManipulationDemo.btnSetAspectRatioClick(Sender: TObject);
begin
  try
    BGRAImageManipulation.AspectRatio := edAspectRatio.Text;
  except
    on E: Exception do
    begin
      ShowMessage('This aspect ratio is invalid');
    end;
  end;
end;

procedure TFormBGRAImageManipulationDemo.KeepAspectRatioClick(Sender: TObject);
begin
  BGRAImageManipulation.KeepAspectRatio := KeepAspectRatio.Checked;
  edAspectRatio.Enabled := KeepAspectRatio.Checked;
  btnSetAspectRatio.Enabled := KeepAspectRatio.Checked;
end;

end.
