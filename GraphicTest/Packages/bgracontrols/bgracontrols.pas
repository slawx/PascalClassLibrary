{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit bgracontrols;

interface

uses
  BCTools, BCBaseCtrls, BCButton, BCLabel, BCPanel, bgrabitmapthemeutils, 
  BGRAFlashProgressBar, BGRAGraphicControl, BGRAImageButton, BGRAImageList, 
  BGRAImageManipulation, BGRAKnob, BGRALabelFX, BGRANeoButton, bgrasamples, 
  BGRAShape, BGRASpeedButton, BGRASpriteAnimation, BGRAVirtualScreen, 
  BGRAWin7ToolBar, BCRTTI, BCStylesForm, BGRAButton, BGRALabel, BGRAPanel, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('BCButton', @BCButton.Register);
  RegisterUnit('BCLabel', @BCLabel.Register);
  RegisterUnit('BCPanel', @BCPanel.Register);
  RegisterUnit('BGRAFlashProgressBar', @BGRAFlashProgressBar.Register);
  RegisterUnit('BGRAGraphicControl', @BGRAGraphicControl.Register);
  RegisterUnit('BGRAImageButton', @BGRAImageButton.Register);
  RegisterUnit('BGRAImageList', @BGRAImageList.Register);
  RegisterUnit('BGRAImageManipulation', @BGRAImageManipulation.Register);
  RegisterUnit('BGRAKnob', @BGRAKnob.Register);
  RegisterUnit('BGRALabelFX', @BGRALabelFX.Register);
  RegisterUnit('BGRANeoButton', @BGRANeoButton.Register);
  RegisterUnit('BGRAShape', @BGRAShape.Register);
  RegisterUnit('BGRASpeedButton', @BGRASpeedButton.Register);
  RegisterUnit('BGRASpriteAnimation', @BGRASpriteAnimation.Register);
  RegisterUnit('BGRAVirtualScreen', @BGRAVirtualScreen.Register);
  RegisterUnit('BGRAWin7ToolBar', @BGRAWin7ToolBar.Register);
  RegisterUnit('BGRAButton', @BGRAButton.Register);
  RegisterUnit('BGRALabel', @BGRALabel.Register);
  RegisterUnit('BGRAPanel', @BGRAPanel.Register);
end;

initialization
  RegisterPackage('bgracontrols', @Register);
end.
