BGRAControls v2.0.6.0 alpha

Site: http://sourceforge.net/projects/bgracontrols
Wiki: http://wiki.lazarus.freepascal.org/BGRAControls
Forum: http://www.lazarus.freepascal.org/index.php/topic,12411.0.html

== Overview ==

License: Modified LGPL.

As many people know, Lazarus GTK doesn't have full alpha support. I created a set of components which use TBGRABitmap for drawing icons, so it partially solves the problem with alpha in GTK. But this components set is not only a GTK patch but has more fancy components which are using the power of the BGRABitmap package.

== Working on ==

Use BC named controls to replace old BGRA named controls.

- TBCButton instead TBGRAButton.

BTW there are some controls that depends on others.

- TBGRAWin7ToolBar requires TBGRAButton.

Some units are deprecated.

- bgrabitmapthemeutils.pas & slicescaling.pas; now use bgraslicescaling (in BGRABitmap package).

Some removed.

- slicescaling; now use bgraslicescaling (in BGRABitmap package).
- BGRARibbon and BGRARibbonGroup (obsolete, use BGRAVirtualScreen instead).

== Author: Dibo.== 
Package founder and package mantainer.

BC Controls
- TBCButton
- TBCLabel
- TBCPanel

BGRA Controls
- TBGRAImageList
- TBGRASpeedButton
- TBGRAButton
- TBGRAPanel

== Author: Circular.== 
BGRABitmap creator, contributor and package mantainer.

- TBGRAFlashProgressBar
- TBGRAGraphicControl
- TBGRAKnob
- TBGRAShape
- TBGRAVirtualScreen

== Author: Lainz.== 
Contributor and package mantainer.

BC Controls
- BC test project and bcsamples unit
- Other test projects
- bgrabitmapthemeutils (deprecated, use BGRASliceScaling)

BGRA Controls
- BGRA test project and bgrasamples unit
- TBGRAImageButton
- TBGRASpriteAnimation
- TBGRALabelFX
- TBGRAWin7ToolBar
- TBGRANeoButton

== Author: Emerson Cavalcanti.== 
- TBGRAImageManipulation

== Author: codedeep.== 
- TBGRALabel