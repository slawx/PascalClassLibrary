=Introduction=

This package contains component which make translation of application easy.

==Features==

* Easy to use non-visual component
* Support runtime translation
* Native to lazarus (use .po files)
* Native to FPC (use resourcestrings)
* Supply available language list

=How translation is done=

Strings are translated runtime and there is no need to do it as first step during initialization phase.

==Resourcestring=

Resourcestrings are translated automatically.

==Components==

All application components are translated automatically.
This mean all TPersistent descendants with published properties linked to Application object.

===Component and properties exclusion===
It is possible to exclude properties which should not be translated

===TCollection, TStrings===
Some class properties are translated.

* TCollection: TListView.Columns
* TStrings: TRadioGroup.Items, TPageControl.Pages, TComboBox.Items, TMemo.Lines

==Other constants==


==Custom translation==

You can handle OnTranslate event and do translation of custom structures.

=Download=

You can download source code using subversion:

svn checkout http://svn.zdechov.net/PascalClassLibrary/CoolTranslator
