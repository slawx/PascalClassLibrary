{$IFDEF Linux}
  {$DEFINE AudioSystemDSP}
  {$DEFINE AudioSystemMAD}
{$ENDIF}
{$IFDEF i386}
  {$DEFINE AudioSystemFMOD}
{$ENDIF}
{$DEFINE AudioSystemMPlayer}
{$IFDEF Windows}
  {$DEFINE AudioSystemWindows}
{$ENDIF}

