{============================================================================================ }
{ FMOD Main header file. Copyright (c), FireLight Technologies Pty, Ltd. 1999-2003.           }
{ =========================================================================================== }
{
  NOTE: For the demos to run you must have either fmod.dll (in Windows)
  or libfmod-3.75.so (in Linux) installed.

  In Windows, copy the fmod.dll file found in the api directory to either of
  the following locations (in order of preference)
  - your application directory
  - Windows\System (95/98) or WinNT\System32 (NT/2000/XP)

  In Linux, make sure you are signed in as root and copy the libfmod-3.75.so
  file from the api directory to your /usr/lib/ directory.
  Then via a command line, navigate to the /usr/lib/ directory and create
  a symbolic link between libfmod-3.75.so and libfmod.so. This is done with
  the following command (assuming you are in /usr/lib/)...
  ln -s libfmod-3.75.so libfmod.so.
}
{ =============================================================================================== }

unit fmoddyn;

{
  Disable assertions by changing the following compiler directive to OFF.
  Assertions are used to check the functions are correctly loaded when using
  dynamic loading.
}
{$ASSERTIONS ON}

interface

{$IFDEF MSWINDOWS}
  {$define WINDOWS}
{$ENDIF}

uses
{$IFDEF WINDOWS}
  Windows,
{$ENDIF}
  fmodtypes;

{
  Disable warning for unsafe types in Delphi 7
}
{$IFDEF VER150}
{$WARN UNSAFE_TYPE OFF}
{$ENDIF}

//===============================================================================================
// FUNCTION PROTOTYPES
//===============================================================================================

{ ================================== }
{ Library load/unload functions.     }
{ ================================== }

{
  If no library name is passed to FMOD_Load, then the default library name
  used.
}

function FMOD_Load(LibName: PChar {$ifndef FPC}= nil{$endif}): Boolean;
procedure FMOD_Unload;

{ ================================== }
{ Initialization / Global functions. }
{ ================================== }

{
  Pre FSOUND_Init functions. These can't be called after FSOUND_Init is
  called (they will fail). They set up FMOD system functionality.
}

var
  FSOUND_SetOutput: function (OutputType: TFSoundOutputTypes): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetDriver: function (Driver: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMixer: function (Mixer: TFSoundMixerTypes): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetBufferSize: function (LenMs: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetHWND: function (Hwnd: THandle): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMinHardwareChannels: function (Min: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMaxHardwareChannels: function (Max: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMemorySystem: function (Pool: Pointer;
        PoolLen: Integer;
        UserAlloc: TFSoundAllocCallback;
        UserRealloc: TFSoundReallocCallback;
        UserFree: TFSoundFreeCallback): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Main initialization / closedown functions
  Note : Use FSOUND_INIT_USEDEFAULTMIDISYNTH with FSOUND_Init for software override with MIDI playback.
       : Use FSOUND_INIT_GLOBALFOCUS with FSOUND_Init to make sound audible
         no matter which window is in focus. (FSOUND_OUTPUT_DSOUND only)
}

var
  FSOUND_Init: function (MixRate: Integer; MaxSoftwareChannels: Integer; Flags: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Close: procedure; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Runtime system level functions
}

var
  FSOUND_Update: procedure; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};  // This is called to update 3d sound / non-realtime output
  FSOUND_SetSpeakerMode: procedure (SpeakerMode: Cardinal); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetSFXMasterVolume: procedure (Volume: Integer); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetPanSeperation: procedure (PanSep: Single); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_File_SetCallbacks: procedure (OpenCallback: TFSoundOpenCallback;
                                       CloseCallback: TFSoundCloseCallback;
                                       ReadCallback: TFSoundReadCallback;
                                       SeekCallback: TFSoundSeekCallback;
                                       TellCallback: TFSoundTellCallback); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  System information functions
}

var
  FSOUND_GetError: function: TFModErrors; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetVersion: function: Single; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetOutput: function: TFSoundOutputTypes; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetOutputHandle: function: Pointer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetDriver: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetMixer: function: TFSoundMixerTypes; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetNumDrivers: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetDriverName: function (Id: Integer): PChar; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetDriverCaps: function (Id: Integer; var Caps: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

var
  FSOUND_GetOutputRate: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetMaxChannels: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetMaxSamples: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetSpeakerMode: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetSFXMasterVolume: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetNumHWChannels: function (var Num2D: Integer; var Num3D: Integer; var Total: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetChannelsPlaying: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetCPUUsage: function: Single; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetMemoryStats: Procedure (var CurrentAlloced: Cardinal; var MaxAlloced: Cardinal); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ =================================== }
{ Sample management / load functions. }
{ =================================== }

{
  Sample creation and management functions
  Note : Use FSOUND_LOADMEMORY   flag with FSOUND_Sample_Load to load from memory.
         Use FSOUND_LOADRAW      flag with FSOUND_Sample_Load to treat as as raw pcm data.
}

var
  FSOUND_Sample_Load: function (Index: Integer; const NameOrData: PChar; Mode: Cardinal; Offset: Integer; Length: Integer): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Alloc: function (Index: Integer; Length: Integer; Mode: Cardinal; DefFreq: Integer; DefVol: Integer; DefPan: Integer; DefPri: Integer): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Free: procedure (Sptr: PFSoundSample); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Upload: function (Sptr: PFSoundSample; SrcData: Pointer; Mode: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Lock: function (Sptr: PFSoundSample; Offset: Integer; Length: Integer; var Ptr1: Pointer; var Ptr2: Pointer; var Len1: Cardinal; var Len2: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Unlock: function (Sptr: PFSoundSample; Ptr1: Pointer; Ptr2: Pointer; Len1: Cardinal; Len2: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Sample control functions
}

var
  FSOUND_Sample_SetMode: function (Sptr: PFSoundSample; Mode: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_SetLoopPoints: function (Sptr: PFSoundSample; LoopStart, LoopEnd: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_SetDefaults: function (Sptr: PFSoundSample; DefFreq, DefVol, DefPan, DefPri: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_SetDefaultsEx: function (Sptr: PFSoundSample; DefFreq, DefVol, DefPan, DefPri, VarFreq, VarVol, VarPan: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_SetMinMaxDistance: function (Sptr: PFSoundSample; Min, Max: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_SetMaxPlaybacks: function (Sptr: PFSoundSample; Max: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Sample information functions
}

var
  FSOUND_Sample_Get: function (SampNo: Integer): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetName: function (Sptr: PFSoundSample): PCHAR; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetLength: function (Sptr: PFSoundSample): Cardinal; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetLoopPoints: function (Sptr: PFSoundSample; var LoopStart: Integer; var LoopEnd: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetDefaults: function (Sptr: PFSoundSample; var DefFreq: Integer; var DefVol: Integer; var DefPan: Integer; var DefPri: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetDefaultsEx: function (Sptr: PFSoundSample; var DefFreq: Integer; var DefVol: Integer; var DefPan: Integer; var DefPri: Integer; var VarFreq: Integer; var VarVol: Integer; var VarPan): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetMode: function (Sptr: PFSoundSample): Cardinal; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetMinMaxDistance: function (Sptr: PFSoundSample; var Min: Single; var Max: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ ============================ }
{ Channel control functions.   }
{ ============================ }

{
  Playing and stopping sounds.
  Note : Use FSOUND_FREE as the 'channel' variable, to let FMOD pick a free channel for you.
         Use FSOUND_ALL as the 'channel' variable to control ALL channels with one function call!
}

var
  FSOUND_PlaySound: function (Channel: Integer; Sptr: PFSoundSample): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_PlaySoundEx: function (Channel: Integer; Sptr: PFSoundSample; Dsp: PFSoundDSPUnit; StartPaused: ByteBool): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_StopSound: function (Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Functions to control playback of a channel.
}

var
  FSOUND_SetFrequency: function (Channel: Integer; Freq: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetVolume: function (Channel: Integer; Vol: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetVolumeAbsolute: function (Channel: Integer; Vol: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetPan: function (Channel: Integer; Pan: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetSurround: function (Channel: Integer; Surround: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMute: function (Channel: Integer; Mute: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetPriority: function (Channel: Integer; Priority: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetReserved: function (Channel: Integer; Reserved: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetPaused: function (Channel: Integer; Paused: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetLoopMode: function (Channel: Integer; LoopMode: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetCurrentPosition: function (Channel: Integer; Offset: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_SetAttributes: function (Channel: Integer; Pos: PFSoundVector; Vel: PFSoundVector): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_SetMinMaxDistance: function (Channel: Integer; Min: Single; Max: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Channel information functions
}

var
  FSOUND_IsPlaying: function (Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetFrequency: function (Channel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetVolume: function (Channel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetAmplitude: function (Channel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetPan: function (Channel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetSurround: function (Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetMute: function (Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetPriority: function (Channel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetReserved: function (Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetPaused: function (Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetLoopMode: function (Channel: Integer): Cardinal; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetCurrentPosition: function (Channel: Integer): Cardinal; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetCurrentSample: function (Channel: Integer): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetCurrentLevels: function (Channel: Integer; L, R: PSingle): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetNumSubChannels: function (Channel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetSubChannel: function (Channel: Integer; SubChannel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_GetAttributes: function (Channel: Integer; Pos: PFSoundVector; Vel: PFSoundVector): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_GetMinMaxDistance: function (Channel: Integer; var Min: Single; var Max: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ =================== }
{ 3D sound functions. }
{ =================== }

{
    See also 3d sample and channel based functions above.
    Call FSOUND_Update once a frame to process 3d information.
}

var
  FSOUND_3D_Listener_SetCurrent: procedure (current: Integer); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_Listener_SetAttributes: procedure (Pos: PFSoundVector; Vel: PFSoundVector;
                                               fx: Single; fy: Single; fz: Single;
                                               tx: Single; ty: Single; tz: Single); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_Listener_GetAttributes: procedure (Pos: PFSoundVector; Vel: PFSoundVector;
                                               fx: PSingle; fy: PSingle; fz: PSingle;
                                               tx: PSingle; ty: PSingle; tz: PSingle); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_SetDopplerFactor: procedure (Scale: Single); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_SetDistanceFactor: procedure (Scale: Single); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_SetRolloffFactor: procedure (Scale: Single); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ =================== }
{ FX functions.       }
{ =================== }

{
    Functions to control DX8 only effects processing.

    - FX enabled samples can only be played once at a time, not multiple times at once.
    - Sounds have to be created with FSOUND_HW2D or FSOUND_HW3D for this to work.
    - FSOUND_INIT_ENABLESYSTEMCHANNELFX can be used to apply hardware effect processing to the
      global mixed output of FMOD's software channels.
    - FSOUND_FX_Enable returns an FX handle that you can use to alter fx parameters.
    - FSOUND_FX_Enable can be called multiple times in a row, even on the same FX type,
      it will return a unique handle for each FX.
    - FSOUND_FX_Enable cannot be called if the sound is playing or locked.
    - Stopping or starting a sound resets all FX and they must be re-enabled each time
      if this happens.
}

var
  FSOUND_FX_Enable: function (Channel: Integer; Fx: TFSoundFXModes): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};    { Set bits to enable following fx }
  FSOUND_FX_Disable: function (Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_FX_SetChorus: function (FXId: Integer; WetDryMix, Depth, Feedback, Frequency: Single; Waveform: Integer; Delay: Single; Phase: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetCompressor: function (FXId: Integer; Gain, Attack, Release, Threshold, Ratio, Predelay: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetDistortion: function (FXId: Integer; Gain, Edge, PostEQCenterFrequency, PostEQBandwidth, PreLowpassCutoff: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetEcho: function (FXId: Integer; WetDryMix, Feedback, LeftDelay, RightDelay: Single; PanDelay: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetFlanger: function (FXId: Integer; WetDryMix, Depth, Feedback, Frequency: Single; Waveform: Integer; Delay: Single; Phase: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetGargle: function (FXId, RateHz, WaveShape: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetI3DL2Reverb: function (FXId, Room, RoomHF: Integer; RoomRolloffFactor, DecayTime, DecayHFRatio: Single; Reflections: Integer; ReflectionsDelay: Single; Reverb: Integer; ReverbDelay, Diffusion, Density, HFReference: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetParamEQ: function (FXId: Integer; Center, Bandwidth, Gain: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetWavesReverb: function (FXId: Integer; InGain, ReverbMix, ReverbTime, HighFreqRTRatio: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ ========================= }
{ File Streaming functions. }
{ ========================= }

{
    Note : Use FSOUND_LOADMEMORY   flag with FSOUND_Stream_Open to stream from memory.
           Use FSOUND_LOADRAW      flag with FSOUND_Stream_Open to treat stream as raw pcm data.
           Use FSOUND_MPEGACCURATE flag with FSOUND_Stream_Open to open mpegs in 'accurate mode' for settime/gettime/getlengthms.
           Use FSOUND_FREE as the 'channel' variable, to let FMOD pick a free channel for you.
}

var
  // call this before opening streams, not after
  FSOUND_Stream_SetBufferSize: function (Ms: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_Open: function(const name_or_data: PChar; Mode: Cardinal; Offset: Integer; Length: Integer): PFSoundStream; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Create: function (Callback: TFSoundStreamCallback; Length: Integer; Mode: Cardinal; SampleRate: Integer; UserData: Integer): PFSoundStream; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Close: function(Stream: PFSoundStream): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_Play: function(Channel: Integer; Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_PlayEx: function (Channel: Integer; Stream: PFSoundStream; Dsp: PFSoundDSPUnit; StartPaused: ByteBool): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Stop: function(Stream: PFSoundStream): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_SetPosition: function (Stream: PFSoundStream; Position: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetPosition: function (Stream: PFSoundStream): Cardinal; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetTime: function (Stream: PFSoundStream; Ms: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetTime: function (Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetLength: function (Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetLengthMs: function (Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_SetMode: function (Stream: PFSoundStream; mode: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetMode: function (Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetLoopPoints: function (Stream: PFSoundStream; LoopStartPCM, LoopEndPCM: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetLoopCount: function (Stream: PFSoundStream; Count: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetOpenState: function (Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetSample: function (Stream: PFSoundStream): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF}; { Every stream contains a sample to play back on }
  FSOUND_Stream_CreateDSP: function (Stream: PFSoundStream; Callback: TFSoundDSPCallback; Priority: Integer; Param: Integer): PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_SetEndCallback: function (Stream: PFSoundStream; Callback: TFSoundStreamCallback; UserData: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetSyncCallback: function (Stream: PFSoundStream; Callback: TFSoundStreamCallback; UserData: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  
  FSOUND_Stream_AddSyncPoint: function (Stream: PFSoundStream; PCMOffset: Cardinal; Name: PChar): PFSyncPoint; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_DeleteSyncPoint: function (Point: PFSyncPoint): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetNumSyncPoints: function (Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetSyncPoint: function (Stream: PFSoundStream; Index: Integer): PFSyncPoint; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetSyncPointInfo: function (Point: PFSyncPoint; var PCMOffset: Cardinal): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_SetSubStream: function (Stream: PFSoundStream; Index: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetNumSubStreams: function (Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetSubStreamSentence: function (Stream: PFSoundStream; var sentencelist: Cardinal; numitems: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
                                                
  FSOUND_Stream_GetNumTagFields: function (Stream: PFSoundStream; var Num: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetTagField: function (Stream: PFSoundStream; Num: Integer; var _Type: TFSoundTagFieldType; var Name: PCHAR; var Value: Pointer; var Length: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_FindTagField: function (Stream: PFSoundStream; _Type: TFSoundTagFieldType; Name: PChar; var Value: Pointer; var Length: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_Net_SetProxy: function (Proxy: PChar): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Net_GetLastServerStatus: function: PChar; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Net_SetBufferProperties: function (BufferSize: Integer; PreBuffer_Percent: Integer; ReBuffer_Percent:  Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Net_GetBufferProperties: function (var Buffersize: Integer; var PreBuffer_Percent: Integer;  var ReBuffer_Percent: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Net_SetMetadataCallback: function (Stream: PFSoundStream; Callback: TFMetaDataCallback; UserData: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Net_GetStatus: function (Stream: PFSoundStream; var Status: TFSoundStreamNetStatus; var BufferPercentUsed: Integer; var BitRate: Integer; var Flags: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ =================== }
{ CD audio functions. }
{ =================== }

{
    Note : 0 = default cdrom.  Otherwise specify the drive letter, for example. 'D'. 
}

var
  FSOUND_CD_Play: function (Drive: Byte; Track: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_SetPlayMode: procedure (Drive: Byte; Mode: Integer); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_Stop: function (Drive: Byte): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_SetPaused: function (Drive: Byte; Paused: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_SetVolume: function (Drive: Byte; Volume: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_SetTrackTime: function (Drive: Byte; ms: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_OpenTray: function (Drive: Byte; Open: Byte): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

var
  FSOUND_CD_GetPaused: function (Drive: Byte): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetTrack: function (Drive: Byte): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetNumTracks: function (Drive: Byte): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetVolume: function (Drive: Byte): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetTrackLength: function (Drive: Byte; Track: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetTrackTime: function (Drive: Byte): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ ============== }
{ DSP functions. }
{ ============== }

{
  DSP Unit control and information functions.
}

var
  FSOUND_DSP_Create: function (Callback: TFSoundDSPCallback; Priority: Integer; Param: Integer): PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_Free: procedure (DSPUnit: PFSoundDSPUnit); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_SetPriority: procedure (DSPUnit: PFSoundDSPUnit; Priority: Integer); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetPriority: function (DSPUnit: PFSoundDSPUnit): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_SetActive: procedure (DSPUnit: PFSoundDSPUnit; Active: ByteBool); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetActive: function (DSPUnit: PFSoundDSPUnit): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Functions to get hold of FSOUND 'system DSP unit' handles.
}

var
  FSOUND_DSP_GetClearUnit: function: PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetSFXUnit: function: PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetMusicUnit: function: PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetClipAndCopyUnit: function: PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetFFTUnit: function: PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Miscellaneous DSP functions
  Note for the spectrum analysis function to work, you have to enable the FFT DSP unit with
  the following code FSOUND_DSP_SetActive(FSOUND_DSP_GetFFTUnit(), TRUE);
  It is off by default to save cpu usage.
}

var
  FSOUND_DSP_MixBuffers: function (DestBuffer: Pointer; SrcBuffer: Pointer; Len: Integer; Freq: Integer; Vol: Integer; Pan: Integer; Mode: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_ClearMixBuffer: procedure; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetBufferLength: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};      { Length of each DSP update }
  FSOUND_DSP_GetBufferLengthTotal: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF}; { Total buffer length due to FSOUND_SetBufferSize }
  FSOUND_DSP_GetSpectrum: function: PSingle; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};          { Array of 512 floats - call FSOUND_DSP_SetActive(FSOUND_DSP_GetFFTUnit(), TRUE)) for this to work. }

{ ========================================================================== }
{ Reverb functions. (eax2/eax3 reverb)  (NOT SUPPORTED IN UNIX/CE)               }
{ ========================================================================== }

{
  See structures above for definitions and information on the reverb parameters.
}

var
  FSOUND_Reverb_SetProperties: function (const Prop: TFSoundReverbProperties): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Reverb_GetProperties: function (var Prop: TFSoundReverbProperties): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Reverb_SetChannelProperties: function (Channel: Integer; var Prop: TFSoundReverbChannelProperties): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Reverb_GetChannelProperties: function (Channel: Integer; var Prop: TFSoundReverbChannelProperties): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ ================================================ }
{ Recording functions  (NOT SUPPORTED IN UNIX/MAC) }
{ ================================================ }

{
  Recording initialization functions
}

var
  FSOUND_Record_SetDriver: function (OutputType: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_GetNumDrivers: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_GetDriverName: function (Id: Integer): PChar; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_GetDriver: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Recording functionality. Only one recording session will work at a time.
}

var
  FSOUND_Record_StartSample: function (Sptr: PFSoundSample; Loop: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_Stop: function: ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_GetPosition: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ ============================================================================================= }
{ FMUSIC API (MOD,S3M,XM,IT,MIDI PLAYBACK)                                                      }
{ ============================================================================================= }

{
  Song management / playback functions.
}

var
  FMUSIC_LoadSong: function (const Name: PChar): PFMusicModule; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_LoadSongEx: function (Name_Or_Data: Pointer; Offset: Integer; Length: Integer; Mode: Cardinal; var SampleList: Integer; SampleListNum: Integer): PFMusicModule; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetOpenState: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_FreeSong: function (Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_PlaySong: function (Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_StopSong: function (Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_StopAllSongs: procedure; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

var
  FMUSIC_SetZxxCallback: function (Module: PFMusicModule; Callback: TFMusicCallback): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetRowCallback: function (Module: PFMusicModule; Callback: TFMusicCallback; RowStep: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetOrderCallback: function (Module: PFMusicModule; Callback: TFMusicCallback; OrderStep: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetInstCallback: function (Module: PFMusicModule; Callback: TFMusicCallback; Instrument: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

var
  FMUSIC_SetSample: function (Module: PFMusicModule; SampNo: Integer; Sptr: PFSoundSample): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetUserData: function (Module: PFMusicModule; userdata: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_OptimizeChannels: function (Module: PFMusicModule; MaxChannels: Integer; MinVolume: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Runtime song functions.
}

var
  FMUSIC_SetReverb: function (Reverb: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetLooping: function (Module: PFMusicModule; Looping: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetOrder: function (Module: PFMusicModule; Order: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetPaused: function (Module: PFMusicModule; Pause: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetMasterVolume: function (Module: PFMusicModule; Volume: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetMasterSpeed: function (Module: PFMusicModule; Speed: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetPanSeperation: function (Module: PFMusicModule; PanSep: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Static song information functions.
}

var
  FMUSIC_GetName: function (Module: PFMusicModule): PCHAR; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetType: function (Module: PFMusicModule): TFMusicTypes; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumOrders: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumPatterns: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumInstruments: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumSamples: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumChannels: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetSample: function (Module: PFMusicModule; SampNo: Integer): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetPatternLength: function (Module: PFMusicModule; OrderNo: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Runtime song information.
}

var
  FMUSIC_IsFinished: function (Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_IsPlaying: function (Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetMasterVolume: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetGlobalVolume: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetOrder: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetPattern: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetSpeed: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetBPM: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetRow: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetPaused: function (Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetTime: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetRealChannel: function (Module: PFMusicModule; ModChannel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetUserData: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

implementation

{$IFDEF UNIX}
uses
  dl; //Libc;
{$ENDIF}

const
{$IFDEF UNIX}
  FMOD_DLL = 'libfmod.so';
{$ENDIF}
{$IFDEF WINDOWS}
  FMOD_DLL = 'fmod.dll';
{$ENDIF}

type
{$IFDEF UNIX}
  TFMODModuleHandle = Pointer;
{$ELSE}
  TFMODModuleHandle = HINST;
{$ENDIF}

const
{$IFDEF UNIX}
  INVALID_MODULEHANDLE_VALUE = TFMODModuleHandle(nil);
{$ELSE}
  INVALID_MODULEHANDLE_VALUE = TFMODModuleHandle(0);
{$ENDIF}

var
  FMODHandle: TFMODModuleHandle;

function GetAddress(Handle: TFMODModuleHandle; FuncName: PChar): Pointer;
begin
{$IFDEF WINDOWS}
  Result := GetProcAddress(Handle, FuncName);
{$ELSE}
  Result := dlsym(Handle, FuncName);
{$ENDIF}
  Assert(Result <> nil, 'Failed to find ' + FuncName + ' in ' + FMOD_DLL);
end;

function FMOD_Load(LibName: PChar): Boolean;
begin
  Result := False;

  { Make sure the previous library is unloaded }
  FMOD_Unload;

  { If no library name given, use the default library names }
  if (LibName = nil) or (LibName='') then
    LibName := FMOD_DLL;
  writeln('loading');
  { Load the library }
{$IFDEF WINDOWS}
  FMODHandle := LoadLibrary(LibName);
{$ELSE}
  FMODHandle := dlopen(LibName, RTLD_NOW);
{$ENDIF}

  if FMODHandle = INVALID_MODULEHANDLE_VALUE then
    Exit;

{$IFDEF WINDOWS}
  { Get all the function addresses from the library }
  pointer(FSOUND_SetOutput)                      := GetAddress(FMODHandle, '_FSOUND_SetOutput@4');
  pointer(FSOUND_SetDriver)                      := GetAddress(FMODHandle, '_FSOUND_SetDriver@4');
  pointer(FSOUND_SetMixer)                       := GetAddress(FMODHandle, '_FSOUND_SetMixer@4');
  pointer(FSOUND_SetBufferSize)                  := GetAddress(FMODHandle, '_FSOUND_SetBufferSize@4');
  pointer(FSOUND_SetHWND)                        := GetAddress(FMODHandle, '_FSOUND_SetHWND@4');
  pointer(FSOUND_SetMinHardwareChannels)         := GetAddress(FMODHandle, '_FSOUND_SetMinHardwareChannels@4');
  pointer(FSOUND_SetMaxHardwareChannels)         := GetAddress(FMODHandle, '_FSOUND_SetMaxHardwareChannels@4');
  pointer(FSOUND_SetMemorySystem)                := GetAddress(FMODHandle, '_FSOUND_SetMemorySystem@20');
  pointer(FSOUND_Init)                           := GetAddress(FMODHandle, '_FSOUND_Init@12');
  pointer(FSOUND_Close)                          := GetAddress(FMODHandle, '_FSOUND_Close@0');
  pointer(FSOUND_Update)                         := GetAddress(FMODHandle, '_FSOUND_Update@0');
  pointer(FSOUND_SetSpeakerMode)                 := GetAddress(FMODHandle, '_FSOUND_SetSpeakerMode@4');
  pointer(FSOUND_SetSFXMasterVolume)             := GetAddress(FMODHandle, '_FSOUND_SetSFXMasterVolume@4');
  pointer(FSOUND_SetPanSeperation)               := GetAddress(FMODHandle, '_FSOUND_SetPanSeperation@4');
  pointer(FSOUND_GetError)                       := GetAddress(FMODHandle, '_FSOUND_GetError@0');
  pointer(FSOUND_GetVersion)                     := GetAddress(FMODHandle, '_FSOUND_GetVersion@0');
  pointer(FSOUND_GetOutput)                      := GetAddress(FMODHandle, '_FSOUND_GetOutput@0');
  pointer(FSOUND_GetOutputHandle)                := GetAddress(FMODHandle, '_FSOUND_GetOutputHandle@0');
  pointer(FSOUND_GetDriver)                      := GetAddress(FMODHandle, '_FSOUND_GetDriver@0');
  pointer(FSOUND_GetMixer)                       := GetAddress(FMODHandle, '_FSOUND_GetMixer@0');
  pointer(FSOUND_GetNumDrivers)                  := GetAddress(FMODHandle, '_FSOUND_GetNumDrivers@0');
  pointer(FSOUND_GetDriverName)                  := GetAddress(FMODHandle, '_FSOUND_GetDriverName@4');
  pointer(FSOUND_GetDriverCaps)                  := GetAddress(FMODHandle, '_FSOUND_GetDriverCaps@8');
  pointer(FSOUND_GetOutputRate)                  := GetAddress(FMODHandle, '_FSOUND_GetOutputRate@0');
  pointer(FSOUND_GetMaxChannels)                 := GetAddress(FMODHandle, '_FSOUND_GetMaxChannels@0');
  pointer(FSOUND_GetMaxSamples)                  := GetAddress(FMODHandle, '_FSOUND_GetMaxSamples@0');
  pointer(FSOUND_GetSpeakerMode)                 := GetAddress(FMODHandle, '_FSOUND_GetSpeakerMode@0');
  pointer(FSOUND_GetSFXMasterVolume)             := GetAddress(FMODHandle, '_FSOUND_GetSFXMasterVolume@0');
  pointer(FSOUND_GetNumHWChannels)               := GetAddress(FMODHandle, '_FSOUND_GetNumHWChannels@12');
  pointer(FSOUND_GetChannelsPlaying)             := GetAddress(FMODHandle, '_FSOUND_GetChannelsPlaying@0');
  pointer(FSOUND_GetCPUUsage)                    := GetAddress(FMODHandle, '_FSOUND_GetCPUUsage@0');
  pointer(FSOUND_GetMemoryStats)                 := GetAddress(FMODHandle, '_FSOUND_GetMemoryStats@8');
  pointer(FSOUND_Sample_Load)                    := GetAddress(FMODHandle, '_FSOUND_Sample_Load@20');
  pointer(FSOUND_Sample_Alloc)                   := GetAddress(FMODHandle, '_FSOUND_Sample_Alloc@28');
  pointer(FSOUND_Sample_Free)                    := GetAddress(FMODHandle, '_FSOUND_Sample_Free@4');
  pointer(FSOUND_Sample_Upload)                  := GetAddress(FMODHandle, '_FSOUND_Sample_Upload@12');
  pointer(FSOUND_Sample_Lock)                    := GetAddress(FMODHandle, '_FSOUND_Sample_Lock@28');
  pointer(FSOUND_Sample_Unlock)                  := GetAddress(FMODHandle, '_FSOUND_Sample_Unlock@20');
  pointer(FSOUND_Sample_SetMode)                 := GetAddress(FMODHandle, '_FSOUND_Sample_SetMode@8');
  pointer(FSOUND_Sample_SetLoopPoints)           := GetAddress(FMODHandle, '_FSOUND_Sample_SetLoopPoints@12');
  pointer(FSOUND_Sample_SetDefaults)             := GetAddress(FMODHandle, '_FSOUND_Sample_SetDefaults@20');
  pointer(FSOUND_Sample_SetDefaultsEx)           := GetAddress(FMODHandle, '_FSOUND_Sample_SetDefaultsEx@32');
  pointer(FSOUND_Sample_SetMinMaxDistance)       := GetAddress(FMODHandle, '_FSOUND_Sample_SetMinMaxDistance@12');
  pointer(FSOUND_Sample_SetMaxPlaybacks)         := GetAddress(FMODHandle, '_FSOUND_Sample_SetMaxPlaybacks@8');
  pointer(FSOUND_Sample_Get)                     := GetAddress(FMODHandle, '_FSOUND_Sample_Get@4');
  pointer(FSOUND_Sample_GetName)                 := GetAddress(FMODHandle, '_FSOUND_Sample_GetName@4');
  pointer(FSOUND_Sample_GetLength)               := GetAddress(FMODHandle, '_FSOUND_Sample_GetLength@4');
  pointer(FSOUND_Sample_GetLoopPoints)           := GetAddress(FMODHandle, '_FSOUND_Sample_GetLoopPoints@12');
  pointer(FSOUND_Sample_GetDefaults)             := GetAddress(FMODHandle, '_FSOUND_Sample_GetDefaults@20');
  pointer(FSOUND_Sample_GetDefaultsEx)           := GetAddress(FMODHandle, '_FSOUND_Sample_GetDefaultsEx@32');
  pointer(FSOUND_Sample_GetMode)                 := GetAddress(FMODHandle, '_FSOUND_Sample_GetMode@4');
  pointer(FSOUND_Sample_GetMinMaxDistance)       := GetAddress(FMODHandle, '_FSOUND_Sample_GetMinMaxDistance@12');
  pointer(FSOUND_PlaySound)                      := GetAddress(FMODHandle, '_FSOUND_PlaySound@8');
  pointer(FSOUND_PlaySoundEx)                    := GetAddress(FMODHandle, '_FSOUND_PlaySoundEx@16');
  pointer(FSOUND_StopSound)                      := GetAddress(FMODHandle, '_FSOUND_StopSound@4');
  pointer(FSOUND_SetFrequency)                   := GetAddress(FMODHandle, '_FSOUND_SetFrequency@8');
  pointer(FSOUND_SetVolume)                      := GetAddress(FMODHandle, '_FSOUND_SetVolume@8');
  pointer(FSOUND_SetVolumeAbsolute)              := GetAddress(FMODHandle, '_FSOUND_SetVolumeAbsolute@8');
  pointer(FSOUND_SetPan)                         := GetAddress(FMODHandle, '_FSOUND_SetPan@8');
  pointer(FSOUND_SetSurround)                    := GetAddress(FMODHandle, '_FSOUND_SetSurround@8');
  pointer(FSOUND_SetMute)                        := GetAddress(FMODHandle, '_FSOUND_SetMute@8');
  pointer(FSOUND_SetPriority)                    := GetAddress(FMODHandle, '_FSOUND_SetPriority@8');
  pointer(FSOUND_SetReserved)                    := GetAddress(FMODHandle, '_FSOUND_SetReserved@8');
  pointer(FSOUND_SetPaused)                      := GetAddress(FMODHandle, '_FSOUND_SetPaused@8');
  pointer(FSOUND_SetLoopMode)                    := GetAddress(FMODHandle, '_FSOUND_SetLoopMode@8');
  pointer(FSOUND_SetCurrentPosition)             := GetAddress(FMODHandle, '_FSOUND_SetCurrentPosition@8');
  pointer(FSOUND_3D_SetAttributes)               := GetAddress(FMODHandle, '_FSOUND_3D_SetAttributes@12');
  pointer(FSOUND_3D_SetMinMaxDistance)           := GetAddress(FMODHandle, '_FSOUND_3D_SetMinMaxDistance@12');
  pointer(FSOUND_IsPlaying)                      := GetAddress(FMODHandle, '_FSOUND_IsPlaying@4');
  pointer(FSOUND_GetFrequency)                   := GetAddress(FMODHandle, '_FSOUND_GetFrequency@4');
  pointer(FSOUND_GetVolume)                      := GetAddress(FMODHandle, '_FSOUND_GetVolume@4');
  pointer(FSOUND_GetAmplitude)                   := GetAddress(FMODHandle, '_FSOUND_GetAmplitude@4');
  pointer(FSOUND_GetPan)                         := GetAddress(FMODHandle, '_FSOUND_GetPan@4');
  pointer(FSOUND_GetSurround)                    := GetAddress(FMODHandle, '_FSOUND_GetSurround@4');
  pointer(FSOUND_GetMute)                        := GetAddress(FMODHandle, '_FSOUND_GetMute@4');
  pointer(FSOUND_GetPriority)                    := GetAddress(FMODHandle, '_FSOUND_GetPriority@4');
  pointer(FSOUND_GetReserved)                    := GetAddress(FMODHandle, '_FSOUND_GetReserved@4');
  pointer(FSOUND_GetPaused)                      := GetAddress(FMODHandle, '_FSOUND_GetPaused@4');
  pointer(FSOUND_GetLoopMode)                    := GetAddress(FMODHandle, '_FSOUND_GetLoopMode@4');
  pointer(FSOUND_GetCurrentPosition)             := GetAddress(FMODHandle, '_FSOUND_GetCurrentPosition@4');
  pointer(FSOUND_GetCurrentSample)               := GetAddress(FMODHandle, '_FSOUND_GetCurrentSample@4');
  pointer(FSOUND_GetCurrentLevels)               := GetAddress(FMODHandle, '_FSOUND_GetCurrentLevels@12');
  pointer(FSOUND_GetNumSubChannels)              := GetAddress(FMODHandle, '_FSOUND_GetNumSubChannels@4');
  pointer(FSOUND_GetSubChannel)                  := GetAddress(FMODHandle, '_FSOUND_GetSubChannel@8');
  pointer(FSOUND_3D_GetAttributes)               := GetAddress(FMODHandle, '_FSOUND_3D_GetAttributes@12');
  pointer(FSOUND_3D_GetMinMaxDistance)           := GetAddress(FMODHandle, '_FSOUND_3D_GetMinMaxDistance@12');
  pointer(FSOUND_3D_Listener_SetCurrent)         := GetAddress(FMODHandle, '_FSOUND_3D_Listener_SetCurrent@8');
  pointer(FSOUND_3D_Listener_SetAttributes)      := GetAddress(FMODHandle, '_FSOUND_3D_Listener_SetAttributes@32');
  pointer(FSOUND_3D_Listener_GetAttributes)      := GetAddress(FMODHandle, '_FSOUND_3D_Listener_GetAttributes@32');
  pointer(FSOUND_3D_SetDopplerFactor)            := GetAddress(FMODHandle, '_FSOUND_3D_SetDopplerFactor@4');
  pointer(FSOUND_3D_SetDistanceFactor)           := GetAddress(FMODHandle, '_FSOUND_3D_SetDistanceFactor@4');
  pointer(FSOUND_3D_SetRolloffFactor)            := GetAddress(FMODHandle, '_FSOUND_3D_SetRolloffFactor@4');
  pointer(FSOUND_FX_Enable)                      := GetAddress(FMODHandle, '_FSOUND_FX_Enable@8');
  pointer(FSOUND_FX_SetChorus)                   := GetAddress(FMODHandle, '_FSOUND_FX_SetChorus@32');
  pointer(FSOUND_FX_SetCompressor)               := GetAddress(FMODHandle, '_FSOUND_FX_SetCompressor@28');
  pointer(FSOUND_FX_SetDistortion)               := GetAddress(FMODHandle, '_FSOUND_FX_SetDistortion@24');
  pointer(FSOUND_FX_SetEcho)                     := GetAddress(FMODHandle, '_FSOUND_FX_SetEcho@24');
  pointer(FSOUND_FX_SetFlanger)                  := GetAddress(FMODHandle, '_FSOUND_FX_SetFlanger@32');
  pointer(FSOUND_FX_SetGargle)                   := GetAddress(FMODHandle, '_FSOUND_FX_SetGargle@12');
  pointer(FSOUND_FX_SetI3DL2Reverb)              := GetAddress(FMODHandle, '_FSOUND_FX_SetI3DL2Reverb@52');
  pointer(FSOUND_FX_SetParamEQ)                  := GetAddress(FMODHandle, '_FSOUND_FX_SetParamEQ@16');
  pointer(FSOUND_FX_SetWavesReverb)              := GetAddress(FMODHandle, '_FSOUND_FX_SetWavesReverb@20');
  pointer(FSOUND_Stream_Open)                    := GetAddress(FMODHandle, '_FSOUND_Stream_Open@16');
  pointer(FSOUND_Stream_Create)                  := GetAddress(FMODHandle, '_FSOUND_Stream_Create@20');
  pointer(FSOUND_Stream_Close)                   := GetAddress(FMODHandle, '_FSOUND_Stream_Close@4');
  pointer(FSOUND_Stream_Play)                    := GetAddress(FMODHandle, '_FSOUND_Stream_Play@8');
  pointer(FSOUND_Stream_PlayEx)                  := GetAddress(FMODHandle, '_FSOUND_Stream_PlayEx@16');
  pointer(FSOUND_Stream_Stop)                    := GetAddress(FMODHandle, '_FSOUND_Stream_Stop@4');
  pointer(FSOUND_Stream_SetEndCallback)          := GetAddress(FMODHandle, '_FSOUND_Stream_SetEndCallback@12');
  pointer(FSOUND_Stream_SetSyncCallback)         := GetAddress(FMODHandle, '_FSOUND_Stream_SetSyncCallback@12');
  pointer(FSOUND_Stream_GetSample)               := GetAddress(FMODHandle, '_FSOUND_Stream_GetSample@4');
  pointer(FSOUND_Stream_CreateDSP)               := GetAddress(FMODHandle, '_FSOUND_Stream_CreateDSP@16');
  pointer(FSOUND_Stream_SetBufferSize)           := GetAddress(FMODHandle, '_FSOUND_Stream_SetBufferSize@4');
  pointer(FSOUND_Stream_SetPosition)             := GetAddress(FMODHandle, '_FSOUND_Stream_SetPosition@8');
  pointer(FSOUND_Stream_GetPosition)             := GetAddress(FMODHandle, '_FSOUND_Stream_GetPosition@4');
  pointer(FSOUND_Stream_SetTime)                 := GetAddress(FMODHandle, '_FSOUND_Stream_SetTime@8');
  pointer(FSOUND_Stream_GetTime)                 := GetAddress(FMODHandle, '_FSOUND_Stream_GetTime@4');
  pointer(FSOUND_Stream_GetLength)               := GetAddress(FMODHandle, '_FSOUND_Stream_GetLength@4');
  pointer(FSOUND_Stream_GetLengthMs)             := GetAddress(FMODHandle, '_FSOUND_Stream_GetLengthMs@4');
  pointer(FSOUND_Stream_SetMode)                 := GetAddress(FMODHandle, '_FSOUND_Stream_SetMode@8');
  pointer(FSOUND_Stream_GetMode)                 := GetAddress(FMODHandle, '_FSOUND_Stream_GetMode@4');
  pointer(FSOUND_Stream_SetLoopPoints)           := GetAddress(FMODHandle, '_FSOUND_Stream_SetLoopPoints@12');
  pointer(FSOUND_Stream_SetLoopCount)            := GetAddress(FMODHandle, '_FSOUND_Stream_SetLoopCount@8');
  pointer(FSOUND_Stream_GetOpenState)            := GetAddress(FMODHandle, '_FSOUND_Stream_GetOpenState@4');
  pointer(FSOUND_Stream_AddSyncPoint)            := GetAddress(FMODHandle, '_FSOUND_Stream_AddSyncPoint@12');
  pointer(FSOUND_Stream_DeleteSyncPoint)         := GetAddress(FMODHandle, '_FSOUND_Stream_DeleteSyncPoint@4');
  pointer(FSOUND_Stream_GetNumSyncPoints)        := GetAddress(FMODHandle, '_FSOUND_Stream_GetNumSyncPoints@4');
  pointer(FSOUND_Stream_GetSyncPoint)            := GetAddress(FMODHandle, '_FSOUND_Stream_GetSyncPoint@8');
  pointer(FSOUND_Stream_GetSyncPointInfo)        := GetAddress(FMODHandle, '_FSOUND_Stream_GetSyncPointInfo@8');
  pointer(FSOUND_Stream_SetSubStream)            := GetAddress(FMODHandle, '_FSOUND_Stream_SetSubStream@8');
  pointer(FSOUND_Stream_GetNumSubStreams)        := GetAddress(FMODHandle, '_FSOUND_Stream_GetNumSubStreams@4');
  pointer(FSOUND_Stream_SetSubStreamSentence)    := GetAddress(FMODHandle, '_FSOUND_Stream_SetSubStreamSentence@12');
  pointer(FSOUND_Stream_GetNumTagFields)         := GetAddress(FMODHandle, '_FSOUND_Stream_GetNumTagFields@8');
  pointer(FSOUND_Stream_GetTagField)             := GetAddress(FMODHandle, '_FSOUND_Stream_GetTagField@24');
  pointer(FSOUND_Stream_FindTagField)            := GetAddress(FMODHandle, '_FSOUND_Stream_FindTagField@20');
  pointer(FSOUND_Stream_Net_SetProxy)            := GetAddress(FMODHandle, '_FSOUND_Stream_Net_SetProxy@4');
  pointer(FSOUND_Stream_Net_GetLastServerStatus) := GetAddress(FMODHandle, '_FSOUND_Stream_Net_GetLastServerStatus@0');
  pointer(FSOUND_Stream_Net_SetBufferProperties) := GetAddress(FMODHandle, '_FSOUND_Stream_Net_SetBufferProperties@12');
  pointer(FSOUND_Stream_Net_GetBufferProperties) := GetAddress(FMODHandle, '_FSOUND_Stream_Net_GetBufferProperties@12');
  pointer(FSOUND_Stream_Net_SetMetadataCallback) := GetAddress(FMODHandle, '_FSOUND_Stream_Net_SetMetadataCallback@12');
  pointer(FSOUND_Stream_Net_GetStatus)           := GetAddress(FMODHandle, '_FSOUND_Stream_Net_GetStatus@20');
  pointer(FSOUND_CD_Play)                        := GetAddress(FMODHandle, '_FSOUND_CD_Play@8');
  pointer(FSOUND_CD_SetPlayMode)                 := GetAddress(FMODHandle, '_FSOUND_CD_SetPlayMode@8');
  pointer(FSOUND_CD_Stop)                        := GetAddress(FMODHandle, '_FSOUND_CD_Stop@4');
  pointer(FSOUND_CD_SetPaused)                   := GetAddress(FMODHandle, '_FSOUND_CD_SetPaused@8');
  pointer(FSOUND_CD_SetVolume)                   := GetAddress(FMODHandle, '_FSOUND_CD_SetVolume@8');
  pointer(FSOUND_CD_SetTrackTime)                := GetAddress(FMODHandle, '_FSOUND_CD_SetTrackTime@8');
  pointer(FSOUND_CD_OpenTray)                    := GetAddress(FMODHandle, '_FSOUND_CD_OpenTray@8');
  pointer(FSOUND_CD_GetPaused)                   := GetAddress(FMODHandle, '_FSOUND_CD_GetPaused@4');
  pointer(FSOUND_CD_GetTrack)                    := GetAddress(FMODHandle, '_FSOUND_CD_GetTrack@4');
  pointer(FSOUND_CD_GetNumTracks)                := GetAddress(FMODHandle, '_FSOUND_CD_GetNumTracks@4');
  pointer(FSOUND_CD_GetVolume)                   := GetAddress(FMODHandle, '_FSOUND_CD_GetVolume@4');
  pointer(FSOUND_CD_GetTrackLength)              := GetAddress(FMODHandle, '_FSOUND_CD_GetTrackLength@8');
  pointer(FSOUND_CD_GetTrackTime)                := GetAddress(FMODHandle, '_FSOUND_CD_GetTrackTime@4');
  pointer(FSOUND_DSP_Create)                     := GetAddress(FMODHandle, '_FSOUND_DSP_Create@12');
  pointer(FSOUND_DSP_Free)                       := GetAddress(FMODHandle, '_FSOUND_DSP_Free@4');
  pointer(FSOUND_DSP_SetPriority)                := GetAddress(FMODHandle, '_FSOUND_DSP_SetPriority@8');
  pointer(FSOUND_DSP_GetPriority)                := GetAddress(FMODHandle, '_FSOUND_DSP_GetPriority@4');
  pointer(FSOUND_DSP_SetActive)                  := GetAddress(FMODHandle, '_FSOUND_DSP_SetActive@8');
  pointer(FSOUND_DSP_GetActive)                  := GetAddress(FMODHandle, '_FSOUND_DSP_GetActive@4');
  pointer(FSOUND_DSP_GetClearUnit)               := GetAddress(FMODHandle, '_FSOUND_DSP_GetClearUnit@0');
  pointer(FSOUND_DSP_GetSFXUnit)                 := GetAddress(FMODHandle, '_FSOUND_DSP_GetSFXUnit@0');
  pointer(FSOUND_DSP_GetMusicUnit)               := GetAddress(FMODHandle, '_FSOUND_DSP_GetMusicUnit@0');
  pointer(FSOUND_DSP_GetClipAndCopyUnit)         := GetAddress(FMODHandle, '_FSOUND_DSP_GetClipAndCopyUnit@0');
  pointer(FSOUND_DSP_GetFFTUnit)                 := GetAddress(FMODHandle, '_FSOUND_DSP_GetFFTUnit@0');
  pointer(FSOUND_DSP_MixBuffers)                 := GetAddress(FMODHandle, '_FSOUND_DSP_MixBuffers@28');
  pointer(FSOUND_DSP_ClearMixBuffer)             := GetAddress(FMODHandle, '_FSOUND_DSP_ClearMixBuffer@0');
  pointer(FSOUND_DSP_GetBufferLength)            := GetAddress(FMODHandle, '_FSOUND_DSP_GetBufferLength@0');
  pointer(FSOUND_DSP_GetBufferLengthTotal)       := GetAddress(FMODHandle, '_FSOUND_DSP_GetBufferLengthTotal@0');
  pointer(FSOUND_DSP_GetSpectrum)                := GetAddress(FMODHandle, '_FSOUND_DSP_GetSpectrum@0');
  pointer(FSOUND_Reverb_SetProperties)           := GetAddress(FMODHandle, '_FSOUND_Reverb_SetProperties@4');
  pointer(FSOUND_Reverb_GetProperties)           := GetAddress(FMODHandle, '_FSOUND_Reverb_GetProperties@4');
  pointer(FSOUND_Reverb_SetChannelProperties)    := GetAddress(FMODHandle, '_FSOUND_Reverb_SetChannelProperties@8');
  pointer(FSOUND_Reverb_GetChannelProperties)    := GetAddress(FMODHandle, '_FSOUND_Reverb_GetChannelProperties@8');
  pointer(FSOUND_Record_SetDriver)               := GetAddress(FMODHandle, '_FSOUND_Record_SetDriver@4');
  pointer(FSOUND_Record_GetNumDrivers)           := GetAddress(FMODHandle, '_FSOUND_Record_GetNumDrivers@0');
  pointer(FSOUND_Record_GetDriverName)           := GetAddress(FMODHandle, '_FSOUND_Record_GetDriverName@4');
  pointer(FSOUND_Record_GetDriver)               := GetAddress(FMODHandle, '_FSOUND_Record_GetDriver@0');
  pointer(FSOUND_Record_StartSample)             := GetAddress(FMODHandle, '_FSOUND_Record_StartSample@8');
  pointer(FSOUND_Record_Stop)                    := GetAddress(FMODHandle, '_FSOUND_Record_Stop@0');
  pointer(FSOUND_Record_GetPosition)             := GetAddress(FMODHandle, '_FSOUND_Record_GetPosition@0');
  pointer(FSOUND_File_SetCallbacks)              := GetAddress(FMODHandle, '_FSOUND_File_SetCallbacks@20');
  pointer(FMUSIC_LoadSong)                       := GetAddress(FMODHandle, '_FMUSIC_LoadSong@4');
  pointer(FMUSIC_LoadSongEx)                     := GetAddress(FMODHandle, '_FMUSIC_LoadSongEx@24');
  pointer(FMUSIC_GetOpenState)                   := GetAddress(FMODHandle, '_FMUSIC_GetOpenState@4');
  pointer(FMUSIC_FreeSong)                       := GetAddress(FMODHandle, '_FMUSIC_FreeSong@4');
  pointer(FMUSIC_PlaySong)                       := GetAddress(FMODHandle, '_FMUSIC_PlaySong@4');
  pointer(FMUSIC_StopSong)                       := GetAddress(FMODHandle, '_FMUSIC_StopSong@4');
  pointer(FMUSIC_StopAllSongs)                   := GetAddress(FMODHandle, '_FMUSIC_StopAllSongs@0');
  pointer(FMUSIC_SetZxxCallback)                 := GetAddress(FMODHandle, '_FMUSIC_SetZxxCallback@8');
  pointer(FMUSIC_SetRowCallback)                 := GetAddress(FMODHandle, '_FMUSIC_SetRowCallback@12');
  pointer(FMUSIC_SetOrderCallback)               := GetAddress(FMODHandle, '_FMUSIC_SetOrderCallback@12');
  pointer(FMUSIC_SetInstCallback)                := GetAddress(FMODHandle, '_FMUSIC_SetInstCallback@12');
  pointer(FMUSIC_SetSample)                      := GetAddress(FMODHandle, '_FMUSIC_SetSample@12');
  pointer(FMUSIC_SetUserData)                    := GetAddress(FMODHandle, '_FMUSIC_SetUserData@8');
  pointer(FMUSIC_OptimizeChannels)               := GetAddress(FMODHandle, '_FMUSIC_OptimizeChannels@12');
  pointer(FMUSIC_SetReverb)                      := GetAddress(FMODHandle, '_FMUSIC_SetReverb@4');
  pointer(FMUSIC_SetLooping)                     := GetAddress(FMODHandle, '_FMUSIC_SetLooping@8');
  pointer(FMUSIC_SetOrder)                       := GetAddress(FMODHandle, '_FMUSIC_SetOrder@8');
  pointer(FMUSIC_SetPaused)                      := GetAddress(FMODHandle, '_FMUSIC_SetPaused@8');
  pointer(FMUSIC_SetMasterVolume)                := GetAddress(FMODHandle, '_FMUSIC_SetMasterVolume@8');
  pointer(FMUSIC_SetMasterSpeed)                 := GetAddress(FMODHandle, '_FMUSIC_SetMasterSpeed@8');
  pointer(FMUSIC_SetPanSeperation)               := GetAddress(FMODHandle, '_FMUSIC_SetPanSeperation@8');
  pointer(FMUSIC_GetName)                        := GetAddress(FMODHandle, '_FMUSIC_GetName@4');
  pointer(FMUSIC_GetType)                        := GetAddress(FMODHandle, '_FMUSIC_GetType@4');
  pointer(FMUSIC_GetNumOrders)                   := GetAddress(FMODHandle, '_FMUSIC_GetNumOrders@4');
  pointer(FMUSIC_GetNumPatterns)                 := GetAddress(FMODHandle, '_FMUSIC_GetNumPatterns@4');
  pointer(FMUSIC_GetNumInstruments)              := GetAddress(FMODHandle, '_FMUSIC_GetNumInstruments@4');
  pointer(FMUSIC_GetNumSamples)                  := GetAddress(FMODHandle, '_FMUSIC_GetNumSamples@4');
  pointer(FMUSIC_GetNumChannels)                 := GetAddress(FMODHandle, '_FMUSIC_GetNumChannels@4');
  pointer(FMUSIC_GetSample)                      := GetAddress(FMODHandle, '_FMUSIC_GetSample@8');
  pointer(FMUSIC_GetPatternLength)               := GetAddress(FMODHandle, '_FMUSIC_GetPatternLength@8');
  pointer(FMUSIC_IsFinished)                     := GetAddress(FMODHandle, '_FMUSIC_IsFinished@4');
  pointer(FMUSIC_IsPlaying)                      := GetAddress(FMODHandle, '_FMUSIC_IsPlaying@4');
  pointer(FMUSIC_GetMasterVolume)                := GetAddress(FMODHandle, '_FMUSIC_GetMasterVolume@4');
  pointer(FMUSIC_GetGlobalVolume)                := GetAddress(FMODHandle, '_FMUSIC_GetGlobalVolume@4');
  pointer(FMUSIC_GetOrder)                       := GetAddress(FMODHandle, '_FMUSIC_GetOrder@4');
  pointer(FMUSIC_GetPattern)                     := GetAddress(FMODHandle, '_FMUSIC_GetPattern@4');
  pointer(FMUSIC_GetSpeed)                       := GetAddress(FMODHandle, '_FMUSIC_GetSpeed@4');
  pointer(FMUSIC_GetBPM)                         := GetAddress(FMODHandle, '_FMUSIC_GetBPM@4');
  pointer(FMUSIC_GetRow)                         := GetAddress(FMODHandle, '_FMUSIC_GetRow@4');
  pointer(FMUSIC_GetPaused)                      := GetAddress(FMODHandle, '_FMUSIC_GetPaused@4');
  pointer(FMUSIC_GetTime)                        := GetAddress(FMODHandle, '_FMUSIC_GetTime@4');
  pointer(FMUSIC_GetRealChannel)                 := GetAddress(FMODHandle, '_FMUSIC_GetRealChannel@8');
  pointer(FMUSIC_GetUserData)                    := GetAddress(FMODHandle, '_FMUSIC_GetUserData@4');
 {$ELSE}
  pointer(FSOUND_SetOutput)                      := GetAddress(FMODHandle, 'FSOUND_SetOutput');
  pointer(FSOUND_SetDriver)                      := GetAddress(FMODHandle, 'FSOUND_SetDriver');
  pointer(FSOUND_SetMixer)                       := GetAddress(FMODHandle, 'FSOUND_SetMixer');
  pointer(FSOUND_SetBufferSize)                  := GetAddress(FMODHandle, 'FSOUND_SetBufferSize');
  pointer(FSOUND_SetHWND)                        := GetAddress(FMODHandle, 'FSOUND_SetHWND');
  pointer(FSOUND_SetMinHardwareChannels)         := GetAddress(FMODHandle, 'FSOUND_SetMinHardwareChannels');
  pointer(FSOUND_SetMaxHardwareChannels)         := GetAddress(FMODHandle, 'FSOUND_SetMaxHardwareChannels');
  pointer(FSOUND_SetMemorySystem)                := GetAddress(FMODHandle, 'FSOUND_SetMemorySystem');
  pointer(FSOUND_Init)                           := GetAddress(FMODHandle, 'FSOUND_Init');
  pointer(FSOUND_Close)                          := GetAddress(FMODHandle, 'FSOUND_Close');
  pointer(FSOUND_Update)                         := GetAddress(FMODHandle, 'FSOUND_Update');
  pointer(FSOUND_SetSpeakerMode)                 := GetAddress(FMODHandle, 'FSOUND_SetSpeakerMode');
  pointer(FSOUND_SetSFXMasterVolume)             := GetAddress(FMODHandle, 'FSOUND_SetSFXMasterVolume');
  pointer(FSOUND_SetPanSeperation)               := GetAddress(FMODHandle, 'FSOUND_SetPanSeperation');
  pointer(FSOUND_GetError)                       := GetAddress(FMODHandle, 'FSOUND_GetError');
  pointer(FSOUND_GetVersion)                     := GetAddress(FMODHandle, 'FSOUND_GetVersion');
  pointer(FSOUND_GetOutput)                      := GetAddress(FMODHandle, 'FSOUND_GetOutput');
  pointer(FSOUND_GetOutputHandle)                := GetAddress(FMODHandle, 'FSOUND_GetOutputHandle');
  pointer(FSOUND_GetDriver)                      := GetAddress(FMODHandle, 'FSOUND_GetDriver');
  pointer(FSOUND_GetMixer)                       := GetAddress(FMODHandle, 'FSOUND_GetMixer');
  pointer(FSOUND_GetNumDrivers)                  := GetAddress(FMODHandle, 'FSOUND_GetNumDrivers');
  pointer(FSOUND_GetDriverName)                  := GetAddress(FMODHandle, 'FSOUND_GetDriverName');
  pointer(FSOUND_GetDriverCaps)                  := GetAddress(FMODHandle, 'FSOUND_GetDriverCaps');
  pointer(FSOUND_GetOutputRate)                  := GetAddress(FMODHandle, 'FSOUND_GetOutputRate');
  pointer(FSOUND_GetMaxChannels)                 := GetAddress(FMODHandle, 'FSOUND_GetMaxChannels');
  pointer(FSOUND_GetMaxSamples)                  := GetAddress(FMODHandle, 'FSOUND_GetMaxSamples');
  pointer(FSOUND_GetSpeakerMode)                 := GetAddress(FMODHandle, 'FSOUND_GetSpeakerMode');
  pointer(FSOUND_GetSFXMasterVolume)             := GetAddress(FMODHandle, 'FSOUND_GetSFXMasterVolume');
  pointer(FSOUND_GetNumHWChannels)               := GetAddress(FMODHandle, 'FSOUND_GetNumHWChannels');
  pointer(FSOUND_GetChannelsPlaying)             := GetAddress(FMODHandle, 'FSOUND_GetChannelsPlaying');
  pointer(FSOUND_GetCPUUsage)                    := GetAddress(FMODHandle, 'FSOUND_GetCPUUsage');
  pointer(FSOUND_GetMemoryStats)                 := GetAddress(FMODHandle, 'FSOUND_GetMemoryStats');
  pointer(FSOUND_Sample_Load)                    := GetAddress(FMODHandle, 'FSOUND_Sample_Load');
  pointer(FSOUND_Sample_Alloc)                   := GetAddress(FMODHandle, 'FSOUND_Sample_Alloc');
  pointer(FSOUND_Sample_Free)                    := GetAddress(FMODHandle, 'FSOUND_Sample_Free');
  pointer(FSOUND_Sample_Upload)                  := GetAddress(FMODHandle, 'FSOUND_Sample_Upload');
  pointer(FSOUND_Sample_Lock)                    := GetAddress(FMODHandle, 'FSOUND_Sample_Lock');
  pointer(FSOUND_Sample_Unlock)                  := GetAddress(FMODHandle, 'FSOUND_Sample_Unlock');
  pointer(FSOUND_Sample_SetMode)                 := GetAddress(FMODHandle, 'FSOUND_Sample_SetMode');
  pointer(FSOUND_Sample_SetLoopPoints)           := GetAddress(FMODHandle, 'FSOUND_Sample_SetLoopPoints');
  pointer(FSOUND_Sample_SetDefaults)             := GetAddress(FMODHandle, 'FSOUND_Sample_SetDefaults');
  pointer(FSOUND_Sample_SetDefaultsEx)           := GetAddress(FMODHandle, 'FSOUND_Sample_SetDefaultsEx');
  pointer(FSOUND_Sample_SetMinMaxDistance)       := GetAddress(FMODHandle, 'FSOUND_Sample_SetMinMaxDistance');
  pointer(FSOUND_Sample_SetMaxPlaybacks)         := GetAddress(FMODHandle, 'FSOUND_Sample_SetMaxPlaybacks');
  pointer(FSOUND_Sample_Get)                     := GetAddress(FMODHandle, 'FSOUND_Sample_Get');
  pointer(FSOUND_Sample_GetName)                 := GetAddress(FMODHandle, 'FSOUND_Sample_GetName');
  pointer(FSOUND_Sample_GetLength)               := GetAddress(FMODHandle, 'FSOUND_Sample_GetLength');
  pointer(FSOUND_Sample_GetLoopPoints)           := GetAddress(FMODHandle, 'FSOUND_Sample_GetLoopPoints');
  pointer(FSOUND_Sample_GetDefaults)             := GetAddress(FMODHandle, 'FSOUND_Sample_GetDefaults');
  pointer(FSOUND_Sample_GetDefaultsEx)           := GetAddress(FMODHandle, 'FSOUND_Sample_GetDefaultsEx');
  pointer(FSOUND_Sample_GetMode)                 := GetAddress(FMODHandle, 'FSOUND_Sample_GetMode');
  pointer(FSOUND_Sample_GetMinMaxDistance)       := GetAddress(FMODHandle, 'FSOUND_Sample_GetMinMaxDistance');
  pointer(FSOUND_PlaySound)                      := GetAddress(FMODHandle, 'FSOUND_PlaySound');
  pointer(FSOUND_PlaySoundEx)                    := GetAddress(FMODHandle, 'FSOUND_PlaySoundEx');
  pointer(FSOUND_StopSound)                      := GetAddress(FMODHandle, 'FSOUND_StopSound');
  pointer(FSOUND_SetFrequency)                   := GetAddress(FMODHandle, 'FSOUND_SetFrequency');
  pointer(FSOUND_SetVolume)                      := GetAddress(FMODHandle, 'FSOUND_SetVolume');
  pointer(FSOUND_SetVolumeAbsolute)              := GetAddress(FMODHandle, 'FSOUND_SetVolumeAbsolute');
  pointer(FSOUND_SetPan)                         := GetAddress(FMODHandle, 'FSOUND_SetPan');
  pointer(FSOUND_SetSurround)                    := GetAddress(FMODHandle, 'FSOUND_SetSurround');
  pointer(FSOUND_SetMute)                        := GetAddress(FMODHandle, 'FSOUND_SetMute');
  pointer(FSOUND_SetPriority)                    := GetAddress(FMODHandle, 'FSOUND_SetPriority');
  pointer(FSOUND_SetReserved)                    := GetAddress(FMODHandle, 'FSOUND_SetReserved');
  pointer(FSOUND_SetPaused)                      := GetAddress(FMODHandle, 'FSOUND_SetPaused');
  pointer(FSOUND_SetLoopMode)                    := GetAddress(FMODHandle, 'FSOUND_SetLoopMode');
  pointer(FSOUND_SetCurrentPosition)             := GetAddress(FMODHandle, 'FSOUND_SetCurrentPosition');
  pointer(FSOUND_3D_SetAttributes)               := GetAddress(FMODHandle, 'FSOUND_3D_SetAttributes');
  pointer(FSOUND_3D_SetMinMaxDistance)           := GetAddress(FMODHandle, 'FSOUND_3D_SetMinMaxDistance');
  pointer(FSOUND_IsPlaying)                      := GetAddress(FMODHandle, 'FSOUND_IsPlaying');
  pointer(FSOUND_GetFrequency)                   := GetAddress(FMODHandle, 'FSOUND_GetFrequency');
  pointer(FSOUND_GetVolume)                      := GetAddress(FMODHandle, 'FSOUND_GetVolume');
  pointer(FSOUND_GetAmplitude)                   := GetAddress(FMODHandle, 'FSOUND_GetAmplitude');
  pointer(FSOUND_GetPan)                         := GetAddress(FMODHandle, 'FSOUND_GetPan');
  pointer(FSOUND_GetSurround)                    := GetAddress(FMODHandle, 'FSOUND_GetSurround');
  pointer(FSOUND_GetMute)                        := GetAddress(FMODHandle, 'FSOUND_GetMute');
  pointer(FSOUND_GetPriority)                    := GetAddress(FMODHandle, 'FSOUND_GetPriority');
  pointer(FSOUND_GetReserved)                    := GetAddress(FMODHandle, 'FSOUND_GetReserved');
  pointer(FSOUND_GetPaused)                      := GetAddress(FMODHandle, 'FSOUND_GetPaused');
  pointer(FSOUND_GetLoopMode)                    := GetAddress(FMODHandle, 'FSOUND_GetLoopMode');
  pointer(FSOUND_GetCurrentPosition)             := GetAddress(FMODHandle, 'FSOUND_GetCurrentPosition');
  pointer(FSOUND_GetCurrentSample)               := GetAddress(FMODHandle, 'FSOUND_GetCurrentSample');
  pointer(FSOUND_GetCurrentLevels)               := GetAddress(FMODHandle, 'FSOUND_GetCurrentLevels');
  pointer(FSOUND_GetNumSubChannels)              := GetAddress(FMODHandle, 'FSOUND_GetNumSubChannels');
  pointer(FSOUND_GetSubChannel)                  := GetAddress(FMODHandle, 'FSOUND_GetSubChannel');
  pointer(FSOUND_3D_GetAttributes)               := GetAddress(FMODHandle, 'FSOUND_3D_GetAttributes');
  pointer(FSOUND_3D_GetMinMaxDistance)           := GetAddress(FMODHandle, 'FSOUND_3D_GetMinMaxDistance');
  pointer(FSOUND_3D_Listener_SetCurrent)         := GetAddress(FMODHandle, 'FSOUND_3D_Listener_SetCurrent');
  pointer(FSOUND_3D_Listener_SetAttributes)      := GetAddress(FMODHandle, 'FSOUND_3D_Listener_SetAttributes');
  pointer(FSOUND_3D_Listener_GetAttributes)      := GetAddress(FMODHandle, 'FSOUND_3D_Listener_GetAttributes');
  pointer(FSOUND_3D_SetDopplerFactor)            := GetAddress(FMODHandle, 'FSOUND_3D_SetDopplerFactor');
  pointer(FSOUND_3D_SetDistanceFactor)           := GetAddress(FMODHandle, 'FSOUND_3D_SetDistanceFactor');
  pointer(FSOUND_3D_SetRolloffFactor)            := GetAddress(FMODHandle, 'FSOUND_3D_SetRolloffFactor');
  pointer(FSOUND_FX_Enable)                      := GetAddress(FMODHandle, 'FSOUND_FX_Enable');
  pointer(FSOUND_FX_SetChorus)                   := GetAddress(FMODHandle, 'FSOUND_FX_SetChorus');
  pointer(FSOUND_FX_SetCompressor)               := GetAddress(FMODHandle, 'FSOUND_FX_SetCompressor');
  pointer(FSOUND_FX_SetDistortion)               := GetAddress(FMODHandle, 'FSOUND_FX_SetDistortion');
  pointer(FSOUND_FX_SetEcho)                     := GetAddress(FMODHandle, 'FSOUND_FX_SetEcho');
  pointer(FSOUND_FX_SetFlanger)                  := GetAddress(FMODHandle, 'FSOUND_FX_SetFlanger');
  pointer(FSOUND_FX_SetGargle)                   := GetAddress(FMODHandle, 'FSOUND_FX_SetGargle');
  pointer(FSOUND_FX_SetI3DL2Reverb)              := GetAddress(FMODHandle, 'FSOUND_FX_SetI3DL2Reverb');
  pointer(FSOUND_FX_SetParamEQ)                  := GetAddress(FMODHandle, 'FSOUND_FX_SetParamEQ');
  pointer(FSOUND_FX_SetWavesReverb)              := GetAddress(FMODHandle, 'FSOUND_FX_SetWavesReverb');
  pointer(FSOUND_Stream_Open)                    := GetAddress(FMODHandle, 'FSOUND_Stream_Open');
  pointer(FSOUND_Stream_Create)                  := GetAddress(FMODHandle, 'FSOUND_Stream_Create');
  pointer(FSOUND_Stream_Close)                   := GetAddress(FMODHandle, 'FSOUND_Stream_Close');
  pointer(FSOUND_Stream_Play)                    := GetAddress(FMODHandle, 'FSOUND_Stream_Play');
  pointer(FSOUND_Stream_PlayEx)                  := GetAddress(FMODHandle, 'FSOUND_Stream_PlayEx');
  pointer(FSOUND_Stream_Stop)                    := GetAddress(FMODHandle, 'FSOUND_Stream_Stop');
  pointer(FSOUND_Stream_SetEndCallback)          := GetAddress(FMODHandle, 'FSOUND_Stream_SetEndCallback');
  pointer(FSOUND_Stream_SetSyncCallback)         := GetAddress(FMODHandle, 'FSOUND_Stream_SetSyncCallback');
  pointer(FSOUND_Stream_GetSample)               := GetAddress(FMODHandle, 'FSOUND_Stream_GetSample');
  pointer(FSOUND_Stream_CreateDSP)               := GetAddress(FMODHandle, 'FSOUND_Stream_CreateDSP');
  pointer(FSOUND_Stream_SetBufferSize)           := GetAddress(FMODHandle, 'FSOUND_Stream_SetBufferSize');
  pointer(FSOUND_Stream_SetPosition)             := GetAddress(FMODHandle, 'FSOUND_Stream_SetPosition');
  pointer(FSOUND_Stream_GetPosition)             := GetAddress(FMODHandle, 'FSOUND_Stream_GetPosition');
  pointer(FSOUND_Stream_SetTime)                 := GetAddress(FMODHandle, 'FSOUND_Stream_SetTime');
  pointer(FSOUND_Stream_GetTime)                 := GetAddress(FMODHandle, 'FSOUND_Stream_GetTime');
  pointer(FSOUND_Stream_GetLength)               := GetAddress(FMODHandle, 'FSOUND_Stream_GetLength');
  pointer(FSOUND_Stream_GetLengthMs)             := GetAddress(FMODHandle, 'FSOUND_Stream_GetLengthMs');
  pointer(FSOUND_Stream_SetMode)                 := GetAddress(FMODHandle, 'FSOUND_Stream_SetMode');
  pointer(FSOUND_Stream_GetMode)                 := GetAddress(FMODHandle, 'FSOUND_Stream_GetMode');
  pointer(FSOUND_Stream_SetLoopPoints)           := GetAddress(FMODHandle, 'FSOUND_Stream_SetLoopPoints');
  pointer(FSOUND_Stream_SetLoopCount)            := GetAddress(FMODHandle, 'FSOUND_Stream_SetLoopCount');
  pointer(FSOUND_Stream_GetOpenState)            := GetAddress(FMODHandle, 'FSOUND_Stream_GetOpenState');
  pointer(FSOUND_Stream_AddSyncPoint)            := GetAddress(FMODHandle, 'FSOUND_Stream_AddSyncPoint');
  pointer(FSOUND_Stream_DeleteSyncPoint)         := GetAddress(FMODHandle, 'FSOUND_Stream_DeleteSyncPoint');
  pointer(FSOUND_Stream_GetNumSyncPoints)        := GetAddress(FMODHandle, 'FSOUND_Stream_GetNumSyncPoints');
  pointer(FSOUND_Stream_GetSyncPoint)            := GetAddress(FMODHandle, 'FSOUND_Stream_GetSyncPoint');
  pointer(FSOUND_Stream_GetSyncPointInfo)        := GetAddress(FMODHandle, 'FSOUND_Stream_GetSyncPointInfo');
  pointer(FSOUND_Stream_SetSubStream)            := GetAddress(FMODHandle, 'FSOUND_Stream_SetSubStream');
  pointer(FSOUND_Stream_GetNumSubStreams)        := GetAddress(FMODHandle, 'FSOUND_Stream_GetNumSubStreams');
  pointer(FSOUND_Stream_SetSubStreamSentence)    := GetAddress(FMODHandle, 'FSOUND_Stream_SetSubStreamSentence');
  pointer(FSOUND_Stream_GetNumTagFields)         := GetAddress(FMODHandle, 'FSOUND_Stream_GetNumTagFields');
  pointer(FSOUND_Stream_GetTagField)             := GetAddress(FMODHandle, 'FSOUND_Stream_GetTagField');
  pointer(FSOUND_Stream_FindTagField)            := GetAddress(FMODHandle, 'FSOUND_Stream_FindTagField');
  pointer(FSOUND_Stream_Net_SetProxy)            := GetAddress(FMODHandle, 'FSOUND_Stream_Net_SetProxy');
  pointer(FSOUND_Stream_Net_GetLastServerStatus) := GetAddress(FMODHandle, 'FSOUND_Stream_Net_GetLastServerStatus');
  pointer(FSOUND_Stream_Net_SetBufferProperties) := GetAddress(FMODHandle, 'FSOUND_Stream_Net_SetBufferProperties');
  pointer(FSOUND_Stream_Net_GetBufferProperties) := GetAddress(FMODHandle, 'FSOUND_Stream_Net_GetBufferProperties');
  pointer(FSOUND_Stream_Net_SetMetadataCallback) := GetAddress(FMODHandle, 'FSOUND_Stream_Net_SetMetadataCallback');
  pointer(FSOUND_Stream_Net_GetStatus)           := GetAddress(FMODHandle, 'FSOUND_Stream_Net_GetStatus');
  pointer(FSOUND_CD_Play)                        := GetAddress(FMODHandle, 'FSOUND_CD_Play');
  pointer(FSOUND_CD_SetPlayMode)                 := GetAddress(FMODHandle, 'FSOUND_CD_SetPlayMode');
  pointer(FSOUND_CD_Stop)                        := GetAddress(FMODHandle, 'FSOUND_CD_Stop');
  pointer(FSOUND_CD_SetPaused)                   := GetAddress(FMODHandle, 'FSOUND_CD_SetPaused');
  pointer(FSOUND_CD_SetVolume)                   := GetAddress(FMODHandle, 'FSOUND_CD_SetVolume');
  pointer(FSOUND_CD_SetTrackTime)                := GetAddress(FMODHandle, 'FSOUND_CD_SetTrackTime');
  pointer(FSOUND_CD_OpenTray)                    := GetAddress(FMODHandle, 'FSOUND_CD_OpenTray');
  pointer(FSOUND_CD_GetPaused)                   := GetAddress(FMODHandle, 'FSOUND_CD_GetPaused');
  pointer(FSOUND_CD_GetTrack)                    := GetAddress(FMODHandle, 'FSOUND_CD_GetTrack');
  pointer(FSOUND_CD_GetNumTracks)                := GetAddress(FMODHandle, 'FSOUND_CD_GetNumTracks');
  pointer(FSOUND_CD_GetVolume)                   := GetAddress(FMODHandle, 'FSOUND_CD_GetVolume');
  pointer(FSOUND_CD_GetTrackLength)              := GetAddress(FMODHandle, 'FSOUND_CD_GetTrackLength');
  pointer(FSOUND_CD_GetTrackTime)                := GetAddress(FMODHandle, 'FSOUND_CD_GetTrackTime');
  pointer(FSOUND_DSP_Create)                     := GetAddress(FMODHandle, 'FSOUND_DSP_Create');
  pointer(FSOUND_DSP_Free)                       := GetAddress(FMODHandle, 'FSOUND_DSP_Free');
  pointer(FSOUND_DSP_SetPriority)                := GetAddress(FMODHandle, 'FSOUND_DSP_SetPriority');
  pointer(FSOUND_DSP_GetPriority)                := GetAddress(FMODHandle, 'FSOUND_DSP_GetPriority');
  pointer(FSOUND_DSP_SetActive)                  := GetAddress(FMODHandle, 'FSOUND_DSP_SetActive');
  pointer(FSOUND_DSP_GetActive)                  := GetAddress(FMODHandle, 'FSOUND_DSP_GetActive');
  pointer(FSOUND_DSP_GetClearUnit)               := GetAddress(FMODHandle, 'FSOUND_DSP_GetClearUnit');
  pointer(FSOUND_DSP_GetSFXUnit)                 := GetAddress(FMODHandle, 'FSOUND_DSP_GetSFXUnit');
  pointer(FSOUND_DSP_GetMusicUnit)               := GetAddress(FMODHandle, 'FSOUND_DSP_GetMusicUnit');
  pointer(FSOUND_DSP_GetClipAndCopyUnit)         := GetAddress(FMODHandle, 'FSOUND_DSP_GetClipAndCopyUnit');
  pointer(FSOUND_DSP_GetFFTUnit)                 := GetAddress(FMODHandle, 'FSOUND_DSP_GetFFTUnit');
  pointer(FSOUND_DSP_MixBuffers)                 := GetAddress(FMODHandle, 'FSOUND_DSP_MixBuffers');
  pointer(FSOUND_DSP_ClearMixBuffer)             := GetAddress(FMODHandle, 'FSOUND_DSP_ClearMixBuffer');
  pointer(FSOUND_DSP_GetBufferLength)            := GetAddress(FMODHandle, 'FSOUND_DSP_GetBufferLength');
  pointer(FSOUND_DSP_GetBufferLengthTotal)       := GetAddress(FMODHandle, 'FSOUND_DSP_GetBufferLengthTotal');
  pointer(FSOUND_DSP_GetSpectrum)                := GetAddress(FMODHandle, 'FSOUND_DSP_GetSpectrum');
  pointer(FSOUND_Reverb_SetProperties)           := GetAddress(FMODHandle, 'FSOUND_Reverb_SetProperties');
  pointer(FSOUND_Reverb_GetProperties)           := GetAddress(FMODHandle, 'FSOUND_Reverb_GetProperties');
  pointer(FSOUND_Reverb_SetChannelProperties)    := GetAddress(FMODHandle, 'FSOUND_Reverb_SetChannelProperties');
  pointer(FSOUND_Reverb_GetChannelProperties)    := GetAddress(FMODHandle, 'FSOUND_Reverb_GetChannelProperties');
  pointer(FSOUND_Record_SetDriver)               := GetAddress(FMODHandle, 'FSOUND_Record_SetDriver');
  pointer(FSOUND_Record_GetNumDrivers)           := GetAddress(FMODHandle, 'FSOUND_Record_GetNumDrivers');
  pointer(FSOUND_Record_GetDriverName)           := GetAddress(FMODHandle, 'FSOUND_Record_GetDriverName');
  pointer(FSOUND_Record_GetDriver)               := GetAddress(FMODHandle, 'FSOUND_Record_GetDriver');
  pointer(FSOUND_Record_StartSample)             := GetAddress(FMODHandle, 'FSOUND_Record_StartSample');
  pointer(FSOUND_Record_Stop)                    := GetAddress(FMODHandle, 'FSOUND_Record_Stop');
  pointer(FSOUND_Record_GetPosition)             := GetAddress(FMODHandle, 'FSOUND_Record_GetPosition');
  pointer(FSOUND_File_SetCallbacks)              := GetAddress(FMODHandle, 'FSOUND_File_SetCallbacks');
  pointer(FMUSIC_LoadSong)                       := GetAddress(FMODHandle, 'FMUSIC_LoadSong');
  pointer(FMUSIC_LoadSongEx)                     := GetAddress(FMODHandle, 'FMUSIC_LoadSongEx');
  pointer(FMUSIC_GetOpenState)                   := GetAddress(FMODHandle, 'FMUSIC_GetOpenState');
  pointer(FMUSIC_FreeSong)                       := GetAddress(FMODHandle, 'FMUSIC_FreeSong');
  pointer(FMUSIC_PlaySong)                       := GetAddress(FMODHandle, 'FMUSIC_PlaySong');
  pointer(FMUSIC_StopSong)                       := GetAddress(FMODHandle, 'FMUSIC_StopSong');
  pointer(FMUSIC_StopAllSongs)                   := GetAddress(FMODHandle, 'FMUSIC_StopAllSongs');
  pointer(FMUSIC_SetZxxCallback)                 := GetAddress(FMODHandle, 'FMUSIC_SetZxxCallback');
  pointer(FMUSIC_SetRowCallback)                 := GetAddress(FMODHandle, 'FMUSIC_SetRowCallback');
  pointer(FMUSIC_SetOrderCallback)               := GetAddress(FMODHandle, 'FMUSIC_SetOrderCallback');
  pointer(FMUSIC_SetInstCallback)                := GetAddress(FMODHandle, 'FMUSIC_SetInstCallback');
  pointer(FMUSIC_SetSample)                      := GetAddress(FMODHandle, 'FMUSIC_SetSample');
  pointer(FMUSIC_SetUserData)                    := GetAddress(FMODHandle, 'FMUSIC_SetUserData');
  pointer(FMUSIC_OptimizeChannels)               := GetAddress(FMODHandle, 'FMUSIC_OptimizeChannels');
  pointer(FMUSIC_SetReverb)                      := GetAddress(FMODHandle, 'FMUSIC_SetReverb');
  pointer(FMUSIC_SetLooping)                     := GetAddress(FMODHandle, 'FMUSIC_SetLooping');
  pointer(FMUSIC_SetOrder)                       := GetAddress(FMODHandle, 'FMUSIC_SetOrder');
  pointer(FMUSIC_SetPaused)                      := GetAddress(FMODHandle, 'FMUSIC_SetPaused');
  pointer(FMUSIC_SetMasterVolume)                := GetAddress(FMODHandle, 'FMUSIC_SetMasterVolume');
  pointer(FMUSIC_SetMasterSpeed)                 := GetAddress(FMODHandle, 'FMUSIC_SetMasterSpeed');
  pointer(FMUSIC_SetPanSeperation)               := GetAddress(FMODHandle, 'FMUSIC_SetPanSeperation');
  pointer(FMUSIC_GetName)                        := GetAddress(FMODHandle, 'FMUSIC_GetName');
  pointer(FMUSIC_GetType)                        := GetAddress(FMODHandle, 'FMUSIC_GetType');
  pointer(FMUSIC_GetNumOrders)                   := GetAddress(FMODHandle, 'FMUSIC_GetNumOrders');
  pointer(FMUSIC_GetNumPatterns)                 := GetAddress(FMODHandle, 'FMUSIC_GetNumPatterns');
  pointer(FMUSIC_GetNumInstruments)              := GetAddress(FMODHandle, 'FMUSIC_GetNumInstruments');
  pointer(FMUSIC_GetNumSamples)                  := GetAddress(FMODHandle, 'FMUSIC_GetNumSamples');
  pointer(FMUSIC_GetNumChannels)                 := GetAddress(FMODHandle, 'FMUSIC_GetNumChannels');
  pointer(FMUSIC_GetSample)                      := GetAddress(FMODHandle, 'FMUSIC_GetSample');
  pointer(FMUSIC_GetPatternLength)               := GetAddress(FMODHandle, 'FMUSIC_GetPatternLength');
  pointer(FMUSIC_IsFinished)                     := GetAddress(FMODHandle, 'FMUSIC_IsFinished');
  pointer(FMUSIC_IsPlaying)                      := GetAddress(FMODHandle, 'FMUSIC_IsPlaying');
  pointer(FMUSIC_GetMasterVolume)                := GetAddress(FMODHandle, 'FMUSIC_GetMasterVolume');
  pointer(FMUSIC_GetGlobalVolume)                := GetAddress(FMODHandle, 'FMUSIC_GetGlobalVolume');
  pointer(FMUSIC_GetOrder)                       := GetAddress(FMODHandle, 'FMUSIC_GetOrder');
  pointer(FMUSIC_GetPattern)                     := GetAddress(FMODHandle, 'FMUSIC_GetPattern');
  pointer(FMUSIC_GetSpeed)                       := GetAddress(FMODHandle, 'FMUSIC_GetSpeed');
  pointer(FMUSIC_GetBPM)                         := GetAddress(FMODHandle, 'FMUSIC_GetBPM');
  pointer(FMUSIC_GetRow)                         := GetAddress(FMODHandle, 'FMUSIC_GetRow');
  pointer(FMUSIC_GetPaused)                      := GetAddress(FMODHandle, 'FMUSIC_GetPaused');
  pointer(FMUSIC_GetTime)                        := GetAddress(FMODHandle, 'FMUSIC_GetTime');
  pointer(FMUSIC_GetRealChannel)                 := GetAddress(FMODHandle, 'FMUSIC_GetRealChannel');
  pointer(FMUSIC_GetUserData)                    := GetAddress(FMODHandle, 'FMUSIC_GetUserData');

{$ENDIF}
  Result := True;
end;

procedure FMOD_Unload;
begin
  { Only free the library if it was already loaded }
  if FMODHandle <> INVALID_MODULEHANDLE_VALUE then
{$IFDEF WINDOWS}
    FreeLibrary(FMODHandle);
{$ELSE}
    dlclose(FMODHandle);
{$ENDIF}
  FMODHandle := INVALID_MODULEHANDLE_VALUE;
end;

var
  Saved8087CW: Word;

{{$ifdef FPC} //FPC do not have this function in its RTL
const
  Default8087CW = $1332; //according to the FPC site it's the value used in the
                         //startup code.
procedure Set8087CW( value :word ); Assembler;
asm
   FLDCW  value
end;
{$endif}
 }
initialization
  FMODHandle := INVALID_MODULEHANDLE_VALUE;

  { Save the current FPU state and then disable FPU exceptions }
  Saved8087CW := Default8087CW;
  Set8087CW($133f); { Disable all fpu exceptions }

finalization
  { Make sure the library is unloaded }
  FMOD_Unload;

  { Reset the FPU to the previous state }
  Set8087CW(Saved8087CW);
end.
