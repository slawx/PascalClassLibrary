unit CfgMgr;

{$MODE Delphi}

interface

uses
  LCLIntf{$IFDEF Windows}, Windows{$ENDIF};

{$IFDEF Windows}
const
// Flags controlling what is included in the device information set built
// by SetupDiGetClassDevs
  DIGCF_DEFAULT =         $00000001;  // only valid with DIGCF_DEVICEINTERFACE
  DIGCF_PRESENT =         $00000002;
  DIGCF_ALLCLASSES =      $00000004;
  DIGCF_PROFILE =         $00000008;
  DIGCF_DEVICEINTERFACE = $00000010;

// Device registry property codes
// (Codes marked as read-only (R) may only be used for
// SetupDiGetDeviceRegistryProperty)
  SPDRP_DEVICEDESC                  = $00000000;  // DeviceDesc (R/W)
  SPDRP_HARDWAREID                  = $00000001;  // HardwareID (R/W)
  SPDRP_COMPATIBLEIDS               = $00000002;  // CompatibleIDs (R/W)
  SPDRP_UNUSED0                     = $00000003;  // unused
  SPDRP_SERVICE                     = $00000004;  // Service (R/W)
  SPDRP_UNUSED1                     = $00000005;  // unused
  SPDRP_UNUSED2                     = $00000006;  // unused
  SPDRP_CLASS                       = $00000007;  // Class (R--tied to ClassGUID)
  SPDRP_CLASSGUID                   = $00000008;  // ClassGUID (R/W)
  SPDRP_DRIVER                      = $00000009;  // Driver (R/W)
  SPDRP_CONFIGFLAGS                 = $0000000A;  // ConfigFlags (R/W)
  SPDRP_MFG                         = $0000000B;  // Mfg (R/W)
  SPDRP_FRIENDLYNAME                = $0000000C;  // FriendlyName (R/W)
  SPDRP_LOCATION_INFORMATION        = $0000000D;  // LocationInformation (R/W)
  SPDRP_PHYSICAL_DEVICE_OBJECT_NAME = $0000000E;  // PhysicalDeviceObjectName (R)
  SPDRP_CAPABILITIES                = $0000000F;  // Capabilities (R)
  SPDRP_UI_NUMBER                   = $00000010;  // UiNumber (R)
  SPDRP_UPPERFILTERS                = $00000011;  // UpperFilters (R/W)
  SPDRP_LOWERFILTERS                = $00000012;  // LowerFilters (R/W)
  SPDRP_BUSTYPEGUID                 = $00000013;  // BusTypeGUID (R)
  SPDRP_LEGACYBUSTYPE               = $00000014;  // LegacyBusType (R)
  SPDRP_BUSNUMBER                   = $00000015;  // BusNumber (R)
  SPDRP_ENUMERATOR_NAME             = $00000016;  // Enumerator Name (R)
  SPDRP_SECURITY                    = $00000017;  // Security (R/W, binary form)
  SPDRP_SECURITY_SDS                = $00000018;  // Security (W, SDS form)
  SPDRP_DEVTYPE                     = $00000019;  // Device Type (R/W)
  SPDRP_EXCLUSIVE                   = $0000001A;  // Device is exclusive-access (R/W)
  SPDRP_CHARACTERISTICS             = $0000001B;  // Device Characteristics (R/W)
  SPDRP_ADDRESS                     = $0000001C;  // Device Address (R)
  SPDRP_UI_NUMBER_DESC_FORMAT       = $0000001E;  // UiNumberDescFormat (R/W)
  SPDRP_MAXIMUM_PROPERTY            = $0000001F;  // Upper bound on ordinals

//
// Configuration Manager return status codes
//
  CR_SUCCESS                  = $00000000;
  CR_DEFAULT                  = $00000001;
  CR_OUT_OF_MEMORY            = $00000002;
  CR_INVALID_POINTER          = $00000003;
  CR_INVALID_FLAG             = $00000004;
  CR_INVALID_DEVNODE          = $00000005;
  CR_INVALID_DEVINST          = CR_INVALID_DEVNODE;
  CR_INVALID_RES_DES          = $00000006;
  CR_INVALID_LOG_CONF         = $00000007;
  CR_INVALID_ARBITRATOR       = $00000008;
  CR_INVALID_NODELIST         = $00000009;
  CR_DEVNODE_HAS_REQS         = $0000000A;
  CR_DEVINST_HAS_REQS         = CR_DEVNODE_HAS_REQS;
  CR_INVALID_RESOURCEID       = $0000000B;
  CR_DLVXD_NOT_FOUND          = $0000000C;   // WIN 95 ONLY
  CR_NO_SUCH_DEVNODE          = $0000000D;
  CR_NO_SUCH_DEVINST          = CR_NO_SUCH_DEVNODE;
  CR_NO_MORE_LOG_CONF         = $0000000E;
  CR_NO_MORE_RES_DES          = $0000000F;
  CR_ALREADY_SUCH_DEVNODE     = $00000010;
  CR_ALREADY_SUCH_DEVINST     = CR_ALREADY_SUCH_DEVNODE;
  CR_INVALID_RANGE_LIST       = $00000011;
  CR_INVALID_RANGE            = $00000012;
  CR_FAILURE                  = $00000013;
  CR_NO_SUCH_LOGICAL_DEV      = $00000014;
  CR_CREATE_BLOCKED           = $00000015;
  CR_NOT_SYSTEM_VM            = $00000016;   // WIN 95 ONLY
  CR_REMOVE_VETOED            = $00000017;
  CR_APM_VETOED               = $00000018;
  CR_INVALID_LOAD_TYPE        = $00000019;
  CR_BUFFER_SMALL             = $0000001A;
  CR_NO_ARBITRATOR            = $0000001B;
  CR_NO_REGISTRY_HANDLE       = $0000001C;
  CR_REGISTRY_ERROR           = $0000001D;
  CR_INVALID_DEVICE_ID        = $0000001E;
  CR_INVALID_DATA             = $0000001F;
  CR_INVALID_API              = $00000020;
  CR_DEVLOADER_NOT_READY      = $00000021;
  CR_NEED_RESTART             = $00000022;
  CR_NO_MORE_HW_PROFILES      = $00000023;
  CR_DEVICE_NOT_THERE         = $00000024;
  CR_NO_SUCH_VALUE            = $00000025;
  CR_WRONG_TYPE               = $00000026;
  CR_INVALID_PRIORITY         = $00000027;
  CR_NOT_DISABLEABLE          = $00000028;
  CR_FREE_RESOURCES           = $00000029;
  CR_QUERY_VETOED             = $0000002A;
  CR_CANT_SHARE_IRQ           = $0000002B;
  CR_NO_DEPENDENT             = $0000002C;
  CR_SAME_RESOURCES           = $0000002D;
  CR_NO_SUCH_REGISTRY_KEY     = $0000002E;
  CR_INVALID_MACHINENAME      = $0000002F;   // NT ONLY
  CR_REMOTE_COMM_FAILURE      = $00000030;   // NT ONLY
  CR_MACHINE_UNAVAILABLE      = $00000031;   // NT ONLY
  CR_NO_CM_SERVICES           = $00000032;   // NT ONLY
  CR_ACCESS_DENIED            = $00000033;   // NT ONLY
  CR_CALL_NOT_IMPLEMENTED     = $00000034;
  CR_INVALID_PROPERTY         = $00000035;
  CR_DEVICE_INTERFACE_ACTIVE  = $00000036;
  CR_NO_SUCH_DEVICE_INTERFACE = $00000037;
  CR_INVALID_REFERENCE_STRING = $00000038;
  CR_INVALID_CONFLICT_LIST    = $00000039;
  CR_INVALID_INDEX            = $0000003A;
  CR_INVALID_STRUCTURE_SIZE   = $0000003B;
  NUM_CR_RESULTS              = $0000003C;

//
// Logical Config Flags (specified in call to CM_Get_First_Log_Conf
//
  BASIC_LOG_CONF    = $00000000;  // Specifies the req list.
  FILTERED_LOG_CONF = $00000001;  // Specifies the filtered req list.
  ALLOC_LOG_CONF    = $00000002;  // Specifies the Alloc Element.
  BOOT_LOG_CONF     = $00000003;  // Specifies the RM Alloc Element.
  FORCED_LOG_CONF   = $00000004;  // Specifies the Forced Log Conf
  OVERRIDE_LOG_CONF = $00000005;  // Specifies the Override req list.
  NUM_LOG_CONF      = $00000006;  // Number of Log Conf type
  LOG_CONF_BITS     = $00000007;  // The bits of the log conf type.

//
// Resource types
//
  ResType_All           = $00000000;   // Return all resource types
  ResType_None          = $00000000;   // Arbitration always succeeded
  ResType_Mem           = $00000001;   // Physical address resource
  ResType_IO            = $00000002;   // Physical I/O address resource
  ResType_DMA           = $00000003;   // DMA channels resource
  ResType_IRQ           = $00000004;   // IRQ resource
  ResType_DoNotUse      = $00000005;   // Used as spacer to sync subsequent ResTypes w/NT
  ResType_BusNumber     = $00000006;   // bus number resource
  ResType_MAX           = $00000006;   // Maximum known (arbitrated) ResType
  ResType_Ignored_Bit   = $00008000;   // Ignore this resource
  ResType_ClassSpecific = $0000FFFF;   // class-specific resource
  ResType_Reserved      = $00008000;   // reserved for internal use
  ResType_DevicePrivate = $00008001;   // device private data
  ResType_PcCardConfig  = $00008002;   // PC Card configuration data
  ResType_MfCardConfig  = $00008003;   // MF Card configuration data


type
  HDEVINFO = DWORD;
  SP_DEVINFO_DATA = packed record
    cbSize:DWORD;
    ClassGuid:TGUID;
    DevInst:DWORD;
    Reserved:pointer;
  end;
  PSP_DEVINFO_DATA = ^SP_DEVINFO_DATA;
  DEVINST = DWORD;
  PDEVINST = ^DEVINST;
  CONFIGRET = DWORD;
  LOG_CONF = DWORD;
  PLOG_CONF = ^LOG_CONF;
  RES_DES = DWORD;
  PRES_DES = ^RES_DES;
  RESOURCEID = ULONG;
  PRESOURCEID = ^RESOURCEID;
  IO_DES = packed record
    IOD_Count:DWORD;          // number of IO_RANGE structs in IO_RESOURCE
    IOD_Type:DWORD;           // size (in bytes) of IO_RANGE (IOType_Range)
    IOD_Alloc_Base:LONGLONG;  // base of allocated port range
    IOD_Alloc_End:LONGLONG;   // end of allocated port range
    IOD_DesFlags:DWORD;       // flags relating to allocated port range
  end;
  PIO_DES = ^IO_DES;
  IO_RANGE = packed record
    IOR_Align:LONGLONG;       // mask for base alignment
    IOR_nPorts:DWORD;         // number of ports
    IOR_Min:LONGLONG;         // minimum port address
    IOR_Max:LONGLONG;         // maximum port address
    IOR_RangeFlags:DWORD;     // flags for this port range
    IOR_Alias:LONGLONG;       // multiplier that generates aliases for port(s)
  end;
  PIO_RANGE = ^IO_RANGE;
  IO_RESOURCE = packed record
    IO_Header:IO_DES;                // info about I/O port range list
    IO_Data:array[0..0] of IO_RANGE; // list of I/O port ranges
  end;
  PIO_RESOURCE = ^IO_RESOURCE;

function SetupDiGetClassDevs(
  ClassGuid:PGUID;
  Enumerator:PChar;
  hwndParent:HWND;
  Flags:DWORD):HDEVINFO; stdcall;

function SetupDiEnumDeviceInfo(
  DeviceInfoSet:HDEVINFO;
  MemberIndex:DWORD;
  DeviceInfoData:PSP_DEVINFO_DATA):BOOL; stdcall;

function SetupDiGetDeviceRegistryProperty(
  DeviceInfoSet:HDEVINFO;
  DeviceInfoData:PSP_DEVINFO_DATA;
  PropertyId:DWORD;
  PropertyRegDataType:PDWORD;
  PropertyBuffer:pointer;
  PropertyBufferSize:DWORD;
  RequiredSize:PDWORD):BOOL; stdcall;

function SetupDiDestroyDeviceInfoList(
  DeviceInfoSet:HDEVINFO):BOOL; stdcall;

function CM_Get_DevNode_Status(
  pulStatus:PULONG;
  pulProblemNumber:PULONG;
  dnDevInst:DEVINST;
  ulFlags:ULONG):CONFIGRET; stdcall;

function CM_Get_First_Log_Conf(
  plcLogConf:PLOG_CONF;
  dnDevInst:DEVINST;
  ulFlags:ULONG):CONFIGRET; stdcall;

function CM_Get_Next_Res_Des(
  prdResDes:PRES_DES;
  rdResDes:RES_DES;
  ForResource:RESOURCEID;
  pResourceID:PRESOURCEID;
  ulFlags:ULONG):CONFIGRET; stdcall;

function CM_Get_Res_Des_Data(
  rdResDes:RES_DES;
  Buffer:pointer;
  BufferLen:ULONG;
  ulFlags:ULONG):CONFIGRET; stdcall;

function CM_Free_Log_Conf_Handle(
  lcLogConf:LOG_CONF):CONFIGRET; stdcall;

{$ENDIF}

implementation

{$IFDEF Windows}

const
  setupapi = 'setupapi.dll';
  cfgmgr32 = 'cfgmgr32.dll';

function SetupDiGetClassDevs;              external setupapi name 'SetupDiGetClassDevsA';
function SetupDiEnumDeviceInfo;            external setupapi name 'SetupDiEnumDeviceInfo';
function SetupDiGetDeviceRegistryProperty; external setupapi name 'SetupDiGetDeviceRegistryPropertyA';
function SetupDiDestroyDeviceInfoList;     external setupapi name 'SetupDiDestroyDeviceInfoList';

function CM_Get_DevNode_Status;            external cfgmgr32 name 'CM_Get_DevNode_Status';
function CM_Get_First_Log_Conf;            external cfgmgr32 name 'CM_Get_First_Log_Conf';
function CM_Get_Next_Res_Des;              external cfgmgr32 name 'CM_Get_Next_Res_Des';
function CM_Get_Res_Des_Data;              external cfgmgr32 name 'CM_Get_Res_Des_Data';
function CM_Free_Log_Conf_Handle;          external cfgmgr32 name 'CM_Free_Log_Conf_Handle';

{$ENDIF}

end.