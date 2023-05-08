{$mode objfpc}{$H+}
{$macro on}

interface

uses
  Classes, SysUtils;

{$ifdef MSWINDOWS}

type
  PCWChar = PWideChar;
  TCWChar = WideChar;

{$else}

type
  PCWChar = ^TCWChar;
  TCWChar = UCS4Char;  // wchar_t of size 4 (NOT ON WINDOWS!)

{$endif}

TCWCharArray = array of TCWChar;

type

  { THidDevice }

  PHidDevice = ^THidDevice;

  THidDevice = object
    function Write(const Data; Length: SizeInt): SizeInt;
    function Read(out Data; Length: SizeInt): SizeInt;
    function ReadTimeout(out Data; Length: SizeInt; Millis: Integer): SizeInt;
    function SetNonBlocking(NonBlock: Integer): Integer;
    function GetInputReport(const Data; Length: SizeInt): SizeInt;
    function SendFeatureReport(const Data; Length: SizeInt): SizeInt;
    function GetFeatureReport(out Data; Length: SizeInt): SizeInt;
    function GetManufacturerString: UnicodeString;
    function GetProductString: UnicodeString;
    function GetSerialNumberString: UnicodeString;
    function GetIndexedString(Index: Integer): UnicodeString;
    function GetError: UnicodeString;
    function GetContainerID: TGuid;
    procedure Close;
    function Open(VID: Word;PID: Word;const SerialNumber: {$ifdef MSWINDOWS}String{$else}UnicodeString{$endif}): PHidDevice; static;
    function OpenPath(const DevicePath: String): PHidDevice; static;
  end;

  { THidDeviceInfo }

  PHidDeviceInfo = ^THidDeviceInfo;

  THidDeviceInfo = object
    Path: PChar;
    VendorID: Word;
    ProductID: Word;
    SerialNumber: PCWChar;
    ReleaseNumber: Word;
    ManufacturerString: PCWChar;
    ProductString: PCWChar;
    UsagePage: Word;
    Usage: Word;
    InterfaceNumber: Integer;
    Next: PHidDeviceInfo;
    function Enumerate(VID: Word; PID: Word): PHidDeviceInfo; static;
    procedure Free;
  end;

  function HidInit(const ALibraryName: String = ''): Integer;
  function HidExit: Integer;
  function HidApiVersion: String;

  function PCWCharToUnicodeString(P: PCWChar): UnicodeString;


implementation

{$ifdef HIDAPI_LOAD_DYNAMICALLY}
uses
  dynlibs;
{$endif}


{$ifdef HIDAPI_LOAD_DYNAMICALLY}
  {$define D}
  {$define HIDAPI_VERSION := 0000}
{$else}
  {$define S}
{
  HidApi Version          HIDAPI_VERSION
  0.9.0                   0090
  0.10.0                  0100
  0.11.2                  0112
  0.12.0                  0120
}
  {$ifndef HIDAPI_VERSION}
    {$warning For static loading you need to define the version of the HidApi library. }
    {$warning i.e. for 0.9.0 use HIDAPI_VERSION = 0090 or for 0.10.0 HIDAPI_VERSION = 0100 etc. }
    {$warning else static loading defaults to 0.9.0 hidapi version library .}
     {$define HIDAPI_VERSION := 0090}
  {$endif}
{$endif}

const
{$ifdef MSWINDOWS}

{$if FPC_FULLVERSION > 30200}
  {$define HIDAPI_CALL := WINAPI }
{$else}
  {$define HIDAPI_CALL := stdcall }
{$endif}

  LIBHIDAPI = 'hidapi.dll';

{$endif}

{$ifdef UNIX}

  {$define HIDAPI_CALL := cdecl }

  {$ifdef LINUX}
      LIBHIDAPI = 'libhidapi-libusb.so';
  {$endif}
  {$ifdef BSD}
    {$ifdef FREEBSD}
      LIBHIDAPI = 'libhidapi.so';
      {$linklib libhidapi}
    {$endif}
    {$ifdef DARWIN}
      LIBHIDAPI = 'hidapi';
      {$linklib hidapi}
    {$endif}
  {$endif}

{$endif}

{$ifdef S}function{$else}var{$endif}hid_init{$ifdef D}: function{$endif}: Integer; HIDAPI_CALL; {$ifdef S}external LIBHIDAPI;{$endif}
{$ifdef S}function{$else}var{$endif}hid_exit{$ifdef D}: function{$endif}: Integer; HIDAPI_CALL; {$ifdef S}external LIBHIDAPI;{$endif}
{$ifdef S}function{$else}var{$endif}hid_enumerate{$ifdef D}: function{$endif}(vendor_id: Word; product_id: Word): PHidDeviceInfo; HIDAPI_CALL; {$ifdef S}external LIBHIDAPI;{$endif}
{$ifdef S}procedure{$else}var{$endif}hid_free_enumeration{$ifdef D}: procedure{$endif}(devs: PHidDeviceInfo); HIDAPI_CALL; {$ifdef S}external LIBHIDAPI;{$endif}
{$ifdef S}function{$else}var{$endif}hid_open{$ifdef D}: function{$endif}(vendor_id: Word; product_id: Word; serial_number: PCWChar): PHidDevice; HIDAPI_CALL; {$ifdef S}external LIBHIDAPI;{$endif}
{$ifdef S}function{$else}var{$endif}hid_open_path{$ifdef D}: function{$endif}(path: PChar): PHidDevice; HIDAPI_CALL; {$ifdef S}external LIBHIDAPI;{$endif}
{$ifdef S}function{$else}var{$endif}hid_write{$ifdef D}: function{$endif}(device: PHidDevice; Data: Pointer; length: SizeInt): Integer; HIDAPI_CALL; {$ifdef S}external LIBHIDAPI;{$endif}
{$ifdef S}function{$else}var{$endif}hid_read_timeout{$ifdef D}: function{$endif}(device: PHidDevice; Data: Pointer; length: SizeInt; millisec: Integer): Integer; HIDAPI_CALL; {$ifdef S}external LIBHIDAPI;{$endif}
{$ifdef S}function{$else}var{$endif}hid_read{$ifdef D}: function{$endif}(device: PHidDevice; Data: Pointer; length: SizeInt): Integer; HIDAPI_CALL; {$ifdef S}external LIBHIDAPI;{$endif}
{$ifdef S}function{$else}var{$endif}hid_set_nonblocking{$ifdef D}: function{$endif}(device: PHidDevice; nonblock: Integer): Integer; HIDAPI_CALL; {$ifdef S}external LIBHIDAPI;{$endif}
{$ifdef S}function{$else}var{$endif}hid_send_feature_report{$ifdef D}: function{$endif}(device: PHidDevice; Data: Pointer; length: SizeInt): Integer; HIDAPI_CALL; {$ifdef S}external LIBHIDAPI;{$endif}
{$ifdef S}function{$else}var{$endif}hid_get_feature_report{$ifdef D}: function{$endif}(device: PHidDevice; Data: Pointer; length: SizeInt): Integer; HIDAPI_CALL; {$ifdef S}external LIBHIDAPI;{$endif}
{$if (HIDAPI_VERSION > 0090) or defined(D)}
{$ifdef S}function{$else}var{$endif}hid_get_input_report{$ifdef D}: function{$endif}(device: PHidDevice; Data: Pointer; length: SizeInt): Integer; HIDAPI_CALL; {$ifdef S}external LIBHIDAPI;{$endif}
{$endif}
{$ifdef S}procedure{$else}var{$endif}hid_close{$ifdef D}: procedure{$endif}(device: PHidDevice); HIDAPI_CALL; {$ifdef S}external LIBHIDAPI;{$endif}
{$ifdef S}function{$else}var{$endif}hid_get_manufacturer_string{$ifdef D}: function{$endif}(device: PHidDevice; str: PCWChar; maxlen: SizeInt): Integer; HIDAPI_CALL; {$ifdef S}external LIBHIDAPI;{$endif}
{$ifdef S}function{$else}var{$endif}hid_get_product_string{$ifdef D}: function{$endif}(device: PHidDevice; str: PCWChar; maxlen: SizeInt): Integer; HIDAPI_CALL; {$ifdef S}external LIBHIDAPI;{$endif}
{$ifdef S}function{$else}var{$endif}hid_get_serial_number_string{$ifdef D}: function{$endif}(device: PHidDevice; str: PCWChar; maxlen: SizeInt): Integer; HIDAPI_CALL; {$ifdef S}external LIBHIDAPI;{$endif}
{$ifdef S}function{$else}var{$endif}hid_get_indexed_string{$ifdef D}: function{$endif}(device: PHidDevice; string_index: Integer; str: PCWChar; maxlen: SizeInt): Integer; HIDAPI_CALL; {$ifdef S}external LIBHIDAPI;{$endif}
{$ifdef S}function{$else}var{$endif}hid_error{$ifdef D}: function{$endif}(device: PHidDevice): PCWChar; HIDAPI_CALL; {$ifdef S}external LIBHIDAPI;{$endif}
{$if (HIDAPI_VERSION > 0090) or defined(D)}
{$ifdef S}function{$else}var{$endif}hid_version_str{$ifdef D}: function{$endif}: PChar; HIDAPI_CALL; {$ifdef S}external LIBHIDAPI;{$endif}
{$endif}
{$if (HIDAPI_VERSION > 0120) or defined(D)}
{$ifdef S}function{$else}var{$endif}hid_winapi_get_container_id{$ifdef D}: function{$endif}(device: PHidDevice; container_id: Pointer): Integer; HIDAPI_CALL; {$ifdef S}external LIBHIDAPI;{$endif}
{$endif}

{$ifdef HIDAPI_LOAD_DYNAMICALLY}

ResourceString
  SErrLoadFailed     = 'Can not load hidapi library "%s". Check your installation.';
  SErrAlreadyLoaded  = 'HidApi library %s already loaded.';

var
  HidApiLibraryRefCount: byte;
  HidApiLibraryHandle: TLibHandle;
  HidApiLoadedLibrary: String = '';

function TryInitializeHidApi(LibraryName: string): byte;
begin
  Result := 0;
  if HidApiLibraryRefCount = 0 then
  begin
    HidApiLibraryHandle := SafeLoadLibrary(LibraryName);
    if HidApiLibraryHandle = NilHandle then
      Exit;
    Inc(HidApiLibraryRefCount);
    HidApiLoadedLibrary := LibraryName;
    Pointer(hid_init)                     := GetProcAddress(HidApiLibraryHandle, 'hid_init');
    Pointer(hid_exit)                     := GetProcAddress(HidApiLibraryHandle, 'hid_exit');
    Pointer(hid_enumerate)                := GetProcAddress(HidApiLibraryHandle, 'hid_enumerate');
    Pointer(hid_free_enumeration)         := GetProcAddress(HidApiLibraryHandle, 'hid_free_enumeration');
    Pointer(hid_open)                     := GetProcAddress(HidApiLibraryHandle, 'hid_open');
    Pointer(hid_open_path)                := GetProcAddress(HidApiLibraryHandle, 'hid_open_path');
    Pointer(hid_write)                    := GetProcAddress(HidApiLibraryHandle, 'hid_write');
    Pointer(hid_read_timeout)             := GetProcAddress(HidApiLibraryHandle, 'hid_read_timeout');
    Pointer(hid_read)                     := GetProcAddress(HidApiLibraryHandle, 'hid_read');
    Pointer(hid_set_nonblocking)          := GetProcAddress(HidApiLibraryHandle, 'hid_set_nonblocking');
    Pointer(hid_send_feature_report)      := GetProcAddress(HidApiLibraryHandle, 'hid_send_feature_report');
    Pointer(hid_get_feature_report)       := GetProcAddress(HidApiLibraryHandle, 'hid_get_feature_report');
    Pointer(hid_get_input_report)         := GetProcAddress(HidApiLibraryHandle, 'hid_get_input_report');
    Pointer(hid_close)                    := GetProcAddress(HidApiLibraryHandle, 'hid_close');
    Pointer(hid_get_manufacturer_string)  := GetProcAddress(HidApiLibraryHandle, 'hid_get_manufacturer_string');
    Pointer(hid_get_product_string)       := GetProcAddress(HidApiLibraryHandle, 'hid_get_product_string');
    Pointer(hid_get_serial_number_string) := GetProcAddress(HidApiLibraryHandle, 'hid_get_serial_number_string');
    Pointer(hid_get_indexed_string)       := GetProcAddress(HidApiLibraryHandle, 'hid_get_indexed_string');
    Pointer(hid_error)                    := GetProcAddress(HidApiLibraryHandle, 'hid_error');
    Pointer(hid_version_str)              := GetProcAddress(HidApiLibraryHandle, 'hid_version_str');
    Pointer(hid_winapi_get_container_id)  := GetProcAddress(HidApiLibraryHandle, 'hid_winapi_get_container_id');
  end
  else
    Inc(HidApiLibraryRefCount);
  Result := HidApiLibraryRefCount;
end;

function InitializeHidApi(LibraryName: String = ''): byte;
begin
  Result := TryInitializeHidApi(LibraryName);
  if Result = 0 then
    Raise EInOutError.CreateFmt(SErrLoadFailed,[LibraryName])
  else if (LibraryName <> HidApiLoadedLibrary) then
  begin
    Dec(HidApiLibraryRefCount);
    Result := HidApiLibraryRefCount;
    Raise EInOUtError.CreateFmt(SErrAlreadyLoaded,[HidApiLoadedLibrary]);
  end;
end;

function ReleaseHidApi: byte;
begin
  if HidApiLibraryRefCount > 1 then
     Dec(HidApiLibraryRefCount)
  else if UnloadLibrary(HidApiLibraryHandle) then
  begin
    Dec(HidApiLibraryRefCount);
    HidApiLibraryHandle := NilHandle;
    HidApiLoadedLibrary := '';
  end;
  Result := HidApiLibraryRefCount;
end;

{$endif}
{ helper functions for dealing with widechar strings }

function PCWCharToUnicodeString(P: PCWChar): UnicodeString;
{$ifdef MSWINDOWS}
{$else}
var
  L: Integer;
  WS: array of TCWChar;
{$endif}

begin
{$ifdef MSWINDOWS}
  Result := P;
{$else}
  if not Assigned(P) then
    exit('');

  // strlen
  L := 0;
  while P[L] <> 0 do begin
    Inc(L);
  end;

  // make a copy including the terminating zero
  Inc(L);
  SetLength(WS, L);
  Move(P^, WS[0], L * SizeOf(TCWChar));

  // for 4-Byte chars we can convert with
  // the existing UCS4 function.
  // NOT SO ON WINDOWS!
  Result := UCS4StringToUnicodeString(WS);
{$endif}
end;

{$ifdef MSWINDOWS}
{$else}
function UnicodeStringToTCWCharNullterminated(S: UnicodeString): TCWCharArray;
begin
  // the chars are of size 4, so we
  // can use the UCS4 functions
  // NOT SO ON WINDOWS!
  Result := UnicodeStringToUCS4String(S);
end;
{$endif}

function HidApiVersion: string;
begin
  Result := '0.9.0_or_older';
{$if (HIDAPI_VERSION > 0090) and defined(S)}
  Result := hid_version_str();
{$elseif defined(D)}
  if Pointer(hid_version_str) <> nil then
    Result := hid_version_str();
{$endif}
end;

{ Initialize and deinitialize the HIDAPI }

function HidInit(const ALibraryName: String = ''): Integer;
begin
{$ifdef HIDAPI_LOAD_DYNAMICALLY}
  if ALibraryName <> '' then
    InitializeHidApi(LIBHIDAPI)
  else
    InitializeHidApi(LIBHIDAPI);
{$endif}
  Result :=  hid_init();
end;

function HidExit: Integer;
begin
  Result :=  hid_exit();
{$ifdef HIDAPI_LOAD_DYNAMICALLY}
  ReleaseHidApi();
{$endif}
end;

{ THidDeviceInfo }

function THidDeviceInfo.Enumerate(VID: Word; PID: Word): PHidDeviceInfo;
begin
  Result :=  hid_enumerate(VID, PID);
end;

procedure THidDeviceInfo.Free;
begin
  hid_free_enumeration(@Self);
end;

{ THidDevice }

function THidDevice.Write(const Data; Length: SizeInt): SizeInt;
begin
  Result := hid_write(@self, @Data, Length);
end;

function THidDevice.Read(out Data; Length: SizeInt): SizeInt;
begin
  Result :=  hid_read(@Self, @Data, Length);
end;

function THidDevice.ReadTimeout(out Data; Length: SizeInt; Millis: Integer): SizeInt;
begin
  Result :=  hid_read_timeout(@Self, @Data, Length, Millis);
end;

function THidDevice.SetNonBlocking(NonBlock: Integer): Integer;
begin
  Result := hid_set_nonblocking(@Self, NonBlock);
end;

function THidDevice.GetInputReport(const Data; Length: SizeInt): SizeInt;
begin
  Result := -1;
{$if (HIDAPI_VERSION > 0090) and defined(S)}
  Result := hid_get_input_report(@Self, @Data, Length);
{$elseif defined(D)}
  if Pointer(hid_get_input_report) <> nil then
    Result := hid_get_input_report(@Self, @Data, Length);
{$endif}
end;

function THidDevice.SendFeatureReport(const Data; Length: SizeInt): SizeInt;
begin
  Result :=  hid_send_feature_report(@Self, @Data, Length);
end;

function THidDevice.GetFeatureReport(out Data; Length: SizeInt): SizeInt;
begin
  Result :=  hid_get_feature_report(@Self, @Data, Length);
end;

function THidDevice.GetManufacturerString: UnicodeString;
var
  Buf: array[0..255] of TCWChar;
begin
  hid_get_manufacturer_string(@Self, @Buf, Length(Buf) - 1);
  Result := PCWCharToUnicodeString(@Buf);
end;

function THidDevice.GetProductString: UnicodeString;
var
  Buf: array[0..255] of TCWChar;
begin
  hid_get_product_string(@Self, @Buf, Length(Buf) - 1);
  Result := PCWCharToUnicodeString(@Buf);
end;

function THidDevice.GetSerialNumberString: UnicodeString;
var
  Buf: array[0..255] of TCWChar;
begin
  hid_get_serial_number_string(@Self, @Buf, Length(Buf) - 1);
  Result := PCWCharToUnicodeString(@Buf);
end;

function THidDevice.GetIndexedString(Index: Integer): UnicodeString;
var
  Buf: array[0..255] of TCWChar;
begin
  hid_get_indexed_string(@Self, Index, @Buf, Length(Buf) - 1);
  Result := PCWCharToUnicodeString(@Buf);
end;

function THidDevice.GetError: UnicodeString;
begin
  Result := PCWCharToUnicodeString(hid_error(@Self));
end;

function THidDevice.GetContainerID: TGuid;
var
  Buf: array[1..SizeOf(TGuid)] of Byte;
begin
  Result := GUID_NULL;
{$if (HIDAPI_VERSION > 0120) and defined(S)}
  if hid_winapi_get_container_id(@Self, @Buf) = 0 then
    Result := TGuid.Create(@Buf);
{$elseif defined(D)}
  if Pointer(hid_winapi_get_container_id) <> nil then
  begin
    if hid_winapi_get_container_id(@Self, @Buf) = 0 then
      Result := TGuid.Create(Buf);
  end;
{$endif}
end;

procedure THidDevice.Close;
begin
  hid_close(@Self);
end;

function THidDevice.Open(VID: Word;PID: Word;const SerialNumber: {$ifdef MSWINDOWS}String{$else}UnicodeString{$endif}): PHidDevice;
var
{$ifdef MSWINDOWS}
  WS: array[0..255] of TCWChar;
{$else}
  WS: TCWCharArray;
{$endif}
begin
  if Length(SerialNumber) > 1 then
  begin
{$ifdef MSWINDOWS}
    StringToWideChar(SerialNumber, @WS[0], Length(SerialNumber) + 1);
{$else}
    WS := UnicodeStringToTCWCharNullterminated(SerialNumber);
{$endif}
    Result := hid_open(VID, PID, @WS[0])
  end
  else
    Result := hid_open(VID, PID, nil);
end;

function THidDevice.OpenPath(const DevicePath: String): PHidDevice;
begin
  Result :=  hid_open_path(PChar(DevicePath));
end;


