
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
}
  {$ifndef HIDAPI_VERSION}
    {$ERROR For static linking you need to define the version of the HidApi library.
     i.e. for 0.9.0 use HIDAPI_VERSION = 0090 or for 0.10.0 HIDAPI_VERSION = 0100 }
  {$endif}
{$endif}

{$ifdef MSWINDOWS}

{$if FPC_FULLVERSION > 30004}
  {$define HIDAPI_CALL := WINAPI }
{$else}
  {$define HIDAPI_CALL := stdcall }
{$endif}

const
  LIBHIDAPI = 'hidapi.dll';

{$else}

{$define HIDAPI_CALL := cdecl }

const
{$ifdef Darwin}
  LIBHIDAPI = 'hidapi';
  {$linklib hidapi}
{$else}
  {$ifdef D}
  LIBHIDAPI = 'libhidapi-libusb.so';
  {$else}
  LIBHIDAPI = 'hidapi-libusb';
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


{$ifdef HIDAPI_LOAD_DYNAMICALLY}

ResourceString
  SErrLoadFailed     = 'Can not load hidapi library "%s". Check your installation.';
  SErrAlreadyLoaded  = 'HidApi library %s already loaded.';

var
  HidApiLibraryRefCount: byte;
  HidApiLibraryHandle: THandle;
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
