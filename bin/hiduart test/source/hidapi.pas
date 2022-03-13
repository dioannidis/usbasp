{ HIDAPI.pas - Bindings for libhidapi

  Copyright (C) 2016 Bernd Kreuss <prof7bit@gmail.com>
  Copyright (C) 2021 Dimitrios Chr. Ioannidis <d.ioannidis@nephelae.eu>

  This library is free software; you can redistribute it and/or modify it under the terms of the GNU Library General
  Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option)
  any later version with the following modification:

  As a special exception, the copyright holders of this library give you permission to link this library with
  independent modules to produce an executable, regardless of the license terms of these independent modules,and to
  copy and distribute the resulting executable under terms of your choice, provided that you also meet, for each
  linked independent module, the terms and conditions of the license of that module. An independent module is a module
  which is not derived from or based on this library. If you modify this library, you may extend this exception to
  your version of the library, but you are not obligated to do so. If you do not wish to do so, delete this exception
  statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
  details.

  You should have received a copy of the GNU Library General Public License along with this library; if not, write to
  the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit hidapi;

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

{$define HIDAPI_LOAD_DYNAMICALLY}

{$ifdef HIDAPI_LOAD_DYNAMICALLY}
uses
  dynlibs;
{$endif}

{$i hidapi.inc}

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
  hid_get_input_report(@Self, @Data, Length);
{$elseif defined(D)}
  if Pointer(hid_get_input_report) <> nil then
    hid_get_input_report(@Self, @Data, Length);
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

end.
