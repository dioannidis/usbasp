unit USBasp_HID;

{

  This file is part of
    Nephelae USBasp HID UART.

  HIDAPI Communications.

  Copyright (C) 2021 - 2022 Dimitrios Chr. Ioannidis.
    Nephelae - https://www.nephelae.eu

  https://www.nephelae.eu/

  Licensed under the MIT License (MIT).
  See licence file in root directory.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
  ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
  TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
  PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
  SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
  ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
  OTHER DEALINGS IN THE SOFTWARE.

}

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, hidapi, fgl;

type

  TUSBaspHIDDevice = record
    HidDevice: PHidDevice;
    index: byte;
    Path: string;
    Serial: string;
    Manufacturer: string;
    Product: string;
    FirmwareVersion: string;
    Capabilities: string;
    VendorID: word;
    ProductID: word;
    InterfaceNumber: integer;
    ReportSize: word;
    PacketCount: byte;
  end;
  PUSBaspHIDDevice = ^TUSBaspHIDDevice;

  { TUSBaspHIDDeviceList }

  TUSBaspHIDDeviceList = class(TFPList)
  private
    function Get(Index: integer): PUSBaspHIDDevice;
  public
    destructor Destroy; override;
    function Add(Value: PUSBaspHIDDevice): integer;
    procedure FreeItems;
    property Items[Index: integer]: PUSBaspHIDDevice read Get; default;
  end;

procedure usbasp_enumerate(const APrintInfo: boolean = False);
procedure usbasp_open(const AUSBaspHIDDevice: PUSBaspHIDDevice);
procedure usbasp_close(const AUSBaspHIDDevice: PUSBaspHIDDevice);
function usbasp_read(const AUSBaspHIDDevice: PUSBaspHIDDevice; var Data): integer;
function usbasp_write(const AUSBaspHIDDevice: PUSBaspHIDDevice; var Data): integer;
function usbasp_uart_set_conf(const AUSBaspHIDDevice: PUSBaspHIDDevice; var Data): integer;
function usbasp_uart_get_conf(const AUSBaspHIDDevice: PUSBaspHIDDevice; var Data): integer;
function usbasp_uart_write_serial(const AUSBaspHIDDevice: PUSBaspHIDDevice; const ASerialNumber: String): integer;

var
  USBaspHIDList: TUSBaspHIDDeviceList;

implementation

uses
  StrUtils;

var
  i: integer;

function enumerate_hid(AUSBaspHIDDeviceList: TUSBaspHIDDeviceList): integer;
var
  HidEnumerateList, HidItem: PHidDeviceInfo;
  USBaspHIDDevice: PUSBaspHIDDevice;
  index: byte;
begin
  AUSBaspHIDDeviceList.FreeItems;
  AUSBaspHIDDeviceList.Clear;
  try
    HidEnumerateList := THidDeviceInfo.Enumerate($16C0, $05DC);
    HidItem := HidEnumerateList;
    index := 0;
    while Assigned(HidItem) do
    begin
      New(USBaspHIDDevice);
      USBaspHIDDevice^.index:=index;
      USBaspHIDDevice^.Path := HidItem^.Path;
      USBaspHIDDevice^.Serial := PCWCharToUnicodeString(HidItem^.SerialNumber);
      USBaspHIDDevice^.Manufacturer :=
        PCWCharToUnicodeString(HidItem^.ManufacturerString);
      USBaspHIDDevice^.Product := PCWCharToUnicodeString(HidItem^.ProductString);
      USBaspHIDDevice^.InterfaceNumber := HidItem^.InterfaceNumber;
      USBaspHIDDevice^.VendorID := HidItem^.VendorID;
      USBaspHIDDevice^.ProductID := HidItem^.ProductID;
      USBaspHIDDevice^.FirmwareVersion :=
        ReverseString(ReverseString(BCDToInt(HidItem^.ReleaseNumber).ToString()).Insert(2, '.'));
      USBaspHIDDevice^.Capabilities := '';
      USBaspHIDDevice^.ReportSize := 8;
      USBaspHIDDevice^.PacketCount := 1;
      AUSBaspHIDDeviceList.Add(USBaspHIDDevice);
      HidItem := HidItem^.Next;
      Inc(Index);
    end;
  finally
    HidEnumerateList^.Free;
  end;
  Result := AUSBaspHIDDeviceList.Count;
end;

procedure PrintInfo(const AUSBaspHIDDevice: PUSBaspHIDDevice);
begin
  WriteLn('-----------');
  WriteLn('USBasp List index : ', AUSBaspHIDDevice^.index);
  WriteLn();
  WriteLn('Type : ', AUSBaspHIDDevice^.VendorID.ToHexString, ' ', AUSBaspHIDDevice^.ProductID.ToHexString);
  WriteLn('Path : ', AUSBaspHIDDevice^.Path);
  WriteLn('Serial number : ', AUSBaspHIDDevice^.Serial);
  WriteLn('Manufacturer  : ', AUSBaspHIDDevice^.Manufacturer);
  WriteLn('Product       : ', AUSBaspHIDDevice^.Product);
  WriteLn('Release       : ', AUSBaspHIDDevice^.FirmwareVersion);
  WriteLn('Interface     : ', AUSBaspHIDDevice^.InterfaceNumber);
  WriteLn();
end;

procedure usbasp_enumerate(const APrintInfo: boolean);
var
  USBasp: PUSBaspHIDDevice;
begin
  WriteLn();
  WriteLn('Enumerating USBasp HIDUART (Hid Api Library Version : ', HidApiVersion, ' )');

  if enumerate_hid(USBaspHIDList) > 0 then
  begin
    for USBasp in USBaspHIDList do
    begin
      usbasp_open(USBasp);
      usbasp_close(USBasp);
      if APrintInfo then
        PrintInfo(USBasp);
    end;
    //ReadLn();
  end
  else
  begin
    WriteLn();
    WriteLn('No USBasp HID UART found.');
  end;
end;

procedure usbasp_open(const AUSBaspHIDDevice: PUSBaspHIDDevice);
begin
  AUSBaspHIDDevice^.HidDevice := THidDevice.OpenPath(AUSBaspHIDDevice^.Path);
end;

procedure usbasp_close(const AUSBaspHIDDevice: PUSBaspHIDDevice);
begin
  AUSBaspHIDDevice^.HidDevice^.Close;
end;

function usbasp_read(const AUSBaspHIDDevice: PUSBaspHIDDevice; var Data): integer;
var
  HidBuffer: array[0..8] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0);
begin
  // Read default 0 Report ID
  Result := AUSBaspHIDDevice^.HidDevice^.ReadTimeout(HidBuffer, AUSBaspHIDDevice^.ReportSize, 250);
  Move(HidBuffer, Data, AUSBaspHIDDevice^.ReportSize);
end;

function usbasp_write(const AUSBaspHIDDevice: PUSBaspHIDDevice; var Data): integer;
var
  HidBuffer: array[0..8] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0);
begin
  // Add Report ID
  HidBuffer[0] := $00;
  Move(Data, HidBuffer[1], AUSBaspHIDDevice^.ReportSize + 1);

  // Report size plus added Report ID
  Result := AUSBaspHIDDevice^.HidDevice^.Write(HidBuffer, AUSBaspHIDDevice^.ReportSize + 1) - 1;
end;

function usbasp_uart_get_conf(const AUSBaspHIDDevice: PUSBaspHIDDevice; var Data): integer;
var
  HidBuffer: array[0..8] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0);
  HidSize: SizeInt;
begin
  // Add Report ID
  HidBuffer[0] := $00;

  // Report size plus added Report ID
  HidSize := AUSBaspHIDDevice^.HidDevice^.GetFeatureReport(HidBuffer, AUSBaspHIDDevice^.ReportSize + 1) - 1;

  Move(HidBuffer[1], Data, HidSize);

  Result := HidSize - 1;
end;

function usbasp_uart_write_serial(const AUSBaspHIDDevice: PUSBaspHIDDevice;
  const ASerialNumber: String): integer;
var
  HidBuffer: array[0..8] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0);
  HidSize: SizeInt;
  ErrorCode: integer;
  SerNumValue: Word;
begin
  Val(ASerialNumber, SerNumValue, ErrorCode);

  // Add Report ID
  HidBuffer[0] := $00;

  HidBuffer[1] := lo(SerNumValue);
  HidBuffer[2] := Hi(SerNumValue);

  HidBuffer[4] := $01;

  // Report size plus added Report ID
  HidSize := AUSBaspHIDDevice^.HidDevice^.SendFeatureReport(HidBuffer, AUSBaspHIDDevice^.ReportSize + 1);

  Result := HidSize;
end;

function usbasp_uart_set_conf(const AUSBaspHIDDevice: PUSBaspHIDDevice; var Data): integer;
var
  HidBuffer: array[0..8] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0);
  HidSize: SizeInt;
begin
  // Add Report ID
  HidBuffer[0] := $00;
  Move(Data, HidBuffer[1], AUSBaspHIDDevice^.ReportSize + 1);

  // Report size plus added Report ID
  HidSize := AUSBaspHIDDevice^.HidDevice^.SendFeatureReport(HidBuffer, AUSBaspHIDDevice^.ReportSize + 1);

  Move(HidBuffer[1], Data, HidSize - 1);

  Result := HidSize;
end;

{ TUSBaspHIDDeviceList }

function TUSBaspHIDDeviceList.Get(Index: integer): PUSBaspHIDDevice;
begin
  Result := PUSBaspHIDDevice(inherited Get(Index));
end;

destructor TUSBaspHIDDeviceList.Destroy;
begin
  FreeItems;
  inherited Destroy;
end;

function TUSBaspHIDDeviceList.Add(Value: PUSBaspHIDDevice): integer;
begin
  Result := inherited Add(Value);
end;

procedure TUSBaspHIDDeviceList.FreeItems;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Dispose(Items[i]);
end;

initialization
  HidInit();
  USBaspHIDList := TUSBaspHIDDeviceList.Create;

finalization;
  USBaspHIDList.Free;
  HidExit();

end.
