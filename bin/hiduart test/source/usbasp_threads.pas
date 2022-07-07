unit USBasp_Threads;

{

  This file is part of
    Nephelae USBasp HID UART.

  USB HID Read / Write threads.

  Copyright (C) 2022 Dimitrios Chr. Ioannidis.
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

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, USBasp_HID, uRingBuffer;

type

  { TThreadUSBRead }

  TThreadUSBRead = class(TThread)
  private
    FBuffer: TRingBuffer;
    FUSBaspDevice: PUSBaspHIDDevice;
  protected
    procedure Execute; override;
  public
    constructor Create(const AUSBaspDevice: PUSBaspHIDDevice;
      const ABuffer: TRingBuffer); reintroduce;
  end;

  { TWriteRead }

  TThreadUSBWrite = class(TThread)
  private
    FBuffer: TRingBuffer;
    FUSBaspDevice: PUSBaspHIDDevice;
  protected
    procedure Execute; override;
  public
    constructor Create(const AUSBaspDevice: PUSBaspHIDDevice;
      const ABuffer: TRingBuffer); reintroduce;
  end;

implementation

{ TThreadUSBRead }

procedure TThreadUSBRead.Execute;
var
  USBAspHidPacket: array[0..7] of byte = (0, 0, 0, 0, 0, 0, 0, 0);
  SerialDataCount: byte;
begin
  repeat
    if usbasp_read(FUSBaspDevice, USBAspHidPacket) > 0 then
    begin
      if (USBAspHidPacket[7] > 0) then
      begin
        if (USBAspHidPacket[7] > 7) then
          SerialDataCount := 8
        else
          SerialDataCount := USBAspHidPacket[7];
        if FBuffer.Write(USBAspHidPacket, SerialDataCount) <> SerialDataCount then
          raise TExceptionClass.Create('Buffer OverRun ');
      end;
    end
    else
      Sleep(2);
  until Terminated;
end;

constructor TThreadUSBRead.Create(const AUSBaspDevice: PUSBaspHIDDevice;
  const ABuffer: TRingBuffer);
begin
  inherited Create(False);
  FBuffer := ABuffer;
  FUSBaspDevice := AUSBaspDevice;
end;

{ TThreadUSBWrite }

procedure TThreadUSBWrite.Execute;
var
  USBAspHidPacket: array[0..7] of byte = (0, 0, 0, 0, 0, 0, 0, 0);
  NextByte: byte = 0;
begin
  repeat
    USBAspHidPacket[7] := FBuffer.Read(USBAspHidPacket, 7);
    if USBAspHidPacket[7] > 0 then
    begin
      if (USBAspHidPacket[7] = 7) then
      begin
        if FBuffer.Peek(NextByte) = 0 then
        begin
          if NextByte > 7 then
          begin
            USBAspHidPacket[7] := NextByte;
            FBuffer.AdvanceReadIdx;
          end;
        end;
      end;
      repeat
      until (usbasp_write(FUSBaspDevice, USBAspHidPacket) = 8) or Terminated;
    end
    else
      Sleep(2);
  until Terminated;
end;

constructor TThreadUSBWrite.Create(const AUSBaspDevice: PUSBaspHIDDevice;
  const ABuffer: TRingBuffer);
begin
  inherited Create(False);
  FBuffer := ABuffer;
  FUSBaspDevice := AUSBaspDevice;
end;


end.
