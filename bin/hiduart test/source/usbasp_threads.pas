unit usbasp_threads;

{

  This file is part of
    Nephelae USBasp HID UART.

  Threads for HIDAPI Communications.

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
  Classes, SysUtils, usbasp_hid, uringbuffer;

type

  { TThreadRead }

  TThreadRead = class(TThread)
  private
    FBuffer: TRingBuffer;
    FUSBaspDevice: PUSBaspHIDDevice;
  protected
    procedure Execute; override;
  public
    constructor Create(const AUSBaspDevice: PUSBaspHIDDevice; const ABuffer: TRingBuffer); reintroduce;
  end;

  { TWriteRead }

  TThreadWrite = class(TThread)
  private
    FBuffer: TRingBuffer;
    FUSBaspDevice: PUSBaspHIDDevice;
  protected
    procedure Execute; override;
  public
    constructor Create(const AUSBaspDevice: PUSBaspHIDDevice; const ABuffer: TRingBuffer); reintroduce;
  end;

implementation

{ TThreadRead }

procedure TThreadRead.Execute;
var
  USBAspHidPacket: array[0..7] of byte = (0, 0, 0, 0, 0, 0, 0, 0);
begin
  repeat
    if usbasp_read(FUSBaspDevice, USBAspHidPacket) > 0 then
    begin
      if (USBAspHidPacket[7] > 0) then
      begin
        if (USBAspHidPacket[7] > 7) then
          FBuffer.Write(USBAspHidPacket, 8)
        else
          FBuffer.Write(USBAspHidPacket, USBAspHidPacket[7]);
      end;
    end
  until Terminated;
end;

constructor TThreadRead.Create(const AUSBaspDevice: PUSBaspHIDDevice; const ABuffer: TRingBuffer);
begin
  inherited Create(False);
  FBuffer := ABuffer;
  FUSBaspDevice:= AUSBaspDevice;
end;

{ TThreadWrite }

procedure TThreadWrite.Execute;
var
  USBAspHidPacket: array[0..7] of byte = (0, 0, 0, 0, 0, 0, 0, 0);
  ReadBytes: PtrInt;
begin
  repeat
    if not FBuffer.Empty then
    begin
      ReadBytes := FBuffer.Read(USBAspHidPacket, 7);
      if (ReadBytes = 7) and (FBuffer.PeekByte > 7) then
        USBAspHidPacket[7] := FBuffer.ReadByte
      else
        USBAspHidPacket[7] := ReadBytes;
      repeat
      until (usbasp_write(FUSBaspDevice, USBAspHidPacket) = 8) or Terminated;
    end;
  until Terminated;
end;

constructor TThreadWrite.Create(const AUSBaspDevice: PUSBaspHIDDevice; const ABuffer: TRingBuffer);
begin
  inherited Create(False);
  FBuffer := ABuffer;
  FUSBaspDevice := AUSBaspDevice;
end;


end.
