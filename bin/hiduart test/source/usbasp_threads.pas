unit usbasp_threads;

{

  This file is part of
    Nephelae USBasp HID UART.

  Threads / Ring Buffer for HIDAPI Communications.

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
  Classes, SysUtils;

type

  { TRingBuffer }

  TRingBuffer = class(TObject)
  private
    FLock: TRTLCriticalSection;
    FMemory: Pointer;
    FSize, FReadIndex, FWriteIndex, FCount: integer;
    function GetCount: integer;
    procedure SetCount(const AValue: integer);
  public
    constructor Create(const ASize: integer);
    destructor Destroy; override;
    procedure Read(var Data; Count: integer);
    procedure Write(var Data; Count: integer);
    function ReadByte: byte;
  published
    property Count: integer read GetCount;
    property Size: integer read FSize;
  end;

  { TThreadRead }

  TThreadRead = class(TThread)
  private
    FBuffer: TRingBuffer;
  protected
    procedure Execute; override;
  public
    constructor Create(const ABuffer: TRingBuffer); reintroduce;
  end;

  { TWriteRead }

  TThreadWrite = class(TThread)
  private
    FBuffer: TRingBuffer;
  protected
    procedure Execute; override;
  public
    constructor Create(const ABuffer: TRingBuffer); reintroduce;
  end;

implementation

uses
  usbasp_hid;

{ TRingBuffer }

function TRingBuffer.GetCount: integer;
begin
  Result := FCount;
end;

procedure TRingBuffer.SetCount(const AValue: integer);
begin
  EnterCriticalSection(FLock);
  try
    FCount := AValue;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

constructor TRingBuffer.Create(const ASize: integer);
begin
  FReadIndex := 0;
  FWriteIndex := 0;
  FCount := 0;
  FSize := ASize;
  FMemory := GetMem(FSize);
  FillChar(FMemory^, FSize, #0);
  InitCriticalSection(FLock);
end;

destructor TRingBuffer.Destroy;
begin
  Freemem(FMemory, FSize);
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

procedure TRingBuffer.Read(var Data; Count: integer);
begin
  if FCount = 0 then
    Exit;
  if Count > FCount then
    Count := FCount;
  if (Count + FReadIndex) > FSize then
  begin
    Move((FMemory + FReadIndex)^, Data, FSize - FReadIndex);
    Move(FMemory^, PByteArray(@Data)[FSize - FReadIndex], Count - (FSize - FReadIndex));
    FReadIndex := Count - (FSize - FReadIndex);
  end
  else
  begin
    Move((FMemory + FReadIndex)^, Data, Count);
    FReadIndex := FReadIndex + Count;
  end;
  FCount := FCount - Count;
end;

procedure TRingBuffer.Write(var Data; Count: integer);
begin
  if FSize = FCount then
    Exit;
  if Count > FSize - FCount then
    Count := FSize - FCount;
  if (Count + FWriteIndex) > FSize then
  begin
    Move(Data, (FMemory + FWriteIndex)^, FSize - FWriteIndex);
    Move(PByteArray(@Data)[FSize - FWriteIndex], FMemory^, Count -
      (FSize - FWriteIndex));
    FWriteIndex := Count - (FSize - FWriteIndex);
  end
  else
  begin
    Move(Data, (FMemory + FWriteIndex)^, Count);
    FWriteIndex := FWriteIndex + Count;
  end;
  FCount := FCount + Count;
end;

function TRingBuffer.ReadByte: byte;
begin
  Read(Result, 1);
end;

{ TThreadRead }

procedure TThreadRead.Execute;
var
  USBAspHidPacket: array[0..7] of byte;
begin
  repeat
    if usbasp_read(USBAspHidPacket) <> 0 then
    begin
      if (USBAspHidPacket[7] > 0) then
        FBuffer.Write(USBAspHidPacket, 8);
    end;
  until Terminated;
end;

constructor TThreadRead.Create(const ABuffer: TRingBuffer);
begin
  inherited Create(False);
  FBuffer := ABuffer;
end;

{ TThreadWrite }

procedure TThreadWrite.Execute;
var
  USBAspHidPacket: array[0..7] of byte;
begin
  repeat
    if usbasp_read(USBAspHidPacket) <> 0 then
      if (USBAspHidPacket[7] > 0) then
        FBuffer.Write(USBAspHidPacket, 8);
  until Terminated;
end;

constructor TThreadWrite.Create(const ABuffer: TRingBuffer);
begin
  inherited Create(False);
  FBuffer := ABuffer;
end;


end.
