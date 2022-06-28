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
    FMemory: Pointer;
    FSize,
    FReadIndex,
    FWriteIndex,
    FCount: integer;
    function GetIsEmpty: boolean;
    function GetIsFull: boolean;
  public
    constructor Create(const ASize: integer);
    destructor Destroy; override;
    procedure Read(out AData; const ACount: integer; const APeakAhead: Boolean = false);
    function Write(var AData; const ACount: integer): Integer;
    procedure AdvanceReadIndex(const ACount: Integer);
  published
    property Count: integer read FCount;
    property Size: integer read FSize;
    property IsEmpty: boolean read GetIsEmpty;
    property IsFull: boolean read GetIsFull;
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

function TRingBuffer.GetIsEmpty: boolean; inline;
begin
  Result := FCount = 0;
end;

function TRingBuffer.GetIsFull: boolean; inline;
begin
  Result :=  FCount = FSize;
end;

constructor TRingBuffer.Create(const ASize: integer);
begin
  FReadIndex := 0;
  FWriteIndex := 0;
  FCount := 0;
  FSize := ASize;
  FMemory := GetMem(FSize);
  FillChar(FMemory^, FSize, #0);
end;

destructor TRingBuffer.Destroy;
begin
  Freemem(FMemory, FSize);
  inherited Destroy;
end;

procedure TRingBuffer.Read(out AData; const ACount: integer; const APeakAhead: Boolean = false);
var
  locCount, locReadIndex: Integer;
  PData: PByte;
begin
  if IsEmPty or (ACount = 0) then
    Exit;

  PData := @AData;
  locCount := ACount;
  locReadIndex := FReadIndex;

  if locCount > FCount then
    locCount := FCount;

  if (locCount + locReadIndex) > FSize then
  begin
    Move((FMemory + locReadIndex)^, PData^, FSize - locReadIndex);
    Inc(PData, FSize - locReadIndex);
    Move(FMemory^, PData^, locCount - (FSize - locReadIndex));
    locReadIndex := locCount - (FSize - locReadIndex);
  end
  else
  begin
    Move((FMemory + FReadIndex)^, PData^, locCount);
    locReadIndex := locReadIndex + locCount;
  end;

  if not APeakAhead then
  begin
    FReadIndex := locReadIndex;
    InterlockedExchangeAdd(FCount, -locCount);
  end;
end;

function TRingBuffer.Write(var AData; const ACount: integer): Integer;
var
  locCount: Integer;
  PData: Pointer;
begin
  Result := 0;

  if IsFull or (ACount = 0) then
    Exit;

  PData := @AData;
  locCount := ACount;

  if locCount > FSize - FCount then
    locCount := FSize - FCount;

  if (locCount + FWriteIndex) > FSize then
  begin
    Move(PData^, (FMemory + FWriteIndex)^, FSize - FWriteIndex);
    Inc(PData, FSize - FWriteIndex);
    Move(PData^, FMemory^, locCount - (FSize - FWriteIndex));
    FWriteIndex := locCount - (FSize - FWriteIndex);
  end
  else
  begin
    Move(PData^, (FMemory + FWriteIndex)^, locCount);
    FWriteIndex := FWriteIndex + locCount;
  end;

  InterlockedExchangeAdd(FCount, locCount);

  Result := locCount;
end;

procedure TRingBuffer.AdvanceReadIndex(const ACount: Integer);
var
  locReadIndex: Integer;
begin
  if ACount = 0 then
    exit;

  locReadIndex := FReadIndex;

  if (ACount + locReadIndex) > FSize then
    locReadIndex := ACount - (FSize - locReadIndex)
  else
    locReadIndex := locReadIndex + ACount;

  FReadIndex := locReadIndex;

  InterlockedExchangeAdd(FCount, -ACount);
end;

{ TThreadRead }

procedure TThreadRead.Execute;
var
  USBAspHidPacket: array[0..7] of byte = (0, 0, 0, 0, 0, 0, 0, 0);
begin
  repeat
    if usbasp_read(USBAspHidPacket) > 0 then
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

constructor TThreadRead.Create(const ABuffer: TRingBuffer);
begin
  inherited Create(False);
  FBuffer := ABuffer;
end;

{ TThreadWrite }

procedure TThreadWrite.Execute;
var
  USBAspHidPacket: array[0..7] of byte;
  locCount, SendPacket, AdvanceAmount: Integer;
begin
  repeat
    SendPacket := 0;
    AdvanceAmount := 0;
    if not FBuffer.IsEmpty then
    begin
      locCount := FBuffer.Count;
      if locCount > 7 then
      begin
        FBuffer.Read(USBAspHidPacket, 8, true);
        if USBAspHidPacket[7] < 7 then
        begin
          USBAspHidPacket[7] := 7;
          AdvanceAmount := 7;
        end
        else
          AdvanceAmount := 8;
      end
      else
      begin
        FBuffer.Read(USBAspHidPacket, locCount);
        USBAspHidPacket[7] := locCount;
      end;
      SendPacket := usbasp_write(USBAspHidPacket);
      if SendPacket > 0 then
        if SendPacket = 8 then
          FBuffer.AdvanceReadIndex(AdvanceAmount)
    end;
  until Terminated;
end;

constructor TThreadWrite.Create(const ABuffer: TRingBuffer);
begin
  inherited Create(False);
  FBuffer := ABuffer;
end;


end.
