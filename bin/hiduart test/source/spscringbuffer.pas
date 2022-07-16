unit SPSCRingBuffer;

{

  Single Producer Single Consumer (SPSC) Ring Buffer .

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

  { TSPSCRingBuffer }

  TSPSCRingBuffer = class(TObject)
  private
    FMemoryData: Pointer;
    FMemorySize, FReadIndex, FWriteIndex: PtrUInt;
    function MaskIndex(const AValue: PtrUInt): PtrUInt;
    function GetEmpty: boolean;
    function GetFull: boolean;
    function GetCapacity: PtrUInt;
  protected
    function ReadByte: byte;
    procedure WriteByte(const AValue: byte);
    function PeekByte(AIndex: PtrUInt = 0): byte;
  public
    constructor Create(const ASize: PtrUInt);
    destructor Destroy; override;
    function Read(out ABuffer; const ALength: PtrUInt): PtrUInt;
    function Write(const ABuffer; const ALength: PtrUInt): PtrUInt;
    function Peek(out ABuffer; ALength: PtrUInt): PtrUInt;
    procedure AdvanceReadIdx(ACount: PtrUInt = 1);
    property Empty: boolean read GetEmpty;
    property Full: boolean read GetFull;
    property Size: PtrUInt read FMemorySize;
  end;

implementation

{ TSPSCRingBuffer }

constructor TSPSCRingBuffer.Create(const ASize: PtrUInt);
begin
  inherited Create;
  FReadIndex := 0;
  FWriteIndex := 0;
  FMemorySize := ASize;
  Getmem(FMemoryData, FMemorySize);
end;

destructor TSPSCRingBuffer.Destroy;
begin
  Freemem(FMemoryData, FMemorySize);
  inherited Destroy;
end;

function TSPSCRingBuffer.GetEmpty: boolean; //inline;
begin
  Result := FReadIndex = FWriteIndex;
end;

function TSPSCRingBuffer.GetFull: boolean; //inline;
begin
  Result := GetCapacity = FMemorySize - 1;
end;

function TSPSCRingBuffer.MaskIndex(const AValue: PtrUInt): PtrUInt; //inline;
begin
  Result := AValue and (FMemorySize - 1);
end;

// See : https://forum.lazarus.freepascal.org/index.php/topic,59796.msg446453.html#msg446453
function TSPSCRingBuffer.GetCapacity: PtrUInt;  //inline;
begin
{$PUSH}
{$Q-}
{$R-}
  Result := MaskIndex(FWriteIndex - FReadIndex);
{$POP}
end;

function TSPSCRingBuffer.ReadByte: byte; inline;
begin
  Result := pbyte(FMemoryData)[MaskIndex(FReadIndex)];
{$PUSH}
{$Q-}
  Inc(FReadIndex);
{$POP}
end;

procedure TSPSCRingBuffer.WriteByte(const AValue: byte); inline;
begin
  pbyte(FMemoryData)[MaskIndex(FWriteIndex)] := AValue;
{$PUSH}
{$Q-}
  Inc(FWriteIndex);
{$POP}
end;

function TSPSCRingBuffer.PeekByte(AIndex: PtrUInt): byte;
begin
  Result := pbyte(FMemoryData)[MaskIndex(FReadIndex + AIndex)];
end;

function TSPSCRingBuffer.Read(out ABuffer; const ALength: PtrUInt): PtrUInt;
begin
  Result := 0;
  while (not Empty) and (Result < ALength) do
  begin
    pbyte(@ABuffer + Result)^ := ReadByte;
    Inc(Result);
  end;
end;

function TSPSCRingBuffer.Write(const ABuffer; const ALength: PtrUInt): PtrUInt;
begin
  Result := 0;
  while (not Full) and (Result < ALength) do
  begin
    WriteByte(pbyte(@ABuffer + Result)^);
    Inc(Result);
  end;
end;

function TSPSCRingBuffer.Peek(out ABuffer; ALength: PtrUInt): PtrUInt;
begin
  Result := 0;
  while (Result < ALength) and (Result < GetCapacity) do
  begin
    pbyte(@ABuffer + Result)^ := PeekByte(Result);
    Inc(Result);
  end;
end;

procedure TSPSCRingBuffer.AdvanceReadIdx(ACount: PtrUInt);
begin
{$PUSH}
{$Q-}
  Inc(FReadIndex, ACount);
{$POP}
end;

end.
