unit uRingBuffer;

{

  Producer / Consumer Ring Buffer .

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
    FMemoryData: Pointer;
    FMemorySize, FReadIndex, FWriteIndex: PtrUInt;
    function MaskIndex(const AValue: PtrUInt): PtrUInt;
    function GetEmpty: boolean;
    function GetFull: boolean;
    function GetCapacity: PtrUInt;
  public
    constructor Create(const ASize: PtrUInt);
    destructor Destroy; override;
    function ReadByte: byte;
    procedure WriteByte(const AValue: byte);
    function Read(out ABuffer; const ALength: PtrInt): PtrInt;
    function Write(var ABuffer; const ALength: PtrInt): PtrInt;
    function PeekByte: byte;
    property Empty: boolean read GetEmpty;
    property Full: boolean read GetFull;
    property Size: PtrUInt read FMemorySize;
  end;

implementation

{ TRingBuffer }

constructor TRingBuffer.Create(const ASize: PtrUInt);
begin
  inherited Create;
  FReadIndex := 0;
  FWriteIndex := 0;
  FMemorySize := ASize;
  Getmem(FMemoryData, FMemorySize);
end;

destructor TRingBuffer.Destroy;
begin
  Freemem(FMemoryData, FMemorySize);
  inherited Destroy;
end;

function TRingBuffer.GetEmpty: boolean; inline;
begin
  Result := FReadIndex = FWriteIndex;
end;

function TRingBuffer.GetFull: boolean; inline;
begin
  Result := GetCapacity = FMemorySize;
end;

function TRingBuffer.MaskIndex(const AValue: PtrUInt): PtrUInt; inline;
begin
  Result := AValue and (FMemorySize - 1);
end;

function TRingBuffer.GetCapacity: PtrUInt; inline;
begin
  if FReadIndex > FWriteIndex then
    Result := FWriteIndex + (PtrUInt.MaxValue - FReadIndex)
  else
    Result := FWriteIndex - FReadIndex;
end;

function TRingBuffer.ReadByte: byte;
begin
  Result := pbyte(FMemoryData)[MaskIndex(FReadIndex)];
{$PUSH}
{$Q-}
  Inc(FReadIndex);
{$POP}
end;

procedure TRingBuffer.WriteByte(const AValue: byte);
begin
  pbyte(FMemoryData)[MaskIndex(FWriteIndex)] := AValue;
{$PUSH}
{$Q-}
  Inc(FWriteIndex);
{$POP}
end;

function TRingBuffer.Read(out ABuffer; const ALength: PtrInt): PtrInt;
begin
  Result := 0;
  while (not GetEmpty) and (Result <= ALength) do
  begin
    pbyte(@ABuffer + Result)^ := ReadByte;
    Inc(Result);
  end;
end;

function TRingBuffer.Write(var ABuffer; const ALength: PtrInt): PtrInt;
begin
  Result := 0;
  while (not GetFull) and (Result <= ALength) do
  begin
    WriteByte(pbyte(@ABuffer + Result)^);
    Inc(Result);
  end;
end;

function TRingBuffer.PeekByte: byte;
begin
  Result := pbyte(FMemoryData)[MaskIndex(FReadIndex)];
end;

end.
