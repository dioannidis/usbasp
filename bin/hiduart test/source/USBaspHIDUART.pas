program USBaspHIDUART;

{

  This file is part of
    Nephelae USBasp HID UART.

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

{$mode objfpc}{$H+}
{$modeswitch autoderef}
{$macro on}

uses
 {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads,  {$ENDIF}  {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  crt,
  usbasp_hid,
  usbasp_threads;

type

  TUSBaspHIDUARTTest = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  procedure TUSBaspHIDUARTTest.DoRun;
  var
    ErrorMsg: string;
    HidPacketRead: array[0..7] of byte;
    SerialData: array of byte;
    x, RcvByte, pos, size: integer;
    prescaler: word;
    rbyte: byte;
    //FileIn: TFileStream;
    SendBuf: boolean;
    baud: integer = 9600;
    index: byte = 0;
    crystal: integer = 12000000;
    tmp: string;
    tmpChar: char;
    ReadThread: TThreadRead;
    ReadRingBuffer: TRingBuffer;

  begin
    // quick check parameters
    ErrorMsg := CheckOptions('h,l,r,w,i,b,c', ['help', 'list',
      'read-input', 'send-output', 'index', 'baud', 'crystal']);
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    if HasOption('l', 'list') then
    begin
      usbasp_enumerate(True);
      ReadLn;
      Terminate;
      Exit;
    end;

    if HasOption('c', 'crystal') then
    begin
      crystal := StrToInt(GetOptionValue('c', 'crystal'));
    end;

    if HasOption('i', 'index') then
    begin
      index := StrToInt(GetOptionValue('i', 'index'));
    end;

    if HasOption('b', 'baud') then
    begin
      baud := StrToInt(GetOptionValue('b', 'baud'));
    end;

    if HasOption('r', 'read-input') then
    begin
      usbasp_enumerate(False);

      if (USBaspHIDList.Count = 0) then
        exit;

      if USBaspHIDList[index].HidDevice <> nil then
        usbasp_open(USBaspHIDList[index])
      else
        exit;

      usbasp_uart_get_conf(HidPacketRead);

      HidPacketRead[0] := 0;
      HidPacketRead[1] := 0;
      usbasp_uart_set_conf(HidPacketRead);

      usbasp_uart_get_conf(HidPacketRead);
      if (not HasOption('c', 'crystal')) then
      begin
        case HidPacketRead[5] of
          1: crystal := 16000000;
          2: crystal := 18000000;
          3: crystal := 20000000;
        end;
      end;

      prescaler := crystal div 8 div baud - 1;

      HidPacketRead[0] := lo(prescaler);
      HidPacketRead[1] := hi(prescaler);
      HidPacketRead[2] := 24;
      HidPacketRead[3] := 0;

      usbasp_uart_set_conf(HidPacketRead);

      WriteLn();
      WriteLn('USBasp device configuration');
      WriteLn('---------------------------');
      WriteLn('Crystal : ', crystal);
      WriteLn('Baud    : ', baud);
      WriteLn();

      ReadRingBuffer := TRingBuffer.Create(4096);
      ReadThread := TThreadRead.Create(ReadRingBuffer);
      try
        while True do
        begin
          if not ReadRingBuffer.IsEmpty then
          begin
            SetLength(SerialData, ReadRingBuffer.Count);
            ReadRingBuffer.Read(SerialData[0], Length(SerialData));
            for rbyte in SerialData do
              Write(char(rbyte));
          end;
          if KeyPressed then
          begin
            if (ReadKey = #3) or (ReadKey = #27) then
              Break;
          end;
          Sleep(1);
        end;
      finally
        ReadThread.Terminate;
        ReadThread.WaitFor;
        ReadThread.Free;

        ReadRingBuffer.Free;
      end;


      HidPacketRead[0] := 0;
      HidPacketRead[1] := 0;
      usbasp_uart_set_conf(HidPacketRead);

      usbasp_close();

      Terminate;
      Exit;
    end;

    if HasOption('w', 'send-output') then
    begin
      usbasp_enumerate(False);

      if (USBaspHIDList.Count > 0) then
      begin

        usbasp_open(USBaspHIDList[index]);

        usbasp_uart_get_conf(HidPacketRead);

        HidPacketRead[0] := 0;
        HidPacketRead[1] := 0;
        usbasp_uart_set_conf(HidPacketRead);

        usbasp_uart_get_conf(HidPacketRead);
        if (not HasOption('c', 'crystal')) then
        begin
          case HidPacketRead[5] of
            1: crystal := 16000000;
            2: crystal := 18000000;
            3: crystal := 20000000;
          end;
        end;
        prescaler := crystal div 8 div baud - 1;

        HidPacketRead[0] := lo(prescaler);
        HidPacketRead[1] := hi(prescaler);
        HidPacketRead[2] := 24;
        HidPacketRead[3] := 0;

        usbasp_uart_set_conf(HidPacketRead);

        WriteLn();
        WriteLn('USBasp device configuration');
        WriteLn('---------------------------');
        WriteLn('Crystal : ', crystal);
        WriteLn('Baud    : ', baud);
        WriteLn();

        while True do
        begin
          x := 1;
          SendBuf := False;

          tmp := '';
          repeat
            if KeyPressed then
            begin
              tmpChar := ReadKey;
              if (tmpChar <> #3) and (tmpChar <> #13) and (tmpChar <> #27) then
              begin
                Write(tmpChar);
                tmp := tmp + tmpChar;
              end;
            end;
          until (tmpChar = #13) or (tmpChar = #3) or (tmpChar <> #27);

          if (tmpChar = #3) or (tmpChar = #27) then
            break
          else
          begin
            tmpChar := #0;
            writeln();
          end;

          pos := 0;
          while pos < tmp.Length do
          begin
            HidPacketRead[x - 1] := Ord(tmp[pos + 1]);
            Inc(pos);
            if (x < 8) and (pos = tmp.length) then
            begin
              HidPacketRead[7] := x;
              SendBuf := True;
            end
            else
            begin
              if (x = 8) then
              begin
                if HidPacketRead[7] < 7 then
                begin
                  pos := pos - 1;
                  HidPacketRead[7] := 7;
                end;
                x := 1;
                SendBuf := True;
              end
              else
                Inc(x);
            end;
            if SendBuf then
            begin
              usbasp_write(HidPacketRead);
              SendBuf := False;
            end;
          end;
        end;

        HidPacketRead[0] := 0;
        HidPacketRead[1] := 0;
        usbasp_uart_set_conf(HidPacketRead);

        usbasp_close();
      end;

      Terminate;
      Exit;
    end;

    WriteHelp;
    // stop program loop
    Terminate;
  end;

  constructor TUSBaspHIDUARTTest.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TUSBaspHIDUARTTest.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TUSBaspHIDUARTTest.WriteHelp;
  begin
    { add your help code here }
    writeln();
    writeln('USBasp HIDUART Test App');
    writeln();
    writeln(' -l  List USBasp HID devices');
    writeln(' -i  Select USBasp index ( default 0 )');
    writeln(' -b  Set Baud ( default 9600 )');
    writeln(' -c  Set Crystal Hz ( default 12 MHz or 12000000 Hz )');
    writeln(' -r  Continuous read input');
    writeln(' -w  Interactive send output');
    writeln();
    writeln('examples');
    writeln();
    writeln('Read from USBasp at index 0 with 4800 baud');
    writeln(' USBaspHIDUART -b 4800 -r');
    writeln();
    writeln('Interactive write to USBasp at index 1 with 9600 baud');
    writeln(' USBaspHIDUART -i 1 -w');
    writeln();
    writeln('Read from USBasp at index 1 with 19200 baud from a device with 20 MHz crystal');
    writeln(' USBaspHIDUART -i 1 -b 19200 -c 20000000 -r');
  end;

var
  Application: TUSBaspHIDUARTTest;

{$R *.res}

begin
  Application := TUSBaspHIDUARTTest.Create(nil);
  Application.Run;
  Application.Free;
end.
