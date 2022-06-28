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
    HidPacketRead: array[0..7] of byte = (0, 0, 0, 0, 0, 0, 0, 0);
    SerialData: array of byte;
    x: integer;
    prescaler: word;
    rbyte: byte;
    BreakLoop: boolean;
    baud: integer = 9600;
    index: byte = 0;
    crystal: integer = 12000000;
    tmp: string;
    tmpChar: char;
    ReceiveThread: TThreadRead;
    ReceiveRingBuffer: TRingBuffer;
    SendThread: TThreadWrite;
    SendRingBuffer: TRingBuffer;

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

      ReceiveRingBuffer := TRingBuffer.Create(4096);
      ReceiveThread := TThreadRead.Create(ReceiveRingBuffer);
      try
        BreakLoop := false;
        while not BreakLoop do
        begin
          if not ReceiveRingBuffer.IsEmpty then
          begin
            SetLength(SerialData, ReceiveRingBuffer.Count);
            ReceiveRingBuffer.Read(SerialData[0], Length(SerialData));
            for rbyte in SerialData do
              Write(char(rbyte));
          end
          else
            Sleep(5);
          if KeyPressed then
          begin
            if ReadKey = #3 then
              BreakLoop := true;
          end;
        end;

        HidPacketRead[0] := 0;
        HidPacketRead[1] := 0;
        usbasp_uart_set_conf(HidPacketRead);

      finally
        ReceiveThread.Terminate;
        ReceiveThread.WaitFor;
        ReceiveThread.Free;

        ReceiveRingBuffer.Free;
      end;

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


        SendRingBuffer := TRingBuffer.Create(4096);
        SendThread := TThreadWrite.Create(SendRingBuffer);
        try
          tmp := '';
          repeat
            if KeyPressed then
            begin
              tmpChar := ReadKey;
              if (tmpChar <> #3) then
              begin
                Write(tmpChar);
                if tmpChar = #13 then
                  WriteLn
                else
                  tmp := tmp + tmpChar;
              end;
            end
            else
              Sleep(5);
            if not SendRingBuffer.IsFull and (tmpChar = #13) then
            begin
              if tmp.Length <= SendRingBuffer.Size then
              begin
                x := SendRingBuffer.Write(tmp[1], tmp.Length);
                tmp := tmp.Remove(0, x);
                if tmp.length = 0 then
                begin
                  tmp := '';
                  tmpChar := #0;
                end;
              end
              else
              begin
                x := SendRingBuffer.Write(tmp[1], SendRingBuffer.Size);
                tmp := tmp.Remove(0, x);
              end;
            end;
          until tmpChar = #3;

          HidPacketRead[0] := 0;
          HidPacketRead[1] := 0;
          usbasp_uart_set_conf(HidPacketRead);

        finally
          SendThread.Terminate;
          SendThread.WaitFor;
          SendThread.Free;

          SendRingBuffer.Free;
        end;

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
