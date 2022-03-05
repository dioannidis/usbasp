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
  cthreads,   {$ENDIF}   {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  usbasp_hid;

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
    ReadBuffer: array[0..7] of byte;
    x, RcvByte, pos, size: integer;
    prescaler: word;
    rbyte: byte;
    FileIn: TFileStream;
    SendBuf: Boolean;
    baud: integer = 9600;
    index: byte = 0;
    crystal: integer = 12000000;
    tmp: string;

  begin
    // quick check parameters
    ErrorMsg := CheckOptions('h,l,r,w,i,b,c', ['help', 'list', 'read-input',
      'send-output', 'index', 'baud', 'crystal']);
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
      usbasp_enumerate(false);
      usbasp_open(USBaspHIDList[index]);

      usbasp_uart_get_conf(ReadBuffer);

      ReadBuffer[0] := 0;
      ReadBuffer[1] := 0;

      usbasp_uart_set_conf(ReadBuffer);
      usbasp_uart_get_conf(ReadBuffer);

      prescaler:= crystal div 8 div baud - 1;

      ReadBuffer[0] := lo(prescaler);
      ReadBuffer[1] := hi(prescaler);
      ReadBuffer[2] := 24;
      ReadBuffer[3] := 0;

      usbasp_uart_set_conf(ReadBuffer);
      usbasp_uart_get_conf(ReadBuffer);

      while true do
      begin
        usbasp_read(ReadBuffer);
        RcvByte:=ReadBuffer[7];
        if (RcvByte > 0) then
        begin
          if RcvByte > 7 then
            RcvByte:=8;
          for x := 0 to RcvByte - 1 do
            Write(Char(ReadBuffer[x]));
        end;
      end;

      usbasp_close();
      ReadLn;
      Terminate;
      Exit;
    end;

    if HasOption('w', 'send-output') then
    begin
      usbasp_enumerate(false);
      usbasp_open(USBaspHIDList[index]);

      usbasp_uart_get_conf(ReadBuffer);

      ReadBuffer[0] := 0;
      ReadBuffer[1] := 0;

      usbasp_uart_set_conf(ReadBuffer);
      usbasp_uart_get_conf(ReadBuffer);

      prescaler:= crystal div 8 div baud - 1;

      ReadBuffer[0] := lo(prescaler);
      ReadBuffer[1] := hi(prescaler);
      ReadBuffer[2] := 24;
      ReadBuffer[3] := 0;

      usbasp_uart_set_conf(ReadBuffer);
      usbasp_uart_get_conf(ReadBuffer);

      while true do
      begin
        x := 1;
        SendBuf := False;
        readln(tmp);
        size := tmp.Length;
        pos := 0;
        while pos < tmp.Length do
        begin
          ReadBuffer[x-1] := ord(tmp[pos+1]);
          Inc(pos);
          if ( x < 8) and (pos = tmp.length) then
          begin
            ReadBuffer[7] := x;
            SendBuf:=true;
          end
          else
          begin
            if (x = 8) then
            begin
              if ReadBuffer[7] < 7 then
              begin
                pos := pos - 1;
                ReadBuffer[7] := 7;
              end;
              x := 1;
              SendBuf:=true;
            end
            else
              Inc(x);
          end;
          if SendBuf then
          begin
            usbasp_write(ReadBuffer);
            SendBuf:=false;
          end;
        end;
      end;

      //FileIn := TFilestream.Create('test.txt', fmOpenRead);
      //try
      //  FileIn.Seek(0, soFromBeginning);
      //  x := 1;
      //  SendBuf := False;
      //  while FileIn.Position < FileIn.Size do
      //  begin
      //    ReadBuffer[x-1] := FileIn.ReadByte;
      //    if ( x < 8) and (FileIn.Position = FileIn.Size) then
      //    begin
      //      ReadBuffer[7] := x;
      //      SendBuf:=true;
      //    end
      //    else
      //    begin
      //      if (x = 8) then
      //      begin
      //        if ReadBuffer[7] < 7 then
      //        begin
      //          FileIn.Seek(FileIn.Position - 1, soFromBeginning);
      //          ReadBuffer[7] := 7;
      //        end;
      //        x := 1;
      //        SendBuf:=true;
      //      end
      //      else
      //        Inc(x);
      //    end;
      //    if SendBuf then
      //    begin
      //      usbasp_write(ReadBuffer);
      //      SendBuf:=false;
      //    end;
      //  end;
      //finally
      //  FileIn.Free;
      //end;
      //end;

      usbasp_close();
      ReadLn;
      Terminate;
      Exit;
    end;

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
