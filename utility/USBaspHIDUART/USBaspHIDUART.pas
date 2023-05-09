program USBaspHIDUART;

{

  This file is part of
    Nephelae USBasp HID UART.

  Copyright (C) 2022 - 2023 Dimitrios Chr. Ioannidis.
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
  USBasp_HID,
  USBasp_Threads,
  SPSCRingBuffer;

type

  TUSBaspHIDUARTTest = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure PrintHidInfo(const AUSBaspHIDDevice: PUSBaspHIDDevice);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  procedure TUSBaspHIDUARTTest.PrintHidInfo(const AUSBaspHIDDevice: PUSBaspHIDDevice);
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
  end;


  procedure TUSBaspHIDUARTTest.DoRun;
  const
    ReceiveBufferSize = 8192;
    SendBufferSize    = 8192;
  var
    ErrorMsg: string;
    HidPacketBuffer: array[0..7] of byte = (0, 0, 0, 0, 0, 0, 0, 0);
    SerialData: array of byte;
    x, RcvBytes, SndBytes: integer;
    prescaler: word;
    BreakLoop, Sending: boolean;
    baud: integer = 9600;
    SerialNum: string = '';
    USBaspIndex: byte = 0;
    crystal: integer = 12000000;
    InputString: string;
    InputChar: char;
    ReceiveThread: TThreadUSBRead;
    ReceiveRingBuffer: TSPSCRingBuffer;
    SendThread: TThreadUSBWrite;
    SendRingBuffer: TSPSCRingBuffer;

  begin
    // quick check parameters
    ErrorMsg := CheckOptions('h,l,r,w,i,b,c,s,u', ['help', 'list',
      'read-input', 'send-output', 'index', 'baud', 'crystal', 'serial', 'update-serial']);
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
      Terminate;
      Exit;
    end;

    if HasOption('c', 'crystal') then
    begin
      crystal := StrToInt(GetOptionValue('c', 'crystal'));
    end;

    if HasOption('i', 'index') then
    begin
      USBaspIndex := StrToInt(GetOptionValue('i', 'index'));
    end;

    if HasOption('b', 'baud') then
    begin
      baud := StrToInt(GetOptionValue('b', 'baud'));
    end;

    if HasOption('s', 'serial') then
    begin
      SerialNum := GetOptionValue('s', 'serial');
      if SerialNum.Length > 4 then
        SerialNum := SerialNum.Substring(0, 4);
    end;

    if HasOption('u', 'update-serial') then
    begin
      usbasp_enumerate(False);

      if (USBaspHIDList.Count > 0) then
      begin

        SerialNum := GetOptionValue('u', 'update-serial');
        if SerialNum.Length > 4 then
          SerialNum := SerialNum.Substring(0, 4);

        if (HasOption('i', 'index')) and (USBaspIndex > USBaspHIDList.Count - 1) then
        begin
          WriteLn();
          WriteLn('ERROR : There is no USBasp HID UART with index ', USBaspIndex);
          terminate;
          exit;
        end;

        if USBaspHIDList[USBaspIndex].HidDevice <> nil then
          usbasp_open(USBaspHIDList[USBaspIndex])
        else
        begin
          WriteLn();
          WriteLn('ERROR : Cannot open USBasp HID UART with index ', USBaspIndex);
          terminate;
          exit;
        end;

        PrintHidInfo(USBaspHIDList[USBaspIndex]);

        usbasp_uart_get_conf(USBaspHIDList[USBaspIndex], HidPacketBuffer);
        if (HidPacketBuffer[4] and $20) <> $20 then
        begin
          WriteLn();
          WriteLn('USBasp does not support Serial Number update ! ');
        end
        else
        begin
          WriteLn();
          WriteLn();
          Write('Updating USBasp Serial Number ... ');
          usbasp_uart_write_serial(USBaspHIDList[USBaspIndex], SerialNum);
          WriteLn('Done .');
          WriteLn();
        end;

        usbasp_close(USBaspHIDList[USBaspIndex]);

        usbasp_enumerate(False);
        PrintHidInfo(USBaspHIDList[USBaspIndex]);

      end;
      Terminate;
      Exit;
    end;

    if HasOption('r', 'read-input') then
    begin
      usbasp_enumerate(False);

      if (USBaspHIDList.Count > 0) then
      begin

        if USBaspIndex > USBaspHIDList.Count - 1 then
        begin
          WriteLn();
          WriteLn('ERROR : There is no USBasp HID UART with index ', USBaspIndex);
          terminate;
          exit;
        end;

        if SerialNum <> '' then
        begin
          USBaspIndex := 255;
          for x := 0 to  USBaspHIDList.Count - 1 do
            if USBaspHIDList[x].Serial = SerialNum then
            begin
              USBaspIndex := USBaspHIDList[x].index;
              Break;
            end;
          If USBaspIndex = 255 then
          begin
            WriteLn('ERROR : Cannot open USBasp HID UART with serial ', SerialNum);
            terminate;
            exit;
          end;
        end;

        if USBaspHIDList[USBaspIndex].HidDevice <> nil then
          usbasp_open(USBaspHIDList[USBaspIndex])
        else
        begin
          WriteLn();
          WriteLn('ERROR : Cannot open USBasp HID UART with index ', USBaspIndex);
          terminate;
          exit;
        end;

        usbasp_uart_get_conf(USBaspHIDList[USBaspIndex], HidPacketBuffer);

        HidPacketBuffer[0] := 0;
        HidPacketBuffer[1] := 0;
        usbasp_uart_set_conf(USBaspHIDList[USBaspIndex], HidPacketBuffer);

        usbasp_uart_get_conf(USBaspHIDList[USBaspIndex], HidPacketBuffer);
        if (not HasOption('c', 'crystal')) then
        begin
          case HidPacketBuffer[5] of
            1: crystal := 16000000;
            2: crystal := 18000000;
            3: crystal := 20000000;
          end;
        end;

        prescaler := crystal div 8 div baud - 1;

        HidPacketBuffer[0] := lo(prescaler);
        HidPacketBuffer[1] := hi(prescaler);
        HidPacketBuffer[2] := 24;
        HidPacketBuffer[3] := 0;

        usbasp_uart_set_conf(USBaspHIDList[USBaspIndex], HidPacketBuffer);

        WriteLn();
        WriteLn('USBasp device configuration');
        WriteLn('---------------------------');
        WriteLn('Serial number : ', USBaspHIDList[USBaspIndex].Serial);
        WriteLn('Crystal       : ', crystal);
        WriteLn('Baud          : ', baud);
        WriteLn();

        ReceiveRingBuffer := TSPSCRingBuffer.Create(ReceiveBufferSize);
        ReceiveThread := TThreadUSBRead.Create(USBaspHIDList[USBaspIndex], ReceiveRingBuffer);
        try
          BreakLoop := false;
          SetLength(SerialData, 128);
          while not BreakLoop do
          begin
            SerialData[ReceiveRingBuffer.Read(SerialData[0], Length(SerialData))] := 0;
            Write(PChar(SerialData));
            if KeyPressed then
            begin
              case ReadKey of
                #3 : BreakLoop := True;
                #13: Writeln();
              end;
            end;
            Sleep(2);
          end;

          HidPacketBuffer[0] := 0;
          HidPacketBuffer[1] := 0;
          usbasp_uart_set_conf(USBaspHIDList[USBaspIndex], HidPacketBuffer);

        finally
          ReceiveThread.Terminate;
          ReceiveThread.WaitFor;
          ReceiveThread.Free;

          ReceiveRingBuffer.Free;
        end;

        usbasp_close(USBaspHIDList[USBaspIndex]);
      end;
      Terminate;
      Exit;
    end;

    if HasOption('w', 'send-output') then
    begin
      usbasp_enumerate(False);

      if (USBaspHIDList.Count > 0) then
      begin

        if USBaspIndex > USBaspHIDList.Count - 1 then
        begin
          WriteLn();
          WriteLn('ERROR : There is no USBasp HID UART with index ', USBaspIndex);
          terminate;
          exit;
        end;

        if SerialNum <> '' then
        begin
          USBaspIndex := 255;
          for x := 0 to  USBaspHIDList.Count - 1 do
            if USBaspHIDList[x].Serial = SerialNum then
            begin
              USBaspIndex := USBaspHIDList[x].index;
              Break;
            end;
          If USBaspIndex = 255 then
          begin
            WriteLn('ERROR : Cannot open USBasp HID UART with serial ', SerialNum);
            terminate;
            exit;
          end;
        end;

        if USBaspHIDList[USBaspIndex].HidDevice <> nil then
          usbasp_open(USBaspHIDList[USBaspIndex])
        else
        begin
          WriteLn();
          WriteLn('ERROR : Cannot open USBasp HID UART with index ', USBaspIndex);
          terminate;
          exit;
        end;

        usbasp_uart_get_conf(USBaspHIDList[USBaspIndex], HidPacketBuffer);

        HidPacketBuffer[0] := 0;
        HidPacketBuffer[1] := 0;
        usbasp_uart_set_conf(USBaspHIDList[USBaspIndex], HidPacketBuffer);

        usbasp_uart_get_conf(USBaspHIDList[USBaspIndex], HidPacketBuffer);
        if (not HasOption('c', 'crystal')) then
        begin
          case HidPacketBuffer[5] of
            1: crystal := 16000000;
            2: crystal := 18000000;
            3: crystal := 20000000;
          end;
        end;
        prescaler := crystal div 8 div baud - 1;

        HidPacketBuffer[0] := lo(prescaler);
        HidPacketBuffer[1] := hi(prescaler);
        HidPacketBuffer[2] := 24;
        HidPacketBuffer[3] := 0;

        usbasp_uart_set_conf(USBaspHIDList[USBaspIndex], HidPacketBuffer);

        WriteLn();
        WriteLn('USBasp device configuration');
        WriteLn('---------------------------');
        WriteLn('Serial number : ', USBaspHIDList[USBaspIndex].Serial);
        WriteLn('Crystal       : ', crystal);
        WriteLn('Baud          : ', baud);
        WriteLn();


        SendRingBuffer := TSPSCRingBuffer.Create(SendBufferSize);
        SendThread := TThreadUSBWrite.Create(USBaspHIDList[USBaspIndex], SendRingBuffer);
        try
          InputString := '';
          Sending := False;
          repeat
            if (not Sending) and KeyPressed then
            begin
              InputChar := ReadKey;
              if (InputChar <> #3) then
              begin
                Write(InputChar);
                if InputChar = #13 then
                begin
                  WriteLn;
                  Sending := True;
                end
                else
                  InputString := InputString + InputChar;
              end;
            end;
            if Sending then
            begin
              if InputString.Length > 0 then
              begin
                SndBytes := SendRingBuffer.Write(InputString[1], InputString.Length);
                InputString := InputString.Remove(0, SndBytes);
              end
              else
              begin
                Sending := False;
                InputChar:= #0;
              end;
            end;
            if InputString.Length = 0 then
              Sleep(2);
          until InputChar = #3;

          HidPacketBuffer[0] := 0;
          HidPacketBuffer[1] := 0;
          usbasp_uart_set_conf(USBaspHIDList[USBaspIndex], HidPacketBuffer);

        finally
          SendThread.Terminate;
          SendThread.WaitFor;
          SendThread.Free;

          SendRingBuffer.Free;
        end;

        usbasp_close(USBaspHIDList[USBaspIndex]);
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
    writeln(' -c  Set USBasp Crystal Hz ( default 12 MHz or 12000000 Hz )');
    writeln(' -s  Select USBasp with serial number.');
    writeln('     4 Digits numeric only i.e. 3456, 2222, etc ).');
    writeln(' -u  Serial Number to update ( 4 Digits numeric only i.e. 3456, 2222, etc ).');
    writeln('     Use with index -i when more than one USBasp are connected.');
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
    writeln('Interactive write to USBasp with serial number 1111 with 9600 baud');
    writeln(' USBaspHIDUART -s 1111 -w');
    writeln();
    writeln('Read from USBasp with 20 MHz crystal at index 1 with 19200 baud');
    writeln(' USBaspHIDUART -i 1 -b 19200 -c 20000000 -r');
    writeln();
    writeln('Read from USBasp with 20 MHz crystal and with serial number 2345 with 19200 baud');
    writeln(' USBaspHIDUART -s 2345 -b 19200 -c 20000000 -r');
    writeln();
    writeln('Update the first found USBasp''s serial number with 3456');
    writeln(' USBaspHIDUART -u 3456');
    writeln();
    writeln('Update the USBasp''s at index 3 serial number with 3456');
    writeln(' USBaspHIDUART -i 3 -u 3456');
  end;

var
  Application: TUSBaspHIDUARTTest;

//{$R *.res}

begin
  Application := TUSBaspHIDUARTTest.Create(nil);
  Application.Run;
  Application.Free;
end.
