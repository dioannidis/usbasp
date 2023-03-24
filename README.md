# usbasp improved
### _WCID compliant_, _HID UART support_

This USBasp firmware is based on [a fork by Ralph Doncaster]. Original fork readme can be found at README_nerdralph.md.

### Features
- From version 1.07 a default SCK clock of 1.5Mhz and automatic SCK slowing if target does not respond.  PORTD is left as input, so this firmware also works [with USBISP modules]. ( nerdralph )
- From version 1.08 the firmware is [WCID] compliant, meaning it should work on Windows without any driver or .inf install.
- From version 1.09 a ( reliable at 9600 Baud, with 120ms - 160ms intervals for higher speeds see note at UART HID protocol ) UART HID implementation is added for debugging purposes. USBasp will appear as a composite device with a WINUSB interface and a HID interface.
- From version 1.10 USBasp WCID switch to Microsoft OS 2.0 Descriptors. It seems that it play better with USB 3.0 ports.
- From version 1.11 USBasp supports updating it's serial number ( see [USBaspHIDUART utility] ).

### Avrdude

 Avrdude v7+ official windows binary from [avrdudes/avrdude] uses libwinusb instead of libusb, which doesn't support composite devices as this firmware implements. Please use the unofficial build using mingw64 ( avrdude-v7.1-mingw64 ) from [mcuee] repo.

### UART GUI Client

From release v0.7.0, the [libUSBUARTTerminal] supports this firmware's HID UART implementation.

### UART HID protocol

> Note: There is a small 128 byte ring buffer for both Tx and Rx. That means you can use higher baud rates ( i.e. 115200 ) if the message length is smaller than 128 bytes and the transmit / receive interval is 160 ms or higher.

##### _Serial Data_

V-USB implementation uses 8 byte size input and output interrupt reports.

The last byte ( 8th ) has special meaning. Its serial data or its the serial bytes count. If its value is greater than 7 then its serial data. If the value is 7 or smaller then its the serial data count and the remaining bytes are ignored.

_Input Reports ( USBasp -> USB PC )_ or _Output Reports ( USB PC -> USBasp)_

i.e.

```sh
0x55,0x34,0x00,0x00,0x00,0x00,0x00,0x02 -> Actual serial bytes 2 : 0x55,0x34

0x00,0x34,0x00,0x66,0x32,0x36,0x00,0x04 -> Actual serial bytes 4 : 0x00,0x34,0x00,0x66

0x00,0xC3,0x34,0x55,0x32,0xF3,0x00,0xAB -> Actual serial bytes 8 ( 8th byte > 7 ) : 0x00,0xC3,0x34,0x55,0x32,0xF3,0x00,0xAB
```

##### _UART Configuration_

The USBasp's UART configuration uses an 8 byte size feature report, with the following format.

| Byte 0   | Byte 1 | Byte 2 | Byte 3 | Byte 4 - 7  RO |
| -------- | --------- | -------- | -------- | -------- |
| Prescaler Low Byte | Prescaler High Byte | See [UART Flags] | Unused | USBasp Capabilities |

To setup and enable the UART, send a feature set report, with the prescaler in the first two bytes and the parity, data bit, stop bit flags at the third byte ( see [UART Flags] ). The fourth byte is ignored. 

To disable the UART, send a feature set report, with the prescaler bytes as zero.

> Note: The UART is disabled by default, if read or write is detected for do to not interfere with those functions.

### USBaspHIDUART utility

The USBaspHIDUART is a simple console utility, used for testing the HID UART implementation. 

This is the help page ( USBaspHIDUART.exe -h ) :

```sh

USBaspHIDUART -h

USBasp HIDUART Test App

 -l  List USBasp HID devices
 -i  Select USBasp index ( default 0 )
 -b  Set Baud ( default 9600 )
 -c  Set USBasp Crystal Hz ( default 12 MHz or 12000000 Hz )
 -s  Select USBasp with serial number.
     4 Digits numeric only i.e. 3456, 2222, etc ).
 -u  Serial Number to update ( 4 Digits numeric only i.e. 3456, 2222, etc ).
     Use with index -i when more than one USBasp are connected.
 -r  Continuous read input
 -w  Interactive send output

examples

Read from USBasp at index 0 with 4800 baud
 USBaspHIDUART -b 4800 -r

Interactive write to USBasp at index 1 with 9600 baud
 USBaspHIDUART -i 1 -w

Interactive write to USBasp with serial number 1111 with 9600 baud
 USBaspHIDUART -s 1111 -w

Read from USBasp with 20 MHz crystal at index 1 with 19200 baud
 USBaspHIDUART -i 1 -b 19200 -c 20000000 -r

Read from USBasp with 20 MHz crystal and with serial number 2345 with 19200 baud
 USBaspHIDUART -s 2345 -b 19200 -c 20000000 -r

Update the first found USBasp's serial number with 3456
 USBaspHIDUART -u 3456

Update the USBasp's at index 3 serial number with 3456
 USBaspHIDUART -i 3 -u 3456
```


[a fork by Ralph Doncaster]: <https://github.com/nerdralph/usbasp>
[with USBISP modules]: <https://www.sciencetronics.com/greenphotons/?p=938>
[WCID]: <https://github.com/pbatard/libwdi/wiki/WCID-Devices>
[MSYS2]: <https://www.msys2.org/>
[avr8-gnu-toolchain (3.6.2.17778)]: <https://www.microchip.com/en-us/tools-resources/develop/microchip-studio/gcc-compilers>
[UART Flags]: <https://github.com/dioannidis/usbasp/blob/167bf1c785b353cba206a0dbcc7d322f7f49d0b9/firmware/usbasp.h#L76)>
[mcuee]: <https://github.com/mcuee/avrdude/releases/tag/v7.1>
[avrdudes/avrdude]: <https://github.com/avrdudes/avrdude>
[libUSBUARTTerminal]: <https://github.com/dioannidis/libUSBUARTTerminal>
[USBaspHIDUART utility]: <https://github.com/dioannidis/usbasp#usbasphiduart-utility>
