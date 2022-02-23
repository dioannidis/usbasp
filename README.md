# usbasp improved
### _WCID compliant_, _HID UART support_

This USBasp firmware is based on [a fork by Ralph Doncaster]. Original fork readme can be found at README_nerdralph.md.

### Features
- From version 1.07 a default SCK clock of 1.5Mhz and automatic SCK slowing if target does not respond.  PORTD is left as input, so this firmware also works [with USBISP modules]. ( nerdralph )
- From version 1.08 the firmware is [WCID] compliant, meaning it should work on Windows without any driver or .inf install.
##### _( beta )_
- From version 1.09 a slow ( 9600 Baud ) UART HID implementation will be added for debugging purposes. USBasp will appear as a composite device with a WINUSB interface and a HID interface.

> Note: A pre-built hex file for the mega8 (main.hex) is in the firmware directory, along with m88.hex for the mega88.

### Avrdude

Pre-built avrdude 6.3 and 6.4, windows executables, if needed, is in the bin\avrdude-winusb directory. Build with [MSYS2] and [avr8-gnu-toolchain (3.6.2.17778)] from Microchip, on a Windows 10 box.
### UART HID protocol


##### _Serial Data_

UART HID implementation uses 8 byte size input and output reports. 

The last byte ( 8th ) has special meaning different for input reports and output reports.

_Input Reports ( USBasp -> USB PC )_

It  holds the actual serial bytes count. The remaining bytes are ignored. If its value is greater than 7 then it is serial data. If the value is 7 or smaller then its the serial data count.

i.e.

```sh
0x55,0x34,0x00,0x00,0x00,0x00,0x00,0x02 -> Actual serial bytes 2 : 0x55,0x34

0x00,0x34,0x00,0x66,0x32,0x36,0x00,0x04 -> Actual serial bytes 4 : 0x00,0x34,0x00,0x66

0x00,0xC3,0x34,0x55,0x32,0xF3,0x00,0xAB -> Actual serial bytes 8 ( 8th byte > 7 ) : 0x00,0xC3,0x34,0x55,0x32,0xF3,0x00,0xAB
```

_Output Reports ( USB PC -> USBasp)_

The last byte ( 8th ) holds the actual serial bytes count. The remaining bytes are ignored.

i.e.

```sh
0x55,0x34,0x00,0x00,0x00,0x00,0x00,0x02 -> Actual serial bytes 2 : 0x55,0x34

0x00,0x34,0x00,0x66,0x32,0x36,0x00,0x04 -> Actual serial bytes 4 : 0x00,0x34,0x00,0x66
```

##### _UART Configuration_
The USBasp's UART configuration uses a 1 byte size feature report, with the following format.

| Byte 0   | Byte 1 | Byte 2 | Byte 3 | Byte 4 - 7  RO |
| -------- | --------- | -------- | -------- | -------- |
| Prescaler Low Byte | Prescaler High Byte | See [UART Flags] | Unused | USBasp Capabilities |

To setup and enable the UART, send a feature set report, with the prescaler in the first two bytes and the parity, data bit, stop bit flags at the third byte ( see [UART Flags] ). The fourth byte is ignored. 

To disable the UART, send a feature set report, with the prescaler bytes as zero.

> Note: The UART is disabled by default, if read or write is detected for do to not interfere with those functions.

[a fork by Ralph Doncaster]: <https://github.com/nerdralph/usbasp>
[with USBISP modules]: <https://www.sciencetronics.com/greenphotons/?p=938>
[WCID]: <https://github.com/pbatard/libwdi/wiki/WCID-Devices>
[MSYS2]: <https://www.msys2.org/>
[avr8-gnu-toolchain (3.6.2.17778)]: <https://www.microchip.com/en-us/tools-resources/develop/microchip-studio/gcc-compilers>
[UART Flags]: <https://github.com/dioannidis/usbasp/blob/167bf1c785b353cba206a0dbcc7d322f7f49d0b9/firmware/usbasp.h#L76)>