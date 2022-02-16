# usbasp improved
### _WCID compliant_, _HID UART support_

This USBasp firmware is based on [a fork by Ralph Doncaster](https://github.com/nerdralph/usbasp). Original fork readme can be found at README_fork.md.

### Features

- From version 1.07 a default SCK clock of 1.5Mhz and automatic SCK slowing if target does not respond.  PORTD is left as input, so this firmware also works [with USBISP modules](https://www.sciencetronics.com/greenphotons/?p=938). ( nerdralph )
- From version 1.08 the firmware is WCID compliant, meaning it should work on Windows without any driver or .inf install.
- From version 1.09 a slow ( 4800 Baud ) uart HID implementaion added for debugging purposes.

A pre-built hex file for the mega8 (main.hex) is in the firmware directory, along with m88.hex for the mega88.