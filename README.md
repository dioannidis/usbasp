# usbasp improved

USBasp firmware based on <a href="https://github.com/bperrybap/usbasp">a fork by Bill Perry</a> and the latest <a href="https://github.com/obdev/v-usb">v-usb</a>.

Firmware version 1.06 (2020-09-14) features a default SCK clock of 1.5Mhz and automatic SCK slowing if target does not respond.  PORTD is left as input, so this firmware also works <a href="https://www.sciencetronics.com/greenphotons/?p=938">with USBISP modules</a>.

Thanks to <a href="https://github.com/nerdralph/usbasp/pull/5">patches from Dimitrios</a>, the USBasp firmware 1.08 is WCID compliant, meaning it should work on Windows without any driver or .inf install.

A pre-built hex file for the mega8 (main.hex) is in the firmware directory, along with m88.hex for the mega88.

## development plans
Recognize when the target device is in debugWIRE mode, and send the dW command to enter ISP mode.

