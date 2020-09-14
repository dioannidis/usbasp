# usbasp improved

USBasp firmware based on <a href="https://github.com/bperrybap/usbasp">a fork by Bill Perry</a> and the latest <a href="https://github.com/obdev/v-usb">v-usb</a>.

Firmware version 1.06 (2020-09-14) features a default SCK clock of 1.5Mhz and automatic SCK slowing if target does not respond.  PORTD is left as innput, so this firmware also works <a href="https://www.sciencetronics.com/greenphotons/?p=938">with USBISP modules</a>.

## development plans
Recognize when the target device is in debugWIRE mode, and send the dW command to enter ISP mode.

