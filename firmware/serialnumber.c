/*
 * serialnumber.c - part of USBasp
 *
 * Autor..........: Dimitrios Chr. Ioannidis ( d.ioannidis@nephelae.eu )
 * Description....: Provides functions for updating the Serial Number
                    using HID
 * Licence........: GNU GPL v2 (see Readme.txt)
 * Creation Date..: 2023-04-01
 * Last change....: 2023-04-11
 */

#include "usbdrv.h"
#include "serialnumber.h"

void serialNumberWrite(uchar *reportData) {
    
    uchar i;
    
    unsigned tmp = (reportData[1] << 8) | reportData[0];
    
    for (i=4; i >= 1; i--)
        {
            eeprom_update_byte(((uint8_t *)&usbDescriptorStringSerialNumber + i*2), 48 + tmp%10);
            tmp /= 10;
        }     
}