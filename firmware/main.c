/*
 * USBasp - USB in-circuit programmer for Atmel AVR controllers
 *
 * Thomas Fischl <tfischl@gmx.de>
 * 2020 fixes and tweaks by Ralph Doncaster (Nerd Ralph)
 * 2021 WCID support by Dimitrios Chr. Ioannidis ( d.ioannidis@nephelae.eu )
  *     ( based on Marius Greuel's https://github.com/mariusgreuel/USBasp )
 * 2022 Composite WCID and HID by Dimitrios Chr. Ioannidis ( d.ioannidis@nephelae.eu )
 *
 * License........: GNU GPL v2 (see Readme.txt)
 * Target.........: ATMega8 at 12 MHz
 * Creation Date..: 2005-02-20
 * Last change....: 2020-11-26
 *
 */

#include <avr/io.h>
#include <avr/interrupt.h>
//#include <avr/pgmspace.h>
#include <avr/wdt.h>

#include "oddebug.h"
#include "usbasp.h"
#include "usbdrv.h"
#include "uart.h"
#include "usb_descriptors.h"
#include "isp.h"
#include "clock.h"
#include "tpi.h"
#include "tpi_defs.h"

static uchar replyBuffer[8];
static uchar interruptBuffer[8];
static uchar uartConf_Capabilities_FeatureReport[8] = { 0, 0, 0, 0, USBASP_CAP_0_TPI | USBASP_CAP_6_UART, 0, 0, 0};

static uchar prog_state = PROG_STATE_IDLE;
uchar prog_sck = USBASP_ISP_SCK_AUTO;

static uchar prog_address_newmode = 0;
static unsigned long prog_address;
static unsigned int prog_nbytes = 0;
static unsigned int prog_pagesize;
static uchar prog_blockflags;
static uchar prog_pagecounter;

/* USBasp default winusb driver for Windows.

    Based on the hard work by Marius Greuel @mariusgreuel ( https://github.com/mariusgreuel/USBasp ).
    This is a non intrusive version, using an unaltered V-USB with no changes in it's usbdrv code.

    To avoid using driver installation (Zadig, libusb) on Windows and use by default
    the winusb default driver for USBasp, we need to use OS feature descriptors. 
    All USB 2.0 devices, when they are enumerated for the first time, Windows asks if 
    there is an OS feature descriptor by sending a specific standard GET_DESCRIPTOR request
    with the format :

    --------------------------------------------------------------------------------------------------
    |    bmRequestType   |   bRequest        |   wValue  |   wIndex  |   wLength |   Data            |
    --------------------------------------------------------------------------------------------------
    |    1000 0000B      |   GET_DESCRIPTOR  |   0x03EE  |   0x0000  |   0x12    |   Returned string |
    --------------------------------------------------------------------------------------------------

    It asks if there is a specific string descriptor at index 0xEE. Because this string 
    descriptor request, is not by default handled by the V-USB, we change the USB_CFG_DESCR_PROPS_UNKNOWN
    to be dynamic (USB_PROP_IS_DYNAMIC). This effectively tell the V-USB that for every 
    unknown string index request to call the usbFunctionDescriptor function.

    usbFunctionDescriptor function returns an OS string descriptor using the version 1.00 format 
    which has a fixed length of 18 bytes, with a structure as shown in the following table :

    --------------------------------------------------------------------------
    |    Length  |   Type    |   Signature   |   MS Vendor Code  |   Pad     |
    --------------------------------------------------------------------------
    |    0x12    |   0x03    |   MSFT100     |   unsigned byte   |   0x00    |
    --------------------------------------------------------------------------

    Length: An unsigned byte and MUST be set to 0x12.

    Type: An unsigned byte and MUST be set to 0x03.

    Signature: A Unicode string and MUST be set to "MSFT100".

    MS Vendor Code: An unsigned byte, it will be used to retrieve associated feature descriptors.

    Pad: An unsigned byte and MUST be set to 0x00.

    The Signature field contains a Unicode character array that identifies the descriptor as an 
    OS string descriptor and includes the version number. For version 1.00, this array must be set 
    to "MSFT100" (0x4D00 0x5300 0x4600 0x5400 0x3100 0x3000 0x3000).

    The MS VendorCode field is used to retrieve the associated feature descriptors. This code is 
    used as Request field in TS_URB_CONTROL_VENDOR_OR_CLASS_REQUEST section 2.2.9.12.
    
    In usbFunctionSetup we handle the feature request associated to MS Vendor Code we replied earlier 
    and send an Extended Compat ID with the information that we want Windows to load the winusb default driver.

    For More information see

    https://docs.microsoft.com/en-us/windows-hardware/drivers/usbcon/microsoft-defined-usb-descriptors
    https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-rdpeusb/c2f351f9-84d2-4a1b-9fe3-a6ca195f84d0


    !!! Notice !!!

    "After the operating system requests a Microsoft OS String Descriptor from a device, it creates the following registry key:

    HLKM\SYSTEM\CurrentControlSet\Control\UsbFlags\vvvvpppprrrrr

    The operating system creates a registry entry, named osvc, under this registry key that indicates 
    whether the device supports Microsoft OS Descriptors. If the device does not provide a valid response 
    the first time that the operating system queries it for a Microsoft OS String Descriptor, 
    the operating system will make no further requests for that descriptor."
    
    If your firmware doesn't work please delete the registry key as stated above to retrigger a query from Windows.
    
    i.e. if your firmware has a device version of 0x07, 0x01 then there will be a registry key with the name :
    
    Computer\HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\usbflags\16C005DC0107
    
*/

usbMsgLen_t usbFunctionDescriptor(struct usbRequest *rq) {

    DBG1(0xFD, (uchar *)rq, sizeof(usbRequest_t));

    usbMsgLen_t len = 0;

    /* string (3) request at index 0xEE, is an OS string descriptor request */   

    if((rq->wValue.bytes[1] == USBDESCR_STRING) && (rq->wValue.bytes[0] == MS_1_0_OS_DESCRIPTOR_INDEX)) {
        usbMsgPtr = (usbMsgPtr_t)&MS_1_0_OS_STRING_DESCRIPTOR;
        len = sizeof(MS_1_0_OS_STRING_DESCRIPTOR);

    }

    return len;
};

usbMsgLen_t usbFunctionSetup(uchar data[8]) {

    DBG1(0xF5, data, 8);

    usbMsgLen_t len = 0;
     
    /* Device Requests */
     
    if ((data[0] & USBRQ_TYPE_MASK) == USBRQ_TYPE_VENDOR) {
        
        if((data[0] & USBRQ_RCPT_MASK) == USBRQ_RCPT_DEVICE) {
        
            if (data[1] == USBASP_FUNC_CONNECT) {
                uart_disable(); // make it not interefere.

                /* set SCK speed */
                ispSetSCKOption(prog_sck);

                /* set compatibility mode of address delivering */
                prog_address_newmode = 0;

                ledRedOn();
                ispConnect();

            } else if (data[1] == USBASP_FUNC_DISCONNECT) {
                ispDisconnect();
                ledRedOff();

            } else if (data[1] == USBASP_FUNC_TRANSMIT) {
                replyBuffer[0] = ispTransmit(data[2]);
                replyBuffer[1] = ispTransmit(data[3]);
                replyBuffer[2] = ispTransmit(data[4]);
                replyBuffer[3] = ispTransmit(data[5]);
                len = 4;

            } else if (data[1] == USBASP_FUNC_READFLASH) {

                if (!prog_address_newmode)
                    prog_address = (data[3] << 8) | data[2];

                prog_nbytes = (data[7] << 8) | data[6];
                prog_state = PROG_STATE_READFLASH;
                len = 0xff; /* multiple in */

            } else if (data[1] == USBASP_FUNC_READEEPROM) {

                if (!prog_address_newmode)
                    prog_address = (data[3] << 8) | data[2];

                prog_nbytes = (data[7] << 8) | data[6];
                prog_state = PROG_STATE_READEEPROM;
                len = 0xff; /* multiple in */

            } else if (data[1] == USBASP_FUNC_ENABLEPROG) {
                replyBuffer[0] = ispEnterProgrammingMode();
                len = 1;

            } else if (data[1] == USBASP_FUNC_WRITEFLASH) {
                
                if (!prog_address_newmode)
                    prog_address = (data[3] << 8) | data[2];

                prog_pagesize = data[4];
                prog_blockflags = data[5] & 0x0F;
                prog_pagesize += (((unsigned int) data[5] & 0xF0) << 4);
                if (prog_blockflags & PROG_BLOCKFLAG_FIRST) {
                    prog_pagecounter = prog_pagesize;
                }
                prog_nbytes = (data[7] << 8) | data[6];
                prog_state = PROG_STATE_WRITEFLASH;
                len = 0xff; /* multiple out */

            } else if (data[1] == USBASP_FUNC_WRITEEEPROM) {

                if (!prog_address_newmode)
                    prog_address = (data[3] << 8) | data[2];

                prog_pagesize = 0;
                prog_blockflags = 0;
                prog_nbytes = (data[7] << 8) | data[6];
                prog_state = PROG_STATE_WRITEEEPROM;
                len = 0xff; /* multiple out */

            } else if (data[1] == USBASP_FUNC_SETLONGADDRESS) {

                /* set new mode of address delivering (ignore address delivered in commands) */
                prog_address_newmode = 1;
                /* set new address */
                prog_address = *((unsigned long*) &data[2]);

            } else if (data[1] == USBASP_FUNC_SETISPSCK) {

                /* set sck option */
                prog_sck = data[2];
                replyBuffer[0] = 0;
                len = 1;

            } else if (data[1] == USBASP_FUNC_TPI_CONNECT) {
                tpi_dly_cnt = data[2] | (data[3] << 8);

                /* RST high */
                ISP_OUT |= (1 << ISP_RST);
                ISP_DDR |= (1 << ISP_RST);

                clockWait(3);

                /* RST low */
                ISP_OUT &= ~(1 << ISP_RST);
                ledRedOn();

                clockWait(16);
                tpi_init();

            } else if (data[1] == USBASP_FUNC_TPI_DISCONNECT) {

                tpi_send_byte(TPI_OP_SSTCS(TPISR));
                tpi_send_byte(0);

                clockWait(10);

                /* pulse RST */
                ISP_OUT |= (1 << ISP_RST);
                clockWait(5);
                ISP_OUT &= ~(1 << ISP_RST);
                clockWait(5);

                /* set all ISP pins inputs */
                ISP_DDR &= ~((1 << ISP_RST) | (1 << ISP_SCK) | (1 << ISP_MOSI));
                /* switch pullups off */
                ISP_OUT &= ~((1 << ISP_RST) | (1 << ISP_SCK) | (1 << ISP_MOSI));

                ledRedOff();
            
            } else if (data[1] == USBASP_FUNC_TPI_RAWREAD) {
                replyBuffer[0] = tpi_recv_byte();
                len = 1;

            } else if (data[1] == USBASP_FUNC_TPI_RAWWRITE) {
                tpi_send_byte(data[2]);

            } else if (data[1] == USBASP_FUNC_TPI_READBLOCK) {
                prog_address = (data[3] << 8) | data[2];
                prog_nbytes = (data[7] << 8) | data[6];
                prog_state = PROG_STATE_TPI_READ;
                len = 0xff; /* multiple in */

            } else if (data[1] == USBASP_FUNC_TPI_WRITEBLOCK) {
                prog_address = (data[3] << 8) | data[2];
                prog_nbytes = (data[7] << 8) | data[6];
                prog_state = PROG_STATE_TPI_WRITE;
                len = 0xff; /* multiple out */

            } else if(data[1] == USBASP_FUNC_GETCAPABILITIES) {
                 replyBuffer[0] = USBASP_CAP_0_TPI | USBASP_CAP_6_UART;
                 replyBuffer[1] = 0;
                 replyBuffer[2] = 0;
                 replyBuffer[3] = 0;
                 len = 4;

            /*  Handle the OS feature request associated with the MS Vendor Code
                we replied earlier in the OS String Descriptor request. See usbFunctionDescriptor. */
            } else if((data[1] == MS_1_0_VENDOR_CODE) &&
                    (data[4] == MS_1_0_EXTEND_COMPAT_ID_FEATURE_INDEX)) {

                        /* Send the Extended Compat ID OS feature descriptor, 
                        requesting to load the default winusb driver for us */
                        usbMsgFlags = USB_FLG_MSGPTR_IS_ROM;
                        usbMsgPtr = (usbMsgPtr_t)&MS_1_0_OS_EXTENDED_COMPAT_ID_FEATURE;
                        return sizeof(MS_1_0_OS_EXTENDED_COMPAT_ID_FEATURE);
                    }
               
        /* Device Requests for Interfaces */
               
        } else if((data[0] & USBRQ_RCPT_MASK) == USBRQ_RCPT_INTERFACE) { 

            switch(data[1]) {
                case MS_1_0_VENDOR_CODE:
                    if(data[4] == MS_1_0_EXTEND_PROPERTIES_FEATURE_INDEX) {
                         usbMsgFlags = USB_FLG_MSGPTR_IS_ROM;
                         switch(data[2]) {
                             case 0:
                                usbMsgPtr = (usbMsgPtr_t)&MS_1_0_OS_EXTENDED_PROPERTIES_FEATURE_INTF0;
                                return sizeof(MS_1_0_OS_EXTENDED_PROPERTIES_FEATURE_INTF0);
                             break;
                             case 1:
                                usbMsgPtr = (usbMsgPtr_t)&MS_1_0_OS_EXTENDED_PROPERTIES_FEATURE_INTF1;
                                return sizeof(MS_1_0_OS_EXTENDED_PROPERTIES_FEATURE_INTF1);
                             break;
                             default:
                             break;
                         }
                     }
                break;
                default:
                break;               
            }
            
        }

    /* Interface Requests */
        
    } else if((data[0] & USBRQ_TYPE_MASK) == USBRQ_TYPE_CLASS) {
        
        if((data[0] & USBRQ_RCPT_MASK) == USBRQ_RCPT_INTERFACE) {

            switch(data[1]) {
                case USBRQ_HID_GET_REPORT:
                    // wValue: ReportType (highbyte), ReportID (lowbyte)           
                    usbMsgPtr = uartConf_Capabilities_FeatureReport;
                    return sizeof(uartConf_Capabilities_FeatureReport);
                case USBRQ_HID_SET_REPORT: 
                    if (((data[6]<<8)|data[5]) != 0) {
                        prog_state = PROG_STATE_SET_REPORT;
                        len = 0xff; /* multiple in */
                    }
                default:
                break;               
            }
            
        }
    }
    
    usbMsgPtr = replyBuffer;

    return len;
}

uchar usbFunctionRead(uchar *data, uchar len) {

    uchar i;

    /* check if programmer is in correct read state */
    if ((prog_state != PROG_STATE_READFLASH) && (prog_state
            != PROG_STATE_READEEPROM) && (prog_state != PROG_STATE_TPI_READ)) {
        return 0xff;
    }

    /* fill packet TPI mode */
    if(prog_state == PROG_STATE_TPI_READ)
    {
        tpi_read_block(prog_address, data, len);
        prog_address += len;
        return len;
    }

    /* fill packet ISP mode */
    if((prog_state == PROG_STATE_READFLASH) || (prog_state == PROG_STATE_READEEPROM)) {
        for (i = 0; i < len; i++) {
            if (prog_state == PROG_STATE_READFLASH) {
                data[i] = ispReadFlash(prog_address);
            } else {
                data[i] = ispReadEEPROM(prog_address);
            }
            prog_address++;
        }
    }
    
    /* last packet? */
    if (len < 8) {
        prog_state = PROG_STATE_IDLE;
    }

    return len;
}

uchar usbFunctionWrite(uchar *data, uchar len) {

    uchar retVal = 0;
    uchar i;

    /* check if programmer is in correct write state */
    if ((prog_state != PROG_STATE_WRITEFLASH) && (prog_state
            != PROG_STATE_WRITEEEPROM) && (prog_state != PROG_STATE_TPI_WRITE)
            && (prog_state != PROG_STATE_SET_REPORT)) {
        return 0xff;
    }

    if (prog_state == PROG_STATE_TPI_WRITE)
    {
        tpi_write_block(prog_address, data, len);
        prog_address += len;
        prog_nbytes -= len;
        if(prog_nbytes <= 0)
        {
            prog_state = PROG_STATE_IDLE;
            return 1;
        }
        return 0;
    }

    if((prog_state == PROG_STATE_WRITEFLASH) || (prog_state == PROG_STATE_WRITEEEPROM)) {
        for (i = 0; i < len; i++) {

            if (prog_state == PROG_STATE_WRITEFLASH) {
                /* Flash */

                if (prog_pagesize == 0) {
                    /* not paged */
                    ispWriteFlash(prog_address, data[i], 1);
                } else {
                    /* paged */
                    ispWriteFlash(prog_address, data[i], 0);
                    prog_pagecounter--;
                    if (prog_pagecounter == 0) {
                        ispFlushPage(prog_address, data[i]);
                        prog_pagecounter = prog_pagesize;
                    }
                }

            } else {
                /* EEPROM */
                ispWriteEEPROM(prog_address, data[i]);
            }

            prog_nbytes--;

            if (prog_nbytes == 0) {
                prog_state = PROG_STATE_IDLE;
                if ((prog_blockflags & PROG_BLOCKFLAG_LAST) && (prog_pagecounter
                        != prog_pagesize)) {

                    /* last block and page flush pending, so flush it now */
                    ispFlushPage(prog_address, data[i]);
                }

                retVal = 1; // Need to return 1 when no more data is to be received
            }

            prog_address++;
        }
    }
    
    /* Feature report */
    if(prog_state == PROG_STATE_SET_REPORT) 
    {

        /*  The first 2 bytes are the uart prescaler ( low byte first then high byte second ) 
            UART settings. The 3rd byte is a bitmask for parity, stop bit and data bit. 
            The 4th byte is reserved for future.
            
            The last 4 bytes are the USBasp capabilities which are readonly. */
            
        for (i = 0; i < 3; i++) {            
            uartConf_Capabilities_FeatureReport[i] = *data != 0 ? *data : 0;
            data++;
        }
        
        /*  Disabling uart communication before applying new values. */            
        uart_disable();
        
        uint16_t baud  = (uartConf_Capabilities_FeatureReport[1]<<8)|uartConf_Capabilities_FeatureReport[0];
        uint8_t  par   = uartConf_Capabilities_FeatureReport[2] & USBASP_UART_PARITY_MASK;
        uint8_t  stop  = uartConf_Capabilities_FeatureReport[2] & USBASP_UART_STOP_MASK;
        uint8_t  bytes = uartConf_Capabilities_FeatureReport[2] & USBASP_UART_BYTES_MASK;
        
            
        /*  To enable the uart the baud needs to be non zero. Meaning that to disable the 
            uart communication just send a set feature report with the first 2 bytes with zero. */
        if (baud) {
            uart_config(baud, par, stop, bytes);
        }
        
        prog_state = PROG_STATE_IDLE;
        
        retVal = 1;
    }

    return retVal;
}

/*
 *
 *  As the input and output report are 8 bytes, we store 
 *  the actual serial bytes count at the last byte. The 
 *  remaining bytes are ignored as garbage. 
 *
 *  i.e.
 *
 *  Input or Output Report 
 *  
 *  0x55,0x34,0x00,0x00,0x00,0x00,0x00,0x02 -> Actual serial bytes 2 : 0x55,0x34
 *  
 *  0x00,0x34,0x00,0x66,0x32,0x36,0x00,0x04 -> Actual serial bytes 4 : 0x00,0x34,0x00,0x66
 *  
 */


/* Host to device. Endpoint 1 Output */
void usbFunctionWriteOut(uchar *data, uchar len){

/*
    AFAIU, interrupt out must be exactly 8 bytes long for USB 1.1 .
    As we must know how many bytes of the report are actual bytes 
    sended, the length is in the last byte. Effectively losing 1 byte.

    TODO: Fix this !

*/

        /* Actual serial bytes in output report */
        data[7] < 8 ? len = data[7] : 0;
        
        /*  If UART enabled ( RXCIE enabled ) 
            is there serial data in the report ? */
        if(len && (USBASPUART_UCSRB & (1<<USBASPUART_RXCIE))) {

            /* If the transmit buffer is near full, disable usb requests
               until the transmit buffer is empty. We rely on 
               usb trasmit retries to not lose any data. */
            if((CBUF_Len(tx_Q)) + len > (tx_Q_SIZE - 8)) {
                usbDisableAllRequests();                
            }
                                   
            do{
                *CBUF_GetPushEntryPtr(tx_Q) = *data++;
                CBUF_AdvancePushIdx(tx_Q);                                              
            }while(--len);
                        
        }
        
}

/* Device to host. Endpoint 1 Input */
void HID_EP_1_IN(){
/*
    AFAIU, interrupt requests must be exactly 8 bytes long for USB 1.1 .
    As we must inform the receiver how many serial bytes we return, we 
    store the actual length in the last byte. Effectively losing 1 byte.

    TODO: Fix this !

*/
    /* As we don't use EEPROM use the EEAR register as variable */
    EEAR = 0;
    
    while((!(CBUF_IsEmpty(rx_Q))) && (EEAR != 7)){
        interruptBuffer[EEAR++] = CBUF_Get(rx_Q, 0);
        CBUF_AdvancePopIdx(rx_Q);
    }

    /* Actual serial bytes in input report */
    interruptBuffer[7] = EEAR;
    
    usbSetInterrupt(interruptBuffer, sizeof(interruptBuffer));
}

int main(void) {

    /* enable debug if DEBUG_LEVEL > 0 */
    odDebugInit();

    /* init timer */
    clockInit();

    /* output SE0 for USB reset */
    DDRB = ~0;
    clockWait(10 / 0.320);              /* 10ms */
    /* all USB and ISP pins inputs to end USB reset */
    DDRB = 0;

    /* USBasp active */
    ledGreenOn();

    /* main event loop */
    usbInit();
     
    sei();
    for (;;) {
        
        
        /*  Enable transmit interrupt if tx buffer has data
            and the transmit interrupt is disabled. */
        if(!(USBASPUART_UCSRB & (1<<USBASPUART_UDRIE)) 
            && !CBUF_IsEmpty(tx_Q)) {
            
            USBASPUART_UCSRB |= (1<<USBASPUART_UDRIE);
            
        /*  Reenable USB requests if they are 
            disabled and tx buffer is empty. */
        } else if(CBUF_IsEmpty(tx_Q)) {          
            if(usbAllRequestsAreDisabled()){
                usbEnableAllRequests();
            }
        }
                
        usbPoll();
        if (usbInterruptIsReady()) {
            HID_EP_1_IN();
        }

    }

    return 0;
}