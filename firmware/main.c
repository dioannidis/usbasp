/*
 * USBasp - USB in-circuit programmer for Atmel AVR controllers
 *
 * Thomas Fischl <tfischl@gmx.de>
 * 2020 fixes and tweaks by Ralph Doncaster (Nerd Ralph)
 * 2021 WCID support by Dimitrios Chr. Ioannidis ( d.ioannidis@nephelae.eu )
 *      ( based on Marius Greuel's https://github.com/mariusgreuel/USBasp )
 * 2022 Composite WCID and HID by Dimitrios Chr. Ioannidis ( d.ioannidis@nephelae.eu )
 * 2023 Serial Number write via HID and 
 *      descriptors stored in EEPROM by Dimitrios Chr. Ioannidis ( d.ioannidis@nephelae.eu )
 *
 * License........: GNU GPL v2 (see Readme.txt)
 * Target.........: ATMega8 at 12 MHz
 * Creation Date..: 2005-02-20
 * Last change....: 2023-03-22
 *
 */

#include <avr/io.h>
#include <avr/interrupt.h>
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
#include "serialnumber.h"

#if F_CPU == 12000000L
    #define CAP_CLOCK USBASP_CAP_12MHZ_CLOCK
#elif F_CPU == 16000000L
    /* ATmega8 max speed */
    #define CAP_CLOCK USBASP_CAP_16MHZ_CLOCK
#elif F_CPU == 18000000L
    #define CAP_CLOCK USBASP_CAP_18MHZ_CLOCK
#elif F_CPU == 20000000L
    #define CAP_CLOCK USBASP_CAP_20MHZ_CLOCK
#endif

static uchar featureReport[8] = {
    0,                                             /* Prescaler Low byte */
    0,                                             /* Prescaler High byte */
    0,                                             /* Bitmask Parity, StopBit and DataBit */
    0,                                             /* Reserved */
    USBASP_CAP_0_TPI | USBASP_CAP_HIDUART | USBASP_CAP_SNHIDUPDATE,         /* Device Capabilities */
    CAP_CLOCK,                                     /* Device Crystal      */
    0,                                             /* Reserved            */
    0                                              /* Reserved            */
  };

static uchar replyBuffer[8];
static uchar interruptBuffer[8];
static uchar monitorBuffer[8];

static uchar uart_state = UART_STATE_DISABLED;

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
    
    Move from Microsoft OS 1.0 Descriptors to Microsoft OS 2.0 Descriptors. This configuration MUST have a serial to work. 
    
    Also it seems to work with USB 3.0 ports ( at least it seems to work in my limited testing ).
    
    TODO: Write comments how it works ...

*/

usbMsgLen_t usbFunctionDescriptor(struct usbRequest *rq) {

    DBG1(0xFD, (uchar *)rq, sizeof(usbRequest_t));

    usbMsgLen_t len = 0;

    /* BOS Descriptor */
    if((rq->wValue.bytes[1] == USBDESCR_BOS) && (rq->wValue.bytes[0] == 0x00)) {
        usbMsgPtr = (usbMsgPtr_t)&BOS_DESCRIPTOR;
        len = sizeof(BOS_DESCRIPTOR);
    }

    return len;
};

usbMsgLen_t usbFunctionSetup(uchar data[8]) {

    DBG1(0xF1, data, 8);

    usbMsgLen_t len = 0;

    /* Device Requests */

    if ((data[0] & USBRQ_TYPE_MASK) == USBRQ_TYPE_VENDOR) {

        if((data[0] & USBRQ_RCPT_MASK) == USBRQ_RCPT_DEVICE) {

            if (data[1] == USBASP_FUNC_CONNECT) {

                uart_state = uart_disable(); // make it not interfere.

                /* set SCK speed */
                ispSetSCKOption(prog_sck);

                /* set compatibility mode of address delivering */
                prog_address_newmode = 0;

                ledRedOn();
                ispConnect();

            } else if (data[1] == USBASP_FUNC_DISCONNECT) {
                ispDisconnect();
                ledRedOff();
  
                /*  If the prescaler "baud" is non zero then it means that
                UART was open and it was interrupted by an ISP connect command.
                Re enable the UART */

                uart_state = uart_config(featureReport);

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
                len = USB_NO_MSG; /* multiple in */

            } else if (data[1] == USBASP_FUNC_READEEPROM) {
                if (!prog_address_newmode)
                    prog_address = (data[3] << 8) | data[2];

                prog_nbytes = (data[7] << 8) | data[6];
                prog_state = PROG_STATE_READEEPROM;
                len = USB_NO_MSG; /* multiple in */

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
                len = USB_NO_MSG; /* multiple out */

            } else if (data[1] == USBASP_FUNC_WRITEEEPROM) {
                if (!prog_address_newmode)
                    prog_address = (data[3] << 8) | data[2];

                prog_pagesize = 0;
                prog_blockflags = 0;
                prog_nbytes = (data[7] << 8) | data[6];
                prog_state = PROG_STATE_WRITEEEPROM;
                len = USB_NO_MSG; /* multiple out */

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
                len = USB_NO_MSG; /* multiple in */

            } else if (data[1] == USBASP_FUNC_TPI_WRITEBLOCK) {
                prog_address = (data[3] << 8) | data[2];
                prog_nbytes = (data[7] << 8) | data[6];
                prog_state = PROG_STATE_TPI_WRITE;
                len = USB_NO_MSG; /* multiple out */

            } else if(data[1] == USBASP_FUNC_GETCAPABILITIES) {
                 replyBuffer[0] = featureReport[4];
                 replyBuffer[1] = featureReport[5];
                 replyBuffer[2] = 0;
                 replyBuffer[3] = 0;
                 len = 4;

            /*  Handle the BOS request associated with the MS Vendor Code
                we replied earlier in the BOS Descriptor request. See usbFunctionDescriptor. */
            } else if((data[1] == VENDOR_CODE) &&
                    (data[4] == MS_OS_2_0_DESCRIPTOR_INDEX)) {
        
                        usbMsgFlags = USB_FLG_MSGPTR_IS_ROM;
                        usbMsgPtr = (usbMsgPtr_t)&MS_2_0_OS_DESCRIPTOR_SET;
                        len = sizeof(MS_2_0_OS_DESCRIPTOR_SET);
                        goto dontAssMsgPtr;

                    }
            
        }

    /* Interface Requests */

    } else if((data[0] & USBRQ_TYPE_MASK) == USBRQ_TYPE_CLASS){

        if((data[0] & USBRQ_RCPT_MASK) == USBRQ_RCPT_INTERFACE){

            switch(data[3]) { // wValue: ReportType (highbyte), ReportID (lowbyte)
                case 3 : // Feature Report
                    switch(data[1]) {
                        case USBRQ_HID_GET_REPORT:

                            usbMsgPtr = (usbMsgPtr_t)&featureReport;
                            len = sizeof(featureReport);
                            goto dontAssMsgPtr;

                        case USBRQ_HID_SET_REPORT:

                            if (((data[6]<<8)|data[5]) != 0){

                                prog_state = PROG_STATE_SET_REPORT;
                                len = USB_NO_MSG; /* multiple in */

                            }
                            break;
                        default:
                            break;
                    }
                    break;
                default:
                    break;
            }
        }
    }

    usbMsgPtr = (usbMsgPtr_t)&replyBuffer;

dontAssMsgPtr:

    return len;
}

uchar usbFunctionRead(uchar *data, uchar len) {

    DBG1(0xF2, data, len);

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
            prog_nbytes--;
        }
    }

    /* last packet? */
    if ((len < 8) || (prog_nbytes == 0))  {
        prog_state = PROG_STATE_IDLE;
    }

    return len;
}

uchar usbFunctionWrite(uchar *data, uchar len) {

    DBG1(0xF3, data, len);
    
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

        switch (data[3]) {
            case 0: {                                             

    /*  The first 2 bytes are the uart prescaler ( low byte first then high byte second ) 
        UART settings. The 3rd byte is a bitmask for parity, stop bit and data bit. 
        The 4th byte is reserved for future.
            
        The last 4 bytes are the USBasp capabilities which are readonly. */

                    featureReport[0] = data[0];
                    featureReport[1] = data[1];
                    featureReport[2] = data[2];

    /*  To enable the UART the baud needs to be non zero. 
        Meaning that to disable the UART communication send a set feature report 
        with the prescaler ( first 2 bytes ) zeroed. */
        
                    uart_state = uart_config(featureReport);

                }
                break;
            case 1: {
                
                    serialNumberWrite(data);
                
                }
                break;
            default:
                break;
        }

        prog_state = PROG_STATE_IDLE;

        retVal = 1;
    }

    return retVal;
}

/*
 *
 * The V-USB uses 8 byte size input and output interrupt reports.
 *
 * The last byte ( 8th ) has special meaning. Its serial data or its the serial bytes count. If its value is greater than 7 then * its serial data. If the value is 7 or smaller then its the serial data count and the remaining bytes are ignored.
 *
 *  i.e.
 *
 *  Input or Output Report 
 *  
 *  0x55,0x34,0x00,0x00,0x00,0x00,0x00,0x02 -> Actual serial bytes 2 : 0x55,0x34
 *  
 *  0x00,0x34,0x00,0x66,0x32,0x36,0x00,0x04 -> Actual serial bytes 4 : 0x00,0x34,0x00,0x66
 *  
 *  0x00,0xC3,0x34,0x55,0x32,0xF3,0x00,0xAB -> Actual serial bytes 8 ( 8th byte > 7 ) : 0x00,0xC3,0x34,0x55,0x32,0xF3,0x00,0xAB
 *
 */

/* Host to device. Endpoint 1 Output */
void usbFunctionWriteOut(uchar *data, uchar len){

    DBG1(0xF4, data, 8);

    /*  The 8th byte ( data[7] ), holds the serial bytes count or
        is actual serial data, depending on the folowing condition:

        If the next byte value in the buffer is greater than 7
        then we treated it as serial data and added it to the out 
        report else the 8th byte holds the serial data count. This
        way the receiver can distinguish the meaning of the 8th byte
        if it is serial data or actually the serial data count.                
        
        With this algorithm we can reliably send 700 - 800 bytes per second
        ( the probability to have 100 7 value at the 8th place of report
        is very low. So the 700 bytes/s is a very rare worst case scenario ), 
        if the host controller polls every 10ms as the spec for USB 1.1 says. 
        But in my tests the host controller or the USB 2.0 hub poll the
        device every ~8 ms, effectively increasing the amount of bytes to
        send to 875 - 1000 bytes/s ( again below 970 bytes/s is a rare case )
        which is enough (more or less) for 9600 Baud ( real speed 960 Bytes/s ).
    */

        if (data[7] > 0) {
            if (data[7] < 8) {
                len = data[7];
            }
        } else {
            len = 0;
        }
        
    /*  If UART enabled ( RXCIE enabled ) 
        is there serial data in the report ? */
        if(len && (USBASPUART_UCSRB & (1<<USBASPUART_RXCIE))) {

         /* If the transmit buffer is near full, disable usb requests
            until the transmit buffer is empty. */
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

    uint8_t count = 0;
    
    /*  We fill the first 7 bytes of the report from
        the receive buffer if they are exist. */
    while((!(CBUF_IsEmpty(rx_Q))) && (count != 7)){
        interruptBuffer[count++] = CBUF_Get(rx_Q, 0);
        CBUF_AdvancePopIdx(rx_Q);
    }
    
    interruptBuffer[7] = count;

    /*  The 8th byte ( interruptBuffer[7] ), holds the serial bytes count or
        is actual serial data, depending on the folowing condition:

        If the next byte value in the buffer is greater than 7
        then we treated it as serial data and added it to the out 
        report else the 8th byte holds the serial data count. This
        way the receiver can distinguish the meaning of the 8th byte
        if it is serial data or actually the serial data count.                
        
        With this algorithm we can reliably send 700 - 800 bytes per second
        ( the probability to have 100 7 value at the 8th place of report
        is very low. So the 700 bytes/s is a very rare worst case scenario ), 
        if the host controller polls every 10ms as the spec for USB 1.1 says. 
        But in my tests the host controller or the USB 2.0 hub poll the
        device every ~8 ms, effectively increasing the amount of bytes to
        send to 875 - 1000 bytes/s ( again below 970 bytes/s is a rare case )
        which is enough (more or less) for 9600 Baud ( real speed 960 Bytes/s ).
    */
        
    if(!(CBUF_IsEmpty(rx_Q)) && (count == 7)){
        uint8_t tmp = CBUF_Get(rx_Q, 0);
        if(tmp > count) {
            interruptBuffer[count] = tmp;
            CBUF_AdvancePopIdx(rx_Q);        
        }
    }

    usbSetInterrupt(interruptBuffer, sizeof(interruptBuffer));
}

/* Device to host. Endpoint 2 Input */
void HID_EP_3_IN(){

    // monitorBuffer[0] = 0;
    // monitorBuffer[1] = 0;
    // monitorBuffer[2] = 0;
    // monitorBuffer[3] = 0;
    // monitorBuffer[4] = 0;
    // monitorBuffer[5] = 0;
    // monitorBuffer[6] = 0;
    monitorBuffer[7] = prog_state | uart_state;
 
    usbSetInterrupt3(monitorBuffer, sizeof(monitorBuffer));
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

        if (usbInterruptIsReady3()) {
            HID_EP_3_IN();
        }

    }

    return 0;
}