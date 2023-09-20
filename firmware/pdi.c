/*
* pdi.c - part of USBasp
*
* Author.........: szu ( from http://szulat.blogspot.com/ )
* Description....: Provides functions for communication/programming
*                  over PDI interface
* Licence........: GNU GPL v2 (see Readme.txt)
* Creation Date..: 2012-08-15
* Last change....: 2012-09-18
*/

#include "pdi.h"
#include "usbasp.h"
#include "usbdrv.h"
#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/pgmspace.h>
#include <util/parity.h>
#include "usbconfig.h"
#include <string.h>
#include "xmega_pdi.h"

#ifndef F_CPU
#define F_CPU (USB_CFG_CLOCK_KHZ*1000)
#define F_CPU 12000000
#endif
#include <util/delay.h>

/* From V-USB usbdrv.h

Interrupt latency:
The application must ensure that the USB interrupt is not disabled for more
than 25 cycles (this is for 12 MHz, faster clocks allow longer latency).
This implies that all interrupt routines must either have the "ISR_NOBLOCK"
attribute set (see "avr/interrupt.h") or be written in assembler with "sei"
as the first instruction.

Maximum interrupt duration / CPU cycle consumption:
The driver handles all USB communication during the interrupt service
routine. The routine will not return before an entire USB message is received
and the reply is sent. This may be up to ca. 1200 cycles @ 12 MHz (= 100us) if
the host conforms to the standard. The driver will consume CPU cycles for all
USB messages, even if they address another (low-speed) device on the same bus.

*/

/* From Atmel-8331-8-and-16-bit-AVR-Microcontroller-XMEGA-AU_Manual.pdf

32.3.2 Disabling
If the clock frequency on PDI_CLK is lower than approximately 10kHz, this is regarded as inactivity on the clock line. This will automatically disable the PDI. If not disabled by a fuse, the reset function of the Reset (PDI_CLK) pin is enabled again. This also means that the minimum programming frequency is approximately 10kHz

*/

uchar pdi_nvmbusy=0;

PROGMEM const uchar pdi_key[8]={0xFF,0x88,0xD8,0xCD,0x45,0xAB,0x89,0x12};

#define pdiIdleMode         0
#define pdiTransmitMode     1
#define pdiReceiveMode      2

volatile uchar pdiState = pdiIdleMode;

volatile uchar pdiDataByte;
uchar pdiParity;

volatile uchar pdiTransmitData = 0;
uchar pdiSPIState = 1;

#define pdiFrameTransmitStartBit        2
#define pdiFrameTrasmitDataByte         4
#define pdiFrameTransmitParityStopBit   8
#define pdiFrameTransmitFinish          16

volatile uchar pdiReceiveData = 0;
volatile uchar pdiTimeout;
volatile uchar pdiDataByteOne;
volatile uchar pdiDataByteTwo;
volatile uchar pdiDataByteThree;

#define pdiReceiveStart             2
#define pdiReceiveFirstByte         4
#define pdiReceiveSecondByte        8
#define pdiReceiveThirdByte         16
#define pdiReceiveFinish            32

volatile uchar pdiEnableCount = 0;

ISR(SPI_STC_vect, ISR_NOBLOCK) {

    if (pdiState == pdiTransmitMode) {

        if (pdiTransmitData){

            pdiSPIState = (pdiSPIState << 1);

            if (pdiSPIState == pdiFrameTransmitStartBit) {
                PDI_OUT |= (1 << PDI_MOSI);
                PDI_DDR |= (1 << PDI_MOSI);
                SPDR = 0x7F;
            } else if (pdiSPIState == pdiFrameTrasmitDataByte) {
                SPDR = pdiDataByte;
            } else if (pdiSPIState == pdiFrameTransmitParityStopBit) {
                if (pdiParity) {
                    SPDR = 0xFF;
                } else {
                    SPDR = 0xFE;
                }
            } else if (pdiSPIState == pdiFrameTransmitFinish) {
                pdiSPIState = 1;
                pdiState = 0;
                pdiTransmitData = 0;
                PDI_DDR &= ~(1 << PDI_MOSI);
                PDI_OUT &= ~(1 << PDI_MOSI);
            }
            
        }

    } else if (pdiState == pdiReceiveMode) {


        if (pdiReceiveData) {

            pdiTimeout--;
            pdiSPIState = (pdiSPIState << 1);

            if (pdiSPIState == pdiReceiveStart){
                PDI_OUT &= ~(1 << PDI_RST);
            } else if (pdiSPIState == pdiReceiveFirstByte) {
                pdiDataByteOne = SPDR;
                if (pdiDataByteOne == 0xFF) {
                    pdiSPIState = pdiReceiveStart;
                }
                if (!pdiTimeout) pdiSPIState = pdiReceiveThirdByte;
            } else if (pdiSPIState == pdiReceiveSecondByte) {
                pdiDataByteTwo = SPDR;
            } else if (pdiSPIState == pdiReceiveThirdByte) {
                pdiDataByteThree = SPDR;
                PDI_OUT |= (1 << PDI_RST);
            } else if (pdiSPIState == pdiReceiveFinish) {
                pdiSPIState = 1;
                pdiState = 0;
                pdiReceiveData = 0;
            }

        }
    }


    if(!pdiEnableCount) {
        if ((!pdiState) || (pdiState == pdiReceiveMode)) {
            SPDR = 0x00;
        }
    } else {
        SPDR = 0xFF;
        pdiEnableCount--;
    }
}

void pdiSendByte(uchar byte)
{    
    while(pdiState);
    pdiDataByte = byte;
    pdiParity = parity_even_bit(byte);

    pdiTransmitData = 1;
    pdiState = pdiTransmitMode;
    usbDisableAllRequests();

}

uint8_t rcvdByte = 0;
uint8_t rcvdByteIndex = 0;
uint8_t foundStartBit = 0;

void extractByteFromSPI(uchar byte) {

    uint8_t mask;
    for (mask = (1 << 7); mask != 0; mask >>= 1) {

        if (!foundStartBit) {
            if (byte & mask) {
                continue;
            } else {
                foundStartBit = 1;
                continue;
            }
        }

        if (rcvdByteIndex++ < 8) {
            rcvdByte = rcvdByte << 1;
            if (byte & mask) {
                rcvdByte++;
            }
        } else {
            break;
        }
    }

}

uchar reverseByte(uchar byte)
{
    uchar rvrsByte = 0;
    if (byte & 0x01) rvrsByte |= 0x80;
    if (byte & 0x02) rvrsByte |= 0x40;
    if (byte & 0x04) rvrsByte |= 0x20;
    if (byte & 0x08) rvrsByte |= 0x10;
    if (byte & 0x10) rvrsByte |= 0x08;
    if (byte & 0x20) rvrsByte |= 0x04;
    if (byte & 0x40) rvrsByte |= 0x02;
    if (byte & 0x80) rvrsByte |= 0x01;
    return(rvrsByte);
}

uchar pdiReadByte(uchar timeout,uchar *result)
{

    pdiTimeout = timeout;

    while(pdiState);
    pdiReceiveData = 1;
    pdiState = pdiReceiveMode;    
    usbDisableAllRequests();

    while(pdiState);

    if (!pdiTimeout) {

        return PDI_STATUS_TIMEOUT;

    } else {

        rcvdByte = 0;
        rcvdByteIndex = 0;
        foundStartBit = 0;

        extractByteFromSPI(reverseByte(pdiDataByteOne));
        extractByteFromSPI(reverseByte(pdiDataByteTwo));
        extractByteFromSPI(reverseByte(pdiDataByteThree));

        *result = reverseByte(rcvdByte);
        return PDI_STATUS_OK;

    }

}

uchar pdiConnect()
{

    /* AFAIU, the only way to connect MOSI and MISO together 
    * for PDI communication, is to tri-state the pins and enable 
    * them when they're needed ( Tx - MOSI, Rx - MISO ), as the
    * PDI_DATA pin has an internal pull resistor when PDI is enabled.
    *
    * Now, when the SPI is enabled in Master Mode,  the input pin 
    * (MISO) is automatically configured, and, AFAIU, the only way
    * to tri-state it, during SPI transmision, is to use a buffer 
    * for to provide HiZ ( i.e. one port of a 74HCT125N ).
    */
    
    /* Tri-state MISO pin connected to 74HCT125N using
    *  SS pin which is also connected to 74HCT125N . */
    PDI_DDR |= (1 << PDI_RST);
    PDI_OUT |= (1 << PDI_RST);

    /* enable pullup on MISO for improved noise immunity */
    PDI_OUT |= (1 << PDI_MISO);

    /* Transmit  */ 
    pdiEnableCount = 6 + 1;

    SPCR = (1 << SPR1) | (1 << SPR0) | (1 << SPE) | (1 << MSTR) | (1 << SPIE) | (1 << CPOL) | (1 << CPHA) | (1 << DORD);

    PDI_DDR |= (1 << PDI_MOSI);
    PDI_OUT &= ~(1 << PDI_MOSI);
    _delay_ms(5);

    SPDR = 0xFF;

    _delay_us(5);
    PDI_OUT &= ~(1 << PDI_SCK);
    PDI_DDR |= (1 << PDI_SCK);

    while(pdiEnableCount);

    /* MOSI HiZ */
    PDI_DDR &= ~(1 << PDI_MOSI);
    PDI_OUT &= ~(1 << PDI_MOSI);
    

    // Guard Time 64 idle bits ( 8 Bytes )
    pdiSendByte(XNVM_PDI_STCS_INSTR | XOCD_CTRL_REGISTER_ADDRESS);

    // Doesn't work ....
    // pdiSendByte(1);

    // The following works on all ATXmega ?
    pdiSendByte(XNVM_PDI_STCS_INSTR | XOCD_CTRL_REGISTER_ADDRESS | PDI_GUARD_TIME);

    uchar guardTime;
    pdiSendByte(XNVM_PDI_LDCS_INSTR | XOCD_CTRL_REGISTER_ADDRESS);
    if (pdiReadByte(6, &guardTime) == PDI_STATUS_OK) {
      return guardTime == PDI_GUARD_TIME;
    }

    return 1;
}

void pdiDisconnect(uchar keep_reset)
{

    pdiResetDev(0);
    while(pdiState);

    SPCR = 0;

    PDI_DDR &= ~((1 << PDI_MOSI) | (1 << PDI_SCK) |(1 << PDI_RST));
    PDI_OUT &= ~(1 << PDI_MOSI);
    PDI_OUT &= ~(1 << PDI_MISO);

    pdiEnableCount = 0;
    pdiSPIState = 1;

    pdiState = pdiIdleMode;
    pdiTransmitData = 0;
    pdiReceiveData = 0;


    switch(keep_reset) {
        case EXIT_RESET_DISABLED:
            // DDRB |= (1<<3);
            // pdiSetClk0();
            // _delay_ms(1);
            // pdiSetClk1();
            break;

        case EXIT_RESET_ENABLED:
            // DDRB |= (1<<3);
            // pdiSetClk0();
            break;
    }
}

uchar pdiEnterProgrammingMode(){

    uchar status;

    pdiSendByte(XNVM_PDI_STCS_INSTR | XOCD_STATUS_REGISTER_ADDRESS );
    pdiSendByte(0xFD);

    pdiSendByte(XNVM_PDI_LDCS_INSTR | XOCD_STATUS_REGISTER_ADDRESS);
    pdiReadByte(6, &status);

    pdiSendByte(XNVM_PDI_STCS_INSTR | XOCD_RESET_REGISTER_ADDRESS);
    pdiSendByte(0x59);

    uchar buf[9];
    buf[0]=XNVM_PDI_KEY_INSTR;
    memcpy_P(&buf[1],pdi_key,8);
    pdiSendBytes(buf,9);
    
    return pdiWaitNVM();
}

void pdiSendBytes(uchar* ptr,uchar count)
{
    for(;count>0;count--,ptr++)
    pdiSendByte(*ptr);
}

// uchar pdiReadCtrl(uint32_t addr, uchar *value)
// {
    // uchar ret;
    // uchar buf[5];

    // buf[0]=XNVM_PDI_LDS_INSTR | XNVM_PDI_LONG_ADDRESS_MASK | XNVM_PDI_BYTE_DATA_MASK;
    // memmove(buf+1,&addr,4);
    // pdiSendBytes(buf,5);
    // ret = pdiReadByte(100,value);
    // return ret;
// }

uchar pdiWaitNVM()
{
    uchar retry=10;
    for(;retry>0;retry--)
    {
        uchar status;
        pdiSendByte(XNVM_PDI_LDCS_INSTR | XOCD_STATUS_REGISTER_ADDRESS);
        pdiReadByte(6, &status);

        if ((status & XNVM_NVM_BUSY)==0)
        {
            pdi_nvmbusy=0;
            return PDI_STATUS_OK;
        }
    }
    return PDI_STATUS_NVM_TIMEOUT;
}

// uchar pdiWriteCtrl(uint32_t addr,uint8_t value)
// {
    // uchar cmd[6];
    // cmd[0]= XNVM_PDI_STS_INSTR | XNVM_PDI_LONG_ADDRESS_MASK | XNVM_PDI_BYTE_DATA_MASK;
    // memmove(cmd+1,&addr,4);
    // cmd[5]=value;
    // pdiSendBytes(cmd,6);
    // return PDI_STATUS_OK;
// }

uchar pdiResetDev(uchar reset)
{
    uchar buf[2];
    buf[0]=XNVM_PDI_STCS_INSTR | XOCD_RESET_REGISTER_ADDRESS;
    buf[1]=reset?XOCD_RESET_SIGNATURE:0;
    pdiSendBytes(buf,2);
    return PDI_STATUS_OK;
}

// uchar pdiSetPointer(uint32_t addr)
// {
    // uchar cmd[5];
    // cmd[0]=XNVM_PDI_ST_INSTR | XNVM_PDI_LD_PTR_ADDRESS_MASK | XNVM_PDI_LONG_DATA_MASK;
    // memmove(cmd+1,&addr,4);
    // pdiSendBytes(cmd,5);
    // return 0;
// }

// uchar pdiReadBlock(uint32_t addr,uchar* data,uchar len)
// {
    // uchar ret=PDI_STATUS_OK;

    // uchar retry=20;
    // for(;retry>0;retry--)
    // {
        // pdiWriteCtrl(XNVM_DATA_BASE+XNVM_CONTROLLER_BASE
        // +XNVM_CONTROLLER_CMD_REG_OFFSET,XNVM_CMD_READ_NVM_PDI);
        // pdiSetPointer(addr);

        // if (len>1)
        // {
            // pdiSendByte(XNVM_PDI_REPEAT_INSTR | XNVM_PDI_BYTE_DATA_MASK);
            // pdiSendByte(len-1);
        // }

        // pdiSendByte(XNVM_PDI_LD_INSTR | XNVM_PDI_LD_PTR_STAR_INC_MASK | XNVM_PDI_BYTE_DATA_MASK);

        // uchar *dst=data;
        // uchar i;
        // for(i=0;i<len;i++,dst++)
        // {
            // ret=pdiReadByte(200,dst);
            // if (ret!=PDI_STATUS_OK) break;
        // }
        
        // if (ret==PDI_STATUS_OK) break;
    // }

    // return ret;
// }