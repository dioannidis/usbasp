/*
 * pdi.c - part of USBasp
 *
 * Author.........: szu ( from http://szulat.blogspot.com/ )
 * Description....: Provides functions for communication/programming
 *                  over PDI interface
 * Licence........: GNU GPL v2 (see Readme.txt)
 * Creation Date..: 2012-08-15
 * Last change....: 2012-08-15
 */
 
#include "pdi.h"
#include "usbasp.h"
#include <avr/io.h>
#include <avr/pgmspace.h>
#include <util/parity.h>
#include "usbconfig.h"
#include <string.h>
#include "xmega_pdi.h"

#ifndef F_CPU
//#define F_CPU (USB_CFG_CLOCK_KHZ*1000)
#define F_CPU 12000000
#endif
#include <util/delay.h>

uchar pdi_nvmbusy=0;

PROGMEM static const uchar pdi_key[8]={0xFF,0x88,0xD8,0xCD,0x45,0xAB,0x89,0x12};

static void pdiWaitBit()
{
    _delay_us(3);
}

uchar pdiInit()
{
    //12MHz/256 -> 46k
    TCCR2=(0<<CS22)|(0<CS21)|(1<<CS20)|(0<<COM21)|(0<<COM20)|(1<<WGM21)|(0<<WGM20);
    OCR2=32;
    PORTB |= (1<<3); PORTB &= ~((1<<4)|(1<<5)|(1<<2));
    DDRB |= (1<<3)|(1<<2); DDRB &= ~((1<<4)|(1<<5));

    pdiSetClk0();
    pdiSetData0();
    _delay_us(110);

    pdiSetData1();
    pdiSetClk1();
    _delay_us(1);
    pdiSendIdle();
    pdiSendIdle();

    pdiResetDev(1);

    uchar buf[9];
    buf[0]=XNVM_PDI_KEY_INSTR;
    memcpy_P(&buf[1],pdi_key,8);
    pdiSendBytes(buf,9);

    pdiSendIdle();

    pdiSendByte(XNVM_PDI_STCS_INSTR | XOCD_CTRL_REGISTER_ADDRESS);
    pdiSendByte(3);

    pdiSendIdle();

    uchar status=pdiWaitNVM();
    if (status==PDI_STATUS_OK)
    {
        pdiEnableTimerClock();
        return PDI_STATUS_OK;
    }

    return status;
}

void pdiCleanup(uchar keep_reset)
{
    pdiDisableTimerClock();
    pdiSendIdle();
    pdiResetDev(0);

    DDRB &= ~((1<<3)|(1<<5)|(1<<2)|(1<<4));
    PORTB &= ~((1<<3)|(1<<5)|(1<<2)|(1<<4));

    switch(keep_reset)
    {
    case EXIT_RESET_DISABLED:
        DDRB |= (1<<3);
        pdiSetClk0();
        _delay_ms(1);
        pdiSetClk1();
        break;

    case EXIT_RESET_ENABLED:
        DDRB |= (1<<3);
        pdiSetClk0();
        break;
    }
}

void pdiEnableTimerClock()
{
    pdiSetData1();
    TCCR2=(0<<CS22)|(0<CS21)|(1<<CS20)|(0<<COM21)|(1<<COM20)|(1<<WGM21)|(0<<WGM20);
}

void pdiDisableTimerClock()
{
    pdiSetData1();
    TCCR2=(0<<CS22)|(0<CS21)|(1<<CS20)|(0<<COM21)|(0<<COM20)|(1<<WGM21)|(0<<WGM20);
}

uchar pdiTimerClockEnabled()
{
    return (TCCR2 & (1<<COM20));
}

void pdiSetClk1()
{
    PORTB|=(1<<3);
}

void pdiSetClk0()
{
    PORTB&=~(1<<3);
}

void pdiSetData1()
{
    PORTB|=(1<<5);
    PORTB&=~(1<<2);
    DDRB|=(1<<5)|(1<<2);
}

void pdiSetData0()
{
    PORTB&=~((1<<5)|(1<<2));
    DDRB|=(1<<5)|(1<<2);
}

void pdiSetDataIn()
{
    PORTB&=~(1<<2);
    DDRB&=~((1<<5)|(1<<2));
    PORTB&=~(1<<5);
}

uchar pdiGetData()
{
    return (PINB&(1<<4))?1:0;
}

void pdiSendIdle()
{
    pdiSendByteX(255,255);
}

void pdiSendBreak()
{
    pdiSendByteX(0,0);
}

#define PDI_BIT_START 1
#define PDI_BIT_PARITY 2
#define PDI_BIT_STOP 4

uchar byteParity(uchar b)
{
    return parity_even_bit(b);
}

void pdiSendByte(uchar b)
{
    uchar extra=PDI_BIT_STOP;
    if (byteParity(b)) extra|=PDI_BIT_PARITY;
    pdiSendByteX(b,extra);
}

void pdiSendBytes(uchar* ptr,uchar count)
{
    for(;count>0;count--,ptr++)
    pdiSendByte(*ptr);
}

void pdiSendByteX(uchar byte,uchar extra)
{
    pdiSetClk0();
    if (extra&PDI_BIT_START) pdiSetData1(); else pdiSetData0();
    pdiWaitBit();
    pdiSetClk1();
    pdiWaitBit();

    uchar bit;
    for(bit=1;bit;bit<<=1)
    {
        pdiSetClk0();
        if (byte & bit) pdiSetData1(); else pdiSetData0();
        pdiWaitBit();
        pdiSetClk1();
        pdiWaitBit();
    }

    pdiSetClk0();
    if (extra&PDI_BIT_PARITY) pdiSetData1(); else pdiSetData0();
    pdiWaitBit();
    pdiSetClk1();
    pdiWaitBit();

    pdiSetClk0();
    if (extra&PDI_BIT_STOP) pdiSetData1(); else pdiSetData0();
    pdiWaitBit();
    pdiSetClk1();
    pdiWaitBit();

    pdiSetClk0();
    if (extra&PDI_BIT_STOP) pdiSetData1(); else pdiSetData0();
    pdiWaitBit();
    pdiSetClk1();
    pdiWaitBit();
}

uchar pdiReadByte(uchar timeout,uchar *result)
{
    uchar in=PDI_STATUS_TIMEOUT,ret=PDI_STATUS_OK;
    for(;timeout>0;timeout--)
    {
        pdiSetClk0();
        pdiSetDataIn();
        pdiWaitBit();
        pdiSetClk1();
        in=pdiGetData();
        pdiWaitBit();
        if (in==0) break;
    }
    if (in) return PDI_STATUS_TIMEOUT;

    uchar bit,byte=0,parity;
    for(bit=1;bit;bit<<=1)
    {
        pdiSetClk0();
        pdiWaitBit();
        pdiSetClk1();
        if (pdiGetData()) byte|=bit;
        pdiWaitBit();
    }
    *result=byte;

    pdiSetClk0();
    pdiWaitBit();
    pdiSetClk1();
    parity=pdiGetData();
    pdiWaitBit();

    if (parity!=byteParity(byte)) ret=PDI_STATUS_PARITY;

    pdiSetClk0();
    pdiWaitBit();
    pdiSetClk1();
    if ((ret==PDI_STATUS_OK)&&(!pdiGetData())) ret=PDI_STATUS_BADSTOP;
    pdiWaitBit();

    pdiSetClk0();
    pdiWaitBit();
    pdiSetClk1();
    if ((ret==PDI_STATUS_OK)&&(!pdiGetData())) ret=PDI_STATUS_BADSTOP;
    pdiWaitBit();

    return ret;
}

uchar pdiReadCtrl(uint32_t addr, uchar *value)
{
    uchar ret;
    uchar buf[5];

    buf[0]=XNVM_PDI_LDS_INSTR | XNVM_PDI_LONG_ADDRESS_MASK | XNVM_PDI_BYTE_DATA_MASK;
    memmove(buf+1,&addr,4);
    pdiSendBytes(buf,5);
    ret = pdiReadByte(200,value);
    pdiSendBreak();
    pdiSendIdle();
    pdiSendBreak();
    pdiSendIdle();
    return ret;
}

uchar pdiWaitNVM()
{
    uchar retry=100;
    for(;retry>0;retry--)
    {
        uchar status;
        if (pdiReadCtrl(XNVM_CONTROLLER_BASE+XNVM_DATA_BASE
                    +XNVM_CONTROLLER_STATUS_REG_OFFSET, &status)==PDI_STATUS_OK)
        if ((status & XNVM_NVM_BUSY)==0)
        {
            pdi_nvmbusy=0;
            return PDI_STATUS_OK;
        }
    }
    return PDI_STATUS_NVM_TIMEOUT;
}

uchar pdiWriteCtrl(uint32_t addr,uint8_t value)
{
    uchar cmd[6];
    cmd[0]= XNVM_PDI_STS_INSTR | XNVM_PDI_LONG_ADDRESS_MASK | XNVM_PDI_BYTE_DATA_MASK;
    memmove(cmd+1,&addr,4);
    cmd[5]=value;
    pdiSendBytes(cmd,6);
    return PDI_STATUS_OK;
}

uchar pdiResetDev(uchar reset)
{
    uchar buf[2];
    buf[0]=XNVM_PDI_STCS_INSTR | XOCD_RESET_REGISTER_ADDRESS;
    buf[1]=reset?XOCD_RESET_SIGNATURE:0;
    pdiSendBytes(buf,2);
    return PDI_STATUS_OK;
}

uchar pdiSetPointer(uint32_t addr)
{
    uchar cmd[5];
    cmd[0]=XNVM_PDI_ST_INSTR | XNVM_PDI_LD_PTR_ADDRESS_MASK | XNVM_PDI_LONG_DATA_MASK;
    memmove(cmd+1,&addr,4);
    pdiSendBytes(cmd,5);
    return 0;
}

uchar pdiReadBlock(uint32_t addr,uchar* data,uchar len)
{
    uchar ret=PDI_STATUS_OK;

    uchar retry=20;
    for(;retry>0;retry--)
    {
        pdiWriteCtrl(XNVM_DATA_BASE+XNVM_CONTROLLER_BASE
        +XNVM_CONTROLLER_CMD_REG_OFFSET,XNVM_CMD_READ_NVM_PDI);
        pdiSetPointer(addr);

        if (len>1)
        {
            pdiSendByte(XNVM_PDI_REPEAT_INSTR | XNVM_PDI_BYTE_DATA_MASK);
            pdiSendByte(len-1);
        }

        pdiSendByte(XNVM_PDI_LD_INSTR | XNVM_PDI_LD_PTR_STAR_INC_MASK | XNVM_PDI_BYTE_DATA_MASK);

        uchar *dst=data;
        uchar i;
        for(i=0;i<len;i++,dst++)
        {
            ret=pdiReadByte(200,dst);
            if (ret!=PDI_STATUS_OK) break;
        }
        pdiSendBreak();
        pdiSendIdle();
        pdiSendBreak();
        pdiSendIdle();

        if (ret==PDI_STATUS_OK) break;
    }

    return ret;
}