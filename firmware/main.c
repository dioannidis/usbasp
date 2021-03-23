/*
 * USBasp - USB in-circuit programmer for Atmel AVR controllers
 *
 * Thomas Fischl <tfischl@gmx.de>
 * 2020 fixes and tweaks by Ralph Doncaster (Nerd Ralph)
 *
 * License........: GNU GPL v2 (see Readme.txt)
 * Target.........: ATMega8 at 12 MHz
 * Creation Date..: 2005-02-20
 * Last change....: 2020-11-26
 *
 */

#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/pgmspace.h>
#include <avr/wdt.h>

#include "usbasp.h"
#include "usbdrv.h"
#include "isp.h"
#include "clock.h"
#include "tpi.h"
#include "tpi_defs.h"

static uchar replyBuffer[8];

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
   |	bmRequestType	|	bRequest		|	wValue	|	wIndex	|	wLength	|	Data			|
   --------------------------------------------------------------------------------------------------
   |	1000 0000B		|	GET_DESCRIPTOR	|	0x03EE	|	0x0000	|	0x12	|	Returned string	|
   --------------------------------------------------------------------------------------------------
   
   It asks if there is a specific string descriptor at index 0xEE. Because this string 
   descriptor request, is not by default handled by the V-USB, we change the USB_CFG_DESCR_PROPS_UNKNOWN
   to be dynamic (USB_PROP_IS_DYNAMIC). This effectively tell the V-USB that for every 
   unknown string index request to call the usbFunctionDescriptor function.
   
   usbFunctionDescriptor function returns an OS string descriptor using the version 1.00 format 
   which has a fixed length of 18 bytes, with a structure as shown in the following table :

   --------------------------------------------------------------------------
   |	Length	|	Type	|	Signature	|	MS Vendor Code	|	Pad		|
   --------------------------------------------------------------------------
   |	0x12	|	0x03	|	MSFT100		|	unsigned byte	|	0x00	|
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
   
   
/* For Windows OS Descriptors we need to report that we support USB 2.0 */

PROGMEM const char usbDescriptorDevice[18] = {    /* USB device descriptor */
    18,         /* sizeof(usbDescriptorDevice): length of descriptor in bytes */
    USBDESCR_DEVICE,        /* descriptor type */
    0x00, 0x02,             /* USB version supported */
    USB_CFG_DEVICE_CLASS,
    USB_CFG_DEVICE_SUBCLASS,
    0,                      /* protocol */
    8,                      /* max packet size */
    /* the following two casts affect the first byte of the constant only, but
     * that's sufficient to avoid a warning with the default values.
     */
    (char)USB_CFG_VENDOR_ID,/* 2 bytes */
    (char)USB_CFG_DEVICE_ID,/* 2 bytes */
    USB_CFG_DEVICE_VERSION, /* 2 bytes */
    1, /*USB_CFG_DESCR_PROPS_STRING_VENDOR != 0 ? 1 : 0,*/         /* manufacturer string index */
    2, /*USB_CFG_DESCR_PROPS_STRING_PRODUCT != 0 ? 2 : 0, */        /* product string index */
    3, /* USB_CFG_DESCR_PROPS_STRING_SERIAL_NUMBER != 0 ? 3 : 0, */  /* serial number string index */
    1,          /* number of configurations */
};

/* OS Extended Compat ID feature descriptor */

const char OS_EXTENDED_COMPAT_ID[40] = {
	/* Header */
	0x28, 0x00, 0x00, 0x00,									/* OS Extended Compat ID feature descriptor length */
	0x00, 0x01,												/* OS Extended Compat ID version */
	0x04, 0x00,												/* Index */
	0x01,													/* Configurations count */
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,				/* Reserved */
	/* Configuration */
	0x00,													/* First Interface Number */
	0x01,													/* Reserved */
	'W','I','N','U','S','B', 0x00, 0x00,					/* Windows string Compatible ID */
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,			/* Windows string SubCompatible ID */ 
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00						/* Reserved */
};

#define MS_VENDOR_CODE 0x5D
PROGMEM const char OS_STRING_DESCRIPTOR[18] = {
  0x12,                                         /* Length: An unsigned byte and MUST be set to 0x12. */  
  /*  https://docs.microsoft.com/en-us/windows-hardware/drivers/network/mb-interface-model-supplement */
  0x03,                                         /* Type: An unsigned byte and MUST be set to 0x03. */
  'M',0,'S',0,'F',0,'T',0,'1',0,'0',0,'0',0,    /* Signature: A Unicode string and MUST be set to "MSFT100". */
  MS_VENDOR_CODE,                               /* MS Vendor Code: An unsigned byte, 
                                                  it will be used to retrieve associated feature descriptors. */
  0x00                                          /* Pad: An unsigned byte and MUST be set to 0x00. */ 
};

usbMsgLen_t usbFunctionDescriptor(struct usbRequest *rq) {

  // DBG1(0xEE, &rq->wValue.bytes[0], 2);

  /* string (3) request at index 0xEE, is an OS string descriptor request */   
  if ((rq->wValue.bytes[1] == 3) && (rq->wValue.bytes[0] == 0xEE)) {

	usbMsgPtr = (usbMsgPtr_t)&OS_STRING_DESCRIPTOR;

	return sizeof(OS_STRING_DESCRIPTOR);

  };

  return 0;

};

uchar usbFunctionSetup(uchar data[8]) {

    const usbRequest_t* request = (const usbRequest_t*)data;

    uchar len = 0;

    if (data[1] == USBASP_FUNC_CONNECT) {

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
    
	/* Handle the OS feature request associated with the MS Vendor Code
		we replied earlier in the OS String Descriptor request. See usbFunctionDescriptor. */
	} else if (request->bRequest == MS_VENDOR_CODE) {		
		if (request->wIndex.word == 0x0004)
		{
			/* Send the Extended Compat ID OS feature descriptor, 
			  requesting to load the default winusb driver for us */
			usbMsgPtr = (usbMsgPtr_t)&OS_EXTENDED_COMPAT_ID;
			return sizeof(OS_EXTENDED_COMPAT_ID);
		}

		return 0;
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
    for (i = 0; i < len; i++) {
        if (prog_state == PROG_STATE_READFLASH) {
            data[i] = ispReadFlash(prog_address);
        } else {
            data[i] = ispReadEEPROM(prog_address);
        }
        prog_address++;
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
            != PROG_STATE_WRITEEEPROM) && (prog_state != PROG_STATE_TPI_WRITE)) {
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

    return retVal;
}

int main(void) {
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
        usbPoll();
    }
    return 0;
}

