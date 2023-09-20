/*
 * pdi.h - part of USBasp
 *
 * Author.........: szu ( from http://szulat.blogspot.com/ )
 * Description....: Provides functions for communication/programming
 *                  over PDI interface
 * Licence........: GNU GPL v2 (see Readme.txt)
 * Creation Date..: 2012-08-15
 * Last change....: 2012-08-15
 */

#ifndef _PDI_H_
#define _PDI_H_

#include <stdint.h>

#ifndef uchar
#define	uchar	unsigned char
#endif

#define PDI_OUT   PORTB
#define PDI_IN    PINB
#define PDI_DDR   DDRB
#define PDI_RST   PB2
#define PDI_MOSI  PB3
#define PDI_MISO  PB4
#define PDI_SCK   PB5

#define PDI_STATUS_OK 0
#define PDI_STATUS_TIMEOUT 1
#define PDI_STATUS_PARITY 2
#define PDI_STATUS_BADSTOP 3
#define PDI_STATUS_NVM_TIMEOUT 4
#define PDI_STATUS_COLLISION 5

/* 64 Idle Bits ( 8 Bytes ) */
#define PDI_GUARD_TIME 1

extern uchar pdi_nvmbusy;
extern volatile uchar pdiState;

uchar pdiConnect();
void pdiDisconnect(uchar keep_reset);
uchar pdiEnterProgrammingMode();
uchar pdiWaitNVM();
uchar pdiReadBlock(uint32_t address, uchar* dataBuf, uchar lenAsked);

#endif