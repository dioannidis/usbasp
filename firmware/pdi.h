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

extern uchar pdi_active;
extern uchar pdi_interrupted;
extern uchar pdi_interrupt_count;
extern uchar pdi_se0_count;
extern uchar pdi_rx;
extern uchar pdi_nvmbusy;

#define PDI_STATUS_OK 0
#define PDI_STATUS_TIMEOUT 1
#define PDI_STATUS_PARITY 2
#define PDI_STATUS_BADSTOP 3
#define PDI_STATUS_NVM_TIMEOUT 4
#define PDI_STATUS_COLLISION 5

extern volatile uchar pdiBusy;

uchar pdiInit();
void pdiCleanup(uchar keep_reset);
void pdiEnableTimerClock();
void pdiDisableTimerClock();
uchar pdiTimerClockEnabled();
void pdiSetClk1();
void pdiSetClk0();
void pdiSetData1();
void pdiSetData0();
void pdiSetDataIn();
uchar pdiGetData();
uchar pdiWaitNVM();
uchar pdiReadBlock(uint32_t addr,uchar* data,uchar len);
void pdiSendIdle();
void pdiSendBreak();
uchar byteParity(uchar b);
void pdiSendByte(uchar b);
void pdiSendBytes(uchar* ptr,uchar count);
void pdiSendByteX(uchar b,uchar extra);
uchar pdiReadCtrl(uint32_t addr, uchar *value);
uchar pdiResetDev(uchar reset);

#endif