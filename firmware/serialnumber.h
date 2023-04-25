/*
 * serialnumber.h - part of USBasp
 *
 * Autor..........: Dimitrios Chr. Ioannidis ( d.ioannidis@nephelae.eu )
 * Description....: Provides functions for updating the Serial Number
                    using HID
 * Licence........: GNU GPL v2 (see Readme.txt)
 * Creation Date..: 2023-04-01
 * Last change....: 2023-04-11
 */

#ifndef __serialnumber_h_included__
#define	__serialnumber_h_included__

#ifndef uchar
#define	uchar	unsigned char
#endif

void serialNumberWrite(uchar *reportData);

#endif /* __serialnumber_h_included__ */