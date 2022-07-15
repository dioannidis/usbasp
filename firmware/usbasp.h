/*
 * usbasp.c - part of USBasp
 *
 * Autor..........: Thomas Fischl <tfischl@gmx.de>
 * Description....: Definitions and macros for usbasp
 * Licence........: GNU GPL v2 (see Readme.txt)
 * Creation Date..: 2009-02-28
 * Last change....: 2009-02-28
 */

#ifndef USBASP_H_
#define USBASP_H_

/* USB function call identifiers */
// ISP:
#define USBASP_FUNC_CONNECT             1
#define USBASP_FUNC_DISCONNECT          2
#define USBASP_FUNC_TRANSMIT            3
#define USBASP_FUNC_READFLASH           4
#define USBASP_FUNC_ENABLEPROG          5
#define USBASP_FUNC_WRITEFLASH          6
#define USBASP_FUNC_READEEPROM          7
#define USBASP_FUNC_WRITEEEPROM         8
#define USBASP_FUNC_SETLONGADDRESS      9
#define USBASP_FUNC_SETISPSCK           10
// TPI:
#define USBASP_FUNC_TPI_CONNECT         11
#define USBASP_FUNC_TPI_DISCONNECT      12
#define USBASP_FUNC_TPI_RAWREAD         13
#define USBASP_FUNC_TPI_RAWWRITE        14
#define USBASP_FUNC_TPI_READBLOCK       15
#define USBASP_FUNC_TPI_WRITEBLOCK      16
#define USBASP_FUNC_GETCAPABILITIES     127
// UART:
#define USBASP_FUNC_UART_CONFIG         60
#define USBASP_FUNC_UART_DISABLE        63

/* USBASP capabilities */
#define USBASP_CAP_0_TPI                0x01
#define USBASP_CAP_6_UART               0x40
#define USBASP_CAP_HIDUART              0x80

#define USBASP_CAP_12MHZ_CLOCK          0x00
#define USBASP_CAP_16MHZ_CLOCK          0x01
#define USBASP_CAP_18MHZ_CLOCK          0x02
#define USBASP_CAP_20MHZ_CLOCK          0x03

/* programming state */
#define PROG_STATE_IDLE                 0
#define PROG_STATE_WRITEFLASH           1
#define PROG_STATE_READFLASH            2
#define PROG_STATE_READEEPROM           3
#define PROG_STATE_WRITEEEPROM          4
#define PROG_STATE_TPI_READ             5
#define PROG_STATE_TPI_WRITE            6
#define PROG_STATE_SET_REPORT           7

/* Block mode flags */
#define PROG_BLOCKFLAG_FIRST            1
#define PROG_BLOCKFLAG_LAST             2

/* ISP SCK speed identifiers */
#define USBASP_ISP_SCK_AUTO             0
#define USBASP_ISP_SCK_0_5              1   /* 500 Hz */
#define USBASP_ISP_SCK_1                2   /*   1 kHz */
#define USBASP_ISP_SCK_2                3   /*   2 kHz */
#define USBASP_ISP_SCK_4                4   /*   4 kHz */
#define USBASP_ISP_SCK_8                5   /*   8 kHz */
#define USBASP_ISP_SCK_16               6   /*  16 kHz */
#define USBASP_ISP_SCK_32               7   /*  32 kHz */
#define USBASP_ISP_SCK_93_75            8   /*  93.75 kHz */
#define USBASP_ISP_SCK_187_5            9   /* 187.5  kHz */
#define USBASP_ISP_SCK_375              10  /* 375 kHz   */
#define USBASP_ISP_SCK_750              11  /* 750 kHz   */
#define USBASP_ISP_SCK_1500             12  /* 1.5 MHz   */
#define USBASP_ISP_SCK_3000             13  /* 3 MHz   */

// UART flags.
#define USBASP_UART_PARITY_MASK         0b11
#define USBASP_UART_PARITY_NONE         0b00
#define USBASP_UART_PARITY_EVEN         0b01
#define USBASP_UART_PARITY_ODD          0b10

#define USBASP_UART_STOP_MASK           0b100
#define USBASP_UART_STOP_1BIT           0b000
#define USBASP_UART_STOP_2BIT           0b100

#define USBASP_UART_BYTES_MASK          0b111000
#define USBASP_UART_BYTES_5B            0b000000
#define USBASP_UART_BYTES_6B            0b001000
#define USBASP_UART_BYTES_7B            0b010000
#define USBASP_UART_BYTES_8B            0b011000
#define USBASP_UART_BYTES_9B            0b100000

/* macros for gpio functions */
/* LEDs are active low */
#define ledRedOff()                     DDRC &= ~(1 << PC1)
#define ledRedOn()                      DDRC |= (1 << PC1)
#define ledGreenOff()                   DDRC &= ~(1 << PC0)
#define ledGreenOn()                    DDRC |= (1 << PC0)

#endif /* USBASP_H_ */