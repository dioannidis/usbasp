/*
 * clock.h - part of USBasp
 *
 * Autor..........: Thomas Fischl <tfischl@gmx.de>
 * Description....: Provides functions for timing/waiting
 * Licence........: GNU GPL v2 (see Readme.txt)
 * Creation Date..: 2005-02-23
 * Last change....: 2006-11-16
 */

#ifndef __clock_h_included__
#define	__clock_h_included__

#define TIMERVALUE      TCNT0

#if F_CPU == 12000000L
# define CLOCK_T_320us      60
#elif F_CPU == 16000000L            /* ATmega8 max speed */
# define CLOCK_T_320us      80
#elif F_CPU == 18000000L
# define CLOCK_T_320us      90
#elif F_CPU == 20000000L
# define CLOCK_T_320us      100
#endif

#if (defined __AVR_ATmega8__) || (defined __AVR_ATmega8A__)
#define TCCR0B  TCCR0
#endif

/* set prescaler to 64 */
#define clockInit()  TCCR0B = (1 << CS01) | (1 << CS00);

/* wait time * 320 us */
void clockWait(uint8_t time);

#endif /* __clock_h_included__ */
