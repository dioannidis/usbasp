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

//#define F_CPU           12000000L   /* 12MHz */
#define TIMERVALUE      TCNT0
//#define CLOCK_T_320us	60

#if F_CPU == 12000000
# define CLOCK_T_320us	60
#elif F_CPU == 18000000
# define CLOCK_T_320us	90
#elif F_CPU == 20000000
# define CLOCK_T_320us	100
#endif


//#ifdef __AVR_ATmega8__
#if (defined __AVR_ATmega8__) || (defined __AVR_ATmega8A__) || (defined __AVR_ATmega88__)
#define TCCR0B  TCCR0
#endif

/* set prescaler to 64 */
#define clockInit()  TCCR0B = (1 << CS01) | (1 << CS00);

/* wait time * 320 us */
void clockWait(uint8_t time);

#endif /* __clock_h_included__ */
