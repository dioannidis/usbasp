/* Name: uart.h
 *
 * UART support 
 *
 * 2016 original implementation Adam Krasuski (https://github.com/akrasuski1)
 * 2021 tweaks by Dimitrios Chr. Ioannidis ( d.ioannidis@nephelae.eu )
 *
 * Tabsize: 4
 * License: GNU GPL v2 (see Readme.txt)
 */

#ifndef UART_H
#define UART_H

#include "cbuf.h"
#include "usbasp.h"
#include <stdint.h>

#if (defined __AVR_ATmega8__) || (defined __AVR_ATmega8A__)
    
#   define USBASPUART_UDR       UDR
#   define USBASPUART_UDRIE     UDRIE
#   define USBASPUART_UCSRA     UCSRA
#   define USBASPUART_UCSRB     UCSRB
#   define USBASPUART_UCSRC     UCSRC
#   define USBASPUART_U2X       U2X
#   define USBASPUART_UCSZ0     UCSZ0
#   define USBASPUART_UCSZ1     UCSZ1
#   define USBASPUART_UCSZ2     UCSZ2
#   define USBASPUART_UPM0      UPM0
#   define USBASPUART_UPM1      UPM1
#   define USBASPUART_USBS      USBS
#   define USBASPUART_UBRRL     UBRRL
#   define USBASPUART_UBRRH     UBRRH
#   define USBASPUART_RXCIE     RXCIE
#   define USBASPUART_RXEN      RXEN
#   define USBASPUART_TXEN      TXEN

#elif (defined __AVR_ATmega88__) || (defined __AVR_ATmega88PA__)
    
#   define USBASPUART_UDR       UDR0
#   define USBASPUART_UDRIE     UDRIE0
#   define USBASPUART_UCSRA     UCSR0A
#   define USBASPUART_UCSRB     UCSR0B
#   define USBASPUART_UCSRC     UCSR0C
#   define USBASPUART_U2X       U2X0
#   define USBASPUART_UCSZ0     UCSZ00
#   define USBASPUART_UCSZ1     UCSZ01
#   define USBASPUART_UCSZ2     UCSZ02
#   define USBASPUART_UPM0      UPM00
#   define USBASPUART_UPM1      UPM01
#   define USBASPUART_USBS      USBS0
#   define USBASPUART_UBRRL     UBRR0L
#   define USBASPUART_UBRRH     UBRR0H
#   define USBASPUART_RXCIE     RXCIE0
#   define USBASPUART_RXEN      RXEN0
#   define USBASPUART_TXEN      TXEN0

#endif    

#define rx_Q_SIZE   128
#define tx_Q_SIZE   128

volatile struct
{
    uint8_t m_getIdx;
    uint8_t m_putIdx;
    uint8_t m_entry[rx_Q_SIZE];
} rx_Q;

volatile struct
{
    uint8_t m_getIdx;
    uint8_t m_putIdx;
    uint8_t m_entry[tx_Q_SIZE];
} tx_Q;

void uart_config(uint16_t baud, uint8_t par, uint8_t stop, uint8_t bytes);
void uart_disable();

#endif // UART_H
