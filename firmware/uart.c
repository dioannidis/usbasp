/* Name: uart.c
 *
 * UART support 
 *
 * 2016 original implementation Adam Krasuski (https://github.com/akrasuski1)
 * 2021 tweaks by Dimitrios Chr. Ioannidis ( d.ioannidis@nephelae.eu )
 *
 * Tabsize: 4
 * License: GNU GPL v2 (see Readme.txt)
 */

#include <avr/io.h>
#include <avr/interrupt.h>
#include <util/atomic.h>

#include "usbdrv.h"
#include "uart.h"
#include "cbuf.h"

volatile uint8_t dataByte;

void __vector_usart_rxc_wrapped() __attribute__ ((signal));
void __vector_usart_rxc_wrapped(){
    if (!CBUF_IsFull(rx_Q)){
      *CBUF_GetPushEntryPtr(rx_Q) = dataByte;
      CBUF_AdvancePushIdx(rx_Q);
    }
}

#if (defined __AVR_ATmega8__) || (defined __AVR_ATmega8A__)
ISR(USART_RXC_vect, ISR_NAKED){
#elif (defined __AVR_ATmega88__) || (defined __AVR_ATmega88PA__)
ISR(USART_RX_vect, ISR_NAKED){
#endif    
    __asm__ volatile(
        "lds     __tmp_reg__, %0  \n"
        "   sts     %1, __tmp_reg__  \n"
        "   rjmp __vector_usart_rxc_wrapped \n"
        ::  "m"(USBASPUART_UDR),"m"(dataByte)
    );
}

void __vector_usart_udre_wrapped() __attribute__ ((signal));
void __vector_usart_udre_wrapped(){
    
    if(!CBUF_IsEmpty(tx_Q)){
        USBASPUART_UDR=*CBUF_GetPopEntryPtr(tx_Q);
        CBUF_AdvancePopIdx(tx_Q);
    } else {
        USBASPUART_UCSRB &= ~(1<<USBASPUART_UDRIE);
    }

}


ISR(USART_UDRE_vect, ISR_NAKED){
  __asm__ volatile(
    "rjmp __vector_usart_udre_wrapped    \n"
    ::
  ); 
}

void uart_disable(){

    /* Switch Rx Pullup off */
    PORTD &= ~(1 << PIND0);

    USBASPUART_UCSRB = 0;
    USBASPUART_UCSRB &= ~(1<<USBASPUART_UDRIE);

    CBUF_Init(tx_Q);
    CBUF_Init(rx_Q);


    if(usbAllRequestsAreDisabled()){
        usbEnableAllRequests();
    }

}

void uart_config(uint16_t baud, uint8_t par, uint8_t stop, uint8_t bytes){

    uart_disable();
     
    CBUF_Init(tx_Q);
    CBUF_Init(rx_Q);

    // Turn 2x mode.
    USBASPUART_UCSRA=(1<<USBASPUART_U2X);

    uint8_t byte=0;

    switch(par){
        case USBASP_UART_PARITY_EVEN: byte|=(1<<USBASPUART_UPM1); break;
        case USBASP_UART_PARITY_ODD:  byte|=(1<<USBASPUART_UPM1)|(1<<USBASPUART_UPM0); break;
        default: break;
    }

    if(stop == USBASP_UART_STOP_2BIT){
        byte|=(1<<USBASPUART_USBS);
    }

    switch(bytes){
        case USBASP_UART_BYTES_6B: byte|=(1<<USBASPUART_UCSZ0); break;
        case USBASP_UART_BYTES_7B: byte|=(1<<USBASPUART_UCSZ1); break;
        case USBASP_UART_BYTES_8B: byte|=(1<<USBASPUART_UCSZ1)|(1<<USBASPUART_UCSZ0); break;
        case USBASP_UART_BYTES_9B: byte|=(1<<USBASPUART_UCSZ2)|(1<<USBASPUART_UCSZ1)|(1<<USBASPUART_UCSZ0); break;
        default: break;
    }

    USBASPUART_UCSRC=byte;

    USBASPUART_UBRRH=(unsigned char)(baud>>8);
    USBASPUART_UBRRL=(unsigned char)baud;

    // Turn on RX/TX and RX interrupt.
    USBASPUART_UCSRB=(1<<USBASPUART_RXCIE)|(1<<USBASPUART_RXEN)|(1<<USBASPUART_TXEN);
    
    /* Enable Rx Pin Pullup */
    PORTD |= (1 << PIND0);

}