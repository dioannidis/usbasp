#include "usbasp.h"
#include "uart.h"
#include "cbuf.h"

#include <avr/io.h>
#include <avr/interrupt.h>
#include <util/atomic.h>

void __vector_usart_rxc_wrapped() __attribute__ ((signal));
void __vector_usart_rxc_wrapped(){
    if (!CBUF_IsFull(rx_Q)){
      CBUF_Push(rx_Q, EEDR);
    }
}

/*
    As we don't use EEPROM we use EEDR to store UDR value.
    No need to disable RXCIE interrupt. Total 4 cycles.
*/
ISR(USART_RXC_vect, ISR_NAKED){
    __asm__ volatile(   "in  __tmp_reg__, %0 \n"                        // 1 cycle
                        "out %1, __tmp_reg__ \n"                        // 1 cycle
                        "rjmp __vector_usart_rxc_wrapped \n"            // 2 cycles
                        ::"i"(_SFR_IO_ADDR(UDR)),"i"(_SFR_IO_ADDR(EEDR))
                    );
}

void __vector_usart_udre_wrapped() __attribute__ ((signal));
void __vector_usart_udre_wrapped(){
    if(!CBUF_IsEmpty(tx_Q)){
        UDR=CBUF_Pop(tx_Q);
        UCSRB|=(1<<UDRIE); // Enable this interrupt back.
    }
}

ISR(USART_UDRE_vect, ISR_NAKED){
    // Disable this interrupt by clearing its Interrupt Enable flag.
    __asm__ volatile(   "cbi %0, %1"::
                        "I"(_SFR_IO_ADDR(UCSRB)),"I"(UDRIE));
    // Now we can enable interrupts without infinite recursion.
    __asm__ volatile("sei"::);
    // Jump into the actual handler.
    __asm__ volatile("rjmp __vector_usart_udre_wrapped"::);
}

void uart_disable(){
    UCSRB=0;
}

void uart_config(uint16_t baud, uint8_t par, uint8_t stop, uint8_t bytes){

    uart_disable();

    CBUF_Init(tx_Q);
    CBUF_Init(rx_Q);

    // Turn 2x mode.
    UCSRA=(1<<U2X);

    uint8_t byte=0;
    switch(par){
        case USBASP_UART_PARITY_EVEN: byte|=(1<<UPM1); break;
        case USBASP_UART_PARITY_ODD:  byte|=(1<<UPM1)|(1<<UPM0); break;
        default: break;
    }

    if(stop == USBASP_UART_STOP_2BIT){
        byte|=(1<<USBS);
    }

    switch(bytes){
        case USBASP_UART_BYTES_6B: byte|=(1<<UCSZ0); break;
        case USBASP_UART_BYTES_7B: byte|=(1<<UCSZ1); break;
        case USBASP_UART_BYTES_8B: byte|=(1<<UCSZ1)|(1<<UCSZ0); break;
        case USBASP_UART_BYTES_9B: byte|=(1<<UCSZ2)|(1<<UCSZ1)|(1<<UCSZ0); break;
        default: break;
    }

    UCSRC=byte;
    
    UBRRL=baud&0xFF;
    UBRRH=baud>>8;

    // Turn on RX/TX and RX interrupt.
    UCSRB=(1<<RXCIE)|(1<<RXEN)|(1<<TXEN);
}