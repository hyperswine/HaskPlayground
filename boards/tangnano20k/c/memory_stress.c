#include "uart.h"
int main(void) {
 volatile uint32_t words[32];
 volatile uint8_t *bytes=(volatile uint8_t*)words;
 volatile uint16_t *halves=(volatile uint16_t*)words;
 for (unsigned pass=0;pass<64;pass++) {
  for(unsigned i=0;i<32;i++) words[i]=0x12345678u;
  for(unsigned i=0;i<128;i++) {
   bytes[i]=(uint8_t)(i+pass);
   if(bytes[i]!=(uint8_t)(i+pass)) {uart_puts("BYTE READ FAIL\n"); return 1;}
  }
  for(unsigned i=0;i<32;i++) {
   unsigned k=4*i;
   uint32_t want=((k+pass)&255)|(((k+pass+1)&255)<<8)|(((k+pass+2)&255)<<16)|(((k+pass+3)&255)<<24);
   if(words[i]!=want) {uart_puts("BYTE MERGE FAIL\n"); return 1;}
  }
  for(unsigned i=0;i<64;i++) {
   halves[i]=(uint16_t)(0x8765+i+pass);
   if(halves[i]!=(uint16_t)(0x8765+i+pass)) {uart_puts("HALF READ FAIL\n"); return 1;}
  }
 }
 uart_puts("MEMORY HOLDS\n");return 0;
}
