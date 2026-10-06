#include <stdint.h>
#include "../c/uart.h"
/* More than the old 64 KiB; physical addresses cover every bank. */
static volatile uint32_t working[262144]; /* 1 MiB, initialized by crt0 */
static uint32_t pattern(uint32_t i) { return (i*0x9e3779b1u)^0xa5c35a69u; }
int main(void) {

 for(uint32_t i=0;i<262144;i++) working[i]=pattern(i);
 for(uint32_t i=262144;i-->0;) { if(working[i]!=pattern(i)) { uart_puts("SDRAM BAD\n"); return 1; } }
 for(uint32_t bank=0;bank<4;bank++) {
  volatile uint32_t *p=(volatile uint32_t *)(0x80000000u+bank*0x200000u+0x1ffffcu);
  uint32_t saved=*p;
  *p=0x12345678u^bank;
  if(*p!=(0x12345678u^bank)) return 2;
  *p=saved;
 }
 volatile uint8_t *end=(volatile uint8_t *)0x807ffff8u;
 uint32_t saved_end=*(volatile uint32_t *)end;
 end[0]=0x12;end[1]=0x34;end[2]=0x56;end[3]=0x78;
 if(*(volatile uint32_t *)end!=0x78563412u) return 3;
 *(volatile uint32_t *)end=saved_end;
 uart_puts("SDRAM 1MiB + ALL BANKS HOLDS\n");
 return 0;
}
