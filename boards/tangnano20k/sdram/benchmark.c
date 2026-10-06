#include "../c/uart.h"
static volatile uint32_t words[128];
// Executable buffer stays within the loader image's legacy fetch boundary.
__attribute__((section(".text.mutable"),aligned(4)))
static const uint32_t code[2]={0x00700513,0x00008067};
static int coherence(void) {
  volatile uint32_t *low=(volatile uint32_t *)((uintptr_t)words & 0x7fffff);
  words[0]=42;if(low[0]!=42) return 1;
  low[0]=123;if(words[0]!=123) return 2;
  int (*fn)(void)=(int (*)(void))(uintptr_t)code;
  if(fn()!=7) return 3;
  volatile uint32_t *alias=(volatile uint32_t *)((uintptr_t)code & 0x7fffff);
  alias[0]=0x00b00513;
  __asm__ volatile("fence.i":::"memory");
  if(fn()!=11) return 4;
  alias[0]=0x00700513;
  __asm__ volatile("fence.i":::"memory");
  return fn()!=7;
}
static uint32_t cycles(void) {uint32_t v; __asm__ volatile("csrr %0, mcycle":"=r"(v)::"memory");return v;}
int main(void) {
  if(coherence()) {uart_puts("COHERENCE FAIL\n");return 1;}
  uart_puts("COHERENCE PASS\n");
  uint32_t i,j,sum=0;
  for(i=0;i<128;i++) words[i]=i;
  uint32_t start=cycles();
  for(j=0;j<200;j++) for(i=0;i<128;i++) sum+=words[i];
  uint32_t elapsed=cycles()-start;
  uart_puts("BENCH ");uart_put_uint(sum);uart_putc(' ');uart_put_uint(elapsed);uart_putc('\n');
  return sum!=1625600;
}
