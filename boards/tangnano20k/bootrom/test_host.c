/* Run the actual ROM C parser with UART/memory substitutions. */
#include <assert.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "host_io.h"

#ifdef ROM_SDRAM
#define CAP (524288u)
#else
#define CAP (16320u)
#endif
#define RAM_WORDS (2097152u)
#define FILL (0xdeadbeefu)
volatile uint32_t host_ram[RAM_WORDS];
uint32_t host_cause, host_register;
static unsigned char *input;
static size_t input_size, position, output_size;
static char output[256];
static int ran, corrupt;
static jmp_buf stop;
void rom_main(void);
uint32_t host_getc(void) {
  if (position == input_size) longjmp(stop, 1);
  uint32_t value = input[position++];
  if (corrupt && value == 'V' && position == input_size) host_ram[19] ^= 0x08001234u;
  return value;
}
void host_putc(uint32_t value) {
  assert(output_size < sizeof(output) - 1);
  output[output_size++] = value;
  output[output_size] = 0;
}
void rom_run(void) {
  ran = 1;
  longjmp(stop, 1);
}
static uint32_t word(unsigned i) { return 0x10204081u * i ^ (i << 11) ^ 0x03515254u; }
static void byte(unsigned value) { input[input_size++] = value; }
static void le32(uint32_t value) { for(unsigned j=0;j<4;j++) byte(value >> (j*8)); }
static void begin(void) {
  for(unsigned i=0;i<RAM_WORDS;i++) host_ram[i] = FILL;
  host_cause=1;host_register=0;input_size=position=output_size=0;output[0]=0;ran=corrupt=0;
}
static void run(void) { if (!setjmp(stop)) rom_main(); }
static void load(unsigned count, int extended) {
  byte(extended ? 'Q' : 'P');byte(count);byte(count >> 8);
  if(extended) { byte(count >> 16);byte(count >> 24); }
  for(unsigned i=0;i<count;i++) le32(word(i));
}
static void verify_text(unsigned count, char *text) {
  uint32_t value=0;for(unsigned i=0;i<count;i++) value^=word(i);
  if(corrupt) value^=0x08001234u;
  sprintf(text,"%08x\n",value);
}
int main(void) {
  input=malloc((CAP+1)*4u+128);assert(input);
  char expected[32];
  begin();byte('T');run();assert(!strcmp(output,"DONE"));
  begin();load(37,1);byte('V');run();verify_text(37,expected);
  assert(!strcmp(output,expected));
  for(unsigned i=0;i<37;i++) assert(host_ram[i]==word(i));
  assert(host_ram[37]==0 && host_ram[38]==FILL);
  begin();load(37,1);corrupt=1;byte('V');run();verify_text(37,expected);
  assert(!strcmp(output,expected)); /* V must reread RAM, not echo upload XOR. */
  begin();load(CAP,1);byte('V');run();verify_text(CAP,expected);
  assert(!strcmp(output,expected));
  for(unsigned i=0;i<CAP;i++) assert(host_ram[i]==word(i));
  assert(host_ram[CAP]==FILL); /* Exact cap must not append outside its bound. */
  begin();load(CAP+1,1);byte('V');byte('T');run();
  assert(!strcmp(output,"ERR\n00000000\nDONE"));
  for(unsigned i=0;i<RAM_WORDS;i++) assert(host_ram[i]==FILL);
  begin();byte('Q');le32(0xffffffff);run();
  for(unsigned i=0;i<RAM_WORDS;i++) assert(host_ram[i]==FILL);
  begin();byte('Q');le32(0);byte('V');run();assert(!strcmp(output,"00000000\n"));
  begin();load(7,0);byte('V');byte('X');byte('T');run();verify_text(7,expected);
  strcat(expected,"DONE");assert(!strcmp(output,expected));assert(host_ram[7]==0);
  begin();load(7,1);byte('M');byte('V');run();assert(!strcmp(output,"00000000\n"));
  for(unsigned i=0;i<16384;i++) assert(host_ram[i]==0);
  assert(host_ram[16384]==FILL);
  begin();byte('R');run();assert(ran && !output_size);
  begin();byte('H');run();assert(ran && !output_size);
  begin();host_cause=3;run();assert(!strcmp(output,"DONE"));
  begin();host_cause=0;run();assert(!strcmp(output,"DONE"));
  begin();host_cause=2;run();assert(!output_size);
  printf("PASS: %u-byte loader bounds, Q/V/H/T, RAM readback corruption, P/R/M/X and reset causes\n",CAP*4u);
  free(input);
}
