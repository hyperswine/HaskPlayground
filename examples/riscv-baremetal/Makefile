# Makefile — Build bare-metal RV32I programs for the CPURiscV processor
#
# Requirements (macOS / Linux):
#   brew install riscv-gnu-toolchain      # macOS
#   apt install gcc-riscv64-unknown-elf   # Debian/Ubuntu
#
# Produces .bin files (flat binary, padded to 4096 bytes by the programmer).
# Upload with programmer-riscv.py (see usage below).

# ── Toolchain ──────────────────────────────────────────────────────────────
CROSS   ?= riscv64-unknown-elf
AS       = $(CROSS)-as
LD       = $(CROSS)-ld
OBJCOPY  = $(CROSS)-objcopy
OBJDUMP  = $(CROSS)-objdump

MARCH    = rv32i
MABI     = ilp32

ASFLAGS  = -march=$(MARCH) -mabi=$(MABI)
# --no-relax: disable linker relaxation so JAL ranges stay predictable
LDFLAGS  = -T link.ld -m elf32lriscv --no-relax

# ── Programs ───────────────────────────────────────────────────────────────
PROGRAMS = count sum fibonacci

all: $(PROGRAMS:%=%.bin)

# Assemble → object
%.o: %.S
	$(AS) $(ASFLAGS) -o $@ $<

# Link → ELF (acceptable by programmer-riscv.py --elf)
%.elf: %.o
	$(LD) $(LDFLAGS) -o $@ $<

# Extract flat binary (acceptable by programmer-riscv.py --bin)
%.bin: %.elf
	$(OBJCOPY) -O binary $< $@

# Disassembly for inspection
%.lst: %.elf
	$(OBJDUMP) -d -M numeric $< > $@

lst: $(PROGRAMS:%=%.lst)

clean:
	rm -f *.o *.elf *.bin *.lst

# ── Upload / run ────────────────────────────────────────────────────────────
# Connect a Tang Nano 20K running the CPURiscVTop bitstream, then:
#   make run PROG=count
#   make run PROG=sum
#   make run PROG=fibonacci
#
# Override PORT if auto-detection fails:
#   make run PROG=fibonacci PORT=/dev/cu.usbserial-XXXX

PROG        ?= count
PROGRAMMER  := ../riscv-verilog/programmer-riscv.py
PORT        ?= /dev/cu.usbserial-20250303171

_port_flag  := $(if $(PORT),--port $(PORT),)

run: $(PROG).bin
	python3 $(PROGRAMMER) --bin $(PROG).bin --run $(_port_flag)

.PHONY: all lst clean run
