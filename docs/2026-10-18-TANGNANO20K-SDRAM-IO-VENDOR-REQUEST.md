# Gowin support request

Status: sent to support@gowinsemi.com on 2026-10-18 at 06:10:59 UTC.
Gmail message/thread ID: `1a14da287d5db635`; Sent label verified after sending.
[Open sent request](https://mail.google.com/mail/u/0/#sent/1a14da287d5db635).

Subject: GW2AR-LV18QN88C8/I7 embedded SDR SDRAM AC timing and STA reference plane

We are using a Sipeed Tang Nano 20K with GW2AR-LV18QN88C8/I7 and Gowin
EDA V1.9.12.04. We have a custom SDR SDRAM controller with the reserved
O_sdram_* and IO_sdram_dq top-level port names. It uses the embedded
64-Mbit x32 SDRAM, CAS latency 2, sequential burst length 4, single-location
writes and automatic refresh. CKE is high and CS_n low.

The memory controller is clocked at 54 MHz. SDRAM receives a second output
from the same memory PLL at 54 MHz with a 135-degree phase shift. The CPU
and cache use a separate 72 MHz PLL and cross through an ordered mailbox.
We seek full I/O timing analysis before increasing clock frequencies.

Could you provide or confirm the following for the QN88 package, across
its supported supply voltage and industrial temperature range?

1. The embedded SDRAM AC specification: tAC maximum and tOH minimum for
   CAS2; command/address setup and hold; write-data and DQM setup and hold;
   tLZ/tHZ; minimum clock period and high/low pulse widths; and applicable
   CAS/frequency limits. Please identify whether these bounds vary with
   device lot or embedded memory supplier.
2. The FPGA-to-memory clock, command/address, write-data, DQM and return-data
   connection delay bounds or skew allowances for QN88. Which of these are
   already included in Gowin's FPGA I/O/clock timing model, and what reference
   plane should set_input_delay and set_output_delay use for the embedded
   memory ports? Please clarify whether any I/O constraints are supplied
   automatically by the reserved port names.
3. A complete SDC example for the embedded x32 SDRAM, including explicit min
   and max I/O delays, forwarded clock modeling, clock uncertainty, and
   setup/hold coverage. How should we verify the clock propagation to
   O_sdram_clk and input/output paths for all IO_sdram_dq bits in the report?
4. The applicable relative jitter/phase-error bound between the memory PLL's
   CLKOUT and CLKOUTP, and recommended uncertainty accounting without
   double-counting the tool's existing corner and clock models.
5. An applicable SDRAM simulation model and timing-aware example covering
   first/last read beats, read/write turnaround, masking and refresh.

DS226-2.7E section 2.2.1 lists a 5.4 ns access time but does not provide the
complete table above. IPUG279-1.4E section 6.2 discusses phase adjustment.
The downloadable SDRC_2AR18K reference design targets GW2AR-18LV7LQ144C;
its SDC contains three clock definitions without input/output delays. The
installed GW2AR simulation readme points to a generic Micron MT48LC8M16A2
model, which does not identify the QN88 embedded memory.

We observed that an earlier 54 MHz full-memory test failed at a 225-degree
SDRAM phase and passed at 135 degrees, despite passing internal STA. We
want the complete supported timing bounds to define the valid read/write
window rather than relying only on a successful phase test.

We can supply the custom controller RTL, generated SDC, routed timing
report and exact bitstream/source hashes if useful.
