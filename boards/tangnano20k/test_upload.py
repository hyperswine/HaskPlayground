#!/usr/bin/env python3
"""Regression for the partial nonblocking write that truncated large images."""
from unittest.mock import patch
from run_program import send_frame
sent=bytearray();calls=0
payload=bytes(range(256))*256
sizes=iter([3,None,1,4096])
def write(fd,view):
 global calls
 calls+=1
 n=next(sizes,len(view))
 if n is None:raise BlockingIOError()
 n=min(n,len(view));sent.extend(view[:n]);return n
with patch('run_program.select.select',return_value=([],[123],[])),patch('run_program.os.write',side_effect=write),patch('run_program.termios.tcdrain') as drain:
 send_frame(123,payload)
 assert sent==payload
 assert calls==5
 drain.assert_called_once_with(123)
print('PASS: complete 64 KiB image across partial writes and backpressure')
