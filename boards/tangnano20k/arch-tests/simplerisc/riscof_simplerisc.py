"""RISCOF DUT plugin: SimpleRisc in Verilator simulation.

Each test is compiled with env/model_test.h (finisher halt) and env/link.ld
(image at 0x8000_0000), then run by run_dut.py on a prebuilt Verilator model
of one generated SimpleRisc image (config key `sim`), which preloads RAM and
writes the signature. No UART loader, no board.
"""
import logging
import os
import sys

import riscof.utils as utils
from riscof.pluginTemplate import pluginTemplate

logger = logging.getLogger()
HERE = os.path.dirname(os.path.abspath(__file__))


class simplerisc(pluginTemplate):
    __model__ = 'simplerisc'
    __version__ = 'working tree'

    def __init__(self, *args, **kwargs):
        sclass = super().__init__(*args, **kwargs)
        config = kwargs.get('config')
        self.num_jobs = str(config.get('jobs', 1))
        self.pluginpath = os.path.abspath(config['pluginpath'])
        self.isa_spec = os.path.abspath(config['ispec'])
        self.platform_spec = os.path.abspath(config['pspec'])
        self.sim = os.path.abspath(config['sim'])
        self.ram_bytes = int(config['ram_bytes'], 0)
        self.limit = int(config.get('limit', '50000000'))
        self.make = config.get('make', 'make')
        return sclass

    def initialise(self, suite, work_dir, archtest_env):
        self.suite = suite
        self.work_dir = work_dir
        self.compile_cmd = ('riscv64-unknown-elf-gcc -march={0} -mabi=ilp32 -static -mcmodel=medany'
                            ' -fvisibility=hidden -nostdlib -nostartfiles'
                            f' -T {self.pluginpath}/env/link.ld -I {self.pluginpath}/env/ -I {archtest_env}')

    def build(self, isa_yaml, platform_yaml):
        pass

    def runTests(self, testList, cgf_file=None):
        makefile = os.path.join(self.work_dir, 'Makefile.' + self.name[:-1])
        if os.path.exists(makefile):
            os.remove(makefile)
        make = utils.makeUtil(makefilePath=makefile)
        make.makeCommand = self.make + ' -j' + self.num_jobs
        runner = os.path.join(os.path.dirname(HERE), 'run_dut.py')
        for testentry in testList.values():
            test = testentry['test_path']
            sig = os.path.join(testentry['work_dir'], self.name[:-1] + '.signature')
            compile_cmd = (self.compile_cmd.format(testentry['isa'].lower()) + f' {test} -o dut.elf'
                           + ' -D' + ' -D'.join(testentry['macros']))
            make.add_target(f'@cd {testentry["work_dir"]}; {compile_cmd}; '
                            f'riscv64-unknown-elf-objdump -D dut.elf > dut.disass; '
                            f'{sys.executable} {runner} --sim {self.sim} --ram-bytes {self.ram_bytes} '
                            f'--limit {self.limit} dut.elf {sig} > sim.log 2>&1 || true')
        make.execute_all(self.work_dir)
