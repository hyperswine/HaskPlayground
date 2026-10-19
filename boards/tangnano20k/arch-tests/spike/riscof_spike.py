"""RISCOF reference plugin: Spike (riscv-isa-sim) as the golden model.

Compiles each test with the HTIF halt in env/model_test.h and runs it on
Spike with the ISA string from the DUT's ISA YAML, writing the signature one
32-bit word per line.
"""
import logging
import os
import shutil

import riscof.utils as utils
from riscof.pluginTemplate import pluginTemplate

logger = logging.getLogger()


class spike(pluginTemplate):
    __model__ = 'spike'
    __version__ = '1.1.1-dev'

    def __init__(self, *args, **kwargs):
        sclass = super().__init__(*args, **kwargs)
        config = kwargs.get('config')
        self.ref_exe = os.path.join(config.get('PATH', ''), 'spike')
        self.num_jobs = str(config.get('jobs', 1))
        self.pluginpath = os.path.abspath(config['pluginpath'])
        self.make = config.get('make', 'make')
        return sclass

    def initialise(self, suite, work_dir, archtest_env):
        if shutil.which(self.ref_exe) is None:
            logger.error(f'spike not found: {self.ref_exe}')
            raise SystemExit(1)
        self.suite = suite
        self.work_dir = work_dir
        self.compile_cmd = ('riscv64-unknown-elf-gcc -march={0} -mabi=ilp32 -static -mcmodel=medany'
                            ' -fvisibility=hidden -nostdlib -nostartfiles'
                            f' -T {self.pluginpath}/env/link.ld -I {self.pluginpath}/env/ -I {archtest_env}')

    def build(self, isa_yaml, platform_yaml):
        ispec = utils.load_yaml(isa_yaml)['hart0']
        isa = ispec['ISA'].lower()
        self.isa = 'rv32i' + ''.join(x for x in 'mc' if x in isa.split('z')[0][4:])
        self.isa += ''.join('_' + z for z in ('zicsr', 'zifencei') if z in isa)

    def runTests(self, testList, cgf_file=None):
        makefile = os.path.join(self.work_dir, 'Makefile.' + self.name[:-1])
        if os.path.exists(makefile):
            os.remove(makefile)
        make = utils.makeUtil(makefilePath=makefile)
        make.makeCommand = self.make + ' -j' + self.num_jobs
        for testentry in testList.values():
            test = testentry['test_path']
            sig = os.path.join(testentry['work_dir'], self.name[:-1] + '.signature')
            compile_cmd = (self.compile_cmd.format(testentry['isa'].lower()) + f' {test} -o ref.elf'
                           + ' -D' + ' -D'.join(testentry['macros']))
            make.add_target(f'@cd {testentry["work_dir"]}; {compile_cmd}; '
                            f'{self.ref_exe} --isa={self.isa} --priv=m +signature={sig} +signature-granularity=4 ref.elf')
        make.execute_all(self.work_dir)
