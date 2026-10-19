"""Negative controls for the architecture regression gate and RTL cache."""
import copy
import contextlib
import io
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

import run_arch_tests as runner


def passing(image):
    count = 92 if runner.IMAGES[image][2] else 63
    result = {f'I/test-{i}': {'status': 'Pass', 'note': 'ARCH END 600d0003'} for i in range(count-7)}
    result.update({name: {'status': 'Too big' if image.startswith('bram') else 'Pass', 'note': ''}
                   for name in runner.BRAM_OVERSIZE})
    return result


class RegressionGate(unittest.TestCase):
    def test_complete_matrix_and_only_documented_skips(self):
        self.assertEqual(runner.validation_errors({i: passing(i) for i in runner.IMAGES}), [])

    def test_empty_or_partial_run_is_failure(self):
        for result in ({}, {'sdram-rvc': {}}, {'bram': {'I/beq-01': {'status': 'Too big', 'note': ''}}}):
            self.assertTrue(runner.validation_errors(result))

    def test_mismatch_missing_reference_and_unexpected_skip_fail(self):
        for status in ('Fail', 'no reference', 'Too big'):
            result = passing('sdram-rvc')
            result['I/test-0']['status'] = status
            self.assertTrue(runner.validation_errors({'sdram-rvc': result}))
        result = passing('bram')
        result['I/test-0']['status'] = 'Too big'
        self.assertTrue(runner.validation_errors({'bram': result}))

    def test_command_exits_unsuccessfully_after_empty_discovery(self):
        with tempfile.TemporaryDirectory() as temp, \
                patch('sys.argv', ['run_arch_tests.py', '--images', 'sdram-rvc', '--out', temp,
                                   '--riscof', __file__]), \
                patch.object(runner, 'arch_test_dir', return_value=Path(temp)), \
                patch.object(runner, 'build_model', return_value=Path(temp)/'model'), \
                patch.object(runner, 'run_riscof', return_value=({}, 0)), \
                contextlib.redirect_stdout(io.StringIO()):
            with self.assertRaisesRegex(SystemExit, 'expected 92 results, got 0'):
                runner.main()
            self.assertTrue((Path(temp)/'results.json').exists())

    def test_sources_and_rtl_must_match_generation_manifest(self):
        with tempfile.TemporaryDirectory() as temp:
            out = Path(temp)
            inputs = {'module': 'SdramPipeline', 'rvc': 1, 'sources': {'src/SimpleRisc.hs': 'old'}}
            calls = []

            def generate_command(*args, **kwargs):
                calls.append(args)
                entity = out/'gen/SdramPipeline-1/SdramPipeline.topEntity'
                entity.mkdir(parents=True, exist_ok=True)
                (entity/'cpu.v').write_text('module cpu; endmodule\n')

            with patch.object(runner, 'generation_inputs', side_effect=lambda *_: copy.deepcopy(inputs)), \
                    patch.object(runner, 'run', side_effect=generate_command):
                entity = runner.generate('SdramPipeline', 1, out, False)
                runner.generate('SdramPipeline', 1, out, False)
                self.assertEqual(len(calls), 1)
                inputs['sources']['src/SimpleRisc.hs'] = 'new'
                runner.generate('SdramPipeline', 1, out, False)
                self.assertEqual(len(calls), 2)
                (entity/'cpu.v').write_text('stale or corrupt RTL')
                runner.generate('SdramPipeline', 1, out, False)
                self.assertEqual(len(calls), 3)
                runner.generate('SdramPipeline', 1, out, True)
                self.assertEqual(len(calls), 4)
                manifest = out/'gen/SdramPipeline-1.json'
                self.assertEqual(json.loads(manifest.read_text())['inputs'], inputs)
                with patch.object(runner, 'run', side_effect=RuntimeError('compiler failed')):
                    with self.assertRaises(RuntimeError):
                        runner.generate('SdramPipeline', 1, out, True)
                self.assertFalse(manifest.exists())


if __name__ == '__main__':
    unittest.main()
