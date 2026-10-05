#!/usr/bin/env python3
"""Repack a routed SimpleRisc image with a different PLL frequency.

Logic placement and routing remain identical. This does not rerun static timing:
retain the source timing report and validate the new clock on the physical board.
"""
import argparse
import json
import os
from pathlib import Path
import subprocess


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('source', type=Path, help='nextpnr simple_risc_pnr.json')
    parser.add_argument('output', type=Path, help='directory for the new JSON and SRAM bitstream')
    parser.add_argument('--freq-mhz', type=int, required=True)
    args = parser.parse_args()
    frequency = args.freq_mhz
    if frequency <= 0 or frequency % 3 or frequency // 3 > 64:
        parser.error('frequency must be a positive multiple of 3, at most 192 MHz')
    divider = next((d for d in [128,112,96,80,64,48,32,16,8,4,2]
                    if 500 <= frequency*d <= 1250), None)
    if divider is None:
        parser.error('no output divider keeps the PLL VCO between 500 and 1250 MHz')
    route = args.output/'simple_risc_pnr.json'
    if route.resolve() == args.source.resolve():
        parser.error('output must not overwrite the source routing')
    data = json.loads(args.source.read_text())
    plls = [c for c in data['modules']['top']['cells'].values() if c['type'] == 'rPLL']
    if len(plls) != 1 or 'NEXTPNR_BEL' not in plls[0]['attributes']:
        parser.error('source must contain exactly one placed rPLL')
    parameters = plls[0]['parameters']
    if parameters['FCLKIN'] != '27' or int(parameters['IDIV_SEL'], 2) != 8:
        parser.error('source must use the SimpleRisc 27 MHz input and divide-by-9 PLL')
    if parameters.get('DEVICE') != 'GW2A-18C' or any(parameters.get(name) != 'false'
            for name in ['DYN_IDIV_SEL', 'DYN_FBDIV_SEL', 'DYN_ODIV_SEL']):
        parser.error('source must use the static GW2A-18C PLL settings')
    parameters['FBDIV_SEL'] = format(frequency//3-1, '032b')
    parameters['ODIV_SEL'] = format(divider, '032b')
    args.output.mkdir(parents=True, exist_ok=True)
    route.write_text(json.dumps(data))
    suite = Path(os.environ.get('OSS_CAD_SUITE', str(Path.home()/'Documents/Libs/oss-cad-suite')))
    subprocess.run([str(suite/'bin/gowin_pack'), '-d', 'GW2A-18C', '-o',
                    str(args.output/'simple_risc.fs'), str(route)], check=True)
    print(f'{frequency} MHz, VCO {frequency*divider} MHz; placement and routing preserved')


if __name__ == '__main__':
    main()
