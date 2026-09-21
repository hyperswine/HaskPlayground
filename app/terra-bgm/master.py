#!/usr/bin/env python3
"""Master a NeoMusic WAV for Terra II. Requires FFmpeg on PATH or FFMPEG env."""
import json
import os
import subprocess
import sys
import wave

if len(sys.argv) != 3:
    raise SystemExit('usage: master.py input.wav output.mp3')
source, target = sys.argv[1:]
ffmpeg = os.environ.get('FFMPEG', 'ffmpeg')
with wave.open(source) as wav:
    duration = wav.getnframes() / wav.getframerate()
# A short softened attack and a resolved, fading tail avoid abrupt repeat edges.
fade = f'afade=t=in:d=0.008,afade=t=out:st={duration-1.6:.6f}:d=1.6'
normalizer = 'loudnorm=I=-15:TP=-1.5:LRA=9'
measure = subprocess.run([ffmpeg, '-hide_banner', '-i', source, '-af',
    fade + ',' + normalizer + ':print_format=json', '-f', 'null', '-'],
    capture_output=True, text=True, check=True)
stats, _ = json.JSONDecoder().raw_decode(measure.stderr[measure.stderr.rfind('{'):])
settings = ':'.join(f'{key}={stats[value]}' for key, value in [
    ('measured_I','input_i'), ('measured_TP','input_tp'), ('measured_LRA','input_lra'),
    ('measured_thresh','input_thresh'), ('offset','target_offset')])
subprocess.run([ffmpeg, '-hide_banner', '-y', '-i', source, '-af',
    fade + ',' + normalizer + ':' + settings + ':linear=true',
    '-ar', '44100', '-ac', '2', '-codec:a', 'libmp3lame', '-b:a', '192k',
    '-id3v2_version', '3', '-metadata', 'title=Skyward, Little World',
    '-metadata', 'artist=NeoMusic', '-metadata', 'album=Terra II', target], check=True)
print(json.dumps({'source_duration': duration, 'input_loudness': stats, 'output': target}, indent=2))
