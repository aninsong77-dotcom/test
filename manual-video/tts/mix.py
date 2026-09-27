"""녹화 로그(<raw>.narr.json)의 시각대로 내레이션 클립을 배치해 narration.wav 생성. 사용: python3 mix.py raw.mjpeg.narr.json 길이(초)"""
import sys, json, numpy as np, soundfile as sf
n = json.load(open(sys.argv[1])); dur = float(sys.argv[2]); SR = 24000
buf = np.zeros(int(SR * (dur + 5)))
for x in n:
    a, sr = sf.read(x["f"])
    if a.ndim > 1: a = a.mean(1)
    if sr != SR: a = np.interp(np.arange(0, len(a) * SR / sr) * sr / SR, np.arange(len(a)), a)
    i = int(x["at"] * SR); buf[i:i + len(a)] += a[:len(buf) - i]
sf.write("narration.wav", buf[:int(SR * dur)] / max(1e-9, np.abs(buf).max()) * 0.9, SR)
