"""장면 목록(*_scenes.json)으로 MP4·자막(SRT)·대본(MD)을 만든다.
사용: python3 build.py tutorial   또는   python3 build.py ai_connect
"""
import json, subprocess, sys, os
HERE = os.path.dirname(os.path.abspath(__file__))
NAMES = {'tutorial': ('tong_tutorial', '통 사용법 영상 대본'), 'ai_connect': ('tong_ai_connect', '통 AI 연결하기 영상 대본')}
key = sys.argv[1] if len(sys.argv) > 1 else 'tutorial'
out, title = NAMES[key]
S = json.load(open(os.path.join(HERE, f'{key}_scenes.json'), encoding='utf-8'))
def ts(x):
    ms = int(round(x * 1000)); h, ms = divmod(ms, 3600000); m, ms = divmod(ms, 60000); s, ms = divmod(ms, 1000)
    return f'{h:02d}:{m:02d}:{s:02d},{ms:03d}'
L, srt, md, t, n = [], [], [], 0.0, 1
for x in S:
    L.append(f"file '{x['f']}'\nduration {x['dur']:.2f}")
    if x['cap']:
        srt.append(f"{n}\n{ts(t)} --> {ts(t + x['dur'] - 0.05)}\n{x['cap']}\n"); n += 1
        md.append(f"- **{int(t // 60)}:{int(t % 60):02d}** {x['cap']}")
    t += x['dur']
L.append(f"file '{S[-1]['f']}'")
lst = os.path.join(HERE, 'frames', f'{key}_list.txt')
open(lst, 'w', encoding='utf-8').write('\n'.join(L) + '\n')
open(os.path.join(HERE, f'{out}.srt'), 'w', encoding='utf-8').write('\n'.join(srt))
open(os.path.join(HERE, f'{out}_script.md'), 'w', encoding='utf-8').write('\n'.join([f'# {title}', '', f'총 길이 약 {int(t // 60)}분 {int(t % 60)}초', ''] + md) + '\n')
subprocess.run(['ffmpeg', '-loglevel', 'error', '-y', '-f', 'concat', '-safe', '0', '-i', lst,
                '-vf', 'fps=30,format=yuv420p,fade=t=in:st=0:d=0.6', '-c:v', 'libx264', '-preset', 'medium', '-crf', '21',
                '-tune', 'stillimage', '-movflags', '+faststart', os.path.join(HERE, f'{out}.mp4')], check=True)
print(f'{out}.mp4  {len(S)} scenes  {t:.1f}s')
