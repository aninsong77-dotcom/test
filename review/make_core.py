# 통의 계산 부분만 모아 review/tong_calc_core.js로 저장 (다른 AI·사람이 검토하기 쉽게)
#   python3 review/make_core.py
# stats-ladder.html이 바뀌면 다시 돌리세요. 구간은 코드 안의 제목 주석으로 찾으므로 줄 번호가 바뀌어도 됩니다.
import os
HERE = os.path.dirname(os.path.abspath(__file__))
src = open(os.path.join(HERE, '..', 'stats-ladder.html'), encoding='utf-8').read()
lines = src.split('\n')

def find(text, start=0):
    for i in range(start, len(lines)):
        if text in lines[i]:
            return i
    raise SystemExit(f'찾지 못함: {text}')

def block(title, begin, end):
    a = find(begin); b = find(end, a + 1)
    # 앞 줄이 ==== 띠면 같이 넣음
    if a > 0 and '// ====' in lines[a - 1]: a -= 1
    return f'\n// ##### {title} (stats-ladder.html {a + 1}~{b}줄) #####\n' + '\n'.join(lines[a:b])

parts = [
    block('1. 계산 도우미 · 분포 · 회귀(QR) · 부트스트랩 · 베이즈 인자 · 숫자 표기', '// ---------- 계산 도우미 ----------', '// ⑤ 분석'),
    block('2. 분석 방법 19가지 (RUN)', '// ⑤ 분석', '// ⑥ 결과 해석'),
    block('3. R 코드 만들기 (재계산용)', '// R 코드 만들기', '// 연구 폴더 (PC)'),
    block('4. 표본 수 계산 (G*Power 방식)', '// ---------- 표본 수 계산: 비중심 분포', '// 설계 점검:'),
    block('5. 탐색적 요인분석 (PAF·PCA, 베리맥스)', '// ---------- 탐색적 요인분석', '// 자료 점검:'),
]
head = '''// 통 — 계산 부분만 모은 파일 (검토용)
// 원본: stats-ladder.html 한 파일(약 1만 줄, 화면·설문·AI 연결 포함)에서 통계 계산과 관련된 부분만 뽑았습니다.
// 이 파일만으로 실행되지는 않습니다(화면 코드와 이어져 있음). 실제 동작은 tests/reference.mjs로 R과 비교해 확인합니다.
// 검증: tests/README.md 참고 — R 4.3.3 기준값과 비교 (분석 19가지의 효과 54개, t검정·회귀·요인분석·베이즈 인자).
'''
out = head + '\n'.join(parts) + '\n'
open(os.path.join(HERE, 'tong_calc_core.js'), 'w', encoding='utf-8').write(out)
print('tong_calc_core.js', out.count('\n'), '줄,', len(out.encode()) // 1024, 'KB')
