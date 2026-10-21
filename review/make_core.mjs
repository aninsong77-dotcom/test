// 통의 계산 부분만 모아 review/tong_calc_core.js로 저장 (다른 AI·사람이 검토하기 쉽게)
//   node review/make_core.mjs
// 통 본체(index.html, 개발 저장소에서는 stats-ladder.html)가 바뀌면 다시 돌리세요. 구간은 코드 안의 제목 주석으로 찾으므로 줄 번호가 바뀌어도 됩니다.
// 새 계산 구간을 만들면 아래 BLOCKS에 넣고, 빠진 구간이 없는지 끝의 '빠진 분석' 점검이 알려 줍니다.
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const HERE = path.dirname(fileURLToPath(import.meta.url));
const APP = ['stats-ladder.html', 'index.html'].map(f => path.join(HERE, '..', f)).find(f => fs.existsSync(f) && fs.statSync(f).size > 100000);   // tong 저장소에서는 index.html이 본체
const lines = fs.readFileSync(APP, 'utf8').split('\n');
const find = (text, from = 0) => { const i = lines.findIndex((l, k) => k >= from && l.includes(text)); if (i < 0) throw new Error('찾지 못함: ' + text); return i; };
const block = (title, begin, end) => {
  let a = find(begin); const b = find(end, a + 1);
  if (a > 0 && lines[a - 1].includes('// ====')) a--;
  return { title, a, b, text: `\n// ##### ${title} (${path.basename(APP)} ${a + 1}~${b}줄) #####\n` + lines.slice(a, b).join('\n') };
};
const BLOCKS = [
  block('1. 응답 읽기 · 동의 판정 · 점수 만들기(역문항, 최소 응답, 중복)', '  function autoMap() {', '    const missing = items.filter(d => colMap[d.key] < 0).length;'),
  block('2. 계산 도우미 · 분포 · 회귀(QR) · 부트스트랩 · 베이즈 인자 · 숫자 표기', '// ---------- 계산 도우미 ----------', '// ⑤ 분석'),
  block('3. 분석 방법 (RUN): 상관·t·분산분석·회귀·로지스틱·매개·조절', '// ⑤ 분석', '// ⑥ 결과 해석'),
  block('4. 분석 방법 (RUN 이어서): 반복측정·혼합설계 분산분석, 맥니마, 코크란 Q', '// 반복측정 · 혼합설계 분산분석, 맥니마, 코크란 Q', '// SPSS로 확인하기:'),
  block('5. 결과 해석: 학생이 읽은 결과 점검(gistCheck), 가설↔효과 연결(autoLink), 결과 문단 틀', '// ---------- 결과의 줄기', '// ---------- AI에게 보낼 자료'),
  block('6. 보고 체크리스트: 효과크기 신뢰구간, 정규성, 가정 점검', '// 보고 체크리스트 (APA 7', '// 허점 점검 (깐깐한 동료)'),
  block('7. 분석 결정 (기본값)', '// 분석 결정 (연구자가 정하고', '// R 코드 만들기'),
  block('8. R 코드 만들기 (재계산용)', '// R 코드 만들기', '// 연구 폴더 (PC)'),
  block('9. SPSS 명령문 만들기 · SPSS 출력과 대조', '// SPSS로 확인하기:', '// ---------- 시작 ----------'),
  block('10. 표본 수 계산 (G*Power 방식): 분포 함수', '// ---------- 표본 수 계산: 비중심 분포', '// 설계 점검:'),
  block('11. 표본 수 계산: 화면이 넘기는 자유도·비중심 모수', '// ---------- 표본 수 계산 ----------', '// ---------- 연구 모형 그림'),
  block('12. 탐색적 요인분석 (PAF·PCA, 베리맥스) · 확인적 요인분석 (webR lavaan, CR·AVE·HTMT)', '// ---------- 탐색적 요인분석', '// 자료 점검:'),
  block('13. 자료 점검: 이상치·결측·표본 확인·측정 점검·사전 분석', '// 자료 점검:', '// 반복측정 · 혼합설계 분산분석'),
];
// 빠진 분석 점검: 'async 이름(' 꼴의 분석 함수가 모두 들어갔나
const all = new Set(lines.map(l => (l.match(/^    async (\w+)\(/) || [])[1]).filter(Boolean));
const body = BLOCKS.map(b => b.text).join('\n');
const missing = [...all].filter(m => !body.includes(`async ${m}(`));
if (missing.length) { console.error('검토 파일에 빠진 분석:', missing.join(', ')); process.exit(1); }
const head = `// 통 — 계산 부분만 모은 파일 (검토용)
// 원본: ${path.basename(APP)} 한 파일(약 1만 줄, 화면·설문·AI 연결 포함)에서 통계 계산과 관련된 부분만 뽑았습니다.
// 이 파일만으로 실행되지는 않습니다(화면 코드와 이어져 있음). 실제 동작은 tests/로 R과 비교해 확인합니다.
// 검증: tests/README.md 참고 — R 기준값 비교, 화면에서 돌린 분석 전부를 통이 만든 R 코드와 비교, 경계 상황 테스트.
// 들어 있는 분석 함수 ${all.size}개: ${[...all].join(', ')}
`;
const out = head + body + '\n';
fs.writeFileSync(path.join(HERE, 'tong_calc_core.js'), out);
console.log(`tong_calc_core.js ${out.split('\n').length}줄, ${Math.round(Buffer.byteLength(out) / 1024)}KB, 분석 함수 ${all.size}개 모두 포함`);
