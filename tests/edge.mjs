// 통 경계 상황 테스트 — 외부 점검(2026-10-21)에서 재현한 문제들이 다시 생기지 않는지 확인
//   cd tests && node edge.mjs
// 점검자가 만든 edge.mjs · hyp.mjs(보고서형)를 통과/실패 판정형으로 옮긴 것입니다. R은 필요 없습니다.
import { chromium } from 'playwright';
import http from 'http';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { execFileSync } from 'child_process';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const server = http.createServer((q, s) => { const f = path.join(ROOT, decodeURIComponent(q.url.split('?')[0])); if (!f.startsWith(ROOT) || !fs.existsSync(f) || fs.statSync(f).isDirectory()) { s.writeHead(404); return s.end(); } s.writeHead(200, { 'Content-Type': { '.js': 'text/javascript', '.json': 'application/json', '.webmanifest': 'application/manifest+json', '.png': 'image/png' }[path.extname(f)] || 'text/html; charset=utf-8' }); fs.createReadStream(f).pipe(s); });
await new Promise(r => server.listen(0, '127.0.0.1', r));
const APP = ['stats-ladder.html', 'index.html'].find(f => fs.existsSync(path.join(ROOT, f)) && fs.statSync(path.join(ROOT, f)).size > 100000);   // tong 저장소에서는 index.html이 본체
const BASE = `http://127.0.0.1:${server.address().port}/${APP}`;
const exe = ['/opt/pw-browsers/chromium'].find(p => fs.existsSync(p));
const browser = await chromium.launch(exe ? { executablePath: exe } : {});
let pass = 0, fail = 0; const fails = [];
const ok = (name, good, detail = '') => { if (good) pass++; else { fail++; fails.push(`${name} ${detail}`); } console.log(`${good ? '  ✓' : '  ✗'} ${name}${detail ? '  ' + detail : ''}`); };
const english = t => /Cannot read|undefined|is not a function|NaN/.test(t);

// 예시 설문을 깔고(필요하면 고친 뒤) 새로 연 페이지
async function open(editSurvey, q = '') {
  const ctx = await browser.newContext({ viewport: { width: 1440, height: 900 } });
  const p = await ctx.newPage(); const errs = [];
  p.on('pageerror', e => errs.push(e.message)); p.on('dialog', d => d.accept());
  await p.goto(BASE + q); await p.evaluate(() => { localStorage.clear(); localStorage.setItem('ladder.noWelcome', 'true'); }); await p.reload(); await p.waitForTimeout(700);
  await p.click('#splash').catch(() => {}); await p.click('[data-hp=mode-hyp]').catch(() => {});
  await p.click('[data-nav=survey]'); await p.click('[data-sv-act=example]'); await p.waitForTimeout(250);
  if (editSurvey) {
    await p.evaluate(src => { const sv = JSON.parse(localStorage.getItem('ladder.survey')); (new Function('sv', src))(sv); localStorage.setItem('ladder.survey', JSON.stringify(sv)); }, editSurvey);
    await p.reload(); await p.waitForTimeout(700); await p.click('#splash').catch(() => {});
  }
  const sv = await p.evaluate(() => JSON.parse(localStorage.getItem('ladder.survey')));
  return { p, errs, sv };
}
// 예시 응답(60명)을 받아 행을 고쳐서 넣고 점수 만들기
async function loadRows(p, edit) {
  await p.click('[data-nav=collect]'); await p.waitForTimeout(250);
  await p.evaluate(() => document.querySelector('#co-sample').click()); await p.waitForTimeout(250);
  const csv = await p.$eval('#co-paste', e => e.value);
  const parse = line => { const o = []; let c = '', q = false; for (let i = 0; i < line.length; i++) { const ch = line[i]; if (q) { if (ch === '"') { if (line[i + 1] === '"') { c += '"'; i++; } else q = false; } else c += ch; } else if (ch === '"') q = true; else if (ch === ',') { o.push(c); c = ''; } else c += ch; } o.push(c); return o; };
  const lines = csv.split('\n').map(parse), head = lines[0]; let rows = lines.slice(1);
  rows = edit ? edit(rows, head) || rows : rows;
  const txt = [head, ...rows].map(r => r.map(v => `"${String(v).replace(/"/g, '""')}"`).join(',')).join('\n');
  await p.$eval('#co-paste', (e, t) => { e.value = t; }, txt);
  await p.click('#co-read'); await p.waitForTimeout(250); await p.click('#co-run'); await p.waitForTimeout(500);
  return { head, rows, stats: (await p.$eval('#co-result .stats', e => e.innerText).catch(() => '')).replace(/\n/g, ' ') };
}
// 분석 하나 돌리고 결과(또는 오류 문구)를 돌려줌
async function run(p, method, roles = {}) {
  await p.click('[data-nav=analysis]'); await p.waitForTimeout(250);
  await p.selectOption('#an-method', method); await p.waitForTimeout(250);
  for (const [slot, val] of Object.entries(roles)) {
    if (Array.isArray(val)) { for (const c of await p.$$(`[data-slot=${slot}]`)) { const lab = await c.evaluate(x => x.parentElement.innerText); if (val.some(n => lab.startsWith(n)) !== await c.isChecked()) await c.click(); } }
    else { const opt = await p.$$eval(`[data-slot=${slot}] option`, (os, v) => os.find(o => o.textContent.startsWith(v))?.value, val); await p.selectOption(`[data-slot=${slot}]`, opt); }
  }
  const n0 = await p.$$eval('.result-card', e => e.length);
  await p.click('[data-an-act=run]');
  await p.waitForFunction(n => document.querySelectorAll('.result-card').length > n || (document.querySelector('#an-prog')?.textContent || '').replace(/부트스트랩.*|계산 중.*|보고 체크.*/, '').trim().length > 5, n0, { timeout: 240000 });
  if (await p.$$eval('.result-card', e => e.length) > n0) return { ok: true, text: await p.$eval('.result-card', e => e.innerText) };
  return { ok: false, text: await p.$eval('#an-prog', e => e.innerText) };
}
const scaleCols = (sv, head, si) => sv.scales[si].items.map(it => ({ c: head.indexOf(it.text), rev: !!it.reverse }));
const setScore = (row, cols, v, labels) => cols.forEach(({ c, rev }) => { row[c] = labels[(rev ? 6 - v : v) - 1]; });

// ===================== 1. 응답 정리 =====================
console.log('\n1. 응답 정리');
{ // 중복: 다른 시각에 낸 같은 답 → 기본은 포함, 목록에 제출 간격 표시
  const { p, errs } = await open();
  const r = await loadRows(p, rows => { const c = rows[5].slice(); c[0] = '2026/10/30 23:59:00'; rows.push(c); return rows; });
  ok('중복 응답은 기본으로 빼지 않음', /중복 0/.test(r.stats), r.stats.match(/제외[^)]*\)/)?.[0] || r.stats);
  await p.click('[data-nav=collect]').catch(() => {});
  const dq = await p.evaluate(() => document.body.innerText);
  ok('중복 목록에 제출 간격이 보임', /\d+(분|시간|일) 뒤 제출|1분 안에 제출/.test(dq), (dq.match(/R\d+ = R\d+[^\n]*/) || [''])[0]);
  ok('페이지 오류 없음 (중복)', !errs.length, errs.join(' | '));
}
{ // 동의 문항이 없는데 척도 문항에 '동의'가 들어가고 선택지가 동의 척도 → 아무도 비동의로 빠지면 안 됨
  const { p, sv, errs } = await open(`sv.consent.on = false; sv.scales[0].items[0].text = '나는 노력에 따라 보상이 달라져야 한다는 생각에 동의한다'; sv.scales[0].labels = ['전혀 동의하지 않는다', '동의하지 않는다', '보통이다', '동의한다', '매우 동의한다'];`);
  let low = 0;
  const r = await loadRows(p, (rows, head) => { const c = head.indexOf(sv.scales[0].items[0].text); rows.forEach(rw => { if (/동의하지/.test(rw[c])) low++; }); return rows; });
  ok('동의 문항이 없으면 척도 응답을 비동의로 빼지 않음', /비동의 0/.test(r.stats) && low > 0, `첫 문항 1~2점 ${low}명 · ${r.stats.match(/제외[^)]*\)/)?.[0] || r.stats}`);
  ok('페이지 오류 없음 (동의)', !errs.length, errs.join(' | '));
}
{ // 동의 문항이 있으면 비동의는 여전히 뺌 (예시 응답에 비동의 1명)
  const { p } = await open();
  const r = await loadRows(p);
  ok('동의 문항이 있으면 비동의 응답은 뺌', /비동의 1/.test(r.stats), r.stats.match(/제외[^)]*\)/)?.[0] || r.stats);
}
{ // 판정 함수: '아니'가 중간에 든 동의 답은 동의, 타임스탬프 읽기, CSV 수식 막기
  const ctx = await browser.newContext(); const p = await ctx.newPage();
  await p.goto(BASE + '?selftest'); await p.waitForFunction(() => window.tongCore);
  const t = await p.evaluate(() => { const C = window.tongCore; return {
    st: [C.parseStamp('2026/10/30 11:59:00 오후 GMT+9'), C.parseStamp('2026. 10. 30 오후 11:59:00'), C.parseStamp('10/30/2026 23:59:00'), C.parseStamp('2026-10-30 23:59')],
    csv: [C.csvCell('=HYPERLINK("x")'), C.csvCell('-1.5'), C.csvCell('+82 10'), C.csvCell('보통')] }; });
  ok('타임스탬프 네 가지 꼴(영어 폼 월/일/연도 포함)을 같은 시각으로 읽음', t.st.every(v => v === t.st[3]) && t.st[3] != null);
  ok('CSV: =로 시작하는 글은 막고 음수는 그대로', t.csv[0].startsWith(`"'=`) && t.csv[1] === '"-1.5"' && t.csv[2].startsWith(`"'+`) && t.csv[3] === '"보통"', t.csv.join(' '));
  await ctx.close();
}

// ===================== 1-2. 클리닝 보강 =====================
console.log('\n1-2. 클리닝: 주의 확인 · 불성실 기준 · 대상 조건 · 값 정리 · 직접 빼기');
{
  const { p, sv, errs } = await open(`sv.attn = [{ text: "이 문항에는 '그렇다'를 골라 주세요.", si: 1, answer: 3 }];`);
  let longId = '', revId = '';
  const r = await loadRows(p, (rows, head) => {
    const gc = head.indexOf(sv.demos[0].text), ac = head.indexOf(sv.demos[1].text);
    rows[2][gc] = '남'; rows[3][gc] = '여자'; rows[4][gc] = '모름';           // 선택지에 없는 값
    rows[5][ac] = '23세'; rows[6][ac] = '스물셋';                           // 숫자 정리
    rows[7][ac] = '1999'; rows[8][ac] = '250'; rows[9][ac] = '20대';          // 출생연도 · 범위 밖 · 연령대
    rows[11][gc] = '응답하지 않음';                                          // 답하지 않은 사람의 범주
    // 같은 번호 연속: 첫 15문항을 '보통이다'로 (나머지는 그대로)
    const it = sv.scales.flatMap((s0, si) => s0.items.map(x => ({ c: head.indexOf(x.text), lab: s0.labels }))).filter(x => x.c >= 0);
    it.slice(0, 15).forEach(x => { rows[10][x.c] = x.lab[2]; }); longId = 'R011';
    // 역문항 모순: 첫 척도의 모든 문항(정·역)에 '매우 그렇다'
    sv.scales[0].items.forEach(x => { rows[12][head.indexOf(x.text)] = sv.scales[0].labels[sv.scales[0].labels.length - 1]; }); revId = 'R013';
    return rows;
  });
  ok('주의 확인 문항: 틀린 응답은 기본으로 뺌', /주의 확인 [1-9]/.test(r.stats), r.stats.match(/제외[^)]*\)/)?.[0]);
  const box = await p.$eval('.dq-clean', e => e.innerText);
  ok('같은 번호 연속 응답을 찾음', box.includes(longId), (box.match(/같은 번호 연속[^\n]*\n[^\n]*/) || [''])[0].replace(/\n/g, ' '));
  ok('정문항·역문항 모순 응답을 찾음', box.includes(revId));
  ok('선택지에 없는 성별 값: 같은 뜻은 자동으로 바꾸고, 모르는 값은 고르게 함', /"남" 1명[^\n]*자동/.test(box) && /"모름" 1명[^\n]*선택지에 없는 값/.test(box));
  ok('나이 "23세", "스물셋"을 숫자로 바꿈', /"23세" 1명 → 23/.test(box) && /"스물셋" 1명 → 23/.test(box));
  ok('나이: 출생연도·범위 밖 값·연령대는 빈 값으로 두고 알림', /"1999" 1명 → 출생연도 같아요/.test(box) && /"250" 1명 → 나이로 보기 어려운 값/.test(box) && /"20대" 1명 → 빈 값/.test(box), (box.match(/"1999"[^\n]*/) || [''])[0]);
  ok('"응답하지 않음"은 인원과 상관없이 정하게 보여 줌', /"응답하지 않음" \d+명[^\n]*(답하지 않은 사람의 범주|선택지에 없는 값)/.test(box));
  const age0 = String(new Date().getFullYear() - 1999);
  await p.selectOption('select[data-dq-recode="1"][data-from="1999"]', age0); await p.waitForTimeout(400);
  ok('출생연도를 나이로 바꾸면 기록에 남음', (await p.evaluate(() => JSON.parse(localStorage.getItem('ladder.plan')).recode['1']['1999'])) === age0);
  // 범주 합치기: '모름'을 빈 값으로
  await p.selectOption('select[data-dq-recode="0"][data-from="모름"]', '');
  await p.waitForTimeout(400);
  const st = await p.evaluate(() => JSON.parse(localStorage.getItem('ladder.plan')));
  ok('값 바꾸기가 기록에 남음', st.recode?.['0']?.['모름'] === '' && st.log.some(l => /모름/.test(l.q)));
  // 연구 대상 조건: 나이 25 이상
  await p.selectOption('select[data-dq-elig=di]', '1'); await p.waitForTimeout(300);
  await p.fill('input[data-dq-elig=min]', '25'); await p.$eval('input[data-dq-elig=min]', e => e.dispatchEvent(new Event('change', { bubbles: true })));
  await p.click('[data-dq-act=elig-add]'); await p.waitForTimeout(400);
  const st2 = await p.$eval('#co-result .stats', e => e.innerText.replace(/\n/g, ' '));
  ok('연구 대상 조건(나이 25 이상)으로 거름', /대상 아님 [1-9]/.test(st2), st2.match(/제외[^)]*\)/)?.[0]);
  // 직접 빼기
  const target = await p.evaluate(() => [...document.querySelectorAll('#dq-ids option')][3].value);
  await p.fill('input[data-dq-man=id]', target); await p.fill('input[data-dq-man=why]', '자유 응답에 장난 글'); await p.click('[data-dq-act=manual-add]'); await p.waitForTimeout(400);
  const st3 = await p.$eval('#co-result .stats', e => e.innerText.replace(/\n/g, ' '));
  ok('응답 번호로 직접 빼고 이유를 남김', /직접 뺌 1/.test(st3) && (await p.evaluate(() => JSON.parse(localStorage.getItem('ladder.plan')).manual))[target] === '자유 응답에 장난 글');
  const rep = await p.$eval('.dq-report p', e => e.innerText);
  ok('연구 대상 절 문장에 제외 사유와 인원이 들어감', /연구 대상 조건에 맞지 않는 \d+명/.test(rep) && /부적절하다고 판단한 1명/.test(rep) && /주의 확인 문항 1개/.test(rep) && /최종 \d+명/.test(rep), rep.slice(0, 120));
  // 분석 화면: 두 집단 칸에 못 들어가는 세 가지 범주(성별에 '응답하지 않음')는 이유를 알려 줌
  await p.click('[data-nav=analysis]'); await p.waitForTimeout(250); await p.selectOption('#an-method', 'ind_t'); await p.waitForTimeout(300);
  const why = await p.$$eval('.slot-why', e => e.map(x => x.innerText).join(' '));
  ok('두 집단 칸에 없는 3가지 범주 변인은 이유와 고칠 곳을 알려 줌', /성별은\(는\) 3가지/.test(why) && /기본 정보 값 정리/.test(why), why.slice(0, 90));
  ok('페이지 오류 없음 (클리닝)', !errs.length, errs.join(' | '));
}
// 엑셀에서 다시 저장한 CSV (한글 윈도우 인코딩 CP949)
{
  const { p, errs } = await open();
  const base = await loadRows(p);
  const csv = await p.$eval('#co-paste', e => e.value);
  const buf = execFileSync('python3', ['-c', 'import sys; sys.stdout.buffer.write(sys.stdin.buffer.read().decode("utf-8").encode("cp949", "replace"))'], { input: Buffer.from(csv, 'utf8') });
  await p.setInputFiles('#co-file', { name: 'excel.csv', mimeType: 'text/csv', buffer: buf }); await p.waitForTimeout(400);
  await p.click('#co-run').catch(() => {}); await p.waitForTimeout(500);
  const st = (await p.$eval('#co-result .stats', e => e.innerText).catch(() => '')).replace(/\n/g, ' ');
  ok('엑셀에서 다시 저장한 CSV(CP949)도 한글이 깨지지 않고 같은 결과', st && st === base.stats, `${st.slice(0, 60)} / ${base.stats.slice(0, 60)}`);
  ok('페이지 오류 없음 (CP949)', !errs.length, errs.join(' | '));
  await p.context().close();
}

// ===================== 2. 계산이 안 되는 자료 =====================
console.log('\n2. 계산이 안 되는 자료 → 한국어 안내');
{ // 로지스틱: 완전 분리
  const { p, sv } = await open(`sv.demos[0].options = ['남성', '여성'];`);
  await loadRows(p, (rows, head) => { const cols = scaleCols(sv, head, 2), gc = head.indexOf(sv.demos[0].text); rows.forEach((rw, i) => { if (rw[1] !== '동의합니다') return; const v = i % 2 ? 4 : 2; setScore(rw, cols, v, sv.scales[2].labels); rw[gc] = v === 4 ? '여성' : '남성'; }); return rows; });
  const r = await run(p, 'logistic', { y: '성별', xs: ['외로움'] });
  ok('로지스틱 완전 분리 → 멈추고 안내', !r.ok && /분리/.test(r.text) && !english(r.text), r.text.slice(0, 80));
}
{ // 공분산분석·편상관: 두 공변인이 완전히 같음
  const { p, sv, errs } = await open();
  await loadRows(p, (rows, head) => { const a = scaleCols(sv, head, 1), b = scaleCols(sv, head, 2); rows.forEach((rw, i) => { if (rw[1] !== '동의합니다') return; const v = 1 + (i * 7) % 5; setScore(rw, a, v, sv.scales[1].labels); setScore(rw, b, v, sv.scales[2].labels); }); return rows; });
  const r = await run(p, 'ancova', { g: '현재 신분', y: '외집단 혐오', covs: ['사회비교경향성', '외로움'] });
  ok('공분산분석 · 같은 공변인 → 한국어 안내', !r.ok && /겹쳐|공선성/.test(r.text) && !english(r.text), r.text.slice(0, 90));
  const r2 = await run(p, 'partial', { x: '능력주의 신념', y: '외집단 혐오', covs: ['사회비교경향성', '외로움'] });
  ok('편상관 · 같은 통제변인 → 한국어 안내', !r2.ok && /겹쳐|공선성/.test(r2.text) && !english(r2.text), r2.text.slice(0, 90));
  ok('페이지 오류 없음 (공선성)', !errs.length, errs.join(' | '));
}
{ // 점이연: 결측을 빼면 한 집단만 남음
  const { p, sv } = await open(`sv.demos[0].options = ['남성', '여성'];`);
  await loadRows(p, (rows, head) => { const cols = scaleCols(sv, head, 3), gc = head.indexOf(sv.demos[0].text); rows.forEach(rw => { if (rw[1] !== '동의합니다') return; if (rw[gc] === '응답하지 않음') rw[gc] = '남성'; if (rw[gc] === '여성') cols.forEach(({ c }) => { rw[c] = ''; }); }); return rows; });
  const r = await run(p, 'pointbiserial', { g: '성별', y: '외집단 혐오' });
  ok('점이연 · 한 집단만 남음 → 이유 안내', !r.ok && /한 가지만|두 집단/.test(r.text), r.text.slice(0, 90));
}
{ // 매개: X가 두 집단이면 부분표준화
  const { p, sv } = await open(`sv.demos[0].options = ['남성', '여성'];`);
  await loadRows(p, (rows, head) => { const gc = head.indexOf(sv.demos[0].text); rows.forEach(rw => { if (rw[gc] === '응답하지 않음') rw[gc] = '여성'; }); return rows; });
  const r = await run(p, 'med_parallel', { x: '성별', ms: ['외로움'], y: '외집단 혐오' });
  ok('매개 · X가 성별(0/1) → 부분표준화 효과', r.ok && /부분표준화/.test(r.text) && !/완전표준화 효과/.test(r.text));
  const r2 = await run(p, 'med_parallel', { x: '능력주의 신념', ms: ['외로움'], y: '외집단 혐오' });
  ok('매개 · X가 점수 → 완전표준화 효과', r2.ok && /완전표준화 효과/.test(r2.text));
}

// ===================== 3. 기법 규칙 (화면의 질문 고르기는 없앴지만, 가설에서 기법을 정할 때 안에서 씀) =====================
console.log('\n3. 기법 규칙: 매개·조절은 맞는 기법에서만');
{
  const ctx = await browser.newContext(); const p = await ctx.newPage();
  await p.goto(BASE + '?selftest'); await p.waitForFunction(() => window.tongCore);
  const res = await p.evaluate(() => {
    const C = window.tongCore, R = C.rules(), seen = [], q = [[]];
    while (q.length) { const path = q.shift(); if (path.length > 14) continue; const w = C.walk(path);
      if (w.cur.type === 'result') { seen.push({ id: w.cur.id, base: w.cur.base || null }); continue; }
      if (w.cur.type === 'out') continue;
      const node = w.cur.type === 'gate' ? R.gate : R.nodes[w.cur.id]; node.options.forEach((_, k) => q.push(path.concat(k))); }
    const bad = seen.filter(s => s.base && !C.gateFor(s.base)).map(s => `${s.base}→${s.id}`);
    // 공분산분석: 세 집단 이상·기준 2가지 이상 경로에서는 매개·조절로 가지 않음
    const g3 = []; { const q2 = [[]]; while (q2.length) { const path = q2.shift(); if (path.length > 14) continue; const w = C.walk(path); if (w.cur.type === 'result') { if (w.cur.base === 'r_ancova' && w.steps.some(st => /세 집단|기준 2가지 이상/.test(st.opt.label))) g3.push(w.cur.id); continue; } if (w.cur.type === 'out') continue; const node = w.cur.type === 'gate' ? R.gate : R.nodes[w.cur.id]; node.options.forEach((_, k) => q2.push(path.concat(k))); } }
    bad.push(...g3.map(x => `세 집단 이상 공분산분석→${x}`));
    const gatedBases = [...new Set(seen.filter(s => s.base).map(s => s.base))];
    const noGate = [...new Set(seen.filter(s => !s.base && /^r_(onesample_t|paired_t|rm_anova|chisq|mcnemar|cochran|logistic|multilevel)$/.test(s.id)).map(s => s.id))];
    return { n: seen.length, bad, gatedBases, noGate };
  });
  ok('어떤 경로로도 단일표본·대응·반복측정·분류 결과 기법 뒤에 매개·조절이 붙지 않음', !res.bad.length, res.bad.slice(0, 5).join(', '));
  ok('그 기법들에는 질문 없이 바로 도착', res.noGate.length >= 6, res.noGate.join(', '));
  ok('회귀·상관·독립 t 뒤에서는 여전히 매개·조절을 물음', ['r_multiple_reg', 'r_pearson', 'r_ind_t'].every(b => res.gatedBases.includes(b)), `경로 ${res.n}개`);
  await ctx.close();
}

// ===================== 3-2. 시작 화면 · 변인만 있어요 =====================
console.log('\n3-2. 시작 화면 · 변인만 있어요');
{
  const ctx = await browser.newContext({ viewport: { width: 1366, height: 1000 } }); const p = await ctx.newPage(); const errs = []; p.on('pageerror', e => errs.push(e.message));
  await p.goto(BASE); await p.evaluate(() => { localStorage.clear(); localStorage.setItem('ladder.noWelcome', 'true'); }); await p.reload();
  await p.waitForTimeout(300); await p.click('#splash').catch(() => {}); await p.waitForTimeout(300);
  const opts = await p.$$eval('.start-opt', e => e.map(x => x.querySelector('b').textContent));
  ok('시작 화면: [가설이 있어요]·[변인만 있어요] (막연해요 없음)', opts.join(',') === '가설이 있어요,변인만 있어요', opts.join(','));
  ok('왼쪽 메뉴에 갈림길 규칙 없음, ①은 가설과 분석 정하기', await p.evaluate(() => !document.body.innerText.includes('갈림길 규칙') && /가설과 분석 정하기/.test(document.querySelector('nav, #nav, .nav')?.innerText || document.body.innerText)));
  await p.click('[data-hp=mode-vars]'); await p.waitForTimeout(300);
  for (const n of ['능력주의 신념', '외로움', '사회비교경향성', '외집단 혐오']) { await p.fill('#mb-newvar', n); await p.keyboard.press('Enter'); await p.waitForTimeout(150); }
  const sg = await p.$eval('.mb-sg', e => e.innerText);
  ok('변인 4개 → 결과 변인과 가설 후보 묶음(관계·매개·조절·순차)', /결과\(Y\)/.test(sg) && ['관계·영향', '매개', '조절', '순차 매개'].every(k => sg.includes(k)), sg.split('\n').slice(0, 4).join(' / '));
  await p.click('.mb-sg [data-mb-act=sg-y][data-v="외집단 혐오"]'); await p.waitForTimeout(200);
  const medBtn = await p.$('.mb-sg [data-mb-act=sg-add][data-t*="외로움은 능력주의 신념과 외집단 혐오의 관계를 매개"]');
  ok('매개 후보: "외로움은 능력주의 신념과 외집단 혐오의 관계를 매개할 것이다"', !!medBtn);
  if (medBtn) { await medBtn.click(); await p.waitForTimeout(300); }
  const cards = await p.evaluate(() => JSON.parse(localStorage.getItem('ladder.hyp') || 'null')?.cards || []);
  ok('[가설로]를 누르면 매개 모형 카드가 생김', cards.some(c => c.model === 'med' && c.m.includes('외로움') && c.y === '외집단 혐오'), cards.map(c => c.model).join(','));
  ok('만든 후보는 "카드로 만듦"으로 표시', /카드로 만듦/.test(await p.$eval('.mb-sg', e => e.innerText)));
  // 가설 문장 점검 · 경로마다 근거
  const chk = await p.$eval('.mb-card', e => e.innerText);
  ok('가설 문장 점검: 방향 빠짐·횡단 자료 인과·근거 없음', /방향\(정적·부적\)이 정해지지 않았어요/.test(chk) && /횡단 자료/.test(chk) && /근거\(이론·선행연구\)를 한 줄씩/.test(chk));
  await p.evaluate(() => { const d = document.querySelector('.mb-ev'); if (d) d.open = true; });
  const evIn = await p.$('.mb-ev input'); if (evIn) { await evIn.fill('사회비교이론(Festinger, 1954)'); await evIn.dispatchEvent('change'); await p.waitForTimeout(300); }
  const hp2 = await p.evaluate(() => JSON.parse(localStorage.getItem('ladder.hyp')));
  ok('경로 근거가 저장됨', Object.values(hp2.cards[0].ev || {}).includes('사회비교이론(Festinger, 1954)'), JSON.stringify(hp2.cards[0].ev || {}));
  // 다르게 생각해 보기
  await p.click('[data-mb-act=sg-close]').catch(() => {}); await p.waitForTimeout(200);
  await p.click('[data-mb-act=alt-toggle][data-ci="0"]'); await p.waitForTimeout(2500);
  const alt = await p.$eval('.mb-alt-body', e => e.innerText).catch(() => '');
  ok('[다르게 생각해 보기]: 다른 모형(영향·조절·조절된 매개)과 바뀌는 것', /다른 모형으로 보기/.test(alt) && /거쳐 가는 길 없이/.test(alt) && /조절로 보기/.test(alt) && /필요 인원 약/.test(alt), alt.split('\n').slice(0, 3).join(' / '));
  ok('기법 나란히 보기: 주 분석·구조방정식과 필요 인원·준비', /기법 나란히 보기/.test(alt) && /구조방정식/.test(alt) && /흔히 200명 이상/.test(alt) && /확인적 요인분석/.test(alt));
  await p.click('[data-mb-act=sim-run]'); await p.waitForFunction(() => !document.querySelector('[data-mb-act=sim-run][disabled]') && document.querySelectorAll('.mb-sim-ctl ~ .mb-cmp .mb-cmp-row').length > 0, null, { timeout: 60000 }).catch(() => {});
  const sim = await p.$eval('.mb-sim-ctl ~ .mb-cmp', e => e.innerText).catch(() => '');
  ok('가상 자료로 미리 돌려 보기: 모형마다 결과 모양과 찾을 확률', /간접효과/.test(sim) && /상호작용/.test(sim) && /찾을 확률 약 \d+%/.test(sim), sim.split('\n').slice(0, 3).join(' / '));
  await p.click('[data-mb-act=ai-ask]'); await p.waitForTimeout(500);
  ok('AI와 상의하기: AI를 정하기 전이면 [AI 함께 쓰기]를 엶', !!(await p.$('#welcome.setup')));
  await p.keyboard.press('Escape'); await p.waitForTimeout(500);
  const n0 = await p.$$eval('.mb-card', e => e.length);
  await p.click('.mb-alt-body [data-mb-act=alt-add][data-k="1"]'); await p.waitForTimeout(400);
  const heads = await p.$$eval('.mb-card .mb-head b', e => e.map(x => x.textContent));
  ok('[이 모형으로 가설 하나 더]: 지금 카드를 두고 새 카드를 만듦', heads.length === n0 + 1 && heads.includes('조절'), heads.join(','));
  ok('페이지 오류 없음 (시작 화면)', !errs.length, errs.join(' | '));
  await ctx.close();
}

// ===================== 4. 가설 붙여 넣기 =====================
console.log('\n4. 가설 붙여 넣기');
{
  const ctx = await browser.newContext({ viewport: { width: 1440, height: 900 } }); const p = await ctx.newPage(); const errs = []; p.on('pageerror', e => errs.push(e.message));
  await p.goto(BASE); await p.evaluate(() => { localStorage.clear(); localStorage.setItem('ladder.noWelcome', 'true'); }); await p.reload(); await p.waitForTimeout(700);
  await p.click('#splash').catch(() => {}); await p.click('[data-hp=mode-hyp]'); await p.waitForTimeout(300);
  const lines = [
    '능력주의 신념은 외집단 혐오에 정적 영향을 미칠 것이다.',
    '성별에 따라 외로움에 차이가 있을 것이다.',
    '사회비교경향성은 능력주의 신념과 외집단 혐오의 관계를 매개할 것이다.',
    '사회적 지지는 스트레스와 우울의 관계를 조절할 것이다.',
    '능력주의 신념이 외로움을 거쳐 외집단 혐오에 미치는 간접효과는 자존감에 따라 달라질 것이다.',
    '프로그램 참여 전보다 참여 후에 자기효능감이 높아질 것이다.',
    '성별에 따라 능력주의 신념이 외집단 혐오에 미치는 영향이 다를 것이다.',
    '사회적 지지가 높을수록 스트레스가 우울에 미치는 영향은 약해질 것이다.',
    '사회비교경향성은 능력주의 신념이 외집단 혐오에 미치는 영향을 매개할 것이다.',
    '능력주의 신념과 외집단 혐오의 관계에서 외로움의 매개효과는 자존감에 의해 조절될 것이다.',
    '자존감은 능력주의 신념이 외로움을 매개로 외집단 혐오에 미치는 간접효과를 조절할 것이다.',
    '학년에 따라 외로움이 진로불안에 미치는 영향은 달라질 것이다.',
  ];
  await p.click('.mb-paste summary'); await p.fill('#hp-text', lines.join('\n'));
  await p.click('[data-mb-act=paste-read]'); await p.waitForTimeout(700);
  const st = await p.evaluate(() => JSON.parse(localStorage.getItem('ladder.hyp')));
  const by = re => st.cards.find(c => re.test(c.src || ''));
  const c3 = by(/관계를 매개/), c7 = by(/간접효과는/), c4 = by(/조절할/);
  ok('"M은 X와 Y의 관계를 매개할 것이다"를 읽음', c3 && c3.model === 'med' && c3.x[0] === '능력주의 신념' && c3.m[0] === '사회비교경향성' && c3.y === '외집단 혐오', c3 ? `X=${c3.x} M=${c3.m} Y=${c3.y}` : '카드 없음');
  ok('"간접효과는 W에 따라 달라질 것이다"를 조절된 매개로 읽고 W를 남김', c7 && c7.model === 'modmed' && c7.w === '자존감' && c7.m[0] === '외로움', c7 ? `[${c7.model}] W=${c7.w}` : '카드 없음');
  ok('조절된 매개의 조절 경로가 문장에서 분명하지 않으면 학생에게 물음', c7 && !c7.wpath && (await p.$$eval('.mb-miss', e => e.map(x => x.innerText).join(' '))).includes('W가 바꾸는 경로'));
  ok('조절 가설은 그대로 읽음', c4 && c4.model === 'mod' && c4.w === '사회적 지지');
  const cs = [[/성별에 따라 능력주의/, 'mod', '능력주의 신념', '성별', '외집단 혐오'], [/높을수록 스트레스/, 'mod', '스트레스', '사회적 지지', '우울'], [/학년에 따라 외로움/, 'mod', '외로움', '학년', '진로불안'], [/자존감에 의해/, 'modmed', '능력주의 신념', '자존감', '외집단 혐오'], [/자존감은 능력주의/, 'modmed', '능력주의 신념', '자존감', '외집단 혐오']];
  cs.forEach(([re, model, X, W, Y]) => { const c = by(re); ok(`조절 문형: ${String(re).slice(1, -1)}`, c && c.model === model && c.x[0] === X && c.w === W && c.y === Y, c ? `[${c.model}] X=${c.x} W=${c.w} Y=${c.y}` : '카드 없음'); });
  const cm = by(/영향을 매개할/); ok('매개 문형: "M은 X가 Y에 미치는 영향을 매개할 것이다"', cm && cm.model === 'med' && cm.m[0] === '사회비교경향성' && cm.x[0] === '능력주의 신념' && cm.y === '외집단 혐오', cm ? `X=${cm.x} M=${cm.m} Y=${cm.y}` : '');
  ok('세 집단 이상인 조절변인(학년)은 매개·조절 계산 불가를 안내하고 완성으로 두지 않음', (await p.$$eval('.mb-warn', e => e.map(x => x.innerText).join(' '))).includes('세 집단 이상이면 PROCESS'));
  // 안전장치: 이름에 문장 조각이 남은 카드는 완성으로 두지 않음
  const ws = await p.evaluate(() => { const st = JSON.parse(localStorage.getItem('ladder.hyp')); st.cards.push({ ...st.cards[0], model: 'pred', x: ['능력주의 신념이 외로움'], m: [''], w: '', y: '미치는 간접효과', src: '(시험)', nameOk: false }); localStorage.setItem('ladder.hyp', JSON.stringify(st)); return st.cards.length; });
  await p.reload(); await p.waitForTimeout(700); await p.click('#splash').catch(() => {});
  ok('안전장치: 변인 이름에 문장 조각이 남으면 "확인해 주세요"로 표시하고 완성으로 두지 않음', (await p.$$eval('.mb-warn', e => e.map(x => x.innerText).join(' '))).includes('제대로 읽었는지'));
  const plan = await p.$$eval('.hp-plan .hp-row', e => e.map(x => x.innerText.replace(/\n/g, ' ')));
  const merged = plan.find(t => /정적 영향을 미칠/.test(t)) || '';
  ok('매개 카드에 합친 회귀 가설은 "매개분석 속 c 경로"로 표시', /속 c 경로/.test(merged), merged.slice(0, 80));
  ok('카드에 합친 사실과 [따로 분석하기]가 보임', (await p.$$('[data-mb-act=unmerge]')).length >= 1);
  await p.click('[data-mb-act=unmerge]'); await p.waitForTimeout(400);
  const st2 = await p.evaluate(() => JSON.parse(localStorage.getItem('ladder.hyp')));
  ok('[따로 분석하기]를 누르면 회귀 가설이 따로 카드가 됨', st2.cards.some(c => c.model === 'pred' && /정적 영향/.test(c.src || '')));
  ok('페이지 오류 없음 (가설)', !errs.length, errs.join(' | '));
  await ctx.close();
}

// ===================== 5. 가설 전부 계산하기 =====================
console.log('\n5. 가설 전부 계산하기');
{
  const ctx = await browser.newContext({ viewport: { width: 1440, height: 900 } }); const p = await ctx.newPage(); const errs = []; p.on('pageerror', e => errs.push(e.message)); p.on('dialog', d => d.accept());
  await p.goto(BASE); await p.evaluate(() => { localStorage.clear(); localStorage.setItem('ladder.noWelcome', 'true'); }); await p.reload(); await p.waitForTimeout(700);
  await p.click('#splash').catch(() => {}); await p.click('[data-hp=mode-hyp]'); await p.waitForTimeout(300);
  await p.fill('#hp-title', '능력주의 신념이 외집단 혐오에 미치는 영향: 사회비교경향성과 외로움의 순차적 매개효과'); await p.waitForTimeout(600);
  await p.click('[data-mb-act=from-title]').catch(() => {}); await p.waitForTimeout(300);
  // 경로 가설도 세움 (c, a, b 경로 → 상관·회귀·매개 결과가 섞임)
  const pc = await p.$('[data-mb="c.0.paths"]'); if (pc) { await pc.check(); await p.waitForTimeout(300); }
  await p.click('[data-hp=apply]').catch(() => {}); await p.waitForTimeout(700);
  await p.evaluate(() => {
    const sv = JSON.parse(localStorage.getItem('ladder.survey'));
    const bank = { '능력주의 신념': ['노력하면 누구나 성공할 수 있다', '성공은 개인의 능력에 달려 있다', '보상은 노력에 비례해야 한다', '가난은 노력 부족 때문이다'], '사회비교경향성': ['나는 내가 한 일을 다른 사람이 한 일과 자주 비교한다', '나는 다른 사람과 나를 비교하지 않는다', '나는 남의 성취에 관심이 많다'], '외로움': ['나는 함께할 사람이 없다고 느낀다', '나는 혼자라고 느낀다', '나는 주변 사람과 가깝다고 느낀다'], '외집단 혐오': ['나와 다른 집단의 사람들은 불편하다', '나는 다른 집단 사람과 어울리기 싫다', '다른 집단 사람도 친구가 될 수 있다'] };
    sv.scales.forEach(s => { const b = bank[s.name]; if (b) s.items = b.map((t, i) => ({ text: t, reverse: i === b.length - 1 })); });
    if (!sv.demos.length) sv.demos = [{ text: '성별', type: 'choice', options: ['남성', '여성'] }, { text: '나이', type: 'number' }];
    localStorage.setItem('ladder.survey', JSON.stringify(sv));
  });
  await p.reload(); await p.waitForTimeout(800); await p.click('#splash').catch(() => {});
  await p.click('[data-nav=collect]'); await p.waitForTimeout(250);
  await p.evaluate(() => document.querySelector('#co-sample').click()); await p.waitForTimeout(300);
  await p.click('#co-run'); await p.waitForTimeout(600);
  const nPlan = await p.evaluate(() => (JSON.parse(localStorage.getItem('ladder.hyp')).plan || []).length);
  await p.click('[data-nav=analysis]'); await p.waitForTimeout(400);
  ok('분석 화면에 [가설 전부 계산하기]가 있음', !!(await p.$('#an-main [data-an-act=run-all]')), `확정한 가설 ${nPlan}개`);
  await p.click('#an-main [data-an-act=run-all]');
  await p.waitForSelector('#an-runall', { timeout: 300000 });
  const rows = await p.$$eval('#an-runall .hp-row', e => e.map(x => x.innerText.replace(/\n/g, ' ')));
  const an = await p.evaluate(() => JSON.parse(localStorage.getItem('ladder.analyses')));
  ok('가설마다 결과 줄이 생김', rows.length === nPlan, rows.map(r => r.slice(0, 40)).join(' | '));
  ok('모든 가설이 계산됨', rows.every(r => /계산함/.test(r)), rows.filter(r => !/계산함/.test(r)).join(' | '));
  ok('같은 분석은 한 번만 계산 (경로 가설이 매개분석 결과를 함께 씀)', an.length >= 1 && an.length < nPlan, `가설 ${nPlan}개 → 분석 ${an.length}개`);
  ok('계산한 가설마다 [결과 보기]가 있음', (await p.$$('#an-runall [data-an-act=goto-res]')).length === nPlan);
  await p.click('#an-main [data-an-act=run-all]'); await p.waitForTimeout(1500);
  const an2 = await p.evaluate(() => JSON.parse(localStorage.getItem('ladder.analyses')));
  ok('다시 눌러도 같은 자료면 다시 계산하지 않음', an2.length === an.length);
  await p.click('[data-nav=interp]'); await p.waitForTimeout(500);
  const intent = await p.evaluate(() => JSON.parse(localStorage.getItem('ladder.intent')));
  ok('결과 해석에서 가설이 효과와 자동으로 이어짐', intent.hyps.every(h => h.effect), intent.hyps.map(h => `${!!h.effect}`).join(','));
  ok('페이지 오류 없음 (전부 계산)', !errs.length, errs.join(' | '));
  await ctx.close();
}

// ===================== 6. AI 연결: 기본은 AI 없이, 필요할 때 연결 =====================
console.log('\n6. AI 연결: 기본은 AI 없이, 필요할 때 연결');
{
  const ctx = await browser.newContext({ viewport: { width: 390, height: 844 }, permissions: ['clipboard-read', 'clipboard-write'] }); const p = await ctx.newPage(); const errs = []; p.on('pageerror', e => errs.push(e.message));
  await p.goto(BASE); await p.evaluate(() => { localStorage.clear(); localStorage.setItem('ladder.noWelcome', 'true'); }); await p.reload();
  await p.waitForTimeout(600); await p.click('#splash').catch(() => {}); await p.waitForTimeout(500);
  ok('처음에는 AI 방식을 묻지 않고 \'AI 정하기 전\'으로 시작', !(await p.$('#welcome.setup')) && /AI 정하기 전/.test(await p.$eval('#ai-dot', e => e.textContent)));
  await p.fill('#cmp-text', '표본은 몇 명이 필요해?'); await p.click('#cmp-send'); await p.waitForTimeout(400);
  ok('아래 칸은 [찾기]: 사용 설명에서 찾아 줌 (AI 답처럼 보이지 않음)', !!(await p.$('#thread .msg.find .links a')) && (await p.$eval('#cmp-send', e => e.textContent)) === '찾기');
  ok('AI를 정하기 전이면 옆 버튼이 [AI 함께 쓰기]', /^AI( 함께 쓰기)?$/.test(await p.$eval('#ai-open', e => !e.hidden && e.textContent)));   // 휴대폰 폭이면 'AI'
  await p.click('#ai-open'); await p.waitForSelector('#welcome.setup'); await p.waitForTimeout(300);
  const opts = await p.$$eval('#welcome input[name=su-mode]', es => es.map(e => e.value));
  ok('AI 함께 쓰기: 채팅 AI에 복사 · AI 도우미 · AI 없이 (claude.ai 링크 방식은 사이트에 안 보임)', opts.join(',') === 'copy,code,off', opts.join(','));
  ok('처음에는 무료인 [채팅 AI에 복사해 묻기]가 골라져 있음', await p.$eval('#welcome input[value=copy]', e => e.checked));
  const who = await p.$eval('#welcome', e => e.innerText);
  ok('방식마다 누구에게 맞는지와 도우미 세 가지가 보임', /구독이 없어도 됨/.test(who) && /연구 폴더로 관리하고 싶은 학생/.test(who) && ['Claude Code', 'Codex', 'Antigravity'].every(k => who.includes(k)), who.replace(/\s+/g, ' ').slice(0, 200));
  await p.click('#welcome input[value=copy]'); await p.waitForTimeout(100);
  await p.click('#welcome [data-ai-site=claude]'); await p.click('#welcome [data-su=next]');
  const sum = await p.$eval('#welcome .su-sum', e => e.innerText);
  await p.click('#welcome [data-su=done]'); await p.waitForTimeout(500);
  const st = await p.evaluate(() => ['ladder.aiMode', 'ladder.aiSite'].map(k => JSON.parse(localStorage.getItem(k))));
  ok('고른 방식과 AI가 저장되고 요약에 보임', st.join(',') === 'copy,claude' && /Claude/.test(sum), `${st} · ${sum.replace(/\s+/g, ' ')}`);
  ok('채팅 AI 방식이면 옆 버튼이 [Claude 열기]', /^Claude( 열기)?$/.test(await p.$eval('#ai-open', e => e.textContent)));
  await p.fill('#cmp-text', '표본은 몇 명이 필요해?');
  const [np] = await Promise.all([ctx.waitForEvent('page', { timeout: 5000 }).catch(() => null), p.click('#ai-open')]);
  const clip = await p.evaluate(() => navigator.clipboard.readText()).catch(() => '');
  ok('[Claude 열기]: 연구 요약과 질문을 복사하고 Claude 창을 엶', !!np && /claude\.ai\/new/.test(np.url()) && clip.length > 100 && /표본은 몇 명이 필요해/.test(clip), `${np ? np.url().slice(0, 30) : '창 없음'} · 복사 ${clip.length}자`);
  if (np) await np.close();
  // 결과 해석에서 권하기 (새로 시작한 상태)
  await p.evaluate(() => { localStorage.removeItem('ladder.aiMode'); }); await p.reload(); await p.waitForTimeout(500); await p.click('#splash').catch(() => {});
  await p.evaluate(() => document.querySelector('[data-nav=interp]')?.click()); await p.waitForTimeout(400);
  ok('결과 해석에 처음 오면 AI 연결을 권하는 카드', !!(await p.$('.ai-sug [data-setup-open]')));
  await p.click('.ai-sug [data-ai-skip]'); await p.waitForTimeout(300);
  ok('[AI 없이 계속]을 누르면 권하지 않음', !(await p.$('.ai-sug')) && JSON.parse(await p.evaluate(() => localStorage.getItem('ladder.aiMode'))) === 'off');
  // AI 도우미: Codex를 고르면 결과 해석 안내가 Codex에 맞게
  await p.evaluate(() => { localStorage.setItem('ladder.aiMode', '"code"'); localStorage.setItem('ladder.aiAgent', '"codex"'); localStorage.removeItem('ladder.interpIntent'); }); await p.reload(); await p.waitForTimeout(500); await p.click('#splash').catch(() => {});
  await p.evaluate(() => document.querySelector('[data-nav=interp]')?.click()); await p.waitForTimeout(400);
  const card = await p.evaluate(() => document.body.innerText);
  ok('AI 도우미를 Codex로 고르면 안내가 Codex·AGENTS.md로 바뀜', /Codex로 그 폴더 열기/.test(card) && /AGENTS\.md 받기/.test(card), (card.match(/.{0,20}Codex.{0,30}/) || [''])[0]);
  ok('페이지 오류 없음 (AI 연결)', !errs.length, errs.join(' | '));
  await ctx.close();
}
// AI 도우미: [Claude Code 열기]로 Claude 앱 Code 탭을 열고 거기서 대화 (통 아래 칸은 찾기만)
{
  const ctx = await browser.newContext({ viewport: { width: 1440, height: 900 } });
  await ctx.addInitScript(() => { window.showDirectoryPicker = async () => (await navigator.storage.getDirectory()).getDirectoryHandle('내논문', { create: true }); });
  const p = await ctx.newPage(); const errs = []; p.on('pageerror', e => errs.push(e.message));
  await p.goto(BASE); await p.evaluate(() => { localStorage.clear(); localStorage.setItem('ladder.noWelcome', 'true'); localStorage.setItem('ladder.aiMode', '"code"'); }); await p.reload();
  await p.waitForTimeout(600); await p.click('#splash').catch(() => {}); await p.waitForTimeout(400);
  ok('폴더를 연결하기 전에는 점에 "폴더 연결 전"이 보임', /폴더 연결 전/.test(await p.$eval('#ai-dot', e => e.textContent)));
  ok('AI 도우미(Claude)면 옆 버튼이 [Claude Code 열기]', (await p.$eval('#ai-open', e => !e.hidden && e.textContent)) === 'Claude Code 열기');
  await p.click('#fold-chip'); await p.waitForTimeout(300); await p.click('[data-fd=connect]'); await p.waitForTimeout(1500);
  const rd = rel => p.evaluate(async rel => { try { let d = await (await navigator.storage.getDirectory()).getDirectoryHandle('내논문'); const ps = rel.split('/'), n = ps.pop(); for (const x of ps) d = await d.getDirectoryHandle(x); return await (await (await d.getFileHandle(n)).getFile()).text(); } catch (e) { return ''; } }, rel);
  const md = await rd('CLAUDE.md');
  ok('안내 파일: 다른 도구는 써도 되지만 숫자·판정은 통 그대로', /다른 도구와 함께 쓸 때/.test(md) && /판정을 바꾸지 않는다/.test(md) && !/통 질문에 답해 줘/.test(md));
  const url0 = p.url();
  // 시험용 브라우저는 앱 링크(claude://)를 열 수 없어 '열기 시도'만 기록
  await p.evaluate(() => { const c = HTMLAnchorElement.prototype.click; HTMLAnchorElement.prototype.click = function () { if (/^claude:/.test(this.href)) { window.__opened = this.href; return; } return c.call(this); }; });
  await p.click('#ai-open'); await p.waitForTimeout(800);
  const tst = await p.$eval('#toast', e => e.textContent).catch(() => '');
  ok('[Claude Code 열기]: Claude 앱 Code 탭(claude://code/new)을 열고 폴더 고르는 법을 알려 줌', p.url() === url0 && (await p.evaluate(() => window.__opened)) === 'claude://code/new' && /Code 탭/.test(tst) && /내논문/.test(tst), tst.slice(0, 60));
  await p.fill('#cmp-text', '관계와 영향은 무슨 차이야?'); await p.click('#cmp-send'); await p.waitForTimeout(500);
  ok('AI 도우미 방식에서도 아래 칸은 사용 설명 찾기만 함', !!(await p.$('#thread .msg.find')) && !(await p.$('#thread .msg.pending')));
  ok('페이지 오류 없음 (AI 도우미 열기)', !errs.length, errs.join(' | '));
  await ctx.close();
}

// ===================== 7. 원척도 정보 · 기법별 필요한 준비 =====================
console.log('\n7. 원척도 정보 · 기법별 필요한 준비');
{
  const ctx = await browser.newContext({ viewport: { width: 1366, height: 1000 } }); const p = await ctx.newPage(); const errs = []; p.on('pageerror', e => errs.push(e.message)); p.on('dialog', d => d.accept());
  await p.goto(BASE); await p.evaluate(() => { localStorage.clear(); localStorage.setItem('ladder.noWelcome', 'true'); }); await p.reload();
  await p.waitForTimeout(300); await p.click('#splash').catch(() => {}); await p.waitForTimeout(300);
  await p.click('[data-hp=mode-hyp]'); await p.click('[data-nav=survey]'); await p.click('[data-sv-act=example]'); await p.waitForTimeout(300);
  await p.fill('[data-sc="0"][data-f="source"]', 'Son & Liu (2018)'); await p.fill('[data-sc="0"][data-f="origAlpha"]', '.91'); await p.fill('[data-sc="0"][data-f="origSample"]', '대학생 312명');
  await p.click('[data-nav=analysis]'); await p.waitForTimeout(300); await p.click('[data-an-act=demo]'); await p.waitForTimeout(800);
  await p.selectOption('#an-method', 'sem'); await p.waitForTimeout(300);
  await p.click('[data-nav=measure]'); await p.waitForTimeout(600);
  const rel = await p.$eval('#dq-measure .card:nth-of-type(2), #dq-measure', e => e.innerText);
  ok('신뢰도 표에 원척도 α 열', /원척도 α/.test(rel) && /\.91/.test(rel));
  const sent = await p.$eval('#ms-sent', e => e.innerText);
  ok('측정 도구 절 문장: 출처·원 연구 α·본 연구 α', /Son & Liu \(2018\)의 척도를 사용하여 측정하였다/.test(sent) && /원 연구\(대학생 312명\)에서 \.91이었으며, 본 연구에서는 \.\d\d(이었|였)다/.test(sent), sent.split('\n')[0].slice(0, 160));
  const prep = await p.$eval('#dq-measure .prep-card', e => e.innerText);
  ok('구조방정식을 고르면 확인적 요인분석이 필수 준비로 보임', /확인적 요인분석/.test(prep) && /필수/.test(prep) && /판별타당도/.test(prep) && /200명/.test(prep));
  await p.click('[data-nav=analysis]'); await p.waitForTimeout(300); await p.selectOption('#an-method', 'ind_t'); await p.waitForTimeout(300);
  const prep2 = await p.$eval('#an-main .prep-card', e => e.innerText);
  ok('t검정이면 정규성·등분산성이 준비로 보이고 확인적 요인분석은 없음', /정규성/.test(prep2) && /등분산성/.test(prep2) && !/확인적 요인분석/.test(prep2));
  ok('페이지 오류 없음 (준비)', !errs.length, errs.join(' | '));
  await ctx.close();
}

// ===================== 8. 앱처럼 설치 (서비스 워커 · 인터넷 없이 열기) =====================
console.log('\n8. 앱처럼 설치');
{
  const race = (pr, ms, v) => Promise.race([pr, new Promise(r => setTimeout(() => r(v), ms))]);
  const ctx = await browser.newContext(); const p = await ctx.newPage(); const errs = []; p.on('pageerror', e => errs.push(e.message));
  await p.goto(BASE); await p.evaluate(() => { localStorage.setItem('ladder.noWelcome', 'true'); });
  const reg = await race(p.evaluate(() => navigator.serviceWorker.ready.then(r => !!r.active).catch(() => false)), 15000, false);
  ok('서비스 워커가 등록됨', reg);
  await p.reload(); await p.waitForTimeout(800);
  ok('다시 열면 서비스 워커가 화면을 맡음', await race(p.evaluate(() => !!navigator.serviceWorker.controller), 5000, false));
  await ctx.setOffline(true);
  let offOk = false; try { await p.reload({ timeout: 15000 }); await p.waitForTimeout(800); offOk = await race(p.evaluate(() => !!document.querySelector('.hdr') && document.body.innerText.length > 100), 5000, false); } catch (e) { offOk = false; }
  ok('인터넷이 끊겨도 통이 열림', offOk);
  await ctx.setOffline(false);
  ok('페이지 오류 없음 (설치)', !errs.length, errs.join(' | '));
  await ctx.close();
  const ctx2 = await browser.newContext(); const p2 = await ctx2.newPage();
  await p2.goto(BASE + '#guide-install'); await p2.waitForTimeout(600);
  ok('사용 설명에 "앱처럼 설치하기"', /앱처럼 설치/.test(await race(p2.$eval('#gd-body', e => e.innerText).catch(() => ''), 5000, '')));
  // 영상으로 배우기: 사용 설명 맨 위에서 재생, 팝업, 환영 카드
  { const p2 = await ctx2.newPage(); const e2 = []; p2.on('pageerror', e => e2.push(e.message));
    await p2.goto(BASE + '?v=vid#guide-start'); await p2.waitForTimeout(900); await p2.click('#splash').catch(() => {}); await p2.waitForTimeout(300);
    const cards = await p2.$$eval('#gd-videos [data-vid-play]', e => e.map(x => x.dataset.vidPlay));
    ok('사용 설명 맨 위에 영상 두 개(통 사용법 · AI 함께 쓰기)', cards.join(',') === 'tutorial,ai', cards.join(','));
    await p2.click('#gd-videos [data-vid-play=tutorial]'); await p2.waitForTimeout(300);
    const src = await p2.$eval('#gd-videos video', v => v.getAttribute('src')).catch(() => '');
    ok('누르면 사용 설명 안에서 재생 (전체 화면 단추 있음)', /tong_tutorial\.mp4$/.test(src) && !!(await p2.$('#gd-videos [data-vid-fs]')), src);
    await p2.click('.gd-item:nth-of-type(3)').catch(() => {}); await p2.waitForTimeout(300);
    ok('사용 설명 쪽을 옮겨도 재생 중인 영상은 그대로', !!(await p2.$('#gd-videos video')));
    await p2.click('.gd-item[href="#guide-start"]'); await p2.waitForTimeout(300);
    await p2.click('#gd-body [data-video="ai"]'); await p2.waitForTimeout(300);
    ok('[▶ AI 함께 쓰기] 단추는 팝업으로 재생', /tong_ai_connect\.mp4$/.test(await p2.$eval('.vid-modal video', v => v.getAttribute('src')).catch(() => '')));
    await p2.keyboard.press('Escape'); await p2.waitForTimeout(200);
    ok('Esc로 팝업 닫힘', !(await p2.$('.vid-modal')));
    await p2.evaluate(() => { localStorage.setItem('ladder.forceWelcome', 'true'); localStorage.removeItem('ladder.noWelcome'); }); await p2.goto(BASE + '?v=wc'); await p2.waitForTimeout(600); await p2.click('#splash').catch(() => {});
    await p2.waitForSelector('#welcome [data-wc=video]', { timeout: 8000 }).catch(() => {});
    ok('환영 카드에 [▶ 영상으로 보기]', !!(await p2.$('#welcome [data-wc=video]')));
    await p2.click('#welcome [data-wc=video]').catch(() => {}); await p2.waitForTimeout(500);
    ok('환영 카드에서 누르면 통 사용법 영상 팝업', /tong_tutorial\.mp4$/.test(await p2.$eval('.vid-modal video', v => v.getAttribute('src')).catch(() => '')));
    ok('페이지 오류 없음 (영상)', !e2.length, e2.join(' | '));
    await p2.close(); }
  await ctx2.close();
}

await browser.close(); server.close();
console.log(`\n${fail ? '✗' : '✓'} 통과 ${pass}, 실패 ${fail}`);
if (fail) { fails.forEach(f => console.log('  - ' + f)); process.exit(1); }
