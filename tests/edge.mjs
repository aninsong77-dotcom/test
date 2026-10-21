// 통 경계 상황 테스트 — 외부 점검(2026-10-21)에서 재현한 문제들이 다시 생기지 않는지 확인
//   cd tests && node edge.mjs
// 점검자가 만든 edge.mjs · hyp.mjs(보고서형)를 통과/실패 판정형으로 옮긴 것입니다. R은 필요 없습니다.
import { chromium } from 'playwright';
import http from 'http';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const server = http.createServer((q, s) => { const f = path.join(ROOT, decodeURIComponent(q.url.split('?')[0])); if (!f.startsWith(ROOT) || !fs.existsSync(f) || fs.statSync(f).isDirectory()) { s.writeHead(404); return s.end(); } s.writeHead(200, { 'Content-Type': 'text/html; charset=utf-8' }); fs.createReadStream(f).pipe(s); });
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
  await p.click('#splash').catch(() => {}); await p.click('[data-hp=mode-ladder]').catch(() => {});
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
    st: [C.parseStamp('2026/10/30 11:59:00 오후 GMT+9'), C.parseStamp('2026. 10. 30 오후 11:59:00'), C.parseStamp('2026-10-30 23:59')],
    csv: [C.csvCell('=HYPERLINK("x")'), C.csvCell('-1.5'), C.csvCell('+82 10'), C.csvCell('보통')] }; });
  ok('타임스탬프 세 가지 꼴을 같은 시각으로 읽음', t.st.every(v => v === t.st[2]) && t.st[2] != null);
  ok('CSV: =로 시작하는 글은 막고 음수는 그대로', t.csv[0].startsWith(`"'=`) && t.csv[1] === '"-1.5"' && t.csv[2].startsWith(`"'+`) && t.csv[3] === '"보통"', t.csv.join(' '));
  await ctx.close();
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

// ===================== 3. 통계 기법 고르기 (사다리) =====================
console.log('\n3. 사다리: 매개·조절 질문은 맞는 기법에서만');
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
    const gatedBases = [...new Set(seen.filter(s => s.base).map(s => s.base))];
    const noGate = [...new Set(seen.filter(s => !s.base && /^r_(onesample_t|paired_t|rm_anova|chisq|mcnemar|cochran|logistic|multilevel)$/.test(s.id)).map(s => s.id))];
    return { n: seen.length, bad, gatedBases, noGate };
  });
  ok('어떤 경로로도 단일표본·대응·반복측정·분류 결과 기법 뒤에 매개·조절이 붙지 않음', !res.bad.length, res.bad.slice(0, 5).join(', '));
  ok('그 기법들에는 질문 없이 바로 도착', res.noGate.length >= 6, res.noGate.join(', '));
  ok('회귀·상관·독립 t 뒤에서는 여전히 매개·조절을 물음', ['r_multiple_reg', 'r_pearson', 'r_ind_t'].every(b => res.gatedBases.includes(b)), `경로 ${res.n}개`);
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

await browser.close(); server.close();
console.log(`\n${fail ? '✗' : '✓'} 통과 ${pass}, 실패 ${fail}`);
if (fail) { fails.forEach(f => console.log('  - ' + f)); process.exit(1); }
