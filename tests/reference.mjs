// 통 기준값 테스트
//   1부 (R 없이 됨): tests/reference_values.json(R로 미리 계산한 값)과 통의 계산 함수를 비교
//   2부 (Rscript가 있을 때): 예시 자료로 화면에서 분석 23가지를 돌리고, 통이 만든 analysis.R을 R로 돌려 모든 효과를 비교
// 실행: cd tests && npm install && node reference.mjs        (2부 건너뛰기: node reference.mjs --no-r)
import { chromium } from 'playwright';
import http from 'http';
import fs from 'fs';
import os from 'os';
import path from 'path';
import { execFileSync } from 'child_process';
import { fileURLToPath } from 'url';

const HERE = path.dirname(fileURLToPath(import.meta.url)), ROOT = path.resolve(HERE, '..');
const REF = JSON.parse(fs.readFileSync(path.join(HERE, 'reference_values.json'), 'utf8'));
const NO_R = process.argv.includes('--no-r');
let pass = 0, fail = 0; const fails = [];
const ok = (name, good, detail = '') => { if (good) pass++; else { fail++; fails.push(`${name} ${detail}`); } console.log(`${good ? '  ✓' : '  ✗'} ${name}${detail ? '  ' + detail : ''}`); };
const near = (a, b, tol, rel = false) => a != null && b != null && isFinite(a) && isFinite(b) && Math.abs(a - b) <= (rel ? tol * Math.max(1, Math.abs(b)) : tol);
const maxDiff = (A, B) => { let m = 0; A.flat().forEach((v, i) => { m = Math.max(m, Math.abs(v - B.flat()[i])); }); return m; };

// ---- 정적 서버 ----
const MIME = { '.html': 'text/html; charset=utf-8', '.js': 'text/javascript', '.png': 'image/png', '.webp': 'image/webp', '.json': 'application/json', '.webmanifest': 'application/manifest+json', '.ico': 'image/x-icon' };
const server = http.createServer((req, res) => {
  const f = path.join(ROOT, decodeURIComponent(req.url.split('?')[0]));
  if (!f.startsWith(ROOT) || !fs.existsSync(f) || fs.statSync(f).isDirectory()) { res.writeHead(404); return res.end(); }
  res.writeHead(200, { 'Content-Type': MIME[path.extname(f)] || 'application/octet-stream' }); fs.createReadStream(f).pipe(res);
});
await new Promise(r => server.listen(0, '127.0.0.1', r));
const APP = ['stats-ladder.html', 'index.html'].find(f => fs.existsSync(path.join(ROOT, f)) && fs.statSync(path.join(ROOT, f)).size > 100000);   // tong 저장소에서는 index.html이 본체
const BASE = `http://127.0.0.1:${server.address().port}/${APP}`;
const exe = ['/opt/pw-browsers/chromium'].find(p => fs.existsSync(p));
const browser = await chromium.launch(exe ? { executablePath: exe } : {});

// =====================================================================
console.log('\n1부: 계산 함수 ↔ R 기준값');
{
  const p = await browser.newPage(); const errs = []; p.on('pageerror', e => errs.push(e.message));
  await p.goto(BASE + '?selftest'); await p.waitForFunction(() => window.tongCore);
  const res = await p.evaluate(REF => {
    const C = window.tongCore, rows = m => m;   // R 행렬은 행 배열로 들어옴
    const out = {};
    out.ttest = REF.ttest.map(c => C.twoSample(c.a, c.b));
    out.ols = REF.ols.map(c => { const f = C.ols(c.y, rows(c.X)); return f && { b: f.b, se: f.cov.map((r, i) => Math.sqrt(r[i])), p: f.p, R2: f.R2, pF: f.pF }; });
    out.olsSingular = C.ols(REF.ols_singular.y, REF.ols_singular.X);
    out.corr = C.corr(REF.corr.a, REF.corr.b);
    out.shapiro = C.shapiro(REF.shapiro.x);
    const X = REF.efa.X, cols = X[0].map((_, j) => X.map(r => r[j]));
    out.pca = C.efa(cols, 3, 'pca'); out.paf = C.efa(cols, 3, 'paf');
    out.bt = REF.bayes.t.map(c => C.BAYES.t(c.t, c.N, c.nu));
    out.br = REF.bayes.r.map(c => C.BAYES.r(c.r, c.n));
    out.bg = REF.bayes.reg.map(c => C.BAYES.reg(c.R2, c.N, c.p));
    return out;
  }, REF);
  REF.ttest.forEach((c, i) => { const a = res.ttest[i];
    ok(`t검정 ${i + 1} Student t·df·p`, near(a.tS, c.tS, 1e-10, true) && a.dfS === c.dfS && near(a.pS, c.pS, 1e-7), `t = ${a.tS.toFixed(4)}, p = ${a.pS.toFixed(5)} (R ${c.pS.toFixed(5)})`);
    ok(`t검정 ${i + 1} Welch t·df·p`, near(a.tW, c.tW, 1e-10, true) && near(a.dfW, c.dfW, 1e-9, true) && near(a.pW, c.pW, 1e-7), `df = ${a.dfW.toFixed(4)}, p = ${a.pW.toFixed(5)} (R ${c.pW.toFixed(5)})`);
  });
  REF.ols.forEach((c, i) => { const a = res.ols[i];
    const d = a && Math.max(...a.b.map((v, j) => Math.abs(v - c.b[j]) / Math.max(1, Math.abs(c.b[j]))), ...a.se.map((v, j) => Math.abs(v - c.se[j]) / Math.max(1e-12, Math.abs(c.se[j]))), ...a.p.map((v, j) => Math.abs(v - c.p[j])));
    ok(`회귀 ${i + 1}${i ? ' (단위가 아주 다른 변인)' : ''} B·SE·p·R²`, a && d < 1e-6 && near(a.R2, c.R2, 1e-9) && near(a.pF, c.pF, 1e-7), a ? `최대 차이 ${d.toExponential(1)}` : '계산 안 됨');
  });
  ok('회귀: 완전 공선성이면 멈춤 (R lm은 NA)', res.olsSingular === null && [].concat(REF.ols_singular.lm_na).length > 0);
  ok('피어슨 r', near(res.corr, REF.corr.r, 1e-12), `r = ${res.corr.toFixed(6)}`);
  ok('Shapiro–Wilk W·p', near(res.shapiro.W, REF.shapiro.W, 1e-6) && near(res.shapiro.p, REF.shapiro.p, 1e-5), `W = ${res.shapiro.W.toFixed(5)}, p = ${res.shapiro.p.toFixed(5)} (R ${REF.shapiro.p.toFixed(5)})`);
  const dP = maxDiff(res.pca.L, REF.efa.pca), dF = maxDiff(res.paf.L, REF.efa.paf), dML = maxDiff(res.paf.L, REF.efa.ml_factanal);
  ok('요인분석: 주성분 + 베리맥스 부하량', dP < 1e-4, `최대 차이 ${dP.toExponential(1)}`);
  ok('요인분석: 주축요인추출(PAF) + 베리맥스 부하량', dF < 1e-4 && res.paf.iter === REF.efa.paf_iter, `최대 차이 ${dF.toExponential(1)}, 반복 ${res.paf.iter}번 (R ${REF.efa.paf_iter}번)`);
  console.log(`    (참고) PAF와 R factanal(최대우도) 부하량의 최대 차이 ${dML.toFixed(3)} — 방법이 달라 조금 다른 것이 정상`);
  const db = arr => Math.max(...arr);
  ok('베이즈 인자: t검정 (JZS, r = √2/2)', db(res.bt.map((v, i) => Math.abs(v - REF.bayes.t[i].lbf))) < 1e-6, `log BF 최대 차이 ${db(res.bt.map((v, i) => Math.abs(v - REF.bayes.t[i].lbf))).toExponential(1)}`);
  ok('베이즈 인자: sleep 자료 = BayesFactor 공개값 17.25888', near(Math.exp(res.bt[1]), REF.bayes.sleep_BayesFactor_published, 1e-5, true), `${Math.exp(res.bt[1]).toFixed(5)}`);
  ok('베이즈 인자: 상관 (Ly et al., κ = 1)', db(res.br.map((v, i) => Math.abs(v - REF.bayes.r[i].lbf))) < 1e-6, `log BF 최대 차이 ${db(res.br.map((v, i) => Math.abs(v - REF.bayes.r[i].lbf))).toExponential(1)}`);
  ok('베이즈 인자: 회귀 (Zellner–Siow, r = √2/4)', db(res.bg.map((v, i) => Math.abs(v - REF.bayes.reg[i].lbf))) < 1e-6, `log BF 최대 차이 ${db(res.bg.map((v, i) => Math.abs(v - REF.bayes.reg[i].lbf))).toExponential(1)}`);
  ok('페이지 오류 없음', !errs.length, errs.join(' | '));
  await p.close();
}

// =====================================================================
let hasR = false; try { execFileSync('Rscript', ['--version'], { stdio: 'ignore' }); hasR = true; } catch (e) { /* 없음 */ }
if (NO_R || !hasR) console.log(`\n2부: 건너뜀 (${NO_R ? '--no-r' : 'Rscript를 찾지 못함'})`);
else {
  console.log('\n2부: 화면에서 분석 23가지 → 통이 만든 analysis.R을 R로 실행 → 효과 비교');
  const ctx = await browser.newContext({ viewport: { width: 1440, height: 900 }, acceptDownloads: true });
  const p = await ctx.newPage(); const errs = []; p.on('pageerror', e => errs.push(e.message)); p.on('dialog', d => d.accept());
  await p.goto(BASE); await p.evaluate(() => { localStorage.clear(); localStorage.setItem('ladder.noWelcome', 'true'); }); await p.reload();
  await p.waitForTimeout(300); await p.click('#splash').catch(() => {}); await p.waitForTimeout(300);
  await p.click('[data-hp=mode-ladder]'); await p.click('[data-nav=survey]'); await p.click('[data-sv-act=example]'); await p.waitForTimeout(200);
  await p.evaluate(() => { const sv = JSON.parse(localStorage.getItem('ladder.survey')); sv.demos[0].options = ['남성', '여성']; ['상담 경험 1차', '상담 경험 2차', '상담 경험 3차'].forEach((t, i) => sv.demos.push({ text: t, code: 'C' + (i + 1), type: 'choice', options: ['예', '아니오'] })); localStorage.setItem('ladder.survey', JSON.stringify(sv)); });
  await p.reload(); await p.waitForTimeout(400); await p.click('#splash').catch(() => {});
  await p.click('[data-nav=analysis]'); await p.waitForTimeout(300); await p.click('[data-an-act=demo]'); await p.waitForTimeout(800);
  const sel = (slot, label) => p.selectOption(`[data-slot=${slot}]`, { label });
  const chips = async (slot, names) => { for (const c of await p.$$(`[data-slot=${slot}]`)) { const lab = await c.evaluate(x => x.parentElement.innerText); if (names.some(n => lab.startsWith(n)) !== await c.isChecked()) await c.click(); } };
  const run = async (m, fix) => {
    await p.selectOption('#an-method', m); await p.waitForTimeout(200); if (fix) await fix();
    const n0 = await p.$$eval('.result-card', e => e.length); await p.click('[data-an-act=run]');
    await p.waitForFunction(n => document.querySelectorAll('.result-card').length > n || /[가-힣]{4}/.test((document.querySelector('#an-prog')?.textContent || '').replace(/부트스트랩.*|계산 중.*/, '')), n0, { timeout: 180000 });
    const prog = await p.$eval('#an-prog', e => e.textContent); ok(`실행: ${m}`, !prog || /부트스트랩|계산 중/.test(prog), prog);
  };
  await run('pearson');
  await run('spearman');
  await run('partial', async () => { await sel('x', '능력주의 신념'); await sel('y', '외집단 혐오'); await chips('covs', ['외로움']); });
  await run('pointbiserial', async () => { await sel('g', '성별 (2가지)'); await sel('y', '외로움'); });
  await run('chisq', async () => { await sel('a', '성별 (2가지)'); await sel('b', '현재 신분 (4가지)'); });
  await run('onesample', async () => { await sel('y', '외로움'); });
  await run('ind_t', async () => { await sel('g', '성별 (2가지)'); await chips('ys', ['외로움', '외집단 혐오']); });
  await run('paired_t', async () => { await sel('a', '능력주의 신념'); await sel('b', '외로움'); });
  await run('oneway', async () => { await sel('g', '현재 신분 (4가지)'); await chips('ys', ['외로움']); });
  await run('factorial', async () => { await sel('a', '성별 (2가지)'); await sel('b', '현재 신분 (4가지)'); await sel('y', '외집단 혐오'); });
  await run('ancova', async () => { await sel('g', '현재 신분 (4가지)'); await sel('y', '외집단 혐오'); await chips('covs', ['외로움']); });
  await run('regression', async () => { await sel('y', '외집단 혐오'); await chips('xs', ['능력주의 신념', '외로움', '현재 신분']); });
  await run('hier', async () => { await sel('y', '외집단 혐오'); await chips('b1', ['성별', '나이']); await chips('b2', ['능력주의 신념', '외로움']); await chips('b3', []); });
  await run('logistic', async () => { await sel('y', '성별 (2가지)'); await chips('xs', ['외로움', '외집단 혐오']); });
  await run('rm_anova', async () => { await chips('ys', ['능력주의 신념', '사회비교경향성', '외로움']); });
  await run('mixed_anova', async () => { await sel('g', '현재 신분 (4가지)'); await chips('ys', ['능력주의 신념', '사회비교경향성', '외로움']); });
  await run('mcnemar', async () => { await sel('a', '상담 경험 1차 (2가지)'); await sel('b', '상담 경험 2차 (2가지)'); });
  await run('cochran', async () => { await chips('vars', ['상담 경험 1차', '상담 경험 2차', '상담 경험 3차']); });
  await run('med_serial');
  await run('med_parallel', async () => { await sel('x', '능력주의 신념'); await chips('ms', ['사회비교경향성', '외로움']); await sel('y', '외집단 혐오'); });
  await run('moderation', async () => { await sel('x', '능력주의 신념'); await sel('w', '사회비교경향성'); await sel('y', '외집단 혐오'); });
  await run('modmed7', async () => { await sel('x', '능력주의 신념'); await sel('m', '외로움'); await sel('w', '사회비교경향성'); await sel('y', '외집단 혐오'); });
  await run('modmed14', async () => { await sel('x', '능력주의 신념'); await sel('m', '외로움'); await sel('w', '사회비교경향성'); await sel('y', '외집단 혐오'); });
  const an = await p.evaluate(() => JSON.parse(localStorage.getItem('ladder.analyses')));
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'tong-ref-'));
  const [d1] = await Promise.all([p.waitForEvent('download'), p.click('[data-an-act=r-data]')]); fs.copyFileSync(await d1.path(), path.join(dir, 'analysis_data.csv'));
  const [d2] = await Promise.all([p.waitForEvent('download'), p.click('[data-an-act=r-code]')]); fs.copyFileSync(await d2.path(), path.join(dir, 'analysis.R'));
  // 결과 카드의 [R 코드 보기]와 p값 너머 읽기가 그려졌는지
  const ui = await p.evaluate(() => ({ rcode: document.querySelectorAll('.result-card .r-code').length, beyond: document.querySelectorAll('.result-card .beyondp').length, bf: [...document.querySelectorAll('.result-card .section-title')].filter(x => x.textContent.startsWith('베이즈 인자')).length }));
  ok('결과마다 R 코드 보기', ui.rcode === an.length, `${ui.rcode}/${an.length}`);
  ok('베이즈 인자 표 (단일·독립·대응 t, 피어슨, 회귀, 위계적 회귀)', ui.bf === 6, `${ui.bf}개`);
  try { execFileSync('Rscript', ['analysis.R'], { cwd: dir, stdio: ['ignore', fs.openSync(path.join(dir, 'R_output.txt'), 'w'), 'pipe'], env: { ...process.env, LANG: 'C.UTF-8' } }); }
  catch (e) { ok('R 실행', false, String(e.stderr || e.message).slice(0, 400)); }
  const csv = fs.readFileSync(path.join(dir, 'R_results.csv'), 'utf8').replace(/^﻿/, '');
  const parse = line => { const o = []; let cur = '', q = false; for (const ch of line) { if (q) { if (ch === '"') q = false; else cur += ch; } else if (ch === '"') q = true; else if (ch === ',') { o.push(cur); cur = ''; } else cur += ch; } o.push(cur); return o; };
  const rrows = csv.trim().split('\n').slice(1).map(parse).map(([analysis, label, est, lo, hi, pv]) => ({ analysis, label, est: +est, lo: lo === 'NA' ? null : +lo, hi: hi === 'NA' ? null : +hi, p: pv === 'NA' ? null : +pv }));
  let nE = 0, worst = { est: 0, p: 0, ci: 0, boot: 0 };
  for (const a of an) {
    const mine = rrows.filter(r => r.analysis === a.id); let good = true; const why = [];
    for (const e of a.effects) {
      const r = mine.find(x => x.label === e.label); if (!r) { good = false; why.push(`R에 없음: ${e.label}`); continue; } nE++;
      const de = Math.abs(e.est - r.est) / Math.max(1, Math.abs(r.est)); worst.est = Math.max(worst.est, de); if (de > 1e-6) { good = false; why.push(`${e.label} 추정값 ${e.est} vs ${r.est}`); }
      if (e.p != null && r.p != null && isFinite(e.p)) { const dp = Math.abs(e.p - r.p); worst.p = Math.max(worst.p, dp); if (dp > 1e-5) { good = false; why.push(`${e.label} p ${e.p} vs ${r.p}`); } }
      if (e.lo != null && r.lo != null) { const dc = Math.max(Math.abs(e.lo - r.lo), Math.abs(e.hi - r.hi)); if (e.ciBased) { worst.boot = Math.max(worst.boot, dc); if (dc > 0.02) { good = false; why.push(`${e.label} 부트스트랩 CI`); } } else { worst.ci = Math.max(worst.ci, dc); if (dc > 1e-5) { good = false; why.push(`${e.label} CI`); } } }
    }
    ok(`R과 일치: ${a.title}`, good, why.slice(0, 3).join(' / '));
  }
  console.log(`    효과 ${nE}개 비교. 최대 차이: 추정값 ${worst.est.toExponential(1)}, p ${worst.p.toExponential(1)}, CI ${worst.ci.toExponential(1)}, 부트스트랩 CI ${worst.boot.toFixed(4)} (R과 난수가 달라 생기는 차이)`);
  ok('페이지 오류 없음', !errs.length, errs.join(' | '));
  fs.rmSync(dir, { recursive: true, force: true });
}

await browser.close(); server.close();
console.log(`\n${fail ? '✗' : '✓'} 통과 ${pass}, 실패 ${fail}`);
if (fail) { fails.forEach(f => console.log('  - ' + f)); process.exit(1); }
