// 웹알 시험: 브라우저 속 lavaan(webR) 결과가 진짜 R lavaan(ref.json)과 같은지, 얼마나 걸리고 얼마나 받는지
import { chromium } from 'playwright';
import http from 'http';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
const HERE = path.dirname(fileURLToPath(import.meta.url));
const ref = JSON.parse(fs.readFileSync(path.join(HERE, 'ref.json'), 'utf8'));
const server = http.createServer((q, s) => { const f = path.join(HERE, decodeURIComponent(q.url.split('?')[0])); if (!fs.existsSync(f) || fs.statSync(f).isDirectory()) { s.writeHead(404); return s.end(); } s.writeHead(200, { 'Content-Type': f.endsWith('.html') ? 'text/html; charset=utf-8' : 'text/plain; charset=utf-8' }); fs.createReadStream(f).pipe(s); });
await new Promise(r => server.listen(0, '127.0.0.1', r));
const browser = await chromium.launch();
const page = await browser.newPage();
let bytes = 0, files = 0; const hosts = new Map();
page.on('response', async r => { try { const b = await r.body(); bytes += b.length; files++; const h = new URL(r.url()).host; hosts.set(h, (hosts.get(h) || 0) + b.length); } catch (e) {} });
page.on('console', m => console.log('  [page]', m.text()));
await page.goto(`http://127.0.0.1:${server.address().port}/probe.html`);
await page.waitForFunction(() => window.__done, null, { timeout: 15 * 60 * 1000 });
const res = await page.evaluate(() => ({ r: window.__result, e: window.__error, t: window.__timing, coi: window.__crossOriginIsolated }));
console.log('\n받은 양:', (bytes / 1048576).toFixed(1), 'MB,', files, '파일 ·', [...hosts].map(([h, b]) => `${h} ${(b / 1048576).toFixed(1)}MB`).join(', '));
console.log('시간:', JSON.stringify(res.t), '· crossOriginIsolated:', res.coi);
let fail = 0;
if (res.e || !res.r) { console.log('✗ 웹알에서 lavaan을 돌리지 못함:', res.e); fail++; }
else {
  console.log(`lavaan 버전: 웹알 ${res.r.lavaan} (R ${res.r.R}) / 진짜 R ${ref.lavaan} (R ${ref.R})`);
  const cmp = (name, a, b, tol) => { const d = Math.max(...a.map((v, i) => Math.abs(v - b[i]))); const ok = a.length === b.length && d < tol; if (!ok) fail++; console.log(`${ok ? '✓' : '✗'} ${name}: 최대 차이 ${d.toExponential(2)} (${a.length}개)`); };
  cmp('확인적 요인분석 적합도 (χ², df, p, CFI, TLI, RMSEA와 90% CI, SRMR)', res.r.cfa_fit, ref.cfa_fit, 1e-6);
  cmp('확인적 요인분석 표준화 부하량', res.r.cfa_std, ref.cfa_std, 1e-6);
  cmp('구조방정식 경로계수와 간접효과', res.r.sem_est, ref.sem_est, 1e-6);
  cmp('구조방정식 표준오차', res.r.sem_se, ref.sem_se, 1e-6);
  console.log('  적합도:', ref.cfa_fit_names.map((n, i) => `${n} ${(+res.r.cfa_fit[i]).toFixed(4)}`).join(', '));
}
await browser.close(); server.close();
console.log(fail ? `\n✗ 실패 ${fail}` : '\n✓ 웹알 lavaan = 진짜 R lavaan');
process.exit(fail ? 1 : 0);
