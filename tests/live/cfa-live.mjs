// 실제 사이트에서 브라우저 속 R(웹알)이 도는지: 크롬·사파리(WebKit)·파이어폭스 엔진으로 확인적 요인분석을 돌려 봄
//   node live/cfa-live.mjs            (LIVE_URL로 다른 주소 지정 가능)
import { chromium, webkit, firefox } from 'playwright';
const URL = process.env.LIVE_URL || 'https://aninsong77-dotcom.github.io/tong/';
let fail = 0;
for (const [name, type, dev] of [['크롬 (갤럭시·PC)', chromium, { viewport: { width: 390, height: 844 }, isMobile: true, hasTouch: true }], ['사파리 (아이폰)', webkit, { viewport: { width: 390, height: 844 }, isMobile: true, hasTouch: true }], ['파이어폭스', firefox, { viewport: { width: 1280, height: 900 } }]]) {
  const b = await type.launch(); const ctx = await b.newContext({ ...dev }); const p = await ctx.newPage();
  const errs = []; p.on('pageerror', e => errs.push(e.message)); p.on('dialog', d => d.accept());
  const t0 = Date.now();
  try {
    await p.goto(URL); await p.evaluate(() => { localStorage.clear(); localStorage.setItem('ladder.noWelcome', 'true'); }); await p.reload();
    await p.waitForTimeout(500); await p.click('#splash').catch(() => {}); await p.waitForTimeout(500);
    await p.evaluate(() => { document.querySelector('[data-hp=mode-ladder]')?.click(); document.querySelector('[data-nav=survey]')?.click(); });
    await p.waitForTimeout(300); await p.evaluate(() => document.querySelector('[data-sv-act=example]')?.click()); await p.waitForTimeout(300);
    await p.evaluate(() => document.querySelector('[data-nav=analysis]')?.click()); await p.waitForTimeout(300);
    await p.evaluate(() => document.querySelector('[data-an-act=demo]')?.click()); await p.waitForTimeout(800);
    await p.evaluate(() => document.querySelector('[data-nav=measure]')?.click()); await p.waitForTimeout(500);
    await p.evaluate(() => document.querySelector('[data-cfa=run]').click());
    await p.waitForFunction(() => window.tongCfaLast || document.querySelector('#cfa-card .errbox'), null, { timeout: 300000 });
    const r = await p.evaluate(() => window.tongCfaLast ? { cfi: window.tongCfaLast.fit[3], rmsea: window.tongCfaLast.fit[5], lav: window.tongCfaLast.lavaan, n: window.tongCfaLast.ld.std.length } : { err: document.querySelector('#cfa-card .errbox').innerText.slice(0, 300) });
    const sec = ((Date.now() - t0) / 1000).toFixed(0);
    if (r.err) { fail++; console.log(`✗ ${name}: ${r.err}`); }
    else console.log(`✓ ${name}: R(lavaan ${r.lav})이 돌아감 · CFI ${(+r.cfi).toFixed(4)}, RMSEA ${(+r.rmsea).toFixed(4)} · 부하량 ${r.n}개 · ${sec}초 (처음 내려받기 포함)`);
    // 두 번째 계산은 내려받은 R을 다시 씀
    const t1 = Date.now(); await p.evaluate(() => { window.tongCfaLast = null; document.querySelector('[data-cfa=run]').click(); });
    await p.waitForFunction(() => window.tongCfaLast || document.querySelector('#cfa-card .errbox'), null, { timeout: 120000 });
    console.log(`    다시 계산: ${((Date.now() - t1) / 1000).toFixed(1)}초`);
  } catch (e) { fail++; console.log(`✗ ${name}: ${e.message.split('\n')[0]}`); }
  if (errs.length) { fail++; console.log(`✗ ${name} 페이지 오류: ${errs.slice(0, 3).join(' / ')}`); }
  await b.close();
}
process.exit(fail ? 1 : 0);
