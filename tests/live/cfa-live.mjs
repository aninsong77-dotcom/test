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
    // 구조방정식 (크롬에서만): 실제 사이트에서 계산하고 결과를 JSON으로 남김 (화면 사진용)
    if (type === chromium) {
      const t2 = Date.now();
      await p.evaluate(() => document.querySelector('[data-nav=analysis]')?.click()); await p.waitForTimeout(400);
      await p.selectOption('#an-method', 'sem'); await p.waitForTimeout(300);
      const chips = async (slot, names) => { for (const c of await p.$$(`[data-slot=${slot}]`)) { const lab = await c.evaluate(x => x.parentElement.innerText); if (names.some(n => lab.startsWith(n)) !== await c.isChecked()) await c.evaluate(x => x.click()); } };
      await chips('xs', ['능력주의 신념']); await chips('ms', ['사회비교경향성', '외로움']); await p.selectOption('[data-slot=y]', { label: '외집단 혐오' }); await chips('covs', []);
      const n0 = await p.$$eval('.result-card', e => e.length);
      await p.evaluate(() => document.querySelector('[data-an-act=run]').click());
      await p.waitForFunction(n => document.querySelectorAll('.result-card').length > n || document.querySelector('#an-prog .a-low'), n0, { timeout: 600000 });
      const a = await p.evaluate(() => JSON.parse(localStorage.getItem('ladder.analyses')).find(x => x.method === 'sem') || null);
      if (!a) { fail++; console.log(`✗ ${name} 구조방정식: ${await p.$eval('#an-prog', e => e.textContent)}`); }
      else { console.log(`✓ ${name} 구조방정식: ${a.title} · ${((Date.now() - t2) / 1000).toFixed(0)}초`); console.log('SEMJSON ' + JSON.stringify(a)); }
    }
  } catch (e) { fail++; console.log(`✗ ${name}: ${e.message.split('\n')[0]}`); }
  if (errs.length) { fail++; console.log(`✗ ${name} 페이지 오류: ${errs.slice(0, 3).join(' / ')}`); }
  await b.close();
}
process.exit(fail ? 1 : 0);
