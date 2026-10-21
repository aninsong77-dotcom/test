// 통 사용법 영상: 장면마다 실제 화면을 찍고 자막·강조를 얹어 MP4로 만든다
import { chromium } from 'playwright';
import fs from 'fs';
import { execSync } from 'child_process';
const OUT = new URL('.', import.meta.url).pathname;
const FR = OUT + 'frames/tutorial/'; fs.rmSync(FR, { recursive: true, force: true }); fs.mkdirSync(FR, { recursive: true });
const URL0 = 'http://localhost:8765/stats-ladder.html';
const W = 1600, H = 900, DPR = 1.2;

const b = await chromium.launch({ executablePath: '/opt/pw-browsers/chromium' });
const ctx = await b.newContext({ viewport: { width: W, height: H }, deviceScaleFactor: DPR, acceptDownloads: true });
const p = await ctx.newPage(); const errs = []; p.on('pageerror', e => errs.push(e.message)); p.on('dialog', d => d.accept());
const card = await ctx.newPage();
const scenes = []; let chapter = '';
const wait = ms => p.waitForTimeout(ms);
const durOf = t => Math.min(12, Math.max(5, 2.8 + t.replace(/\s/g, '').length * 0.12));

async function overlay(cap, hi = [], opt = {}) {
  await p.evaluate(({ cap, hi, chap, spot, top }) => {
    document.querySelector('#__ov')?.remove();
    const o = document.createElement('div'); o.id = '__ov';
    o.style.cssText = 'position:fixed;inset:0;z-index:2147483646;pointer-events:none;font-family:"Nanum Gothic",sans-serif';
    const z = parseFloat(document.documentElement.style.zoom || 1) || 1;
    hi.forEach((sel, i) => {
      const el = typeof sel === 'string' ? document.querySelector(sel) : null; if (!el) return;
      const r = el.getBoundingClientRect(); const pad = 6;
      const d = document.createElement('div');
      d.style.cssText = `position:fixed;left:${r.left * z - pad}px;top:${r.top * z - pad}px;width:${r.width * z + pad * 2}px;height:${r.height * z + pad * 2}px;border:3px solid #f5841f;border-radius:12px;box-sizing:border-box;${spot && i === 0 ? 'box-shadow:0 0 0 4000px rgba(10,14,30,.38);' : 'box-shadow:0 0 0 3px rgba(245,132,31,.25);'}`;
      o.appendChild(d);
    });
    if (cap) {
      const c = document.createElement('div');
      c.style.cssText = `position:fixed;left:50%;transform:translateX(-50%);${top ? 'top:70px' : 'bottom:26px'};max-width:1240px;width:max-content;background:rgba(17,22,40,.93);color:#fff;border-radius:14px;padding:14px 26px 16px;font-size:25px;line-height:1.55;font-weight:700;text-align:center;letter-spacing:-.01em;box-shadow:0 8px 30px rgba(0,0,0,.25);word-break:keep-all`;
      c.innerHTML = (chap ? `<div style="display:inline-block;margin-bottom:6px;padding:2px 12px;border-radius:999px;background:#f5841f;color:#fff;font-size:15px;font-weight:800">${chap}</div><br>` : '') + cap;
      o.appendChild(c);
    }
    document.body.appendChild(o);
  }, { cap, hi, chap: opt.chap === false ? '' : chapter, spot: opt.spot !== false && hi.length === 1, top: !!opt.top });
}
async function shot(cap, hi = [], opt = {}) {
  if (opt.into) { await p.evaluate(s => document.querySelector(s)?.scrollIntoView({ block: opt_block(s) }), opt.into).catch(() => {}); }
  await wait(opt.wait ?? 350);
  await overlay(cap, hi, opt);
  await wait(120);
  const f = FR + String(scenes.length).padStart(3, '0') + '.png';
  await p.screenshot({ path: f });
  await p.evaluate(() => document.querySelector('#__ov')?.remove());
  scenes.push({ f, cap: cap.replace(/<br>/g, ' ').replace(/<[^>]+>/g, ''), dur: opt.dur || durOf(cap.replace(/<[^>]+>/g, '')) });
}
function opt_block() { return 'center'; }
async function into(sel, block = 'center') { await p.evaluate(({ sel, block }) => document.querySelector(sel)?.scrollIntoView({ block }), { sel, block }).catch(() => {}); await wait(300); }
async function cardShot(html, dur, cap) {
  await card.setContent(`<!doctype html><html><head><meta charset="utf-8"><link href="https://fonts.googleapis.com/css2?family=Nanum+Gothic:wght@400;700;800&display=swap" rel="stylesheet">
  <style>body{margin:0;width:${W}px;height:${H}px;display:flex;align-items:center;justify-content:center;background:#fff;font-family:'Nanum Gothic',sans-serif;color:#1d2333}
  .wrap{text-align:center}.logo svg{height:120px;width:auto}.wm{height:46px;width:auto;color:#1f2a52;margin-top:18px}
  h1{font-size:46px;margin:26px 0 10px;letter-spacing:-.02em}p{font-size:24px;color:#555d72;margin:6px 0}
  .n{display:inline-grid;place-items:center;width:84px;height:84px;border-radius:50%;background:#f5841f;color:#fff;font-size:40px;font-weight:800}
  .chap h1{font-size:52px}.chap p{font-size:26px}.list{display:grid;grid-template-columns:repeat(3,auto);gap:10px 40px;justify-content:center;margin-top:30px;font-size:21px;text-align:left}
  .list span b{color:#f5841f;margin-right:8px}.grp{color:#002f82;font-weight:800}.url{margin-top:26px;font-size:26px;font-weight:800;color:#2563eb}.copy{margin-top:20px;font-size:17px;color:#8a91a5}
  .bar{position:fixed;left:0;right:0;top:0;height:10px;background:#002f82}.bar2{position:fixed;left:0;right:0;bottom:0;height:10px;background:#f5841f}</style></head>
  <body><div class="bar"></div><div class="wrap">${html}</div><div class="bar2"></div></body></html>`);
  await card.waitForTimeout(700);
  const f = FR + String(scenes.length).padStart(3, '0') + '.png';
  await card.screenshot({ path: f });
  scenes.push({ f, cap: cap || '', dur });
}
async function chapCard(no, title, sub) { chapter = `${no} ${title}`; await cardShot(`<div class="chap"><div class="n">${no}</div><h1>${title}</h1><p>${sub}</p></div>`, 3.2, `${no} ${title}`); }
const click = async (sel, ms = 300) => { await p.click(sel); await wait(ms); };
const nav = id => click(`[data-nav=${id}]`, 450);

// ---------------- 시작 ----------------
await p.goto(URL0); await p.evaluate(() => { localStorage.clear(); localStorage.setItem('ladder.aiNoteClosed', 'true'); });
await p.reload(); await wait(900);
const LOGO = await p.evaluate(() => { const l = document.querySelector('.hdr-logo')?.outerHTML || ''; const w = document.querySelector('svg.wm')?.outerHTML || ''; return { l, w }; });
const logoHtml = `<div class="logo">${LOGO.l.replace(/class="[^"]*"/, 'class="tong"')}</div><div>${LOGO.w.replace('class="wm"', 'class="wm"')}</div>`;

await cardShot(`${logoHtml}<h1>통 사용법</h1><p>설문부터 통계 해석까지 · 사회과학 석사논문을 위한 무료 도구</p>
  <div class="list">
   <span class="grp">설계 · 응답 받기 전</span><span class="grp">자료 점검 · 응답 받은 뒤</span><span class="grp">가설 검증과 글쓰기</span>
   <span><b>1</b>통계 기법 고르기</span><span><b>6</b>응답 취합·정리</span><span><b>10</b>분석 결정·계산</span>
   <span><b>2</b>설문 만들기</span><span><b>7</b>표본 확인</span><span><b>11</b>결과 해석</span>
   <span><b>3</b>표본 수 계산</span><span><b>8</b>측정 점검</span><span><b>12</b>허점 점검</span>
   <span><b>4</b>모델 검토</span><span><b>9</b>사전 분석</span><span></span>
   <span><b>5</b>구글 폼 배포</span><span></span><span></span></div>`, 9, '통 사용법 — 설문부터 통계 해석까지. 12단계를 차례로 따라가 봅니다.');

// 시작 화면 (로고 애니메이션)
await p.reload(); await wait(1250);
await p.screenshot({ path: FR + String(scenes.length).padStart(3, '0') + '.png' }); scenes.push({ f: FR + String(scenes.length).padStart(3, '0') + '.png', cap: '', dur: 1.6 });
await wait(1500);
await p.screenshot({ path: FR + String(scenes.length).padStart(3, '0') + '.png' }); scenes.push({ f: FR + String(scenes.length).padStart(3, '0') + '.png', cap: '', dur: 1.6 });
await wait(1700); await p.click('#splash').catch(() => {}); await wait(600);
// 환영 카드 (자동화 브라우저에서는 꺼져 있어 한 번만 켜서 찍음)
await p.evaluate(() => localStorage.setItem('ladder.forceWelcome', 'true')); await p.reload(); await wait(800); await p.click('#splash').catch(() => {});
await p.waitForSelector('#welcome.in', { timeout: 8000 }).catch(() => {}); await p.mouse.move(5, 5); await wait(500);
await shot('처음 열면 환영 카드가 잠깐 나왔다 사라집니다. 사용법은 오른쪽 위 [? 사용 설명]에 있고, 귀찮으면 [다시 보지 않기]를 누르세요.', ['.wc-card'], { chap: false });
await p.evaluate(() => localStorage.removeItem('ladder.forceWelcome')); await p.click('[data-wc=start]').catch(() => {}); await wait(600);
await p.evaluate(() => { const s = document.querySelector('#scroller'); if (s) s.style.scrollBehavior = 'auto'; });

chapter = '화면 둘러보기';
await shot('주소 하나로 브라우저에서 바로 열립니다. 설치는 필요 없습니다.', [], { chap: false });
await shot('왼쪽 메뉴는 연구 진행 순서입니다.<br>끝낸 단계에는 ✓가 붙고, 지금 단계가 표시됩니다.', ['#side']);
await shot('가운데는 고르고 적는 곳입니다. 맨 아래 대화창에 궁금한 것을 물어볼 수 있습니다.', ['.center']);
await shot('오른쪽 보기 창에는 고른 통계 방법, 설문, 결과, 사용 설명이 늘 함께 보입니다.', ['#right']);
await shot('위 띠에는 연구 이름 단추(연구 여러 개 관리), 연구 폴더, 글씨 크기, 사용 설명이 있습니다.', ['.hdr, header']);

// ---------------- 1 기법 고르기 ----------------
await chapCard('1', '통계 기법 고르기', '변인과 모형 그림으로 가설을 만들면 기법이 정해집니다');
await shot('두 가지로 시작합니다. 가설이 있으면 [가설이 있어요]로 모형을 만들고, 아직 막연하면 쉬운 질문 5개로 후보를 좁힙니다.', ['.start-opts']);
await click('[data-hp=mode-hyp]');
await p.fill('#hp-title', '능력주의 신념이 외집단 혐오에 미치는 영향: 사회비교경향성과 외로움의 순차적 매개효과'); await wait(700);
await shot('논문 제목을 적으면 제목에서 모형을 짐작해 줍니다. 여기서는 순차매개 모형으로 읽었습니다.', ['#mb-tg-slot']);
await click('[data-mb-act=from-title]', 500);
await p.fill('#mb-newvar', '성별'); await p.press('#mb-newvar', 'Enter'); await wait(400);
await into('.mb-vars', 'center');
await shot('변인이 들어왔습니다. 점수(척도)인지, 성별처럼 집단으로 나뉘는지 고르고, 하위 차원이 있으면 개수와 이름을 적습니다.', ['.mb-vars']);
await into('.mb-card', 'start');
await shot('모형 카드의 빈칸에 원인·매개·결과 변인을 넣고 예상 방향을 고릅니다. 문장을 쓰는 대신 그림을 고르는 방식이라 말투와 상관없습니다.', ['.mb-card'], { spot: false });
await into('.mb-sent', 'center');
await shot('빈칸을 채우면 논문 문체의 가설 문장과 연구모형 그림이 저절로 만들어집니다.', ['.mb-sent']);
await into('.mb-an', 'start');
await shot('이 모형으로 할 수 있는 분석이 모두 나옵니다. 추천 표시가 기본이고, 계산 여부(통 계산·안내만)도 보입니다. [주 분석]과 [보조]를 고릅니다.', ['.mb-an'], { spot: false });
await p.fill('[data-mb="c.0.why"]', '두 매개변인의 순서를 이론으로 정했고, 순차 간접효과를 부트스트랩으로 검정하는 PROCESS Model 6을 쓴다.');
await into('.mb-why', 'center');
await shot('[이 분석을 고른 이유]를 내 말로 한두 줄 적습니다. 심사에서 꼭 묻는 질문이고, 분석 계획서에 그대로 남습니다.', ['.mb-why']);
const fillMb = async (key, v) => { await p.fill(`[data-mb="${key}"]`, v); await p.press(`[data-mb="${key}"]`, 'Tab'); await wait(200); };
await click('[data-mb-act=add-card]', 300);
await into('.mb-picker', 'center');
await shot('[+ 가설 추가]를 누르면 모형 카드가 나옵니다. 집단 차이 · 관계 · 영향 · 매개 · 순차매개 · 조절 · 조절된 매개 중에서 고릅니다.', ['.mb-picker']);
await click('.mb-picker [data-model=pred]', 300); await fillMb('c.1.x.0', '능력주의 신념'); await fillMb('c.1.y', '외집단 혐오');
await click('[data-mb-act=add-card]', 300); await click('.mb-picker [data-model=diff]', 300); await fillMb('c.2.x.0', '성별'); await fillMb('c.2.y', '외집단 혐오');
await p.selectOption('[data-mb="c.2.dir"]', '≠'); await wait(300);
await p.click('.rtab[data-rt=method]').catch(() => {}); await wait(300);
await shot('오른쪽 [통계 방법] 탭에 만들어진 가설과 기법이 모입니다: 가설 1 순차매개, 가설 2 단순회귀, 가설 3 독립표본 t검정.', ['#right']);
await into('[data-hp=apply]', 'center');
await shot('빈칸이 다 차면 [가설 확정하고 설문 만들기로]. 이미 써 둔 가설은 아래 [붙여 넣기]로 카드로 바꿀 수도 있습니다.', ['[data-hp=apply]', '.mb-paste'], { spot: false });
await click('[data-hp=apply]', 700);

// ---------------- 2 설문 ----------------
await chapCard('2', '설문 만들기', '변인마다 문항을 넣으면 구글 폼까지 만들어집니다');
await into('#view-survey .page-head', 'start');
await shot('가설의 변인이 설문에 들어왔습니다. 이제 변인마다 문항을 채웁니다.', ['#sv-main'], { spot: false });
await click('[data-sv-act=scale-src]', 500);
await shot('척도 원본(PDF·사진·복사한 글)을 넣으면 문항, 응답 선택지, 역문항을 찾아 자동으로 채웁니다.', ['#sv-import']);
await p.click('[data-si=close]').catch(() => {}); await wait(300);
await click('[data-sv-act=example]', 200); await click('[data-sv-act=example]', 500);
await into('#sv-main', 'start');
await shot('여기서는 예시 설문을 불러왔습니다. 문항 끝의 (역)은 거꾸로 채점하는 역문항입니다.', ['#sv-main'], { spot: false });
await click('[data-sv-act=preview]', 500);
await shot('[미리보기]로 응답자가 보게 될 화면을 구글 폼과 같은 쪽 나눔으로 확인합니다.', ['#right']);

// ---------------- 3 표본 수 ----------------
await chapCard('3', '표본 수 계산', '몇 명을 모아야 하는지 공식으로 계산합니다');
await nav('power');
await p.waitForFunction(() => !document.querySelector('#pw-busy')?.textContent, null, { timeout: 30000 }).catch(() => {});
await wait(300);
await shot('가설마다 정한 기법에 맞는 공식으로 필요한 인원을 계산합니다. G*Power와 같은 값이 나옵니다.', ['.pw-list']);
await shot('효과크기는 선행연구 값을 [직접 입력]으로 넣는 것이 가장 좋고, 없으면 보통 "중간"을 씁니다.', ['.pw-card .pw-es']);
await into('.pw-sum');
await shot('가장 큰 값이 최소 인원, 탈락률을 더한 값이 모집 목표입니다. 연구계획서에 쓸 문장도 만들어 줍니다.', ['.pw-sum']);

// ---------------- 4 모델 검토 ----------------
await chapCard('4', '모델 검토와 가설 확정', '응답을 받기 전에 모형을 한 번 더 따져 봅니다');
await nav('model');
await shot('가설로 연구 모형 그림을 그립니다. 가설을 고치면 그림도 바뀝니다.', ['.mv-model']);
await into('.mv-chk:nth-of-type(2)', 'start');
await shot('점검 목록: 한 시점 설문이면 인과를 주장할 수 없다는 점, 결론을 어떤 수위로 쓸지 알려 줍니다.', ['.mv-chk:nth-of-type(2)']);
const ctl = await p.$$('[data-mv-ctl]'); for (const c of ctl) { if ((await c.innerText()).includes('나이')) { await c.click(); await wait(300); break; } }
await into('.mv-chk:nth-of-type(3)');
await shot('통제변인(예: 나이)을 고르면 분석할 때 통제 칸에 저절로 들어갑니다.', ['.mv-chk:nth-of-type(3)']);
await p.fill('[data-mv-alt]', '사회비교이론에 따르면 남과 비교하는 경향이 먼저 있고, 그 결과 외로움을 느낀다.'); await p.click('#view-model h2'); await wait(300);
const ack = await p.$('[data-mv-ack=design]'); if (ack) { await ack.click(); await wait(300); }
await click('[data-mv-act=dry]', 300); await p.waitForFunction(() => document.querySelector('.mv-dry'), null, { timeout: 60000 }); await wait(400);
await into('.mv-dry', 'center');
await shot('[모의 실행]: 가짜 응답 300개로 처음부터 끝까지 돌려 봅니다.<br>성별 선택지가 세 가지(응답하지 않음 포함)라 t검정이 안 된다는 문제를 미리 찾았습니다.', ['.mv-dry']);
// 설문에서 성별 선택지 고치기
await nav('survey');
const gi = await p.$('[data-dm][data-f=options]');
if (gi) { await gi.fill('남성, 여성'); await gi.dispatchEvent('change'); await wait(300); await into('[data-dm][data-f=options]'); await shot('설문 만들기에서 성별 선택지를 "남성, 여성"으로 고쳤습니다. 응답을 받기 전이라 바로 고칠 수 있습니다.', ['[data-dm][data-f=options]']); }
await nav('model');
await click('[data-mv-act=dry]', 300); await p.waitForFunction(() => document.querySelector('.mv-dry li') && ![...document.querySelectorAll('.mv-dry li')].some(l => l.textContent.includes('3가지')), null, { timeout: 60000 }).catch(() => {}); await wait(400);
await into('.mv-dry', 'center');
await shot('다시 돌려 보니 모든 가설이 끝까지 계산됩니다.', ['.mv-dry']);
await into('.mv-qs', 'center');
await shot('심사에서 나올 만한 질문도 미리 보여 줍니다.', ['.mv-qs']);
await into('.mv-lock', 'center');
await click('[data-mv-act=lock]', 500);
await shot('[가설 확정하기]: 가설과 분석 계획을 날짜와 함께 잠급니다. 이후 계획에 없던 분석은 "탐색적 분석"으로 표시됩니다.', ['.cfm'], { top: true });
await click('.cfm-wrap [data-c="1"]', 500);
await into('.mv-lock', 'center');
await shot('왼쪽 메뉴에 🔒 가설 확정 날짜가 표시됩니다. 결과를 보고 가설을 바꾸지 않았다는 근거가 됩니다.', ['.side-lock', '.mv-lock'], { spot: false });

// ---------------- 5 구글 폼 ----------------
await chapCard('5', '구글 폼 배포', '스크립트를 한 번 실행하면 폼과 응답 시트가 생깁니다');
await nav('gform'); await wait(500);
await into('#sv-script', 'start');
await shot('[코드 복사]를 누르고, 구글 앱스 스크립트에 붙여 넣어 실행하면 내 드라이브에 구글 폼과 응답 시트가 생깁니다.', ['#sv-script']);
await p.evaluate(() => { location.hash = ''; });
await p.click('.rtab[data-rt=guide]').catch(() => {}); await wait(300);
await p.evaluate(() => { const a = [...document.querySelectorAll('#rp-guide a, #rp-guide button')].find(x => /구글 폼 배포/.test(x.textContent)); a?.click(); }); await wait(500);
await shot('따라 하기는 오른쪽 [사용 설명]에 단계별로 있습니다. 작업 화면을 그대로 두고 볼 수 있습니다.', ['#right']);

// ---------------- 6 응답 취합·정리 ----------------
await chapCard('6', '응답 취합·정리', '응답을 넣으면 점수를 만들고 자료를 정리합니다');
await nav('collect');
await p.click('#co-sample'); await wait(300);
let t = await p.$eval('#co-paste', e => e.value); const L = t.split('\n'); L.push(L[5]); await p.fill('#co-paste', L.join('\n'));
await shot('응답 시트를 CSV로 받아 넣거나 붙여 넣습니다. 예비조사 자료면 [예비조사]를 고릅니다.', ['#view-collect .card'], { spot: false });
await click('#co-read', 400);
const run = await p.$('button:has-text("점수 만들기")'); if (run) { await into('button:has-text("점수 만들기")'); await shot('문항 문장으로 열이 자동 연결됩니다. [점수 만들기]를 누릅니다.', ['#co-map']); await run.click(); await wait(600); }
await into('.dq-clean', 'start');
await shot('중복 응답, 이상치, 빠진 값을 자동으로 찾고, 몇 명이 왜 빠졌는지 흐름으로 보여 줍니다.', ['.dq-clean']);
await shot('이상치는 진짜 극단적인 사람일 수 있어 기본은 빼지 않습니다. 뺄지는 연구자가 정하고 기록에 남습니다.', ['.dq-box:nth-of-type(3)']);

// ---------------- 7~9 자료 점검 ----------------
await chapCard('7–9', '표본 확인 · 측정 점검 · 사전 분석', '논문 4장 앞부분의 표와 점검을 만들어 줍니다');
chapter = '7 표본 확인';
await nav('sample');
await shot('계획한 최소 인원을 채웠는지, 집단별 인원이 고른지 확인합니다.', ['.dq-target, .dq-flow']);
await into('#dq-sample .card:last-child', 'center');
await shot('표 1. 연구 대상자의 일반적 특성을 만들어 줍니다. CSV로 받아 한글·워드에 붙여 넣으세요.', ['#dq-sample .card:last-child']);
chapter = '8 측정 점검';
await nav('measure');
await shot('신뢰도: 척도마다 크론바흐 α와 살펴볼 문항을 보여 줍니다.', ['#dq-measure .card:first-of-type']);
await into('.dq-kv', 'start');
await shot('타당도: 탐색적 요인분석(KMO, 요인부하량). 문항이 원래 척도의 요인에 잘 묶이는지 봅니다.', ['#dq-measure .card:nth-of-type(2)'], { spot: false });
await into('.dq-harman', 'center');
await shot('공통방법편의(Harman 단일요인 검정)도 함께 확인합니다.', ['.dq-harman']);
chapter = '9 사전 분석';
await nav('prelim');
await shot('정규성: 왜도·첨도로 점수 분포가 크게 치우치지 않았는지 봅니다.', ['#dq-prelim .card:first-of-type']);
await into('#dq-prelim .card:nth-of-type(2)', 'start');
await shot('표 2. 주요 변인의 평균·표준편차·상관, 그리고 예측변인끼리 너무 겹치지 않는지(VIF) 확인합니다.', ['#dq-prelim .card:nth-of-type(2)'], { spot: false });

// ---------------- 10 분석 ----------------
await chapCard('10', '분석 결정·계산', '미리 골라 둔 기법으로 가설을 검증합니다');
await nav('analysis');
await shot('분석 방법과 변인이 가설대로 미리 채워집니다.', ['#an-main .card'], { spot: false });
await into('.dec-box', 'start');
await shot('분석 결정(사후검정, 부트스트랩 횟수 등)은 결과를 보기 전에 정하고, 나중에 바꾸면 "사후 변경"으로 기록됩니다.', ['.dec-box']);
const runNo = async no => {
  await p.click('.rtab[data-rt=method]').catch(() => {}); await wait(300);
  await p.click(`[data-hp-calc][data-hp-no="${no}"]`); await wait(400);
  const n0 = await p.$$eval('.result-card', e => e.length);
  await p.click('[data-an-act=run]'); await p.waitForFunction(n => document.querySelectorAll('.result-card').length > n, n0, { timeout: 120000 }); await wait(500);
};
await runNo(1); await runNo(2); await runNo(3);
await shot('가설마다 [계산]을 누르면 그 가설의 변인으로 계산합니다. 결과는 오른쪽 [결과] 탭에 논문 표 양식으로 나옵니다. 계획한 분석이라 "확증" 표시가 붙었습니다.', ['#an-results .result-card']);
await p.evaluate(() => { const s = document.querySelector('#an-method'); s.value = 'pearson'; s.dispatchEvent(new Event('change', { bubbles: true })); }); await wait(300);
const n1 = await p.$$eval('.result-card', e => e.length);
await click('[data-an-act=run]', 300); await p.waitForFunction(n => document.querySelectorAll('.result-card').length > n, n1, { timeout: 60000 }); await wait(500);
await shot('계획에 없던 분석을 더 하면 "탐색적" 표시가 붙습니다. 논문에는 추가 분석으로 따로 보고합니다.', ['#an-results .result-card:first-of-type .card-title']);
await into('#an-results .rpanel-r, #an-results .card', 'start');
await nav('rcheck'); await wait(500);
await shot('[R·SPSS로 확인하기]: 같은 분석을 R 코드나 SPSS 명령문(.sav 자료 파일도 만들어 줌)으로 다시 계산하고, 숫자가 같은지 대조합니다.', ['#an-results > .card:first-child']);

// ---------------- 11~12 해석 ----------------
await chapCard('11–12', '결과 해석 · 허점 점검', '판정은 코드가, 해석은 내가, 문장은 AI(또는 틀)가');
chapter = '11 결과 해석';
await nav('interp'); await wait(400);
await into('.hyps', 'start');
await shot('가설마다 검증할 효과가 저절로 연결되고, 코드가 정해진 규칙으로 지지·기각을 판정합니다. AI는 판정을 바꿀 수 없습니다.', ['.hyps .hyp:first-child'], { spot: false });
await p.fill('[data-in-h="0"][data-k=gist]', '능력주의 신념이 높을수록 외집단 혐오로 이어지는 간접효과가 있었고, 완전매개였다.'); await wait(900);
await into('[data-in-h="0"][data-k=gist]', 'center');
await shot('판정 아래 [내가 읽은 결과]에 결과가 무엇을 뜻하는지 내 말로 적습니다. 효과마다 읽는 법이 나오고, 적는 즉시 판정·방향·매개 해석이 맞는지 점검합니다.', ['.hyps .hyp:first-child .gist']);
await p.fill('[data-in-h="0"][data-k=gist]', '능력주의 신념이 높을수록 사회비교와 외로움을 차례로 거쳐 외집단 혐오로 이어지는 간접효과가 있었다.'); await wait(900);
await shot('고쳐 적으면 ✓가 나옵니다. 숫자는 다시 쓰지 않아도 됩니다.', ['.hyps .hyp:first-child .gist']);
await click('[data-in-act=tmpl]', 700);
await into('#in-out', 'start');
await shot('AI 없이도 [결과 문단 틀 만들기]로 판정과 숫자가 들어간 결과 문단을 만들고, 내가 읽은 결과는 논의 자리에 모아 둡니다.', ['#in-out'], { spot: false });
await p.click('.rtab[data-rt=results]').catch(() => {}); await wait(400);
await shot('오른쪽 [결과] 탭에서 가설 판정과 근거를 한눈에 봅니다.', ['#rp-results-top .card:first-child']);
chapter = 'AI 연결하기';
await chapCard('AI', 'AI 연결하기', '세 가지 방법 중 고를 수 있습니다');
await click('#ai-dot', 400);
await shot('대화창 왼쪽 아래 점을 누르면 AI 방식을 고릅니다: Claude 바로 쓰기 / 다른 AI 쓰기 / AI 쓰지 않기.', ['#ai-pop']);
await shot('어느 방식이든 응답 원자료는 AI에 보내지 않습니다. 연구 의도, 가설, 결과표만 씁니다.', ['#ai-pop .ap-f']);
await p.click('#ai-pop [data-ap=close]').catch(() => {}); await wait(300);
await p.click('[data-in-mode=auto]').catch(() => {}); await wait(400);
await into('.mode-tabs', 'start');
await shot('① Claude 바로 쓰기: claude.ai 링크로 열면 화면 안에서 바로 답합니다. 내가 읽은 결과를 뼈대로 결과·논의를 논문 문체로 풀어 씁니다.', ['.mode-tabs', '#in-main .notice'], { spot: false });
await p.click('[data-in-mode=chat]').catch(() => {}); await wait(400);
await into('.chat-steps', 'start');
await shot('② 다른 AI 쓰기: ChatGPT·Gemini 등 아무 AI. 지시문을 복사해 붙여 넣고, AI의 답을 다시 붙여 넣습니다. 무료 계정도 됩니다.', ['.chat-steps']);
await into('.paste-box', 'center');
await shot('AI가 쓴 초안을 [초안 점검]에 붙여 넣으면, 숫자가 결과표와 같은지, 판정과 맞는지, 인과 표현이 없는지 다시 점검합니다.', ['.paste-box']);
await p.click('[data-in-mode=code]').catch(() => {}); await wait(400);
await into('.chat-steps', 'start');
await shot('③ 연구 폴더 + Claude 데스크톱 앱(Pro 이상): 폴더를 열면 Claude가 규칙대로 해석·R 분석·허점 점검을 합니다. 터미널이 필요 없습니다.', ['.chat-steps']);
await click('#fold-chip', 400);
await shot('[📁 연구 폴더]를 연결하면 설문, 분석 계획서, R 코드, 결과가 PC 폴더에 저절로 저장됩니다. (크롬·엣지·웨일)', ['#fold-pop']);
await p.click('#fold-chip').catch(() => {}); await wait(300);
chapter = '12 허점 점검';
await nav('review'); await wait(500);
await shot('허점 점검: AI가 깐깐한 동료처럼 심사위원이 물을 만한 곳을 짚어 줍니다.', ['#rv-card']);

// ---------------- 끝 ----------------
chapter = '';
await cardShot(`${logoHtml}<h1>설문부터 통계 해석까지, 한곳에서</h1>
  <p>모형 카드로 가설 · 표본 수 · 모델 검토 · 설문 · 자료 점검 · 계산 · 내가 읽는 해석</p>
  <p class="url">aninsong77-dotcom.github.io/tong</p>
  <p class="copy">© 2026. An In-song. Distributed for free. (2026. 안인성. 무료 배포)</p>`, 8, '주소: aninsong77-dotcom.github.io/tong — 무료로 쓸 수 있습니다.');

console.log('scenes', scenes.length, 'total', scenes.reduce((s, x) => s + x.dur, 0).toFixed(1), 's', 'errs', errs);
fs.writeFileSync(OUT + 'tutorial_scenes.json', JSON.stringify(scenes, null, 1));
await b.close();
