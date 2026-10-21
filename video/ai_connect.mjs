// 통 AI 연결 안내 영상: 네 가지 방식, 연결하는 법, 방식마다 쓸 수 있는 기능
import { chromium } from 'playwright';
import fs from 'fs';
const OUT = new URL('.', import.meta.url).pathname;
const FR = OUT + 'frames/ai_connect/'; fs.rmSync(FR, { recursive: true, force: true }); fs.mkdirSync(FR, { recursive: true });
const URL0 = 'http://localhost:8765/stats-ladder.html';
const W = 1600, H = 900, DPR = 1.2;
const SCAN = OUT + 'assets/scale_scan.png';

const b = await chromium.launch({ executablePath: '/opt/pw-browsers/chromium' });
const errs = [];
// 연구 폴더 연결을 찍기 위해: 폴더 고르기 창 대신 브라우저 안의 저장 공간을 '내논문' 폴더로 씀
const ctx = await b.newContext({ viewport: { width: W, height: H }, deviceScaleFactor: DPR, acceptDownloads: true });
await ctx.addInitScript(() => { window.showDirectoryPicker = async () => (await navigator.storage.getDirectory()).getDirectoryHandle('내논문', { create: true }); });
// claude.ai 링크에서 연 것처럼: Claude 연결 상태만 보여 주고 AI 답은 만들지 않음
const ctxC = await b.newContext({ viewport: { width: W, height: H }, deviceScaleFactor: DPR });
await ctxC.addInitScript(() => { window.claude = { use: async n => (n === 'sample' ? Object.assign(async () => ({ text: '' }), { json: async () => ({}) }) : null) }; });
let p = await ctx.newPage(); p.on('pageerror', e => errs.push(e.message)); p.on('dialog', d => d.accept());
const card = await ctx.newPage();
const scenes = []; let chapter = '';
const wait = ms => p.waitForTimeout(ms);
const durOf = t => Math.min(13, Math.max(5, 2.8 + t.replace(/\s/g, '').length * 0.12));
const fno = () => FR + String(scenes.length).padStart(3, '0') + '.png';

async function overlay(cap, hi = [], opt = {}) {
  await p.evaluate(({ cap, hi, chap, spot, top }) => {
    document.querySelector('#__ov')?.remove();
    const o = document.createElement('div'); o.id = '__ov';
    o.style.cssText = 'position:fixed;inset:0;z-index:2147483646;pointer-events:none;font-family:"Nanum Gothic",sans-serif';
    hi.forEach((sel, i) => {
      const el = document.querySelector(sel); if (!el) return;
      const r = el.getBoundingClientRect(), pad = 6, d = document.createElement('div');
      d.style.cssText = `position:fixed;left:${r.left - pad}px;top:${r.top - pad}px;width:${r.width + pad * 2}px;height:${r.height + pad * 2}px;border:3px solid #f5841f;border-radius:12px;box-sizing:border-box;${spot && i === 0 ? 'box-shadow:0 0 0 4000px rgba(10,14,30,.38);' : 'box-shadow:0 0 0 3px rgba(245,132,31,.25);'}`;
      o.appendChild(d);
    });
    if (cap) {
      const c = document.createElement('div');
      c.style.cssText = `position:fixed;left:50%;transform:translateX(-50%);${top ? 'top:70px' : 'bottom:26px'};max-width:1260px;width:max-content;background:rgba(17,22,40,.93);color:#fff;border-radius:14px;padding:14px 26px 16px;font-size:25px;line-height:1.55;font-weight:700;text-align:center;box-shadow:0 8px 30px rgba(0,0,0,.25);word-break:keep-all`;
      c.innerHTML = (chap ? `<div style="display:inline-block;margin-bottom:6px;padding:2px 12px;border-radius:999px;background:#f5841f;color:#fff;font-size:15px;font-weight:800">${chap}</div><br>` : '') + cap;
      o.appendChild(c);
    }
    document.body.appendChild(o);
  }, { cap, hi, chap: opt.chap === false ? '' : chapter, spot: opt.spot !== false && hi.length === 1, top: !!opt.top });
}
async function shot(cap, hi = [], opt = {}) {
  await wait(opt.wait ?? 350); await overlay(cap, hi, opt); await wait(120);
  const f = fno(); await p.screenshot({ path: f });
  await p.evaluate(() => document.querySelector('#__ov')?.remove());
  scenes.push({ f, cap: cap.replace(/<br>/g, ' ').replace(/<[^>]+>/g, ''), dur: opt.dur || durOf(cap.replace(/<[^>]+>/g, '')) });
}
async function into(sel, block = 'center') { await p.evaluate(({ sel, block }) => document.querySelector(sel)?.scrollIntoView({ block }), { sel, block }).catch(() => {}); await wait(300); }
const CSS = `body{margin:0;width:${W}px;height:${H}px;display:flex;align-items:center;justify-content:center;background:#f4f6fb;font-family:'Nanum Gothic',sans-serif;color:#1d2333}
  .wrap{text-align:center;max-width:1460px}.logo svg{height:110px;width:auto}.wm{height:42px;width:auto;color:#1f2a52;margin-top:16px}
  h1{font-size:44px;margin:22px 0 10px;letter-spacing:-.02em}h2{font-size:34px;margin:0 0 18px}p{font-size:23px;color:#555d72;margin:6px 0}
  .n{display:inline-grid;place-items:center;min-width:84px;height:84px;padding:0 14px;box-sizing:border-box;border-radius:42px;background:#f5841f;color:#fff;font-size:38px;font-weight:800}
  .bar{position:fixed;left:0;right:0;top:0;height:10px;background:#002f82}.bar2{position:fixed;left:0;right:0;bottom:0;height:10px;background:#f5841f}
  table{border-collapse:separate;border-spacing:0;width:100%;font-size:19px;background:#fff;border-radius:14px;overflow:hidden;box-shadow:0 4px 20px rgba(31,42,82,.08);text-align:left}
  th{background:#1f2a52;color:#fff;padding:12px 14px;font-size:19px}th small{display:block;font-weight:400;font-size:14px;opacity:.8}
  td{padding:10px 14px;border-top:1px solid #e2e5ee;vertical-align:top;line-height:1.45}td:first-child{font-weight:800;white-space:nowrap;background:#fafbfd}
  .y{color:#1f8a5b;font-weight:800}.m{color:#c2410c;font-weight:700}.x{color:#8a91a5}
  .box{background:#fff;border-radius:16px;padding:22px 28px;box-shadow:0 4px 20px rgba(31,42,82,.08);text-align:left;font-size:24px;line-height:1.7}
  .box li{margin:4px 0}.two{display:grid;grid-template-columns:1fr 1fr;gap:22px;text-align:left}
  .win{background:#fff;border-radius:14px;box-shadow:0 12px 40px rgba(31,42,82,.18);overflow:hidden;text-align:left;width:1200px;margin:0 auto}
  .wt{background:#e9ecf3;padding:10px 16px;display:flex;align-items:center;gap:8px;font-size:16px;color:#555d72}.wt i{width:12px;height:12px;border-radius:50%;background:#c9cdd8;display:inline-block}
  .addr{flex:1;margin-left:16px;background:#fff;border-radius:999px;padding:6px 16px;font-size:17px;color:#1d2333}
  .wb{padding:22px 26px;font-size:20px;line-height:1.6}.lab{position:fixed;top:24px;right:30px;background:#fff3e8;color:#c2410c;border:1px solid #fbd3ad;border-radius:999px;padding:4px 14px;font-size:16px;font-weight:700}
  .msg{border-radius:14px;padding:12px 16px;margin:10px 0;max-width:86%;font-size:18px;line-height:1.55;white-space:pre-wrap}.me{background:#eef3ff;margin-left:auto}.ai{background:#f6f7fb}
  .tag{display:inline-block;background:#f5841f;color:#fff;border-radius:6px;padding:0 8px;font-size:15px;margin-right:6px}
  .tabs span{display:inline-block;padding:6px 16px;border-radius:8px;margin-right:6px;background:#f1f3f8}.tabs .on{background:#1f2a52;color:#fff}
  .tree{font-family:monospace;font-size:19px;line-height:1.55;white-space:pre}.new{color:#c2410c;font-weight:800}`;
async function cardShot(html, dur, cap, label) {
  await card.setContent(`<!doctype html><html><head><meta charset="utf-8"><link href="https://fonts.googleapis.com/css2?family=Nanum+Gothic:wght@400;700;800&display=swap" rel="stylesheet"><style>${CSS}</style></head>
  <body><div class="bar"></div>${label ? `<div class="lab">${label}</div>` : ''}<div class="wrap">${html}</div><div class="bar2"></div></body></html>`);
  await card.waitForTimeout(700);
  const f = fno(); await card.screenshot({ path: f });
  scenes.push({ f, cap: cap || '', dur: dur || durOf(cap || '') });
}
const DRAWN = '그림 · 실제 화면과 조금 다를 수 있습니다';
async function chapCard(no, title, sub) { chapter = `${no} ${title}`; await cardShot(`<div class="n">${no}</div><h1>${title}</h1><p>${sub}</p>`, 3.4, `${title}`); }
const click = async (sel, ms = 300) => { await p.click(sel); await wait(ms); };
const nav = id => click(`[data-nav=${id}]`, 450);
async function setMode(m) { await click('#ai-dot', 300); await p.click(`#ai-pop input[value=${m}]`).catch(() => {}); await wait(300); await p.click('#ai-pop [data-ap=close]').catch(() => {}); await wait(200); }

// ---------------- 예시 연구 준비 (화면에 찍지 않음) ----------------
await p.goto(URL0); await p.evaluate(() => { localStorage.clear(); localStorage.setItem('ladder.aiNoteClosed', 'true'); }); await p.reload(); await wait(600);
await p.click('#splash').catch(() => {}); await wait(500);
await p.evaluate(() => { const s = document.querySelector('#scroller'); if (s) s.style.scrollBehavior = 'auto'; });
await click('[data-hp=mode-hyp]');
await p.fill('#hp-title', '능력주의 신념이 외집단 혐오에 미치는 영향: 사회비교경향성과 외로움의 순차적 매개효과');
await wait(600); await click('[data-mb-act=from-title]', 400);
await p.fill('#mb-newvar', '성별'); await p.press('#mb-newvar', 'Enter'); await wait(300);
const fillMb = async (key, v) => { await p.fill(`[data-mb="${key}"]`, v); await p.press(`[data-mb="${key}"]`, 'Tab'); await wait(200); };
await click('[data-mb-act=add-card]', 300); await click('.mb-picker [data-model=pred]', 300); await fillMb('c.1.x.0', '능력주의 신념'); await fillMb('c.1.y', '외집단 혐오');
await click('[data-mb-act=add-card]', 300); await click('.mb-picker [data-model=diff]', 300); await fillMb('c.2.x.0', '성별'); await fillMb('c.2.y', '외집단 혐오');
await p.selectOption('[data-mb="c.2.dir"]', '≠'); await wait(300);
await click('[data-hp=apply]', 600);
await click('[data-sv-act=example]', 200); await click('[data-sv-act=example]', 400);
const gi = await p.$('[data-dm][data-f=options]'); if (gi) { await gi.fill('남성, 여성'); await gi.dispatchEvent('change'); await wait(200); }
await nav('analysis'); await click('[data-an-act=demo]', 800);
const runNo = async no => {
  await p.click('.rtab[data-rt=method]').catch(() => {}); await wait(250);
  await p.click(`[data-hp-calc][data-hp-no="${no}"]`); await wait(350);
  const n0 = await p.$$eval('.result-card', e => e.length);
  await p.click('[data-an-act=run]'); await p.waitForFunction(n => document.querySelectorAll('.result-card').length > n, n0, { timeout: 120000 }); await wait(300);
};
await runNo(1); await runNo(2); await runNo(3);
await nav('interp'); await wait(400);
// 효과는 저절로 연결됨. 내가 읽은 결과를 적어 둠
const GIST = ['능력주의 신념이 높을수록 사회비교와 외로움을 차례로 거쳐 외집단 혐오로 이어지는 길을 확인하려 했다.', '능력주의 신념이 높을수록 외집단 혐오도 높았다.', '성별에 따른 외집단 혐오 차이는 통계적으로 유의하지 않았다.'];
for (let i = 0; i < 3; i++) await p.fill(`[data-in-h="${i}"][data-k=gist]`, GIST[i]).catch(() => {});
await wait(600);
await p.fill('[data-in-k=purpose]', '능력주의 신념이 높은 사람이 남과 비교하고 외로움을 느끼는 과정을 거쳐 외집단 혐오에 이르는지 알아본다.').catch(() => {});
await p.dispatchEvent('[data-in-k=purpose]', 'input').catch(() => {});
const STATE = await p.evaluate(() => { const o = {}; for (let i = 0; i < localStorage.length; i++) { const k = localStorage.key(i); o[k] = localStorage.getItem(k); } return o; });
const AN = JSON.parse(STATE['ladder.analyses']);
const effStat = re => { for (const a of AN) for (const e of a.effects) if (re.test(e.label)) return e.stat; return ''; };
const VER = await p.evaluate(() => { const L = JSON.parse(localStorage.getItem('ladder.intent')).hyps; return L.map(h => h.effect); });
const s1 = effStat(/^능력주의 신념 → 외집단 혐오$/) || effStat(/능력주의/), s2 = effStat(/간접효과: .*→.*→.*→/), s3 = effStat(/남성|−/);

// ---------------- 시작 ----------------
const LOGO = await p.evaluate(() => ({ l: document.querySelector('.hdr-logo')?.outerHTML || '', w: document.querySelector('svg.wm')?.outerHTML || '' }));
const logoHtml = `<div class="logo">${LOGO.l.replace(/class="[^"]*"/, 'class="tong"')}</div><div>${LOGO.w}</div>`;
await cardShot(`${logoHtml}<h1>AI 연결하기</h1><p>네 가지 방법 · 연결하는 법 · 방법마다 쓸 수 있는 기능</p>`, 6, '통에 AI를 연결하는 네 가지 방법과, 방법마다 쓸 수 있는 기능을 자세히 봅니다.');
await cardShot(`<h2>먼저 알아 둘 것</h2><div class="two">
  <div class="box"><b>AI 없이도 되는 것</b><ul><li>통계 기법 고르기</li><li>설문 만들기, 구글 폼</li><li>표본 수 계산, 모델 검토</li><li>응답 정리, 신뢰도·타당도</li><li>통계 계산과 <b>가설 판정</b></li><li>내가 읽은 결과 점검, 결과 문단 틀</li></ul></div>
  <div class="box"><b>AI가 돕는 것</b><ul><li>내가 읽은 결과를 결과·논의 문장으로 풀어 쓰기</li><li>허점 점검 (깐깐한 동료)</li><li>대화창 도우미</li><li>척도 사진·스캔 PDF 읽기</li><li>써 둔 가설 문장을 모형 카드로 바꾸기</li></ul></div></div>
  <p style="margin-top:20px"><b>판정은 코드가 정하고 AI는 바꿀 수 없습니다. 응답 원자료는 어떤 방법으로도 AI에 보내지 않습니다.</b></p>`, 12,
  'AI가 없어도 통계 계산, 가설 판정, 내가 읽은 결과 점검까지 모두 됩니다. AI는 글쓰기와 읽기를 돕습니다. 판정은 코드가 정하고, 응답 원자료는 AI에 보내지 않습니다.');
await cardShot(`<h2>네 가지 방법: 연결하는 법</h2><table>
  <tr><th></th><th>① Claude 바로 쓰기</th><th>② 다른 AI 쓰기<small>복사·붙여 넣기</small></th><th>③ Claude 데스크톱 앱<small>+ 연구 폴더</small></th><th>④ AI 쓰지 않기</th></tr>
  <tr><td>여는 곳</td><td>claude.ai 링크</td><td>통 웹 주소<br>(PC·휴대폰 어디서나)</td><td>통 웹 주소 (PC 크롬·엣지·웨일)<br>+ Claude 데스크톱 앱</td><td>어디서나</td></tr>
  <tr><td>필요한 것</td><td>Claude 계정 (무료도 됨)</td><td>ChatGPT·Gemini·Claude 등<br>아무 AI 채팅 (무료도 됨)</td><td>Claude <b>Pro 이상</b> 요금제</td><td>없음</td></tr>
  <tr><td>복사·붙여 넣기</td><td class="y">없음</td><td class="m">있음</td><td class="y">없음</td><td class="x">-</td></tr>
  <tr><td>연구 폴더 저장</td><td class="m">안 됨 (zip으로 백업)</td><td class="y">됨</td><td class="y">됨 (꼭 필요)</td><td class="y">됨</td></tr></table>`, 14,
  '① claude.ai 링크로 열기 ② 평소 쓰는 AI 채팅에 복사·붙여 넣기 ③ Claude 데스크톱 앱에 연구 폴더 맡기기 ④ AI 끄기. 필요한 계정과 연결 방법이 다릅니다.');
await cardShot(`<h2>네 가지 방법: 쓸 수 있는 기능</h2><table>
  <tr><th>기능</th><th>① Claude 바로 쓰기</th><th>② 다른 AI 쓰기</th><th>③ Claude 앱 + 폴더</th><th>④ AI 쓰지 않기</th></tr>
  <tr><td>결과·논의 풀어 쓰기</td><td class="y">버튼 두 번</td><td class="m">지시문 복사·붙여 넣기 2번</td><td class="y">"해석해 줘" → draft.md</td><td class="x">결과 문단 틀 + 직접 쓰기</td></tr>
  <tr><td>허점 점검</td><td class="y">버튼 한 번</td><td class="m">지시문 복사·붙여 넣기</td><td class="y">"허점 점검해줘" → review.md</td><td class="x">직접</td></tr>
  <tr><td>R로 다시 계산</td><td class="x">R 코드 받아 직접</td><td class="x">R 코드 받아 직접</td><td class="y">"R로 분석해줘" → 자동 대조</td><td class="x">R 코드 받아 직접</td></tr>
  <tr><td>대화창 도우미</td><td class="y">화면에서 바로 답</td><td class="m">사용 설명 찾기 + 질문 복사</td><td class="m">웹은 ②와 같음 · 앱에서 직접 묻기</td><td class="x">사용 설명 찾기만</td></tr>
  <tr><td>척도 사진·스캔 PDF</td><td class="y">화면에서 바로 읽기</td><td class="m">지시문 복사 → 사진과 함께 AI에</td><td class="m">②와 같음</td><td class="x">안 됨 (글자 PDF·글은 됨)</td></tr>
  <tr><td>써 둔 가설 붙여 넣기</td><td class="y">[AI로 읽기] → 카드</td><td class="x">규칙으로 읽고 모형 카드로</td><td class="x">규칙 + 모형 카드</td><td class="x">규칙 + 모형 카드</td></tr>
  <tr><td>초안 점검<small style="display:block;font-weight:400">숫자·판정·인과 표현</small></td><td class="y">자동</td><td class="y">됨</td><td class="y">됨</td><td class="y">됨 (AI 아님)</td></tr></table>`, 18,
  '초안 쓰기와 허점 점검은 ①②③ 모두 되고, 방법만 다릅니다. R로 다시 계산은 ③만 대신 해 줍니다. 초안 점검은 AI가 아니라 통이 하므로 어느 방법이든 됩니다.');

// ---------------- 방식 고르는 곳 ----------------
chapter = 'AI 방식 고르기';
await p.reload(); await wait(500); await p.click('#splash').catch(() => {}); await wait(400);
await p.evaluate(() => { const s = document.querySelector('#scroller'); if (s) s.style.scrollBehavior = 'auto'; });
await nav('interp'); await wait(300);
await shot('방법은 대화창 왼쪽 아래의 점에서 고릅니다. 점 옆 글자가 지금 상태입니다.', ['#ai-dot']);
await click('#ai-dot', 400);
await shot('AI 방식 고르기 창. 고른 방식은 대화창, 결과 해석, 허점 점검, 척도 사진 읽기에 모두 적용됩니다.', ['#ai-pop']);
await p.click('#ai-pop [data-ap=close]').catch(() => {}); await wait(200);
await cardShot(`<h2>점 색으로 지금 상태 알기</h2><div class="box" style="font-size:28px">
  <div><b style="color:#1f8a5b">● Claude 연결됨</b> — ① claude.ai 링크에서 열어 바로 쓰는 중</div>
  <div><b style="color:#e06f0b">● 다른 AI (복사)</b> — ② 지시문을 복사해 다른 AI에 붙여 넣는 방식</div>
  <div><b style="color:#8a90a3">● AI 없음</b> — 이 화면에서는 Claude를 바로 쓸 수 없음 (②나 ③을 쓰세요)</div>
  <div><b style="color:#c0392b">● AI 끔</b> — ④ AI를 쓰지 않음. 아무것도 밖으로 보내지 않음</div></div>`, 11, '점의 색과 글자로 지금 어떤 방식인지 알 수 있습니다.');

// ---------------- ① Claude 바로 쓰기 ----------------
await chapCard('①', 'Claude 바로 쓰기', 'claude.ai 링크로 열면 화면 안에서 바로 답합니다');
const pc = await ctxC.newPage(); pc.on('pageerror', e => errs.push(e.message));
await pc.goto(URL0); await pc.evaluate(s => { localStorage.clear(); for (const [k, v] of Object.entries(s)) localStorage.setItem(k, v); localStorage.setItem('ladder.aiMode', JSON.stringify('auto')); }, STATE); await pc.reload(); await pc.waitForTimeout(700); await pc.click('#splash').catch(() => {}); await pc.waitForTimeout(500);
await pc.evaluate(() => { const s = document.querySelector('#scroller'); if (s) s.style.scrollBehavior = 'auto'; });
const pMain = p; p = pc;
await nav('interp'); await wait(400);
const shotC = fno().replace('.png', '_raw.png'); await p.screenshot({ path: shotC });
const imgC = 'data:image/png;base64,' + fs.readFileSync(shotC).toString('base64');
await cardShot(`<div class="win"><div class="wt"><i></i><i></i><i></i><span class="addr">🔒 claude.ai/artifact/…</span></div><img src="${imgC}" style="width:100%;display:block"></div>`, 10,
  '1. 사용 설명의 [Claude 바로 쓰기 링크]를 눌러 claude.ai에서 엽니다. 2. Claude 계정으로 로그인합니다. 무료 계정도 됩니다.', DRAWN);
await shot('claude.ai에서 열면 점이 초록색 "Claude 연결됨"으로 바뀝니다. 따로 설치하거나 설정할 것은 없습니다.', ['#ai-dot']);
await into('.mode-tabs', 'start');
await shot('결과 해석: [① AI가 내 의도 확인하기] → AI가 연구 목적과 가설을 다시 말해 보고, 효과 연결이 맞는지 확인하고, 빠진 정보를 묻습니다.', ['[data-in-act=check]']);
await shot('[② 내 해석으로 결과·논의 풀어 쓰기] → 2번에 적은 "내가 읽은 결과"를 뼈대로, 코드의 판정과 숫자 그대로 논문 문체로 풀어 씁니다. 내 해석에 없는 주장은 [연구자 보완] 자리로 남기고, 다 쓰면 통이 다시 점검합니다.', ['[data-in-act=write]']);
await nav('review'); await wait(400); await into('#rv-card', 'start');
await shot('허점 점검: [허점 점검 받기] 한 번이면, AI가 깐깐한 동료처럼 심사위원이 물을 만한 곳을 짚어 줍니다.', ['[data-in-act=rv-run]']);
await p.click('#cmp-text'); await p.fill('#cmp-text', '매개효과가 유의하지 않으면 논의에 어떻게 써야 해?'); await wait(200);
await shot('대화창 도우미: 궁금한 것을 적고 보내면, 지금 화면과 내 연구 내용을 보고 화면 안에서 바로 답합니다.', ['#composer']);
await nav('ladder'); await wait(300);
await p.evaluate(() => { const d = document.querySelector('.mb-paste'); if (d) d.open = true; document.querySelector('[data-mb-act=paste-ai]')?.scrollIntoView({ block: 'center' }); }); await wait(400);
await shot('이미 써 둔 가설·연구문제는 [붙여 넣기]에 넣고 [AI로 읽어서 카드로 바꾸기]를 누르면 모형 카드로 바뀝니다. 척도 사진·스캔 PDF도 화면에서 바로 읽습니다.', ['[data-mb-act=paste-ai]']);
await cardShot(`<h2>① Claude 바로 쓰기 — 알아 둘 것</h2><div class="box"><ul>
  <li>내 Claude 계정의 사용량을 씁니다. 사용량을 넘으면 잠시 기다려야 합니다.</li>
  <li>claude.ai 안에서는 <b>연구 폴더 연결이 안 됩니다.</b> [연구 폴더 zip 받기]로 백업하세요.</li>
  <li>다른 사람이 이 링크를 쓰려면, 링크를 만든 사람이 claude.ai에서 <b>공유를 켜야</b> 합니다.</li></ul></div>`, 12,
  '① 방식은 가장 편하지만, claude.ai 안에서는 연구 폴더 연결이 안 되니 zip으로 백업하세요.');
p = pMain;

// ---------------- ② 다른 AI 쓰기 ----------------
await chapCard('②', '다른 AI 쓰기 (복사·붙여 넣기)', 'ChatGPT·Gemini·Claude 등 평소 쓰는 AI 채팅으로');
await setMode('copy');
await shot('점을 눌러 [다른 AI 쓰기]를 고르면 점이 주황색 "다른 AI (복사)"로 바뀝니다.', ['#ai-dot']);
await nav('interp'); await wait(300);
await p.click('[data-in-mode=chat]').catch(() => {}); await wait(300);
await into('.chat-steps', 'start');
await into('.hyps', 'start');
await shot('먼저 결과 해석 2번에서 가설마다 [내가 읽은 결과]를 적어 둡니다. 지시문에 함께 들어가, AI가 내 해석을 뼈대로 씁니다.', ['.hyps .hyp:first-child .gist']);
await into('.chat-steps', 'start');
await shot('결과 해석 → [다른 AI (복사)]: 네 단계로 진행합니다. 지시문 복사 → AI에 붙여 넣기 → AI의 답 붙여 넣기 → 초안 점검.', ['.chat-steps']);
await p.click('[data-in-act=copy-prompt][data-which=check]'); await wait(600);
const sheet = await p.$('#sheet:not([hidden])');
if (sheet) await shot('[① 의도 확인 지시문 복사]: 연구 의도, 가설, 코드의 판정, 결과표가 담긴 지시문이 복사됩니다. 응답 원자료는 들어 있지 않습니다.', ['#sheet-text'], { spot: false });
else await shot('[① 의도 확인 지시문 복사]: 연구 의도, 가설, 코드의 판정, 결과표가 담긴 지시문이 복사됩니다. 응답 원자료는 들어 있지 않습니다.', ['[data-in-act=copy-prompt][data-which=check]']);
await p.click('#sheet-close').catch(() => {}); await wait(300);
await cardShot(`<div class="win"><div class="wt"><i></i><i></i><i></i><span class="addr">AI 채팅 (ChatGPT · Gemini · Claude 등)</span></div><div class="wb">
  <div class="msg me">(Ctrl+V로 붙여 넣기)\n너는 사회과학 석사논문의 통계 해석을 돕는 조교다. …\n[연구 자료] 제목: 능력주의 신념이 외집단 혐오에 미치는 영향 …\n가설 1 … 판정: 지지 / 가설 2 … 판정: 기각 …</div>
  <div class="msg ai">연구 목적: 능력주의 신념이 사회비교경향성과 외로움을 거쳐 외집단 혐오로 이어지는지 확인하는 연구입니다.\n가설 1: 연결한 효과가 가설에 알맞습니다.\n가설 2: 간접효과에 알맞게 연결되었습니다.\n질문 1. '외집단'은 구체적으로 어떤 집단을 말하나요?\n질문 2. 참여자는 어디에서 모집했나요?</div></div></div>`, 11,
  '복사한 지시문을 평소 쓰는 AI 채팅에 붙여 넣습니다. AI가 연구를 이해한 내용과 질문을 답합니다. (AI의 답은 예시입니다)', DRAWN);
await p.fill('[data-in-chat=aiText]', '연구 목적: 능력주의 신념이 사회비교경향성과 외로움을 거쳐 외집단 혐오로 이어지는지 확인하는 연구입니다.\n가설 1·2: 연결한 효과가 알맞습니다.\n질문 1. 외집단은 어떤 집단인가요? 질문 2. 참여자 모집 경로는?');
await p.fill('[data-in-chat=myAnswers]', '1. 이주민과 성소수자 2. 대학 커뮤니티에서 모집함'); await wait(200);
await into('[data-in-chat=aiText]', 'center');
await shot('AI의 답을 [AI의 답] 칸에, AI가 물은 것에 대한 내 답을 그 아래 칸에 붙여 넣습니다.', ['.chat-step:nth-child(2)']);
await into('[data-in-act=copy-prompt][data-which=write]', 'center');
await shot('[② 초안 쓰기 지시문 복사] → 같은 AI 채팅에 붙여 넣습니다. 앞의 답까지 담겨 있어 AI가 초안을 씁니다.', ['.chat-step:nth-child(3)']);
const vv = await p.evaluate(() => [...document.querySelectorAll('#rp-results-top .rv-verdict b')].map(b => b.textContent));
const vs = i => (vv[i] === '지지' ? '지지되었다' : '기각되었다');
const draft = `[결과]
가설 1은 ${vs(0)}. 능력주의 신념은 외집단 혐오를 정적으로 예측하였다(${s1.replace(/B = (-?[\d.]+)/, (m, x) => 'B = ' + (Math.abs(+x) + 0.91).toFixed(2))}).
가설 2는 ${vs(1)}. 간접효과의 부트스트랩 95% 신뢰구간은 ${s2}였다.
가설 3은 ${vs(2)}. 성별에 따른 외집단 혐오의 차이를 검정하였다(${s3}).
[논의]
능력주의 신념이 높기 때문에 외집단 혐오가 높아진다.
[연구의 한계]
- 한 시점 설문 자료라 변인 간 시간 순서를 확인할 수 없다.
[확인할 것]
- [선행연구 비교: 능력주의와 편견 연구]`;
await cardShot(`<div class="win"><div class="wt"><i></i><i></i><i></i><span class="addr">AI 채팅 (ChatGPT · Gemini · Claude 등)</span></div><div class="wb">
  <div class="msg me">(② 초안 쓰기 지시문 붙여 넣기)</div><div class="msg ai">${draft.replace(/</g, '&lt;')}</div></div></div>`, 10,
  'AI가 [결과] [논의] [연구의 한계] [확인할 것] 초안을 씁니다. 이 초안을 복사합니다. (예시: 일부러 틀린 숫자와 인과 표현을 넣었습니다)', DRAWN);
await p.fill('#in-draft', draft); await p.dispatchEvent('#in-draft', 'input'); await wait(200);
await into('.paste-box', 'center');
await shot('초안을 [초안 점검] 칸에 붙여 넣고 [초안 점검하기]를 누릅니다.', ['.paste-box']);
await click('[data-in-act=check-draft]', 700);
const iss = await p.$('.rissue, .issues, .draft-issues, .in-issues');
await p.click('.rtab[data-rt=results]').catch(() => {}); await wait(400);
await shot('통이 결과표에 없는 숫자, 판정과 다른 문장, "때문에" 같은 인과 표현을 찾아 알려 줍니다. AI가 실수해도 여기서 걸러집니다.', ['#rp-results-top .rdraft, #rp-results-top .card:nth-child(2)']);
await nav('review'); await wait(300); await into('#rv-card', 'start');
await shot('허점 점검도 같은 방식입니다: 지시문 복사 → AI에 붙여 넣기 → AI의 답을 붙여 넣으면 목록으로 정리됩니다.', ['#rv-card']);
await nav('survey'); await wait(300);
await click('[data-sv-act=scale-src]', 400);
await p.setInputFiles('#si-file', SCAN).catch(() => {}); await wait(1500);
await into('#sv-import', 'start');
await shot('척도 사진·스캔 PDF: 지시문을 복사해 사진과 함께 AI 채팅에 올리고, AI가 정리한 글을 [글 붙여 넣기]에 넣으면 문항이 채워집니다.', ['#sv-import']);
await p.click('[data-si=close]').catch(() => {}); await wait(200);
await nav('interp'); await wait(300);
await p.fill('#cmp-text', '매개효과가 유의하지 않으면 논의에 어떻게 써야 해?'); await p.press('#cmp-text', 'Enter'); await wait(700);
await shot('대화창: 사용 설명에서 관련된 곳을 찾아 주고, [AI에게 물을 지시문 복사]로 질문을 내 연구 정보와 함께 다른 AI에 물을 수 있습니다.', ['#thread .msg.ai:last-of-type']);

// ---------------- ③ Claude 데스크톱 앱 + 연구 폴더 ----------------
await chapCard('③', 'Claude 데스크톱 앱 + 연구 폴더', '복사·붙여 넣기 없이, Claude가 폴더의 파일을 직접 읽고 씁니다');
await cardShot(`<h2>③ 준비물</h2><div class="box"><ul>
  <li>PC의 <b>크롬·엣지·웨일</b>에서 연 통 (claude.ai 링크 말고 웹 주소)</li>
  <li>Claude <b>Pro 이상</b> 요금제</li><li><b>Claude 데스크톱 앱</b> (claude.ai/download) — 터미널이 필요 없습니다</li>
  <li>R 분석까지 맡기려면 PC에 <b>R</b> 설치 (선택)</li></ul></div>`, 11, '③ 준비물: PC 크롬·엣지·웨일, Claude Pro 이상 요금제, Claude 데스크톱 앱. R 분석까지 맡기려면 R도 설치합니다.');
chapter = '③ STEP 1 연구 폴더 연결';
await click('#fold-chip', 400);
await shot('통 위 띠의 [📁 연구 폴더] → [연구 폴더 연결]을 누릅니다.', ['#fold-pop']);
await cardShot(`<div class="win" style="width:900px"><div class="wt"><i></i><i></i><i></i><span>폴더 선택</span></div><div class="wb">
  <div style="color:#555d72">문서 ›</div><div style="margin:10px 0;padding:10px 14px;border-radius:8px;background:#eef3ff">📁 <b>내논문</b> &nbsp;<span style="color:#8a91a5">(새 폴더를 만들어 고르세요)</span></div>
  <div style="text-align:right;margin-top:16px"><span style="background:#2563eb;color:#fff;border-radius:8px;padding:8px 18px">폴더 선택</span></div>
  <div style="margin-top:26px;padding:14px 16px;border:1px solid #e2e5ee;border-radius:10px">이 사이트가 '내논문'의 파일을 보고 수정하도록 허용할까요?<div style="text-align:right;margin-top:10px"><span style="background:#2563eb;color:#fff;border-radius:8px;padding:6px 16px">파일 수정 허용</span></div></div></div></div>`, 10,
  '폴더 고르는 창에서 새 폴더(예: 내논문)를 만들어 고르고, 브라우저가 묻는 파일 수정을 허용합니다.', DRAWN);
await p.click('[data-fd=connect]').catch(() => {}); await wait(1500);
const pend = await p.$('[data-fd=use-screen]'); if (pend) { await pend.click(); await wait(200); await pend.click().catch(() => {}); await wait(800); }
await p.evaluate(() => { const pop = document.querySelector('#fold-pop'); if (pop) pop.hidden = false; }); await wait(300);
await shot('연결되면 폴더 이름과 저장 시각이 나옵니다. 이제 작업이 이 폴더에 파일로 저장됩니다.', ['#fold-pop']);
await p.click('#fold-chip').catch(() => {}); await wait(1600);
const files = await p.evaluate(async () => { const out = []; const walk = async (d, pre) => { for await (const [n, h] of d.entries()) { if (h.kind === 'directory') await walk(h, pre + n + '/'); else out.push(pre + n); } }; const root = await navigator.storage.getDirectory(); await walk(await root.getDirectoryHandle('내논문'), ''); return out.sort(); });
const tree = files.map(f => '  ' + f).join('\n');
await cardShot(`<div class="win" style="width:1000px"><div class="wt"><i></i><i></i><i></i><span>내논문</span></div><div class="wb tree">${tree.replace(/</g, '&lt;')}</div></div>`, 10,
  '폴더에 저절로 생긴 파일들입니다. CLAUDE.md는 Claude가 지킬 규칙, 나머지는 설문·자료·분석 계획서·R 코드·해석 자료입니다.', '실제로 저장된 파일 목록');
chapter = '③ STEP 2 앱에서 폴더 열기';
await cardShot(`<div class="win"><div class="wt"><i></i><i></i><i></i><span>Claude 데스크톱 앱</span></div><div class="wb">
  <div class="tabs"><span>Chat</span><span class="on">Code</span></div>
  <div style="margin-top:22px;display:flex;gap:16px;align-items:center"><span style="border:2px solid #f5841f;border-radius:8px;padding:8px 18px">Local</span><span>→</span><span style="border:2px solid #f5841f;border-radius:8px;padding:8px 18px">Select folder</span><span>→</span><span style="background:#eef3ff;border-radius:8px;padding:8px 18px">📁 내논문</span></div>
  <p style="font-size:18px;margin-top:22px">메뉴 이름이 다르면 통 사용 설명의 [Claude Code로 맡기기] → 공식 안내를 보세요.</p></div></div>`, 11,
  'Claude 데스크톱 앱을 설치하고 로그인한 뒤, 위쪽 Code 탭 → Local → Select folder → 연구 폴더(내논문)를 고릅니다.', DRAWN);
chapter = '③ STEP 3 말로 시키기';
await cardShot(`<div class="win"><div class="wt"><i></i><i></i><i></i><span>Claude 데스크톱 앱 · 내논문</span></div><div class="wb">
  <div class="msg me">해석해 줘</div><div class="msg ai">통 해석 모드입니다. interpretation.json을 읽었습니다. 먼저 연구 의도를 확인할게요. …</div>
  <div class="msg me">R로 분석해줘</div><div class="msg ai">analysis.R을 실행해 R_output.txt와 R_results.csv를 만들었습니다.</div>
  <div class="msg me">허점 점검해줘</div><div class="msg ai">review.md에 짚어 볼 곳을 정리했습니다.</div></div></div>`, 13,
  '"해석해 줘", "R로 분석해줘", "허점 점검해줘"라고 말하면 됩니다. 첫 답에 "통 해석 모드입니다."가 나오면 제대로 연결된 것입니다.', DRAWN);
chapter = '③ STEP 4 결과 보기';
await cardShot(`<div class="win" style="width:1000px"><div class="wt"><i></i><i></i><i></i><span>내논문</span></div><div class="wb tree">${(tree + '\n  <span class="new">3_analysis/R_output.txt      ← Claude가 만든 파일</span>\n  <span class="new">4_interpretation/draft.md     ← Claude가 만든 파일</span>\n  <span class="new">4_interpretation/review.md    ← Claude가 만든 파일</span>')}</div></div>`, 11,
  'Claude가 만든 초안(draft.md), R 결과, 허점 점검(review.md)은 폴더에 저장되고, 통 창을 다시 누르면 저절로 읽어 와 대조·점검합니다.', DRAWN);
await nav('interp'); await wait(300);
await p.click('[data-in-mode=code]').catch(() => {}); await wait(300); await into('.chat-steps', 'start');
await shot('통의 결과 해석 → [Claude Code] 탭에도 같은 순서가 적혀 있습니다. 폴더 없이 파일 두 개만 받아 쓰는 방법도 있습니다.', ['.chat-steps']);

// ---------------- ④ AI 쓰지 않기 ----------------
await chapCard('④', 'AI 쓰지 않기', '기관 규정상 외부 AI를 쓸 수 없을 때');
await setMode('off');
await shot('[AI 쓰지 않기]를 고르면 점이 "AI 끔"이 되고, 아무것도 밖으로 보내지 않습니다.', ['#ai-dot']);
await into('#in-main .notice', 'center');
await shot('기법 고르기부터 계산·가설 판정까지는 그대로 쓰고, 해석 글은 직접 씁니다. 직접 쓴 글도 [초안 점검]으로 숫자와 판정을 점검할 수 있습니다.', ['#in-main .notice']);
await setMode('copy');

// ---------------- 고르기 ----------------
chapter = '';
await cardShot(`<h2>나에게 맞는 방법</h2><div class="box" style="font-size:27px;line-height:1.9">
  <div>🤔 처음이고, 평소 ChatGPT·Gemini를 쓴다 → <b>② 다른 AI 쓰기</b></div>
  <div>🙂 Claude 계정이 있고, 복사·붙여 넣기가 번거롭다 → <b>① Claude 바로 쓰기</b></div>
  <div>🚀 Claude Pro가 있고, R 분석과 파일 정리까지 맡기고 싶다 → <b>③ Claude 앱 + 연구 폴더</b></div>
  <div>🔒 학교·기관 규정상 외부 AI를 쓸 수 없다 → <b>④ AI 쓰지 않기</b></div></div>
  <p style="margin-top:18px">중간에 언제든 바꿀 수 있습니다. 어느 방법이든 판정은 코드가 정하고, 응답 원자료는 보내지 않습니다.</p>`, 14,
  '처음이면 평소 쓰는 AI로 ②부터 시작하세요. 방법은 언제든 바꿀 수 있습니다.');
await cardShot(`${logoHtml}<h1>사용 설명 → [AI 방식] · [Claude Code로 맡기기]</h1><p>더 자세한 내용은 통 오른쪽 위 [사용 설명]에 있습니다</p><p style="margin-top:20px;font-size:26px;font-weight:800;color:#2563eb">aninsong77-dotcom.github.io/tong</p>`, 7, '더 자세한 내용은 통의 [사용 설명]에 있습니다.');

console.log('scenes', scenes.length, 'total', scenes.reduce((s, x) => s + x.dur, 0).toFixed(1), 's', 'errs', errs, 'files', files.length, 'sheet', !!sheet, 'issue', !!iss);
fs.writeFileSync(OUT + 'ai_connect_scenes.json', JSON.stringify(scenes, null, 1));
await b.close();
