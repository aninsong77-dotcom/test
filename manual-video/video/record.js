// 곤글박이 사용 설명서 영상 녹화 스크립트
const fs = require("fs"), path = require("path");
const { chromium } = require("playwright-core");
const { serve, setup } = require("./common");
const OUT = process.argv[2] || path.join(__dirname, "raw.mjpeg");
const ONLY = process.env.ONLY; // 디버그용: 특정 챕터까지만

(async () => {
  const server = await serve();
  const browser = await chromium.launch({ executablePath: "/opt/pw-browsers/chromium-1194/chrome-linux/chrome", args: ["--force-device-scale-factor=1.5", "--window-size=1280,807"] });
  const ctx = await setup(browser);
  await ctx.addInitScript({ path: path.join(__dirname, "director.js") });
  const page = await ctx.newPage();
  page.on("pageerror", e => console.log("pageerror", e.message));

  // ── 화면 캡처: CDP 스크린캐스트 → 30fps 고정 프레임으로 기록 ──
  const cdp = await ctx.newCDPSession(page);
  let latest = null, written = 0, t0 = 0;
  const out = fs.createWriteStream(OUT);
  cdp.on("Page.screencastFrame", f => { latest = Buffer.from(f.data, "base64"); cdp.send("Page.screencastFrameAck", { sessionId: f.sessionId }).catch(() => {}); });
  await cdp.send("Page.startScreencast", { format: "jpeg", quality: 90, maxWidth: 1920, maxHeight: 1200, everyNthFrame: 1 });
  const pump = setInterval(() => {
    if (!latest) return;
    if (!t0) t0 = Date.now();
    const want = Math.floor((Date.now() - t0) * 30 / 1000);
    while (written <= want) { out.write(latest); written++; }
  }, 8);

  const wait = ms => page.waitForTimeout(ms);
  const D = (fn, ...a) => page.evaluate(([fn, a]) => window.__dir[fn](...a), [fn, a]);
  // ── 내레이션 (오프라인 TTS, 캐시) ──
  const { execFileSync } = require("child_process"); const crypto = require("crypto");
  const TTSDIR = path.join(__dirname, "tts_cache"); fs.mkdirSync(TTSDIR, { recursive: true });
  const narr = []; let busyUntil = 0;
  const REPL = [["(제노그램)", ", 제노그램"], ["(//)", ""], ["(X)", ", 엑스 표시"], ["(이중 도형)", ", 이중 도형"], ["(V자 + 가로선)", ""],
    ["(예: 결혼 → 별거)", "예를 들어 결혼선을 별거선으로 바꿀 수 있어요."], ["(Ctrl+Z)", ", 컨트롤 제트,"], ["퍼센트(%)", "퍼센트"],
    ["Shift+클릭", "시프트 클릭"], ["Enter", "엔터"], ["Esc", "이에스씨"], ["Alt", "알트"], ["Delete", "딜리트"], ["PNG", "피엔지"], ["SVG", "에스브이지"],
    ["JSON", "제이슨"], ["T 텍스트", "티, 텍스트"], ["Aa 글씨", "에이에이 글씨"], ["X", "엑스"], ["‹ ›", ""], ["컬러/흑백", "컬러, 흑백"], ["'", ""], ["→", " "],
    [" □", ""], [" ○", ""], [" ◇", ""], ["//", ""]];
  const norm = h => { let t = h.replace(/<br\s*\/?>/g, " ").replace(/<[^>]+>/g, ""); for (const [a, b] of REPL) t = t.split(a).join(b); return t.replace(/\s*·\s*/g, ", ").replace(/\s+/g, " ").trim(); };
  const free = async () => { while (Date.now() < busyUntil) await wait(80); };
  const narrate = async html => {
    const text = norm(html); if (!text || !t0) return;
    const f = path.join(TTSDIR, crypto.createHash("md5").update(text).digest("hex").slice(0, 12) + ".wav");
    if (!fs.existsSync(f + ".dur")) execFileSync("python3", [path.join(__dirname, "../tts/gen.py"), text, f], { stdio: ["ignore", "ignore", "inherit"] });
    const dur = +fs.readFileSync(f + ".dur", "utf8");
    narr.push({ at: (Date.now() - t0) / 1000 + 0.25, f, text }); busyUntil = Date.now() + 250 + dur * 1000 + 450;
  };
  const say = async (num, lab, txt, keys) => { await free(); await D("say", num, lab, txt, keys || []); await narrate(txt); };
  let mouse = { x: 720, y: 460 };
  const move = async (x, y, ms = 650) => {
    const steps = Math.max(8, Math.round(ms / 16));
    const sx = mouse.x, sy = mouse.y;
    for (let i = 1; i <= steps; i++) {
      const t = i / steps, e = t < .5 ? 4 * t * t * t : 1 - Math.pow(-2 * t + 2, 3) / 2;
      await page.mouse.move(sx + (x - sx) * e, sy + (y - sy) * e);
    }
    mouse = { x, y };
  };
  const click = async (x, y, opt = {}) => { await move(x, y, opt.ms); await wait(120); await page.mouse.click(x, y, { button: opt.button || "left" }); await wait(opt.after ?? 350); };
  const rclick = async (x, y) => { await move(x, y); await D("tag", "마우스 우클릭", 1500); await wait(200); await page.mouse.click(x, y, { button: "right" }); await wait(450); };
  const dbl = async (x, y) => { await move(x, y); await D("tag", "더블클릭", 1500); await wait(200); await page.mouse.dblclick(x, y); await wait(400); };
  const drag = async (x1, y1, x2, y2, ms = 900) => { await move(x1, y1); await wait(120); await page.mouse.down(); await move(x2, y2, ms); await page.mouse.up(); await wait(250); };
  const btn = sel => page.locator(sel).first();
  const B = t => btn(`button:has(span:text-is("${t}"))`);
  const center = async loc => { const b = await loc.boundingBox(); return { x: b.x + b.width / 2, y: b.y + b.height / 2, b }; };
  const clickEl = async (loc, opt) => { const c = await center(loc); await click(c.x, c.y, opt); };
  const ringEl = async (loc, opt) => { const b = await loc.boundingBox(); await D("ring", { x: b.x, y: b.y, w: b.width, h: b.height }, opt || {}); };
  const cam = async (sel, opt) => { await D("cam", sel, opt || {}); await wait(1100); };

  // 캔버스 좌표 ↔ 화면 좌표 (줌/패닝 반영)
  const view = () => page.evaluate(() => {
    const svg = document.querySelector("[data-tour=geo-canvas] > svg"); const r = svg.getBoundingClientRect();
    const g = svg.querySelector(":scope > g"); const m = /translate\(([-\d.e+]+),([-\d.e+]+)\) scale\(([-\d.e+]+)\)/.exec(g.getAttribute("transform"));
    return { ox: r.x, oy: r.y, px: +m[1], py: +m[2], z: +m[3] };
  });
  const scr = async (cx, cy) => { const v = await view(); return { x: v.ox + v.px + cx * v.z, y: v.oy + v.py + cy * v.z }; };
  const nodePos = i => page.evaluate(i => {
    const gs = [...document.querySelectorAll('[data-tour=geo-canvas] svg g[style*="grab"]')];
    const m = /translate\(([-\d.]+),([-\d.]+)\)/.exec(gs[i].getAttribute("transform")); return { x: +m[1], y: +m[2] };
  }, i);
  const NS = 56;
  const nodeCenter = async i => { const p = await nodePos(i); return scr(p.x + NS / 2, p.y + NS / 2); };
  const ageAt = i => page.evaluate(i => {
    const gs = [...document.querySelectorAll('[data-tour=geo-canvas] svg g[style*="grab"]')];
    const t = [...gs[i].querySelectorAll("text")].find(t => t.style.cursor === "text"); const r = t.getBoundingClientRect();
    return { x: r.x + r.width / 2, y: r.y + r.height / 2 };
  }, i);
  const placeNode = async (i, cx, cy) => { const a = await nodeCenter(i); const b = await scr(cx + NS / 2, cy + NS / 2); await drag(a.x, a.y, b.x, b.y, 800); };
  const EMPTY = async () => scr(1000, 30);
  const commit = async () => { const e = await EMPTY(); await click(e.x, e.y, { ms: 500 }); };
  const nameNode = async (i, name, age, fast) => {
    const c = await nodeCenter(i); await dbl(c.x, c.y);
    await page.keyboard.type(name, { delay: fast ? 45 : 110 }); await wait(fast ? 200 : 450); await commit();
    if (age) { const a = await ageAt(i); await click(a.x, a.y, { after: 250 }); await page.keyboard.type(age, { delay: fast ? 60 : 130 }); await wait(200); await page.keyboard.press("Enter"); await wait(fast ? 200 : 400); }
  };
  const addNode = async (label, i, cx, cy) => { await clickEl(B(label), { after: 500 }); await placeNode(i, cx, cy); };
  const pick = async (...ids) => {
    await page.keyboard.press("Escape");
    const ps = []; for (const i of ids) ps.push(await nodePos(i));
    const x1 = Math.min(...ps.map(p => p.x)) - 40, y1 = Math.min(...ps.map(p => p.y)) - 35;
    const x2 = Math.max(...ps.map(p => p.x)) + NS + 40, y2 = Math.max(...ps.map(p => p.y)) + NS + 38;
    const a = await scr(x1, y1), b = await scr(x2, y2);
    await move(a.x, a.y); await D("tag", "드래그로 선택", 1600); await wait(150);
    await page.mouse.down(); await move(b.x, b.y, 900); await wait(150); await page.mouse.up(); await wait(600);
  };
  const closeup = async (num, lab, txt, ms = 3400) => { await free(); await D("ringOff"); await cam({ x: 70, y: 120, w: 800, h: 340 }, { max: 1.45, margin: 40, bottom: 130 }); await say(num, lab, txt); await wait(ms); await cam(null); };
  const connect = async (a, b) => { const p = await nodeCenter(a); await rclick(p.x, p.y); await wait(500); const q = await nodeCenter(b); await click(q.x, q.y, { ms: 800, after: 700 }); };

  // 인물 배치 (캔버스 좌표, 도형 좌상단)
  const L = { dad: [330, 60], mom: [650, 60], ex: [130, 60], tw1: [380, 230], tw2: [490, 230], dau: [600, 230], preg: [720, 230] };
  const I = { dad: 0, mom: 1, ex: 2, tw1: 3, tw2: 4, dau: 5, preg: 6 };
  const marriagePt = async () => scr(600, 60 + NS / 2);

  // ═════════ 시작 ═════════
  await page.goto(`http://localhost:${server.address().port}/`);
  await page.waitForFunction(() => window.__dir);
  await D("card", "", "곤글박이\n가계도 스케치북", "사용 설명서", { logo: true, foot: "aninsong77-dotcom.github.io/gonglbaki" });
  await wait(700); await narrate("곤글박이 가계도 스케치북, 사용 설명서입니다.");
  await wait(2700); await free();
  await D("cardOff"); // 뒤에서 돌고 있던 스플래시(카드 회전)가 드러남
  await page.waitForFunction(() => document.querySelector("[data-tour=geo-canvas]"), null, { timeout: 15000 });
  await wait(900);
  await page.mouse.move(mouse.x, mouse.y);

  // ── 00 소개 ──
  await say("00", "소개", "곤글박이는 가족 구조와 관계를 그리는<br>무료 가계도(제노그램) 편집기입니다.");
  await wait(3600);
  await say("00", "소개", "설치 없이 웹 브라우저에서 바로 사용할 수 있어요.<br>주요 기능을 차례로 살펴볼게요.");
  await wait(3400);
  await D("hush");

  // ── 01 화면 구성 ──
  const KN = ["", "일", "이", "삼", "사", "오", "육", "칠", "팔", "구", "십"];
  const chapter = async (n, title, sub) => { await free(); await D("hush"); await D("ringOff"); await D("card", `STEP ${n}`, title, sub); await wait(500); await narrate(`${KN[+n]} 단계. ${title}`); await wait(2100); await free(); await D("cardOff"); await wait(700); };
  await chapter("01", "화면 한눈에 보기", "도구 모음, 캔버스, 추가 패널");
  const tour = async (sel, lab, txt, camOpt, keys) => {
    await free(); if (camOpt !== false) await cam(sel, camOpt); else await wait(200);
    await D("ring", sel, { dim: true }); await say("01", lab, txt, keys); await wait(3300);
  };
  await tour("[data-tour=geo-nodes]", "인물 도형", "남성 □, 여성 ○, 논바이너리 ◇ 등<br>인물 도형을 추가하는 버튼이에요.", { max: 2.4 });
  await tour("[data-tour=geo-lines]", "관계선", "가족관계 · 감정관계 · 학대/갈등 선을<br>종류별로 골라 그릴 수 있어요.", { max: 1.6 });
  await tour("[data-tour=geo-child-types]", "자녀 · 쌍둥이 · 상태 표시", "자녀선 종류, 쌍둥이, 임신·유산 등 특수 자녀,<br>약물·정신·신체 문제 표시 도구가 모여 있어요.", false);
  await D("ring", { x: 0, y: 0, w: 0, h: 0 });
  await cam(null);
  await D("ring", "[data-tour=geo-canvas]", { dim: true, pad: -4, radius: 6 }); await say("01", "캔버스", "가운데 넓은 영역이 가계도를 그리는 캔버스예요."); await wait(3000);
  await D("ring", "[data-tour=geo-side-panel]", { dim: true, pad: -4, radius: 6 }); await say("01", "추가 패널", "오른쪽 패널에는 무관심 · 정서적학대 · 방임 · 통제<br>관계선이 더 있어요. ‹ › 버튼으로 접고 펼 수 있어요."); await wait(3800);
  await D("ringOff");

  // ── 02 인물 추가 ──
  await chapter("02", "인물 추가하기", "도형을 추가하고 이름과 나이를 적어요");
  await say("02", "인물 추가", "도구 모음의 <b>남성</b> 버튼을 누르면<br>캔버스에 도형이 나타나요.");
  await ringEl(B("남성")); await wait(900); await D("ringOff");
  await addNode("남성", I.dad, ...L.dad);
  await say("02", "위치 옮기기", "도형은 드래그해서 원하는 곳으로 옮길 수 있어요.<br>다른 도형과 줄이 맞으면 자동으로 정렬돼요.", [["드래그", "이동"]]);
  await addNode("여성", I.mom, ...L.mom);
  await wait(600);
  await say("02", "이름 입력", "도형을 <b>더블클릭</b>하면 이름을 적을 수 있어요.", [["더블클릭", "이름 편집"]]);
  await wait(800);
  await nameNode(I.dad, "아버지", null);
  await say("02", "나이 입력", "도형 안의 <b>'나이'</b> 글자를 클릭하면 나이를 적어요.<br>Enter 키로 입력을 마칩니다.", [["나이 클릭", "나이 편집"], ["Enter", "완료"]]);
  await wait(500);
  { const a = await ageAt(I.dad); await click(a.x, a.y, { after: 300 }); await page.keyboard.type("52", { delay: 160 }); await wait(400); await page.keyboard.press("Enter"); await wait(900); }
  await say("02", "인물 추가", "같은 방법으로 어머니와 전 배우자를 추가할게요.");
  await nameNode(I.mom, "어머니", "50", true);
  await addNode("여성", I.ex, ...L.ex);
  await nameNode(I.ex, "전 배우자", "53", true);
  await wait(800);

  // ── 03 관계선 ──
  await chapter("03", "관계선 연결하기", "선 종류를 고르고, 두 인물을 이어요");
  await cam("[data-tour=geo-lines]", { max: 1.7 });
  await ringEl(B("결혼")); await say("03", "① 선 종류 선택", "먼저 위쪽에서 그릴 선의 종류를 고릅니다.<br>여기서는 <b>결혼</b>을 선택할게요.");
  await wait(1500); await clickEl(B("결혼"), { after: 1200 }); await D("ringOff"); await cam(null);
  await say("03", "② 연결 시작", "시작할 인물을 <b>마우스 오른쪽 버튼</b>으로 클릭해요.", [["우클릭", "연결 시작"]]);
  await wait(900);
  { const p = await nodeCenter(I.dad); await rclick(p.x, p.y); }
  await D("ring", "[data-tour=geo-canvas] > div.bg-amber-500", { pad: 4, radius: 999 });
  await say("03", "③ 대상 선택", "안내 문구가 뜨면, 연결할 인물을 클릭하세요.<br>취소하려면 Esc 키를 눌러요.", [["클릭", "연결"], ["Esc", "취소"]]);
  await wait(2200); await D("ringOff");
  { const q = await nodeCenter(I.mom); await click(q.x, q.y, { ms: 900, after: 1400 }); }
  await say("03", "이혼선", "이번에는 <b>이혼</b>을 골라 아버지와 전 배우자를 이어볼게요.");
  await clickEl(B("이혼"), { ms: 900, after: 600 });
  await connect(I.dad, I.ex);
  await say("03", "알아두기", "같은 두 사람 사이에 가족관계선을 다시 그리면<br>새 선으로 바뀌어요. (예: 결혼 → 별거)");
  await wait(3600);
  await closeup("03", "결과", "아버지와 어머니는 결혼선, 아버지와 전 배우자는<br>이혼선(//)으로 이어졌어요.");

  // ── 04 자녀 ──
  await chapter("04", "자녀 연결하기", "자녀선 · 쌍둥이 · 특수 자녀 · 위치 조정");
  await say("04", "자녀 추가", "자녀가 될 인물을 먼저 추가합니다.");
  await addNode("남성", I.tw1, ...L.tw1);
  await addNode("남성", I.tw2, ...L.tw2);
  await addNode("여성", I.dau, ...L.dau);
  await nameNode(I.tw1, "첫째", "24", true);
  await nameNode(I.tw2, "둘째", "24", true);
  await nameNode(I.dau, "막내", "19", true);
  await say("04", "자녀 연결", "부부의 <b>결혼선을 우클릭</b>한 뒤<br>자녀를 클릭하면 자녀선이 이어져요.", [["결혼/동거선 우클릭", "자녀 연결"]]);
  await wait(1200);
  for (const k of ["dau", "tw1", "tw2"]) {
    const m = await marriagePt(); await rclick(m.x, m.y); await wait(300);
    const c = await nodeCenter(I[k]); await click(c.x, c.y, { ms: 800, after: 700 });
  }
  await cam("[data-tour=geo-child-line]", { max: 2.6 });
  await D("ring", "[data-tour=geo-child-line]");
  await say("04", "자녀선 종류", "연결하기 전에 <b>일반 · 위탁 · 입양</b> 중에서<br>자녀선 모양을 고를 수 있어요.");
  await wait(3400); await D("ringOff"); await cam(null);
  await say("04", "쌍둥이", "두 자녀를 <b>드래그</b>로 감싸 함께 선택하고<br><b>쌍둥이</b> 또는 <b>일란성</b> 버튼을 눌러요.", [["드래그", "범위 선택"], ["Shift + 클릭", "하나씩 추가 선택"]]);
  await wait(1200);
  await pick(I.tw1, I.tw2);
  await ringEl(B("일란성")); await wait(700);
  await clickEl(B("일란성"), { ms: 900, after: 400 }); await D("ringOff");
  { const e = await EMPTY(); await click(e.x, e.y, { ms: 700, after: 1400 }); }
  await say("04", "특수 자녀", "임신 · 사산 · 자연유산 · 인공유산은<br><b>자녀 유형</b> 버튼으로 추가해요.");
  await ringEl(btn("[data-tour=geo-child-types]")); await wait(1400); await D("ringOff");
  await addNode("임신", I.preg, ...L.preg);
  { const m = await marriagePt(); await rclick(m.x, m.y); await wait(300); const c = await nodeCenter(I.preg); await click(c.x, c.y + 8, { ms: 800, after: 1000 }); }
  await nameNode(I.preg, "임신 중", null, true);
  // 이름 칸 자유 이동 + 안내 점선
  await say("04", "이름 칸 옮기기", "도형 아래 <b>이름 글자를 드래그</b>하면 원하는 곳으로 옮길 수 있어요.<br>멀리 옮기면 <b>점선</b>으로 이어져 어느 도형의 설명인지 알 수 있어요.", [["이름 드래그", "위치 이동"]]);
  await wait(1200);
  { const r = await page.evaluate(i => { const gs = [...document.querySelectorAll('[data-tour=geo-canvas] svg g[style*="grab"]')]; const t = [...gs[i].querySelectorAll("text")].find(t => t.style.cursor === "move"); const b = t.getBoundingClientRect(); return { x: b.x + b.width / 2, y: b.y + b.height / 2 }; }, I.preg);
    const z = (await view()).z; await D("tag", "드래그", 1600); await drag(r.x, r.y, r.x + 80 * z, r.y + 45 * z, 1200); await wait(2600); }
  // 자녀선 세로선 위치 조정
  await say("04", "자녀선 위치 조정", "<b>결혼선을 클릭</b>하면 자녀선이 내려오는 지점에 <b>동그란 손잡이</b>가 생겨요.<br>손잡이를 좌우로 끌어 세로선 위치를 옮길 수 있어요.", [["결혼선 클릭", "선택"], ["손잡이 드래그", "좌우 이동"]]);
  await wait(1000);
  { const m = await marriagePt(); await click(m.x, m.y, { ms: 800, after: 900 });
    const h = await page.evaluate(() => { const c = document.querySelector('[data-tour=geo-canvas] svg circle[style*="ew-resize"]'); if (!c) return null; const b = c.getBoundingClientRect(); return { x: b.x + b.width / 2, y: b.y + b.height / 2 }; });
    if (h) { await D("ring", { x: h.x - 14, y: h.y - 14, w: 28, h: 28 }, { radius: 999, pad: 2 }); await wait(1400); await D("ringOff");
      const z = (await view()).z; await D("tag", "손잡이 드래그", 1800); await drag(h.x, h.y, h.x + 120 * z, h.y, 1400); await wait(1500); }
    else console.log("WARN: drop handle not found");
    const e = await EMPTY(); await click(e.x, e.y, { ms: 700, after: 800 }); }
  // 어머니 도형을 옮겨 결혼선 가운데에서 자녀선이 내려오도록 정리
  await say("04", "보기 좋게 정리", "세로선이 한쪽 도형에 너무 붙어 보이면, <b>도형을 드래그</b>해<br>결혼선 위에서 자녀선이 내려오도록 정리해 주세요.", [["도형 드래그", "위치 이동"]]);
  await wait(1400);
  { const c = await nodeCenter(I.mom); await D("ring", { x: c.x - 40, y: c.y - 40, w: 80, h: 80 }, { radius: 999 }); await wait(1000); await D("ringOff");
    await D("tag", "드래그", 1800); await placeNode(I.mom, L.mom[0] + 130, L.mom[1]); await wait(2200); }
  await closeup("04", "결과", "자녀선, 일란성 쌍둥이(V자 + 가로선), 임신 표시,<br>옮긴 이름 칸과 자녀선 위치까지 완성됐어요.");

  // ── 05 상태 표시 ──
  await chapter("05", "인물 상태 표시하기", "내담자 · 사망 · 약물/정신/신체 문제");
  await say("05", "내담자", "인물을 <b>드래그로 감싸거나 Shift+클릭</b>해 선택한 뒤<br><b>내담자 토글</b>을 누르면 이중 도형으로 표시돼요.", [["드래그", "선택"], ["Shift + 클릭", "선택"], ["내담자 토글", "이중 도형"]]);
  await pick(I.dau);
  await clickEl(B("내담자"), { ms: 900, after: 1800 });
  await say("05", "사망", "<b>사망 토글</b>을 누르면 도형에 X 표시가 생겨요.<br>한 번 더 누르면 해제돼요.");
  await pick(I.ex);
  await clickEl(B("사망"), { ms: 900, after: 1800 });
  await say("05", "약물 · 정신 · 신체", "약물남용, 정신·신체 문제, 의심, 회복 상태를<br>도형 안에 채워서 표시할 수 있어요.");
  await pick(I.dad);
  await ringEl(btn("[data-tour=geo-substance]")); await wait(1000); await D("ringOff");
  await clickEl(B("약물남용"), { ms: 900, after: 1600 });
  { const e = await EMPTY(); await click(e.x, e.y, { ms: 700, after: 800 }); }
  await closeup("05", "결과", "전 배우자는 사망(X), 아버지는 약물남용,<br>막내는 내담자(이중 도형)로 표시됐어요.");

  // ── 06 감정·갈등선 ──
  await chapter("06", "감정 · 갈등 관계 표시하기", "관계의 질을 선으로 나타내요");
  await say("06", "갈등", "<b>갈등</b>을 고른 뒤, 인물 연결과 같은 방법으로<br>막내와 어머니를 이어볼게요.");
  await clickEl(B("갈등"), { ms: 900, after: 600 });
  await connect(I.dau, I.mom);
  await say("06", "추가 패널", "오른쪽 패널의 <b>통제</b> 선으로<br>아버지와 첫째의 관계도 표시할게요.");
  await clickEl(B("통제"), { ms: 1100, after: 600 });
  await connect(I.dad, I.tw1);
  { const e = await EMPTY(); await click(e.x, e.y, { ms: 700, after: 500 }); }
  await closeup("06", "결과", "빨간 선은 갈등 · 학대처럼 주의가 필요한 관계,<br>화살표는 방향을 나타내요.");
  const legend = await page.evaluate(() => { const t = [...document.querySelectorAll("[data-tour=geo-canvas] svg text")].find(t => t.textContent.replace(/\s/g, "") === "범례"); if (!t) return null; const g = t.closest("g"); const r = g.getBoundingClientRect(); return { x: r.x, y: r.y, w: r.width, h: r.height }; });
  if (legend) await D("ring", legend, { pad: 4 }); else await D("ringOff");
  await say("06", "범례", "사용한 도형과 선은 <b>범례</b>에 자동으로 정리돼요.<br>드래그로 옮기고, 항목을 더블클릭해 이름을 바꿀 수 있어요.");
  await wait(4200); await D("ringOff");
  if (legend) {
    await say("06", "범례 지우기", "범례를 <b>클릭</b>하면 선택되고 빨간 <b>X</b> 버튼이 생겨요.<br>X를 누르거나 <b>Delete</b> 키로 범례를 지울 수 있어요.", [["클릭", "범례 선택"], ["X / Delete", "지우기"]]);
    await wait(900);
    await click(legend.x + legend.w * 0.55, legend.y + 14, { ms: 900, after: 1300 });
    const x = await page.evaluate(() => { const c = [...document.querySelectorAll("[data-tour=geo-canvas] svg circle")].find(c => c.getAttribute("fill") === "#ef4444"); if (!c) return null; const b = c.getBoundingClientRect(); return { x: b.x + b.width / 2, y: b.y + b.height / 2 }; });
    if (x) { await D("ring", { x: x.x - 12, y: x.y - 12, w: 24, h: 24 }, { radius: 999, pad: 3 }); await wait(1200); await D("ringOff"); await click(x.x, x.y, { ms: 700, after: 1500 }); }
    else { console.log("WARN: legend X not found"); await page.keyboard.press("Delete"); await wait(1500); }
    await say("06", "범례 다시 넣기", "지운 범례는 위쪽의 <b>범례 표시</b> 버튼을 누르면<br>언제든 다시 나타나요.", [["범례 표시", "다시 넣기"]]);
    const show = btn('button:has(span:text-is("범례"))');
    await ringEl(show); await wait(1600); await D("ringOff");
    await clickEl(show, { ms: 1000, after: 2200 });
  }

  // ── 07 텍스트 & 글씨 ──
  await chapter("07", "메모와 글씨 꾸미기", "텍스트 상자 · 글씨 크기 · 굵기");
  await say("07", "텍스트 상자", "<b>T 텍스트</b> 버튼을 누르고 캔버스를 클릭하면<br>그 자리에 텍스트 상자가 생겨요.");
  await clickEl(B("텍스트"), { ms: 900, after: 500 });
  { const p = await scr(60, 380); await click(p.x, p.y, { ms: 900, after: 900 });
    await say("07", "텍스트 상자", "상자가 생기면 <b>더블클릭</b>해서 내용을 적어요.<br>상자는 드래그로 옮길 수 있어요.", [["더블클릭", "내용 수정"], ["드래그", "이동"]]);
    await wait(600); await dbl(p.x + 30, p.y + 18); }
  await page.keyboard.press("Control+A"); await page.keyboard.type("2026. 9. 상담 기록", { delay: 110 }); await wait(500);
  await commit();
  await say("07", "글자 색", "텍스트 상자를 선택하고 색상 점을 누르면<br>글자 색이 바뀌어요.");
  { const p = await scr(120, 400); await click(p.x, p.y, { ms: 900, after: 500 }); }
  await clickEl(btn('button[title^="빨강"]'), { ms: 900, after: 1400 });
  await say("07", "글씨 크기 · 굵기", "이름이나 텍스트를 선택하고 <b>Aa 글씨</b>를 누르면<br>크기와 굵기를 조절할 수 있어요.");
  await pick(I.dau);
  await clickEl(B("글씨"), { ms: 900, after: 700 });
  { const inp = btn('input[type=number]'); const c = await center(inp); await click(c.x, c.y, { after: 300 });
    for (let k = 0; k < 4; k++) { await page.keyboard.press("ArrowUp"); await wait(260); }
    await wait(300); await clickEl(btn('button:has-text("굵게")'), { ms: 600, after: 1200 }); }
  await clickEl(B("글씨"), { ms: 600, after: 600 });
  { const e = await EMPTY(); await click(e.x, e.y, { ms: 700, after: 1000 }); }

  // ── 08 화면 조작 ──
  await chapter("08", "화면 조작과 편집", "확대 · 이동 · 선택 · 삭제 · 되돌리기");
  await say("08", "확대 · 축소", "마우스 <b>휠</b>을 굴리면 확대하거나 축소돼요.", [["휠", "줌 인/아웃"]]);
  { const p = await scr(520, 200); await move(p.x, p.y, 800);
    await D("tag", "휠 굴리기", 2600); for (let k = 0; k < 6; k++) { await page.mouse.wheel(0, -60); await wait(90); } await wait(900);
    for (let k = 0; k < 6; k++) { await page.mouse.wheel(0, 60); await wait(90); } await wait(600); }
  await say("08", "화면 이동", "<b>Alt</b>를 누른 채 빈 곳을 드래그하면 화면을 옮겨요.<br>퍼센트(%) 표시를 누르면 원래 크기로 돌아가요.", [["Alt + 드래그", "화면 이동"]]);
  { const p = await scr(840, 400); await move(p.x, p.y); await page.keyboard.down("Alt"); await D("tag", "Alt + 드래그", 1600);
    await page.mouse.down(); await move(p.x - 160, p.y - 60, 1100); await page.mouse.up(); await page.keyboard.up("Alt"); await wait(900); }
  await clickEl(btn('button[title="100% 리셋"]'), { ms: 1000, after: 1200 });
  await say("08", "범위 선택", "빈 곳에서 드래그하면 여러 인물을 한꺼번에 선택해요.<br>선택한 채로 도형을 끌면 함께 옮겨져요.", [["드래그", "범위 선택"], ["Shift + 클릭", "다중 선택"]]);
  { const a = await scr(350, 205); const b = await scr(820, 330); await drag(a.x, a.y, b.x, b.y, 1100); await wait(900);
    const c = await nodeCenter(I.dau); await drag(c.x, c.y, c.x, c.y + 40, 700); await wait(400); const d = await nodeCenter(I.dau); await drag(d.x, d.y, d.x, d.y - 40, 700); await wait(500); }
  await say("08", "삭제와 되돌리기", "선택한 항목은 <b>Delete</b> 키나 삭제 버튼으로 지워요.<br>실수했다면 <b>뒤로</b> 버튼(Ctrl+Z)으로 되돌려요.", [["Delete", "삭제"], ["Ctrl + Z", "되돌리기"]]);
  await wait(800);
  await pick(I.preg);
  await D("tag", "Delete"); await page.keyboard.press("Delete"); await wait(1400);
  await clickEl(B("뒤로"), { ms: 1000, after: 1600 });
  await say("08", "흑백 모드", "<b>컬러/흑백</b> 스위치로 인쇄용 흑백 화면으로 바꿀 수 있어요.");
  { const sw = btn("[data-tour=geo-save] button.rounded-full"); await clickEl(sw, { ms: 1000, after: 1800 }); await clickEl(sw, { ms: 400, after: 900 }); }

  // ── 09 저장 ──
  await chapter("09", "저장하고 불러오기", "이미지로 내보내고, 이어서 편집해요");
  await say("09", "저장", "<b>저장</b> 버튼을 누르면 세 가지 형식을 고를 수 있어요.");
  await clickEl(B("저장"), { ms: 1000, after: 900 });
  const menuBtn = t => btn(`button:has-text("${t}")`);
  await ringEl(menuBtn("PNG로 저장"), { pad: 2 }); await say("09", "PNG", "한글 · 워드 문서에 붙여 넣을 때는 <b>PNG</b>를 권장해요.");
  { const c = await center(menuBtn("PNG로 저장")); await move(c.x, c.y, 700); } await wait(2600);
  await ringEl(menuBtn("SVG로 저장"), { pad: 2 }); await say("09", "SVG", "<b>SVG</b>는 확대해도 깨지지 않는 고화질 이미지예요.");
  { const c = await center(menuBtn("SVG로 저장")); await move(c.x, c.y, 500); } await wait(2600);
  await ringEl(menuBtn("JSON으로 저장"), { pad: 2 }); await say("09", "JSON", "나중에 이어서 고치려면 <b>JSON</b>으로 저장하세요.<br>작업 내용이 그대로 보관돼요.");
  { const c = await center(menuBtn("JSON으로 저장")); await move(c.x, c.y, 500); } await wait(3000);
  await D("ringOff"); await clickEl(B("저장"), { ms: 700, after: 600 });
  await D("hush");
  await D("card", "알아두기", "저장 위치 정하기", "", { steps: [
    ["저장한 파일은 기본으로 내 컴퓨터의 <b>다운로드</b> 폴더에 들어가요.", "파일 탐색기(Finder)에서 '다운로드' 폴더를 열어 확인하세요."],
    ["원하는 폴더에 저장하려면 브라우저 설정에서<br><b>다운로드 전 저장 위치 확인</b>을 켜 주세요.", "크롬 · 엣지: 설정 → 다운로드", true],
    ["이제 저장할 때마다 창이 떠요. <b>폴더와 파일 이름</b>을 정해 저장하세요.", "JSON 파일은 나중에 '열기'로 불러올 수 있게 잘 보관해 두세요."],
  ] });
  await wait(900); await narrate("알아두기. 저장한 파일은 기본으로 내 컴퓨터의 다운로드 폴더에 들어가요. 원하는 폴더에 저장하려면, 브라우저 설정의 다운로드 메뉴에서 다운로드 전 저장 위치 확인을 켜 주세요. 그러면 저장할 때마다 창이 떠서, 폴더와 파일 이름을 정할 수 있어요.");
  await wait(3000); await free(); await wait(600); await D("cardOff"); await wait(800);
  await ringEl(B("열기")); await say("09", "열기", "<b>열기</b>로 저장해 둔 JSON 파일을 불러와<br>이어서 편집할 수 있어요.");
  { const c = await center(B("열기")); await move(c.x, c.y, 800); } await wait(3200); await D("ringOff");

  // ── 10 도움말 ──
  await chapter("10", "도움이 필요할 때", "앱 안의 사용 안내");
  const help = btn('button[title="사용 안내 투어 다시 보기"]');
  await ringEl(help, { radius: 999 }); await say("10", "사용 안내", "오른쪽 아래 <b>말풍선</b> 버튼을 누르면<br>언제든 앱 안의 사용 안내를 다시 볼 수 있어요.");
  await wait(1500); await D("ringOff"); await clickEl(help, { ms: 1000, after: 2200 });
  await clickEl(btn('button:has-text("다음 →")'), { ms: 900, after: 2000 });
  await clickEl(btn('button:has-text("다음 →")'), { ms: 500, after: 2000 });
  await clickEl(btn('button:has-text("건너뛰기")'), { ms: 900, after: 1000 });
  await closeup("10", "완성", "완성된 가계도예요.<br>PNG로 저장해 보고서에 바로 붙여 넣을 수 있어요.", 4200);
  await free(); await D("hush");
  await D("card", "", "이제 직접 그려보세요", "aninsong77-dotcom.github.io/gonglbaki", { logo: true, foot: "© 2026. 안인성 · 무료 배포   |   음성: Mimic3 KSS voice (CC BY-NC-SA 4.0)" });
  await wait(600); await narrate("이제 직접 그려보세요."); await free();
  await wait(4200);

  await free(); await wait(400);
  fs.writeFileSync(OUT + ".narr.json", JSON.stringify(narr, null, 1));
  clearInterval(pump);
  await cdp.send("Page.stopScreencast");
  await new Promise(r => out.end(r));
  console.log("frames", written, "seconds", (written / 30).toFixed(1));
  await browser.close(); server.close();
})().catch(e => { console.error(e); process.exit(1); });
