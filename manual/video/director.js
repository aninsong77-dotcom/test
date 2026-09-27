// 영상 연출 레이어 — 페이지 안에 주입되어 자막·카드·카메라·커서를 그린다. (앱 코드는 건드리지 않음)
(() => {
  const BRAND = "#2f6b47";
  const css = `
  #dir{position:fixed;inset:0;z-index:2147483000;pointer-events:none;font-family:'Pretendard',sans-serif;-webkit-font-smoothing:antialiased;letter-spacing:-0.01em}
  #dir *{box-sizing:border-box}
  #dir .cur{position:fixed;left:0;top:0;width:26px;height:26px;transform:translate(-100px,-100px);filter:drop-shadow(0 2px 3px rgba(0,0,0,.25));z-index:5}
  #dir .tag{position:fixed;left:0;top:0;display:flex;align-items:center;gap:8px;padding:6px 12px 6px 8px;border-radius:14px;background:rgba(29,29,31,.92);color:#fff;font-size:14px;font-weight:600;opacity:0;transition:opacity .2s;z-index:6;white-space:nowrap;box-shadow:0 6px 18px rgba(0,0,0,.22)}
  #dir .tag svg{flex:none}
  @keyframes mpress{0%,100%{opacity:1}50%{opacity:.35}}
  #dir .tag .hot{animation:mpress .5s ease-in-out 2}
  #dir .rip{position:fixed;width:44px;height:44px;margin:-22px 0 0 -22px;border-radius:50%;background:rgba(47,107,71,.28);border:2px solid rgba(47,107,71,.7);animation:rip .6s ease-out forwards;z-index:4}
  #dir .rip.r{background:rgba(245,158,11,.25);border-color:rgba(217,119,6,.8)}
  @keyframes rip{from{transform:scale(.3);opacity:1}to{transform:scale(1.35);opacity:0}}
  #dir .ring{position:fixed;border-radius:14px;border:2.5px solid ${BRAND};box-shadow:0 0 0 6px rgba(47,107,71,.16),0 8px 30px rgba(47,107,71,.25);opacity:0;transition:all .55s cubic-bezier(.2,.8,.2,1);z-index:2}
  #dir .ring.dim{box-shadow:0 0 0 6px rgba(47,107,71,.16),0 0 0 9999px rgba(20,24,22,.32)}
  #dir .cap{position:fixed;left:28px;bottom:26px;max-width:720px;display:flex;gap:16px;align-items:flex-start;padding:18px 22px 18px 18px;border-radius:20px;
    background:rgba(255,255,255,.88);backdrop-filter:blur(24px) saturate(180%);-webkit-backdrop-filter:blur(24px) saturate(180%);
    border:1px solid rgba(0,0,0,.06);box-shadow:0 12px 40px rgba(0,0,0,.14),0 2px 6px rgba(0,0,0,.05);
    opacity:0;transform:translateY(14px);transition:opacity .4s ease,transform .5s cubic-bezier(.2,.8,.2,1);z-index:3}
  #dir .cap.on{opacity:1;transform:none}
  #dir .num{flex:none;width:40px;height:40px;border-radius:12px;background:${BRAND};color:#fff;font-weight:700;font-size:16px;display:flex;align-items:center;justify-content:center;letter-spacing:0}
  #dir .lab{font-size:13px;font-weight:600;color:${BRAND};margin:1px 0 4px}
  #dir .txt{font-size:21px;font-weight:600;color:#1d1d1f;line-height:1.42;transition:opacity .18s}
  #dir .keys{display:flex;flex-wrap:wrap;gap:6px;margin-top:10px}
  #dir .keys:empty{display:none}
  #dir .key{font-size:13px;font-weight:600;color:#1d1d1f;background:#f5f5f7;border:1px solid #d2d2d7;border-bottom-width:2px;border-radius:8px;padding:3px 9px}
  #dir .key i{font-style:normal;color:#6e6e73;font-weight:500;margin-left:6px}
  #dir .card{position:fixed;inset:0;background:radial-gradient(ellipse at 50% 40%,#ffffff 0%,#f5f5f7 100%);display:flex;flex-direction:column;align-items:center;justify-content:center;text-align:center;opacity:0;transition:opacity .6s ease;z-index:10}
  #dir .card.on{opacity:1}
  #dir .card .ey{font-size:15px;font-weight:700;color:${BRAND};letter-spacing:.14em;margin-bottom:18px;opacity:0;transform:translateY(12px);transition:all .7s cubic-bezier(.2,.8,.2,1) .1s}
  #dir .card .ti{font-size:58px;font-weight:800;color:#1d1d1f;letter-spacing:-.035em;line-height:1.12;opacity:0;transform:translateY(18px);transition:all .8s cubic-bezier(.2,.8,.2,1) .2s;white-space:pre-line}
  #dir .card .su{font-size:22px;font-weight:500;color:#6e6e73;margin-top:20px;line-height:1.5;opacity:0;transform:translateY(14px);transition:all .8s cubic-bezier(.2,.8,.2,1) .35s;white-space:pre-line}
  #dir .card.on .ey,#dir .card.on .ti,#dir .card.on .su{opacity:1;transform:none}
  #dir .card .logo{width:88px;height:88px;border-radius:22px;background:#fff;box-shadow:0 10px 30px rgba(0,0,0,.10);display:flex;align-items:center;justify-content:center;margin-bottom:28px;opacity:0;transform:scale(.9);transition:all .8s cubic-bezier(.2,.8,.2,1)}
  #dir .card.on .logo{opacity:1;transform:none}
  #dir .card .logo img{width:64px;height:64px;object-fit:contain}
  #dir .card .ft{position:absolute;bottom:40px;font-size:14px;color:#86868b;font-weight:500}
  #dir .card .steps{margin-top:34px;display:flex;flex-direction:column;gap:14px;text-align:left;opacity:0;transform:translateY(14px);transition:all .8s cubic-bezier(.2,.8,.2,1) .45s}
  #dir .card.on .steps{opacity:1;transform:none}
  #dir .card .steps:empty{display:none}
  #dir .st{display:flex;align-items:center;gap:16px;background:#fff;border-radius:18px;padding:16px 22px;box-shadow:0 4px 18px rgba(0,0,0,.06);width:720px}
  #dir .st b.n{flex:none;width:34px;height:34px;border-radius:50%;background:#2f6b47;color:#fff;font-size:16px;display:flex;align-items:center;justify-content:center}
  #dir .st .tx{flex:1;font-size:19px;font-weight:600;color:#1d1d1f;line-height:1.45}
  #dir .st .tx small{display:block;font-size:15px;font-weight:500;color:#6e6e73;margin-top:2px}
  #dir .tg{flex:none;width:46px;height:28px;border-radius:999px;background:#34a853;position:relative}
  #dir .tg:after{content:'';position:absolute;right:3px;top:3px;width:22px;height:22px;border-radius:50%;background:#fff;box-shadow:0 1px 3px rgba(0,0,0,.3)}
  #root svg{-webkit-user-select:none;user-select:none}
  #root{transform-origin:0 0;transition:transform 1s cubic-bezier(.25,.8,.25,1)}
  `;
  function build() {
    if (document.getElementById("dir")) return;
    const st = document.createElement("style"); st.textContent = css; document.head.appendChild(st);
    const d = document.createElement("div"); d.id = "dir";
    d.innerHTML = `
      <div class="card on" id="dcard"><div class="logo" id="dlogo"><img src="images/logo.png"></div><div class="ey" id="dey"></div><div class="ti" id="dti"></div><div class="su" id="dsu"></div><div class="steps" id="dsteps"></div><div class="ft" id="dft"></div></div>
      <div class="ring" id="dring"></div>
      <div class="cap" id="dcap"><div class="num" id="dnum"></div><div><div class="lab" id="dlab"></div><div class="txt" id="dtxt"></div><div class="keys" id="dkeys"></div></div></div>
      <svg class="cur" id="dcur" viewBox="0 0 26 26"><path d="M4 2 L4 21 L9 16.5 L12.5 24 L15.8 22.6 L12.4 15.2 L19 15.2 Z" fill="#111" stroke="#fff" stroke-width="1.6" stroke-linejoin="round"/></svg>
      <div class="tag" id="dtag"></div>`;
    document.body.appendChild(d);
    const cur = d.querySelector("#dcur"), tag = d.querySelector("#dtag");
    let mx = -100, my = -100, tagTimer;
    window.addEventListener("mousemove", e => { mx = e.clientX; my = e.clientY; cur.style.transform = `translate(${mx - 4}px,${my - 2}px)`; tag.style.transform = `translate(${mx + 22}px,${my + 20}px)`; }, true);
    window.addEventListener("mousedown", e => {
      const r = document.createElement("div"); r.className = "rip" + (e.button === 2 ? " r" : "");
      r.style.left = e.clientX + "px"; r.style.top = e.clientY + "px"; d.appendChild(r); setTimeout(() => r.remove(), 700);
    }, true);
    const $ = id => d.querySelector("#" + id);
    let cam = { s: 1, tx: 0, ty: 0 };
    const root = () => document.getElementById("root");
    const toLayout = r => ({ x: (r.x - cam.tx) / cam.s, y: (r.y - cam.ty) / cam.s, w: r.width / cam.s, h: r.height / cam.s });
    const rectOf = sel => { const e = typeof sel === "string" ? document.querySelector(sel) : sel; return e ? e.getBoundingClientRect() : null; };
    window.__dir = {
      card(ey, ti, su, opt = {}) {
        $("dey").textContent = ey || ""; $("dti").textContent = ti || ""; $("dsu").textContent = su || ""; $("dft").textContent = opt.foot || "";
        $("dlogo").style.display = opt.logo ? "" : "none";
        $("dsteps").innerHTML = (opt.steps || []).map((t, i) => `<div class="st"><b class="n">${i + 1}</b><div class="tx">${t[0]}${t[1] ? `<small>${t[1]}</small>` : ""}</div>${t[2] ? `<span class="tg"></span>` : ""}</div>`).join("");
        const c = $("dcard"); c.classList.remove("on"); void c.offsetWidth; c.classList.add("on");
      },
      cardOff() { $("dcard").classList.remove("on"); },
      say(num, lab, txt, keys = []) {
        const cap = $("dcap"), t = $("dtxt");
        const apply = () => {
          $("dnum").textContent = num; $("dlab").textContent = lab; t.innerHTML = txt;
          $("dkeys").innerHTML = keys.map(k => Array.isArray(k) ? `<span class="key">${k[0]}<i>${k[1]}</i></span>` : `<span class="key">${k}</span>`).join("");
          t.style.opacity = 1; cap.classList.add("on");
        };
        if (cap.classList.contains("on")) { t.style.opacity = 0; setTimeout(apply, 180); } else apply();
      },
      hush() { $("dcap").classList.remove("on"); },
      ring(sel, opt = {}) {
        const r = typeof sel === "object" && sel && "w" in sel ? { x: sel.x, y: sel.y, width: sel.w, height: sel.h } : rectOf(sel);
        const g = $("dring"); if (!r) { g.style.opacity = 0; return; }
        const p = opt.pad ?? 6;
        Object.assign(g.style, { left: r.x - p + "px", top: r.y - p + "px", width: r.width + 2 * p + "px", height: r.height + 2 * p + "px", opacity: 1, borderRadius: (opt.radius ?? 14) + "px" });
        g.classList.toggle("dim", !!opt.dim);
      },
      ringOff() { const g = $("dring"); g.style.opacity = 0; g.classList.remove("dim"); },
      cam(sel, opt = {}) {
        const el = root(); const vw = innerWidth, vh = innerHeight;
        if (!sel) { cam = { s: 1, tx: 0, ty: 0 }; el.style.transform = ""; return; }
        const rr = typeof sel === "object" && "w" in sel ? sel : toLayout(rectOf(sel));
        const margin = opt.margin ?? 80;
        let s = Math.min((vw - margin * 2) / rr.w, (vh - margin * 2 - (opt.bottom ?? 120)) / rr.h, opt.max ?? 2.2);
        s = Math.max(1, s);
        const cx = rr.x + rr.w / 2, cy = rr.y + rr.h / 2;
        let tx = vw / 2 - cx * s, ty = (vh - (opt.bottom ?? 120)) / 2 - cy * s;
        tx = Math.min(0, Math.max(vw - vw * s, tx)); ty = Math.min(0, Math.max(vh - vh * s, ty));
        cam = { s, tx, ty }; el.style.transform = `translate(${tx}px,${ty}px) scale(${s})`;
      },
      tag(text, ms = 1200) {
        clearTimeout(tagTimer);
        const kind = /우클릭/.test(text) ? "R" : /휠/.test(text) ? "W" : /클릭|드래그/.test(text) ? "L" : "";
        const G = "#34a853", O = "#f59e0b";
        const lf = kind === "L" ? G : "none", rf = kind === "R" ? O : "none", wf = kind === "W" ? G : "#fff";
        const extra = /더블클릭/.test(text) ? `<text x="7" y="15" font-size="8" font-weight="800" fill="#fff" text-anchor="middle">2</text>`
          : /드래그/.test(text) ? `<path d="M26 26 h8 m-3 -3 l3 3 l-3 3" stroke="#fff" stroke-width="1.6" fill="none" stroke-linecap="round"/>` : "";
        const mouse = kind ? `<svg width="${/드래그/.test(text) ? 36 : 28}" height="38" viewBox="0 0 ${/드래그/.test(text) ? 36 : 28} 38">
          <path d="M14 2 C7 2 2 7 2 14 V24 C2 31 7 36 14 36 C21 36 26 31 26 24 V14 C26 7 21 2 14 2 Z" fill="none" stroke="#fff" stroke-width="1.8"/>
          <path class="${kind === "L" ? "hot" : ""}" d="M13 3.2 C7.5 3.6 3.2 8 3.2 14 V17 H13 Z" fill="${lf}"/>
          <path class="${kind === "R" ? "hot" : ""}" d="M15 3.2 C20.5 3.6 24.8 8 24.8 14 V17 H15 Z" fill="${rf}"/>
          <line x1="2" y1="17" x2="26" y2="17" stroke="#fff" stroke-width="1.5"/><line x1="14" y1="2" x2="14" y2="17" stroke="#fff" stroke-width="1.5"/>
          <rect class="${kind === "W" ? "hot" : ""}" x="11.5" y="7" width="5" height="8" rx="2.5" fill="${wf}" stroke="#1d1d1f" stroke-width="1"/>${extra}</svg>` : "";
        tag.innerHTML = mouse + `<span>${text}</span>`;
        tag.style.opacity = 1; tagTimer = setTimeout(() => tag.style.opacity = 0, ms);
      },
    };
  }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", build); else build();
})();
