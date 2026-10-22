// 통 — 계산 부분만 모은 파일 (검토용)
// 원본: stats-ladder.html 한 파일(약 1만 줄, 화면·설문·AI 연결 포함)에서 통계 계산과 관련된 부분만 뽑았습니다.
// 이 파일만으로 실행되지는 않습니다(화면 코드와 이어져 있음). 실제 동작은 tests/로 R과 비교해 확인합니다.
// 검증: tests/README.md 참고 — R 기준값 비교, 화면에서 돌린 분석 전부를 통이 만든 R 코드와 비교, 경계 상황 테스트.
// 들어 있는 분석 함수 23개: pearson, spearman, partial, pointbiserial, chisq, onesample, ind_t, paired_t, oneway, factorial, ancova, regression, hier, logistic, med_parallel, med_serial, moderation, modmed7, modmed14, rm_anova, mixed_anova, mcnemar, cochran

// ##### 1. 응답 읽기 · 동의 판정 · 점수 만들기(역문항, 최소 응답, 중복) (stats-ladder.html 4115~4225줄) #####
  function autoMap() {
    colMap = {}; mapAuto = {};
    const H = raw.headers.map(h => norm(h).toLowerCase());
    const used = new Set();
    designItems().forEach(d => {
      const t = d.text.toLowerCase();
      let idx = H.findIndex((h, i) => !used.has(i) && h === t);
      if (idx < 0) idx = H.findIndex((h, i) => !used.has(i) && (h.includes(t) || (h.length > 6 && t.includes(h))));
      if (idx >= 0) { used.add(idx); mapAuto[d.key] = true; }
      colMap[d.key] = idx;
    });
    // 동의 열: 척도 문항("…에 동의한다")을 잘못 잡지 않도록, 문항에 연결된 열은 빼고
    // 통이 만든 폼의 '연구 참여 동의' 열, 또는 '참여 동의'·'동의 여부'처럼 동의 질문임이 분명한 머리글만 찾음
    const isConsentHead = h => /^연구 참여 동의$/.test(h) || /(연구|참여)\s*(에\s*)?(대한\s*)?동의|동의\s*(여부|서)|^동의$/.test(h);
    consentCol = H.findIndex((h, i) => !used.has(i) && h === '연구 참여 동의');
    if (consentCol < 0) consentCol = H.findIndex((h, i) => !used.has(i) && isConsentHead(h));
  }
  // 비동의 판정: 답이 '동의하지 않습니다'·'비동의'·'아니요'로 시작할 때만 (중간에 '아니'가 들어간 동의 답은 빼지 않음)
  const isRefusal = v => /^(동의\s*하?지\s*않|동의\s*안\s*함|비동의|아니[요오]?(\s|,|\.|$)|no\b|disagree)/i.test(norm(v));

  // 구글 폼 타임스탬프 등 '2026/10/30 11:59:00 오후', '2026. 10. 30 오후 11:59:00', '2026-10-30 23:59' → 밀리초 (못 읽으면 null)
  function parseStamp(v) {
    const t = norm(v); if (!t) return null;
    let n = t.match(/(\d{4})\D+(\d{1,2})\D+(\d{1,2})\D+?(?:(오전|오후|AM|PM)\s*)?(\d{1,2}):(\d{2})(?::(\d{2}))?\s*(오전|오후|AM|PM)?/i);
    // 영어 구글 폼: '10/21/2026 16:41:21' (월/일/연도)
    const us = !n && t.match(/^(\d{1,2})\/(\d{1,2})\/(\d{4})\s+(\d{1,2}):(\d{2})(?::(\d{2}))?\s*(AM|PM)?/i);
    if (us) n = [us[0], us[3], us[1], us[2], '', us[4], us[5], us[6], us[7]];
    if (!n) return null;
    let h = +n[5]; const ap = (n[4] || n[8] || '').toUpperCase();
    if ((ap === '오후' || ap === 'PM') && h < 12) h += 12; if ((ap === '오전' || ap === 'AM') && h === 12) h = 0;
    return new Date(+n[1], +n[2] - 1, +n[3], h, +n[6], +(n[7] || 0)).getTime();
  }
  function loadRaw(text, label) {
    rawText = text;
    try { raw = parseTable(text); }
    catch (err) { $('#co-status').textContent = '읽지 못했습니다: ' + err.message; raw = null; renderCollect(); return; }
    autoMap();
    raw.sig = designItems().map(d => d.key + d.text).join('|');
    $('#co-status').textContent = `${label}: 응답 ${raw.rows.length}줄, 열 ${raw.headers.length}개를 읽었습니다.`;
    renderCollect();
  }

  $('#co-file').addEventListener('change', async e => {
    const f = e.target.files[0]; e.target.value = '';
    if (f) loadRaw(await f.text(), f.name);
  });
  $('#co-read').addEventListener('click', () => {
    const t = $('#co-paste').value;
    if (!norm(t)) { toast('붙여 넣은 내용이 없습니다.'); return; }
    loadRaw(t, '붙여 넣은 내용');
  });

  // 예시 응답: 설계를 바탕으로 만든 가짜 자료 (X → 매개 → Y 관계를 약하게 넣음)
  function sampleCsv(n = 60) {
    let seed = 20261019;
    const rnd = () => { seed = (seed + 0x6D2B79F5) | 0; let t = seed; t = Math.imul(t ^ (t >>> 15), t | 1); t ^= t + Math.imul(t ^ (t >>> 7), t | 61); return ((t ^ (t >>> 14)) >>> 0) / 4294967296; };
    const gauss = () => Math.sqrt(-2 * Math.log(rnd() || 1e-9)) * Math.cos(2 * Math.PI * rnd());
    const items = designItems();
    const headers = ['타임스탬프'].concat(sv.consent.on ? ['연구 참여 동의'] : [], items.map(d => d.text));
    const rows = [headers];
    for (let r = 0; r < n; r++) {
      const z = {}; let x = gauss(), prevM = 0;
      sv.scales.forEach((s, si) => {
        if (s.role === '독립') z[si] = x;
        else if (s.role === '매개') { z[si] = 0.45 * x + 0.3 * prevM + 0.8 * gauss(); prevM = z[si]; }
        else if (s.role === '종속') z[si] = 0.2 * x + 0.4 * prevM + 0.8 * gauss();
        else z[si] = gauss();
      });
      const straight = r === 7 || r === 33;
      const refuse = r === 20 && sv.consent.on;
      const row = [`2026/10/${String(1 + (r % 18)).padStart(2, '0')} ${String(9 + (r % 12)).padStart(2, '0')}:${String((r * 7) % 60).padStart(2, '0')}:00`];
      if (sv.consent.on) row.push(refuse ? '동의하지 않습니다' : '동의합니다');
      items.forEach(d => {
        if (refuse) { row.push(''); return; }
        if (d.kind === 'item') {
          const s = sv.scales[d.si], P = +s.points, it = s.items[d.ii];
          let v = straight ? P : Math.round((P + 1) / 2 + z[d.si] * (P / 5) + gauss() * 0.7);
          v = Math.min(P, Math.max(1, v));
          if (it.reverse && !straight) v = P + 1 - v;
          row.push(s.labels[v - 1] ?? String(v));
        } else if (d.kind === 'attn') {
          // 대부분 정답, 같은 번호만 고른 사람과 한 사람은 틀림
          const a = sv.attn[d.ai], L = sv.scales[a.si].labels;
          row.push(straight ? L[L.length - 1] : r === 45 ? L[0] : L[a.answer]);
        } else {
          const dm = sv.demos[d.di];
          if (dm.type === 'choice') row.push(dm.options[Math.floor(rnd() * dm.options.length)] || '');
          else if (dm.type === 'number') row.push(String(20 + Math.floor(rnd() * 12)));
          else row.push('');
        }
      });
      rows.push(row);
    }
    return rows.map(r => r.map(csvCell).join(',')).join('\n');
  }
  $('#co-sample').addEventListener('click', () => {
    if (!designItems().length) { toast('먼저 설문 설계에 문항을 넣어 주세요. [예시 설문 불러오기]도 있습니다.'); return; }
    const t = sampleCsv();
    $('#co-paste').value = t;
    loadRaw(t, '예시 응답(가짜 자료)');
    if (raw) raw.sample = true;
  });

  function renderCollect() {
    if (!raw) { $('#co-map').innerHTML = ''; $('#co-result').innerHTML = ''; return; }
    const items = designItems();
    if (!items.length) {
      $('#co-map').innerHTML = `<div class="card"><div class="errbox">설문 설계에 문항이 없어 어떤 열이 어떤 문항인지 알 수 없습니다. [설문 설계] 메뉴에서 문항을 먼저 넣어 주세요.</div></div>`;
      $('#co-result').innerHTML = ''; return;
    }
    const hOpts = sel => `<option value="-1" ${sel < 0 ? 'selected' : ''}>(연결 안 함)</option>` + raw.headers.map((h, i) => `<option value="${i}" ${i === sel ? 'selected' : ''}>${i + 1}. ${esc(h.length > 40 ? h.slice(0, 40) + '…' : h)}</option>`).join('');

// ##### 2. 계산 도우미 · 분포 · 회귀(QR) · 부트스트랩 · 베이즈 인자 · 숫자 표기 (stats-ladder.html 4256~4766줄) #####
  // ---------- 계산 도우미 ----------
  const mean = a => a.reduce((x, y) => x + y, 0) / a.length;
  const sd = a => { if (a.length < 2) return NaN; const m = mean(a); return Math.sqrt(a.reduce((x, y) => x + (y - m) ** 2, 0) / (a.length - 1)); };
  const variance = a => sd(a) ** 2;
  function corr(a, b) {
    const ma = mean(a), mb = mean(b); let sab = 0, saa = 0, sbb = 0;
    for (let i = 0; i < a.length; i++) { sab += (a[i] - ma) * (b[i] - mb); saa += (a[i] - ma) ** 2; sbb += (b[i] - mb) ** 2; }
    return sab / Math.sqrt(saa * sbb);
  }
  function skew(a) {
    const n = a.length; if (n < 3) return NaN; const m = mean(a), s = sd(a); if (!s) return NaN;
    return n / ((n - 1) * (n - 2)) * a.reduce((x, y) => x + ((y - m) / s) ** 3, 0);
  }
  function kurt(a) {
    const n = a.length; if (n < 4) return NaN; const m = mean(a), s = sd(a); if (!s) return NaN;
    const k = a.reduce((x, y) => x + ((y - m) / s) ** 4, 0);
    return n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) * k - 3 * (n - 1) ** 2 / ((n - 2) * (n - 3));
  }
  // 크론바흐 알파: cols = 문항별 값 배열(같은 사람 순서), 모두 응답한 사람만
  function alpha(cols) {
    const k = cols.length; if (k < 2) return NaN;
    const n = cols[0].length; if (n < 3) return NaN;
    const totals = Array.from({ length: n }, (_, r) => cols.reduce((x, c) => x + c[r], 0));
    const vt = variance(totals); if (!vt) return NaN;
    return k / (k - 1) * (1 - cols.reduce((x, c) => x + variance(c), 0) / vt);
  }
  const f2 = x => x == null || !isFinite(x) ? '-' : x.toFixed(2);
  const fr = x => x == null || !isFinite(x) ? '-' : x.toFixed(2).replace(/^(-?)0\./, '$1.');
  const aClass = a => !isFinite(a) ? '' : a >= 0.7 ? 'a-ok' : a >= 0.6 ? 'a-mid' : 'a-low';

  let lastScored = null;

  // 기본 정보 값 정리: 숫자 문항은 '23세', '만 23', '스물셋'을 숫자로, 고르기 문항은 연구자가 정한 바꾸기(합치기) 표를 따름
  const KTEN = { 열: 10, 스물: 20, 서른: 30, 마흔: 40, 쉰: 50, 예순: 60, 일흔: 70, 여든: 80 }, KONE = { 하나: 1, 한: 1, 둘: 2, 두: 2, 셋: 3, 세: 3, 넷: 4, 네: 4, 다섯: 5, 여섯: 6, 일곱: 7, 여덟: 8, 아홉: 9 };
  function demoNum(v) {
    const t = norm(v); if (!t) return '';
    const m = t.replace(/,/g, '').match(/-?\d+(?:\.\d+)?/); if (m) return m[0];
    const k = t.replace(/\s/g, '').replace(/^만/, '').match(/^(열|스물|서른|마흔|쉰|예순|일흔|여든)?(하나|한|둘|두|셋|세|넷|네|다섯|여섯|일곱|여덟|아홉)?(?:살|세)?$/);
    return k && (k[1] || k[2]) ? String((KTEN[k[1]] || 0) + (KONE[k[2]] || 0)) : null;
  }
  // 자주 쓰는 같은 뜻의 값 (선택지에 그 이름이 있을 때만 자동으로)
  const SYNO = [[/^(남|남자|남성|male|m)$/i, '남성'], [/^(여|여자|여성|female|f)$/i, '여성']];
  function demoCat(dm, di, cell) {
    const map = (plan.recode || {})[di] || {};
    if (Object.prototype.hasOwnProperty.call(map, cell)) return { v: map[cell], how: 'recode' };
    if (!cell || (dm.options || []).includes(cell)) return { v: cell, how: '' };
    const syn = SYNO.find(([re, to]) => re.test(cell) && (dm.options || []).includes(to));
    return syn ? { v: syn[1], how: 'auto' } : { v: cell, how: 'other' };
  }
  // 연속 같은 답의 가장 긴 길이
  const longestRun = a => { let best = 0, cur = 0; a.forEach((v, i) => { cur = i && v === a[i - 1] ? cur + 1 : 1; best = Math.max(best, cur); }); return best; };
  const longRunCut = n => Math.max(10, Math.ceil(n * 0.5));
  // 연구 대상 조건에 맞나
  function eligOk(p) {
    return (plan.elig || []).every(c => {
      const v = p.items[`d${c.di}`];
      if (c.type === 'num') { const x = v === '' || v == null ? NaN : +v; return isFinite(x) && (c.min === '' || c.min == null || x >= +c.min) && (c.max === '' || c.max == null || x <= +c.max); }
      return (c.keep || []).includes(v);
    });
  }

  function runScoring() {
    const items = designItems();
    const unknown = new Map(); // 값 -> 횟수
    const demoFix = new Map(); // `${di}|원래값` -> { di, from, to, how, n }
    const people = raw.rows.map((row, r) => {
      const p = { id: 'R' + String(r + 1).padStart(3, '0'), row, items: {}, rawLikert: [], excluded: '', careless: [], attn: [] };
      const cv = consentCol >= 0 ? norm(row[consentCol]) : '';
      if (consentCol >= 0 && isRefusal(cv)) p.excluded = '비동의';
      items.forEach(d => {
        const cell = norm(colMap[d.key] >= 0 ? row[colMap[d.key]] : '');
        if (d.kind === 'demo') {
          const dm = sv.demos[d.di];
          if (dm.type === 'number') { const x = demoNum(cell); p.items[d.key] = x == null ? '' : x; if (cell && x !== cell) { const k = `${d.di}|${cell}`, o = demoFix.get(k) || { di: d.di, from: cell, to: x, how: x == null ? 'bad' : 'num', n: 0 }; o.n++; demoFix.set(k, o); } }
          else if (dm.type === 'choice') { const c = demoCat(dm, d.di, cell); p.items[d.key] = c.v; if (c.how) { const k = `${d.di}|${cell}`, o = demoFix.get(k) || { di: d.di, from: cell, to: c.v, how: c.how, n: 0 }; o.n++; demoFix.set(k, o); } }
          else p.items[d.key] = cell;
          return;
        }
        const s = sv.scales[d.si], P = +s.points;
        let v = null;
        if (cell !== '') {
          const li = s.labels.findIndex(l => norm(l) === cell);
          if (li >= 0) v = li + 1;
          else { const m = cell.match(/^(\d+)/); if (m && +m[1] >= 1 && +m[1] <= P) v = +m[1]; else unknown.set(cell, (unknown.get(cell) || 0) + 1); }
        }
        if (d.kind === 'attn') { const a = sv.attn[d.ai]; p.attn.push({ code: d.code, ok: v === a.answer + 1, ans: cell }); return; }
        if (v != null) p.rawLikert.push(v);
        p.items[d.key] = v == null ? null : (s.items[d.ii].reverse ? P + 1 - v : v);
      });
      const likertN = items.filter(d => d.kind === 'item').length;
      if (!p.excluded && !p.rawLikert.length && likertN) p.excluded = '척도 응답 없음';
      // 불성실 의심: ① 모든 문항 같은 번호 ② 같은 번호를 길게 이어 고름 ③ 같은 변인의 정문항과 역문항에 모두 강하게 동의(또는 모두 강하게 반대)
      p.straight = p.rawLikert.length >= 8 && p.rawLikert.every(v => v === p.rawLikert[0]);
      if (p.straight) p.careless.push('모든 문항 같은 번호');
      else { const run = longestRun(p.rawLikert); p.longRun = run; if (p.rawLikert.length >= 12 && run >= longRunCut(p.rawLikert.length)) p.careless.push(`같은 번호 ${run}문항 연속`); }
      sv.scales.forEach((s, si) => {
        const P = +s.points, its = items.filter(d => d.kind === 'item' && d.si === si), raw0 = d => p.items[d.key] == null ? null : (s.items[d.ii].reverse ? P + 1 - p.items[d.key] : p.items[d.key]);
        const pos = its.filter(d => !s.items[d.ii].reverse).map(raw0).filter(v => v != null), neg = its.filter(d => s.items[d.ii].reverse).map(raw0).filter(v => v != null);
        if (!p.straight && pos.length >= 2 && neg.length >= 1 && ((mean(pos) >= P - 0.5 && mean(neg) >= P - 0.5) || (mean(pos) <= 1.5 && mean(neg) <= 1.5))) p.careless.push(`${s.name || scaleCode(s, si)} 역문항 모순`);
      });
      p.revCon = p.careless.some(c => /역문항 모순/.test(c));
      p.attnFail = p.attn.some(a => !a.ok);
      return p;
    });
    // 제외 순서: 비동의 → 빈 응답 → 대상 아님 → 주의 확인 실패 → 불성실 의심 → 직접 뺌 → 중복 → 이상치 (사람마다 첫 사유 하나만)
    const dropCareless = p => (p.straight && plan.dec.straight === 'drop') || (p.careless.some(c => /연속/.test(c)) && plan.dec.longrun === 'drop') || (p.revCon && plan.dec.revcon === 'drop');
    people.forEach(p => {
      if (p.excluded) return;
      if (!eligOk(p)) p.excluded = '대상 아님';
      else if (p.attnFail && (plan.dec.attn || 'drop') === 'drop') p.excluded = '주의 확인 실패';
      else if (dropCareless(p)) p.excluded = '불성실 의심';
      else if ((plan.manual || {})[p.id] != null) p.excluded = '직접 뺌';
    });
    // 중복 응답: 문항 답이 통째로 같은 응답 (뒤의 것). 제출 시각 열이 있으면 먼저 낸 응답과의 간격(분)을 남김
    const tsCol = raw.headers.findIndex(h => /타임스탬프|timestamp|제출\s*(시각|시간|일시)|응답\s*(시각|시간|일시)/i.test(norm(h)));
    const seenSig = new Map();
    people.forEach(p => {
      p.ts = tsCol >= 0 ? parseStamp(p.row[tsCol]) : null;
      if (p.excluded) return;
      const sig = items.map(d => p.items[d.key] ?? '').join('|'); if (!/[^|]/.test(sig)) return;
      if (seenSig.has(sig)) {
        const first = seenSig.get(sig); p.dup = first.id;
        p.dupGap = p.ts != null && first.ts != null ? Math.abs(p.ts - first.ts) / 60000 : null;
        if ((plan.dec.dup || 'keep') === 'drop') p.excluded = '중복';
      }
      else seenSig.set(sig, p);
    });
    const outliers = findOutliers(people.filter(p => !p.excluded), items);
    if ((plan.dec.outlier || 'keep') === 'drop') outliers.forEach(o => { o.p.excluded = '이상치'; });
    const inc = people.filter(p => !p.excluded);

    // 변인 점수
    const scaleInfo = sv.scales.map((s, si) => {
      const its = items.filter(d => d.kind === 'item' && d.si === si);
      const need = Math.ceil(its.length * (+plan.dec.minPct || 80) / 100);
      inc.forEach(p => {
        const vals = its.map(d => p.items[d.key]).filter(v => v != null);
        p.score = p.score || {};
        p.score[si] = vals.length && vals.length >= need ? (sv.scoring === 'sum' ? mean(vals) * its.length : mean(vals)) : null;
      });
      const scores = inc.map(p => p.score[si]).filter(v => v != null);
      const complete = inc.filter(p => its.every(d => p.items[d.key] != null));
      const cols = its.map(d => complete.map(p => p.items[d.key]));
      const a = alpha(cols);
      const itemStats = its.map((d, j) => {
        const col = cols[j];
        const rest = complete.map((p, r) => cols.reduce((x, c, k) => k === j ? x : x + c[r], 0));
        const all = inc.map(p => p.items[d.key]).filter(v => v != null);
        return { d, m: all.length ? mean(all) : NaN, s: sd(all), r: cols.length > 1 && complete.length > 2 ? corr(col, rest) : NaN, aDel: alpha(cols.filter((_, k) => k !== j)) };
      });
      return { s, si, its, n: scores.length, m: scores.length ? mean(scores) : NaN, sdv: sd(scores), min: scores.length ? Math.min(...scores) : NaN, max: scores.length ? Math.max(...scores) : NaN, sk: skew(scores), ku: kurt(scores), a, nComplete: complete.length, itemStats };
    });

    // 기본 정보 요약
    const demoInfo = sv.demos.map((dm, di) => {
      const key = `d${di}`; const vals = inc.map(p => p.items[key]).filter(v => v !== '' && v != null);
      if (dm.type === 'number') { const nums = vals.map(Number).filter(isFinite); return { dm, di, kind: 'num', n: nums.length, m: nums.length ? mean(nums) : NaN, s: sd(nums), min: Math.min(...nums), max: Math.max(...nums) }; }
      const freq = new Map(); vals.forEach(v => freq.set(v, (freq.get(v) || 0) + 1));
      const order = (dm.options || []).filter(o => freq.has(o)).concat([...freq.keys()].filter(k => !(dm.options || []).includes(k)));
      return { dm, di, kind: 'cat', n: vals.length, freq: order.map(k => [k, freq.get(k)]) };
    });

    lastScored = { people, inc, items, scaleInfo, demoInfo, unknown, demoFix: [...demoFix.values()], sample: !!raw.sample, outliers, pilot: dataKind() === 'pilot' };
    dataVersion++;
    scheduleSave();
    renderScored();
  }

  function analysisRows() {
    const { inc, items } = lastScored;
    const head = ['ID'];
    sv.demos.forEach((d, di) => { head.push(demoCode(d, di)); if (d.type === 'choice') head.push(demoCode(d, di) + '_n'); });
    items.filter(d => d.kind === 'item').forEach(d => head.push(d.code));
    sv.scales.forEach((s, si) => head.push(scaleCode(s, si)));
    head.push('불성실의심');
    const rows = [head];
    inc.forEach(p => {
      const r = [p.id];
      sv.demos.forEach((d, di) => {
        const v = p.items[`d${di}`] ?? '';
        r.push(v);
        if (d.type === 'choice') { const k = d.options.indexOf(v); r.push(k >= 0 ? k + 1 : ''); }
      });
      items.filter(d => d.kind === 'item').forEach(d => r.push(p.items[d.key] ?? ''));
      sv.scales.forEach((s, si) => { const v = p.score[si]; r.push(v == null ? '' : +v.toFixed(6)); });
      r.push(p.straight ? 1 : 0);
      rows.push(r);
    });
    return rows;
  }

  function itemStatsRows() {
    const rows = [['변인', '변수 이름', '문항', '평균', '표준편차', '수정된 문항-총점 상관', '삭제 시 알파', '변인 알파']];
    lastScored.scaleInfo.forEach(si => si.itemStats.forEach(it => rows.push([si.s.name, it.d.code, it.d.text, f2(it.m), f2(it.s), fr(it.r), fr(it.aDel), fr(si.a)])));
    return rows;
  }

  function renderScored() {
    const L = lastScored;
    const ex = L.people.filter(p => p.excluded);
    const exCount = k => ex.filter(p => p.excluded === k).length;
    const straightKept = L.inc.filter(p => p.careless.length).length;
    const extra = [['대상 아님', '대상 아님'], ['주의 확인 실패', '주의 확인'], ['직접 뺌', '직접 뺌']].filter(([k]) => exCount(k)).map(([k, l]) => ` · ${l} ${exCount(k)}`).join('');
    const unk = [...L.unknown.entries()];
    const preview = analysisRows();
    const pvCols = preview[0].length;

    $('#co-result').innerHTML = `
    <div class="card">
      <h3 class="card-title">3. 결과</h3>
      <div class="stats">
        <div class="stat"><b>${L.people.length}</b><span>전체 응답</span></div>
        <div class="stat"><b>${L.inc.length}</b><span>분석에 포함</span></div>
        <div class="stat"><b>${ex.length}</b><span>제외 (비동의 ${exCount('비동의')} · 빈 응답 ${exCount('척도 응답 없음')} · 불성실 ${exCount('불성실 의심')} · 중복 ${exCount('중복')} · 이상치 ${exCount('이상치')}${extra})</span></div>
        <div class="stat"><b>${straightKept}</b><span>불성실 의심이지만 포함됨</span></div>
      </div>
      ${unk.length ? `<div class="errbox">선택지에 없는 값이 있어 빈 값으로 처리했습니다: ${unk.slice(0, 8).map(([v, c]) => `"${esc(v)}" ${c}회`).join(', ')}${unk.length > 8 ? ' 외' : ''}\n폼의 선택지 문장을 고쳤다면 설문 설계의 선택지도 같게 맞춰 주세요.</div>` : ''}
      ${ex.length ? `<p class="excl">제외된 응답: ${ex.slice(0, 20).map(p => `${p.id}(${p.excluded})`).join(', ')}${ex.length > 20 ? ' …' : ''}</p>` : ''}
      ${L.pilot ? '<p class="cf-note"><b>예비조사 자료</b>로 표시했습니다. 계산 결과에 예비조사 표시가 붙습니다.</p>' : ''}
    </div>
    ${cleaningHtml()}
    <div class="card">

      <div class="section-title">변인 점수와 신뢰도</div>
      <div class="table-wrap"><table>
        <thead><tr><th>변인</th><th>역할</th><th class="num">문항</th><th class="num">N</th><th class="num">평균</th><th class="num">표준편차</th><th class="num">최솟값</th><th class="num">최댓값</th><th class="num">왜도</th><th class="num">첨도</th><th class="num">크론바흐 α</th></tr></thead>
        <tbody>${L.scaleInfo.map(x => `<tr><td><b>${esc(x.s.name || scaleCode(x.s, x.si))}</b> <small>${esc(scaleCode(x.s, x.si))}</small></td><td>${esc(x.s.role)}</td>
          <td class="num">${x.its.length}</td><td class="num">${x.n}</td><td class="num">${f2(x.m)}</td><td class="num">${f2(x.sdv)}</td><td class="num">${f2(x.min)}</td><td class="num">${f2(x.max)}</td>
          <td class="num">${f2(x.sk)}</td><td class="num">${f2(x.ku)}</td><td class="num ${aClass(x.a)}">${fr(x.a)}</td></tr>`).join('')}</tbody>
      </table></div>
      <p class="hint">점수는 역문항을 뒤집은 뒤 문항 ${sv.scoring === 'sum' ? '합계입니다. 빠진 문항이 있는 사람은 답한 문항의 평균 × 전체 문항 수(비례 환산 합계)로 냈습니다. 방법 절에 이렇게 적으세요' : '평균입니다'}. 한 변인의 문항 중 ${+plan.dec.minPct === 100 ? '모두' : `${plan.dec.minPct}% 이상`} 답한 사람만 점수를 냅니다 ([분석] 탭의 분석 결정에서 바꿀 수 있음). 크론바흐 α는 보통 .70 이상이면 받아들일 만하다고 보고(.60~.70은 탐색 연구에서 허용하는 경우가 있음), 왜도 절댓값 2·첨도 절댓값 7 미만이면 정규분포에서 크게 벗어나지 않았다고 보는 기준이 흔히 쓰입니다.</p>
      ${L.scaleInfo.map(x => `<details class="item-detail">
        <summary>${esc(x.s.name || scaleCode(x.s, x.si))} 문항 분석 (모든 문항에 답한 ${x.nComplete}명 기준)</summary>
        <div class="table-wrap"><table class="compact">
          <thead><tr><th>변수 이름</th><th>문항</th><th class="num">평균</th><th class="num">표준편차</th><th class="num">수정된 문항-총점 r</th><th class="num">삭제 시 α</th></tr></thead>
          <tbody>${x.itemStats.map(it => `<tr><td><b>${esc(it.d.code)}</b></td><td>${esc(it.d.text)}</td><td class="num">${f2(it.m)}</td><td class="num">${f2(it.s)}</td>
            <td class="num ${isFinite(it.r) && it.r < 0.3 ? 'a-low' : ''}">${fr(it.r)}</td><td class="num ${isFinite(it.aDel) && isFinite(x.a) && it.aDel > x.a + 0.005 ? 'better' : ''}">${fr(it.aDel)}</td></tr>`).join('')}</tbody>
        </table></div>
        <p class="hint" style="padding:0 14px 12px">문항-총점 r이 .30보다 낮거나, 지웠을 때 α가 오르는 문항(빨간 숫자)은 역문항 표시가 맞는지 먼저 확인하세요. 문항을 빼는 것은 이론적 근거가 있을 때만 하고, 뺐다면 논문에 적어야 합니다.</p>
      </details>`).join('')}

      ${L.demoInfo.length ? `<div class="section-title">기본 정보</div>
      <div class="table-wrap"><table class="compact">
        <thead><tr><th>문항</th><th>구분</th><th class="num">빈도 / 값</th><th class="num">비율</th></tr></thead>
        <tbody>${L.demoInfo.map(x => x.kind === 'num'
          ? `<tr><td><b>${esc(x.dm.text)}</b></td><td>평균 (표준편차), 범위</td><td class="num">${f2(x.m)} (${f2(x.s)})</td><td class="num">${x.n ? `${x.min}~${x.max}` : '-'}</td></tr>`
          : x.freq.map(([k, c], j) => `<tr><td>${j === 0 ? `<b>${esc(x.dm.text)}</b>` : ''}</td><td>${esc(k)}</td><td class="num">${c}</td><td class="num">${(c / x.n * 100).toFixed(1)}%</td></tr>`).join('')).join('')}</tbody>
      </table></div>` : ''}

      <div class="section-title">분석용 자료 미리보기 (앞의 8명)</div>
      <div class="table-wrap"><table class="compact preview">
        <thead><tr>${preview[0].map(h => `<th>${esc(h)}</th>`).join('')}</tr></thead>
        <tbody>${preview.slice(1, 9).map(r => `<tr>${r.map(v => `<td class="num">${esc(typeof v === 'number' && !Number.isInteger(v) ? v.toFixed(2) : v)}</td>`).join('')}</tr>`).join('') || `<tr><td colspan="${pvCols}" class="empty">포함된 응답이 없습니다.</td></tr>`}</tbody>
      </table></div>
      <p class="hint">역문항은 변수 이름 뒤에 _r을 붙이고 이미 뒤집은 값을 넣었습니다. 선택형 기본 정보는 글자(예: GEN)와 번호(예: GEN_n)를 함께 넣었습니다. 이 파일을 SPSS·jamovi·R에서 그대로 불러 분석할 수 있습니다.</p>
      <div class="actions">
        <button class="btn primary" id="co-dl">분석용 자료 내려받기 (CSV)</button>
        <button class="btn" id="co-dl-items">문항 분석 결과 내려받기 (CSV)</button>
        <button class="btn" data-sv-go="codebook">코드북 내려받기 (CSV)</button>
      </div>
      <div class="actions"><button class="btn navy" data-sv-go="sample">다음: 표본 확인 →</button></div>
    </div>`;
    $('#co-result').scrollIntoView({ behavior: 'smooth', block: 'start' });
  }

  $('#co-result').addEventListener('click', e => {
    if (e.target.id === 'co-dl') download(`분석용자료_${today()}.csv`, toCsv(analysisRows()), 'text/csv');
    else if (e.target.id === 'co-dl-items') download(`문항분석_${today()}.csv`, toCsv(itemStatsRows()), 'text/csv');
    else if (e.target.dataset.svGo === 'codebook') download(`코드북_${today()}.csv`, codebookCsv(), 'text/csv');
    else if (e.target.dataset.svGo === 'analysis') switchView('analysis');
    else if (e.target.dataset.svGo === 'sample') gotoNav('sample');
  });

  // =================================================================
  // 통계 계산 엔진 (분포, 회귀, 부트스트랩)
  // =================================================================
  const ST = (() => {
    const LG = [0.99999999999980993, 676.5203681218851, -1259.1392167224028, 771.32342877765313, -176.61502916214059, 12.507343278686905, -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7];
    function lgamma(x) {
      if (x < 0.5) return Math.log(Math.PI / Math.sin(Math.PI * x)) - lgamma(1 - x);
      x -= 1; let a = LG[0]; const t = x + 7.5;
      for (let i = 1; i < 9; i++) a += LG[i] / (x + i);
      return 0.5 * Math.log(2 * Math.PI) + (x + 0.5) * Math.log(t) - t + Math.log(a);
    }
    function betacf(a, b, x) {
      const FP = 1e-300; const qab = a + b, qap = a + 1, qam = a - 1;
      let c = 1, d = 1 - qab * x / qap; if (Math.abs(d) < FP) d = FP; d = 1 / d; let h = d;
      for (let m = 1; m <= 400; m++) {
        const m2 = 2 * m;
        let aa = m * (b - m) * x / ((qam + m2) * (a + m2));
        d = 1 + aa * d; if (Math.abs(d) < FP) d = FP; c = 1 + aa / c; if (Math.abs(c) < FP) c = FP; d = 1 / d; h *= d * c;
        aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2));
        d = 1 + aa * d; if (Math.abs(d) < FP) d = FP; c = 1 + aa / c; if (Math.abs(c) < FP) c = FP; d = 1 / d;
        const del = d * c; h *= del; if (Math.abs(del - 1) < 3e-15) break;
      }
      return h;
    }
    function ibeta(x, a, b) {
      if (x <= 0) return 0; if (x >= 1) return 1;
      const bt = Math.exp(lgamma(a + b) - lgamma(a) - lgamma(b) + a * Math.log(x) + b * Math.log(1 - x));
      return x < (a + 1) / (a + b + 2) ? bt * betacf(a, b, x) / a : 1 - bt * betacf(b, a, 1 - x) / b;
    }
    function gammaP(a, x) {
      if (x <= 0) return 0;
      if (x < a + 1) {
        let ap = a, sum = 1 / a, del = sum;
        for (let n = 0; n < 1000; n++) { ap++; del *= x / ap; sum += del; if (Math.abs(del) < Math.abs(sum) * 1e-15) break; }
        return sum * Math.exp(-x + a * Math.log(x) - lgamma(a));
      }
      const FP = 1e-300; let b = x + 1 - a, c = 1 / FP, d = 1 / b, h = d;
      for (let i = 1; i < 1000; i++) {
        const an = -i * (i - a); b += 2;
        d = an * d + b; if (Math.abs(d) < FP) d = FP; c = b + an / c; if (Math.abs(c) < FP) c = FP; d = 1 / d;
        const del = d * c; h *= del; if (Math.abs(del - 1) < 1e-15) break;
      }
      return 1 - Math.exp(-x + a * Math.log(x) - lgamma(a)) * h;
    }
    const pT = (t, df) => ibeta(df / (df + t * t), df / 2, 0.5);           // 양측
    const pF = (F, d1, d2) => (!(F > 0) ? 1 : ibeta(d2 / (d2 + d1 * F), d2 / 2, d1 / 2));
    const pChi = (x, df) => (!(x > 0) ? 1 : 1 - gammaP(df / 2, x / 2));
    const Phi = z => { const g = gammaP(0.5, z * z / 2); return z >= 0 ? 0.5 + 0.5 * g : 0.5 - 0.5 * g; };
    const pZ = z => 2 * (1 - Phi(Math.abs(z)));
    function tInv(p2, df) { let lo = 0, hi = 1e3; for (let i = 0; i < 120; i++) { const m = (lo + hi) / 2; if (pT(m, df) > p2) lo = m; else hi = m; } return (lo + hi) / 2; }
    const zInv = p2 => { let lo = 0, hi = 40; for (let i = 0; i < 120; i++) { const m = (lo + hi) / 2; if (pZ(m) > p2) lo = m; else hi = m; } return (lo + hi) / 2; };
    return { pT, pF, pChi, pZ, Phi, tInv, zInv, lgamma };
  })();

  function matInv(A) {
    const n = A.length; const M = A.map((r, i) => r.concat(Array.from({ length: n }, (_, j) => (i === j ? 1 : 0))));
    for (let c = 0; c < n; c++) {
      let piv = c;
      for (let r = c + 1; r < n; r++) if (Math.abs(M[r][c]) > Math.abs(M[piv][c])) piv = r;
      if (Math.abs(M[piv][c]) < 1e-10 * Math.max(1, Math.abs(A[c][c]))) return null;
      [M[c], M[piv]] = [M[piv], M[c]];
      const d = M[c][c]; for (let j = 0; j < 2 * n; j++) M[c][j] /= d;
      for (let r = 0; r < n; r++) if (r !== c) { const f = M[r][c]; if (f) for (let j = 0; j < 2 * n; j++) M[r][j] -= f * M[c][j]; }
    }
    return M.map(r => r.slice(n));
  }

  // 최소제곱 회귀. X: 사람별 예측변인 배열(절편 제외)
  // Householder QR 분해로 계산(정규방정식보다 수치적으로 안정). 어떤 열이 앞의 열들로 거의 완전히 설명되면
  // (남은 크기 < 원래 크기의 1e-7, R lm()의 기준과 같음) 완전 공선성으로 보고 null. 그 열 번호는 olsSingular에 남김
  let olsSingular = -1;
  function ols(y, X, fast) {
    const n = y.length, k = X.length ? X[0].length : 0, p = k + 1;
    olsSingular = -1;
    if (n < p || (!fast && n === p)) return null;
    const A = Array.from({ length: p }, (_, j) => { const c = new Float64Array(n); for (let i = 0; i < n; i++) c[i] = j === 0 ? 1 : X[i][j - 1]; return c; });
    const qy = Float64Array.from(y), Rm = Array.from({ length: p }, () => new Float64Array(p));
    const n0 = A.map(c => Math.sqrt(c.reduce((s, v) => s + v * v, 0)));
    for (let j = 0; j < p; j++) {
      const c = A[j]; let nr = 0; for (let i = j; i < n; i++) nr += c[i] * c[i]; nr = Math.sqrt(nr);
      if (!(nr > 1e-7 * n0[j])) { olsSingular = j; return null; }
      const al = c[j] > 0 ? -nr : nr, v0 = c[j] - al; let vv = v0 * v0; for (let i = j + 1; i < n; i++) vv += c[i] * c[i];
      const refl = col => { let s = v0 * col[j]; for (let i = j + 1; i < n; i++) s += c[i] * col[i]; const f = 2 * s / vv; col[j] -= f * v0; for (let i = j + 1; i < n; i++) col[i] -= f * c[i]; };
      for (let q = j + 1; q < p; q++) refl(A[q]);
      refl(qy);
      Rm[j][j] = al; for (let q = j + 1; q < p; q++) Rm[j][q] = A[q][j];
      c[j] = al; // 이 열은 다시 쓰지 않음
    }
    const b = new Array(p);
    for (let j = p - 1; j >= 0; j--) { let s = qy[j]; for (let q = j + 1; q < p; q++) s -= Rm[j][q] * b[q]; b[j] = s / Rm[j][j]; }
    if (fast) return { b };
    // (X'X)^-1 = R^-1 (R^-1)'
    const Ri = Array.from({ length: p }, () => new Array(p).fill(0));
    for (let j = 0; j < p; j++) { Ri[j][j] = 1 / Rm[j][j]; for (let i = j - 1; i >= 0; i--) { let s = 0; for (let q = i + 1; q <= j; q++) s += Rm[i][q] * Ri[q][j]; Ri[i][j] = -s / Rm[i][i]; } }
    const iv = Ri.map((r, i) => Ri.map((r2, j) => { let s = 0; for (let q = Math.max(i, j); q < p; q++) s += r[q] * r2[q]; return s; }));
    const my = mean(y); let sse = 0, sst = 0; const resid = new Array(n);
    for (let i = 0; i < n; i++) { let yh = b[0]; for (let j = 0; j < k; j++) yh += b[j + 1] * X[i][j]; resid[i] = y[i] - yh; sse += resid[i] ** 2; sst += (y[i] - my) ** 2; }
    const dfe = n - p, mse = sse / dfe;
    const cov = iv.map(r => r.map(v => v * mse));
    const se = cov.map((r, i) => Math.sqrt(r[i]));
    const t = b.map((v, i) => v / se[i]);
    const tc = ST.tInv(0.05, dfe);
    const R2 = sst ? 1 - sse / sst : 0;
    const F = k ? (R2 / k) / ((1 - R2) / dfe) : NaN;
    return { n, k, b, se, t, p: t.map(v => ST.pT(Math.abs(v), dfe)), lo: b.map((v, i) => v - tc * se[i]), hi: b.map((v, i) => v + tc * se[i]),
      R2, adj: 1 - (1 - R2) * (n - 1) / dfe, F, pF: k ? ST.pF(F, k, dfe) : NaN, dfe, sse, sst, mse, cov, resid };
  }

  // 정규분포 분위수 (Acklam)
  function qnorm(p) {
    const a = [-3.969683028665376e+01, 2.209460984245205e+02, -2.759285104469687e+02, 1.383577518672690e+02, -3.066479806614716e+01, 2.506628277459239e+00];
    const b = [-5.447609879822406e+01, 1.615858368580409e+02, -1.556989798598866e+02, 6.680131188771972e+01, -1.328068155288572e+01];
    const c = [-7.784894002430293e-03, -3.223964580411365e-01, -2.400758277161838e+00, -2.549732539343734e+00, 4.374664141464968e+00, 2.938163982698783e+00];
    const d = [7.784695709041462e-03, 3.224671290700398e-01, 2.445134137142996e+00, 3.754408661907416e+00];
    const lo = 0.02425;
    if (p < lo) { const q = Math.sqrt(-2 * Math.log(p)); return (((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5]) / ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1); }
    if (p > 1 - lo) { const q = Math.sqrt(-2 * Math.log(1 - p)); return -(((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5]) / ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1); }
    const q = p - 0.5, r = q * q;
    return (((((a[0] * r + a[1]) * r + a[2]) * r + a[3]) * r + a[4]) * r + a[5]) * q / (((((b[0] * r + b[1]) * r + b[2]) * r + b[3]) * r + b[4]) * r + 1);
  }
  // Shapiro–Wilk 정규성 검정 (Royston 1992·1995 근사, n = 3~5000)
  function shapiro(x) {
    const n = x.length; if (n < 3 || n > 5000) return null;
    const s = [...x].sort((a, b) => a - b), m0 = mean(s), ss = s.reduce((q, v) => q + (v - m0) ** 2, 0);
    if (!(ss > 1e-12)) return null;
    if (n === 3) { const W = Math.min(1, (Math.SQRT1_2 * (s[2] - s[0])) ** 2 / ss); return { W, p: Math.min(1, Math.max(0, 6 / Math.PI * (Math.asin(Math.sqrt(W)) - Math.asin(Math.sqrt(0.75))))) }; }
    const m = Array.from({ length: n }, (_, i) => qnorm((i + 1 - 0.375) / (n + 0.25)));
    const mm = m.reduce((q, v) => q + v * v, 0), u = 1 / Math.sqrt(n);
    const poly = (cs, v) => cs.reduce((acc, ci, i) => acc + ci * v ** i, 0);
    const a = new Array(n);
    const an = poly([0, 0.221157, -0.147981, -2.071190, 4.434685, -2.706056], u) + m[n - 1] / Math.sqrt(mm);
    if (n > 5) {
      const an1 = poly([0, 0.042981, -0.293762, -1.752461, 5.682633, -3.582633], u) + m[n - 2] / Math.sqrt(mm);
      const phi = (mm - 2 * m[n - 1] ** 2 - 2 * m[n - 2] ** 2) / (1 - 2 * an ** 2 - 2 * an1 ** 2);
      for (let i = 2; i < n - 2; i++) a[i] = m[i] / Math.sqrt(phi);
      a[n - 1] = an; a[0] = -an; a[n - 2] = an1; a[1] = -an1;
    } else {
      const phi = (mm - 2 * m[n - 1] ** 2) / (1 - 2 * an ** 2);
      for (let i = 1; i < n - 1; i++) a[i] = m[i] / Math.sqrt(phi);
      a[n - 1] = an; a[0] = -an;
    }
    const W = Math.min(1, a.reduce((q, ai, i) => q + ai * s[i], 0) ** 2 / ss);
    let z;
    if (n <= 11) {
      const g = 0.459 * n - 2.273, y0 = Math.log(1 - W);
      if (y0 >= g) return { W, p: 0 };
      const y = -Math.log(g - y0), mu = 0.5440 - 0.39978 * n + 0.025054 * n * n - 0.0006714 * n ** 3, sg = Math.exp(1.3822 - 0.77857 * n + 0.062767 * n * n - 0.0020322 * n ** 3);
      z = (y - mu) / sg;
    } else {
      const L = Math.log(n), y = Math.log(1 - W), mu = 0.0038915 * L ** 3 - 0.083751 * L * L - 0.31082 * L - 1.5861, sg = Math.exp(0.0030302 * L * L - 0.082676 * L - 0.4803);
      z = (y - mu) / sg;
    }
    return { W, p: 1 - ST.Phi(z) };
  }

  function rng(seed) { return () => { seed = (seed + 0x6D2B79F5) | 0; let t = seed; t = Math.imul(t ^ (t >>> 15), t | 1); t ^= t + Math.imul(t ^ (t >>> 7), t | 61); return ((t ^ (t >>> 14)) >>> 0) / 4294967296; }; }
  const pct = (sorted, q) => { const i = Math.min(sorted.length - 1, Math.max(0, Math.floor(q * sorted.length))); return sorted[i]; };
  const tick = () => new Promise(r => setTimeout(r, 0));

  // 부트스트랩: stat(idx)는 재표집된 사람 번호로 효과 배열을 돌려줌
  async function bootstrap(n, B, stat, onProg) {
    const r = rng(20261019); const out = [];
    for (let s = 0; s < B; s++) {
      const idx = new Array(n); for (let i = 0; i < n; i++) idx[i] = Math.floor(r() * n);
      const v = stat(idx); if (v) out.push(v);
      if (s % 250 === 249) { onProg && onProg((s + 1) / B); await tick(); }
    }
    if (!out.length) return null;
    const res = out[0].map((_, j) => { const col = out.map(v => v[j]).filter(isFinite).sort((a, b) => a - b); return { lo: pct(col, 0.025), hi: pct(col, 0.975), se: sd(col) }; });
    res.B = B; res.ok = out.length;
    return res;
  }
  // 계산이 안 된 재표집(변인이 완전히 겹친 표본 등)을 숨기지 않고 알림
  function bootNote(R, bs) {
    if (!bs) return;
    const bad = bs.B - bs.ok;
    R.notes.push(bad ? `${bad / bs.B > 0.01 ? '⚠ ' : ''}부트스트랩 ${bs.B.toLocaleString()}번 중 ${bad.toLocaleString()}번은 재표집된 자료에서 계산이 되지 않아(변인이 완전히 겹침 등) 빼고, ${bs.ok.toLocaleString()}번으로 신뢰구간을 구했습니다.${bad / bs.B > 0.01 ? ' 1%를 넘으므로 표본이 작거나 응답이 한쪽에 몰린 변인이 없는지 확인하세요.' : ''}` : `부트스트랩 ${bs.B.toLocaleString()}번이 모두 계산되었습니다.`);
  }

  function ranks(a) {
    const idx = a.map((v, i) => [v, i]).sort((x, y) => x[0] - y[0]); const r = new Array(a.length);
    for (let i = 0; i < idx.length;) { let j = i; while (j + 1 < idx.length && idx[j + 1][0] === idx[i][0]) j++; const avg = (i + j) / 2 + 1; for (let k = i; k <= j; k++) r[idx[k][1]] = avg; i = j + 1; }
    return r;
  }

  // ---------- 베이즈 인자 (JASP·R BayesFactor의 기본 사전분포와 같음) ----------
  // 모두 로그 BF10(효과가 있다 ÷ 없다)을 돌려줌. 적분은 로그 피적분함수를 심프슨 공식으로 (최댓값을 빼서 넘침 방지)
  function logIntegral(lf, a, b, m) {
    const h = (b - a) / m, v = new Float64Array(m + 1); let mx = -Infinity;
    for (let i = 0; i <= m; i++) { const x = lf(a + i * h); v[i] = isNaN(x) ? -Infinity : x; if (v[i] > mx) mx = v[i]; }
    if (!isFinite(mx)) return -Infinity;
    let s = 0; for (let i = 0; i <= m; i++) s += (i === 0 || i === m ? 1 : i % 2 ? 4 : 2) * Math.exp(v[i] - mx);
    return mx + Math.log(s * h / 3);
  }
  const BAYES = {
    // t검정: 효과크기 δ ~ Cauchy(0, r), r = √2/2 (Rouder et al., 2009). N = n(단일·대응) 또는 n1n2/(n1+n2)(독립), nu = 자유도
    t(t, N, nu, r = Math.SQRT1_2) {
      const l0 = -(nu + 1) / 2 * Math.log1p(t * t / nu);
      const lf = u => { const g = Math.exp(u), a = 1 + N * g * r * r; return -0.5 * Math.log(a) - (nu + 1) / 2 * Math.log1p(t * t / (a * nu)) - 0.5 * Math.log(2 * Math.PI) - 0.5 * u - 1 / (2 * g); };
      return logIntegral(lf, -30, 30, 6000) - l0;
    },
    // 피어슨 상관: ρ ~ 균등(-1, 1) (stretched beta, κ = 1; Ly, Verhagen, & Wagenmakers, 2016). JASP 기본값
    r(r, n) {
      if (n < 3 || !isFinite(r)) return NaN;
      const a = (n - 1) / 2, c = (n + 2) / 2, z = Math.min(r * r, 1 - 1e-12), lg = ST.lgamma;
      // 2F1(a, a; c; z) = Γ(c)/(Γ(a)Γ(c−a)) ∫ t^(a−1) (1−t)^(c−a−1) (1−zt)^(−a) dt, t = 1 − s²
      const lf = sv => { const t = 1 - sv * sv; return Math.log(2) + 2 * Math.log(sv) + (a - 1 ? (a - 1) * Math.log(t) : 0) - a * Math.log(1 - z * t); };
      const l2f1 = lg(c) - lg(a) - lg(c - a) + logIntegral(lf, 0, 1, 6000);
      return Math.log(Math.sqrt(Math.PI) / 2) + lg((n + 1) / 2) - lg((n + 2) / 2) + l2f1;
    },
    // 회귀 모형 전체 (절편만 있는 모형과 비교): g ~ InvGamma(1/2, r²N/2), r = √2/4 (Zellner–Siow; Liang et al., 2008; Rouder & Morey, 2012)
    reg(R2, N, p, r = Math.SQRT2 / 4) {
      if (N - p - 1 < 1 || !isFinite(R2)) return NaN;
      if (p === 0) return 0;
      const sc = r * r * N / 2, lc = 0.5 * Math.log(sc) - 0.5 * Math.log(Math.PI);
      const lf = u => { const g = Math.exp(u); return 0.5 * ((N - p - 1) * Math.log1p(g) - (N - 1) * Math.log1p(g * (1 - R2))) + lc - 0.5 * u - sc / g; };
      return logIntegral(lf, -40, 80, 12000);
    },
  };
  // BF 표기: 1000 이상·0.001 이하면 지수 표기
  const BFF = lb => { if (!isFinite(lb)) return '-'; const v = Math.exp(lb); if (v >= 1000 || v < 0.001) { const e = Math.floor(lb / Math.LN10), m = Math.exp(lb - e * Math.LN10); return `${m.toFixed(2)} × 10${String(e).replace(/./g, c => '⁰¹²³⁴⁵⁶⁷⁸⁹⁻'['0123456789-'.indexOf(c)])}`; } return v >= 100 ? v.toFixed(0) : v >= 10 ? v.toFixed(1) : v.toFixed(2); };
  // 증거의 강도 (Lee & Wagenmakers, 2013이 다듬은 Jeffreys 기준)
  const bfWord = lb => { if (!isFinite(lb)) return '-'; const v = Math.round(Math.exp(Math.abs(lb)) * 100) / 100, side = lb >= 0 ? '효과 있음' : '효과 없음'; if (v < 3) return '판단 보류 (약한 증거)'; return `${side}을 ${v < 10 ? '중간 정도로' : v < 30 ? '강하게' : v < 100 ? '매우 강하게' : '극단적으로'} 지지`; };
  // 결과에 베이즈 인자 표를 붙이고, 효과에 logBF를 적어 둠
  function addBayes(R, rows, prior) {
    if (!rows.length) return;
    R.tables.push({ title: '베이즈 인자 (JASP 기본 사전분포)', head: ['효과', 'BF₁₀', 'BF₀₁', '해석'], num: [1, 2], rows: rows.map(([lab, lb]) => [lab, BFF(lb), BFF(-lb), bfWord(lb)]), foot: `BF₁₀은 이 자료가 "효과가 없다"보다 "효과가 있다"에서 몇 배 더 잘 나오는지입니다. BF₀₁ = 1/BF₁₀. 사전분포: ${prior}.` });
  }

  // ---------- 숫자 표기 (APA) ----------
  const F2 = (x, d = 2) => (x == null || !isFinite(x) ? '-' : (Math.abs(x) < 0.5 * 10 ** -d ? 0 : x).toFixed(d));
  const NZ = (x, d = 2) => F2(x, d).replace(/^(-?)0\./, '$1.');           // 1을 넘지 않는 값: 앞 0 생략
  const PV = p => (!isFinite(p) ? '-' : p < 0.001 ? '< .001' : NZ(p, 3));
  const PS = p => (!isFinite(p) ? 'p = -' : p < 0.001 ? 'p < .001' : `p = ${NZ(p, 3)}`);
  const STAR = p => (!isFinite(p) ? '' : p < 0.001 ? '***' : p < 0.01 ? '**' : p < 0.05 ? '*' : '');
  const CI = (lo, hi, d = 2) => `[${F2(lo, d)}, ${F2(hi, d)}]`;

  // =================================================================

// ##### 3. 분석 방법 (RUN): 상관·t·분산분석·회귀·로지스틱·매개·조절 (stats-ladder.html 4766~5819줄) #####
  // =================================================================
  // ⑤ 분석
  // =================================================================
  let dataVersion = 0;
  let analyses = store.get('ladder.analyses', []);   // 집계 결과만 저장 (원자료 없음)
  const saveAnalyses = () => store.set('ladder.analyses', analyses);

  // 응답 취합 결과에서 분석에 쓸 변인 목록
  function dataVars() {
    if (!lastScored) return [];
    const inc = lastScored.inc, out = [];
    sv.scales.forEach((s, si) => out.push({ key: 'S' + si, name: s.name || scaleCode(s, si), code: scaleCode(s, si), type: 'num', role: s.role, vals: inc.map(p => (p.score[si] == null ? null : p.score[si])) }));
    sv.demos.forEach((d, di) => {
      if (d.type === 'text' || !norm(d.text)) return;
      const raw = inc.map(p => norm(p.items[`d${di}`]) || null);
      if (d.type === 'number') out.push({ key: 'D' + di, name: d.text, code: demoCode(d, di), type: 'num', role: '기본 정보', vals: raw.map(v => (v == null || !isFinite(+v) ? null : +v)) });
      else {
        const seen = (d.options || []).filter(o => raw.includes(o)).concat([...new Set(raw.filter(v => v != null && !(d.options || []).includes(v)))]);
        out.push({ key: 'D' + di, name: d.text, code: demoCode(d, di), type: 'cat', role: '기본 정보', levels: seen, vals: raw });
      }
    });
    return out;
  }
  const varByKey = k => dataVars().find(v => v.key === k);

  const METHODS = {
    pearson: { name: '피어슨 상관분석', group: '관계', slots: [{ k: 'vars', label: '변인 (2개 이상)', type: 'num', multi: true, min: 2 }] },
    spearman: { name: '스피어만 순위상관분석', group: '관계', slots: [{ k: 'vars', label: '변인 (2개 이상)', type: 'num', multi: true, min: 2 }] },
    partial: { name: '편상관분석', group: '관계', slots: [{ k: 'x', label: '변인 1', type: 'num' }, { k: 'y', label: '변인 2', type: 'num' }, { k: 'covs', label: '통제할 변인', type: 'any', multi: true, min: 1 }] },
    pointbiserial: { name: '점이연 상관분석', group: '관계', slots: [{ k: 'g', label: '두 가지 분류', type: 'cat2' }, { k: 'y', label: '점수', type: 'num' }] },
    chisq: { name: '카이제곱 독립성 검정', group: '관계', slots: [{ k: 'a', label: '분류 1 (행)', type: 'cat' }, { k: 'b', label: '분류 2 (열)', type: 'cat' }] },
    onesample: { name: '단일표본 t검정', group: '집단 차이', slots: [{ k: 'y', label: '점수', type: 'num' }], opts: ['testValue'] },
    ind_t: { name: '독립표본 t검정', group: '집단 차이', slots: [{ k: 'g', label: '집단 (두 가지)', type: 'cat2' }, { k: 'ys', label: '비교할 점수', type: 'num', multi: true, min: 1 }] },
    paired_t: { name: '대응표본 t검정', group: '집단 차이', slots: [{ k: 'a', label: '점수 1 (예: 사전)', type: 'num' }, { k: 'b', label: '점수 2 (예: 사후)', type: 'num' }] },
    oneway: { name: '일원분산분석', group: '집단 차이', slots: [{ k: 'g', label: '집단', type: 'cat' }, { k: 'ys', label: '비교할 점수', type: 'num', multi: true, min: 1 }] },
    factorial: { name: '이원분산분석', group: '집단 차이', slots: [{ k: 'a', label: '집단 기준 1', type: 'cat' }, { k: 'b', label: '집단 기준 2', type: 'cat' }, { k: 'y', label: '비교할 점수', type: 'num' }] },
    ancova: { name: '공분산분석', group: '집단 차이', slots: [{ k: 'g', label: '집단', type: 'cat' }, { k: 'y', label: '비교할 점수', type: 'num' }, { k: 'covs', label: '공변인 (통제할 점수)', type: 'num', multi: true, min: 1 }] },
    regression: { name: '회귀분석 (단순·다중)', group: '예측', slots: [{ k: 'y', label: '종속변인', type: 'num' }, { k: 'xs', label: '예측변인', type: 'any', multi: true, min: 1 }] },
    hier: { name: '위계적 회귀분석', group: '예측', slots: [{ k: 'y', label: '종속변인', type: 'num' }, { k: 'b1', label: '1단계 (주로 통제변인)', type: 'any', multi: true, min: 1 }, { k: 'b2', label: '2단계', type: 'any', multi: true, min: 1 }, { k: 'b3', label: '3단계 (선택)', type: 'any', multi: true, min: 0 }] },
    logistic: { name: '이항 로지스틱 회귀분석', group: '예측', slots: [{ k: 'y', label: '결과 (두 가지 분류)', type: 'cat2' }, { k: 'xs', label: '예측변인', type: 'any', multi: true, min: 1 }] },
    med_parallel: { name: '매개분석 (단순·병렬, PROCESS Model 4)', group: '매개·조절', slots: [{ k: 'x', label: '독립변인 (X)', type: 'numbin' }, { k: 'ms', label: '매개변인 (M, 여러 개면 병렬)', type: 'num', multi: true, min: 1 }, { k: 'y', label: '종속변인 (Y)', type: 'num' }, { k: 'covs', label: '통제변인 (선택)', type: 'any', multi: true, min: 0 }], opts: ['boot'] },
    med_serial: { name: '순차매개분석 (PROCESS Model 6)', group: '매개·조절', slots: [{ k: 'x', label: '독립변인 (X)', type: 'numbin' }, { k: 'm1', label: '매개변인 1 (M1, 먼저)', type: 'num' }, { k: 'm2', label: '매개변인 2 (M2, 나중)', type: 'num' }, { k: 'y', label: '종속변인 (Y)', type: 'num' }, { k: 'covs', label: '통제변인 (선택)', type: 'any', multi: true, min: 0 }], opts: ['boot'] },
    moderation: { name: '조절효과 분석 (PROCESS Model 1)', group: '매개·조절', slots: [{ k: 'x', label: '독립변인 (X)', type: 'numbin' }, { k: 'w', label: '조절변인 (W)', type: 'numbin' }, { k: 'y', label: '종속변인 (Y)', type: 'num' }, { k: 'covs', label: '통제변인 (선택)', type: 'any', multi: true, min: 0 }], opts: ['center'] },
    modmed7: { name: '조절된 매개분석 · 앞쪽 경로 (PROCESS Model 7)', group: '매개·조절', slots: [{ k: 'x', label: '독립변인 (X)', type: 'numbin' }, { k: 'm', label: '매개변인 (M)', type: 'num' }, { k: 'w', label: '조절변인 (W, X→M을 조절)', type: 'numbin' }, { k: 'y', label: '종속변인 (Y)', type: 'num' }, { k: 'covs', label: '통제변인 (선택)', type: 'any', multi: true, min: 0 }], opts: ['boot', 'center'] },
    modmed14: { name: '조절된 매개분석 · 뒤쪽 경로 (PROCESS Model 14)', group: '매개·조절', slots: [{ k: 'x', label: '독립변인 (X)', type: 'numbin' }, { k: 'm', label: '매개변인 (M)', type: 'num' }, { k: 'w', label: '조절변인 (W, M→Y를 조절)', type: 'numbin' }, { k: 'y', label: '종속변인 (Y)', type: 'num' }, { k: 'covs', label: '통제변인 (선택)', type: 'any', multi: true, min: 0 }], opts: ['boot', 'center'] },
    sem: { name: '구조방정식 (SEM · 잠재변인 경로모형)', group: '구조방정식 (R lavaan)', slots: [{ k: 'xs', label: '원인 (X)', type: 'num', multi: true, min: 1 }, { k: 'ms', label: '매개변인 (M, 선택 · 여러 개면 나란히)', type: 'num', multi: true, min: 0 }, { k: 'y', label: '결과 (Y)', type: 'num' }, { k: 'covs', label: '통제변인 (선택)', type: 'numbin', multi: true, min: 0 }], opts: ['semBoot'] },
    sem_serial: { name: '구조방정식 (SEM · 순차 경로)', group: '구조방정식 (R lavaan)', slots: [{ k: 'x', label: '원인 (X)', type: 'num' }, { k: 'm1', label: '매개변인 1 (M1, 먼저)', type: 'num' }, { k: 'm2', label: '매개변인 2 (M2, 나중)', type: 'num' }, { k: 'y', label: '결과 (Y)', type: 'num' }, { k: 'covs', label: '통제변인 (선택)', type: 'numbin', multi: true, min: 0 }], opts: ['semBoot'] }
  };
  // 사다리 도착점 → 계산 방법 (null: 이 사이트에서 계산하지 않음)
  const LADDER_TO_METHOD = {
    r_onesample_t: 'onesample', r_ind_t: 'ind_t', r_paired_t: 'paired_t', r_ancova: 'ancova', r_oneway: 'oneway', r_rel_anova: 'oneway',
    r_factorial_anova: 'factorial', r_chisq: 'chisq', r_logistic: 'logistic', r_pearson: 'pearson', r_spearman: 'spearman', r_partial: 'partial',
    r_pointbiserial: 'pointbiserial', r_simple_reg: 'regression', r_multiple_reg: 'regression', r_hier_reg: 'hier', r_med_simple: 'med_parallel',
    r_med_parallel: 'med_parallel', r_med_serial: 'med_serial', r_moderation: 'moderation', r_modmed_a: 'modmed7', r_modmed_b: 'modmed14',
    r_rm_anova: 'rm_anova', r_mixed_anova: 'mixed_anova', r_rm_factorial: null, r_mcnemar: 'mcnemar', r_cochran: 'cochran', r_sem: 'sem', r_sem_serial: 'sem_serial', r_multilevel: null, r_multinom: null, r_ordinal: null
  };

  const ladderResultId = () => { const w = walk(path); return w.cur.type === 'result' ? w.cur.id : null; };
  let anSel = store.get('ladder.anSel', null);   // {method, roles:{}, opts:{}}
  const saveAnSel = () => store.set('ladder.anSel', anSel);

  function autoRoles(method) {
    const vs = dataVars(); const roles = {};
    const byRole = r => vs.filter(v => v.type === 'num' && v.role === r).map(v => v.key);
    const nums = vs.filter(v => v.type === 'num' && v.key[0] === 'S').map(v => v.key);
    const cats = vs.filter(v => v.type === 'cat'), cat2 = cats.filter(v => v.levels.length === 2);
    const X = byRole('독립')[0], M = byRole('매개'), Y = byRole('종속')[0], W = byRole('조절')[0], C = [...new Set(byRole('통제').concat(typeof md !== 'undefined' && md ? ctlKeys(vs) : []))];
    for (const s of METHODS[method].slots) {
      let v;
      if (s.k === 'x') v = X ?? nums[0];
      else if (s.k === 'y' && s.type === 'num') v = Y ?? nums[nums.length - 1];
      else if (s.k === 'y' && s.type === 'cat2') v = cat2[0]?.key;
      else if (s.k === 'ms') v = M.length ? M : nums.slice(1, 2);
      else if (s.k === 'm' || s.k === 'm1') v = M[0];
      else if (s.k === 'm2') v = M[1];
      else if (s.k === 'w') v = W;
      else if (s.k === 'covs') v = C;
      else if (s.k === 'vars') v = nums;
      else if (s.k === 'xs' || s.k === 'b2') v = byRole('독립').concat(M, W ? [W] : []);
      else if (s.k === 'b1') v = C.length ? C : vs.filter(x => x.key[0] === 'D').map(x => x.key);
      else if (s.k === 'ys') v = Y ? [Y] : nums;
      else if (s.k === 'g') v = (s.type === 'cat2' ? cat2[0] : (cats.find(c => c.levels.length > 2) || cats[0]))?.key;
      else if (s.k === 'a' && s.type === 'cat') v = cats[0]?.key;
      else if (s.k === 'b' && s.type === 'cat') v = cats[1]?.key;
      else if (s.k === 'a') v = nums[0];
      else if (s.k === 'b') v = nums[1];
      roles[s.k] = s.multi ? (v || []).filter(Boolean) : (v || '');
    }
    return roles;
  }

  function ensureSel() {
    const lm = LADDER_TO_METHOD[ladderResultId()];
    if (!anSel || !METHODS[anSel.method]) anSel = { method: lm || 'pearson', roles: {}, opts: { boot: 5000, center: true, testValue: 3, ...(anSel?.opts || {}) } };
    const valid = new Set(dataVars().map(v => v.key));
    const r = anSel.roles || {};
    const bad = METHODS[anSel.method].slots.some(s => s.multi ? (r[s.k] || []).some(k => !valid.has(k)) : (r[s.k] && !valid.has(r[s.k])));
    if (!Object.keys(r).length || bad) anSel.roles = autoRoles(anSel.method);
    ensureSelLite();
  }

  function renderAnalysis() {
    const box = $('#an-main');
    const lid = ladderResultId(), lm = lid ? LADDER_TO_METHOD[lid] : undefined;
    const ladderNote = lid ? (lm ? `가설로 정한 기법: <b>${esc(RULES.results[lid].name)}</b> → 아래에서 <b>${esc(METHODS[lm].name)}</b>으로 계산합니다.`
      : `가설로 정한 기법 <b>${esc(RULES.results[lid].name)}</b>은 이 사이트에서 아직 계산하지 않습니다. jamovi(무료)나 SPSS에서 계산하고, 결과 해석 메뉴에 그 결과를 직접 적어 쓸 수 있습니다.`) : '①에서 가설을 정하면 그 기법이 먼저 선택됩니다.';
    if (!lastScored) {
      box.innerHTML = `<div class="card">
        <h3 class="card-title">분석할 자료가 아직 없습니다</h3>
        <p class="hint">[응답 취합] 메뉴에서 응답을 넣고 [점수 만들기]를 누르면 여기서 계산할 수 있습니다. 응답 원자료는 저장하지 않아서, 페이지를 새로 열었다면 다시 넣어야 합니다.</p>
        <p class="hint">${ladderNote}</p>
        <div class="actions"><button class="btn primary" data-an-act="demo">예시 자료로 해 보기</button><button class="btn" data-an-act="to-collect">응답 취합으로</button></div>
      </div>`;
      renderAnalyses(); return;
    }
    ensureSel();
    const vs = dataVars(), M = METHODS[anSel.method];
    const fits = (v, t) => t === 'any' ? true : t === 'num' ? v.type === 'num' : t === 'cat' ? v.type === 'cat' : t === 'cat2' ? v.type === 'cat' && v.levels.length === 2 : t === 'numbin' ? v.type === 'num' || (v.type === 'cat' && v.levels.length === 2) : false;
    const vlabel = v => `${v.name}${v.type === 'cat' ? ` (${v.levels.length}가지)` : ''}`;
    const groups = [...new Set(Object.values(METHODS).map(m => m.group))];
    const slotHtml = s => {
      const cand = vs.filter(v => fits(v, s.type));
      if (s.multi) {
        const cur = anSel.roles[s.k] || [];
        return `<div class="slot"><div class="slot-l">${esc(s.label)}</div><div class="chips">${cand.map(v => `<label class="chip ${cur.includes(v.key) ? 'on' : ''}"><input type="checkbox" data-slot="${s.k}" value="${v.key}" ${cur.includes(v.key) ? 'checked' : ''}>${esc(vlabel(v))}</label>`).join('') || '<span class="hint">알맞은 변인이 없습니다.</span>'}</div></div>`;
      }
      return `<label class="f slot">${esc(s.label)}<select data-slot="${s.k}"><option value="">(고르세요)</option>${cand.map(v => `<option value="${v.key}" ${anSel.roles[s.k] === v.key ? 'selected' : ''}>${esc(vlabel(v))}</option>`).join('')}</select></label>`;
    };
    const o = anSel.opts;
    box.innerHTML = `${prepCard('analysis')}${planItems().some(p => LADDER_TO_METHOD[p.result]) ? `<div class="card"><h3 class="card-title">확정한 가설 ${planItems().length}개</h3><p class="hint">미리 정한 통계 방법과 분석 결정 그대로 가설의 분석을 모두 계산합니다. 하나씩 하려면 아래에서 방법과 변인을 고르세요.</p>${runAllBtn()}</div>` : ''}${runAllHtml()}<div class="card">
      <div class="data-line"><span class="st auto">자료</span> 분석에 포함된 응답 ${lastScored.inc.length}명 · 변인 ${vs.length}개${lastScored.sample ? ' · <b>예시(가짜) 자료</b>' : ''}</div>
      <p class="hint">${ladderNote}</p>
      <label class="f" style="max-width:520px;margin-top:12px">분석 방법<select id="an-method">${groups.map(g => `<optgroup label="${esc(g)}">${Object.entries(METHODS).filter(([, m]) => m.group === g).map(([k, m]) => `<option value="${k}" ${k === anSel.method ? 'selected' : ''}>${esc(m.name)}${k === lm ? ' ← 가설' : ''}</option>`).join('')}</optgroup>`).join('')}</select></label>
      <div class="slots">${M.slots.map(slotHtml).join('')}</div>
      ${(M.opts || []).includes('testValue') ? `<div class="opts-row">
        ${M.opts.includes('testValue') ? `<label class="f">비교할 기준값<input data-opt="testValue" inputmode="decimal" value="${esc(o.testValue)}"></label>` : ''}
      </div>` : ''}
      ${decisionsHtml(anSel.method)}
      <div class="actions"><button class="btn primary" data-an-act="run">계산하기</button><button class="btn" data-an-act="auto">역할대로 다시 채우기</button><span class="hint" id="an-prog"></span></div>
    </div>`;
    renderAnalyses();
  }

  $('#app').addEventListener('change', e => {
    const t = e.target;
    if (t.id === 'an-method') { anSel.method = t.value; anSel.roles = autoRoles(t.value); saveAnSel(); renderAnalysis(); return; }
    if (t.dataset.slot) {
      const s = METHODS[anSel.method].slots.find(x => x.k === t.dataset.slot);
      if (s.multi) { const cur = new Set(anSel.roles[s.k] || []); t.checked ? cur.add(t.value) : cur.delete(t.value); anSel.roles[s.k] = [...cur]; t.closest('.chip').classList.toggle('on', t.checked); }
      else anSel.roles[s.k] = t.value;
      saveAnSel(); return;
    }
    if (t.dataset.opt) { anSel.opts[t.dataset.opt] = t.type === 'checkbox' ? t.checked : t.value; saveAnSel(); }
    if (t.dataset.dec) { changeDecision(t.dataset.dec, t.value); renderAnalysis(); }
  });
  $('#app').addEventListener('input', e => {
    const t = e.target;
    if (t.dataset.decWhy != null) { plan.log[+t.dataset.decWhy].reason = t.value; savePlan(); }
  });
  $('#app').addEventListener('click', async e => {
    const b = e.target.closest('[data-an-act]'); if (!b) return;
    const act = b.dataset.anAct;
    if (act === 'to-collect') switchView('collect');
    else if (act === 'demo') {
      if (!designItems().length) sv = exampleSurvey(), saveSv(), renderSurvey();
      const t = sampleCsv(300); $('#co-paste').value = t; loadRaw(t, '예시 응답(가짜 자료)'); runScoring(); lastScored.sample = true;
      switchView('analysis'); toast('예시 자료 300명으로 점수를 만들었습니다.');
    }
    else if (act === 'auto') { anSel.roles = autoRoles(anSel.method); saveAnSel(); renderAnalysis(); }
    else if (act === 'run') {
      b.disabled = true;
      try { await runAnalysis(); } catch (err) { $('#an-prog').innerHTML = `<span class="a-low">${esc(err.message || String(err))}</span>`; }
      b.disabled = false;
    }
    else if (act === 'del') { analyses = analyses.filter(a => a.id !== b.dataset.id); saveAnalyses(); renderAnalyses(); renderInterp(); }
    else if (act === 'csv') { const a = analyses.find(x => x.id === b.dataset.id); download(`분석결과_${a.title.replace(/[^\w가-힣]+/g, '_').slice(0, 40)}.csv`, toCsv(analysisCsvRows(a)), 'text/csv'); }
    else if (act === 'to-interp') switchView('interp');
    else if (act === 'copy-sent') { const t = (analyses.find(x => x.id === b.dataset.id) || {}).sent || ''; try { navigator.clipboard.writeText(t).then(() => toast('문장을 복사했습니다.'), () => showSheet('논문에 쓸 문장', t)); } catch (err) { showSheet('논문에 쓸 문장', t); } }
    else if (act === 'lock-plan') { plan.locked = new Date().toLocaleString('ko-KR'); savePlan(); renderAnalysis(); addSys('analysis', '분석 결정을 확정했습니다.'); toast('분석 결정을 확정했습니다. 이후 바꾸면 기록에 남습니다.'); }
    else if (act === 'plan-dl') download('analysis_plan.md', planText());
    else if (act === 'r-data') download('analysis_data.csv', toCsv(analysisRows()), 'text/csv');
    else if (act === 'r-code') download('analysis.R', rScript(analyses.slice().reverse()));
    else if (act === 'run-all') { b.disabled = true; try { await runAllHyps(); } finally { b.disabled = false; } }
    else if (act === 'goto-res') { rtabManual = false; setRtab('results'); openRight(); $(`#res-${b.dataset.id}`)?.scrollIntoView({ block: 'start' }); }
    else if (act === 'r-copy' || act === 'r-one') {
      const a = analyses.find(x => x.id === b.dataset.id); if (!a) return; const txt = rScript([a]);
      if (act === 'r-one') download(`analysis_${a.method}.R`, txt);
      else { const fb = () => download(`analysis_${a.method}.R`, txt); try { navigator.clipboard.writeText(txt).then(() => toast('R 코드를 복사했습니다. RStudio에 붙여 넣으세요.'), fb); } catch (err) { fb(); } }
    }
    else if (act === 'spss-code') download('analysis.sps', '\ufeff' + spssSyntax(analyses.slice().reverse()));
    else if (act === 'rcompare') {
      const a = analyses.find(x => x.id === b.dataset.id), txt = $(`[data-rpaste="${a.id}"]`).value;
      const parsed = parseROut(txt), rows = parsed[a.id] || parsed._ || (Object.keys(parsed).length === 1 ? Object.values(parsed)[0] : null);
      if (!rows) { toast('R 출력에서 이 분석의 EFFECT 줄을 찾지 못했습니다.'); return; }
      a.rcheck = { at: new Date().toLocaleString('ko-KR'), rows }; saveAnalyses(); renderAnalyses();
      $(`#res-${a.id}`)?.scrollIntoView({ block: 'start' });
    }
  });

  // ---------- 계산 실행 ----------
  function prep(keysByRole) {
    // 쓰는 변인 모두 응답한 사람만 (목록별 제외)
    const all = Object.values(keysByRole).flat().filter(Boolean);
    const uniq = [...new Set(all)];
    if (uniq.length !== all.length) throw new Error('같은 변인을 두 역할에 넣었습니다. 역할마다 다른 변인을 골라 주세요.');
    const V = Object.fromEntries(uniq.map(k => [k, varByKey(k)]));
    const n0 = lastScored.inc.length;
    const keep = []; for (let i = 0; i < n0; i++) if (uniq.every(k => V[k].vals[i] != null)) keep.push(i);
    const col = k => keep.map(i => V[k].vals[i]);
    return { V, keep, n: keep.length, dropped: n0 - keep.length, col };
  }
  // 예측변인 열 만들기: 점수는 그대로, 분류는 더미(첫 범주 기준)
  // 회귀가 안 될 때 이유: 사람 수가 모자람 / 어떤 예측변인이 다른 예측변인들로 완전히 설명됨
  const collinMsg = (names, n) => n <= names.length + 1 ? `분석한 사람(${n}명)이 예측변인 수(${names.length}개)보다 충분히 많아야 합니다.` : `예측변인끼리 완전히 겹쳐(완전 공선성) 계산할 수 없습니다.${olsSingular > 0 && names[olsSingular - 1] ? ` '${names[olsSingular - 1]}'${J(names[olsSingular - 1], '이/가').slice(names[olsSingular - 1].length)} 앞의 예측변인들로 완전히 설명됩니다. 같은 정보를 담은 변인(예: 합계와 그 하위 점수)을 함께 넣지 않았는지 확인하세요.` : ''}`;
  function designCols(P, keys) {
    const cols = [], names = [], src = [];
    keys.forEach(k => {
      const v = P.V[k];
      if (v.type === 'num') { cols.push(P.col(k)); names.push(v.name); src.push(k); }
      else {
        const lv = v.levels.filter(l => P.col(k).includes(l));
        lv.slice(1).forEach(l => { cols.push(P.col(k).map(x => (x === l ? 1 : 0))); names.push(`${v.name}: ${l} (기준 ${lv[0]})`); src.push(k); });
      }
    });
    return { cols, names, src };
  }
  const binCode = (P, k) => { const v = P.V[k]; if (v.type === 'num') return { x: P.col(k), note: '' }; const lv = v.levels.filter(l => P.col(k).includes(l)); return { x: P.col(k).map(x => (x === lv[1] ? 1 : 0)), note: `${v.name}: 0 = ${lv[0]}, 1 = ${lv[1]}`, bin: true }; };
  const rowsOf = cols => cols[0] ? cols[0].map((_, i) => cols.map(c => c[i])) : [];
  const center = a => { const m = mean(a); return a.map(v => v - m); };
  const need = (cond, msg) => { if (!cond) throw new Error(msg); };

  function newResult(method, title) { return { id: 'A' + Date.now().toString(36) + Math.floor(Math.random() * 1e4), method, title, at: today(), dv: dataVersion, sample: !!lastScored.sample, n: 0, tables: [], notes: [], effects: [] }; }
  const eff = (R, o) => { const e = { id: `${R.id}.${R.effects.length}`, kind: 'signed', ...o }; R.effects.push(e); return e; };
  const sigOf = e => e.ciBased ? !(e.lo <= 0 && e.hi >= 0) : e.p < 0.05;

  async function runAnalysis(quiet) {
    ensureSel();
    const M = METHODS[anSel.method], r = anSel.roles;
    for (const s of M.slots) {
      if (s.multi) need((r[s.k] || []).length >= (s.min || 0), `'${s.label}'에 변인을 ${s.min}개 이상 골라 주세요.`);
      else need(r[s.k], `'${s.label}'을(를) 골라 주세요.`);
    }
    const prog = $('#an-prog'); prog.textContent = '계산 중…'; await tick();
    const spec = specOf(anSel.method, r, anSel.opts);
    const R = await RUN[anSel.method](r, anSel.opts, f => { prog.textContent = `부트스트랩 ${Math.round(f * 100)}%`; });
    R.spec = spec; R.decisions = DECISIONS.filter(d => applicable(d, anSel.method)).map(d => `${d.q} → ${decLabel(d, d.get())}`);
    R.conf = confOf(anSel.method, r);
    plan.seen = true; savePlan();
    prog.textContent = '보고 체크리스트 점검 중…'; await tick();
    buildChecks(R, anSel.roles);
    prog.textContent = '';
    analyses.unshift(R); saveAnalyses();
    if (quiet) return R;
    renderAnalyses(); renderInterp();
    addSys('analysis', `계산했습니다: ${R.title} (N = ${R.n}). 결과는 오른쪽 [결과] 탭에 있습니다.`);
    rtabManual = false; setRtab('results'); openRight();
    $(`#res-${R.id}`)?.scrollIntoView({ block: 'start' });
  }

  function regTable(fit, names, ysd, xsds, title) {
    return { title, head: ['변인', 'B', 'SE', 'β', 't', 'p', '95% CI', 'VIF'], num: [1, 2, 3, 4, 5, 6, 7],
      rows: [['(상수)', F2(fit.b[0]), F2(fit.se[0]), '', F2(fit.t[0]), PV(fit.p[0]), CI(fit.lo[0], fit.hi[0]), '']].concat(names.map((nm, j) => [nm, F2(fit.b[j + 1]), F2(fit.se[j + 1]), NZ(fit.b[j + 1] * xsds[j] / ysd), F2(fit.t[j + 1]), PV(fit.p[j + 1]), CI(fit.lo[j + 1], fit.hi[j + 1]), fit.vif ? F2(fit.vif[j]) : ''])) };
  }
  function vifs(cols) {
    if (cols.length < 2) return cols.map(() => 1);
    return cols.map((c, j) => { const others = cols.filter((_, k) => k !== j); const f = ols(c, rowsOf(others)); return f ? 1 / (1 - f.R2) : NaN; });
  }
  const fitLine = (fit) => `R² = ${NZ(fit.R2)}, 수정된 R² = ${NZ(fit.adj)}, F(${fit.k}, ${fit.dfe}) = ${F2(fit.F)}, ${PS(fit.pF)}`;

  const RUN = {
    async pearson(r) { return corrMatrix(r.vars, false); },
    async spearman(r) { return corrMatrix(r.vars, true); },

    async partial(r) {
      const P = prep({ x: r.x, y: r.y, c: r.covs });
      const D = designCols(P, r.covs), Z = rowsOf(D.cols); need(P.n > D.cols.length + 3, `분석한 사람(${P.n}명)이 통제변인 수(더미 포함 ${D.cols.length}개)에 비해 너무 적습니다.`);
      const res = k => { const y = P.col(k), f = ols(y, Z); need(f, collinMsg(D.names, P.n).replace('예측변인', '통제변인')); return y.map((v, i) => v - (f.b[0] + Z[i].reduce((s, z, j) => s + z * f.b[j + 1], 0))); };
      const rr = corr(res(r.x), res(r.y)), df = P.n - 2 - D.cols.length, t = rr * Math.sqrt(df / (1 - rr * rr)), p = ST.pT(Math.abs(t), df);
      const r0 = corr(P.col(r.x), P.col(r.y));
      const R = newResult('partial', `편상관: ${P.V[r.x].name} – ${P.V[r.y].name}`); R.n = P.n;
      R.tables.push({ title: '편상관 결과', head: ['', 'r', 'df', 't', 'p'], num: [1, 2, 3, 4], rows: [['단순상관 (통제 전)', NZ(r0), String(P.n - 2), '', PV(ST.pT(Math.abs(r0 * Math.sqrt((P.n - 2) / (1 - r0 * r0))), P.n - 2))], [`편상관 (${D.names.join(', ')} 통제)`, NZ(rr), String(df), F2(t), PV(p)]] });
      eff(R, { label: `${P.V[r.x].name} – ${P.V[r.y].name} 편상관 (통제: ${D.names.join(', ')})`, est: rr, p, stat: `r = ${NZ(rr)}, t(${df}) = ${F2(t)}, ${PS(p)}` });
      dropNote(R, P); return R;
    },

    async pointbiserial(r) {
      const P = prep({ g: r.g, y: r.y }); const lv2 = P.V[r.g].levels.filter(l => P.col(r.g).includes(l));
      need(lv2.length === 2, `빠진 값을 뺀 뒤 ${P.V[r.g].name}에 ${lv2.length ? `'${lv2[0]}' 한 가지만` : '값이'} 남았습니다. 두 집단이 모두 있어야 합니다.`);
      const B = binCode(P, r.g), y = P.col(r.y); need(sd(y) > 0, `${P.V[r.y].name}의 응답이 모두 같아(분산 0) 상관을 구할 수 없습니다.`);
      const rr = corr(B.x, y), df = P.n - 2, t = rr * Math.sqrt(df / (1 - rr * rr)), p = ST.pT(Math.abs(t), df);
      const R = newResult('pointbiserial', `점이연 상관: ${P.V[r.g].name} – ${P.V[r.y].name}`); R.n = P.n;
      R.tables.push({ title: '점이연 상관', head: ['', 'r', 'df', 't', 'p'], num: [1, 2, 3, 4], rows: [[`${P.V[r.g].name} – ${P.V[r.y].name}`, NZ(rr), String(df), F2(t), PV(p)]] });
      R.notes.push(B.note + '. 양수이면 1로 코딩한 집단의 점수가 더 높다는 뜻입니다. 이 결과는 등분산을 가정한 독립표본 t검정과 같습니다.');
      addFig(R, { type: 'dots', title: `${P.V[r.g].name}에 따른 ${P.V[r.y].name}의 평균`, yLabel: P.V[r.y].name, yRange: scoreRange(r.y), cats: lv2, series: [{ name: P.V[r.y].name, pts: lv2.map(l => ciOf(y.filter((_, i) => P.col(r.g)[i] === l))) }], note: '점은 집단 평균, 막대는 평균의 95% 신뢰구간입니다.' });
      eff(R, { label: `${P.V[r.g].name}(1 = ${P.V[r.g].levels[1]}) – ${P.V[r.y].name} 점이연 상관`, est: rr, p, stat: `r = ${NZ(rr)}, t(${df}) = ${F2(t)}, ${PS(p)}` });
      dropNote(R, P); return R;
    },

    async chisq(r) {
      const P = prep({ a: r.a, b: r.b }); const A = P.col(r.a), Bc = P.col(r.b);
      const la = P.V[r.a].levels.filter(l => A.includes(l)), lb = P.V[r.b].levels.filter(l => Bc.includes(l));
      need(la.length > 1 && lb.length > 1, '두 분류 모두 두 가지 이상의 값이 있어야 합니다.');
      const O = la.map(a => lb.map(b => A.filter((v, i) => v === a && Bc[i] === b).length));
      const rs = O.map(rw => rw.reduce((s, v) => s + v, 0)), cs = lb.map((_, j) => O.reduce((s, rw) => s + rw[j], 0)), N = P.n;
      let chi = 0, low = 0;
      O.forEach((rw, i) => rw.forEach((o, j) => { const E = rs[i] * cs[j] / N; if (E < 5) low++; chi += (o - E) ** 2 / E; }));
      const df = (la.length - 1) * (lb.length - 1), p = ST.pChi(chi, df), V = Math.sqrt(chi / (N * (Math.min(la.length, lb.length) - 1)));
      const R = newResult('chisq', `카이제곱: ${P.V[r.a].name} × ${P.V[r.b].name}`); R.n = N;
      R.tables.push({ title: '교차표 (빈도, 행 %)', head: [P.V[r.a].name + ' \\ ' + P.V[r.b].name].concat(lb, ['전체']), num: lb.map((_, j) => j + 1).concat([lb.length + 1]),
        rows: O.map((rw, i) => [la[i]].concat(rw.map(o => `${o} (${F2(o / rs[i] * 100, 1)}%)`), [String(rs[i])])).concat([['전체'].concat(cs.map(String), [String(N)])]) });
      { const swap = lb.length > 3 && la.length <= 3;
        if (!swap && lb.length <= 3) addFig(R, { type: 'props', title: `${P.V[r.a].name}에 따른 ${P.V[r.b].name}의 비율`, yLabel: '비율', cats: la, series: lb.map((b, j) => ({ name: `${P.V[r.b].name}: ${b}`, vals: O.map((rw, i) => rw[j] / rs[i] * 100) })), note: `막대는 ${P.V[r.a].name}의 각 집단 안에서 ${P.V[r.b].name}의 비율(행 %)입니다.` });
        else if (swap) addFig(R, { type: 'props', title: `${P.V[r.a].name}에 따른 ${P.V[r.b].name}의 비율`, yLabel: '비율', cats: lb, series: la.map((a, i) => ({ name: `${P.V[r.a].name}: ${a}`, vals: O[i].map(o => o / rs[i] * 100) })), note: `막대는 ${P.V[r.a].name}의 각 집단 안에서 ${P.V[r.b].name}의 비율(행 %)입니다.` }); }
      R.tables.push({ title: '검정 결과', head: ['χ²', 'df', 'p', 'Cramér의 V', '기대빈도 5 미만 칸'], num: [0, 1, 2, 3, 4], rows: [[F2(chi), String(df), PV(p), NZ(V), `${low}개 / ${la.length * lb.length}칸`]] });
      if (low / (la.length * lb.length) > 0.2) R.notes.push('기대빈도가 5보다 작은 칸이 20%를 넘습니다. 피셔 정확검정을 쓰거나 범주를 합치는 것을 검토하세요.');
      eff(R, { kind: 'omnibus', label: `${P.V[r.a].name}와(과) ${P.V[r.b].name}의 관련성`, est: V, p, stat: `χ²(${df}, N = ${N}) = ${F2(chi)}, ${PS(p)}, V = ${NZ(V)}` });
      dropNote(R, P); return R;
    },

    async onesample(r, o) {
      const tv = Number(o.testValue); need(isFinite(tv), '기준값을 숫자로 적어 주세요.');
      const P = prep({ y: r.y }); const y = P.col(r.y), m = mean(y), s = sd(y), n = P.n, t = (m - tv) / (s / Math.sqrt(n)), df = n - 1, p = ST.pT(Math.abs(t), df), d = (m - tv) / s;
      const tc = ST.tInv(0.05, df), se = s / Math.sqrt(n);
      const R = newResult('onesample', `단일표본 t검정: ${P.V[r.y].name} (기준값 ${tv})`); R.n = n;
      R.tables.push({ title: '결과', head: ['변인', 'N', 'M', 'SD', '기준값', '평균 차이', '95% CI', 't', 'df', 'p', 'Cohen의 d'], num: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], rows: [[P.V[r.y].name, String(n), F2(m), F2(s), String(tv), F2(m - tv), CI(m - tv - tc * se, m - tv + tc * se), F2(t), String(df), PV(p), F2(d)]] });
      const lbf = BAYES.t(t, n, df);
      eff(R, { label: `${P.V[r.y].name} 평균 − 기준값 ${tv}`, est: m - tv, p, es: d, esKind: 'd', lbf, stat: `M = ${F2(m)}, SD = ${F2(s)}, t(${df}) = ${F2(t)}, ${PS(p)}, d = ${F2(d)}` });
      addFig(R, { type: 'dots', title: `${P.V[r.y].name}의 평균과 기준값(${tv})`, yLabel: P.V[r.y].name, yRange: scoreRange(r.y), cats: [P.V[r.y].name], series: [{ name: P.V[r.y].name, pts: [ciOf(y)] }], ref: { y: tv, label: `기준값 ${tv}` }, note: '점은 평균, 막대는 평균의 95% 신뢰구간입니다.' });
      addBayes(R, [[`${P.V[r.y].name} 평균 ≠ ${tv}`, lbf]], `효과크기 δ ~ 코시(0, √2/2), JASP·BayesFactor의 기본값(medium)`);
      dropNote(R, P); return R;
    },

    async ind_t(r, o) {
      const R = newResult('ind_t', `독립표본 t검정: ${varByKey(r.g).name}`);
      const rows = [], lev = [], bfr = [], fc = [], fa = [], fb = [], fr = [];
      let nMax = 0, lv;
      for (const yk of r.ys) {
        const P = prep({ g: r.g, y: yk }); const g = P.col(r.g), y = P.col(yk);
        lv = P.V[r.g].levels.filter(l => g.includes(l)); need(lv.length === 2, '집단 변인에 두 가지 값이 있어야 합니다.');
        const a = y.filter((_, i) => g[i] === lv[0]), b = y.filter((_, i) => g[i] === lv[1]);
        need(a.length > 1 && b.length > 1, '각 집단에 2명 이상 있어야 합니다.');
        const { ma, mb, va, vb, na, nb, sp, tS, dfS, tW, dfW } = twoSample(a, b);
        const L = levene([a, b]); const useW = o.welch === 'never' ? false : o.welch === 'auto' ? L.p < 0.05 : true;
        const t = useW ? tW : tS, df = useW ? dfW : dfS, p = ST.pT(Math.abs(t), df), d = (ma - mb) / sp;
        rows.push([P.V[yk].name, `${F2(ma)} (${F2(Math.sqrt(va))})`, `${F2(mb)} (${F2(Math.sqrt(vb))})`, `${F2(L.F)} (${PV(L.p)})`, F2(t), useW ? F2(df, 1) : String(df), PV(p), F2(d), useW ? 'Welch' : 'Student']);
        const lbf = BAYES.t(tS, na * nb / (na + nb), dfS); bfr.push([`${P.V[yk].name}: ${lv[0]} ≠ ${lv[1]}`, lbf]);
        fc.push(P.V[yk].name); fa.push(ciOf(a)); fb.push(ciOf(b)); fr.push(scoreRange(yk));
        eff(R, { label: `${P.V[yk].name}: ${lv[0]} − ${lv[1]}`, est: ma - mb, p, es: d, esKind: 'd', lbf, stat: `${lv[0]} M = ${F2(ma)}, ${lv[1]} M = ${F2(mb)}, t(${useW ? F2(df, 1) : df}) = ${F2(t)}, ${PS(p)}, d = ${F2(d)}` });
        nMax = Math.max(nMax, P.n); lev.push([na, nb]);
      }
      R.n = nMax;
      const nr = j => { const a = lev.map(x => x[j]), lo = Math.min(...a), hi = Math.max(...a); return lo === hi ? String(lo) : `${lo}~${hi}`; };
      R.tables.push({ title: `집단 비교 (${lv[0]} n = ${nr(0)}, ${lv[1]} n = ${nr(1)}${lev.some(x => x[0] !== lev[0][0] || x[1] !== lev[0][1]) ? ', 점수마다 빠진 값이 달라 n이 다름' : ''})`, head: ['변인', `${lv[0]} M (SD)`, `${lv[1]} M (SD)`, 'Levene F (p)', 't', 'df', 'p', 'Cohen의 d', '검정'], num: [1, 2, 3, 4, 5, 6, 7], rows });
      R.notes.push((o.welch === 'never' ? '분석 결정에 따라 언제나 등분산을 가정한 Student t검정 값을 보고했습니다.' : o.welch === 'auto' ? 'Levene 검정이 유의하면(p < .05) 등분산을 가정하지 않는 Welch t검정 값을 보고했습니다.' : '분산이 같다고 가정하지 않는 Welch t검정 값을 보고했습니다(분산이 같아도 거의 손해가 없어 최근 권장되는 방식). Levene 검정은 참고로 함께 보였습니다.') + ' d는 앞 집단 − 뒤 집단 기준입니다.');
      addFig(R, { type: 'dots', title: `${varByKey(r.g).name}에 따른 ${fc.join(', ')}의 평균`, yLabel: fc.length > 1 ? '점수' : fc[0], yRange: unionRange(fr), cats: fc, series: [{ name: lv[0], pts: fa }, { name: lv[1], pts: fb }], note: '점은 집단 평균, 막대는 평균의 95% 신뢰구간입니다.' });
      addBayes(R, bfr, `효과크기 δ ~ 코시(0, √2/2), JASP·BayesFactor의 기본값(medium). JASP처럼 등분산을 가정한 모형으로 계산`);
      return R;
    },

    async paired_t(r) {
      const P = prep({ a: r.a, b: r.b }); const a = P.col(r.a), b = P.col(r.b), dif = a.map((v, i) => v - b[i]);
      const md = mean(dif), sdd = sd(dif), n = P.n, df = n - 1, t = md / (sdd / Math.sqrt(n)), p = ST.pT(Math.abs(t), df), dz = md / sdd;
      const tc = ST.tInv(0.05, df), se = sdd / Math.sqrt(n);
      const R = newResult('paired_t', `대응표본 t검정: ${P.V[r.a].name} – ${P.V[r.b].name}`); R.n = n;
      R.tables.push({ title: '결과', head: ['', 'M', 'SD'], num: [1, 2], rows: [[P.V[r.a].name, F2(mean(a)), F2(sd(a))], [P.V[r.b].name, F2(mean(b)), F2(sd(b))]] });
      R.tables.push({ title: '차이 검정', head: ['평균 차이', 'SD', '95% CI', 't', 'df', 'p', 'Cohen의 dz', 'r (두 점수)'], num: [0, 1, 2, 3, 4, 5, 6, 7], rows: [[F2(md), F2(sdd), CI(md - tc * se, md + tc * se), F2(t), String(df), PV(p), F2(dz), NZ(corr(a, b))]] });
      const lbf = BAYES.t(t, n, df);
      eff(R, { label: `${P.V[r.a].name} − ${P.V[r.b].name}`, est: md, p, es: dz, esKind: 'd', lbf, stat: `M차이 = ${F2(md)}, t(${df}) = ${F2(t)}, ${PS(p)}, dz = ${F2(dz)}` });
      addFig(R, { type: 'lines', title: `${J(P.V[r.a].name, '과/와')} ${P.V[r.b].name}의 평균`, yLabel: '점수', yRange: unionRange([scoreRange(r.a), scoreRange(r.b)]), xs: [P.V[r.a].name, P.V[r.b].name], series: [{ name: '평균', pts: [ciOf(a), ciOf(b)] }], note: '점은 평균, 막대는 각 점수의 95% 신뢰구간입니다. 같은 사람의 점수라 차이 검정의 신뢰구간(표)과는 다릅니다.' });
      addBayes(R, [[`${P.V[r.a].name} − ${P.V[r.b].name} ≠ 0`, lbf]], `효과크기 δ ~ 코시(0, √2/2), JASP·BayesFactor의 기본값(medium)`);
      dropNote(R, P); return R;
    },

    async oneway(r, o) {
      const R = newResult('oneway', `일원분산분석: ${varByKey(r.g).name}`);
      for (const yk of r.ys) {
        const P = prep({ g: r.g, y: yk }); const g = P.col(r.g), y = P.col(yk);
        const lv = P.V[r.g].levels.filter(l => g.includes(l)); need(lv.length >= 2, '집단이 두 가지 이상 있어야 합니다.');
        const G = lv.map(l => y.filter((_, i) => g[i] === l)); need(G.every(x => x.length > 1), '각 집단에 2명 이상 있어야 합니다.');
        const N = y.length, k = lv.length, gm = mean(y);
        const ssb = G.reduce((s, x) => s + x.length * (mean(x) - gm) ** 2, 0), ssw = G.reduce((s, x) => s + x.reduce((q, v) => q + (v - mean(x)) ** 2, 0), 0);
        const dfb = k - 1, dfw = N - k, F = (ssb / dfb) / (ssw / dfw), p = ST.pF(F, dfb, dfw), eta = ssb / (ssb + ssw);
        const L = levene(G);
        // Welch F
        const wts = G.map(x => x.length / variance(x)), Wsum = wts.reduce((a, b) => a + b, 0), mw = G.reduce((s, x, i) => s + wts[i] * mean(x), 0) / Wsum;
        const A = G.reduce((s, x, i) => s + wts[i] * (mean(x) - mw) ** 2, 0) / (k - 1), tmp = G.reduce((s, x, i) => s + (1 - wts[i] / Wsum) ** 2 / (x.length - 1), 0);
        const FW = A / (1 + 2 * (k - 2) / (k * k - 1) * tmp), dfW = (k * k - 1) / (3 * tmp), pW = ST.pF(FW, k - 1, dfW);
        R.tables.push({ title: `${P.V[yk].name}: 집단별 기술통계`, head: ['집단', 'n', 'M', 'SD'], num: [1, 2, 3], rows: G.map((x, i) => [lv[i], String(x.length), F2(mean(x)), F2(sd(x))]) });
        addFig(R, { type: 'dots', title: `${P.V[r.g].name}에 따른 ${P.V[yk].name}의 평균`, yLabel: P.V[yk].name, yRange: scoreRange(yk), cats: lv, series: [{ name: P.V[yk].name, pts: G.map(ciOf) }], note: '점은 집단 평균, 막대는 평균의 95% 신뢰구간입니다.' });
        R.tables.push({ title: `${P.V[yk].name}: 분산분석`, head: ['', 'SS', 'df', 'MS', 'F', 'p', 'η²'], num: [1, 2, 3, 4, 5, 6], rows: [['집단 간', F2(ssb), String(dfb), F2(ssb / dfb), F2(F), PV(p), NZ(eta, 3)], ['집단 내', F2(ssw), String(dfw), F2(ssw / dfw), '', '', ''], ['전체', F2(ssb + ssw), String(N - 1), '', '', '', '']] });
        const post = [], bon = o.posthoc === 'bonferroni', mPairs = k * (k - 1) / 2;
        for (let i = 0; i < k; i++) for (let j = i + 1; j < k; j++) {
          const diff = mean(G[i]) - mean(G[j]), se2 = (ssw / dfw) * (1 / G[i].length + 1 / G[j].length);
          const pp = bon ? Math.min(1, ST.pT(Math.abs(diff / Math.sqrt(se2)), dfw) * mPairs) : ST.pF(diff ** 2 / (se2 * (k - 1)), k - 1, dfw);
          post.push([`${lv[i]} − ${lv[j]}`, F2(diff), PV(pp)]);
        }
        R.tables.push({ title: `${P.V[yk].name}: 사후검정 (${bon ? 'Bonferroni' : 'Scheffé'})`, head: ['비교', '평균 차이', 'p'], num: [1, 2], rows: post });
        R.notes.push(`${P.V[yk].name}: Levene F(${dfb}, ${dfw}) = ${F2(L.F)}, ${PS(L.p)}.${L.p < 0.05 ? (isFinite(FW) ? ` 등분산 가정이 어긋나 Welch F(${dfb}, ${F2(dfW, 1)}) = ${F2(FW)}, ${PS(pW)}도 함께 보고하는 것이 좋습니다.` : ` 등분산 가정이 어긋나지만, 응답이 모두 같은(분산 0) 집단(${lv.filter((_, i) => !(variance(G[i]) > 0)).join(', ')})이 있어 Welch F를 계산할 수 없습니다.`) : ''}`);
        eff(R, { kind: 'omnibus', label: `${P.V[yk].name}의 ${P.V[r.g].name}별 차이`, est: eta, p, stat: `F(${dfb}, ${dfw}) = ${F2(F)}, ${PS(p)}, η² = ${NZ(eta, 3)}` });
        R.n = Math.max(R.n, P.n);
      }
      return R;
    },

    async factorial(r) {
      const P = prep({ a: r.a, b: r.b, y: r.y }); const A = P.col(r.a), Bv = P.col(r.b), y = P.col(r.y);
      const la = P.V[r.a].levels.filter(l => A.includes(l)), lb = P.V[r.b].levels.filter(l => Bv.includes(l));
      need(la.length > 1 && lb.length > 1, '두 기준 모두 두 가지 이상의 값이 있어야 합니다.');
      const eff1 = (vals, lv) => lv.slice(0, -1).map(l => vals.map(v => (v === l ? 1 : v === lv[lv.length - 1] ? -1 : 0)));
      const ca = eff1(A, la), cb = eff1(Bv, lb), cab = []; ca.forEach(x => cb.forEach(z => cab.push(x.map((v, i) => v * z[i]))));
      const full = ols(y, rowsOf([...ca, ...cb, ...cab])); need(full, '빈 칸(어떤 조합에 응답이 없음)이 있어 계산할 수 없습니다.');
      const drop = cols => ols(y, rowsOf(cols)).sse - full.sse;
      const terms = [[P.V[r.a].name, drop([...cb, ...cab]), ca.length], [P.V[r.b].name, drop([...ca, ...cab]), cb.length], [`${P.V[r.a].name} × ${P.V[r.b].name}`, drop([...ca, ...cb]), cab.length]];
      const mse = full.mse;
      const R = newResult('factorial', `이원분산분석: ${P.V[r.a].name} × ${P.V[r.b].name} → ${P.V[r.y].name}`); R.n = P.n;
      { const [xa, xb, la2, lb2] = lb.length > 3 && la.length <= 3 ? [Bv, A, lb, la] : [A, Bv, la, lb], na = lb.length > 3 && la.length <= 3 ? r.b : r.a, nb = lb.length > 3 && la.length <= 3 ? r.a : r.b;
        if (lb2.length <= 3) addFig(R, { type: 'lines', title: `${J(P.V[na].name, '과/와')} ${P.V[nb].name}에 따른 ${P.V[r.y].name}의 평균 (상호작용)`, yLabel: P.V[r.y].name, yRange: scoreRange(r.y), xLabel: P.V[na].name, xs: la2, series: lb2.map(b => ({ name: `${P.V[nb].name}: ${b}`, pts: la2.map(a => { const c = y.filter((_, i) => xa[i] === a && xb[i] === b); return c.length ? ciOf(c) : { m: NaN, lo: NaN, hi: NaN, n: 0 }; }) })), note: '선이 서로 평행하지 않으면 상호작용이 있다는 뜻입니다. 점은 칸 평균, 막대는 95% 신뢰구간입니다.' }); }
      R.tables.push({ title: '칸별 평균 (SD, n)', head: [P.V[r.a].name + ' \\ ' + P.V[r.b].name].concat(lb), num: lb.map((_, j) => j + 1),
        rows: la.map(a => [a].concat(lb.map(b => { const c = y.filter((_, i) => A[i] === a && Bv[i] === b); return c.length ? `${F2(mean(c))} (${F2(sd(c))}, ${c.length})` : '-'; }))) });
      R.tables.push({ title: '분산분석 (제III유형 제곱합)', head: ['', 'SS', 'df', 'MS', 'F', 'p', '부분 η²'], num: [1, 2, 3, 4, 5, 6],
        rows: terms.map(([nm, ss, df]) => { const F = (ss / df) / mse; return [nm, F2(ss), String(df), F2(ss / df), F2(F), PV(ST.pF(F, df, full.dfe)), NZ(ss / (ss + full.sse), 3)]; }).concat([['오차', F2(full.sse), String(full.dfe), F2(mse), '', '', '']]) });
      terms.forEach(([nm, ss, df]) => { const F = (ss / df) / mse, p = ST.pF(F, df, full.dfe), pe = ss / (ss + full.sse); eff(R, { kind: 'omnibus', label: `${nm} 효과 (→ ${P.V[r.y].name})`, est: pe, p, stat: `F(${df}, ${full.dfe}) = ${F2(F)}, ${PS(p)}, 부분 η² = ${NZ(pe, 3)}` }); });
      R.notes.push('상호작용이 유의하면 주효과보다 상호작용을 먼저 해석하고, 단순주효과 분석을 추가로 검토하세요.');
      dropNote(R, P); return R;
    },

    async ancova(r) {
      const P = prep({ g: r.g, y: r.y, c: r.covs }); const g = P.col(r.g), y = P.col(r.y);
      const lv = P.V[r.g].levels.filter(l => g.includes(l)); need(lv.length >= 2, '집단이 두 가지 이상 있어야 합니다.');
      const dum = lv.slice(1).map(l => g.map(v => (v === l ? 1 : 0))), covs = r.covs.map(k => P.col(k));
      const full = ols(y, rowsOf([...dum, ...covs])), red = ols(y, rowsOf(covs));
      need(full && red, collinMsg([...lv.slice(1).map(l => `${P.V[r.g].name}: ${l}`), ...r.covs.map(k => P.V[k].name)], P.n).replace('예측변인', '집단과 공변인'));
      const ssG = red.sse - full.sse, dfG = dum.length, FG = (ssG / dfG) / full.mse, pG = ST.pF(FG, dfG, full.dfe), peG = ssG / (ssG + full.sse);
      const rows = [[P.V[r.g].name, F2(ssG), String(dfG), F2(ssG / dfG), F2(FG), PV(pG), NZ(peG, 3)]];
      r.covs.forEach((k, j) => { const others = covs.filter((_, q) => q !== j); const f = ols(y, rowsOf([...dum, ...others])); need(f, collinMsg(r.covs.map(c => P.V[c].name), P.n)); const ss = f.sse - full.sse, F = ss / full.mse; rows.push([P.V[k].name + ' (공변인)', F2(ss), '1', F2(ss), F2(F), PV(ST.pF(F, 1, full.dfe)), NZ(ss / (ss + full.sse), 3)]); });
      rows.push(['오차', F2(full.sse), String(full.dfe), F2(full.mse), '', '', '']);
      // 조정된 평균: 공변인 평균에서 예측
      const cm = covs.map(mean);
      const adj = lv.map((l, i) => full.b[0] + (i ? full.b[i] : 0) + cm.reduce((s, m, j) => s + m * full.b[1 + dum.length + j], 0));
      // 회귀 기울기 동질성
      const inter = []; dum.forEach(d => covs.forEach(c => inter.push(d.map((v, i) => v * c[i]))));
      const fi = ols(y, rowsOf([...dum, ...covs, ...inter])); const ssI = fi ? full.sse - fi.sse : NaN, dfI = inter.length, FI = fi ? (ssI / dfI) / fi.mse : NaN, pI = fi ? ST.pF(FI, dfI, fi.dfe) : NaN;
      const R = newResult('ancova', `공분산분석: ${P.V[r.g].name} → ${P.V[r.y].name}`); R.n = P.n;
      R.tables.push({ title: '집단별 평균', head: ['집단', 'n', '관찰 평균 (SD)', '조정된 평균'], num: [1, 2, 3], rows: lv.map((l, i) => { const c = y.filter((_, q) => g[q] === l); return [l, String(c.length), `${F2(mean(c))} (${F2(sd(c))})`, F2(adj[i])]; }) });
      R.tables.push({ title: '공분산분석', head: ['', 'SS', 'df', 'MS', 'F', 'p', '부분 η²'], num: [1, 2, 3, 4, 5, 6], rows });
      if (!fi) R.notes.push('회귀 기울기 동질성은 집단마다 사람이 적거나 공변인 값이 겹쳐 계산하지 못했습니다. 집단별 산점도로 확인하세요.');
      else R.notes.push(`회귀 기울기 동질성: F(${dfI}, ${fi.dfe}) = ${F2(FI)}, ${PS(pI)}.${pI < 0.05 ? ' 공변인과 종속변인의 관계가 집단마다 달라 공분산분석의 전제가 어긋납니다. 상호작용을 넣은 회귀분석을 검토하세요.' : ' 전제가 크게 어긋나지 않았습니다.'}`);
      eff(R, { kind: 'omnibus', label: `${P.V[r.y].name}의 ${P.V[r.g].name}별 차이 (공변인 통제)`, est: peG, p: pG, stat: `F(${dfG}, ${full.dfe}) = ${F2(FG)}, ${PS(pG)}, 부분 η² = ${NZ(peG, 3)}` });
      dropNote(R, P); return R;
    },

    async regression(r) {
      const P = prep({ y: r.y, x: r.xs }); const y = P.col(r.y), D = designCols(P, r.xs);
      const fit = ols(y, rowsOf(D.cols)); need(fit, collinMsg(D.names, P.n)); fit.vif = vifs(D.cols);
      const R = newResult('regression', `${D.cols.length > 1 ? '다중' : '단순'}회귀: → ${P.V[r.y].name}`); R.n = P.n;
      R.tables.push(regTable(fit, D.names, sd(y), D.cols.map(sd), `종속변인: ${P.V[r.y].name}`));
      R.notes.push('모형: ' + fitLine(fit));
      if (fit.vif.some(v => v >= 10)) R.notes.push('VIF가 10 이상인 변인이 있어 다중공선성을 의심할 수 있습니다.');
      if (D.cols.length === 1 && P.V[r.xs[0]].type === 'num') addFig(R, { type: 'scatter', title: `${J(D.names[0], '과/와')} ${P.V[r.y].name}의 관계`, xLabel: D.names[0], yLabel: P.V[r.y].name, xRange: scoreRange(r.xs[0]), yRange: scoreRange(r.y), pts: thinPts(D.cols[0].map((v, i) => [v, y[i]])), line: [fit.b[0], fit.b[1]], tag: `B = ${F2(fit.b[1])}, R² = ${NZ(fit.R2)}`, note: '선은 회귀선입니다. 같은 자리에 겹친 사람이 많을수록 점이 큽니다.' });
      addFig(R, residFig(fit, y, `${P.V[r.y].name} 회귀모형의 잔차`));
      const lbF = BAYES.reg(fit.R2, P.n, fit.k);
      // 예측변인 하나의 BF = 전체 모형 BF ÷ 그 변인을 뺀 모형 BF (BayesFactor regressionBF의 whichModels = "top"과 같음)
      const lbj = D.names.map((_, j) => { if (D.cols.length === 1) return lbF; const f = ols(y, rowsOf(D.cols.filter((_, q) => q !== j))); return f ? lbF - BAYES.reg(f.R2, P.n, f.k) : NaN; });
      addBayes(R, [['회귀모형 전체 (절편만 있는 모형과 비교)', lbF], ...D.names.map((nm, j) => [`${nm} (빼면 나빠지는 정도)`, lbj[j]])], '회귀계수 ~ JZS(Zellner–Siow) 코시, 폭 √2/4 = 0.354. JASP·BayesFactor의 기본값(medium)');
      D.names.forEach((nm, j) => eff(R, { label: `${nm} → ${P.V[r.y].name}`, est: fit.b[j + 1], std: fit.b[j + 1] * sd(D.cols[j]) / sd(y), es: fit.b[j + 1] * sd(D.cols[j]) / sd(y), esKind: 'beta', lbf: lbj[j], p: fit.p[j + 1], lo: fit.lo[j + 1], hi: fit.hi[j + 1], stat: `B = ${F2(fit.b[j + 1])}, β = ${NZ(fit.b[j + 1] * sd(D.cols[j]) / sd(y))}, t(${fit.dfe}) = ${F2(fit.t[j + 1])}, ${PS(fit.p[j + 1])}` }));
      eff(R, { kind: 'omnibus', label: `회귀모형 전체 설명력 (→ ${P.V[r.y].name})`, est: fit.R2, p: fit.pF, lbf: lbF, stat: fitLine(fit) });
      dropNote(R, P); return R;
    },

    async hier(r) {
      const blocks = [r.b1, r.b2, r.b3 || []].filter(b => b.length);
      const P = prep({ y: r.y, x: blocks.flat() }); const y = P.col(r.y);
      const R = newResult('hier', `위계적 회귀: → ${P.V[r.y].name}`); R.n = P.n;
      let prev = null; const summ = [], hbf = []; let keys = [];
      blocks.forEach((b, s) => {
        keys = keys.concat(b); const D = designCols(P, keys); const fit = ols(y, rowsOf(D.cols)); need(fit, collinMsg(D.names, P.n)); fit.vif = vifs(D.cols);
        R.tables.push(regTable(fit, D.names, sd(y), D.cols.map(sd), `모형 ${s + 1}`));
        const dR = prev ? fit.R2 - prev.R2 : fit.R2, df1 = prev ? fit.k - prev.k : fit.k, dF = (dR / df1) / ((1 - fit.R2) / fit.dfe), dp = ST.pF(dF, df1, fit.dfe);
        summ.push([`모형 ${s + 1}`, NZ(fit.R2), NZ(fit.adj), NZ(dR), `${F2(dF)} (${df1}, ${fit.dfe})`, PV(dp), `${F2(fit.F)} (${PV(fit.pF)})`]);
        const lbS = BAYES.reg(fit.R2, P.n, fit.k), lbD = prev ? lbS - BAYES.reg(prev.R2, P.n, prev.k) : lbS; hbf.push([s ? `모형 ${s + 1} vs 모형 ${s} (${s + 1}단계 투입 변인)` : '모형 1 vs 절편만 있는 모형', lbD]);
        if (s > 0) eff(R, { kind: 'omnibus', label: `${s + 1}단계 투입 변인의 추가 설명력 (ΔR²)`, est: dR, p: dp, lbf: lbD, stat: `ΔR² = ${NZ(dR)}, ΔF(${df1}, ${fit.dfe}) = ${F2(dF)}, ${PS(dp)}` });
        if (s === blocks.length - 1) addFig(R, residFig(fit, y, `${P.V[r.y].name} 최종 모형(모형 ${s + 1})의 잔차`));
        if (s === blocks.length - 1) D.names.forEach((nm, j) => eff(R, { label: `${nm} → ${P.V[r.y].name} (모형 ${s + 1})`, est: fit.b[j + 1], std: fit.b[j + 1] * sd(D.cols[j]) / sd(y), p: fit.p[j + 1], lo: fit.lo[j + 1], hi: fit.hi[j + 1], stat: `B = ${F2(fit.b[j + 1])}, β = ${NZ(fit.b[j + 1] * sd(D.cols[j]) / sd(y))}, t(${fit.dfe}) = ${F2(fit.t[j + 1])}, ${PS(fit.p[j + 1])}` }));
        prev = fit;
      });
      R.tables.unshift({ title: '모형 요약', head: ['', 'R²', '수정된 R²', 'ΔR²', 'ΔF (df)', 'ΔF의 p', '모형 F (p)'], num: [1, 2, 3, 4, 5, 6], rows: summ });
      addBayes(R, hbf, '회귀계수 ~ JZS(Zellner–Siow) 코시, 폭 √2/4 = 0.354. JASP·BayesFactor의 기본값(medium)');
      dropNote(R, P); return R;
    },

    async logistic(r) {
      const P = prep({ y: r.y, x: r.xs }); const B = binCode(P, r.y), y = B.x, D = designCols(P, r.xs), X = rowsOf(D.cols), n = P.n, k = D.cols.length, p = k + 1;
      let b = new Array(p).fill(0), ll = -Infinity, cov = null;
      for (let it = 0; it < 60; it++) {
        const H = Array.from({ length: p }, () => new Array(p).fill(0)), g = new Array(p).fill(0); let L = 0;
        for (let i = 0; i < n; i++) {
          const xi = [1, ...X[i]]; const eta = xi.reduce((s, v, j) => s + v * b[j], 0), pi = 1 / (1 + Math.exp(-eta)), w = pi * (1 - pi);
          L += y[i] ? Math.log(pi || 1e-300) : Math.log(1 - pi || 1e-300);
          for (let a = 0; a < p; a++) { g[a] += (y[i] - pi) * xi[a]; for (let c = 0; c < p; c++) H[a][c] += w * xi[a] * xi[c]; }
        }
        cov = matInv(H); need(cov, '완전 분리(어떤 변인이 결과를 완벽하게 나눔) 등으로 계산할 수 없습니다.');
        const step = cov.map(rw => rw.reduce((s, v, j) => s + v * g[j], 0));
        b = b.map((v, j) => v + step[j]);
        if (Math.abs(L - ll) < 1e-10) { ll = L; break; } ll = L;
      }
      const p1 = mean(y), ll0 = n * (p1 * Math.log(p1) + (1 - p1) * Math.log(1 - p1));
      const chi = 2 * (ll - ll0), pm = ST.pChi(chi, k), cs = 1 - Math.exp((2 / n) * (ll0 - ll)), nk = cs / (1 - Math.exp((2 / n) * ll0));
      let hit = 0; for (let i = 0; i < n; i++) { const eta = [1, ...X[i]].reduce((s, v, j) => s + v * b[j], 0); hit += ((eta >= 0 ? 1 : 0) === y[i]); }
      const se = cov.map((rw, i) => Math.sqrt(rw[i])), z = 1.959964;
      const R = newResult('logistic', `로지스틱 회귀: → ${P.V[r.y].name}`); R.n = n;
      R.tables.push({ title: `결과: ${B.note}`, head: ['변인', 'B', 'SE', 'Wald χ²', 'p', 'Exp(B)', 'Exp(B) 95% CI'], num: [1, 2, 3, 4, 5, 6],
        rows: [['(상수)', F2(b[0]), F2(se[0]), F2((b[0] / se[0]) ** 2), PV(ST.pChi((b[0] / se[0]) ** 2, 1)), F2(Math.exp(b[0])), '']].concat(D.names.map((nm, j) => { const q = j + 1, W = (b[q] / se[q]) ** 2; return [nm, F2(b[q]), F2(se[q]), F2(W), PV(ST.pChi(W, 1)), F2(Math.exp(b[q])), CI(Math.exp(b[q] - z * se[q]), Math.exp(b[q] + z * se[q]))]; })) });
      R.notes.push(`모형: χ²(${k}) = ${F2(chi)}, ${PS(pm)}, −2LL = ${F2(-2 * ll)}, Cox–Snell R² = ${NZ(cs)}, Nagelkerke R² = ${NZ(nk)}, 분류 정확도 ${F2(hit / n * 100, 1)}%`);
      D.names.forEach((nm, j) => { const q = j + 1, W = (b[q] / se[q]) ** 2, pp = ST.pChi(W, 1); eff(R, { label: `${nm} → ${P.V[r.y].name}(${P.V[r.y].levels[1]}) 가능성`, est: b[q], p: pp, stat: `B = ${F2(b[q])}, Wald χ²(1) = ${F2(W)}, ${PS(pp)}, OR = ${F2(Math.exp(b[q]))}` }); });
      dropNote(R, P); return R;
    },

    async med_parallel(r, o, onProg) {
      const P = prep({ x: r.x, m: r.ms, y: r.y, c: r.covs });
      const X = binCode(P, r.x), Ms = r.ms.map(k => P.col(k)), y = P.col(r.y), C = designCols(P, r.covs).cols, n = P.n;
      const fitAll = idx => {
        const pick = a => idx ? idx.map(i => a[i]) : a;
        const x = pick(X.x), yy = pick(y), cc = C.map(pick), ms = Ms.map(pick);
        const aF = ms.map(m => ols(m, rowsOf([x, ...cc]), !!idx)); const yF = ols(yy, rowsOf([x, ...ms, ...cc]), !!idx); const tF = ols(yy, rowsOf([x, ...cc]), !!idx);
        if (aF.some(f => !f) || !yF || !tF) return null;
        return { aF, yF, tF, a: aF.map(f => f.b[1]), b: ms.map((_, j) => yF.b[2 + j]), cp: yF.b[1], c: tF.b[1] };
      };
      const F0 = fitAll(null); need(F0, '변인끼리 완전히 겹쳐 계산할 수 없습니다.');
      const ind = f => f.a.map((a, j) => a * f.b[j]);
      const stat = f => { const id = ind(f); const tot = id.reduce((s, v) => s + v, 0); const out = [...id, tot]; for (let i = 0; i < id.length; i++) for (let j = i + 1; j < id.length; j++) out.push(id[i] - id[j]); return out; };
      const bs = await bootstrap(n, +o.boot || 5000, idx => { const f = fitAll(idx); return f ? stat(f) : null; }, onProg); need(bs, '부트스트랩 재표집이 하나도 계산되지 않았습니다. 표본이 너무 작거나 변인끼리 겹칩니다.');
      const est = stat(F0), sx = X.bin ? 1 : sd(X.x), sy = sd(y);   // X가 두 집단(0/1)이면 부분표준화(÷ SD(Y)), Hayes 권고
      const nmX = P.V[r.x].name, nmY = P.V[r.y].name, nmM = r.ms.map(k => P.V[k].name);
      const R = newResult('med_parallel', `${r.ms.length > 1 ? '병렬 다중' : '단순'}매개: ${nmX} → ${nmM.join(', ')} → ${nmY}`); R.n = n; bootNote(R, bs);
      pathTables(R, P, F0, nmX, nmY, nmM, r.covs, X.x, y, Ms); addFig(R, medFig(F0, nmX, nmY, nmM, false));
      const idRows = [];
      nmM.forEach((m, j) => idRows.push([`${nmX} → ${m} → ${nmY}`, F2(est[j], 3), F2(bs[j].se, 3), CI(bs[j].lo, bs[j].hi, 3), F2(est[j] * sx / sy, 3)]));
      if (nmM.length > 1) idRows.push(['총 간접효과', F2(est[nmM.length], 3), F2(bs[nmM.length].se, 3), CI(bs[nmM.length].lo, bs[nmM.length].hi, 3), F2(est[nmM.length] * sx / sy, 3)]);
      let q = nmM.length + 1; const con = [];
      for (let i = 0; i < nmM.length; i++) for (let j = i + 1; j < nmM.length; j++) { con.push([`대비: (${nmM[i]}) − (${nmM[j]})`, F2(est[q], 3), F2(bs[q].se, 3), CI(bs[q].lo, bs[q].hi, 3), '']); q++; }
      R.tables.push({ title: `간접효과 (부트스트랩 ${(+o.boot || 5000).toLocaleString()}번, 95% 백분위 신뢰구간)`, head: ['경로', '효과', 'Boot SE', '95% CI', X.bin ? '부분표준화 효과' : '완전표준화 효과'], num: [1, 2, 3, 4], rows: idRows.concat(con), foot: X.bin ? `${nmX}가 두 집단(${X.note})이라, 표준화 효과는 집단 비율에 따라 달라지는 SD(X)를 쓰지 않고 효과 ÷ SD(${nmY})로 구했습니다(부분표준화, Hayes 권고). 두 집단의 ${nmY} 차이를 SD 단위로 나타낸 값입니다.` : '' });
      R.tables.push(effectsTable(F0, nmX, nmY));
      nmM.forEach((m, j) => eff(R, { label: `간접효과: ${nmX} → ${m} → ${nmY}`, est: est[j], lo: bs[j].lo, hi: bs[j].hi, ciBased: true, std: est[j] * sx / sy, stat: `B = ${F2(est[j], 3)}, 95% CI ${CI(bs[j].lo, bs[j].hi, 3)}` }));
      if (nmM.length > 1) eff(R, { label: `총 간접효과: ${nmX} → (${nmM.join(', ')}) → ${nmY}`, est: est[nmM.length], lo: bs[nmM.length].lo, hi: bs[nmM.length].hi, ciBased: true, stat: `B = ${F2(est[nmM.length], 3)}, 95% CI ${CI(bs[nmM.length].lo, bs[nmM.length].hi, 3)}` });
      pathEffects(R, F0, nmX, nmY, nmM);
      medNotes(R, X, P); return R;
    },

    async med_serial(r, o, onProg) {
      const P = prep({ x: r.x, m1: r.m1, m2: r.m2, y: r.y, c: r.covs });
      const X = binCode(P, r.x), m1 = P.col(r.m1), m2 = P.col(r.m2), y = P.col(r.y), C = designCols(P, r.covs).cols, n = P.n;
      const fitAll = idx => {
        const pick = a => idx ? idx.map(i => a[i]) : a;
        const x = pick(X.x), a1 = pick(m1), a2 = pick(m2), yy = pick(y), cc = C.map(pick), f = !!idx;
        const f1 = ols(a1, rowsOf([x, ...cc]), f), f2 = ols(a2, rowsOf([x, a1, ...cc]), f), fy = ols(yy, rowsOf([x, a1, a2, ...cc]), f), ft = ols(yy, rowsOf([x, ...cc]), f);
        if (!f1 || !f2 || !fy || !ft) return null;
        return { aF: [f1, f2], yF: fy, tF: ft, a: [f1.b[1], f2.b[1]], d21: f2.b[2], b: [fy.b[2], fy.b[3]], cp: fy.b[1], c: ft.b[1] };
      };
      const F0 = fitAll(null); need(F0, '변인끼리 완전히 겹쳐 계산할 수 없습니다.');
      const stat = f => { const i1 = f.a[0] * f.b[0], i2 = f.a[0] * f.d21 * f.b[1], i3 = f.a[1] * f.b[1]; return [i1, i2, i3, i1 + i2 + i3, i1 - i2, i1 - i3, i2 - i3]; };
      const bs = await bootstrap(n, +o.boot || 5000, idx => { const f = fitAll(idx); return f ? stat(f) : null; }, onProg); need(bs, '부트스트랩 재표집이 하나도 계산되지 않았습니다. 표본이 너무 작거나 변인끼리 겹칩니다.');
      const est = stat(F0), sx = X.bin ? 1 : sd(X.x), sy = sd(y);   // X가 두 집단(0/1)이면 부분표준화(÷ SD(Y)), Hayes 권고
      const nx = P.V[r.x].name, n1 = P.V[r.m1].name, n2 = P.V[r.m2].name, ny = P.V[r.y].name;
      const R = newResult('med_serial', `순차매개: ${nx} → ${n1} → ${n2} → ${ny}`); R.n = n; bootNote(R, bs);
      pathTables(R, P, F0, nx, ny, [n1, n2], r.covs, X.x, y, [m1, m2], true); addFig(R, medFig(F0, nx, ny, [n1, n2], true));
      const lab = [`${nx} → ${n1} → ${ny}`, `${nx} → ${n1} → ${n2} → ${ny}`, `${nx} → ${n2} → ${ny}`, '총 간접효과', '대비: (1) − (2)', '대비: (1) − (3)', '대비: (2) − (3)'];
      R.tables.push({ title: `간접효과 (부트스트랩 ${(+o.boot || 5000).toLocaleString()}번, 95% 백분위 신뢰구간)`, head: ['경로', '효과', 'Boot SE', '95% CI', X.bin ? '부분표준화 효과' : '완전표준화 효과'], num: [1, 2, 3, 4], foot: X.bin ? `${nx}가 두 집단(${X.note})이라 효과 ÷ SD(${ny})로 구한 부분표준화 효과입니다(Hayes 권고).` : '',
        rows: lab.map((l, j) => [j < 3 ? `(${j + 1}) ${l}` : l, F2(est[j], 3), F2(bs[j].se, 3), CI(bs[j].lo, bs[j].hi, 3), j < 4 ? F2(est[j] * sx / sy, 3) : '']) });
      R.tables.push(effectsTable(F0, nx, ny));
      lab.slice(0, 4).forEach((l, j) => eff(R, { label: j < 3 ? `간접효과: ${l}` : `총 간접효과: ${nx} → ${ny}`, est: est[j], lo: bs[j].lo, hi: bs[j].hi, ciBased: true, std: est[j] * sx / sy, stat: `B = ${F2(est[j], 3)}, 95% CI ${CI(bs[j].lo, bs[j].hi, 3)}` }));
      pathEffects(R, F0, nx, ny, [n1, n2], true);
      medNotes(R, X, P); return R;
    },

    sem(r, o, onProg) { return runSem('par', r, o, onProg); },
    sem_serial(r, o, onProg) { return runSem('serial', r, o, onProg); },

    async moderation(r, o) {
      const P = prep({ x: r.x, w: r.w, y: r.y, c: r.covs });
      const X = binCode(P, r.x), W = binCode(P, r.w), y = P.col(r.y), C = designCols(P, r.covs), n = P.n;
      const xc = o.center && !X.bin ? center(X.x) : X.x, wc = o.center && !W.bin ? center(W.x) : W.x, xw = xc.map((v, i) => v * wc[i]);
      const fit = ols(y, rowsOf([xc, wc, xw, ...C.cols])), red = ols(y, rowsOf([xc, wc, ...C.cols])); need(fit && red, '변인끼리 완전히 겹쳐 계산할 수 없습니다.');
      const nx = P.V[r.x].name, nw = P.V[r.w].name, ny = P.V[r.y].name;
      const dR = fit.R2 - red.R2, dF = dR / ((1 - fit.R2) / fit.dfe), dp = ST.pF(dF, 1, fit.dfe);
      const R = newResult('moderation', `조절효과: ${nx} × ${nw} → ${ny}`); R.n = n;
      const names = [nx, nw, `${nx} × ${nw}`, ...C.names];
      R.tables.push({ title: `종속변인: ${ny}`, head: ['변인', 'B', 'SE', 't', 'p', '95% CI'], num: [1, 2, 3, 4, 5],
        rows: [['(상수)', F2(fit.b[0]), F2(fit.se[0]), F2(fit.t[0]), PV(fit.p[0]), CI(fit.lo[0], fit.hi[0])]].concat(names.map((nm, j) => [nm, F2(fit.b[j + 1]), F2(fit.se[j + 1]), F2(fit.t[j + 1]), PV(fit.p[j + 1]), CI(fit.lo[j + 1], fit.hi[j + 1])])) });
      R.notes.push('모형: ' + fitLine(fit) + `. 상호작용항 추가에 따른 ΔR² = ${NZ(dR, 3)}, F(1, ${fit.dfe}) = ${F2(dF)}, ${PS(dp)}.`);
      // 단순기울기
      const sw = sd(wc), mw = mean(wc);
      const pts = W.bin ? [[`${nw} = ${P.V[r.w].levels[0]}`, 0], [`${nw} = ${P.V[r.w].levels[1]}`, 1]] : [['M − 1SD', mw - sw], ['M', mw], ['M + 1SD', mw + sw]];
      const tc = ST.tInv(0.05, fit.dfe), V = fit.cov;
      const ss = pts.map(([l, w]) => { const b = fit.b[1] + fit.b[3] * w, se = Math.sqrt(V[1][1] + w * w * V[3][3] + 2 * w * V[1][3]), t = b / se; return { l, w, b, se, t, p: ST.pT(Math.abs(t), fit.dfe), lo: b - tc * se, hi: b + tc * se }; });
      R.tables.push({ title: `단순기울기: ${nw} 수준별 ${nx}의 효과${o.center && !W.bin ? ' (W는 평균중심화 값)' : ''}`, head: [`${nw} 수준`, 'W 값', 'B', 'SE', 't', 'p', '95% CI'], num: [1, 2, 3, 4, 5, 6], rows: ss.map(q => [q.l, F2(q.w), F2(q.b), F2(q.se), F2(q.t), PV(q.p), CI(q.lo, q.hi)]) });
      { // 단순기울기 그림: X를 평균 ± 1SD(두 집단이면 0·1)에서, 공변인은 평균에서 예측
        const mx = mean(X.x), sx = sd(X.x), xL = X.bin ? 0 : -sx, xH = X.bin ? 1 : sx, cAdd = C.cols.reduce((t, c, j) => t + fit.b[4 + j] * mean(c), 0);
        const toC = v => o.center && !X.bin ? v : v + (X.bin ? 0 : mx), pred = (xv, w) => fit.b[0] + fit.b[1] * xv + fit.b[2] * w + fit.b[3] * xv * w + cAdd;
        const xa = toC(xL), xb = toC(xH);
        addFig(R, { type: 'slopes', title: `${nw} 수준에 따른 ${J(nx, '과/와')} ${ny}의 관계`, xLabel: nx, yLabel: ny, yRange: scoreRange(r.y), wName: nw, xs: [0, 1],
          xTicks: X.bin ? [[0, P.V[r.x].levels[0]], [1, P.V[r.x].levels[1]]] : [[0, `낮음 (M − 1SD${o.center ? '' : `, ${F2(mx - sx)}`})`], [1, `높음 (M + 1SD${o.center ? '' : `, ${F2(mx + sx)}`})`]],
          lines: ss.map(q => ({ name: q.l, y0: pred(xa, q.w), y1: pred(xb, q.w), b: q.b, p: q.p })), note: `선마다 ${nw}가 그 수준일 때 ${nx}의 효과(단순기울기)입니다. 선의 기울기가 서로 다르면 조절효과가 있다는 뜻입니다.${C.cols.length ? ' 공변인은 평균에서 고정했습니다.' : ''}` }); }
      // Johnson-Neyman
      if (!W.bin) {
        const a = fit.b[3] ** 2 - tc * tc * V[3][3], bq = 2 * (fit.b[1] * fit.b[3] - tc * tc * V[1][3]), cq = fit.b[1] ** 2 - tc * tc * V[1][1], disc = bq * bq - 4 * a * cq;
        const lo = Math.min(...wc), hi = Math.max(...wc);
        const roots = disc >= 0 && a !== 0 ? [(-bq - Math.sqrt(disc)) / (2 * a), (-bq + Math.sqrt(disc)) / (2 * a)].filter(v => v >= lo && v <= hi) : [];
        R.notes.push(roots.length ? `Johnson–Neyman 경계: W = ${roots.map(v => F2(v)).join(', ')}${o.center ? ' (중심화 값, 원점수로는 ' + roots.map(v => F2(v + mean(W.x))).join(', ') + ')' : ''}. 이 값을 기준으로 X의 효과가 유의해지거나 유의하지 않게 됩니다.` : 'Johnson–Neyman: 관찰된 W 범위 안에서 X의 효과가 유의성이 바뀌는 지점이 없습니다.');
      }
      if (o.center) R.notes.push('점수 변인은 평균중심화한 뒤 곱했습니다. X와 W의 계수는 상대 변인이 평균일 때의 효과입니다.');
      [X, W].forEach(b => b.note && R.notes.push(b.note));
      eff(R, { label: `상호작용: ${nx} × ${nw} → ${ny} (조절효과)`, est: fit.b[3], p: fit.p[3], lo: fit.lo[3], hi: fit.hi[3], stat: `B = ${F2(fit.b[3])}, t(${fit.dfe}) = ${F2(fit.t[3])}, ${PS(fit.p[3])}, ΔR² = ${NZ(dR, 3)}` });
      eff(R, { label: `${nx} → ${ny} (주효과)`, est: fit.b[1], p: fit.p[1], lo: fit.lo[1], hi: fit.hi[1], stat: `B = ${F2(fit.b[1])}, t(${fit.dfe}) = ${F2(fit.t[1])}, ${PS(fit.p[1])}` });
      ss.forEach(q => eff(R, { label: `단순기울기: ${nw} ${q.l}에서 ${nx} → ${ny}`, est: q.b, p: q.p, lo: q.lo, hi: q.hi, stat: `B = ${F2(q.b)}, t(${fit.dfe}) = ${F2(q.t)}, ${PS(q.p)}` }));
      dropNote(R, P); return R;
    },

    async modmed7(r, o, onProg) { return modmed(r, o, onProg, 7); },
    async modmed14(r, o, onProg) { return modmed(r, o, onProg, 14); }
  };

  async function modmed(r, o, onProg, model) {
    const P = prep({ x: r.x, m: r.m, w: r.w, y: r.y, c: r.covs });
    const X = binCode(P, r.x), Wb = binCode(P, r.w), m = P.col(r.m), y = P.col(r.y), C = designCols(P, r.covs).cols, n = P.n;
    const cen = (b) => (o.center && !b.bin ? center(b.x) : b.x);
    const xv = cen(X), wv = cen(Wb), mv = o.center ? center(m) : m;
    const sw = sd(wv), mw = mean(wv);
    const pts = Wb.bin ? [0, 1] : [mw - sw, mw, mw + sw];
    const ptLab = Wb.bin ? P.V[r.w].levels.slice(0, 2).map(l => `${P.V[r.w].name} = ${l}`) : ['M − 1SD', 'M', 'M + 1SD'];
    const fitAll = idx => {
      const pick = a => idx ? idx.map(i => a[i]) : a; const f = !!idx;
      const x = pick(xv), w = pick(wv), mm = pick(model === 14 ? mv : m), yy = pick(y), cc = C.map(pick);
      if (model === 7) {
        const fm = ols(mm, rowsOf([x, w, x.map((v, i) => v * w[i]), ...cc]), f), fy = ols(yy, rowsOf([x, mm, ...cc]), f);
        if (!fm || !fy) return null; return { fm, fy, a1: fm.b[1], a3: fm.b[3], b: fy.b[2], cp: fy.b[1] };
      }
      const fm = ols(mm, rowsOf([x, ...cc]), f), fy = ols(yy, rowsOf([x, mm, w, mm.map((v, i) => v * w[i]), ...cc]), f);
      if (!fm || !fy) return null; return { fm, fy, a: fm.b[1], b1: fy.b[2], b3: fy.b[4], cp: fy.b[1] };
    };
    const stat = f => model === 7 ? [f.a3 * f.b, ...pts.map(w => (f.a1 + f.a3 * w) * f.b)] : [f.a * f.b3, ...pts.map(w => f.a * (f.b1 + f.b3 * w))];
    const F0 = fitAll(null); need(F0, '변인끼리 완전히 겹쳐 계산할 수 없습니다.');
    const bs = await bootstrap(n, +o.boot || 5000, idx => { const f = fitAll(idx); return f ? stat(f) : null; }, onProg); need(bs, '부트스트랩 재표집이 하나도 계산되지 않았습니다. 표본이 너무 작거나 변인끼리 겹칩니다.');
    const est = stat(F0);
    const nx = P.V[r.x].name, nm = P.V[r.m].name, nw = P.V[r.w].name, ny = P.V[r.y].name;
    const R = newResult('modmed' + model, `조절된 매개 (Model ${model}): ${nx} → ${nm} → ${ny}, 조절: ${nw}`); R.n = n; bootNote(R, bs);
    const tab = (fit, names, title) => ({ title, head: ['변인', 'B', 'SE', 't', 'p', '95% CI'], num: [1, 2, 3, 4, 5], rows: [['(상수)', F2(fit.b[0]), F2(fit.se[0]), F2(fit.t[0]), PV(fit.p[0]), CI(fit.lo[0], fit.hi[0])]].concat(names.map((nmx, j) => [nmx, F2(fit.b[j + 1]), F2(fit.se[j + 1]), F2(fit.t[j + 1]), PV(fit.p[j + 1]), CI(fit.lo[j + 1], fit.hi[j + 1])])) });
    const cn = designCols(P, r.covs).names;
    if (model === 7) { R.tables.push(tab(F0.fm, [nx, nw, `${nx} × ${nw}`, ...cn], `종속변인: ${nm} (매개변인 모형)`)); R.tables.push(tab(F0.fy, [nx, nm, ...cn], `종속변인: ${ny}`)); }
    else { R.tables.push(tab(F0.fm, [nx, ...cn], `종속변인: ${nm} (매개변인 모형)`)); R.tables.push(tab(F0.fy, [nx, nm, nw, `${nm} × ${nw}`, ...cn], `종속변인: ${ny}`)); }
    R.tables.push({ title: `조건부 간접효과와 조절된 매개 지수 (부트스트랩 ${(+o.boot || 5000).toLocaleString()}번)`, head: ['', '효과', 'Boot SE', '95% CI'], num: [1, 2, 3],
      rows: [['조절된 매개 지수', F2(est[0], 3), F2(bs[0].se, 3), CI(bs[0].lo, bs[0].hi, 3)]].concat(ptLab.map((l, j) => [`간접효과 (${nw} ${l})`, F2(est[j + 1], 3), F2(bs[j + 1].se, 3), CI(bs[j + 1].lo, bs[j + 1].hi, 3)])) });
    const inter = model === 7 ? F0.fm : F0.fy, ii = model === 7 ? 3 : 4;
    { const k = (f, j) => ({ est: f.b[j], p: f.p[j] });
      addFig(R, { type: 'path', kind: 'med', title: `조절된 매개 모형 검증 결과 (PROCESS Model ${model})`, X: nx, Y: ny, Ms: [nm], W: nw, wOn: model === 7 ? 'a' : 'b',
        a: [k(F0.fm, 1)], b: [k(F0.fy, 2)], cp: k(F0.fy, 1), c: null, inter: k(inter, ii),
        note: `계수는 비표준화 계수(B)입니다. ${nw}에서 나온 화살표는 ${model === 7 ? `${nx} → ${nm}` : `${nm} → ${ny}`} 경로의 크기를 바꾸는 상호작용(${model === 7 ? 'a3' : 'b3'})입니다. *p < .05, **p < .01, ***p < .001. 조절된 매개의 유의성은 표의 조절된 매개 지수와 그 부트스트랩 신뢰구간으로 판단합니다.` }); }
    R.notes.push(`상호작용항: B = ${F2(inter.b[ii])}, t(${inter.dfe}) = ${F2(inter.t[ii])}, ${PS(inter.p[ii])}.`);
    if (o.center) R.notes.push('점수 변인은 평균중심화한 뒤 곱했습니다. W 수준은 중심화 값 기준입니다.');
    [X, Wb].forEach(b => b.note && R.notes.push(b.note));
    eff(R, { label: `조절된 매개 지수: ${J(nw, '이/가')} ${nx} → ${nm} → ${ny} 간접효과를 조절`, est: est[0], lo: bs[0].lo, hi: bs[0].hi, ciBased: true, stat: `Index = ${F2(est[0], 3)}, 95% CI ${CI(bs[0].lo, bs[0].hi, 3)}` });
    ptLab.forEach((l, j) => eff(R, { label: `조건부 간접효과 (${nw} ${l}): ${nx} → ${nm} → ${ny}`, est: est[j + 1], lo: bs[j + 1].lo, hi: bs[j + 1].hi, ciBased: true, stat: `B = ${F2(est[j + 1], 3)}, 95% CI ${CI(bs[j + 1].lo, bs[j + 1].hi, 3)}` }));
    eff(R, { label: `상호작용: ${model === 7 ? nx : nm} × ${nw} → ${model === 7 ? nm : ny}`, est: inter.b[ii], p: inter.p[ii], lo: inter.lo[ii], hi: inter.hi[ii], stat: `B = ${F2(inter.b[ii])}, t(${inter.dfe}) = ${F2(inter.t[ii])}, ${PS(inter.p[ii])}` });
    dropNote(R, P); return R;
  }

  function corrMatrix(keys, spearman) {
    const vs = keys.map(varByKey);
    const R = newResult(spearman ? 'spearman' : 'pearson', `${spearman ? '스피어만' : '피어슨'} 상관: ${vs.map(v => v.name).join(', ')}`);
    const n0 = lastScored.inc.length;
    const pair = (a, b) => { const idx = []; for (let i = 0; i < n0; i++) if (a.vals[i] != null && b.vals[i] != null) idx.push(i); let x = idx.map(i => a.vals[i]), y = idx.map(i => b.vals[i]); if (spearman) { x = ranks(x); y = ranks(y); }
      // 계산할 수 없는 경우를 숨기지 않음: 사람이 3명 미만 / 한쪽 응답이 모두 같음(분산 0)
      if (idx.length < 3) return { r: NaN, n: idx.length, p: NaN, df: idx.length - 2, why: `두 변인에 모두 응답한 사람이 ${idx.length}명뿐` };
      const cst = [[a, x], [b, y]].filter(([, v]) => !(sd(v) > 0)).map(([w]) => w.name);
      if (cst.length) return { r: NaN, n: idx.length, p: NaN, df: idx.length - 2, why: `${cst.join(', ')}의 응답이 모두 같음(분산 0)` };
      const r = Math.max(-1, Math.min(1, corr(x, y))), df = idx.length - 2, t = Math.abs(r) >= 1 ? Infinity : r * Math.sqrt(df / (1 - r * r));
      return { r, n: idx.length, p: Math.abs(r) >= 1 ? 0 : ST.pT(Math.abs(t), df), df }; };
    const bad = [], bfr = [];
    const sym = spearman ? 'ρ' : 'r';
    const rows = vs.map((a, i) => [`${i + 1}. ${a.name}`, F2(mean(a.vals.filter(v => v != null))), F2(sd(a.vals.filter(v => v != null)))].concat(vs.map((b, j) => {
      if (j > i) return ''; if (j === i) return '—';
      const c = pair(a, b);
      if (c.why) { bad.push(`${a.name} – ${b.name}: ${c.why}`); return '계산 불가'; }
      if (Math.abs(c.r) > 0.999) bad.push(`${a.name} – ${b.name}: ${sym} = ${NZ(c.r, 3)}로 사실상 같은 변인입니다. 같은 문항으로 만든 점수가 아닌지 확인하세요`);
      const lbf = spearman ? undefined : BAYES.r(c.r, c.n); if (!spearman) bfr.push([`${a.name} – ${b.name}`, lbf]);
      eff(R, { label: `${a.name} – ${b.name} 상관`, est: c.r, p: c.p, es: c.r, esKind: 'r', lbf, stat: `${sym} = ${NZ(c.r)}, ${PS(c.p)}${c.n !== n0 ? `, n = ${c.n}` : ''}` });
      return `${NZ(c.r)}${STAR(c.p)}`;
    })));
    const nP = []; vs.forEach((a, i) => vs.forEach((b, j) => { if (j < i) nP.push(pair(a, b).n); }));
    const nLo = nP.length ? Math.min(...nP) : n0; R.n = nLo;   // 결과 카드의 N = 짝별 N의 최소 (표 제목과 같게)
    R.tables.push({ title: `${spearman ? '스피어만 순위' : '피어슨'} 상관계수 (N = ${nLo < n0 ? `${nLo}~${n0}, 빠진 값은 짝마다 제외` : n0})`, head: ['변인', 'M', 'SD'].concat(vs.map((_, i) => String(i + 1))), num: vs.map((_, i) => i + 1).concat([vs.length + 1, vs.length + 2]), rows });
    R.notes.push('* p < .05, ** p < .01, *** p < .001. 짝마다 두 변인 모두 응답한 사람으로 계산했습니다.');
    if (nLo < n0 && nLo < 0.9 * n0) R.notes.push(`⚠ 짝마다 분석한 사람 수가 ${nLo}~${n0}명으로 많이 다릅니다. 상관마다 다른 사람들로 계산된 셈이라, 결측이 몰린 변인이 없는지 확인하세요.`);
    bad.forEach(w => R.notes.push('⚠ ' + w + '.'));
    addBayes(R, bfr, '상관 ρ ~ 균등분포(−1, 1), 즉 stretched beta 폭 1. JASP 기본값');
    if (vs.length === 2) { const a = vs[0], b = vs[1], idx = []; for (let i = 0; i < n0; i++) if (a.vals[i] != null && b.vals[i] != null) idx.push(i); const xx = idx.map(i => a.vals[i]), yy = idx.map(i => b.vals[i]), c = pair(a, b), bb = sd(xx) > 0 ? corr(xx, yy) * sd(yy) / sd(xx) : 0;
      addFig(R, { type: 'scatter', title: `${J(a.name, '과/와')} ${b.name}의 관계`, xLabel: a.name, yLabel: b.name, xRange: scoreRange(a.key), yRange: scoreRange(b.key), pts: thinPts(idx.map((_, k) => [xx[k], yy[k]])), line: spearman ? null : [mean(yy) - bb * mean(xx), bb], tag: `${sym} = ${NZ(c.r)}${STAR(c.p)}`, note: `${spearman ? '' : '선은 회귀선입니다. '}같은 자리에 겹친 사람이 많을수록 점이 큽니다.` }); }
    else if (vs.length >= 3) { const M = vs.map(a => vs.map(b => a === b ? { r: 1, p: 0 } : pair(a, b)));
      addFig(R, { type: 'corrmat', title: `변인 간 ${spearman ? '스피어만 순위' : '피어슨'} 상관`, names: vs.map(v => v.name), sym, r: M.map(rw => rw.map(c => c.r)), p: M.map(rw => rw.map(c => c.p)), note: '파랑은 부적(−), 빨강은 정적(+) 상관이고 색이 진할수록 큽니다. * p < .05, ** p < .01, *** p < .001.' }); }
    return R;
  }

  // 독립 두 집단: Student t(등분산 가정)와 Welch t를 함께 계산
  function twoSample(a, b) {
    const ma = mean(a), mb = mean(b), va = variance(a), vb = variance(b), na = a.length, nb = b.length;
    const sp = Math.sqrt(((na - 1) * va + (nb - 1) * vb) / (na + nb - 2));
    const tS = (ma - mb) / (sp * Math.sqrt(1 / na + 1 / nb)), dfS = na + nb - 2;
    const seW = Math.sqrt(va / na + vb / nb), tW = (ma - mb) / seW, dfW = (va / na + vb / nb) ** 2 / ((va / na) ** 2 / (na - 1) + (vb / nb) ** 2 / (nb - 1));
    return { ma, mb, va, vb, na, nb, sp, tS, dfS, pS: ST.pT(Math.abs(tS), dfS), tW, dfW, pW: ST.pT(Math.abs(tW), dfW), d: (ma - mb) / sp };
  }
  function levene(G) {
    const Z = G.map(x => { const m = mean(x); return x.map(v => Math.abs(v - m)); }); const all = Z.flat(), N = all.length, k = G.length, gm = mean(all);
    const ssb = Z.reduce((s, z) => s + z.length * (mean(z) - gm) ** 2, 0), ssw = Z.reduce((s, z) => s + z.reduce((q, v) => q + (v - mean(z)) ** 2, 0), 0);
    const F = (ssb / (k - 1)) / (ssw / (N - k)); return { F, p: ST.pF(F, k - 1, N - k) };
  }
  function dropNote(R, P) { if (P.dropped) R.notes.push(`분석에 쓴 변인 중 하나라도 비어 있는 ${P.dropped}명을 빼고 ${P.n}명으로 계산했습니다.`); }
  function medNotes(R, X, P) {
    R.notes.push('간접효과는 부트스트랩 95% 신뢰구간이 0을 포함하지 않으면 유의하다고 판단합니다. 재표집 시작값을 고정해 같은 자료는 언제나 같은 결과가 나옵니다.');
    if (X.note) R.notes.push(X.note); dropNote(R, P);
  }
  function pathTables(R, P, F0, nx, ny, nms, covKeys, x, y, Ms, serial) {
    const cn = designCols(P, covKeys).names;
    F0.aF.forEach((f, j) => {
      const names = [nx].concat(serial && j === 1 ? [nms[0]] : [], cn);
      R.tables.push({ title: `종속변인: ${nms[j]}`, head: ['변인', 'B', 'SE', 't', 'p', '95% CI'], num: [1, 2, 3, 4, 5], rows: [['(상수)', F2(f.b[0]), F2(f.se[0]), F2(f.t[0]), PV(f.p[0]), CI(f.lo[0], f.hi[0])]].concat(names.map((nm, q) => [nm, F2(f.b[q + 1]), F2(f.se[q + 1]), F2(f.t[q + 1]), PV(f.p[q + 1]), CI(f.lo[q + 1], f.hi[q + 1])])), foot: fitLine(f) });
    });
    const f = F0.yF, names = [nx, ...nms, ...cn];
    R.tables.push({ title: `종속변인: ${ny}`, head: ['변인', 'B', 'SE', 't', 'p', '95% CI'], num: [1, 2, 3, 4, 5], rows: [['(상수)', F2(f.b[0]), F2(f.se[0]), F2(f.t[0]), PV(f.p[0]), CI(f.lo[0], f.hi[0])]].concat(names.map((nm, q) => [nm, F2(f.b[q + 1]), F2(f.se[q + 1]), F2(f.t[q + 1]), PV(f.p[q + 1]), CI(f.lo[q + 1], f.hi[q + 1])])), foot: fitLine(f) });
  }
  // 매개 경로 그림의 계수 (비표준화 B와 p)
  function medFig(F0, nx, ny, nms, serial) {
    const k = (f, j) => ({ est: f.b[j], p: f.p[j] });
    return { type: 'path', kind: serial ? 'serial' : 'med', title: `${serial ? '순차' : nms.length > 1 ? '병렬 다중' : '단순'}매개 모형 검증 결과`, X: nx, Y: ny, Ms: nms,
      a: F0.aF.map(f => k(f, 1)), b: nms.map((_, j) => k(F0.yF, 2 + j)), d: serial ? k(F0.aF[1], 2) : null, cp: k(F0.yF, 1), c: k(F0.tF, 1),
      note: "계수는 비표준화 계수(B)입니다. 굵은 선은 p < .05인 경로, 점선은 유의하지 않은 직접효과입니다. 괄호 안 c는 총효과입니다. *p < .05, **p < .01, ***p < .001. 간접효과의 유의성은 표의 부트스트랩 신뢰구간으로 판단합니다." };
  }
  function effectsTable(F0, nx, ny) {
    const t = F0.tF, d = F0.yF;
    return { title: `총효과와 직접효과 (${nx} → ${ny})`, head: ['', 'B', 'SE', 't', 'p', '95% CI'], num: [1, 2, 3, 4, 5],
      rows: [['총효과 (c)', F2(t.b[1]), F2(t.se[1]), F2(t.t[1]), PV(t.p[1]), CI(t.lo[1], t.hi[1])], ["직접효과 (c')", F2(d.b[1]), F2(d.se[1]), F2(d.t[1]), PV(d.p[1]), CI(d.lo[1], d.hi[1])]] };
  }
  function pathEffects(R, F0, nx, ny, nms, serial) {
    F0.aF.forEach((f, j) => eff(R, { label: `경로: ${nx} → ${nms[j]}`, est: f.b[1], p: f.p[1], lo: f.lo[1], hi: f.hi[1], stat: `B = ${F2(f.b[1])}, t(${f.dfe}) = ${F2(f.t[1])}, ${PS(f.p[1])}` }));
    if (serial) { const f = F0.aF[1]; eff(R, { label: `경로: ${nms[0]} → ${nms[1]}`, est: f.b[2], p: f.p[2], lo: f.lo[2], hi: f.hi[2], stat: `B = ${F2(f.b[2])}, t(${f.dfe}) = ${F2(f.t[2])}, ${PS(f.p[2])}` }); }
    const d = F0.yF;
    nms.forEach((m, j) => eff(R, { label: `경로: ${m} → ${ny}`, est: d.b[2 + j], p: d.p[2 + j], lo: d.lo[2 + j], hi: d.hi[2 + j], stat: `B = ${F2(d.b[2 + j])}, t(${d.dfe}) = ${F2(d.t[2 + j])}, ${PS(d.p[2 + j])}` }));
    eff(R, { label: `직접효과: ${nx} → ${ny}`, est: d.b[1], p: d.p[1], lo: d.lo[1], hi: d.hi[1], stat: `B = ${F2(d.b[1])}, t(${d.dfe}) = ${F2(d.t[1])}, ${PS(d.p[1])}` });
    const t = F0.tF; eff(R, { label: `총효과: ${nx} → ${ny}`, est: t.b[1], p: t.p[1], lo: t.lo[1], hi: t.hi[1], stat: `B = ${F2(t.b[1])}, t(${t.dfe}) = ${F2(t.t[1])}, ${PS(t.p[1])}` });
  }

  function analysisCsvRows(a) {
    const rows = [[a.title], [`N = ${a.n}`, `계산일 ${a.at}`]];
    a.tables.forEach(t => { rows.push([]); rows.push([t.title]); rows.push(t.head); t.rows.forEach(r => rows.push(r)); if (t.foot) rows.push([t.foot]); });
    if (a.notes.length) { rows.push([]); a.notes.forEach(n => rows.push([n])); }
    return rows;
  }

  // =================================================================
  // 결과 그래프 (논문 그림): 계산할 때 그림 자료를 결과에 함께 저장(R.figs)해서, 원자료 없이도 다시 그림
  // 화면은 지금 테마 색, 받기는 흰 바탕(컬러·흑백). 계열은 색과 함께 선 모양·점 모양으로도 구분(흑백 인쇄·색각이상)
  // 색: 범주 3색(파랑·주황·청록, 색각이상 시뮬레이션 검증), 상관 행렬은 파랑↔빨강 + 가운데 회색
  // =================================================================
  const FIG_FONT = '"Nanum Gothic","Apple SD Gothic Neo","Malgun Gothic","Noto Sans KR",sans-serif';
  const FIG_PAL = { light: ['#2a78d6', '#eb6834', '#1baf7a'], dark: ['#3987e5', '#d95926', '#199e70'], mono: ['#1f1f1f', '#6b6b6b', '#a5a5a5'] };
  const FIG_DASH = ['', '7 4', '2 4'];
  const ciOf = x => { const n = x.length, m = mean(x), h = n > 1 ? ST.tInv(0.05, n - 1) * sd(x) / Math.sqrt(n) : NaN; return { m, lo: m - h, hi: m + h, n }; };
  // 비율의 95% 신뢰구간 (Wilson), 백분율로
  const wilsonPct = (k, n) => { const z = 1.96, p = k / n, d = 1 + z * z / n, c = (p + z * z / (2 * n)) / d, h = z * Math.sqrt(p * (1 - p) / n + z * z / (4 * n * n)) / d; return { m: p * 100, lo: (c - h) * 100, hi: (c + h) * 100, n }; };
  const addFig = (R, f) => { (R.figs = R.figs || []).push(f); };
  function scoreRange(key) { if (!/^S\d+$/.test(key || '')) return null; const s = sv.scales[+key.slice(1)]; if (!s) return null; const k = s.items.filter(i => norm(i.text)).length, P = +s.points; return sv.scoring === 'sum' ? [k, k * P] : [1, P]; }
  const unionRange = rs => { const v = rs.filter(Boolean); return v.length ? [Math.min(...v.map(r => r[0])), Math.max(...v.map(r => r[1]))] : null; };
  function niceTicks(lo, hi, n = 5) {
    if (!(hi > lo)) hi = lo + 1;
    const raw = (hi - lo) / n, mag = 10 ** Math.floor(Math.log10(raw)), st = [1, 2, 2.5, 5, 10].map(v => v * mag).find(v => v >= raw * 0.999);
    const out = []; for (let v = Math.floor(lo / st + 1e-9) * st; ; v += st) { out.push(+v.toFixed(10)); if (v >= hi - st * 1e-6 || out.length > 40) break; } return out;   // 마지막 눈금은 최댓값 이상
  }
  const figTw = (t, fs) => [...String(t)].reduce((w, c) => w + (/[ㄱ-힣]/.test(c) ? fs * 0.98 : /[A-Z0-9%]/.test(c) ? fs * 0.62 : c === ' ' ? fs * 0.3 : fs * 0.52), 0);
  const fx = v => esc(String(v));
  // 글자가 넘치면 두 줄로 (그래도 넘치면 …)
  function figWrap(t, maxW, fs) {
    t = String(t); if (figTw(t, fs) <= maxW) return [t];
    const ws = t.split(' '); let a = ''; const L = [];
    ws.forEach(w => { const nx = a ? a + ' ' + w : w; if (figTw(nx, fs) > maxW && a) { L.push(a); a = w; } else a = nx; }); L.push(a);
    const out = L.slice(0, 2); if (L.length > 2 || figTw(out[1] || '', fs) > maxW) { let x = out[out.length - 1]; while (x.length > 1 && figTw(x + '…', fs) > maxW) x = x.slice(0, -1); out[out.length - 1] = x + '…'; }
    return out;
  }
  const figText = (x, y, lines, o = {}) => `<text x="${x}" y="${y}" font-size="${o.fs || 12}" fill="${o.fill}" text-anchor="${o.anchor || 'middle'}"${o.weight ? ` font-weight="${o.weight}"` : ''}${o.rot ? ` transform="rotate(${o.rot} ${x} ${y})"` : ''}>${[].concat(lines).map((l, i) => `<tspan x="${x}" dy="${i ? (o.fs || 12) * 1.25 : 0}">${fx(l)}</tspan>`).join('')}</text>`;
  // 테마: 화면은 앱의 색 변수, 받기는 흰 바탕
  function figTheme(mode) {
    if (mode === 'print' || mode === 'mono') return { bg: '#ffffff', ink: '#111111', ink2: '#444444', ink3: '#777777', grid: '#e3e3e3', axis: '#9a9a9a', s: mode === 'mono' ? FIG_PAL.mono : FIG_PAL.light, mono: mode === 'mono', div: ['#2a78d6', '#f0efec', '#e34948'] };
    const cs = getComputedStyle(document.documentElement), v = k => cs.getPropertyValue(k).trim();
    const bg = v('--bg') || '#ffffff', dark = (() => { const m = bg.match(/^#?([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})/i); return m ? (parseInt(m[1], 16) + parseInt(m[2], 16) + parseInt(m[3], 16)) / 3 < 100 : false; })();
    return { bg, ink: v('--ink') || '#1d2333', ink2: v('--ink-2') || '#4a5168', ink3: v('--ink-3') || '#7a8196', grid: dark ? '#262b3b' : '#e8eaf0', axis: dark ? '#4a5168' : '#a9aebd', s: dark ? FIG_PAL.dark : FIG_PAL.light, mono: false, div: dark ? ['#3987e5', '#383835', '#e66767'] : ['#2a78d6', '#f0efec', '#e34948'] };
  }
  const figSvg = (T, W, H, body, label) => `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ${W} ${H}" width="${W}" height="${H}" style="max-width:${W}px" role="img" aria-label="${fx(label)}" font-family='${FIG_FONT}'><rect width="${W}" height="${H}" fill="${T.bg}"/>${body}</svg>`;
  // 점 모양: 원 · 네모 · 세모 (흑백에서도 계열 구분)
  function figMarker(T, i, x, y, r = 5) {
    const c = T.s[i % 3], ring = `stroke="${T.bg}" stroke-width="2"`;
    if (i % 3 === 1) return `<rect x="${x - r}" y="${y - r}" width="${2 * r}" height="${2 * r}" rx="1.5" fill="${c}" ${ring}/>`;
    if (i % 3 === 2) return `<path d="M${x} ${y - r * 1.2}L${x + r * 1.15} ${y + r * 0.85}L${x - r * 1.15} ${y + r * 0.85}Z" fill="${c}" ${ring} stroke-linejoin="round"/>`;
    return `<circle cx="${x}" cy="${y}" r="${r}" fill="${c}" ${ring}/>`;
  }
  // 범례 (계열이 둘 이상일 때만)
  // 범례: 넓이를 넘으면 다음 줄로. { svg, h } (h = 범례가 차지하는 높이)
  function figLegend(T, names, x0, y, kind, maxW = 560, head = '') {
    if (names.length < 2) return { svg: '', h: 0 };
    let x = x0, row = 0, out = head ? figText(x0, y + 4, head, { fill: T.ink2, anchor: 'start', fs: 12, weight: 700 }) : '';
    if (head) x += figTw(head, 12) + 12;
    names.forEach((n, i) => {
      const kw = kind === 'bar' ? 18 : 28, w = kw + figTw(n, 12) + 18;
      if (x > x0 + (head ? figTw(head, 12) + 12 : 0) && x + w - 18 > x0 + maxW) { row++; x = x0 + (head ? figTw(head, 12) + 12 : 0); }
      const yy = y + row * 20;
      const key = kind === 'bar' ? `<rect x="${x}" y="${yy - 6}" width="12" height="12" rx="2" fill="${T.s[i % 3]}"/>` : `<line x1="${x}" y1="${yy}" x2="${x + 22}" y2="${yy}" stroke="${T.s[i % 3]}" stroke-width="2" ${FIG_DASH[i % 3] ? `stroke-dasharray="${FIG_DASH[i % 3]}"` : ''}/>${figMarker(T, i, x + 11, yy, 4)}`;
      out += key + figText(x + kw, yy + 4, n, { fill: T.ink2, anchor: 'start', fs: 12 });
      x += w;
    });
    return { svg: out, h: (row + 1) * 20 };
  }
  const legTop = (T, names, L, R, kind, head) => { const lg = figLegend(T, names, L, 22, kind, R - L, head); return { lg, top: lg.h ? 22 + lg.h + 12 : 26 }; };
  // y축(눈금·격자·축 이름)
  function figYAxis(T, tk, yS, L, R, label, top, bot) {
    return tk.map(v => `<line x1="${L}" x2="${R}" y1="${yS(v)}" y2="${yS(v)}" stroke="${T.grid}" stroke-width="1"/>` + figText(L - 8, yS(v) + 4, F2(v, Number.isInteger(v) ? 0 : v * 10 % 1 ? 2 : 1), { fill: T.ink3, anchor: 'end', fs: 11.5 })).join('')
      + figText(16, (top + bot) / 2, figWrap(label, bot - top, 12.5), { fill: T.ink2, rot: -90, fs: 12.5 });
  }
  const tipOf = (t) => `<title>${fx(t)}</title>`;

  const FIGS = {
    // 평균과 95% 신뢰구간 (점 + 막대): 집단·변인 비교
    dots(f, T) {
      const W = 640, L = 72, R = W - 24, { lg, top } = legTop(T, f.series.map(s => s.name), L, R, 'line'), H = 354 + top, B = H - 70;
      const all = f.series.flatMap(s => s.pts).flatMap(p => [p.lo, p.hi, p.m]).filter(isFinite).concat(f.ref ? [f.ref.y] : []);
      let lo = Math.min(...all), hi = Math.max(...all); if (f.yRange) { lo = Math.min(lo, f.yRange[0]); hi = Math.max(hi, f.yRange[1]); } else { const pd = (hi - lo) * 0.15 || 1; lo -= pd; hi += pd; }
      const tk = niceTicks(lo, hi, 5), y0 = tk[0], y1 = tk[tk.length - 1], yS = v => B - (v - y0) / (y1 - y0) * (B - top);
      const nc = f.cats.length, bw = (R - L) / nc, ns = f.series.length, off = i => (i - (ns - 1) / 2) * Math.min(26, bw / (ns + 1));
      let g = figYAxis(T, tk, yS, L, R, f.yLabel, top, B);
      if (f.ref) g += `<line x1="${L}" x2="${R}" y1="${yS(f.ref.y)}" y2="${yS(f.ref.y)}" stroke="${T.ink3}" stroke-width="1.5"/>` + figText(R, yS(f.ref.y) - 6, f.ref.label, { fill: T.ink3, anchor: 'end', fs: 11.5 });
      f.cats.forEach((c, j) => { g += figText(L + bw * (j + 0.5), B + 20, figWrap(c, bw - 8, 12.5), { fill: T.ink2, fs: 12.5 }); });
      const few = nc * ns <= 6;
      f.series.forEach((s, i) => s.pts.forEach((p, j) => {
        if (!isFinite(p.m)) return; const x = L + bw * (j + 0.5) + off(i), c = T.s[i % 3];
        const tip = tipOf(`${ns > 1 ? s.name + ' · ' : ''}${f.cats[j]}: M = ${F2(p.m)}, 95% CI [${F2(p.lo)}, ${F2(p.hi)}], n = ${p.n}`);
        g += `<g>${tip}${isFinite(p.lo) ? `<line x1="${x}" x2="${x}" y1="${yS(p.lo)}" y2="${yS(p.hi)}" stroke="${c}" stroke-width="2" stroke-linecap="round"/><line x1="${x - 5}" x2="${x + 5}" y1="${yS(p.lo)}" y2="${yS(p.lo)}" stroke="${c}" stroke-width="2" stroke-linecap="round"/><line x1="${x - 5}" x2="${x + 5}" y1="${yS(p.hi)}" y2="${yS(p.hi)}" stroke="${c}" stroke-width="2" stroke-linecap="round"/>` : ''}${figMarker(T, i, x, yS(p.m), 5.5)}<rect x="${x - 12}" y="${yS(isFinite(p.hi) ? p.hi : p.m) - 6}" width="24" height="${Math.abs(yS(isFinite(p.lo) ? p.lo : p.m) - yS(isFinite(p.hi) ? p.hi : p.m)) + 12}" fill="transparent"/></g>`;
        if (few) g += figText(x + 10, yS(p.m) + 4, F2(p.m), { fill: T.ink, anchor: 'start', fs: 12 });
      }));
      g += lg.svg;
      return figSvg(T, W, H, g, f.title);
    },
    // 시점·조건별 평균을 이은 선 (반복측정, 상호작용, 비율 변화)
    lines(f, T) {
      const W = 640, L = 72, R = W - 24, { lg, top } = legTop(T, f.series.map(s => s.name), L, R, 'line'), H = 354 + top, B = H - 70;
      const all = f.series.flatMap(s => s.pts).flatMap(p => [p.lo, p.hi, p.m]).filter(isFinite);
      let lo = Math.min(...all), hi = Math.max(...all);
      if (f.pct) { lo = 0; hi = 100; } else if (f.yRange) { lo = Math.min(lo, f.yRange[0]); hi = Math.max(hi, f.yRange[1]); } else { const pd = (hi - lo) * 0.15 || 1; lo -= pd; hi += pd; }
      const tk = niceTicks(lo, hi, 5), y0 = tk[0], y1 = tk[tk.length - 1], yS = v => B - (v - y0) / (y1 - y0) * (B - top);
      const nx = f.xs.length, bw = (R - L) / nx, ns = f.series.length, off = i => (i - (ns - 1) / 2) * 7;
      let g = figYAxis(T, tk, yS, L, R, f.yLabel + (f.pct ? ' (%)' : ''), top, B);
      f.xs.forEach((c, j) => { g += figText(L + bw * (j + 0.5), B + 20, figWrap(c, bw - 8, 12.5), { fill: T.ink2, fs: 12.5 }); });
      if (f.xLabel) g += figText((L + R) / 2, H - 12, f.xLabel, { fill: T.ink2, fs: 12.5 });
      f.series.forEach((s, i) => {
        const c = T.s[i % 3], P = s.pts.map((p, j) => [L + bw * (j + 0.5) + off(i), yS(p.m)]);
        g += `<polyline points="${P.map(q => q.join(',')).join(' ')}" fill="none" stroke="${c}" stroke-width="2" stroke-linejoin="round" stroke-linecap="round" ${FIG_DASH[i % 3] ? `stroke-dasharray="${FIG_DASH[i % 3]}"` : ''}/>`;
        s.pts.forEach((p, j) => {
          const [x, y] = P[j];
          const tip = tipOf(`${ns > 1 ? s.name + ' · ' : ''}${f.xs[j]}: ${f.pct ? `${F2(p.m, 1)}%` : `M = ${F2(p.m)}`}${isFinite(p.lo) ? `, 95% CI [${F2(p.lo, f.pct ? 1 : 2)}, ${F2(p.hi, f.pct ? 1 : 2)}]` : ''}${p.n ? `, n = ${p.n}` : ''}`);
          g += `<g>${tip}${isFinite(p.lo) ? `<line x1="${x}" x2="${x}" y1="${yS(p.lo)}" y2="${yS(p.hi)}" stroke="${c}" stroke-width="1.5" stroke-linecap="round" opacity=".8"/>` : ''}${figMarker(T, i, x, y, 5)}<circle cx="${x}" cy="${y}" r="12" fill="transparent"/></g>`;
        });
      });
      g += lg.svg;
      return figSvg(T, W, H, g, f.title);
    },
    // 산점도 (+ 회귀선, 또는 잔차의 0선)
    scatter(f, T) {
      const W = 640, H = 420, top = 26, L = 72, R = W - 24, B = H - 64;
      const xs = f.pts.map(p => p[0]), ys = f.pts.map(p => p[1]);
      const rx = f.xRange || [Math.min(...xs), Math.max(...xs)], ry = f.yRange || [Math.min(...ys), Math.max(...ys)];
      const pad = (a, b) => (b - a) * 0.06 || 0.5;
      const tx = niceTicks(Math.min(rx[0], Math.min(...xs)) - (f.xRange ? 0 : pad(...rx)), Math.max(rx[1], Math.max(...xs)) + (f.xRange ? 0 : pad(...rx)), 6), ty = niceTicks(Math.min(ry[0], Math.min(...ys)) - (f.yRange ? 0 : pad(...ry)), Math.max(ry[1], Math.max(...ys)) + (f.yRange ? 0 : pad(...ry)), 5);
      const xS = v => L + (v - tx[0]) / (tx[tx.length - 1] - tx[0]) * (R - L), yS = v => B - (v - ty[0]) / (ty[ty.length - 1] - ty[0]) * (B - top);
      let g = figYAxis(T, ty, yS, L, R, f.yLabel, top, B);
      g += tx.map(v => `<line x1="${xS(v)}" x2="${xS(v)}" y1="${B}" y2="${B + 4}" stroke="${T.axis}"/>` + figText(xS(v), B + 18, F2(v, Number.isInteger(v) ? 0 : 1), { fill: T.ink3, fs: 11.5 })).join('');
      g += `<line x1="${L}" x2="${R}" y1="${B}" y2="${B}" stroke="${T.axis}"/>` + figText((L + R) / 2, H - 12, f.xLabel, { fill: T.ink2, fs: 12.5 });
      if (f.zero) g += `<line x1="${L}" x2="${R}" y1="${yS(0)}" y2="${yS(0)}" stroke="${T.ink3}" stroke-width="1.5"/>`;
      // 같은 자리에 겹친 점은 하나로 그리고 크기로 사람 수 표시 (평균 점수는 같은 값이 많음)
      const cnt = new Map(); f.pts.forEach(([a, b]) => { const k = `${a.toFixed(3)}|${b.toFixed(3)}`; cnt.set(k, (cnt.get(k) || 0) + 1); });
      g += [...cnt.entries()].map(([k, n]) => { const [a, b] = k.split('|').map(Number); return `<circle cx="${xS(a)}" cy="${yS(b)}" r="${Math.min(9, 3.2 + Math.sqrt(n - 1) * 1.6)}" fill="${T.s[0]}" fill-opacity="${T.mono ? 0.45 : 0.5}" stroke="${T.bg}" stroke-width="1">${tipOf(`${f.xLabel} ${F2(a)}, ${f.yLabel} ${F2(b)}${n > 1 ? ` (${n}명)` : ''}`)}</circle>`; }).join('');
      if (f.line) { const x0 = tx[0], x1 = tx[tx.length - 1], cl = v => Math.max(ty[0], Math.min(ty[ty.length - 1], v)); g += `<line x1="${xS(x0)}" x2="${xS(x1)}" y1="${yS(cl(f.line[0] + f.line[1] * x0))}" y2="${yS(cl(f.line[0] + f.line[1] * x1))}" stroke="${T.ink}" stroke-width="2" stroke-linecap="round"/>`; }
      if (f.tag) g += figText(R - 4, top + 14, f.tag, { fill: T.ink, anchor: 'end', fs: 12.5, weight: 700 });
      return figSvg(T, W, H, g, f.title);
    },
    // 상관 행렬 (아래 삼각형, 색 = 상관의 크기와 방향)
    corrmat(f, T) {
      const k = f.names.length, cell = Math.max(44, Math.min(70, 420 / k)), lab = Math.min(230, Math.max(...f.names.map((n, i) => figTw(`${i + 1}. ${n}`, 12.5))) + 14);
      const W = Math.max(420, lab + cell * k + 30), H = 40 + cell * k + 54, x0 = lab, y0 = 36;
      const mix = (a, b, t) => { const h = q => [1, 3, 5].map(i => parseInt(q.slice(i, i + 2), 16)); const A = h(a), Bc = h(b); return '#' + A.map((v, i) => Math.round(v + (Bc[i] - v) * t).toString(16).padStart(2, '0')).join(''); };
      const colr = r => r >= 0 ? mix(T.div[1], T.div[2], Math.min(1, r)) : mix(T.div[1], T.div[0], Math.min(1, -r));
      const lum = h => { const v = [1, 3, 5].map(i => parseInt(h.slice(i, i + 2), 16) / 255); return 0.299 * v[0] + 0.587 * v[1] + 0.114 * v[2]; };
      let g = f.names.map((n, i) => figText(x0 - 10, y0 + cell * (i + 0.5) + 4, `${i + 1}. ${n}`.length > 22 ? `${i + 1}. ${n}`.slice(0, 21) + '…' : `${i + 1}. ${n}`, { fill: T.ink2, anchor: 'end', fs: 12.5 })).join('')
        + f.names.map((_, j) => figText(x0 + cell * (j + 0.5), y0 - 10, String(j + 1), { fill: T.ink2, fs: 12.5 })).join('');
      for (let i = 0; i < k; i++) for (let j = 0; j <= i; j++) {
        const x = x0 + cell * j, y = y0 + cell * i;
        if (i === j) { g += `<rect x="${x + 1}" y="${y + 1}" width="${cell - 2}" height="${cell - 2}" rx="4" fill="${T.grid}"/>` + figText(x + cell / 2, y + cell / 2 + 4, '—', { fill: T.ink3, fs: 12 }); continue; }
        const r = f.r[i][j], p = f.p[i][j], c = isFinite(r) ? (T.mono ? mix('#f2f2f2', '#3a3a3a', Math.abs(r)) : colr(r)) : T.grid;
        g += `<g>${tipOf(`${f.names[i]} – ${f.names[j]}: ${f.sym} = ${NZ(r)}, ${PS(p)}`)}<rect x="${x + 1}" y="${y + 1}" width="${cell - 2}" height="${cell - 2}" rx="4" fill="${c}"/>${figText(x + cell / 2, y + cell / 2 + 4, isFinite(r) ? NZ(r) + STAR(p) : '-', { fill: lum(c) < 0.5 ? '#ffffff' : '#111111', fs: 12.5 })}</g>`;
      }
      // 색 막대 (−1 ~ +1)
      const lx = x0, ly = H - 34, lw = Math.min(260, cell * k);
      for (let t = 0; t < 40; t++) { const r = -1 + 2 * t / 39; g += `<rect x="${lx + lw * t / 40}" y="${ly}" width="${lw / 40 + 0.5}" height="10" fill="${T.mono ? mix('#f2f2f2', '#3a3a3a', Math.abs(r)) : colr(r)}"/>`; }
      g += figText(lx, ly + 24, '−1', { fill: T.ink3, fs: 11.5 }) + figText(lx + lw / 2, ly + 24, '0', { fill: T.ink3, fs: 11.5 }) + figText(lx + lw, ly + 24, '+1', { fill: T.ink3, fs: 11.5 });
      return figSvg(T, W, H, g, f.title);
    },
    // 조절효과: 조절변인 수준별 단순기울기
    slopes(f, T) {
      const W = 640, L = 72, R = W - 24, { lg, top } = legTop(T, f.lines.map(l => `${l.name} (B = ${F2(l.b)}${STAR(l.p)})`), L, R, 'line', `${f.wName}:`), H = 348 + top, B = H - 64;
      const ys = f.lines.flatMap(l => [l.y0, l.y1]);
      let lo = Math.min(...ys), hi = Math.max(...ys); if (f.yRange) { lo = Math.min(lo, f.yRange[0]); hi = Math.max(hi, f.yRange[1]); } else { const pd = (hi - lo) * 0.2 || 0.5; lo -= pd; hi += pd; }
      const ty = niceTicks(lo, hi, 5), yS = v => B - (v - ty[0]) / (ty[ty.length - 1] - ty[0]) * (B - top);
      const xa = f.xs[0], xb = f.xs[1], xS = v => L + 40 + (v - xa) / (xb - xa || 1) * (R - L - 80);
      let g = figYAxis(T, ty, yS, L, R, f.yLabel, top, B);
      g += f.xTicks.map(([v, t]) => figText(xS(v), B + 20, t, { fill: T.ink2, fs: 12.5 })).join('') + figText((L + R) / 2, H - 12, f.xLabel, { fill: T.ink2, fs: 12.5 });
      f.lines.forEach((l, i) => {
        const c = T.s[i % 3];
        g += `<g>${tipOf(`${f.wName} ${l.name}: 기울기 B = ${F2(l.b)}, ${PS(l.p)}`)}<line x1="${xS(xa)}" x2="${xS(xb)}" y1="${yS(l.y0)}" y2="${yS(l.y1)}" stroke="${c}" stroke-width="2.5" stroke-linecap="round" ${FIG_DASH[i % 3] ? `stroke-dasharray="${FIG_DASH[i % 3]}"` : ''}/>${figMarker(T, i, xS(xa), yS(l.y0), 5)}${figMarker(T, i, xS(xb), yS(l.y1), 5)}<line x1="${xS(xa)}" x2="${xS(xb)}" y1="${yS(l.y0)}" y2="${yS(l.y1)}" stroke="transparent" stroke-width="14"/></g>`;
      });
      g += lg.svg;
      return figSvg(T, W, H, g, f.title);
    },
    // 집단별 비율 (묶은 세로 막대)
    props(f, T) {
      const W = 640, L = 72, R = W - 24, { lg, top } = legTop(T, f.series.map(s => s.name), L, R, 'bar'), H = 354 + top, B = H - 70;
      const ty = [0, 20, 40, 60, 80, 100], yS = v => B - v / 100 * (B - top);
      const nc = f.cats.length, ns = f.series.length, bw = (R - L) / nc, cw = Math.min(24, (bw - 24) / ns - 2);
      let g = figYAxis(T, ty, yS, L, R, f.yLabel + ' (%)', top, B);
      f.cats.forEach((c, j) => { g += figText(L + bw * (j + 0.5), B + 20, figWrap(c, bw - 8, 12.5), { fill: T.ink2, fs: 12.5 }); });
      const lab = nc * ns <= 12;
      f.series.forEach((s, i) => s.vals.forEach((v, j) => {
        const x = L + bw * (j + 0.5) + (i - (ns - 1) / 2) * (cw + 2) - cw / 2, y = yS(v), h = B - y, rr = Math.min(4, h);
        g += `<g>${tipOf(`${f.cats[j]} · ${s.name}: ${F2(v, 1)}%`)}<path d="M${x} ${B}V${y + rr}Q${x} ${y} ${x + rr} ${y}H${x + cw - rr}Q${x + cw} ${y} ${x + cw} ${y + rr}V${B}Z" fill="${T.s[i % 3]}"/></g>`;
        if (lab && figTw(F2(v, 0), 11) < cw + 14) g += figText(x + cw / 2, y - 5, F2(v, 0), { fill: T.ink2, fs: 11 });
      }));
      g += `<line x1="${L}" x2="${R}" y1="${B}" y2="${B}" stroke="${T.axis}"/>` + lg.svg;
      return figSvg(T, W, H, g, f.title);
    },
    // 점수 분포 (히스토그램 + 같은 평균·표준편차의 정규분포 곡선)
    hist(f, T) {
      const W = 640, H = 340, top = 26, L = 72, R = W - 24, B = H - 60;
      const x = f.vals, n = x.length, lo = f.range ? f.range[0] : Math.min(...x), hi = f.range ? f.range[1] : Math.max(...x);
      const k = Math.max(6, Math.min(16, Math.ceil(Math.log2(n) + 1))), w = (hi - lo) / k || 1, cnt = new Array(k).fill(0);
      x.forEach(v => { cnt[Math.min(k - 1, Math.max(0, Math.floor((v - lo) / w)))]++; });
      const m = mean(x), s0 = sd(x), dens = v => n * w * Math.exp(-0.5 * ((v - m) / s0) ** 2) / (s0 * Math.sqrt(2 * Math.PI));
      const ymax = Math.max(...cnt, dens(m)), ty = niceTicks(0, ymax, 4), yS = v => B - v / ty[ty.length - 1] * (B - top), xS = v => L + (v - lo) / (hi - lo) * (R - L);
      let g = figYAxis(T, ty, yS, L, R, '사람 수', top, B);
      cnt.forEach((c, i) => { const xa = xS(lo + i * w) + 1, xb = xS(lo + (i + 1) * w) - 1, y = yS(c), rr = Math.min(3, B - y); if (c) g += `<g>${tipOf(`${F2(lo + i * w)} ~ ${F2(lo + (i + 1) * w)}: ${c}명`)}<path d="M${xa} ${B}V${y + rr}Q${xa} ${y} ${xa + rr} ${y}H${xb - rr}Q${xb} ${y} ${xb} ${y + rr}V${B}Z" fill="${T.s[0]}" fill-opacity="${T.mono ? 0.55 : 0.75}"/></g>`; });
      if (s0 > 0) { const P = Array.from({ length: 61 }, (_, i) => { const v = lo + (hi - lo) * i / 60; return `${xS(v)},${yS(dens(v))}`; }); g += `<polyline points="${P.join(' ')}" fill="none" stroke="${T.ink}" stroke-width="2"/>`; }
      g += niceTicks(lo, hi, 6).filter(v => v >= lo - 1e-9 && v <= hi + 1e-9).map(v => figText(xS(v), B + 18, F2(v, Number.isInteger(v) ? 0 : 1), { fill: T.ink3, fs: 11.5 })).join('');
      g += `<line x1="${L}" x2="${R}" y1="${B}" y2="${B}" stroke="${T.axis}"/>` + figText((L + R) / 2, H - 10, f.xLabel, { fill: T.ink2, fs: 12.5 }) + figText(R - 4, top + 12, `M = ${F2(m)}, SD = ${F2(s0)}, n = ${n}`, { fill: T.ink2, anchor: 'end', fs: 12 });
      return figSvg(T, W, H, g, f.title);
    },
    // 매개·조절된 매개: 경로 그림에 계수와 유의 표시
    path(f, T) {
      // 배치: 단순(매개 위) · 병렬 2개(위·아래) · 병렬 3개 이상(가운데 세로, 직접효과는 아래로 돌아감) · 순차(위에 나란히)
      const k = f.Ms.length, ser = f.kind === 'serial', bw = 150, bh = 46, W = 640;
      const H = ser ? 340 : k === 1 ? (f.W ? 380 : 330) : k === 2 ? 380 : 70 + k * 66 + 40;
      const nodes = {}, yB = ser || k === 1 ? H - 60 : (k === 2 ? H / 2 : (H - 40) / 2);
      nodes.X = [95, yB]; nodes.Y = [W - 95, yB];
      if (ser) { nodes.M0 = [200, 80]; nodes.M1 = [W - 200, 80]; }
      else if (k === 1) nodes.M0 = [W / 2, f.W ? 132 : 92];
      else if (k === 2) { nodes.M0 = [W / 2, 52]; nodes.M1 = [W / 2, H - 52]; }
      else f.Ms.forEach((_, j) => { nodes['M' + j] = [W / 2, 44 + j * 66]; });
      const names = { X: f.X, Y: f.Y }; f.Ms.forEach((m, j) => { names['M' + j] = m; });
      if (f.W) { names.W = f.W; nodes.W = f.wOn === 'a' ? [95, 52] : [W - 95, 52]; }
      const edge = (a, b) => { const [x1, y1] = nodes[a], [x2, y2] = nodes[b], dx = x2 - x1, dy = y2 - y1; const t1 = Math.min(Math.abs((bw / 2) / (dx || 1e-9)), Math.abs((bh / 2) / (dy || 1e-9))), t2 = t1 + 0.02; return [x1 + dx * t1, y1 + dy * t1, x2 - dx * t2, y2 - dy * t2]; };
      let defs = `<defs><marker id="fa${f.uid}" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto"><path d="M0 0L10 5L0 10z" fill="${T.ink2}"/></marker></defs>`, g = '';
      const lab = (x, y, t, strong) => { const w = figTw(t, 12) + 10; return `<rect x="${x - w / 2}" y="${y - 10}" width="${w}" height="20" rx="4" fill="${T.bg}"/>` + figText(x, y + 4, t, { fill: strong ? T.ink : T.ink3, fs: 12, weight: strong ? 700 : 400 }); };
      const coef = (nm, c) => `${nm} = ${F2(c.est)}${STAR(c.p)}`;
      const arrow = (a, b, c, nm, dash, lp = 0.5) => { const [x1, y1, x2, y2] = edge(a, b), mx = x1 + (x2 - x1) * lp, my = y1 + (y2 - y1) * lp; return { svg: `<line x1="${x1}" y1="${y1}" x2="${x2}" y2="${y2}" stroke="${T.ink2}" stroke-width="${c && c.p < 0.05 ? 2 : 1.4}" ${dash ? 'stroke-dasharray="5 4"' : ''} marker-end="url(#fa${f.uid})"/>`, lab: c ? lab(mx, my, typeof nm === 'string' ? coef(nm, c) : nm(c), c.p < 0.05) : '', mx, my }; };
      const parts = [];
      f.Ms.forEach((_, j) => parts.push(arrow('X', 'M' + j, f.a[j], k > 1 ? `a${j + 1}` : 'a', false, ser && j === 1 ? 0.36 : 0.5)));
      if (ser) parts.push(arrow('M0', 'M1', f.d, 'd'));
      f.Ms.forEach((_, j) => parts.push(arrow('M' + j, 'Y', f.b[j], k > 1 ? `b${j + 1}` : 'b', false, ser && j === 0 ? 0.64 : 0.5)));
      const cpTxt = c => `c' = ${F2(c.est)}${STAR(c.p)}${f.c ? ` (c = ${F2(f.c.est)}${STAR(f.c.p)})` : ''}`;
      if (!ser && k >= 3) { // 가운데 매개 상자를 피해 아래로 돌아가는 직접효과
        const [x1, y1] = nodes.X, [x2] = nodes.Y, yb = H - 22, sig = f.cp.p < 0.05;
        parts.push({ svg: `<path d="M${x1} ${y1 + bh / 2}V${yb}H${x2}V${y1 + bh / 2 + 2}" fill="none" stroke="${T.ink2}" stroke-width="${sig ? 2 : 1.4}" ${sig ? '' : 'stroke-dasharray="5 4"'} marker-end="url(#fa${f.uid})"/>`, lab: lab(W / 2, yb, cpTxt(f.cp), sig) });
      } else parts.push(arrow('X', 'Y', f.cp, cpTxt, !(f.cp.p < 0.05)));
      if (f.W) { const tgt = f.wOn === 'a' ? parts[0] : parts[f.Ms.length]; const [wx, wy] = nodes.W; parts.push({ svg: `<line x1="${wx}" y1="${wy + bh / 2}" x2="${tgt.mx}" y2="${tgt.my - 12}" stroke="${T.ink2}" stroke-width="${f.inter.p < 0.05 ? 2 : 1.4}" marker-end="url(#fa${f.uid})"/>`, lab: lab((wx + tgt.mx) / 2 + (f.wOn === 'a' ? 26 : -26), (wy + bh / 2 + tgt.my) / 2, coef(f.wOn === 'a' ? 'a3' : 'b3', f.inter), f.inter.p < 0.05) }); }
      g += parts.map(q => q.svg).join('');
      g += Object.keys(nodes).map(id => { const [x, y] = nodes[id]; return `<rect x="${x - bw / 2}" y="${y - bh / 2}" width="${bw}" height="${bh}" rx="10" fill="${T.bg}" stroke="${T.ink}" stroke-width="1.5"/>` + figText(x, y + (figWrap(names[id], bw - 14, 13).length > 1 ? -3 : 5), figWrap(names[id], bw - 14, 13), { fill: T.ink, fs: 13, weight: 700 }); }).join('');
      g += parts.map(q => q.lab).join('');
      return figSvg(T, W, H, defs + g, f.title);
    },
  };
  let figUid = 0;
  const figRender = (f, mode) => { try { return FIGS[f.type] ? FIGS[f.type]({ ...f, uid: ++figUid }, figTheme(mode)) : ''; } catch (e) { return ''; } };
  // 잔차 그림: 예측값 × 잔차 (점이 0선 위아래로 고르게 흩어지면 선형성·등분산성이 괜찮음)
  const thinPts = (pts, max = 600) => pts.length <= max ? pts : pts.filter((_, i) => i % Math.ceil(pts.length / max) === 0);
  const residFig = (fit, y, title) => ({ type: 'scatter', title, xLabel: '예측값', yLabel: '잔차', pts: thinPts(y.map((v, i) => [v - fit.resid[i], fit.resid[i]])), zero: true, note: '점이 0선 위아래로 고르게 흩어져 있고 깔때기나 곡선 모양이 보이지 않으면, 회귀의 선형성과 등분산성 가정이 크게 어긋나지 않습니다.' });
  // 결과 카드의 그림들 (번호는 계산한 순서대로, 논문의 그림 번호처럼)
  function figNoMap() { const m = new Map(); let n = 0; analyses.slice().reverse().forEach(a => (a.figs || []).forEach((_, i) => m.set(`${a.id}:${i}`, ++n))); return m; }
  function figsHtml(a, nos) {
    if (!a.figs || !a.figs.length) return '';
    return `<div class="tfigs">${a.figs.map((f, i) => { const svg = figRender(f); if (!svg) return ''; return `<figure class="tfig"><figcaption><b>그림 ${nos.get(`${a.id}:${i}`) || i + 1}.</b> ${esc(f.title)}</figcaption>
      <div class="tfig-svg">${svg}</div>${f.note ? `<p class="hint">${esc(f.note)}</p>` : ''}
      <div class="actions tight"><button class="btn sm" data-fig-act="png" data-id="${a.id}" data-i="${i}">그림 받기 (PNG)</button><button class="btn sm" data-fig-act="png" data-id="${a.id}" data-i="${i}" data-mono="1">흑백으로 받기</button><button class="btn sm" data-fig-act="cap" data-id="${a.id}" data-i="${i}">그림 제목 복사</button></div></figure>`; }).join('')}</div>`;
  }
  // SVG → PNG (3배 크기, 흰 바탕). claude.ai 화면에서는 이미지를 띄워 저장하게 함
  async function figPng(f, mono, name) {
    const svg = figRender(f, mono ? 'mono' : 'print'), m = svg.match(/viewBox="0 0 (\d+(?:\.\d+)?) (\d+(?:\.\d+)?)"/), W = +m[1], H = +m[2], sc = 3;
    const img = new Image(), url = URL.createObjectURL(new Blob([svg], { type: 'image/svg+xml;charset=utf-8' }));
    await new Promise((res, rej) => { img.onload = res; img.onerror = rej; img.src = url; });
    const c = document.createElement('canvas'); c.width = W * sc; c.height = H * sc; const ctx = c.getContext('2d'); ctx.fillStyle = '#ffffff'; ctx.fillRect(0, 0, c.width, c.height); ctx.drawImage(img, 0, 0, c.width, c.height); URL.revokeObjectURL(url);
    const dataUrl = c.toDataURL('image/png');
    if (hasClaude) { const ov = document.createElement('div'); ov.className = 'fig-ov'; ov.innerHTML = `<div class="fig-ov-card"><p><b>${esc(name)}</b> · 이미지를 길게 누르거나 오른쪽 버튼으로 눌러 [이미지 저장]을 고르세요.</p><img src="${dataUrl}" alt="${esc(f.title)}"><div class="actions tight"><button class="btn sm" data-fig-ov="close">닫기</button></div></div>`; ov.addEventListener('click', e => { if (e.target === ov || e.target.closest('[data-fig-ov]')) ov.remove(); }); document.body.appendChild(ov); return; }
    const a = document.createElement('a'); a.href = dataUrl; a.download = name; document.body.appendChild(a); a.click(); setTimeout(() => a.remove(), 300);
  }
  let preFigs = [];   // 사전 분석의 점수 분포 그림 (결과에 저장하지 않음)
  document.addEventListener('click', async e => {
    const b = e.target.closest('[data-fig-act]'); if (!b) return;
    if (b.dataset.src === 'pre') { const f = preFigs[+b.dataset.i]; if (f) { try { await figPng(f, b.dataset.mono === '1', `${f.title.replace(/[^\w가-힣]+/g, '_')}${b.dataset.mono === '1' ? '_흑백' : ''}.png`); } catch (err) { toast('그림을 만들지 못했습니다.'); } } return; }
    const a = analyses.find(x => x.id === b.dataset.id), f = a && a.figs && a.figs[+b.dataset.i]; if (!f) return;
    const no = figNoMap().get(`${a.id}:${b.dataset.i}`) || 1;
    if (b.dataset.figAct === 'cap') { const t = `그림 ${no}. ${f.title}`; try { navigator.clipboard.writeText(t).then(() => toast('그림 제목을 복사했습니다.'), () => showSheet('그림 제목', t)); } catch (err) { showSheet('그림 제목', t); } return; }
    try { await figPng(f, b.dataset.mono === '1', `그림${no}_${f.title.replace(/[^\w가-힣]+/g, '_').slice(0, 40)}${b.dataset.mono === '1' ? '_흑백' : ''}.png`); } catch (err) { toast('그림을 만들지 못했습니다.'); }
  });

  function renderAnalyses() {
    const box = $('#an-results');
    if (!analyses.length) { box.innerHTML = ''; return; }
    const fnos = figNoMap();
    box.innerHTML = rPanelHtml() + analyses.map(a => `<div class="card result-card" id="res-${a.id}">
      <div class="res-head">
        <div><h3 class="card-title">${confTag(a)}${esc(a.title)}</h3><div class="hint">N = ${a.n} · ${esc(a.at)}${a.sample ? ' · <b>예시(가짜) 자료</b>' : ''}${a.dv !== dataVersion ? ' · <span class="a-mid">지금 자료와 다른 자료로 계산함</span>' : ''}</div></div>
        <div class="head-btns"><button class="btn sm" data-an-act="csv" data-id="${a.id}">표 CSV</button><button class="btn sm" data-an-act="del" data-id="${a.id}">지우기</button></div>
      </div>
      ${confNote(a)}${a.tables.map(t => `<div class="section-title">${esc(t.title)}</div>
        <div class="table-wrap"><table class="compact stat-table"><thead><tr>${t.head.map((h, i) => `<th class="${(t.num || []).includes(i) ? 'num' : ''}">${esc(h)}</th>`).join('')}</tr></thead>
        <tbody>${t.rows.map(r => `<tr>${r.map((c, i) => `<td class="${(t.num || []).includes(i) ? 'num' : ''}">${esc(c)}</td>`).join('')}</tr>`).join('')}</tbody></table></div>
        ${t.foot ? `<p class="hint">${esc(t.foot)}</p>` : ''}`).join('')}
      ${figsHtml(a, fnos)}
      ${a.notes.length ? `<ul class="notes">${a.notes.map(n => `<li>${esc(n)}</li>`).join('')}</ul>` : ''}
      ${a.sent ? `<div class="dq-report"><h4>논문에 쓸 문장 (초안)</h4><p>${esc(a.sent)}</p><div class="actions tight"><button class="btn sm" data-an-act="copy-sent" data-id="${a.id}">문장 복사</button></div><p class="hint">숫자와 유의 여부만 적었습니다. 적합도가 괜찮은지, 가설이 지지되는지는 직접 판단해 쓰고, 학교 양식에 맞게 다듬으세요.</p></div>` : ''}
      ${a.decisions ? `<p class="hint" style="margin-top:12px">분석 결정: ${a.decisions.map(esc).join(' · ')}</p>` : ''}
      ${checksHtml(a)}
      ${beyondPHtml(a)}
      ${rcodeHtml(a)}
      ${rcheckHtml(a)}
      ${spssHtml(a)}
      <details class="item-detail"><summary>가설에 연결할 수 있는 효과 ${a.effects.length}개</summary>
        <div class="table-wrap"><table class="compact"><thead><tr><th>효과</th><th>통계값</th><th>유의</th></tr></thead>
        <tbody>${a.effects.map(e => `<tr><td>${esc(e.label)}</td><td>${esc(e.stat)}</td><td>${sigOf(e) ? '<span class="a-ok">유의</span>' : '<span class="hint">유의하지 않음</span>'}</td></tr>`).join('')}</tbody></table></div>
      </details>
      <div class="actions"><button class="btn navy" data-an-act="to-interp">결과 해석으로 →</button></div>
    </div>`).join('');
  }

  // =================================================================

// ##### 4. 분석 방법 (RUN 이어서): 반복측정·혼합설계 분산분석, 맥니마, 코크란 Q (stats-ladder.html 11572~11849줄) #####
  // =================================================================
  // 반복측정 · 혼합설계 분산분석, 맥니마, 코크란 Q
  // =================================================================
  // 집단 안 대비: 직교 정규 (k × (k−1)). 결과는 어떤 직교 대비를 써도 같음
  function orthoC(k) {
    const C = [];
    for (let j = 1; j < k; j++) { const v = Array.from({ length: k }, (_, i) => (i < j ? 1 : i === j ? -j : 0)); const nrm = Math.sqrt(v.reduce((s, x) => s + x * x, 0)); C.push(v.map(x => x / nrm)); }
    return C; // (k−1)개의 행
  }
  const matDet = A => { const e = jacobiEig(A); return e.values.reduce((s, v) => s * v, 1); };
  const trace = A => A.reduce((s, r, i) => s + r[i], 0);
  // 구형성: T는 대비 점수의 제곱합·교차곱 행렬, rdf는 그 자유도, kOrig는 원래 측정 수 (R의 mauchly.test와 같은 식)
  function sphericity(T, rdf, kOrig) {
    const p = T.length;
    if (p < 2) return { W: 1, chi: 0, df: 0, p: 1, gg: 1, hf: 1 };
    const S = T.map(r => r.map(v => v / rdf));
    const logW = Math.log(Math.max(matDet(S), 1e-300)) - p * Math.log(trace(S) / p);
    const rho = 1 - (2 * p * p + p + 2) / (6 * p * rdf);
    const w2 = (p + 2) * (p - 1) * (p - 2) * (2 * p ** 3 + 6 * p * p + 3 * kOrig + 2) / (288 * (rdf * p * rho) ** 2);
    const z = -rdf * rho * logW, f = p * (p + 1) / 2 - 1;
    const P1 = ST.pChi(z, f), P2 = ST.pChi(z, f + 4);
    const S2 = mmul(S, S), gg = trace(S) ** 2 / (p * trace(S2));
    const hf = ((rdf + 1) * p * gg - 2) / (p * (rdf - p * gg));
    return { W: Math.exp(logW), chi: z, df: f, p: Math.min(1, Math.max(0, P1 + w2 * (P2 - P1))), gg, hf };
  }
  const sphChoice = (o, sp, k) => { const m = o.sph || 'auto'; return k < 3 || m === 'none' ? 'none' : m === 'gg' ? 'gg' : sp.p < 0.05 ? 'gg' : 'none'; };
  const SPH_LABEL = { none: '구형성 가정', gg: 'Greenhouse-Geisser', hf: 'Huynh-Feldt' };
  // 집단 안 효과 한 줄: F와 보정된 자유도·p
  function withinRows(name, ss, df1, sse, df2, sp) {
    const F = (ss / df1) / (sse / df2), pe = ss / (ss + sse);
    return ['none', 'gg', 'hf'].map(c => { const e = c === 'none' ? 1 : c === 'gg' ? sp.gg : Math.min(1, sp.hf); return { c, name, ss, df1: df1 * e, df2: df2 * e, F, p: ST.pF(F, df1 * e, df2 * e), pe }; });
  }
  const fmtDf = x => (Math.abs(x - Math.round(x)) < 1e-9 ? String(Math.round(x)) : F2(x));
  function rmPrep(keys) {
    const P = prep(Object.fromEntries(keys.map((k, i) => ['y' + i, k])));
    const Y = P.keep.map((_, r) => keys.map(k => P.col(k)[r]));
    return { P, Y, n: Y.length, k: keys.length };
  }
  // 한 집단 안 반복측정 (짝지은 t검정 Bonferroni 사후검정 포함)
  function rmCore(Y) {
    const n = Y.length, k = Y[0].length, gm = mean(Y.flat());
    const cm = Array.from({ length: k }, (_, j) => mean(Y.map(r => r[j]))), sm = Y.map(r => mean(r));
    const ssC = n * cm.reduce((s, m) => s + (m - gm) ** 2, 0), ssS = k * sm.reduce((s, m) => s + (m - gm) ** 2, 0);
    const ssT = Y.flat().reduce((s, v) => s + (v - gm) ** 2, 0), ssE = ssT - ssC - ssS;
    const C = orthoC(k), Z = Y.map(r => C.map(c => c.reduce((s, w, j) => s + w * r[j], 0)));
    const zm = C.map((_, q) => mean(Z.map(z => z[q])));
    const T = C.map((_, a) => C.map((_, b) => Z.reduce((s, z) => s + (z[a] - zm[a]) * (z[b] - zm[b]), 0)));
    const sp = sphericity(T, n - 1, k);
    return { n, k, cm, ssC, ssS, ssE, df1: k - 1, df2: (k - 1) * (n - 1), sp, ges: ssC / (ssC + ssS + ssE) };
  }
  function pairwiseRm(Y, names) {
    const k = names.length, m = k * (k - 1) / 2, out = [];
    for (let i = 0; i < k; i++) for (let j = i + 1; j < k; j++) {
      const d = Y.map(r => r[i] - r[j]), n = d.length, t = mean(d) / (sd(d) / Math.sqrt(n)), p = ST.pT(Math.abs(t), n - 1);
      out.push([`${names[i]} − ${names[j]}`, F2(mean(d)), F2(t), String(n - 1), PV(Math.min(1, p * m))]);
    }
    return out;
  }
  // 이항 분포 양측 정확 p (p = .5)
  function binomTwo(x, n) {
    if (!n) return 1;
    const lg = v => { let s = 0; for (let i = 2; i <= v; i++) s += Math.log(i); return s; };
    const lc = (n, k) => lg(n) - lg(k) - lg(n - k);
    const lo = Math.min(x, n - x); let s = 0;
    for (let i = 0; i <= lo; i++) s += Math.exp(lc(n, i) - n * Math.LN2);
    return Math.min(1, 2 * s - (lo * 2 === n ? Math.exp(lc(n, lo) - n * Math.LN2) : 0));
  }
  const binTarget = (P, keys) => { const v = P.V[keys[0]]; return v.levels.filter(l => keys.some(k => P.col(k).includes(l)))[0]; };

  Object.assign(METHODS, {
    rm_anova: { name: '반복측정 분산분석', group: '집단 차이', slots: [{ k: 'ys', label: '같은 사람이 여러 번 답한 점수 (시점·조건 순서대로 2개 이상)', type: 'num', multi: true, min: 2 }] },
    mixed_anova: { name: '혼합설계 분산분석 (집단 × 시점)', group: '집단 차이', slots: [{ k: 'g', label: '집단 (예: 실험·통제)', type: 'cat' }, { k: 'ys', label: '같은 사람이 여러 번 답한 점수 (시점 순서대로 2개 이상)', type: 'num', multi: true, min: 2 }] },
    mcnemar: { name: '맥니마 검정', group: '관계', slots: [{ k: 'a', label: '첫 번째 분류 (예: 사전, 두 가지)', type: 'cat2' }, { k: 'b', label: '두 번째 분류 (예: 사후, 같은 두 가지)', type: 'cat2' }] },
    cochran: { name: '코크란 Q 검정', group: '관계', slots: [{ k: 'vars', label: '같은 사람을 여러 번 분류한 변인 (두 가지, 3개 이상)', type: 'cat2', multi: true, min: 3 }] }
  });
  Object.assign(LADDER_TO_METHOD, { r_rm_anova: 'rm_anova', r_mixed_anova: 'mixed_anova', r_mcnemar: 'mcnemar', r_cochran: 'cochran' });
  DECISIONS.push({ id: 'sph', scope: ['rm_anova', 'mixed_anova'], q: '구형성 가정이 어긋나면(Mauchly p < .05) 어떻게 할까요?',
    opts: [['auto', 'Mauchly 검정에 따라 보정', 'Mauchly p < .05이면 Greenhouse-Geisser로 자유도를 보정합니다. SPSS를 쓴 국내 논문에서 가장 흔합니다.'], ['gg', '언제나 Greenhouse-Geisser', '구형성 검정은 표본 크기에 민감해서, 늘 보정하는 쪽을 권하는 방법론 연구도 많습니다.'], ['none', '보정하지 않음', '측정이 두 번뿐이면 구형성 문제가 없어 이 값과 같습니다.']],
    get: () => AO().sph || 'auto', set: v => { AO().sph = v; } });

  Object.assign(RUN, {
    async rm_anova(r, o) {
      const { P, Y, n, k } = rmPrep(r.ys); need(n >= 3, '모든 시점에 답한 사람이 3명 이상 있어야 합니다.');
      const names = r.ys.map(x => P.V[x].name), c = rmCore(Y), use = sphChoice(o, c.sp, k);
      const rows = withinRows('시점', c.ssC, c.df1, c.ssE, c.df2, c.sp), main = rows.find(x => x.c === use);
      const R = newResult('rm_anova', `반복측정 분산분석: ${names.join(' → ')}`); R.n = n;
      addFig(R, { type: 'lines', title: `시점별 평균: ${names.join(' → ')}`, yLabel: '점수', yRange: unionRange(r.ys.map(scoreRange)), xs: names, series: [{ name: '평균', pts: r.ys.map((_, j) => ciOf(Y.map(row => row[j]))) }], note: '점은 평균, 막대는 각 시점 점수의 95% 신뢰구간입니다(같은 사람의 점수라 시점 차이 검정과는 다름).' });
      R.tables.push({ title: '시점별 기술통계', head: ['시점', 'n', 'M', 'SD'], num: [1, 2, 3], rows: r.ys.map((x, j) => [names[j], String(n), F2(c.cm[j]), F2(sd(Y.map(row => row[j])))]) });
      if (k >= 3) R.tables.push({ title: '구형성 검정 (Mauchly)', head: ['W', 'χ²', 'df', 'p', 'ε (Greenhouse-Geisser)', 'ε (Huynh-Feldt)'], num: [0, 1, 2, 3, 4, 5], rows: [[F2(c.sp.W, 3), F2(c.sp.chi), String(c.sp.df), PV(c.sp.p), F2(c.sp.gg, 3), F2(Math.min(1, c.sp.hf), 3)]] });
      R.tables.push({ title: '집단 안 효과', head: ['', '보정', 'SS', 'df', 'MS', 'F', 'p', '부분 η²'], num: [2, 3, 4, 5, 6, 7],
        rows: (k >= 3 ? rows : rows.slice(0, 1)).map(x => [x.c === 'none' ? '시점' : '', SPH_LABEL[x.c] + (x.c === use ? ' ← 보고' : ''), F2(x.ss), fmtDf(x.df1), F2(x.ss / x.df1), F2(x.F), PV(x.p), NZ(x.pe, 3)])
          .concat([['오차 (시점)', '', F2(c.ssE), fmtDf(main.df2), F2(c.ssE / main.df2), '', '', '']]) });
      R.tables.push({ title: '사후검정 (짝지은 t검정, Bonferroni)', head: ['비교', '평균 차이', 't', 'df', 'p'], num: [1, 2, 3, 4], rows: pairwiseRm(Y, names) });
      R.notes.push(k < 3 ? '측정이 두 번이라 구형성 가정이 필요 없습니다. 이 결과는 대응표본 t검정과 같습니다(F = t²).'
        : `구형성: Mauchly W = ${F2(c.sp.W, 3)}, ${PS(c.sp.p)}. ${use === 'gg' ? 'Greenhouse-Geisser로 자유도를 보정한 값을 보고했습니다' : '구형성 가정 값을 보고했습니다'} (분석 결정).`);
      R.notes.push(`일반화 η²(ges) = ${NZ(c.ges, 3)}. 부분 η²는 SPSS, 일반화 η²는 R(afex) 기본값입니다.`);
      eff(R, { kind: 'omnibus', label: `${names.join(', ')}: 시점에 따른 차이`, est: main.pe, p: main.p, sph: use, stat: `F(${fmtDf(main.df1)}, ${fmtDf(main.df2)}) = ${F2(main.F)}, ${PS(main.p)}, 부분 η² = ${NZ(main.pe, 3)}${use === 'gg' ? ' (Greenhouse-Geisser)' : ''}` });
      dropNote(R, P); return R;
    },

    async mixed_anova(r, o) {
      const keys = r.ys, P = prep(Object.fromEntries([['g', r.g]].concat(keys.map((k, i) => ['y' + i, k]))));
      const g = P.col(r.g), lv = P.V[r.g].levels.filter(l => g.includes(l)); need(lv.length >= 2, '집단이 두 가지 이상 있어야 합니다.');
      const Y = P.keep.map((_, i) => keys.map(k => P.col(k)[i])), N = Y.length, k = keys.length, a = lv.length, p = k - 1;
      const gi = g.map(x => lv.indexOf(x)), ng = lv.map((_, j) => gi.filter(x => x === j).length); need(ng.every(x => x >= 2), '각 집단에 2명 이상 있어야 합니다.');
      const names = keys.map(x => P.V[x].name);
      const figMixed = R => { if (lv.length <= 3) addFig(R, { type: 'lines', title: `${P.V[r.g].name}별 시점 평균: ${names.join(' → ')}`, yLabel: '점수', yRange: unionRange(keys.map(scoreRange)), xs: names, series: lv.map((l, j) => ({ name: `${P.V[r.g].name}: ${l}`, pts: keys.map((_, q) => ciOf(Y.filter((_, i) => gi[i] === j).map(rw => rw[q]))) })), note: '선이 평행하지 않으면 시점에 따른 변화가 집단마다 다르다는 뜻(상호작용)입니다. 막대는 95% 신뢰구간입니다.' }); };
      // 집단 사이: 사람 평균으로 일원분산분석 × k
      const s = Y.map(rw => mean(rw)), sg = lv.map((_, j) => mean(s.filter((_, i) => gi[i] === j))), sAll = mean(s);
      const ssG = k * lv.reduce((t, _, j) => t + ng[j] * (sg[j] - sAll) ** 2, 0), ssEb = k * s.reduce((t, v, i) => t + (v - sg[gi[i]]) ** 2, 0);
      // 집단 안: 직교 대비 점수 (제III유형: 집단 평균의 비가중 평균)
      const C = orthoC(k), Z = Y.map(rw => C.map(c => c.reduce((t, w, j) => t + w * rw[j], 0)));
      let ssT = 0, ssI = 0, ssEw = 0; const vfac = lv.reduce((t, _, j) => t + 1 / ng[j], 0) / (a * a);
      const resid = Z.map(() => new Array(p).fill(0));
      for (let q = 0; q < p; q++) {
        const zg = lv.map((_, j) => mean(Z.filter((_, i) => gi[i] === j).map(z => z[q]))), zw = mean(Z.map(z => z[q])), mu = mean(zg);
        ssT += mu * mu / vfac;
        ssI += lv.reduce((t, _, j) => t + ng[j] * (zg[j] - zw) ** 2, 0);
        Z.forEach((z, i) => { resid[i][q] = z[q] - zg[gi[i]]; ssEw += resid[i][q] ** 2; });
      }
      const T = C.map((_, x) => C.map((_, y) => resid.reduce((t, e) => t + e[x] * e[y], 0)));
      const sp = sphericity(T, N - a, k), use = sphChoice(o, sp, k);
      const dfEb = N - a, dfEw = p * (N - a);
      const FG = (ssG / (a - 1)) / (ssEb / dfEb), pG = ST.pF(FG, a - 1, dfEb), peG = ssG / (ssG + ssEb);
      const rT = withinRows('시점', ssT, p, ssEw, dfEw, sp), rI = withinRows(`${P.V[r.g].name} × 시점`, ssI, p * (a - 1), ssEw, dfEw, sp);
      const mT = rT.find(x => x.c === use), mI = rI.find(x => x.c === use);
      const R = newResult('mixed_anova', `혼합설계 분산분석: ${P.V[r.g].name} × 시점 (${names.join(' → ')})`); R.n = N; figMixed(R);
      R.tables.push({ title: '집단 × 시점 평균 (SD)', head: ['집단', 'n'].concat(names), num: names.map((_, j) => j + 2).concat([1]),
        rows: lv.map((l, j) => { const rows = Y.filter((_, i) => gi[i] === j); return [l, String(ng[j])].concat(names.map((_, q) => `${F2(mean(rows.map(x => x[q])))} (${F2(sd(rows.map(x => x[q])))})`)); }) });
      if (k >= 3) R.tables.push({ title: '구형성 검정 (Mauchly)', head: ['W', 'χ²', 'df', 'p', 'ε (Greenhouse-Geisser)', 'ε (Huynh-Feldt)'], num: [0, 1, 2, 3, 4, 5], rows: [[F2(sp.W, 3), F2(sp.chi), String(sp.df), PV(sp.p), F2(sp.gg, 3), F2(Math.min(1, sp.hf), 3)]] });
      const wrow = x => [x.name, SPH_LABEL[x.c] + (x.c === use ? ' ← 보고' : ''), F2(x.ss), fmtDf(x.df1), F2(x.ss / x.df1), F2(x.F), PV(x.p), NZ(x.pe, 3)];
      R.tables.push({ title: '집단 사이 효과', head: ['', 'SS', 'df', 'MS', 'F', 'p', '부분 η²'], num: [1, 2, 3, 4, 5, 6], rows: [[P.V[r.g].name, F2(ssG), String(a - 1), F2(ssG / (a - 1)), F2(FG), PV(pG), NZ(peG, 3)], ['오차', F2(ssEb), String(dfEb), F2(ssEb / dfEb), '', '', '']] });
      R.tables.push({ title: '집단 안 효과', head: ['', '보정', 'SS', 'df', 'MS', 'F', 'p', '부분 η²'], num: [2, 3, 4, 5, 6, 7],
        rows: (k >= 3 ? rT : rT.slice(0, 1)).map(wrow).concat((k >= 3 ? rI : rI.slice(0, 1)).map(wrow), [['오차 (시점)', '', F2(ssEw), fmtDf(mT.df2), F2(ssEw / mT.df2), '', '', '']]) });
      const levs = names.map((nm, q) => { const L = levene(lv.map((_, j) => Y.filter((_, i) => gi[i] === j).map(x => x[q]))); return `${nm} F = ${F2(L.F)}, ${PS(L.p)}`; });
      R.notes.push(`집단 크기가 다르면 집단 안 효과는 SPSS와 같은 제III유형(집단 평균을 같은 무게로)으로 계산했습니다.${k >= 3 ? ` 구형성: Mauchly W = ${F2(sp.W, 3)}, ${PS(sp.p)} → ${use === 'gg' ? 'Greenhouse-Geisser 보정 값을 보고' : '구형성 가정 값을 보고'}(분석 결정).` : ''}`);
      R.notes.push(`등분산성(Levene, 시점별): ${levs.join(' / ')}`);
      R.notes.push('프로그램 효과는 보통 집단 × 시점 상호작용으로 판단합니다. 상호작용이 유의하면 집단별·시점별 단순효과를 추가로 살펴보세요.');
      eff(R, { kind: 'omnibus', label: `${P.V[r.g].name} × 시점 상호작용`, est: mI.pe, p: mI.p, sph: use, stat: `F(${fmtDf(mI.df1)}, ${fmtDf(mI.df2)}) = ${F2(mI.F)}, ${PS(mI.p)}, 부분 η² = ${NZ(mI.pe, 3)}` });
      eff(R, { kind: 'omnibus', label: `시점 주효과 (${names.join(', ')})`, est: mT.pe, p: mT.p, sph: use, stat: `F(${fmtDf(mT.df1)}, ${fmtDf(mT.df2)}) = ${F2(mT.F)}, ${PS(mT.p)}, 부분 η² = ${NZ(mT.pe, 3)}` });
      eff(R, { kind: 'omnibus', label: `${P.V[r.g].name} 주효과`, est: peG, p: pG, stat: `F(${a - 1}, ${dfEb}) = ${F2(FG)}, ${PS(pG)}, 부분 η² = ${NZ(peG, 3)}` });
      dropNote(R, P); return R;
    },

    async mcnemar(r) {
      const P = prep({ a: r.a, b: r.b }), A = P.col(r.a), B = P.col(r.b);
      const lv = P.V[r.a].levels.filter(l => A.includes(l) || B.includes(l)); need(lv.length === 2, '두 분류 모두 같은 두 가지 값이어야 합니다 (예: 예·아니오).');
      need(B.every(x => lv.includes(x)), '두 분류의 선택지가 같아야 합니다.');
      const t = lv[0], n11 = A.filter((x, i) => x === t && B[i] === t).length, n10 = A.filter((x, i) => x === t && B[i] !== t).length, n01 = A.filter((x, i) => x !== t && B[i] === t).length, n00 = P.n - n11 - n10 - n01;
      const disc = n10 + n01; need(disc > 0, '두 번의 분류가 모두 같아 변화가 없습니다.');
      const chi = (Math.abs(n10 - n01) - 1) ** 2 / disc, p = ST.pChi(chi, 1), pEx = binomTwo(n01, disc);
      const p1 = (n11 + n10) / P.n, p2 = (n11 + n01) / P.n;
      const R = newResult('mcnemar', `맥니마 검정: ${P.V[r.a].name} → ${P.V[r.b].name}`); R.n = P.n;
      addFig(R, { type: 'lines', pct: true, title: `'${t}' 응답 비율의 변화: ${P.V[r.a].name} → ${P.V[r.b].name}`, yLabel: `'${t}' 비율`, xs: [P.V[r.a].name, P.V[r.b].name], series: [{ name: `'${t}' 비율`, pts: [wilsonPct(n11 + n10, P.n), wilsonPct(n11 + n01, P.n)] }], note: '막대는 비율의 95% 신뢰구간(Wilson)입니다.' });
      R.tables.push({ title: '짝지은 교차표', head: [`${P.V[r.a].name} \\ ${P.V[r.b].name}`, lv[0], lv[1], '전체'], num: [1, 2, 3], rows: [[lv[0], String(n11), String(n10), String(n11 + n10)], [lv[1], String(n01), String(n00), String(n01 + n00)], ['전체', String(n11 + n01), String(n10 + n00), String(P.n)]] });
      R.tables.push({ title: '검정 결과', head: [`'${t}' 비율 (첫째)`, `'${t}' 비율 (둘째)`, 'χ² (연속성 교정)', 'df', 'p', '정확 p (이항)', '바뀐 사람'], num: [0, 1, 2, 3, 4, 5, 6], rows: [[`${F2(p1 * 100, 1)}%`, `${F2(p2 * 100, 1)}%`, F2(chi), '1', PV(p), PV(pEx), `${disc}명 (${lv[0]}→${lv[1]} ${n10}, ${lv[1]}→${lv[0]} ${n01})`]] });
      if (disc < 25) R.notes.push(`바뀐 사람이 ${disc}명으로 적어 정확 p(이항검정)를 함께 보고하는 것이 좋습니다.`);
      eff(R, { label: `'${t}' 비율 변화 (${P.V[r.b].name} − ${P.V[r.a].name})`, est: p2 - p1, p, pEx, stat: `χ²(1, N = ${P.n}) = ${F2(chi)}, ${PS(p)}, '${t}' ${F2(p1 * 100, 1)}% → ${F2(p2 * 100, 1)}%` });
      dropNote(R, P); return R;
    },

    async cochran(r) {
      const keys = r.vars, P = prep(Object.fromEntries(keys.map((k, i) => ['v' + i, k])));
      const t = binTarget(P, keys); const lvAll = [...new Set(keys.flatMap(k => P.col(k)))];
      need(lvAll.length === 2, '모든 변인이 같은 두 가지 값이어야 합니다 (예: 예·아니오).');
      const X = P.keep.map((_, i) => keys.map(k => (P.col(k)[i] === t ? 1 : 0))), n = X.length, k = keys.length;
      const Cj = keys.map((_, j) => X.reduce((s, x) => s + x[j], 0)), Ri = X.map(x => x.reduce((s, v) => s + v, 0)), Nt = Ri.reduce((s, v) => s + v, 0);
      const den = k * Nt - Ri.reduce((s, v) => s + v * v, 0); need(den > 0, '모든 사람의 분류가 시점마다 같아 검정할 수 없습니다.');
      const Q = (k - 1) * (k * Cj.reduce((s, c) => s + c * c, 0) - Nt * Nt) / den, df = k - 1, p = ST.pChi(Q, df), W = Q / (n * (k - 1));
      const names = keys.map(x => P.V[x].name);
      const R = newResult('cochran', `코크란 Q 검정: ${names.join(', ')}`); R.n = n;
      addFig(R, { type: 'lines', pct: true, title: `'${t}' 응답 비율: ${names.join(', ')}`, yLabel: `'${t}' 비율`, xs: names, series: [{ name: `'${t}' 비율`, pts: Cj.map(c => wilsonPct(c, n)) }], note: '막대는 비율의 95% 신뢰구간(Wilson)입니다.' });
      R.tables.push({ title: `'${t}' 응답 비율`, head: ['변인', `'${t}' (명)`, '비율'], num: [1, 2], rows: names.map((nm, j) => [nm, String(Cj[j]), `${F2(Cj[j] / n * 100, 1)}%`]) });
      R.tables.push({ title: '검정 결과', head: ['Q', 'df', 'p', 'Kendall의 W'], num: [0, 1, 2, 3], rows: [[F2(Q), String(df), PV(p), NZ(W, 3)]] });
      const m = k * (k - 1) / 2, post = [];
      for (let i = 0; i < k; i++) for (let j = i + 1; j < k; j++) {
        const b = X.filter(x => x[i] === 1 && x[j] === 0).length, c = X.filter(x => x[i] === 0 && x[j] === 1).length;
        const chi = b + c ? (b - c) ** 2 / (b + c) : 0; post.push([`${names[i]} − ${names[j]}`, `${b} / ${c}`, F2(chi), PV(Math.min(1, ST.pChi(chi, 1) * m))]);
      }
      R.tables.push({ title: '사후검정 (짝지은 비교, Bonferroni)', head: ['비교', '바뀐 사람 (앞만 / 뒤만)', 'χ²', 'p'], num: [2, 3], rows: post });
      eff(R, { kind: 'omnibus', label: `'${t}' 비율의 차이 (${names.join(', ')})`, est: W, p, q: Q, stat: `Q(${df}, N = ${n}) = ${F2(Q)}, ${PS(p)}, W = ${NZ(W, 3)}` });
      dropNote(R, P); return R;
    }
  });

  Object.assign(CHECKS, {
    rm_anova(r, R) {
      const P = prep(Object.fromEntries(r.ys.map((k, i) => ['y' + i, k])));
      const sp = R.tables.find(t => /Mauchly/.test(t.title));
      return { desc: { st: 'ok', note: '시점별 n, M, SD를 보고했습니다.' }, es: { text: `부분 η² = ${NZ(R.effects[0].est, 3)} (${sizeLabel('eta', R.effects[0].est)})`, cut: 'eta' }, ci: { st: 'warn', note: '부분 η²의 신뢰구간은 계산하지 않습니다.' },
        norm: normality(r.ys.map(k => [P.V[k].name, P.col(k)])),
        assum: [{ item: '구형성 (Mauchly)', st: !sp ? 'na' : 'ok', note: sp ? `W = ${sp.rows[0][0]}, p ${sp.rows[0][3]} · 보정은 분석 결정대로 했습니다` : '측정이 두 번이라 필요 없음' }],
        post: { st: 'ok', note: '짝지은 t검정에 Bonferroni 보정을 한 사후검정을 보고했습니다.' } };
    },
    mixed_anova(r, R) {
      const P = prep(Object.fromEntries([['g', r.g]].concat(r.ys.map((k, i) => ['y' + i, k]))));
      return { desc: { st: 'ok', note: '집단 × 시점 평균(SD)을 보고했습니다.' }, es: { text: R.effects.map(e => `${e.label}: 부분 η² = ${NZ(e.est, 3)} (${sizeLabel('eta', e.est)})`).join(' / '), cut: 'eta' },
        ci: { st: 'warn', note: '부분 η²의 신뢰구간은 계산하지 않습니다.' }, norm: normality(r.ys.map(k => [P.V[k].name, P.col(k)])),
        assum: [{ item: '등분산성 (Levene, 시점별)', st: 'ok', note: (R.notes.find(n => n.startsWith('등분산성')) || '').replace(/^등분산성\(Levene, 시점별\): /, '') }, { item: '공분산 행렬 동질성 (Box의 M)', st: 'warn', note: 'Box의 M 검정은 이 사이트에서 하지 않습니다. 집단 크기가 비슷하면 크게 문제되지 않습니다.' }],
        post: { st: sigOf(R.effects[0]) ? 'warn' : 'ok', note: sigOf(R.effects[0]) ? '상호작용이 유의합니다. 집단별 시점 변화(단순효과)를 추가로 살펴보세요.' : '상호작용이 유의하지 않아 주효과 중심으로 해석할 수 있습니다.' } };
    },
    mcnemar(r, R) {
      return { desc: { st: 'ok', note: '짝지은 교차표에 빈도를 보고했습니다.' }, es: { text: `비율 변화 = ${F2(R.effects[0].est * 100, 1)}%p`, cutText: '비율 변화는 합의된 크기 기준이 없어 크기만 적습니다.' }, ci: { st: 'na', note: '비율 차이의 신뢰구간은 계산하지 않습니다.' },
        norm: { na: '빈도 자료라 정규성 가정이 없음' }, assum: [{ item: '바뀐 사람 수', st: 'ok', note: R.notes.find(n => n.startsWith('바뀐 사람')) || '바뀐 사람이 25명 이상이라 χ² 근사를 써도 됩니다.' }], post: null };
    },
    cochran(r, R) {
      return { desc: { st: 'ok', note: '변인별 비율을 보고했습니다.' }, es: { text: `Kendall의 W = ${NZ(R.effects[0].est, 3)}`, cutText: 'Kendall의 W: .10 작음, .30 중간, .50 큼 (Cohen 기준을 빌려 씀)' }, ci: { st: 'na', note: 'W의 신뢰구간은 계산하지 않습니다.' },
        norm: { na: '빈도 자료라 정규성 가정이 없음' }, assum: [], post: { st: 'ok', note: '짝지은 비교에 Bonferroni 보정을 한 사후검정을 보고했습니다.' } };
    }
  });

  // R 코드: 기본 R만 (stats)
  const rYmat = (S, keys) => `Y <- cbind(${keys.map(k => rNum(S, k)).join(', ')}); colnames(Y) <- ${rvec(keys.map(k => nm(S, k)))}`;
  const rSph = (S) => { const m = S.opts.sph || 'auto'; return m === 'gg' ? 'use <- "GG"' : m === 'none' ? 'use <- "none"' : 'use <- if (ncol(Y) >= 3 && mt$p.value < .05) "GG" else "none"'; };
  Object.assign(RGEN, {
    rm_anova: S => { const r = S.roles; return `${rComplete(S, r.ys)}
${rYmat(S, r.ys)}
k <- ncol(Y); n <- nrow(Y)
long <- data.frame(y = c(Y), time = factor(rep(colnames(Y), each = n), levels = colnames(Y)), id = factor(rep(seq_len(n), k)))
a <- summary(aov(y ~ time + Error(id/time), data = long)); print(a)
w <- a[["Error: id:time"]][[1]]; ss <- w[["Sum Sq"]]; pe <- ss[1] / (ss[1] + ss[2])
fit <- lm(Y ~ 1)
mt <- if (k >= 3) mauchly.test(fit, X = ~1) else list(p.value = 1); if (k >= 3) print(mt)
sph <- anova(fit, X = ~1, test = "Spherical"); print(sph)   # G-G, H-F 보정
${rSph(S)}   # 분석 결정: 구형성
p <- if (use == "GG") sph[["G-G Pr"]][1] else w[["Pr(>F)"]][1]
print(pairwise.t.test(long$y, long$time, paired = TRUE, p.adjust.method = "bonferroni"))
eff(${rq(`${r.ys.map(k => nm(S, k)).join(', ')}: 시점에 따른 차이`)}, pe, NA, NA, p)`; },
    mixed_anova: S => { const r = S.roles, G = nm(S, r.g); return `${rComplete(S, [r.g, ...r.ys])}
g <- ${rCat(S, r.g)}; ${rYmat(S, r.ys)}
k <- ncol(Y); N <- nrow(Y); a <- nlevels(g); contrasts(g) <- contr.sum(a)
# 집단 사이: 사람 평균으로 일원분산분석 (× k)
s <- rowMeans(Y); ab <- anova(lm(s ~ g)); ssG <- k * ab[1, 2]; ssEb <- k * ab[2, 2]
FG <- (ssG / (a - 1)) / (ssEb / (N - a)); pG <- pf(FG, a - 1, N - a, lower.tail = FALSE)
# 집단 안: 직교 대비 점수, 제III유형 (집단 평균을 같은 무게로)
C <- contr.poly(k); Z <- Y %*% C; ssT <- 0; ssI <- 0; E <- matrix(0, N, k - 1)
for (q in 1:(k - 1)) { m1 <- lm(Z[, q] ~ g); m0 <- lm(Z[, q] ~ 1); v <- summary(m1)$cov.unscaled[1, 1]
  ssT <- ssT + coef(m1)[1]^2 / v; ssI <- ssI + sum(resid(m0)^2) - sum(resid(m1)^2); E[, q] <- resid(m1) }
ssE <- sum(E^2); p <- k - 1; dfE <- p * (N - a)
fit <- lm(Y ~ g); mt <- if (k >= 3) mauchly.test(fit, X = ~1) else list(p.value = 1); if (k >= 3) print(mt)
Tm <- crossprod(E) / (N - a); gg <- if (k >= 3) sum(diag(Tm))^2 / (p * sum(diag(Tm %*% Tm))) else 1
${rSph(S)}   # 분석 결정: 구형성
e <- if (use == "GG") gg else 1
FT <- (ssT / p) / (ssE / dfE); FI <- (ssI / (p * (a - 1))) / (ssE / dfE)
pT <- pf(FT, p * e, dfE * e, lower.tail = FALSE); pI <- pf(FI, p * (a - 1) * e, dfE * e, lower.tail = FALSE)
cat(sprintf("집단: F = %.2f, p = %.4f\\n시점: F = %.2f, p = %.4f\\n상호작용: F = %.2f, p = %.4f (ε = %.3f)\\n", FG, pG, FT, pT, FI, pI, e))
long <- data.frame(y = c(Y), time = factor(rep(colnames(Y), each = N), levels = colnames(Y)), g = rep(g, k), id = factor(rep(seq_len(N), k)))
print(summary(aov(y ~ g * time + Error(id/time), data = long)))   # 참고: 집단 크기가 같으면 위 값과 같음
eff(${rq(`${G} × 시점 상호작용`)}, ssI / (ssI + ssE), NA, NA, pI)
eff(${rq(`시점 주효과 (${r.ys.map(k => nm(S, k)).join(', ')})`)}, ssT / (ssT + ssE), NA, NA, pT)
eff(${rq(`${G} 주효과`)}, ssG / (ssG + ssEb), NA, NA, pG)`; },
    mcnemar: S => { const r = S.roles, t = S.vars[r.a].levels[0]; return `${rComplete(S, [r.a, r.b])}
lv <- ${rvec(S.vars[r.a].levels)}; a <- factor(catf(${rq(S.vars[r.a].code)}, lv)[u], levels = lv[1:2]); b <- factor(catf(${rq(S.vars[r.b].code)}, lv)[u], levels = lv[1:2])
tab <- table(a, b); print(tab); mc <- mcnemar.test(tab); print(mc)
print(binom.test(tab[2, 1], tab[1, 2] + tab[2, 1]))   # 정확 p
eff(${rq(`'${t}' 비율 변화 (${nm(S, r.b)} − ${nm(S, r.a)})`)}, mean(b == lv[1]) - mean(a == lv[1]), NA, NA, mc$p.value)`; },
    cochran: S => { const r = S.roles, t = S.vars[r.vars[0]].levels[0]; return `${rComplete(S, r.vars)}
X <- cbind(${r.vars.map(k => `as.numeric(catf(${rq(S.vars[k].code)}, ${rvec(S.vars[k].levels)})[u] == ${rq(t)})`).join(', ')})
k <- ncol(X); n <- nrow(X); Cj <- colSums(X); Ri <- rowSums(X); Nt <- sum(X)
Q <- (k - 1) * (k * sum(Cj^2) - Nt^2) / (k * Nt - sum(Ri^2)); p <- pchisq(Q, k - 1, lower.tail = FALSE)
cat(sprintf("Cochran Q(%d) = %.3f, p = %.4f\\n", k - 1, Q, p))
eff(${rq(`'${t}' 비율의 차이 (${r.vars.map(k => nm(S, k)).join(', ')})`)}, Q / (n * (k - 1)), NA, NA, p)`; }
  });

  // 분석 칸 처음 값: 반복측정은 점수 여러 개, 맥니마·코크란은 두 가지 분류
  const _autoRoles = autoRoles;
  autoRoles = function (method) {
    const roles = _autoRoles(method), vs = dataVars();
    const nums = vs.filter(v => v.type === 'num' && v.key[0] === 'S').map(v => v.key), cat2 = vs.filter(v => v.type === 'cat' && v.levels.length === 2).map(v => v.key);
    const dvs = vs.filter(v => v.type === 'num' && v.role === '종속').map(v => v.key);
    if (method === 'rm_anova' || method === 'mixed_anova') roles.ys = dvs.length >= 2 ? dvs : nums.slice(0, 3);
    if (method === 'mixed_anova') { const cats = vs.filter(v => v.type === 'cat'); roles.g = (cats.find(c => c.levels.length === 2) || cats[0])?.key || ''; }
    if (method === 'mcnemar') { roles.a = cat2[0] || ''; roles.b = cat2[1] || ''; }
    if (method === 'cochran') roles.vars = cat2;
    return roles;
  };

  // =================================================================

// ##### 5. 결과 해석: 학생이 읽은 결과 점검(gistCheck), 가설↔효과 연결(autoLink), 결과 문단 틀 (stats-ladder.html 5873~6100줄) #####
  // ---------- 결과의 줄기: 학생이 먼저 읽고, 코드가 판정과 맞는지 봄 ----------
  function readHint(e) {
    if (e.ciBased) return `신뢰구간 [${e.lo?.toFixed(3)}, ${e.hi?.toFixed(3)}]이 0을 포함하는지 보세요. 포함하지 않으면 간접효과가 있다고 읽습니다. 직접효과도 함께 보면 매개의 모양을 알 수 있어요.`;
    if (e.kind === 'omnibus') return 'F·χ² 검정은 "어딘가 차이(관계)가 있는지"만 알려 줍니다. 어느 집단끼리 다른지는 사후검정 표를 보세요. 효과크기(η², V)도 함께 읽습니다.';
    if (/상관/.test(e.label)) return 'r의 부호는 방향(+ 함께 커짐, − 반대로), 크기는 .10 작음 · .30 중간 · .50 큼(Cohen, 1988)으로 읽습니다.';
    if (/ − /.test(e.label)) return '두 집단의 평균(M)이 어느 쪽이 높은지, 그 차이가 유의한지(p), 얼마나 큰지(d: .2 작음 · .5 중간 · .8 큼)를 함께 읽습니다.';
    if (/→/.test(e.label)) return 'B는 원인이 1점 높을 때 결과가 몇 점 달라지는지, β는 다른 변인과 견줄 수 있는 표준화된 크기입니다. p < .05면 유의로 읽습니다.';
    return 'p < .05이면 통계적으로 유의하다고 읽고, 부호로 방향을, 효과크기로 크기를 읽습니다.';
  }
  function gistCheck(h) {
    const g = norm(h.gist), e = effById(h.effect), v = verdict(h); if (!g || !e || !v) return [];
    const out = [], neg = /(유의하지\s*않|유의미하지\s*않|없었|없다|없음|없는|나타나지\s*않|보이지\s*않|기각|지지되지\s*않|확인되지\s*않)/.test(g);
    const pos = /(유의(?!하지|미하지)|있었|있다|나타났|확인되었|지지되었|보였|수록|높았|낮았|컸|커졌|증가|감소|관련되|영향을\s*(미쳤|주었|줬))/.test(g.replace(/(유의하지\s*않|없었|나타나지\s*않|보이지\s*않)[^.]*/g, ''));
    if (v.sig && neg && !pos) out.push({ st: 'warn', t: `결과는 ${e.ciBased ? '신뢰구간이 0을 포함하지 않아' : 'p < .05로'} 유의한데, 효과가 없다고 읽었어요. ${e.stat}를 다시 보세요.` });
    if (!v.sig && pos && !neg) out.push({ st: 'warn', t: `결과는 ${e.ciBased ? '신뢰구간이 0을 포함해' : 'p ≥ .05로'} 통계적으로 유의하지 않은데, 효과가 있는 것처럼 읽었어요. 유의하지 않으면 "유의하지 않았다"로 읽습니다.` });
    if (v.sig && e.kind !== 'omnibus' && !/ − /.test(e.label)) {
      const sP = /(정적|양의|플러스|함께\s*(높|커|증가)|높을수록[^.,]*?(높|크|많|강)|클수록[^.,]*?(높|크|많)|많을수록[^.,]*?(높|크|많)|낮을수록[^.,]*?(낮|작|적))/.test(g);
      const sN = /(부적|음의|마이너스|반대로|높을수록[^.,]*?(낮|작|적|약)|클수록[^.,]*?(낮|작|적)|많을수록[^.,]*?(낮|적)|낮을수록[^.,]*?(높|크|많))/.test(g);
      if (sP && !sN && e.est < 0) out.push({ st: 'warn', t: '방향을 반대로 읽었어요. 계수(부호)가 −라서 한쪽이 높을수록 다른 쪽은 낮은 관계입니다.' });
      if (sN && !sP && e.est > 0) out.push({ st: 'warn', t: '방향을 반대로 읽었어요. 계수(부호)가 +라서 함께 높아지는 관계입니다.' });
    }
    if (/간접효과/.test(e.label) && /(완전\s*매개|부분\s*매개)/.test(g)) {
      const dir = e.analysis.effects.find(x => /^직접효과/.test(x.label));
      if (dir) {
        const dSig = sigOf(dir);
        if (/완전\s*매개/.test(g) && dSig) out.push({ st: 'warn', t: `완전매개로 읽었는데, 직접효과가 유의합니다(${dir.stat}). 직접효과가 남아 있으면 보통 부분매개로 읽습니다.` });
        if (/부분\s*매개/.test(g) && !dSig) out.push({ st: 'warn', t: `부분매개로 읽었는데, 직접효과가 유의하지 않습니다(${dir.stat}). 이런 경우 완전매개로 읽기도 합니다.` });
      }
      out.push({ st: 'info', t: '요즘은 완전·부분매개로 나누기보다 간접효과의 크기와 신뢰구간을 보고하는 쪽을 권하기도 합니다 (Hayes, 2018). 지도교수님 방식에 맞추세요.' });
    }
    if (/횡단/.test(intent.design) && /(때문|원인|야기|유발|인과|증명)/.test(g)) out.push({ st: 'warn', t: '한 시점 설문이라 인과로 읽을 수 없어요. "때문에", "원인" 대신 "관련된다", "설명한다"로 읽어 보세요.' });
    // 숫자는 이 효과의 값(통계값)과 견줌
    const P = new Set(numsIn(e.stat + ' ' + JSON.stringify(e.analysis.tables || [])).flatMap(x => { const k = numKey(x); return [k, k.replace(/^-/, '')]; }));
    const own = new Set(numsIn(e.stat).map(x => numKey(x).replace(/^-/, '')));
    const badN = numsIn(g).filter(x => { const k = numKey(x).replace(/^-/, ''); return !own.has(k) && !(/^\d+$/.test(k) && +k <= 20) && !/^(95|99|90)$/.test(k); });
    if (badN.length) out.push({ st: 'warn', t: `숫자 ${badN.slice(0, 4).join(', ')}${badN.some(x => P.has(numKey(x).replace(/^-/, ''))) ? '는 다른 효과의 값이에요.' : '는 결과표에 없어요.'} 이 효과의 값은 ${e.stat}입니다. 숫자는 다시 쓰지 않아도 됩니다.` });
    if (!out.some(x => x.st === 'warn')) out.unshift({ st: 'ok', t: `판정(${v.v})${v.sig && e.kind !== 'omnibus' ? '과 방향' : ''}에 맞게 읽었어요.` });
    return out;
  }
  const gistHtml = (h, i) => { const L = gistCheck(h); return L.length ? `<ul class="gist-ck">${L.map(x => `<li class="${x.st}">${x.st === 'ok' ? '✓' : x.st === 'warn' ? '!' : 'i'} ${esc(x.t)}</li>`).join('')}</ul>` : ''; };

  // 효과 이름을 문장 속 말로: "간접효과: A → M → Y" → "A → M → Y의 간접효과"
  function labelKo(l) {
    let m;
    if ((m = l.match(/^(총\s*간접효과|간접효과|직접효과|총효과):\s*(.+)$/))) return `${m[2]}의 ${m[1]}`;
    if ((m = l.match(/^경로:\s*(.+)$/))) return `${m[1]} 경로`;
    if ((m = l.match(/^(.+?) – (.+?) 상관$/))) return `${J(m[1], '과/와')} ${m[2]}의 상관`;
    if ((m = l.match(/^(.+?):\s*(.+?) − (.+)$/))) return `${m[1]}의 ${J(m[2], '과/와')} ${m[3]} 간 차이`;
    if ((m = l.match(/^([^→]+?) → ([^→]+)$/))) return `${J(m[1], '이/가')} ${m[2]}에 미치는 영향`;
    return l;
  }
  // AI 없이: 판정과 숫자로 결과 문단 틀을 만들고, 줄기는 논의 자리에 모음
  function templateDraft() {
    const R = [], D = [], cross = /횡단/.test(intent.design);
    intent.hyps.forEach((h, i) => {
      if (!norm(h.text)) return;
      const no = `가설 ${i + 1}`, e = effById(h.effect), v = verdict(h);
      if (!e) { R.push(`${no}: [검증할 효과를 2번에서 연결해 주세요]`); return; }
      const mname = (METHODS[e.analysis.method]?.name || e.analysis.title).replace(/\s*\(.*\)\s*$/, '');
      const sigW = e.ciBased ? (v.sig ? '부트스트랩 95% 신뢰구간이 0을 포함하지 않아 통계적으로 유의하였다' : '부트스트랩 95% 신뢰구간이 0을 포함하여 통계적으로 유의하지 않았다') : (v.sig ? '통계적으로 유의하였다' : '통계적으로 유의하지 않았다');
      const dirW = v.sig && e.kind !== 'omnibus' && !/ − /.test(e.label) ? `${e.est > 0 ? '정적' : '부적'} 방향으로 ` : '';
      R.push(`${J(no, '을/를')} 검증하기 위해 ${J(mname, '을/를')} 실시하였다. 그 결과, ${J(labelKo(e.label), '은/는')} ${dirW}${sigW}(${e.stat}). 따라서 ${J(no, '은/는')} ${v.v === '지지' ? '지지되었다' : '기각되었다'}.`);
      if (norm(h.gist)) D.push(`[${no} · 내가 읽은 결과] ${norm(h.gist)}\n[선행연구 비교: 이 결과를 어떤 선행연구와 견줄지]`);
    });
    return { results: R.join('\n\n'), discussion: D.length ? D.join('\n\n') : '[논의: 2번에서 가설마다 "내가 읽은 결과"를 적으면 여기에 모입니다]',
      limitations: cross ? ['한 시점에 모두 측정한 자료라 변인 사이의 인과관계를 단정할 수 없다.'] : [], check: ['통이 판정과 숫자로 만든 틀 문장입니다. 문장을 다듬고, 논의는 내가 읽은 결과를 바탕으로 이어 쓰세요.'] };
  }

  // 가설로 시작했으면, 계산한 결과에서 그 가설을 검증하는 효과를 찾아 연결해 둠 (바꿀 수 있음)
  function autoLink() {
    if (!(hp && hp.mode === 'hyp' && hp.plan && hp.plan.length) || !analyses.length) return false;
    let changed = false;
    intent.hyps.forEach(h => {
      if (h.effect || !norm(h.text)) return;
      const p = hp.plan.find(x => norm(x.text) === norm(h.text)); if (!p) return;
      const it = hp.items[p.no - 1] || {}, v = it.vars || {}, X = (v.iv || [])[0] || '', Y = v.dv || '', M = v.med || [];
      // 확정한 뒤면 확증 표시로, 아니면 그 가설의 변인 그대로 계산한 결과로 찾음
      const pi = planItems().find(x => x.no === p.no), lm = LADDER_TO_METHOD[p.result];
      const A = analyses.filter(a => (a.conf && a.conf.k === 'confirm' && (a.conf.hyps || []).includes(p.no)) || (lm && a.method === lm && a.spec && pi && pi.v && roleSig(a.spec.roles) === roleSig(rolesFor(pi, lm))));
      const pick = es => {
        const has = (e, n) => n && e.label.includes(n);
        if (it.type === 'rel') return es.find(e => /상관$/.test(e.label) && has(e, X) && has(e, Y)) || es.find(e => /상관$/.test(e.label));
        if (it.part && it.part !== 'main') { const k = it.part[0], i = +it.part.slice(1) || 0;
          if (k === 'c') return es.find(e => /^총효과/.test(e.label)) || es.find(e => /^직접효과/.test(e.label));
          if (k === 'a') return es.find(e => /^경로/.test(e.label) && has(e, X) && has(e, M[i]));
          if (k === 'b') return es.find(e => /^경로/.test(e.label) && has(e, M[i]) && has(e, Y));
          if (k === 'd') return es.find(e => /^경로/.test(e.label) && has(e, M[i]) && has(e, M[i + 1])); }
        if (it.type === 'med') { const ind = es.filter(e => /^간접효과/.test(e.label)); return v.serial ? ind.sort((a, b) => b.label.split('→').length - a.label.split('→').length)[0] : ind[0]; }
        if (it.type === 'modmed') return es.find(e => /조절된 매개 지수/.test(e.label));
        if (it.type === 'mod') return es.find(e => /^상호작용/.test(e.label));
        if (it.type === 'pred') return es.find(e => has(e, X) && e.label.includes('→')) || es[0];
        return es.find(e => has(e, Y)) || es[0];
      };
      for (const a of A) { const e = pick(a.effects); if (e) { h.effect = e.id; h.auto = true; changed = true; break; } }
    });
    if (changed) saveIntent();
    return changed;
  }
  function renderInterp() {
    const box = $('#in-main'); if (!box) return;
    autoLink();
    const effs = allEffects();
    const aiChip = aiUnset() ? '<span class="st missing">AI 정하기 전</span>' : aiMode === 'off' ? '<span class="st missing">AI 쓰지 않음</span>' : aiMode === 'copy' ? '<span class="st manual">채팅 AI에 복사해 묻기</span>' : aiMode === 'code' ? `<span class="st manual">AI 도우미 (${agentName()})</span>` : aiState === 'on' ? '<span class="st auto">AI 연결됨</span>' : aiState === 'checking' ? '<span class="st manual">AI 확인 중…</span>'
      : aiState === 'denied' ? '<span class="st missing">AI 사용이 허용되지 않음</span>' : '<span class="st missing">AI 없음 · claude.ai 링크에서 열면 됩니다</span>';
    const effOpts = cur => `<option value="">(효과 고르기)</option>` + analyses.map(a => `<optgroup label="${esc(a.title)}">${a.effects.map(e => `<option value="${e.id}" ${e.id === cur ? 'selected' : ''}>${esc(e.label)}</option>`).join('')}</optgroup>`).join('');
    const hyps = intent.hyps.map((h, i) => {
      const v = verdict(h); const e = effById(h.effect);
      return `<div class="hyp">
        <div class="hyp-no">가설 ${i + 1}</div>
        <div class="hyp-body">
          <label class="f">가설 문장<textarea rows="2" data-in-h="${i}" data-k="text" placeholder="예: 능력주의 신념은 사회비교경향성과 외로움을 차례로 거쳐 외집단 혐오에 정적 영향을 미칠 것이다">${esc(h.text)}</textarea></label>
          <div class="grid2" style="margin-top:10px">
            <label class="f">검증할 통계 효과${h.auto && h.effect ? ' <span class="st auto" title="가설로 시작해 계산한 결과에서 찾아 연결했습니다. 다르면 고르세요.">자동 연결</span>' : ''}<select data-in-h="${i}" data-k="effect">${effOpts(h.effect)}</select></label>
            <label class="f">예상 방향<select data-in-h="${i}" data-k="expect">${Object.entries(EXPECT).map(([k, l]) => `<option value="${k}" ${k === h.expect ? 'selected' : ''}>${esc(l)}</option>`).join('')}</select></label>
          </div>
          ${e ? `<div class="verdict ${v.v === '지지' ? 'ok' : 'no'}"><b>${v.v}</b><span>${esc(e.stat)}</span><small>${esc(v.why)}</small></div>
          <div class="gist"><label class="f">내가 읽은 결과 (줄기) <small>숫자는 다시 쓰지 않아도 돼요. 이 결과가 무엇을 뜻하는지 내 말로 한두 줄</small>
            <textarea rows="2" data-in-h="${i}" data-k="gist" placeholder="${esc(v.sig ? (e.ciBased ? '예: 능력주의 신념이 높으면 인내력 부족을 거쳐 사회비교경향성이 높아지는 길이 있었다. 직접효과도 남아 있었다.' : '예: 능력주의 신념이 높을수록 사회비교경향성도 높았고, 그 관계는 중간 정도였다.') : '예: 두 집단의 차이는 통계적으로 유의하지 않았다. 표본에서 성별 차이가 크지 않았던 것 같다.')}">${esc(h.gist || '')}</textarea></label>
            <p class="hint gist-how"><b>읽는 법</b> ${esc(readHint(e))}</p>
            <div id="gist-ck-${i}">${gistHtml(h, i)}</div></div>` : ''}
        </div>
        <button class="icon-btn" data-in-act="h-del" data-i="${i}" title="가설 지우기" ${intent.hyps.length < 2 ? 'disabled' : ''}>×</button>
      </div>`;
    }).join('');

    const ck = intent.check;
    let mode = intent.mode || (aiOn() || (aiMode === 'auto' && aiState === 'checking') ? 'auto' : aiMode === 'code' ? 'code' : 'chat');
    if (mode === 'auto' && !(aiMode === 'auto' && aiState !== 'off')) mode = aiMode === 'code' ? 'code' : 'chat';
    if (aiMode === 'off') mode = 'off';
    const checkHtml = ck ? `<div class="ai-box">
      <div class="ai-label">AI가 이해한 연구 의도</div>
      <p>${esc(ck.understanding)}</p>
      ${(ck.hypotheses || []).length ? `<ul class="ai-hyps">${ck.hypotheses.map(h => `<li><b>가설 ${esc(h.no)}</b> ${esc(h.restatement)} ${h.fit === '알맞음' ? '<span class="st auto">효과 연결 알맞음</span>' : `<span class="st missing">확인 필요</span> <span class="hint">${esc(h.note || '')}</span>`}</li>`).join('')}</ul>` : ''}
      ${(ck.questions || []).length ? `<div class="ai-label" style="margin-top:14px">AI의 질문 (답하면 해석에 반영됩니다)</div>
        ${ck.questions.map((q, i) => `<label class="f" style="margin-top:8px">${esc(q)}<input data-in-ans="${i}" value="${esc(intent.answers[i] || '')}" placeholder="짧게 답해 주세요"></label>`).join('')}` : ''}
      <label class="chk" style="margin-top:14px"><input type="checkbox" data-in-k="confirmed" ${intent.confirmed ? 'checked' : ''}> AI가 이해한 내용이 맞습니다</label>
    </div>` : '';

    const out = intent.out;
    const outHtml = out ? `<div class="card" id="in-out">
      <h3 class="card-title">${out.src && /틀/.test(out.src) ? '결과 · 논의 틀' : 'AI가 쓴 결과 · 논의 초안'}${out.src ? ` <span class="st manual">${esc(out.src)}</span>` : ''}</h3>
      ${out.sample ? '<div class="errbox">예시(가짜) 자료로 계산한 결과입니다. 논문에 쓰지 마세요.</div>' : ''}
      ${out.issues.length ? `<div class="section-title">자동 점검: 확인할 곳 ${out.issues.length}개</div><ul class="warn-list">${out.issues.map(x => `<li>${esc(x)}</li>`).join('')}</ul>` : '<ul class="warn-list"><li class="ok">자동 점검 통과: 숫자가 모두 분석 결과에 있고, 판정과 다른 서술이나 인과 단정 표현을 찾지 못했습니다.</li></ul>'}
      <div class="section-title">결과</div><div class="ai-text" id="in-res">${hl(out.results, out.badNums)}</div>
      <div class="section-title">논의</div><div class="ai-text">${hl(out.discussion, out.badNums)}</div>
      ${(out.limitations || []).length ? `<div class="section-title">연구의 한계</div><ul class="notes">${out.limitations.map(x => `<li>${esc(x)}</li>`).join('')}</ul>` : ''}
      ${(out.check || []).length ? `<div class="section-title">연구자가 확인할 것</div><ul class="notes">${out.check.map(x => `<li>${esc(x)}</li>`).join('')}</ul>` : ''}
      <p class="hint">노란 표시는 분석 결과에서 찾지 못한 숫자입니다. 초안은 연구자가 읽고 고쳐 쓰는 출발점입니다. 선행연구 인용은 넣지 않았으니 직접 채워 주세요.</p>
      <div class="actions"><button class="btn primary" data-in-act="copy-out">초안 복사</button><button class="btn" data-in-act="dl-out">텍스트로 내려받기</button>${out.src ? '' : '<button class="btn" data-in-act="write" data-fresh="1">다시 쓰기</button>'}</div>
    </div>` : '';

    box.innerHTML = `
      ${!analyses.length ? `<div class="card"><div class="errbox">아직 계산한 결과가 없습니다. [분석] 메뉴에서 먼저 계산해 주세요. 가설마다 검증할 효과를 골라야 판정할 수 있습니다.</div><div class="actions"><button class="btn primary" data-in-act="to-analysis">분석으로</button></div></div>` : ''}
      <div class="card">
        <div class="res-head"><h3 class="card-title">1. 연구 의도</h3>${aiChip}</div>
        <p class="hint">AI는 여기 적은 내용과 계산 결과표만 봅니다. 응답 원자료는 보내지 않습니다.</p>
        <div class="field-grid">
          <label class="f">논문 제목<input data-in-k="title" value="${esc(intent.title)}" placeholder="예: 능력주의 신념이 사회비교경향성·외로움을 거쳐 외집단 혐오에 미치는 영향"></label>
          <label class="f">연구 목적 · 연구 문제<textarea rows="3" data-in-k="purpose" placeholder="무엇을 밝히려는 연구인지 두세 문장으로">${esc(intent.purpose)}</textarea></label>
          <div class="grid2">
            <label class="f">연구 설계<select data-in-k="design">${DESIGNS.map(d => `<option ${d === intent.design ? 'selected' : ''}>${esc(d)}</option>`).join('')}</select></label>
            <label class="f">표본<input value="${lastScored ? `분석에 포함된 응답 ${lastScored.inc.length}명` : (analyses[0] ? `N = ${analyses[0].n}` : '-')}" disabled></label>
          </div>
          <label class="f">이론적 배경 (한두 줄)<textarea rows="2" data-in-k="theory" placeholder="예: 사회비교이론(Festinger, 1954)에 따르면 …">${esc(intent.theory)}</textarea></label>
          <label class="f">해석할 때 특히 신경 쓰는 점 (선택)<textarea rows="2" data-in-k="concern" placeholder="예: 외집단 혐오 점수가 전반적으로 낮게 나올 수 있다는 점">${esc(intent.concern)}</textarea></label>
        </div>
      </div>
      <div class="card">
        <h3 class="card-title">2. 가설과 판정</h3>
        <p class="hint">판정은 코드가 정해진 규칙으로 합니다. 간접효과는 95% 신뢰구간이 0을 포함하지 않으면, 나머지는 p < .05이면 유의로 보고, 예상 방향과 같을 때만 '지지'입니다. AI는 이 판정을 바꿀 수 없습니다.</p>
        <div class="hyps">${hyps}</div>
        <div class="item-tools"><button class="btn sm" data-in-act="h-add">+ 가설 추가</button></div>
      </div>
      <div class="card">
        <div class="res-head"><h3 class="card-title">3. AI와 함께 해석하기</h3><a class="guide-link" href="#guide-ai">AI 연결 방법 설명</a></div>
        <p class="hint">어느 방법을 쓰든 판정은 위의 코드 판정을 따르고, 초안의 숫자와 판정은 이 화면이 다시 점검합니다. 먼저 2번에서 가설마다 <b>내가 읽은 결과</b>를 적어 두면 AI가 그 해석을 뼈대로 씁니다.${intent.hyps.some(h => h.effect && !norm(h.gist)) ? ` <span class="st missing">아직 안 적은 가설 ${intent.hyps.filter(h => h.effect && !norm(h.gist)).length}개</span>` : ''}</p>
        <div class="tmpl-box"><b>AI 없이</b> 판정과 숫자로 결과 문단의 틀을 만들고, 내가 읽은 결과를 논의 자리에 모아 둡니다. <button class="btn sm" data-in-act="tmpl">결과 문단 틀 만들기</button></div>
        ${mode === 'off' && aiUnset() ? aiSuggest('결과를 두고 AI와 상의하고 싶으면 방법을 고르세요.') : mode === 'off' ? `<div class="notice">AI를 쓰지 않도록 설정했습니다. 아무것도 밖으로 보내지 않습니다. 가설 판정과 결과표는 그대로 쓸 수 있으니, 해석 글은 직접 쓰고 아래 [초안 점검] 칸에 붙여 넣으면 숫자·판정을 점검합니다. <button class="btn sm" data-ai-mode-open>AI 방식 바꾸기</button></div>` : ''}
        <div class="mode-tabs" role="tablist" ${mode === 'off' ? 'hidden' : ''}>
          ${[['auto', 'Claude 바로 쓰기', '이 화면에서'], ['chat', '채팅 AI에 복사', 'ChatGPT·Claude 등'], ['code', 'AI 도우미', `${agentName()} · 연구 폴더`]].filter(([k]) => k !== 'auto' || (aiMode === 'auto' && aiState !== 'off')).map(([k, l, sub]) => `<button class="mode-tab" role="tab" data-in-mode="${k}" aria-selected="${mode === k}"><b>${l}</b><span>${sub}</span></button>`).join('')}
        </div>
        ${mode === 'auto' ? `
          ${aiOn() ? '' : `<div class="notice">이 화면에서는 Claude 바로 쓰기를 할 수 없습니다. ${aiState === 'denied' ? 'AI 사용이 허용되지 않았거나 이 계정에서 쓸 수 없습니다.' : '잠시 뒤 다시 보세요.'} 다른 방법은 위의 [채팅 AI에 복사]나 [AI 도우미]를 고르세요.</div>`}
          <ol class="steps">
            <li><b>의도 확인</b>: AI가 연구 목적과 가설을 다시 말해 보고, 효과 연결이 맞는지 보고, 빠진 정보를 묻습니다.</li>
            <li><b>풀어 쓰기</b>: 2번에 적은 <b>내가 읽은 결과</b>를 뼈대로, 판정과 숫자를 그대로 써서 결과·논의를 논문 문체로 풀어 씁니다. 내 해석에 없는 주장은 덧붙이지 않고 [연구자 보완] 자리로 남깁니다. 쓴 뒤 코드가 다시 점검합니다.</li>
          </ol>
          ${checkHtml}
          <div class="actions">
            <button class="btn ${ck ? '' : 'primary'}" data-in-act="check" ${aiOn() ? '' : 'disabled'}>${ck ? '의도 다시 확인' : '① AI가 내 의도 확인하기'}</button>
            <button class="btn ${ck ? 'primary' : ''}" data-in-act="write" ${aiOn() ? '' : 'disabled'}>② 내 해석으로 결과·논의 풀어 쓰기</button>
            <button class="btn" data-in-act="stop" id="in-stop" hidden>멈추기</button>
            <span class="hint" id="in-prog"></span>
          </div>` : ''}
        ${mode === 'chat' ? `
          <p class="hint">내가 쓰는 AI 채팅(ChatGPT, Claude, Gemini 등)이면 됩니다. 무료 계정도 됩니다. 지시문을 복사해 붙여 넣고, AI의 답을 다시 여기에 붙여 넣으세요. <a href="#" data-ai-mode-open>쓰는 AI 바꾸기</a></p>
          <div class="chat-steps">
            <div class="chat-step"><span class="n">1</span><div><b>의도 확인 지시문을 AI 채팅에 붙여 넣기</b>
              <div class="actions tight"><button class="btn sm primary" data-in-act="copy-prompt" data-which="check">① 의도 확인 지시문 복사</button>${aiGo('data-in-act="copy-prompt" data-which="check"')}</div></div></div>
            <div class="chat-step"><span class="n">2</span><div><b>AI의 답을 붙여 넣고, AI가 물은 것에 답하기</b>
              <label class="f">AI의 답<textarea rows="4" data-in-chat="aiText" placeholder="AI가 이해한 연구 목적, 가설별 확인, 질문을 붙여 넣으세요">${esc(intent.chat.aiText)}</textarea></label>
              <label class="f" style="margin-top:8px">AI의 질문에 대한 내 답<textarea rows="3" data-in-chat="myAnswers" placeholder="예: 1. 외집단은 이주민과 성소수자를 말함 2. 대학 커뮤니티에서 모집함">${esc(intent.chat.myAnswers)}</textarea></label>
              <p class="hint">AI가 '확인 필요'라고 한 가설은 위의 2번에서 효과 연결을 먼저 고치세요.</p></div></div>
            <div class="chat-step"><span class="n">3</span><div><b>초안 쓰기 지시문을 ①과 같은 대화창에 붙여 넣기</b>
              <div class="actions tight"><button class="btn sm primary" data-in-act="copy-prompt" data-which="write">② 초안 쓰기 지시문 복사</button></div></div></div>
            <div class="chat-step"><span class="n">4</span><div><b>AI가 쓴 초안을 아래 [초안 점검] 칸에 붙여 넣기</b></div></div>
          </div>` : ''}
        ${mode === 'code' ? `
          <p class="hint">lazzzyResearch처럼 내 컴퓨터의 AI 도우미가 연구 폴더를 보고 대화하며 해석합니다. 쓰는 도우미: ${agentChips()} 처음이면 <a href="#guide-claude-code">처음부터 따라 하기</a>를 보세요.</p>
          <div class="chat-steps">
            <div class="chat-step"><span class="n">1</span><div><b>연구 폴더를 연결하기</b> (연결했으면 파일이 이미 들어 있습니다. 아니면 새 폴더에 아래 두 파일을 받아 넣기)
              <div class="actions tight"><button class="btn sm primary" data-in-act="dl-claude-md">${AGENTS[aiAgent].file} 받기</button><button class="btn sm primary" data-in-act="dl-json">interpretation.json 받기</button></div>
              <p class="hint">${AGENTS[aiAgent].file}는 AI 도우미가 지킬 규칙, interpretation.json은 연구 의도·가설·판정·결과표입니다. 응답 원자료는 들어 있지 않습니다. 가설이나 분석을 바꾸면 interpretation.json을 다시 받으세요.</p></div></div>
            <div class="chat-step"><span class="n">2</span><div><b>${agentName()}로 그 폴더 열기</b>
              <p class="hint" style="margin-top:4px">${{ claude: '<b>Claude 데스크톱 앱</b> 위쪽 <b>Code</b> 탭 → <b>Local</b> → <b>Select folder</b>. 터미널이 필요 없습니다. (<a href="https://claude.ai/download" target="_blank" rel="noopener">앱 설치</a> · 터미널이 익숙하면 그 폴더에서 <code>claude</code> 실행)', codex: '<b>Codex 앱</b>에서 그 폴더를 열거나, 그 폴더에서 터미널을 열고 <code>codex</code> 실행. 처음에 ChatGPT 계정으로 로그인합니다. (<a href="https://developers.openai.com/codex" target="_blank" rel="noopener">설치 안내</a>)', google: '<b>Antigravity 앱</b>에서 그 폴더를 엽니다. 처음에 구글 계정으로 로그인합니다. (<a href="https://antigravity.google" target="_blank" rel="noopener">앱 설치</a>)' }[aiAgent]}</p>
              <p class="hint">"통 해석 모드입니다."가 나오면 성공입니다. "해석해 줘"라고 쓰면 시작합니다.</p></div></div>
            <div class="chat-step"><span class="n">3</span><div><b>AI가 묻는 것에 답하면 폴더에 <code>draft.md</code>가 생깁니다</b> (연구 폴더를 연결했으면 통 창으로 돌아올 때 저절로 들어옵니다)</div></div>
            <div class="chat-step"><span class="n">4</span><div><b>draft.md 내용을 아래 [초안 점검] 칸에 붙여 넣기</b></div></div>
          </div>` : ''}
        ${mode !== 'auto' ? `<div class="paste-box">
          <label class="f">초안 점검: AI가 쓴 초안 붙여 넣기<textarea rows="7" id="in-draft" placeholder="[결과] [논의] [연구의 한계] [확인할 것] 제목이 있는 초안을 붙여 넣으세요">${esc(intent.chat.draft)}</textarea></label>
          <div class="actions tight"><button class="btn primary" data-in-act="check-draft">초안 점검하기</button></div>
        </div>` : ''}
      </div>
      ${outHtml}
      ${reviewHtml(mode)}`;
  }


// ##### 6. 보고 체크리스트: 효과크기 신뢰구간, 정규성, 가정 점검 (stats-ladder.html 6966~7178줄) #####
  // =================================================================
  // 보고 체크리스트 (APA 7 결과 보고 항목을 코드가 점검)
  // =================================================================
  const CUTS = { d: [0.2, 0.5, 0.8], r: [0.1, 0.3, 0.5], eta: [0.01, 0.06, 0.14], f2: [0.02, 0.15, 0.35] };
  function sizeLabel(kind, v, dfStar) {
    const a = Math.abs(v); let c = CUTS[kind];
    if (kind === 'V') c = CUTS.r.map(x => x / Math.sqrt(Math.max(1, dfStar || 1)));
    if (!c || !isFinite(a)) return '';
    return a < c[0] ? '아주 작음' : a < c[1] ? '작음' : a < c[2] ? '중간' : '큼';
  }
  const CUT_TEXT = { d: 'Cohen(1988): .20 작음, .50 중간, .80 큼', r: 'Cohen(1988): .10 작음, .30 중간, .50 큼', eta: 'Cohen(1988): .01 작음, .06 중간, .14 큼', f2: 'Cohen(1988): .02 작음, .15 중간, .35 큼' };
  const fisherCI = (r, n, k = 0) => { const z = Math.atanh(r), se = 1 / Math.sqrt(n - 3 - k); return [Math.tanh(z - 1.96 * se), Math.tanh(z + 1.96 * se)]; };
  const dCI = (d, n1, n2) => { const se = Math.sqrt((n1 + n2) / (n1 * n2) + d * d / (2 * (n1 + n2))); return [d - 1.96 * se, d + 1.96 * se]; };
  const dzCI = (d, n) => { const se = Math.sqrt(1 / n + d * d / (2 * n)); return [d - 1.96 * se, d + 1.96 * se]; };

  // 정규성: 여러 묶음의 Shapiro–Wilk + 왜도·첨도
  function normality(sets) {
    const rows = sets.map(([label, arr]) => { const sw = shapiro(arr); return { label, n: arr.length, W: sw?.W, p: sw?.p, sk: skew(arr), ku: kurt(arr) }; }).filter(r => r.n >= 3);
    const bad = rows.filter(r => r.p < 0.05), big = rows.some(r => r.n >= 200);
    const shapeOk = rows.every(r => !(Math.abs(r.sk) >= 2 || Math.abs(r.ku) >= 7));
    let st = 'ok', note = '모든 묶음에서 Shapiro–Wilk p ≥ .05로 정규성 가정이 크게 어긋나지 않았습니다.';
    if (bad.length) {
      st = shapeOk ? 'ok' : 'warn';
      note = `Shapiro–Wilk p < .05인 묶음이 ${bad.length}개 있습니다 (${bad.map(r => r.label).slice(0, 3).join(', ')}${bad.length > 3 ? ' 등' : ''}).` +
        (shapeOk ? ` 다만 왜도 절댓값 2, 첨도 절댓값 7 미만이라 심하게 벗어나지는 않았다고 보고할 수 있습니다.${big ? ' 표본이 크면 작은 벗어남에도 검정이 유의해지기 쉽습니다.' : ''}` : ' 왜도나 첨도도 큰 편이라 비모수 검정이나 부트스트랩을 함께 검토하세요.');
    }
    return { rows, st, note };
  }
  const normTable = rows => ({ title: '가정 검정: 정규성 (Shapiro–Wilk)', head: ['대상', 'n', 'W', 'p', '왜도', '첨도'], num: [1, 2, 3, 4, 5], rows: rows.map(r => [r.label, String(r.n), r.W == null ? '-' : F2(r.W, 3), PV(r.p), F2(r.sk), F2(r.ku)]) });
  const vifCheck = vifs => {
    const mx = Math.max(...vifs.filter(isFinite));
    return !vifs.length || !isFinite(mx) ? { item: '다중공선성 (VIF)', st: 'na', note: '예측변인이 하나라 해당 없음' }
      : { item: '다중공선성 (VIF)', st: mx < 10 ? 'ok' : 'warn', note: `가장 큰 VIF = ${F2(mx)}${mx < 10 ? ' (10 미만)' : ' — 10 이상이라 다중공선성을 의심할 수 있습니다'}` };
  };
  const residPlot = { item: '잔차의 등분산성·선형성', st: 'warn', note: '결과 아래의 잔차 그림(예측값 × 잔차)을 보고, 깔때기나 곡선 모양이 없는지 확인해 결과에 한 줄 적으세요.' };

  // 방법별로 필요한 값을 다시 계산해 체크 항목을 만듦
  const CHECKS = {
    pearson(r, R) { return corrChecks(r.vars, R, false); },
    spearman(r, R) { return corrChecks(r.vars, R, true); },
    partial(r, R) {
      const P = prep({ x: r.x, y: r.y, c: r.covs }); const k = designCols(P, r.covs).cols.length;
      const e = R.effects[0], ci = fisherCI(e.est, P.n, k), N = normality([[P.V[r.x].name, P.col(r.x)], [P.V[r.y].name, P.col(r.y)]]);
      return { desc: descOf(P, [r.x, r.y]), es: { text: `편상관 r = ${NZ(e.est)} (${sizeLabel('r', e.est)})`, cut: 'r' }, ci: { st: 'ok', note: `r의 95% CI ${CI(ci[0], ci[1])} (Fisher z)` }, norm: N, assum: [], post: null };
    },
    pointbiserial(r, R) {
      const P = prep({ g: r.g, y: r.y }), g = P.col(r.g), y = P.col(r.y), lv = P.V[r.g].levels.filter(l => g.includes(l));
      const G = lv.map(l => y.filter((_, i) => g[i] === l)), e = R.effects[0], ci = fisherCI(e.est, P.n), L = levene(G);
      return { desc: descOf(P, [r.y]), es: { text: `r = ${NZ(e.est)} (${sizeLabel('r', e.est)})`, cut: 'r' }, ci: { st: 'ok', note: `r의 95% CI ${CI(ci[0], ci[1])}` }, norm: normality(G.map((x, i) => [`${P.V[r.y].name} (${lv[i]})`, x])), assum: [{ item: '등분산성 (Levene)', st: L.p >= 0.05 ? 'ok' : 'warn', note: `F = ${F2(L.F)}, ${PS(L.p)}` }], post: null };
    },
    chisq(r, R) {
      const e = R.effects[0], P = prep({ a: r.a, b: r.b });
      const ka = P.V[r.a].levels.filter(l => P.col(r.a).includes(l)).length, kb = P.V[r.b].levels.filter(l => P.col(r.b).includes(l)).length, dfs = Math.min(ka, kb) - 1;
      const lowNote = R.tables[1]?.rows[0]?.[4] || '';
      const lowN = +(lowNote.match(/^(\d+)/) || [0, 0])[1], cells = ka * kb;
      return { desc: { st: 'ok', note: '교차표에 빈도와 비율을 함께 보고했습니다.' }, es: { text: `Cramér의 V = ${NZ(e.est)} (${sizeLabel('V', e.est, dfs)}, df* = ${dfs} 기준)`, cutText: `Cohen(1988), df* = ${dfs}: ${[0.1, 0.3, 0.5].map(x => NZ(x / Math.sqrt(dfs))).join(' / ')}` },
        ci: { st: 'na', note: 'V의 신뢰구간은 이 사이트에서 계산하지 않음 — 필요하면 jamovi 등에서 보충' }, norm: { na: '빈도 자료라 정규성 가정이 없음' },
        assum: [{ item: '기대빈도', st: lowN / cells <= 0.2 ? 'ok' : 'warn', note: `기대빈도 5 미만 칸 ${lowNote}${lowN / cells > 0.2 ? ' — 20%를 넘어 피셔 정확검정이나 범주 합치기를 검토하세요' : ''}` }], post: null };
    },
    onesample(r, R) {
      const P = prep({ y: r.y }), y = P.col(r.y), d = (mean(y) - Number(anSel.opts.testValue)) / sd(y), ci = dzCI(d, P.n);
      return { desc: descOf(P, [r.y]), es: { text: `Cohen의 d = ${F2(d)} (${sizeLabel('d', d)})`, cut: 'd' }, ci: { st: 'ok', note: `평균 차이의 95% CI 보고, d의 95% CI ${CI(ci[0], ci[1])} (근사)` }, norm: normality([[P.V[r.y].name, y]]), assum: [], post: null };
    },
    ind_t(r, R) {
      const sets = [], es = [], assum = [];
      r.ys.forEach(yk => {
        const P = prep({ g: r.g, y: yk }), g = P.col(r.g), y = P.col(yk), lv = P.V[r.g].levels.filter(l => g.includes(l));
        const a = y.filter((_, i) => g[i] === lv[0]), b = y.filter((_, i) => g[i] === lv[1]);
        const sp = Math.sqrt(((a.length - 1) * variance(a) + (b.length - 1) * variance(b)) / (a.length + b.length - 2)), d = (mean(a) - mean(b)) / sp, ci = dCI(d, a.length, b.length);
        sets.push([`${P.V[yk].name} (${lv[0]})`, a], [`${P.V[yk].name} (${lv[1]})`, b]);
        es.push(`${P.V[yk].name}: d = ${F2(d)} (${sizeLabel('d', d)}), 95% CI ${CI(ci[0], ci[1])}`);
        const L = levene([a, b]); assum.push({ item: `등분산성 (Levene) · ${P.V[yk].name}`, st: 'ok', note: `F = ${F2(L.F)}, ${PS(L.p)}${(R.spec?.opts?.welch || 'always') === 'never' ? (L.p < 0.05 ? ' — 분산이 달라 Student t가 맞지 않을 수 있습니다. Welch t를 검토하세요' : '') : (R.spec?.opts?.welch === 'auto' ? (L.p < 0.05 ? ' — 어긋나서 Welch t를 보고했습니다' : '') : ' — Welch t는 등분산을 가정하지 않아 결과에 영향이 없습니다')}` });
      });
      return { desc: { st: 'ok', note: '집단별 평균(표준편차)을 보고했습니다.' }, es: { text: es.join(' / '), cut: 'd' }, ci: { st: 'ok', note: 'd의 95% CI는 근사식(Hedges & Olkin)으로 계산했습니다.' }, norm: normality(sets), assum, post: null };
    },
    paired_t(r, R) {
      const P = prep({ a: r.a, b: r.b }), dif = P.col(r.a).map((v, i) => v - P.col(r.b)[i]), dz = mean(dif) / sd(dif), ci = dzCI(dz, P.n);
      return { desc: { st: 'ok', note: '두 점수의 평균·표준편차를 보고했습니다.' }, es: { text: `Cohen의 dz = ${F2(dz)} (${sizeLabel('d', dz)}), 95% CI ${CI(ci[0], ci[1])}`, cut: 'd' }, ci: { st: 'ok', note: '평균 차이와 dz의 95% CI를 보고했습니다.' }, norm: normality([['두 점수의 차이', dif]]), assum: [], post: null };
    },
    oneway(r, R) {
      const sets = [], es = [], assum = [];
      r.ys.forEach(yk => {
        const P = prep({ g: r.g, y: yk }), g = P.col(r.g), y = P.col(yk), lv = P.V[r.g].levels.filter(l => g.includes(l));
        const G = lv.map(l => y.filter((_, i) => g[i] === l)), N = y.length, k = G.length, gm = mean(y);
        const ssb = G.reduce((s, x) => s + x.length * (mean(x) - gm) ** 2, 0), ssw = G.reduce((s, x) => s + x.reduce((q, v) => q + (v - mean(x)) ** 2, 0), 0), msw = ssw / (N - k);
        const eta = ssb / (ssb + ssw), omega = Math.max(0, (ssb - (k - 1) * msw) / (ssb + ssw + msw));
        es.push(`${P.V[yk].name}: η² = ${NZ(eta, 3)} (${sizeLabel('eta', eta)}), ω² = ${NZ(omega, 3)}`);
        sets.push([`${P.V[yk].name} 잔차`, G.flatMap(x => x.map(v => v - mean(x)))]);
        const L = levene(G); assum.push({ item: `등분산성 (Levene) · ${P.V[yk].name}`, st: L.p >= 0.05 ? 'ok' : 'warn', note: `F = ${F2(L.F)}, ${PS(L.p)}${L.p < 0.05 ? ' — Welch F와 Games–Howell 사후검정을 함께 검토하세요' : ''}` });
      });
      return { desc: { st: 'ok', note: '집단별 n, M, SD를 보고했습니다.' }, es: { text: es.join(' / '), cut: 'eta', extra: 'ω²는 η²보다 덜 부풀려진 효과크기입니다.' }, ci: { st: 'warn', note: 'η²의 신뢰구간은 이 사이트에서 계산하지 않습니다. 필요하면 jamovi 등에서 보충하세요.' }, norm: normality(sets), assum,
        post: { st: 'ok', note: `${anSel.opts.posthoc === 'bonferroni' ? 'Bonferroni' : 'Scheffé'} 사후검정을 보고했습니다 (분석 결정). 사후검정 방법을 연구방법에 밝히세요.` } };
    },
    factorial(r, R) {
      const P = prep({ a: r.a, b: r.b, y: r.y }), A = P.col(r.a), B = P.col(r.b), y = P.col(r.y);
      const cells = [...new Set(A.map((a, i) => a + '' + B[i]))].map(key => y.filter((_, i) => A[i] + '' + B[i] === key));
      const L = levene(cells.filter(c => c.length > 1));
      return { desc: { st: 'ok', note: '칸별 평균(SD, n)을 보고했습니다.' }, es: { text: R.effects.map(e => `${e.label.replace(/ 효과.*$/, '')}: 부분 η² = ${NZ(e.est, 3)} (${sizeLabel('eta', e.est)})`).join(' / '), cut: 'eta' },
        ci: { st: 'warn', note: '부분 η²의 신뢰구간은 계산하지 않습니다.' }, norm: normality([['칸 평균에서의 잔차', cells.flatMap(c => c.map(v => v - mean(c)))]]),
        assum: [{ item: '등분산성 (Levene, 칸 기준)', st: L.p >= 0.05 ? 'ok' : 'warn', note: `F = ${F2(L.F)}, ${PS(L.p)}` }],
        post: { st: R.effects[2] && sigOf(R.effects[2]) ? 'warn' : 'ok', note: R.effects[2] && sigOf(R.effects[2]) ? '상호작용이 유의합니다. 단순주효과 분석은 이 사이트에서 하지 않으니 jamovi 등에서 보충하세요.' : '상호작용이 유의하지 않아 주효과 중심으로 해석할 수 있습니다. 세 집단 이상 기준의 주효과가 유의하면 사후검정을 보충하세요.' } };
    },
    ancova(r, R) {
      const P = prep({ g: r.g, y: r.y, c: r.covs }), g = P.col(r.g), y = P.col(r.y), lv = P.V[r.g].levels.filter(l => g.includes(l));
      const dum = lv.slice(1).map(l => g.map(v => (v === l ? 1 : 0))), fit = ols(y, rowsOf([...dum, ...r.covs.map(k => P.col(k))]));
      const slope = (R.notes.find(n => n.startsWith('회귀 기울기')) || '');
      return { desc: descOf(P, [r.y, ...r.covs]), es: { text: `${P.V[r.g].name}: 부분 η² = ${NZ(R.effects[0].est, 3)} (${sizeLabel('eta', R.effects[0].est)})`, cut: 'eta' }, ci: { st: 'warn', note: '부분 η²의 신뢰구간은 계산하지 않습니다.' },
        norm: normality([['잔차', fit.resid]]), assum: [{ item: '회귀 기울기 동질성', st: /전제가 어긋/.test(slope) ? 'warn' : 'ok', note: slope.replace(/^회귀 기울기 동질성: /, '') }, vifCheck(vifs(r.covs.map(k => P.col(k))))],
        post: lv.length > 2 ? { st: 'warn', note: '세 집단 이상이면 조정된 평균의 사후 비교(Bonferroni 등)를 보충하세요.' } : null };
    },
    regression(r, R) { return regChecks(r.y, r.xs, R); },
    hier(r, R) { return regChecks(r.y, [r.b1, r.b2, r.b3 || []].flat(), R, true); },
    logistic(r, R) {
      const P = prep({ y: r.y, x: r.xs }), D = designCols(P, r.xs);
      return { desc: descOf(P, r.xs.filter(k => P.V[k].type === 'num')), es: { text: 'Exp(B)(승산비)와 Nagelkerke R²를 보고했습니다.', cutText: '승산비는 1에서 멀수록 효과가 큽니다. 널리 합의된 작음·중간·큼 기준은 없습니다.' }, ci: { st: 'ok', note: 'Exp(B)의 95% CI를 보고했습니다.' }, norm: { na: '로지스틱 회귀는 정규성 가정이 없음' },
        assum: [vifCheck(vifs(D.cols)), { item: '로짓의 선형성', st: 'warn', note: '점수 예측변인과 로짓의 선형성(Box–Tidwell)은 이 사이트에서 점검하지 않습니다.' }], post: null };
    },
    med_parallel(r, R) { return medChecks(r.x, r.ms, r.y, r.covs, R); },
    sem(r, R) { return semChecks(R); },
    sem_serial(r, R) { return semChecks(R); },
    med_serial(r, R) { return medChecks(r.x, [r.m1, r.m2], r.y, r.covs, R, true); },
    moderation(r, R) {
      const P = prep({ x: r.x, w: r.w, y: r.y, c: r.covs }), X = binCode(P, r.x), W = binCode(P, r.w), y = P.col(r.y), C = designCols(P, r.covs).cols;
      const xc = anSel.opts.center && !X.bin ? center(X.x) : X.x, wc = anSel.opts.center && !W.bin ? center(W.x) : W.x;
      const cols = [xc, wc, xc.map((v, i) => v * wc[i]), ...C], fit = ols(y, rowsOf(cols)), red = ols(y, rowsOf([xc, wc, ...C]));
      const f2 = (fit.R2 - red.R2) / (1 - fit.R2);
      return { desc: descOf(P, [r.x, r.w, r.y].filter(k => P.V[k].type === 'num')), es: { text: `상호작용의 f² = ${F2(f2, 3)} (${sizeLabel('f2', f2)}), ΔR² = ${NZ(fit.R2 - red.R2, 3)}`, cut: 'f2', extra: '조절효과의 f²는 대개 작게 나오며, .005~.01 수준도 흔하다는 보고가 있습니다.' },
        ci: { st: 'ok', note: '계수와 단순기울기의 95% CI를 보고했습니다.' }, norm: normality([['종속변인 모형 잔차', fit.resid]]), assum: [vifCheck(vifs(cols)), residPlot], post: null };
    },
    modmed7(r, R) { return modmedChecks(r, R, 7); },
    modmed14(r, R) { return modmedChecks(r, R, 14); }
  };
  function descOf(P, keys) {
    const ks = keys.filter(k => P.V[k]?.type === 'num'); if (!ks.length) return { st: 'ok', note: '분류 변인은 빈도로 보고하세요.' };
    return { st: 'ok', note: '쓴 변인의 n, M, SD를 아래 기술통계 표에 넣었습니다.', table: { title: '기술통계', head: ['변인', 'n', 'M', 'SD', '최솟값', '최댓값'], num: [1, 2, 3, 4, 5], rows: ks.map(k => { const v = P.col(k); return [P.V[k].name, String(v.length), F2(mean(v)), F2(sd(v)), F2(Math.min(...v)), F2(Math.max(...v))]; }) } };
  }
  function corrChecks(keys, R, sp) {
    const vs = keys.map(varByKey), n0 = lastScored.inc.length, rows = [];
    vs.forEach((a, i) => vs.forEach((b, j) => {
      if (j >= i) return; const idx = []; for (let q = 0; q < n0; q++) if (a.vals[q] != null && b.vals[q] != null) idx.push(q);
      let x = idx.map(q => a.vals[q]), y = idx.map(q => b.vals[q]); if (sp) { x = ranks(x); y = ranks(y); }
      const rr = corr(x, y), ci = fisherCI(rr, idx.length); rows.push([`${b.name} – ${a.name}`, NZ(rr), CI(ci[0], ci[1]), sizeLabel('r', rr)]);
    }));
    return { desc: { st: 'ok', note: '상관표에 M, SD를 함께 넣었습니다.' }, es: { text: `상관계수 자체가 효과크기입니다. 크기는 아래 표 참고.`, cut: 'r', table: { title: `${sp ? 'ρ' : 'r'}의 95% 신뢰구간 (Fisher z)`, head: ['짝', sp ? 'ρ' : 'r', '95% CI', '크기'], num: [1, 2], rows } },
      ci: { st: 'ok', note: `${sp ? 'ρ' : 'r'}의 95% CI를 표로 넣었습니다${sp ? ' (스피어만 ρ는 근사)' : ''}.` }, norm: sp ? { na: '순위상관은 정규성 가정이 없음' } : normality(vs.map(v => [v.name, v.vals.filter(x => x != null)])), assum: [],
      post: vs.length > 2 ? { st: 'warn', note: `상관을 ${vs.length * (vs.length - 1) / 2}쌍 함께 검정했습니다. 여러 번 검정하면 우연히 유의해질 가능성이 커지니, 가설에 해당하는 짝 위주로 해석하세요.` } : null };
  }
  function regChecks(yk, xs, R, hier) {
    const P = prep({ y: yk, x: xs }), D = designCols(P, xs), fit = ols(P.col(yk), rowsOf(D.cols)), f2 = fit.R2 / (1 - fit.R2);
    return { desc: descOf(P, [yk, ...xs]), es: { text: `β(표준화 계수)와 R² = ${NZ(fit.R2)}, f² = ${F2(f2, 3)} (${sizeLabel('f2', f2)})${hier ? ', 단계별 ΔR²' : ''}를 보고했습니다.`, cut: 'f2' },
      ci: { st: 'ok', note: 'B의 95% CI를 보고했습니다. β의 CI가 필요하면 B의 CI에 SD(X)/SD(Y)를 곱한 근사값을 쓸 수 있습니다.' }, norm: normality([['잔차', fit.resid]]), assum: [vifCheck(vifs(D.cols)), residPlot], post: null };
  }
  function semChecks(R) {
    const ft = R.tables.find(t => t.title === '모형 적합도'), bad = ft ? ft.rows.filter(r => r[3] === '미흡').map(r => r[0]) : [];
    const ld = R.tables.find(t => /측정모형/.test(t.title)), low = ld ? ld.rows.filter(r => +r[2] < 0.5).map(r => r[1]) : [];
    return { desc: { st: 'ok', note: '잠재변인은 문항으로 측정했습니다. 척도 점수의 평균·표준편차는 표본 확인과 사전 분석의 표를 쓰세요.' },
      es: { text: '표준화 경로계수(β)와 설명된 분산(R²)을 보고했습니다.', cutText: '표준화 경로계수는 흔히 .10 작음, .30 중간, .50 큼으로 읽습니다. 간접효과에는 합의된 기준이 없어 크기만 적습니다.' },
      ci: { st: 'ok', note: '경로는 95% CI, 간접효과와 총효과는 부트스트랩 95% 백분위 신뢰구간을 보고했습니다.' }, stat: { st: 'ok', note: '경로는 z·p를, 간접효과는 p 대신 부트스트랩 신뢰구간으로 판단했습니다.' },
      norm: { na: '최대우도(ML)는 다변량 정규성을 가정합니다. 문항 분포가 크게 치우치면 R 코드에서 estimator = "MLR"로 바꿔 비교해 보세요' },
      assum: [{ item: '모형 적합도', st: bad.length ? 'warn' : 'ok', note: bad.length ? `${bad.join(', ')}가 기준에 못 미칩니다. 측정모형(확인적 요인분석)부터 점검하세요. 수정지수만 보고 경로를 더하지 마세요.` : '적합도 지표가 흔히 쓰는 기준을 넘었습니다.' },
        { item: '측정모형 부하량', st: low.length ? 'warn' : 'ok', note: low.length ? `표준화 부하량이 .50보다 낮은 문항: ${low.join(', ')}` : '모든 문항의 표준화 부하량이 .50 이상입니다.' },
        { item: '표본 크기', st: R.n >= 200 ? 'ok' : 'warn', note: R.n >= 200 ? `N = ${R.n}` : `N = ${R.n}. 구조방정식은 흔히 200명 이상을 권합니다. 결과를 조심해서 해석하고 척도 점수로 한 매개·회귀분석과 비교하세요.` }], post: null };
  }
  function medChecks(xk, ms, yk, covs, R, serial) {
    const P = prep({ x: xk, m: ms, y: yk, c: covs }), X = binCode(P, xk), C = designCols(P, covs).cols, y = P.col(yk), M = ms.map(k => P.col(k));
    const fy = ols(y, rowsOf([X.x, ...M, ...C])), sets = [[`${P.V[yk].name} 모형 잔차`, fy.resid]];
    M.forEach((m, j) => { const pre = serial && j === 1 ? [M[0]] : []; sets.push([`${P.V[ms[j]].name} 모형 잔차`, ols(m, rowsOf([X.x, ...pre, ...C])).resid]); });
    return { desc: descOf(P, [xk, ...ms, yk]), es: { text: `간접효과와 ${X.bin ? '부분표준화(X가 두 집단)' : '완전표준화'} 간접효과를 보고했습니다.`, cutText: '간접효과에는 널리 합의된 작음·중간·큼 기준이 없습니다. 표준화 효과의 크기를 그대로 적고, 크다·작다는 조심스럽게 쓰세요.' },
      ci: { st: 'ok', note: '간접효과는 부트스트랩 95% CI, 경로계수는 95% CI를 보고했습니다.' }, stat: { st: 'ok', note: '경로계수는 t·df·p를, 간접효과는 p 대신 부트스트랩 신뢰구간으로 판단했습니다.' },
      norm: normality(sets), assum: [vifCheck(vifs([X.x, ...M, ...C])), residPlot, { item: '부트스트랩', st: 'ok', note: `재표집 ${(+anSel.opts.boot || 5000).toLocaleString()}번, 백분위 신뢰구간 (간접효과의 정규성 가정 불필요)` }], post: null };
  }
  function modmedChecks(r, R, model) {
    const P = prep({ x: r.x, m: r.m, w: r.w, y: r.y, c: r.covs }), y = P.col(r.y), m = P.col(r.m), X = binCode(P, r.x), C = designCols(P, r.covs).cols;
    const fy = ols(y, rowsOf([X.x, m, ...C]));
    return { desc: descOf(P, [r.x, r.m, r.w, r.y]), es: { text: '조절된 매개 지수와 조건부 간접효과를 보고했습니다.', cutText: '조절된 매개 지수에는 합의된 크기 기준이 없습니다. 신뢰구간이 0을 포함하는지로 판단합니다.' },
      ci: { st: 'ok', note: '지수와 조건부 간접효과의 부트스트랩 95% CI를 보고했습니다.' }, stat: { st: 'ok', note: '경로계수는 t·df·p를, 지수는 부트스트랩 신뢰구간으로 판단했습니다.' },
      norm: normality([[`${P.V[r.y].name} 모형 잔차`, fy.resid]]), assum: [residPlot, { item: '부트스트랩', st: 'ok', note: `재표집 ${(+anSel.opts.boot || 5000).toLocaleString()}번, 백분위 신뢰구간` }], post: null };
  }

  function buildChecks(R, roles) {
    let c; try { c = CHECKS[R.method]?.(roles, R); } catch (e) { c = null; }
    if (!c) return;
    const L = [];
    L.push({ g: '기술통계', item: '표본 크기 (N)', st: 'ok', note: `N = ${R.n}` });
    L.push({ g: '기술통계', item: '평균·표준편차', st: c.desc.st, note: c.desc.note });
    L.push({ g: '검정통계량', item: '통계량 값·자유도·정확한 p', st: c.stat?.st || 'ok', note: c.stat?.note || '표에 통계량, df, p를 넣었습니다. p는 .001 미만일 때만 "< .001"로 적습니다 (APA 7).' });
    L.push({ g: '효과크기', item: '효과크기 지표', st: 'ok', note: c.es.text });
    L.push({ g: '효과크기', item: '크기 해석 기준', st: 'ok', note: [c.es.cutText || CUT_TEXT[c.es.cut], c.es.extra].filter(Boolean).join(' ') });
    L.push({ g: '신뢰구간', item: '95% 신뢰구간', st: c.ci.st, note: c.ci.note });
    if (c.norm.na) L.push({ g: '가정 검정', item: '정규성', st: 'na', note: `해당 없음 — ${c.norm.na}` });
    else L.push({ g: '가정 검정', item: '정규성 (Shapiro–Wilk)', st: c.norm.st, note: c.norm.note });
    c.assum.forEach(a => L.push({ g: '가정 검정', ...a }));
    L.push({ g: '다중비교', item: '사후검정·다중비교', st: c.post ? c.post.st : 'na', note: c.post ? c.post.note : '해당 없음 — 여러 집단을 짝지어 비교하는 분석이 아님' });
    L.push({ g: '유의성 판단', item: 'α 수준 밝히기', st: 'ok', note: '이 사이트는 α = .05 기준입니다. 연구방법에 "유의수준 .05"를 미리 적으세요.' });
    L.push({ g: '유의성 판단', item: '통계적 유의성과 실질적 의미 구별', st: 'ok', note: '효과크기와 크기 해석을 함께 적었습니다. 유의해도 효과가 작으면 그렇게 쓰세요.' });
    R.checks = L;
    if (c.desc.table) R.tables.unshift(c.desc.table);
    if (c.es.table) R.tables.push(c.es.table);
    if (!c.norm.na && c.norm.rows?.length) R.tables.push(normTable(c.norm.rows));
  }

  function checksHtml(a) {
    if (!a.checks) return `<details class="item-detail"><summary>보고 체크리스트</summary><p class="hint" style="padding:0 14px 12px">이 결과는 체크리스트가 생기기 전에 계산했습니다. 다시 계산하면 생깁니다.</p></details>`;
    const warn = a.checks.filter(c => c.st === 'warn').length;
    const icon = st => st === 'ok' ? '<span class="ck ok" title="됨">✓</span>' : st === 'warn' ? '<span class="ck warn" title="확인 필요">!</span>' : '<span class="ck na" title="해당 없음">–</span>';
    const groups = [...new Set(a.checks.map(c => c.g))];
    return `<details class="item-detail checklist" ${warn ? 'open' : ''}>
      <summary>보고 체크리스트 (APA 7) · ${warn ? `<span class="a-mid">확인할 것 ${warn}개</span>` : '<span class="a-ok">모두 갖춤</span>'}</summary>
      <div class="ck-body">${groups.map(g => `<div class="ck-group">${esc(g)}</div>` + a.checks.filter(c => c.g === g).map(c => `<div class="ck-row ${c.st}">${icon(c.st)}<div><b>${esc(c.item)}</b><span>${esc(c.note)}</span></div></div>`).join('')).join('')}</div>
    </details>`;
  }

  // =================================================================

// ##### 7. 분석 결정 (기본값) (stats-ladder.html 7290~7407줄) #####
  // =================================================================
  // 분석 결정 (연구자가 정하고, 바꾼 것은 기록)
  // =================================================================
  let plan = store.get('ladder.plan', null) || { dec: { minPct: '80', straight: 'keep', dup: 'keep', outlier: 'keep' }, locked: '', seen: false, log: [] };
  plan.elig = plan.elig || []; plan.recode = plan.recode || {}; plan.manual = plan.manual || {};
  const savePlan = () => store.set('ladder.plan', plan);
  const AO = () => { ensureSelLite(); return anSel.opts; };
  function ensureSelLite() { if (!anSel) anSel = { method: null, roles: {}, opts: {} }; const o = anSel.opts; if (o.boot == null) o.boot = 5000; if (o.semBoot == null) o.semBoot = 1000; if (o.center == null) o.center = true; if (!o.welch) o.welch = 'always'; if (!o.posthoc) o.posthoc = 'scheffe'; if (o.testValue == null) o.testValue = 3; }

  // scope: 'data'는 모든 분석에, 그 밖은 해당 방법에만
  const DECISIONS = [
    { id: 'scoring', scope: 'data', q: '변인 점수는 문항을 어떻게 합쳐 만들까요?',
      opts: [['mean', '문항 평균', '점수 범위가 응답 범위(예: 1~5)와 같아 읽기 쉽고, 빠진 문항이 있어도 비교할 수 있습니다. 대부분의 논문이 씁니다.'], ['sum', '문항 합계', '원척도 개발 논문이 합계로 보고하라고 했을 때 씁니다. 빠진 문항이 있으면 답한 문항 평균 × 전체 문항 수(비례 환산 합계)로 냅니다.']],
      get: () => sv.scoring || 'mean', set: v => { sv.scoring = v; saveSv(); } },
    { id: 'minPct', scope: 'data', q: '한 변인의 문항을 몇 % 이상 답한 사람에게 점수를 줄까요?',
      opts: [['100', '모두 답한 사람만', '가장 엄격합니다. 빠진 사람이 많으면 표본이 줄어듭니다.'], ['80', '80% 이상', '흔히 쓰는 기준입니다. 한두 문항을 빠뜨린 사람은 나머지 문항 평균으로 점수를 줍니다.'], ['50', '절반 이상', '너그러운 기준입니다. 점수의 정확도가 떨어질 수 있습니다.']],
      get: () => plan.dec.minPct, set: v => { plan.dec.minPct = v; } },
    { id: 'straight', scope: 'data', q: '모든 척도 문항에 같은 번호로 답한 응답(불성실 의심)을 뺄까요?',
      opts: [['keep', '빼지 않음', '실제로 그렇게 느낀 사람일 수도 있습니다. 빼지 않되 몇 명인지 보고합니다.'], ['drop', '뺌', '역문항이 있는데도 같은 번호만 고른 것은 대충 답한 신호일 수 있습니다. 뺀 인원을 보고합니다.']],
      get: () => plan.dec.straight, set: v => { plan.dec.straight = v; } },
    { id: 'longrun', scope: 'data', q: '같은 번호를 길게 이어 고른 응답(불성실 의심)을 뺄까요?',
      opts: [['keep', '빼지 않음', '문항이 비슷하면 실제로 이어서 같은 답을 할 수 있습니다. 빼지 않되 몇 명인지 보고합니다.'], ['drop', '뺌', '척도 문항 절반 이상(최소 10문항)을 연달아 같은 번호로 고른 응답을 뺍니다(long-string 기준). 뺀 인원을 보고합니다.']],
      get: () => plan.dec.longrun || 'keep', set: v => { plan.dec.longrun = v; } },
    { id: 'revcon', scope: 'data', q: '같은 변인의 정문항과 역문항에 모두 강하게 동의한 응답(앞뒤가 맞지 않음)을 뺄까요?',
      opts: [['keep', '빼지 않음', '역문항 문장이 헷갈리게 쓰였을 수도 있습니다. 빼지 않되 몇 명인지 보고합니다.'], ['drop', '뺌', '"나는 행복하다"와 "나는 불행하다"에 모두 "매우 그렇다"처럼 앞뒤가 맞지 않는 응답을 뺍니다. 뺀 인원을 보고합니다.']],
      get: () => plan.dec.revcon || 'keep', set: v => { plan.dec.revcon = v; } },
    { id: 'attn', scope: 'data', q: '주의 확인 문항에서 정답을 고르지 않은 응답을 뺄까요?',
      opts: [['drop', '뺌 (기본)', '문항을 읽지 않고 답한 신호입니다. 미리 넣어 둔 문항이므로 연구 계획대로 빼는 것이 보통입니다. 뺀 인원을 보고합니다.'], ['keep', '빼지 않음', '문항이 헷갈리게 쓰였다고 판단할 때. 이유를 기록해 두세요.']],
      get: () => plan.dec.attn || 'drop', set: v => { plan.dec.attn = v; }, when: () => attnItems().length > 0 },
    { id: 'dup', scope: 'data', q: '모든 답이 똑같은 응답(중복)이 있으면 어떻게 할까요?',
      opts: [['keep', '빼지 않음 (기본)', '모든 답이 같아도 다른 사람일 수 있습니다(문항이 적거나, 모두 같은 번호로 답한 경우). [응답 정리]의 목록과 제출 시각 간격을 보고 판단하세요.'], ['drop', '뒤의 것을 뺌', '같은 사람이 두 번 낸 것으로 볼 때. 먼저 들어온 응답만 남깁니다. 뺀 수를 논문에 적으세요.']],
      get: () => plan.dec.dup || 'keep', set: v => { plan.dec.dup = v; } },
    { id: 'outlier', scope: 'data', q: '점수가 크게 벗어난 응답(이상치)을 뺄까요?',
      opts: [['keep', '빼지 않음', '실제로 극단적인 사람일 수 있습니다. 빼지 않되 몇 명인지 보고합니다.'], ['drop', '뺌', '변인 점수 |z| > 3.29 또는 마할라노비스 거리 p < .001인 응답을 뺍니다. 뺀 인원을 보고합니다.']],
      get: () => plan.dec.outlier || 'keep', set: v => { plan.dec.outlier = v; } },
    { id: 'welch', scope: ['ind_t'], q: '두 집단의 흩어진 정도(분산)가 다르면 어떻게 할까요?',
      opts: [['always', '언제나 Welch t (권장)', '분산이 같든 다르든 잘 맞아서 최근 방법론 연구들이 권하는 방식입니다(Delacre, Lakens, & Leys, 2017). 분산이 같을 때도 Student t와 거의 같은 값이 나옵니다.'], ['auto', 'Levene 검정에 따라 고르기', 'Levene p < .05이면 Welch t, 아니면 일반(Student) t를 씁니다. 국내 논문에서 흔하지만, 사전 검정으로 방법을 고르면 1종 오류율이 흔들린다는 비판이 있습니다.'], ['never', '언제나 Student t', '분산이 같다고 가정합니다. 집단 크기가 같고 분산도 비슷할 때만 권합니다.']],
      get: () => AO().welch, set: v => { AO().welch = v; } },
    { id: 'posthoc', scope: ['oneway'], q: '세 집단 이상에서 차이가 있으면, 어느 집단끼리 다른지 무엇으로 볼까요?',
      opts: [['scheffe', 'Scheffé', '가장 보수적입니다. 집단 크기가 달라도 쓸 수 있어 국내 논문에서 많이 씁니다.'], ['bonferroni', 'Bonferroni', '비교 횟수만큼 유의수준을 나눕니다. 비교가 적으면 Scheffé보다 덜 보수적입니다.']],
      get: () => AO().posthoc, set: v => { AO().posthoc = v; } },
    { id: 'boot', scope: ['med_parallel', 'med_serial', 'modmed7', 'modmed14'], q: '간접효과의 신뢰구간을 구할 때 재표집을 몇 번 할까요?',
      opts: [['1000', '1,000번', '빠르지만 신뢰구간이 조금 흔들릴 수 있습니다.'], ['5000', '5,000번', 'PROCESS의 기본값이라 논문에서 가장 흔합니다.'], ['10000', '10,000번', '가장 안정적입니다. 계산이 조금 오래 걸립니다.']],
      get: () => String(AO().boot), set: v => { AO().boot = +v; } },
    { id: 'semBoot', scope: ['sem', 'sem_serial'], q: '구조방정식의 간접효과 신뢰구간을 구할 때 재표집을 몇 번 할까요?',
      opts: [['500', '500번', '빠르지만 신뢰구간이 조금 흔들릴 수 있습니다.'], ['1000', '1,000번', '구조방정식 논문에서 흔합니다. 브라우저에서 1분 안팎 걸립니다.'], ['2000', '2,000번', '더 안정적입니다. 시간이 두 배쯤 걸립니다.'], ['5000', '5,000번', '가장 안정적입니다. 몇 분 걸릴 수 있습니다.']],
      get: () => String(AO().semBoot || 1000), set: v => { AO().semBoot = +v; } },
    { id: 'center', scope: ['moderation', 'modmed7', 'modmed14'], q: '상호작용항을 만들기 전에 점수 변인을 평균중심화할까요?',
      opts: [['on', '평균중심화함', '계수가 "상대 변인이 평균일 때의 효과"가 되어 해석하기 쉽습니다. 많이 권합니다.'], ['off', '원점수 그대로', '0점이 의미 있는 척도일 때 씁니다. 상호작용 계수와 검정 결과는 같습니다.']],
      get: () => (AO().center ? 'on' : 'off'), set: v => { AO().center = v === 'on'; } }
  ];
  const FIXED = [
    ['빠진 값', '분석에 쓴 변인 중 하나라도 비어 있는 사람은 그 분석에서 뺍니다 (목록별 제외). 다중대체·FIML이 필요하면 R 코드에서 바꿀 수 있습니다.'],
    ['유의수준', 'α = .05, 양측 검정. 간접효과는 95% 신뢰구간이 0을 포함하지 않으면 유의.'],
    ['기준 집단', '분류 변인을 회귀에 넣을 때는 설문 선택지 순서의 첫 범주가 기준(0)입니다.'],
    ['부트스트랩 방식', '백분위 신뢰구간, 재표집 시작값 고정 (같은 자료면 같은 결과).'],
    ['효과크기 기준', 'Cohen(1988)의 작음·중간·큼 기준. 간접효과는 합의된 기준이 없어 크기만 적습니다.']
  ];
  const decLabel = (d, v) => (d.opts.find(o => o[0] === String(v)) || [v, v])[1];
  const liveDecisions = () => DECISIONS.filter(d => !d.when || d.when());
  const applicable = (d, method) => (d.scope === 'data' || d.scope.includes(method)) && (!d.when || d.when());

  function changeDecision(id, v) {
    const d = DECISIONS.find(x => x.id === id), from = String(d.get());
    if (from === String(v)) return;
    d.set(v);
    plan.log.push({ at: new Date().toLocaleString('ko-KR'), id, q: d.q, from: decLabel(d, from), to: decLabel(d, v), after: !!plan.seen, reason: '' });
    savePlan(); saveAnSel();
    addSys('analysis', `분석 결정: ${d.q} → ${decLabel(d, v)}${plan.seen ? ' (결과를 본 뒤 바꿈: 사후 변경으로 기록)' : ''}`);
    if (d.scope === 'data' && raw && lastScored) { runScoring(); toast('바뀐 결정으로 점수를 다시 만들었습니다.'); }
  }

  function decisionsHtml(method) {
    const ds = DECISIONS.filter(d => applicable(d, method));
    const row = d => `<div class="dec">
      <div class="dec-q">${esc(d.q)}${d.scope === 'data' ? ' <span class="st manual">모든 분석</span>' : ''}</div>
      <div class="dec-opts">${d.opts.map(([v, l, why]) => `<label class="dec-opt ${String(d.get()) === v ? 'on' : ''}"><input type="radio" name="dec-${d.id}" data-dec="${d.id}" value="${v}" ${String(d.get()) === v ? 'checked' : ''}><b>${esc(l)}</b><span>${esc(why)}</span></label>`).join('')}</div>
    </div>`;
    const after = plan.log.filter(l => l.after);
    return `<details class="dec-box" ${plan.locked ? '' : 'open'}>
      <summary><b>분석 결정</b> ${plan.locked ? `<span class="st auto">확정 ${esc(plan.locked)}</span>` : '<span class="st missing">아직 확정 안 함</span>'} ${after.length ? `<span class="st manual">사후 변경 ${after.length}건</span>` : ''}<span class="hint">결과를 보기 전에 정해 두세요</span></summary>
      <p class="hint">같은 자료도 이 결정에 따라 결과가 달라집니다. 결과를 보고 유의하게 나오는 쪽으로 바꾸면 우연한 결과를 진짜처럼 보고하게 되므로(p-해킹), 결과를 보기 전에 정하고, 나중에 바꾼 것은 <b>사후 변경</b>으로 기록해 논문에 밝힙니다.</p>
      ${ds.map(row).join('')}
      <details class="item-detail"><summary>정해진 기본값 ${FIXED.length}가지</summary><ul class="notes" style="padding:0 18px 12px 34px">${FIXED.map(([k, v]) => `<li><b>${esc(k)}</b> · ${esc(v)}</li>`).join('')}</ul></details>
      ${plan.log.length ? `<div class="section-title">결정 기록</div><div class="table-wrap"><table class="compact"><thead><tr><th>때</th><th>결정</th><th>바꾼 내용</th><th>구분</th><th>이유</th></tr></thead><tbody>
        ${plan.log.map((l, i) => `<tr><td>${esc(l.at)}</td><td>${esc(l.q)}</td><td>${esc(l.from)} → ${esc(l.to)}</td><td>${l.after ? '<span class="a-mid">사후 변경</span>' : '사전'}</td><td>${l.after ? `<input class="dec-why" data-dec-why="${i}" value="${esc(l.reason)}" placeholder="바꾼 이유 (논문에 밝힐 내용)">` : esc(l.reason || '')}</td></tr>`).join('')}
      </tbody></table></div>` : ''}
      <div class="actions tight" style="margin-top:12px">
        ${plan.locked ? '' : '<button class="btn sm primary" data-an-act="lock-plan">결정 확정하기</button>'}
        <button class="btn sm" data-an-act="plan-dl">분석 계획서 받기</button>
      </div>
    </details>`;
  }

  function planText() {
    const lines = ['# 분석 계획서 — 통', '', `작성: ${today()}`, `확정: ${plan.locked || '확정하지 않음'}`, '', '## 결정'];
    liveDecisions().forEach(d => lines.push(`- ${d.q} → **${decLabel(d, d.get())}**${d.scope === 'data' ? '' : ` (해당: ${d.scope.map(m => METHODS[m]?.name || m).join(', ')})`}`));
    lines.push('', '## 정해진 기본값'); FIXED.forEach(([k, v]) => lines.push(`- ${k}: ${v}`));
    lines.push('', '## 결정 기록');
    if (!plan.log.length) lines.push('- 없음');
    plan.log.forEach(l => lines.push(`- ${l.at} · ${l.q}: ${l.from} → ${l.to} · ${l.after ? '사후 변경(결과를 본 뒤)' : '사전'}${l.reason ? ` · 이유: ${l.reason}` : ''}`));
    if (typeof md !== 'undefined' && md) {
      lines.push('', '## 가설 확정');
      if (md.lock) { lines.push(`- 확정: ${md.lock.at}`, `- 설계: ${md.lock.design}`, `- 통제변인: ${md.lock.controls.length ? md.lock.controls.join(', ') : '없음'}`); md.lock.plan.forEach(p => lines.push(`- 가설 ${p.no}: ${p.text || '(문장 없음)'} → ${RULES.results[p.result]?.name || p.result}`)); }
      else lines.push('- 확정하지 않음');
      md.unlocks.forEach(u => lines.push(`- ${u.prevAt} 확정을 ${u.at}에 풀었음${u.why ? ` · 이유: ${u.why}` : ''}`));
      if (md.pwResult) { lines.push('', '## 표본 수 근거', `- 최소 ${md.pwResult.N}명, 모집 목표 ${md.pwResult.recruit}명 (탈락 ${md.pwResult.attr}% 감안)`); md.pwResult.rows.forEach(r => lines.push(`- 가설 ${r.no} ${r.method}: ${r.N}명 · ${r.es ? r.es + ' · ' : ''}${r.how} (${r.cite})`)); lines.push(`- 계획서 문장: ${md.pwResult.sentence}`); }
      if (norm(md.altWhy)) lines.push('', '## 모형의 순서를 이렇게 정한 이유', md.altWhy);
    }
    if (hp && hp.mode === 'hyp' && hp.cards && hp.cards.length) { const L = mbPlanLines(); if (L.length) lines.push('', '## 연구모형과 분석 방법', ...L); }
    lines.push('', '## 계산한 분석');
    analyses.slice().reverse().forEach(a => lines.push(`- ${a.at} · ${a.conf ? `[${confLabel(a, true)}] ` : ''}${a.title} (N = ${a.n})`));
    return lines.join('\n');
  }
  const planPayload = () => ({ 결정: liveDecisions().map(d => `${d.q} → ${decLabel(d, d.get())}`), 기본값: FIXED.map(([k, v]) => `${k}: ${v}`),
    사후_변경: plan.log.filter(l => l.after).map(l => `${l.q}: ${l.from} → ${l.to}${l.reason ? ` (이유: ${l.reason})` : ' (이유 미기재)'}`) });

  // =================================================================

// ##### 8. R 코드 만들기 (재계산용) (stats-ladder.html 7407~7774줄) #####
  // =================================================================
  // R 코드 만들기 (분석 계획서 그대로, 기본 R만 사용)
  // =================================================================
  const rq = s => '"' + String(s).replace(/\\/g, '\\\\').replace(/"/g, '\\"').replace(/\n/g, ' ') + '"';
  const rvec = a => `c(${a.map(rq).join(', ')})`;
  // 계산할 때의 변인 정보를 결과에 남겨 둠 (나중에 자료 없이도 R 코드를 만들 수 있게)
  function specOf(method, roles, opts) {
    const keys = [...new Set(Object.values(roles).flat().filter(Boolean))];
    const vars = Object.fromEntries(keys.map(k => { const v = varByKey(k); return [k, { code: v.code, name: v.name, type: v.type, levels: v.levels || null }]; }));
    const spec = { method, roles: JSON.parse(JSON.stringify(roles)), opts: { ...opts }, vars };
    if (/^sem/.test(method) && lastScored) { const S = semSpec(method === 'sem_serial' ? 'serial' : 'par', roles); spec.sem = { syntax: S.syntax, items: S.items.map(i => ({ nm: i.nm, code: i.code })), obs: S.obs.map(n => ({ id: n.id, k: n.k })), P: S.P, D: S.D }; }
    return spec;
  }

  const R_HEAD = `options(warn = 1)
# 한글을 읽으려면 UTF-8 환경이어야 합니다 (R 4.2 이상의 윈도우·맥은 기본값). 아니면 바꿔 봅니다.
if (!isTRUE(l10n_info()[["UTF-8"]])) for (loc in c("C.UTF-8", "en_US.UTF-8", "ko_KR.UTF-8", "Korean_Korea.utf8")) if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break
DATA <- c("analysis_data.csv", "../2_data/analysis_data.csv", "2_data/analysis_data.csv"); DATA <- DATA[file.exists(DATA)][1]
if (is.na(DATA)) stop("analysis_data.csv를 찾지 못했습니다. 이 R 파일과 같은 폴더나 ../2_data 폴더에 두세요.")
d <- read.csv(DATA, fileEncoding = "UTF-8-BOM", check.names = FALSE, stringsAsFactors = FALSE, na.strings = c("", "NA"))
EFF <- data.frame(analysis = character(), label = character(), est = numeric(), lo = numeric(), hi = numeric(), p = numeric(), stringsAsFactors = FALSE)
AID <- ""
g8 <- function(x) if (is.na(x)) "NA" else sprintf("%.8g", x)
eff <- function(label, est, lo = NA, hi = NA, p = NA) {
  EFF[nrow(EFF) + 1, ] <<- list(AID, label, est, lo, hi, p)
  cat(sprintf("EFFECT\\t%s\\t%s\\t%s\\t%s\\t%s\\n", label, g8(est), g8(lo), g8(hi), g8(p)))
}
num <- function(col) as.numeric(d[[col]])
catf <- function(col, lv) factor(d[[col]], levels = unique(c(lv, setdiff(unique(na.omit(d[[col]])), lv))))
# 분류 변인 → 더미 (첫 범주가 기준). 이름은 통에서와 같게
dum <- function(f, name) {
  f <- droplevels(f); lv <- levels(f)
  m <- sapply(lv[-1], function(l) as.numeric(f == l)); m <- matrix(m, ncol = length(lv) - 1)
  colnames(m) <- paste0(name, ": ", lv[-1], " (기준 ", lv[1], ")"); m
}
bin01 <- function(f) { f <- droplevels(f); as.numeric(f == levels(f)[2]) }
# 계수표: 이름, B, 95% CI, p
coefs <- function(m) { s <- summary(m)$coefficients; ci <- suppressMessages(confint(m)); data.frame(B = s[, 1], lo = ci[, 1], hi = ci[, 2], p = s[, 4], row.names = rownames(s)) }
lev <- function(groups) { z <- unlist(lapply(groups, function(x) abs(x - mean(x)))); g <- factor(rep(seq_along(groups), sapply(groups, length))); a <- anova(lm(z ~ g)); c(F = a[["F value"]][1], p = a[["Pr(>F)"]][1]) }
pct <- function(v, q) { v <- sort(v); v[min(length(v), max(1, floor(q * length(v)) + 1))] }
fitb <- function(y, X) as.numeric(lm.fit(cbind(1, X), y)$coefficients)
`;

  // 예측변인 묶음을 행렬로 (숫자는 그대로, 분류는 더미)
  function rDesign(S, keys, u = 'u') {
    if (!keys.length) return 'matrix(nrow = sum(' + u + '), ncol = 0)';
    return 'cbind(' + keys.map(k => { const v = S.vars[k]; return v.type === 'num' ? `${rq(v.name)} = num(${rq(v.code)})[${u}]` : `dum(catf(${rq(v.code)}, ${rvec(v.levels)})[${u}], ${rq(v.name)})`; }).join(', ') + ')';
  }
  const rComplete = (S, keys) => `u <- complete.cases(d[, ${rvec(keys.map(k => S.vars[k].code))}, drop = FALSE])`;
  const rNum = (S, k) => `num(${rq(S.vars[k].code)})[u]`;
  const rCat = (S, k) => `droplevels(catf(${rq(S.vars[k].code)}, ${rvec(S.vars[k].levels)})[u])`;
  const rX = (S, k) => S.vars[k].type === 'num' ? rNum(S, k) : `bin01(${rCat(S, k)})`;
  const nm = (S, k) => S.vars[k].name;

  const RGEN = {
    sem: S => rSem(S), sem_serial: S => rSem(S),
    pearson: S => rCorr(S, 'pearson'), spearman: S => rCorr(S, 'spearman'),
    partial: S => { const r = S.roles; return `${rComplete(S, [r.x, r.y, ...r.covs])}
x <- ${rNum(S, r.x)}; y <- ${rNum(S, r.y)}; Z <- ${rDesign(S, r.covs)}
rx <- resid(lm(x ~ Z)); ry <- resid(lm(y ~ Z))
r <- cor(rx, ry); df <- sum(u) - 2 - ncol(Z); t <- r * sqrt(df / (1 - r^2)); p <- 2 * pt(-abs(t), df)
cat(sprintf("편상관 r = %.3f, t(%d) = %.2f, p = %.4f\\n", r, df, t, p))
eff(paste0(${rq(nm(S, r.x))}, " – ", ${rq(nm(S, r.y))}, " 편상관 (통제: ", paste(colnames(Z), collapse = ", "), ")"), r, NA, NA, p)`; },
    pointbiserial: S => { const r = S.roles; return `${rComplete(S, [r.g, r.y])}
g <- ${rCat(S, r.g)}; y <- ${rNum(S, r.y)}
ct <- cor.test(bin01(g), y); print(ct)
eff(${rq(`${nm(S, r.g)}(1 = ${S.vars[r.g].levels[1]}) – ${nm(S, r.y)} 점이연 상관`)}, unname(ct$estimate), NA, NA, ct$p.value)`; },
    chisq: S => { const r = S.roles; return `${rComplete(S, [r.a, r.b])}
tab <- table(${rCat(S, r.a)}, ${rCat(S, r.b)}); print(tab)
cs <- suppressWarnings(chisq.test(tab, correct = FALSE)); print(cs)
V <- sqrt(unname(cs$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
eff(${rq(`${nm(S, r.a)}와(과) ${nm(S, r.b)}의 관련성`)}, V, NA, NA, cs$p.value)`; },
    onesample: S => { const r = S.roles, tv = Number(S.opts.testValue); return `${rComplete(S, [r.y])}
y <- ${rNum(S, r.y)}; tt <- t.test(y, mu = ${tv}); print(tt)
eff(${rq(`${nm(S, r.y)} 평균 − 기준값 ${tv}`)}, mean(y) - ${tv}, NA, NA, tt$p.value)
# 베이즈 인자 (JASP와 같은 사전분포, BayesFactor 패키지 필요): library(BayesFactor); print(ttestBF(y, mu = ${tv}))`; },
    ind_t: S => { const r = S.roles, w = S.opts.welch || 'always'; return r.ys.map(yk => `${rComplete(S, [r.g, yk])}
g <- ${rCat(S, r.g)}; y <- ${rNum(S, yk)}; lv <- levels(g)
a <- y[g == lv[1]]; b <- y[g == lv[2]]; L <- lev(list(a, b))
ve <- ${w === 'never' ? 'TRUE' : w === 'auto' ? 'L[["p"]] >= .05' : 'FALSE'}   # 분석 결정: 분산이 다를 때 (${w === 'never' ? '언제나 Student' : w === 'auto' ? 'Levene에 따라' : '언제나 Welch'})
cat(sprintf("Levene F = %.2f, p = %.4f → %s t검정\\n", L[["F"]], L[["p"]], if (ve) "Student" else "Welch"))
tt <- t.test(a, b, var.equal = ve); print(tt)
eff(paste0(${rq(nm(S, yk))}, ": ", lv[1], " − ", lv[2]), mean(a) - mean(b), NA, NA, tt$p.value)
# 베이즈 인자 (JASP와 같은 사전분포, BayesFactor 패키지 필요): library(BayesFactor); print(ttestBF(a, b))`).join('\n\n'); },
    paired_t: S => { const r = S.roles; return `${rComplete(S, [r.a, r.b])}
a <- ${rNum(S, r.a)}; b <- ${rNum(S, r.b)}; tt <- t.test(a, b, paired = TRUE); print(tt)
eff(${rq(`${nm(S, r.a)} − ${nm(S, r.b)}`)}, mean(a - b), NA, NA, tt$p.value)
# 베이즈 인자 (JASP와 같은 사전분포, BayesFactor 패키지 필요): library(BayesFactor); print(ttestBF(a, b, paired = TRUE))`; },
    oneway: S => { const r = S.roles, ph = S.opts.posthoc || 'scheffe'; return r.ys.map(yk => `${rComplete(S, [r.g, yk])}
g <- ${rCat(S, r.g)}; y <- ${rNum(S, yk)}
a <- anova(lm(y ~ g)); print(a)
eta <- a[["Sum Sq"]][1] / sum(a[["Sum Sq"]])
print(lev(split(y, g)))
${ph === 'bonferroni' ? 'print(pairwise.t.test(y, g, p.adjust.method = "bonferroni", pool.sd = TRUE))   # 분석 결정: 사후검정 Bonferroni'
  : `# 분석 결정: 사후검정 Scheffé
msw <- a[["Mean Sq"]][2]; k <- nlevels(g); dfw <- a[["Df"]][2]; m <- tapply(y, g, mean); n <- tapply(y, g, length)
for (i in 1:(k - 1)) for (j in (i + 1):k) { Fs <- (m[i] - m[j])^2 / (msw * (1 / n[i] + 1 / n[j]) * (k - 1)); cat(sprintf("%s − %s: 차이 %.2f, Scheffé p = %.4f\\n", names(m)[i], names(m)[j], m[i] - m[j], pf(Fs, k - 1, dfw, lower.tail = FALSE))) }`}
eff(${rq(`${nm(S, yk)}의 ${nm(S, r.g)}별 차이`)}, eta, NA, NA, a[["Pr(>F)"]][1])`).join('\n\n'); },
    factorial: S => { const r = S.roles, A = nm(S, r.a), B = nm(S, r.b), Y = nm(S, r.y); return `${rComplete(S, [r.a, r.b, r.y])}
A <- ${rCat(S, r.a)}; B <- ${rCat(S, r.b)}; y <- ${rNum(S, r.y)}
contrasts(A) <- contr.sum(nlevels(A)); contrasts(B) <- contr.sum(nlevels(B))
m <- lm(y ~ A * B)
t3 <- drop1(m, scope = c("A", "B", "A:B"), test = "F"); print(t3)   # 효과 코딩 + drop1 = 제III유형 제곱합
sse <- sum(resid(m)^2)
for (term in c("A", "B", "A:B")) {
  ss <- t3[term, "Sum of Sq"]; lab <- c(A = ${rq(A)}, B = ${rq(B)}, "A:B" = ${rq(`${A} × ${B}`)})[[term]]
  eff(paste0(lab, " 효과 (→ ", ${rq(Y)}, ")"), ss / (ss + sse), NA, NA, t3[term, "Pr(>F)"])
}`; },
    ancova: S => { const r = S.roles; return `${rComplete(S, [r.g, r.y, ...r.covs])}
g <- ${rCat(S, r.g)}; y <- ${rNum(S, r.y)}; C <- ${rDesign(S, r.covs)}
full <- lm(y ~ dum(g, "g") + C); red <- lm(y ~ C)
a <- anova(red, full); print(a)
ss <- a[["Sum of Sq"]][2]; sse <- sum(resid(full)^2)
eff(${rq(`${nm(S, r.y)}의 ${nm(S, r.g)}별 차이 (공변인 통제)`)}, ss / (ss + sse), NA, NA, a[["Pr(>F)"]][2])
inter <- lm(y ~ dum(g, "g") * C); print(anova(full, inter))   # 회귀 기울기 동질성`; },
    regression: S => { const r = S.roles; return `${rComplete(S, [r.y, ...r.xs])}
y <- ${rNum(S, r.y)}; X <- ${rDesign(S, r.xs)}
m <- lm(y ~ X); print(summary(m))
cf <- coefs(m)[-1, , drop = FALSE]; nmx <- colnames(X)
for (j in seq_along(nmx)) eff(paste0(nmx[j], " → ", ${rq(nm(S, r.y))}), cf$B[j], cf$lo[j], cf$hi[j], cf$p[j])
fs <- summary(m)$fstatistic
eff(${rq(`회귀모형 전체 설명력 (→ ${nm(S, r.y)})`)}, summary(m)$r.squared, NA, NA, pf(fs[1], fs[2], fs[3], lower.tail = FALSE))
# 베이즈 인자 (JASP와 같은 사전분포, BayesFactor 패키지 필요): library(BayesFactor); db <- data.frame(y = y, X, check.names = TRUE); print(lmBF(y ~ ., data = db)); print(regressionBF(y ~ ., data = db, whichModels = "top"))   # "top"은 변인 하나를 뺀 모형 ÷ 전체 모형 (통의 값의 역수)`; },
    hier: S => { const r = S.roles, blocks = [r.b1, r.b2, r.b3 || []].filter(b => b.length); let keys = []; return `${rComplete(S, [r.y, ...blocks.flat()])}
y <- ${rNum(S, r.y)}
` + blocks.map((b, s) => { keys = keys.concat(b); return `X${s + 1} <- ${rDesign(S, keys)}; m${s + 1} <- lm(y ~ X${s + 1}); cat("\\n모형 ${s + 1}\\n"); print(summary(m${s + 1}))` + (s ? `
a <- anova(m${s}, m${s + 1}); eff(${rq(`${s + 1}단계 투입 변인의 추가 설명력 (ΔR²)`)}, summary(m${s + 1})$r.squared - summary(m${s})$r.squared, NA, NA, a[["Pr(>F)"]][2])` : ''); }).join('\n') + `
cf <- coefs(m${blocks.length})[-1, , drop = FALSE]; nmx <- colnames(X${blocks.length})
for (j in seq_along(nmx)) eff(paste0(nmx[j], " → ", ${rq(nm(S, r.y))}, " (모형 ${blocks.length})"), cf$B[j], cf$lo[j], cf$hi[j], cf$p[j])`; },
    logistic: S => { const r = S.roles; return `${rComplete(S, [r.y, ...r.xs])}
y <- bin01(${rCat(S, r.y)}); X <- ${rDesign(S, r.xs)}
m <- glm(y ~ X, family = binomial); print(summary(m)); print(exp(cbind(OR = coef(m), suppressMessages(confint.default(m)))))
s <- summary(m)$coefficients[-1, , drop = FALSE]; nmx <- colnames(X)
for (j in seq_along(nmx)) eff(paste0(nmx[j], " → ", ${rq(`${nm(S, r.y)}(${S.vars[r.y].levels[1]}) 가능성`)}), s[j, 1], NA, NA, s[j, 4])`; },
    med_parallel: S => rMed(S, S.roles.ms, false),
    med_serial: S => rMed(S, [S.roles.m1, S.roles.m2], true),
    moderation: S => { const CT = S.roles.covs.length ? ' + C' : '', r = S.roles, X = nm(S, r.x), W = nm(S, r.w), Y = nm(S, r.y), wbin = S.vars[r.w].type === 'cat', xbin = S.vars[r.x].type === 'cat', cen = S.opts.center !== false; return `${rComplete(S, [r.x, r.w, r.y, ...r.covs])}
x <- ${rX(S, r.x)}; w <- ${rX(S, r.w)}; y <- ${rNum(S, r.y)}; C <- ${rDesign(S, r.covs)}
${cen ? `# 분석 결정: 평균중심화함
${xbin ? '' : 'x <- x - mean(x); '}${wbin ? '' : 'w <- w - mean(w)'}` : '# 분석 결정: 원점수 그대로'}
xw <- x * w; m <- lm(y ~ x + w + xw${CT}); print(summary(m))
cf <- coefs(m); V <- vcov(m); dfe <- m$df.residual
eff(${rq(`상호작용: ${X} × ${W} → ${Y} (조절효과)`)}, cf["xw", "B"], cf["xw", "lo"], cf["xw", "hi"], cf["xw", "p"])
eff(${rq(`${X} → ${Y} (주효과)`)}, cf["x", "B"], cf["x", "lo"], cf["x", "hi"], cf["x", "p"])
pts <- ${wbin ? `c(0, 1); labs <- ${rvec(S.vars[r.w].levels.slice(0, 2).map(l => `${W} = ${l}`))}` : `mean(w) + c(-1, 0, 1) * sd(w); labs <- c("M − 1SD", "M", "M + 1SD")`}
for (i in seq_along(pts)) { wv <- pts[i]; b <- cf["x", "B"] + cf["xw", "B"] * wv; se <- sqrt(V["x", "x"] + wv^2 * V["xw", "xw"] + 2 * wv * V["x", "xw"])
  eff(paste0("단순기울기: ", ${rq(W)}, " ", labs[i], "에서 ", ${rq(X)}, " → ", ${rq(Y)}), b, b - qt(.975, dfe) * se, b + qt(.975, dfe) * se, 2 * pt(-abs(b / se), dfe)) }`; },
    modmed7: S => rModmed(S, 7), modmed14: S => rModmed(S, 14)
  };
  function rCorr(S, method) {
    const ks = S.roles.vars;
    return `vars <- ${rvec(ks.map(k => S.vars[k].code))}; nms <- ${rvec(ks.map(k => S.vars[k].name))}
for (i in seq_along(vars)) for (j in seq_len(i - 1)) {
  a <- num(vars[i]); b <- num(vars[j]); ok <- !is.na(a) & !is.na(b)
  ct <- cor.test(a[ok], b[ok], method = "${method}"${method === 'spearman' ? ', exact = FALSE' : ''})
  eff(paste0(nms[i], " – ", nms[j], " 상관"), unname(ct$estimate), NA, NA, ct$p.value)${method === 'pearson' ? `
  # 베이즈 인자 (JASP와 같은 사전분포, BayesFactor 패키지 필요): library(BayesFactor); print(correlationBF(a[ok], b[ok], rscale = 1))   # rscale = 1: JASP 기본(균등분포)` : ''}
}`;
  }
  function rMed(S, ms, serial) {
    const CT = S.roles.covs.length ? ' + C' : '', r = S.roles, X = nm(S, r.x), Y = nm(S, r.y), M = ms.map(k => nm(S, k)), B = +S.opts.boot || 5000;
    const mList = ms.map((k, j) => `M${j + 1} <- ${rNum(S, k)}`).join('; ');
    const fits = serial
      ? `f1 <- fitb(M1[i], cbind(x[i], C[i, , drop = FALSE])); f2 <- fitb(M2[i], cbind(x[i], M1[i], C[i, , drop = FALSE])); fy <- fitb(y[i], cbind(x[i], M1[i], M2[i], C[i, , drop = FALSE]))
    c(f1[2] * fy[3], f1[2] * f2[3] * fy[4], f2[2] * fy[4], f1[2] * fy[3] + f1[2] * f2[3] * fy[4] + f2[2] * fy[4])`
      : `a <- sapply(list(${ms.map((_, j) => `M${j + 1}`).join(', ')}), function(m) fitb(m[i], cbind(x[i], C[i, , drop = FALSE]))[2]); fy <- fitb(y[i], cbind(x[i], ${ms.map((_, j) => `M${j + 1}[i]`).join(', ')}, C[i, , drop = FALSE]))
    ind <- a * fy[3:${2 + ms.length}]; ${ms.length > 1 ? 'c(ind, sum(ind))' : 'ind'}`;
    const indLabs = serial ? [`간접효과: ${X} → ${M[0]} → ${Y}`, `간접효과: ${X} → ${M[0]} → ${M[1]} → ${Y}`, `간접효과: ${X} → ${M[1]} → ${Y}`, `총 간접효과: ${X} → ${Y}`]
      : M.map(m => `간접효과: ${X} → ${m} → ${Y}`).concat(M.length > 1 ? [`총 간접효과: ${X} → (${M.join(', ')}) → ${Y}`] : []);
    const pathCode = serial
      ? `m1 <- lm(M1 ~ x${CT}); m2 <- lm(M2 ~ x + M1${CT}); my <- lm(y ~ x + M1 + M2${CT})
c1 <- coefs(m1); c2 <- coefs(m2)
eff(${rq(`경로: ${X} → ${M[0]}`)}, c1["x", "B"], c1["x", "lo"], c1["x", "hi"], c1["x", "p"])
eff(${rq(`경로: ${X} → ${M[1]}`)}, c2["x", "B"], c2["x", "lo"], c2["x", "hi"], c2["x", "p"])
eff(${rq(`경로: ${M[0]} → ${M[1]}`)}, c2["M1", "B"], c2["M1", "lo"], c2["M1", "hi"], c2["M1", "p"])`
      : ms.map((_, j) => `c${j + 1} <- coefs(lm(M${j + 1} ~ x${CT})); eff(${rq(`경로: ${X} → ${M[j]}`)}, c${j + 1}["x", "B"], c${j + 1}["x", "lo"], c${j + 1}["x", "hi"], c${j + 1}["x", "p"])`).join('\n') + `
my <- lm(y ~ x + ${ms.map((_, j) => `M${j + 1}`).join(' + ')}${CT})`;
    return `${rComplete(S, [r.x, ...ms, r.y, ...r.covs])}
x <- ${rX(S, r.x)}; ${mList}; y <- ${rNum(S, r.y)}; C <- ${rDesign(S, r.covs)}; n <- sum(u)
${pathCode}
cy <- coefs(my); print(summary(my))
${ms.map((_, j) => `eff(${rq(`경로: ${M[j]} → ${Y}`)}, cy["M${j + 1}", "B"], cy["M${j + 1}", "lo"], cy["M${j + 1}", "hi"], cy["M${j + 1}", "p"])`).join('\n')}
eff(${rq(`직접효과: ${X} → ${Y}`)}, cy["x", "B"], cy["x", "lo"], cy["x", "hi"], cy["x", "p"])
ct <- coefs(lm(y ~ x${CT})); eff(${rq(`총효과: ${X} → ${Y}`)}, ct["x", "B"], ct["x", "lo"], ct["x", "hi"], ct["x", "p"])
# 간접효과: 백분위 부트스트랩 (분석 결정: ${B.toLocaleString()}번)
stat <- function(i) {
    ${fits}
}
est <- stat(1:n)
set.seed(20261019); bs <- t(replicate(${B}, stat(sample.int(n, n, replace = TRUE))))
if (is.null(dim(bs)) || nrow(bs) == 1) bs <- matrix(bs, ncol = length(est))
labs <- ${rvec(indLabs)}
for (j in seq_along(est)) eff(labs[j], est[j], pct(bs[, j], .025), pct(bs[, j], .975))`;
  }
  function rModmed(S, model) {
    const CT = S.roles.covs.length ? ' + C' : '', r = S.roles, X = nm(S, r.x), M = nm(S, r.m), W = nm(S, r.w), Y = nm(S, r.y), B = +S.opts.boot || 5000, cen = S.opts.center !== false;
    const xbin = S.vars[r.x].type === 'cat', wbin = S.vars[r.w].type === 'cat';
    const labs = wbin ? S.vars[r.w].levels.slice(0, 2).map(l => `${W} = ${l}`) : ['M − 1SD', 'M', 'M + 1SD'];
    return `${rComplete(S, [r.x, r.m, r.w, r.y, ...r.covs])}
x <- ${rX(S, r.x)}; m <- ${rNum(S, r.m)}; w <- ${rX(S, r.w)}; y <- ${rNum(S, r.y)}; C <- ${rDesign(S, r.covs)}; n <- sum(u)
${cen ? `# 분석 결정: 평균중심화함
${xbin ? '' : 'x <- x - mean(x); '}${wbin ? '' : 'w <- w - mean(w); '}${model === 14 ? 'm <- m - mean(m)' : ''}` : '# 분석 결정: 원점수 그대로'}
pts <- ${wbin ? 'c(0, 1)' : 'mean(w) + c(-1, 0, 1) * sd(w)'}; labs <- ${rvec(labs)}
${model === 7 ? `fm <- lm(m ~ x + w + I(x * w)${CT}); fy <- lm(y ~ x + m${CT}); print(summary(fm)); print(summary(fy))
ci <- coefs(fm); eff(${rq(`상호작용: ${X} × ${W} → ${M}`)}, ci[4, "B"], ci[4, "lo"], ci[4, "hi"], ci[4, "p"])
stat <- function(i) { a <- fitb(m[i], cbind(x[i], w[i], x[i] * w[i], C[i, , drop = FALSE])); b <- fitb(y[i], cbind(x[i], m[i], C[i, , drop = FALSE]))[3]
  c(a[4] * b, (a[2] + a[4] * pts) * b) }`
  : `fm <- lm(m ~ x${CT}); fy <- lm(y ~ x + m + w + I(m * w)${CT}); print(summary(fm)); print(summary(fy))
ci <- coefs(fy); eff(${rq(`상호작용: ${M} × ${W} → ${Y}`)}, ci[5, "B"], ci[5, "lo"], ci[5, "hi"], ci[5, "p"])
stat <- function(i) { a <- fitb(m[i], cbind(x[i], C[i, , drop = FALSE]))[2]; b <- fitb(y[i], cbind(x[i], m[i], w[i], m[i] * w[i], C[i, , drop = FALSE]))
  c(a * b[5], a * (b[3] + b[5] * pts)) }`}
est <- stat(1:n)
set.seed(20261019); bs <- t(replicate(${B}, stat(sample.int(n, n, replace = TRUE))))   # 분석 결정: 부트스트랩 ${B.toLocaleString()}번
eff(${rq(`조절된 매개 지수: ${J(W, '이/가')} ${X} → ${M} → ${Y} 간접효과를 조절`)}, est[1], pct(bs[, 1], .025), pct(bs[, 1], .975))
for (j in seq_along(pts)) eff(paste0("조건부 간접효과 (", ${rq(W)}, " ", labs[j], "): ", ${rq(`${X} → ${M} → ${Y}`)}), est[j + 1], pct(bs[, j + 1], .025), pct(bs[, j + 1], .975))`;
  }

  function rScript(list) {
    const ok = list.filter(a => a.spec && RGEN[a.spec.method]);
    const parts = ok.map((a, i) => `
# ---------------------------------------------------------------------
# [${i + 1}] ${a.title}
#     통 계산: ${a.at}, N = ${a.n}
${a.conf ? `#     구분: ${confLabel(a)}\n` : ''}${(a.decisions || []).map(x => `#     분석 결정: ${x}`).join('\n')}
# ---------------------------------------------------------------------
AID <- ${rq(a.id)}; cat("\\nANALYSIS\\t", AID, "\\t", ${rq(a.title)}, "\\n", sep = "")
local({
${RGEN[a.spec.method](a.spec).split('\n').map(l => '  ' + l).join('\n')}
})`);
    return `# =====================================================================
# 통 R 분석 코드 — 만든 날 ${today()}
# 연구자가 정한 분석 결정을 그대로 따르는 코드입니다. 기본 R만 씁니다 (추가 패키지 없음${ok.some(a => /^sem/.test(a.spec.method)) ? '. 구조방정식만 lavaan 패키지가 필요합니다' : ''}).
# 결정을 바꾸려면 코드를 고치지 말고 통에서 바꾼 뒤 다시 받으세요.
#
# 분석 결정
${liveDecisions().map(d => `#   - ${d.q} → ${decLabel(d, d.get())}`).join('\n')}
${plan.log.filter(l => l.after).map(l => `#   * 사후 변경: ${l.q} ${l.from} → ${l.to}${l.reason ? ` (이유: ${l.reason})` : ''}`).join('\n')}
#
# 실행: 이 파일과 analysis_data.csv를 같은 폴더에 두고 (연구 폴더라면 3_analysis 폴더에서 그대로)
#   터미널:  Rscript analysis.R > R_output.txt
#   RStudio: 파일을 열고 [Source]
# 끝에 나오는 EFFECT 줄을 통 [R 결과와 맞춰 보기]에 붙여 넣으면 숫자를 대조합니다.
# =====================================================================
${R_HEAD}${parts.join('\n')}

write.csv(EFF, "R_results.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\\n완료: 효과", nrow(EFF), "개 (R_results.csv에 저장)\\n")
`;
  }

  // R 출력과 대조
  function parseROut(text) {
    const out = {}; let cur = '_';
    String(text).split(/\r?\n/).forEach(l => {
      const a = l.split('\t');
      if (a[0] === 'ANALYSIS') { cur = a[1]; return; }
      if (a[0] === 'EFFECT' && a.length >= 6) (out[cur] = out[cur] || []).push({ label: a[1], est: +a[2], lo: a[3] === 'NA' ? null : +a[3], hi: a[4] === 'NA' ? null : +a[4], p: a[5] === 'NA' ? null : +a[5] });
    });
    // R_results.csv를 붙여 넣은 경우
    if (!Object.keys(out).length && /"?analysis"?,"?label"?/.test(text)) {
      parseTable(text).rows.forEach(r => (out[r[0]] = out[r[0]] || []).push({ label: r[1], est: +r[2], lo: r[3] === 'NA' || r[3] === '' ? null : +r[3], hi: r[4] === 'NA' || r[4] === '' ? null : +r[4], p: r[5] === 'NA' || r[5] === '' ? null : +r[5] }));
    }
    return out;
  }
  function compareR(a, rows) {
    const near = (x, y, tol) => x != null && y != null && Math.abs(x - y) <= tol + 1e-4 * Math.abs(x);
    return a.effects.map(e => {
      const r = (rows || []).find(x => x.label === e.label);
      if (!r) return { e, r: null, st: 'missing' };
      const estOk = near(e.est, r.est, 5e-4);
      const pOk = e.p == null || r.p == null || (e.p < 0.001 && r.p < 0.001) || near(e.p, r.p, 1e-3);
      let ciOk = true, ciNote = '';
      if (e.lo != null && r.lo != null) {
        if (e.ciBased) { const s1 = !(e.lo <= 0 && e.hi >= 0), s2 = !(r.lo <= 0 && r.hi >= 0); ciOk = s1 === s2; ciNote = '부트스트랩 구간은 난수가 달라 조금 다를 수 있음'; }
        else ciOk = near(e.lo, r.lo, 5e-3) && near(e.hi, r.hi, 5e-3);
      }
      return { e, r, st: estOk && pOk && ciOk ? 'ok' : 'diff', estOk, pOk, ciOk, ciNote };
    });
  }
  // p값 너머 읽기: p값 하나로 결론 내리지 않도록 효과크기·신뢰구간·베이즈 인자·검정 횟수를 함께 짚음
  const esSmall = e => e.es != null && isFinite(e.es) && Math.abs(e.es) < (e.esKind === 'd' ? 0.2 : 0.1);
  function beyondPList(a) {
    const out = [], L3 = Math.log(2.995);   // 표의 BF 반올림(3.00)과 같은 기준
    a.effects.filter(e => e.kind !== 'omnibus' && isFinite(e.p) && !e.ciBased).forEach(e => {
      const sig = e.p < 0.05, nm = e.label, bf = e.lbf != null && isFinite(e.lbf);
      const ci = e.lo != null && isFinite(e.lo) ? ` 95% CI ${CI(e.lo, e.hi)}` : '';
      if (sig && esSmall(e)) out.push({ e: nm, t: `유의하지만 효과가 아주 작습니다(${e.esKind === 'd' ? 'd' : e.esKind === 'beta' ? 'β' : 'r'} = ${NZ(e.es)}). 사람 수가 많으면 아주 작은 효과도 유의해집니다. 이 크기가 실제로 의미 있는지 따로 논의하세요.` });
      if (sig && bf && e.lbf < L3) out.push({ e: nm, t: `p < .05지만 베이즈 인자 BF₁₀ = ${BFF(e.lbf)}로, 효과가 있다는 증거는 아직 약합니다. "지지되었다"를 강하게 쓰지 마세요.` });
      if (e.p >= 0.04 && e.p < 0.06) out.push({ e: nm, t: `p = ${NZ(e.p, 3)}로 .05 경계에 있습니다. 표본이 조금만 달라도 판정이 바뀔 수 있습니다. "유의한 경향"처럼 쓰기보다 효과크기와 신뢰구간으로 설명하세요.` });
      if (!sig) {
        if (bf && e.lbf <= -L3) out.push({ e: nm, t: `유의하지 않고, 베이즈 인자 BF₀₁ = ${BFF(-e.lbf)}로 효과가 없다는 쪽을 지지합니다. 이 경우에는 "관련이 거의 없다"고 쓸 근거가 있습니다.` });
        else out.push({ e: nm, t: `유의하지 않다는 것은 "효과가 없다"는 뜻이 아닙니다.${ci ? `${ci}에는 0과 함께 의미 있는 크기의 값도 들어 있습니다.` : ''}${bf ? ` 베이즈 인자 BF₀₁ = ${BFF(-e.lbf)}로 자료가 어느 쪽도 뚜렷이 지지하지 않아, 판단할 증거가 부족하다고 쓰는 것이 정확합니다.` : ' 판단할 증거가 부족하다고 쓰는 것이 정확합니다.'}` });
      }
    });
    return out;
  }
  function beyondPHtml(a) {
    if (!a.effects.some(e => isFinite(e.p))) return '';
    const L = beyondPList(a);
    const nTests = analyses.reduce((s, x) => s + x.effects.filter(e => e.kind !== 'omnibus' && isFinite(e.p) && !e.ciBased).length, 0);
    const hasBF = a.effects.some(e => e.lbf != null && isFinite(e.lbf));
    return `<details class="item-detail beyondp"><summary>p값 너머 읽기 ${L.length ? `<span class="a-mid">· 살펴볼 것 ${L.length}개</span>` : '<span class="hint">· 특별히 짚을 것 없음</span>'}</summary>
      <div style="padding:4px 16px 14px">
        <p class="hint">p값은 "효과가 없다면 이만큼 극단적인 결과가 나올 확률"입니다. 가설이 맞을 확률도, 효과의 크기도 아닙니다. 그래서 효과크기, 신뢰구간${hasBF ? ', 베이즈 인자' : ''}를 함께 읽습니다.</p>
        ${L.length ? `<ul class="notes">${L.map(x => `<li><b>${esc(x.e)}</b>: ${esc(x.t)}</li>`).join('')}</ul>` : ''}
        ${nTests >= 10 ? `<p class="hint">⚠ 지금까지 한 검정이 ${nTests}개입니다. 효과가 하나도 없어도 약 ${Math.round(nTests * 0.05)}개는 우연히 p < .05가 나옵니다. 연구 계획 때 정한 가설 검정과 그 밖의 탐색적 분석을 논문에서 나누어 쓰세요.</p>` : ''}
      </div></details>`;
  }

  // 결과마다 그 결과를 만든 R 코드 (jamovi의 syntax mode처럼). 기본 R만 써서 누구나 같은 숫자를 다시 얻을 수 있음
  function rcodeHtml(a) {
    if (!a.spec || !RGEN[a.spec.method]) return '';
    let body = ''; try { body = RGEN[a.spec.method](a.spec); } catch (e) { return ''; }
    const boot = /set\.seed/.test(body);
    const hl = esc(body).split('\n').map(l => l.replace(/(^|\s)(#.*)$/, '$1<span class="rc-c">$2</span>')).join('\n');
    return `<details class="item-detail rcode"><summary>R 코드 보기</summary>
      <div style="padding:4px 16px 14px">
        <p class="hint">이 결과를 만든 계산을 R 코드로 옮긴 것입니다. 추가 패키지 없이 기본 R만 씁니다. [이 분석만 .R 파일로 받기]로 받은 파일과 analysis_data.csv를 한 폴더에 두고 R에서 돌리면 위 표와 같은 숫자가 나옵니다${boot ? ' (부트스트랩 신뢰구간은 R의 난수가 달라 소수 셋째 자리에서 조금 다를 수 있습니다)' : ''}. 같은 자료와 같은 코드면 언제 누가 돌려도 같은 결과가 나오는 것, 이것이 <b>재현성</b>입니다.</p>
        <pre class="r-code no-tw">${hl}</pre>
        <div class="actions tight"><button class="btn sm" data-an-act="r-copy" data-id="${a.id}">코드 복사 (자료 읽기 포함)</button><button class="btn sm" data-an-act="r-one" data-id="${a.id}">이 분석만 .R 파일로 받기</button><button class="btn sm" data-an-act="r-data" ${lastScored ? '' : 'disabled'}>analysis_data.csv 받기</button></div>
      </div></details>`;
  }

  function rcheckHtml(a) {
    if (!a.spec) return '';
    const c = a.rcheck;
    const tbl = c ? (() => {
      const rows = compareR(a, c.rows), ok = rows.filter(x => x.st === 'ok').length;
      return `<p class="${ok === rows.length ? 'a-ok' : 'a-mid'}" style="margin:8px 0">${ok === rows.length ? `✓ 효과 ${rows.length}개 모두 R과 일치합니다.` : `효과 ${rows.length}개 중 ${ok}개 일치. 다른 곳을 확인하세요.`} <span class="hint">(${esc(c.at)})</span></p>
        <div class="table-wrap"><table class="compact"><thead><tr><th>효과</th><th class="num">사이트</th><th class="num">R</th><th>일치</th></tr></thead><tbody>
        ${rows.map(x => `<tr><td>${esc(x.e.label)}</td><td class="num">${F2(x.e.est, 3)}${x.e.lo != null ? ` ${CI(x.e.lo, x.e.hi, 3)}` : ''}${x.e.p != null ? ` ${PS(x.e.p)}` : ''}</td>
          <td class="num">${x.r ? `${F2(x.r.est, 3)}${x.r.lo != null ? ` ${CI(x.r.lo, x.r.hi, 3)}` : ''}${x.r.p != null ? ` ${PS(x.r.p)}` : ''}` : '-'}</td>
          <td>${x.st === 'missing' ? '<span class="a-mid">R 출력에 없음</span>' : x.st === 'ok' ? `<span class="a-ok">✓</span>${x.ciNote ? ` <span class="hint">${esc(x.ciNote)}, 판정 같음</span>` : ''}` : `<span class="a-low">다름</span> <span class="hint">${[!x.estOk && '추정값', !x.pOk && 'p', !x.ciOk && '신뢰구간'].filter(Boolean).join(', ')}</span>`}</td></tr>`).join('')}
        </tbody></table></div>`;
    })() : '';
    return `<details class="item-detail" ${c ? 'open' : ''}><summary>R 결과와 맞춰 보기 ${c ? (compareR(a, c.rows).every(x => x.st === 'ok') ? '<span class="a-ok">· R과 일치</span>' : '<span class="a-mid">· 차이 있음</span>') : ''}</summary>
      <div style="padding:4px 16px 14px">
        <p class="hint">위의 [R로 확인하기]에서 받은 코드를 R로 돌린 출력(R_output.txt)이나 R_results.csv를 붙여 넣으세요.</p>
        <textarea rows="3" class="code-box" style="height:auto" data-rpaste="${a.id}" placeholder="EFFECT 줄이 들어 있는 R 출력"></textarea>
        <div class="actions tight"><button class="btn sm primary" data-an-act="rcompare" data-id="${a.id}">대조하기</button></div>
        ${tbl}
      </div></details>`;
  }

  function rPanelHtml() {
    const n = analyses.filter(a => a.spec).length;
    return `<div class="card">
      <div class="res-head"><h3 class="card-title">R·SPSS로 확인하기</h3><a class="guide-link" href="#guide-r">설명</a></div>
      <div class="sp-box"><b>SPSS로 확인하기</b><p class="hint">SPSS가 있으면 같은 분석을 SPSS로 다시 돌려 숫자를 대조할 수 있습니다. AI는 쓰지 않습니다.</p>
        <ol class="sp-steps"><li><b>analysis_data.csv</b>와 <b>analysis.sps</b>를 받아 한 폴더에 넣기 <span class="actions tight" style="display:inline-flex"><button class="btn sm primary" data-an-act="r-data" ${lastScored ? '' : 'disabled'}>analysis_data.csv</button><button class="btn sm primary" data-an-act="spss-code" ${n ? '' : 'disabled'}>analysis.sps (분석 ${n}개)</button></span></li>
          <li>SPSS에서 analysis.sps 열기 → 맨 위 <code>FILE HANDLE</code> 줄의 경로를 그 폴더로 바꾸기 → <b>[실행] → [모두]</b></li>
          <li>결과의 숫자를 각 결과 아래 <b>[SPSS와 맞춰 보기]</b>에 적기 → ✓가 나오면 같은 값</li></ol>
        <p class="hint">매개·조절은 Hayes의 PROCESS 매크로(무료)가 SPSS에 설치되어 있어야 합니다. 부트스트랩 신뢰구간은 재표집 때문에 소수 둘째 자리쯤에서 조금 다를 수 있습니다(정상).</p></div>
      <h4 style="margin:14px 0 4px">R로 확인하기</h4>
      <p class="hint">통의 숫자를 표준 통계 프로그램인 R로 다시 계산해 대조합니다. R 코드는 위의 분석 결정을 그대로 따르고, 추가 패키지 없이 기본 R만 씁니다. AI가 숫자를 만들지 않습니다.</p>
      <div class="chat-steps">
        <div class="chat-step"><span class="n">1</span><div><b>두 파일을 받아 한 폴더에 넣기</b>
          <div class="actions tight"><button class="btn sm primary" data-an-act="r-data" ${lastScored ? '' : 'disabled'}>analysis_data.csv 받기</button><button class="btn sm primary" data-an-act="r-code" ${n ? '' : 'disabled'}>analysis.R 받기 (분석 ${n}개)</button></div>
          ${lastScored ? '' : '<p class="hint">analysis_data.csv는 [응답 취합]에서 점수를 만든 뒤 받을 수 있습니다.</p>'}</div></div>
        <div class="chat-step"><span class="n">2</span><div><b>R 실행</b> — 둘 중 하나
          <ul class="notes"><li><b>AI 도우미</b> (Claude Code · Codex · Antigravity): 그 폴더를 열고 <b>"R로 분석해줘"</b>. AI가 R을 대신 돌리고 결과를 정리합니다. 컴퓨터에 R이 있어야 합니다.</li>
          <li><b>RStudio</b>: <code>analysis.R</code>을 열고 [Source]. R을 몰라도 버튼 하나면 됩니다.</li></ul></div></div>
        <div class="chat-step"><span class="n">3</span><div><b>R 출력을 각 결과의 [R 결과와 맞춰 보기]에 붙여 넣기</b><p class="hint">효과마다 사이트와 R의 숫자가 같은지 표로 보여 줍니다.</p></div></div>
      </div>
    </div>`;
  }

  // =================================================================

// ##### 9. SPSS 명령문 만들기 · SPSS 출력과 대조 (stats-ladder.html 11849~12026줄) #####
  // =================================================================
  // SPSS로 확인하기: 명령문(.sps) 만들기 + SPSS 출력의 숫자와 맞춰 보기 (AI 없이 규칙으로)
  // =================================================================
  // 분석용 자료 열 이름 (자료 없이도 설문 설계로 만들 수 있게)
  function dataHeader() {
    const head = [{ code: 'ID', kind: 'id' }];
    sv.demos.forEach((d, di) => { head.push({ code: demoCode(d, di), kind: d.type === 'number' ? 'num' : 'text', label: d.text }); if (d.type === 'choice') head.push({ code: demoCode(d, di) + '_n', kind: 'code', label: d.text, demo: d }); });
    designItems().filter(d => d.kind === 'item').forEach(d => head.push({ code: d.code, kind: 'item' }));
    sv.scales.forEach((s, si) => head.push({ code: scaleCode(s, si), kind: 'score', label: s.name }));
    head.push({ code: '불성실의심', kind: 'flag' });
    return head;
  }
  // SPSS 변수 이름: 글자로 시작, 공백·기호는 _로, 겹치면 번호
  function spssNames(head) {
    const used = new Set(), map = {};
    head.forEach(h => {
      let n = h.kind === 'flag' ? 'STRAIGHT' : String(h.code).replace(/[^\p{L}\p{N}_]/gu, '_');
      if (!/^\p{L}/u.test(n)) n = 'V' + n;
      let m = n, i = 2; while (used.has(m.toUpperCase())) m = n + '_' + i++;
      used.add(m.toUpperCase()); map[h.code] = m;
    });
    return map;
  }
  const sq = s => "'" + String(s).replace(/'/g, "''") + "'";
  const wrapList = (arr, ind = '    ') => { const out = []; let line = ''; arr.forEach(a => { if ((line + ' ' + a).length > 70) { out.push(line); line = a; } else line = line ? line + ' ' + a : a; }); if (line) out.push(line); return out.join('\n' + ind); };

  function spssSyntax(list) {
    const head = dataHeader(), N = spssNames(head);
    const fmt = h => h.kind === 'id' ? 'A12' : h.kind === 'text' ? 'A200' : h.kind === 'num' ? 'F10.2' : h.kind === 'code' ? 'F3.0' : h.kind === 'item' ? 'F4.0' : h.kind === 'score' ? 'F12.6' : 'F1.0';
    const L = [];
    L.push(`* =====================================================================.`,
      `* 통 분석을 SPSS로 다시 계산하는 명령문. 만든 날: ${today()}.`,
      `* 1) 이 파일과 analysis_data.csv를 한 폴더에 둡니다.`,
      `* 2) 아래 FILE HANDLE 줄의 따옴표 안을 그 폴더 경로로 바꿉니다 (예: C:\\내논문\\2_data).`,
      `* 3) 메뉴 [실행] → [모두]. 결과의 숫자를 통 각 결과의 [SPSS와 맞춰 보기]에 넣습니다.`,
      `* 매개·조절은 Hayes의 PROCESS 매크로가 설치되어 있어야 합니다 (processmacro.org).`,
      `* =====================================================================.`, '',
      `* 한글이 깨지지 않게 유니코드 모드로 (열린 자료가 있으면 모두 닫고 실행하세요).`, 'SET UNICODE=ON.', '',
      `FILE HANDLE tongdir /NAME='C:\\tong'.`, '',
      `GET DATA /TYPE=TXT /FILE='tongdir/analysis_data.csv' /ENCODING='UTF8'`,
      `  /DELCASE=LINE /DELIMITERS="," /QUALIFIER='"' /ARRANGEMENT=DELIMITED /FIRSTCASE=2`,
      `  /VARIABLES=`);
    head.forEach(h => L.push(`    ${N[h.code]} ${fmt(h)}`));
    L.push('.', 'EXECUTE.', 'DATASET NAME tong WINDOW=FRONT.', '');
    const labs = head.filter(h => h.label && (h.kind === 'score' || h.kind === 'code' || h.kind === 'num'));
    if (labs.length) L.push('VARIABLE LABELS', '  ' + labs.map(h => `${N[h.code]} ${sq(h.label)}`).join('\n  /') + '.');
    const codes = head.filter(h => h.kind === 'code');
    if (codes.length) L.push('VALUE LABELS', '  ' + codes.map(h => `${N[h.code]} ${h.demo.options.map((o, i) => `${i + 1} ${sq(o)}`).join(' ')}`).join('\n  /') + '.', '');
    // 측정 수준: 점수·숫자는 척도, 분류는 명목, 문항은 서열
    const lv = [['SCALE', head.filter(h => h.kind === 'score' || h.kind === 'num')], ['NOMINAL', head.filter(h => h.kind === 'code' || h.kind === 'flag')], ['ORDINAL', head.filter(h => h.kind === 'item')]].filter(x => x[1].length);
    if (lv.length) L.push('VARIABLE LEVEL', '  ' + lv.map(([k, hs]) => `${hs.map(h => N[h.code]).join(' ')} (${k})`).join('\n  /') + '.', '');
    L.push('* 변인 이름·설명·값 설명·측정 수준을 붙인 자료를 SPSS 파일(.sav)로 저장합니다. 다음부터는 이 .sav를 바로 열어도 됩니다.', `SAVE OUTFILE='tongdir/analysis_data.sav'.`, '');
    // 변인 키 → SPSS 이름 (분류는 _n 번호)
    list.forEach((a, i) => {
      const S = a.spec; if (!S || !SPSSGEN[S.method]) return;
      const v = k => { const x = S.vars[k]; return x.type === 'cat' ? N[x.code + '_n'] || N[x.code] : N[x.code]; };
      const lvCode = (k, lv) => { const x = S.vars[k], d = sv.demos.find((dm, di) => demoCode(dm, di) === x.code); const j = d ? d.options.indexOf(lv) : -1; return j >= 0 ? j + 1 : null; };
      const dummies = [], dum = keys => keys.flatMap(k => {
        const x = S.vars[k]; if (x.type !== 'cat') return [v(k)];
        return (x.levels || []).slice(1).map(l => { const c = lvCode(k, l), nmv = `${v(k)}_${c}`; dummies.push(`COMPUTE ${nmv} = (${v(k)} = ${c}).\nVARIABLE LABELS ${nmv} ${sq(`${x.name}: ${l} (기준 ${x.levels[0]})`)}.`); return nmv; });
      });
      const body = SPSSGEN[S.method](S, { v, lvCode, dum, opts: S.opts || {} });
      L.push(`* ---------------------------------------------------------------------.`, `* [${i + 1}] ${a.title} (통 N = ${a.n}).`, `* ---------------------------------------------------------------------.`);
      if (dummies.length) L.push(...dummies, 'EXECUTE.');
      L.push(body, '');
    });
    return L.join('\n');
  }
  const proc = (S, h, model, extra = '') => { const r = S.roles, o = h.opts; const cov = (r.covs || []).length ? ` /cov=${h.dum(r.covs).join(' ')}` : '';
    return `process y=${h.v(r.y)} /x=${h.v(r.x)}${extra}${cov} /model=${model} /boot=${+o.boot || 5000} /seed=20261020 /total=1${o.center ? ' /center=1' : ''}.`; };
  const SPSSGEN = {
    pearson: (S, h) => `CORRELATIONS /VARIABLES=${S.roles.vars.map(h.v).join(' ')} /PRINT=TWOTAIL NOSIG /MISSING=PAIRWISE.`,
    spearman: (S, h) => `NONPAR CORR /VARIABLES=${S.roles.vars.map(h.v).join(' ')} /PRINT=SPEARMAN TWOTAIL NOSIG /MISSING=PAIRWISE.`,
    partial: (S, h) => `PARTIAL CORR /VARIABLES=${h.v(S.roles.x)} ${h.v(S.roles.y)} BY ${h.dum(S.roles.covs).join(' ')} /SIGNIFICANCE=TWOTAIL /MISSING=LISTWISE.`,
    pointbiserial: (S, h) => `CORRELATIONS /VARIABLES=${h.v(S.roles.g)} ${h.v(S.roles.y)} /PRINT=TWOTAIL NOSIG /MISSING=LISTWISE.`,
    chisq: (S, h) => `CROSSTABS /TABLES=${h.v(S.roles.a)} BY ${h.v(S.roles.b)} /STATISTICS=CHISQ PHI /CELLS=COUNT ROW.`,
    onesample: (S, h) => `T-TEST /TESTVAL=${Number(h.opts.testValue)} /VARIABLES=${h.v(S.roles.y)}.`,
    ind_t: (S, h) => { const lv = S.vars[S.roles.g].levels; return `T-TEST GROUPS=${h.v(S.roles.g)}(${h.lvCode(S.roles.g, lv[0])} ${h.lvCode(S.roles.g, lv[1])}) /VARIABLES=${S.roles.ys.map(h.v).join(' ')}.`; },
    paired_t: (S, h) => `T-TEST PAIRS=${h.v(S.roles.a)} WITH ${h.v(S.roles.b)} (PAIRED).`,
    oneway: (S, h) => `* 효과크기(/ES)는 SPSS 27 이상에서 나옵니다. 그 아래 버전이면 /ES=OVERALL을 지우세요.\nONEWAY ${S.roles.ys.map(h.v).join(' ')} BY ${h.v(S.roles.g)} /ES=OVERALL /STATISTICS=DESCRIPTIVES HOMOGENEITY WELCH /POSTHOC=${(h.opts.posthoc || 'scheffe') === 'bonferroni' ? 'BONFERRONI' : 'SCHEFFE'} ALPHA(0.05).`,
    factorial: (S, h) => { const a = h.v(S.roles.a), b = h.v(S.roles.b); return `UNIANOVA ${h.v(S.roles.y)} BY ${a} ${b} /METHOD=SSTYPE(3) /PRINT=ETASQ HOMOGENEITY DESCRIPTIVE /DESIGN=${a} ${b} ${a}*${b}.`; },
    ancova: (S, h) => { const g = h.v(S.roles.g), c = S.roles.covs.map(h.v).join(' '); return `UNIANOVA ${h.v(S.roles.y)} BY ${g} WITH ${c} /METHOD=SSTYPE(3) /PRINT=ETASQ DESCRIPTIVE /EMMEANS=TABLES(${g}) WITH(MEAN) /DESIGN=${c} ${g}.\n* 회귀 기울기 동질성 확인.\nUNIANOVA ${h.v(S.roles.y)} BY ${g} WITH ${c} /METHOD=SSTYPE(3) /DESIGN=${g} ${c} ${S.roles.covs.map(k => `${g}*${h.v(k)}`).join(' ')}.`; },
    regression: (S, h) => `REGRESSION /STATISTICS=COEFF OUTS CI(95) R ANOVA COLLIN TOL /DEPENDENT=${h.v(S.roles.y)} /METHOD=ENTER ${h.dum(S.roles.xs).join(' ')}.`,
    hier: (S, h) => { const r = S.roles; return `REGRESSION /STATISTICS=COEFF OUTS CI(95) R ANOVA CHANGE COLLIN TOL /DEPENDENT=${h.v(r.y)}\n  /METHOD=ENTER ${h.dum(r.b1).join(' ')}\n  /METHOD=ENTER ${h.dum(r.b2).join(' ')}${(r.b3 || []).length ? `\n  /METHOD=ENTER ${h.dum(r.b3).join(' ')}` : ''}.`; },
    logistic: (S, h) => { const r = S.roles, cats = r.xs.filter(k => S.vars[k].type === 'cat').map(h.v); return `LOGISTIC REGRESSION VARIABLES ${h.v(r.y)} /METHOD=ENTER ${r.xs.map(h.v).join(' ')}${cats.length ? ` /CATEGORICAL=${cats.join(' ')} ${cats.map(c => `/CONTRAST(${c})=Indicator(1)`).join(' ')}` : ''} /PRINT=CI(95).`; },
    med_parallel: (S, h) => proc(S, h, 4, ` /m=${S.roles.ms.map(h.v).join(' ')}`),
    med_serial: (S, h) => proc(S, h, 6, ` /m=${h.v(S.roles.m1)} ${h.v(S.roles.m2)}`),
    moderation: (S, h) => proc(S, h, 1, ` /w=${h.v(S.roles.w)} /moments=1`),
    modmed7: (S, h) => proc(S, h, 7, ` /m=${h.v(S.roles.m)} /w=${h.v(S.roles.w)} /moments=1`),
    modmed14: (S, h) => proc(S, h, 14, ` /m=${h.v(S.roles.m)} /w=${h.v(S.roles.w)} /moments=1`),
    rm_anova: (S, h) => { const ys = S.roles.ys.map(h.v); return `GLM ${ys.join(' ')} /WSFACTOR=time ${ys.length} Polynomial /METHOD=SSTYPE(3) /PRINT=DESCRIPTIVE ETASQ /EMMEANS=TABLES(time) COMPARE ADJ(BONFERRONI) /WSDESIGN=time.`; },
    mixed_anova: (S, h) => { const ys = S.roles.ys.map(h.v), g = h.v(S.roles.g); return `GLM ${ys.join(' ')} BY ${g} /WSFACTOR=time ${ys.length} Polynomial /METHOD=SSTYPE(3) /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY /WSDESIGN=time /DESIGN=${g}.`; },
    mcnemar: (S, h) => `CROSSTABS /TABLES=${h.v(S.roles.a)} BY ${h.v(S.roles.b)} /STATISTICS=MCNEMAR /CELLS=COUNT.`,
    cochran: (S, h) => `NPAR TESTS /COCHRAN=${S.roles.vars.map(h.v).join(' ')}.`
  };

  // ---------- SPSS 출력에서 찾을 숫자 ----------
  const KW = (ko, en) => `${ko} (${en})`;
  function spssTargets(a) {
    const m = a.method, out = [];
    const add = (e, k, v, name, where, o = {}) => { if (v == null || !isFinite(v)) return; out.push({ id: `${e.id}.${k}`, eff: e, k, v, name, where, ...o }); };
    a.effects.forEach(e => {
      const P = (where) => add(e, 'p', e.p, 'p', where, { isP: true });
      if (['pearson', 'spearman', 'pointbiserial'].includes(m)) { add(e, 'est', e.est, 'r', KW(m === 'spearman' ? '상관계수' : 'Pearson 상관', 'Correlation Coefficient')); P(KW('유의확률 (양측)', 'Sig. (2-tailed)')); }
      else if (m === 'partial') { add(e, 'est', e.est, 'r', KW('상관', 'Correlation')); P(KW('유의확률 (양측)', 'Significance (2-tailed)')); }
      else if (m === 'chisq') { add(e, 'est', e.est, "Cramer's V", KW('대칭적 측도 표의 Cramer의 V', "Cramer's V")); P(KW('카이제곱 검정 표의 Pearson 카이제곱 근사 유의확률', 'Asymptotic Significance')); }
      else if (m === 'onesample' || m === 'paired_t') { add(e, 'est', e.est, m === 'paired_t' ? '대응차 평균' : '평균 차이', KW(m === 'paired_t' ? '대응표본 검정 표의 평균' : '일표본 검정 표의 평균차이', 'Mean (Difference)')); P(KW('유의확률 (양측)', 'Sig. (2-tailed)')); }
      else if (m === 'ind_t') {
        const row = (a.tables[0]?.rows || []).find(r => e.label.startsWith(r[0] + ':')), welch = row && row[row.length - 1] === 'Welch';
        add(e, 'est', e.est, '평균 차이', KW('독립표본 검정 표의 평균차이', 'Mean Difference')); P(KW(`독립표본 검정 표 "${welch ? '등분산을 가정하지 않음' : '등분산을 가정함'}" 줄의 유의확률 (양측)`, welch ? 'Equal variances not assumed' : 'Equal variances assumed'));
      }
      else if (m === 'oneway') { add(e, 'est', e.est, 'η²', KW('ANOVA 효과크기 표의 에타 제곱 점추정 (SPSS 27 이상)', 'Eta-squared Point Estimate')); P(KW('ANOVA 표의 유의확률', 'Sig.')); }
      else if (m === 'factorial' || m === 'ancova') { add(e, 'est', e.est, '부분 η²', KW('개체-간 효과 검정 표의 부분 에타 제곱', 'Partial Eta Squared')); P(KW('개체-간 효과 검정 표의 유의확률', 'Sig.')); }
      else if (m === 'rm_anova' || m === 'mixed_anova') {
        const between = /주효과$/.test(e.label) && !/시점/.test(e.label), row = e.sph === 'gg' ? 'Greenhouse-Geisser' : '구형성 가정 (Sphericity Assumed)';
        add(e, 'est', e.est, '부분 η²', between ? KW('개체-간 효과 검정 표의 부분 에타 제곱', 'Partial Eta Squared') : KW(`개체-내 효과 검정 표 "${row}" 줄의 부분 에타 제곱`, 'Partial Eta Squared'));
        P(between ? KW('개체-간 효과 검정 표의 유의확률', 'Sig.') : KW(`개체-내 효과 검정 표 "${row}" 줄의 유의확률`, 'Sig.'));
      }
      else if (m === 'mcnemar') add(e, 'pEx', e.pEx, 'p (정확)', KW('카이제곱 검정 표의 McNemar 검정 정확한 유의확률 (양측)', 'Exact Sig. (2-sided)'), { isP: true });
      else if (m === 'cochran') { add(e, 'q', e.q, 'Q', KW('검정 통계량 표의 Cochran의 Q', "Cochran's Q")); P(KW('검정 통계량 표의 근사 유의확률', 'Asymp. Sig.')); }
      else if (m === 'regression' || m === 'hier') {
        if (/설명력 \(→|전체 설명력/.test(e.label)) { add(e, 'est', e.est, 'R²', KW('모형 요약 표의 R 제곱', 'R Square')); P(KW('ANOVA 표의 유의확률', 'Sig.')); }
        else if (/ΔR²/.test(e.label)) { add(e, 'est', e.est, 'ΔR²', KW('모형 요약 표의 R 제곱 변화량', 'R Square Change')); P(KW('모형 요약 표의 유의확률 F 변화량', 'Sig. F Change')); }
        else { add(e, 'est', e.est, 'B', KW('계수 표의 비표준화 계수 B', 'Unstandardized B')); P(KW('계수 표의 유의확률', 'Sig.')); }
      }
      else if (m === 'logistic') { add(e, 'est', e.est, 'B', KW('방정식에 포함된 변수 표의 B', 'B')); P(KW('방정식에 포함된 변수 표의 유의확률', 'Sig.')); }
      else if (/^med_|^moderation|^modmed/.test(m)) {
        if (e.ciBased) {
          const idx = /조절된 매개 지수/.test(e.label), cond = /조건부/.test(e.label), tot = /총 간접/.test(e.label);
          const where = idx ? 'Index of moderated mediation 표' : cond ? 'Conditional indirect effects 표' : 'Indirect effect(s) of X on Y 표' + (tot ? '의 TOTAL 줄' : '');
          add(e, 'est', e.est, idx ? 'Index' : 'Effect', `PROCESS ${where}의 ${idx ? 'Index' : 'Effect'}`);
          add(e, 'lo', e.lo, 'BootLLCI', `PROCESS ${where}의 BootLLCI`, { boot: true }); add(e, 'hi', e.hi, 'BootULCI', `PROCESS ${where}의 BootULCI`, { boot: true });
        } else {
          const where = /^직접효과/.test(e.label) ? 'Direct effect of X on Y 표의 Effect' : /^총효과/.test(e.label) ? 'Total effect of X on Y 표의 Effect' : /단순기울기/.test(e.label) ? 'Conditional effects of the focal predictor 표의 Effect' : '해당 결과변인(OUTCOME VARIABLE) 모형 표의 coeff';
          add(e, 'est', e.est, 'B', `PROCESS ${where}`); P('PROCESS 같은 줄의 p');
        }
      }
    });
    return out;
  }
  // 입력한 값과 비교: 적힌 자릿수만큼 반올림 차이는 같다고 봄. 부트스트랩 구간은 재표집 차이를 허용
  function spssMatch(t, raw) {
    const s = String(raw || '').trim().replace(/[\s,]/g, ''); if (!s) return null;
    if (/^<0?\.0*1$/.test(s.replace(/^p/, ''))) { if (!t.isP) return 'no'; const lim = Number(s.slice(1).replace(/^\./, '0.')); return t.v < lim ? 'ok' : 'no'; }
    const x = Number(s.replace(/^(-?)\./, '$10.').replace(/^−/, '-')); if (!isFinite(x)) return 'bad';
    const dec = (s.split('.')[1] || '').length, tol = 0.6 * 10 ** -Math.max(dec, 0) + 1e-9;
    if (t.isP && x === 0 && t.v < 0.5 * 10 ** -dec) return 'ok';
    if (Math.abs(x - t.v) <= tol) return 'ok';
    if (t.boot) { const w = Math.abs((t.eff.hi ?? 0) - (t.eff.lo ?? 0)); if (Math.abs(x - t.v) <= Math.max(tol, 0.1 * w)) return 'near'; }
    if (Math.abs(Math.abs(x) - Math.abs(t.v)) <= tol) return 'sign';
    return 'no';
  }
  const showV = (t) => t.isP ? (t.v < 0.0001 ? '< .0001' : t.v.toFixed(4).replace(/^0\./, '.')) : (Math.abs(t.v) < 1 ? t.v.toFixed(4).replace(/^(-?)0\./, '$1.') : t.v.toFixed(4));
  const SPSS_ST = { ok: ['✓', '같음'], near: ['≈', '가까움 (부트스트랩 재표집 차이)'], sign: ['±', '부호만 다름 (집단·범주 순서 확인)'], no: ['✗', '다름'], bad: ['?', '숫자를 읽지 못함'] };
  function spssHtml(a) {
    if (!a.spec || !SPSSGEN[a.method]) return '';
    const T = spssTargets(a); if (!T.length) return '';
    const vals = a.spss || {}, sts = T.map(t => spssMatch(t, vals[t.id])), done = sts.filter(Boolean), ok = sts.filter(s => s === 'ok' || s === 'near').length;
    const sum = done.length ? (ok === done.length ? `<span class="a-ok">${ok}/${T.length} 일치</span>` : `<span class="a-low">${done.length - ok}개 다름</span> · ${ok}/${T.length} 일치`) : `숫자 ${T.length}개`;
    let lastEff = '';
    return `<details class="item-detail sp-chk" data-spss-a="${a.id}"><summary>SPSS와 맞춰 보기 · ${sum}</summary>
      <p class="hint" style="padding:0 14px"><b>R·SPSS로 확인하기</b>에서 <b>analysis.sps</b>를 받아 SPSS에서 같은 분석을 돌린 뒤, 출력에 보이는 숫자를 그대로 적으세요. 적은 자릿수만큼 반올림해 비교합니다.</p>
      <div class="table-wrap"><table class="compact sp-table"><thead><tr><th>SPSS에서 찾을 곳</th><th class="num">통</th><th>SPSS 값</th><th></th></tr></thead><tbody>
      ${T.map((t, i) => { const st = sts[i], first = t.eff.label !== lastEff; lastEff = t.eff.label;
        return `${first ? `<tr class="sp-grp"><td colspan="4">${esc(t.eff.label)}</td></tr>` : ''}<tr><td><span class="sp-k">${esc(t.name)}</span> ${esc(t.where)}</td><td class="num">${showV(t)}</td><td><input class="sp-in" data-spss="${a.id}|${t.id}" value="${esc(vals[t.id] || '')}" inputmode="decimal" placeholder="예: .461"></td><td class="sp-st ${st || ''}" title="${st ? SPSS_ST[st][1] : ''}">${st ? SPSS_ST[st][0] : ''}</td></tr>`; }).join('')}
      </tbody></table></div>
      <p class="hint" style="padding:0 14px 12px">✓ 같음 · ≈ 부트스트랩이라 조금 다름(정상) · ± 부호만 다름(집단 순서가 반대) · ✗ 다름. 다르면 변인, 제외된 응답, 분석 결정이 같은지 먼저 확인하세요.</p></details>`;
  }
  document.addEventListener('input', e => {
    const t = e.target; if (!t.matches || !t.matches('.sp-in')) return;
    const [aid, tid] = t.dataset.spss.split('|'), a = analyses.find(x => x.id === aid); if (!a) return;
    a.spss = a.spss || {}; a.spss[tid] = t.value; saveAnalyses();
    const tgt = spssTargets(a).find(x => x.id === tid), st = spssMatch(tgt, t.value), cell = t.closest('tr').querySelector('.sp-st');
    cell.className = 'sp-st ' + (st || ''); cell.textContent = st ? SPSS_ST[st][0] : ''; cell.title = st ? SPSS_ST[st][1] : '';
    const box = t.closest('.sp-chk'), T = spssTargets(a), sts = T.map(x => spssMatch(x, a.spss[x.id])), done = sts.filter(Boolean), ok = sts.filter(s => s === 'ok' || s === 'near').length;
    box.querySelector('summary').innerHTML = `SPSS와 맞춰 보기 · ${done.length ? (ok === done.length ? `<span class="a-ok">${ok}/${T.length} 일치</span>` : `<span class="a-low">${done.length - ok}개 다름</span> · ${ok}/${T.length} 일치`) : `숫자 ${T.length}개`}`;
  });


// ##### 10. 표본 수 계산 (G*Power 방식): 분포 함수 (stats-ladder.html 10084~10181줄) #####
  // ---------- 표본 수 계산: 비중심 분포 (G*Power와 같은 방식) ----------
  const PW = (() => {
    const { pF, pChi, zInv } = ST;
    const lgam = x => { // Lanczos (ST 안의 것과 같은 식)
      const g = [0.99999999999980993, 676.5203681218851, -1259.1392167224028, 771.32342877765313, -176.61502916214059, 12.507343278686905, -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7];
      if (x < 0.5) return Math.log(Math.PI / Math.sin(Math.PI * x)) - lgam(1 - x);
      x -= 1; let a = g[0]; const t = x + 7.5; for (let i = 1; i < 9; i++) a += g[i] / (x + i);
      return 0.5 * Math.log(2 * Math.PI) + (x + 0.5) * Math.log(t) - t + Math.log(a);
    };
    const bisect = (f, lo, hi) => { for (let i = 0; i < 100; i++) { const m = (lo + hi) / 2; if (f(m)) hi = m; else lo = m; } return (lo + hi) / 2; };
    const qF = (a, d1, d2) => bisect(x => pF(x, d1, d2) <= a, 0, 1e4);         // 위쪽 a 지점
    const qChi = (a, df) => bisect(x => pChi(x, df) <= a, 0, 1e4);
    // 푸아송 가중치를 곱해 더함: 가운데(λ/2)에서 양쪽으로
    function poisMix(lam, term) {
      const h = lam / 2; if (h < 1e-12) return term(0);
      const lw = j => -h + j * Math.log(h) - lgam(j + 1);
      const j0 = Math.floor(h); let s = 0;
      for (let j = j0; j < j0 + 2000; j++) { const w = Math.exp(lw(j)); s += w * term(j); if (w < 1e-14 && j > j0 + 5) break; }
      for (let j = j0 - 1; j >= 0; j--) { const w = Math.exp(lw(j)); s += w * term(j); if (w < 1e-14) break; }
      return s;
    }
    // 위쪽 꼬리 확률 = 검정력
    const powF = (lam, d1, d2, a) => { const c = qF(a, d1, d2); return poisMix(lam, j => pF(c * d1 / (d1 + 2 * j), d1 + 2 * j, d2)); };
    const powChi = (lam, df, a) => { const c = qChi(a, df); return poisMix(lam, j => pChi(c, df + 2 * j)); };
    // 목표 검정력을 넘는 가장 작은 정수 n
    function minN(pow, target, lo, hi = 100000) {
      let a = lo, b = lo; while (b < hi && !(pow(b) >= target)) { a = b; b = Math.max(b + 1, Math.ceil(b * 1.6)); }
      if (b >= hi && !(pow(hi) >= target)) return null;
      while (b - a > 1) { const m = Math.floor((a + b) / 2); if (pow(m) >= target) b = m; else a = m; }
      return pow(a) >= target ? a : b;
    }
    const z2 = (a, pw) => (zInv(a) + zInv(2 * (1 - pw))) ** 2;
    return {
      // 집단 하나(단일표본)·대응표본: N = 사람 수
      t1: (d, a, pw) => minN(n => powF(n * d * d, 1, n - 1, a), pw, 3),
      // 독립표본 (두 집단 같은 크기): 집단당 n, 합계 2n
      t2: (d, a, pw) => { const n = minN(n => powF(d * d * n / 2, 1, 2 * n - 2, a), pw, 3); return n && { N: 2 * n, per: n }; },
      // 분산분석·회귀·공분산분석 등 F 검정: λ = f²·N
      F: (f2, d1, extra, a, pw) => minN(N => powF(f2 * N, d1, N - extra, a), pw, extra + 2),
      // 반복측정 (집단 안): λ = f²·N·m/(1-ρ), 구형성 가정(ε = 1)
      rm: (f, m, rho, a, pw) => minN(N => powF(f * f * N * m / (1 - rho), m - 1, (N - 1) * (m - 1), a), pw, 3),
      // 혼합설계의 집단 × 시점 상호작용
      mixed: (f, k, m, rho, a, pw) => minN(N => powF(f * f * N * m / (1 - rho), (k - 1) * (m - 1), (N - k) * (m - 1), a), pw, k + 2),
      chi: (w, df, a, pw) => minN(N => powChi(w * w * N, df, a), pw, 5),
      // 상관: 피셔 z 근사, 통제변인 c개면 c를 더함
      corr: (r, c, a, pw) => Math.ceil(z2(a, pw) / (0.5 * Math.log((1 + r) / (1 - r))) ** 2 + 3 + c),
      // 이항 로지스틱: Hsieh, Bloch, & Larsen (1998), 연속 예측변인 1 SD당 OR
      logit: (or, P, a, pw) => Math.ceil(z2(a, pw) / (P * (1 - P) * Math.log(or) ** 2)),
      powF, powChi, minN
    };
  })();

  // 매개효과: 몬테카를로 모의실험 (경로마다 유의한지 = 결합 유의성 검정, Fritz & MacKinnon, 2007)
  // paths: 표준화 경로계수 [a, b] (단순매개) 또는 [a1, d21, b2] (순차매개). 직접효과는 0으로 둠
  function medPower(paths, N, a, reps, seed) {
    const r = rng(seed), gauss = () => Math.sqrt(-2 * Math.log(r() || 1e-12)) * Math.cos(2 * Math.PI * r());
    const k = paths.length;
    const tcs = paths.map((_, j) => ST.tInv(a, N - (j + 1) - 1));   // j+1개 예측변인 식의 기준 t
    const cols = Array.from({ length: k + 1 }, () => new Float64Array(N));
    let hit = 0;
    for (let rep = 0; rep < reps; rep++) {
      for (let i = 0; i < N; i++) {
        cols[0][i] = gauss();
        for (let j = 1; j <= k; j++) { const p = paths[j - 1]; cols[j][i] = p * cols[j - 1][i] + Math.sqrt(1 - p * p) * gauss(); }
      }
      let ok = true;
      for (let j = 1; j <= k && ok; j++) {       // cols[j] ~ 1 + cols[0..j-1], 볼 계수 = cols[j-1]
        const p = j + 1, A = Array.from({ length: p }, () => new Array(p).fill(0)), v = new Array(p).fill(0);
        const y = cols[j];
        for (let i = 0; i < N; i++) {
          for (let u = 0; u < p; u++) {
            const xu = u === 0 ? 1 : cols[u - 1][i]; v[u] += xu * y[i];
            for (let w = u; w < p; w++) A[u][w] += xu * (w === 0 ? 1 : cols[w - 1][i]);
          }
        }
        for (let u = 0; u < p; u++) for (let w = 0; w < u; w++) A[u][w] = A[w][u];
        const iv = matInv(A); if (!iv) { ok = false; break; }
        const b = iv.map(row => row.reduce((s, x, q) => s + x * v[q], 0));
        let sse = 0;
        for (let i = 0; i < N; i++) { let yh = b[0]; for (let q = 1; q < p; q++) yh += b[q] * cols[q - 1][i]; sse += (y[i] - yh) ** 2; }
        const se = Math.sqrt(sse / (N - p) * iv[p - 1][p - 1]);
        if (!(Math.abs(b[p - 1] / se) > tcs[j - 1])) ok = false;
      }
      if (ok) hit++;
    }
    return hit / reps;
  }
  // 목표 검정력을 넘는 가장 작은 N (같은 씨앗으로 N마다 다시 뽑아 비교가 흔들리지 않게)
  async function medMinN(paths, a, target, reps = 1000, onProg) {
    const P = n => medPower(paths, n, a, reps, 20261020);
    let lo = 10, hi = 10, steps = 0;
    while (hi < 5000 && !(P(hi) >= target)) { lo = hi; hi = Math.ceil(hi * 1.6); onProg && onProg(++steps); await tick(); }
    if (hi >= 5000) return null;
    while (hi - lo > 1) { const m = Math.floor((lo + hi) / 2); if (P(m) >= target) hi = m; else lo = m; onProg && onProg(++steps); await tick(); }
    return hi;
  }

  // =================================================================

// ##### 11. 표본 수 계산: 화면이 넘기는 자유도·비중심 모수 (stats-ladder.html 10257~10438줄) #####
  // ---------- 표본 수 계산 ----------
  const ESZ = {
    d: { lab: 'd', v: [0.2, 0.5, 0.8] }, f: { lab: 'f', v: [0.1, 0.25, 0.4] }, f2: { lab: 'f²', v: [0.02, 0.15, 0.35] }, r: { lab: 'r', v: [0.1, 0.3, 0.5] },
    w: { lab: 'w', v: [0.1, 0.3, 0.5] }, path: { lab: '경로 β', v: [0.14, 0.39, 0.59] }, or: { lab: 'OR', v: [1.68, 3.47, 6.71] }
  };
  const SIZE_W = { s: '작음', m: '중간', l: '큼', c: '직접 입력' };
  const PFAM = {
    r_onesample_t: 't1', r_paired_t: 't1', r_ind_t: 't2', r_pointbiserial: 't2', r_oneway: 'anova', r_rel_anova: 'anova', r_factorial_anova: 'factorial',
    r_ancova: 'ancova', r_rm_anova: 'rm', r_rm_factorial: 'rm', r_mixed_anova: 'mixed', r_chisq: 'chi', r_pearson: 'corr', r_spearman: 'corr', r_partial: 'corr',
    r_simple_reg: 'reg', r_multiple_reg: 'reg', r_hier_reg: 'hier', r_logistic: 'logit', r_multinom: 'epv', r_ordinal: 'epv', r_moderation: 'inter',
    r_med_simple: 'med', r_med_parallel: 'med', r_med_serial: 'med3', r_modmed_a: 'modmed', r_modmed_b: 'modmed', r_sem: 'med', r_sem_serial: 'med3'
  };
  const FAM_ES = { t1: 'd', t2: 'd', anova: 'f', factorial: 'f', ancova: 'f', rm: 'f', mixed: 'f', chi: 'w', corr: 'r', reg: 'f2', hier: 'f2', inter: 'f2', logit: 'or', med: 'path', med3: 'path', modmed: 'path' };
  const levelsOf = name => { const iv = vars.ivs.find(x => sameName(x.name, name)); const n = iv && iv.levels ? iv.levels.split(/[,，]/).filter(s => norm(s)).length : 0; return n >= 2 ? n : 0; };
  // 가설마다 처음 값 (변인 수에서 짐작)
  function pwDefaults(p) {
    const fam = PFAM[p.result], v = itemVars(p), c = md.controls.length;
    const o = { size: fam === 'inter' ? 's' : 'm' };
    if (FAM_ES[fam]) o.es = ESZ[FAM_ES[fam]].v[o.size === 's' ? 0 : 1];
    if (fam === 'anova' || fam === 'ancova') o.k = levelsOf(v.X[0]) || 3;
    if (fam === 'ancova') o.c = Math.max(1, c);
    if (fam === 'factorial') { o.la = levelsOf(v.X[0]) || 2; o.lb = levelsOf(v.X[1]) || 2; }
    if (fam === 'rm') { o.m = 3; o.rho = 0.5; }
    if (fam === 'mixed') { o.k = levelsOf(v.X[0]) || 2; o.m = 2; o.rho = 0.5; }
    if (fam === 'chi') o.df = Math.max(1, (levelsOf(v.X[0]) || 2) - 1);
    if (fam === 'corr') o.c = p.result === 'r_partial' ? Math.max(1, c) : 0;
    if (fam === 'reg') o.k = Math.max(1, v.X.length) + c;
    if (fam === 'hier') { o.q = Math.max(1, v.X.length); o.k = o.q + Math.max(1, c); }
    if (fam === 'inter') o.k = 3 + c;
    if (fam === 'logit') { o.P = 0.5; o.k = Math.max(1, v.X.length) + c; }
    if (fam === 'epv') { o.P = 0.2; o.k = Math.max(1, v.X.length) + c; }
    if (fam === 'modmed') o.k = 4 + c;
    if (fam === 'med' || fam === 'med3' || fam === 'modmed') o.km = 1 + Math.max(1, v.M.length) + c;   // 가장 큰 회귀식의 예측변인 수
    return o;
  }
  const pwOf = p => { const d = pwDefaults(p), s = md.pw.items[p.no] || {}; return { ...d, ...s }; };
  const mcKey = (paths) => `${paths.join(',')}|${md.pw.a}|${md.pw.pw}`;
  let mcBusy = null;
  async function runMc(paths) {
    const key = mcKey(paths); if (md.mc[key] != null || mcBusy) return;
    mcBusy = key; const bar = $('#pw-busy'); if (bar) bar.textContent = '매개효과 모의실험 중… (몇 초 걸려요)';
    try { md.mc[key] = await medMinN(paths, md.pw.a, md.pw.pw, 1000) || -1; saveMd(); }
    finally { mcBusy = null; }
    if (!$('#view-power').hidden) renderPower();
    if (curView() === 'ladder' && hp && hp.cards && hp.cards.some(c => c.altOpen)) render();
  }
  const fmtEs = (k, x) => k === 'or' ? (+x).toFixed(2) : (+x).toFixed(2).replace(/^0\./, '.');
  // 가설 하나의 필요 인원
  function pwCalc(p) {
    const fam = PFAM[p.result], o = pwOf(p), a = +md.pw.a, pw = +md.pw.pw, es = +o.es;
    const name = RULES.results[p.result].name;
    if (!fam) return { none: true, note: p.result === 'r_multilevel' ? '다층모형은 정해진 공식이 없어요. 흔히 집단 30개 × 집단당 30명(Kreft, 1996)을 기준으로 삼고, 정확히는 모의실험으로 정합니다.' : '이 기법은 통에 계산기가 없어요. G*Power(무료)의 같은 이름 메뉴로 계산하세요.' };
    const ek0 = FAM_ES[fam]; if ((ek0 && !(es > 0)) || ((ek0 === 'r' || ek0 === 'path') && es >= 1)) return { err: '효과크기를 0보다 큰 값으로 넣어 주세요.' };
    const esTxt = FAM_ES[fam] ? `${ESZ[FAM_ES[fam]].lab} = ${fmtEs(FAM_ES[fam], es)}(${SIZE_W[o.size]})` : '';
    const cond = `유의수준 ${a.toFixed(2).replace(/^0\./, '.')}, 검정력 ${pw.toFixed(2).replace(/^0\./, '.')}`;
    const roundTo = (n, g) => Math.ceil(n / g) * g;
    let N = null, per = null, how = '', cite = 'Cohen, 1988', gp = true, green = null;
    switch (fam) {
      case 't1': N = PW.t1(es, a, pw); how = p.result === 'r_paired_t' ? '대응표본 t검정 (두 번 측정한 사람 수)' : '단일표본 t검정'; break;
      case 't2': { const r = PW.t2(es, a, pw); if (r) { N = r.N; per = r.per; } how = '두 집단 t검정 (집단 크기 같음)'; break; }
      case 'anova': N = PW.F(es * es, o.k - 1, +o.k, a, pw); if (N) { N = roundTo(N, +o.k); per = N / o.k; } how = `${o.k}집단 일원분산분석`; break;
      case 'factorial': { const cells = o.la * o.lb; N = PW.F(es * es, (o.la - 1) * (o.lb - 1), cells, a, pw); if (N) { N = roundTo(N, cells); per = N / cells; } how = `${o.la} × ${o.lb} 분산분석의 상호작용`; break; }
      case 'ancova': N = PW.F(es * es, o.k - 1, +o.k + +o.c, a, pw); if (N) { N = roundTo(N, +o.k); per = N / o.k; } how = `${o.k}집단 공분산분석 (공변인 ${o.c}개)`; break;
      case 'rm': N = PW.rm(es, +o.m, +o.rho, a, pw); how = `반복측정 ${o.m}번 (측정 간 상관 ${String(o.rho).replace(/^0\./, '.')}, 구형성 가정)`; break;
      case 'mixed': N = PW.mixed(es, +o.k, +o.m, +o.rho, a, pw); if (N) { N = roundTo(N, +o.k); per = N / o.k; } how = `${o.k}집단 × ${o.m}시점 상호작용 (측정 간 상관 ${String(o.rho).replace(/^0\./, '.')})`; break;
      case 'chi': N = PW.chi(es, +o.df, a, pw); how = `카이제곱 검정 (자유도 ${o.df})`; break;
      case 'corr': N = PW.corr(es, +o.c, a, pw); if (p.result === 'r_spearman') N = Math.ceil(N * (1 + es * es / 2)); how = p.result === 'r_partial' ? `편상관 (통제 ${o.c}개, 피셔 z 근사)` : p.result === 'r_spearman' ? '스피어만 상관 (피셔 z 근사 × Bonett & Wright, 2000 보정)' : '상관 (피셔 z 근사)'; gp = false; break;
      case 'reg': N = PW.F(es, +o.k, +o.k + 1, a, pw); how = `회귀 전체 모형 (예측변인 ${o.k}개)`; green = +o.k; break;
      case 'hier': N = PW.F(es, +o.q, +o.k + 1, a, pw); how = `설명력 증가량 ΔR² (더 넣는 변인 ${o.q}개, 전체 ${o.k}개)`; green = +o.k; break;
      case 'inter': N = PW.F(es, 1, +o.k + 1, a, pw); how = `상호작용항 하나의 ΔR² (전체 예측변인 ${o.k}개)`; green = +o.k; break;
      case 'logit': { const h = PW.logit(es, +o.P, a, pw), epv = Math.ceil(10 * o.k / Math.min(o.P, 1 - o.P)); N = Math.max(h, epv); how = `로지스틱 (1 SD당 OR, '예' 비율 ${Math.round(o.P * 100)}%) · Hsieh 등 공식 ${h}명, 변인당 사건 10개 기준 ${epv}명 중 큰 값`; cite = 'Hsieh et al., 1998; Peduzzi et al., 1996'; gp = false; break; }
      case 'epv': N = Math.ceil(10 * o.k / o.P); how = `가장 적은 범주 비율 ${Math.round(o.P * 100)}%, 예측변인 ${o.k}개 · 변인당 사건 10개 기준`; cite = 'Peduzzi et al., 1996'; gp = false; break;
      case 'med': case 'med3': case 'modmed': {
        const paths = fam === 'med3' ? [es, es, es] : [es, es];
        const got = md.mc[mcKey(paths)];
        if (got == null) { runMc(paths); return { pending: true, how: '모의실험 중…' }; }
        N = got > 0 ? got : null; cite = 'Fritz & MacKinnon, 2007'; gp = false; green = +o.km;
        how = `${fam === 'med3' ? '순차매개 (경로 3개 모두 유의)' : '간접효과 (경로 a, b 모두 유의)'} · 몬테카를로 모의실험 1,000번`;
        if (fam === 'modmed') { const ni = PW.F(0.02, 1, +o.k + 1, a, pw); if (ni > N) { N = ni; how += ` · 조절 상호작용(f² = .02) ${ni}명이 더 커서 그 값`; } else how += ` · 조절 상호작용(f² = .02) ${ni}명`; }
        break;
      }
    }
    if (!N) return { err: '이 조건으로는 계산할 수 없어요. 값을 확인해 주세요.' };
    return { N, per, how, cite, gp, esTxt, cond, name, green: green ? { all: 50 + 8 * green, each: 104 + green, k: green } : null };
  }
  function pwSummary() {
    const items = planItems().map(p => ({ p, r: pwCalc(p) }));
    const ok = items.filter(x => x.r.N);
    const max = ok.slice().sort((a, b) => b.r.N - a.r.N)[0] || null;
    const N = max ? max.r.N : null, recruit = N ? Math.ceil(N / (1 - md.pw.attr / 100)) : null;
    return { items, max, N, recruit, pending: items.some(x => x.r.pending) };
  }
  function pwSentence(S) {
    if (!S.max) return '';
    const r = S.max.r, fam = PFAM[S.max.p.result];
    const head = fam === 'med' || fam === 'med3' || fam === 'modmed'
      ? `Fritz와 MacKinnon(2007)의 방식으로 몬테카를로 모의실험(1,000회)을 한 결과, ${r.name}에서 경로계수 ${r.esTxt}, ${r.cond}에서 간접효과를 확인하려면 최소 ${r.N}명이 필요하였다`
      : fam === 'logit' ? `Hsieh 등(1998)의 공식과 변인당 사건 10개 기준(Peduzzi et al., 1996)에 따라 ${r.name}에 최소 ${r.N}명이 필요하였다`
      : fam === 'epv' ? `변인당 사건 10개 기준(Peduzzi et al., 1996)에 따라 ${r.name}에 최소 ${r.N}명이 필요하였다`
      : `${r.gp ? 'G*Power 3.1과 같은 방식의 ' : ''}검정력 분석(Cohen, 1988) 결과, ${r.name}에서 효과크기 ${r.esTxt}, ${r.cond}에서 최소 ${r.N}명이 필요하였다`;
    return `${head}. 탈락과 불성실 응답 ${md.pw.attr}%를 고려하여 ${S.recruit}명을 모집 목표로 하였다.`;
  }
  function renderPower() {
    const box = $('#pw-main'); if (!box) return;
    const items = planItems();
    if (!items.length) { box.innerHTML = `<div class="card"><h3 class="card-title">먼저 통계 기법을 정해 주세요</h3><p class="hint">가설마다 기법이 정해지면, 그 기법에 맞는 공식으로 필요한 인원을 계산합니다.</p><div class="actions"><button class="btn primary" data-nav-to="ladder">통계 기법 고르기로</button></div></div>`; return; }
    const S = pwSummary();
    const sel = (k, opts, cur) => `<select data-pw-g="${k}">${opts.map(([v, l]) => `<option value="${v}" ${+cur === +v ? 'selected' : ''}>${l}</option>`).join('')}</select>`;
    const num = (no, k, val, label, step = 1, min = 1) => `<label class="f pw-f">${label}<input type="number" data-pw="${no}.${k}" value="${esc(val)}" step="${step}" min="${min}"></label>`;
    const card = ({ p, r }) => {
      const fam = PFAM[p.result], o = pwOf(p), ek = FAM_ES[fam];
      const esCtl = ek ? `<label class="f pw-f">효과크기 ${ESZ[ek].lab}<span class="pw-es"><select data-pw-size="${p.no}">${['s', 'm', 'l', 'c'].map((s, i) => `<option value="${s}" ${o.size === s ? 'selected' : ''}>${SIZE_W[s]}${s !== 'c' ? ` (${fmtEs(ek, ESZ[ek].v[i])})` : ''}</option>`).join('')}</select><input type="number" data-pw="${p.no}.es" value="${esc(o.es)}" step="${ek === 'or' ? 0.01 : 0.01}" min="0"></span></label>` : '';
      const extra = {
        anova: () => num(p.no, 'k', o.k, '집단 수', 1, 2), ancova: () => num(p.no, 'k', o.k, '집단 수', 1, 2) + num(p.no, 'c', o.c, '공변인 수', 1, 1),
        factorial: () => num(p.no, 'la', o.la, '기준 1 수준 수', 1, 2) + num(p.no, 'lb', o.lb, '기준 2 수준 수', 1, 2),
        rm: () => num(p.no, 'm', o.m, '측정 횟수', 1, 2) + num(p.no, 'rho', o.rho, '측정 간 상관', 0.1, 0),
        mixed: () => num(p.no, 'k', o.k, '집단 수', 1, 2) + num(p.no, 'm', o.m, '측정 횟수', 1, 2) + num(p.no, 'rho', o.rho, '측정 간 상관', 0.1, 0),
        chi: () => num(p.no, 'df', o.df, '자유도 (행−1)×(열−1)', 1, 1), corr: () => p.result === 'r_partial' ? num(p.no, 'c', o.c, '통제변인 수', 1, 1) : '',
        reg: () => num(p.no, 'k', o.k, '예측변인 수 (통제 포함)', 1, 1), hier: () => num(p.no, 'q', o.q, '더 넣는 변인 수', 1, 1) + num(p.no, 'k', o.k, '전체 예측변인 수', 1, 1),
        inter: () => num(p.no, 'k', o.k, '전체 예측변인 수 (X, W, X×W, 통제)', 1, 3),
        logit: () => num(p.no, 'P', o.P, "'예' 비율 (0~1)", 0.05, 0.05) + num(p.no, 'k', o.k, '예측변인 수', 1, 1),
        epv: () => num(p.no, 'P', o.P, '가장 적은 범주 비율', 0.05, 0.05) + num(p.no, 'k', o.k, '예측변인 수', 1, 1),
        med: () => num(p.no, 'km', o.km, '가장 큰 회귀식 예측변인 수', 1, 2), med3: () => num(p.no, 'km', o.km, '가장 큰 회귀식 예측변인 수', 1, 3),
        modmed: () => num(p.no, 'k', o.k, '조절 식 예측변인 수', 1, 4)
      }[fam];
      const res = r.none ? `<p class="hint pw-none">${esc(r.note)}</p>` : r.err ? `<p class="a-low">${esc(r.err)}</p>` : r.pending ? '<p class="hint">모의실험 중…</p>'
        : `<div class="pw-n"><b>${r.N}</b><span>명${r.per ? ` (집단당 ${r.per}명)` : ''}</span></div><p class="hint pw-how">${esc(r.how)} · ${esc(r.cite)}</p>
          ${r.green ? `<p class="hint pw-how">참고 · 경험 법칙(Green, 1991): 전체 모형 N ≥ 50 + 8×${r.green.k} = ${r.green.all}, 개별 계수 N ≥ 104 + ${r.green.k} = ${r.green.each}</p>` : ''}`;
      return `<div class="pw-card ${S.max && S.max.p === p ? 'max' : ''}">
        <div class="pw-h"><span class="hi-no">${hLabel(p)}</span><b>${esc(RULES.results[p.result].name)}</b>${S.max && S.max.p === p ? '<span class="pw-badge">가장 큰 기준</span>' : ''}</div>
        ${p.text ? `<p class="pw-t">${esc(p.text)}</p>` : ''}
        ${esCtl || extra ? `<div class="pw-row">${esCtl}${extra ? extra() : ''}</div>` : ''}${res}</div>`;
    };
    box.innerHTML = `<div class="card">
        <h3 class="card-title">공통 조건</h3>
        <div class="pw-row">
          <label class="f pw-f">유의수준 α${sel('a', [[0.05, '.05'], [0.01, '.01']], md.pw.a)}</label>
          <label class="f pw-f">검정력 (1−β)${sel('pw', [[0.8, '.80'], [0.9, '.90'], [0.95, '.95']], md.pw.pw)}</label>
          <label class="f pw-f">예상 탈락·불성실${sel('attr', [[0, '0%'], [10, '10%'], [15, '15%'], [20, '20%'], [30, '30%']], md.pw.attr)}</label>
        </div>
        <p class="hint">효과크기는 <b>선행연구의 값</b>을 '직접 입력'으로 넣는 것이 가장 좋습니다. 없으면 보통 '중간'을 씁니다(Cohen, 1988). 조절효과(상호작용)는 대개 작아서 처음 값을 '작음'으로 두었습니다.</p>
      </div>
      <div class="card">
        <h3 class="card-title">가설별 필요 인원</h3>
        <div class="pw-list">${S.items.map(card).join('')}</div>
        <p class="hint" id="pw-busy">${S.pending ? '매개효과 모의실험 중… (몇 초 걸려요)' : ''}</p>
      </div>
      ${S.N ? `<div class="card pw-sum">
        <div class="pw-big"><div><span>최소 분석 인원 (가장 큰 기준)</span><b>${S.N}<small>명</small></b></div><div class="rec"><span>모집 목표 (탈락 ${md.pw.attr}% 감안)</span><b>${S.recruit}<small>명</small></b></div></div>
        <div class="section-title">연구계획서에 쓸 문장</div>
        <p class="pw-sent" id="pw-sent">${esc(pwSentence(S))}</p>
        <div class="actions tight"><button class="btn sm" data-pw-act="copy">문장 복사</button></div>
        <p class="hint">t검정·분산분석·회귀·카이제곱은 G*Power 3.1(무료)에 같은 조건을 넣으면 같은 값이 나옵니다. 상관은 근사식이라 1명 안팎 다를 수 있고, 매개효과는 모의실험이라 몇 명 오차가 있습니다.</p>
      </div>` : ''}`;
    syncPw(S);
  }
  // 계산 결과 보관 (모델 검토·계획서에 씀)
  function syncPw(S) {
    const keep = S.N ? { at: koNow(), N: S.N, recruit: S.recruit, attr: md.pw.attr, sentence: pwSentence(S), rows: S.items.filter(x => x.r.N).map(x => ({ no: x.p.no, method: x.r.name, N: x.r.N, how: x.r.how, es: x.r.esTxt, cite: x.r.cite })) } : null;
    const sig = o => o && JSON.stringify([o.N, o.recruit, o.rows]);
    if (!S.pending && sig(keep) !== sig(md.pwResult)) { md.pwResult = keep; saveMd(); renderNav(); }
  }
  $('#view-power').addEventListener('change', e => {
    const t = e.target;
    if (t.dataset.pwG) { md.pw[t.dataset.pwG] = +t.value; saveMd(); renderPower(); return; }
    if (t.dataset.pwSize) {
      const no = t.dataset.pwSize, p = planItems().find(x => String(x.no) === no), ek = FAM_ES[PFAM[p.result]];
      const s = md.pw.items[no] = md.pw.items[no] || {}; s.size = t.value; if (t.value !== 'c') s.es = ESZ[ek].v[{ s: 0, m: 1, l: 2 }[t.value]];
      saveMd(); renderPower(); return;
    }
    if (t.dataset.pw) {
      const [no, k] = t.dataset.pw.split('.'); const s = md.pw.items[no] = md.pw.items[no] || {};
      s[k] = +t.value; if (k === 'es') s.size = 'c';
      saveMd(); renderPower();
    }
  });
  $('#view-power').addEventListener('click', e => {
    const b = e.target.closest('[data-pw-act]'); if (!b) return;
    const t = $('#pw-sent')?.textContent || '';
    try { navigator.clipboard.writeText(t).then(() => toast('문장을 복사했습니다.'), () => showSheet('표본수_문장.txt', t)); } catch (err) { showSheet('표본수_문장.txt', t); }
  });


// ##### 12. 탐색적 요인분석 (PAF·PCA, 베리맥스) · 확인적 요인분석 (webR lavaan, CR·AVE·HTMT) (stats-ladder.html 10691~11123줄) #####
  // ---------- 탐색적 요인분석: 주축요인추출(PAF) 또는 주성분 추출 + 베리맥스 회전 ----------
  // PAF는 R psych::fa(fm = "pa")·SPSS '주축 요인 추출'과 같은 반복 방식, 주성분은 R의 eigen과 같음. 회전은 R의 varimax(정규화)와 같음
  function jacobiEig(A) {
    const n = A.length, a = A.map(r => r.slice()), V = a.map((_, i) => a.map((_, j) => (i === j ? 1 : 0)));
    for (let sweep = 0; sweep < 100; sweep++) {
      let off = 0; for (let p = 0; p < n; p++) for (let q = p + 1; q < n; q++) off += a[p][q] * a[p][q];
      if (off < 1e-22) break;
      for (let p = 0; p < n; p++) for (let q = p + 1; q < n; q++) {
        if (Math.abs(a[p][q]) < 1e-300) continue;
        const th = (a[q][q] - a[p][p]) / (2 * a[p][q]), t = Math.sign(th || 1) / (Math.abs(th) + Math.sqrt(th * th + 1)), c = 1 / Math.sqrt(t * t + 1), s = t * c;
        for (let k = 0; k < n; k++) { const akp = a[k][p], akq = a[k][q]; a[k][p] = c * akp - s * akq; a[k][q] = s * akp + c * akq; }
        for (let k = 0; k < n; k++) { const apk = a[p][k], aqk = a[q][k]; a[p][k] = c * apk - s * aqk; a[q][k] = s * apk + c * aqk; }
        for (let k = 0; k < n; k++) { const vkp = V[k][p], vkq = V[k][q]; V[k][p] = c * vkp - s * vkq; V[k][q] = s * vkp + c * vkq; }
      }
    }
    const idx = a.map((_, i) => i).sort((i, j) => a[j][j] - a[i][i]);
    return { values: idx.map(i => a[i][i]), vectors: V.map(r => idx.map(i => r[i])) };
  }
  const mmul = (A, B) => A.map(r => B[0].map((_, j) => r.reduce((s, v, k) => s + v * B[k][j], 0)));
  const tr = A => A[0].map((_, j) => A.map(r => r[j]));
  function varimax(L, eps = 1e-5) {
    const p = L.length, nc = L[0].length; if (nc < 2) return L.map(r => r.slice());
    const sc = L.map(r => Math.sqrt(r.reduce((s, v) => s + v * v, 0)) || 1);
    const x = L.map((r, i) => r.map(v => v / sc[i]));
    let T = x[0].map((_, i) => x[0].map((_, j) => (i === j ? 1 : 0))), d = 0;
    for (let it = 0; it < 1000; it++) {
      const z = mmul(x, T), cs = z[0].map((_, j) => z.reduce((s, r) => s + r[j] * r[j], 0));
      const B = mmul(tr(x), z.map(r => r.map((v, j) => v * v * v - v * cs[j] / p)));
      // 극분해: B = U S V' 이면 U V' = B (B'B)^(-1/2)
      const e = jacobiEig(mmul(tr(B), B));
      const inv = e.vectors.map(r => r.map((v, j) => v / Math.sqrt(Math.max(e.values[j], 1e-300))));
      T = mmul(B, mmul(inv, tr(e.vectors)));
      const dp = d; d = e.values.reduce((s, v) => s + Math.sqrt(Math.max(v, 0)), 0);
      if (d < dp * (1 + eps)) break;
    }
    return mmul(x, T).map((r, i) => r.map(v => v * sc[i]));
  }
  // cols: 문항별 값 배열(같은 사람 순서, 모두 응답한 사람), k: 요인 수 (null이면 고유값 1 이상)
  function efa(cols, k, ex = 'paf') {
    const p = cols.length, n = cols[0].length;
    const R = cols.map((a, i) => cols.map((b, j) => (i === j ? 1 : corr(a, b))));
    const e = jacobiEig(R);
    const nEig1 = e.values.filter(v => v > 1).length;
    const nf = Math.max(1, Math.min(k || nEig1, p - 1));
    const load = (ev, m) => ev.vectors.map(r => r.slice(0, m).map((v, j) => v * Math.sqrt(Math.max(ev.values[j], 0))));
    let L, h2 = null, h2init = null, iter = 0, conv = true;
    if (ex === 'paf') {
      // 처음 공통성 = 다중상관제곱(SMC), 공통성이 거의 바뀌지 않을 때까지 반복
      const Ri0 = matInv(R);
      h2init = Ri0 ? Ri0.map((r, i) => Math.min(1, Math.max(0, 1 - 1 / r[i]))) : R.map((r, i) => Math.max(...r.filter((_, j) => j !== i).map(Math.abs)));
      h2 = h2init.slice(); conv = false;
      for (iter = 1; iter <= 50; iter++) {
        const Rr = R.map((r, i) => r.map((v, j) => (i === j ? h2[i] : v)));
        L = load(jacobiEig(Rr), nf);
        const nh = L.map(r => r.reduce((s, v) => s + v * v, 0));
        const diff = Math.abs(nh.reduce((s, v) => s + v, 0) - h2.reduce((s, v) => s + v, 0));
        h2 = nh;
        if (diff < 0.001) { conv = true; break; }
      }
    } else L = load(e, nf);
    L = varimax(L);
    // 요인을 설명량 순으로, 큰 부하량이 양수가 되게
    const ss = L[0].map((_, j) => L.reduce((s, r) => s + r[j] * r[j], 0));
    const ord = ss.map((_, j) => j).sort((a, b) => ss[b] - ss[a]);
    L = L.map(r => ord.map(j => r[j]));
    L[0].forEach((_, j) => { const big = L.reduce((m, r) => (Math.abs(r[j]) > Math.abs(m) ? r[j] : m), 0); if (big < 0) L.forEach(r => { r[j] = -r[j]; }); });
    const ssO = ord.map(j => ss[j]);
    // KMO, Bartlett
    const Ri = matInv(R);
    let r2 = 0, q2 = 0;
    if (Ri) for (let i = 0; i < p; i++) for (let j = 0; j < p; j++) if (i !== j) { r2 += R[i][j] ** 2; q2 += (Ri[i][j] / Math.sqrt(Ri[i][i] * Ri[j][j])) ** 2; }
    const logDet = e.values.reduce((s, v) => s + Math.log(Math.max(v, 1e-300)), 0);
    const chi = -((n - 1) - (2 * p + 5) / 6) * logDet, df = p * (p - 1) / 2;
    const comm = L.map(r => r.reduce((s, v) => s + v * v, 0));
    return { n, p, nf, nEig1, ex, iter, conv, h2init, comm, heywood: comm.some(v => v >= 0.999), eig: e.values, L, ss: ssO, pct: ssO.map(v => v / p * 100), first: e.values[0] / p * 100, kmo: Ri ? r2 / (r2 + q2) : NaN, bart: { chi, df, p: ST.pChi(chi, df) } };
  }

  // =================================================================
  // 확인적 요인분석 (CFA): 브라우저 속 R(webR)에서 lavaan으로 계산. 응답 자료는 브라우저 밖으로 나가지 않음
  // (R과 lavaan 프로그램만 webr.r-wasm.org에서 내려받음). 안 되면 같은 모형의 lavaan R 코드를 받게 함
  // =================================================================
  const WEBR_URL = 'https://webr.r-wasm.org/v0.6.0/webr.mjs';
  let webRP = null;
  function getWebR(say) {
    if (webRP) return webRP;
    webRP = (async () => {
      say('R 불러오는 중… (처음 한 번은 몇십 초 걸릴 수 있습니다)');
      const { WebR } = await import(WEBR_URL);
      const w = new WebR(); await w.init();
      say('lavaan 설치 중…');
      await w.installPackages(['lavaan', 'jsonlite'], { quiet: true });
      return w;
    })();
    webRP.catch(() => { webRP = null; });
    return webRP;
  }
  const cfaSt = () => (plan.cfa = plan.cfa || { est: 'ML', covs: [] });   // 예전 프로젝트에는 없을 수 있음
  let cfaRes = null, cfaBusy = false, cfaErr = '';
  const CFA_EST = { ML: '최대우도(ML)', MLR: '강건 최대우도(MLR, 정규성이 어긋날 때)', WLSMV: 'WLSMV (문항을 서열 변수로)' };
  // 모형: 척도마다 요인 하나, 문항 2개 이상인 척도만. lavaan 이름은 영문 약칭(아니면 v1, v2 …)
  function cfaModel() {
    const L = lastScored; if (!L) return null;
    const its = L.items.filter(d => d.kind === 'item'), ok = n => /^[A-Za-z][A-Za-z0-9_.]*$/.test(n);
    const name = new Map(); its.forEach((d, k) => name.set(d.key, ok(d.code) && its.filter(x => x.code === d.code).length === 1 ? d.code : `v${k + 1}`));
    const facs = sv.scales.map((s, si) => ({ si, f: `F${si + 1}`, label: s.name || scaleCode(s, si), items: its.filter(d => d.si === si) })).filter(x => x.items.length >= 2);
    const used = facs.flatMap(f => f.items);
    const covs = (cfaSt().covs || []).filter(c => used.some(d => name.get(d.key) === c.a) && used.some(d => name.get(d.key) === c.b));
    const syntax = facs.map(f => `${f.f} =~ ${f.items.map(d => name.get(d.key)).join(' + ')}`).concat(covs.map(c => `${c.a} ~~ ${c.b}`)).join('\n');
    return { facs, used, name, covs, syntax };
  }
  const CFA_FM = { ML: ['chisq', 'df', 'pvalue', 'cfi', 'tli', 'rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper', 'srmr'],
    MLR: ['chisq.scaled', 'df.scaled', 'pvalue.scaled', 'cfi.robust', 'tli.robust', 'rmsea.robust', 'rmsea.ci.lower.robust', 'rmsea.ci.upper.robust', 'srmr'],
    WLSMV: ['chisq.scaled', 'df.scaled', 'pvalue.scaled', 'cfi.scaled', 'tli.scaled', 'rmsea.scaled', 'rmsea.ci.lower.scaled', 'rmsea.ci.upper.scaled', 'srmr'] };
  // R 코드: forFile이면 analysis_data.csv를 읽어 사람이 볼 출력을, 아니면 브라우저용 JSON을 돌려줌
  function cfaR(M, forFile) {
    const est = cfaSt().est || 'ML', vars = M.used.map(d => M.name.get(d.key)), q = s => `"${String(s).replace(/["\\]/g, '\\$&')}"`;
    const head = `suppressPackageStartupMessages(library(lavaan))
model <- '
${M.syntax}
'
# 요인: ${M.facs.map(f => `${f.f} = ${f.label}`).join(', ')}
${M.covs.map(c => `# 연구자가 추가한 오차 공분산 ${c.a} ~~ ${c.b}: ${String(c.why).replace(/\n/g, ' ')}`).join('\n')}`;
    const fit = `fit <- cfa(model, data = d, estimator = "${est}", missing = "listwise"${est === 'WLSMV' ? ', ordered = TRUE' : ''})`;
    if (forFile) return `# 통 확인적 요인분석 (lavaan) — 만든 날 ${today()}
# 실행: 이 파일과 analysis_data.csv를 같은 폴더에 두고 R에서 실행. lavaan이 없으면 먼저 install.packages("lavaan")
DATA <- c("analysis_data.csv", "../2_data/analysis_data.csv", "2_data/analysis_data.csv"); DATA <- DATA[file.exists(DATA)][1]
raw <- read.csv(DATA, fileEncoding = "UTF-8-BOM", check.names = FALSE)
d <- data.frame(${M.used.map(x => `${M.name.get(x.key)} = raw[[${q(x.code)}]]`).join(', ')})
${head}
${fit}
summary(fit, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit, c(${CFA_FM[est].map(q).join(', ')}))
ss <- standardizedSolution(fit); ld <- ss[ss$op == "=~", ]
lam <- split(ld$est.std, ld$lhs)
print(round(cbind(CR = sapply(lam, function(l) sum(l)^2 / (sum(l)^2 + sum(1 - l^2))), AVE = sapply(lam, function(l) mean(l^2))), 3))   # 합성신뢰도, 평균분산추출
print(round(lavInspect(fit, "cor.lv"), 3))   # 요인 간 상관
mi <- modindices(fit, sort. = TRUE); print(head(mi[mi$op == "~~" & mi$lhs != mi$rhs, ], 10))   # 수정지수 (이론적 근거 없이 따르지 말 것)
`;
    return `d <- read.csv(text = csvtxt)
${head}
warn <- character(0)
fit <- withCallingHandlers(${fit.replace('fit <- ', '')}, warning = function(w) { warn <<- c(warn, conditionMessage(w)); invokeRestart("muffleWarning") })
fm <- fitMeasures(fit, c(${CFA_FM[est].map(q).join(', ')}))
ss <- standardizedSolution(fit); ld <- ss[ss$op == "=~", ]
lv <- lavInspect(fit, "cor.lv")
mi <- tryCatch({ m <- modindices(fit, sort. = TRUE); m <- m[(m$op == "~~" & m$lhs != m$rhs) | m$op == "=~", ]; head(m[, c("lhs", "op", "rhs", "mi", "epc")], 12) }, error = function(e) NULL)
as.character(jsonlite::toJSON(list(lavaan = as.character(packageVersion("lavaan")), n = lavInspect(fit, "nobs"), converged = lavInspect(fit, "converged"),
  fit = as.list(unname(fm)), ld = list(lhs = ld$lhs, rhs = ld$rhs, std = ld$est.std, se = ld$se, p = ld$pvalue),
  lv = list(names = colnames(lv), m = unname(lv)), mi = mi, warn = warn), digits = NA, auto_unbox = TRUE, na = "null", dataframe = "columns"))`;
  }
  async function runCfa() {
    const M = cfaModel(); if (!M || !M.facs.length) return;
    cfaBusy = true; cfaErr = ''; renderCfa();
    const say = t => { const el = $('#cfa-prog'); if (el) el.textContent = t; };
    try {
      const vars = M.used.map(d => M.name.get(d.key));
      const csv = [vars.join(',')].concat(lastScored.inc.map(p => M.used.map(d => p.items[d.key] == null ? 'NA' : p.items[d.key]).join(','))).join('\n');
      const w = await getWebR(say);
      say('계산 중…');
      const json = await w.evalRString(cfaR(M, false), { env: { csvtxt: csv } });
      const r = JSON.parse(json);
      r.est = cfaSt().est; r.dv = dataVersion; r.syntax = M.syntax; r.at = koNow();
      // 합성신뢰도(CR)·평균분산추출(AVE), 판별타당도(HTMT)
      const fac = M.facs.map(f => { const ix = r.ld.lhs.map((l, i) => l === f.f ? i : -1).filter(i => i >= 0), lam = ix.map(i => r.ld.std[i]); const s1 = lam.reduce((a, b) => a + b, 0), e = lam.reduce((a, b) => a + (1 - b * b), 0); return { ...f, CR: s1 * s1 / (s1 * s1 + e), AVE: lam.reduce((a, b) => a + b * b, 0) / lam.length }; });
      const comp = lastScored.inc.filter(p => M.used.every(d => p.items[d.key] != null)), col = d => comp.map(p => p.items[d.key]);
      const rr = (A, B) => mean(A.flatMap(a => B.filter(b => b !== a).map(b => Math.abs(corr(col(a), col(b))))));
      r.htmt = fac.map(a => fac.map(b => a === b ? NaN : rr(a.items, b.items) / Math.sqrt(rr(a.items, a.items) * rr(b.items, b.items))));
      r.fac = fac.map(({ f, label, CR, AVE, si }) => ({ f, label, CR, AVE, si }));
      cfaRes = r; window.tongCfaLast = r;
    } catch (e) { cfaErr = String((e && e.message) || e); }
    cfaBusy = false; renderCfa(); renderMeasure();
  }
  const renderCfa = () => { const c = $('#cfa-card'); if (c) c.innerHTML = cfaHtml(); };
  function cfaHtml() {
    const M = cfaModel();
    if (!M || !M.facs.length) return `<h3 class="card-title">확인적 요인분석</h3><p class="hint">문항이 2개 이상인 척도가 있어야 합니다.</p>`;
    const r = cfaRes, est = cfaSt().est || 'ML';
    const fitRow = (lab, v, rule, good, ok2) => `<tr><td>${lab}</td><td class="num">${v}</td><td>${rule}</td><td>${good == null ? '<span class="hint">참고</span>' : good ? '<span class="a-ok">좋음</span>' : ok2 ? '<span class="a-mid">받아들일 만함</span>' : '<span class="a-low">미흡</span>'}</td></tr>`;
    let out = '';
    if (r) {
      const [chi, df, pv, cfi, tli, rmsea, rlo, rhi, srmr] = r.fit;
      const stale = r.dv !== dataVersion || r.est !== est || r.syntax !== M.syntax;
      out += `${stale ? '<p class="cf-note">자료나 모형이 바뀌었습니다. [다시 계산]을 누르세요.</p>' : ''}
        <div class="dq-kv"><span>분석한 사람</span><b>${r.n}명 <small>(모든 문항에 답한 사람)</small></b><span>추정</span><b>${esc(CFA_EST[r.est])}</b><span>계산</span><b>R lavaan ${esc(r.lavaan)} <small>(브라우저 안에서, ${esc(r.at)})</small></b></div>
        ${!r.converged ? '<p class="a-low">⚠ 계산이 수렴하지 않았습니다. 결과를 믿을 수 없습니다. 문항 수가 적은 요인이나 응답이 몰린 문항을 확인하세요.</p>' : ''}
        ${(r.warn || []).length ? `<p class="a-mid">⚠ lavaan 경고: ${r.warn.map(esc).join(' / ')}</p>` : ''}
        <div class="section-title">적합도</div>
        <div class="table-wrap"><table class="compact"><thead><tr><th>지표</th><th class="num">값</th><th>흔히 쓰는 기준</th><th>판정</th></tr></thead><tbody>
          ${fitRow(`χ²${r.est !== 'ML' ? ' (보정)' : ''}`, `${F2(chi)} (df = ${F2(df, 0)}), ${PS(pv)}`, '표본이 크면 거의 언제나 유의해 다른 지표와 함께 봄', null)}
          ${fitRow('CFI', NZ(cfi, 3), '.90 이상 받아들일 만함, .95 이상 좋음', cfi >= 0.95, cfi >= 0.90)}
          ${fitRow('TLI', NZ(tli, 3), '.90 이상 받아들일 만함, .95 이상 좋음', tli >= 0.95, tli >= 0.90)}
          ${fitRow('RMSEA [90% CI]', `${NZ(rmsea, 3)} [${NZ(rlo, 3)}, ${NZ(rhi, 3)}]`, '.08 이하 받아들일 만함, .06 이하 좋음', rmsea <= 0.06, rmsea <= 0.08)}
          ${fitRow('SRMR', NZ(srmr, 3), '.08 이하 좋음', srmr <= 0.08, false)}
        </tbody></table></div>
        <p class="hint">기준은 Hu와 Bentler(1999) 등이 제안한 경험칙입니다. 하나만 보고 판단하지 말고 함께 보고하세요.</p>
        <div class="section-title">표준화 요인부하량</div>
        <div class="table-wrap"><table class="compact"><thead><tr><th>요인</th><th>문항</th><th class="num">표준화 부하량</th><th class="num">SE</th><th class="num">p</th></tr></thead><tbody>
          ${r.ld.lhs.map((l, i) => { const f = M.facs.find(x => x.f === l), d = M.used.find(x => M.name.get(x.key) === r.ld.rhs[i]), v = r.ld.std[i]; return `<tr><td>${esc(f ? f.label : l)}</td><td>${esc(d ? d.code : r.ld.rhs[i])} <small>${esc(d ? d.text.slice(0, 18) : '')}</small></td><td class="num"><span class="${v < 0.4 ? 'a-low' : v < 0.5 ? 'a-mid' : ''}">${NZ(v, 3)}</span></td><td class="num">${F2(r.ld.se[i], 3)}</td><td class="num">${PV(r.ld.p[i])}</td></tr>`; }).join('')}
        </tbody></table></div>
        <p class="hint">.50 이상이면 좋다고 봅니다(주황 .40~.50, 빨강 .40 미만). 낮은 문항을 뺄지는 이론과 원척도를 보고 정하고, 뺐다면 논문에 적습니다.</p>
        <div class="section-title">집중타당도와 판별타당도</div>
        <div class="table-wrap"><table class="compact"><thead><tr><th>요인</th><th class="num">CR</th><th class="num">AVE</th>${r.fac.map((_, j) => `<th class="num">${j + 1}</th>`).join('')}</tr></thead><tbody>
          ${r.fac.map((f, i) => `<tr><td>${i + 1}. ${esc(f.label)}</td><td class="num"><span class="${f.CR >= 0.7 ? 'a-ok' : 'a-mid'}">${NZ(f.CR, 3)}</span></td><td class="num"><span class="${f.AVE >= 0.5 ? 'a-ok' : 'a-mid'}">${NZ(f.AVE, 3)}</span></td>${r.fac.map((g, j) => { if (j > i) return '<td></td>'; if (j === i) return `<td class="num"><b>${NZ(Math.sqrt(f.AVE), 3)}</b></td>`; const ii = r.lv.names.indexOf(f.f), jj = r.lv.names.indexOf(g.f), c = r.lv.m[ii][jj]; const bad = Math.abs(c) >= Math.min(Math.sqrt(f.AVE), Math.sqrt(g.AVE)); return `<td class="num"><span class="${bad ? 'a-low' : ''}">${NZ(c, 3)}</span></td>`; }).join('')}</tr>`).join('')}
        </tbody></table></div>
        <p class="hint">CR(합성신뢰도) .70 이상, AVE(평균분산추출) .50 이상이면 집중타당도가 있다고 봅니다. 대각선의 굵은 값은 √AVE이고, 그 아래는 요인 간 상관입니다. √AVE가 같은 줄·칸의 상관보다 크면 판별타당도가 있다고 봅니다(Fornell & Larcker, 1981, 빨강은 어긋남).</p>
        ${r.fac.length > 1 ? `<div class="table-wrap"><table class="compact"><thead><tr><th>HTMT</th>${r.fac.map((_, j) => `<th class="num">${j + 1}</th>`).join('')}</tr></thead><tbody>${r.fac.map((f, i) => `<tr><td>${i + 1}. ${esc(f.label)}</td>${r.fac.map((_, j) => j >= i ? '<td></td>' : `<td class="num"><span class="${r.htmt[i][j] >= 0.9 ? 'a-low' : r.htmt[i][j] >= 0.85 ? 'a-mid' : ''}">${NZ(r.htmt[i][j], 3)}</span></td>`).join('')}</tr>`).join('')}</tbody></table></div>
        <p class="hint">HTMT가 .85보다 작으면(느슨하게는 .90) 두 요인이 구별된다고 봅니다(Henseler, Ringle, & Sarstedt, 2015). 최근에는 Fornell–Larcker보다 HTMT를 더 권합니다.</p>` : ''}
        ${cfaMiHtml(r, M)}
        <div class="dq-report"><h4>논문에 쓸 문장 (초안)</h4><p id="cfa-sent">${esc(cfaSentence(r, M))}</p><div class="actions tight"><button class="btn sm" data-cfa="copy">문장 복사</button></div></div>`;
    }
    return `<h3 class="card-title">확인적 요인분석 <small class="hint">(R lavaan)</small></h3>
      <p class="hint">척도마다 요인 하나로 보고, 원척도의 요인 구조가 이 자료에서도 맞는지 확인합니다. 계산은 이 브라우저 안의 R(lavaan)이 하며 <b>응답 자료는 밖으로 나가지 않습니다</b>(R 프로그램만 처음 한 번 약 10MB를 내려받습니다).</p>
      <details class="item-detail"><summary>모형 보기 (lavaan 문장)</summary><pre class="r-code no-tw">${esc(M.syntax)}</pre><p class="hint" style="padding:0 14px">${M.facs.map(f => `${f.f} = ${esc(f.label)}`).join(', ')}${M.facs.some(f => f.items.length < 3) ? ' · ⚠ 문항이 2개뿐인 요인은 혼자서는 식별되지 않아 다른 요인과의 상관에 기대어 추정됩니다.' : ''}</p></details>
      ${M.covs.length ? `<div class="cf-note"><b>연구자가 추가한 오차 공분산 ${M.covs.length}개</b> (사후 수정으로 기록됨)<ul class="dq-out">${M.covs.map((c, k) => `<li>${esc(c.a)} ~~ ${esc(c.b)}: ${esc(c.why)} <button class="linkish" data-cfa="cov-del" data-k="${k}">빼기</button></li>`).join('')}</ul></div>` : ''}
      <div class="actions tight"><label class="f pw-f" style="flex-direction:row;align-items:center;gap:8px">추정<select data-cfa="est">${Object.entries(CFA_EST).map(([k, l]) => `<option value="${k}" ${k === est ? 'selected' : ''}>${esc(l)}</option>`).join('')}</select></label>
        <button class="btn primary" data-cfa="run" ${cfaBusy ? 'disabled' : ''}>${r ? '다시 계산' : '확인적 요인분석 하기'}</button><button class="btn sm" data-cfa="rcode">lavaan R 코드 받기</button><span class="hint" id="cfa-prog">${cfaBusy ? '계산 중…' : ''}</span></div>
      ${cfaErr ? `<div class="errbox">브라우저 안에서 R을 불러오지 못했습니다(${esc(cfaErr.slice(0, 120))}). 인터넷 연결을 확인하세요. claude.ai 화면에서는 보안 정책상 R을 불러올 수 없을 수 있습니다. 사이트(https://aninsong77-dotcom.github.io/tong/)에서 하거나, [lavaan R 코드 받기]로 받은 코드를 R에서 실행하세요.</div>` : ''}
      ${out}`;
  }
  // 수정지수: 보여 주되, 모형을 고치려면 이론적 이유를 적게 하고 기록에 남김
  function cfaMiHtml(r, M) {
    const mi = (r.mi && r.mi.lhs ? r.mi.lhs.map((_, i) => ({ lhs: r.mi.lhs[i], op: r.mi.op[i], rhs: r.mi.rhs[i], mi: r.mi.mi[i], epc: r.mi.epc[i] })) : []).filter(x => x.mi >= 10);
    if (!mi.length) return '<p class="hint">수정지수가 10을 넘는 곳이 없습니다.</p>';
    const lab = n => { const d = M.used.find(x => M.name.get(x.key) === n), f = M.facs.find(x => x.f === n); return d ? d.code : f ? f.label : n; };
    return `<details class="item-detail"><summary>수정지수 (10 이상, ${mi.length}개)</summary><div style="padding:4px 14px 12px">
      <p class="a-mid">수정지수를 보고 모형을 고치면 적합도는 좋아지지만, 이 자료에만 맞춘 모형이 되기 쉽습니다(MacCallum 외, 1992). 같은 척도 안의 비슷한 문장처럼 <b>이론적 이유가 있을 때만</b> 오차 공분산을 추가하고, 그 이유를 논문에 적으세요. 추가하면 분석 결정 기록에 사후 수정으로 남습니다.</p>
      <ul class="dq-out">${mi.map(x => `<li>${esc(lab(x.lhs))} ${x.op === '~~' ? '↔' : '→'} ${esc(lab(x.rhs))}: MI = ${F2(x.mi)}, 예상 변화량 ${F2(x.epc)}${x.op === '~~' ? ` <button class="linkish" data-cfa="cov-add" data-a="${esc(x.lhs)}" data-b="${esc(x.rhs)}">오차 공분산 추가…</button>` : ' <span class="hint">(교차부하: 문항이 다른 요인에도 걸림)</span>'}</li>`).join('')}</ul></div></details>`;
  }
  function cfaSentence(r, M) {
    const [chi, df, pv, cfi, tli, rmsea, rlo, rhi, srmr] = r.fit, lo = Math.min(...r.ld.std), hi = Math.max(...r.ld.std);
    return `측정모형의 타당성을 확인하기 위해 ${M.facs.length}개 요인(${M.facs.map(f => f.label).join(', ')})으로 확인적 요인분석을 실시하였다(${r.est === 'ML' ? '최대우도법' : r.est === 'MLR' ? '강건 최대우도법(MLR)' : 'WLSMV'}, R lavaan ${r.lavaan}). 그 결과 χ²(${F2(df, 0)}) = ${F2(chi)}, ${PS(pv)}, CFI = ${NZ(cfi)}, TLI = ${NZ(tli)}, RMSEA = ${NZ(rmsea, 3)} (90% CI [${NZ(rlo, 3)}, ${NZ(rhi, 3)}]), ${J(`SRMR = ${NZ(srmr, 3)}`, '으로/로')} 나타났다. 표준화 요인부하량은 ${J(`${NZ(lo)}~${NZ(hi)}`, '이었/였')}고, 합성신뢰도(CR)는 ${NZ(Math.min(...r.fac.map(f => f.CR)))}~${NZ(Math.max(...r.fac.map(f => f.CR)))}, 평균분산추출(AVE)은 ${J(`${NZ(Math.min(...r.fac.map(f => f.AVE)))}~${NZ(Math.max(...r.fac.map(f => f.AVE)))}`, '이었/였')}다.${M.covs.length ? ` 수정지수를 참고하되 이론적 근거가 있는 ${M.covs.length}개의 오차 공분산을 추가하였다(${M.covs.map(c => `${c.a}–${c.b}: ${c.why}`).join('; ')}).` : ''} [적합도와 타당도의 판단은 연구자가 기준과 함께 서술]`;
  }
  document.addEventListener('click', async e => {
    const b = e.target.closest('[data-cfa]'); if (!b) return;
    const act = b.dataset.cfa;
    if (act === 'run') { if (!cfaBusy) await runCfa(); }
    else if (act === 'rcode') { const M = cfaModel(); if (M) download('cfa_lavaan.R', cfaR(M, true)); }
    else if (act === 'copy') { const t = $('#cfa-sent')?.textContent || ''; try { navigator.clipboard.writeText(t).then(() => toast('문장을 복사했습니다.'), () => showSheet('확인적 요인분석 문장', t)); } catch (err) { showSheet('확인적 요인분석 문장', t); } }
    else if (act === 'cov-add') {
      const why = prompt(`${b.dataset.a} ↔ ${b.dataset.b} 오차 공분산을 추가하는 이론적 이유를 적어 주세요 (예: 두 문항이 거의 같은 내용을 반대로 물음). 이 이유는 기록과 논문 문장에 들어갑니다.`);
      if (!why || !norm(why)) { toast('이유를 적어야 추가할 수 있습니다.'); return; }
      cfaSt().covs = (cfaSt().covs || []).concat({ a: b.dataset.a, b: b.dataset.b, why: norm(why) });
      plan.log.push({ at: koNow(), id: 'cfa', q: '확인적 요인분석 모형 수정 (오차 공분산 추가)', from: '-', to: `${b.dataset.a} ~~ ${b.dataset.b} (이유: ${norm(why)})`, after: true, reason: norm(why) });
      savePlan(); addSys('analysis', `확인적 요인분석: 오차 공분산 ${b.dataset.a} ~~ ${b.dataset.b} 추가 (사후 수정으로 기록)`); await runCfa();
    }
    else if (act === 'cov-del') { const c = cfaSt().covs.splice(+b.dataset.k, 1)[0]; if (c) { plan.log.push({ at: koNow(), id: 'cfa', q: '확인적 요인분석 모형 수정 (오차 공분산 빼기)', from: `${c.a} ~~ ${c.b}`, to: '-', after: !!plan.seen, reason: '' }); savePlan(); } renderCfa(); }
  });
  document.addEventListener('change', e => { const t = e.target; if (t.dataset && t.dataset.cfa === 'est') { cfaSt().est = t.value; savePlan(); renderCfa(); } });

  // =================================================================
  // =================================================================
  // 구조방정식 (SEM): 문항이 2개 이상인 척도는 잠재변인(요인), 나머지는 관측변인. 브라우저 속 R lavaan으로 계산
  // 간접효과·총효과는 부트스트랩 백분위 신뢰구간. 시작값을 정해 100번씩 나눠 돌리고(진행 표시), R 파일도 같은 순서로 돌려 같은 값이 나옴
  // =================================================================
  const SEM_SEED = 20261019, SEM_CHUNK = 100;
  const cfaItemNames = () => { const its = lastScored.items.filter(d => d.kind === 'item'), ok = n => /^[A-Za-z][A-Za-z0-9_.]*$/.test(n), m = new Map(); its.forEach((d, k) => m.set(d.key, ok(d.code) && its.filter(x => x.code === d.code).length === 1 ? d.code : `v${k + 1}`)); return m; };
  function semSpec(kind, r) {
    const nmap = cfaItemNames();
    const node = k => {
      const v = varByKey(k); need(v, '변인을 찾지 못했습니다. 역할을 다시 골라 주세요.');
      if (/^S\d+$/.test(k)) { const si = +k.slice(1), its = lastScored.items.filter(d => d.kind === 'item' && d.si === si);
        if (its.length >= 2) return { k, name: v.name, id: `F${si + 1}`, items: its.map(d => ({ key: d.key, code: d.code, nm: nmap.get(d.key) })) }; }
      need(v.type === 'num' || (v.type === 'cat' && v.levels.length === 2), `'${v.name}'은(는) 구조방정식에 넣을 수 없습니다. 점수나 두 집단 변인을 고르세요.`);
      return { k, name: v.name, id: `o_${k}`, obs: true, v };
    };
    const ser = kind === 'serial';
    const X = (ser ? [r.x] : r.xs).map(node), M = (ser ? [r.m1, r.m2] : (r.ms || [])).map(node), Y = node(r.y), C = (r.covs || []).map(node);
    const all = [...X, ...M, Y, ...C]; need(new Set(all.map(n => n.k)).size === all.length, '같은 변인을 두 역할에 넣었습니다. 역할마다 다른 변인을 골라 주세요.');
    const cv = C.map(n => n.id).join(' + '), plus = s => (s ? ' + ' + s : '');
    const lines = all.filter(n => n.items).map(n => `${n.id} =~ ${n.items.map(i => i.nm).join(' + ')}`);
    const P = [], D = [];   // P: 경로(회귀) 라벨, D: 정의한 효과(간접·총) 라벨
    const nx = X.length, xi = i => (nx > 1 ? `${i + 1}` : '');
    if (!M.length) {
      lines.push(`${Y.id} ~ ${X.map((x, i) => `c${xi(i)}*${x.id}`).join(' + ')}${plus(cv)}`);
      X.forEach((x, i) => P.push({ lab: `c${xi(i)}`, label: `경로: ${x.name} → ${Y.name}` }));
    } else if (ser) {
      const [x] = X, [m1, m2] = M;
      lines.push(`${m1.id} ~ a1*${x.id}${plus(cv)}`, `${m2.id} ~ a2*${x.id} + d*${m1.id}${plus(cv)}`, `${Y.id} ~ cp*${x.id} + b1*${m1.id} + b2*${m2.id}${plus(cv)}`);
      P.push({ lab: 'a1', label: `경로: ${x.name} → ${m1.name}` }, { lab: 'a2', label: `경로: ${x.name} → ${m2.name}` }, { lab: 'd', label: `경로: ${m1.name} → ${m2.name}` },
        { lab: 'b1', label: `경로: ${m1.name} → ${Y.name}` }, { lab: 'b2', label: `경로: ${m2.name} → ${Y.name}` }, { lab: 'cp', label: `직접효과: ${x.name} → ${Y.name}` });
      D.push({ lab: 'ind1', def: 'a1*b1', label: `간접효과: ${x.name} → ${m1.name} → ${Y.name}` }, { lab: 'ind2', def: 'a1*d*b2', label: `간접효과: ${x.name} → ${m1.name} → ${m2.name} → ${Y.name}` },
        { lab: 'ind3', def: 'a2*b2', label: `간접효과: ${x.name} → ${m2.name} → ${Y.name}` }, { lab: 'tind', def: 'a1*b1 + a1*d*b2 + a2*b2', label: `총 간접효과: ${x.name} → ${Y.name}` },
        { lab: 'tot', def: 'cp + a1*b1 + a1*d*b2 + a2*b2', label: `총효과: ${x.name} → ${Y.name}` });
    } else {
      M.forEach((m, j) => { lines.push(`${m.id} ~ ${X.map((x, i) => `a${i + 1}_${j + 1}*${x.id}`).join(' + ')}${plus(cv)}`); X.forEach((x, i) => P.push({ lab: `a${i + 1}_${j + 1}`, label: `경로: ${x.name} → ${m.name}` })); });
      lines.push(`${Y.id} ~ ${X.map((x, i) => `cp${xi(i)}*${x.id}`).join(' + ')} + ${M.map((m, j) => `b${j + 1}*${m.id}`).join(' + ')}${plus(cv)}`);
      M.forEach((m, j) => P.push({ lab: `b${j + 1}`, label: `경로: ${m.name} → ${Y.name}` }));
      X.forEach((x, i) => P.push({ lab: `cp${xi(i)}`, label: `직접효과: ${x.name} → ${Y.name}` }));
      for (let j = 0; j < M.length; j++) for (let k = j + 1; k < M.length; k++) lines.push(`${M[j].id} ~~ ${M[k].id}`);   // 병렬 매개변인의 잔차 공분산 (PROCESS처럼 서로 통제)
      X.forEach((x, i) => {
        const ind = M.map((m, j) => `a${i + 1}_${j + 1}*b${j + 1}`);
        M.forEach((m, j) => D.push({ lab: `ind${i + 1}_${j + 1}`, def: ind[j], label: `간접효과: ${x.name} → ${m.name} → ${Y.name}` }));
        if (M.length > 1) D.push({ lab: `tind${xi(i)}`, def: ind.join(' + '), label: `총 간접효과: ${x.name} → (${M.map(m => m.name).join(', ')}) → ${Y.name}` });
        D.push({ lab: `tot${xi(i)}`, def: `cp${xi(i)} + ${ind.join(' + ')}`, label: `총효과: ${x.name} → ${Y.name}` });
      });
    }
    D.forEach(d => lines.push(`${d.lab} := ${d.def}`));
    const items = all.filter(n => n.items).flatMap(n => n.items), obs = all.filter(n => n.obs);
    return { kind, X, M, Y, C, all, P, D, syntax: lines.join('\n'), items, obs };
  }
  // 분석에 쓸 자료 (모든 문항·변인에 답한 사람만, 목록별 제외). 두 집단 변인은 0/1
  function semData(S) {
    const val = (n, i) => { const x = n.v.vals[i]; if (x == null) return null; return n.v.type === 'cat' ? (x === n.v.levels[1] ? 1 : x === n.v.levels[0] ? 0 : null) : x; };
    const rows = [];
    lastScored.inc.forEach((p, i) => { const r = S.items.map(d => p.items[d.key]).concat(S.obs.map(n => val(n, i))); if (r.every(v => v != null && v !== '')) rows.push(r); });
    return { head: S.items.map(d => d.nm).concat(S.obs.map(n => n.id)), rows };
  }
  async function runSem(kind, r, o, onProg) {
    const S = semSpec(kind, r), dat = semData(S), B = +o.semBoot || 1000;
    need(dat.rows.length >= 30, `모든 문항에 답한 사람이 ${dat.rows.length}명뿐이라 구조방정식을 계산할 수 없습니다.`);
    const say = t => { const el = $('#an-prog'); if (el) el.textContent = t; };
    let w; try { w = await getWebR(say); } catch (e) { throw new Error(`브라우저 안에서 R을 불러오지 못했습니다(${(e && e.message) || e}). 인터넷 연결을 확인하세요. claude.ai 화면에서는 보안 정책상 R을 불러올 수 없을 수 있으니 사이트(${TONG_URL})에서 계산하세요.`); }
    say('구조방정식 계산 중…');
    const csv = [dat.head.join(',')].concat(dat.rows.map(x => x.join(','))).join('\n');
    const want = S.P.map(p => p.lab).concat(S.D.map(d => d.lab));
    const res = JSON.parse(await w.evalRString(semR(S, 'fit'), { env: { csvtxt: csv } }));
    need(res.converged, '계산이 수렴하지 않았습니다. 문항이 적거나 응답이 한쪽으로 몰린 척도가 있는지 확인하세요.');
    // 부트스트랩: 100번씩
    const draws = []; let ok = 0;
    for (let c = 0; c * SEM_CHUNK < B; c++) {
      const nb = Math.min(SEM_CHUNK, B - c * SEM_CHUNK);
      const part = JSON.parse(await w.evalRString(`tong_boot(${c}, ${nb})`));
      (part || []).forEach(row => { if (row.every(v => v != null && isFinite(v))) { draws.push(row); ok++; } });
      onProg && onProg(Math.min(1, (c * SEM_CHUNK + nb) / B));
    }
    const bs = want.map((_, j) => { const col = draws.map(d => d[j]).sort((a, b) => a - b); return col.length ? { lo: pct(col, 0.025), hi: pct(col, 0.975), se: sd(col) } : { lo: NaN, hi: NaN, se: NaN }; });
    bs.B = B; bs.ok = ok;
    const pe = res.pe, row = lab => { const i = pe.label.indexOf(lab); return i < 0 ? null : { est: pe.est[i], se: pe.se[i], z: pe.z[i], p: pe.pvalue[i], lo: pe.lo[i], hi: pe.hi[i], std: pe.std[i] }; };
    const X = S.X, Mn = S.M.map(m => m.name), Yn = S.Y.name, ser = kind === 'serial';
    const R = newResult(ser ? 'sem_serial' : 'sem', `구조방정식: ${X.map(x => x.name).join(', ')}${Mn.length ? ` → ${Mn.join(ser ? ' → ' : ', ')}` : ''} → ${Yn}`); R.n = res.n; bootNote(R, bs);
    const [chi, df, pv, cfi, tli, rmsea, rlo, rhi, srmr] = res.fit;
    const judge = (v, good, ok2, lower) => !isFinite(v) ? '' : (lower ? v <= good : v >= good) ? '좋음' : (lower ? v <= ok2 : v >= ok2) ? '받아들일 만함' : '미흡';
    R.tables.push({ title: '모형 적합도', head: ['지표', '값', '흔히 쓰는 기준', '판정'], num: [1],
      rows: [['χ²', `${F2(chi)} (df = ${F2(df, 0)}), ${PS(pv)}`, '표본이 크면 거의 언제나 유의해 다른 지표와 함께 봄', ''], ['CFI', NZ(cfi, 3), '.90 이상 받아들일 만함, .95 이상 좋음', judge(cfi, 0.95, 0.9)], ['TLI', NZ(tli, 3), '.90 이상 받아들일 만함, .95 이상 좋음', judge(tli, 0.95, 0.9)],
        ['RMSEA', `${NZ(rmsea, 3)} [${NZ(rlo, 3)}, ${NZ(rhi, 3)}]`, '.08 이하 받아들일 만함, .06 이하 좋음', judge(rmsea, 0.06, 0.08, true)], ['SRMR', NZ(srmr, 3), '.08 이하 받아들일 만함', judge(srmr, 0.08, 0.08, true)]] });
    const lat = S.all.filter(n => n.items);
    if (lat.length) R.tables.push({ title: '측정모형 (표준화 요인부하량)', head: ['요인', '문항', 'λ', 'p'], num: [2, 3],
      rows: lat.flatMap(n => n.items.map((it, q) => { const i = pe.lhs.findIndex((l, ii) => l === n.id && pe.op[ii] === '=~' && pe.rhs[ii] === it.nm); return [q ? '' : n.name, it.code, NZ(pe.std[i], 3), q ? PV(pe.pvalue[i]) : '(고정)']; })),
      foot: '각 요인의 첫 문항 부하량을 1로 고정해 척도를 정했습니다(비표준화). 표준화 부하량 .50 이상이면 좋다고 봅니다.' });
    R.tables.push({ title: '구조모형 경로', head: ['경로', 'B', 'SE', 'β', 'z', 'p', '95% CI'], num: [1, 2, 3, 4, 5, 6],
      rows: S.P.map(p => { const q = row(p.lab); return [p.label.replace(/^[^:]+: /, ''), F2(q.est, 3), F2(q.se, 3), NZ(q.std, 3), F2(q.z), PV(q.p), CI(q.lo, q.hi, 3)]; }) });
    if (S.D.length) R.tables.push({ title: `간접효과와 총효과 (부트스트랩 ${B.toLocaleString()}번, 95% 백분위 신뢰구간)`, head: ['경로', '효과', 'Boot SE', '95% CI', 'β'], num: [1, 2, 3, 4],
      rows: S.D.map((d, j) => { const q = row(d.lab), b = bs[S.P.length + j]; return [d.label.replace(/^간접효과: /, ''), F2(q.est, 3), F2(b.se, 3), CI(b.lo, b.hi, 3), NZ(q.std, 3)]; }) });
    const r2 = (res.r2 || { names: [], v: [] });
    const endo = [...S.M, S.Y];
    R.notes.push(`설명된 분산 R²: ${endo.map(n => { const i = r2.names.indexOf(n.id); return `${n.name} ${i < 0 ? '-' : NZ(r2.v[i], 3)}`; }).join(', ')}`);
    R.notes.push(`계산: R lavaan ${res.lavaan} (브라우저 안에서), 최대우도(ML), 모든 문항에 답한 ${res.n}명 (목록별 제외). 잠재변인: ${lat.map(n => `${n.name}(문항 ${n.items.length}개)`).join(', ') || '없음'}${S.obs.length ? `. 관측변인: ${S.obs.map(n => n.name).join(', ')}` : ''}.`);
    if ((res.warn || []).length) R.notes.push(`⚠ lavaan 경고: ${res.warn.join(' / ')}`);
    if (S.C.length) R.notes.push(`통제변인 ${S.C.map(n => n.name).join(', ')}은(는) 매개변인과 결과변인에 함께 넣었습니다.`);
    // 효과 (가설과 이어짐)
    S.P.forEach(p => { const q = row(p.lab); eff(R, { label: p.label, est: q.est, p: q.p, lo: q.lo, hi: q.hi, std: q.std, stat: `B = ${F2(q.est, 3)}, β = ${NZ(q.std, 3)}, z = ${F2(q.z)}, ${PS(q.p)}` }); });
    S.D.forEach((d, j) => { const q = row(d.lab), b = bs[S.P.length + j]; eff(R, { label: d.label, est: q.est, lo: b.lo, hi: b.hi, ciBased: true, std: q.std, stat: `B = ${F2(q.est, 3)}, β = ${NZ(q.std, 3)}, 95% CI ${CI(b.lo, b.hi, 3)}` }); });
    // 그림: 원인 하나·매개가 있을 때 (표준화 계수)
    if (X.length === 1 && S.M.length && S.M.length <= 4) {
      const k = lab => { const q = row(lab); return { est: q.std, p: q.p }; };
      const a = ser ? [k('a1'), k('a2')] : S.M.map((_, j) => k(`a1_${j + 1}`)), b = S.M.map((_, j) => k(`b${j + 1}`));
      addFig(R, { type: 'path', kind: ser ? 'serial' : 'med', title: '구조방정식 경로 결과 (표준화 계수)', X: X[0].name, Y: Yn, Ms: Mn, a, b, d: ser ? k('d') : null, cp: k('cp'), c: k('tot'),
        note: '계수는 표준화 계수(β)입니다. 굵은 선은 p < .05인 경로, 점선은 유의하지 않은 직접효과입니다. 괄호 안 c는 총효과입니다. 측정모형(문항)은 그림에서 생략했습니다. 간접효과의 유의성은 표의 부트스트랩 신뢰구간으로 판단합니다. *p < .05, **p < .01, ***p < .001.' });
    }
    let cfaOk = false; try { cfaOk = !!(cfaRes && cfaRes.dv === dataVersion); } catch (e) { }
    if (!cfaOk && S.all.some(n => n.items)) R.notes.unshift('⚠ 확인적 요인분석을 아직 하지 않았습니다. 구조방정식은 측정모형이 맞아야 구조모형을 믿을 수 있으니, ⑧ 측정 점검에서 확인적 요인분석을 먼저 하고 보고하세요.');
    R.sem = { syntax: S.syntax, lavaan: res.lavaan };
    R.sent = semSentence(S, res, row, bs, B, lat, cfaOk);
    return R;
  }
  // 논문에 쓸 문장 (초안): 숫자와 유의 여부만 적고, 적합도 판단과 가설 지지는 연구자가 씀
  function semSentence(S, res, row, bs, B, lat, cfaOk) {
    const [chi, df, pv, cfi, tli, rmsea, rlo, rhi, srmr] = res.fit, Xn = S.X.map(x => x.name).join(', '), Yn = S.Y.name, ser = S.kind === 'serial';
    const what = !S.M.length ? `${J(Xn, '이/가')} ${Yn}에 미치는 영향을 검증하기 위해`
      : `${J(Xn, '과/와')} ${Yn}의 관계에서 ${ser ? `${J(S.M[0].name, '과/와')} ${S.M[1].name}의 이중` : `${S.M.map(m => m.name).join(', ')}의`} 매개효과를 검증하기 위해`;
    const out = [`${what} 구조방정식 모형을 분석하였다(최대우도법, R lavaan ${res.lavaan}, N = ${res.n}).`];
    if (lat.length) out.push(`${J(lat.map(n => `${n.name}(문항 ${n.items.length}개)`).join(', '), '은/는')} 잠재변인으로${S.obs.length ? `, ${J(S.obs.map(n => n.name).join(', '), '은/는')} 관측변인으로` : ''} 모형에 넣었다.${cfaOk ? '' : ' [측정모형의 확인적 요인분석 결과를 먼저 보고]'}`);
    if (S.C.length) out.push(`${J(S.C.map(n => n.name).join(', '), '을/를')} 통제변인으로 포함하였다.`);
    out.push(`모형 적합도는 χ²(${F2(df, 0)}) = ${F2(chi)}, ${PS(pv)}, CFI = ${NZ(cfi, 3)}, TLI = ${NZ(tli, 3)}, RMSEA = ${NZ(rmsea, 3)} (90% CI [${NZ(rlo, 3)}, ${NZ(rhi, 3)}]), ${J(`SRMR = ${NZ(srmr, 3)}`, '으로/로')} 나타났다.`);
    const path = p => { const q = row(p.lab); return { t: p.label.replace(/^[^:]+: /, ''), s: `β = ${NZ(q.std, 3)}, ${PS(q.p)}`, sig: q.p < 0.05 }; };
    const ps = S.P.map(path), sg = ps.filter(x => x.sig), ns = ps.filter(x => !x.sig);
    const list = a => a.map(x => `${x.t} 경로(${x.s})`).join(', ');
    if (sg.length) out.push(`경로계수를 보면 ${list(sg)}는 통계적으로 유의하였${ns.length ? '고,' : '다.'}`);
    if (ns.length) out.push(`${sg.length ? '' : '경로계수를 보면 '}${list(ns)}는 유의하지 않았다.`);
    const ind = S.D.map((d, j) => ({ d, q: row(d.lab), b: bs[S.P.length + j] })).filter(x => !/^tot/.test(x.d.lab));
    if (ind.length) {
      out.push(`간접효과는 부트스트랩(${B.toLocaleString()}번) 95% 백분위 신뢰구간으로 검증하였다.`);
      ind.forEach(({ d, q, b }) => { const sig = b.lo > 0 || b.hi < 0; out.push(`${d.label.replace(/^간접효과: /, '').replace(/^총 간접효과: /, '총 간접효과(')}${/^총/.test(d.label) ? ')' : ' 간접효과'}는 B = ${F2(q.est, 3)}, β = ${NZ(q.std, 3)}, ${J(`95% CI ${CI(b.lo, b.hi, 3)}`, '으로/로')} ${sig ? '신뢰구간에 0이 포함되지 않아 유의하였다' : '신뢰구간에 0이 포함되어 유의하지 않았다'}.`); });
    }
    const r2 = res.r2 || { names: [], v: [] }, r2s = [...S.M, S.Y].map(n => { const i = r2.names.indexOf(n.id); return i < 0 ? null : `${n.name} ${NZ(r2.v[i], 3)}`; }).filter(Boolean);
    if (r2s.length) out.push(`설명된 분산(R²)은 ${J(r2s.join(', '), '이었/였')}다.`);
    out.push('[적합도 판단과 가설의 지지 여부는 연구자가 기준과 함께 서술]');
    return out.join(' ');
  }
  // R 코드: mode 'fit' = 브라우저(JSON, 부트스트랩 함수 tong_boot 정의), 'file' = analysis.R 안에서 (EFFECT 줄)
  function semR(S, mode) {
    const want = S.P.map(p => p.lab).concat(S.D.map(d => d.lab)), q = s => `"${String(s).replace(/["\\]/g, '\\$&')}"`;
    const fm = CFA_FM.ML.map(q).join(', ');
    const model = `model <- '\n${S.syntax}\n'`;
    // 재표집마다 표준오차·적합도·기저모형은 계산하지 않아 빠르게 (추정값은 같음). 경로는 coef에서, 간접·총효과는 그 곱·합으로
    const boot = `fitb <- sem(model, data = ${mode === 'fit' ? 'd' : 'dd'}, estimator = "ML", missing = "listwise", se = "none", test = "none", baseline = FALSE, h1 = FALSE)
PLAB <- c(${S.P.map(p => q(p.lab)).join(', ')})
bootfun <- function(f) { cf <- coef(f); c(unname(cf[PLAB])${S.D.length ? `, with(as.list(cf), c(${S.D.map(d => d.def).join(', ')}))` : ''}) }
tong_boot <- function(k, nb) { set.seed(${SEM_SEED} + k); b <- suppressWarnings(bootstrapLavaan(fitb, R = nb, FUN = bootfun)); b <- matrix(b, ncol = ${want.length}); b }`;
    if (mode === 'fit') return `suppressPackageStartupMessages(library(lavaan))
d <- read.csv(text = csvtxt)
${model}
warn <- character(0)
fit <- withCallingHandlers(sem(model, data = d, estimator = "ML", missing = "listwise"), warning = function(w) { warn <<- c(warn, conditionMessage(w)); invokeRestart("muffleWarning") })
${boot}
assign("tong_boot", function(k, nb) as.character(jsonlite::toJSON(unname(tong_boot(k, nb)), digits = NA, na = "null")), envir = globalenv())   # 다음 호출(100번씩)에서 씀
pe <- parameterEstimates(fit, standardized = TRUE)
r2 <- lavInspect(fit, "r2")
as.character(jsonlite::toJSON(list(lavaan = as.character(packageVersion("lavaan")), n = lavInspect(fit, "nobs"), converged = lavInspect(fit, "converged"),
  fit = as.list(unname(fitMeasures(fit, c(${fm})))), pe = list(lhs = pe$lhs, op = pe$op, rhs = pe$rhs, label = pe$label, est = pe$est, se = pe$se, z = pe$z, pvalue = pe$pvalue, lo = pe$ci.lower, hi = pe$ci.upper, std = pe$std.all),
  r2 = list(names = names(r2), v = unname(r2)), warn = warn), digits = NA, auto_unbox = TRUE, na = "null"))`;
    return { model, boot, want };
  }
  // analysis.R 안의 구조방정식 (분석할 때 저장한 모형 S.sem으로)
  function rSem(S) {
    const m = S.sem; if (!m) return '# 이 분석은 통에서 다시 계산해야 R 코드를 만들 수 있습니다.';
    const R0 = semR(m, 'file'), B = +S.opts.semBoot || 1000, cols = m.items.map(i => i.code).concat(m.obs.map(o => S.vars[o.k].code));
    return `suppressPackageStartupMessages(library(lavaan))   # 구조방정식만 lavaan 패키지가 필요합니다. 없으면 install.packages("lavaan")
u <- complete.cases(d[, ${rvec(cols)}, drop = FALSE])
dd <- data.frame(${m.items.map(i => `${i.nm} = num(${rq(i.code)})[u]`).concat(m.obs.map(o => `${o.id} = ${rX(S, o.k)}`)).join(', ')})
${R0.model}
fit <- sem(model, data = dd, estimator = "ML", missing = "listwise")
print(summary(fit, fit.measures = TRUE, standardized = TRUE))
pe <- parameterEstimates(fit, standardized = TRUE)
pr <- function(lab) pe[which(pe$label == lab)[1], ]
${m.P.map(p => `q <- pr(${rq(p.lab)}); eff(${rq(p.label)}, q$est, q$ci.lower, q$ci.upper, q$pvalue)`).join('\n')}
# 간접효과·총효과: 백분위 부트스트랩 (분석 결정: ${B.toLocaleString()}번). 통과 같은 시작값으로 ${SEM_CHUNK}번씩 돌려 같은 값이 나옴
${R0.boot}
bs <- do.call(rbind, lapply(0:${Math.ceil(B / SEM_CHUNK) - 1}, function(k) tong_boot(k, min(${SEM_CHUNK}, ${B} - k * ${SEM_CHUNK}))))
bs <- bs[complete.cases(bs), , drop = FALSE]
${m.D.map((d, j) => `q <- pr(${rq(d.lab)}); eff(${rq(d.label)}, q$est, pct(bs[, ${m.P.length + j + 1}], .025), pct(bs[, ${m.P.length + j + 1}], .975))`).join('\n')}`;
  }


// ##### 13. 자료 점검: 이상치·결측·표본 확인·측정 점검·사전 분석 (stats-ladder.html 11124~11572줄) #####
  // 자료 점검: 응답 정리(중복·이상치·결측) · 표본 확인 · 측정 점검 · 사전 분석
  // =================================================================
  const chiCrit = (a, df) => { let lo = 0, hi = 1000; for (let i = 0; i < 100; i++) { const m = (lo + hi) / 2; if (ST.pChi(m, df) > a) lo = m; else hi = m; } return (lo + hi) / 2; };
  const scName = si => sv.scales[si] ? (sv.scales[si].name || scaleCode(sv.scales[si], si)) : '';
  // 이상치: 변인 점수가 평균에서 3.29 SD 넘게 벗어남 / 변인 점수 조합이 튐(마할라노비스 거리, p < .001)
  function findOutliers(pre, items) {
    const k = sv.scales.length; if (!k || pre.length < 10) return [];
    const sc = pre.map(p => sv.scales.map((s, si) => {
      const its = items.filter(d => d.kind === 'item' && d.si === si), need = Math.ceil(its.length * (+plan.dec.minPct || 80) / 100);
      const vals = its.map(d => p.items[d.key]).filter(v => v != null);
      return its.length && vals.length && vals.length >= need ? mean(vals) : null;
    }));
    const out = new Map(), get = i => { if (!out.has(i)) out.set(i, { p: pre[i], z: [], d2: null, pD: null }); return out.get(i); };
    for (let si = 0; si < k; si++) {
      const v = sc.map(r => r[si]).filter(x => x != null); if (v.length < 10) continue;
      const m = mean(v), s = sd(v); if (!(s > 0)) continue;
      sc.forEach((r, i) => { if (r[si] == null) return; const z = (r[si] - m) / s; if (Math.abs(z) > 3.29) get(i).z.push({ si, z }); });
    }
    const cs = [...Array(k).keys()].filter(si => sc.filter(r => r[si] != null).length >= 10);
    if (cs.length >= 2) {
      const rows = sc.map((r, i) => [i, cs.map(c => r[c])]).filter(([, v]) => v.every(x => x != null));
      if (rows.length > cs.length + 5) {
        const mu = cs.map((_, j) => mean(rows.map(([, v]) => v[j])));
        const S = cs.map((_, a) => cs.map((_, b) => rows.reduce((t, [, v]) => t + (v[a] - mu[a]) * (v[b] - mu[b]), 0) / (rows.length - 1)));
        const Si = matInv(S);
        if (Si) rows.forEach(([i, v]) => {
          const dv = v.map((x, j) => x - mu[j]), d2 = dv.reduce((t, x, a) => t + x * dv.reduce((u, y, b) => u + Si[a][b] * y, 0), 0), pD = ST.pChi(d2, cs.length);
          if (pD < 0.001) { const o = get(i); o.d2 = d2; o.pD = pD; }
        });
      }
    }
    return [...out.values()].map(o => ({ ...o, id: o.p.id }));
  }
  const dataKind = () => (typeof md !== 'undefined' && md && md.dataKind) || 'main';
  function renderKind() {
    const box = $('#co-kind'); if (!box) return;
    const k = dataKind();
    box.innerHTML = `<span class="dk-l">이 응답은</span>${[['main', '본조사'], ['pilot', '예비조사 (파일럿)']].map(([v, l]) => `<label class="dk-o ${k === v ? 'on' : ''}"><input type="radio" name="co-kind" value="${v}" ${k === v ? 'checked' : ''}>${l}</label>`).join('')}
      <span class="hint">${k === 'pilot' ? '예비조사 자료는 문항과 모형을 다듬는 데만 씁니다. 계산 결과에 <b>예비조사</b> 표시가 붙고, 가설 확정 전이어도 기록에 남습니다.' : '예비조사(30~50명)를 먼저 했다면 그 자료를 넣을 때 [예비조사]를 고르세요.'}</span>`;
  }
  $('#co-kind')?.addEventListener('change', e => {
    if (e.target.name !== 'co-kind') return;
    md.dataKind = e.target.value; saveMd(); renderKind();
    if (lastScored) { lastScored.pilot = md.dataKind === 'pilot'; renderScored(); }
    addSys('collect', md.dataKind === 'pilot' ? '이 응답을 예비조사 자료로 표시했습니다.' : '이 응답을 본조사 자료로 표시했습니다.');
  });

  // ---------- 공통 ----------
  const needData = (what) => `<div class="card"><h3 class="card-title">응답을 먼저 넣어 주세요</h3>
    <p class="hint">[응답 취합·정리]에서 응답 파일을 넣고 [점수 만들기]를 누르면 ${what}을(를) 볼 수 있습니다. 응답 원자료는 저장하지 않아서, 페이지를 새로 열었다면 다시 넣어야 합니다.</p>
    <div class="actions"><button class="btn primary" data-dq-act="to-collect">응답 취합·정리로</button><button class="btn" data-dq-act="demo">예시 자료로 해 보기</button></div></div>`;
  const pilotNote = () => lastScored && lastScored.pilot ? '<p class="cf-note"><b>예비조사 자료</b>입니다. 여기서 본 결과로 문항과 모형을 다듬는 것은 괜찮지만, 본조사 결과로 보고하지 마세요.</p>' : '';
  const sampleNote = () => lastScored && lastScored.sample ? '<p class="hint"><b>예시(가짜) 자료</b>입니다. 논문에 쓰면 안 됩니다.</p>' : '';
  function markSeen(k) { if (!md.seen) md.seen = {}; if (!md.seen[k]) { md.seen[k] = true; saveMd(); renderNav(); } }
  const tblCsv = (head, rows) => toCsv([head].concat(rows));
  const tableHtml = (head, rows, num = []) => `<div class="table-wrap"><table class="compact stat-table"><thead><tr>${head.map((h, i) => `<th class="${num.includes(i) ? 'num' : ''}">${esc(h)}</th>`).join('')}</tr></thead><tbody>${rows.map(r => `<tr>${r.map((c, i) => `<td class="${num.includes(i) ? 'num' : ''}">${c}</td>`).join('')}</tr>`).join('')}</tbody></table></div>`;

  // ---------- 응답 정리 흐름 · 결측 ----------
  function flowSteps() {
    const L = lastScored, ex = L.people.filter(p => p.excluded), c = k => ex.filter(p => p.excluded === k).length;
    let n = L.people.length; const st = [{ l: '전체 응답', n }];
    [['비동의', '동의 안 함'], ['척도 응답 없음', '빈 응답'], ['대상 아님', '연구 대상 아님'], ['주의 확인 실패', '주의 확인 문항 틀림'], ['불성실 의심', '불성실 의심'], ['직접 뺌', '연구자가 직접 뺌'], ['중복', '중복 응답'], ['이상치', '이상치']].forEach(([k, l]) => { const m = c(k); if (m) { n -= m; st.push({ l, minus: m, n }); } });
    return st;
  }
  function flowHtml() {
    const st = flowSteps();
    return `<div class="dq-flow">${st.map((s, i) => `${i ? '<span class="dq-arrow">→</span>' : ''}<div class="dq-step ${i === st.length - 1 ? 'last' : ''}">${s.minus ? `<small>${esc(s.l)} −${s.minus}</small>` : `<small>${esc(s.l)}</small>`}<b>${s.n}</b></div>`).join('')}${st.length === 1 ? '<span class="dq-arrow">→</span><div class="dq-step last"><small>분석 포함</small><b>' + lastScored.inc.length + '</b></div>' : ''}</div>`;
  }
  function missingInfo() {
    const L = lastScored, its = L.items.filter(d => d.kind === 'item');
    const per = its.map(d => ({ d, n: L.inc.filter(p => p.items[d.key] == null).length }));
    const cells = its.length * L.inc.length, miss = per.reduce((s, x) => s + x.n, 0);
    const persons = L.inc.filter(p => its.some(d => p.items[d.key] == null)).length;
    const noScore = sv.scales.map((s, si) => ({ si, n: L.inc.filter(p => p.score[si] == null).length })).filter(x => x.n);
    return { pct: cells ? miss / cells * 100 : 0, miss, persons, top: per.filter(x => x.n).sort((a, b) => b.n - a.n).slice(0, 6), noScore };
  }
  function cleaningHtml() {
    const L = lastScored; if (!L) return '';
    const M = missingInfo(), O = L.outliers || [], dups = L.people.filter(p => p.dup);
    const dupOn = (plan.dec.dup || 'keep') === 'drop', outOn = (plan.dec.outlier || 'keep') === 'drop';
    return `<div class="card dq-clean"><h3 class="card-title">응답 정리 (클리닝)</h3>
      ${flowHtml()}
      <div class="dq-grid">
        <div class="dq-box"><h4>빠진 값 (결측)</h4>${M.miss ? `<p>분석에 포함된 응답의 척도 문항 중 <b>${M.pct.toFixed(1)}%</b>(${M.miss}칸)가 비어 있습니다. 하나라도 빠뜨린 사람 ${M.persons}명.</p>
          ${M.top.length ? `<p class="hint">많이 빠진 문항: ${M.top.map(x => `${esc(x.d.code)} ${x.n}명`).join(', ')}</p>` : ''}
          <p class="hint">처리: 한 변인의 문항 ${+plan.dec.minPct === 100 ? '모두' : `${plan.dec.minPct}% 이상`} 답했으면 답한 문항 평균으로 점수를 내고${M.noScore.length ? ` (점수가 없는 사람: ${M.noScore.map(x => `${esc(scName(x.si))} ${x.n}명`).join(', ')})` : ''}, 분석마다 쓰는 변인을 모두 가진 사람만 넣습니다(목록별 제외).${M.pct > 5 ? ' <b class="a-mid">빠진 값이 5%를 넘어 결과가 치우칠 수 있습니다. 다중대체(R)를 고려하세요.</b>' : ''}</p>` : '<p><b class="a-ok">빠진 값이 없습니다.</b></p><p class="hint">통이 만든 구글 폼은 척도 문항이 모두 필수 응답이라 보통 빠진 값이 없습니다.</p>'}</div>
        <div class="dq-box"><h4>중복 응답</h4>${dups.length ? `<p>모든 답이 같은 응답 <b>${dups.length}개</b> ${dupOn ? '(뺌)' : '(포함함)'}</p>
          <ul class="dq-out">${dups.slice(0, 8).map(p => `<li><b>${p.id}</b> = ${p.dup}${p.dupGap != null ? ` · ${p.dupGap < 1 ? '1분 안에' : p.dupGap < 120 ? `${Math.round(p.dupGap)}분 뒤` : p.dupGap < 2880 ? `${Math.round(p.dupGap / 60)}시간 뒤` : `${Math.round(p.dupGap / 1440)}일 뒤`} 제출${p.dupGap <= 30 ? ' <span class="a-mid">같은 사람일 가능성 큼</span>' : ''}` : ''}${p.straight ? ' · 모든 문항을 같은 번호로 답함' : ''}</li>`).join('')}${dups.length > 8 ? `<li>… 외 ${dups.length - 8}개</li>` : ''}</ul>
          <p class="hint">답이 모두 같아도 다른 사람일 수 있습니다. 제출 간격이 짧으면 같은 사람이 두 번 냈을 가능성이 큽니다. 빼기로 했다면 몇 개를 뺐는지 논문에 적으세요.</p>` : '<p><b class="a-ok">중복 응답이 없습니다.</b></p>'}
          <label class="chk"><input type="checkbox" data-dq-dec="dup" ${dupOn ? 'checked' : ''}> 뒤에 들어온 중복 응답 빼기</label></div>
        <div class="dq-box"><h4>이상치</h4>${O.length ? `<p>점수가 크게 벗어난 응답 <b>${O.length}개</b> ${outOn ? '(뺌)' : '(포함함)'}</p>
          <ul class="dq-out">${O.slice(0, 8).map(o => `<li><b>${o.id}</b> ${o.z.map(z => `${esc(scName(z.si))} z = ${F2(z.z)}`).join(', ')}${o.d2 != null ? `${o.z.length ? ' · ' : ''}마할라노비스 D² = ${F2(o.d2)}, p ${o.pD < 0.001 ? '< .001' : '= ' + NZ(o.pD, 3)}` : ''}</li>`).join('')}${O.length > 8 ? '<li>…</li>' : ''}</ul>` : '<p><b class="a-ok">기준을 넘는 이상치가 없습니다.</b></p>'}
          <label class="chk"><input type="checkbox" data-dq-dec="outlier" ${outOn ? 'checked' : ''}> 이상치 빼고 계산</label>
          <p class="hint">기준: 변인 점수 |z| > 3.29, 마할라노비스 거리 p < .001 (Tabachnick & Fidell). 실제로 극단적인 사람일 수 있어 기본은 빼지 않습니다. 뺐다면 몇 명인지 논문에 밝힙니다.</p></div>
        ${carelessBox()}${attnBox()}
      </div>
      <div class="dq-grid" style="margin-top:12px">${eligBox()}${demoFixBox()}${manualBox()}</div>
      ${cleanReportBox()}
    </div>`;
  }
  // ----- 응답 정리 상자들 -----
  const dqList = (arr, f, n = 8) => arr.length ? `<ul class="dq-out">${arr.slice(0, n).map(p => `<li><b>${p.id}</b> ${f(p)}</li>`).join('')}${arr.length > n ? `<li>… 외 ${arr.length - n}명</li>` : ''}</ul>` : '';
  const earlyOut = p => ['비동의', '척도 응답 없음', '대상 아님', '주의 확인 실패'].includes(p.excluded);
  function carelessBox() {
    const pool = lastScored.people.filter(p => !earlyOut(p));
    const c1 = pool.filter(p => p.straight), c2 = pool.filter(p => p.careless.some(c => /연속/.test(c))), c3 = pool.filter(p => p.revCon);
    const row = (id, on, label, arr, f) => `<label class="chk"><input type="checkbox" data-dq-dec="${id}" ${on ? 'checked' : ''}> ${label} <b>${arr.length}명</b> 빼기</label>${dqList(arr, f, 5)}`;
    return `<div class="dq-box"><h4>불성실 의심</h4>
      ${row('straight', plan.dec.straight === 'drop', '모든 문항 같은 번호', c1, () => '')}
      ${row('longrun', plan.dec.longrun === 'drop', `같은 번호 연속 (척도 문항 절반 이상, 최소 10문항)`, c2, p => `${p.longRun}문항 연속`)}
      ${row('revcon', plan.dec.revcon === 'drop', '정문항·역문항 모두 강하게 동의(반대)', c3, p => p.careless.filter(c => /역문항/.test(c)).map(c => c.replace(' 역문항 모순', '')).join(', '))}
      <p class="hint">모두 기본은 빼지 않고 보여 줍니다. 실제로 그렇게 느낀 사람일 수도 있으니, 뺐다면 기준과 인원을 논문에 적으세요(예: Curran, 2016의 long-string 기준).</p></div>`;
  }
  function attnBox() {
    if (!attnItems().length) return `<div class="dq-box"><h4>주의 확인 문항</h4><p class="hint">설문에 주의 확인 문항이 없습니다. 다음 조사부터는 [설문 설계]에서 넣을 수 있습니다("이 문항에는 '그렇다'를 골라 주세요").</p></div>`;
    const fail = lastScored.people.filter(p => p.attnFail && !['비동의', '척도 응답 없음', '대상 아님'].includes(p.excluded)), on = (plan.dec.attn || 'drop') === 'drop';
    return `<div class="dq-box"><h4>주의 확인 문항</h4><p>정답을 고르지 않은 응답 <b>${fail.length}명</b> ${on ? '(뺌)' : '(포함함)'}</p>
      ${dqList(fail, p => p.attn.filter(a => !a.ok).map(a => `${a.code}: '${esc(a.ans || '빈 칸')}'`).join(', '))}
      <label class="chk"><input type="checkbox" data-dq-dec="attn" ${on ? 'checked' : ''}> 주의 확인 문항을 틀린 응답 빼기</label></div>`;
  }
  // 연구 대상 조건: 기본 정보로 대상이 아닌 사람 거르기
  function eligBox() {
    const ds = sv.demos.map((d, di) => ({ d, di })).filter(x => norm(x.d.text) && x.d.type !== 'text');
    const out = lastScored.people.filter(p => p.excluded === '대상 아님').length;
    const vals = di => [...new Set(lastScored.people.map(p => p.items[`d${di}`]).filter(v => v !== '' && v != null))];
    const desc = c => { const d = sv.demos[c.di]; return d ? (c.type === 'num' ? `${esc(d.text)} ${c.min !== '' && c.min != null ? `${esc(c.min)} 이상` : ''}${c.max !== '' && c.max != null ? ` ${esc(c.max)} 이하` : ''}` : `${esc(d.text)}: ${c.keep.map(esc).join(', ')}만`) : '(지워진 문항)'; };
    const sel = eligDraft.di != null && sv.demos[eligDraft.di] ? eligDraft.di : (ds[0] || {}).di;
    const dsel = sel != null ? sv.demos[sel] : null;
    return `<div class="dq-box"><h4>연구 대상 조건</h4>
      ${(plan.elig || []).length ? `<ul class="dq-out">${plan.elig.map((c, k) => `<li>${desc(c)} <button class="linkish" data-dq-act="elig-del" data-k="${k}">지우기</button></li>`).join('')}</ul><p>조건에 맞지 않아 뺀 응답 <b>${out}명</b></p>` : '<p class="hint">연구 대상이 정해져 있으면(예: 대학생만, 만 19세 이상) 기본 정보로 거릅니다.</p>'}
      ${ds.length ? `<div class="dq-elig">
        <select data-dq-elig="di">${ds.map(x => `<option value="${x.di}" ${x.di === sel ? 'selected' : ''}>${esc(x.d.text)}</option>`).join('')}</select>
        ${dsel && dsel.type === 'number' ? `<input data-dq-elig="min" inputmode="numeric" placeholder="이상" value="${esc(eligDraft.min ?? '')}"><input data-dq-elig="max" inputmode="numeric" placeholder="이하" value="${esc(eligDraft.max ?? '')}">`
          : `<span class="dq-keep">${vals(sel).map(v => `<label class="chk"><input type="checkbox" data-dq-elig="keep" value="${esc(v)}" ${(eligDraft.keep || []).includes(v) ? 'checked' : ''}> ${esc(v)}</label>`).join('')}</span>`}
        <button class="btn sm" data-dq-act="elig-add">${dsel && dsel.type === 'number' ? '범위 조건 추가' : '고른 값만 남기기'}</button></div>` : '<p class="hint">기본 정보 문항이 없습니다.</p>'}</div>`;
  }
  let eligDraft = {};
  // 기본 정보 값 정리: 숫자로 바꾼 값, 선택지에 없는 값, 인원이 적은 범주 합치기
  function demoFixBox() {
    const L = lastScored, rows = [];
    sv.demos.forEach((d, di) => {
      if (!norm(d.text) || d.type === 'text') return;
      const fx = L.demoFix.filter(f => f.di === di);
      if (d.type === 'number') { fx.forEach(f => rows.push(`<li>${esc(d.text)}: "${esc(f.from)}" → ${f.how === 'bad' ? '<span class="a-low">숫자로 못 읽어 빈 값</span>' : `<b>${esc(f.to)}</b>`} (${f.n}명)</li>`)); return; }
      const info = L.demoInfo.find(x => x.di === di), small = info && info.kind === 'cat' ? info.freq.filter(([v, n]) => n < 5 && v !== '') : [];
      const targets = [...new Set([...(d.options || []), ...((info && info.freq) || []).map(([v]) => v)].filter(Boolean))];
      const pick = (from, cur) => `<select data-dq-recode="${di}" data-from="${esc(from)}"><option value="__keep" ${cur === from ? 'selected' : ''}>그대로 (${esc(from)})</option>${targets.filter(t => t !== from).map(t => `<option value="${esc(t)}" ${cur === t ? 'selected' : ''}>→ ${esc(t)}</option>`).join('')}<option value="" ${cur === '' ? 'selected' : ''}>→ 빈 값(결측)</option></select>`;
      fx.forEach(f => rows.push(`<li>${esc(d.text)}: "${esc(f.from)}" ${f.n}명 ${f.how === 'auto' ? '<span class="hint">(같은 뜻이라 자동으로 바꿈)</span>' : f.how === 'other' ? '<span class="a-mid">선택지에 없는 값</span>' : ''} ${pick(f.from, f.to)}</li>`));
      small.filter(([v]) => !fx.some(f => f.to === v && f.from === v)).forEach(([v, n]) => rows.push(`<li>${esc(d.text)}: "${esc(v)}" <span class="a-mid">${n}명뿐</span> ${pick(v, v)}</li>`));
    });
    return `<div class="dq-box"><h4>기본 정보 값 정리</h4>${rows.length ? `<ul class="dq-out">${rows.join('')}</ul><p class="hint">인원이 5명보다 적은 범주는 집단 비교에서 막히기 쉬워, 이론적으로 가까운 범주와 합치는 것을 검토하세요(예: 대학원생 → 대학생). 합친 기준은 논문에 적습니다.</p>` : '<p><b class="a-ok">정리할 값이 없습니다.</b></p><p class="hint">숫자 문항의 "23세", "스물셋" 같은 값은 숫자로 바꾸고, 선택지에 없는 값과 인원이 적은 범주를 여기에 보여 줍니다.</p>'}</div>`;
  }
  function manualBox() {
    const M = Object.entries(plan.manual || {});
    return `<div class="dq-box"><h4>직접 빼기</h4>
      ${M.length ? `<ul class="dq-out">${M.map(([id, why]) => `<li><b>${esc(id)}</b> ${esc(why || '(이유 없음)')} <button class="linkish" data-dq-act="manual-del" data-id="${esc(id)}">되돌리기</button></li>`).join('')}</ul>` : '<p class="hint">위 기준에 걸리지 않지만 빼야 할 응답(예: 자유 응답에 장난으로 쓴 것)을 응답 번호로 뺍니다. 이유는 기록에 남습니다.</p>'}
      <div class="dq-elig"><input data-dq-man="id" list="dq-ids" placeholder="응답 번호 (예: R012)" style="width:9em"><input data-dq-man="why" placeholder="빼는 이유"><button class="btn sm" data-dq-act="manual-add">빼기</button></div>
      <datalist id="dq-ids">${lastScored.people.filter(p => !p.excluded).map(p => `<option value="${p.id}">`).join('')}</datalist></div>`;
  }
  // 논문 '연구 대상' 절에 쓸 문장
  function cleanReport() {
    const L = lastScored, c = k => L.people.filter(p => p.excluded === k).length, parts = [];
    [['비동의', '연구 참여에 동의하지 않은'], ['척도 응답 없음', '척도 문항에 응답하지 않은'], ['대상 아님', '연구 대상 조건에 맞지 않는'], ['주의 확인 실패', '주의 확인 문항에 정답을 고르지 않은'], ['불성실 의심', '불성실 응답으로 판단된'], ['직접 뺌', '연구자가 검토해 부적절하다고 판단한'], ['중복', '중복 응답으로 판단된'], ['이상치', '이상치로 판단된']].forEach(([k, l]) => { if (c(k)) parts.push(`${l} ${c(k)}명`); });
    const crit = [];
    if ((plan.elig || []).length) crit.push(`연구 대상 조건(${plan.elig.map(x => { const d = sv.demos[x.di]; return d ? (x.type === 'num' ? `${d.text} ${x.min !== '' && x.min != null ? `${x.min} 이상` : ''}${x.max !== '' && x.max != null ? ` ${x.max} 이하` : ''}` : `${d.text}: ${x.keep.join('·')}`) : ''; }).filter(Boolean).join(', ')})`);
    if (attnItems().length && (plan.dec.attn || 'drop') === 'drop') crit.push(`설문 중간에 넣은 주의 확인 문항 ${attnItems().length}개`);
    const cr = [plan.dec.straight === 'drop' && '모든 문항에 같은 번호로 답한 경우', plan.dec.longrun === 'drop' && '척도 문항의 절반 이상(최소 10문항)을 연달아 같은 번호로 답한 경우', plan.dec.revcon === 'drop' && '같은 변인의 정문항과 역문항에 모두 강하게 동의하거나 반대한 경우'].filter(Boolean);
    if (cr.length) crit.push(`불성실 응답 기준(${cr.join(', ')})`);
    if ((plan.dec.dup || 'keep') === 'drop') crit.push('모든 응답이 같은 중복 응답');
    if ((plan.dec.outlier || 'keep') === 'drop') crit.push('변인 점수 |z| > 3.29 또는 마할라노비스 거리 p < .001인 이상치');
    return `자료는 총 ${L.people.length}명에게서 수집되었다. ${parts.length ? `이 중 ${parts.join(', ')}을 제외하고, ` : ''}최종 ${L.inc.length}명의 자료를 분석에 사용하였다.${crit.length ? ` 자료 정제에는 ${crit.join(', ')}을 기준으로 삼았다.` : ''}`;
  }
  const cleanReportBox = () => `<div class="dq-report"><h4>논문 '연구 대상' 절에 쓸 문장</h4><p>${esc(cleanReport())}</p><div class="actions tight"><button class="btn sm" data-dq-act="copy-report">문장 복사</button></div><p class="hint">정리 결과가 바뀌면 문장도 바뀝니다. 학교 양식과 문체에 맞게 다듬어 쓰세요.</p></div>`;
  // 정리 기준을 바꾸면 기록에 남기고 다시 계산
  function cleanChange(q, from, to) {
    plan.log.push({ at: new Date().toLocaleString('ko-KR'), id: 'clean', q, from, to, after: !!plan.seen, reason: '' });
    savePlan(); addSys('analysis', `응답 정리: ${q} → ${to}${plan.seen ? ' (결과를 본 뒤 바꿈: 사후 변경으로 기록)' : ''}`);
    runScoring();
  }
  $('#co-result').addEventListener('change', e => {
    const t = e.target;
    if (t.dataset.dqElig) {
      const k = t.dataset.dqElig;
      if (k === 'di') eligDraft = { di: +t.value };
      else if (k === 'keep') { const set = new Set(eligDraft.keep || []); t.checked ? set.add(t.value) : set.delete(t.value); eligDraft.keep = [...set]; return; }
      else { eligDraft[k] = t.value; return; }
      renderScored(); return;
    }
    if (t.dataset.dqRecode != null) {
      const di = +t.dataset.dqRecode, from = t.dataset.from, d = sv.demos[di]; plan.recode = plan.recode || {}; const m = plan.recode[di] = plan.recode[di] || {};
      const before = Object.prototype.hasOwnProperty.call(m, from) ? (m[from] || '빈 값') : from;
      if (t.value === '__keep') m[from] = from; else m[from] = t.value;
      cleanChange(`${d.text} 값 "${from}" 바꾸기`, before, t.value === '__keep' ? from : (t.value || '빈 값')); return;
    }
    if (!t.dataset.dqDec) return;
    const k = t.dataset.dqDec; changeDecision(k, t.checked ? 'drop' : 'keep');
  });
  $('#co-result').addEventListener('click', e => {
    const b = e.target.closest('[data-dq-act]'); if (!b) return;
    const act = b.dataset.dqAct; plan.elig = plan.elig || []; plan.manual = plan.manual || {};
    if (act === 'elig-add') {
      const di = eligDraft.di != null && sv.demos[eligDraft.di] ? eligDraft.di : sv.demos.findIndex(d => norm(d.text) && d.type !== 'text'), d = sv.demos[di]; if (!d) return;
      if (d.type === 'number') { const min = norm(eligDraft.min || ''), max = norm(eligDraft.max || ''); if (!min && !max) { toast('이상·이하 중 하나는 적어 주세요.'); return; } plan.elig.push({ di, type: 'num', min, max }); cleanChange('연구 대상 조건 추가', '-', `${d.text} ${min ? min + ' 이상' : ''}${max ? ' ' + max + ' 이하' : ''}`); }
      else { const keep = eligDraft.keep || []; if (!keep.length) { toast('남길 값을 하나 이상 골라 주세요.'); return; } plan.elig.push({ di, type: 'cat', keep }); cleanChange('연구 대상 조건 추가', '-', `${d.text}: ${keep.join(', ')}만`); }
      eligDraft = {};
    }
    else if (act === 'elig-del') { const c = plan.elig.splice(+b.dataset.k, 1)[0]; cleanChange('연구 대상 조건 지우기', c ? (sv.demos[c.di] || {}).text || '-' : '-', '없음'); }
    else if (act === 'manual-add') {
      const id = norm($('[data-dq-man=id]').value).toUpperCase(), why = norm($('[data-dq-man=why]').value);
      if (!lastScored.people.some(p => p.id === id)) { toast('그런 응답 번호가 없습니다. 예: R012'); return; }
      if (!why) { toast('빼는 이유를 적어 주세요. 논문과 기록에 남습니다.'); return; }
      plan.manual[id] = why; cleanChange(`응답 ${id} 직접 빼기`, '포함', `뺌 (${why})`);
    }
    else if (act === 'manual-del') { const id = b.dataset.id; delete plan.manual[id]; cleanChange(`응답 ${id} 직접 빼기 되돌리기`, '뺌', '포함'); }
    else if (act === 'copy-report') { const txt = cleanReport(); try { navigator.clipboard.writeText(txt).then(() => toast('문장을 복사했습니다.'), () => showSheet('연구 대상 문장', txt)); } catch (err) { showSheet('연구 대상 문장', txt); } }
  });

  // ---------- 표본 확인 ----------
  function demoTable() {
    const L = lastScored, rows = [];
    L.demoInfo.forEach(x => {
      if (x.kind === 'num') rows.push([esc(x.dm.text.replace(/\s*\(.*\)$/, '')), 'M (SD)', `${F2(x.m)} (${F2(x.s)})`, x.n ? `${x.min}~${x.max}` : '-']);
      else x.freq.forEach(([k, c], j) => rows.push([j === 0 ? `<b>${esc(x.dm.text.replace(/\s*\(.*\)$/, ''))}</b>` : '', esc(k), String(c), (c / x.n * 100).toFixed(1)]));
    });
    return rows;
  }
  function renderSample() {
    const box = $('#dq-sample'); if (!box) return;
    if (!lastScored) { box.innerHTML = needData('표본'); return; }
    const L = lastScored, N = L.inc.length, pr = md.pwResult;
    const groups = planItems().filter(p => /_t$|oneway|anova|ancova|pointbiserial|chisq/.test(p.result)).map(p => {
      const v = itemVars(p), dm = L.demoInfo.find(x => x.kind === 'cat' && v.X.some(n => sameName(x.dm.text, n)));
      return dm ? { p, dm } : null;
    }).filter(Boolean);
    box.innerHTML = `${pilotNote()}${sampleNote()}
      <div class="card"><h3 class="card-title">응답 정리 흐름</h3>${flowHtml()}
        ${pr ? `<div class="dq-target ${N >= pr.N ? 'ok' : 'no'}"><b>${N >= pr.N ? '✓' : '!'}</b><div>계획한 최소 인원 <b>${pr.N}명</b> (모집 목표 ${pr.recruit}명) · 지금 분석 포함 <b>${N}명</b>${N >= pr.N ? '으로 최소 인원을 채웠습니다.' : `으로 <b>${pr.N - N}명</b> 모자랍니다. 검정력이 계획보다 낮아져서, 효과가 있어도 유의하게 나오지 않을 수 있습니다.`}</div></div>`
          : '<p class="hint">표본 수 계산을 하지 않아 목표 인원과 비교하지 못했습니다. <button class="linkish" data-nav-to="power">표본 수 계산으로</button></p>'}
      </div>
      ${groups.length ? `<div class="card"><h3 class="card-title">집단별 인원</h3>${groups.map(({ p, dm }) => {
        const mn = Math.min(...dm.freq.map(f => f[1])), mx = Math.max(...dm.freq.map(f => f[1]));
        return `<p><b>${hLabel(p)}</b> ${esc(dm.dm.text)}: ${dm.freq.map(([k, c]) => `${esc(k)} ${c}명`).join(' · ')}${mn < 20 ? ' <span class="a-low">한 집단이 20명보다 적어 검정력이 낮습니다.</span>' : mx > mn * 2 ? ' <span class="a-mid">집단 크기 차이가 커서 등분산 가정을 꼭 확인하세요.</span>' : ''}</p>`;
      }).join('')}</div>` : ''}
      <div class="card"><h3 class="card-title">표 1. 연구 대상자의 일반적 특성 <small class="hint">(N = ${N})</small></h3>
        ${L.demoInfo.length ? tableHtml(['구분', '범주', '빈도(명) / 값', '비율(%) / 범위'], demoTable(), [2, 3]) + '<div class="actions tight"><button class="btn sm" data-dq-act="t1-csv">표 1 CSV</button></div>' : '<p class="hint">설문에 기본 정보 문항(성별, 나이 등)이 없습니다.</p>'}
      </div>`;
    markSeen('sample');
  }

  // ---------- 측정 점검 ----------
  let efaOpt = { k: 'scales', ex: 'paf' };
  // 원척도 α (설문 만들기에 적은 값, ".87"·"0.87" 모두)
  function origAlphaOf(s) { const v = parseFloat(String(s.origAlpha || '').replace(/\s/g, '').replace(/^\./, '0.')); return isFinite(v) && v > 0 && v <= 1 ? v : null; }
  // 논문 측정 도구 절 문장 (변인마다)
  function measureSentences() {
    const L = lastScored; if (!L) return [];
    return L.scaleInfo.filter(x => x.its.length).map(x => {
      const s = x.s, nm = s.name || scaleCode(s, x.si), oa = origAlphaOf(s), nRev = s.items.filter(i => norm(i.text) && i.reverse).length;
      const src = norm(s.source) ? `${s.source}의 척도` : '[출처를 적어 주세요]의 척도';
      const was = v => { const t = NZ(v); return t + ('013678'.includes(t.slice(-1)) ? '이었' : '였'); };   // 숫자 읽는 소리에 맞춘 '이었/였' (.91 → 구일 → 이었)
      return `${J(nm, '은/는')} ${J(src, '을/를')} 사용하여 측정하였다. 총 ${x.its.length}문항${norm(s.subf) ? `(${s.subf})` : ''}이며, ${s.points}점 척도(1 = ${s.labels[0] || ''}, ${s.points} = ${s.labels[s.points - 1] || ''})로 응답하게 하였다. 점수는 문항 ${sv.scoring === 'sum' ? '합계' : '평균'}으로 구하였고 점수가 높을수록 ${J(nm, '이/가')} 높음을 의미한다.${nRev ? ` 역문항 ${nRev}개는 역채점하였다.` : ''} ${oa != null ? `신뢰도(Cronbach's α)는 원 연구${norm(s.origSample) ? `(${s.origSample})` : ''}에서 ${was(oa)}으며, 본 연구에서는 ${was(x.a)}다.` : `본 연구에서 신뢰도(Cronbach's α)는 ${was(x.a)}다.`}`;
    });
  }
  // ---------- 고른 기법에 필요한 준비: 확정한 가설(없으면 지금 고른 분석)의 기법마다 ⑧ 측정 점검·⑨ 사전 분석에서 할 일 ----------
  const PREP_FAM = {
    sem: ['orig', 'rel', 'cfa', 'htmt', 'n200', 'corr'],
    reg: ['orig', 'rel', 'efa', 'corr', 'vif', 'norm'],
    corr: ['orig', 'rel', 'corr'],
    group: ['orig', 'rel', 'norm', 'levene'],
    cat: ['orig']
  };
  const famOf = m => /^sem/.test(m) ? 'sem' : ['regression', 'hier', 'partial', 'med_parallel', 'med_serial', 'moderation', 'modmed7', 'modmed14', 'logistic'].includes(m) ? 'reg'
    : ['pearson', 'spearman', 'pointbiserial'].includes(m) ? 'corr' : ['onesample', 'ind_t', 'paired_t', 'oneway', 'factorial', 'ancova', 'rm_anova', 'mixed_anova'].includes(m) ? 'group' : 'cat';
  function prepItems() {
    let methods = [...new Set(planItems().map(p => LADDER_TO_METHOD[p.result]).filter(Boolean))];
    if (!methods.length && anSel && anSel.method) methods = [anSel.method];
    if (!methods.length) return null;
    const L = lastScored, fams = [...new Set(methods.map(famOf))], keys = [...new Set(fams.flatMap(f => PREP_FAM[f]))];
    const semNeed = fams.includes('sem'), multi = sv.scales.filter(x => x.items.filter(i => norm(i.text)).length >= 2);
    let cfa = null; try { cfa = cfaRes && cfaRes.dv === dataVersion ? cfaRes : null; } catch (e) { cfa = null; }
    const D = {
      orig: () => { const miss = sv.scales.filter(x => x.items.some(i => norm(i.text)) && (!norm(x.source) || origAlphaOf(x) == null)); return { t: '원척도 정보 (출처·원 연구 신뢰도)', why: '척도를 고르기 전에 원 논문에서 신뢰도와 요인 구조를 확인하고 적습니다. 측정 도구 절에 씁니다.', go: 'survey', st: !sv.scales.length ? 'todo' : miss.length ? 'warn' : 'ok', note: miss.length ? `비어 있음: ${miss.map(x => x.name || '').join(', ')}` : '' }; },
      rel: () => { const low = L ? L.scaleInfo.filter(x => x.its.length > 1 && isFinite(x.a) && x.a < 0.7) : []; return { t: '신뢰도 (크론바흐 α)', why: '모든 척도에 필요합니다. .70 이상이면 받아들일 만합니다.', go: 'measure', st: !L ? 'todo' : low.length ? 'warn' : 'ok', note: !L ? '응답을 넣으면 저절로 계산됩니다.' : low.length ? `.70 미만: ${low.map(x => x.s.name).join(', ')}` : '저절로 계산됨' }; },
      efa: () => ({ t: '탐색적 요인분석', why: '문항이 원래 척도대로 묶이는지 봅니다. 회귀·매개·조절 논문에서 흔히 보고합니다.', go: 'measure', st: !L ? 'todo' : 'auto', note: !L ? '' : '저절로 계산됨 — 묶임을 확인하세요' }),
      cfa: () => ({ t: '확인적 요인분석', why: '구조방정식은 측정모형이 맞아야 구조모형을 믿을 수 있어 꼭 먼저 합니다.', go: 'measure', req: true, st: !L ? 'todo' : !cfa ? 'todo' : cfa.fit[3] < 0.9 || cfa.fit[5] > 0.08 ? 'warn' : 'ok', note: !L ? '' : !cfa ? '[확인적 요인분석 하기]를 누르세요.' : `CFI ${NZ(cfa.fit[3], 3)}, RMSEA ${NZ(cfa.fit[5], 3)}` }),
      htmt: () => { const bad = cfa ? cfa.htmt.flatMap((r, i) => r.map((v, j) => j > i && v >= 0.85 ? `${cfa.fac[i].label}–${cfa.fac[j].label}` : null)).filter(Boolean) : []; return { t: '판별타당도 (HTMT·Fornell–Larcker)', why: '잠재변인끼리 서로 다른 것을 재는지 봅니다. 확인적 요인분석에 함께 나옵니다.', go: 'measure', st: !cfa ? 'todo' : bad.length ? 'warn' : 'ok', note: !cfa ? '확인적 요인분석을 하면 나옵니다.' : bad.length ? `HTMT .85 이상: ${bad.join(', ')}` : 'HTMT 모두 .85 미만' }; },
      n200: () => ({ t: '표본 200명 이상', why: '구조방정식은 흔히 200명 이상을 권합니다.', go: 'sample', st: !L ? 'todo' : L.inc.length >= 200 ? 'ok' : 'warn', note: L ? `지금 ${L.inc.length}명` : '' }),
      corr: () => ({ t: '상관표 (표 2)', why: '주요 변인의 평균·표준편차·상관. 가설 검증 전에 관계를 먼저 봅니다.', go: 'prelim', st: !L ? 'todo' : 'auto', note: L ? '저절로 계산됨' : '' }),
      vif: () => ({ t: '다중공선성 (VIF)', why: '예측변인끼리 너무 겹치면 계수가 불안정해집니다. 10 이상이면 심각.', go: 'prelim', st: !L ? 'todo' : 'auto', note: L ? '저절로 계산됨' : '' }),
      norm: () => ({ t: '정규성 (왜도·첨도)', why: '모수 검정의 가정입니다. |왜도| < 2, |첨도| < 7이면 크게 벗어나지 않음.', go: 'prelim', st: !L ? 'todo' : 'auto', note: L ? '저절로 계산됨' : '' }),
      levene: () => ({ t: '등분산성 (Levene)', why: '집단 비교의 가정입니다. 결과마다 보고 체크리스트에서 자동으로 점검합니다.', go: 'analysis', st: !L ? 'todo' : 'auto', note: '계산할 때 저절로 점검' })
    };
    return { methods, items: keys.map(k => ({ k, ...D[k]() })), semNeed, multi };
  }
  function prepCard(here) {
    const P = prepItems(); if (!P) return '';
    const ST = { ok: '<span class="st auto">했음</span>', auto: '<span class="st auto">저절로</span>', warn: '<span class="st manual">확인 필요</span>', todo: '<span class="st missing">아직</span>' };
    const VN = { survey: '② 설문 만들기', sample: '⑦ 표본 확인', measure: '⑧ 측정 점검', prelim: '⑨ 사전 분석', analysis: '⑩ 분석' };
    const todo = P.items.filter(i => i.st === 'todo' || i.st === 'warn').length;
    return `<div class="card prep-card"><h3 class="card-title">고른 기법에 필요한 준비 <small class="hint">(${P.methods.map(m => METHODS[m] ? METHODS[m].name.replace(/ \(.*$/, '') : m).join(', ')})</small></h3>
      <div class="prep-list">${P.items.map(i => `<div class="prep-row"><div class="prep-h"><b>${esc(i.t)}</b>${i.req ? ' <span class="st missing">필수</span>' : ''}</div>
        <div class="prep-st">${ST[i.st]}${i.go === here ? '' : `<button class="btn sm" data-prep-go="${i.go}">${VN[i.go]}</button>`}</div>
        <div class="prep-why">${esc(i.why)}${i.note ? ` <b>${esc(i.note)}</b>` : ''}</div></div>`).join('')}</div>
      <p class="hint">${todo ? `확인할 것이 ${todo}개 있습니다. ` : '준비가 다 됐습니다. '}가설을 확정했으면 그 기법들, 아니면 지금 고른 분석 기준입니다.</p></div>`;
  }
  document.addEventListener('click', e => { const g = e.target.closest('[data-prep-go]'); if (g) gotoNav(g.dataset.prepGo); });
  $('#dq-measure').addEventListener('click', e => {
    const nt = e.target.closest('[data-nav-to]'); if (nt) { e.preventDefault(); gotoNav(nt.dataset.navTo); return; }
    if (e.target.closest('[data-dq-act="copy-measure"]')) { const txt = measureSentences().join('\n\n'); try { navigator.clipboard.writeText(txt).then(() => toast('문장을 복사했습니다.'), () => showSheet('측정 도구 문장', txt)); } catch (err) { showSheet('측정 도구 문장', txt); } }
  });
  function renderMeasure() {
    const box = $('#dq-measure'); if (!box) return;
    if (!lastScored) { box.innerHTML = needData('신뢰도와 타당도'); return; }
    const L = lastScored;
    const relRows = L.scaleInfo.map(x => {
      const bad = x.itemStats.filter(it => (isFinite(it.r) && it.r < 0.3) || (isFinite(it.aDel) && isFinite(x.a) && it.aDel > x.a + 0.005));
      return [`<b>${esc(x.s.name || scaleCode(x.s, x.si))}</b>`, String(x.its.length), String(x.nComplete), `<span class="${aClass(x.a)}">${fr(x.a)}</span>`, !isFinite(x.a) ? '-' : x.a >= 0.7 ? '<span class="a-ok">좋음</span>' : x.a >= 0.6 ? '<span class="a-mid">낮은 편</span>' : '<span class="a-low">낮음</span>', bad.length ? `<span class="a-mid">${bad.map(it => esc(it.d.code)).join(', ')}</span>` : '-'];
    });
    // 요인분석: 모든 척도 문항, 모두 답한 사람
    const its = L.items.filter(d => d.kind === 'item'), comp = L.inc.filter(p => its.every(d => p.items[d.key] != null));
    let E = null, efaErr = '';
    if (its.length < 3) efaErr = '문항이 3개보다 적어 요인분석을 할 수 없습니다.';
    else if (comp.length < its.length + 5) efaErr = `모든 문항에 답한 사람(${comp.length}명)이 문항 수(${its.length})에 비해 너무 적습니다.`;
    else {
      const cols = its.map(d => comp.map(p => p.items[d.key]));
      if (cols.some(c => !(sd(c) > 0))) efaErr = '응답이 모두 같은 문항이 있어 요인분석을 할 수 없습니다.';
      else E = efa(cols, efaOpt.k === 'eig' ? null : sv.scales.filter(s => s.items.some(i => norm(i.text))).length, efaOpt.ex);
    }
    let efaHtml = efaErr ? `<p class="hint">${esc(efaErr)}</p>` : '';
    if (E) {
      // 요인마다 가장 잘 맞는 척도 이름
      const facName = E.L[0].map((_, j) => { let best = -1, nm = ''; sv.scales.forEach((s, si) => { const ix = its.map((d, i) => d.si === si ? i : -1).filter(i => i >= 0); if (!ix.length) return; const m = mean(ix.map(i => Math.abs(E.L[i][j]))); if (m > best) { best = m; nm = s.name || scaleCode(s, si); } }); return nm; });
      const home = its.map((d, i) => { const row = E.L[i].map(Math.abs), top = row.indexOf(Math.max(...row)); return { top, ok: facName[top] === (sv.scales[d.si].name || scaleCode(sv.scales[d.si], d.si)), cross: row.filter(v => v >= 0.4).length > 1, low: Math.max(...row) < 0.4 }; });
      const issues = its.map((d, i) => ({ d, h: home[i] })).filter(x => !x.h.ok || x.h.cross || x.h.low);
      const head = ['문항', ...facName.map((n, j) => `요인 ${j + 1}${n ? ` (${n})` : ''}`)];
      const rows = its.map((d, i) => [`<span title="${esc(d.text)}">${esc(d.code)} <small>${esc(d.text.slice(0, 12))}${d.text.length > 12 ? '…' : ''}</small></span>`, ...E.L[i].map((v, j) => `<span class="${Math.abs(v) >= 0.4 ? (j === home[i].top ? 'ld-hi' : 'ld-x') : 'ld-lo'}">${NZ(v)}</span>`)]);
      rows.push([`<b>${E.ex === 'paf' ? '회전 후 제곱합 적재량' : '고유값 (회전 후)'}</b>`, ...E.ss.map(v => F2(v))], ['<b>설명 분산 (%)</b>', ...E.pct.map(v => v.toFixed(2))], ['<b>누적 (%)</b>', ...E.pct.map((_, j) => E.pct.slice(0, j + 1).reduce((s, v) => s + v, 0).toFixed(2))]);
      efaHtml = `<div class="dq-kv"><span>KMO</span><b class="${E.kmo >= 0.7 ? 'a-ok' : E.kmo >= 0.6 ? 'a-mid' : 'a-low'}">${NZ(E.kmo, 3)}</b><span>Bartlett 구형성 검정</span><b>χ²(${E.bart.df}) = ${F2(E.bart.chi)}, ${PS(E.bart.p)}</b><span>추출</span><b>${E.ex === 'paf' ? `주축요인추출 (PAF)${E.conv ? ` <small>${E.iter}번 반복해 수렴</small>` : ' <small class="a-low">50번 반복해도 수렴하지 않음</small>'}` : '주성분 분석 (PCA)'}</b><span>요인 수</span><b>${E.nf}개 <small>(고유값 1 이상: ${E.nEig1}개)</small></b><span>분석한 사람</span><b>${E.n}명 (모든 문항에 답한 사람)</b></div>
        <div class="actions tight"><label class="f pw-f" style="flex-direction:row;align-items:center;gap:8px">추출 방법<select data-dq-efa-ex><option value="paf" ${E.ex === 'paf' ? 'selected' : ''}>주축요인추출 (PAF, 권장)</option><option value="pca" ${E.ex === 'pca' ? 'selected' : ''}>주성분 분석 (PCA, SPSS 기본값)</option></select></label><label class="f pw-f" style="flex-direction:row;align-items:center;gap:8px">요인 수<select data-dq-efa><option value="scales" ${efaOpt.k === 'scales' ? 'selected' : ''}>척도 수만큼</option><option value="eig" ${efaOpt.k === 'eig' ? 'selected' : ''}>고유값 1 이상</option></select></label></div>
        ${tableHtml(head, rows, head.map((_, i) => i).slice(1))}
        ${E.heywood ? '<p class="hint a-low">⚠ 공통성이 1에 이른 문항이 있습니다(Heywood 사례). 요인 수가 너무 많거나 문항끼리 거의 같은 경우입니다. 요인 수를 줄여 보세요.</p>' : ''}
        <p class="hint">${E.ex === 'paf' ? '주축요인추출, 베리맥스 회전. 문항들이 함께 나누는 분산(공통성)만으로 요인을 찾는 진짜 요인분석입니다. 논문에는 "주축요인추출과 베리맥스 회전을 사용한 탐색적 요인분석"으로 적습니다.' : '주성분 분석, 베리맥스 회전. 엄밀히는 요인분석이 아니라 분산을 요약하는 방법이라 부하량이 PAF보다 크게 나옵니다. 논문에는 "주성분 분석"으로 정확히 적으세요.'} 굵은 값은 그 문항이 가장 크게 묶인 요인(|부하량| ≥ .40), 주황색은 다른 요인에도 .40 이상 걸친 값입니다. KMO .70 이상, Bartlett p < .05면 요인분석에 알맞은 자료로 봅니다.</p>
        ${issues.length ? `<div class="dq-issue"><b>살펴볼 문항 ${issues.length}개</b><ul>${issues.map(x => `<li>${esc(x.d.code)}: ${!x.h.ok ? `원래 척도가 아닌 '${esc(facName[x.h.top])}' 요인에 가장 크게 묶임` : x.h.low ? '어느 요인에도 .40 이상으로 묶이지 않음' : '두 요인 이상에 걸침'}</li>`).join('')}</ul><p class="hint">문항을 빼는 것은 이론적 근거가 있을 때만 하고, 뺐다면 논문에 적어야 합니다. 원척도의 요인 구조를 확인하려면 확인적 요인분석(R의 lavaan)을 씁니다.</p></div>` : '<p><b class="a-ok">모든 문항이 원래 척도의 요인에 .40 이상으로 묶였습니다.</b></p>'}
        <div class="dq-harman"><b>공통방법편의 (Harman 단일요인 검정)</b> 회전하지 않은 첫 요인이 전체 분산의 <b>${E.first.toFixed(1)}%</b>를 설명합니다. ${E.first < 50 ? '<span class="a-ok">50%보다 작습니다.</span> 다만 이 검정은 민감도가 낮아, 이것만으로 공통방법편의가 없다고 단정할 수는 없습니다(Podsakoff et al., 2003). 한계로 함께 적으세요.' : '<span class="a-low">50%를 넘어 공통방법편의를 의심할 수 있습니다.</span>'}</div>`;
    }
    const hasOrig = L.scaleInfo.some(x => origAlphaOf(x.s) != null);
    if (hasOrig) relRows.forEach((r, i) => { const x = L.scaleInfo[i], oa = origAlphaOf(x.s); r.splice(4, 0, oa == null ? '<span class="hint">-</span>' : `${NZ(oa)}${isFinite(x.a) && x.a < oa - 0.1 ? ' <span class="a-mid" title="원 연구보다 .10 넘게 낮음">↓</span>' : ''}`); });
    const ms = measureSentences(), noOrig = L.scaleInfo.filter(x => x.its.length && (!norm(x.s.source) || origAlphaOf(x.s) == null));
    box.innerHTML = `${pilotNote()}${sampleNote()}${prepCard('measure')}
      <div class="card"><h3 class="card-title">신뢰도 (크론바흐 α)</h3>${tableHtml(['변인', '문항 수', '분석한 사람', 'α', ...(hasOrig ? ['원척도 α'] : []), '판정', '살펴볼 문항'], relRows, hasOrig ? [1, 2, 3, 4] : [1, 2, 3])}
        ${hasOrig ? '<p class="hint">원척도 α는 설문 만들기에 적은 원 연구의 값입니다. 내 자료가 원 연구보다 .10 넘게 낮으면 ↓로 표시합니다. 표본이나 번역의 차이일 수 있으니 문항을 살펴보세요.</p>' : ''}
        <p class="hint">.70 이상이면 받아들일 만하다고 봅니다(.60~.70은 탐색 연구에서 허용하기도 함). 살펴볼 문항은 문항-총점 상관이 .30보다 낮거나 지우면 α가 오르는 문항입니다. 문항별 자세한 값은 [응답 취합·정리]에 있습니다.</p></div>
      <div class="card"><h3 class="card-title">타당도 (탐색적 요인분석)</h3>${efaHtml}</div>
      <div class="card" id="cfa-card">${cfaHtml()}</div>
      <div class="card"><h3 class="card-title">논문 측정 도구 절 문장 (초안)</h3>
        ${noOrig.length ? `<p class="hint">원척도 정보가 비어 있는 변인: <b>${noOrig.map(x => esc(x.s.name || scaleCode(x.s, x.si))).join(', ')}</b>. <a href="#" data-nav-to="survey">설문 만들기</a>의 변인마다 [원척도 정보]에 출처와 원 연구 신뢰도를 적으면 문장에 들어갑니다.</p>` : ''}
        <div class="dq-report" id="ms-sent">${ms.map(t => `<p>${esc(t)}</p>`).join('')}</div>
        <div class="actions tight"><button class="btn sm" data-dq-act="copy-measure">문장 복사</button></div></div>`;
    markSeen('measure');
  }

  // ---------- 사전 분석 ----------
  function corrTable() {
    const L = lastScored, S = L.scaleInfo.filter(x => x.n > 2);
    const cols = S.map(x => L.inc.map(p => p.score[x.si]));
    const rows = S.map((x, i) => {
      const cells = S.map((y, j) => {
        if (j > i) return '';
        if (j === i) return '—';
        const pr = L.inc.map((_, r) => [cols[i][r], cols[j][r]]).filter(([a, b]) => a != null && b != null);
        const rr = corr(pr.map(q => q[0]), pr.map(q => q[1])), df = pr.length - 2, t = rr * Math.sqrt(df / (1 - rr * rr)), p = ST.pT(Math.abs(t), df);
        return `${NZ(rr)}${STAR(p)}`;
      });
      return [`${i + 1}. ${esc(x.s.name || scaleCode(x.s, x.si))}`, F2(x.m), F2(x.sdv), ...cells];
    });
    return { head: ['변인', 'M', 'SD', ...S.map((_, i) => String(i + 1))], rows, S, cols };
  }
  function vifInfo() {
    const out = [];
    planItems().forEach(p => {
      const m = LADDER_TO_METHOD[p.result]; if (!m || !/regression|hier|med_|moderation|modmed|logistic/.test(m)) return;
      const r = rolesFor(p, m);
      const keys = [...new Set([].concat(r.xs || [], r.b1 || [], r.b2 || [], r.x || [], r.ms || [], r.m1 || [], r.m2 || [], r.m || [], r.w || [], r.covs || []).filter(Boolean))];
      const vs = dataVars(), V = keys.map(k => vs.find(v => v.key === k)).filter(v => v && v.type === 'num');
      if (V.length < 2) return;
      const ok = lastScored.inc.map((_, i) => i).filter(i => V.every(v => v.vals[i] != null));
      const vf = vifs(V.map(v => ok.map(i => v.vals[i])));
      out.push({ p, rows: V.map((v, j) => [v.name, vf[j]]) });
    });
    return out;
  }
  function renderPrelim() {
    const box = $('#dq-prelim'); if (!box) return;
    if (!lastScored) { box.innerHTML = needData('정규성과 상관'); return; }
    const L = lastScored;
    const normRows = L.scaleInfo.map(x => {
      const vals = L.inc.map(p => p.score[x.si]).filter(v => v != null), sw = shapiro(vals);
      const ok = Math.abs(x.sk) < 2 && Math.abs(x.ku) < 7;
      return [`<b>${esc(x.s.name || scaleCode(x.s, x.si))}</b>`, String(x.n), F2(x.m), F2(x.sdv), F2(x.min), F2(x.max), F2(x.sk), F2(x.ku), sw ? `${F2(sw.W, 3)} (${PS(sw.p).replace(/^p /, 'p ')})` : '-', isFinite(x.sk) ? (ok ? '<span class="a-ok">충족</span>' : '<span class="a-low">벗어남</span>') : '-'];
    });
    const C = corrTable(), V = vifInfo();
    const preLock = !md.lock && !L.pilot && !L.sample;
    box.innerHTML = `${pilotNote()}${sampleNote()}${prepCard('prelim')}
      ${preLock ? '<p class="cf-note">아직 가설을 확정하지 않았습니다. 본조사 자료의 상관을 보고 모형을 바꾸면 결과를 보고 가설을 정한 셈이 됩니다. <button class="linkish" data-nav-to="model">모델 검토에서 먼저 확정하기</button></p>' : ''}
      <div class="card"><h3 class="card-title">기술통계와 정규성</h3>${tableHtml(['변인', 'N', 'M', 'SD', '최솟값', '최댓값', '왜도', '첨도', 'Shapiro-Wilk W', '정규성'], normRows, [1, 2, 3, 4, 5, 6, 7])}
        <p class="hint">왜도 절댓값 2, 첨도 절댓값 7보다 작으면 정규분포에서 크게 벗어나지 않았다고 봅니다(Curran, West, & Finch, 1996). Shapiro-Wilk는 표본이 크면 작은 차이에도 유의하게 나오므로 왜도·첨도와 함께 판단합니다.</p>
        ${(() => { preFigs = sv.scales.map((sc, si) => ({ type: 'hist', title: `${sc.name || scaleCode(sc, si)} 점수의 분포`, xLabel: sc.name || scaleCode(sc, si), vals: L.inc.map(p => p.score[si]).filter(v => v != null), range: scoreRange('S' + si), note: '막대는 점수 구간별 사람 수, 선은 같은 평균·표준편차의 정규분포입니다. 막대가 선에서 크게 벗어나 한쪽으로 쏠리면 정규성을 다시 확인하세요.' })).filter(f => f.vals.length >= 5);
          return preFigs.length ? `<details class="item-detail"><summary>점수 분포 그림 ${preFigs.length}개</summary><div class="tfigs" style="padding:4px 12px 12px">${preFigs.map((f, i) => `<figure class="tfig"><figcaption><b>${esc(f.title)}</b></figcaption><div class="tfig-svg">${figRender(f)}</div><p class="hint">${esc(f.note)}</p><div class="actions tight"><button class="btn sm" data-fig-act="png" data-src="pre" data-i="${i}">그림 받기 (PNG)</button><button class="btn sm" data-fig-act="png" data-src="pre" data-i="${i}" data-mono="1">흑백으로 받기</button></div></figure>`).join('')}</div></details>` : ''; })()}</div>
      <div class="card"><h3 class="card-title">표 2. 주요 변인의 기술통계와 상관 <small class="hint">(N = ${L.inc.length})</small></h3>${tableHtml(C.head, C.rows, C.head.map((_, i) => i).slice(1))}
        <p class="hint">*p < .05, **p < .01, ***p < .001. 상관이 .80을 넘는 변인 쌍은 서로 너무 비슷해(다중공선성) 함께 예측변인으로 넣기 어렵습니다.</p>
        <div class="actions tight"><button class="btn sm" data-dq-act="t2-csv">표 2 CSV</button></div></div>
      ${V.length ? `<div class="card"><h3 class="card-title">다중공선성 (VIF)</h3>${V.map(x => `<p><b>${hLabel(x.p)}</b> ${esc(RULES.results[x.p.result].name)}: ${x.rows.map(([n, v]) => `${esc(n)} <span class="${v >= 10 ? 'a-low' : v >= 5 ? 'a-mid' : ''}">${F2(v)}</span>`).join(' · ')}</p>`).join('')}
        <p class="hint">VIF가 10 이상이면 다중공선성이 심각하다고 봅니다(5 이상이면 주의).</p></div>` : ''}
      <div class="actions"><button class="btn navy" data-nav-to="analysis">가설 검증하러 →</button></div>`;
    markSeen('prelim');
  }
  // 연구 폴더에 남길 사전 점검 보고서
  function prelimMd() {
    const L = lastScored; if (!L) return '';
    const md2 = [`# 자료 점검 보고서 — 통`, '', `작성: ${koNow()} · ${L.pilot ? '예비조사' : '본조사'} 자료${L.sample ? ' (예시 가짜 자료)' : ''}`, '', '## 응답 정리'];
    md2.push(flowSteps().map(s => s.minus ? `${s.l} −${s.minus} → ${s.n}명` : `${s.l} ${s.n}명`).join(' → '));
    const M = missingInfo(); md2.push('', `- 빠진 값: 척도 문항 칸의 ${M.pct.toFixed(1)}% (${M.miss}칸), 하나라도 빠뜨린 사람 ${M.persons}명. 변인 점수는 문항 ${plan.dec.minPct}% 이상 응답 시 응답 문항 평균, 분석은 목록별 제외.`);
    md2.push(`- 중복 응답: ${L.people.filter(p => p.dup).length}개 (${(plan.dec.dup || 'keep') === 'drop' ? '뺌' : '포함'})`, `- 이상치(|z| > 3.29 또는 마할라노비스 p < .001): ${(L.outliers || []).length}개 (${(plan.dec.outlier || 'keep') === 'drop' ? '뺌' : '포함'})${(L.outliers || []).length ? ': ' + L.outliers.map(o => o.id).join(', ') : ''}`);
    md2.push('', '## 신뢰도', '| 변인 | 문항 수 | α |', '|---|---|---|'); L.scaleInfo.forEach(x => md2.push(`| ${x.s.name || scaleCode(x.s, x.si)} | ${x.its.length} | ${fr(x.a)} |`));
    md2.push('', '## 기술통계와 정규성', '| 변인 | N | M | SD | 왜도 | 첨도 |', '|---|---|---|---|---|---|'); L.scaleInfo.forEach(x => md2.push(`| ${x.s.name || scaleCode(x.s, x.si)} | ${x.n} | ${F2(x.m)} | ${F2(x.sdv)} | ${F2(x.sk)} | ${F2(x.ku)} |`));
    const C = corrTable(); md2.push('', '## 상관', '| ' + C.head.join(' | ') + ' |', '|' + C.head.map(() => '---').join('|') + '|'); C.rows.forEach(r => md2.push('| ' + r.map(c => String(c).replace(/<[^>]+>/g, '')).join(' | ') + ' |'));
    return md2.join('\n');
  }

  function dqDemo() {
    if (!designItems().length) { sv = exampleSurvey(); saveSv(); renderSurvey(); }
    const t = sampleCsv(300); $('#co-paste').value = t; loadRaw(t, '예시 응답(가짜 자료)'); runScoring(); lastScored.sample = true;
    toast('예시 자료 300명으로 점수를 만들었습니다.');
  }
  ['sample', 'measure', 'prelim'].forEach(v => {
    $('#view-' + v).addEventListener('click', e => {
      const nt = e.target.closest('[data-nav-to]'); if (nt) { gotoNav(nt.dataset.navTo); return; }
      const b = e.target.closest('[data-dq-act]'); if (!b) return;
      const a = b.dataset.dqAct;
      if (a === 'to-collect') gotoNav('collect');
      else if (a === 'demo') { dqDemo(); switchView(v); }
      else if (a === 't1-csv') download(`표1_일반적특성_${today()}.csv`, tblCsv(['구분', '범주', '빈도(명) / 값', '비율(%) / 범위'], demoTable().map(r => r.map(c => String(c).replace(/<[^>]+>/g, '')))), 'text/csv');
      else if (a === 't2-csv') { const C = corrTable(); download(`표2_상관_${today()}.csv`, tblCsv(C.head, C.rows.map(r => r.map(c => String(c).replace(/<[^>]+>/g, '')))), 'text/csv'); }
    });
  });
  $('#view-measure').addEventListener('change', e => { if (e.target.matches('[data-dq-efa]')) { efaOpt.k = e.target.value; renderMeasure(); } if (e.target.matches('[data-dq-efa-ex]')) { efaOpt.ex = e.target.value; renderMeasure(); } });
  renderKind();

  // =================================================================
