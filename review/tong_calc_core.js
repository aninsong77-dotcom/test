// 통 — 계산 부분만 모은 파일 (검토용)
// 원본: stats-ladder.html 한 파일(약 1만 줄, 화면·설문·AI 연결 포함)에서 통계 계산과 관련된 부분만 뽑았습니다.
// 이 파일만으로 실행되지는 않습니다(화면 코드와 이어져 있음). 실제 동작은 tests/reference.mjs로 R과 비교해 확인합니다.
// 검증: tests/README.md 참고 — R 4.3.3 기준값과 비교 (분석 19가지의 효과 54개, t검정·회귀·요인분석·베이즈 인자).

// ##### 1. 계산 도우미 · 분포 · 회귀(QR) · 부트스트랩 · 베이즈 인자 · 숫자 표기 (stats-ladder.html 4117~4565줄) #####
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

  function runScoring() {
    const items = designItems();
    const unknown = new Map(); // 값 -> 횟수
    const people = raw.rows.map((row, r) => {
      const p = { id: 'R' + String(r + 1).padStart(3, '0'), row, items: {}, rawLikert: [], excluded: '' };
      const cv = consentCol >= 0 ? norm(row[consentCol]) : '';
      if (consentCol >= 0 && /(동의하지|비동의|아니)/.test(cv)) p.excluded = '비동의';
      items.forEach(d => {
        const cell = norm(d.kind === 'item' || d.kind === 'demo' ? (colMap[d.key] >= 0 ? row[colMap[d.key]] : '') : '');
        if (d.kind === 'demo') { p.items[d.key] = cell; return; }
        const s = sv.scales[d.si], P = +s.points;
        let v = null;
        if (cell !== '') {
          const li = s.labels.findIndex(l => norm(l) === cell);
          if (li >= 0) v = li + 1;
          else { const m = cell.match(/^(\d+)/); if (m && +m[1] >= 1 && +m[1] <= P) v = +m[1]; else unknown.set(cell, (unknown.get(cell) || 0) + 1); }
        }
        if (v != null) p.rawLikert.push(v);
        p.items[d.key] = v == null ? null : (s.items[d.ii].reverse ? P + 1 - v : v);
      });
      const likertN = items.filter(d => d.kind === 'item').length;
      if (!p.excluded && !p.rawLikert.length && likertN) p.excluded = '척도 응답 없음';
      p.straight = p.rawLikert.length >= 8 && p.rawLikert.every(v => v === p.rawLikert[0]);
      if (!p.excluded && p.straight && plan.dec.straight === 'drop') p.excluded = '불성실 의심';
      return p;
    });
    // 중복 응답: 문항 답이 통째로 같은 응답 (뒤의 것)
    const seenSig = new Map();
    people.forEach(p => {
      if (p.excluded) return;
      const sig = items.map(d => p.items[d.key] ?? '').join('|'); if (!/[^|]/.test(sig)) return;
      if (seenSig.has(sig)) { p.dup = seenSig.get(sig); if ((plan.dec.dup || 'drop') === 'drop') p.excluded = '중복'; }
      else seenSig.set(sig, p.id);
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

    lastScored = { people, inc, items, scaleInfo, demoInfo, unknown, sample: !!raw.sample, outliers, pilot: dataKind() === 'pilot' };
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
    const straightKept = L.inc.filter(p => p.straight).length;
    const unk = [...L.unknown.entries()];
    const preview = analysisRows();
    const pvCols = preview[0].length;

    $('#co-result').innerHTML = `
    <div class="card">
      <h3 class="card-title">3. 결과</h3>
      <div class="stats">
        <div class="stat"><b>${L.people.length}</b><span>전체 응답</span></div>
        <div class="stat"><b>${L.inc.length}</b><span>분석에 포함</span></div>
        <div class="stat"><b>${ex.length}</b><span>제외 (비동의 ${exCount('비동의')} · 빈 응답 ${exCount('척도 응답 없음')} · 불성실 ${exCount('불성실 의심')} · 중복 ${exCount('중복')} · 이상치 ${exCount('이상치')})</span></div>
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
      <p class="hint">점수는 역문항을 뒤집은 뒤 문항 ${sv.scoring === 'sum' ? '합계' : '평균'}입니다. 한 변인의 문항 중 ${+plan.dec.minPct === 100 ? '모두' : `${plan.dec.minPct}% 이상`} 답한 사람만 점수를 냅니다 ([분석] 탭의 분석 결정에서 바꿀 수 있음). 크론바흐 α는 보통 .70 이상이면 받아들일 만하다고 보고(.60~.70은 탐색 연구에서 허용하는 경우가 있음), 왜도 절댓값 2·첨도 절댓값 7 미만이면 정규분포에서 크게 벗어나지 않았다고 보는 기준이 흔히 쓰입니다.</p>
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

// ##### 2. 분석 방법 19가지 (RUN) (stats-ladder.html 4565~5298줄) #####
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
    modmed14: { name: '조절된 매개분석 · 뒤쪽 경로 (PROCESS Model 14)', group: '매개·조절', slots: [{ k: 'x', label: '독립변인 (X)', type: 'numbin' }, { k: 'm', label: '매개변인 (M)', type: 'num' }, { k: 'w', label: '조절변인 (W, M→Y를 조절)', type: 'numbin' }, { k: 'y', label: '종속변인 (Y)', type: 'num' }, { k: 'covs', label: '통제변인 (선택)', type: 'any', multi: true, min: 0 }], opts: ['boot', 'center'] }
  };
  // 사다리 도착점 → 계산 방법 (null: 이 사이트에서 계산하지 않음)
  const LADDER_TO_METHOD = {
    r_onesample_t: 'onesample', r_ind_t: 'ind_t', r_paired_t: 'paired_t', r_ancova: 'ancova', r_oneway: 'oneway', r_rel_anova: 'oneway',
    r_factorial_anova: 'factorial', r_chisq: 'chisq', r_logistic: 'logistic', r_pearson: 'pearson', r_spearman: 'spearman', r_partial: 'partial',
    r_pointbiserial: 'pointbiserial', r_simple_reg: 'regression', r_multiple_reg: 'regression', r_hier_reg: 'hier', r_med_simple: 'med_parallel',
    r_med_parallel: 'med_parallel', r_med_serial: 'med_serial', r_moderation: 'moderation', r_modmed_a: 'modmed7', r_modmed_b: 'modmed14',
    r_rm_anova: null, r_mixed_anova: null, r_rm_factorial: null, r_mcnemar: null, r_cochran: null, r_multilevel: null, r_multinom: null, r_ordinal: null
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
    const ladderNote = lid ? (lm ? `사다리가 고른 기법: <b>${esc(RULES.results[lid].name)}</b> → 아래에서 <b>${esc(METHODS[lm].name)}</b>으로 계산합니다.`
      : `사다리가 고른 기법 <b>${esc(RULES.results[lid].name)}</b>은 이 사이트에서 아직 계산하지 않습니다. jamovi(무료)나 SPSS에서 계산하고, 결과 해석 메뉴에 그 결과를 직접 적어 쓸 수 있습니다.`) : '사다리에서 기법을 고르면 그 기법이 먼저 선택됩니다.';
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
    box.innerHTML = `<div class="card">
      <div class="data-line"><span class="st auto">자료</span> 분석에 포함된 응답 ${lastScored.inc.length}명 · 변인 ${vs.length}개${lastScored.sample ? ' · <b>예시(가짜) 자료</b>' : ''}</div>
      <p class="hint">${ladderNote}</p>
      <label class="f" style="max-width:520px;margin-top:12px">분석 방법<select id="an-method">${groups.map(g => `<optgroup label="${esc(g)}">${Object.entries(METHODS).filter(([, m]) => m.group === g).map(([k, m]) => `<option value="${k}" ${k === anSel.method ? 'selected' : ''}>${esc(m.name)}${k === lm ? ' ← 사다리' : ''}</option>`).join('')}</optgroup>`).join('')}</select></label>
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
    else if (act === 'lock-plan') { plan.locked = new Date().toLocaleString('ko-KR'); savePlan(); renderAnalysis(); addSys('analysis', '분석 결정을 확정했습니다.'); toast('분석 결정을 확정했습니다. 이후 바꾸면 기록에 남습니다.'); }
    else if (act === 'plan-dl') download('analysis_plan.md', planText());
    else if (act === 'r-data') download('analysis_data.csv', toCsv(analysisRows()), 'text/csv');
    else if (act === 'r-code') download('analysis.R', rScript(analyses.slice().reverse()));
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

  async function runAnalysis() {
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
    analyses.unshift(R); saveAnalyses(); renderAnalyses(); renderInterp();
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
      const P = prep({ x: r.x, y: r.y, c: r.covs }); need(P.n > r.covs.length + 3, '응답 수가 너무 적습니다.');
      const D = designCols(P, r.covs), Z = rowsOf(D.cols);
      const res = k => { const y = P.col(k), f = ols(y, Z); return y.map((v, i) => v - (f.b[0] + Z[i].reduce((s, z, j) => s + z * f.b[j + 1], 0))); };
      const rr = corr(res(r.x), res(r.y)), df = P.n - 2 - D.cols.length, t = rr * Math.sqrt(df / (1 - rr * rr)), p = ST.pT(Math.abs(t), df);
      const r0 = corr(P.col(r.x), P.col(r.y));
      const R = newResult('partial', `편상관: ${P.V[r.x].name} – ${P.V[r.y].name}`); R.n = P.n;
      R.tables.push({ title: '편상관 결과', head: ['', 'r', 'df', 't', 'p'], num: [1, 2, 3, 4], rows: [['단순상관 (통제 전)', NZ(r0), String(P.n - 2), '', PV(ST.pT(Math.abs(r0 * Math.sqrt((P.n - 2) / (1 - r0 * r0))), P.n - 2))], [`편상관 (${D.names.join(', ')} 통제)`, NZ(rr), String(df), F2(t), PV(p)]] });
      eff(R, { label: `${P.V[r.x].name} – ${P.V[r.y].name} 편상관 (통제: ${D.names.join(', ')})`, est: rr, p, stat: `r = ${NZ(rr)}, t(${df}) = ${F2(t)}, ${PS(p)}` });
      dropNote(R, P); return R;
    },

    async pointbiserial(r) {
      const P = prep({ g: r.g, y: r.y }); const B = binCode(P, r.g), y = P.col(r.y);
      const rr = corr(B.x, y), df = P.n - 2, t = rr * Math.sqrt(df / (1 - rr * rr)), p = ST.pT(Math.abs(t), df);
      const R = newResult('pointbiserial', `점이연 상관: ${P.V[r.g].name} – ${P.V[r.y].name}`); R.n = P.n;
      R.tables.push({ title: '점이연 상관', head: ['', 'r', 'df', 't', 'p'], num: [1, 2, 3, 4], rows: [[`${P.V[r.g].name} – ${P.V[r.y].name}`, NZ(rr), String(df), F2(t), PV(p)]] });
      R.notes.push(B.note + '. 양수이면 1로 코딩한 집단의 점수가 더 높다는 뜻입니다. 이 결과는 등분산을 가정한 독립표본 t검정과 같습니다.');
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
      addBayes(R, [[`${P.V[r.y].name} 평균 ≠ ${tv}`, lbf]], `효과크기 δ ~ 코시(0, √2/2), JASP·BayesFactor의 기본값(medium)`);
      dropNote(R, P); return R;
    },

    async ind_t(r, o) {
      const R = newResult('ind_t', `독립표본 t검정: ${varByKey(r.g).name}`);
      const rows = [], lev = [], bfr = [];
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
        eff(R, { label: `${P.V[yk].name}: ${lv[0]} − ${lv[1]}`, est: ma - mb, p, es: d, esKind: 'd', lbf, stat: `${lv[0]} M = ${F2(ma)}, ${lv[1]} M = ${F2(mb)}, t(${useW ? F2(df, 1) : df}) = ${F2(t)}, ${PS(p)}, d = ${F2(d)}` });
        nMax = Math.max(nMax, P.n); lev.push([na, nb]);
      }
      R.n = nMax;
      R.tables.push({ title: `집단 비교 (${lv[0]} n = ${lev[0][0]}, ${lv[1]} n = ${lev[0][1]})`, head: ['변인', `${lv[0]} M (SD)`, `${lv[1]} M (SD)`, 'Levene F (p)', 't', 'df', 'p', 'Cohen의 d', '검정'], num: [1, 2, 3, 4, 5, 6, 7], rows });
      R.notes.push((o.welch === 'never' ? '분석 결정에 따라 언제나 등분산을 가정한 Student t검정 값을 보고했습니다.' : o.welch === 'auto' ? 'Levene 검정이 유의하면(p < .05) 등분산을 가정하지 않는 Welch t검정 값을 보고했습니다.' : '분산이 같다고 가정하지 않는 Welch t검정 값을 보고했습니다(분산이 같아도 거의 손해가 없어 최근 권장되는 방식). Levene 검정은 참고로 함께 보였습니다.') + ' d는 앞 집단 − 뒤 집단 기준입니다.');
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
        R.tables.push({ title: `${P.V[yk].name}: 분산분석`, head: ['', 'SS', 'df', 'MS', 'F', 'p', 'η²'], num: [1, 2, 3, 4, 5, 6], rows: [['집단 간', F2(ssb), String(dfb), F2(ssb / dfb), F2(F), PV(p), NZ(eta, 3)], ['집단 내', F2(ssw), String(dfw), F2(ssw / dfw), '', '', ''], ['전체', F2(ssb + ssw), String(N - 1), '', '', '', '']] });
        const post = [], bon = o.posthoc === 'bonferroni', mPairs = k * (k - 1) / 2;
        for (let i = 0; i < k; i++) for (let j = i + 1; j < k; j++) {
          const diff = mean(G[i]) - mean(G[j]), se2 = (ssw / dfw) * (1 / G[i].length + 1 / G[j].length);
          const pp = bon ? Math.min(1, ST.pT(Math.abs(diff / Math.sqrt(se2)), dfw) * mPairs) : ST.pF(diff ** 2 / (se2 * (k - 1)), k - 1, dfw);
          post.push([`${lv[i]} − ${lv[j]}`, F2(diff), PV(pp)]);
        }
        R.tables.push({ title: `${P.V[yk].name}: 사후검정 (${bon ? 'Bonferroni' : 'Scheffé'})`, head: ['비교', '평균 차이', 'p'], num: [1, 2], rows: post });
        R.notes.push(`${P.V[yk].name}: Levene F(${dfb}, ${dfw}) = ${F2(L.F)}, ${PS(L.p)}.${L.p < 0.05 ? ` 등분산 가정이 어긋나 Welch F(${dfb}, ${F2(dfW, 1)}) = ${F2(FW)}, ${PS(pW)}도 함께 보고하는 것이 좋습니다.` : ''}`);
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
      const ssG = red.sse - full.sse, dfG = dum.length, FG = (ssG / dfG) / full.mse, pG = ST.pF(FG, dfG, full.dfe), peG = ssG / (ssG + full.sse);
      const rows = [[P.V[r.g].name, F2(ssG), String(dfG), F2(ssG / dfG), F2(FG), PV(pG), NZ(peG, 3)]];
      r.covs.forEach((k, j) => { const others = covs.filter((_, q) => q !== j); const f = ols(y, rowsOf([...dum, ...others])); const ss = f.sse - full.sse, F = ss / full.mse; rows.push([P.V[k].name + ' (공변인)', F2(ss), '1', F2(ss), F2(F), PV(ST.pF(F, 1, full.dfe)), NZ(ss / (ss + full.sse), 3)]); });
      rows.push(['오차', F2(full.sse), String(full.dfe), F2(full.mse), '', '', '']);
      // 조정된 평균: 공변인 평균에서 예측
      const cm = covs.map(mean);
      const adj = lv.map((l, i) => full.b[0] + (i ? full.b[i] : 0) + cm.reduce((s, m, j) => s + m * full.b[1 + dum.length + j], 0));
      // 회귀 기울기 동질성
      const inter = []; dum.forEach(d => covs.forEach(c => inter.push(d.map((v, i) => v * c[i]))));
      const fi = ols(y, rowsOf([...dum, ...covs, ...inter])); const ssI = full.sse - fi.sse, dfI = inter.length, FI = (ssI / dfI) / fi.mse, pI = ST.pF(FI, dfI, fi.dfe);
      const R = newResult('ancova', `공분산분석: ${P.V[r.g].name} → ${P.V[r.y].name}`); R.n = P.n;
      R.tables.push({ title: '집단별 평균', head: ['집단', 'n', '관찰 평균 (SD)', '조정된 평균'], num: [1, 2, 3], rows: lv.map((l, i) => { const c = y.filter((_, q) => g[q] === l); return [l, String(c.length), `${F2(mean(c))} (${F2(sd(c))})`, F2(adj[i])]; }) });
      R.tables.push({ title: '공분산분석', head: ['', 'SS', 'df', 'MS', 'F', 'p', '부분 η²'], num: [1, 2, 3, 4, 5, 6], rows });
      R.notes.push(`회귀 기울기 동질성: F(${dfI}, ${fi.dfe}) = ${F2(FI)}, ${PS(pI)}.${pI < 0.05 ? ' 공변인과 종속변인의 관계가 집단마다 달라 공분산분석의 전제가 어긋납니다. 상호작용을 넣은 회귀분석을 검토하세요.' : ' 전제가 크게 어긋나지 않았습니다.'}`);
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
      const est = stat(F0), sx = sd(X.x), sy = sd(y);
      const nmX = P.V[r.x].name, nmY = P.V[r.y].name, nmM = r.ms.map(k => P.V[k].name);
      const R = newResult('med_parallel', `${r.ms.length > 1 ? '병렬 다중' : '단순'}매개: ${nmX} → ${nmM.join(', ')} → ${nmY}`); R.n = n; bootNote(R, bs);
      pathTables(R, P, F0, nmX, nmY, nmM, r.covs, X.x, y, Ms);
      const idRows = [];
      nmM.forEach((m, j) => idRows.push([`${nmX} → ${m} → ${nmY}`, F2(est[j], 3), F2(bs[j].se, 3), CI(bs[j].lo, bs[j].hi, 3), F2(est[j] * sx / sy, 3)]));
      if (nmM.length > 1) idRows.push(['총 간접효과', F2(est[nmM.length], 3), F2(bs[nmM.length].se, 3), CI(bs[nmM.length].lo, bs[nmM.length].hi, 3), F2(est[nmM.length] * sx / sy, 3)]);
      let q = nmM.length + 1; const con = [];
      for (let i = 0; i < nmM.length; i++) for (let j = i + 1; j < nmM.length; j++) { con.push([`대비: (${nmM[i]}) − (${nmM[j]})`, F2(est[q], 3), F2(bs[q].se, 3), CI(bs[q].lo, bs[q].hi, 3), '']); q++; }
      R.tables.push({ title: `간접효과 (부트스트랩 ${(+o.boot || 5000).toLocaleString()}번, 95% 백분위 신뢰구간)`, head: ['경로', '효과', 'Boot SE', '95% CI', '완전표준화 효과'], num: [1, 2, 3, 4], rows: idRows.concat(con) });
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
      const est = stat(F0), sx = sd(X.x), sy = sd(y);
      const nx = P.V[r.x].name, n1 = P.V[r.m1].name, n2 = P.V[r.m2].name, ny = P.V[r.y].name;
      const R = newResult('med_serial', `순차매개: ${nx} → ${n1} → ${n2} → ${ny}`); R.n = n; bootNote(R, bs);
      pathTables(R, P, F0, nx, ny, [n1, n2], r.covs, X.x, y, [m1, m2], true);
      const lab = [`${nx} → ${n1} → ${ny}`, `${nx} → ${n1} → ${n2} → ${ny}`, `${nx} → ${n2} → ${ny}`, '총 간접효과', '대비: (1) − (2)', '대비: (1) − (3)', '대비: (2) − (3)'];
      R.tables.push({ title: `간접효과 (부트스트랩 ${(+o.boot || 5000).toLocaleString()}번, 95% 백분위 신뢰구간)`, head: ['경로', '효과', 'Boot SE', '95% CI', '완전표준화 효과'], num: [1, 2, 3, 4],
        rows: lab.map((l, j) => [j < 3 ? `(${j + 1}) ${l}` : l, F2(est[j], 3), F2(bs[j].se, 3), CI(bs[j].lo, bs[j].hi, 3), j < 4 ? F2(est[j] * sx / sy, 3) : '']) });
      R.tables.push(effectsTable(F0, nx, ny));
      lab.slice(0, 4).forEach((l, j) => eff(R, { label: j < 3 ? `간접효과: ${l}` : `총 간접효과: ${nx} → ${ny}`, est: est[j], lo: bs[j].lo, hi: bs[j].hi, ciBased: true, std: est[j] * sx / sy, stat: `B = ${F2(est[j], 3)}, 95% CI ${CI(bs[j].lo, bs[j].hi, 3)}` }));
      pathEffects(R, F0, nx, ny, [n1, n2], true);
      medNotes(R, X, P); return R;
    },

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
    R.n = Math.min(...vs.map(v => v.vals.filter(x => x != null).length));
    const nP = []; vs.forEach((a, i) => vs.forEach((b, j) => { if (j < i) nP.push(pair(a, b).n); }));
    const nLo = nP.length ? Math.min(...nP) : n0;
    R.tables.push({ title: `${spearman ? '스피어만 순위' : '피어슨'} 상관계수 (N = ${nLo < n0 ? `${nLo}~${n0}, 빠진 값은 짝마다 제외` : n0})`, head: ['변인', 'M', 'SD'].concat(vs.map((_, i) => String(i + 1))), num: vs.map((_, i) => i + 1).concat([vs.length + 1, vs.length + 2]), rows });
    R.notes.push('* p < .05, ** p < .01, *** p < .001. 짝마다 두 변인 모두 응답한 사람으로 계산했습니다.');
    if (nLo < n0 && nLo < 0.9 * n0) R.notes.push(`⚠ 짝마다 분석한 사람 수가 ${nLo}~${n0}명으로 많이 다릅니다. 상관마다 다른 사람들로 계산된 셈이라, 결측이 몰린 변인이 없는지 확인하세요.`);
    bad.forEach(w => R.notes.push('⚠ ' + w + '.'));
    addBayes(R, bfr, '상관 ρ ~ 균등분포(−1, 1), 즉 stretched beta 폭 1. JASP 기본값');
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

  function renderAnalyses() {
    const box = $('#an-results');
    if (!analyses.length) { box.innerHTML = ''; return; }
    box.innerHTML = rPanelHtml() + analyses.map(a => `<div class="card result-card" id="res-${a.id}">
      <div class="res-head">
        <div><h3 class="card-title">${confTag(a)}${esc(a.title)}</h3><div class="hint">N = ${a.n} · ${esc(a.at)}${a.sample ? ' · <b>예시(가짜) 자료</b>' : ''}${a.dv !== dataVersion ? ' · <span class="a-mid">지금 자료와 다른 자료로 계산함</span>' : ''}</div></div>
        <div class="head-btns"><button class="btn sm" data-an-act="csv" data-id="${a.id}">표 CSV</button><button class="btn sm" data-an-act="del" data-id="${a.id}">지우기</button></div>
      </div>
      ${confNote(a)}${a.tables.map(t => `<div class="section-title">${esc(t.title)}</div>
        <div class="table-wrap"><table class="compact stat-table"><thead><tr>${t.head.map((h, i) => `<th class="${(t.num || []).includes(i) ? 'num' : ''}">${esc(h)}</th>`).join('')}</tr></thead>
        <tbody>${t.rows.map(r => `<tr>${r.map((c, i) => `<td class="${(t.num || []).includes(i) ? 'num' : ''}">${esc(c)}</td>`).join('')}</tr>`).join('')}</tbody></table></div>
        ${t.foot ? `<p class="hint">${esc(t.foot)}</p>` : ''}`).join('')}
      ${a.notes.length ? `<ul class="notes">${a.notes.map(n => `<li>${esc(n)}</li>`).join('')}</ul>` : ''}
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

// ##### 3. R 코드 만들기 (재계산용) (stats-ladder.html 6739~7103줄) #####
  // =================================================================
  // R 코드 만들기 (분석 계획서 그대로, 기본 R만 사용)
  // =================================================================
  const rq = s => '"' + String(s).replace(/\\/g, '\\\\').replace(/"/g, '\\"').replace(/\n/g, ' ') + '"';
  const rvec = a => `c(${a.map(rq).join(', ')})`;
  // 계산할 때의 변인 정보를 결과에 남겨 둠 (나중에 자료 없이도 R 코드를 만들 수 있게)
  function specOf(method, roles, opts) {
    const keys = [...new Set(Object.values(roles).flat().filter(Boolean))];
    const vars = Object.fromEntries(keys.map(k => { const v = varByKey(k); return [k, { code: v.code, name: v.name, type: v.type, levels: v.levels || null }]; }));
    return { method, roles: JSON.parse(JSON.stringify(roles)), opts: { ...opts }, vars };
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
# 연구자가 정한 분석 결정을 그대로 따르는 코드입니다. 기본 R만 씁니다 (추가 패키지 없음).
# 결정을 바꾸려면 코드를 고치지 말고 통에서 바꾼 뒤 다시 받으세요.
#
# 분석 결정
${DECISIONS.map(d => `#   - ${d.q} → ${decLabel(d, d.get())}`).join('\n')}
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
          <ul class="notes"><li><b>Claude Code</b>: 그 폴더에서 <code>claude</code> 실행 → <b>"R로 분석해줘"</b>. Claude가 R을 대신 돌리고 결과를 정리합니다. (CLAUDE.md를 이 버전에서 다시 받으세요)</li>
          <li><b>RStudio</b>: <code>analysis.R</code>을 열고 [Source]. R을 몰라도 버튼 하나면 됩니다.</li></ul></div></div>
        <div class="chat-step"><span class="n">3</span><div><b>R 출력을 각 결과의 [R 결과와 맞춰 보기]에 붙여 넣기</b><p class="hint">효과마다 사이트와 R의 숫자가 같은지 표로 보여 줍니다.</p></div></div>
      </div>
    </div>`;
  }

  // =================================================================

// ##### 4. 표본 수 계산 (G*Power 방식) (stats-ladder.html 9091~9188줄) #####
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

// ##### 5. 탐색적 요인분석 (PAF·PCA, 베리맥스) (stats-ladder.html 9696~9773줄) #####
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
