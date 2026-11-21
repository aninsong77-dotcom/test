# 서울 돔 조각 지도: overpass-turbo GeoJSON들을 1.5km 조각 파일로 자름 (돔 둘레 조각만 그때그때 불러오기 위해)
# 쓰는 법: python3 prep-tiles.py 출력폴더 입력1.geojson [입력2.geojson ...]
# 이미 seoul-detail.json(도심)에 있는 구역은 빼고, 조각 목록은 출력폴더/index.json. 출처: © OpenStreetMap contributors (ODbL)
import json, math, os, sys, importlib.util
spec = importlib.util.spec_from_file_location('p2', os.path.join(os.path.dirname(os.path.abspath(__file__)), 'prep2.py'))
OUT, SRCS = sys.argv[1], sys.argv[2:]
LON0, LAT0 = 127.01, 37.525
KX = math.cos(math.radians(LAT0)) * 111320; KZ = 110540
P = lambda lon, lat: ((lon - LON0) * KX, -(lat - LAT0) * KZ)
T = 1500
DOWNTOWN = [-4804, 1587, -7564, -1802]   # seoul-detail.json 구역
def simp(pts, eps):
    if len(pts) < 3: return pts
    def dp(a, b, ps):
        (x1, y1), (x2, y2) = a, b; dx, dy = x2 - x1, y2 - y1; L = math.hypot(dx, dy) or 1e-9
        best, bi = 0, -1
        for i, (x, y) in enumerate(ps):
            dd = abs(dy * x - dx * y + x2 * y1 - y2 * x1) / L
            if dd > best: best, bi = dd, i
        if best > eps: return dp(a, ps[bi], ps[:bi]) + [ps[bi]] + dp(ps[bi], b, ps[bi + 1:])
        return []
    return [pts[0]] + dp(pts[0], pts[-1], pts[1:-1]) + [pts[-1]]
def simpring(pts, eps):
    if len(pts) > 1 and pts[0] == pts[-1]: pts = pts[:-1]
    if len(pts) < 4: return pts
    m = len(pts) // 2
    a = simp(pts[:m + 1], eps); b = simp(pts[m:] + [pts[0]], eps)
    return a[:-1] + b[:-1]
def num(v):
    try: return float(str(v).split(';')[0].replace('m', '').strip())
    except Exception: return None
def area(r): return abs(sum(r[i][0] * r[i - 1][1] - r[i - 1][0] * r[i][1] for i in range(len(r)))) / 2
WID = {'tertiary': 10, 'tertiary_link': 8, 'unclassified': 7, 'residential': 7, 'living_street': 6, 'service': 5, 'busway': 8}
WALK = {'footway', 'path', 'pedestrian', 'steps', 'cycleway', 'corridor'}
inDown = lambda x, z: DOWNTOWN[0] <= x <= DOWNTOWN[1] and DOWNTOWN[2] <= z <= DOWNTOWN[3]
key = lambda x, z: (math.floor(x / T), math.floor(z / T))
tiles = {}; seen = set()
def tile(k): return tiles.setdefault(k, {'b': [], 's': [], 'w': []})
for src in SRCS:
    for f in json.load(open(src))['features']:
        p = f.get('properties', {}); g = f['geometry']; fid = f.get('id') or p.get('@id')
        if not g or fid in seen: continue
        seen.add(fid)
        if 'building' in p and g['type'] in ('Polygon', 'MultiPolygon'):
            for poly in ([g['coordinates']] if g['type'] == 'Polygon' else g['coordinates']):
                r = simpring([P(*c) for c in poly[0]], 1.2)
                if len(r) < 3: continue
                a = area(r)
                if a < 25: continue
                cx = sum(q[0] for q in r) / len(r); cz = sum(q[1] for q in r) / len(r)
                if inDown(cx, cz): continue
                h = num(p.get('height')); lv = num(p.get('building:levels'))
                if h is None and lv: h = lv * 3.4 + 1
                if h is None:
                    s = (int(abs(r[0][0]) * 7 + abs(r[0][1]) * 13) % 100) / 100
                    h = 10 + s * 6 if a < 120 else 12 + s * 12 if a < 400 else 18 + s * 30 if a < 1500 else 25 + s * 45   # 서울 빌라·상가는 보통 4~6층
                h = max(4, min(320, h))
                flat = [round(h)]
                for x, z in r: flat += [round(x, 1), round(z, 1)]
                tile(key(cx, cz))['b'].append(flat)
        elif 'highway' in p and g['type'] == 'LineString':
            t = p['highway']
            if t not in WALK and t not in WID: continue   # 큰길은 이미 seoul-data.json에 있음
            pts = simp([P(*c) for c in g['coordinates']], 1.5)
            if len(pts) < 2: continue
            mx = sum(q[0] for q in pts) / len(pts); mz = sum(q[1] for q in pts) / len(pts)
            if inDown(mx, mz): continue
            flat = []
            for x, z in pts: flat += [round(x, 1), round(z, 1)]
            if t in WALK: tile(key(mx, mz))['w'].append(flat)
            else: tile(key(mx, mz))['s'].append([WID[t]] + flat)
os.makedirs(OUT, exist_ok=True)
idx = json.load(open(os.path.join(OUT, 'index.json'))) if os.path.exists(os.path.join(OUT, 'index.json')) else {'size': T, 'attribution': '© OpenStreetMap contributors (ODbL)', 'tiles': []}
have = {(a, b) for a, b, _ in idx['tiles']}
for (i, j), d in tiles.items():
    fn = os.path.join(OUT, f'{i}_{j}.json')
    if (i, j) in have and os.path.exists(fn):   # 이미 있는 조각에 더함 (조각 경계에 걸친 파일들)
        old = json.load(open(fn)); [old[k].extend(d[k]) for k in 'bsw']; d = old
    json.dump(d, open(fn, 'w'), separators=(',', ':'))
    idx['tiles'] = [t for t in idx['tiles'] if (t[0], t[1]) != (i, j)] + [[i, j, len(d['b'])]]
json.dump(idx, open(os.path.join(OUT, 'index.json'), 'w'), separators=(',', ':'))
print('tiles', len(tiles), 'buildings', sum(len(d['b']) for d in tiles.values()))
