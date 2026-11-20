# 서울 돔 자세한 지도: overpass-turbo에서 받은 GeoJSON(건물 테두리 + 모든 길)을 사이트용 seoul-detail.json으로 줄임
# 쓰는 법: python3 prep2.py 출력.json 입력1.geojson [입력2.geojson ...]
# 좌표는 prep.py와 같은 기준 (127.01, 37.525), 단위 m. 출처: © OpenStreetMap contributors (ODbL)
import json, math, sys
OUT = sys.argv[1]; SRCS = sys.argv[2:]
LON0, LAT0 = 127.01, 37.525
KX = math.cos(math.radians(LAT0)) * 111320; KZ = 110540
P = lambda lon, lat: ((lon - LON0) * KX, -(lat - LAT0) * KZ)

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

def area(r):
    return abs(sum(r[i][0] * r[i - 1][1] - r[i - 1][0] * r[i][1] for i in range(len(r)))) / 2

# 길 너비 (m)
WID = {'tertiary': 10, 'tertiary_link': 8, 'unclassified': 7, 'residential': 7, 'living_street': 6, 'service': 5, 'busway': 8}
WALK = {'footway', 'path', 'pedestrian', 'steps', 'cycleway', 'corridor'}
MAJOR = {'motorway', 'trunk', 'primary', 'secondary', 'motorway_link', 'trunk_link', 'primary_link', 'secondary_link'}

blds, streets, walks, boxes = [], [], [], []
for src in SRCS:
    d = json.load(open(src))
    xs, zs = [], []
    for f in d['features']:
        p = f.get('properties', {}); g = f['geometry']
        if not g: continue
        if 'building' in p and g['type'] in ('Polygon', 'MultiPolygon'):
            polys = [g['coordinates']] if g['type'] == 'Polygon' else g['coordinates']
            for poly in polys:
                r = simpring([P(*c) for c in poly[0]], 1.2)
                if len(r) < 3: continue
                a = area(r)
                if a < 25: continue
                h = num(p.get('height'))
                lv = num(p.get('building:levels'))
                if h is None and lv: h = lv * 3.4 + 1
                if h is None:
                    # 높이 정보가 없으면 넓이로 짐작 (작은 집은 낮게, 큰 건물은 높게). 같은 건물은 늘 같은 높이
                    s = (int(abs(r[0][0]) * 7 + abs(r[0][1]) * 13) % 100) / 100
                    h = 7 + s * 6 if a < 120 else 10 + s * 12 if a < 400 else 16 + s * 30 if a < 1500 else 25 + s * 45
                h = max(4, min(320, h))
                flat = [round(h)]
                for x, z in r: flat += [round(x, 1), round(z, 1)]; xs.append(x); zs.append(z)
                blds.append(flat)
        elif 'highway' in p and g['type'] == 'LineString':
            t = p['highway']
            pts = simp([P(*c) for c in g['coordinates']], 1.5)
            if len(pts) < 2: continue
            flat = []
            for x, z in pts: flat += [round(x, 1), round(z, 1)]; xs.append(x); zs.append(z)
            if t in WALK: walks.append(flat)
            elif t in WID: streets.append([WID[t]] + flat)
            # 큰길은 이미 data.json에 있음
    if xs: boxes.append([round(min(xs)), round(max(xs)), round(min(zs)), round(max(zs))])

out = {'attribution': '© OpenStreetMap contributors (ODbL)', 'boxes': boxes, 'b': blds, 's': streets, 'w': walks}
json.dump(out, open(OUT, 'w'), separators=(',', ':'))
print('buildings', len(blds), 'streets', len(streets), 'walks', len(walks), 'boxes', boxes)
