import json, math, sys
from PIL import Image, ImageDraw
SRC=sys.argv[1]; SRC2=sys.argv[2]; OUT=sys.argv[3]
d=json.load(open(SRC)); d2=json.load(open(SRC2))
d['features']+=d2['features']
LON0,LAT0=127.01,37.525
KX=math.cos(math.radians(LAT0))*111320; KZ=110540
P=lambda lon,lat: (round((lon-LON0)*KX,1), round(-(lat-LAT0)*KZ,1))
def simp(pts,eps):
    if len(pts)<3: return pts
    def dp(a,b,ps):
        (x1,y1),(x2,y2)=a,b; dx,dy=x2-x1,y2-y1; L=math.hypot(dx,dy) or 1e-9
        best,bi=0,-1
        for i,(x,y) in enumerate(ps):
            dd=abs(dy*x-dx*y+x2*y1-y2*x1)/L
            if dd>best: best,bi=dd,i
        if best>eps: return dp(a,ps[bi],ps[:bi])+[ps[bi]]+dp(ps[bi],b,ps[bi+1:])
        return []
    return [pts[0]]+dp(pts[0],pts[-1],pts[1:-1])+[pts[-1]]
def simpring(pts,eps):
    if len(pts)>1 and pts[0]==pts[-1]: pts=pts[:-1]
    if len(pts)<4: return pts
    m=len(pts)//2
    a=simp(pts[:m+1],eps); b=simp(pts[m:]+[pts[0]],eps)
    return a[:-1]+b[:-1]
def rings(g):
    if g['type']=='Polygon': return [g['coordinates']]
    if g['type']=='MultiPolygon': return g['coordinates']
    return []
water=[]; parks=[]; roads=[]; bridges=[]; palaces=[]; walls=[]; streams=[]
for f in d['features']:
    p=f['properties']; g=f['geometry']
    if p.get('natural')=='water' or p.get('waterway')=='riverbank':
        for poly in rings(g):
            rr=[simpring([P(*c) for c in ring],4) for ring in poly]
            if len(rr[0])>=3: water.append(rr)
    elif p.get('historic')=='castle' and g['type'] in ('Polygon','MultiPolygon'):
        for poly in rings(g):
            r=simpring([P(*c) for c in poly[0]],3)
            if len(r)>=3: palaces.append({'n':p.get('name',''),'p':r})
    elif p.get('historic')=='citywalls' and g['type']=='LineString':
        walls.append(simp([P(*c) for c in g['coordinates']],4))
    elif p.get('waterway') in ('stream','river') and g['type']=='LineString' and p.get('name')=='청계천':
        streams.append(simp([P(*c) for c in g['coordinates']],3))
    elif p.get('leisure')=='park':
        for poly in rings(g):
            rr=[simpring([P(*c) for c in ring],4) for ring in poly][:1]
            if len(rr[0])>=3:
                xs=[a[0] for a in rr[0]]; ys=[a[1] for a in rr[0]]
                if (max(xs)-min(xs))*(max(ys)-min(ys))>6000: parks.append(rr[0])
    elif p.get('highway') in ('trunk','primary','motorway','secondary') and g['type']=='LineString':
        roads.append({'w':22 if p.get('highway') in ('trunk','motorway') else 14 if p.get('highway')=='primary' else 10,'p':simp([P(*c) for c in g['coordinates']],3)})
    elif p.get('man_made')=='bridge' and g['type'] in ('Polygon','MultiPolygon'):
        for poly in rings(g):
            r=simpring([P(*c) for c in poly[0]],2)
            if len(r)>=3: bridges.append({'n':p.get('name',''),'p':r})
# 물 마스크 (10m 격자)
xs=[a[0] for w in water for a in w[0]]; zs=[a[1] for w in water for a in w[0]]
X0,X1,Z0,Z1=-11000,10500,-8300,3400; RES=10
W=int((X1-X0)/RES); H=int((Z1-Z0)/RES)
mask=Image.new('L',(W,H),0); dr=ImageDraw.Draw(mask)
tp=lambda a:((a[0]-X0)/RES,(a[1]-Z0)/RES)
for w in water:
    dr.polygon([tp(a) for a in w[0]],fill=255)
    for hole in w[1:]: dr.polygon([tp(a) for a in hole],fill=0)
pm=Image.new('L',(W,H),0); dp_=ImageDraw.Draw(pm)
for pk in parks: dp_.polygon([tp(a) for a in pk],fill=255)
for pl in palaces: dp_.polygon([tp(a) for a in pl['p']],fill=255)
# 세종대로·광화문광장 (숭례문~광화문)은 넓게 비움
dp_.line([tp(P(126.9754,37.5600)),tp(P(126.9780,37.5662)),tp(P(126.9768,37.5700)),tp(P(126.9768,37.5755))],fill=255,width=int(90/RES))
# 산(북악산, 인왕산, 남산) 자리는 비움
for (lon,lat,rr) in [(126.9786,37.5925,700),(126.9579,37.585,650),(126.9882,37.5513,800)]:
    cx,cz=tp(P(lon,lat)); dp_.ellipse([cx-rr/RES,cz-rr/RES,cx+rr/RES,cz+rr/RES],fill=255)
for r in roads: dp_.line([tp(a) for a in r['p']],fill=255,width=max(1,int(r['w']/RES)))
mp=mask.load(); pp=pm.load()
# 한강 위 다리만: 가운데가 물 위인 다리
def cen(r): return (sum(a[0] for a in r)/len(r), sum(a[1] for a in r)/len(r))
def onwater(x,z):
    i,j=int((x-X0)/RES),int((z-Z0)/RES); return 0<=i<W and 0<=j<H and mp[i,j]>0
hb=[b for b in bridges if onwater(*cen(b['p'])) ]
# 건물: 땅 위, 공원·큰길 밖에 무작위로
import random; random.seed(3)
blds=[]
for _ in range(90000):
    x=random.uniform(X0,X1); z=random.uniform(Z0,Z1); i,j=int((x-X0)/RES),int((z-Z0)/RES)
    if mp[i,j] or pp[i,j]: continue
    # 강에서 너무 가까우면(강변 도로·공원 자리) 비움
    near=False
    for di in (-6,0,6):
        for dj in (-6,0,6):
            ii,jj=i+di,j+dj
            if 0<=ii<W and 0<=jj<H and mp[ii,jj]: near=True
    if near: continue
    blds.append([round(x),round(z)])
    if len(blds)>=19000: break
LM={'gwanghwamun':(126.9768,37.5759),'geunjeong':(126.9769,37.5785),'sungnyemun':(126.9754,37.5600),'ddp':(127.0097,37.5669),'myeongdong':(126.9875,37.5634),'cityhall':(126.9780,37.5662),'seoulst':(126.9727,37.5555),'bugak':(126.9786,37.5925),'inwang':(126.9579,37.585),'63':(126.9402,37.5198),'gukhoe':(126.9143,37.5318),'namsan':(126.9882,37.5513),'sebit':(126.9954,37.5121),'lotte':(127.1027,37.5126)}
lm={k:P(*v) for k,v in LM.items()}
# 돔이 지날 길: 한강 다리들의 가운데를 서쪽→동쪽으로
names=['마포대교','원효대교','한강철교 B선','동작대교','반포대교','한남대교','동호대교','성수대교','영동대교','청담대교','잠실대교','올림픽대교']
path=[]
for n in names:
    for b in hb:
        if b['n']==n: path.append(cen(b['p'])); break
path=[[round(a[0]),round(a[1])] for a in path]
loop=[(126.9880,37.5560),(126.9754,37.5600),(126.9780,37.5662),(126.9768,37.5700),(126.9768,37.5738),(126.9790,37.5690),(126.9945,37.5691),(127.0090,37.5670),(126.9870,37.5640),(126.9880,37.5560)]
path2=[[round(a[0]),round(a[1])] for a in (P(*q) for q in loop)]
out={'attribution':'© OpenStreetMap contributors (ODbL)','origin':[LON0,LAT0],'water':water,'parks':parks,'roads':roads,'bridges':[b['p'] for b in hb],'bridgeNames':[b['n'] for b in hb],'buildings':blds,'landmarks':lm,'path':path,'path2':path2,'palaces':palaces,'walls':walls,'streams':streams,'bounds':[X0,X1,Z0,Z1]}
json.dump(out,open(OUT,'w'),separators=(',',':'),ensure_ascii=False)
print('water',len(water),'parks',len(parks),'roads',len(roads),'bridges all',len(bridges),'han',len(hb),'buildings',len(blds),'path',len(path))
print('path',path); print('landmarks',lm)
