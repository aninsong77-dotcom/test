// 통 서비스 워커: 웹사이트를 앱처럼 설치하고, 인터넷이 없을 때도 열리게 함
// 화면 파일은 늘 인터넷에서 먼저 받아(고친 내용이 바로 반영) 저장해 두고, 연결이 없을 때만 저장본을 씀
// R(webR)·영상처럼 다른 주소나 큰 파일은 건드리지 않음
const CACHE = 'tong-v1';
const CORE = ['./', 'index.html', 'tong.webmanifest', 'tong-icon-192.png', 'tong-icon-512.png', 'tong-apple-touch-icon.png', 'tong-favicon.ico', 'tong-favicon-48.png'];
self.addEventListener('install', e => {
  e.waitUntil(caches.open(CACHE).then(c => Promise.all(CORE.map(u => c.add(u).catch(() => null)))));
  self.skipWaiting();
});
self.addEventListener('activate', e => {
  e.waitUntil(caches.keys().then(ks => Promise.all(ks.filter(k => k !== CACHE).map(k => caches.delete(k)))).then(() => self.clients.claim()));
});
self.addEventListener('fetch', e => {
  const req = e.request, url = new URL(req.url);
  if (req.method !== 'GET' || url.origin !== location.origin || req.headers.has('range') || /\/video\//.test(url.pathname)) return;
  e.respondWith(fetch(req).then(res => {
    if (res.ok && res.status === 200) { const copy = res.clone(); caches.open(CACHE).then(c => c.put(req, copy)).catch(() => {}); }
    return res;
  }).catch(() => caches.match(req, { ignoreSearch: true }).then(r => r || (req.mode === 'navigate' ? caches.match('./').then(x => x || caches.match('index.html')) : null)).then(r => r || Response.error())));
});
