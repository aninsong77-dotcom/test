const http=require('http'),fs=require('fs'),path=require('path');
const APP='/home/user/aninsong77-dotcom/gonglbaki', W=__dirname;
const TYPES={'.html':'text/html; charset=utf-8','.js':'text/javascript; charset=utf-8','.css':'text/css','.png':'image/png','.jpg':'image/jpeg','.woff2':'font/woff2'};
function serve(){return new Promise(r=>{const s=http.createServer((q,res)=>{let p=decodeURIComponent(q.url.split('?')[0]);let f=p.startsWith('/__font/')?path.join(W,'node_modules/pretendard/dist/web/variable/woff2',path.basename(p)):path.join(APP,p==='/'?'index.html':p);fs.readFile(f,(e,d)=>{if(e){res.writeHead(404);return res.end()}res.writeHead(200,{'Content-Type':TYPES[path.extname(f)]||'application/octet-stream'});res.end(d)})});s.listen(0,()=>r(s))})}
const TW=fs.readFileSync(path.join(W,'tw.css'),'utf8');
const FONTCSS=`@font-face{font-family:'Malgun Gothic';src:url(/__font/PretendardVariable.woff2) format('woff2');font-weight:45 920}@font-face{font-family:'Pretendard';src:url(/__font/PretendardVariable.woff2) format('woff2');font-weight:45 920}`;
async function setup(browser,opts={}){
  const ctx=await browser.newContext({viewport:null});
  await ctx.route('https://cdn.tailwindcss.com/**',r=>r.fulfill({contentType:'text/javascript',body:`(function(){var s=document.createElement('style');s.textContent=${JSON.stringify(TW+FONTCSS)};document.head.appendChild(s)})()`}));
  await ctx.route(/cdn\.tailwindcss\.com\/?$/,r=>r.fulfill({contentType:'text/javascript',body:`(function(){var s=document.createElement('style');s.textContent=${JSON.stringify(TW+FONTCSS)};document.head.appendChild(s)})()`}));
  await ctx.addInitScript(()=>{try{localStorage.setItem('gb_geo_tour_done','1')}catch(e){}});
  return ctx;
}
module.exports={serve,setup};
