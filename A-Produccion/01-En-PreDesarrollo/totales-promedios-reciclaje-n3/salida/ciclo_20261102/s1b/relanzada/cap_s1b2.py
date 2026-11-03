"""Tanda S1b relanzada (2026-11-03): instrumento HTML de la S1b (cap_s1b_punt.py) con sus dos defectos corregidos.

Defecto 1 (corregido): «fórmula + unidad partidas» no reconocía la unidad cuando el signo iba dentro de la fórmula
  (<mtext>kilos.</mtext>), así que en V1' contaba sobre 428 y no sobre 1158. Ahora el <mtext> de la unidad admite un
  signo final (. , : ; ! ? ) ] » ” …).
Defecto 2 (corregido): el detector de signos huérfanos solo miraba [.,:;] al principio del nodo de texto. Ahora mira
  todo signo ante el que el texto no admite corte (. , : ; ! ? ) ] » ” …), y distingue el que sigue a una fórmula
  (<math>, o un elemento que la contiene, como el <span> nowrap de V2) del que sigue a otro elemento.
Métricas nuevas: «renglón que empieza con un espacio tras una fórmula» (el primer carácter del texto que sigue a la
  fórmula es un espacio visible al principio del renglón siguiente; un espacio normal colapsa y no cuenta) y
  «fórmula partida por dentro» (<math> con más de una caja).
Tanda S2a (H4 de la FASE 2C r5): en el JS, los U+00A0 y los signos no ASCII de las clases van con su escape
  (\\u00a0, \\u00bb, \\u201d, \\u2026). Hasta la S2a el JS llevaba 10 U+00A0 literales, aunque esta
  línea decía que no. Un assert comprueba que ni el JS ni este archivo llevan U+00A0, U+2009, U+202F o U+2060
  literales. Los comentarios del JS conservan sus tildes.
Uso: python3 -I cap_s1b2.py v1=<dir_html>,v2=<dir_html>,... <anchos: 360,320> <salida.json> <dir_capturas|-> <perfil>
     <dir_html>/sNNN/ej1.html, semillas 1..100. El perfil de Chromium va FUERA del repo."""
import asyncio, json, os, sys, subprocess, time, base64, urllib.request
import websockets
HTD = dict(x.split("=", 1) for x in sys.argv[1].split(","))
VERS = list(HTD)
WIDTHS = [int(x) for x in sys.argv[2].split(",")]
OUT, SHOTS, PROF = sys.argv[3], sys.argv[4], sys.argv[5]
assert "/RepositorioMatematicasICFES_R_Exams/" not in os.path.abspath(PROF) + "/", "el perfil de Chromium va fuera del repo"
if SHOTS != "-":
    os.makedirs(SHOTS, exist_ok=True)
JS = r"""(() => {
const SIG=/^[.,:;!?)\]\u00bb\u201d\u2026]/;
const o={iw:innerWidth, docSW:document.documentElement.scrollWidth, bodySW:document.body.scrollWidth, bodyCW:document.body.clientWidth, H:document.documentElement.scrollHeight};
const isSc=el=>{const x=getComputedStyle(el).overflowX;return x==='auto'||x==='scroll'};
const inSc=el=>{let p=el.parentElement;while(p&&p!==document.body){if(isSc(p))return true;p=p.parentElement}return false};
o.tables=[...document.querySelectorAll('table')].map(t=>{const c=t.parentElement;const tr=t.getBoundingClientRect(),cr=c.getBoundingClientRect();return {tableW:+tr.width.toFixed(1),contW:c.clientWidth,contScrollW:c.scrollWidth,needsScroll:c.scrollWidth>c.clientWidth+0.5,outside:(tr.right>cr.right+1&&!isSc(c))||tr.left<cr.left-1}});
o.spills=[];
for(const el of document.body.querySelectorAll('*')){if(inSc(el))continue;const r=el.getBoundingClientRect();if(!r.width&&!r.height)continue;if(r.right>innerWidth+1||r.left<-1){o.spills.push({tag:el.tagName,t:(el.textContent||'').trim().slice(0,40),l:+r.left.toFixed(1),r:+r.right.toFixed(1)});if(o.spills.length>8)break}}
const chrect=(n,i)=>{const g=document.createRange();g.setStart(n,i);g.setEnd(n,i+1);const rs=[...g.getClientRects()].filter(x=>x.width>0);return rs[0]||null};
const isF=el=>el&&el.nodeType===1&&(el.tagName.toUpperCase()==='MATH'||!!el.querySelector('math'));
const lastRect=el=>{const rs=[...el.getClientRects()].filter(x=>x.width>0);return rs[rs.length-1]||null};
const otraLinea=(a,b)=>Math.abs((a.top+a.bottom)/2-(b.top+b.bottom)/2)>Math.max(a.height,b.height)*0.5;
o.pairs=[];o.splits=[];o.punt=[];o.esp=[];
const UN=/(\d)([ \u00a0])(kilos?|cursos?)/g, EL=/\b(El|La)([ \u00a0])(Bosque|Pradera)/g, MU=/^([ \u00a0]*)(kilos?|cursos?)/;
const w=document.createTreeWalker(document.body,NodeFilter.SHOW_TEXT);let n;
while(n=w.nextNode()){const s=n.nodeValue;if(!s)continue;const intd=!!n.parentElement.closest('td,th');const inmath=!!n.parentElement.closest('math');
 let m;UN.lastIndex=0;
 if(!inmath)while(m=UN.exec(s)){const a=chrect(n,m.index),b=chrect(n,m.index+2);const nb=m[2]==='\u00a0';
  if(a&&b){const sp=otraLinea(a,b);o.pairs.push({k:'cifra-unidad',nb,intd,sp});if(sp)o.splits.push({k:'cifra-unidad',nb,intd,t:s.slice(Math.max(0,m.index-25),m.index+14)})}}
 EL.lastIndex=0;
 if(!inmath)while(m=EL.exec(s)){const a=chrect(n,m.index+1),b=chrect(n,m.index+3);const nb=m[2]==='\u00a0';
  if(a&&b){const sp=otraLinea(a,b);o.pairs.push({k:'El-Bosque',nb,intd,sp});if(sp)o.splits.push({k:'El-Bosque',nb,intd,t:s.slice(Math.max(0,m.index-25),m.index+20)})}}
 if(inmath)continue;
 const ps=n.previousSibling;
 // fórmula seguida de su unidad, fuera de la fórmula (base, V2)
 const mm=MU.exec(s);
 if(mm&&isF(ps)){const b=chrect(n,mm[1].length);const mt=ps.tagName.toUpperCase()==='MATH'?ps:ps.querySelector('math');const mr=lastRect(mt);
  if(b&&mr){const sp=otraLinea(mr,b);const nb=mm[1].indexOf('\u00a0')>=0;o.pairs.push({k:'formula-unidad',nb,intd,sp});if(sp)o.splits.push({k:'formula-unidad',nb,intd,t:'MATH '+(mt.querySelector('annotation')||mt).textContent.slice(-25)+' '+mm[2]})}}
 // signo ante el que el texto no admite corte, tras un elemento
 if(SIG.test(s)&&!intd&&ps&&ps.nodeType===1){const pr=lastRect(ps);const b=chrect(n,0);
  if(pr&&b){const sp=otraLinea(pr,b);o.punt.push({sp,fm:isF(ps),prev:ps.tagName.toLowerCase(),t:s.slice(0,1)});
   if(sp)o.splits.push({k:'signo',fm:isF(ps),t:(ps.textContent||'').slice(-25)+'|'+s.slice(0,12)})}}
 // renglón que empieza con un espacio tras una fórmula
 if(/^[ \u00a0]/.test(s)&&!intd&&isF(ps)){const pr=lastRect(ps);const b=chrect(n,0);
  if(pr){const sp=!!b&&otraLinea(pr,b);o.esp.push({sp,nb:s[0]==='\u00a0'});if(sp)o.splits.push({k:'espacio-inicial',t:(ps.textContent||'').slice(-25)+'|'+s.slice(0,12)})}}
}
// unidad dentro de la fórmula (V1, V1'): <mtext> con la unidad, con o sin signo final
for(const mt of document.querySelectorAll('math')){const tk=[...mt.querySelectorAll('mn,mi,mo,mtext')];
 tk.forEach((t,i)=>{if(t.tagName.toLowerCase()!=='mtext'||!/^[\s\u00a0]*(kilos?|cursos?)[.,:;!?)\]\u00bb\u201d\u2026]*[\s\u00a0]*$/.test(t.textContent))return;
  let p=null;for(let j=i-1;j>=0;j--){if(tk[j].textContent.trim()){p=tk[j];break}}
  const a=p&&p.getBoundingClientRect(),b=t.getBoundingClientRect();
  const sp=!!a&&otraLinea(a,b);const intd=!!mt.closest('td,th');
  o.pairs.push({k:'formula-unidad-dentro',nb:false,intd,sp,rects:mt.getClientRects().length,sig:/[.,:;!?)\]\u00bb\u201d\u2026]/.test(t.textContent)});
  if(sp)o.splits.push({k:'formula-unidad-dentro',t:(mt.textContent||'').slice(-24)})})}
o.mathSplit=[...document.querySelectorAll('math')].filter(m=>[...m.getClientRects()].filter(x=>x.width>0).length>1).length;
o.nowrap=[...document.querySelectorAll('span')].filter(x=>getComputedStyle(x).whiteSpace==='nowrap').length;
o.mathCount=document.querySelectorAll('math').length;
return JSON.stringify(o)})()"""
# H4 de la FASE 2C r5 (tanda S2a): ningún espacio especial literal, ni en el JS ni en este archivo.
for _c in (0xA0, 0x2009, 0x202F, 0x2060):
    assert chr(_c) not in JS, f"U+{_c:04X} literal en el JS"
    assert chr(_c) not in open(__file__, encoding="utf-8").read(), f"U+{_c:04X} literal en el fuente"



class CDP:
    def __init__(s, ws): s.ws, s.n, s.pend, s.ev = ws, 0, {}, asyncio.Queue()
    async def start(s): s.t = asyncio.create_task(s._rd())
    async def _rd(s):
        try:
            async for m in s.ws:
                m = json.loads(m)
                if "id" in m and m["id"] in s.pend: s.pend.pop(m["id"]).set_result(m)
                else: await s.ev.put(m)
        except Exception: pass
    async def send(s, method, **p):
        s.n += 1; i = s.n; f = asyncio.get_running_loop().create_future(); s.pend[i] = f
        await s.ws.send(json.dumps({"id": i, "method": method, "params": p}))
        r = await asyncio.wait_for(f, 60)
        if "error" in r: raise RuntimeError(r["error"])
        return r.get("result", {})
    async def wev(s, name, to):
        end = time.time() + to
        while True:
            l = end - time.time()
            if l <= 0: return False
            try: m = await asyncio.wait_for(s.ev.get(), l)
            except asyncio.TimeoutError: return False
            if m.get("method") == name: return True


async def job(port, sem, v, seed, vw):
    f = f"{HTD[v]}/s{seed:03d}/ej1.html"; tid = None
    async with sem:
        try:
            async def run():
                nonlocal tid
                t = json.loads(urllib.request.urlopen(urllib.request.Request(f"http://127.0.0.1:{port}/json/new?about:blank", method="PUT"), timeout=10).read()); tid = t["id"]
                async with websockets.connect(t["webSocketDebuggerUrl"], max_size=None) as ws:
                    c = CDP(ws); await c.start()
                    await c.send("Page.enable"); await c.send("Runtime.enable")
                    await c.send("Emulation.setDeviceMetricsOverride", width=vw, height=900, deviceScaleFactor=1, mobile=False)
                    await c.send("Page.navigate", url="file://" + f); await c.wev("Page.loadEventFired", 25)
                    await c.send("Runtime.evaluate", expression="document.fonts.ready.then(()=>1)", awaitPromise=True)
                    await asyncio.sleep(0.15)
                    r = await c.send("Runtime.evaluate", expression=JS, returnByValue=True)
                    if "exceptionDetails" in r: raise RuntimeError(json.dumps(r["exceptionDetails"])[:300])
                    m = json.loads(r["result"]["value"])
                    if SHOTS != "-":
                        h = min(m["H"], 9000)
                        sh = await c.send("Page.captureScreenshot", format="png", captureBeyondViewport=True, clip={"x": 0, "y": 0, "width": vw, "height": h, "scale": 1})
                        open(f"{SHOTS}/{v}_s{seed:03d}_{vw}.png", "wb").write(base64.b64decode(sh["data"]))
                    return m
            m = await asyncio.wait_for(run(), 90)
            return dict(v=v, seed=seed, vw=vw, m=m)
        except Exception as e: return dict(v=v, seed=seed, vw=vw, error=repr(e)[:300])
        finally:
            if tid:
                try: urllib.request.urlopen(f"http://127.0.0.1:{port}/json/close/{tid}", timeout=5).read()
                except Exception: pass


async def main():
    port = 9377
    p = subprocess.Popen(["chromium", "--headless", "--no-sandbox", "--disable-gpu", "--hide-scrollbars", "--force-device-scale-factor=1",
                          f"--remote-debugging-port={port}", "--user-data-dir=" + PROF, "--no-first-run", "about:blank"],
                         stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    for _ in range(150):
        try: urllib.request.urlopen(f"http://127.0.0.1:{port}/json/version", timeout=1).read(); break
        except Exception: time.sleep(0.2)
    sem = asyncio.Semaphore(6)
    try:
        return await asyncio.gather(*[job(port, sem, v, s, vw) for s in range(1, 101) for v in VERS for vw in WIDTHS])
    finally:
        p.terminate()

res = asyncio.run(main())
json.dump(res, open(OUT, "w"))
print(len(res), "errores:", sum('error' in r for r in res), [r for r in res if 'error' in r][:3])
print("iw:", sorted({r['m']['iw'] for r in res if 'm' in r}))
