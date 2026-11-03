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
Uso: python3 -E -P cap_s1b2.py v1=<dir_html>,v2=<dir_html>,... <anchos: 360,320> <salida.json> <dir_capturas|-> <perfil>
     <dir_html>/sNNN/ej1.html, semillas 1..100. El perfil de Chromium va FUERA del repo.
Tanda S2b (H4 de la FASE 2C r6): -E -P y no -I. websockets está en el site de usuario, que -I deja fuera; -P no mete
  en sys.path el directorio del script ni el de trabajo, y -E ignora las variables PYTHON*. El directorio de trabajo
  va FUERA del repo: lo comprueba un assert antes de importar websockets."""
import asyncio, json, os, sys, subprocess, time, base64, urllib.request
MARCA_REPO = "/RepositorioMatematicasICFES_R_Exams/"
assert MARCA_REPO not in os.path.realpath(os.getcwd()) + "/", "el directorio de trabajo va fuera del repo (H4 de la r6)"
import websockets
HTD = dict(x.split("=", 1) for x in sys.argv[1].split(","))
VERS = list(HTD)
WIDTHS = [int(x) for x in sys.argv[2].split(",")]
OUT, SHOTS, PROF = sys.argv[3], sys.argv[4], sys.argv[5]
assert "/RepositorioMatematicasICFES_R_Exams/" not in os.path.abspath(PROF) + "/", "el perfil de Chromium va fuera del repo"
if SHOTS != "-":
    os.makedirs(SHOTS, exist_ok=True)
JS = r"""(() => {
const li=document.querySelector('body > ol > li');const o={iw:innerWidth,H:document.documentElement.scrollHeight,boxes:{}};
const bx=(k,els)=>{const rs=els.map(e=>e.getBoundingClientRect());o.boxes[k]=[Math.min(...rs.map(r=>r.top))+scrollY,Math.max(...rs.map(r=>r.bottom))+scrollY]};
const K=[...li.children];const T=e=>(e.textContent||'').trim();
const iH=K.findIndex((e,i)=>e.tagName==='H4'&&i>0);
const iQ1=K.findIndex(e=>e.tagName==='P'&&/^En un colegio/.test(T(e)));const iQ2=K.findIndex(e=>e.tagName==='P'&&/^Las Partes 2 a 6/.test(T(e)));
bx('Q_P1',K.slice(1,iQ2));
const ul=K.find((e,i)=>i>iH&&e.tagName==='UL');bx('Resp_P1',[ul.children[0]]);
const ip=K.findIndex((e,i)=>i>iH&&e.tagName==='P'&&/^Parte 1\./.test(T(e)));const ip2=K.findIndex((e,i)=>i>iH&&e.tagName==='P'&&/^Parte 2\./.test(T(e)));
bx('Proc_P1',K.slice(ip,ip2));
const ic=K.findIndex(e=>e.tagName==='P'&&/^En la Parte 1,/.test(T(e)));bx('Caso_P1',[K[ic]]);
const i6=K.findIndex((e,i)=>i>iH&&e.tagName==='P'&&/^Parte 6\./.test(T(e)));bx('Parte6',[K[i6]]);
const ia=K.findIndex(e=>e.tagName==='P'&&/^Parte 1 · ERR/.test(T(e)));bx('Anal_P1_primero',K.slice(ia,ia+6));
const fb=K.filter(e=>e.tagName==='OL'&&e.getAttribute('type')==='a').pop();bx('FB_a',[fb.children[0]]);
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
                          f"--remote-debugging-port={port}", "--user-data-dir=" + PROF, "--no-first-run", "--host-resolver-rules=MAP * ~NOTFOUND , EXCLUDE 127.0.0.1", "about:blank"],
                         stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    for _ in range(150):
        try: urllib.request.urlopen(f"http://127.0.0.1:{port}/json/version", timeout=1).read(); break
        except Exception: time.sleep(0.2)
    sem = asyncio.Semaphore(6)
    try:
        return await asyncio.gather(*[job(port, sem, v, s, vw) for s in (1,4,7) for v in VERS for vw in WIDTHS])
    finally:
        p.terminate()

res = asyncio.run(main())
json.dump(res, open(OUT, "w"))
print(len(res), "errores:", sum('error' in r for r in res), [r for r in res if 'error' in r][:3])
print("iw:", sorted({r['m']['iw'] for r in res if 'm' in r}))
