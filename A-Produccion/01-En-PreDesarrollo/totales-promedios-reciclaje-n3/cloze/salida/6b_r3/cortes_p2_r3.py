#!/usr/bin/env python3
"""cortes_p2_r3.py — líneas de cada opción de la Parte 2, de su pregunta y de las viñetas de la Solution, a 320 y 360 (modo box). Uso: python3 cortes_p2_r3.py html . fonts.conf tmp"""
import asyncio, json, os, sys, subprocess, time, urllib.request, re, collections
import websockets
HTML_DIR, OUT, FONTS, SP = sys.argv[1:5]
src = open("capturar_cdp_6b_r3.py", encoding="utf-8").read()
PRE_JS = re.search(r'PRE_JS = r"""(.*?)"""', src, re.S).group(1)
SEEDS = [7,3,2,1,4,5,6]+list(range(8,25))
MEAS = r"""(() => {
  const li = document.querySelector('body > ol > li');
  const lines = el => { const w = document.createTreeWalker(el, NodeFilter.SHOW_TEXT); const ws = []; let n;
    while ((n = w.nextNode())) { const t = n.textContent; const re = /\S+/g; let m;
      while ((m = re.exec(t))) { const r = document.createRange(); r.setStart(n, m.index); r.setEnd(n, m.index + m[0].length); const rc = r.getClientRects(); if (rc.length) ws.push({w: m[0], top: Math.round(rc[0].top)}); } }
    const out = []; let cur = null; for (const x of ws) { if (cur === null || Math.abs(x.top - cur) > 4) { out.push([x.w]); cur = x.top; } else out[out.length - 1].push(x.w); }
    return out.map(l => l.join(' ')); };
  const o = {}; o.opts = [...li.querySelectorAll('.opt .txt')].map(lines);
  const ps = [...li.querySelectorAll('p')];
  o.q2 = ps.filter(p => /^Parte 2\./.test(p.textContent.trim())).map(lines);
  const h = ps.find(p => /^Parte 2 ·/.test(p.textContent.trim())); o.head = h ? lines(h) : null;
  const ul = h ? h.nextElementSibling : null; o.sol = ul && ul.tagName === 'UL' ? [...ul.children].map(lines) : null;
  o.iw = innerWidth; return JSON.stringify(o); })()"""
class CDP:
    def __init__(self, ws): self.ws, self.n, self.pend, self.ev = ws, 0, {}, asyncio.Queue()
    async def start(self): self.task = asyncio.create_task(self._rd())
    async def _rd(self):
        try:
            async for msg in self.ws:
                m = json.loads(msg)
                if "id" in m and m["id"] in self.pend: self.pend.pop(m["id"]).set_result(m)
                else: await self.ev.put(m)
        except Exception: pass
    async def send(self, method, **p):
        self.n += 1; i = self.n; fut = asyncio.get_running_loop().create_future(); self.pend[i] = fut
        await self.ws.send(json.dumps({"id": i, "method": method, "params": p})); r = await asyncio.wait_for(fut, 60)
        if "error" in r: raise RuntimeError(f"{method}: {r['error']}")
        return r.get("result", {})
    async def wait_event(self, name, timeout):
        end = time.time() + timeout
        while True:
            left = end - time.time()
            if left <= 0: return False
            try: m = await asyncio.wait_for(self.ev.get(), left)
            except asyncio.TimeoutError: return False
            if m.get("method") == name: return True
def launch(port, env_extra, profile):
    env = dict(os.environ); env.update(env_extra)
    p = subprocess.Popen(["chromium", "--headless", "--no-sandbox", "--disable-gpu", "--hide-scrollbars", "--force-device-scale-factor=1", f"--remote-debugging-port={port}", f"--user-data-dir={profile}", "--no-first-run", "--disable-extensions", "about:blank"], env=env, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    for _ in range(150):
        try: urllib.request.urlopen(f"http://127.0.0.1:{port}/json/version", timeout=1).read(); return p
        except Exception: time.sleep(0.2)
    raise RuntimeError("chromium no arrancó")
async def job(port, sem, seed, idx, vw):
    html = os.path.abspath(os.path.join(HTML_DIR, f"v{idx}.html")); tid = None
    async with sem:
        try:
            req = urllib.request.Request(f"http://127.0.0.1:{port}/json/new?about:blank", method="PUT"); t = json.loads(urllib.request.urlopen(req, timeout=10).read()); tid = t["id"]
            async with websockets.connect(t["webSocketDebuggerUrl"], max_size=None, open_timeout=15) as ws:
                c = CDP(ws); await c.start()
                for m in ("Page.enable", "Runtime.enable", "Network.enable"): await c.send(m)
                await c.send("Network.setBlockedURLs", urls=["*mathjax*"])
                await c.send("Emulation.setDeviceMetricsOverride", width=vw, height=900, deviceScaleFactor=1, mobile=False)
                await c.send("Page.navigate", url="file://" + html); await c.wait_event("Page.loadEventFired", 25)
                r = await c.send("Runtime.evaluate", expression=PRE_JS.replace("__BOX__", "true").replace("__VW__", str(vw)), returnByValue=True)
                if "exceptionDetails" in r: raise RuntimeError("PRE_JS")
                await c.send("Runtime.evaluate", expression="document.fonts.ready.then(() => 1)", awaitPromise=True); await asyncio.sleep(0.3)
                r = await c.send("Runtime.evaluate", expression=MEAS, returnByValue=True)
                if "exceptionDetails" in r: raise RuntimeError("MEAS " + json.dumps(r["exceptionDetails"])[:200])
                return {"seed": seed, "vw": vw, "m": json.loads(r["result"]["value"])}
        except Exception as e: return {"seed": seed, "vw": vw, "error": repr(e)[:200]}
        finally:
            if tid:
                try: urllib.request.urlopen(f"http://127.0.0.1:{port}/json/close/{tid}", timeout=5).read()
                except Exception: pass
async def main():
    p = launch(9343, {"FONTCONFIG_FILE": os.path.abspath(FONTS)}, os.path.abspath(SP) + "/prof_cortes")
    try:
        sem = asyncio.Semaphore(6)
        return await asyncio.gather(*[job(9343, sem, s, i, vw) for i, s in enumerate(SEEDS, start=1) for vw in (320, 360)])
    finally: p.terminate()
res = asyncio.run(main()); json.dump(res, open("cortes_p2_r3.json", "w"), ensure_ascii=False)
errs = [r for r in res if "error" in r]; print("errores:", len(errs), errs[:2]); ok = [r for r in res if "m" in r]
print("iw:", sorted({(r["vw"], r["m"]["iw"]) for r in ok}))
FW = {"de", "del", "la", "el", "los", "las", "un", "una", "y", "o", "que", "en", "por", "con", "al", "a", "su", "sus", "se", "lo", "e", "u"}
uniq = collections.OrderedDict()
for r in sorted(ok, key=lambda r: (r["vw"], r["seed"])):
    for k, ls in enumerate(r["m"]["opts"][4:8]): uniq.setdefault((r["vw"], " ".join(ls)), (ls, []))[1].append(r["seed"])
for vw in (320, 360):
    print(f"\n=== Parte 2, opciones únicas @ {vw} (líneas | semillas) ===")
    for (w, txt), (ls, seeds) in uniq.items():
        if w != vw: continue
        flag = []
        if len(ls) > 1 and len(ls[-1].split()) == 1: flag.append("ORFANA-ultima-linea")
        if any(len(l.split()) == 1 for l in ls[:-1]): flag.append("LINEA-DE-1-PALABRA")
        fwend = [l.split()[-1] for l in ls[:-1] if l.split() and l.split()[-1].lower().strip('.,;') in FW]
        if fwend: flag.append("fin-de-linea-en-palabra-funcional:" + ",".join(fwend))
        print(f"[{len(ls)} lín] {' // '.join(ls)}  | n={len(seeds)} {','.join('%02d' % s for s in seeds[:4])}{'…' if len(seeds) > 4 else ''} {'<<' + ';'.join(flag) + '>>' if flag else ''}")
print("\n=== Solution Parte 2: viñetas y encabezado ===")
seen = set()
for r in sorted(ok, key=lambda r: (r["vw"], r["seed"])):
    m = r["m"]
    if m["sol"] is None: print("SIN LISTA", r["seed"], r["vw"]); continue
    for ls in [m["head"]] + m["sol"]:
        key = (r["vw"], " ".join(ls))
        if key in seen: continue
        seen.add(key); fl = []
        if len(ls) > 1 and len(ls[-1].split()) == 1: fl.append("ORFANA")
        if any(len(l.split()) == 1 for l in ls[:-1]): fl.append("LINEA-DE-1-PALABRA")
        print(f"@{r['vw']} s{r['seed']:02d} [{len(ls)}] {' // '.join(ls)} {'<<' + ';'.join(fl) + '>>' if fl else ''}")
