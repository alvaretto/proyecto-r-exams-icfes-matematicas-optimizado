#!/usr/bin/env python3
"""cortes_p2_r3.py — líneas de cada opción de la Parte 2, de su pregunta y de las viñetas de la Solution, a 320 y 360 (modo box). Uso: python3 cortes_p2_r3.py html . fonts.conf tmp"""
import asyncio, json, os, sys, subprocess, time, urllib.request, re, collections
import websockets
HTML_DIR, OUT, FONTS, SP = sys.argv[1:5]
src = open("capturar_cdp_6b_r3.py", encoding="utf-8").read()
PRE_JS = re.search(r'PRE_JS = r"""(.*?)"""', src, re.S).group(1)
SEEDS = [7,3,2,1,4,5,6]+list(range(8,25))
MEAS = r"""(() => {
  const li = document.querySelector('body > ol > li'); const kids = [...li.children];
  const h4s = kids.filter(e => e.tagName === 'H4'); const solTop = h4s[1] ? h4s[1].getBoundingClientRect().top + scrollY : 1e9;
  const topOf = el => { let c = el; while (c && c.parentElement !== li) c = c.parentElement; return c; };
  const isHead = k => k.tagName === 'P' && k.children.length === 1 && k.firstElementChild.tagName === 'STRONG' && k.textContent.trim() === k.firstElementChild.textContent.trim();
  const isParteQ = k => k.tagName === 'P' && k.firstElementChild && k.firstElementChild.tagName === 'STRONG' && /^Parte \d\./.test(k.textContent.trim());
  const where = (el, y) => { const t = topOf(el); let i = kids.indexOf(t), sec = '';
    for (; i >= 0; i--) { const k = kids[i]; if (y < solTop ? isParteQ(k) : isHead(k)) { sec = k.textContent.trim().slice(0, 34); break; } }
    const lie = el.closest('li'); let item = '';
    if (lie && lie !== li) { const par = lie.parentElement; item = (par.tagName === 'OL' && par.getAttribute('type') === 'a') ? 'fb-' + 'abcdef'[[...par.children].indexOf(lie)] : lie.textContent.trim().slice(0, 22); }
    else item = t ? t.textContent.trim().slice(0, 22) : '';
    return (y < solTop ? 'Q' : 'S') + '|' + sec + '|' + item; };
  const out = {u1: [], u2: [], p: [], iw: innerWidth};
  const w = document.createTreeWalker(li, NodeFilter.SHOW_TEXT); let n;
  while ((n = w.nextNode())) { const t = n.data; if (!t.trim()) continue; const el = n.parentElement;
    if (el.closest('math, .MathJax, .MathJax_Preview, .MathJax_Display, script, style, .MJX_Assistive_MathML')) continue;
    const rect = (a, b) => { const r = document.createRange(); r.setStart(n, a); r.setEnd(n, b); const rr = r.getClientRects(); return rr.length ? rr[0] : null; };
    let m; const re1 = /(\d+)(\s)(kilos?|cursos?)\b/g;
    while ((m = re1.exec(t))) { const a = rect(m.index, m.index + m[1].length), u = rect(m.index + m[1].length + 1, m.index + m[0].length);
      if (a && u) out.u1.push({split: u.top > a.bottom - 3, w: where(el, a.top + scrollY)}); } }
  for (const e of li.querySelectorAll('math, .MathJax')) { if (e.parentElement.closest('math, .MathJax')) continue;
    let nx = e.nextSibling; while (nx && nx.nodeType === 1 && nx.tagName === 'SCRIPT') nx = nx.nextSibling; if (!nx || nx.nodeType !== 3) continue;
    const er = e.getBoundingClientRect(), y = er.top + scrollY; const mm = /^([\s\u00a0]*)(kilos?|cursos?)\b/.exec(nx.data);
    if (mm) { const r = document.createRange(); r.setStart(nx, mm[1].length); r.setEnd(nx, mm[1].length + mm[2].length); const u = r.getClientRects()[0]; if (u) out.u2.push({split: u.top > er.bottom - 3, w: where(e, y)}); }
    else if (/^[.,;:)»]/.test(nx.data)) { const r = document.createRange(); r.setStart(nx, 0); r.setEnd(nx, 1); const pr = r.getClientRects()[0]; if (pr) out.p.push({split: (pr.top > er.bottom - 2 || pr.left < er.left - 1), w: where(e, y)}); } }
  return JSON.stringify(out); })()"""
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

import collections, json
res = asyncio.run(main()); json.dump(res, open("cortes_ctx_r3.json", "w"), ensure_ascii=False)
errs = [r for r in res if "error" in r]; ok = [r for r in res if "m" in r]; print("errores:", len(errs), errs[:2], "| iw:", sorted({(r["vw"], r["m"]["iw"]) for r in ok}))
NAMES = {"u1": "A) número en texto plano + unidad ('15 kilos')", "u2": "B) ecuación (MathML) + unidad ('= 224 / kilos')", "p": "C) puntuación tras ecuación"}
for cls in ("u1", "u2", "p"):
    tot = collections.Counter(); spl = collections.Counter(); seedset = collections.defaultdict(set); loc = collections.Counter()
    for r in ok:
        for x in r["m"][cls]:
            qs, sec, item = x["w"].split("|", 2); key = (qs, sec if qs == "S" else sec[:9]); tot[key] += 1
            if x["split"]:
                spl[key] += 1; seedset[key].add((r["vw"], r["seed"])); loc[(qs, sec[:30], item[:20] if not item.startswith("fb-") else item)] += 1
    print("\n== " + NAMES[cls] + f"  (320 y 360, 24 semillas; MathML nativo): {sum(spl.values())} cortes de {sum(tot.values())} casos ==")
    for key in sorted(tot, key=lambda k: (-spl[k], str(k))): print(f"   {key[0]} | {key[1]:34s} cortes {spl[key]:3d} / {tot[key]:4d}  (semillas: {len({s for _, s in seedset[key]})})")
    print("   ubicaciones con corte (top 10):", [(k, v) for k, v in loc.most_common(10)])
