#!/usr/bin/env python3
"""evidencia_6b.py — recortes de evidencia (cortes número/unidad, nombres partidos, puntuación huérfana) tomados en la misma
sesión de navegador que la medición. Uso: python3 evidencia_6b.py ./html . ./fonts.conf <dir_temporal>"""
import asyncio, json, os, sys, subprocess, time, base64, urllib.request
import websockets
from PIL import Image, ImageDraw
HTML_DIR, OUT, FONTS, SP = sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4]
SEEDS = [7, 3, 2, 1, 4, 5, 6] + list(range(8, 25)); os.makedirs(OUT + "/evidencia", exist_ok=True)
PRE_JS = r"""(() => {
  const BOX = __BOX__, VW = __VW__;
  if (BOX) {
    const st = document.createElement('style');
    st.textContent = `
      html,body{margin:0;padding:0;background:#fff}
      body{font-family:system-ui,-apple-system,"Segoe UI",Roboto,"Helvetica Neue",Arial,sans-serif;font-size:15px;line-height:1.5;color:#212529;width:__VW__px}
      body > h2{display:none}
      body > ol{margin:0;padding:0;list-style:none}
      body > ol > li{margin:0 25px;padding:0;outline:1px dashed #d33}
      body > ol > li > h4{margin:14px 0 6px;font-size:12px;color:#999;font-weight:normal;text-transform:uppercase;letter-spacing:.06em}
      .opt{display:flex;align-items:flex-start;margin:4px 0}
      .opt .ctl{flex:0 0 16px;height:16px;margin:4px 8px 0 0;border:1px solid #777;border-radius:50%;box-sizing:border-box}
      .opt.chk .ctl{border-radius:3px}
      .opt .txt{flex:1 1 auto;min-width:0}
      .numfield{display:inline-block;width:90px;height:30px;border:1px solid #888;border-radius:4px;box-sizing:border-box;margin:2px 0 8px}`;
    document.head.appendChild(st);
  }
  const q = document.querySelector('body > ol > li');
  const kids = [...q.children];
  const solIdx = kids.findIndex(e => e.tagName === 'H4' && /Solution/i.test(e.textContent));
  const qk = solIdx >= 0 ? kids.slice(0, solIdx) : kids;
  const info = [];
  qk.forEach((e, i) => {
    if (e.tagName !== 'P') return;
    const m = (e.textContent.match(/^Parte (\d)\./) || [])[1]; if (!m) return;
    const n = +m, nxt = qk[i + 1];
    if (!nxt || nxt.tagName !== 'P') { info.push({parte: n, error: 'sin parrafo siguiente'}); return; }
    if (n >= 3 && n <= 5) {
      info.push({parte: n, tipo: 'num', vacio: nxt.textContent.trim() === ''});
      if (BOX) { const f = document.createElement('div'); f.innerHTML = '<span class="numfield"></span>'; nxt.replaceWith(f); }
      return;
    }
    const opts = nxt.innerHTML.split(' / ').map(s => s.trim());
    const seen = new Set(); let dup = 0; opts.forEach(t => { if (seen.has(t)) dup++; seen.add(t); });
    info.push({parte: n, tipo: n === 6 ? 'mchoice' : 'schoice', nopt: opts.length, dup, vacias: opts.filter(t => !t).length,
               maxLen: Math.max(...opts.map(t => t.length)), minLen: Math.min(...opts.map(t => t.length))});
    if (BOX) {
      const wrap = document.createElement('div');
      opts.forEach(t => { const d = document.createElement('div'); d.className = 'opt' + (n === 6 ? ' chk' : '');
        d.innerHTML = '<span class="ctl"></span><span class="txt"></span>'; d.querySelector('.txt').innerHTML = t; wrap.appendChild(d); });
      nxt.replaceWith(wrap);
    }
  });
  window.__gaps = info; return 1;
})()"""
WAIT_MJ = """new Promise(res => { const t = setTimeout(() => res('timeout'), 6000);
  if (window.MathJax && MathJax.Hub && MathJax.Hub.Queue) { MathJax.Hub.Queue(() => { clearTimeout(t); res('mj'); }); } else { clearTimeout(t); res('sin-mj'); } })"""
JS3 = r"""(() => {
  const li = document.querySelector('body > ol > li');
  const h4s = [...li.children].filter(e => e.tagName === 'H4'); const solTop = h4s[1] ? h4s[1].getBoundingClientRect().top + scrollY : 1e9;
  const o = {names: [], units: [], orphans: []};
  const w = document.createTreeWalker(li, NodeFilter.SHOW_TEXT); let n;
  while ((n = w.nextNode())) {
    const t = n.data; if (!t.trim()) continue; const el = n.parentElement;
    if (el.closest('math, .MathJax, .MathJax_Preview, .MathJax_Display, script, style, .MJX_Assistive_MathML, table')) continue;
    const rect = (a, b) => { const r = document.createRange(); r.setStart(n, a); r.setEnd(n, b); const rr = r.getClientRects(); return rr.length ? rr[0] : null; };
    let m; const re1 = /(\d+)(\s)(kilos?|cursos?)\b/g;
    while ((m = re1.exec(t))) { const a = rect(m.index, m.index + m[1].length), u = rect(m.index + m[1].length + 1, m.index + m[0].length);
      if (a && u && u.top > a.bottom - 3 && a.top + scrollY < solTop) o.units.push(Math.round(a.top + scrollY)); }
    const re2 = /\b(El|La)(\s)(Bosque|Pradera)\b/g;
    while ((m = re2.exec(t))) { const a = rect(m.index, m.index + m[1].length), u = rect(m.index + m[1].length + 1, m.index + m[0].length);
      if (a && u && u.top > a.bottom - 3 && a.top + scrollY < solTop) o.names.push(Math.round(a.top + scrollY)); }
  }
  for (const e of li.querySelectorAll('.MathJax, math')) {
    if (e.parentElement.closest('math, .MathJax, .MJX_Assistive_MathML')) continue;
    const er = e.getBoundingClientRect(); if (er.width < 1 || er.height < 1) continue;
    let nx = e.nextSibling; while (nx && nx.nodeType === 1 && nx.tagName === 'SCRIPT') nx = nx.nextSibling;
    if (nx && nx.nodeType === 3 && /^[.,;:)»]/.test(nx.data)) { const r = document.createRange(); r.setStart(nx, 0); r.setEnd(nx, 1); const pr = r.getClientRects()[0];
      if (pr && (pr.top > er.bottom - 2 || pr.left < er.left - 1)) o.orphans.push(Math.round(er.top + scrollY)); }
  }
  return JSON.stringify(o);
})()"""
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
        await self.ws.send(json.dumps({"id": i, "method": method, "params": p}))
        r = await asyncio.wait_for(fut, 60)
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
    p = subprocess.Popen(["chromium", "--headless", "--no-sandbox", "--disable-gpu", "--hide-scrollbars", "--force-device-scale-factor=1",
        f"--remote-debugging-port={port}", f"--user-data-dir={profile}", "--no-first-run", "--disable-extensions", "about:blank"],
        env=env, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    for _ in range(150):
        try: urllib.request.urlopen(f"http://127.0.0.1:{port}/json/version", timeout=1).read(); return p
        except Exception: time.sleep(0.2)
    raise RuntimeError("chromium no arrancó")

ME = json.load(open(f"{OUT}/metricas_extra.json"))
useeds = []
for r in ME:
    if "m" in r and r["label"] == "box320" and any(not u["sol"] for u in r["m"]["unit"]) and r["seed"] not in useeds: useeds.append(r["seed"])
TARGETS = [(9, "box320", 320, "box"), (19, "box320", 320, "box"), (6, "box360", 360, "box"), (10, "box360", 360, "box"), (16, "box360", 360, "box"), (6, "plain360", 360, "plain"),
           (13, "plain320", 320, "plain"), (6, "plain320", 320, "plain")] + [(s, "box320", 320, "box") for s in useeds[:5]]
async def job(port, sem, seed, label, vw, mode):
    html = os.path.abspath(os.path.join(HTML_DIR, f"v{SEEDS.index(seed) + 1}.html")); tid = None
    async with sem:
        try:
            req = urllib.request.Request(f"http://127.0.0.1:{port}/json/new?about:blank", method="PUT"); t = json.loads(urllib.request.urlopen(req, timeout=10).read()); tid = t["id"]
            async with websockets.connect(t["webSocketDebuggerUrl"], max_size=None, open_timeout=15) as ws:
                c = CDP(ws); await c.start()
                for m in ("Page.enable", "Runtime.enable"): await c.send(m)
                await c.send("Emulation.setDeviceMetricsOverride", width=vw, height=900, deviceScaleFactor=1, mobile=False)
                await c.send("Page.navigate", url="file://" + html); await c.wait_event("Page.loadEventFired", 25)
                await c.send("Runtime.evaluate", expression=PRE_JS.replace("__BOX__", "true" if mode == "box" else "false").replace("__VW__", str(vw)), returnByValue=True)
                await c.send("Runtime.evaluate", expression=WAIT_MJ, awaitPromise=True, returnByValue=True)
                await c.send("Runtime.evaluate", expression="document.fonts.ready.then(() => 1)", awaitPromise=True); await asyncio.sleep(0.5)
                o = json.loads((await c.send("Runtime.evaluate", expression=JS3, returnByValue=True))["result"]["value"]); outs = []
                for kind in ("names", "orphans", "units"):
                    for k, y in enumerate(o[kind][:2]):
                        shot = await c.send("Page.captureScreenshot", format="png", captureBeyondViewport=True, clip={"x": 0, "y": max(0, y - 90), "width": vw, "height": 200, "scale": 1})
                        fn = f"{OUT}/evidencia/{kind}_s{seed:02d}_{label}_{k + 1}.png"; open(fn, "wb").write(base64.b64decode(shot["data"])); outs.append((kind, fn, seed, label, y))
                return {"seed": seed, "label": label, "n": {k: len(v) for k, v in o.items()}, "outs": outs}
        except Exception as e: return {"seed": seed, "label": label, "error": repr(e)[:200]}
        finally:
            if tid:
                try: urllib.request.urlopen(f"http://127.0.0.1:{port}/json/close/{tid}", timeout=5).read()
                except Exception: pass
async def main():
    procs = []
    try:
        procs.append(launch(9361, {}, SP + "/prof_plain3")); procs.append(launch(9362, {"FONTCONFIG_FILE": FONTS}, SP + "/prof_box3"))
        sems = {9361: asyncio.Semaphore(4), 9362: asyncio.Semaphore(4)}
        return await asyncio.gather(*[job(9362 if md == "box" else 9361, sems[9362 if md == "box" else 9361], s, lb, vw, md) for (s, lb, vw, md) in TARGETS])
    finally:
        for p in procs: p.terminate()
res = asyncio.run(main()); allouts = []
for r in res:
    print(r["seed"], r["label"], r.get("n", r.get("error"))); allouts += r.get("outs", [])
for kind in ("names", "orphans", "units"):
    its = [o for o in allouts if o[0] == kind][:6]
    if not its: continue
    ims = []
    for _, fn, seed, label, y in its:
        im = Image.open(fn).convert("RGB"); cv = Image.new("RGB", (im.size[0], im.size[1] + 14), (255, 255, 255)); cv.paste(im, (0, 14)); ImageDraw.Draw(cv).text((3, 1), f"s{seed:02d} {label} y={y}", fill=(200, 0, 0)); ims.append(cv)
    cols = 3 if kind != "units" else 4; rows = [ims[i:i + cols] for i in range(0, len(ims), cols)]
    W = max(sum(i.size[0] + 6 for i in r) for r in rows); H = sum(max(i.size[1] for i in r) + 6 for r in rows); mo = Image.new("RGB", (W, H), (235, 235, 240)); yy = 0
    for r in rows:
        x = 0
        for i in r: mo.paste(i, (x, yy)); x += i.size[0] + 6
        yy += max(i.size[1] for i in r) + 6
    mo.save(f"{OUT}/evidencia/montaje_{kind}.png"); print("montaje", kind, len(its), mo.size)
