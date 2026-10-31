#!/usr/bin/env python3
"""capturar_cdp_6b.py — Paso 6b: captura + métricas por CDP con viewport REAL.
Motivo: `chromium --headless --window-size=320,...` deja innerWidth=500 (mínimo de ventana del modo headless nuevo),
o sea un layout de 500 px recortado. Con Emulation.setDeviceMetricsOverride el viewport es el pedido.
Modos: plain = el HTML de exams2html tal cual; box = emulación APROXIMADA de la caja .qtext del Moodle del colegio
(pantalla - 50 px: 270 a 320, 310 a 360), Roboto 15 px, opciones en vertical con radio/casilla y campo numérico.
Uso: python3 capturar_cdp_6b.py <dir_html> <dir_salida> <fonts.conf> <bloquear_mathjax 0|1> <dir_temporal>"""
import asyncio, json, os, sys, subprocess, time, base64, glob, shutil, urllib.request
from collections import Counter
import websockets
from PIL import Image, ImageDraw

HTML_DIR, OUT, FONTS, BLOCK, SP = sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4] == "1", sys.argv[5]
SEEDS = [7, 3, 2, 1, 4, 5, 6] + list(range(8, 25))
MODES = [("plain320", 320, "plain"), ("plain360", 360, "plain"), ("plain1024", 1024, "plain"),
         ("box320", 320, "box"), ("box360", 360, "box")]
for d in ("shots", "strips", "textos"):
    os.makedirs(os.path.join(OUT, d), exist_ok=True)

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

METRICS_JS = r"""(() => {
  const li = document.querySelector('body > ol > li'); const box = li.getBoundingClientRect();
  const isScroller = el => { const o = getComputedStyle(el).overflowX; return o === 'auto' || o === 'scroll'; };
  const inScroller = el => { let p = el.parentElement; while (p && p !== li) { if (isScroller(p)) return true; p = p.parentElement; } return false; };
  const linesOf = el => { const cs = getComputedStyle(el); let lh = parseFloat(cs.lineHeight); if (!isFinite(lh)) lh = parseFloat(cs.fontSize) * 1.2;
    return Math.max(1, Math.round(el.getBoundingClientRect().height / lh)); };
  const o = {iw: innerWidth, docSW: document.documentElement.scrollWidth, docCW: document.documentElement.clientWidth,
             pageH: Math.ceil(document.documentElement.scrollHeight), box: {left: +box.left.toFixed(1), right: +box.right.toFixed(1), width: +box.width.toFixed(1)}};
  o.scrollers = [...li.querySelectorAll('div')].filter(isScroller).map(d => { const t = d.querySelector('table'); const tr = t ? t.getBoundingClientRect() : null;
    return {clientW: d.clientWidth, scrollW: d.scrollWidth, tableW: tr ? +tr.width.toFixed(1) : null, needsScroll: d.scrollWidth > d.clientWidth + 0.5,
            head: t && t.rows[0] ? [...t.rows[0].cells].map(c => c.textContent.trim().slice(0, 32)) : [], rows: t ? t.rows.length : 0}; });
  o.spills = [];
  for (const el of li.querySelectorAll('*')) { if (inScroller(el)) continue; const r = el.getBoundingClientRect(); if (r.width === 0 && r.height === 0) continue;
    if (r.right > box.right + 1 || r.left < box.left - 1) { o.spills.push({tag: el.tagName, text: (el.textContent || '').trim().slice(0, 40), left: +r.left.toFixed(1), right: +r.right.toFixed(1)}); if (o.spills.length >= 10) break; } }
  o.cells = [];
  for (const c of li.querySelectorAll('th, td')) { const s = c.querySelector('span') || c; const n = linesOf(s); const txt = c.textContent.trim();
    if (n > 1 || /Bosque|Pradera/.test(txt)) o.cells.push({tag: c.tagName, text: txt.slice(0, 44), lines: n}); }
  o.headGaps = [...li.querySelectorAll('table')].map(t => { const rects = [...t.querySelectorAll('thead th')].map(h => { const rg = document.createRange(); rg.selectNodeContents(h);
      const rr = [...rg.getClientRects()].filter(x => x.width > 0.5); return rr.length ? {l: Math.min(...rr.map(x => x.left)), r: Math.max(...rr.map(x => x.right))} : null; });
    const g = []; for (let i = 1; i < rects.length; i++) if (rects[i] && rects[i - 1]) g.push(+(rects[i].l - rects[i - 1].r).toFixed(1)); return g; });
  const txt = li.innerText || '';
  const pats = {asteriscos: /\*\*/, html: /<span|<\/span|<div|<table|<td|<th|<strong|<\/p>/, estilo: /font-size|padding|inline-block|overflow-x|display:/,
    latex_raw: /\{=latex\}|makeatletter|newcounter|nobreak|afterheading|pandocbounded/, answer: /##ANSWER/, dolar: /\$/, tex: /\\times|\\frac|\\text|\\cdot|\\left|\\right|\\mathrm|\\le\b|\\ge\b/,
    na: /\bNA\b|\bNaN\b|\bInf\b|\bNULL\b|no disponible/, mojibake: /Ã|Â|â€|�/, md_tabla: /\|\s*-{3,}|:::/, backtick: /`/};
  o.leaks = []; for (const k in pats) { const m = txt.match(pats[k]); if (m) o.leaks.push({k, ctx: txt.slice(Math.max(0, m.index - 30), m.index + 40).replace(/\n/g, ' ')}); }
  o.optLines = [...li.querySelectorAll('.opt .txt')].map(linesOf);
  o.gaps = window.__gaps; o.maths = li.querySelectorAll('math').length; o.tables = li.querySelectorAll('table').length;
  const h4 = [...li.children].filter(e => e.tagName === 'H4'); o.solY = h4.length > 1 ? Math.round(h4[1].getBoundingClientRect().top + scrollY) : null;
  o.mjax = (typeof MathJax !== 'undefined') ? (document.querySelectorAll('.MathJax, .MathJax_Display, .MJX-TEX, mjx-container').length) : -1;
  o.text = txt; return JSON.stringify(o);
})()"""

WAIT_MJ = """new Promise(res => { const t = setTimeout(() => res('timeout'), 6000);
  if (window.MathJax && MathJax.Hub && MathJax.Hub.Queue) { MathJax.Hub.Queue(() => { clearTimeout(t); res('mj'); }); } else { clearTimeout(t); res('sin-mj'); } })"""

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

async def job(port, sem, seed, idx, label, vw, mode):
    html = os.path.abspath(os.path.join(HTML_DIR, f"v{idx}.html")); tid = None
    async with sem:
        try:
            async def run():
                nonlocal tid
                req = urllib.request.Request(f"http://127.0.0.1:{port}/json/new?about:blank", method="PUT")
                t = json.loads(urllib.request.urlopen(req, timeout=10).read()); tid = t["id"]
                async with websockets.connect(t["webSocketDebuggerUrl"], max_size=None, open_timeout=15) as ws:
                    c = CDP(ws); await c.start()
                    for m in ("Page.enable", "Runtime.enable", "Network.enable"): await c.send(m)
                    if BLOCK: await c.send("Network.setBlockedURLs", urls=["*mathjax*"])
                    await c.send("Emulation.setDeviceMetricsOverride", width=vw, height=900, deviceScaleFactor=1, mobile=False)
                    await c.send("Page.navigate", url="file://" + html)
                    ok_load = await c.wait_event("Page.loadEventFired", 25)
                    r = await c.send("Runtime.evaluate", expression=PRE_JS.replace("__BOX__", "true" if mode == "box" else "false").replace("__VW__", str(vw)), returnByValue=True)
                    if "exceptionDetails" in r: raise RuntimeError("PRE_JS: " + json.dumps(r["exceptionDetails"])[:300])
                    mj = (await c.send("Runtime.evaluate", expression=WAIT_MJ, awaitPromise=True, returnByValue=True))["result"].get("value")
                    await c.send("Runtime.evaluate", expression="document.fonts.ready.then(() => 1)", awaitPromise=True)
                    await asyncio.sleep(0.4)
                    r = await c.send("Runtime.evaluate", expression=METRICS_JS, returnByValue=True)
                    if "exceptionDetails" in r: raise RuntimeError("METRICS_JS: " + json.dumps(r["exceptionDetails"])[:300])
                    m = json.loads(r["result"]["value"]); m["mj"] = mj; m["load"] = ok_load
                    h = min(int(m["pageH"]), 16000); m["truncada"] = int(m["pageH"]) > 16000
                    shot = await c.send("Page.captureScreenshot", format="png", captureBeyondViewport=True, clip={"x": 0, "y": 0, "width": vw, "height": h, "scale": 1})
                    with open(f"{OUT}/shots/s{seed:02d}_{label}.png", "wb") as f: f.write(base64.b64decode(shot["data"]))
                    if m["docSW"] > vw + 1:
                        sw = await c.send("Page.captureScreenshot", format="png", captureBeyondViewport=True, clip={"x": 0, "y": 0, "width": m["docSW"], "height": h, "scale": 1})
                        with open(f"{OUT}/shots/s{seed:02d}_{label}_wide.png", "wb") as f: f.write(base64.b64decode(sw["data"]))
                    return m
            m = await asyncio.wait_for(run(), 120)
            return {"seed": seed, "idx": idx, "label": label, "vw": vw, "mode": mode, "m": m}
        except Exception as e:
            return {"seed": seed, "idx": idx, "label": label, "vw": vw, "mode": mode, "error": repr(e)[:300]}
        finally:
            if tid:
                try: urllib.request.urlopen(f"http://127.0.0.1:{port}/json/close/{tid}", timeout=5).read()
                except Exception: pass

async def main():
    procs = []
    try:
        procs.append(launch(9341, {}, SP + "/prof_plain"))
        procs.append(launch(9342, {"FONTCONFIG_FILE": FONTS}, SP + "/prof_box"))
        sems = {9341: asyncio.Semaphore(6), 9342: asyncio.Semaphore(6)}
        tasks = [job(9342 if mode == "box" else 9341, sems[9342 if mode == "box" else 9341], seed, i, label, vw, mode)
                 for i, seed in enumerate(SEEDS, start=1) for (label, vw, mode) in MODES]
        return await asyncio.gather(*tasks)
    finally:
        for p in procs: p.terminate()

def make_strips(seed, label, vw):
    png = f"{OUT}/shots/s{seed:02d}_{label}.png"
    if not os.path.exists(png): return
    im = Image.open(png).convert("RGB"); W, H = im.size
    sh, per = (1500, 4) if vw <= 400 else (1300, 2)
    sl = [im.crop((0, y, W, min(y + sh, H))) for y in range(0, H, sh)]
    for k in range(0, len(sl), per):
        g = sl[k:k + per]; cv = Image.new("RGB", (len(g) * (W + 4) - 4, sh), (255, 255, 255)); dr = ImageDraw.Draw(cv)
        for j, s in enumerate(g):
            cv.paste(s, (j * (W + 4), 0))
            if j: dr.rectangle((j * (W + 4) - 4, 0, j * (W + 4) - 1, sh), fill=(120, 140, 170))
        cv.save(f"{OUT}/strips/s{seed:02d}_{label}_p{k // per + 1}.png")

t0 = time.time()
res = asyncio.run(main())
print(f"capturas terminadas en {time.time() - t0:.0f} s; trabajos: {len(res)}")
errs = [r for r in res if "error" in r]
print(f"ERRORES de captura: {len(errs)}"); [print("  ", r["seed"], r["label"], r["error"]) for r in errs[:10]]
ok = [r for r in res if "m" in r]
for r in ok:
    if r["label"] == "box320":
        with open(f"{OUT}/textos/s{r['seed']:02d}.txt", "w", encoding="utf-8") as f: f.write(r["m"]["text"])
    r["m"].pop("text", None)
json.dump(res, open(f"{OUT}/metricas.json", "w"), ensure_ascii=False, indent=1)
for r in ok: make_strips(r["seed"], r["label"], r["vw"])
for label, vw, mode in MODES:
    fs = [f"{OUT}/shots/s{s:02d}_{label}.png" for s in sorted(SEEDS) if os.path.exists(f"{OUT}/shots/s{s:02d}_{label}.png")]
    if fs: subprocess.run(["magick", "montage", "-label", "%t", "-pointsize", "13"] + fs + ["-tile", "6x", "-geometry", "200x820+4+4", "-background", "white", f"{OUT}/contact_{label}.png"])
print("mathjax:", dict(Counter(r["m"]["mj"] for r in ok)), "| iw por modo:", {l: sorted({r["m"]["iw"] for r in ok if r["label"] == l}) for l, _, _ in MODES})
lines = []
for r in sorted(ok, key=lambda r: (r["label"], r["seed"])):
    m = r["m"]; sc = m["scrollers"]; ns = sum(1 for s in sc if s["needsScroll"]); tdw = [c for c in m["cells"] if c["tag"] == "TD" and c["lines"] > 1]
    gp = [g for gg in m["headGaps"] for g in gg]
    lines.append(f"s{r['seed']:02d} {r['label']:9s} H={m['pageH']:5d} solY={m['solY']} box={m['box']['width']:5.0f} sw={m['docSW']:4d} scroll={ns}/{len(sc)} tdwrap={len(tdw)} spills={len(m['spills'])} leaks={len(m['leaks'])} gapmin={min(gp) if gp else None} partes={[g.get('parte') for g in (m['gaps'] or [])]}")
open(f"{OUT}/resumen_metricas.txt", "w").write("\n".join(lines))
print("\n".join(l for l in lines if "box320" in l))
