#!/usr/bin/env python3
"""medir_cortes_6b.py — métricas de cortes de línea (número/unidad, nombres propios), puntuación huérfana tras ecuaciones,
encabezados en negrita y lista (a)-(f) de la Solution. Reusa el cliente CDP de capturar_cdp_6b.py. Uso: python3 medir_cortes_6b.py ./html . ./fonts.conf <dir_temporal>"""
import asyncio, json, os, sys, subprocess, time, urllib.request
from collections import Counter
import websockets
HTML_DIR, OUT, FONTS, SP = sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4]
SEEDS = [7, 3, 2, 1, 4, 5, 6] + list(range(8, 25))
MODES = [("plain320", 320, "plain"), ("plain360", 360, "plain"), ("box320", 320, "box"), ("box360", 360, "box")]
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
JS2 = r"""(() => {
  const li = document.querySelector('body > ol > li');
  const h4s = [...li.children].filter(e => e.tagName === 'H4'); const solTop = h4s[1] ? h4s[1].getBoundingClientRect().top + scrollY : 1e9;
  const o = {unit: [], names: [], punct: [], units_total: 0};
  const w = document.createTreeWalker(li, NodeFilter.SHOW_TEXT); let n;
  while ((n = w.nextNode())) {
    const t = n.data; if (!t.trim()) continue; const el = n.parentElement;
    if (el.closest('math, .MathJax, .MathJax_Preview, .MathJax_Display, script, style, .MJX_Assistive_MathML')) continue;
    const rect = (a, b) => { const r = document.createRange(); r.setStart(n, a); r.setEnd(n, b); const rr = r.getClientRects(); return rr.length ? rr[0] : null; };
    let m; const re1 = /(\d+)(\s)(kilos?|cursos?)\b/g;
    while ((m = re1.exec(t))) { o.units_total++; const a = rect(m.index, m.index + m[1].length), u = rect(m.index + m[1].length + 1, m.index + m[0].length);
      if (a && u && u.top > a.bottom - 3) o.unit.push({txt: m[0], y: Math.round(a.top + scrollY), sol: a.top + scrollY > solTop, tabla: !!el.closest('table')}); }
    const re2 = /\b(El|La)(\s)(Bosque|Pradera)\b/g;
    while ((m = re2.exec(t))) { const a = rect(m.index, m.index + m[1].length), u = rect(m.index + m[1].length + 1, m.index + m[0].length);
      o.names.push({txt: m[0], nbsp: m[2] === ' ', split: !!(a && u && u.top > a.bottom - 3), tabla: !!el.closest('table'), sol: !!(a && a.top + scrollY > solTop)}); }
  }
  for (const e of li.querySelectorAll('math, .MathJax')) {
    if (e.parentElement.closest('math, .MathJax')) continue;
    let nx = e.nextSibling; while (nx && nx.nodeType === 1 && nx.tagName === 'SCRIPT') nx = nx.nextSibling;
    if (nx && nx.nodeType === 3 && /^[.,;:)»]/.test(nx.data)) { const r = document.createRange(); r.setStart(nx, 0); r.setEnd(nx, 1); const pr = r.getClientRects()[0], er = e.getBoundingClientRect();
      o.punct.push({ch: nx.data[0], orphan: !!(pr && (pr.top > er.bottom - 2 || pr.left < er.left - 1)), y: Math.round(er.top + scrollY)}); }
  }
  o.hgap = [...li.querySelectorAll('table')].map(t => { const rs = [...t.querySelectorAll('thead th')].map(h => { const sp = h.querySelector('span') || h; const rg = document.createRange(); rg.selectNodeContents(sp);
      const rr = [...rg.getClientRects()].filter(x => x.width > 0.5); return rr.length ? {l: Math.min(...rr.map(x => x.left)), r: Math.max(...rr.map(x => x.right))} : null; });
    const g = []; for (let i = 1; i < rs.length; i++) if (rs[i] && rs[i - 1]) g.push(+(rs[i].l - rs[i - 1].r).toFixed(1)); return g; });
  const HEAD = ['Respuestas', 'Procedimiento correcto', 'Análisis de los distractores', 'Propiedades del concepto', 'Caso específico', 'Reflexión metacognitiva', 'Estrategia para evitar el error'];
  o.heads = HEAD.map(h => { const c = [...li.querySelectorAll('p, strong, b, h5, h6')].find(x => x.textContent.trim() === h); return c ? (parseInt(getComputedStyle(c).fontWeight) >= 600 || !!c.closest('strong, b') || !!c.querySelector('strong, b')) : null; });
  const ols = [...li.querySelectorAll('ol')]; const last = ols[ols.length - 1]; o.fb = last ? {n: last.children.length, type: last.getAttribute('type')} : null;
  o.mathN = li.querySelectorAll('math').length; o.mjN = li.querySelectorAll('.MathJax').length;
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
                    for m in ("Page.enable", "Runtime.enable"): await c.send(m)
                    await c.send("Emulation.setDeviceMetricsOverride", width=vw, height=900, deviceScaleFactor=1, mobile=False)
                    await c.send("Page.navigate", url="file://" + html); await c.wait_event("Page.loadEventFired", 25)
                    r = await c.send("Runtime.evaluate", expression=PRE_JS.replace("__BOX__", "true" if mode == "box" else "false").replace("__VW__", str(vw)), returnByValue=True)
                    mj = (await c.send("Runtime.evaluate", expression=WAIT_MJ, awaitPromise=True, returnByValue=True))["result"].get("value")
                    await c.send("Runtime.evaluate", expression="document.fonts.ready.then(() => 1)", awaitPromise=True); await asyncio.sleep(0.5)
                    r = await c.send("Runtime.evaluate", expression=JS2, returnByValue=True)
                    if "exceptionDetails" in r: raise RuntimeError(json.dumps(r["exceptionDetails"])[:300])
                    m = json.loads(r["result"]["value"]); m["mj"] = mj; return m
            return {"seed": seed, "label": label, "m": await asyncio.wait_for(run(), 120)}
        except Exception as e: return {"seed": seed, "label": label, "error": repr(e)[:300]}
        finally:
            if tid:
                try: urllib.request.urlopen(f"http://127.0.0.1:{port}/json/close/{tid}", timeout=5).read()
                except Exception: pass
async def main():
    procs = []
    try:
        procs.append(launch(9351, {}, SP + "/prof_plain2")); procs.append(launch(9352, {"FONTCONFIG_FILE": FONTS}, SP + "/prof_box2"))
        sems = {9351: asyncio.Semaphore(6), 9352: asyncio.Semaphore(6)}
        return await asyncio.gather(*[job(9352 if md == "box" else 9351, sems[9352 if md == "box" else 9351], s, i, lb, vw, md) for i, s in enumerate(SEEDS, 1) for (lb, vw, md) in MODES])
    finally:
        for p in procs: p.terminate()
res = asyncio.run(main()); json.dump(res, open(f"{OUT}/metricas_extra.json", "w"), ensure_ascii=False, indent=1)
ok = [r for r in res if "m" in r]; print("errores:", [(r["seed"], r["label"], r["error"]) for r in res if "error" in r][:5])
for lb, _, _ in MODES:
    rs = [r for r in ok if r["label"] == lb]; sp = [r for r in rs if r["m"]["unit"]]
    print(f"{lb:9s} n={len(rs)} 'N kilos|cursos' partidos entre líneas: {sum(len(r['m']['unit']) for r in rs)} de {sum(r['m']['units_total'] for r in rs)} en {len(sp)} versiones | en opciones/enunciado: {sum(1 for r in rs for u in r['m']['unit'] if not u['sol'])} | en Solution: {sum(1 for r in rs for u in r['m']['unit'] if u['sol'])} | en tabla: {sum(1 for r in rs for u in r['m']['unit'] if u['tabla'])}")
    nm = [(r["seed"], x) for r in rs for x in r["m"]["names"]]
    print(f"{'':9s} nombres El Bosque/La Pradera: {len(nm)} apariciones, partidos: {[(s, x['txt'], 'tabla' if x['tabla'] else ('sol' if x['sol'] else 'enunc/opc')) for s, x in nm if x['split']]}, con NBSP: {sum(1 for _, x in nm if x['nbsp'])}, en tabla: {sum(1 for _, x in nm if x['tabla'])}")
    pu = [(r["seed"], x) for r in rs for x in r["m"]["punct"]]
    print(f"{'':9s} puntuación tras ecuación: {len(pu)}, huérfana (en otra línea): {sum(1 for _, x in pu if x['orphan'])} -> {[(s, x['ch'], x['y']) for s, x in pu if x['orphan']][:6]}")
    print(f"{'':9s} separación mínima entre el TEXTO de cabeceras vecinas (px): {min(g for r in rs for gg in r['m']['hgap'] for g in gg)}; por tabla T1/T2: {min(g for r in rs for g in r['m']['hgap'][0])}/{min(g for r in rs for g in r['m']['hgap'][1])}")
    print(f"{'':9s} encabezados Solution no en negrita o ausentes: {[(r['seed'], r['m']['heads']) for r in rs if not all(r['m']['heads'])][:4]} | lista (a)-(f): {Counter((json.dumps(r['m']['fb'])) for r in rs)} | mj: {dict(Counter(r['m']['mj'] for r in rs))} | math/MathJax nodos: {sorted({(r['m']['mathN'] > 0, r['m']['mjN'] > 0) for r in rs})}")
