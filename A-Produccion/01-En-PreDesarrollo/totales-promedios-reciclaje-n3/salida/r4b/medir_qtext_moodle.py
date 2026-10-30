"""Ancho real de la caja de la pregunta (.qtext) en el Moodle del colegio y desborde de la tabla del ítem 50.

Residuo H2 de la FASE 2C r4b. Informe y cifras: medicion_qtext_moodle.md (mismo directorio).
Solo lectura en el servidor: inicia sesión con la cuenta de prueba y abre la revisión de un intento que ya
existe. Las versiones se ponen en .qtext en el DOM del navegador (Runtime.evaluate); nada se envía.
Credenciales: ~/.config/aulaviva/prueba.json (no se imprimen). Plantilla CDP:
Proyectos-Varios/Moodle/tests/mathjax_navegador.py.

  Rscript generar_moodle_100.R <ruta del .Rmd> <dir>
  FONTCONFIG_FILE=<fonts.conf con Roboto> python3 medir_qtext_moodle.py --base https://aula.senodeteta.com \
      --attempt 4658 --xmldir <dir> --salida medicion.json --png s006.png

Sin FONTCONFIG_FILE, system-ui resuelve en Linux a la fuente por defecto (Noto Sans), no a Roboto.
En el aula, --base es http://<IP del servidor en 192.168.0.x>.
"""
import argparse, asyncio, base64, glob, json, os, re, subprocess, sys, tempfile, time
import xml.etree.ElementTree as ET
import requests, websockets

ap = argparse.ArgumentParser()
ap.add_argument('--base', required=True)
ap.add_argument('--attempt', type=int, required=True)
ap.add_argument('--xmldir', required=True)
ap.add_argument('--anchos', default='320,360')
ap.add_argument('--quien', default='prof')
ap.add_argument('--puerto', type=int, default=9351)
ap.add_argument('--png', default='')
ap.add_argument('--salida', required=True)
a = ap.parse_args()
base = a.base.rstrip('/')
cred = json.load(open(os.path.expanduser('~/.config/aulaviva/prueba.json')))[a.quien]

versiones = []
for f in sorted(glob.glob(a.xmldir + '/s*.xml')):
    t = ET.parse(f).getroot().find(".//question[@type='multichoice']/questiontext/text").text
    versiones.append({'semilla': int(re.search(r's(\d+)\.xml', f).group(1)), 'html': t,
                      'bosque': ('Bosque' in t or 'Pradera' in t)})

s = requests.Session()
r = s.get(base + '/login/index.php', timeout=30)
tok = re.search(r'name="logintoken" value="([^"]+)"', r.text)
r = s.post(base + '/login/index.php', data={'username': cred['username'], 'password': cred['password'],
                                            'logintoken': tok.group(1) if tok else ''}, timeout=30)
sess = s.cookies.get('MoodleSession')
if not sess or '/login/' in r.url:
    sys.exit(f'login fallido (terminó en {r.url})')

perfil = tempfile.mkdtemp()
nav = subprocess.Popen(['chromium', '--headless=new', '--disable-gpu', '--no-sandbox', '--hide-scrollbars',
                        f'--remote-debugging-port={a.puerto}', f'--user-data-dir={perfil}', 'about:blank'],
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
for _ in range(50):
    try:
        ws_url = [t for t in requests.get(f'http://127.0.0.1:{a.puerto}/json', timeout=1).json()
                  if t['type'] == 'page'][0]['webSocketDebuggerUrl']
        break
    except Exception:
        time.sleep(0.2)

CAJA = r"""(function(){
  var q = document.querySelector('.que .qtext');
  if (!q) return JSON.stringify({err: 'sin .qtext', url: location.href, titulo: document.title});
  var cs = getComputedStyle(q), r = q.getBoundingClientRect();
  var pl = parseFloat(cs.paddingLeft), pr = parseFloat(cs.paddingRight);
  function ancho(sel){ var e = document.querySelector(sel); return e ? Math.round(e.getBoundingClientRect().width*10)/10 : null; }
  return JSON.stringify({url: location.href, nque: document.querySelectorAll('.que').length,
    viewport: window.innerWidth, html_scroll: document.documentElement.scrollWidth,
    qtext_rect: Math.round(r.width*10)/10, qtext_left: Math.round(r.left*10)/10, qtext_contenido: q.clientWidth - pl - pr,
    padding: [pl, pr], fuente: cs.fontFamily, tam: cs.fontSize,
    region_main: ancho('#region-main'), formulation: ancho('.que .formulation'), content: ancho('.que .content'),
    info: ancho('.que .info')});
})()"""

PONER = r"""(function(html){
  var q = document.querySelector('.que .qtext');
  q.innerHTML = html;
  var caja = q.querySelector('div[style*="overflow-x"]'), tb = caja.querySelector('table');
  var bw = caja.clientWidth, sw = caja.scrollWidth, tw = tb.getBoundingClientRect().width;
  var w0 = tb.style.width; tb.style.width = 'min-content';
  var mc = tb.getBoundingClientRect().width; tb.style.width = w0;
  var ths = Array.from(tb.querySelectorAll('th')).map(function(t){ return Math.round(t.getBoundingClientRect().height); });
  return JSON.stringify({caja: bw, scroll: sw, tabla: Math.round(tw*10)/10, min_content: Math.round(mc*10)/10,
    desborda: sw > bw, pagina: document.documentElement.scrollWidth > window.innerWidth, alto_th: ths});
})"""


async def main():
    res = {'anchos': {}}
    async with websockets.connect(ws_url, max_size=100_000_000) as ws:
        n = [0]; pend = {}

        async def cmd(method, params=None):
            n[0] += 1
            fut = asyncio.get_event_loop().create_future()
            pend[n[0]] = fut
            await ws.send(json.dumps({'id': n[0], 'method': method, 'params': params or {}}))
            return await fut

        async def lector():
            async for raw in ws:
                m = json.loads(raw)
                if 'id' in m and m['id'] in pend:
                    pend.pop(m['id']).set_result(m.get('result', m.get('error')))

        tarea = asyncio.create_task(lector())
        for d in ('Runtime', 'Network', 'Page', 'DOM', 'CSS'):
            await cmd(d + '.enable')
        await cmd('Network.setCookie', {'name': 'MoodleSession', 'value': sess, 'url': base + '/'})
        for W in [int(x) for x in a.anchos.split(',')]:
            await cmd('Emulation.setDeviceMetricsOverride', {'width': W, 'height': 800, 'deviceScaleFactor': 2, 'mobile': True})
            await cmd('Page.navigate', {'url': f'{base}/mod/quiz/review.php?attempt={a.attempt}'})
            t0 = time.time(); caja = None
            while time.time() - t0 < 30:
                await asyncio.sleep(1)
                rr = await cmd('Runtime.evaluate', {'expression': 'document.readyState', 'returnByValue': True})
                if rr['result']['value'] == 'complete':
                    break
            await asyncio.sleep(1.5)
            rr = await cmd('Runtime.evaluate', {'expression': CAJA, 'returnByValue': True})
            caja = json.loads(rr['result']['value'])
            filas = []
            for v in versiones:
                rr = await cmd('Runtime.evaluate', {'expression': PONER + '(' + json.dumps(v['html']) + ')', 'returnByValue': True})
                m = json.loads(rr['result']['value']); m.update(semilla=v['semilla'], bosque=v['bosque'])
                if v['semilla'] == 6:
                    doc = await cmd('DOM.getDocument', {'depth': 0})
                    nd = await cmd('DOM.querySelector', {'nodeId': doc['root']['nodeId'], 'selector': '.que .qtext table tbody tr td:nth-child(2) span'})
                    pf = await cmd('CSS.getPlatformFontsForNode', {'nodeId': nd['nodeId']})
                    m['fuente_real'] = [f['familyName'] for f in pf.get('fonts', [])]
                    if a.png:
                        await cmd('Runtime.evaluate', {'expression': "document.querySelector('.que .qtext').scrollIntoView()"})
                        await asyncio.sleep(0.3)
                        shot = await cmd('Page.captureScreenshot', {'format': 'png'})
                        open(a.png.replace('.png', f'_{W}.png'), 'wb').write(base64.b64decode(shot['data']))
                filas.append(m)
            res['anchos'][W] = {'caja_vacia': caja, 'versiones': filas}
        tarea.cancel()
    json.dump(res, open(a.salida, 'w'), ensure_ascii=False, indent=1)
    for W, d in res['anchos'].items():
        c = d['caja_vacia']; f = d['versiones']
        print(f'== {W} px: .qtext {c.get("qtext_rect")} px (contenido {c.get("qtext_contenido")}, padding {c.get("padding")}), '
              f'#region-main {c.get("region_main")}, .formulation {c.get("formulation")}, .info {c.get("info")}, '
              f'fuente {c.get("tam")} {str(c.get("fuente"))[:60]}…, desborde de página {c.get("html_scroll")}>{c.get("viewport")}')
        cajas = sorted({x['caja'] for x in f})
        des = [x for x in f if x['desborda']]; desb = [x for x in des if x['bosque']]
        fb = [x for x in f if x['bosque']]
        print(f'   caja overflow-x: {cajas} px; tabla > caja en {len(des)}/{len(f)} '
              f'(El Bosque/La Pradera: {len(desb)}/{len(fb)}); página desbordada en {sum(x["pagina"] for x in f)}')
        mc = sorted(x['min_content'] for x in f)
        print(f'   ancho mínimo de la tabla: mín {mc[0]}, mediana {mc[len(mc)//2]}, máx {mc[-1]}; '
              f'El Bosque/La Pradera: máx {max(x["min_content"] for x in fb)}')
        if des:
            print('   desbordan: ' + ', '.join(f's{x["semilla"]:03d} (+{x["scroll"]-x["caja"]} px{"; B/P" if x["bosque"] else ""})' for x in des))
        s6 = [x for x in f if x['semilla'] == 6][0]
        print(f'   fuente usada en las celdas (s006): {s6.get("fuente_real")}; más anchas: ' + ', '.join(f's{x["semilla"]:03d} {x["min_content"]}' for x in sorted(f, key=lambda x: -x['min_content'])[:4]))


try:
    asyncio.run(main())
finally:
    nav.terminate()
