"""Ancho real de la caja de la pregunta (.qtext) en el Moodle del colegio y desborde de las DOS tablas del CLOZE
hermano de totales-promedios-reciclaje-n3: la tabla de la Parte 1 (las sedes) y la del otro colegio (los grados).

Copia adaptada de ../../salida/r4b/medir_qtext_moodle.py, que solo lee question[@type='multichoice'] y mide una
tabla. Cambios: lee question[@type='cloze'], mide cada div overflow-x de la pregunta y sustituye los gaps.

Solo lectura en el servidor: inicia sesión con la cuenta de prueba y abre la revisión de un intento que ya existe
(review.php). Las versiones se ponen en .qtext en el DOM del navegador (Runtime.evaluate): nada se envía, no se
crean intentos y no se importan preguntas. Credenciales: ~/.config/aulaviva/prueba.json (no se imprimen).

Gaps. El questiontext de un cloze trae la sintaxis {peso:TIPO:...}, que Moodle cambia por controles al mostrar la
pregunta. Aquí se cambia antes de inyectar, imitando el marcado de qtype_multianswer:
  - MULTICHOICE_V y MULTIRESPONSE: un bloque <div class="answer"> con una fila por opción (botón o casilla y el
    texto de la opción, sin el '=' de la correcta ni la retroalimentación tras '#');
  - NUMERICAL y SHORTANSWER: un campo de texto de 7 caracteres (size="7").
Las tablas son bloques aparte, así que su ancho no depende de los gaps; los gaps sí cuentan para el desborde de la
página.

  Rscript --no-init-file ../generar_moodle_100_cloze.R <ruta del .Rmd> <dir>
  FONTCONFIG_FILE=<fonts.conf con Roboto> python3 medir_qtext_moodle_cloze.py --base https://aula.senodeteta.com \
      --attempt 4658 --xmldir <dir> --salida medicion.json --png s006.png

Sin FONTCONFIG_FILE, system-ui resuelve en Linux a la fuente por defecto (Noto Sans), no a Roboto.
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
ap.add_argument('--puerto', type=int, default=9361)
ap.add_argument('--png', default='')
ap.add_argument('--salida', required=True)
a = ap.parse_args()
base = a.base.rstrip('/')
cred = json.load(open(os.path.expanduser('~/.config/aulaviva/prueba.json')))[a.quien]

GAP = re.compile(r'\{(\d*):(MULTICHOICE_V|MULTICHOICE_H|MULTICHOICE|MULTIRESPONSE_H|MULTIRESPONSE|NUMERICAL|SHORTANSWER):'
                 r'((?:\\.|[^}\\])*)\}')


def partir(s, sep):
    out, cur, esc = [], '', False
    for ch in s:
        if esc:
            cur += ch
            esc = False
        elif ch == '\\':
            esc = True
        elif ch == sep:
            out.append(cur)
            cur = ''
        else:
            cur += ch
    out.append(cur)
    return out


def opcion(s):
    s = partir(s, '#')[0]
    s = re.sub(r'^%-?[0-9.]+%', '', s)
    return s.lstrip('=')


def control(m):
    tipo, cuerpo = m.group(2), m.group(3)
    if tipo in ('NUMERICAL', 'SHORTANSWER'):
        return '<span class="subquestion form-inline d-inline"><input type="text" size="7" class="form-control d-inline mb-1"></span>'
    caja = 'checkbox' if tipo.startswith('MULTIRESPONSE') else 'radio'
    filas = ''.join(f'<div class="r{i % 2}"><input type="{caja}" class="form-check-input"> <label>{opcion(o)}</label></div>'
                    for i, o in enumerate(partir(cuerpo, '~')))
    return f'<div class="answer">{filas}</div>'


versiones = []
for f in sorted(glob.glob(a.xmldir + '/s*.xml')):
    t = ET.parse(f).getroot().find(".//question[@type='cloze']/questiontext/text").text
    ngaps = len(GAP.findall(t))
    versiones.append({'semilla': int(re.search(r's(\d+)\.xml', f).group(1)), 'html': GAP.sub(control, t), 'gaps': ngaps,
                      'bosque': ('Bosque' in t or 'Pradera' in t)})
assert versiones and all(v['gaps'] == 6 for v in versiones), 'cada versión debe traer 6 gaps'
assert not any('{' in v['html'] and re.search(r'\{\d*:[A-Z]', v['html']) for v in versiones), 'quedó un gap sin sustituir'

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
ws_url = None
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
  var cajas = Array.from(q.querySelectorAll('div[style*="overflow-x"]'));
  var t = cajas.map(function(caja){
    var tb = caja.querySelector('table');
    var bw = caja.clientWidth, sw = caja.scrollWidth, tw = tb.getBoundingClientRect().width;
    var w0 = tb.style.width; tb.style.width = 'min-content';
    var mc = tb.getBoundingClientRect().width; tb.style.width = w0;
    var ths = Array.from(tb.querySelectorAll('th')).map(function(x){ return Math.round(x.getBoundingClientRect().height); });
    return {caja: bw, scroll: sw, tabla: Math.round(tw*10)/10, min_content: Math.round(mc*10)/10, desborda: sw > bw, alto_th: ths};
  });
  return JSON.stringify({tablas: t, ntablas: cajas.length,
    pagina: document.documentElement.scrollWidth > window.innerWidth, qtext_scroll: q.scrollWidth > q.clientWidth});
})"""


async def main():
    res = {'anchos': {}}
    async with websockets.connect(ws_url, max_size=100_000_000) as ws:
        n = [0]
        pend = {}

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
            t0 = time.time()
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
                m = json.loads(rr['result']['value'])
                m.update(semilla=v['semilla'], bosque=v['bosque'])
                if v['semilla'] == 6:
                    doc = await cmd('DOM.getDocument', {'depth': 0})
                    nd = await cmd('DOM.querySelector', {'nodeId': doc['root']['nodeId'],
                                                         'selector': '.que .qtext table tbody tr td:nth-child(2) span'})
                    pf = await cmd('CSS.getPlatformFontsForNode', {'nodeId': nd['nodeId']})
                    m['fuente_real'] = [f['familyName'] for f in pf.get('fonts', [])]
                    if a.png:
                        for it in (0, 1):
                            await cmd('Runtime.evaluate', {'expression':
                                f"document.querySelectorAll('.que .qtext div[style*=\"overflow-x\"]')[{it}].scrollIntoView()"})
                            await asyncio.sleep(0.3)
                            shot = await cmd('Page.captureScreenshot', {'format': 'png'})
                            open(a.png.replace('.png', f'_{W}_tabla{it + 1}.png'), 'wb').write(base64.b64decode(shot['data']))
                filas.append(m)
            res['anchos'][W] = {'caja_vacia': caja, 'versiones': filas}
        tarea.cancel()
    json.dump(res, open(a.salida, 'w'), ensure_ascii=False, indent=1)
    for W, d in res['anchos'].items():
        c = d['caja_vacia']
        f = d['versiones']
        print(f'== {W} px: .qtext {c.get("qtext_rect")} px (contenido {c.get("qtext_contenido")}, padding {c.get("padding")}), '
              f'#region-main {c.get("region_main")}, .formulation {c.get("formulation")}, '
              f'fuente {c.get("tam")} {str(c.get("fuente"))[:50]}…')
        assert all(x['ntablas'] == 2 for x in f), 'cada versión debe traer dos tablas'
        for it, nombre in ((0, 'tabla de la Parte 1'), (1, 'tabla del otro colegio')):
            cajas = sorted({x['tablas'][it]['caja'] for x in f})
            des = [x for x in f if x['tablas'][it]['desborda']]
            mc = sorted(x['tablas'][it]['min_content'] for x in f)
            fb = [x for x in f if x['bosque']]
            extra = (f'; El Bosque/La Pradera: {sum(x["tablas"][it]["desborda"] for x in fb)}/{len(fb)}, '
                     f'ancho mínimo máx {max(x["tablas"][it]["min_content"] for x in fb)}') if (it == 0 and fb) else ''
            print(f'   {nombre}: caja {cajas} px; fuera de su caja en {len(des)}/{len(f)}{extra}; '
                  f'ancho mínimo mín {mc[0]}, mediana {mc[len(mc) // 2]}, máx {mc[-1]}')
            if des:
                print('     desbordan: ' + ', '.join(f's{x["semilla"]:03d} (+{x["tablas"][it]["scroll"] - x["tablas"][it]["caja"]} px)' for x in des))
        print(f'   página desbordada en {sum(x["pagina"] for x in f)}/{len(f)}; .qtext con scroll en {sum(x["qtext_scroll"] for x in f)}/{len(f)}')
        s6 = [x for x in f if x['semilla'] == 6][0]
        print(f'   fuente usada en las celdas (s006): {s6.get("fuente_real")}')


try:
    asyncio.run(main())
finally:
    nav.terminate()
