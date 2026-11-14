"""Mide un cuadernillo PDF con poppler (pdfinfo, pdftotext -bbox, pdfimages, pdffonts, pdftohtml).
Uso: python3 -I medir_pdf.py examen.pdf [--log archivo.log] [--columnas 1|2] [--marcas auto|dests|texto]
                                        [--inicio Escenario:] [--fin "d)"] [--min-pt 7]
Preguntas y partidas:
  - dests (plantillas del motor): anclas qini.N / qfin.N que lee `pdfinfo -dests`, con el formato
    «<pág> [ XYZ <x> <y> null ] "qini.N"». Columna 0 si x < ancho/2. Partida si (pág, col) de qini.N ≠ qfin.N.
  - texto (plantilla oficial vieja): una pregunta empieza en la palabra --inicio y termina en la última --fin antes de la
    siguiente; está «partida» si su inicio y su fin caen en distinta (página, columna).
  - auto (por defecto): dests si el PDF trae anclas qini.*; si no, texto.
Tamaño del texto: `rachas_bajo_min` lista cada racha de texto por debajo de --min-pt con su longitud (C4 distingue WARN de FAIL).
Fuentes Type 3 (`fuentes_type3`, `texto_type3`): poppler ESTIMA su tamaño (HtmlOutputDev::updateFont: «This is a hack»:
multiplica por el ancho de la «m» entre 0,6 y por la matriz de la fuente). MEDIDO el 2026-11-14: un signo «×» de 10,95 pt
en una fuente de mapa de bits (tctt1095) sale como 5 pt. Ese tamaño no sirve ni para aprobar ni para reprobar: las rachas
de una fuente Type 3 llevan `type3: true` y el verificador las declara NO VERIFICABLES.
Numeración (`numeros`): los N de las anclas qini.N, en orden. La plantilla escribe el ancla con el mismo contador que
imprime el número de la pregunta, así que deben ser exactamente 1..Q (C13)."""
import re, json, argparse, subprocess, html, tempfile, os, collections

ap = argparse.ArgumentParser()
ap.add_argument("pdf"); ap.add_argument("--log"); ap.add_argument("--columnas", type=int, default=1)
ap.add_argument("--min-pt", dest="min_pt", type=float, default=7)
ap.add_argument("--marcas", choices=["auto", "dests", "texto"], default="auto")
ap.add_argument("--inicio", default="Escenario:"); ap.add_argument("--fin", default="d)")
a = ap.parse_args()


def correr(*cmd):
    return subprocess.run(list(cmd), capture_output=True, text=True, errors="replace").stdout


bb = correr("pdftotext", "-bbox", a.pdf, "-")
paginas = re.findall(r'<page width="([\d.]+)" height="([\d.]+)">(.*?)</page>', bb, re.S)
ancho = {n: float(w) for n, (w, h, _) in enumerate(paginas, 1)}


def col_de(pg, x):
    return 0 if a.columnas == 1 or x < ancho.get(pg, 612) / 2 else 1


def preguntas_dests(texto_dests):
    ini, fin = {}, {}
    for pg, x, y, tipo, num in re.findall(r'^\s*(\d+)\s+\[\s*XYZ\s+([\d.]+)\s+([\d.]+)\s+null\s*\]\s+"q(ini|fin)\.(\d+)"',
                                          texto_dests, re.M):
        pg, num = int(pg), int(num)
        (ini if tipo == "ini" else fin)[num] = (pg, col_de(pg, float(x)))
    preg = [{"ini": ini[k], "fin": fin.get(k)} for k in sorted(ini)]
    return preg, sorted(ini)


def preguntas_texto():
    marcas = []
    for n, (w, h, cuerpo) in enumerate(paginas, 1):
        for x0, y0, txt in re.findall(r'<word xMin="([\d.]+)" yMin="([\d.]+)"[^>]*>(.*?)</word>', cuerpo):
            txt = html.unescape(txt)
            if txt == a.inicio: marcas.append((n, col_de(n, float(x0)), float(y0), "I"))
            elif txt == a.fin: marcas.append((n, col_de(n, float(x0)), float(y0), "F"))
    marcas.sort()
    preg, act = [], None
    for pg, col, y, t in marcas:
        if t == "I":
            if act: preg.append(act)
            act = {"ini": (pg, col), "fin": None}
        elif act is not None:
            act["fin"] = (pg, col)
    if act: preg.append(act)
    return preg


modo = a.marcas
dests = correr("pdfinfo", "-dests", a.pdf) if modo in ("auto", "dests") else ""
if modo == "auto":
    modo = "dests" if re.search(r'"qini\.\d+"', dests) else "texto"
preg, numeros = preguntas_dests(dests) if modo == "dests" else (preguntas_texto(), None)
partidas = [i + 1 for i, q in enumerate(preg) if q["fin"] and q["fin"] != q["ini"]]
sin_fin = [i + 1 for i, q in enumerate(preg) if not q["fin"]]
por_pag = {}
for q in preg: por_pag[q["ini"][0]] = por_pag.get(q["ini"][0], 0) + 1

# imágenes raster y su resolución efectiva (pdfimages -list: columnas x-ppi, y-ppi)
img = correr("pdfimages", "-list", a.pdf).splitlines()[2:]
filas = [l.split() for l in img if len(l.split()) > 13 and l.split()[2] == "image"]
ppi = [min(int(f[12]), int(f[13])) for f in filas]

# tamaño del texto vectorial (pdftohtml -xml a escala 1:1 da el cuerpo en pt, redondeado)
with tempfile.TemporaryDirectory() as td:
    subprocess.run(["pdftohtml", "-xml", "-i", "-q", "-zoom", "1", a.pdf, os.path.join(td, "t")], capture_output=True)
    xml = open(os.path.join(td, "t.xml"), encoding="utf-8", errors="replace").read()
ff = correr("pdffonts", a.pdf).splitlines()[2:]
no_emb = [l.split()[0] for l in ff if re.search(r"\s(no)\s+(yes|no)\s+(yes|no)\s+\d+\s+\d+\s*$", l)]
type3 = sorted({l.split()[0] for l in ff if re.search(r"\sType 3\s", l)})
sp = {m.group(1): float(m.group(2)) for m in re.finditer(r'<fontspec id="(\d+)" size="([\d.]+)"', xml)}
fam = {m.group(1): m.group(2) for m in re.finditer(r'<fontspec id="(\d+)"[^>]*family="([^"]*)"', xml)}
tam = collections.Counter(); rachas = []; texto_type3 = 0
for m in re.finditer(r'<text [^>]*font="(\d+)"[^>]*>(.*?)</text>', xml):
    t = html.unescape(re.sub("<[^>]+>", "", m.group(2))).strip()
    es_t3 = fam.get(m.group(1)) in type3
    if es_t3: texto_type3 += max(1, len(t))       # un glifo sin Unicode (texto vacío) también cuenta
    if not t: continue
    if not es_t3: tam[sp[m.group(1)]] += len(t)   # el tamaño de una Type 3 es una estimación: no entra en el mínimo
    if sp[m.group(1)] < a.min_pt:
        rachas.append({"pt": sp[m.group(1)], "n": len(t.replace(" ", "")), "texto": t[:30], "type3": es_t3})
over = []
if a.log and os.path.exists(a.log):
    over = [float(x) for x in re.findall(r"Overfull \\hbox \(([\d.]+)pt too wide\)", open(a.log, errors="replace").read())]
papeles = sorted({f"{float(w)/72*25.4:.1f} x {float(h)/72*25.4:.1f}" for w, h, _ in paginas})
w, h = (float(paginas[0][0]), float(paginas[0][1])) if paginas else (0, 0)
print(json.dumps({"papel_mm": f"{w/72*25.4:.0f} x {h/72*25.4:.0f}", "papeles_mm": papeles,
  "ancho_mm": round(w/72*25.4, 2), "alto_mm": round(h/72*25.4, 2), "paginas": len(paginas), "marcas": modo,
  "preguntas": len(preg), "numeros": numeros, "inicios_por_pagina": por_pag, "partidas": partidas, "sin_fin": sin_fin,
  "imagenes_raster": len(filas), "raster_ppi": ppi,
  "texto_min_pt": min(tam) if tam else None, "texto_bajo_min": [f"{r['pt']:g}pt:{r['texto']}" for r in rachas][:12],
  "rachas_bajo_min": rachas, "fuentes_no_incrustadas": no_emb, "fuentes_total": len(ff),
  "fuentes_type3": type3, "texto_type3": texto_type3,
  "overfull_n": len(over), "overfull_max_pt": max(over) if over else 0}, ensure_ascii=False))
