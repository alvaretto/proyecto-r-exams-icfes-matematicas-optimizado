"""Mide un cuadernillo PDF con poppler (pdftotext -bbox, pdfimages, pdffonts).
Uso: python3 -I medir_pdf.py examen.pdf [--log archivo.log] [--columnas 1|2] [--inicio Escenario:] [--fin "(d)"]
Una pregunta empieza en la palabra --inicio y termina en la última --fin antes de la siguiente;
está «partida» si su inicio y su fin caen en distinta (página, columna)."""
import re, json, argparse, subprocess, html
ap = argparse.ArgumentParser()
ap.add_argument("pdf"); ap.add_argument("--log"); ap.add_argument("--columnas", type=int, default=1)
ap.add_argument("--min-pt", dest="min_pt", type=float, default=7)
ap.add_argument("--inicio", default="Escenario:"); ap.add_argument("--fin", default="d)")
a = ap.parse_args()
bb = subprocess.run(["pdftotext", "-bbox", a.pdf, "-"], capture_output=True, text=True).stdout
paginas = re.findall(r'<page width="([\d.]+)" height="([\d.]+)">(.*?)</page>', bb, re.S)
marcas = []
for n, (w, h, cuerpo) in enumerate(paginas, 1):
    mitad = float(w) / 2
    for x0, y0, txt in re.findall(r'<word xMin="([\d.]+)" yMin="([\d.]+)"[^>]*>(.*?)</word>', cuerpo):
        txt = html.unescape(txt); col = 0 if a.columnas == 1 or float(x0) < mitad else 1
        if txt == a.inicio: marcas.append((n, col, float(y0), "I"))
        elif txt == a.fin: marcas.append((n, col, float(y0), "F"))
marcas.sort()
preg = []; act = None
for pg, col, y, t in marcas:
    if t == "I":
        if act: preg.append(act)
        act = {"ini": (pg, col), "fin": None}
    elif act is not None: act["fin"] = (pg, col)
if act: preg.append(act)
partidas = [i + 1 for i, q in enumerate(preg) if q["fin"] and q["fin"] != q["ini"]]
sin_fin = [i + 1 for i, q in enumerate(preg) if not q["fin"]]
por_pag = {}
for q in preg: por_pag[q["ini"][0]] = por_pag.get(q["ini"][0], 0) + 1
img = subprocess.run(["pdfimages", "-list", a.pdf], capture_output=True, text=True).stdout.splitlines()[2:]
raster = sum(1 for l in img if l.split()[2] == "image")
ppi = [int(l.split()[12]) for l in img if l.split()[2] == "image"]
# tamaño del texto vectorial (pdftohtml -xml a escala 1:1 da el cuerpo en pt, redondeado)
import tempfile, os, collections
with tempfile.TemporaryDirectory() as td:
    subprocess.run(["pdftohtml", "-xml", "-i", "-q", "-zoom", "1", a.pdf, os.path.join(td, "t")], capture_output=True)
    xml = open(os.path.join(td, "t.xml"), encoding="utf-8", errors="replace").read()
sp = {m.group(1): float(m.group(2)) for m in re.finditer(r'<fontspec id="(\d+)" size="([\d.]+)"', xml)}
tam = collections.Counter(); chico = []
for m in re.finditer(r'<text [^>]*font="(\d+)"[^>]*>(.*?)</text>', xml):
    t = html.unescape(re.sub("<[^>]+>", "", m.group(2))).strip()
    if not t: continue
    tam[sp[m.group(1)]] += len(t)
    if sp[m.group(1)] < a.min_pt: chico.append(f"{sp[m.group(1)]:g}pt:{t[:30]}")
ff = subprocess.run(["pdffonts", a.pdf], capture_output=True, text=True).stdout.splitlines()[2:]
no_emb = [l.split()[0] for l in ff if re.search(r"\s(no)\s+(yes|no)\s+(yes|no)\s+\d+\s+\d+\s*$", l)]
over = []
if a.log:
    over = [float(x) for x in re.findall(r"Overfull \\hbox \(([\d.]+)pt too wide\)", open(a.log, errors="replace").read())]
w, h = float(paginas[0][0]), float(paginas[0][1])
print(json.dumps({"papel_mm": f"{w/72*25.4:.0f} x {h/72*25.4:.0f}", "paginas": len(paginas), "preguntas": len(preg),
  "inicios_por_pagina": por_pag, "partidas": partidas, "sin_fin": sin_fin, "imagenes_raster": raster, "raster_ppi": ppi,
  "texto_min_pt": min(tam) if tam else None, "texto_bajo_min": chico[:12],
  "fuentes_no_incrustadas": no_emb, "overfull_n": len(over), "overfull_max_pt": max(over) if over else 0}, ensure_ascii=False))
