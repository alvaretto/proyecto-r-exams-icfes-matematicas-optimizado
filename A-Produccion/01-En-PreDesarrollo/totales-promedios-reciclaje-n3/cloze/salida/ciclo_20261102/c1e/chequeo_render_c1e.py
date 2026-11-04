#!/usr/bin/env python3
"""chequeo_render_c1e.py — tanda C1e: añade a render.log las comprobaciones de la validación realista. Uso: python3 -I chequeo_render_c1e.py <render>"""
import glob, re, sys
B = sys.argv[1]
leer = lambda p: open(p, encoding="utf-8", errors="replace").read()
tex = sorted(glob.glob(B + "/tex/*.tex"))
tt = [l for p in tex for l in leer(p).splitlines()]
usos = [l for l in tt if "pandocbounded" in l and "providecommand" not in l]
lt = sum(l.count("LTcaptype{none}") for l in tt)
guard = "@ifundefined{c@none}" in leer(sys.argv[2])
html = sorted(glob.glob(B + "/html/*.html"))
ans = sum(leer(p).count("##ANSWER") for p in html)
pdf = glob.glob(B + "/pdf/*.pdf"); docx = glob.glob(B + "/docx/*.docx")
x = leer(glob.glob(B + "/moodle/*.xml")[0])
gaps = re.findall(r"\{[0-9]+:(?:MULTICHOICE|MULTIRESPONSE)[^}]*", x)
img = [g for g in gaps if "<img" in g or "@@PLUGINFILE@@" in g or "&lt;img" in g]
print(f"tex: {len(tex)} · usos de pandocbounded fuera de providecommand: {len(usos)} · LTcaptype{{none}}: {lt} · guarda c@none en el .Rmd: {guard}")
print(f"##ANSWER en el HTML: {ans}")
print(f"PDF: {len(pdf)} · HTML: {len(html)} · DOCX: {len(docx)}")
print(f"Moodle: {len(gaps)} gaps de elección, con imagen: {len(img)}")
