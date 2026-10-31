#!/usr/bin/env python3
"""comparar_r3_r8.py — 6b r8: (1) qué bloques del HTML cambian entre r3 y r8 (solo debe cambiar lo de la Parte 2); (2) cortes de «ecuación + kilos» por grupo (JSON idéntico a r3);
(3) píxeles de color intenso en las capturas."""
import re, json, difflib, collections, itertools
from bs4 import BeautifulSoup
SEEDS = [7,3,2,1,4,5,6]+list(range(8,25)); NB = " "
norm = lambda s: re.sub(r"\s+", " ", s.replace(NB, " ")).strip()
def blocks(path):
    soup = BeautifulSoup(open(path, encoding="utf-8").read(), "html.parser")
    for a in soup.find_all("annotation"): a.decompose()
    li = soup.select_one("body > ol > li"); kids = [c for c in li.children if getattr(c, "name", None)]
    out, p2next, insol = [], False, False
    for c in kids:
        t = norm(c.get_text(" "))
        if c.name == "h4" and t.lower() == "solution": insol = True
        if c.name == "ol" and c.get("type") == "a":
            for j, it in enumerate(c.find_all("li", recursive=False)): out.append((f"fb-{'abcdef'[j]}", norm(it.get_text(' ')), j == 1))
            continue
        p2 = p2next; p2next = False
        if c.name == "p" and re.match(r"Parte 2\.", t) and not insol: p2 = True; p2next = True
        if c.name == "p" and re.match(r"Parte 2 ·", t): p2 = True; p2next = True
        if c.name == "p" and re.match(r"Parte 2\.", t) and insol: p2 = True
        if re.search(r"Parte 2\. «", t): p2 = True
        out.append((c.name, t, p2))
    return out
chg_non, chg_p2, total_non = [], 0, 0
for idx, seed in enumerate(SEEDS, start=1):
    A, B = blocks(f"../6b_r3/html/v{idx}.html"), blocks(f"html/v{idx}.html")
    sm = difflib.SequenceMatcher(None, [(a[0], a[1]) for a in A], [(b[0], b[1]) for b in B], autojunk=False)
    for tag, i1, i2, j1, j2 in sm.get_opcodes():
        if tag == "equal": continue
        for blk in A[i1:i2] + B[j1:j2]:
            if blk[2]: chg_p2 += 1
            else: chg_non.append((idx, seed, blk[0], blk[1][:110]))
    total_non += sum(1 for b in B if not b[2])
print(f"bloques NO de la Parte 2 en r8 (suma de 24 versiones): {total_non}; bloques no-Parte-2 que cambian entre r3 y r8: {len(chg_non)}; bloques de la Parte 2 que cambian: {chg_p2}")
for x in chg_non[:25]: print("  CAMBIA fuera de Parte 2:", x)
# (2) ecuación + unidad por grupo
cx = json.load(open("cortes_ctx_r3.json", encoding="utf-8")); g = collections.defaultdict(lambda: [0, 0]); gu1 = collections.defaultdict(lambda: [0, 0])
for e in cx:
    for u in e["m"]["u2"]:
        w = u["w"].split("|"); k = (w[0], w[1][:30], w[2][:12] if w[2].startswith("fb-") else ""); g[k][0] += 1; g[k][1] += bool(u["split"])
    for u in e["m"]["u1"]:
        w = u["w"].split("|"); k = (w[0], w[1][:30], w[2][:12] if w[2].startswith("fb-") else ""); gu1[k][0] += 1; gu1[k][1] += bool(u["split"])
print("\nB) ecuación + kilos|cursos, por grupo (casos, cortes) [r3 == r8, JSON byte-idéntico]:")
for k, v in sorted(g.items(), key=lambda t: -t[1][0]): print("  ", k, tuple(v))
items = list(g.items()); reach = {(0, 0): ()}
for k, v in items:
    new = dict(reach)
    for (c, s), sel in reach.items(): new.setdefault((c + v[0], s + v[1]), sel + (k,))
    reach = new
print("subconjunto de grupos B con 594 casos y 129 cortes:", reach.get((594, 129)))
sol_all = [(k, v) for k, v in g.items() if k[0] == "S"]
print("B en toda la Solution:", sum(v[0] for _, v in sol_all), "casos,", sum(v[1] for _, v in sol_all), "cortes")
p36 = [(k, v) for k, v in g.items() if k[0] == "S" and (k[2] in ("fb-c", "fb-d", "fb-e", "fb-f") or k[1].startswith(("Procedimiento", "Caso", "Parte 6")))]
print("B en Partes 3 a 6 de la Solution (fb-c..f + Procedimiento + Caso + Parte 6 falsas):", sum(v[0] for _, v in p36), "casos,", sum(v[1] for _, v in p36), "cortes")
# (3) píxeles de color intenso
rows = []
for l in open("pixeles_r8.txt", encoding="utf-8"):
    m = re.match(r"\s+s(\d+) (\w+) y=(\d+)-(\d+) x=(\d+)-(\d+) rgb≈\((\d+), (\d+), (\d+)\) n=(\d+)", l)
    if m:
        s, mode, y0, y1, x0, x1, r, gg, b, n = m.groups(); r, gg, b = int(r), int(gg), int(b)
        if (max(r, gg, b) - min(r, gg, b)) > 40 and not (mode.startswith("box") and y0 == "13"): rows.append((s, mode, y0, y1, (r, gg, b), n))
print("\ngrupos de píxeles con media muy cromática (fuera del contorno de la caja):", len(rows), rows[:8])
