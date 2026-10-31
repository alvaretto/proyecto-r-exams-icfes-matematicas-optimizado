#!/usr/bin/env python3
"""pixeles_r3.py — escaneo por píxeles de shots/*.png: (1) píxeles coloreados (rojo de error, etc.) fuera del contorno de la emulación de caja; (2) tinta en las 3 columnas del borde derecho/izquierdo (contenido cortado por el borde); (3) mayor hueco en blanco vertical."""
import glob, re, os, sys
import numpy as np
from PIL import Image
SEEDS = [7,3,2,1,4,5,6]+list(range(8,25))
rows = []; colored_report = []
for f in sorted(glob.glob("shots/s*_*.png")):
    mo = re.match(r"shots/s(\d+)_(plain|box)(\d+)\.png$", f)
    if not mo: print("archivo inesperado:", f); continue
    seed, mode, vw = int(mo.group(1)), mo.group(2), int(mo.group(3))
    a = np.asarray(Image.open(f).convert("RGB")).astype(int); H, W, _ = a.shape
    mx, mn = a.max(axis=2), a.min(axis=2); chroma = mx - mn; gray = a.mean(axis=2)
    colmask = chroma > 70
    if mode == "box":    # el contorno rojo discontinuo (#d33) de la caja está en x = 25 y x = vw-25: se excluye
        colmask[:, max(0, 22):28] = False; colmask[:, max(0, vw-28):vw-22] = False
    ys = np.where(colmask.any(axis=1))[0]
    clusters = []
    if len(ys):
        start = prev = ys[0]
        for y in ys[1:]:
            if y - prev > 25: clusters.append((start, prev)); start = y
            prev = y
        clusters.append((start, prev))
    for (y0, y1) in clusters[:6]:
        sub = a[y0:y1+1][colmask[y0:y1+1]]; xs = np.where(colmask[y0:y1+1].any(axis=0))[0]
        colored_report.append((seed, mode, vw, int(y0), int(y1), int(xs.min()), int(xs.max()), tuple(int(v) for v in sub.mean(axis=0)), int(colmask[y0:y1+1].sum())))
    dark = gray < 140
    edge_r = int(dark[:, W-3:].sum()); edge_l = int(dark[:, :3].sum())
    white = (gray > 250).all(axis=1); run = best = 0; best_at = 0
    for y, w in enumerate(white):
        run = run + 1 if w else 0
        if run > best: best, best_at = run, y - run + 1
    rows.append((seed, mode, vw, W, H, int(colmask.sum()), len(clusters), edge_l, edge_r, best, best_at))
print(f"imágenes: {len(rows)}; anchos distintos por modo:", sorted({(r[1], r[2], r[3]) for r in rows}))
print("ancho de imagen != viewport:", [r[:4] for r in rows if r[3] != r[2]])
print("tinta en las 3 columnas del borde derecho o izquierdo (px oscuros):", [(r[0], r[1], r[2], r[7], r[8]) for r in rows if r[7] or r[8]][:20])
print("mayor hueco en blanco vertical (px) por modo (máx, semilla, y):")
for md in ("plain320", "plain360", "plain1024", "box320", "box360"):
    sel = [r for r in rows if f"{r[1]}{r[2]}" == md]; b = max(sel, key=lambda r: r[9]); print(f"  {md}: {b[9]} px, s{b[0]:02d}, y={b[10]}")
print(f"PÍXELES COLOREADOS (croma>70) fuera del contorno: {len(colored_report)} grupos")
for c in colored_report[:40]: print("  s%02d %s%d y=%d-%d x=%d-%d rgb≈%s n=%d" % c)
