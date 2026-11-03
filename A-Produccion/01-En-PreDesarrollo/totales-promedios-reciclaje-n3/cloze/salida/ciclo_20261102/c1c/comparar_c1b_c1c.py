#!/usr/bin/env python3
"""comparar_c1b_c1c.py — tanda C1c: la salida del verificador c1c contra la de la C1b, línea a línea.

Se apartan solo las líneas que la C1c añade o cambia a propósito: las dos de Crit-P6, las de MC50 a MC57, las filas de la
tabla de mutantes que nombran esos mutantes, los md5 de la cabecera, el recuento del resultado y la línea de cierre (exit,
segundos). MC52 es la guarda del data_generation y no cambia: no se aparta. Todo lo demás —entre ello las 368 cifras firmadas—
tiene que salir idéntico. Además compara, una a una, las 368 líneas «ROJO, cubierto por». Uso:
python3 -I comparar_c1b_c1c.py <salida c1b> <salida c1c>
"""
import difflib
import re
import sys

PROPIAS = re.compile(r"C1c · Crit-P6|^\s*OK: MC5[013-7] |^\s*FALLA: MC5[013-7]|^MC5[013-7]\s|"
                     r"^Versión verificada|^=== RESULTADO|^exit=")


def lineas(p):
    with open(p, encoding="utf-8") as f:
        return [l.rstrip() for l in f]


def filtra(ls):
    return [l for l in ls if not PROPIAS.search(l)]


a, b = lineas(sys.argv[1]), lineas(sys.argv[2])
fa, fb = filtra(a), filtra(b)
print(f"c1b: {len(a)} líneas ({len(fa)} comparables) · c1c: {len(b)} líneas ({len(fb)} comparables)")
apartadas = [l for l in b if PROPIAS.search(l)]
print(f"apartadas en c1c: {len(apartadas)}")
for l in apartadas:
    print("   ", l[:250])
rojo = re.compile(r"ROJO, cubierto por")
ra, rb = [l for l in a if rojo.search(l)], [l for l in b if rojo.search(l)]
print(f"líneas «ROJO, cubierto por»: c1b {len(ra)} · c1c {len(rb)} · idénticas una a una: {ra == rb}")
d = [x for x in difflib.unified_diff(fa, fb, "c1b", "c1c", n=0, lineterm="") if not x.startswith(("---", "+++", "@@"))]
print(f"diferencias fuera de lo apartado: {len(d)}")
for x in d[:40]:
    print("   ", x[:250])
for l in b:
    if l.startswith("=== RESULTADO") or l.startswith("exit="):
        print(l[:400])
