#!/usr/bin/env python3
"""comparar_c1d_c1e.py — tanda C1e: la salida del verificador c1e contra la de la C1d, línea a línea.

Se apartan solo cuatro líneas: la cabecera con el md5, la línea «OK: MC56» (su descripción dice ahora «32243d0a»), la del resultado
y la de cierre (exit, segundos). Todo lo demás —las 368 cifras firmadas, Crit-P6 y los mutantes MC50 a MC55 y MC57— tiene que salir
idéntico byte a byte. Además compara, una a una, las 368 líneas «ROJO, cubierto por». Uso:
python3 -I comparar_c1d_c1e.py <salida c1d> <salida c1e>
"""
import difflib
import re
import sys

PROPIAS = re.compile(r"^Versión verificada|^=== RESULTADO|^exit=|^\s*OK: MC56 ")


def lineas(p):
    with open(p, encoding="utf-8") as f:
        return [l.rstrip() for l in f]


def filtra(ls):
    return [l for l in ls if not PROPIAS.search(l)]


a, b = lineas(sys.argv[1]), lineas(sys.argv[2])
fa, fb = filtra(a), filtra(b)
print(f"c1d: {len(a)} líneas ({len(fa)} comparables) · c1e: {len(b)} líneas ({len(fb)} comparables)")
apartadas = [l for l in b if PROPIAS.search(l)]
print(f"apartadas en c1e: {len(apartadas)}")
for l in apartadas:
    print("   ", l[:250])
rojo = re.compile(r"ROJO, cubierto por")
ra, rb = [l for l in a if rojo.search(l)], [l for l in b if rojo.search(l)]
print(f"líneas «ROJO, cubierto por»: c1d {len(ra)} · c1e {len(rb)} · idénticas una a una: {ra == rb}")
d = [x for x in difflib.unified_diff(fa, fb, "c1d", "c1e", n=0, lineterm="") if not x.startswith(("---", "+++", "@@"))]
print(f"diferencias fuera de lo apartado: {len(d)}")
for x in d[:40]:
    print("   ", x[:250])
for l in b:
    if l.startswith("=== RESULTADO") or l.startswith("exit="):
        print(l[:400])
