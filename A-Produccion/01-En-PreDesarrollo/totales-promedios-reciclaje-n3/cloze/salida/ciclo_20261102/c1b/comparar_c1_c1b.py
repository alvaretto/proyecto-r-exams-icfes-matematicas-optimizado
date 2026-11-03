#!/usr/bin/env python3
"""comparar_c1_c1b.py — tanda C1b: la salida del verificador c1b contra la de la C1, línea a línea.

Se apartan solo las líneas que la C1b añade o cambia a propósito: las dos de la sonda H1-Sol, las de MC50 a MC54, la de
MC39 (H2 de la r4), las filas de la tabla de mutantes que nombran esos mutantes, los md5 de la cabecera, el recuento
del resultado y la línea de cierre (exit, segundos). Todo lo demás —entre ello las 368 cifras firmadas— tiene que salir
idéntico. Uso: python3 -I comparar_c1_c1b.py <salida c1> <salida c1b>
"""
import re
import sys

PROPIAS = re.compile(r"C1b · H1-Sol|^\s*OK: MC5[0-4] |^\s*FALLA: MC5[0-4]|MC52 \(|MC39 \(L-C7|^MC5[0-4]\s|^MC39\s|"
                     r"^Versión verificada|^=== RESULTADO|^exit=|^\s+id\s+veredicto")


def lineas(p):
    with open(p, encoding="utf-8") as f:
        return [l.rstrip() for l in f]


def filtra(ls):
    return [l for l in ls if not PROPIAS.search(l)]


a, b = lineas(sys.argv[1]), lineas(sys.argv[2])
fa, fb = filtra(a), filtra(b)
print(f"c1: {len(a)} líneas ({len(fa)} comparables) · c1b: {len(b)} líneas ({len(fb)} comparables)")
apartadas = [l for l in b if PROPIAS.search(l)]
print(f"apartadas en c1b: {len(apartadas)}")
for l in apartadas:
    print("   ", l[:230])
import difflib
d = [x for x in difflib.unified_diff(fa, fb, "c1", "c1b", n=0, lineterm="") if not x.startswith(("---", "+++", "@@"))]
print(f"diferencias fuera de lo apartado: {len(d)}")
for x in d[:40]:
    print("   ", x[:230])
for l in b:
    if l.startswith("=== RESULTADO") or l.startswith("exit="):
        print(l[:400])
