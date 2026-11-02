#!/usr/bin/env python3
"""Compara verificar_espacio_cloze_r9.txt con verificar_espacio_cloze_va.txt (tanda 9a, cierre).

Solo deben cambiar MC39 (líneas nuevas) y los rótulos «muestral sobre la Parte 1» de L-C2.
Deshace los rótulos nuevos en la r9, aparta las líneas de MC39 y la de tiempo, y muestra el diff
de lo que queda. Uso, desde cloze/salida:  python3 -I comparar_r9_va.py
"""
import difflib
import re
import sys

VA, R9 = "verificar_espacio_cloze_va.txt", "verificar_espacio_cloze_r9.txt"

# Rótulo nuevo (r9) -> rótulo viejo (va). Son los tres cambios de cat()/sprintf() del diff de E3.
DESHACER = [
    (" y muestrales sobre la Parte 1 (las mismas N = 100 por estrato):", ":"),
    ("exacta sobre el sorteo de la Parte 2 y muestral sobre la Parte 1 (N = 100; decisión «Va» del profesor, 2026-10-31)",
     "exacta sobre el sorteo de la Parte 2 (decisión «Va» del profesor, 2026-10-31)"),
    ("X6 exacta sobre la Parte 2 (muestral sobre la Parte 1), ", "X6 exacta, "),
]
TIEMPO = re.compile(r"^exit=\d+ · \d+ s$")


def leer(ruta):
    with open(ruta, encoding="utf-8") as f:
        return f.read().splitlines()


va, r9 = leer(VA), leer(R9)
n_rotulos = 0
r9_norm, mc39 = [], []
for linea in r9:
    if "MC39" in linea:
        mc39.append(linea)
        continue
    for nuevo, viejo in DESHACER:
        if nuevo in linea:
            linea = linea.replace(nuevo, viejo)
            n_rotulos += 1
    r9_norm.append(linea)

# print() de la tabla TM rellena las columnas al ancho de su fila más larga; la de MC39 la ensancha.
# Por eso se comparan sin los espacios finales.
va_cmp = [l.rstrip() for l in va if not TIEMPO.match(l)]
r9_cmp = [l.rstrip() for l in r9_norm if not TIEMPO.match(l)]

print(f"Líneas: va {len(va)}, r9 {len(r9)}; rótulos «muestral» deshechos en la r9: {n_rotulos}; líneas con MC39: {len(mc39)}")
print("--- líneas de MC39 en la r9:")
for l in mc39:
    print("   ", l)
print("--- tiempo: va", [l for l in va if TIEMPO.match(l)], "· r9", [l for l in r9 if TIEMPO.match(l)])
d = list(difflib.unified_diff(va_cmp, r9_cmp, "va", "r9 (normalizada, sin MC39)", n=0, lineterm=""))
print(f"--- diff restante: {sum(1 for l in d if l[:1] in '+-' and l[:3] not in ('+++', '---'))} líneas")
for l in d:
    print(l)
sys.exit(0)
