#!/usr/bin/env python3
"""contar_cortes_frase_c1e.py — tanda C1e: cortes de renglón en el criterio general de la frase nueva, que contar_h2.py no cuenta.
Sobre el JSON de cap7_r5.py. Mismos criterios que contar_h2.py (renglón que acaba en X y siguiente que empieza por Y, U+00A0 como espacio).
  G  «habría ⏎ ganado con»   H  «habría ⏎ perdido con»   (las dos solo existen en la frase nueva)
  I  «con un ⏎ curso más»    J  «de cada ⏎ uno»          K  «mayor, ⏎ sí;»     L  «menor, ⏎ no.»
Control de la lectura: la frase literal aparece en la unión de los renglones. Uso: python3 -I contar_cortes_frase_c1e.py <salida.json>"""
import json
import sys

NB = chr(0xA0)
FRASE = "Por eso, para saber si el grado que perdió habría ganado con un curso más, o si el que ganó habría perdido con uno menos, basta comparar el promedio de cada uno con la diferencia entre los totales: si el promedio es mayor, sí; si es menor, no."
CORTES = {"G": ("habría", "ganado con"), "H": ("habría", "perdido con"), "I": ("con un", "curso más, o si"),
          "J": ("de cada", "uno con"), "K": ("mayor,", "sí;"), "L": ("menor,", "no.")}


def corte(lin, x, y):
    return any(a.endswith(" " + x) or a == x for a, b in zip(lin, lin[1:]) if b.startswith(y))


d = json.load(open(sys.argv[1], encoding="utf-8"))
tab = {}
for r in d:
    p6 = r["m"].get("p6") or {}
    lin = [s.replace(NB, " ").strip() for s in p6.get("lines") or []]
    tx = " ".join(lin)
    t = tab.setdefault((r["v"], r["vw"]), {k: 0 for k in CORTES} | {"paginas": 0, "con_frase": 0})
    t["paginas"] += 1
    t["con_frase"] += FRASE in tx
    if FRASE in tx:
        for k, (x, y) in CORTES.items():
            t[k] += corte(lin, x, y)
print("versión ancho | páginas | con la frase literal | " + " | ".join(f"{k} {x}/{y.split()[0]}" for k, (x, y) in CORTES.items()))
for (v, w), t in sorted(tab.items(), key=lambda z: (z[0][0], -z[0][1])):
    print(f"{v} {w} | {t['paginas']} | {t['con_frase']} | " + " | ".join(str(t[k]) for k in CORTES))
