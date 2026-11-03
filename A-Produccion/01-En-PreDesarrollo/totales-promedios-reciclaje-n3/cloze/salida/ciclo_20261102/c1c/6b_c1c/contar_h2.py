#!/usr/bin/env python3
"""contar_h2.py — tanda C1c: las cuatro frecuencias de H2 de la FASE 2C r5, contadas sobre los renglones del instrumento.

Entrada: el JSON de cap7_r5.py (lista de {v, seed, vw, m}), con m.p6.lines = renglones del párrafo «Parte 6.» en orden.
Un corte «X ⏎ Y» existe si un renglón termina en X y el siguiente empieza por Y, con U+00A0 convertido en espacio. Cada corte
se cuenta sobre las páginas cuyo párrafo contiene la frase (el denominador sale del texto, no del generador):
  A «habría ⏎ ganado.»               sobre las que dicen «habría ganado.»   (HP verdadera)
  B «habría ⏎ seguido ganando.»      sobre las que dicen «habría seguido ganando.»   (HG falsa)
  C «no le ⏎ habría alcanzado»       sobre las que dicen «no le habría alcanzado»   (HP falsa)
  D «habría ⏎ sumado»                sobre todas
  A o B: páginas con alguno de los dos.
Control de la lectura: que renglones unidos con un espacio reproduzcan el párrafo sin partes perdidas (cada frase aparece
tal cual en la unión). Uso: python3 -I contar_h2.py <salida.json>
"""
import json
import sys

NB = chr(0xA0)
CORTES = {"A": ("habría", "ganado.", "habría ganado."),
          "B": ("habría", "seguido ", "habría seguido ganando."),
          "C": ("no le", "habría alcanzado", "no le habría alcanzado"),
          "D": ("habría", "sumado", "habría sumado")}


def corte(lin, x, y):
    return any(a.endswith(" " + x) or a == x for a, b in zip(lin, lin[1:]) if b.startswith(y))


def main(ruta):
    d = json.load(open(ruta, encoding="utf-8"))
    tab = {}
    malos = 0
    for r in d:
        p6 = r["m"].get("p6") or {}
        if p6.get("missing") or not p6.get("lines"):
            malos += 1
            continue
        lin = [s.replace(NB, " ").strip() for s in p6["lines"]]
        tx = " ".join(lin)
        t = tab.setdefault((r["v"], r["vw"]), {k: [0, 0] for k in CORTES} | {"AB": [0, 0], "paginas": [0, 0]})
        t["paginas"][0] += 1
        ab = False
        for k, (x, y, frase) in CORTES.items():
            if frase in tx:
                t[k][1] += 1
                if corte(lin, x, y):
                    t[k][0] += 1
                    ab = ab or k in "AB"
        t["AB"][0] += ab
        t["AB"][1] += 1
    print(f"páginas sin párrafo: {malos}")
    print("versión ancho | habría/ganado | habría/seguido ganando | A o B (páginas) | no le/habría alcanzado | habría/sumado | páginas")
    for (v, w), t in sorted(tab.items(), key=lambda z: (z[0][0], -z[0][1])):
        f = lambda k: f"{t[k][0]}/{t[k][1]}"
        print(f"{v} {w} | {f('A')} | {f('B')} | {f('AB')} | {f('C')} | {f('D')} | {t['paginas'][0]}")
    return tab


if __name__ == "__main__":
    main(sys.argv[1])
