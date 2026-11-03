#!/usr/bin/env python3
"""Tanda S2b (2026-11-03): saca de la salida de verificar_espacio.R las tablas de cifras para la firma (§P7-E, regla por
regla; aclaración iii: las cifras salen del código, no de un modelo transcrito). Lee la tabla de H1 (print de TAB_H1) y las
líneas del barrido, y escribe tablas Markdown con coma decimal: acierto (exceso sobre el 25 % del azar) por estrato, y el
peso de la canónica en la mezcla (0,10 × exceso). Uso: python3 -I tabla_firma_s2b.py <salida.txt> <tablas.md>
"""
import re, sys

lin = open(sys.argv[1], encoding="utf-8").read().split("\n")
EST = ["mezcla", "CURSOS", "PROMEDIO", "canonica"]


def coma(x, signo=False):
    s = f"{x:+.1f}" if signo else f"{x:.1f}"
    return s.replace(".", ",")


def celda(t):
    return f"{coma(t)} % ({coma(t - 25, True)})"


def marca(t):
    return "**" if t - 25 > 8 + 1e-9 else ""


# Tabla de H1: el print de R la parte en cuatro bloques (una columna por bloque) cuando no cabe en 80 caracteres.
i = next(k for k, t in enumerate(lin) if t.startswith("  S2b · H1 de la FASE 2C r6")) + 1
vals, nombres = {}, []
for es in EST:
    assert lin[i].split() == [es], lin[i]
    i += 1
    for _ in range(7):
        nombre, v = lin[i].rsplit(None, 1)
        vals.setdefault(nombre.strip(), []).append(float(v))
        if es == "mezcla":
            nombres.append(nombre.strip())
        i += 1
h1 = [(n, vals[n]) for n in nombres]
assert len(h1) == 7 and all(len(z) == 4 for _, z in h1), h1
# Barrido: «    <op> (i) a b c d · (ii) a b c d · aplica a b c d».
pat = re.compile(r"^    (.+?)\s+\(i\)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+) · \(ii\)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+) · aplica\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)$")
bar = []
for s in lin:
    m = pat.match(s)
    if m:
        v = [float(m.group(k)) for k in range(2, 14)]
        bar.append((m.group(1).strip(), v[0:4], v[4:8], v[8:12]))
assert len(bar) == 28, len(bar)

out = []
cab = "| ID | Mezcla de E1 | Rama CURSOS | Rama PROMEDIO | Canónica (exacta, 24 órdenes) | Peso de la canónica en la mezcla |"
sep = "|---|---|---|---|---|---|"


def fila(ide, z):
    return ("| " + ide + " | " + " | ".join(f"{marca(t)}{celda(t)}{marca(t)}" for t in z) +
            f" | {coma2(0.1 * (z[3] - 25))} pp |")


def coma2(x):
    return f"{x:+.2f}".replace(".", ",")


out += ["Reglas de H1, todas; en negrita, las cifras que pasan de +8 pp sobre el 25 % del azar:", "", cab, sep]
out += [fila(f"RS{k}", z) for k, (n, z) in enumerate(h1, 1)]
out += [""] + [f"- RS{k}: `{n}`" for k, (n, z) in enumerate(h1, 1)]
out += ["", "Barrido del estímulo: las reglas con alguna cifra por encima de +8 pp (en negrita). «Aplica»: en qué fracción de las",
        "versiones alguna opción cita el resultado (mezcla / CURSOS / PROMEDIO / canónica).", "",
        "| ID | Mezcla de E1 | Rama CURSOS | Rama PROMEDIO | Canónica (exacta, 24 órdenes) | Peso de la canónica en la mezcla | Aplica |",
        "|---|---|---|---|---|---|---|"]
n_pend, leyenda, k = 0, [], 0
for nombre, zi, zii, ap in bar:
    for lec, z in (("(i)", zi), ("(ii)", zii)):
        if nombre == "|a1 - a2|" and lec == "(i)":
            continue                                   # es «resta» (RS1)
        c = sum(1 for t in z if t - 25 > 8 + 1e-9)
        if c:
            n_pend += c; k += 1
            out.append(fila(f"BE{k}", z) + " " + " / ".join(f"{coma(a)}" for a in ap) + " % |")
            leyenda.append(f"- BE{k}: `{nombre} {lec}`")
out += [""] + leyenda + ["", f"Cifras del barrido por encima de +8 pp: {n_pend}, en {k} reglas."]
open(sys.argv[2], "w", encoding="utf-8").write("\n".join(out) + "\n")
print("\n".join(out))
