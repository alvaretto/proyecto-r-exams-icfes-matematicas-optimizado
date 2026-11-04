#!/usr/bin/env python3
"""editar_verificador_c1e.py — tanda C1e del ciclo 2026-11-02 del CLOZE (2026-11-03).

Sobre verificar_espacio_cloze.R (md5 de partida 5df824570b49c2aff49412b4807cb917, comprobado). Solo los literales que dependen de la
frase del criterio general; ninguna sonda, ningún umbral ni ninguna firma cambia:
  1. Cabecera: la tanda C1e.
  2. P6_CRITERIO: el literal nuevo (Crit-P6).
  3. P6_NUEVO (el literal de MC51): leído de los archivos, no transcrito.
  4. MC56: vuelve a la frase de 32243d0a (de, a y P6_CRIT_VIEJO), y sigue sin mover H1-Sol.
  5. MC57: su ancla. La cifra se sigue metiendo en el tramo general, con el literal intacto.
Cada patrón se exige exactamente una vez.
Uso: python3 -I editar_verificador_c1e.py <verificador> <copia 32243d0a> <.Rmd nuevo>
"""
import hashlib
import sys

ver_p, viejo_p, nuevo_p = sys.argv[1:4]
leer = lambda p: open(p, encoding="utf-8").read()
txt = leer(ver_p)
md5 = hashlib.md5(txt.encode("utf-8")).hexdigest()
assert md5 == "5df824570b49c2aff49412b4807cb917", f"md5 de partida inesperado: {md5}"
assert hashlib.md5(leer(viejo_p).encode("utf-8")).hexdigest() == "32243d0a6e6c499a4c6b1213a49e95cd"
assert hashlib.md5(leer(nuevo_p).encode("utf-8")).hexdigest() == "e16842c1157b3bffe35b8f21682ed6ad"


def bloque(t):
    assert t.count('cat("Parte 6. Cada hipótesis') == 1
    i = t.index('cat("Parte 6. Cada hipótesis')
    j = t.index('sep = "")', i) + len('sep = "")')
    b = t[i:j]
    assert ")---" not in b
    return b


P6_ANTES = bloque(leer(viejo_p))
P6_AHORA = bloque(leer(nuevo_p))
assert "Por eso, si el grado que perdió tiene" in P6_ANTES and "Por eso, para saber si el grado" in P6_AHORA


def cambia(t, de, a):
    n = t.count(de)
    assert n == 1, f"el patrón aparece {n} veces (se exige 1): {de[:80]!r}"
    return t.replace(de, a, 1)


CRIT_EF2C = "Por eso, con un curso más o uno menos, basta comparar ese promedio con la diferencia entre los totales: si el promedio es mayor, el ganador cambia; si es menor, no."
CRIT_ANTES = "Por eso, si el grado que perdió tiene un curso más o el que ganó tiene uno menos, basta comparar el promedio de ese grado con la diferencia entre los totales: si el promedio es mayor, el ganador cambia; si es menor, no."
CRIT_AHORA = "Por eso, para saber si el grado que perdió habría ganado con un curso más, o si el que ganó habría perdido con uno menos, basta comparar el promedio de cada uno con la diferencia entre los totales: si el promedio es mayor, sí; si es menor, no."

# 1. Cabecera
CAB = "##       el ancla de MC57. Ninguna sonda, ningún umbral ni ninguna firma.\n"
txt = cambia(txt, CAB, CAB + r'''##       Tanda C1e (2026-11-03): el criterio general cambia otra vez por decisión del profesor («Reformular (Recomendada)»).
##       Solo cambian los literales que dependen de la frase: P6_CRITERIO, P6_NUEVO (MC51), MC56 (vuelve a la frase de 32243d0a) y
##       el ancla de MC57. Ninguna sonda, ningún umbral ni ninguna firma.
''')

# 2. P6_CRITERIO
txt = cambia(txt, 'P6_CRITERIO <- "' + CRIT_ANTES + '"', 'P6_CRITERIO <- "' + CRIT_AHORA + '"')
txt = cambia(txt, "## ef2c96dc) y MC57 (una cifra en el tramo general, con el literal intacto).\n",
             "## 32243d0a) y MC57 (una cifra en el tramo general, con el literal intacto).\n")

# 3. P6_NUEVO (el literal de MC51), leído de los archivos
txt = cambia(txt, 'P6_NUEVO <- r"---(' + P6_ANTES + ')---"', 'P6_NUEVO <- r"---(' + P6_AHORA + ')---"')

# 4. MC56
txt = cambia(txt, 'P6_CRIT_VIEJO <- "' + CRIT_EF2C + '"', 'P6_CRIT_VIEJO <- "' + CRIT_ANTES + '"')
MC56_ANTES = r'''mut_crit("MC56", r"---(Por eso, si el grado que perdió tiene un curso más o el que ganó ",
    "tiene uno menos, basta comparar el promedio de ese grado con la diferencia entre los totales: si el promedio es ",
    "mayor, el ganador cambia; si es menor, no. ",)---",
  r"---(Por eso, con un curso más o uno menos, basta comparar ese promedio ",
    "con la diferencia entre los totales: si el promedio es mayor, el ganador cambia; si es menor, no. ",)---",
  "falta", "vuelve a la frase de ef2c96dc, sin tocar las comparaciones", lit_mut = P6_CRIT_VIEJO)
'''
MC56_AHORA = r'''mut_crit("MC56", r"---(Por eso, para saber si el grado que perdió habría ganado con un ",
    "curso más, o si el que ganó habría perdido con uno menos, basta comparar el promedio de cada uno con la ",
    "diferencia entre los totales: si el promedio es mayor, sí; si es menor, no. ",)---",
  r"---(Por eso, si el grado que perdió tiene un curso más o el que ganó ",
    "tiene uno menos, basta comparar el promedio de ese grado con la diferencia entre los totales: si el promedio es ",
    "mayor, el ganador cambia; si es menor, no. ",)---",
  "falta", "vuelve a la frase de 32243d0a, sin tocar las comparaciones", lit_mut = P6_CRIT_VIEJO)
'''
txt = cambia(txt, MC56_ANTES, MC56_AHORA)

# 5. MC57: su ancla
MC57_ANTES = r'''mut_crit("MC57", '"promedio al total, y un curso menos lo resta. Por eso, si el grado', '"promedio al total, y un curso menos lo resta (", dp$D, "). Por eso, si el grado','''
MC57_AHORA = r'''mut_crit("MC57", '"promedio al total, y un curso menos lo resta. Por eso, para saber', '"promedio al total, y un curso menos lo resta (", dp$D, "). Por eso, para saber','''
txt = cambia(txt, MC57_ANTES, MC57_AHORA)

assert not any(chr(c) in txt for c in (0xA0, 0x2009, 0x202F, 0x2060))
with open(ver_p, "w", encoding="utf-8") as f:
    f.write(txt)
print("md5 de partida", md5, "· nuevo", hashlib.md5(txt.encode("utf-8")).hexdigest())
