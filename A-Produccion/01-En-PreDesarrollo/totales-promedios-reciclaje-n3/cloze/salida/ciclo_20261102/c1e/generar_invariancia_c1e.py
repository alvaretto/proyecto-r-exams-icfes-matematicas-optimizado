#!/usr/bin/env python3
"""generar_invariancia_c1e.py — tanda C1e: deriva invariancia_c1e.R de invariancia_c1d.R, sin transcribirla (cada patrón, las veces
declaradas). Solo cambian el md5 de partida, los dos literales de la frase y sus patrones fuente; el procedimiento es el mismo.
Primero las etiquetas, después los md5 (precaución de la C1d).
Uso: python3 -I generar_invariancia_c1e.py <invariancia_c1d.R> <invariancia_c1e.R>
"""
import sys

ent, sal = sys.argv[1:3]
t = open(ent, encoding="utf-8").read()


def cambia(t, de, a, n=1):
    c = t.count(de)
    assert c == n, f"{de[:70]!r}: {c} veces (se exigen {n})"
    return t.replace(de, a)


F_ANT = "Por eso, con un curso más o uno menos, basta comparar ese promedio con la diferencia entre los totales: si el promedio es mayor, el ganador cambia; si es menor, no."
F_VIE = "Por eso, si el grado que perdió tiene un curso más o el que ganó tiene uno menos, basta comparar el promedio de ese grado con la diferencia entre los totales: si el promedio es mayor, el ganador cambia; si es menor, no."
F_NUE = "Por eso, para saber si el grado que perdió habría ganado con un curso más, o si el que ganó habría perdido con uno menos, basta comparar el promedio de cada uno con la diferencia entre los totales: si el promedio es mayor, sí; si es menor, no."
SRC_N_ANT = r"""SRC_N <- '"promedio al total, y un curso menos lo resta. Por eso, si el grado que perdió tiene un curso más o el que ganó ",\n    "tiene uno menos, basta comparar el promedio de ese grado con la diferencia entre los totales: si el promedio es ",\n    "mayor, el ganador cambia; si es menor, no. ",\n    "Con un curso más, el grado ", per_par,\n'"""
SRC_V_ANT = r"""SRC_V <- '"promedio al total, y un curso menos lo resta. Por eso, con un curso más o uno menos, basta comparar ese promedio ",\n    "con la diferencia entre los totales: si el promedio es mayor, el ganador cambia; si es menor, no. ",\n    "Con un curso más, el grado ", per_par,\n'"""
SRC_N_NUE = r"""SRC_N <- '"promedio al total, y un curso menos lo resta. Por eso, para saber si el grado que perdió habría ganado con un ",\n    "curso más, o si el que ganó habría perdido con uno menos, basta comparar el promedio de cada uno con la ",\n    "diferencia entre los totales: si el promedio es mayor, sí; si es menor, no. ",\n    "Con un curso más, el grado ", per_par,\n'"""
SRC_V_NUE = SRC_N_ANT.replace("SRC_N", "SRC_V")

t = cambia(t, 'F_VIEJA <- "' + F_ANT + '"', 'F_VIEJA <- "' + F_VIE + '"')
t = cambia(t, 'F_NUEVA <- "' + F_VIE + '"', 'F_NUEVA <- "' + F_NUE + '"')
t = cambia(t, SRC_N_ANT, SRC_N_NUE)
t = cambia(t, SRC_V_ANT, SRC_V_NUE)
# Primero c1d -> c1e; después los md5.
t = t.replace("c1d", "c1e").replace("C1d", "C1e")
t = t.replace("cloze_ef2c96dc.Rmd.txt", "cloze_32243d0a.Rmd.txt")
t = t.replace("ef2c96dc1c2b667fa904ea231329b4dc", "32243d0a6e6c499a4c6b1213a49e95cd")
t = t.replace("ef2c96dc", "32243d0a")
assert "ef2c96" not in t and t.count("32243d0a6e6c499a4c6b1213a49e95cd") == 2
open(sal, "w", encoding="utf-8").write(t)
print("escrito", sal)
