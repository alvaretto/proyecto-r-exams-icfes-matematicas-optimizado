#!/usr/bin/env python3
"""generar_invariancia_c1d.py — tanda C1d: deriva invariancia_c1d.R de invariancia_c1c.R, sin transcribirla (cada patrón, las veces
declaradas). Solo cambian el md5 de partida, los dos literales de la frase y sus patrones fuente; el procedimiento es el mismo.
Uso: python3 -I generar_invariancia_c1d.py <invariancia_c1c.R> <invariancia_c1d.R>
"""
import sys

ent, sal = sys.argv[1:3]
t = open(ent, encoding="utf-8").read()


def cambia(t, de, a, n=1):
    c = t.count(de)
    assert c == n, f"{de[:70]!r}: {c} veces (se exigen {n})"
    return t.replace(de, a)


F_ANT = "Por eso basta comparar ese promedio con la diferencia entre los totales: si es mayor, el ganador cambia; si es menor, no."
F_VIE = "Por eso, con un curso más o uno menos, basta comparar ese promedio con la diferencia entre los totales: si el promedio es mayor, el ganador cambia; si es menor, no."
F_NUE = "Por eso, si el grado que perdió tiene un curso más o el que ganó tiene uno menos, basta comparar el promedio de ese grado con la diferencia entre los totales: si el promedio es mayor, el ganador cambia; si es menor, no."
SRC_N_ANT = r"""SRC_N <- '"promedio al total, y un curso menos lo resta. Por eso, con un curso más o uno menos, basta comparar ese promedio ",\n    "con la diferencia entre los totales: si el promedio es mayor, el ganador cambia; si es menor, no. ",\n    "Con un curso más, el grado ", per_par,\n'"""
SRC_V_ANT = r"""SRC_V <- '"promedio al total, y un curso menos lo resta. Por eso basta comparar ese promedio con la diferencia entre los ",\n    "totales: si es mayor, el ganador cambia; si es menor, no. Con un curso más, el grado ", per_par,\n'"""
SRC_N_NUE = r"""SRC_N <- '"promedio al total, y un curso menos lo resta. Por eso, si el grado que perdió tiene un curso más o el que ganó ",\n    "tiene uno menos, basta comparar el promedio de ese grado con la diferencia entre los totales: si el promedio es ",\n    "mayor, el ganador cambia; si es menor, no. ",\n    "Con un curso más, el grado ", per_par,\n'"""
SRC_V_NUE = SRC_N_ANT.replace("SRC_N", "SRC_V")

t = cambia(t, 'F_VIEJA <- "' + F_ANT + '"', 'F_VIEJA <- "' + F_VIE + '"')
t = cambia(t, 'F_NUEVA <- "' + F_VIE + '"', 'F_NUEVA <- "' + F_NUE + '"')
t = cambia(t, SRC_N_ANT, SRC_N_NUE)
t = cambia(t, SRC_V_ANT, SRC_V_NUE)
# Primero c1c -> c1d: el md5 de partida contiene la cadena «c1c», y al revés quedaría falso.
t = t.replace("c1c", "c1d").replace("C1c", "C1d")
t = t.replace("cloze_6f6f1b13.Rmd.txt", "cloze_ef2c96dc.Rmd.txt")
t = t.replace("6f6f1b1311b0d681bd974c719ecd75d5", "ef2c96dc1c2b667fa904ea231329b4dc")
t = t.replace("6f6f1b13", "ef2c96dc")
assert "6f6f1b" not in t and t.count("ef2c96dc1c2b667fa904ea231329b4dc") == 2
open(sal, "w", encoding="utf-8").write(t)
print("escrito", sal)
