#!/usr/bin/env python3
"""editar_rmd_c1d.py — tanda C1d del ciclo 2026-11-02 del CLOZE (2026-11-03).

Lectura residual del criterio (decisión del profesor del 2026-11-03, «Cambiar la frase (Recomendada)»): una sola frase de la Solution.
  Antes: «Por eso, con un curso más o uno menos, basta comparar ese promedio con la diferencia entre los totales: si el promedio
          es mayor, el ganador cambia; si es menor, no.»
  Ahora: «Por eso, si el grado que perdió tiene un curso más o el que ganó tiene uno menos, basta comparar el promedio de ese grado
          con la diferencia entre los totales: si el promedio es mayor, el ganador cambia; si es menor, no.»
Nada más cambia: ni el data_generation, ni las dos frases resueltas, ni el espacio duro final.
El patrón se exige exactamente una vez. Parte de ef2c96dc1c2b667fa904ea231329b4dc (md5 comprobado).
Uso: python3 -I editar_rmd_c1d.py <ruta del .Rmd>
"""
import hashlib
import sys

ruta = sys.argv[1]
with open(ruta, encoding="utf-8") as f:
    txt = f.read()
md5 = hashlib.md5(txt.encode("utf-8")).hexdigest()
assert md5 == "ef2c96dc1c2b667fa904ea231329b4dc", f"md5 de partida inesperado: {md5}"

DE = r'''    "promedio al total, y un curso menos lo resta. Por eso, con un curso más o uno menos, basta comparar ese promedio ",
    "con la diferencia entre los totales: si el promedio es mayor, el ganador cambia; si es menor, no. ",
    "Con un curso más, el grado ", per_par,
'''
A = r'''    "promedio al total, y un curso menos lo resta. Por eso, si el grado que perdió tiene un curso más o el que ganó ",
    "tiene uno menos, basta comparar el promedio de ese grado con la diferencia entre los totales: si el promedio es ",
    "mayor, el ganador cambia; si es menor, no. ",
    "Con un curso más, el grado ", per_par,
'''
n = txt.count(DE)
assert n == 1, f"el patrón aparece {n} veces (se exige 1)"
txt = txt.replace(DE, A, 1)
with open(ruta, "w", encoding="utf-8") as f:
    f.write(txt)
print("md5 de partida", md5, "· nuevo", hashlib.md5(txt.encode("utf-8")).hexdigest())
