#!/usr/bin/env python3
"""editar_rmd_c1e.py — tanda C1e del ciclo 2026-11-02 del CLOZE (2026-11-03).

Redacción del criterio (decisión del profesor del 2026-11-03, «Reformular (Recomendada)»): una sola frase de la Solution.
  Antes: «Por eso, si el grado que perdió tiene un curso más o el que ganó tiene uno menos, basta comparar el promedio de ese grado
          con la diferencia entre los totales: si el promedio es mayor, el ganador cambia; si es menor, no.»
  Ahora: «Por eso, para saber si el grado que perdió habría ganado con un curso más, o si el que ganó habría perdido con uno menos,
          basta comparar el promedio de cada uno con la diferencia entre los totales: si el promedio es mayor, sí; si es menor, no.»
Nada más cambia: ni el data_generation, ni las dos frases resueltas, ni el espacio duro final.
El patrón se exige exactamente una vez. Parte de 32243d0a6e6c499a4c6b1213a49e95cd (md5 comprobado).
Uso: python3 -I editar_rmd_c1e.py <ruta del .Rmd>
"""
import hashlib
import sys

ruta = sys.argv[1]
with open(ruta, encoding="utf-8") as f:
    txt = f.read()
md5 = hashlib.md5(txt.encode("utf-8")).hexdigest()
assert md5 == "32243d0a6e6c499a4c6b1213a49e95cd", f"md5 de partida inesperado: {md5}"

DE = r'''    "promedio al total, y un curso menos lo resta. Por eso, si el grado que perdió tiene un curso más o el que ganó ",
    "tiene uno menos, basta comparar el promedio de ese grado con la diferencia entre los totales: si el promedio es ",
    "mayor, el ganador cambia; si es menor, no. ",
    "Con un curso más, el grado ", per_par,
'''
A = r'''    "promedio al total, y un curso menos lo resta. Por eso, para saber si el grado que perdió habría ganado con un ",
    "curso más, o si el que ganó habría perdido con uno menos, basta comparar el promedio de cada uno con la ",
    "diferencia entre los totales: si el promedio es mayor, sí; si es menor, no. ",
    "Con un curso más, el grado ", per_par,
'''
LITERAL = ("Por eso, para saber si el grado que perdió habría ganado con un curso más, o si el que ganó habría perdido con uno menos, "
           "basta comparar el promedio de cada uno con la diferencia entre los totales: si el promedio es mayor, sí; si es menor, no. ")
unido = "".join(l.split('"')[1] for l in A.splitlines()[:3])
assert LITERAL in unido, "las tres cadenas no dan la frase literal"
n = txt.count(DE)
assert n == 1, f"el patrón aparece {n} veces (se exige 1)"
txt = txt.replace(DE, A, 1)
assert not any(chr(c) in txt for c in (0xA0, 0x2009, 0x202F, 0x2060))
with open(ruta, "w", encoding="utf-8") as f:
    f.write(txt)
print("md5 de partida", md5, "· nuevo", hashlib.md5(txt.encode("utf-8")).hexdigest())
