#!/usr/bin/env python3
"""editar_rmd_c1b.py — tanda C1b del ciclo 2026-11-02 del CLOZE (2026-11-03).

H1 de la FASE 2C r4 (decisión del profesor del 2026-11-03, «Reescribir ahora (Recomendada)»):
  1. data_generation: hp_supera y hg_supera salen de los datos y un stopifnot los compara con v_hp y v_hg.
  2. Solution: el párrafo «Parte 6.» resuelve cada comparación con las cifras de la versión («más» o «menos»)
     y su conclusión; el criterio general queda sin cifras.
Cada patrón se exige exactamente una vez. Parte de 27f789dc6327f7d6bda72cded61502cd (md5 comprobado).
Uso: python3 -I editar_rmd_c1b.py <ruta del .Rmd>
"""
import hashlib
import sys

ruta = sys.argv[1]
with open(ruta, encoding="utf-8") as f:
    txt = f.read()
md5 = hashlib.md5(txt.encode("utf-8")).hexdigest()
assert md5 == "27f789dc6327f7d6bda72cded61502cd", f"md5 de partida inesperado: {md5}"

CAMBIOS = [
    # 1. data_generation: la comparación sale de los datos y se compara con los veredictos sorteados.
    (
        r'''stopifnot("retroalimentación de la Parte 6 coherente con la marca de la afirmación de ventaja" =
            ((tot_v > calc_v$rival) == grepl("habría ganado", c6$texto, fixed = TRUE)) == v_c)
''',
        r'''stopifnot("retroalimentación de la Parte 6 coherente con la marca de la afirmación de ventaja" =
            ((tot_v > calc_v$rival) == grepl("habría ganado", c6$texto, fixed = TRUE)) == v_c)
## H1 de la FASE 2C r4 (tanda C1b, decisión del profesor del 2026-11-03 «Reescribir ahora (Recomendada)»): la Solution de
## la Parte 6 resuelve cada comparación con las cifras de la versión. La comparación sale de los datos y tiene que
## coincidir con los veredictos sorteados de HP y HG (los empates los excluye «Parte 6: hipótesis sin empate»). MC52.
hp_supera <- dp$aP > dp$D
hg_supera <- dp$aG > dp$D
stopifnot("H1 r4: la comparación de la Solution de la Parte 6 coincide con los veredictos sorteados de HP y HG" =
            hp_supera == v_hp && hg_supera == v_hg)
''',
    ),
    # 2. Solution: el párrafo «Parte 6.».
    (
        r'''cat("Parte 6. Cada hipótesis cambia un solo dato de la tabla. Un curso más con el mismo promedio suma ese ",
    "promedio al total, y un curso menos lo resta. Por eso el grado ", per_par, " habría ganado con un curso más ",
    "si y solo si su promedio por curso, ", dp$aP, ED, "kilos, es mayor que la diferencia, ", dp$D, ED, "kilos; y el grado ",
    gan_par, " habría perdido con un curso menos si y solo si su promedio, ", dp$aG, ED, "kilos, es mayor que esa ",
    "diferencia.\n\n", sep = "")
''',
        r'''## H1 de la FASE 2C r4 (tanda C1b, 2026-11-03): cada comparación con cifras de la versión va resuelta con los datos
## (hp_supera y hg_supera, del data_generation) y lleva la conclusión que se sigue; el criterio general va sin cifras.
## Las dos últimas palabras llevan el espacio duro: el párrafo no termina con una palabra sola en su último renglón.
cat("Parte 6. Cada hipótesis cambia un solo dato de la tabla. Un curso más con el mismo promedio suma ese ",
    "promedio al total, y un curso menos lo resta. Por eso basta comparar ese promedio con la diferencia entre los ",
    "totales: si es mayor, el ganador cambia; si es menor, no. Con un curso más, el grado ", per_par,
    " habría sumado a su total ", dp$aP, ED, "kilos, ", if (hp_supera) "más" else "menos", " que la diferencia de ",
    dp$D, ED, "kilos: ", if (hp_supera) "habría ganado" else "no le habría alcanzado para ganar",
    ". Con un curso menos, el grado ", gan_par, " habría restado a su total ", dp$aG, ED, "kilos, ",
    if (hg_supera) "más" else "menos", " que la diferencia de ", dp$D, ED, "kilos: ",
    if (hg_supera) paste0("habría", ED, "perdido") else paste0("habría seguido", ED, "ganando"), ".\n\n", sep = "")
''',
    ),
]

for de, a in CAMBIOS:
    n = txt.count(de)
    assert n == 1, f"el patrón aparece {n} veces (se exige 1): {de[:70]!r}"
    txt = txt.replace(de, a, 1)

with open(ruta, "w", encoding="utf-8") as f:
    f.write(txt)
print("md5 de partida", md5, "· nuevo", hashlib.md5(txt.encode("utf-8")).hexdigest())
