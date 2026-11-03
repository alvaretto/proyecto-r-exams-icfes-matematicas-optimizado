"""Tanda S2a (2026-11-03): H2 de la FASE 2C r5, cerrado por decisión del profesor («Cerrarlo ahora (Recomendada)»).
1. El punto que sigue a la fórmula de la explicación de ERR-RESTA entra en la fórmula: «\\text{.}$ Esa resta…».
2. La guarda de L-6 del signo pasa a cubrir toda fórmula, con unidad o sin ella, y cualquier separador que no sea
   letra ni cifra entre el cierre y el signo.
3. Comentario de cabecera.
Cada sustitución exige que el patrón aparezca exactamente una vez. Sin escapes de Unicode en este fuente.
Uso: python3 -I editar_rmd_s2a.py <ej.Rmd> <copia_de_respaldo>"""
import sys, shutil, hashlib
rmd, copia = sys.argv[1], sys.argv[2]
shutil.copyfile(rmd, copia)
t = open(rmd, encoding="utf-8").read()
antes = hashlib.md5(t.encode("utf-8")).hexdigest()
CAMBIOS = [
    # 1. H2: el punto entra en la fórmula (línea 262 de 6c9d7705)
    (r'" = ", p$delta, "$. Esa resta compara un curso típico de cada ",',
     r'" = ", p$delta, "\\text{.}$ Esa resta compara un curso típico de cada ",'),
    # 2. La guarda del signo, generalizada: mensaje, comentario y patrón
    (r'''## L-6 (tanda S1b relanzada, V1'): el signo que sigue a la unidad de una fórmula va dentro de ella, «\\text{kilos.}$».
stopifnot("L-6: el signo que sigue a la unidad de una fórmula va dentro de la fórmula" =
            !any(grepl("\\\\text\\{(kilos?|cursos?)\\}\\$[.,:;''',
     r'''## L-6 (tanda S1b relanzada, V1'): el signo que sigue a la unidad de una fórmula va dentro de ella, «\\text{kilos.}$».
## Tanda S2a (2026-11-03, decisión del profesor «Cerrarlo ahora»): también el de toda fórmula sin unidad, «= 5\\text{.}$»
## (ERR-RESTA), y con cualquier carácter que no sea letra ni cifra entre el cierre de la fórmula y el signo.
stopifnot("L-6: el signo que sigue a una fórmula, con unidad o sin ella, va dentro de la fórmula" =
            !any(grepl("\\$[^A-Za-z0-9$\\n]{0,3}[.,:;'''),
    # 3. Cabecera
    (r'''## fórmula y el signo (6b de V1, N = 100: el punto solo en 34 semillas a 360 px y en 5 a 320 px).
''',
     r'''## fórmula y el signo (6b de V1, N = 100: el punto solo en 34 semillas a 360 px y en 5 a 320 px).
## Tanda S2a (2026-11-03, decisión del profesor «Cerrarlo ahora (Recomendada)»): el signo que sigue a una fórmula SIN
## unidad también entra en ella, «$... = 5\\text{.}$». Es la explicación de ERR-RESTA, la única: con el punto fuera,
## Chromium podía partir el renglón entre la fórmula y el punto (42 casos en 100 versiones; H2 de la FASE 2C r5).
'''),
]
for de, a in CAMBIOS:
    n = t.count(de)
    if n != 1:
        sys.exit(f"ABORTO: el patrón aparece {n} veces (se exige 1): {de[:70]!r}")
    t = t.replace(de, a)
for ch in (0xA0, 0x2009, 0x202F, 0x2060):
    assert chr(ch) not in t, f"carácter U+{ch:04X} literal en el .Rmd"
open(rmd, "w", encoding="utf-8").write(t)
print("md5 antes:", antes, "· después:", hashlib.md5(t.encode("utf-8")).hexdigest())
