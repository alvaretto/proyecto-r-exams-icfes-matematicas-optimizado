#!/usr/bin/env python3
"""Tanda C2: deriva por reemplazo los scripts de los pasos 9 y 10 (etiquetas primero, md5 después; cada patrón exigido una vez).
Uso: cd cloze/salida && python3 -I ciclo_20261102/c2/derivar_c2.py"""
def rep(s, a, b, n=1):
    assert s.count(a) == n, (a, s.count(a))
    return s.replace(a, b)
# 1. análisis por rama
s = open("diagnosticidad_por_rama_t8.R", encoding="utf-8").read()
s = rep(s, "## diagnosticidad_por_rama_t8.R — paso 9 del CLOZE, tanda 8 (2026-10-31): copia de diagnosticidad_por_rama.R (tanda 3)\n",
        "## diagnosticidad_por_rama_c2.R — paso 9 del CLOZE, tanda C2 (2026-11-03): copia de diagnosticidad_por_rama_t8.R, sin cambios de\n"
        "## medida. Lee el .Rmd y las funciones de ../verificar_espacio_cloze.R vigentes.\n"
        "## Origen, tanda 8 (2026-10-31): copia de diagnosticidad_por_rama.R (tanda 3)\n")
s = rep(s, "## Uso (desde cloze/salida): Rscript --no-init-file diagnosticidad_por_rama.R [N]",
        "## Uso (desde cloze/salida): Rscript --no-init-file ciclo_20261102/c2/diagnosticidad_por_rama_c2.R [N]")
open("ciclo_20261102/c2/diagnosticidad_por_rama_c2.R", "w", encoding="utf-8").write(s)
# 2. literalidad propia de la tanda 9b, con el catálogo
s = open("literalidad_exextra_r9.py", encoding="utf-8").read()
s = rep(s, '"""Paso 10 (tanda 9b): literalidad de los exextra del CLOZE contra el catálogo canónico (solo lectura).\nUso: cd cloze/salida && python3 -I literalidad_exextra_r9.py"""',
        '"""Paso 10 (tanda C2; copia de literalidad_exextra_r9.py de la tanda 9b): literalidad de los exextra del CLOZE contra el catálogo canónico (solo lectura).\nUso: cd cloze/salida && python3 -I ciclo_20261102/c2/literalidad_exextra_c2.py"""')
open("ciclo_20261102/c2/literalidad_exextra_c2.py", "w", encoding="utf-8").write(s)
print("derivados: diagnosticidad_por_rama_c2.R, literalidad_exextra_c2.py")
# 3. V5 a V11 (etiquetas primero; rutas de los artefactos de la tanda C2)
s = open("v5_v11_r9.py", encoding="utf-8").read()
s = rep(s, '"""Paso 10 (tanda 9b): V5 a V11 del orquestador-cloze sobre el md5 vigente.', '"""Paso 10 (tanda C2; copia de v5_v11_r9.py de la tanda 9b): V5 a V11 del orquestador-cloze sobre el md5 vigente.')
s = rep(s, "de coherencias_r9/) y comprueba", "de coherencias_r10/) y comprueba")
s = rep(s, "Uso: cd cloze/salida && python3 -I v5_v11_r9.py", "Uso: cd cloze/salida && python3 -I ciclo_20261102/c2/v5_v11_c2.py")
s = rep(s, "«RESIDUO DECLARADO: V11 a un paso» del verificador r9", "«RESIDUO DECLARADO: V11 a un paso» del verificador de la C1e")
s = rep(s, '"verificador r9: md5 vigente, exit 4, 0 fallos, 0 pendientes"', '"verificador de la C1e: md5 vigente, exit 4, 0 fallos, 0 pendientes"')
s = rep(s, "# --- V10 y V11: verificador r9", "# --- V10 y V11: verificador de la C1e")
s = rep(s, 'r = "verificar_espacio_cloze_r9.txt"', 'r = "ciclo_20261102/c1e/verificar_espacio_cloze_c1e.txt"')
s = rep(s, '("diversidad_sustantiva_r9.txt", "6 gap(s) medidos de 6 declarados")', '("ciclo_20261102/c2/diversidad_sustantiva_c2.txt", "6 gap(s) medidos de 6 declarados")')
s = rep(s, '("diagnosticidad_r9.txt", "PASS: ninguna sonda superficial supera el 70 %")', '("ciclo_20261102/c2/diagnosticidad_c2.txt", "PASS: ninguna sonda superficial supera el 70 %")')
s = rep(s, '("diagnosticidad_por_rama_r8.txt", "N = 100 por estrato")', '("ciclo_20261102/c2/diagnosticidad_por_rama_c2.txt", "N = 100 por estrato")')
s = s.replace("coherencias_r9/", "coherencias_r10/")
s = rep(s, "coherencias_r10/verificacion_N100.txt", "coherencias_r10/verificar_coherencias.txt")
s = rep(s, "coherencias_r10/verificacion_formatos.txt", "coherencias_r10/verificar_formatos.txt")
s = rep(s, '"TOTAL: fallos por versión = 0 · fallos globales = 0" in t', '"TOTAL: fallos por versión = 0 · fallos globales = 0 · mutaciones sin cazar o sin su sonda = 0" in t')
assert "coherencias_r9" not in s and "_r9.txt" not in s and "_r8.txt" not in s
open("ciclo_20261102/c2/v5_v11_c2.py", "w", encoding="utf-8").write(s)
print("derivado: v5_v11_c2.py")
