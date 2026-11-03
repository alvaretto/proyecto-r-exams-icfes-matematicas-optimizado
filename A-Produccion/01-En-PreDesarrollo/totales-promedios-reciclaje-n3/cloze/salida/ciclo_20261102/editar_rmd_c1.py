#!/usr/bin/env python3
# editar_rmd_c1.py — tanda C1 del ciclo 2026-11-02 del CLOZE (2026-11-03). Ediciones del .Rmd FUERA del bloque P1
# (el bloque lo pone empalmar_bloque_p1.R). Cada patrón se exige un número exacto de veces; si no, aborta sin escribir.
#   1. Comentarios que nombraban el SCHOICE viejo (e293d5c8, líneas 2-561) y el espacio duro «pendiente».
#   2. Los dos U+00A0 literales del código propio (gsub del espacio duro) pasan a su escape "\u00a0" (mismo valor en R).
#   3. L-6 en la prosa propia de la Parte 1 de la Solution: la unidad y el signo dentro de la fórmula (V1 y V1′) y el
#      espacio duro entre cifra y unidad, con las mismas cadenas que la Solution del SCHOICE 63e2713e.
#   4. H3 de la FASE 2C r3 (decisión del profesor del 2026-11-02, diferida a este ciclo): «si y solo si … es mayor».
# Uso: python3 -I editar_rmd_c1.py <ruta del .Rmd del CLOZE>
import hashlib, sys
ruta = sys.argv[1]
src = open(ruta, encoding="utf-8").read()
antes = hashlib.md5(src.encode("utf-8")).hexdigest()
NB = "\u00a0"
E = [
  # 1. comentarios
  ("## Todo lo anterior es el data_generation del SCHOICE aprobado (md5 e293d5c812b1f9790e2bb777b62de9a5,\n"
   "## sus líneas 2 a 561), trasplantado con un script y con tres renombres: opciones_p1, sol_p1 y feedback_p1.",
   "## Todo lo anterior es el data_generation del SCHOICE aprobado (md5 63e2713e6ac0bdbf250f4dc33901b2ec, sus líneas\n"
   "## 2 a 612; re-empalmado en la tanda C1 del ciclo 2026-11-02, el 2026-11-03), trasplantado con un script y con\n"
   "## tres renombres: opciones_p1, sol_p1 y feedback_p1.", 1),
  ("## 2026-10-30; BAJA 1 de la 6b: «en 15 / kilos.» partido). La Parte 1 sale igual que el SCHOICE (decisión 2 a del\n"
   "## profesor): su espacio duro va al próximo ciclo del SCHOICE y luego se re-empalma. En PDF pandoc lo vuelve «~».",
   "## 2026-10-30; BAJA 1 de la 6b: «en 15 / kilos.» partido). La Parte 1 sale igual que el SCHOICE: desde la tanda C1\n"
   "## (2026-11-03) trae el espacio duro de L-6 (firmada en el ciclo 2026-11-02 del SCHOICE), y el bloque P1 define ED\n"
   "## con este mismo valor (E1 lo compara). En PDF pandoc lo vuelve «~».", 1),
  # 2. literales U+00A0 del código propio
  ('gsub("' + NB + '", " ",', 'gsub("\\u00a0", " ",', 2),
  # 3. L-6 en la prosa propia de la Parte 1 (las mismas cadenas que la Solution del SCHOICE 63e2713e)
  ('      "$ kilos.\\n", sep = "")', '      "\\\\ \\\\text{kilos.}$\\n", sep = "")', 1),
  ('" = ", d$D, "$ kilos más. ",', '" = ", d$D, "\\\\ \\\\text{kilos}$ más. ",', 1),
  ('paste0("Su promedio era menor (", d$aG, " kilos frente a ", d$aP,',
   'paste0("Su promedio era menor (", d$aG, ED, "kilos frente a ", d$aP,', 1),
  ('             " kilos frente a ", d$aP, "), y eso pesó más.")',
   '             ED, "kilos frente a ", d$aP, "), y eso pesó más.")', 1),
  ('", solo una cita la diferencia correcta, ", d$D, " kilos.\\n\\n", sep = "")',
   '", solo una cita la diferencia correcta, ", d$D, ED, "kilos.\\n\\n", sep = "")', 1),
  ('" = ", d$TG, "$ kilos y ", nomP, " recogió $",', '" = ", d$TG, "\\\\ \\\\text{kilos}$ y ", nomP, " recogió $",', 1),
  ('" = ", d$TP, "$ kilos. La diferencia es $",', '" = ", d$TP, "\\\\ \\\\text{kilos.}$ La diferencia es $",', 1),
  ('    "$ kilos a favor de ", nomG, ".\\n\\n", sep = "")', '    "\\\\ \\\\text{kilos}$ a favor de ", nomG, ".\\n\\n", sep = "")', 1),
  # 4. H3 (Parte 6): condición necesaria y suficiente, en indicativo (sin lectura contrafáctica)
  ('"solo si su promedio por curso, ", dp$aP, ED, "kilos, fuera mayor que la diferencia, "',
   '"si y solo si su promedio por curso, ", dp$aP, ED, "kilos, es mayor que la diferencia, "', 1),
  ('" habría perdido con un curso menos solo si su promedio, ", dp$aG, ED, "kilos, fuera mayor que esa ",',
   '" habría perdido con un curso menos si y solo si su promedio, ", dp$aG, ED, "kilos, es mayor que esa ",', 1),
]
for viejo, nuevo, n in E:
    k = src.count(viejo)
    if k != n:
        sys.exit(f"ABORTO sin escribir: el patrón aparece {k} veces (se esperaban {n}): {viejo[:90]!r}")
    src = src.replace(viejo, nuevo)
for ch in ("\u00a0", "\u2009", "\u202f", "\u2060"):
    if ch in src:
        sys.exit(f"ABORTO sin escribir: queda un U+{ord(ch):04X} literal en el .Rmd")
if "$ kilos" in src:
    sys.exit("ABORTO sin escribir: queda una fórmula que cierra antes de «kilos»")
open(ruta, "w", encoding="utf-8").write(src)
print(f"{len(E)} patrones aplicados ({sum(n for _, _, n in E)} sitios); md5 {antes} -> {hashlib.md5(src.encode('utf-8')).hexdigest()}")
