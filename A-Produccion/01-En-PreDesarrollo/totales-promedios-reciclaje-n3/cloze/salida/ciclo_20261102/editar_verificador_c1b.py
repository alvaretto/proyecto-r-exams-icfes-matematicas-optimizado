#!/usr/bin/env python3
# editar_verificador_c1b.py — tanda C1 (2026-11-03), segunda edición de verificar_espacio_cloze.R, tras la primera corrida
# (salida/ciclo_20261102/verificar_espacio_cloze_c1.txt: FALLA (1), «E2 p1: 283/330 versiones»). El oráculo de la Parte 1
# comparaba el nombre de la sede leído de la tabla (sin U+00A0) con el sujeto de la opción, que desde L-6 lleva «El Bosque»
# y «La Pradera» con espacio duro. Lee ahora el texto como se ve (decisión (a), por F-2); MC49 lo corre sin normalizar.
import hashlib, sys
ruta = sys.argv[1]; src = open(ruta, encoding="utf-8").read(); antes = hashlib.md5(src.encode("utf-8")).hexdigest()
MC49 = '''## MC49: el oráculo de la Parte 1 (E2) sin normalizar el espacio duro. La sede que lee de la tabla (sus celdas no llevan
## U+00A0) deja de casar con el sujeto de la opción cuando el nombre tiene dos palabras («El Bosque», «La Pradera»).
ok49 <- vapply(TODAS, or_p1, NA, nf = identity); dos <- vapply(TODAS, function(x) grepl(" ", tabla_leida(x$tabla_md)$nombre[1], fixed = TRUE), NA)
ver49 <- if (!all(vapply(TODAS, or_p1, NA))) "mal_construido" else if (all(ok49)) "no_detectado" else if (identical(unname(!ok49), unname(dos))) "cazado_por_su_sonda" else "cazado_por_otra"
d49 <- sprintf("sin normalizar, el oráculo acierta %d de %d versiones; fallan %d, y las versiones con nombres de dos palabras son %d", sum(ok49), length(ok49), sum(!ok49), sum(dos))
ok(ver49 == "cazado_por_su_sonda", "MC49 (E2 p1 sin normalizar el espacio duro): ", d49, " — ", ver49)
filas_mut[["MC49"]] <- c("MC49", "C1: E2 p1 sin normalizar", d49, ver49, NA)
'''
E = [
  ("or_p1 <- function(x) {\n",
   "## Tanda C1: el oráculo lee el texto como se ve, con U+00A0 como espacio (decisión (a) del SCHOICE, por F-2). MC49.\n"
   "or_p1 <- function(x, nf = norm_ed) {\n", 1),
  ("grepl(tolower(tb$nombre[g]), tolower(sujeto(o)), fixed = TRUE), NA)", "grepl(tolower(tb$nombre[g]), tolower(sujeto(nf(o))), fixed = TRUE), NA)", 1),
  ('       "dentro", "caso específico: la unidad desaparece de «a favor de» (solo el control exacto)")\n',
   '       "dentro", "caso específico: la unidad desaparece de «a favor de» (solo el control exacto)")\n' + MC49, 1),
  ('"MC47", "MC48"), c("id"', '"MC47", "MC48", "MC49"), c("id"', 1),
]
for viejo, nuevo, n in E:
    k = src.count(viejo)
    if k != n: sys.exit(f"ABORTO sin escribir: el patrón aparece {k} veces (se esperaban {n}): {viejo[:80]!r}")
    src = src.replace(viejo, nuevo)
for ch in (0xa0, 0x2009, 0x202f, 0x2060):
    if chr(ch) in src: sys.exit(f"ABORTO: U+{ch:04X} literal")
open(ruta, "w", encoding="utf-8").write(src)
print(f"{len(E)} cambios; md5 {antes} -> {hashlib.md5(src.encode('utf-8')).hexdigest()}")
