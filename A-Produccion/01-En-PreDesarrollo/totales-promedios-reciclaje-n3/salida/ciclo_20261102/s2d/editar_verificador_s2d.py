# Tanda S2d (2026-11-03): cablea en verificar_espacio.R la lectura (iii) en E4 y la firma del profesor
# («Firmar las 41 (Recomendada)»), regla por regla y por estrato (§P7-E), con M44 adaptado y M45 nuevo.
# Uso: python3 -I editar_verificador_s2d.py   (desde cualquier directorio; rutas absolutas)
# Cada patrón tiene que aparecer exactamente una vez; si no, aborta sin escribir. Copia previa en s2d/.
import hashlib, pathlib, shutil, sys

SUB = pathlib.Path("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3")
VER = SUB / "verificar_espacio.R"
S2D = SUB / "salida/ciclo_20261102/s2d"
MD5_ANTES = "9c65474801b5b838183a0507233b8ea1"

def md5(p): return hashlib.md5(p.read_bytes()).hexdigest()

if md5(VER) != MD5_ANTES:
    sys.exit(f"verificar_espacio.R no es {MD5_ANTES}: {md5(VER)}")
txt = VER.read_text(encoding="utf-8")
PROHIBIDOS = [" ", " ", " ", "⁠"]

CAMBIOS = []

# 1. Cabecera: la tanda S2d y el exit 4.
CAMBIOS.append((r'''##        pendientes nuevos del proceso sin normalizar.
''', r'''##        pendientes nuevos del proceso sin normalizar.
## Tanda S2d (2026-11-03), decisión del profesor «Firmar las 41 (Recomendada)»: FIRMA_S2D, regla por regla y por estrato
##        (§P7-E), con las 36 cifras de la S2b (RS1-RS7, BE1-BE8) y las 5 de la lectura (iii) de la S2c (RS8, BE9-BE12).
##        E4 mide ahora la lectura (iii) («la sede que cita la cifra pierde») en «resta» y en el barrido. Dentro de su
##        cifra: «ROJO, cubierto por la firma de la S2d» (exit 4); por encima, en otro estrato o una regla nueva > +8 pp:
##        pendiente (exit 3). M44 saca «resta» de «cubierto»; M45 (la fila de G siempre abajo) saca las de dos pasos (iii).
'''))
CAMBIOS.append((r'''##      bajo override firmado (todo dentro del alcance de «O-1 corregido» y de la decisión 2 a); 0 = sin canales en rojo.
''', r'''##      bajo override firmado (todo dentro del alcance de «O-1 corregido», de la decisión 2 a y de la firma de la S2d);
##      0 = sin canales en rojo.
'''))

# 2. Comentario de la sección S2b: la firma existe desde la S2d.
CAMBIOS.append((r'''## órdenes), con su peso en la mezcla (0,10 × exceso). Ninguna regla de la S2b está firmada: toda la que pase de +8 pp
## sobre el azar queda PENDIENTE DE FIRMA (exit 3, O-1 § Alcance). M44 (E5) fuerza ERR-RESTA y tiene que subir «resta»;
## M19 tiene que subir el producto de una fila. Nada de esta sección consume el RNG.
''', r'''## órdenes), con su peso en la mezcla (0,10 × exceso). En la S2b ninguna estaba firmada; desde la S2d rige FIRMA_S2D
## (abajo), regla por regla y por estrato, y lo que pase de +8 pp fuera de ella queda PENDIENTE DE FIRMA (exit 3, O-1
## § Alcance). M44 (E5) fuerza ERR-RESTA y tiene que sacar «resta» de «cubierto»; M19 tiene que subir el producto de una
## fila. Nada de esta sección consume el RNG.
'''))

# 3. La lectura (iii).
CAMBIOS.append((r'''lee_ii <- function(x, r) { if (is.na(r)) return(0.25); sc(!(vapply(x$opciones, cif, 0) %in% r), x$clave) }
''', r'''lee_ii <- function(x, r) { if (is.na(r)) return(0.25); sc(!(vapply(x$opciones, cif, 0) %in% r), x$clave) }
## S2d: (iii) la sede de la opción que cita el resultado pierde, y se elige al azar entre las dos opciones de la otra sede;
## sin cita, azar. Es la de la S2c (s2c/bloque_s2c.R), sin cambios. Su máximo es 50 %: elige entre dos opciones.
lee_iii <- function(x, r) { if (is.na(r)) return(0.25); v <- vapply(x$opciones, cif, 0); h <- which(v %in% r)
  if (length(h) != 1L) return(0.25); sc(x$sede_o != x$sede_o[h], x$clave) }
'''))
CAMBIOS.append((r'''R_RESTA <- "resta: la sede de la opción que cita |aG - aP| gana; la otra de esa sede"
''', r'''R_RESTA <- "resta: la sede de la opción que cita |aG - aP| gana; la otra de esa sede"
R_RESTA_III <- "resta (iii): la sede de la opción que cita |aG - aP| pierde; azar entre las dos de la otra"
'''))

# 4. La firma de la S2d y el juicio regla por regla.
CAMBIOS.append((r'''juzga_s2b <- function(TABX, grupo) {
  n_pend <- 0L
  for (rg in rownames(TABX)) for (es in colnames(TABX)) {
    tasa <- TABX[rg, es]; if (is.na(tasa)) next; ex <- tasa - 0.25
    if (ex <= 0.08 + 1e-9) next
    n_pend <- n_pend + 1L
    pendiente(sprintf("%s: «%s», %s: %.1f %% (%+.1f pp sobre el azar)%s; regla nueva por encima de +8 pp, sin firma: vuelve al profesor (O-1 § Alcance)",
                      grupo, rg, ROT_S2B[[es]], 100 * tasa, 100 * ex,
                      if (es == "canonica") sprintf("; peso en la mezcla %+.2f pp", 10 * ex) else ""))
  }
  n_pend
}
''', r'''## ---- S2d (2026-11-03): la firma del profesor, regla por regla y por estrato (§P7-E) ------------------------------
## Decisión del profesor tras la S2c, literal: «Firmar las 41 (Recomendada)». Override estructural, como O-1 (regla #24
## H-5; .claude/CLAUDE.md local, § «Ciclo 2026-11-02 (SCHOICE), tanda S2d»): las 36 cifras de la S2b
## (salida/ciclo_20261102/s2b/tabla_firma_s2b.md: RS1-RS7 y BE1-BE8) y las 5 de la lectura (iii) que midió la S2c sobre
## el diseño actual (s2c/salida_B0.txt, líneas 311-322: RS8 y BE9-BE12). Acierto en %, a un decimal, como se firmó, y se
## compara igual. Dentro de su cifra: «ROJO, cubierto por la firma de la S2d» (exit 4), nunca PASS. Por encima de su
## cifra, la misma regla en otro estrato por encima de +8 pp, o una regla nueva por encima de +8 pp: PENDIENTE (exit 3).
## No hay tope por estrato ni por familia: cada fila cubre solo su regla en su estrato. Estratos: mezcla de E1, ramas
## CURSOS y PROMEDIO (N = 100 cada una) y canónica (24 órdenes, exacta). M44 y M45 (E5) sacan cifras de «cubierto».
POR_S2D <- "la firma de la S2d"
EST4 <- c("mezcla", "CURSOS", "PROMEDIO", "canonica")
fila_s2d <- function(id, regla, estrato, acierto) data.frame(id = id, regla = regla, estrato = estrato, acierto = acierto,
                                                             stringsAsFactors = FALSE)
FIRMA_S2D <- do.call(rbind, list(
  ## 27 cifras de H1 de la r6 («resta» y sus combinaciones con las pistas de «O-1 corregido»; tabla de la S2b)
  fila_s2d("RS1", R_RESTA, EST4[1:3], c(54.2, 58.8, 67.8)),
  fila_s2d("RS2", "S_U + resta", EST4, c(71.0, 72.5, 78.5, 50.0)),
  fila_s2d("RS3", "S_U + S1 + resta", EST4, c(79.0, 86.0, 79.5, 100.0)),
  fila_s2d("RS4", "S_U + suma + resta", EST4, c(79.0, 82.0, 90.0, 50.0)),
  fila_s2d("RS5", "S_U + S1 + suma + resta", EST4, c(86.0, 93.5, 91.0, 100.0)),
  fila_s2d("RS6", "S_U + atípica + resta", EST4, c(81.5, 71.0, 86.0, 100.0)),
  fila_s2d("RS7", "S_U + S1 + suma + atípica + resta", EST4, c(93.0, 94.5, 98.0, 100.0)),
  ## 9 cifras del barrido del estímulo, lecturas (i) y (ii) (tabla de la S2b)
  fila_s2d("BE1", "|a1 - a2| (ii)", "canonica", 33.3),
  fila_s2d("BE2", "n1 * a1 (i)", "mezcla", 34.0),
  fila_s2d("BE3", "|n1 - a2| (i)", "canonica", 100.0),
  fila_s2d("BE4", "|n1 - a2| (ii)", "canonica", 33.3),
  fila_s2d("BE5", "dos pasos: |n1 - n2| * a1 (i)", c("CURSOS", "canonica"), c(34.0, 100.0)),
  fila_s2d("BE6", "dos pasos: |n1 - n2| * a1 (ii)", "canonica", 33.3),
  fila_s2d("BE7", "dos pasos: |n1 - n2| * a2 (i)", "CURSOS", 37.0),
  fila_s2d("BE8", "dos pasos: n1 * |a1 - a2| (i)", "PROMEDIO", 37.0),
  ## 5 cifras de la lectura (iii) (S2c, diseño actual B0)
  fila_s2d("RS8", R_RESTA_III, "canonica", 50.0),
  fila_s2d("BE9", "dos pasos: n1 * |a1 - a2| (iii)", "CURSOS", 38.5),
  fila_s2d("BE10", "dos pasos: n2 * |a1 - a2| (iii)", "CURSOS", 35.5),
  fila_s2d("BE11", "dos pasos: |n1 - n2| * a1 (iii)", "PROMEDIO", 35.8),
  fila_s2d("BE12", "dos pasos: |n1 - n2| * a2 (iii)", "PROMEDIO", 39.2)))
stopifnot(nrow(FIRMA_S2D) == 41L, !anyDuplicated(paste(FIRMA_S2D$regla, FIRMA_S2D$estrato)),
          all(FIRMA_S2D$estrato %in% EST4), all(FIRMA_S2D$acierto - 25 > 8))
writeLines(sprintf("  S2d · firma del profesor, regla por regla y por estrato (%d cifras; acierto en %%, como se firmó):", nrow(FIRMA_S2D)))
writeLines(sprintf("    %-5s %-9s %5.1f %%  %s", FIRMA_S2D$id, FIRMA_S2D$estrato, FIRMA_S2D$acierto, FIRMA_S2D$regla))
veredicto_s2d <- function(rg, es, tasa) {
  if (is.na(tasa) || tasa - 0.25 <= 0.08 + 1e-9) return("ok")
  i <- which(FIRMA_S2D$regla == rg & FIRMA_S2D$estrato == es)
  if (length(i) == 1L && round(100 * tasa, 1) <= FIRMA_S2D$acierto[i] + 1e-9) "cubierto" else "pendiente"
}
juzga_s2b <- function(TABX, grupo) {
  n <- c(cubierto = 0L, pendiente = 0L)
  for (rg in rownames(TABX)) for (es in colnames(TABX)) {
    tasa <- TABX[rg, es]; v <- veredicto_s2d(rg, es, tasa); if (v == "ok") next
    ex <- tasa - 0.25; i <- which(FIRMA_S2D$regla == rg & FIRMA_S2D$estrato == es)
    m <- sprintf("%s: %s«%s», %s: %.1f %% (%+.1f pp sobre el azar)%s", grupo, if (length(i) == 1L) paste0(FIRMA_S2D$id[i], " ") else "",
                 rg, ROT_S2B[[es]], 100 * tasa, 100 * ex, if (es == "canonica") sprintf("; peso en la mezcla %+.2f pp", 10 * ex) else "")
    n[[v]] <- n[[v]] + 1L
    if (v == "cubierto") firmado(sprintf("%s (firmado hasta %.1f %%)", m, FIRMA_S2D$acierto[i]), por = POR_S2D)
    else if (length(i) == 1L) pendiente(sprintf("%s, por encima de lo firmado (%.1f %%): vuelve al profesor", m, FIRMA_S2D$acierto[i]))
    else pendiente(paste0(m, "; regla nueva por encima de +8 pp, sin firma en este estrato: vuelve al profesor (O-1 § Alcance)"))
  }
  n
}
'''))
CAMBIOS.append((r'''writeLines(sprintf("  S2b H1: %d de %d cifras (%d reglas × 4 estratos) por encima de +8 pp sobre el azar: pendientes de firma",
                   np_h1, length(TAB_H1), nrow(TAB_H1)))
''', r'''writeLines(sprintf("  S2b H1: %d de %d cifras (%d reglas × 4 estratos) por encima de +8 pp sobre el azar: %d cubiertas por la firma de la S2d y %d pendientes",
                   sum(np_h1), length(TAB_H1), nrow(TAB_H1), np_h1[["cubierto"]], np_h1[["pendiente"]]))
'''))
CAMBIOS.append((r'''regla_de <- function(fr, lec) { force(fr); force(lec); function(x) { r <- fr(celdas(x)); if (lec == "i") lee_i(x, r) else lee_ii(x, r) } }
''', r'''regla_de <- function(fr, lec) { force(fr); force(lec); function(x) { r <- fr(celdas(x))
  if (lec == "i") lee_i(x, r) else if (lec == "ii") lee_ii(x, r) else if (lec == "iii") lee_iii(x, r) else stop("lectura desconocida: ", lec) } }
'''))
CAMBIOS.append((r'''writeLines(sprintf("  S2b barrido: %d operaciones (24 de un paso entre dos celdas y 4 de dos pasos) × 2 lecturas × 4 estratos; %d cifras por encima de +8 pp sobre el azar, pendientes de firma (sin «|a1 - a2| (i)», que es «resta»)",
                   length(OPS_EST), np_est))
''', r'''writeLines(sprintf("  S2b barrido: %d operaciones (24 de un paso entre dos celdas y 4 de dos pasos) × 2 lecturas × 4 estratos; %d cifras por encima de +8 pp sobre el azar (sin «|a1 - a2| (i)», que es «resta»): %d cubiertas por la firma de la S2d y %d pendientes",
                   length(OPS_EST), sum(np_est), np_est[["cubierto"]], np_est[["pendiente"]]))
'''))

# 5. El bloque de la lectura (iii) y el cotejo de la firma, antes de la batería.
CAMBIOS.append((r'''writeLines("  S2b: ninguna regla de un paso equivale a resolver el ítem. Los dos productos de fila juntos (n1 * a1 y n2 * a2) son los dos totales, y compararlos es resolverlo; cada uno por separado es «un solo total» y se mide.")
''', r'''writeLines("  S2b: ninguna regla de un paso equivale a resolver el ítem. Los dos productos de fila juntos (n1 * a1 y n2 * a2) son los dos totales, y compararlos es resolverlo; cada uno por separado es «un solo total» y se mide.")
## ---- S2d: la lectura (iii), en «resta» y en el barrido (L-7 candidata, ampliada) ----------------------------------
## La S2c la midió en su copia del instrumento (s2c/bloque_s2c.R) y halló 5 cifras por encima de +8 pp que la S2b no
## medía; el profesor las firmó en la S2d. Es el canal inverso de (i) (Incidente F, punto 4). Por construcción, cuando
## aplica, la cifra es la de padv (en la canónica, la de la C), así que señala como perdedora a la misma sede que S_U.
## Se mide en «resta» y en las 28 operaciones del barrido, en los cuatro estratos; «|a1 - a2| (iii)» es «resta (iii)»,
## se coteja con ella y se juzga una sola vez. No se miden sus combinaciones con S_U. Nada de esta sección consume el RNG.
TAB_RESTA_III <- mide(setNames(list(function(x) lee_iii(x, delta_t(x))), R_RESTA_III))
BARRIDO_III <- list()
for (nm_op in names(OPS_EST)) BARRIDO_III[[paste(nm_op, "(iii)")]] <- regla_de(OPS_EST[[nm_op]], "iii")
TAB_EST3 <- mide(BARRIDO_III)
writeLines("  S2d · lectura (iii), por fila de la tabla (acierto en %, mezcla / CURSOS / PROMEDIO / canónica):")
writeLines(do.call(sprintf, c(list("    %-31s %5.1f %5.1f %5.1f %5.1f", "resta (iii)"), as.list(round(100 * TAB_RESTA_III[R_RESTA_III, ], 1)))))
for (nm_op in names(OPS_EST))
  writeLines(do.call(sprintf, c(list("    %-31s %5.1f %5.1f %5.1f %5.1f", paste(nm_op, "(iii)")), as.list(round(100 * TAB_EST3[paste(nm_op, "(iii)"), ], 1)))))
ok(isTRUE(all.equal(TAB_EST3["|a1 - a2| (iii)", ], TAB_RESTA_III[R_RESTA_III, ])),
   "S2d: el barrido reproduce «resta (iii)» con sus propias celdas: «|a1 - a2| (iii)» es la misma regla, y se juzga una sola vez")
np_r3 <- juzga_s2b(TAB_RESTA_III, "S2d lectura (iii)")
np_e3 <- juzga_s2b(TAB_EST3[rownames(TAB_EST3) != "|a1 - a2| (iii)", , drop = FALSE], "S2d barrido (iii)")
writeLines(sprintf("  S2d lectura (iii): «resta» y %d operaciones × 4 estratos; %d cifras por encima de +8 pp sobre el azar (sin «|a1 - a2| (iii)», que es «resta (iii)»): %d cubiertas por la firma de la S2d y %d pendientes",
                   length(OPS_EST) - 1L, sum(np_r3) + sum(np_e3), np_r3[["cubierto"]] + np_e3[["cubierto"]], np_r3[["pendiente"]] + np_e3[["pendiente"]]))
## Cotejo de la firma: cada una de las 41 filas existe en E4 y se compara con lo firmado, a un decimal. Una cifra que baja
## sigue cubierta, pero se declara aquí: lo firmado son las cifras de este diseño (md5 63e2713e).
TAB_S2D <- rbind(TAB_H1, TAB_EST, TAB_RESTA_III, TAB_EST3)
v_s2d <- mapply(function(rg, es) if (rg %in% rownames(TAB_S2D)) TAB_S2D[rg, es] else NA_real_, FIRMA_S2D$regla, FIRMA_S2D$estrato)
ok(!anyNA(v_s2d), sprintf("firma de la S2d: las %d cifras firmadas existen en E4 (regla y estrato)", nrow(FIRMA_S2D)))
ig_s2d <- !is.na(v_s2d) & abs(round(100 * v_s2d, 1) - FIRMA_S2D$acierto) < 1e-9
writeLines(sprintf("  firma de la S2d: %d cifras firmadas (36 de la S2b y 5 de la lectura (iii)), %d iguales a lo firmado (a un decimal)%s",
                   nrow(FIRMA_S2D), sum(ig_s2d), if (all(ig_s2d)) "" else paste0(" — distintas: ", paste(sprintf("%s %s %.1f %% (firmado %.1f %%)",
                   FIRMA_S2D$id, FIRMA_S2D$estrato, 100 * v_s2d, FIRMA_S2D$acierto)[!ig_s2d], collapse = "; "))))
'''))

# 6. La declaración del estímulo dice qué mide E4 desde la S2d.
CAMBIOS.append((r'''  "|aG - aP| («resta», S2b), sola y con S_U y las pistas firmadas en «O-1 corregido»; y las operaciones de un paso entre",
  "dos celdas (suma, resta, producto y cociente entero), con las lecturas (i) y (ii), más las cuatro de dos pasos que usa",
  "el ítem (barrido, S2b). No mide otras operaciones de dos o más pasos ni el texto del enunciado fuera de los nombres",
  "de las sedes. En la canónica, regla por regla (decisión 2 a y S2b)")
''', r'''  "|aG - aP| («resta», S2b), en sus tres lecturas, y en la (i) también con S_U y las pistas firmadas en «O-1 corregido»;",
  "y las operaciones de un paso entre dos celdas (suma, resta, producto y cociente entero), con las lecturas (i), (ii) y",
  "(iii), más las cuatro de dos pasos que usa el ítem (barrido, S2b y S2d). No mide otras operaciones de dos o más pasos,",
  "las combinaciones del barrido o de la lectura (iii) con S_U, ni el texto del enunciado fuera de los nombres de las",
  "sedes. En la canónica, regla por regla (decisión 2 a, S2b y S2d)")
'''))

# 7. M44: tiene que sacar «resta» de «cubierto».
CAMBIOS.append((r'''## M44 (S2b, H1 de la FASE 2C r6): con ERR-RESTA como distractor de G siempre que se pueda, la cifra |aG - aP| sale más a
## menudo, y «resta» tiene que subir en las dos ramas y seguir pendiente de firma (visible). Mismo método que M19.
''', r'''## M44 (S2b, H1 de la FASE 2C r6): con ERR-RESTA como distractor de G siempre que se pueda, la cifra |aG - aP| sale más a
## menudo, y «resta» tiene que subir en las dos ramas. Desde la S2d, además, tiene que salir de «cubierto» (RS1 de la firma
## de la S2d) por la misma puerta que la firma, veredicto_s2d(), en alguna rama. Mismo método que M19.
'''))
CAMBIOS.append((r'''  ver44 <- if (!all(lengths(C44) >= 20L) || !all(q44 > q0)) "mal_construido" else if (!all(t44 > t0)) "no_detectado" else
    if (!all(t44 - 0.25 > 0.08 + 1e-9)) "no_detectado" else "cazado_por_su_sonda"
  ok(ver44 == "cazado_por_su_sonda",
     sprintf("M44 (ERR-RESTA siempre que se pueda; su cuota en el distractor de G pasa de %.0f a %.0f %% en CURSOS y de %.0f a %.0f %% en PROMEDIO): «resta» sube de %.1f a %.1f %% en CURSOS y de %.1f a %.1f %% en PROMEDIO, y sigue pendiente de firma — %s",
             100 * q0[["CURSOS"]], 100 * q44[["CURSOS"]], 100 * q0[["PROMEDIO"]], 100 * q44[["PROMEDIO"]],
             100 * t0[["CURSOS"]], 100 * t44[["CURSOS"]], 100 * t0[["PROMEDIO"]], 100 * t44[["PROMEDIO"]], ver44))
''', r'''  v44 <- vapply(c("CURSOS", "PROMEDIO"), function(b) veredicto_s2d(R_RESTA, b, t44[[b]]), "")
  ver44 <- if (!all(lengths(C44) >= 20L) || !all(q44 > q0)) "mal_construido" else if (!all(t44 > t0)) "no_detectado" else
    if (!any(v44 == "pendiente")) "no_detectado" else "cazado_por_su_sonda"
  ok(ver44 == "cazado_por_su_sonda",
     sprintf("M44 (ERR-RESTA siempre que se pueda; su cuota en el distractor de G pasa de %.0f a %.0f %% en CURSOS y de %.0f a %.0f %% en PROMEDIO): «resta» sube de %.1f a %.1f %% en CURSOS y de %.1f a %.1f %% en PROMEDIO; frente a la firma de la S2d (RS1: %.1f y %.1f %%) queda «%s» en CURSOS y «%s» en PROMEDIO — %s",
             100 * q0[["CURSOS"]], 100 * q44[["CURSOS"]], 100 * q0[["PROMEDIO"]], 100 * q44[["PROMEDIO"]],
             100 * t0[["CURSOS"]], 100 * t44[["CURSOS"]], 100 * t0[["PROMEDIO"]], 100 * t44[["PROMEDIO"]],
             FIRMA_S2D$acierto[FIRMA_S2D$id == "RS1" & FIRMA_S2D$estrato == "CURSOS"],
             FIRMA_S2D$acierto[FIRMA_S2D$id == "RS1" & FIRMA_S2D$estrato == "PROMEDIO"], v44[["CURSOS"]], v44[["PROMEDIO"]], ver44))
'''))

# 8. M45, nuevo: la lectura (iii) de las de dos pasos sale de «cubierto».
CAMBIOS.append((r'''## Mutante de medición M20 (H3 de la FASE 2C r4): una celda de la canónica cambia DESPUÉS de tabla_df.
''', r'''## M45 (S2d, lectura (iii)): con la fila de G siempre abajo (gi = 2: la posición de la fila de G deja de sortearse, P4),
## la fila 1 es siempre la de P, así que n1 * |a1 - a2| (CURSOS: nP · δ) y |n1 - n2| * a1 (PROMEDIO: k · aP) son siempre la
## cifra de padv (parcialP), y su lectura (iii) tiene que subir y salir de «cubierto» de la firma de la S2d (BE9, BE11) por
## la misma puerta, veredicto_s2d(). Mismo método que M19 y M44; un render con error lo clasifica «cazado_por_otra».
mm <- mutar(list(c("gi <- pick(1:2)", "gi <- 2L")))
if (!is.null(mm$error)) falla(paste("M45 MAL CONSTRUIDO:", mm$error)) else {
  C45 <- list(CURSOS = list(), PROMEDIO = list()); err45 <- 0L
  for (s in 1000L + seq_len(TOPE)) {
    if (all(lengths(C45) >= N)) break
    r <- correr(s, mm$expr); if (!is.null(r$err)) { err45 <- err45 + 1L; next }
    if (isTRUE(r$env$es_canonica)) next
    x <- rasgos(r$env); if (length(C45[[x$rama]]) < N) C45[[x$rama]][[length(C45[[x$rama]]) + 1]] <- x
  }
  F45 <- FIRMA_S2D[FIRMA_S2D$id %in% c("BE9", "BE10", "BE11", "BE12"), ]
  t45 <- mapply(function(rg, es) if (length(C45[[es]])) mean(vapply(C45[[es]], BARRIDO_III[[rg]], 0)) else NA_real_, F45$regla, F45$estrato)
  v45 <- mapply(veredicto_s2d, F45$regla, F45$estrato, t45)
  llego45 <- all(lengths(C45) >= 20L) && all(vapply(c(C45$CURSOS, C45$PROMEDIO), function(x) identical(as.integer(x$gi), 2L), TRUE)) &&
    any(vapply(c(CU$CURSOS, CU$PROMEDIO), function(x) identical(as.integer(x$gi), 1L), TRUE))
  ver45 <- if (!llego45) "mal_construido" else if (err45 > 0L) "cazado_por_otra" else if (any(v45 == "pendiente")) "cazado_por_su_sonda" else "no_detectado"
  ok(ver45 == "cazado_por_su_sonda",
     sprintf("M45 (la fila de G siempre abajo, gi = 2), lectura (iii) de las de dos pasos frente a la firma de la S2d: %s; %d renders con error — %s",
             paste(sprintf("%s %s %.1f -> %.1f %% «%s»", F45$id, F45$estrato, F45$acierto, 100 * t45, v45), collapse = "; "), err45, ver45))
}
## Mutante de medición M20 (H3 de la FASE 2C r4): una celda de la canónica cambia DESPUÉS de tabla_df.
'''))

# 9. M28: con la firma, este proceso ya no tiene pendientes.
CAMBIOS.append((r'''  ## M28 juzga los pendientes NUEVOS del proceso sin normalizar: los que este proceso no tiene.
''', r'''  ## M28 juzga los pendientes NUEVOS del proceso sin normalizar: los que este proceso no tiene.
  ## S2d: con la firma de la S2d este proceso ya no tiene pendientes; el filtro se conserva y no quita nada.
'''))

# 10. El resumen y la línea de resultado.
CAMBIOS.append((r'''if (length(FIRMADOS)) cat("\nEn rojo, cubiertos por un override firmado («O-1 corregido», 2026-10-29; decisión 2 a, 2026-10-31; regla #22 §P7-E):\n",
''', r'''if (length(FIRMADOS)) cat("\nEn rojo, cubiertos por un override firmado («O-1 corregido», 2026-10-29; decisión 2 a, 2026-10-31; firma de la S2d, 2026-11-03; regla #22 §P7-E):\n",
'''))
CAMBIOS.append((r'''  if (length(FIRMADOS)) sprintf("EN ROJO BAJO OVERRIDE FIRMADO (%d canales: %d dentro del alcance de O-1 corregido y %d de la decisión 2 a; 0 fallos, 0 pendientes)",
                                 length(FIRMADOS), sum(POR == "O-1 corregido"), sum(POR == "la decisión 2 a")) else
''', r'''  if (length(FIRMADOS)) sprintf("EN ROJO BAJO OVERRIDE FIRMADO (%d canales: %d dentro del alcance de O-1 corregido, %d de la decisión 2 a y %d de la firma de la S2d; 0 fallos, 0 pendientes)",
                                 length(FIRMADOS), sum(POR == "O-1 corregido"), sum(POR == "la decisión 2 a"), sum(POR == POR_S2D)) else
'''))

for viejo, nuevo in CAMBIOS:
    n = txt.count(viejo)
    if n != 1:
        sys.exit(f"patrón con {n} apariciones (se exige 1): {viejo[:90]!r}")
    if any(c in nuevo for c in PROHIBIDOS):
        sys.exit(f"el texto nuevo lleva un separador prohibido: {nuevo[:90]!r}")
    txt = txt.replace(viejo, nuevo, 1)

shutil.copy2(VER, S2D / "verificar_espacio_9c654748_antes.R.txt")
VER.write_text(txt, encoding="utf-8")
print(f"{len(CAMBIOS)} cambios aplicados; verificar_espacio.R {MD5_ANTES} -> {md5(VER)}")
