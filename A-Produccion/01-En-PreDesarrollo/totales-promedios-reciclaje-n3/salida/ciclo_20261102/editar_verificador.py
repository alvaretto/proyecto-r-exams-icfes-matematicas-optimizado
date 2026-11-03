#!/usr/bin/env python3
"""Ciclo 2026-11-02, tanda S1: ediciones de verificar_espacio.R (S1, S3, S4, S7 y mutantes M23-M27)."""
import sys
ruta = sys.argv[1]
src = open(ruta, encoding="utf-8").read()
src = src.replace('gsub("\u00a0", " ", gsub("<span', 'gsub("\\u00a0", " ", gsub("<span', 1)   # literal previo -> escape

def rep(old, new, n=1):
    global src
    c = src.count(old)
    if c != n:
        sys.exit(f"ABORTA: «{old[:80]}» aparece {c} veces (se esperaban {n})")
    src = src.replace(old, new)

U = "\\u00a0"   # escape de R: el archivo no lleva el carácter literal

# --- cabecera ------------------------------------------------------------------------------------
rep("## Ronda 4b (FASE 2C r4b, H1): guarda estructural de los encabezados de la Solution (mutantes M21, M22).\n",
    "## Ronda 4b (FASE 2C r4b, H1): guarda estructural de los encabezados de la Solution (mutantes M21, M22).\n"
    "## Ciclo 2026-11-02, tanda S1: espacio duro (S1) en el oráculo léxico, en M16 y en la Solution (sonda S1-Sol,\n"
    "##        mutantes M23-M27); MOLDES sin el parámetro m (S7); la canónica regla por regla con las 24 cifras de\n"
    "##        la decisión 2 a y sus mutantes MP1 y MP2 (S3); el estímulo de la batería, declarado (§P7-G, S4).\n")
rep("bajo override firmado (todo dentro del alcance de «O-1 corregido»)",
    "bajo override firmado (todo dentro del alcance de «O-1 corregido» y de la decisión 2 a)")

# --- S1: oráculo léxico de E3 --------------------------------------------------------------------
rep('"juntaron|con|de|es|son|hubo) [0-9]+ kilos")',
    f'"juntaron|con|de|es|son|hubo) [0-9]+[ {U}]kilos")   # S1: espacio normal o duro')

# --- S7: MOLDES sin el parámetro m ---------------------------------------------------------------
rep('t <- env0$MOLDES[[cc$m]]$f(S, O, cc$x, "papel")', 't <- env0$MOLDES[[cc$m]]$f(S, O, cc$x)')
rep('grepl("estudiantes", e$MOLDES$U_c$f("S", "O", 1, "papel"))', 'grepl("estudiantes", e$MOLDES$U_c$f("S", "O", 1))')

# --- S1: M16 con el cierre nuevo -----------------------------------------------------------------
rep("""       cambios = list(c('cierre <- function(x, O) paste0("su total fue más alto que el de ", O, " en ", x, " kilos.")',
                        'cierre <- function(x, O) paste0("su total fue de ", x, " kilos más que el de ", O, ".")')),
       llego = function(e) !isTRUE(e$es_canonica) && grepl("fue de [0-9]+ kilos más que", e$afs$clave$texto)),""",
    f"""       cambios = list(c('cierre <- function(x, O) paste0("su total fue más alto que el de ", O, " en ", x, ED, "kilos.")',
                        'cierre <- function(x, O) paste0("su total fue de ", x, ED, "kilos más que el de ", O, ".")')),
       llego = function(e) !isTRUE(e$es_canonica) && grepl("fue de [0-9]+[ {U}]kilos más que", e$afs$clave$texto)),""")

# --- S1: mutantes M23-M26 del chunk data_generation ----------------------------------------------
rep("""       cambios = list(c("reciclaron durante más días que los demás", "reciclaron durante 5 días más que los demás")),
       llego = function(e) identical(e$afs$psin$molde, "U_d")))
""",
    f"""       cambios = list(c("reciclaron durante más días que los demás", "reciclaron durante 5 días más que los demás")),
       llego = function(e) identical(e$afs$psin$molde, "U_d")),
  ## Ciclo 2026-11-02 (S1): el espacio duro. M23 lo pone en la canónica fuera de su sitio (la guarda verbatim lo
  ## compara sin él y no lo ve); M24 lo quita de la canónica; M25, de «El Bosque»; M26, del cierre de las no
  ## canónicas, que vigila la guarda del complemento.
  list(id = "M23", sonda = "S1: en la canónica, el espacio duro solo entre cifra y unidad",
       desc = "la canónica lleva un espacio duro fuera de su sitio («que la sede»)",
       cambios = list(c('"kilos más que la sede de secundaria."', '"kilos más que la", ED, "sede de secundaria."')),
       llego = function(e) grepl("que la{U}sede de secundaria", e$CANON_OPCIONES[["A"]], fixed = TRUE)),
  list(id = "M24", sonda = "S1: espacio duro entre cifra y unidad",
       desc = "la canónica pierde el espacio duro de «40 kilos»",
       cambios = list(c('porque recogió 40", ED, "kilos más', 'porque recogió 40 kilos más')),
       llego = function(e) grepl("recogió 40 kilos", e$CANON_OPCIONES[["A"]], fixed = TRUE)),
  list(id = "M25", sonda = "S1: espacio duro entre cifra y unidad",
       desc = "«El Bosque» vuelve al espacio normal en los nombres",
       cambios = list(c('paste0("la sede El", ED, "Bosque")', '"la sede El Bosque"')),
       llego = function(e) identical(e$CONJUNTOS[[5]]$nombres[1], "la sede El Bosque")),
  list(id = "M26", sonda = "regla #13 v1.1: la cifra va solo en el complemento",
       desc = "el cierre de las no canónicas pierde el espacio duro",
       cambios = list(c('" en ", x, ED, "kilos.")', '" en ", x, " kilos.")')),
       llego = function(e) !isTRUE(e$es_canonica) && grepl(" en [0-9]+ kilos\\\\.$", e$afs$clave$texto)))
""")

# --- S4 (§P7-G): el estímulo de la batería, declarado --------------------------------------------
rep('NA_FAM <- c(signo = "todas las cifras son positivas y ninguna opción niega ni compara con desigualdades")\n',
    'NA_FAM <- c(signo = "todas las cifras son positivas y ninguna opción niega ni compara con desigualdades")\n'
    '## Regla #22 §P7-G (S4, ciclo 2026-11-02): la batería solo recibe las opciones. Los canales del estímulo (el\n'
    '## enunciado y la tabla) se miden aparte, en las sondas de E4, que sí ven la tabla: S1 y S1b leen sus cifras;\n'
    '## S2 y S3, sus filas; S5, S7 y S_U, las sedes que muestra, y sus combinaciones firmadas en «O-1 corregido»;\n'
    '## y en la canónica, regla por regla (decisión 2 a). Declararlo no mueve ninguna cifra.\n'
    'ESTIMULO_SC <- paste("los canales del enunciado y de la tabla se miden aparte, en las sondas de E4 (S1 y S1b leen",\n'
    '                     "las cifras de la tabla; S2 y S3, sus filas; S5, S7 y S_U, las sedes que muestra, y sus",\n'
    '                     "combinaciones) y, en la canónica, regla por regla (decisión 2 a)")\n')
rep("""  res <- evaluar_bateria(REGLAS, lapply(V, `[[`, "opciones"), vapply(V, `[[`, 0L, "clave"),
                         familias_no_aplicables = NA_FAM)""",
    """  res <- evaluar_bateria(REGLAS, lapply(V, `[[`, "opciones"), vapply(V, `[[`, 0L, "clave"),
                         familias_no_aplicables = NA_FAM, estimulo_no_aplicable = ESTIMULO_SC)""")
rep("""                            vapply(V, `[[`, 0L, "clave"), familias_no_aplicables = NA_FAM)""",
    """                            vapply(V, `[[`, 0L, "clave"), familias_no_aplicables = NA_FAM,
                            estimulo_no_aplicable = ESTIMULO_SC)""")

# --- firmado() con su override -------------------------------------------------------------------
rep('firmado <- function(msg) { FIRMADOS <<- c(FIRMADOS, msg); cat("  ROJO, cubierto por O-1 corregido:", msg, "\\n") }',
    'POR <- character(0)   # qué override cubre cada canal en rojo\n'
    'firmado <- function(msg, por = "O-1 corregido") { FIRMADOS <<- c(FIRMADOS, msg); POR <<- c(POR, por)\n'
    '  cat("  ROJO, cubierto por ", por, ": ", msg, "\\n", sep = "") }')

# --- S3: la canónica regla por regla (decisión 2 a) ----------------------------------------------
rep("""  cat("      top:", paste(sprintf("%s %.1f%%", vapply(REGLAS[ord], `[[`, "", "nombre"), 100 * sc_reg[ord]), collapse = " · "), "\\n")
}
""",
    """  cat("      top:", paste(sprintf("%s %.1f%%", vapply(REGLAS[ord], `[[`, "", "nombre"), 100 * sc_reg[ord]), collapse = " · "), "\\n")
}
## ---- Decisión 2 a (S3, ciclo 2026-11-02): la canónica, regla por regla -----------------------------
## Decisión del profesor del 2026-10-31 (tanda 3 del CLOZE, .claude/CLAUDE.md local): override de la canónica
## REGLA POR REGLA, por dominancia (en la canónica, el atajo firmado de «O-1 corregido» acierta el 100 %).
## Cifras EXACTAS de los 24 órdenes de las opciones del impreso (cloze/salida/p1_canonica_exacta_96cfcf35.txt;
## por E1 del CLOZE son las de este SCHOICE): 12 reglas de la batería §P7 y 12 sondas de E4, en pp sobre el
## 25 % del azar, comparadas redondeadas a un decimal, como se firmaron. Son FIRMA_P1_CANON y FIRMA_P1_SONDAS
## de cloze/verificar_espacio_cloze.R (E4). Dentro: «ROJO, cubierto» (exit 4), nunca PASS. Por encima, o con
## cualquier otra regla o sonda > +8 pp en la canónica: PENDIENTE (exit 3). Sustituye a la firma por el máximo
## (0,10 × 75 pp) de «O-1 corregido» (aclaración ii de §P7-E). Las nueve que están al 100 % (+75,0 pp) no
## pueden subir: no admiten mutante, y se declara. MP1 y MP2 (E5) sacan de «cubierto» dos de las que están al 50 %.
FIRMA_P1_CANON <- c("la que MAS comparte" = 75.0, "quedarse con 'total'" = 75.0, "cifra mediana" = 75.0, "mas digitos" = 25.0,
                    "primer numero PAR" = 25.0, "ultimo multiplo de 10" = 25.0, "quedarse con 'promedio'" = 25.0,
                    "par con más palabras en común (relacional)" = 25.0, "descartar ultimo negativo" = 8.3,
                    "par por sede: menor cifra" = 8.3, "descartar sin cifra" = 8.3, "descartar 'de más'" = 8.3)
FIRMA_P1_SONDAS <- c("S1 descarta cifras de la tabla" = 25.0, "S2 par de la sede de la fila 1" = 25.0,
                     "S3 par de la sede de más cursos" = 25.0, "S5 más cursos + menor cifra del par" = 75.0,
                     "S7 G real + menor cifra del par (cota canal 2)" = 75.0, "S_U la sede de la razón sin sustento pierde" = 25.0,
                     "S_U + S1 (par de G sin la cifra de la tabla)" = 75.0, "S_U + suma (par de G sin la cifra que es suma de otras dos)" = 25.0,
                     "Atípica: descarta la cifra que dobla a todas las demás" = 8.3, "S_U + S1 + suma (las dos pistas)" = 75.0,
                     "S_U + atípica" = 75.0, "S_U + S1 + suma + atípica" = 75.0)
perms <- function(v) if (length(v) <= 1) list(v) else do.call(c, lapply(seq_along(v), function(i) lapply(perms(v[-i]), function(p) c(v[i], p))))
canon_v <- function(CA) lapply(perms(1:4), function(p) { x <- CA; x$opciones <- CA$opciones[p]; x$sede_o <- CA$sede_o[p]
  x$rol <- CA$rol[p]; x$nom_o <- CA$nom_o[p]; x$clave <- which(p == CA$clave); x })
canon_p1 <- function(CV) {
  r <- vapply(REGLAS, function(rg) mean(vapply(CV, function(x) {
    S <- tryCatch(.conjunto_de(rg$fn(x$opciones), 4L), error = function(e) rep(FALSE, 4)); sc(S, x$clave) }, 0)), 0)
  names(r) <- vapply(REGLAS, function(rg) rg$nombre, "")
  s <- as.matrix(sondas(CV))[, 1]; s <- s[!grepl("frecuencia", names(s))]
  list(reglas = 100 * (r - 0.25), sondas = 100 * (s - 0.25))
}
veredicto_p1c <- function(exc, firma) vapply(names(exc), function(nm) { e <- exc[[nm]]
  if (e <= 8 + 1e-9) "ok" else if (nm %in% names(firma) && round(e, 1) <= firma[[nm]]) "cubierto" else "pendiente" }, "")
if (is.null(CANON)) falla("decisión 2 a: no se encontró la canónica; no se puede juzgar") else {
  C1 <- canon_p1(canon_v(CANON))
  ok(all(names(FIRMA_P1_CANON) %in% names(C1$reglas)) && all(names(FIRMA_P1_SONDAS) %in% names(C1$sondas)),
     "decisión 2 a: las 12 reglas y las 12 sondas firmadas existen en la batería y en E4")
  for (cl in list(list("regla", C1$reglas, FIRMA_P1_CANON), list("sonda", C1$sondas, FIRMA_P1_SONDAS))) {
    vv <- veredicto_p1c(cl[[2]], cl[[3]])
    for (nm in names(vv)[vv != "ok"]) {
      m <- sprintf("canónica, %s «%s»: %+.1f pp exactos (24 órdenes); peso en la mezcla %+.2f pp", cl[[1]], nm, cl[[2]][[nm]], cl[[2]][[nm]] / 10)
      if (vv[[nm]] == "cubierto") firmado(paste0(m, sprintf(" (firmado hasta %+.1f pp)", cl[[3]][[nm]])), por = "la decisión 2 a")
      else pendiente(paste0(m, if (nm %in% names(cl[[3]])) sprintf(", por encima de lo firmado (%+.1f pp)", cl[[3]][[nm]]) else
                                 ", fuera de lo firmado en la decisión 2 a"))
    }
    igual <- vapply(names(cl[[3]]), function(nm) isTRUE(round(cl[[2]][[nm]], 1) == cl[[3]][[nm]]), TRUE)
    cat(sprintf("  decisión 2 a, %ss: %d firmadas, %d iguales a lo firmado (a un decimal)%s; por encima de +8 pp en la canónica: %d\\n",
                cl[[1]], length(cl[[3]]), sum(igual), if (all(igual)) "" else paste0(" — distintas: ", paste(names(igual)[!igual], collapse = ", ")),
                sum(vv != "ok")))
  }
  nl_can <- mean(vapply(canon_v(CANON), function(x) { n <- nchar(x$opciones); sc(n != max(n), x$clave) }, 0))
  cat(sprintf("  RESIDUO DECLARADO (decisión 2 a: fuera de la batería congelada, §P7-C): en la canónica, «descarta la más larga» %.1f %% (%+.1f pp)\\n",
              100 * nl_can, 100 * (nl_can - 0.25)))
}
""")

# --- S3: mutantes MP1 y MP2; S1: sonda de la Solution y M27 ---------------------------------------
rep("""if (length(FIRMADOS)) cat("\\nEn rojo, cubiertos por el override firmado «O-1 corregido» (2026-10-29; regla #22 §P7-E):\\n", paste0("  - ", FIRMADOS, "\\n"), sep = "")""",
    f"""## Mutantes de la decisión 2 a (S3): COPIAS del registro canónico con una opción retocada (la canónica es verbatim
## y su guarda de literales aborta cualquier mutante en el código). Cada uno tiene que sacar SU regla de «cubierto»,
## con la misma puerta (canon_p1 + veredicto_p1c, 24 órdenes). Son MP1 y MP2 de cloze/verificar_espacio_cloze.R (E8);
## MP2 lleva el espacio duro de S1 en su patrón.
mut_p1 <- function(id, regla, de, a) {{
  i <- grep(de, CANON$opciones, fixed = TRUE)
  if (length(i) != 1L) return(invisible(falla(sprintf("%s MAL CONSTRUIDO: «%s» aparece en %d opciones de la canónica", id, de, length(i)))))
  CM <- CANON; CM$opciones[i] <- sub(de, a, CM$opciones[i], fixed = TRUE)
  ex <- canon_p1(canon_v(CM))$reglas; vv <- veredicto_p1c(ex, FIRMA_P1_CANON)
  llego <- !identical(CM$opciones, CANON$opciones) && identical(CM$clave, CANON$clave)
  ver <- if (!llego) "mal_construido" else if (vv[[regla]] == "pendiente") "cazado_por_su_sonda" else "no_detectado"
  ok(ver == "cazado_por_su_sonda", sprintf("%s (decisión 2 a; copia de la canónica, «%s» -> «%s»): %s %+.1f pp (firmado %+.1f) -> %s — %s",
                                           id, de, a, regla, ex[[regla]], FIRMA_P1_CANON[[regla]], vv[[regla]], ver))
}}
if (!is.null(CANON)) {{
  mut_p1("MP1", "quedarse con 'promedio'", "tuvo mayor promedio", "tuvo mayor media")
  mut_p1("MP2", "mas digitos", "recogió 40{U}kilos más", "recogió 4{U}kilos más")
  cat("  Decisión 2 a: las nueve reglas y sondas al 100 % (+75,0 pp) no pueden subir; ningún mutante puede sacarlas de «cubierto».\\n")
}}
## S1 (ciclo 2026-11-02), sonda S1-Sol: en las opciones, la Solution y el feedback por opción que se emiten, ninguna cifra
## (ni fórmula que cierra en $) separada de su unidad por un espacio normal, ni «El Bosque» o «La Pradera». La guarda
## de data_generation no ve la Solution, que se arma en su propio chunk. M27 quita un espacio duro de la Solution.
chunk_de <- function(lin, nm) {{ i0 <- grep(paste0("^```\\\\{{r ", nm, "[,}}]"), lin); i1 <- which(lin == "```"); i1 <- i1[i1 > i0][1]
  parse(text = paste(lin[(i0 + 1):(i1 - 1)], collapse = "\\n"), encoding = "UTF-8") }}
PAT_S1 <- "[0-9$] (kilos?|cursos?)\\\\b|El Bosque|La Pradera"
s1_sol <- function(lin, semillas) {{
  ch <- lapply(c("opciones", "solucion", "feedback"), function(nm) chunk_de(lin, nm))
  sum(vapply(semillas, function(s) {{ r <- correr(s); if (!is.null(r$err)) return(NA_real_)
    t <- paste(unlist(lapply(ch, function(e) capture.output(eval(e, envir = r$env)))), collapse = "\\n")
    lengths(regmatches(t, gregexpr(PAT_S1, t, perl = TRUE))) }}, 0))
}}
n_s1 <- s1_sol(l, 1:N)
ok(identical(n_s1, 0), sprintf("S1-Sol: en las %d versiones de E1, opciones, Solution y feedback sin cifra ni nombre partido por un espacio normal (%s)", N, n_s1))
l_m27 <- sub('d$D,\\n    "$", ED, "kilos a favor de "', 'd$D,\\n    "$ kilos a favor de "', paste(l, collapse = "\\n"), fixed = TRUE)
l_m27 <- strsplit(l_m27, "\\n", fixed = TRUE)[[1]]
n_m27 <- if (identical(l_m27, l)) NA else s1_sol(l_m27, 1:N)
ok(!is.na(n_m27) && n_m27 > 0, sprintf("M27 (la Solution pierde el espacio duro de «kilos a favor de»): la sonda S1-Sol lo detecta en %s casos", n_m27))
if (length(FIRMADOS)) cat("\\nEn rojo, cubiertos por un override firmado («O-1 corregido», 2026-10-29; decisión 2 a, 2026-10-31; regla #22 §P7-E):\\n",
                          paste0("  - [", POR, "] ", FIRMADOS, "\\n"), sep = "")""")

rep("""  if (length(FIRMADOS)) sprintf("EN ROJO BAJO OVERRIDE FIRMADO (%d canales dentro del alcance de O-1 corregido; 0 fallos, 0 pendientes)", length(FIRMADOS)) else""",
    """  if (length(FIRMADOS)) sprintf("EN ROJO BAJO OVERRIDE FIRMADO (%d canales: %d dentro del alcance de O-1 corregido y %d de la decisión 2 a; 0 fallos, 0 pendientes)",
                                 length(FIRMADOS), sum(POR == "O-1 corregido"), sum(POR == "la decisión 2 a")) else""")
if "\u00a0" in src:
    sys.exit("ABORTA: U+00A0 literal en el verificador")
open(ruta, "w", encoding="utf-8").write(src)
print("escrito", ruta)
