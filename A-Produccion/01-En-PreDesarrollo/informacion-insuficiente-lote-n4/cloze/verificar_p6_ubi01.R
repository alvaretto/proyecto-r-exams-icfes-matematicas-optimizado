# Verificador de la reapertura del 2026-10-19 (tres pasadas de detractor
# independiente: RECHAZAR, RECHAZAR, APROBAR_CON_CAMBIOS):
#   1) Parte 6: el estudiante lee el enunciado HIPOTÉTICO con el que se decide la
#      clave. Oráculo literal en DOS direcciones (O-2 de la pasada 2): los datos
#      que el texto citado trae son exactamente los declarados en k_p6, y con los
#      datos deducidos del TEXTO la clave no tiene respuesta y los tres
#      distractores sí. Incluye las versiones canónicas.
#   2) Parte 6: no imprime la respuesta de la Parte 2 ni de la 3 (O-1, G12).
#   3) Opción (a): UBI-01 fuera de las ramas -TAN y LTA- (filtro + G8).
#   4) Parte 5: canal de longitud; tasa bruta, parte EXPLOTABLE (margen >= 15 %,
#      §P7-B) y frecuencia del caso sin restricción.
#   5) Ninguna rama se queda sin material: 0 fallos de generación.
# Mutantes (en memoria, el .Rmd no se toca), cada uno debe MORIR:
#   M1 motivo del banco real en la Parte 6 -> G10; M2 sin filtro de UBI-01 -> G8;
#   M3 Parte 6 con el enunciado original -> G11; M4 el texto hipotético pierde el
#   trazado aunque siga declarado -> oráculo; M5 sin dejar L al final -> G12;
#   M6 el reintento de P2/P3 no excluye el área tras quitar L -> G12.
# Muestra por cuotas: N = 100 por rama (regla #23); las canónicas que aparezcan
# en el recorrido se miden aparte (estrato con n < 20 = NO CONCLUYENTE).
# Uso: Rscript verificar_p6_ubi01.R [rmd] [N_por_rama]
suppressMessages(library(exams))
args <- commandArgs(TRUE)
RMD <- if (length(args) >= 1) args[1] else
  "informacion_insuficiente_lote_geometrico_metrico_argumentacion_n4_cloze_v1.Rmd"
N <- if (length(args) >= 2) as.integer(args[2]) else 100L
TOPE <- 3000L
RAMAS <- c("-T-N", "-TAN", "LT--", "LTA-", "L--N")
U <- "GEO-INF-UBI-01"

L <- readLines(RMD, warn = FALSE)
i0 <- grep("^```\\{r data_generation", L)[1]
i1 <- which(grepl("^```\\s*$", L) & seq_along(L) > i0)[1]
fuente <- paste(L[(i0 + 1):(i1 - 1)], collapse = "\n")

mutar <- function(txt, de, a) {
  stopifnot("el patrón del mutante no aparece exactamente una vez" =
              lengths(regmatches(txt, gregexpr(de, txt, fixed = TRUE))) == 1L)
  sub(de, a, txt, fixed = TRUE)
}
evaluar <- function(expr, s) {
  set.seed(s); e <- new.env(parent = globalenv())
  err <- tryCatch({ suppressWarnings(suppressMessages(eval(expr, envir = e))); NA_character_ },
                  error = function(z) conditionMessage(z))
  list(e = e, err = err)
}
# Datos que un enunciado TRAE, deducidos del texto (no de las banderas).
# L se deduce también de «un lado suelto + el área» (con el área, un lado da el
# otro): es el modo de fallo de la reapertura, que solo veía G11 (FASE 2C de
# confirmación, O-C).
datos_texto <- function(txt, n_hijos, b_plur) c(
  L = grepl("metros de largo y", txt, fixed = TRUE) ||
      (grepl("[0-9] metros(?! cuadrados)", txt, perl = TRUE) &&
       grepl("metros cuadrados", txt, fixed = TRUE)),
  T = grepl("franjas iguales, todas paralelas", txt, fixed = TRUE),
  A = grepl("se asignan", txt, fixed = TRUE),
  N = grepl(paste0(n_hijos, " ", b_plur), txt, fixed = TRUE))
# Violaciones del oráculo en una versión ya evaluada.
oraculo <- function(e) {
  g <- function(x) get(x, envir = e)
  dep <- g("DEP"); det_bajo <- g("det_bajo")
  kt <- datos_texto(g("enunciado_p6"), g("n_hijos"), g("b_plur"))
  kd <- unlist(g("k_p6")[c("L", "T", "A", "N")])
  k_txt <- as.list(kt)
  cods <- g("cods"); idx <- g("idx_p6"); es_nd <- g("es_nd_p6")
  c(texto_vs_declarados = any(kt != kd),
    clave_con_respuesta = det_bajo(dep[[cods[idx[es_nd]]]], k_txt),
    distractor_sin_respuesta = !all(vapply(cods[idx[!es_nd]], function(cc)
      det_bajo(dep[[cc]], k_txt), logical(1))),
    original_vs_declarados = any(datos_texto(g("enunciado"), g("n_hijos"), g("b_plur")) !=
                                 unlist(g("K_ACTUAL")[c("L", "T", "A", "N")])))
}

# ---------------------------------------------------------------- versión real
expr <- parse(text = fuente)
cuenta <- setNames(integer(length(RAMAS)), RAMAS)
errores <- character(0); filas <- list(); s <- 0L
while (any(cuenta < N) && s < TOPE) {
  s <- s + 1L
  r <- evaluar(expr, s)
  if (!is.na(r$err)) { errores <- c(errores, sprintf("semilla %d: %s", s, r$err)); next }
  g <- function(x) get(x, envir = r$e)
  rama <- if (g("is_canonical")) "canonica" else g("info_txt")
  if (rama != "canonica") {
    if (cuenta[[rama]] >= N) next
    cuenta[[rama]] <- cuenta[[rama]] + 1L
  }
  cods <- g("cods")
  partes <- c(g("codigos_p1"), cods[g("par_23")], cods[g("idx_p4")],
              cods[g("idx_p5")], cods[g("idx_p6")])
  op5 <- g("opciones_p5"); so5 <- g("sol_p5"); nc <- nchar(op5); ic <- which(so5 == 1L)
  larga <- nc[ic] == max(nc) && sum(nc == max(nc)) == 1L
  margen <- (nc[ic] - max(nc[-ic])) / nc[ic]
  filas[[length(filas) + 1L]] <- data.frame(
    seed = s, rama = rama, ubi01 = U %in% partes,
    clave_p6 = g("clave_p6")$codigo, hecho_p6 = g("hecho_p6"),
    oraculo = any(oraculo(r$e)),
    fuga_p23 = isTRUE(g("p6_fuga_p23")), L_forzado = isTRUE(g("p6_L_forzado")),
    p5_larga = larga, p5_explotable = larga && margen >= 0.15,
    p5_sin_largo = isTRUE(g("p5_sin_largo")),
    stringsAsFactors = FALSE)
}
D <- do.call(rbind, filas)
n_can <- sum(D$rama == "canonica")
cat(sprintf("Versión real: %d semillas recorridas, %d errores de generación\n", s, length(errores)))
if (length(errores)) print(head(errores, 10))
cat("n por rama:\n"); print(c(cuenta, canonica = n_can))
cat("\nUBI-01 en alguna parte, por rama:\n"); print(tapply(D$ubi01, D$rama, sum))
cat("\nParte 6, violaciones del oráculo literal (dos direcciones), por rama:\n")
print(tapply(D$oraculo, D$rama, sum))
cat("\nParte 6 imprime la respuesta de P2/P3 | casos con L como único recurso, por rama:\n")
print(cbind(fuga = tapply(D$fuga_p23, D$rama, sum), L_forzado = tapply(D$L_forzado, D$rama, sum)))
cat("\nParte 6 por rama: dato quitado y cuota máxima de una misma clave (%):\n")
for (rr in sort(unique(D$rama))) {
  d <- D[D$rama == rr, ]
  tb <- table(d$hecho_p6); mx <- max(table(d$clave_p6))
  cat(sprintf("  %-9s n=%3d | quitado: %s | cuota máxima: %s (%.0f %%)\n", rr, nrow(d),
              paste(names(tb), tb, sep = "=", collapse = " "),
              names(which.max(table(d$clave_p6))), 100 * mx / nrow(d)))
}
cat("\nParte 5 por rama (%): clave = opción más larga | explotable (margen >= 15 %) | sin restricción\n")
print(round(100 * cbind(
  larga = tapply(D$p5_larga, D$rama, mean),
  explotable = tapply(D$p5_explotable, D$rama, mean),
  sin_restriccion = tapply(D$p5_sin_largo, D$rama, mean)), 1))

fallos <- character(0)
if (length(errores)) fallos <- c(fallos, "errores de generación en la versión real")
if (any(cuenta < 20L)) fallos <- c(fallos, "estrato con n < 20: NO CONCLUYENTE")
if (n_can < 20L) fallos <- c(fallos, sprintf("canónica con n = %d < 20: NO CONCLUYENTE", n_can))
if (any(D$ubi01[D$rama %in% c("-TAN", "LTA-")])) fallos <- c(fallos, "UBI-01 en -TAN o LTA-")
if (!any(D$ubi01)) fallos <- c(fallos, "UBI-01 no sale en ninguna rama (¿filtro excesivo?)")
if (any(D$oraculo)) fallos <- c(fallos, "Parte 6: violaciones del oráculo literal")
if (any(D$fuga_p23 & !D$L_forzado)) fallos <- c(fallos, "Parte 6 imprime la respuesta de P2/P3 sin ser el único recurso")

# -------------------------------------------------------------------- mutantes
correr_mutante <- function(nombre, txt, firma, max_s = 600L) {
  ex <- parse(text = txt); muertes <- 0L; otros <- 0L; s <- 0L
  while (s < max_s && muertes == 0L) {
    s <- s + 1L
    r <- evaluar(ex, s)
    if (!is.na(r$err)) {
      if (grepl(firma, r$err, fixed = TRUE)) muertes <- muertes + 1L else otros <- otros + 1L
    } else if (firma == "oráculo" && any(oraculo(r$e))) muertes <- muertes + 1L
  }
  cat(sprintf("%s: %s en la semilla %d (otros errores: %d)\n", nombre,
              if (muertes) paste("MUERE por", firma) else "SOBREVIVE", s, otros))
  muertes > 0L
}
cat("\n--- Mutantes ---\n")
mut <- list(
  list("M1 (motivo del banco real en la Parte 6)", "G10",
       "motivo_p6  <- q_p6(clave_p6$codigo)$motivo", "motivo_p6  <- clave_p6$motivo"),
  list("M2 (sin filtro de UBI-01)", "G8",
       "if (!is_canonical && info_T && info_A)\n  preguntas <- Filter(",
       "if (FALSE)\n  preguntas <- Filter("),
  list("M3 (Parte 6 con el enunciado original)", "G11",
       "enunciado_p6 <- construir_enunciado(k_p6$L, k_p6$T, k_p6$A, k_p6$N)",
       "enunciado_p6 <- enunciado"),
  list("M4 (el texto hipotético pierde el trazado declarado)", "oráculo",
       "enunciado_p6 <- construir_enunciado(k_p6$L, k_p6$T, k_p6$A, k_p6$N)",
       "enunciado_p6 <- construir_enunciado(k_p6$L, FALSE, k_p6$A, k_p6$N)"),
  list("M5 (sin dejar L al final cuando P2/P3 piden el área)", "G12",
       "pasadas_h6 <- if (area_en_p23 && ", "pasadas_h6 <- if (FALSE && "),
  list("M6 (el reintento de P2/P3 no excluye el área tras quitar L)", "G12",
       "if (identical(hecho_p6, \"L\")) ex23 <- unique(c(ex23, cods_area))", "NULL"))
for (m in mut)
  if (!correr_mutante(m[[1]], mutar(fuente, m[[3]], m[[4]]), m[[2]]))
    fallos <- c(fallos, paste(m[[1]], "sobrevive"))

cat("\n", if (length(fallos)) paste("VEREDICTO: FALLA —", paste(fallos, collapse = "; ")) else
  "VEREDICTO: APROBADO", "\n", sep = "")
quit(status = if (length(fallos)) 1L else 0L)
