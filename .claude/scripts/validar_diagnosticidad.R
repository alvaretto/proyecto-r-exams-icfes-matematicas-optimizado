#!/usr/bin/env Rscript
# =============================================================================
# validar_diagnosticidad.R
# -----------------------------------------------------------------------------
# ¿Es la opción correcta de un gap identificable por una heurística SUPERFICIAL,
# sin razonar el contenido? Si lo es en la mayoría de versiones, ese gap
# discrimina por artefacto y no por el constructo que dice evaluar.
#
# MOTIVO (2026-08-06): el resto del arsenal verifica corrección, formato,
# unicidad y diversidad, pero NADA medía si los distractores discriminan.
# Medido sobre dos CLOZE ya aprobados, dos gaps resultaron resolubles por
# longitud en el 100% de las versiones ("la opción más larga es la correcta"
# es una de las heurísticas de examen más conocidas).
#
# Sondas (elegidas por ser crisp: separan en 0%/100%, no en una nube):
#   H1 LONGITUD  la correcta es la ÚNICA más larga (o la ÚNICA más corta)
#                Y POR UN MARGEN RELATIVO APRECIABLE frente a su rival más
#                próximo (>= --margen, por defecto 15%).
#   H2 PREFIJO   la correcta es la única con su primera palabra Y es el único
#                singleton — si TODAS las opciones tienen prefijo distinto, no
#                destaca y NO se cuenta (falso positivo de opciones numéricas).
#   H3 VEREDICTO la primera palabra de la correcta es SIEMPRE la misma A LO
#                LARGO DE LAS VERSIONES. Única sonda CROSS-versión: H1 y H2
#                miran dentro de una versión y por eso no la ven. Caso típico:
#                ítem de conclusión binaria ("Sí, porque…" / "No, porque…") en
#                el que la clave es siempre "No" porque la afirmación evaluada
#                es falsa por construcción. Un balance 2+2 dentro de cada
#                versión NO protege: el estudiante que aprende el patrón
#                descarta la mitad de las opciones sin razonar (25% -> 50% de
#                acierto por azar). Solo cuentan las versiones en que el prefijo
#                actúa como CATEGORÍA (al menos dos opciones lo comparten y no
#                todas son iguales): si todas empiezan igual no informa de nada,
#                y si todas empiezan distinto el prefijo identifica cada opción
#                por separado — caso de las opciones numéricas, donde la primera
#                palabra ES el valor.
#                Añadida 2026-08-08 tras detectarse en un SCHOICE N4 de
#                argumentación con 60/60 versiones de clave "No". Ver la regla
#                #22 §P4, que ya describía el caso sin que nada lo midiera.
#
# POR QUÉ EL MARGEN (2026-08-06): la v1 de la sonda H1 solo miraba el orden
# ("¿es la única más larga?"), no la distancia. Tras igualar deliberadamente la
# extensión de las opciones de un gap, el margen bajó de 29 a 8 caracteres
# medianos (32% -> 7% relativo) y la sonda seguía reportando 100%: el ejercicio
# quedaba bloqueado por una diferencia que ningún estudiante puede usar como
# heurística. Un ítem con opciones de 115 y 123 caracteres NO es resoluble "por
# la más larga"; uno con 90 y 119 sí. La sonda mide ahora la distancia, no solo
# el orden. Umbral por defecto 15%: calibrado contra las dos versiones medidas
# del mismo gap (original 32% y 21% -> siguen cazadas; corregido 7% y 5% -> ya
# no). Entre paréntesis se reporta SIEMPRE el margen mediano observado, aunque
# no supere el umbral, para que "no dispara" no se confunda con "no hay señal".
#
# Uso:  Rscript validar_diagnosticidad.R archivo.Rmd [--n 30] [--umbral 70] [--margen 15]
#
# Veredictos:
#   PASS                   (exit 0) ninguna sonda supera el umbral
#   WARN_DIAG_SUPERFICIAL  (exit 0) alguna sonda entre umbral y 99%
#   ERR_DIAG_SUPERFICIAL   (exit 1) alguna sonda al 100%: en TODAS las versiones
#                                   la correcta se identifica sin razonar
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
rmd <- if (length(args) >= 1) args[1] else NA
n <- 30L; umbral <- 70; margen <- 15
if ("--n" %in% args) { v <- suppressWarnings(as.integer(args[which(args == "--n") + 1])); if (!is.na(v)) n <- v }
if ("--umbral" %in% args) { v <- suppressWarnings(as.numeric(args[which(args == "--umbral") + 1])); if (!is.na(v)) umbral <- v }
if ("--margen" %in% args) { v <- suppressWarnings(as.numeric(args[which(args == "--margen") + 1])); if (!is.na(v)) margen <- v }
if (is.na(rmd) || !file.exists(rmd)) { cat("WARN_DIAG_INDET: .Rmd no encontrado:", rmd, "\n"); quit(status = 0) }

lines <- readLines(rmd, warn = FALSE)
dg <- grep("^```\\{r[ ]*data_generation", lines)
start <- if (length(dg)) dg[1] else NA
fences <- grep("^```\\s*$", lines)
end <- if (!is.na(start)) fences[fences > start][1] else NA
if (is.na(start) || is.na(end)) { cat("WARN_DIAG_INDET: data_generation no aislable\n"); quit(status = 0) }
expr <- tryCatch(parse(text = paste(lines[(start + 1):(end - 1)], collapse = "\n")), error = function(e) NULL)
if (is.null(expr)) { cat("WARN_DIAG_INDET: data_generation no parseable\n"); quit(status = 0) }

txt_of <- function(x) {
  if (is.list(x)) {
    for (f in c("texto", "descrip", "descripcion_corta", "label"))
      if (!is.null(x[[f]])) return(as.character(x[[f]])[1])
    return(paste(unlist(lapply(x, as.character)), collapse = " "))
  }
  as.character(x)[1]
}

tmp <- file.path(tempdir(), paste0("diag_", Sys.getpid())); dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
old <- getwd(); on.exit({ setwd(old); unlink(tmp, recursive = TRUE) }, add = TRUE)

acc <- list()
for (i in seq_len(n)) {
  setwd(tmp); env <- new.env(parent = globalenv()); assign("n", NA, envir = env)
  set.seed(i * 7919L + 13L)
  ok <- tryCatch({ suppressWarnings(suppressMessages(eval(expr, envir = env))); TRUE }, error = function(e) FALSE)
  setwd(old)
  if (!ok) next
  nms <- ls(envir = env)
  # Gaps schoice del CLOZE (opciones_pN + sol_pN con una marca) y SCHOICE puro.
  pares <- list()
  for (k in unique(sub("^opciones_p(\\d+)$", "\\1", grep("^opciones_p\\d+$", nms, value = TRUE))))
    if (paste0("sol_p", k) %in% nms) pares[[paste0("p", k)]] <- c(paste0("opciones_p", k), paste0("sol_p", k))
  for (o in c("opciones_mezcladas", "opciones"))
    if (o %in% nms) for (s in c("sol", "solucion"))
      if (s %in% nms && is.null(pares[["unico"]])) pares[["unico"]] <- c(o, s)
  for (g in names(pares)) {
    ops <- tryCatch(get(pares[[g]][1], envir = env), error = function(e) NULL)
    sol <- tryCatch(as.logical(get(pares[[g]][2], envir = env)), error = function(e) NULL)
    if (is.null(ops) || is.null(sol) || length(ops) != length(sol)) next
    if (sum(sol, na.rm = TRUE) != 1L || length(ops) < 3L) next
    tx <- vapply(ops, txt_of, character(1))
    if (any(is.na(tx)) || length(unique(tx)) < 2L) next
    ic <- which(sol)[1]
    L <- nchar(tx)
    # Orden: ¿es la unica en el extremo? Distancia: ¿por cuanto, frente a su
    # rival mas proximo? La sonda dispara solo si se cumplen AMBAS (ver cabecera).
    unica_max <- L[ic] == max(L) && sum(L == max(L)) == 1L
    unica_min <- L[ic] == min(L) && sum(L == min(L)) == 1L
    rival_max <- max(L[-ic]); rival_min <- min(L[-ic])
    m_max <- if (unica_max) { if (rival_max > 0) 100 * (L[ic] - rival_max) / rival_max else Inf } else NA_real_
    m_min <- if (unica_min) { if (rival_min > 0) 100 * (rival_min - L[ic]) / rival_min else Inf } else NA_real_
    es_max <- unica_max && m_max >= margen
    es_min <- unica_min && m_min >= margen
    pw <- tolower(sub("^[^[:alnum:]]*([[:alnum:]á-úÁ-Úñü]+).*$", "\\1", tx))
    tb <- table(pw)
    # Solo cuenta si la correcta es el UNICO singleton: si todas las primeras
    # palabras son distintas, ser unica no la hace destacar.
    es_unica <- tb[[pw[ic]]] == 1L && sum(tb == 1L) == 1L
    if (is.null(acc[[g]]))
      acc[[g]] <- list(cnt = c(h1max = 0, h1min = 0, h2 = 0, n = 0, umax = 0, umin = 0),
                       mmax = numeric(0), mmin = numeric(0), pwc = character(0))
    acc[[g]]$cnt <- acc[[g]]$cnt + c(es_max, es_min, es_unica, 1, unica_max, unica_min)
    # H3: primera palabra de la correcta, para medir invariancia ENTRE versiones.
    # Solo cuenta si el prefijo funciona como CATEGORIA, es decir si agrupa
    # opciones: al menos dos comparten prefijo (unicos < total) y no todas son
    # iguales (unicos >= 2). Misma logica de guarda que H2.
    #   - todas iguales  -> el prefijo no distingue nada.
    #   - todas distintas-> el prefijo identifica a cada opcion por separado, no
    #     hay categoria binaria que aprender; ademas en opciones numericas la
    #     primera palabra ES el valor y "siempre la misma" no se sostiene.
    # Sin esta guarda la sonda daba un 100% espurio en items cuyas opciones
    # llevan etiquetas fijas distintas (fixture de test_diagnosticidad.R).
    if (length(unique(pw)) >= 2L && length(unique(pw)) < length(pw))
      acc[[g]]$pwc <- c(acc[[g]]$pwc, pw[ic])
    if (unica_max) acc[[g]]$mmax <- c(acc[[g]]$mmax, m_max)
    if (unica_min) acc[[g]]$mmin <- c(acc[[g]]$mmin, m_min)
  }
}

cat("=== Diagnosticidad de distractores (gaps de seleccion unica) ===\n")
cat("Archivo:", basename(rmd), "| semillas:", n, "| umbral:", umbral, "% | margen H1:", margen, "%\n\n")
if (!length(acc)) { cat("WARN_DIAG_INDET: sin gaps analizables (opciones+sol con una sola marca).\n"); quit(status = 0) }

cat(sprintf("%-6s %6s  %20s  %20s  %12s\n", "gap", "vers",
            "H1 mas-larga", "H1 mas-corta", "H2 prefijo"))
cat(sprintf("%-6s %6s  %20s  %20s  %12s\n", "", "",
            "tasa (margen med)", "tasa (margen med)", "tasa"))
peor <- 0; criticos <- character(0); avisos <- character(0); notas <- character(0)
fmt_m <- function(v) if (!length(v)) "  -" else sprintf("%.0f%%", median(v))
for (g in names(acc)[order(names(acc))]) {
  a <- acc[[g]]$cnt; N <- a[["n"]]
  tasas <- 100 * c(a[["h1max"]], a[["h1min"]], a[["h2"]]) / N
  cat(sprintf("%-6s %6d  %13.0f%% (%5s)  %13.0f%% (%5s)  %11.0f%%\n", g, N,
              tasas[1], fmt_m(acc[[g]]$mmax), tasas[2], fmt_m(acc[[g]]$mmin), tasas[3]))
  # H3 (cross-version): ¿la primera palabra de la correcta es siempre la misma?
  pwc <- acc[[g]]$pwc
  if (length(pwc) >= 5L) {
    tasa_h3 <- 100 * max(table(pwc)) / length(pwc)
    moda_h3 <- names(which.max(table(pwc)))
    cat(sprintf("%-6s %6s  H3 veredicto: la correcta empieza por \"%s\" en el %.0f%% de %d versiones\n",
                "", "", moda_h3, tasa_h3, length(pwc)))
    if (tasa_h3 >= 100)
      criticos <- c(criticos, sprintf("%s (H3 veredicto invariante: siempre \"%s\")", g, moda_h3))
    else if (tasa_h3 >= 90)
      avisos <- c(avisos, sprintf("%s (H3 veredicto casi invariante: \"%s\" en %.0f%%)", g, moda_h3, tasa_h3))
  }
  peor <- max(peor, max(tasas))
  etiq <- c("H1 mas-larga", "H1 mas-corta", "H2 prefijo")
  for (j in seq_along(tasas)) {
    if (tasas[j] >= 100) criticos <- c(criticos, sprintf("%s (%s)", g, etiq[j]))
    else if (tasas[j] >= umbral) avisos <- c(avisos, sprintf("%s (%s, %.0f%%)", g, etiq[j], tasas[j]))
  }
  # "No dispara" != "no hay senal": si la correcta ocupa el extremo en (casi)
  # todas las versiones y solo la salva el margen, hay que verlo explicitamente.
  for (s in list(list("umax", "mas larga", acc[[g]]$mmax, tasas[1]),
                 list("umin", "mas corta", acc[[g]]$mmin, tasas[2]))) {
    tasa_orden <- 100 * a[[s[[1]]]] / N
    if (tasa_orden >= umbral && s[[4]] < umbral)
      notas <- c(notas, sprintf("%s: la correcta es la unica %s en el %.0f%% de las versiones, pero por un margen mediano de %s (< %g%%)",
                                g, s[[2]], tasa_orden, fmt_m(s[[3]]), margen))
  }
}
cat("\n")
if (length(notas)) {
  cat("NOTA DE ORDEN (informativa, no bloquea):\n")
  for (x in notas) cat("  - ", x, "\n", sep = "")
  cat("\n")
}
if (length(criticos)) {
  cat("ERR_DIAG_SUPERFICIAL: en TODAS las versiones la correcta se identifica sin razonar en: ",
      paste(criticos, collapse = ", "), "\n", sep = "")
  cat("  -> Igualar la extension de las opciones (basta bajar el margen por debajo del ", margen,
      "%) / evitar que la correcta sea la unica con su prefijo.\n", sep = "")
  cat("  -> Ver .claude/rules/diversidad-sustantiva.md y el catalogo de distractores.\n")
  quit(status = 1)
}
if (length(avisos)) {
  cat("WARN_DIAG_SUPERFICIAL: tasa alta (no total) en: ", paste(avisos, collapse = ", "), "\n", sep = "")
  quit(status = 0)
}
cat("PASS: ninguna sonda superficial supera el", umbral, "%.\n")
quit(status = 0)
