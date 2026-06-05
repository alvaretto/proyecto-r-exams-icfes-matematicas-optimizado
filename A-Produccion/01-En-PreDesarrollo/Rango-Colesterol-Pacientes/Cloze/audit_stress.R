# Auditoría profunda: stress test runtime con timeout por semilla.
# Detecta: (a) cuelgue del repeat de Parte 6, (b) fallo de stopifnot(datos_validos),
# (c) ambigüedad P3 con el set REAL de opciones mostradas, (d) invariantes de solución.
repo <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
setwd(repo)
suppressWarnings(suppressMessages(source(".claude/scripts/validar_coherencia_matematica.R")))
rmd <- "A-Produccion/01-En-PreDesarrollo/Rango-Colesterol-Pacientes/Cloze/rango_colesterol_metacognitivo_interpretacion_n3_cloze_v1.Rmd"
parsed <- parsear_rmd(rmd)
codigo_data <- parse(text = paste(parsed$chunks_r[[2]], collapse = "\n"))

run_data_timed <- function(limite = 4) {
  env <- new.env(parent = globalenv())
  setTimeLimit(cpu = limite, elapsed = limite, transient = TRUE)
  on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE))
  eval(codigo_data, envir = env)
  env
}

N <- 600
hangs <- integer(0); stopfail <- integer(0); otros_err <- list()
ambiguos_real <- integer(0)
p6_hang_risk <- integer(0)   # rango_mas_grande >= span: imposible cumplir P6=TRUE
inv_fail <- list()
span_de <- function(ctx, env) {
  rv <- env$rango_var; rv[2]-rv[1]
}
disp_min <- Inf
intentos_max <- 0
for (i in 1:N) {
  set.seed(i)
  e <- tryCatch(run_data_timed(4),
    error = function(err) {
      msg <- conditionMessage(err)
      if (grepl("reached .*time limit|tiempo|time limit", msg, ignore.case = TRUE)) {
        hangs[[length(hangs)+1]] <<- i
      } else if (grepl("datos_validos", msg)) {
        stopfail[[length(stopfail)+1]] <<- i
      } else {
        otros_err[[length(otros_err)+1]] <<- list(seed=i, msg=msg)
      }
      NULL
    })
  if (is.null(e)) next

  intentos_max <- max(intentos_max, e$intentos)
  disp_min <- min(disp_min, max(e$rangos) - min(e$rangos))

  # (c) Ambigüedad P3 con el set REAL mostrado (codigos_p3_opciones -> índices)
  # Reconstruir qué índice de paciente corresponde a cada código mostrado.
  cod2idx <- c(
    "EST-RAN-01" = e$idx_error_ran01,
    "EST-RAN-05" = e$idx_error_ran05,
    "EST-RAN-06" = e$idx_error_ran06,
    "EST-RAN-02" = e$idx_error_ran02,
    "EST-RAN-03" = e$idx_error_ran03
  )
  idx_mostrados <- unname(cod2idx[e$codigos_p3_opciones])
  d <- e$distractor_p3_idx
  if (sum(idx_mostrados == d) != 1) ambiguos_real[[length(ambiguos_real)+1]] <- i

  # (d) Riesgo de cuelgue P6: si rango_mas_grande >= span, P6=TRUE es imposible
  sp <- span_de(e$ctx, e)
  if (e$rango_mas_grande >= sp) p6_hang_risk[[length(p6_hang_risk)+1]] <- i

  # Invariantes de solución
  problemas <- character(0)
  if (sum(e$sol_p1) != 1) problemas <- c(problemas, "sol_p1 != 1 correcta")
  if (sum(e$sol_p3) != 1) problemas <- c(problemas, "sol_p3 != 1 correcta")
  nv <- sum(e$sol_p5); nf <- length(e$sol_p5) - nv
  if (nv < 2 || nf < 2) problemas <- c(problemas, paste0("P5 V/F desbalance V=",nv," F=",nf))
  if (length(e$sol_p5) != e$n_afirmaciones) problemas <- c(problemas, "len sol_p5 != n_afirmaciones")
  if (sum(e$sol_p6) != 1) problemas <- c(problemas, "sol_p6 != 1")
  if (e$valor_rango_menor == e$valor_rango_mayor) problemas <- c(problemas, "menor==mayor rango")
  if (e$valor_rango_menor != min(e$rangos)) problemas <- c(problemas, "valor_rango_menor != min(rangos)")
  if (e$valor_rango_mayor != max(e$rangos)) problemas <- c(problemas, "valor_rango_mayor != max(rangos)")
  # Coherencia P6: rango_extra vs es_verdadero
  if (e$es_verdadero_p6 && !(e$rango_extra > e$rango_mas_grande)) problemas <- c(problemas, "P6 TRUE pero rango_extra<=max")
  if (!e$es_verdadero_p6 && !(e$rango_extra <= e$rango_mas_grande)) problemas <- c(problemas, "P6 FALSE pero rango_extra>max")
  if (length(problemas) > 0) inv_fail[[length(inv_fail)+1]] <- list(seed=i, problemas=problemas)
}

cat("\n========== AUDIT STRESS (", N, " semillas, timeout 4s) ==========\n", sep="")
cat("Cuelgues (timeout):        ", length(hangs), if(length(hangs)) paste0(" -> seeds: ", paste(head(hangs,15), collapse=",")) else "", "\n")
cat("Fallos stopifnot(validos): ", length(stopfail), if(length(stopfail)) paste0(" -> seeds: ", paste(head(stopfail,15), collapse=",")) else "", "\n")
cat("Otros errores ejecución:   ", length(otros_err), "\n")
if (length(otros_err)) for (o in head(otros_err,5)) cat("   seed", o$seed, ":", o$msg, "\n")
cat("Riesgo cuelgue P6 (rmg>=span):", length(p6_hang_risk), if(length(p6_hang_risk)) paste0(" -> seeds: ", paste(head(p6_hang_risk,15), collapse=",")) else "", "\n")
cat("Ambiguos P3 (set REAL):    ", length(ambiguos_real), if(length(ambiguos_real)) paste0(" -> seeds: ", paste(head(ambiguos_real,15), collapse=",")) else "", "\n")
cat("Intentos max validez:      ", intentos_max, "\n")
cat("Dispersion minima:         ", disp_min, "\n")
cat("Invariantes violadas:      ", length(inv_fail), "\n")
if (length(inv_fail)) for (f in head(inv_fail,10)) cat("   seed", f$seed, ":", paste(f$problemas, collapse="; "), "\n")
