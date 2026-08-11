#!/usr/bin/env Rscript
# ============================================================================
# verificar_render.R -- excedente_almuerzo_metacognitivo_argumentacion_n4
#
# Verifica:
#   Fase 0 -- Enumerar espacio parametrico, encontrar semilla canonica.
#   Fase 1 -- Invariantes I-1..I-5, I-7, I-8 + correctitud de la clave
#             sobre >= 300 semillas.
#   Fase 2 -- I-6 (instancia canonica): fuerza los parametros canonicos y
#             compara enunciado + 4 opciones caracter por caracter contra
#             el texto oficial de ESPECIFICACION.md seccion 3.
#   Fase 3 -- Mutacion A (clave falsa): copia del .Rmd con sol invertido,
#             la bateria de chequeos DEBE rechazarla.
#   Fase 4 -- Mutacion B (fuga I-8): copia del .Rmd donde TIPO 2a provee T
#             en el enunciado; detectado por I-8 (frase + valor).
#   Fase 5 -- Mutacion C (omision de n en vez de T): mismo mutante, pero
#             verificado por KEY_SUFF: los datos del enunciado BASTAN para
#             calcular a = c/T x (T-P), asi que "No" es clave FALSA.
# ============================================================================
suppressPackageStartupMessages(library(exams))

RMD     <- "excedente_almuerzo_numerico_variacional_argumentacion_n4_v1.Rmd"
N_SEEDS <- 300L
PRIMES  <- c(7, 11, 13, 17, 19, 23, 29, 31, 37, 41)
SCRATCHPAD <- tempdir()

cat("=== verificar_render.R ===\n")
cat("Archivo:", RMD, "\nSemillas:", N_SEEDS, "\n\n")

# --- Helpers ---------------------------------------------------------------
fmt_cop_local <- function(x) {
  paste0("\\$", formatC(x, format = "f", digits = 0,
                        big.mark = ".", decimal.mark = ","))
}

knit_env <- function(rmd_path, seed) {
  set.seed(seed)
  env <- new.env(parent = globalenv())
  invisible(knitr::knit(rmd_path, output = tempfile(tmpdir = SCRATCHPAD),
                        envir = env, quiet = TRUE))
  env
}

# --- Battery: all invariant checks on a knitted environment ----------------
run_battery <- function(env) {
  errs <- character(0)
  # I-1: E > 0 (tipos donde E es real)
  if (env$tipo != "2a" && env$E <= 0)
    errs <- c(errs, paste0("I-1: E=", env$E))
  # I-2: q entera y c < q
  if (env$tipo %in% c("1", "2a")) {
    if (env$q != floor(env$q)) errs <- c(errs, paste0("I-2: q=", env$q))
    if (env$c_val > 0 && env$c_val >= env$q)
      errs <- c(errs, paste0("I-2: c=", env$c_val, ">=q=", env$q))
  }
  # I-3, I-4: a entero y > 0 (tipo 1)
  if (env$tipo == "1") {
    if (env$a_val != floor(env$a_val)) errs <- c(errs, paste0("I-3: a=", env$a_val))
    if (env$a_val <= 0) errs <- c(errs, paste0("I-4: a=", env$a_val))
    a_check <- env$c_val * env$E / env$Ttotal
    if (abs(env$a_val - a_check) > 0.01)
      errs <- c(errs, paste0("I-3_formula: a=", env$a_val, " vs ", a_check))
  }
  # I-5: multiplos
  if (env$P %% 50000 != 0) errs <- c(errs, paste0("I-5: P=", env$P))
  if (env$tipo %in% c("1", "2b") && env$Ttotal %% 50000 != 0)
    errs <- c(errs, paste0("I-5: T=", env$Ttotal))
  if (env$tipo %in% c("1", "2a") && env$c_val > 0 && env$c_val %% 500 != 0)
    errs <- c(errs, paste0("I-5: c=", env$c_val))
  # Unicidad de opciones
  if (length(unique(env$opciones_texto)) != length(env$opciones_texto))
    errs <- c(errs, "UNIQUENESS")
  # Sol sum
  if (sum(env$sol) != 1) errs <- c(errs, paste0("SOL: sum=", sum(env$sol)))

  # --- Correctitud de la clave (KEY) ---
  correct_idx  <- which(env$sol)
  if (length(correct_idx) != 1) {
    errs <- c(errs, "SOL_IDX: no hay exactamente 1 TRUE en sol")
    return(errs)
  }
  correct_text <- env$opciones_texto[correct_idx]
  if (env$tipo == "1") {
    if (!grepl("^Sí,", correct_text))
      errs <- c(errs, "KEY_CAT: tipo=1 pero correcta no empieza con Si")
    if (!grepl("se conoce el valor total de la cuenta", correct_text))
      errs <- c(errs, "KEY_PHRASE: tipo=1 sin frase clave")
  } else if (env$tipo == "2a") {
    if (!grepl("^No,", correct_text))
      errs <- c(errs, "KEY_CAT: tipo=2a pero correcta no empieza con No")
    if (!grepl("sin conocer el valor total de la cuenta", correct_text))
      errs <- c(errs, "KEY_PHRASE: tipo=2a sin frase clave")
  } else {
    if (!grepl("^No,", correct_text))
      errs <- c(errs, "KEY_CAT: tipo=2b pero correcta no empieza con No")
    if (!grepl("sin conocer el valor del consumo", correct_text))
      errs <- c(errs, "KEY_PHRASE: tipo=2b sin frase clave")
  }

  # --- I-8 reforzado: dato omitido no aparece en enunciado (frase + valor) ---
  enun <- env$enunciado_completo
  if (env$tipo == "2a") {
    if (grepl("total de la cuenta por pagar fue de", enun))
      errs <- c(errs, "I-8_phrase: frase de T en tipo 2a")
    if (env$Ttotal > 0) {
      tfmt <- fmt_cop_local(env$Ttotal)
      if (grepl(tfmt, enun, fixed = TRUE))
        errs <- c(errs, paste0("I-8_value: T=", tfmt, " en tipo 2a"))
    }
  }
  if (env$tipo == "2b") {
    if (grepl("consumió solamente", enun))
      errs <- c(errs, "I-8_phrase: frase de c en tipo 2b")
    if (env$c_val > 0) {
      cfmt <- fmt_cop_local(env$c_val)
      if (grepl(cfmt, enun, fixed = TRUE))
        errs <- c(errs, paste0("I-8_value: c=", cfmt, " en tipo 2b"))
    }
  }

  # --- KEY_SUFF: suficiencia real derivada del enunciado (C1) ----------------
  # a = c/T x (T-P) es determinable sii el enunciado entrega T y c.
  # P siempre esta. n es irrelevante para la formula.
  # Derivado del TEXTO del enunciado, no de la etiqueta tipo.
  T_in_enun <- grepl("total de la cuenta por pagar fue de \\$", enun, fixed = TRUE)
  c_in_enun <- grepl("consumió solamente", enun, fixed = TRUE)
  determinable <- T_in_enun && c_in_enun

  correct_cat <- if (grepl("^Sí", correct_text)) "si" else "no"
  if (determinable && correct_cat == "no")
    errs <- c(errs, "KEY_SUFF: datos en enunciado bastan para a, pero clave dice No")
  if (!determinable && correct_cat == "si")
    errs <- c(errs, "KEY_SUFF: datos en enunciado insuficientes para a, pero clave dice Si")

  errs
}

# ===== FASE 0: Enumerar combos y encontrar semilla canonica ================
cat("Fase 0: Enumerar espacio parametrico y buscar semilla canonica\n")

# El espacio se OBTIENE del .Rmd, no se re-implementa aqui. Una copia de la
# enumeracion diverge en silencio en cuanto el ejercicio cambia una cota: al
# acotar `c` a C_MAX_VEROSIMIL el .Rmd paso a 2291 combinaciones mientras esta
# fase seguia reportando 3948 y buscando la canonica en el espacio viejo.
.rmd_txt <- readLines(RMD, warn = FALSE)
.ini <- grep("^```\\{r data_generation", .rmd_txt)[1]
.fin <- grep("^```\\s*$", .rmd_txt); .fin <- .fin[.fin > .ini][1]
stopifnot(!is.na(.ini), !is.na(.fin))
.code_dg <- paste(.rmd_txt[(.ini + 1):(.fin - 1)], collapse = "\n")
.code_t1 <- sub("tipo <- safe_sample\\(c\\(\"1\", \"2a\", \"2b\"\\), 1L\\)",
                "tipo <- \"1\"", .code_dg)
stopifnot(grepl("tipo <- \"1\"", .code_t1, fixed = TRUE))  # la sustitucion ocurrio

.env_esp <- new.env(); set.seed(1L)
eval(parse(text = .code_t1), envir = .env_esp)
combos <- .env_esp$combos
stopifnot(is.data.frame(combos), nrow(combos) > 0,
          all(c("P", "n", "Ttotal", "a_val", "c_val") %in% names(combos)))
N_COMBOS <- nrow(combos)

canon_idx <- which(combos$P == 300000 & combos$n == 10 &
                   combos$Ttotal == 500000 & combos$c_val == 5000)
stopifnot(length(canon_idx) >= 1)
canon_idx <- canon_idx[1]
cat("  Combos TYPE 1:", N_COMBOS, "\n")
cat("  Indice canonico:", canon_idx, "\n")

# Buscar semilla canonica simulando el path RNG del .Rmd.
# Draws: d1=tipo(3), d2=combo(N). Los draws de contexto y articulo ocurren pero
# no condicionan (es_canonico los sobrescribe con contextos[[1]] y "un jugo").
# YA NO se filtra por el sorteo de sel_si: la EXCEPCION CANONICA fija los tres
# distractores oficiales por construccion, asi que ese draw no ocurre en esta
# rama. Filtrar por el era lo que hacia que la canonica saliera bien "de suerte".
canon_seed <- NA_integer_
for (s in seq_len(500000L)) {
  set.seed(s)
  if (sample.int(3L, 1L) != 1L) next
  if (sample.int(N_COMBOS, 1L) != canon_idx) next
  canon_seed <- s; break
}
if (is.na(canon_seed)) stop("No se encontro semilla canonica en 500000 intentos")
cat("  Semilla canonica:", canon_seed, "\n\n")

# ===== FASE 1: Invariantes sobre N semillas ================================
cat("Fase 1: Invariantes I-1..I-5, I-7, I-8, KEY (", N_SEEDS, " semillas)\n")

errors_f1 <- character(0)
tipos <- character(N_SEEDS)
for (s in seq_len(N_SEEDS)) {
  seed <- s * PRIMES[((s - 1L) %% length(PRIMES)) + 1L]
  env <- tryCatch(knit_env(RMD, seed), error = function(e) {
    errors_f1 <<- c(errors_f1, paste0("CRASH seed=", seed, ": ", conditionMessage(e)))
    NULL
  })
  if (is.null(env)) { tipos[s] <- "CRASH"; next }
  tipos[s] <- env$tipo
  batch <- run_battery(env)
  if (length(batch) > 0)
    errors_f1 <- c(errors_f1, paste0("seed=", seed, " ", batch))
}
cat("  Errores:", length(errors_f1), "\n")
cat("  Tipo distribution:\n"); print(table(tipos)); cat("\n")

# ===== FASE 2: I-6 canonica (INCONDICIONAL) ================================
cat("Fase 2: I-6 instancia canonica (incondicional, seed=", canon_seed, ")\n")

errors_f2 <- character(0)
env_canon <- knit_env(RMD, canon_seed)

if (!env_canon$es_canonico)
  errors_f2 <- c(errors_f2, "I-6: seed no produjo es_canonico=TRUE")

# Enunciado esperado (caracter por caracter)
exp_enun <- paste0(
  "Una empresa destina \\$300.000 para invitar a sus 10 empleados ",
  "a un almuerzo en un restaurante. ",
  "Debido a que el total de la cuenta por pagar fue de \\$500.000, ",
  "se propone que el excedente sea pagado de manera proporcional ",
  "al valor del pedido que hizo cada empleado.",
  "\n\n",
  "De acuerdo con la propuesta, si un empleado consumió solamente ",
  "un jugo de \\$5.000, ¿es posible determinar el valor que este ",
  "empleado debe pagar?")

if (env_canon$enunciado_completo != exp_enun) {
  errors_f2 <- c(errors_f2, "I-6_ENUNCIADO: mismatch caracter por caracter")
  # base R: `repr` no es dependencia del repo y esta rama solo se ejecuta
  # cuando I-6 falla, asi que su crash pasaba inadvertido.
  cat("  ESPERADO:\n  ", encodeString(exp_enun, quote = '"'), "\n")
  cat("  ACTUAL:\n  ", encodeString(env_canon$enunciado_completo, quote = '"'), "\n")
}

# 4 opciones esperadas (como conjunto, independiente del orden)
exp_opts <- sort(c(
  paste0("Sí, porque se conoce el valor total de la cuenta y se puede calcular ",
         "la proporción de esta que corresponde al jugo que consumió el empleado."),
  paste0("Sí, porque el empleado consumió menos de lo que le correspondía y, ",
         "por lo tanto, esta persona no debe pagar nada."),
  paste0("No, porque el procedimiento planteado no contemplaba que uno de los ",
         "empleados pudiera tener un consumo inferior a \\$30.000."),
  paste0("No, porque falta conocer los valores de los pedidos de los demás ",
         "empleados para saber quién debe pagar más y quién debe pagar menos.")
))
act_opts <- sort(env_canon$opciones_texto)

if (!identical(act_opts, exp_opts)) {
  errors_f2 <- c(errors_f2, "I-6_OPCIONES: mismatch en el conjunto de 4 opciones")
  for (i in seq_along(exp_opts)) {
    if (i <= length(act_opts) && exp_opts[i] != act_opts[i]) {
      cat("  OPT[", i, "] DIFF\n")
      cat("    EXP:", substr(exp_opts[i], 1, 100), "\n")
      cat("    ACT:", substr(act_opts[i], 1, 100), "\n")
    }
  }
}

cat("  Errores I-6:", length(errors_f2), "\n\n")

# ===== FASE 3: Mutacion A (clave falsa) ====================================
cat("Fase 3: Mutacion A (clave falsa sobre copia del .Rmd)\n")

errors_f3 <- character(0)
rmd_lines  <- readLines(RMD)
mut_a_path <- file.path(SCRATCHPAD, "mutant_A.Rmd")

rmd_mut_a <- gsub(
  "sol <- c(TRUE, rep(FALSE, length(sel_distractores)))",
  "sol <- c(FALSE, TRUE, FALSE, FALSE)",
  paste(rmd_lines, collapse = "\n"), fixed = TRUE)
writeLines(strsplit(rmd_mut_a, "\n")[[1]], mut_a_path)

# Contrato de mutacion (Incidente P -- INC-MUTANTE-SONDA): el mutante declara
# la sonda que DEBE cazarlo. Morir por otra sonda no cuenta como deteccion.
sonda_esperada_A <- "KEY_PHRASE"

env_mut_a <- tryCatch(knit_env(mut_a_path, 42L), error = function(e) {
  errors_f3 <<- c(errors_f3, paste0("CRASH: ", conditionMessage(e))); NULL
})
if (!is.null(env_mut_a)) {
  # Guarda sobre el ENTORNO (no sobre el texto del .Rmd): la marca debe haber
  # caido sobre un distractor, o el mutante no es el que dice ser.
  marcada <- which(as.logical(env_mut_a$sol))
  if (length(marcada) != 1L || isTRUE(env_mut_a$opciones[[marcada[1]]]$es_correcta))
    errors_f3 <- c(errors_f3,
      "MUTANTE A MAL CONSTRUIDO: sol no quedo invertido (la marca sigue en la correcta)")

  batch_a <- run_battery(env_mut_a)
  if (length(batch_a) == 0) {
    errors_f3 <- c(errors_f3,
      "MUTACION A NO DETECTADA: el mutante paso la bateria sin errores")
  } else if (!any(grepl(sonda_esperada_A, batch_a, fixed = TRUE))) {
    errors_f3 <- c(errors_f3, paste0(
      "MUTANTE A CAZADO POR LA SONDA EQUIVOCADA: esperaba ", sonda_esperada_A,
      ", rechazado por: ", paste(batch_a, collapse = " | ")))
  } else {
    cat("  Mutante A rechazado por su sonda (", sonda_esperada_A, "):\n")
    for (e in batch_a) cat("    ", e, "\n")
  }
}
cat("  Errores Fase 3:", length(errors_f3), "\n\n")

# ===== FASE 4: Mutacion B (fuga I-8: TIPO 2a provee T en enunciado) ========
cat("Fase 4: Mutacion B (TIPO 2a filtra T; detectado por I-8)\n")

errors_f4 <- character(0)
sonda_esperada_B <- "I-8"   # contrato de mutacion (Incidente P)
mut_b_path <- file.path(SCRATCHPAD, "mutant_B.Rmd")

# Mod 1: TYPE 2a provee T real
rmd_mut_b <- gsub(
  "# T, E, a no aplican en TIPO 2a (sentinelas, nunca emitidos)\n  Ttotal <- 0; E <- 0; a_val <- 0",
  paste0("T_pool_2a <- seq(P + 50000, min(P + 500000, 1200000), by = 50000)\n",
         "  Ttotal <- safe_sample(T_pool_2a, 1L); E <- Ttotal - P; a_val <- 0"),
  paste(rmd_lines, collapse = "\n"), fixed = TRUE)

# Mod 2: enunciado siempre incluye T
rmd_mut_b <- sub(
  'if (tipo == "1" || tipo == "2b") {',
  'if (TRUE) {  # MUTANT_B: siempre incluye T',
  rmd_mut_b, fixed = TRUE)

writeLines(strsplit(rmd_mut_b, "\n")[[1]], mut_b_path)

# Buscar semilla que produzca TYPE 2a en el mutante
mut_b_seed <- NA_integer_
for (s in 1:200) {
  env_try <- tryCatch(knit_env(mut_b_path, s), error = function(e) NULL)
  if (!is.null(env_try) && env_try$tipo == "2a") { mut_b_seed <- s; break }
}

if (is.na(mut_b_seed)) {
  errors_f4 <- c(errors_f4, "No se encontro semilla TYPE 2a para mutante B")
} else {
  env_mut_b <- knit_env(mut_b_path, mut_b_seed)
  # Guarda sobre el ENTORNO: la fuga debe existir de verdad (T emitido en 2a).
  if (env_mut_b$Ttotal <= 0 ||
      !grepl("total de la cuenta por pagar fue de",
             env_mut_b$enunciado_completo, fixed = TRUE))
    errors_f4 <- c(errors_f4,
      "MUTANTE B MAL CONSTRUIDO: TIPO 2a no llego a emitir T en el enunciado")

  batch_b <- run_battery(env_mut_b)
  if (length(batch_b) == 0) {
    errors_f4 <- c(errors_f4,
      "MUTACION B NO DETECTADA: el mutante paso la bateria sin errores")
  } else if (!any(grepl(sonda_esperada_B, batch_b, fixed = TRUE))) {
    errors_f4 <- c(errors_f4, paste0(
      "MUTANTE B CAZADO POR LA SONDA EQUIVOCADA: esperaba ", sonda_esperada_B,
      ", rechazado por: ", paste(batch_b, collapse = " | ")))
  } else {
    cat("  Mutante B (seed=", mut_b_seed, ") rechazado por su sonda (",
        sonda_esperada_B, "):\n")
    for (e in batch_b) cat("    ", e, "\n")
  }
}
cat("  Errores Fase 4:", length(errors_f4), "\n\n")

# ===== FASE 5: Mutacion C (omision de n: datos bastan, "No" es falsa) ======
cat("Fase 5: Mutacion C (TIPO 2 omite n en vez de T/c; KEY_SUFF debe rechazar)\n")

errors_f5 <- character(0)
sonda_esperada_C <- "KEY_SUFF"   # contrato de mutacion (Incidente P)
# Mutante C INDEPENDIENTE (no reutiliza el B): copia del .Rmd donde la rama
# TIPO 2b omite `n` en lugar de `c`. Resultado: el enunciado entrega P, T y c
# (datos suficientes para a = c/T*(T-P)) y omite n, que es IRRELEVANTE para la
# formula. La clave sigue siendo "No, porque sin conocer el valor del consumo",
# que en esa version es FALSA. Ningun chequeo basado en la etiqueta `tipo`
# puede verlo: solo KEY_SUFF, que se deriva del texto del enunciado.
mut_c_path <- file.path(SCRATCHPAD, "mutant_C.Rmd")

# Mod 1: omitir n del enunciado (se pasa "" como numero de miembros)
rmd_mut_c <- gsub(
  "ctx$plantilla(ctx$sujeto_indef, n_emp, ctx$miembros,",
  "ctx$plantilla(ctx$sujeto_indef, \"\", ctx$miembros,",
  paste(rmd_lines, collapse = "\n"), fixed = TRUE)

# Mod 2: la rama que emite la frase de c deja de excluir a TIPO 2b
rmd_mut_c <- sub(
  'if (tipo == "1" || tipo == "2a") {',
  'if (TRUE) {  # MUTANT_C: c siempre presente, el dato omitido pasa a ser n',
  rmd_mut_c, fixed = TRUE)

writeLines(strsplit(rmd_mut_c, "\n")[[1]], mut_c_path)

# Buscar semilla que produzca TYPE 2b en el mutante
mut_c_seed <- NA_integer_
for (s in 1:200) {
  env_try <- tryCatch(knit_env(mut_c_path, s), error = function(e) NULL)
  if (!is.null(env_try) && env_try$tipo == "2b") { mut_c_seed <- s; break }
}

if (is.na(mut_c_seed)) {
  errors_f5 <- c(errors_f5, "No se encontro semilla TYPE 2b para mutante C")
} else {
  env_mut_c <- knit_env(mut_c_path, mut_c_seed)
  # Comprobar que el mutante es realmente lo que dice ser antes de juzgarlo
  if (grepl(as.character(env_mut_c$n_emp), env_mut_c$enunciado_completo, fixed = TRUE))
    errors_f5 <- c(errors_f5,
      "MUTANTE C MAL CONSTRUIDO: n sigue apareciendo en el enunciado")
  if (!grepl("consumió solamente", env_mut_c$enunciado_completo, fixed = TRUE))
    errors_f5 <- c(errors_f5,
      "MUTANTE C MAL CONSTRUIDO: c no aparece en el enunciado")
  batch_c   <- run_battery(env_mut_c)
  has_key_suff <- any(grepl(sonda_esperada_C, batch_c, fixed = TRUE))
  if (!has_key_suff) {
    errors_f5 <- c(errors_f5, paste0(
      "MUTANTE C CAZADO POR LA SONDA EQUIVOCADA (o no detectado): esperaba ",
      sonda_esperada_C, ", rechazado por: ",
      if (length(batch_c)) paste(batch_c, collapse = " | ") else "<nada>"))
  } else {
    suff_msgs <- batch_c[grepl(sonda_esperada_C, batch_c, fixed = TRUE)]
    cat("  Mutante C (seed=", mut_c_seed, ") rechazado por su sonda (",
        sonda_esperada_C, "):\n")
    for (e in suff_msgs) cat("    ", e, "\n")
  }
}
cat("  Errores Fase 5:", length(errors_f5), "\n\n")

# ===== RESUMEN =============================================================
all_errors <- c(errors_f1, errors_f2, errors_f3, errors_f4, errors_f5)

cat("============================================\n")
if (length(all_errors) == 0) {
  cat("  RESULTADO: APROBADO (0 errores)\n")
} else {
  cat("  RESULTADO: ERRORES DETECTADOS (", length(all_errors), ")\n")
  for (e in all_errors) cat("    ", e, "\n")
}
cat("============================================\n")

quit(status = if (length(all_errors) == 0) 0L else 1L)
