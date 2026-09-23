#!/usr/bin/env Rscript
# ============================================================================
# verificar_render.R -- CLOZE excedente_almuerzo_..._cloze_v2
#
# Cambios respecto del v1 (ESPECIFICACION_CLOZE_V2.md D4):
#   - D4.1: Guarda inalcanzable eliminada (Fase 4 Mutacion B)
#   - D4.2: I-1 recalculada desde el entorno, no desde combos
#   - D4.3: Sondas KEY_P5 extraidas del entorno, no transcritas a mano
#   - C-4: paridad de longitudes en gaps schoice (Fase 1 + Fase 6 mutante)
#   - KEY_P4: verifica por contenido (dato necesario?), no por patron fijo
#
# Fases:
#   0 -- Enumerar espacio, encontrar semilla canonica
#   1 -- Invariantes I-1..I-5, I-7, C-1..C-4, KEY sobre >= 300 semillas
#   2 -- I-6 (instancia canonica incondicional)
#   3 -- Mutacion A (clave falsa: sol_p3 invertido) -> sonda KEY_P3
#   4 -- Mutacion B (E negativo post-hoc) -> sonda I-1
#   5 -- Mutacion C (dato_retirado=n con respuesta "No") -> sonda KEY_P6
#   6 -- Mutacion D (corromper paridad de longitudes) -> sonda C-4
# ============================================================================
suppressPackageStartupMessages(library(exams))

RMD     <- "excedente_almuerzo_numerico_variacional_argumentacion_n4_cloze_v2.Rmd"
N_SEEDS <- 300L
PRIMES  <- c(7, 11, 13, 17, 19, 23, 29, 31, 37, 41)
SCRATCHPAD <- tempdir()

cat("=== verificar_render.R (CLOZE v2) ===\n")
cat("Archivo:", RMD, "\nSemillas:", N_SEEDS, "\n\n")

# --- Helpers ---------------------------------------------------------------
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

  # C-1: siempre TIPO 1
  if (env$tipo != "1")
    errs <- c(errs, paste0("C-1: tipo=", env$tipo, " (esperado 1)"))

  # I-1: E > 0 -- D4.2: recalcular desde el entorno, no desde combos
  E_recalc <- env$Ttotal - env$P
  if (E_recalc <= 0) errs <- c(errs, paste0("I-1: E_recalc=", E_recalc))
  if (env$E != E_recalc)
    errs <- c(errs, paste0("I-1_coherencia: env$E=", env$E,
                           " vs Ttotal-P=", E_recalc))

  # I-2: q entera y c < q
  if (env$q != floor(env$q)) errs <- c(errs, paste0("I-2: q=", env$q))
  if (env$c_val >= env$q)
    errs <- c(errs, paste0("I-2: c=", env$c_val, ">=q=", env$q))

  # I-3: a entero y formula correcta
  if (env$a_val != floor(env$a_val))
    errs <- c(errs, paste0("I-3: a=", env$a_val))
  a_check <- env$c_val * env$E / env$Ttotal
  if (abs(env$a_val - a_check) > 0.01)
    errs <- c(errs, paste0("I-3_formula: a=", env$a_val, " vs ", a_check))

  # I-4: a > 0
  if (env$a_val <= 0) errs <- c(errs, paste0("I-4: a=", env$a_val))

  # I-5: multiplos
  if (env$P %% 50000 != 0) errs <- c(errs, paste0("I-5: P=", env$P))
  if (env$Ttotal %% 50000 != 0) errs <- c(errs, paste0("I-5: T=", env$Ttotal))
  if (env$c_val %% 500 != 0) errs <- c(errs, paste0("I-5: c=", env$c_val))

  # I-7: sin Unicode problematico en textos emitidos
  textos_emitidos <- c(env$enunciado_completo, env$opciones_p3,
                       env$opciones_p4, env$opciones_p5, env$opciones_p6)
  for (t in textos_emitidos) {
    if (grepl("−", t)) errs <- c(errs, "I-7: U+2212 en texto emitido")
    if (grepl("≤", t)) errs <- c(errs, "I-7: U+2264 en texto emitido")
    if (grepl("≥", t)) errs <- c(errs, "I-7: U+2265 en texto emitido")
  }

  # C-2: dato_retirado valido
  if (!(env$dato_retirado %in% c("T", "c", "n")))
    errs <- c(errs, paste0("C-2: dato_retirado=", env$dato_retirado))

  # C-3: mchoice (Part 4) tiene al menos 1 TRUE y 1 FALSE, 5 opciones
  if (sum(env$sol_p4) < 1) errs <- c(errs, "C-3: sol_p4 sin TRUE")
  if (sum(env$sol_p4) == length(env$sol_p4)) errs <- c(errs, "C-3: sol_p4 todo TRUE")
  if (length(env$opciones_p4) != 5) errs <- c(errs, "C-3: mchoice != 5 opciones")

  # C-4: paridad de longitudes en gaps schoice (p3, p5, p6)
  # Per-seed: threshold 30% (catches deliberate corruption ~110% while passing
  # normal variation ~25%). Population checks (<=60%) in aggregate section.
  # H1 median margin (<15%) checked by validar_diagnosticidad.R (paso 9).
  for (pname in c("p3", "p5", "p6")) {
    sol_vec <- env[[paste0("sol_", pname)]]
    opc_vec <- env[[paste0("opciones_", pname)]]
    if (is.null(sol_vec) || is.null(opc_vec)) next
    idx_cor <- which(sol_vec)
    if (length(idx_cor) != 1) next
    nch <- nchar(opc_vec)
    med_otros <- median(nch[-idx_cor])
    if (med_otros > 0) {
      margen <- abs(nch[idx_cor] - med_otros) / med_otros
      if (margen >= 0.30)
        errs <- c(errs, paste0("C-4_", pname, ": margen=",
                               round(margen * 100, 1), "% (>=30%)"))
    }
  }

  # Structure checks
  if (sum(env$sol_p3) != 1) errs <- c(errs, paste0("P3: sum(sol)=", sum(env$sol_p3)))
  if (sum(env$sol_p5) != 1) errs <- c(errs, paste0("P5: sum(sol)=", sum(env$sol_p5)))
  if (sum(env$sol_p6) != 1) errs <- c(errs, paste0("P6: sum(sol)=", sum(env$sol_p6)))
  if (length(env$opciones_p3) != 4) errs <- c(errs, "P3: != 4 opciones")
  if (length(env$opciones_p5) != 4) errs <- c(errs, "P5: != 4 opciones")
  if (length(env$opciones_p6) != 4) errs <- c(errs, "P6: != 4 opciones")

  # Uniqueness of options within each part
  for (pname in c("p3", "p4", "p5", "p6")) {
    opc <- env[[paste0("opciones_", pname)]]
    if (length(unique(opc)) != length(opc))
      errs <- c(errs, paste0(toupper(pname), "_UNIQ: opciones duplicadas"))
  }

  # KEY_P3: Part 3 correcta debe contener frase clave
  p3_correct <- env$opciones_p3[which(env$sol_p3)]
  if (length(p3_correct) != 1)
    errs <- c(errs, "KEY_P3: no exactamente 1 correcta")
  else if (!grepl("se conoce el total de la cuenta", p3_correct))
    errs <- c(errs, "KEY_P3: frase clave ausente en correcta")

  # KEY_P4: verificar por ETIQUETA DE CONTENIDO, no por cifra formateada (D3)
  # Los 3 datos necesarios son T (total cuenta), P (presupuesto), c (consumo).
  # Usamos las etiquetas de texto para identificar, no las cifras, porque
  # E o q pueden coincidir numéricamente con T, P o c y producir falsos positivos.
  if (length(env$opciones_p4) == 5 && length(env$sol_p4) == 5) {
    for (i in seq_along(env$opciones_p4)) {
      txt <- env$opciones_p4[i]
      es_T <- grepl("total de la cuenta", txt, ignore.case = TRUE)
      es_P <- grepl("presupuesto", txt, ignore.case = TRUE)
      es_c <- grepl("consumo individual", txt, ignore.case = TRUE)
      deberia_ser_true <- es_T || es_P || es_c
      if (deberia_ser_true && !env$sol_p4[i])
        errs <- c(errs, paste0("KEY_P4: dato necesario no marcado TRUE: '",
                               substr(txt, 1, 50), "'"))
      if (!deberia_ser_true && env$sol_p4[i])
        errs <- c(errs, paste0("KEY_P4: dato no necesario marcado TRUE: '",
                               substr(txt, 1, 50), "'"))
    }
  }

  # KEY_P5: Part 5 correcta debe ser una afirmacion FALSA (D4.3)
  # Extraer los textos de las verdaderas del ENTORNO, no transcribir a mano
  p5_correct_idx <- which(env$sol_p5)
  if (length(p5_correct_idx) == 1) {
    p5_text <- env$opciones_p5[p5_correct_idx]
    # Las verdaderas son las que NO estan marcadas como correcta
    p5_verdaderas <- env$opciones_p5[-p5_correct_idx]
    for (vt in p5_verdaderas) {
      # La correcta (= la INCORRECTA que hay que marcar) NO debe coincidir
      # con ninguna de las verdaderas
      if (identical(p5_text, vt))
        errs <- c(errs, paste0("KEY_P5: correcta es identica a una verdadera"))
    }
    # Ademas: la correcta no debe contener indicadores de afirmacion verdadera.
    # Usamos los textos reales de las verdaderas seleccionadas en esta semilla.
    sel_verd <- env$sel_verdaderas
    if (!is.null(sel_verd)) {
      for (sv in sel_verd) {
        if (identical(p5_text, sv))
          errs <- c(errs, paste0("KEY_P5: correcta == verdadera seleccionada"))
      }
    }
  }

  # KEY_P6: coherencia dato_retirado vs respuesta
  p6_correct_idx <- which(env$sol_p6)
  if (length(p6_correct_idx) == 1) {
    p6_text <- env$opciones_p6[p6_correct_idx]
    if (env$dato_retirado %in% c("T", "c")) {
      if (!grepl("^No,", p6_text))
        errs <- c(errs, paste0("KEY_P6: dato_retirado=", env$dato_retirado,
                               " pero correcta no empieza con No"))
    } else if (env$dato_retirado == "n") {
      if (!grepl("^S[ií],", p6_text))
        errs <- c(errs, paste0("KEY_P6: dato_retirado=n pero correcta no empieza con Si"))
    }
  }

  errs
}

# ===== FASE 0: Enumerar combos y encontrar semilla canonica ================
cat("Fase 0: Enumerar espacio parametrico y buscar semilla canonica\n")

n_pool <- c(5, 6, 8, 10, 12, 15, 20)
P_grid <- seq(150000, 600000, by = 50000)
pn_pares <- expand.grid(P = P_grid, n = n_pool, stringsAsFactors = FALSE)
pn_pares$q <- pn_pares$P / pn_pares$n
pn_pares <- pn_pares[pn_pares$q == floor(pn_pares$q) & pn_pares$q >= 5000, ]

combos_list <- list(); counter <- 0L
for (i in seq_len(nrow(pn_pares))) {
  Pi <- pn_pares$P[i]; qi <- pn_pares$q[i]
  for (Tj in seq(Pi + 50000, min(Pi + 500000, 1200000), by = 50000)) {
    Ej <- Tj - Pi; a_upper <- floor(qi * Ej / Tj)
    if (a_upper < 500) next
    a_cands <- seq(500, min(10000, a_upper), by = 500)
    c_exact <- a_cands * Tj / Ej; c_round <- round(c_exact)
    v <- abs(c_exact - c_round) < 0.01 & (c_round %% 500) == 0 &
         c_round > 0 & c_round < qi
    for (k in which(v)) {
      counter <- counter + 1L
      combos_list[[counter]] <- c(Pi, pn_pares$n[i], Tj,
                                  a_cands[k], c_round[k], qi, Ej)
    }
  }
}
combos <- as.data.frame(do.call(rbind, combos_list))
names(combos) <- c("P", "n", "Ttotal", "a_val", "c_val", "q", "E")
N_COMBOS <- nrow(combos)

canon_idx <- which(combos$P == 300000 & combos$n == 10 &
                   combos$Ttotal == 500000 & combos$c_val == 5000)
stopifnot(length(canon_idx) >= 1)
canon_idx <- canon_idx[1]
cat("  Combos TYPE 1:", N_COMBOS, "\n")
cat("  Indice canonico:", canon_idx, "\n")

canon_seed <- NA_integer_
for (s in seq_len(500000L)) {
  set.seed(s)
  if (sample.int(N_COMBOS, 1L) != canon_idx) next
  canon_seed <- s; break
}
if (is.na(canon_seed)) stop("No se encontro semilla canonica en 500000 intentos")
cat("  Semilla canonica:", canon_seed, "\n\n")

# ===== FASE 1: Invariantes sobre N semillas ================================
cat("Fase 1: Invariantes I-1..I-5, I-7, C-1..C-4, KEY (", N_SEEDS, " semillas)\n")

errors_f1 <- character(0)
datos_retirados <- character(N_SEEDS)
E_vals <- integer(N_SEEDS)
a_vals <- integer(N_SEEDS)

# C-4 tracking: para cada gap schoice, registrar si la correcta es la unica
# mas larga y la unica mas corta
c4_longest <- list(p3 = integer(0), p5 = integer(0), p6 = integer(0))
c4_shortest <- list(p3 = integer(0), p5 = integer(0), p6 = integer(0))

for (s in seq_len(N_SEEDS)) {
  seed <- s * PRIMES[((s - 1L) %% length(PRIMES)) + 1L]
  env <- tryCatch(knit_env(RMD, seed), error = function(e) {
    errors_f1 <<- c(errors_f1, paste0("CRASH seed=", seed, ": ", conditionMessage(e)))
    NULL
  })
  if (is.null(env)) { datos_retirados[s] <- "CRASH"; next }
  datos_retirados[s] <- env$dato_retirado
  E_vals[s] <- env$E
  a_vals[s] <- env$a_val
  batch <- run_battery(env)
  if (length(batch) > 0)
    errors_f1 <- c(errors_f1, paste0("seed=", seed, " ", batch))

  # C-4 tracking
  for (pname in c("p3", "p5", "p6")) {
    sol_vec <- env[[paste0("sol_", pname)]]
    opc_vec <- env[[paste0("opciones_", pname)]]
    if (is.null(sol_vec) || is.null(opc_vec)) next
    idx_cor <- which(sol_vec)
    if (length(idx_cor) != 1) next
    nch <- nchar(opc_vec)
    is_unique_longest <- (nch[idx_cor] == max(nch)) && (sum(nch == max(nch)) == 1)
    is_unique_shortest <- (nch[idx_cor] == min(nch)) && (sum(nch == min(nch)) == 1)
    c4_longest[[pname]] <- c(c4_longest[[pname]], as.integer(is_unique_longest))
    c4_shortest[[pname]] <- c(c4_shortest[[pname]], as.integer(is_unique_shortest))
  }
}

cat("  Errores Fase 1:", length(errors_f1), "\n")
cat("  dato_retirado distribution:\n"); print(table(datos_retirados)); cat("\n")
cat("  E unique values:", length(unique(E_vals)), "\n")
cat("  a unique values:", length(unique(a_vals)), "\n")

# C-4 aggregate check
cat("\n  C-4 paridad de longitudes (sobre", N_SEEDS, "semillas):\n")
for (pname in c("p3", "p5", "p6")) {
  n_eval <- length(c4_longest[[pname]])
  pct_longest <- if (n_eval > 0) round(100 * mean(c4_longest[[pname]]), 1) else NA
  pct_shortest <- if (n_eval > 0) round(100 * mean(c4_shortest[[pname]]), 1) else NA
  cat("    ", pname, ": unica-mas-larga=", pct_longest, "%, unica-mas-corta=",
      pct_shortest, "%\n")
  if (!is.na(pct_longest) && pct_longest > 60)
    errors_f1 <- c(errors_f1, paste0("C-4_", pname, ": unica-mas-larga=",
                                     pct_longest, "% (>60%)"))
  if (!is.na(pct_shortest) && pct_shortest > 60)
    errors_f1 <- c(errors_f1, paste0("C-4_", pname, ": unica-mas-corta=",
                                     pct_shortest, "% (>60%)"))
}
cat("\n")

# ===== FASE 2: I-6 canonica (INCONDICIONAL) ================================
cat("Fase 2: I-6 instancia canonica (incondicional, seed=", canon_seed, ")\n")

errors_f2 <- character(0)
env_canon <- knit_env(RMD, canon_seed)

if (!env_canon$es_canonico)
  errors_f2 <- c(errors_f2, "I-6: seed no produjo es_canonico=TRUE")

if (env_canon$E != 200000)
  errors_f2 <- c(errors_f2, paste0("I-6: E=", env_canon$E, " esperado 200000"))
if (env_canon$a_val != 2000)
  errors_f2 <- c(errors_f2, paste0("I-6: a=", env_canon$a_val, " esperado 2000"))

enun <- env_canon$enunciado_completo
if (!grepl("Una empresa destina \\\\\\$300\\.000 para invitar a sus 10 empleados", enun))
  errors_f2 <- c(errors_f2, "I-6_ENUNCIADO: falta frase canonica p1")
if (!grepl("total de la cuenta por pagar fue de \\\\\\$500\\.000", enun))
  errors_f2 <- c(errors_f2, "I-6_ENUNCIADO: falta frase canonica T")
if (!grepl("consumió solamente un jugo de \\\\\\$5\\.000", enun))
  errors_f2 <- c(errors_f2, "I-6_ENUNCIADO: falta frase canonica c")

p3_correct <- env_canon$opciones_p3[which(env_canon$sol_p3)]
if (!grepl("se conoce el total de la cuenta", p3_correct))
  errors_f2 <- c(errors_f2, "I-6_P3: frase clave ausente")

cat("  Errores I-6:", length(errors_f2), "\n\n")

# ===== FASE 3: Mutacion A (clave falsa: sol_p3 invertido) =================
cat("Fase 3: Mutacion A (Part 3 clave falsa -- sol_p3 invertido)\n")
cat("  Sonda esperada: KEY_P3\n")

errors_f3 <- character(0)
SONDA_A <- "KEY_P3"
rmd_lines  <- readLines(RMD)
mut_a_path <- file.path(SCRATCHPAD, "mutant_A_cloze_v2.Rmd")

rmd_mut_a <- gsub(
  "sol_p3_pre <- c(TRUE, FALSE, FALSE, FALSE)",
  "sol_p3_pre <- c(FALSE, TRUE, FALSE, FALSE)",
  paste(rmd_lines, collapse = "\n"), fixed = TRUE)
writeLines(strsplit(rmd_mut_a, "\n")[[1]], mut_a_path)

env_mut_a <- tryCatch(knit_env(mut_a_path, 42L), error = function(e) {
  errors_f3 <<- c(errors_f3, paste0("CRASH: ", conditionMessage(e))); NULL
})
if (!is.null(env_mut_a)) {
  if (identical(as.logical(env_mut_a$sol_p3),
                as.logical(c(TRUE, FALSE, FALSE, FALSE)[env_mut_a$perm_p3])))
    errors_f3 <- c(errors_f3,
      "MUTANTE A MAL CONSTRUIDO: sol_p3 no cambio")

  batch_a <- run_battery(env_mut_a)
  has_sonda_a <- any(grepl(SONDA_A, batch_a))
  if (length(batch_a) == 0) {
    errors_f3 <- c(errors_f3,
      "MUTACION A NO DETECTADA: el mutante paso la bateria sin errores")
  } else if (!has_sonda_a) {
    errors_f3 <- c(errors_f3,
      paste0("MUTANTE A CAZADO POR LA SONDA EQUIVOCADA: esperaba '",
             SONDA_A, "', obtuve '", paste(batch_a, collapse = "; "), "'"))
  } else {
    cat("  Mutante A rechazado por", SONDA_A, ":\n")
    for (e in batch_a[grepl(SONDA_A, batch_a)]) cat("    ", e, "\n")
  }
}
cat("  Errores Fase 3:", length(errors_f3), "\n\n")

# ===== FASE 4: Mutacion B (I-1: E negativo post-hoc) =======================
# D4.1: SIN guarda inalcanzable. La verificacion de que la mutacion tomo
# efecto es que run_battery detecte I-1, no una condicion tautologica.
cat("Fase 4: Mutacion B (I-1: E negativo post-hoc)\n")
cat("  Sonda esperada: I-1\n")

errors_f4 <- character(0)
SONDA_B <- "I-1"

env_mut_b <- tryCatch(knit_env(RMD, 42L), error = function(e) {
  errors_f4 <<- c(errors_f4, paste0("CRASH: ", conditionMessage(e))); NULL
})
if (!is.null(env_mut_b)) {
  # Corrupt post-hoc: force E and Ttotal to make E_recalc = Ttotal - P < 0
  env_mut_b$E <- -50000L
  env_mut_b$Ttotal <- env_mut_b$P - 50000L  # D4.2: I-1 recalcula E = Ttotal - P

  batch_b <- run_battery(env_mut_b)
  has_sonda_b <- any(grepl(SONDA_B, batch_b))
  if (length(batch_b) == 0) {
    errors_f4 <- c(errors_f4,
      "MUTACION B NO DETECTADA: el mutante paso sin errores")
  } else if (!has_sonda_b) {
    errors_f4 <- c(errors_f4,
      paste0("MUTANTE B CAZADO POR LA SONDA EQUIVOCADA: esperaba '",
             SONDA_B, "', obtuve '", paste(batch_b, collapse = "; "), "'"))
  } else {
    cat("  Mutante B rechazado por", SONDA_B, ":\n")
    for (e in batch_b[grepl(SONDA_B, batch_b)]) cat("    ", e, "\n")
  }
}
cat("  Errores Fase 4:", length(errors_f4), "\n\n")

# ===== FASE 5: Mutacion C (dato_retirado=n con respuesta "No") =============
cat("Fase 5: Mutacion C (dato_retirado=n forzado con respuesta No)\n")
cat("  Sonda esperada: KEY_P6\n")

errors_f5 <- character(0)
SONDA_C <- "KEY_P6"

mut_c_seed <- NA_integer_
for (s in 1:200) {
  env_try <- tryCatch(knit_env(RMD, s), error = function(e) NULL)
  if (!is.null(env_try) && env_try$dato_retirado == "n") {
    mut_c_seed <- s; break
  }
}
if (is.na(mut_c_seed)) {
  errors_f5 <- c(errors_f5, "No se encontro semilla con dato_retirado=n")
} else {
  env_mut_c <- knit_env(RMD, mut_c_seed)

  if (env_mut_c$dato_retirado != "n")
    errors_f5 <- c(errors_f5, "MUTANTE C MAL CONSTRUIDO: dato_retirado != n")

  p6_correct_idx <- which(env_mut_c$sol_p6)
  if (length(p6_correct_idx) != 1) {
    errors_f5 <- c(errors_f5, "MUTANTE C: sol_p6 no tiene exactamente 1 TRUE")
  } else {
    p6_no_idx <- which(grepl("^No,", env_mut_c$opciones_p6))
    if (length(p6_no_idx) == 0) {
      errors_f5 <- c(errors_f5, "MUTANTE C: no hay opciones No en P6")
    } else {
      env_mut_c$sol_p6[p6_correct_idx] <- FALSE
      env_mut_c$sol_p6[p6_no_idx[1]] <- TRUE

      batch_c <- run_battery(env_mut_c)
      has_sonda_c <- any(grepl(SONDA_C, batch_c))
      if (length(batch_c) == 0) {
        errors_f5 <- c(errors_f5,
          "MUTACION C NO DETECTADA: el mutante paso sin errores")
      } else if (!has_sonda_c) {
        errors_f5 <- c(errors_f5,
          paste0("MUTANTE C CAZADO POR LA SONDA EQUIVOCADA: esperaba '",
                 SONDA_C, "', obtuve '", paste(batch_c, collapse = "; "), "'"))
      } else {
        cat("  Mutante C rechazado por", SONDA_C, ":\n")
        for (e in batch_c[grepl(SONDA_C, batch_c)]) cat("    ", e, "\n")
      }
    }
  }
}
cat("  Errores Fase 5:", length(errors_f5), "\n\n")

# ===== FASE 6: Mutacion D (corromper paridad de longitudes) ================
# Demuestra que C-4 mata un mutante que alarga la correcta de p3
cat("Fase 6: Mutacion D (corromper paridad p3 -> C-4 debe matar)\n")
cat("  Sonda esperada: C-4\n")

errors_f6 <- character(0)
SONDA_D <- "C-4"

mut_d_path <- file.path(SCRATCHPAD, "mutant_D_cloze_v2.Rmd")
# Insert extra text into the correct option of p3 to make it much longer
rmd_mut_d <- gsub(
  'opcion_p3_correcta <- paste0(\n  "Sí, porque se conoce el total de la cuenta",',
  'opcion_p3_correcta <- paste0(\n  "Sí, porque se conoce el total de la cuenta y además se dispone de toda la información complementaria necesaria que permite verificar cada componente",',
  paste(rmd_lines, collapse = "\n"), fixed = TRUE)

# If the gsub didn't match (exact whitespace), try a simpler approach
if (identical(rmd_mut_d, paste(rmd_lines, collapse = "\n"))) {
  # Simpler: just add extra text via a different substitution
  rmd_mut_d <- sub(
    '"Sí, porque se conoce el total de la cuenta"',
    '"Sí, porque se conoce el total de la cuenta y además se dispone de toda la información complementaria necesaria que permite verificar cada uno de los componentes de la fórmula del aporte proporcional al excedente"',
    paste(rmd_lines, collapse = "\n"), fixed = FALSE)
}

writeLines(strsplit(rmd_mut_d, "\n")[[1]], mut_d_path)

env_mut_d <- tryCatch(knit_env(mut_d_path, 42L), error = function(e) {
  errors_f6 <<- c(errors_f6, paste0("CRASH: ", conditionMessage(e))); NULL
})
if (!is.null(env_mut_d)) {
  batch_d <- run_battery(env_mut_d)
  has_sonda_d <- any(grepl(SONDA_D, batch_d))
  if (length(batch_d) == 0) {
    errors_f6 <- c(errors_f6,
      "MUTACION D NO DETECTADA: el mutante paso la bateria sin errores")
  } else if (!has_sonda_d) {
    errors_f6 <- c(errors_f6,
      paste0("MUTANTE D CAZADO POR LA SONDA EQUIVOCADA: esperaba '",
             SONDA_D, "', obtuve '", paste(batch_d, collapse = "; "), "'"))
  } else {
    cat("  Mutante D rechazado por", SONDA_D, ":\n")
    for (e in batch_d[grepl(SONDA_D, batch_d)]) cat("    ", e, "\n")
  }
}
cat("  Errores Fase 6:", length(errors_f6), "\n\n")

# ===== RESUMEN =============================================================
all_errors <- c(errors_f1, errors_f2, errors_f3, errors_f4, errors_f5, errors_f6)

cat("============================================\n")
if (length(all_errors) == 0) {
  cat("  RESULTADO: APROBADO (0 errores)\n")
} else {
  cat("  RESULTADO: ERRORES DETECTADOS (", length(all_errors), ")\n")
  for (e in all_errors) cat("    ", e, "\n")
}
cat("============================================\n")

quit(status = if (length(all_errors) == 0) 0L else 1L)
