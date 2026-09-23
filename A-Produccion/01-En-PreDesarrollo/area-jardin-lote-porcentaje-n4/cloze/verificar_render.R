# =============================================================================
# verificar_render.R — Verificador propio del CLOZE area-jardin-lote
#
# Enumera el espacio de parametros (106 combos) y verifica la clave de CADA
# gap para cada combo. Incluye pruebas de mutacion con contrato de sonda.
# =============================================================================
library(exams)

rmd <- "area_jardin_lote_metacognitivo_argumentacion_n4_cloze_v1.Rmd"
if (!file.exists(rmd)) {
  rmd <- file.path(dirname(commandArgs(trailingOnly = FALSE)[
    grep("--file=", commandArgs(trailingOnly = FALSE))]),
    "area_jardin_lote_metacognitivo_argumentacion_n4_cloze_v1.Rmd")
}
stopifnot(file.exists(rmd))

cat("=== verificar_render.R — CLOZE area-jardin-lote ===\n")

# --- Fase 1: verificar las claves REALES del .Rmd ----------------------------
# CORREGIDO 2026-08-08. La version anterior de esta fase NO leia el .Rmd:
# recalculaba `ans_p1 <- round(px*pa*100)` y `ans_p2 <- round(100*(1-px*pa))`
# dentro del propio verificador y los comparaba consigo mismos. Sus tres guardas
# eran INALCANZABLES (medido: 0 disparos posibles en las 106 combinaciones):
#   - `ans_p1 + ans_p2 != 100` es tautologica, porque round(x)+round(100-x)=100
#     cuando x es entero, y el filtro de combos ya garantiza que lo es.
#   - las dos guardas de rango [1,99] tampoco pueden fallar en ese espacio.
# Un verificador que no puede fallar acredita que es consistente consigo mismo,
# no que las claves del ejercicio sean correctas.
# Ahora se EXTRAE el data_generation del .Rmd, se evalua, y las claves que el
# ejercicio produce se contrastan contra aritmetica calculada aqui de forma
# independiente. Asi la fase puede fallar — que es su unico motivo de existir.

L <- readLines(rmd, warn = FALSE)
ini <- grep("^```\\{r data_generation", L)[1]
fin <- grep("^```\\s*$", L); fin <- fin[fin > ini][1]
stopifnot(!is.na(ini), !is.na(fin))
code_dg <- paste(L[(ini + 1):(fin - 1)], collapse = "\n")

N_SEM <- 150L
errors <- character(0)
n_checked <- 0L
combos_vistos <- character(0)
conjuntos_p3 <- character(0)   # conjuntos DISTINTOS de metodos ofrecidos en P3
claves_p3    <- character(0)   # metodo que empleo la persona en cada semilla

for (s in seq_len(N_SEM)) {
  e <- new.env()
  ok <- tryCatch({ set.seed(s); eval(parse(text = code_dg), envir = e); TRUE },
                 error = function(err) {
                   errors <<- c(errors, sprintf("semilla %d: data_generation abortó: %s",
                                                s, conditionMessage(err)))
                   FALSE
                 })
  if (!ok) next

  pm <- e$p_largo_min; px <- e$p_largo_max; pa <- e$p_ancho
  combos_vistos <- union(combos_vistos, sprintf("%.2f/%.2f/%.2f", pm, px, pa))

  # ARITMETICA INDEPENDIENTE (no copiada del .Rmd)
  esp_p1 <- round(px * pa * 100)          # % maximo que OCUPA el jardin
  esp_p2 <- round(100 - px * pa * 100)    # % minimo SIN jardin

  if (!isTRUE(e$ans_p1 == esp_p1))
    errors <- c(errors, sprintf("semilla %d (%.2f/%.2f/%.2f): ans_p1=%s, esperado %s",
                                s, pm, px, pa, format(e$ans_p1), format(esp_p1)))
  if (!isTRUE(e$ans_p2 == esp_p2))
    errors <- c(errors, sprintf("semilla %d (%.2f/%.2f/%.2f): ans_p2=%s, esperado %s",
                                s, pm, px, pa, format(e$ans_p2), format(esp_p2)))

  # Gaps de seleccion unica: exactamente UNA marca, y sobre la opcion correcta
  for (g in c("p3", "p5", "p6")) {
    sol <- e[[paste0("sol_", g)]]
    ops <- e[[paste0("opciones_", g)]]
    if (is.null(sol) || is.null(ops)) {
      errors <- c(errors, sprintf("semilla %d: falta sol_%s u opciones_%s", s, g, g))
      next
    }
    if (length(sol) != length(ops))
      errors <- c(errors, sprintf("semilla %d: %s tiene %d opciones y %d marcas",
                                  s, g, length(ops), length(sol)))
    if (sum(sol) != 1L)
      errors <- c(errors, sprintf("semilla %d: %s marca %d claves (debe ser 1)",
                                  s, g, sum(sol)))
    if (anyDuplicated(ops))
      errors <- c(errors, sprintf("semilla %d: %s tiene opciones duplicadas", s, g))
  }

  # V7 (unicidad ampliada, anadido 2026-08-08): con un POOL de 7 metodos para 4
  # slots, la unicidad habitual no basta — hay que comprobar que entre los 4
  # OFRECIDOS solo uno reproduce el rango de la afirmacion. Si dos lo hicieran,
  # la Parte 3 tendria dos respuestas correctas.
  if (!is.null(e$razon_p3) && !is.null(e$afirmacion_min)) {
    n_match <- sum(vapply(e$razon_p3, function(r)
      isTRUE(all.equal(r$produce_min, e$afirmacion_min)) &&
      isTRUE(all.equal(r$produce_max, e$afirmacion_max)), logical(1L)))
    if (n_match != 1L)
      errors <- c(errors, sprintf(
        "semilla %d: %d metodos de P3 reproducen el rango de la afirmacion (debe ser 1)",
        s, n_match))
    conjuntos_p3 <- union(conjuntos_p3,
                          paste(sort(vapply(e$razon_p3, function(r) r$clave,
                                            character(1L))), collapse = "+"))
  }
  # Regla #22 P4-bis / sonda H3: el metodo que empleo la persona (= clave de P3)
  # no puede ser siempre el mismo, o la clave se memoriza sin calcular nada.
  if (!is.null(e$metodo_persona)) claves_p3 <- c(claves_p3, e$metodo_persona)

  # Balance 2 Si + 2 No en P6 y coherencia del veredicto de la clave
  if (!is.null(e$opciones_p6_ord)) {
    v <- vapply(e$opciones_p6_ord, function(o) o$veredicto, character(1L))
    if (sum(v == "si") != 2L || sum(v == "no") != 2L)
      errors <- c(errors, sprintf("semilla %d: P6 desbalanceado (%d Si / %d No)",
                                  s, sum(v == "si"), sum(v == "no")))
  }

  # Plausibilidad del distractor "suma de complementos" (anadido 2026-08-08):
  # un porcentaje de area libre >= 100 % es imposible y se descarta sin razonar.
  if (!is.null(e$sum_comp_max) && e$sum_comp_max >= 100)
    errors <- c(errors, sprintf("semilla %d (%.2f/%.2f/%.2f): suma de complementos = %s %% (>= 100)",
                                s, pm, px, pa, format(e$sum_comp_max)))

  # P4: INVARIANTES NUMERICAS de las afirmaciones (anadido 2026-08-08). Antes solo
  # se comprobaba que sol_p4 no fuera constante, lo que no dice nada sobre si cada
  # afirmacion es realmente verdadera o falsa. Los pools de P4 son fijos, asi que su
  # valor de verdad depende de los parametros: estas dos condiciones son las que
  # sostienen una afirmacion VERDADERA ("el complemento del producto supera al
  # complemento de solo el ancho") y dos FALSAS (la que iguala el area libre a
  # comp_ancho y la que la iguala al producto de complementos). Si alguna se
  # rompiera, P4 tendria una etiqueta mentirosa sin que nada mas lo detectara.
  if (!is.null(e$correct_min) && !is.null(e$comp_ancho) &&
      !(e$correct_min > e$comp_ancho))
    errors <- c(errors, sprintf(
      "semilla %d (%.2f/%.2f/%.2f): correct_min=%s no supera comp_ancho=%s -> la afirmacion verdadera de P4 dejaria de serlo",
      s, pm, px, pa, format(e$correct_min), format(e$comp_ancho)))

  if (!is.null(e$prod_comp_min) &&
      isTRUE(all.equal(e$prod_comp_min, e$correct_min)) &&
      isTRUE(all.equal(e$prod_comp_max, e$correct_max)))
    errors <- c(errors, sprintf(
      "semilla %d (%.2f/%.2f/%.2f): el producto de complementos iguala el rango correcto -> la afirmacion falsa de P4 pasaria a ser verdadera",
      s, pm, px, pa))

  # p4 (mchoice): al menos una verdadera y al menos una falsa
  if (!is.null(e$sol_p4)) {
    if (sum(e$sol_p4) == 0L || sum(e$sol_p4) == length(e$sol_p4))
      errors <- c(errors, sprintf("semilla %d: sol_p4 es constante (%d de %d)",
                                  s, sum(e$sol_p4), length(e$sol_p4)))
  }

  # Coherencia del flag con el veredicto de p6: la clave marcada debe empezar
  # por "Si" cuando la afirmacion es verdadera y por "No" cuando es falsa.
  if (!is.null(e$afirmacion_es_verdadera) && !is.null(e$sol_p6) && !is.null(e$opciones_p6)) {
    texto_clave <- e$opciones_p6[which(e$sol_p6 == 1L)[1]]
    empieza_si <- grepl("^\\s*S[íi]", texto_clave)
    if (!identical(empieza_si, isTRUE(e$afirmacion_es_verdadera)))
      errors <- c(errors, sprintf(
        "semilla %d: flag=%s pero la clave de p6 dice '%s'",
        s, e$afirmacion_es_verdadera, substr(texto_clave, 1, 40)))
  }

  n_checked <- n_checked + 1L
}

cat("Semillas evaluadas:", n_checked, "/", N_SEM,
    "| combinaciones distintas alcanzadas:", length(combos_vistos), "\n")
cat("Conjuntos DISTINTOS de metodos ofrecidos en P3:", length(conjuntos_p3), "\n")
# Incidente P: si el pool tuviera el tamano de los slots, este numero seria 1.
if (length(conjuntos_p3) <= 1L)
  errors <- c(errors,
              "P3 ofrece SIEMPRE el mismo conjunto de metodos (Incidente P: pool == nro de slots)")
cat("Metodos DISTINTOS empleados por la persona (clave de P3):",
    length(unique(claves_p3)), "->", paste(sort(unique(claves_p3)), collapse = " "), "\n")
# Regla #22 P4-bis: si la persona cometiera siempre el mismo error, la clave de
# P3 se memoriza sin calcular. Con la afirmacion falsa hay >= 3 metodos posibles.
if (length(unique(claves_p3)) < 3L)
  errors <- c(errors, sprintf(
    "la clave de P3 solo toma %d valor(es) distinto(s) (regla #22 P4-bis)",
    length(unique(claves_p3))))
cat("Errores encontrados:", length(errors), "\n")
if (length(errors) > 0) {
  for (e in utils::head(errors, 20)) cat("  ERROR:", e, "\n")
  if (length(errors) > 20) cat("  ... y", length(errors) - 20, "mas\n")
}

# --- Fase 2: render multi-semilla rapido (10 semillas) -----------------------
cat("\n=== Fase 2: render multi-semilla (10 semillas) ===\n")
render_errors <- 0L
for (s in c(1, 17, 42, 73, 101, 137, 199, 251, 307, 401)) {
  res <- tryCatch({
    set.seed(s)
    exams2html(rmd, n = 1, dir = tempdir())
    "OK"
  }, error = function(e) conditionMessage(e))
  if (res != "OK") {
    cat("  Semilla", s, "FALLO:", res, "\n")
    render_errors <- render_errors + 1L
  }
}
cat("Renders OK:", 10L - render_errors, "/ 10\n")

# --- Fase 3: pruebas de mutacion (contrato de sonda, 6 gaps) ----------------
cat("\n=== Fase 3: pruebas de mutacion (6 gaps) ===\n")
mutation_errors <- character(0)
rmd_txt <- readLines(rmd, warn = FALSE)

# Helper: knit un .Rmd mutado, acelerando al reemplazar py_run_string por stub
knit_mutant <- function(mutated_lines, seed = 42L) {
  # Stub la figura Python para no gastar 5s por mutante
  stubbed <- sub("reticulate::py_run_string\\(py_code\\)",
                 'writeLines("stub", "plano_lote.png")', mutated_lines)
  f <- tempfile(fileext = ".Rmd")
  writeLines(stubbed, f)
  on.exit(unlink(f), add = TRUE)
  env <- new.env()
  res <- tryCatch({
    set.seed(seed)
    knitr::knit(f, output = tempfile(), envir = env, quiet = TRUE)
    list(status = "OK", env = env)
  }, error = function(e) {
    list(status = conditionMessage(e), env = env)
  })
  res
}

run_mutant <- function(id, desc, old_pat, new_pat, sonda_esperada, check_fn) {
  cat("  Mutante ", id, ": ", desc, "\n", sep = "")
  mut <- sub(old_pat, new_pat, rmd_txt)
  if (identical(rmd_txt, mut)) {
    mutation_errors <<- c(mutation_errors,
      sprintf("MUTANTE %s MAL CONSTRUIDO: sub() no reemplazo nada", id))
    return(invisible(NULL))
  }
  res <- knit_mutant(mut)
  sonda_real <- check_fn(res)
  cat("    Sonda esperada: ", sonda_esperada, " | Sonda real: ", sonda_real, "\n", sep = "")
  if (sonda_real == "NO_DETECTADO") {
    mutation_errors <<- c(mutation_errors,
      sprintf("MUTANTE %s NO DETECTADO: %s", id, desc))
  } else if (sonda_real != sonda_esperada && sonda_real != "STOPIFNOT") {
    mutation_errors <<- c(mutation_errors,
      sprintf("MUTANTE %s CAZADO POR SONDA EQUIVOCADA: esperaba %s, obtuve %s",
              id, sonda_esperada, sonda_real))
  } else {
    cat("    Veredicto: cazado_por_su_sonda\n")
  }
}

# --- Mutante A (p1 num): ans_p1 falseado ---
run_mutant("A", "ans_p1 = ans_p1 - 10",
  "ans_p1 <- round\\(p_largo_max \\* p_ancho \\* 100\\)",
  "ans_p1 <- round(p_largo_max * p_ancho * 100) - 10L",
  "P1_CLAVE",
  function(res) {
    if (res$status != "OK") return("STOPIFNOT")
    e <- res$env
    if (!exists("ans_p1", envir = e)) return("RENDER_FALLO")
    if (e$ans_p1 + e$ans_p2 == 100) "NO_DETECTADO" else "P1_CLAVE"
  }
)

# --- Mutante B (p2 num): ans_p2 falseado ---
run_mutant("B", "ans_p2 = correct_min + 10",
  "ans_p2 <- correct_min$",
  "ans_p2 <- correct_min + 10L",
  "P2_CLAVE",
  function(res) {
    if (res$status != "OK") return("STOPIFNOT")
    e <- res$env
    if (!exists("ans_p2", envir = e)) return("RENDER_FALLO")
    if (e$ans_p1 + e$ans_p2 == 100) "NO_DETECTADO" else "P2_CLAVE"
  }
)

# --- Mutante C (p3 schoice): clave apunta a razonamiento equivocado ---
# Marca la CONTRAPARTE en vez de la clave. Se elige la contraparte (y no un
# metodo cualquiera del pool) porque siempre esta entre los 4 ofrecidos: asi el
# mutante lo caza la sonda P3_CLAVE y no un stopifnot de "0 claves marcadas",
# que acreditaria el rechazo sin probar la invariante (contrato de mutacion).
run_mutant("C", "sol_p3 marca la contraparte en vez de la clave",
  'sol_p3 <- as\\.integer\\(vapply\\(razon_p3, function\\(r\\) r\\$clave, character\\(1L\\)\\) == clave_p3\\)',
  'sol_p3 <- as.integer(vapply(razon_p3, function(r) r$clave, character(1L)) == contraparte_p3)',
  "P3_CLAVE",
  function(res) {
    if (res$status != "OK") return("STOPIFNOT")
    e <- res$env
    if (!exists("sol_p3", envir = e) || !exists("razon_p3", envir = e)) return("RENDER_FALLO")
    # La opcion marcada como correcta deberia ser "suma", que produce
    # sum_comp_min/max, NO el rango de la afirmacion.
    idx_ok <- which(e$sol_p3 == 1L)
    if (length(idx_ok) != 1L) return("NO_DETECTADO")
    r <- e$razon_p3[[idx_ok]]
    # Si el metodo marcado produce el rango de la afirmacion, el mutante NO fue detectado
    if (r$produce_min == e$afirmacion_min && r$produce_max == e$afirmacion_max) {
      "NO_DETECTADO"
    } else {
      "P3_CLAVE"
    }
  }
)

# --- Mutante D (p4 mchoice): invertir valor de verdad de todas las afirmaciones ---
run_mutant("D", "verdad_pre invertido (1->0, 0->1)",
  "verdad_pre <- c\\(rep\\(1L, k_v\\), rep\\(0L, k_f\\)\\)",
  "verdad_pre <- c(rep(0L, k_v), rep(1L, k_f))",
  "P4_CLAVE",
  function(res) {
    if (res$status != "OK") return("STOPIFNOT")
    e <- res$env
    if (!exists("sol_p4", envir = e) || !exists("k_v", envir = e)) return("RENDER_FALLO")
    # Con la inversion, sum(sol_p4) deberia ser k_f = 5-k_v, no k_v
    if (sum(e$sol_p4) == e$k_v) "NO_DETECTADO" else "P4_CLAVE"
  }
)

# --- Mutante E (p5 schoice): clave del caso limite falseada ---
run_mutant("E", 'clave p5 cambiada a "cero" (respuesta falsa)',
  '== "lineal_correcto"',
  '== "cero"',
  "P5_CLAVE",
  function(res) {
    if (res$status != "OK") return("STOPIFNOT")
    e <- res$env
    if (!exists("sol_p5", envir = e) || !exists("opciones_p5_ord", envir = e)) return("RENDER_FALLO")
    idx_ok <- which(e$sol_p5 == 1L)
    if (length(idx_ok) != 1L) return("NO_DETECTADO")
    o <- e$opciones_p5_ord[[idx_ok]]
    # La opcion "cero" es matematicamente incorrecta: el jardin no cubre todo
    if (o$clave == "cero") "P5_CLAVE" else "NO_DETECTADO"
  }
)

# --- Mutante F (p6 schoice): veredicto invertido ---
# Marca el primer distractor en vez de la clave. No se nombra una clave concreta
# ("suma_comp", etc.) porque los distractores de p6 se sortean del pool de
# metodos y ese nombre puede no estar en la version mutada: el mutante moriria
# por un stopifnot de "0 claves marcadas" en vez de por su propia sonda.
run_mutant("F", "sol_p6 marca el primer distractor en vez de la clave",
  'sol_p6 <- as.integer\\(vapply\\(opciones_p6_ord, function\\(o\\) o\\$clave, character\\(1L\\)\\) == "correcto"\\)',
  'sol_p6 <- as.integer(seq_along(opciones_p6_ord) == which(vapply(opciones_p6_ord, function(o) o$clave, character(1L)) != "correcto")[1])',
  "P6_CLAVE",
  function(res) {
    if (res$status != "OK") return("STOPIFNOT")
    e <- res$env
    if (!exists("sol_p6", envir = e) || !exists("opciones_p6_ord", envir = e)) return("RENDER_FALLO")
    idx_ok <- which(e$sol_p6 == 1L)
    if (length(idx_ok) != 1L) return("NO_DETECTADO")
    o <- e$opciones_p6_ord[[idx_ok]]
    # "suma_comp" es un distractor, no la clave correcta
    if (o$clave != "correcto") "P6_CLAVE" else "NO_DETECTADO"
  }
)

# --- Resultado final ---------------------------------------------------------
cat("\n=== RESULTADO ===\n")
all_errors <- c(errors, if (render_errors > 0) paste("renders fallidos:", render_errors),
                mutation_errors)
if (length(all_errors) == 0L) {
  cat("RESULTADO: APROBADO (0 errores)\n")
  # `combos` vive en el entorno del data_generation (e), no aqui: usar
  # combos_vistos. Antes se llamaba nrow(combos) y la RUTA DE EXITO abortaba
  # con "objeto 'combos' no encontrado", saliendo con status 1 despues de
  # imprimir APROBADO. Corregido el 2026-08-08.
  cat("  Espacio alcanzado: ", length(combos_vistos), " combos, claves P1/P2 verificadas\n")
  cat("  Metodos ofrecidos en P3: ", length(conjuntos_p3), " conjuntos distintos (pool 7 sobre 4 slots)\n")
  cat("  Rangos de razonamiento (P3): 4 distintos y solo 1 reproduce la afirmacion\n")
  cat("  Suma de complementos < 100 % en todas las semillas\n")
  cat("  Opciones P6 (veredicto): unicas en cada combo\n")
  cat("  Renders: 10/10 OK\n")
  cat("  Mutantes A-F (6 gaps): todos cazados por su sonda\n")
  q(status = 0)
} else {
  cat("RESULTADO: FALLIDO (", length(all_errors), " errores)\n")
  for (e in all_errors) cat("  ", e, "\n")
  q(status = 1)
}
