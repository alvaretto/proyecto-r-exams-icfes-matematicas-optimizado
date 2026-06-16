# Tests de regresión: soporte de _neg_ Variante B + params propio en el validador
# ----------------------------------------------------------------------------
# Fija el comportamiento del fix 2026-06-16 a validar_coherencia_matematica.R:
#
#   1. validar_5c_unicidad (Nivel 5C): distingue
#        - Variante B (texto sinónimo: etiquetas correcta*/error, textos distintos
#          por diseño)  -> NO debe exigir hashes idénticos
#        - Variante A (datos idénticos: (N-1) iguales + 1 diferente) -> sin cambios
#   2. construir_params_desde_env (Capa A / ERR_SEM_A): respeta el objeto `params`
#      que el propio ejercicio declara (p.ej. funciones lineales con m, b), además
#      del schema estadístico (n, datos_ord).
#
# Origen: grafica_funcion_lineal_..._schoice_neg_v1 (SCHOICE _neg_ Variante B) que
# generaba falsos positivos ERR_ANS_C y ERR_SEM_A antes del fix.
# Regla: .claude/rules/validacion-neg-opciones-repetidas.md (Variantes A y B)

library(testthat)
library(exams)
library(digest)

source("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/validar_coherencia_matematica.R")

# Helper: escribe un .Rmd temporal y devuelve su ruta. El prefijo controla si el
# nombre contiene "_neg_" (necesario para activar la rama _neg_ del validador).
.rmd_temp <- function(chunk_lines, prefix = "test_neg_", exname = "test_neg_vb") {
  f <- tempfile(pattern = prefix, fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    chunk_lines,
    "```",
    "",
    "Question",
    "========",
    "Cual afirmacion NO es correcta?",
    "",
    "Answerlist",
    "----------",
    "* A",
    "* B",
    "* C",
    "* D",
    "",
    "Solution",
    "========",
    "La afirmacion incorrecta es la que viola la propiedad.",
    "",
    "Meta-information",
    "================",
    paste0("exname: ", exname),
    "extype: schoice",
    "exsolution: 0100",
    "exshuffle: TRUE",
    "exextra[Type]: SCHOICE",
    "exextra[Competencia]: Argumentacion",
    "exextra[Componente]: Numerico-variacional",
    "exextra[Afirmacion]: Test",
    "exextra[Evidencia]: Test",
    "exextra[Nivel]: 3"
  ), f)
  f
}

# ============================================================
# Nivel 5C — Variante B (texto sinónimo)
# ============================================================

test_that("5C Variante B: 4 textos distintos + etiquetas correcta*/error NO disparan ERR_ANS_C", {
  f <- .rmd_temp(c(
    "opciones_mezcladas <- list('La pendiente es 3', 'El corte y es -2',",
    "                            'La raiz es x=1', 'AFIRMACION FALSA distinta')",
    "etiquetas_mezcladas <- c('correcta1', 'correcta2', 'correcta3', 'error')",
    "sol <- c(0, 0, 0, 1)"
  ))
  result <- validar_coherencia_matematica(f)
  expect_false(any(grepl("ERR_ANS_C", result$errores)),
    info = paste("Variante B válida no debe disparar ERR_ANS_C. Errores:",
                 paste(result$errores, collapse = "; ")))
  unlink(f)
})

test_that("5C Variante B: estructura semántica inválida (2 'error') dispara ERR_ANS_C", {
  f <- .rmd_temp(c(
    "opciones_mezcladas <- list('texto uno', 'texto dos', 'texto tres', 'texto cuatro')",
    "etiquetas_mezcladas <- c('error', 'error', 'correcta1', 'correcta2')",
    "sol <- c(1, 0, 0, 0)"
  ))
  result <- validar_coherencia_matematica(f)
  expect_true(any(grepl("ERR_ANS_C", result$errores)),
    info = "Variante B con 2 etiquetas 'error' debe disparar ERR_ANS_C (estructura inválida)")
  unlink(f)
})

test_that("5C Variante B: textos duplicados (copiar-pegar) disparan ERR_ANS_C", {
  f <- .rmd_temp(c(
    "opciones_mezcladas <- list('texto identico', 'texto identico',",
    "                            'texto distinto', 'afirmacion falsa')",
    "etiquetas_mezcladas <- c('correcta1', 'correcta2', 'correcta3', 'error')",
    "sol <- c(0, 0, 0, 1)"
  ))
  result <- validar_coherencia_matematica(f)
  expect_true(any(grepl("ERR_ANS_C", result$errores)),
    info = "Variante B con textos idénticos (copiar-pegar) debe disparar ERR_ANS_C")
  unlink(f)
})

test_that("5C Variante B: detección vía nombres de opciones_pre_mezcla (sin etiquetas_mezcladas)", {
  f <- .rmd_temp(c(
    "opciones_pre_mezcla <- list(correcta1='texto a', correcta2='texto b',",
    "                             correcta3='texto c', error='texto d')",
    "opciones_mezcladas <- opciones_pre_mezcla",
    "sol <- c(0, 0, 0, 1)"
  ))
  result <- validar_coherencia_matematica(f)
  expect_false(any(grepl("ERR_ANS_C", result$errores)),
    info = "Variante B detectada por nombres de opciones_pre_mezcla no debe disparar ERR_ANS_C")
  unlink(f)
})

# ============================================================
# Nivel 5C — Variante A (datos idénticos): comportamiento preservado
# ============================================================

test_that("5C Variante A: (N-1) datos idénticos + 1 diferente NO dispara ERR_ANS_C", {
  f <- .rmd_temp(c(
    "opciones_mezcladas <- list(list(v=1), list(v=1), list(v=1), list(v=2))",
    "sol <- c(0, 0, 0, 1)"
  ))
  result <- validar_coherencia_matematica(f)
  expect_false(any(grepl("ERR_ANS_C", result$errores)),
    info = paste("Variante A válida (3 idénticos + 1) no debe disparar ERR_ANS_C. Errores:",
                 paste(result$errores, collapse = "; ")))
  unlink(f)
})

test_that("5C Variante A: 4 datos distintos (sin etiquetas) SÍ dispara ERR_ANS_C", {
  f <- .rmd_temp(c(
    "opciones_mezcladas <- list(list(v=1), list(v=2), list(v=3), list(v=4))",
    "sol <- c(0, 0, 0, 1)"
  ))
  result <- validar_coherencia_matematica(f)
  expect_true(any(grepl("ERR_ANS_C", result$errores)),
    info = "Variante A con 4 datos distintos debe disparar ERR_ANS_C (no cumple (N-1)+1)")
  unlink(f)
})

# ============================================================
# Capa A (ERR_SEM_A): construir_params_desde_env respeta `params` del ejercicio
# ============================================================

test_that("Capa A: precondición con params propio (m,b) NO dispara ERR_SEM_A (fix function-params)", {
  # Sin el fix, params estaría vacío -> params$m es NULL -> precondicion da logical(0)
  # -> falso ERR_SEM_A. Con el fix, se respeta el objeto `params` del ejercicio.
  f <- .rmd_temp(c(
    "errores_conceptuales <- list(list(",
    "  codigo = 'FUN-TEST-01', nombre = 'Signo de pendiente',",
    "  descripcion_corta = 'Cambia el signo de la pendiente',",
    "  descripcion_larga = 'Afirma que la recta tiene direccion contraria a la real.',",
    "  precondicion = function(params) params$m != 0L))",
    "error_sel <- errores_conceptuales[[1]]",
    "params <- list(m = 3L, b = 5L, corte_x = -2L)",
    "opciones_mezcladas <- c(10, 20, 30, 40)",
    "sol <- c(0, 1, 0, 0)"
  ), prefix = "file", exname = "test_capa_a_params")
  result <- validar_coherencia_matematica(f)
  expect_false(any(grepl("ERR_SEM_A", result$errores)),
    info = paste("Precondición que usa params$m (con params del ejercicio) no debe dar ERR_SEM_A.",
                 "Errores:", paste(result$errores, collapse = "; ")))
  unlink(f)
})

test_that("Capa A: precondición genuinamente incumplida SÍ dispara ERR_SEM_A (guarda de regresión)", {
  # El fix no debe silenciar detecciones reales: aquí la precondición no se cumple.
  f <- .rmd_temp(c(
    "errores_conceptuales <- list(list(",
    "  codigo = 'FUN-TEST-02', nombre = 'Caso no aplicable',",
    "  descripcion_corta = 'Error que requiere m igual a cero',",
    "  descripcion_larga = 'Solo aplica cuando la pendiente vale cero.',",
    "  precondicion = function(params) params$m == 0L))",
    "error_sel <- errores_conceptuales[[1]]",
    "params <- list(m = 3L, b = 5L)",  # m != 0 -> precondicion FALSE -> ERR_SEM_A esperado
    "opciones_mezcladas <- c(10, 20, 30, 40)",
    "sol <- c(0, 1, 0, 0)"
  ), prefix = "file", exname = "test_capa_a_fail")
  result <- validar_coherencia_matematica(f)
  expect_true(any(grepl("ERR_SEM_A", result$errores)),
    info = "Precondición incumplida (m==0 con m=3) debe seguir disparando ERR_SEM_A")
  unlink(f)
})
