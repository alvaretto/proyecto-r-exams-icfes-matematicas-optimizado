# Revisión profunda P1-P6: recomputa la VERDAD de cada parte desde los datos
# crudos (valores) y la compara con lo que el ejercicio marca/muestra.
# Detecta data-vs-sol y data-vs-texto que las verificaciones de plantilla no ven.
repo <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
setwd(repo)
suppressWarnings(suppressMessages(source(".claude/scripts/validar_coherencia_matematica.R")))
rmd <- "A-Produccion/01-En-PreDesarrollo/Rango-Colesterol-Pacientes/Cloze/rango_colesterol_metacognitivo_interpretacion_n3_cloze_v1.Rmd"
parsed <- parsear_rmd(rmd)
cod <- parse(text = paste(parsed$chunks_r[[2]], collapse = "\n"))

N <- 300
fallos <- list()
add <- function(s, msg) fallos[[length(fallos)+1]] <<- paste0("seed ", s, ": ", msg)

for (s in 1:N) {
  set.seed(s)
  e <- tryCatch({ env <- new.env(parent = globalenv()); eval(cod, envir = env); env },
               error = function(err) NULL)
  if (is.null(e)) { add(s, "ejecución falló"); next }

  v <- e$valores
  rng <- apply(v, 1, function(x) max(x) - min(x))   # verdad cruda
  mx  <- apply(v, 1, max); mn <- apply(v, 1, min)
  idx_min <- which(rng == min(rng)); idx_max <- which(rng == max(rng))

  # ---- P1: sol marca al de menor rango ----
  if (sum(e$sol_p1) != 1) add(s, "P1 sol != 1")
  if (which(e$sol_p1 == 1) != e$idx_menor_rango) add(s, "P1 sol no apunta a idx_menor_rango")
  if (!(e$idx_menor_rango %in% idx_min)) add(s, "P1 idx_menor_rango no es min(rango) real")
  if (!identical(as.character(e$opciones_p1), as.character(e$nombres_individuos))) add(s, "P1 opciones != nombres")

  # ---- P2: valor menor rango == min real == rango del correcto ----
  if (e$valor_rango_menor != min(rng)) add(s, paste0("P2 valor_rango_menor=", e$valor_rango_menor, " != min real ", min(rng)))
  if (e$valor_rango_menor != rng[e$idx_menor_rango]) add(s, "P2 != rango del paciente P1")

  # ---- P3: error correcto selecciona al distractor; distractor != correcto ----
  if (e$distractor_p3_idx == e$idx_menor_rango) add(s, "P3 distractor == correcto P1")
  cod2idx <- c("EST-RAN-01"=e$idx_error_ran01, "EST-RAN-05"=e$idx_error_ran05,
               "EST-RAN-06"=e$idx_error_ran06, "EST-RAN-02"=e$idx_error_ran02,
               "EST-RAN-03"=e$idx_error_ran03)
  if (cod2idx[[e$error_p3_codigo]] != e$distractor_p3_idx)
    add(s, paste0("P3 error correcto ", e$error_p3_codigo, " no selecciona al distractor"))
  if (sum(e$sol_p3) != 1) add(s, "P3 sol != 1")
  if (e$codigos_p3_opciones[which(e$sol_p3 == 1)] != e$error_p3_codigo) add(s, "P3 sol no marca error correcto")
  # claims "no explica": cada opción mostrada != correcta cuyo índice == distractor => sería falso claim
  for (k in seq_along(e$codigos_p3_opciones)) {
    cc <- e$codigos_p3_opciones[k]
    if (cc != e$error_p3_codigo && cod2idx[[cc]] == e$distractor_p3_idx)
      add(s, paste0("P3 opción ", cc, " también selecciona al distractor (claim 'no explica' sería falso)"))
  }

  # ---- P4: valor mayor rango == max real ----
  if (e$valor_rango_mayor != max(rng)) add(s, paste0("P4 valor_rango_mayor=", e$valor_rango_mayor, " != max real ", max(rng)))

  # ---- P5: recomputar verdad de cada afirmación seleccionada ----
  for (i in seq_along(e$afirmaciones_p5_texto)) {
    t <- e$afirmaciones_p5_texto[i]; marca <- e$sol_p5[i] == 1
    verdad <- NA
    if (grepl("se calcula como la diferencia entre el valor máximo y el valor mínimo", t)) verdad <- TRUE
    else if (grepl("promediando el valor", t)) verdad <- FALSE
    else if (grepl("menor rango es", t) && grepl("estables", t)) verdad <- TRUE
    else if (grepl("rango más grande", t) && grepl("variabilidad", t)) verdad <- TRUE
    else if (grepl("varían poco entre sí", t) && grepl("independientemente", t)) verdad <- TRUE
    else if (grepl("aunque este", t) && grepl("valor individual más bajo", t)) verdad <- TRUE
    else if (grepl("se expresa en las mismas unidades", t)) verdad <- TRUE
    else if (grepl("porque su valor máximo", t) && grepl("más bajo de los cuatro", t)) verdad <- FALSE
    else if (grepl("basta con restar el valor del primer control", t)) verdad <- FALSE
    else if (grepl("aumentaran en 10", t)) verdad <- FALSE
    else if (grepl("necesariamente tiene el menor rango", t)) verdad <- FALSE
    else if (grepl("aproximadamente el mismo rango", t)) verdad <- FALSE
    if (is.na(verdad)) add(s, paste0("P5 afirmación no clasificable: ", substr(t,1,40)))
    else if (verdad != marca) add(s, paste0("P5 marca=", marca, " verdad=", verdad, " :: ", substr(t,1,45)))
    # Verificar números embebidos clave
    m <- regmatches(t, regexpr("rango más grande \\(([0-9]+)", t))
    if (length(m)) { num <- as.integer(sub(".*\\(", "", m)); if (num != max(rng)) add(s, "P5 'rango más grande' número != max real") }
  }
  nv <- sum(e$sol_p5); if (nv < 2 || (length(e$sol_p5)-nv) < 2) add(s, "P5 balance V/F < 2")

  # ---- P6: relación rango_extra y números del enunciado ----
  re_real <- max(e$vals_extra) - min(e$vals_extra)
  if (re_real != e$rango_extra) add(s, "P6 rango_extra inconsistente")
  if (e$es_verdadero_p6 != (re_real > max(rng))) add(s, "P6 es_verdadero no coincide con relación real")
  if (e$rango_mas_grande != max(rng)) add(s, "P6 rango_mas_grande != max real")
  if (any(e$vals_extra < e$rango_var[1]) || any(e$vals_extra > e$rango_var[2])) add(s, "P6 vals_extra fuera de rango")
  if (length(e$vals_extra) != e$n_controles) add(s, "P6 vals_extra longitud != n_controles")
  if (sum(e$sol_p6) != 1) add(s, "P6 sol != 1")
}

cat("\n========== REVISIÓN PROFUNDA P1-P6 (", N, " semillas) ==========\n", sep="")
cat("Hallazgos:", length(fallos), "\n")
if (length(fallos)) for (f in head(fallos, 40)) cat("  -", f, "\n") else cat("  TODAS LAS PARTES CORRECTAS — 0 inconsistencias data/sol/texto\n")
