# Test de diversidad: 300 generaciones, contar versiones únicas (objetivo >= 250)
repo <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
setwd(repo)
suppressWarnings(suppressMessages(source(".claude/scripts/validar_coherencia_matematica.R")))
suppressPackageStartupMessages(library(digest))

rmd <- "A-Produccion/01-En-PreDesarrollo/Rango-Colesterol-Pacientes/Cloze/rango_colesterol_metacognitivo_interpretacion_n3_cloze_v1.Rmd"
parsed <- parsear_rmd(rmd)

N <- 300
firmas <- character(0)
fallos <- 0
sink(tempfile())  # silenciar la salida 'asis' de los chunks
for (i in 1:N) {
  set.seed(i * 131 + 7)
  res <- tryCatch(ejecutar_chunks(parsed$chunks_r), error = function(e) NULL)
  if (is.null(res) || length(res$errores) > 0 || is.null(res$env)) { fallos <- fallos + 1; next }
  e <- res$env
  firma <- digest::digest(list(
    variable      = e$ctx$variable,
    nombres       = e$nombres_individuos,
    valores       = as.vector(e$valores),
    distractor_p3 = e$distractor_p3_idx,
    afirmaciones  = e$afirmaciones_p5_texto,
    sol_p5        = e$sol_p5,
    p6_verdadero  = e$es_verdadero_p6,
    vals_extra    = e$vals_extra,
    nombre_extra  = e$nombre_extra
  ))
  firmas <- c(firmas, firma)
}
sink()

unicas <- length(unique(firmas))
generadas <- length(firmas)
cat("\n========== DIVERSIDAD ==========\n")
cat("Generaciones intentadas:", N, "\n")
cat("Generaciones válidas:   ", generadas, "\n")
cat("Fallos de ejecución:    ", fallos, "\n")
cat("Versiones únicas:       ", unicas, "/", generadas, "\n")
cat("Objetivo (>=250):       ", if (unicas >= 250) "CUMPLIDO" else "NO CUMPLIDO", "\n")
quit(status = if (unicas >= 250) 0 else 1)
