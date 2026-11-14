## preflight_hijo.R: pre-vuelo del motor de cuadernillos, en un proceso aparte.
## Lo lanza generar_cuadernillos.R en segundo plano y lo vigila leyendo el log de progreso (C9). Uso:
##   Rscript preflight_hijo.R <manifiesto.rds>
## El manifiesto trae: ejercicios (rutas absolutas), S (matriz N x Q de semillas), N y log (ruta de preflight.log).
## Por cada ejercicio j, EN UNA SOLA SESIÓN y en orden (como teje R/exams un examen real):
##   «INICIO j t=<epoch> <ruta>»; instantánea estado(); xexams(); instantánea; «FIN j tipos=… max_opciones=… estado=…».
## Si un ejercicio cambia el estado de knitr, los siguientes se tejen con el estado contaminado: eso reproduce el examen.

args <- commandArgs(TRUE)
m <- readRDS(args[1])
suppressMessages(library(exams))
Sys.setenv(MPLBACKEND = "Agg")
assign(".exams_generation_mode", TRUE, envir = globalenv())
suppressMessages(requireNamespace("tinytex", quietly = TRUE))

## línea base normalizada (C12): un ejercicio que solo RESTAURA este motor no cambia nada
knitr::knit_engines$set(python = reticulate::eng_python)

## Calibración del estado (2026-11-13), con el control negativo y con el examen real de 11 preguntas:
##   - opts_chunk: ninguna clave volátil (knitr la restaura al terminar cada documento).
##   - motores: cargar un paquete puede AÑADIR motores sin que el ejercicio toque knitr. MEDIDO: el ejercicio que carga
##     `glue` (por una dependencia) deja 3 motores nuevos (glue, glue_sql, gluesql) y no cambia ninguno de los que había.
##     Un motor nuevo no altera a los ejercicios siguientes: se informa (nuevos=…) y no es fuga.
##   - Es FUGA que un motor que YA EXISTÍA cambie de código o desaparezca (el caso real: python reemplazado).
CLAVES_VOLATILES <- character(0)

estado <- function() {
  motores <- knitr::knit_engines$get()
  oc <- knitr::opts_chunk$get()
  list(codigo = vapply(motores, function(f) paste(deparse(f), collapse = "\n"), ""),   # código, sin el entorno
       chunk  = oc[setdiff(sort(names(oc)), CLAVES_VOLATILES)])
}

comparar <- function(e0, e1) {
  antes <- names(e0$codigo); despues <- names(e1$codigo)
  quitados <- setdiff(antes, despues)
  comunes <- intersect(antes, despues)
  cambiados <- comunes[e0$codigo[comunes] != e1$codigo[comunes]]
  cambia <- c(if (length(quitados)) paste0("motores(quitados:", paste(quitados, collapse = "+"), ")"),
              if (length(cambiados)) paste0("codigo(", paste(cambiados, collapse = "+"), ")"),
              if (!identical(e0$chunk, e1$chunk)) "chunk")
  list(cambia = cambia, nuevos = setdiff(despues, antes))
}

escribir <- function(...) cat(..., "\n", file = m$log, append = TRUE)

for (j in seq_along(m$ejercicios)) {
  escribir("INICIO", j, paste0("t=", format(as.numeric(Sys.time()), nsmall = 3)), m$ejercicios[j])
  e0 <- estado()
  x <- tryCatch(
    exams::xexams(m$ejercicios[j], n = m$N, seed = m$S[, j, drop = FALSE], dir = tempfile()),
    error = function(e) e)
  if (inherits(x, "error")) {
    escribir("ERROR", j, gsub("[\r\n]+", " ", conditionMessage(x)))
    quit(status = 3)
  }
  e1 <- estado()
  tipos <- unique(vapply(x, function(v) v[[1]]$metainfo$type, ""))
  maxop <- max(vapply(x, function(v) length(v[[1]]$questionlist), 1L))   # C11: ≤ 5 con NOPS (M11b)
  cmp <- comparar(e0, e1)                                                  # qué componente cambió
  escribir("FIN", j, paste0("tipos=", paste(tipos, collapse = "/")), paste0("max_opciones=", maxop),
           if (length(cmp$cambia) == 0) "estado=igual" else paste0("estado=CAMBIA:", paste(cmp$cambia, collapse = ",")),
           if (length(cmp$nuevos)) paste0("nuevos=", paste(cmp$nuevos, collapse = ",")) else "")
}
escribir("TERMINADO")
