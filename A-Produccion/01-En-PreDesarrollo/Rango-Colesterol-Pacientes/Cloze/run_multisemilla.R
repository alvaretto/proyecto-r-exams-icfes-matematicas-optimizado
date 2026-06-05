# Wrapper robusto: replica validar_multisemilla() sin tocar la línea sys.frame(1) rota.
repo <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
setwd(repo)
suppressWarnings(suppressMessages(source(".claude/scripts/validar_coherencia_matematica.R")))

rmd <- "A-Produccion/01-En-PreDesarrollo/Rango-Colesterol-Pacientes/Cloze/rango_colesterol_metacognitivo_interpretacion_n3_cloze_v1.Rmd"
n_semillas <- 20

parsed <- parsear_rmd(rmd)
extype <- tolower(trimws(extraer_meta(parsed$meta, "extype")))
fallos <- list()
semillas <- (1:n_semillas) * 7919

for (i in seq_along(semillas)) {
  semilla_actual <- semillas[i]
  resultado <- tryCatch({ set.seed(semilla_actual); ejecutar_chunks(parsed$chunks_r) },
    error = function(e) list(env = NULL, errores = paste0("Error global: ", conditionMessage(e)), warnings = character(0)))
  if (length(resultado$errores) > 0) {
    fallos[[length(fallos)+1]] <- list(semilla=semilla_actual, tipo="ejecucion", errores=resultado$errores); next
  }
  errores_n5 <- validar_nivel5_correctitud(parsed, resultado$env, extype, rmd)
  errores_math <- validar_coherencia_matematica_general(resultado$env)
  errores_sem <- validar_precondiciones_error_pool(resultado$env)
  errores_sem_bloq <- errores_sem[grepl("^ERR_SEM", errores_sem)]
  todos <- c(errores_n5, errores_math, errores_sem_bloq)
  if (length(todos) > 0) fallos[[length(fallos)+1]] <- list(semilla=semilla_actual, tipo="validacion", errores=todos)
}

tasa <- 1 - length(fallos)/n_semillas
cat("\n========== MULTI-SEMILLA (", n_semillas, " semillas, Nivel 5 + semantica) ==========\n", sep="")
cat("Semillas:", n_semillas, "| Fallos:", length(fallos), "| Tasa exito:", round(tasa*100,1), "%\n")
cat("APROBADO:", length(fallos)==0, "\n")
if (length(fallos) > 0) {
  for (f in fallos) { cat("Semilla", f$semilla, "(", f$tipo, "):\n  ", paste(unlist(f$errores), collapse="\n  "), "\n") }
}
quit(status = if (length(fallos)==0) 0 else 1)
