# ============================================================
# verificar_render.R — Verificación de salud del ejercicio
# Subproyecto: plano-cartesiano-barco-n2
# ============================================================
#
# Renderiza los 4 formatos canónicos (HTML/PDF/DOCX/NOPS) + Moodle XML
# y verifica la regla #22 §P6 (fuga de la respuesta por nombre de archivo).
#
# NO reemplaza a SemilleroUnico_v2.R:
#   - SemilleroUnico_v2.R  -> EXPORTACIÓN para uso real (plantillas pcielo, 5 preguntas, webquiz).
#   - verificar_render.R   -> VERIFICACIÓN rápida de que el .Rmd no está roto (1 versión, sin plantillas).
#
# Uso:  Rscript verificar_render.R
# Sale con status 1 si algún formato falla o si hay fuga de nombre en el XML.
# ============================================================

library(exams)

rmd  <- "coordenadas_vertices_plano_cartesiano_metacognitivo_interpretacion_n2_schoice_v1.Rmd"
base <- sub("\\.Rmd$", "", rmd)
out  <- "verif_render"   # directorio transitorio, NO se commitea
dir.create(out, recursive = TRUE, showWarnings = FALSE)

ok <- list()

run <- function(etiqueta, expr) {
  cat("\n=== ", etiqueta, " ===\n", sep = "")
  r <- tryCatch({ force(expr); "OK" },
                error = function(e) paste("FAIL:", conditionMessage(e)))
  cat(etiqueta, "->", r, "\n")
  r
}

set.seed(20260728)
ok$html   <- run("HTML",   exams2html(rmd, n = 1, dir = out, edir = ".",
                                      name = paste0(base, "_h"), encoding = "UTF-8"))
ok$pdf    <- run("PDF",    exams2pdf(rmd, n = 1, dir = out, edir = ".",
                                     name = paste0(base, "_p"), encoding = "UTF-8"))
ok$docx   <- run("DOCX",   exams2pandoc(rmd, n = 1, type = "docx", dir = out, edir = ".",
                                        name = paste0(base, "_d"), encoding = "UTF-8"))
ok$nops   <- run("NOPS",   exams2nops(rep(rmd, 3), n = 1, dir = out, edir = ".",
                                      name = paste0(base, "_n"), encoding = "UTF-8",
                                      language = "es", institution = "I. E. Pedacito de Cielo",
                                      title = "Verificacion", date = Sys.Date()))
ok$moodle <- run("MOODLE", exams2moodle(rmd, n = 1, dir = out, edir = ".",
                                        name = paste0(base, "_m"), encoding = "UTF-8"))

# --- Regla #22 §P6: ningún nombre de archivo debe delatar el rol de una opción ---
# Este ejercicio tiene opciones de TEXTO y una sola figura compartida por las 4
# opciones, así que P6 no puede aplicarle por construcción. La verificación se
# mantiene igualmente como red de seguridad ante un futuro cambio a opciones gráficas.
cat("\n=== REGLA #22 §P6 (fuga por nombre de archivo) ===\n")
xml <- list.files(out, pattern = "_m\\.xml$", full.names = TRUE)
fuga <- character(0)
if (length(xml)) {
  nombres <- unique(unlist(regmatches(
    readLines(xml[1], warn = FALSE),
    gregexpr("[A-Za-z0-9_]+\\.(png|svg|jpg)", readLines(xml[1], warn = FALSE))
  )))
  cat("Archivos referenciados en el XML:", paste(nombres, collapse = ", "), "\n")
  fuga <- grep("correct|distractor|error|inv_ejes|diagonal|centro|rango",
               nombres, ignore.case = TRUE, value = TRUE)
  if (length(fuga)) cat("FUGA DETECTADA:", paste(fuga, collapse = ", "), "\n")
  else cat("Sin fuga: ningún nombre revela el rol de una opción.\n")
}

cat("\n===== RESUMEN =====\n")
for (k in names(ok)) cat(sprintf("%-8s %s\n", k, ok[[k]]))

fallos <- sum(!vapply(ok, identical, logical(1), "OK")) + length(fuga)
if (fallos > 0L) {
  cat("\nRESULTADO: FALLO (", fallos, " problema(s))\n", sep = "")
  quit(status = 1)
}
cat("\nRESULTADO: OK (5 formatos + P6 sin fuga)\n")
