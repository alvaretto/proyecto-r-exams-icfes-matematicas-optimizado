#!/usr/bin/env Rscript
# Uso (desde la raíz del repo): Rscript tests/contraste_exshuffle_rexams.R
# Reproduce la cifra "0 discordancias con read_metainfo()" del Error 38
# (.claude/docs/patrones-errores-conocidos.md). No lo ejecuta run_all_tests.R: recorre todo A-Produccion.
# Contraste validador vs exams:::read_metainfo() sobre todo A-Produccion.
# read_metainfo() lee el .md tejido; sobre el .Rmd crudo aborta si exsolution/extype llevan
# R en línea o faltan. Se sustituyen por valores válidos SIN tocar exshuffle ni la sección.
e <- new.env(); sys.source("SOURCES/scripts_validacion/validar_coherencia_matematica.R", envir = e)
fs <- list.files("A-Produccion", pattern = "\\.Rmd$", recursive = TRUE, full.names = TRUE)
r <- do.call(rbind, lapply(fs, function(f) {
  l <- readLines(f, warn = FALSE, encoding = "UTF-8")
  ev <- e$evaluar_exshuffle(l, f)
  l2 <- sub("^exsolution:.*$", "exsolution: 1000", l)
  l2 <- sub("^extype:.*$", "extype: schoice", l2)
  tmp <- tempfile(fileext = ".Rmd"); writeLines(l2, tmp)
  sh <- tryCatch(suppressWarnings(exams:::read_metainfo(tmp)$shuffle), error = function(x) "ERROR")
  unlink(tmp)
  rx <- if (identical(sh, "ERROR")) "no_lee" else if (identical(sh, FALSE)) "no_mezcla" else "mezcla"
  data.frame(f = f, nuestro = ev$estado, rexams = rx)
}))
print(table(r$nuestro, r$rexams))
dice_sin_mezcla <- r$nuestro %in% c("sin_mezcla", "sin_mezcla_aceptado", "ausente", "ausente_plantilla", "fuera_de_seccion")
dice_mezcla <- r$nuestro %in% c("mezcla")
cat("discordancias: validador 'no mezcla' y R/exams mezcla:", sum(dice_sin_mezcla & r$rexams == "mezcla"),
    "| validador 'mezcla' y R/exams no:", sum(dice_mezcla & r$rexams == "no_mezcla"), "\n")
