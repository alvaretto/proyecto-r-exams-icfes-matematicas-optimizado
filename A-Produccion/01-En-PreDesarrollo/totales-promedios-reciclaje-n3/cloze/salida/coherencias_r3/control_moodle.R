## control_moodle.R — mutaciones de control del verificador de Moodle: cada alteración de una COPIA del XML debe hacerlo fallar.
suppressMessages(library(xml2))
ENV <- Sys.getenv("TMPDIR", tempdir())
for (arch in c("verificar_coherencias.R", "verificar_formatos.R"))
  for (e in parse(arch, encoding = "UTF-8"))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in%
        c("nb", "limpiar", "num", "P2_PREG", "P2_OK", "P2_MAL", "P2_ANCLA", "eval_p1", "eval_p6", "verificar_moodle")) eval(e, globalenv())
orig <- normalizePath("../moodle/cloze.xml")
x <- paste(readLines(orig, encoding = "UTF-8", warn = FALSE), collapse = "\n")
corre <- function(nombre, y) {
  f <- file.path(ENV, "mutado.xml"); writeLines(y, f, useBytes = TRUE)
  r <- tryCatch(verificar_moodle(f), error = function(e) list(n = NA, fallos = paste("error:", conditionMessage(e))))
  cat(sprintf("  %-62s %s (%d fallos)\n", nombre, if (length(r$fallos) >= 1L) "CAZADA" else "NO CAZADA  <<<<<<", length(r$fallos)))
}
cat("Control sin mutar:", length(verificar_moodle(orig)$fallos), "fallos\n")
corre("Parte 3 numérica +1", sub("NUMERICAL:=192:0", "NUMERICAL:=193:0", x, fixed = TRUE))
corre("peso del gap 1: 3 -> 2", sub("{3:MULTICHOICE_V:", "{2:MULTICHOICE_V:", x, fixed = TRUE))
corre("celda «5 cursos» -> «6 cursos» (tabla 1)", sub("5 cursos", "6 cursos", x, fixed = TRUE))
## mover la marca «=» de la Parte 2 a otra opción
y <- sub("{1:MULTICHOICE_V:=Lo que recogió ese grado dividido en partes iguales entre sus cursos.~Lo que recogió cualquiera",
         "{1:MULTICHOICE_V:Lo que recogió ese grado dividido en partes iguales entre sus cursos.~=Lo que recogió cualquiera", x, fixed = TRUE)
corre("marca de la Parte 2 movida a otra opción", y)
## invertir una marca de la Parte 6
y <- sub("{1:MULTIRESPONSE:%-33.33333%Ganó el concurso el grado noveno.", "{1:MULTIRESPONSE:=Ganó el concurso el grado noveno.", x, fixed = TRUE)
corre("marca de «Ganó el concurso el grado noveno» invertida (Parte 6)", y)
corre("un marcador ##ANSWER1## sin resolver", sub("<p><strong>Parte 2.</strong>", "<p>##ANSWER1##</p><p><strong>Parte 2.</strong>", x, fixed = TRUE))
corre("gap de la Parte 3 desplazado (pregunta 4 antes de la 3)",
      sub("<p><strong>Parte 3.</strong>", "<p><strong>Parte 4.</strong> ¿Cuántos kilos?</p><p><strong>Parte 3.</strong>", x, fixed = TRUE))
