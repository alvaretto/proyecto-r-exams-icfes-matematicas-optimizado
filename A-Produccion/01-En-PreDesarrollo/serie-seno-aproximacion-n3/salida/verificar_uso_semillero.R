# Guarda de uso del semillero de serie-seno-aproximacion-n3 (regla #26, material del aula).
# Uso: Rscript --no-init-file salida/verificar_uso_semillero.R   (desde cualquier carpeta)
#
# (a) Literales: SemilleroUnico_v2.R y SemilleroMoodle_v2.R apuntan al .Rmd del ejercicio, conservan el tope de 100,
#     el seed de Moodle lo lleva SemilleroMoodle_v2.R, y SemilleroUnico_v2.R lleva el paso de pdfjam a oficio.
# (b) Render real en un directorio temporal: PDF de preguntas con solpcielo.tex y NOPS (exams2nops + el mismo bucle
#     de pdfjam que trae el semillero). Se miden con pdfinfo: oficio = 216 x 330 mm (612 x 935-936 pt).
# (c) Mutante: copia del semillero sin el paso de pdfjam; (a) debe fallar.
#
# Salida: una linea por falla "FALLA <codigo>" y una linea final VEREDICTO.
# Exit: 0 todo en regla; 1 falla de literales (a); 2 falla del render o de las medidas (b); 3 el mutante no se detecta (c);
#       4 no se pudo ejecutar (falta pdfjam, pdfinfo o Rscript, o no se encontro un archivo).
suppressWarnings(args_all <- commandArgs(FALSE))
f_arg <- sub("^--file=", "", args_all[grepl("^--file=", args_all)])
aqui <- normalizePath(dirname(f_arg[1]), mustWork = TRUE)         # .../salida
ej <- dirname(aqui)                                                # carpeta del ejercicio
RMD <- "serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd"
UNICO <- file.path(ej, "SemilleroUnico_v2.R"); MOODLE <- file.path(ej, "SemilleroMoodle_v2.R")

fallas <- character(0); salida_cod <- 0L
falla <- function(cod, exit, msg = "") { fallas <<- c(fallas, cod); salida_cod <<- if (salida_cod == 0L) exit else salida_cod
  cat("FALLA", cod, msg, "\n") }
fin <- function() {
  if (length(fallas)) cat("VEREDICTO: ROJO (", paste(unique(fallas), collapse = ", "), ") exit ", salida_cod, "\n", sep = "")
  else cat("VEREDICTO: VERDE (literales, render oficio y mutante en regla)\n")
  quit(status = salida_cod, save = "no")
}
for (b in c("pdfjam", "pdfinfo", "pdftoppm")) if (!nzchar(Sys.which(b))) { falla("USO_SIN_HERRAMIENTA", 4L, b); fin() }
for (f in c(UNICO, MOODLE, file.path(ej, RMD), file.path(ej, "solpcielo.tex"))) if (!file.exists(f)) { falla("USO_SIN_ARCHIVO", 4L, f); fin() }

cuenta <- function(lineas, lit) sum(grepl(lit, lineas[!grepl("^\\s*#", lineas)], fixed = TRUE))

# ---- (a) literales -------------------------------------------------------------------------
LIT_UNICO <- c(paste0('archivo_examen <- "', RMD, '"'),
               'stopifnot("copias: máximo 100 (decisión del profesor)" = copias <= 100)',
               '"--papersize", shQuote("{216mm,330mm}")',
               '"--noautoscale", "true"',
               'system2("pdfjam"')
LIT_MOODLE <- c(paste0('archivo_examen <- "', RMD, '"'),
                'copias <- 100',
                'stopifnot("copias: máximo 100" = copias <= 100)',
                'seed = matrix(seq_len(copias), ncol = 1)')
sondas_literales <- function(un = readLines(UNICO, encoding = "UTF-8"), mo = readLines(MOODLE, encoding = "UTF-8")) {
  r <- character(0)
  if (any(vapply(LIT_UNICO, function(l) cuenta(un, l) != 1L, TRUE))) r <- c(r, "USO_LITERAL_UNICO")
  if (any(vapply(LIT_MOODLE, function(l) cuenta(mo, l) != 1L, TRUE))) r <- c(r, "USO_LITERAL_MOODLE")
  r
}
cat("[a] literales de SemilleroUnico_v2.R y SemilleroMoodle_v2.R\n")
ra <- sondas_literales()
for (x in ra) falla(x, 1L)
if (!length(ra)) cat("  sin fallas\n")

# ---- (b) render real ------------------------------------------------------------------------
medir <- function(pdf) {
  o <- suppressWarnings(system2("pdfinfo", shQuote(pdf), stdout = TRUE, stderr = FALSE))
  l <- grep("^Page size:", o, value = TRUE); p <- grep("^Pages:", o, value = TRUE)
  m <- regmatches(l, regexec("([0-9.]+) x ([0-9.]+) pts", l))[[1]]
  list(w = as.numeric(m[2]), h = as.numeric(m[3]), paginas = as.integer(sub("^Pages:\\s+", "", p)))
}
es_oficio <- function(m) isTRUE(abs(m$w - 612) <= 1.5 && m$h >= 934.5 && m$h <= 937)
cat("[b] render real en directorio temporal (PDF con solpcielo.tex y NOPS centrado con pdfjam), 2 preguntas\n")
if (!length(ra)) {
  tmp <- tempfile("uso_seno_"); dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  for (f in c(RMD, "solpcielo.tex", "pcielo.tex", "pcielo_nosol.tex")) file.copy(file.path(ej, f), tmp)
  un <- readLines(UNICO, encoding = "UTF-8")
  i0 <- grep("^for \\(pdf in list.files", un); i1 <- which(un == "}" & seq_along(un) > i0[1])[1]
  if (!length(i0) || is.na(i1)) { falla("USO_SIN_BUCLE_PDFJAM", 2L) } else {
    bucle <- un[i0[1]:i1]
    guion <- c('library(exams)', '.exams_generation_mode <- TRUE',
               sprintf('archivo_examen <- "%s"', RMD), 'numpreg <- 2; copias <- 1',
               'dir_salida <- "salida"; dir_ejercicios <- "."; dir.create(dir_salida)',
               'nombre_sin_extension <- sub("\\\\.Rmd$", "", archivo_examen)',
               'exams2pdf(rep(archivo_examen, numpreg), n = copias, name = paste0(nombre_sin_extension, "_"), encoding = "UTF-8",',
               '          template = "solpcielo", dir = dir_salida, edir = dir_ejercicios)',
               'exams2nops(rep(archivo_examen, numpreg), n = copias, name = paste0(nombre_sin_extension, "_nops_"), encoding = "UTF-8",',
               '           dir = dir_salida, edir = dir_ejercicios, language = "es", title = "Evaluación de Matemáticas",',
               '           institution = "I. E. Pedacito de Cielo", date = Sys.Date(), replacement = FALSE, blank = 0, duplex = TRUE)',
               bucle)
    writeLines(guion, file.path(tmp, "render.R"), useBytes = TRUE)
    old <- setwd(tmp); Sys.setenv(MPLBACKEND = "Agg")
    st <- system2(file.path(R.home("bin"), "Rscript"), c("--no-init-file", "render.R"), stdout = FALSE, stderr = FALSE)
    setwd(old)
    sal <- file.path(tmp, "salida")
    pdf <- list.files(sal, "_1\\.pdf$", full.names = TRUE); pdf <- pdf[!grepl("_nops_", pdf)]
    nops <- list.files(sal, "_nops_1\\.pdf$", full.names = TRUE)
    nops_of <- list.files(sal, "_nops_1_oficio\\.pdf$", full.names = TRUE)
    if (st != 0L || !length(pdf) || !length(nops) || !length(nops_of)) {
      falla("USO_RENDER", 2L, sprintf("(exit %d; pdf=%d nops=%d nops_oficio=%d)", st, length(pdf), length(nops), length(nops_of)))
    } else {
      mp <- medir(pdf[1]); mn <- medir(nops[1]); mo <- medir(nops_of[1])
      mm <- function(m) sprintf("%.1f x %.1f pt = %.1f x %.1f mm, %d pág.", m$w, m$h, m$w / 72 * 25.4, m$h / 72 * 25.4, m$paginas)
      cat("  PDF preguntas :", mm(mp), "\n  NOPS exams2nops:", mm(mn), "\n  NOPS oficio    :", mm(mo), "\n")
      if (!es_oficio(mp)) falla("USO_PDF_NO_OFICIO", 2L, mm(mp))
      if (!es_oficio(mo)) falla("USO_NOPS_NO_OFICIO", 2L, mm(mo))
      if (es_oficio(mn)) falla("USO_NOPS_YA_ERA_OFICIO", 2L, "el NOPS crudo ya mide oficio: el paso de pdfjam no se probo")
    }
  }
} else cat("  omitido: fallaron los literales\n")

# ---- (c) mutante: semillero sin el paso de pdfjam -----------------------------------------
cat("[c] mutante: SemilleroUnico_v2.R sin el paso de pdfjam\n")
un <- readLines(UNICO, encoding = "UTF-8")
mut <- un[!grepl("pdfjam|--papersize|--noautoscale", un, perl = TRUE)]
if (identical(mut, un)) { falla("USO_MUTANTE_NO_APLICA", 3L) } else if (!length(sondas_literales(un = mut))) {
  falla("USO_MUTANTE_VIVO", 3L, "la sonda (a) no detecta un semillero sin pdfjam")
} else cat("  mutante detectado por (a): USO_LITERAL_UNICO\n")
fin()
