# Comprobación propia del coordinador del H1 de la FASE 2C r4 del CLOZE (.Rmd 27f789dc), 2026-11-03.
# Pregunta: en cuántas versiones el párrafo «Parte 6» de la Solution lleva una comparación que, leída como
# enunciado suelto, es falsa («su promedio por curso, 30 kilos, es mayor que la diferencia, 40 kilos»).
# Lee el texto que emite R/exams (xexams + read_exercise), no los objetos del data_generation.
# Muestra estándar: semillas 1 a 100 (regla #23). El detractor usó 111 (estas 100 más 11 canónicas).
# Uso, desde la raíz del repo: Rscript --no-init-file <este archivo>
suppressPackageStartupMessages(library(exams))
rmd <- normalizePath(file.path("A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze",
                               "totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"))
stopifnot(unname(tools::md5sum(rmd)) == "27f789dc6327f7d6bda72cded61502cd")
tmp <- tempfile("h1r4_")
dir.create(tmp)
pat <- paste0("si y solo si su promedio por curso, ([0-9]+) kilos, es mayor que la diferencia, ([0-9]+) kilos; ",
              "y el grado (.+) habría perdido con un curso menos si y solo si su promedio, ([0-9]+) kilos, ",
              "es mayor que esa diferencia\\.$")
filas <- vector("list", 100L)
for (s in 1:100) {
  x <- xexams(rmd, n = 1L, dir = tmp, seed = s,
              driver = list(sweave = list(quiet = TRUE), read = NULL, transform = NULL, write = NULL))
  ex <- x[[1]][[1]]
  if (!is.list(ex) || is.null(ex$solution)) stop("xexams no devolvió el ejercicio leído")
  sol <- gsub(" ", " ", ex$solution, fixed = TRUE)
  lin <- grep("^Parte 6\\. Cada hipótesis", sol, value = TRUE)
  if (length(lin) != 1L) stop("semilla ", s, ": hay ", length(lin), " líneas «Parte 6. Cada hipótesis»")
  m <- regmatches(lin, regexec(pat, lin))[[1]]
  if (length(m) != 5L) stop("semilla ", s, ": el párrafo no tiene la forma esperada:\n", lin)
  aP <- as.integer(m[2]); D <- as.integer(m[3]); aG <- as.integer(m[5])
  filas[[s]] <- data.frame(semilla = s, aP = aP, D = D, aG = aG, hp = aP > D, hg = aG > D)
}
r <- do.call(rbind, filas)
cat("md5 del .Rmd: 27f789dc6327f7d6bda72cded61502cd · semillas 1 a 100\n")
cat("versiones leídas:", nrow(r), "\n")
cat("empates (aP == D o aG == D):", sum(r$aP == r$D | r$aG == r$D), "\n")
cat("«su promedio por curso … es mayor que la diferencia» (HP) falsa como enunciado suelto:", sum(!r$hp), "\n")
cat("«su promedio … es mayor que esa diferencia» (HG) falsa como enunciado suelto:", sum(!r$hg), "\n")
cat(sprintf("al menos una de las dos falsa: %d de %d (%.0f %%)\n", sum(!(r$hp & r$hg)), nrow(r),
            100 * mean(!(r$hp & r$hg))))
cat("las dos verdaderas:", sum(r$hp & r$hg), "· las dos falsas:", sum(!r$hp & !r$hg), "\n")
ej <- r[!r$hp, ][1, ]
cat(sprintf("ejemplo (semilla %d): «su promedio por curso, %d kilos, es mayor que la diferencia, %d kilos»\n",
            ej$semilla, ej$aP, ej$D))
