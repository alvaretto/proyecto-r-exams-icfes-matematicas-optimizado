## Mutantes de los controles de exportar_moodle_cloze.R (detractor r4, H4 y H5), aplicados en memoria al
## XML real del aula. Cada mutante tiene que hacer fallar su control. Uso: desde cloze/salida.
x0 <- paste(readLines("moodle_aula/probabilidad_fichas_cloze.xml", encoding = "UTF-8", warn = FALSE), collapse = "\n")
cuenta <- function(x, p) lengths(regmatches(x, gregexpr(p, x)))
img_nuevo <- function(x) sum(grepl("<img|@@PLUGINFILE@@", regmatches(x, gregexpr(
  "\\{[0-9]+:(?:MULTICHOICE|MULTIRESPONSE)(?:\\\\.|[^}\\\\])*", x, perl = TRUE))[[1]]))
img_viejo <- function(x) sum(grepl("<img|@@PLUGINFILE@@", regmatches(x, gregexpr(
  "\\{1:(MULTICHOICE|MULTIRESPONSE)[^}]*", x))[[1]]))
pesos <- function(x) { q <- strsplit(x, "<question type=\"cloze\">", fixed = TRUE)[[1]][-1]
  vapply(q, function(b) paste(regmatches(b, gregexpr("(?<=\\{)[0-9]+(?=:(MULTICHOICE|MULTIRESPONSE|NUMERICAL))", b, perl = TRUE))[[1]], collapse = "|"), "") }
ok_pesos <- function(x) all(pesos(x) == "3|1|1|1|1|1")
uno <- function(x, de, a) sub(de, a, x, fixed = TRUE)            # muta solo la primera aparición
cat("Original: pesos OK =", ok_pesos(x0), "| imágenes en gaps =", img_nuevo(x0),
    "| desplegables =", cuenta(x0, "\\{[0-9]+:MULTICHOICE:"), "\n\n")
res <- data.frame(mutante = character(), control = character(), resultado = character())
anota <- function(m, ctl, muere) res[nrow(res) + 1L, ] <<- list(m, ctl, if (muere) "MUERE" else "SOBREVIVE")
x <- uno(x0, "{3:MULTICHOICE_V:", "{1:MULTICHOICE_V:");                anota("M1 Parte 1 a peso 1 (una pregunta)", "pesos", !ok_pesos(x))
x <- uno(x0, "{3:MULTICHOICE_V:", "{3:MULTICHOICE_V:<img src=\"x.png\">"); anota("M2 <img> en el gap de la Parte 1", "imágenes (patrón nuevo)", img_nuevo(x) > 0)
anota("M2 con el patrón viejo {1:", "imágenes (patrón viejo)", img_viejo(x) > 0)
x <- uno(x0, "{1:MULTICHOICE_V:", "{1:MULTICHOICE:");                  anota("M3 Parte 5 como desplegable", "desplegables", cuenta(x, "\\{[0-9]+:MULTICHOICE:") > 0)
x <- uno(x0, "{3:MULTICHOICE_V:", "{3:MULTICHOICE_V:texto \\} sigue<img src=\"x.png\">"); anota("M4 <img> tras un \\} escapado", "imágenes (patrón nuevo)", img_nuevo(x) > 0)
anota("M4 con el patrón [^}]*", "imágenes (patrón de 15:46)", sum(grepl("<img", regmatches(x, gregexpr("\\{[0-9]+:(MULTICHOICE|MULTIRESPONSE)[^}]*", x))[[1]])) > 0)
q1 <- regexpr("\\{1:NUMERICAL:", x0)                                    # el 3 pasa a la Parte 2 en la primera pregunta
x <- x0; substr(x, q1 + 1L, q1 + 1L) <- "3"; x <- uno(x, "{3:MULTICHOICE_V:", "{1:MULTICHOICE_V:"); anota("M5 el peso 3 en la Parte 2", "pesos", !ok_pesos(x))
print(res, row.names = FALSE)
