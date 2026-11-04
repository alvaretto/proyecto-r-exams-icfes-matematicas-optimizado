## control_moodle.R — Paso 8 (coherencias_5), r10: mutaciones de control del verificador de Moodle. Cada alteración de una COPIA del
## XML de la canónica (moodle/s007.xml) debe hacerlo fallar, y por su sonda propia. Las nuevas cubren la Parte 2 de un solo tipo y la
## retroalimentación general. Uso (desde esta carpeta): TMPDIR=<tmp> Rscript --no-init-file control_moodle.R
SOLO_DEFINICIONES <- TRUE
DIR_S <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))))
source(file.path(DIR_S, "verificar_formatos.R"), encoding = "UTF-8")        # define funciones y bancos; no repite la verificación de los 100 archivos
ENV <- Sys.getenv("TMPDIR", tempdir())
orig <- file.path(SAL, "moodle", "s007.xml")
x <- paste(readLines(orig, encoding = "UTF-8", warn = FALSE), collapse = "\n")
etq_py <- function(f) { o <- system2("python3", c("-I", normalizePath(file.path(DIR_S, "..", "..", "verificar_gaps_xml.py")), shQuote(f)), stdout = TRUE); as.integer(sub("^.*etiquetas dentro de gaps: ([0-9]+).*$", "\\1", grep("^TOTAL:", o, value = TRUE))) }
corre <- function(nombre, y, espera, python = FALSE) {
  f <- file.path(ENV, "mutado.xml"); writeLines(y, f, useBytes = TRUE)
  REG <<- REG[0, ]
  err <- tryCatch({ verificar_moodle(f, "CTRL", function(k) 0L); NULL }, error = function(e) conditionMessage(e))
  mal <- REG[!REG$ok, ]; hit <- grepl(espera, mal$id)
  py <- if (python) etq_py(f) else NA_integer_
  caz <- nrow(mal) >= 1L || !is.null(err) || (!is.na(py) && py >= 1L)
  prop <- any(hit) || (python && !is.na(py) && py >= 1L)
  cat(sprintf("  %-98s %s · sonda propia: %s (%d fallos%s)\n", nombre, if (caz) "CAZADA" else "NO CAZADA  <<<<<<", if (prop) "sí" else "NO  <<<<<<", nrow(mal),
              if (!is.na(py)) paste0("; etiquetas en gaps según el script de Python: ", py) else ""))
  invisible(c(caz = caz, prop = prop))
}
sub1 <- function(pat, rep, x, perl = TRUE) { stopifnot(grepl(pat, x, perl = perl)); sub(pat, rep, x, perl = perl) }
## control sin mutar
REG <- REG[0, ]; verificar_moodle(orig, "CTRL", function(k) 0L); cat("Control sin mutar (debe dar 0 fallos):", sum(!REG$ok), "de", nrow(REG), "comprobaciones\n")
gap2 <- regmatches(x, regexpr("\\{1:MULTICHOICE_V:[^}]*\\}", x))
it2 <- strsplit(sub("\\}$", "", sub("^\\{1:MULTICHOICE_V:", "", gap2)), "~", fixed = TRUE)[[1]]
cat("Gap de la Parte 2 de la canónica:", gap2, "\n\n")
res <- list(
  corre("Parte 3 numérica +1", sub1("NUMERICAL:=192:0", "NUMERICAL:=193:0", x), "Partes 3 y 4"),
  corre("peso del gap 1: 3 → 2", sub1("\\{3:MULTICHOICE_V:", "{2:MULTICHOICE_V:", x), "pesos de los gaps"),
  corre("tolerancia de la Parte 3: 0 → 1", sub1("NUMERICAL:=192:0", "NUMERICAL:=192:1", x), "tolerancia"),
  corre("defaultgrade 8 → 6", sub1("<defaultgrade>8", "<defaultgrade>6", x), "defaultgrade"),
  corre("celda «5 cursos» → «6 cursos» (tabla 1)", sub1("5 cursos", "6 cursos", x), "Parte 1"),
  corre("Parte 1: la marca «=» pasa a otra opción", { g1 <- regmatches(x, regexpr("\\{3:MULTICHOICE_V:[^}]*\\}", x)); it <- strsplit(sub("\\}$", "", sub("^\\{3:MULTICHOICE_V:", "", g1)), "~", fixed = TRUE)[[1]]
        k <- which(startsWith(it, "=")); j <- setdiff(1:4, k)[1]; it[k] <- sub("^=", "", it[k]); it[j] <- paste0("=", it[j]); sub(g1, paste0("{3:MULTICHOICE_V:", paste(it, collapse = "~"), "}"), x, fixed = TRUE) }, "Parte 1: una sola verdadera"),
  corre("Parte 2: la marca «=» pasa a otra opción", { it <- it2; k <- which(startsWith(it, "=")); j <- setdiff(1:4, k)[1]; it[k] <- sub("^=", "", it[k]); it[j] <- paste0("=", it[j])
        sub(gap2, paste0("{1:MULTICHOICE_V:", paste(it, collapse = "~"), "}"), x, fixed = TRUE) }, "Parte 2: las cuatro opciones salen de mi banco"),
  corre("Parte 2: una opción incorrecta sustituida por una cuarta paráfrasis correcta mía (dos correctas)", { it <- it2; j <- which(!startsWith(it, "="))[1]; it[j] <- P2_EXTRA_CORRECTA
        sub(gap2, paste0("{1:MULTICHOICE_V:", paste(it, collapse = "~"), "}"), x, fixed = TRUE) }, "Parte 2: las cuatro opciones salen de mi banco"),
  corre("Parte 2: pregunta con otro material (papel → cartón)", sub1("(el promedio de )papel( recogido por curso de un grado)", "\\1cartón\\2", x), "Parte 2: la pregunta es la vigente"),
  corre("Parte 2: pregunta del tipo retirado", sub1("¿Qué indica, en la tabla del otro colegio, el promedio de papel recogido por curso de un grado\\?", "¿Qué se compara en la tabla del otro colegio al decidir qué grado ganó el concurso?", x), "Parte 2: la pregunta es la vigente"),
  corre("Parte 6: una marca invertida", { g6 <- regmatches(x, regexpr("\\{1:MULTIRESPONSE:[^}]*\\}", x)); it <- strsplit(sub("\\}$", "", sub("^\\{1:MULTIRESPONSE:", "", g6)), "~", fixed = TRUE)[[1]]
        it[1] <- if (startsWith(it[1], "=")) sub("^=", "%-50%", it[1]) else sub("^%-[0-9.]+%", "=", it[1]); sub(g6, paste0("{1:MULTIRESPONSE:", paste(it, collapse = "~"), "}"), x, fixed = TRUE) }, "Parte 6"),
  corre("un marcador ##ANSWER1## sin resolver", sub1("<p><strong>Parte 2.</strong>", "<p>##ANSWER1##</p><p><strong>Parte 2.</strong>", x), "ningún ##ANSWERi##"),
  corre("gap de la Parte 3 desplazado (pregunta 4 antes de la 3)", sub1("<p><strong>Parte 3.</strong>", "<p><strong>Parte 4.</strong> ¿Cuántos kilos?</p><p><strong>Parte 3.</strong>", x), "las seis preguntas aparecen en orden"),
  corre("generalfeedback: encabezado de la Parte 2 duplicado", sub1("<p><strong>Parte 2 · Lecturas del promedio que no sirven</strong></p>", "<p><strong>Parte 2 · Lecturas del promedio que no sirven</strong></p>\n<p><strong>Parte 2 · Lecturas del promedio que no sirven</strong></p>", x), "generalfeedback: un único encabezado"),
  corre("generalfeedback: encabezado del tipo retirado", sub1("Parte 2 · Lecturas del promedio que no sirven", "Parte 2 · Comparaciones que no deciden qué grado ganó", x), "generalfeedback: un único encabezado"),
  corre("generalfeedback: viñeta con la operación cambiada («la mitad» → «el doble del promedio»)", sub1("da la mitad del promedio", "da el doble del promedio", x), "generalfeedback: lo que cada viñeta"),
  corre("generalfeedback: viñeta con la operación cambiada («menos que» → «más que el promedio»)", sub1("da menos que el promedio", "da más que el promedio", x), "generalfeedback: lo que cada viñeta"),
  corre("generalfeedback: viñeta con una afirmación añadida que el oráculo no sabe recalcular («y da el triple del promedio»)", sub1("da la mitad del promedio", "da la mitad del promedio y da el triple del promedio", x), "generalfeedback: lo que cada viñeta"),
  corre("generalfeedback: viñeta con la explicación de otra opción", sub1("Dividir otra vez entre dos da la mitad del promedio: el promedio es lo recogido dividido entre el número de cursos, sin más divisiones.", "Dividir entre los cursos de los dos grados da menos que el promedio de ese grado: lo recogido se reparte solo entre sus propios cursos.", x), "generalfeedback: las tres viñetas"),
  corre("generalfeedback: «Respuestas» de la Parte 2 cita un distractor", sub1("<li>Parte 2\\. «Lo que recogió ese grado dividido entre el número de sus cursos\\.»</li>", "<li>Parte 2. «Lo que recogió ese grado dividido entre el promedio de sus cursos.»</li>", x), "generalfeedback: «Respuestas»"),
  corre("generalfeedback: «Procedimiento» del tipo retirado", sub1("Parte 2\\. El promedio por curso es el total del grado dividido entre la cantidad de cursos: lo que le tocaría a cada curso si el total se repartiera por igual\\. No dice cuánto recogió cada curso en particular\\.", "Parte 2. Gana el grado que recogió más en total.", x), "generalfeedback: «Procedimiento»"),
  corre("generalfeedback: texto del tipo retirado añadido", sub1("<p><strong>Procedimiento correcto</strong></p>", "<p>Gana el grado que recogió más en total.</p><p><strong>Procedimiento correcto</strong></p>", x), "sin texto del tipo retirado"),
  corre("una imagen dentro de la pregunta (Incidente G)", sub1("<p><strong>Parte 3.</strong>", "<p><img src=\"@@PLUGINFILE@@/x.png\"></p><p><strong>Parte 3.</strong>", x), "guarda del archivo: ninguna imagen"),
  corre("una etiqueta <b> dentro de un gap (Incidente G)", { it <- it2; it[2] <- paste0("<b>", it[2], "</b>"); sub(gap2, paste0("{1:MULTICHOICE_V:", paste(it, collapse = "~"), "}"), x, fixed = TRUE) }, "^$", python = TRUE),
  corre("L-6: el punto sale de la fórmula de un total de la Parte 1 («kilos.» → «kilos» + «.»)", sub1("(\\\\text\\{kilos\\.\\}\\\\\\))(</span>)", "\\\\text{kilos}\\\\)\\2.", x), "generalfeedback: ningún signo"),
  corre("una fórmula con la aritmética mala (200 - 180 = 20 → = 21)", sub1("200 - 180 = 20", "200 - 180 = 21", x), "generalfeedback: toda fórmula"),
  corre("una fórmula como $…$ (MathJax no la compone)", sub1("\\\\\\(5 \\\\times 40 = 200\\\\ \\\\text\\{kilos\\.\\}\\\\\\)", "$5 \\\\times 40 = 200$", x), "generalfeedback: las fórmulas van como"),
  corre("el nombre de la pregunta lleva la ruta absoluta", sub1("<text> Q1 : totales_promedios", "<text> Q1 : _home_bootcamp_Proyectos_totales_promedios", x), "el nombre de la pregunta no lleva ruta"))
m <- do.call(rbind, res)
cat(sprintf("\nMutaciones de Moodle: %d de %d cazadas · %d de %d por su sonda propia\n", sum(m[, "caz"]), nrow(m), sum(m[, "prop"]), nrow(m)))
