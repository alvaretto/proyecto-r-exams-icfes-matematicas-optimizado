## control_html_pdf.R — Paso 8 (coherencias_5), r10: mutaciones de control de los verificadores de HTML y PDF. Cada alteración de una COPIA
## del HTML de la canónica (html/cloze7.html) o del texto de su PDF (pdf/cloze007.pdf) debe hacerlos fallar, y por su sonda propia.
## Uso (desde esta carpeta): TMPDIR=<tmp> Rscript --no-init-file control_html_pdf.R
SOLO_DEFINICIONES <- TRUE
DIR_S <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))))
source(file.path(DIR_S, "verificar_formatos.R"), encoding = "UTF-8")        # define funciones y bancos; no repite la verificación de los 100 archivos
ENV <- Sys.getenv("TMPDIR", tempdir())
e7 <- ex[[7]][[1]]; ql7 <- nb(e7$questionlist); ep7 <- esperados_p2(e7)
## ------------------------------ HTML ------------------------------------------------------------------------------------
h <- paste(readLines(file.path(SAL, "html", "cloze7.html"), encoding = "UTF-8", warn = FALSE), collapse = "\n")
corre_html <- function(nombre, y, espera) {
  f <- file.path(ENV, "mutado.html"); writeLines(y, f, useBytes = TRUE)
  REG <<- REG[0, ]; verificar_html(7L, f, "CTRL")
  mal <- REG[!REG$ok, ]; caz <- nrow(mal) >= 1L; prop <- any(grepl(espera, mal$id))
  cat(sprintf("  HTML · %-100s %s · sonda propia: %s (%d fallos)\n", nombre, if (caz) "CAZADA" else "NO CAZADA  <<<<<<", if (prop) "sí" else "NO  <<<<<<", nrow(mal)))
  invisible(c(caz = caz, prop = prop))
}
s1 <- function(pat, rep, x, fixed = FALSE) { stopifnot(grepl(pat, x, fixed = fixed)); sub(pat, rep, x, fixed = fixed) }
REG <- REG[0, ]; verificar_html(7L, file.path(SAL, "html", "cloze7.html"), "CTRL"); cat("Control HTML sin mutar (debe dar 0 fallos):", sum(!REG$ok), "de", nrow(REG), "comprobaciones\n")
op_par <- paste(ql7[5:8], collapse = " / ")
rh <- list(
  corre_html("una celda de la tabla 2 alterada (6 cursos → 7 cursos)", s1("6 cursos", "7 cursos", h, fixed = TRUE), "las celdas de las dos tablas"),
  corre_html("un ##ANSWER2## sin resolver", s1("<p><strong>Parte 3.</strong>", "<p>##ANSWER2##</p>\n<p><strong>Parte 3.</strong>", h, fixed = TRUE), "ningún ##ANSWERi##"),
  corre_html("las dos primeras opciones de la Parte 2 intercambiadas", s1(op_par, paste(c(ql7[6], ql7[5], ql7[7:8]), collapse = " / "), h, fixed = TRUE), "cada gap"),
  corre_html("LaTeX crudo a la vista", s1("<p><strong>Parte 3.</strong>", "<p>$5 \\times 40 = 200$</p>\n<p><strong>Parte 3.</strong>", h, fixed = TRUE), "sin LaTeX crudo"),
  corre_html("L-6: la unidad fuera de la fórmula en la lista de totales de la Parte 1 (TeX sin «\\ \\text{kilos.}»)", s1("5 \\times 40 = 200\\ \\text{kilos.}</annotation>", "5 \\times 40 = 200</annotation>", h, fixed = TRUE), "las fórmulas del HTML"),
  corre_html("L-6: el signo fuera de la fórmula (TeX «kilos:» → «kilos»)", s1("\\text{kilos:}</annotation>", "\\text{kilos}</annotation>", h, fixed = TRUE), "las fórmulas del HTML"),
  corre_html("encabezado de la Parte 2 del tipo retirado", s1("Parte 2 · Lecturas del promedio que no sirven", "Parte 2 · Comparaciones que no deciden qué grado ganó", h, fixed = TRUE), "Solution con el único encabezado"),
  corre_html("encabezado de la Parte 2 duplicado", s1("<p><strong>Parte 2 · Lecturas del promedio que no sirven</strong></p>", "<p><strong>Parte 2 · Lecturas del promedio que no sirven</strong></p>\n<p><strong>Parte 2 · Lecturas del promedio que no sirven</strong></p>", h, fixed = TRUE), "Solution con el único encabezado"),
  corre_html("viñeta con la operación cambiada («la mitad» → «el doble del promedio»)", s1("da la mitad del promedio", "da el doble del promedio", h, fixed = TRUE), "bajo el encabezado de la Parte 2"),
  corre_html("una viñeta de la Parte 2 omitida", s1("<li>«Lo que recogió ese grado dividido entre el número de sus cursos al cuadrado.»[^\n]*</li>\n", "", h), "bajo el encabezado de la Parte 2"),
  corre_html("«Respuestas» de la Parte 2 cita un distractor", s1("<li>Parte 2\\. «Lo que recogió ese grado dividido entre el número de sus cursos\\.»</li>", "<li>Parte 2. «Lo que recogió ese grado dividido entre el promedio de sus cursos.»</li>", h), "«Respuestas»"),
  corre_html("«Procedimiento» de la Parte 2 con el párrafo del tipo retirado", s1("Parte 2\\. El promedio por curso es el total del grado dividido entre la cantidad de cursos: lo que le tocaría a cada curso si el total se repartiera por igual\\. No dice cuánto recogió cada curso en particular\\.", "Parte 2. Gana el grado que recogió más en total.", h), "«Procedimiento»"),
  corre_html("una letra de opción en la Solution (regla #19)", s1("<p><strong>Procedimiento correcto</strong></p>", "<p>La opción B es la correcta.</p>\n<p><strong>Procedimiento correcto</strong></p>", h, fixed = TRUE), "regla #19"),
  corre_html("texto del tipo retirado en el cuerpo", s1("<p><strong>Procedimiento correcto</strong></p>", "<p>¿Qué se compara en la tabla del otro colegio?</p>\n<p><strong>Procedimiento correcto</strong></p>", h, fixed = TRUE), "ni el enunciado ni la Solution conservan"))
m <- do.call(rbind, rh); cat(sprintf("\nMutaciones de HTML: %d de %d cazadas · %d de %d por su sonda propia\n\n", sum(m[, "caz"]), nrow(m), sum(m[, "prop"]), nrow(m)))
## ------------------------------ PDF (texto crudo de pdftotext) ------------------------------------------------------------
arch7 <- file.path(SAL, "pdf", "cloze007.pdf")
cru0 <- paste(system2("pdftotext", c("-raw", shQuote(arch7), "-"), stdout = TRUE), collapse = " ")
corre_pdf <- function(nombre, cru, espera) {
  REG <<- REG[0, ]; verificar_pdf(7L, arch7, cru = cru, fmt = "CTRL")
  mal <- REG[!REG$ok, ]; caz <- nrow(mal) >= 1L; prop <- any(grepl(espera, mal$id))
  cat(sprintf("  PDF  · %-100s %s · sonda propia: %s (%d fallos)\n", nombre, if (caz) "CAZADA" else "NO CAZADA  <<<<<<", if (prop) "sí" else "NO  <<<<<<", nrow(mal)))
  invisible(c(caz = caz, prop = prop))
}
REG <- REG[0, ]; verificar_pdf(7L, arch7, fmt = "CTRL"); cat("Control PDF sin mutar (debe dar 0 fallos):", sum(!REG$ok), "de", nrow(REG), "comprobaciones\n")
rp <- list(
  corre_pdf("un ##ANSWER1## a la vista", paste(cru0, "##ANSWER1##"), "ningún ##ANSWERi##"),
  corre_pdf("el encabezado de la Parte 2 de la Solution ausente", s1("Parte 2 · Lecturas del promedio que no sirven", "", cru0, fixed = TRUE), "encabezado de la Parte 2 una sola vez"),
  corre_pdf("el encabezado de la Parte 2 del tipo retirado", s1("Parte 2 · Lecturas del promedio que no sirven", "Parte 2 · Comparaciones que no deciden qué grado ganó", cru0, fixed = TRUE), "encabezado de la Parte 2 una sola vez"),
  corre_pdf("la primera opción de la Parte 2 repetida (la segunda en su lugar)", s1(ql7[5], ql7[6], cru0, fixed = TRUE), "cada pregunta va seguida"),
  corre_pdf("una celda de la tabla alterada (39 kilos → 93 kilos; el texto «39 kilos» sigue en la Solution)", s1("39 kilos", "93 kilos", cru0, fixed = TRUE), "las seis filas"),
  corre_pdf("una cifra de una fórmula de la Solution alterada en todo el PDF (200 kilos → 210 kilos)", gsub("200[^0-9A-Za-z]kilos", "210 kilos", cru0), "cada fórmula de la Solution"),
  corre_pdf("texto del tipo retirado a la vista", paste(cru0, "Gana el grado que recogió más en total"), "ni el enunciado ni la Solution conservan"),
  corre_pdf("explicación de una viñeta con la operación cambiada", s1("la mitad del promedio", "el doble del promedio", cru0, fixed = TRUE), "las tres viñetas"),
  corre_pdf("«Respuestas» de la Parte 2 cita un distractor (en el texto crudo los « » salen como caracteres de control)",
            s1("(Parte 2\\. .)Lo que recogió ese grado dividido entre el número de sus cursos\\.", "\\1Lo que recogió ese grado dividido entre el promedio de sus cursos.", cru0), "«Respuestas» cita"),
  corre_pdf("una fila de la tabla 2 alterada por completo (Séptimo 6 cursos 39 kilos → Séptimo 6 cursos 93 kilos)", s1("Séptimo 6 cursos 39 kilos", "Séptimo 6 cursos 93 kilos", cru0, fixed = TRUE), "las seis filas"),
  corre_pdf("las dos filas de la tabla 2 intercambiadas", s1("Séptimo 6 cursos 39 kilos Noveno 4 cursos 48 kilos", "Noveno 4 cursos 48 kilos Séptimo 6 cursos 39 kilos", cru0, fixed = TRUE), "las seis filas"),
  corre_pdf("LaTeX crudo a la vista", paste(cru0, "$5 \\times 40 = 200$"), "ningún ##ANSWERi##"))
m <- do.call(rbind, rp); cat(sprintf("\nMutaciones de PDF: %d de %d cazadas · %d de %d por su sonda propia\n", sum(m[, "caz"]), nrow(m), sum(m[, "prop"]), nrow(m)))
