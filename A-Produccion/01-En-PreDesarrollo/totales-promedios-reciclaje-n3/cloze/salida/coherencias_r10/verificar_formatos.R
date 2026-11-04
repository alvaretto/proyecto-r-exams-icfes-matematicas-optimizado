## verificar_formatos.R — Paso 8 (coherencias_5), r10, .Rmd md5 e16842c157…
## Coherencia entre lo que R/exams emite (xexams, semillas 1:100) y los ARCHIVOS ya renderizados en esta carpeta:
##   html/cloze1..100.html (100) · pdf/cloze001..100.pdf (100) · docx/cloze_s0071.docx (semilla 7) ·
##   moodle/s001..100.xml (100, un XML por semilla) · moodle_aula/totales_promedios_cloze.xml (30 versiones en un solo XML,
##   la ruta real de exportar_moodle_cloze.R). Los dos XML de Moodle se recalculan desde cero: tablas, marcas de cada gap, pesos
##   3|1|1|1|1|1, ninguna imagen ni etiqueta dentro de un gap, retroalimentación general (Solution).
## Reutiliza los evaluadores y el banco juzgado de verificar_coherencias.R (solo sus definiciones, sin ejecutarlo).
## Uso (desde esta carpeta): TMPDIR=<tmp> Rscript --no-init-file verificar_formatos.R
suppressMessages({library(exams); library(xml2)})
if (!exists("DIR_S")) DIR_S <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))))
SAL <- DIR_S                                                # coherencias_r10: los renders viven en sus subcarpetas
F <- normalizePath(file.path(DIR_S, "..", "..", "totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"))
cat("md5 del .Rmd:", unname(tools::md5sum(F)), "\n")
for (e in parse(file.path(DIR_S, "verificar_coherencias.R"), encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in%
      c("NBSP", "nb", "limpiar", "num", "P2_RE_PREG", "TSV", "P2_ID", "P2_ANCLA", "POOL_P2", "P2_OP", "P2_EXTRA_CORRECTA", "juicio_p2", "P2_AFIRMA", "P2_RE_AFIRMA", "eval_p1", "eval_p6", "lee_f", "frm", "sh", "fok")) eval(e, globalenv())
SEM <- 1:100
ex <- xexams(F, n = length(SEM), seed = SEM, driver = list(sweave = list(quiet = TRUE, pdf = FALSE, png = TRUE, encoding = "UTF-8"),
                                                           read = NULL, transform = NULL, write = NULL))

## ---- registro de comprobaciones: (formato, comprobación, semilla o pregunta, resultado) -------------------------------
REG <- data.frame(fmt = character(), id = character(), sem = integer(), ok = logical(), det = character(), decl = logical(), stringsAsFactors = FALSE)
chk <- function(fmt, id, sem, ok, det = "", decl = FALSE) { REG[nrow(REG) + 1L, ] <<- list(fmt, id, as.integer(sem), isTRUE(ok), substr(paste(det, collapse = " "), 1, 300), decl); invisible(isTRUE(ok)) }
cel_md <- function(q) { tl <- grep("^\\|", q, value = TRUE); tl <- tl[!grepl("^\\|[-:| ]+\\|$", tl)]
  lapply(tl, function(r) limpiar(strsplit(sub("\\|\\s*$", "", sub("^\\|", "", r)), "|", fixed = TRUE)[[1]])) }
ws <- function(x) trimws(gsub("\\s+", " ", nb(x)))
## Fórmulas ($…$) de un texto emitido, sin los signos de dólar, y su texto plano (como lo ve un lector del PDF)
fs_tex <- function(x) gsub("^\\$|\\$$", "", unlist(regmatches(x, gregexpr("\\$[^$]+\\$", x))))
form_txt <- function(g) { g <- gsub("\\\\times", "×", g); g <- gsub("\\\\ \\\\text\\{kilos[.,:;]?\\}", " kilos", g); gsub("\\\\text\\{[.,:;]\\}", "", g) }
H2 <- "Parte 2 · Lecturas del promedio que no sirven"
PROC2 <- "Parte 2. El promedio por curso es el total del grado dividido entre la cantidad de cursos: lo que le tocaría a cada curso si el total se repartiera por igual. No dice cuánto recogió cada curso en particular."
RETIRADO_FMT <- "Comparaciones que no deciden|¿Qué se compara|Gana el grado que recogió más en total|decidir qué grado ganó"
## lo que debe llevar cada formato de la Parte 2: las tres viñetas (opción incorrecta + su explicación) y la clave
esperados_p2 <- function(e) { ql <- nb(e$questionlist); sl <- nb(e$solutionlist); jui <- juicio_p2(ql[5:8])
  list(bullets = vapply(which(!(jui %in% "correcta")), function(j) paste0("«", ql[4 + j], "» ", sub("^Falso\\. ", "", sl[4 + j])), ""),
       clave = ql[5:8][jui %in% "correcta"]) }

## ================================ HTML =============================================================================
## verificar_html(i, arch): las comprobaciones de un HTML renderizado contra lo que R/exams emite (xexams) para la semilla i.
verificar_html <- function(i, arch, fmt = "HTML") {
  e <- ex[[i]][[1]]; q <- nb(e$question); ql <- nb(e$questionlist); ep <- esperados_p2(e)
  doc <- read_html(arch, encoding = "UTF-8")
  tabs <- xml_find_all(doc, "//table")
  cel_html <- lapply(tabs, function(t) lapply(xml_find_all(t, ".//tr"), function(r) limpiar(xml_text(xml_find_all(r, "./th|./td")))))
  md <- cel_md(q)
  chk(fmt, "dos tablas, cada una de 3 filas × 3 columnas", i, length(tabs) == 2L && all(lengths(cel_html) == 3L))
  chk(fmt, "las celdas de las dos tablas = las de la pregunta emitida por R/exams (misma versión)", i, identical(unlist(cel_html), unlist(md)), paste(setdiff(unlist(md), unlist(cel_html)), collapse = "|"))
  ps <- vapply(xml_find_all(doc, "//p"), function(p) trimws(nb(xml_text(p))), "")
  chk(fmt, "ningún ##ANSWERi## sin resolver", i, !any(grepl("##ANSWER", ps, fixed = TRUE)) && !grepl("##ANSWER", paste(xml_text(doc)), fixed = TRUE))
  pos <- vapply(1:6, function(k) which(startsWith(ps, sprintf("Parte %d.", k)))[1], 1L)
  chk(fmt, "las seis preguntas aparecen en orden", i, !anyNA(pos) && !is.unsorted(pos))
  rangos <- list(1:4, 5:8, NULL, NULL, NULL, 12:15)
  ok_gap <- vapply(1:6, function(k) {
    sig <- ps[pos[k] + 1L]
    if (is.null(rangos[[k]])) identical(sig, "") else identical(sig, paste(ql[rangos[[k]]], collapse = " / "))
  }, NA)
  chk(fmt, "cada gap (opciones o campo num) va en el párrafo siguiente a su pregunta, con las opciones del exshuffle", i, all(ok_gap), paste(which(!ok_gap), collapse = ","))
  an_tex <- xml_text(xml_find_all(doc, "//annotation"))
  chk(fmt, "las fórmulas del HTML (TeX de cada MathML) son exactamente las de la Solution y la retroalimentación emitidas, con la unidad y el signo dentro (L-6)", i,
      identical(sort(an_tex), sort(c(fs_tex(e$solution), fs_tex(e$solutionlist)))), paste(setdiff(c(fs_tex(e$solution), fs_tex(e$solutionlist)), an_tex), collapse = " | "))
  xml_remove(xml_find_all(doc, "//annotation"))     # el MathML guarda el TeX en <annotation>: no se muestra
  txt <- nb(xml_text(doc))
  chk(fmt, "sin LaTeX crudo ni «$» sueltos en el texto visible", i, !grepl("\\\\times|\\$|\\\\nobreak|\\{=latex\\}", txt))
  chk(fmt, "las ecuaciones salieron como MathML/MathJax (cuenta ≥ las de la Solution)", i, length(xml_find_all(doc, "//math|//span[contains(@class,'math')]")) >= 10L, length(xml_find_all(doc, "//math|//span[contains(@class,'math')]")))
  chk(fmt, "Solution con el único encabezado de la Parte 2 («Parte 2 · Lecturas del promedio que no sirven») y ninguno del tipo retirado",
      i, sum(ps == H2) == 1L && sum(startsWith(ps, "Parte 2 ·")) == 1L)
  ra <- which(ps == "Respuestas")[1]
  chk(fmt, "la Solution no cita letras de opciones (regla #19)", i,
      !is.na(ra) && !grepl("[Oo]pci[oó]n [A-D]\\b|\\([a-d]\\)|\\b[Ll]etra\\b", paste(ps[ra:length(ps)], collapse = " ")))
  ## --- Parte 2 en la Solution del HTML ---
  li2 <- vapply(xml_find_all(doc, sprintf("//p[normalize-space(.)='%s']/following-sibling::ul[1]/li", H2)), function(z) ws(xml_text(z)), "")
  chk(fmt, "bajo el encabezado de la Parte 2, las tres viñetas son las tres opciones incorrectas de la versión con la explicación de su retroalimentación", i,
      length(li2) == 3L && setequal(li2, ws(ep$bullets)), paste(setdiff(ws(ep$bullets), li2), collapse = " ; "))
  resp2 <- vapply(xml_find_all(doc, "//li[starts-with(normalize-space(.), 'Parte 2.')]"), function(z) ws(xml_text(z)), "")
  chk(fmt, "«Respuestas»: la línea de la Parte 2 cita la opción correcta", i, length(resp2) == 1L && identical(resp2, ws(paste0("Parte 2. «", ep$clave, "»"))), paste(resp2, collapse = " | "))
  chk(fmt, "«Procedimiento»: la línea de la Parte 2 define el promedio por curso", i, identical(ws(ps[startsWith(ps, "Parte 2. El promedio por curso")]), ws(PROC2)))
  chk(fmt, "ni el enunciado ni la Solution conservan texto del tipo retirado de la Parte 2", i, !grepl(RETIRADO_FMT, txt))
}
if (!exists("SOLO_DEFINICIONES")) {
cat("\n=== HTML (html/cloze1..100.html) ===\n")
for (i in SEM) verificar_html(i, file.path(SAL, "html", sprintf("cloze%d.html", i)))
}

## ================================ PDF ==============================================================================
nrm <- function(x) gsub("[^[:alnum:]áéíóúüñ]", "", gsub("fi|fl|¿|¾", "", tolower(nb(x))))   # ligaturas fi/fl y «¿» salen mal en pdftotext
## Las seis filas de las dos tablas (cabecera + 2 filas, cada una COMPLETA) deben aparecer en el texto, en ese orden. La comprobación de la
## ronda 3 buscaba cada celda suelta en todo el documento: «39 kilos» sigue apareciendo en la Solution aunque se altere la tabla (hallazgo de la
## ronda 9, medido con la mutación «una celda de la tabla alterada»: NO CAZADA con la comprobación antigua).
filas_en_orden <- function(celdas, texto_nrm) {
  ult <- 0L; ok <- logical(length(celdas))
  for (k in seq_along(celdas)) { p <- regexpr(nrm(paste(celdas[[k]], collapse = " ")), substring(texto_nrm, ult + 1L), fixed = TRUE)
    if (p < 0) break; ok[k] <- TRUE; ult <- ult + p + attr(p, "match.length") - 1L }
  ok
}
## verificar_pdf(i, arch, cru): `cru` = texto crudo de pdftotext (se puede pasar mutado para las mutaciones de control)
verificar_pdf <- function(i, arch, cru = NULL, fmt = "PDF") {
  e <- ex[[i]][[1]]; q <- nb(e$question); ql <- nb(e$questionlist); ep <- esperados_p2(e)
  if (is.null(cru)) cru <- paste(system2("pdftotext", c("-raw", shQuote(arch), "-"), stdout = TRUE), collapse = " ")
  pdf <- nrm(cru)
  chk(fmt, "ningún ##ANSWERi##, «$» ni LaTeX crudo", i, !grepl("##ANSWER|\\$|\\\\times|\\\\nobreak|\\{=latex\\}", cru))
  pq <- function(k) sub(paste0("^\\*\\*Parte ", k, "\\.\\*\\* "), paste0("Parte ", k, ". "), grep(paste0("^\\*\\*Parte ", k, "\\.\\*\\* "), q, value = TRUE))
  rang <- list(1:4, 5:8, NULL, NULL, NULL, 12:15)
  ult <- 0L; ok <- logical(6); det <- character(0)
  for (k in 1:6) {
    cadena <- nrm(pq(k)); p <- regexpr(cadena, substring(pdf, ult + 1L), fixed = TRUE)
    if (p < 0) { ok[k] <- FALSE; det <- c(det, paste("no hallo la pregunta", k)); next }
    fin_q <- ult + p + attr(p, "match.length") - 1L
    if (is.null(rang[[k]])) { ok[k] <- TRUE; ult <- fin_q; next }
    opc <- nrm(paste(ql[rang[[k]]], collapse = " / "))
    ok[k] <- identical(substring(pdf, fin_q + 1L, fin_q + nchar(opc)), opc)       # las opciones empiezan justo donde termina la pregunta
    if (!ok[k]) det <- c(det, paste("Parte", k, "las opciones no siguen a la pregunta"))
    ult <- fin_q + nchar(opc)
  }
  chk(fmt, "cada pregunta va seguida, sin nada en medio, de sus opciones (Partes 1, 2, 6) o de la pregunta siguiente (3, 4, 5)", i, all(ok), det)
  fo <- filas_en_orden(cel_md(q), pdf)
  chk(fmt, "las seis filas de las dos tablas (cabecera y dos filas, completas y en orden) aparecen en el PDF", i, all(fo), paste("filas que faltan:", paste(which(!fo), collapse = ",")))
  fpdf <- vapply(fs_tex(e$solution), function(g) grepl(nrm(form_txt(g)), pdf, fixed = TRUE), NA)
  chk(fmt, "cada fórmula de la Solution (cifras y unidad, como texto) aparece en el PDF", i, all(fpdf), paste(fs_tex(e$solution)[!fpdf], collapse = " | "))
  ## --- Parte 2 en la Solution del PDF ---
  chk(fmt, "la Solution lleva el encabezado de la Parte 2 una sola vez y ninguno del tipo retirado", i,
      lengths(regmatches(pdf, gregexpr(nrm(H2), pdf, fixed = TRUE))) == 1L && !grepl(nrm("Comparaciones que no deciden"), pdf, fixed = TRUE))
  okb <- vapply(ep$bullets, function(b) grepl(nrm(b), pdf, fixed = TRUE), NA)
  chk(fmt, "las tres viñetas de la Parte 2 (opción incorrecta + explicación de su retroalimentación) aparecen en el PDF", i, all(okb), paste(ep$bullets[!okb], collapse = " ; "))
  chk(fmt, "«Respuestas» cita la opción correcta de la Parte 2 y «Procedimiento» define el promedio por curso", i,
      grepl(nrm(paste0("Parte 2. «", ep$clave, "»")), pdf, fixed = TRUE) && grepl(nrm(PROC2), pdf, fixed = TRUE))
  chk(fmt, "ni el enunciado ni la Solution conservan texto del tipo retirado de la Parte 2", i, !grepl(RETIRADO_FMT, cru))
}
if (!exists("SOLO_DEFINICIONES")) {
cat("\n=== PDF (pdf/cloze001..100.pdf) ===\n")
npag <- integer(0)
for (i in SEM) {
  arch <- file.path(SAL, "pdf", sprintf("cloze%03d.pdf", i))
  npag[i] <- as.integer(sub("Pages:\\s+", "", grep("Pages", system2("pdfinfo", shQuote(arch), stdout = TRUE), value = TRUE)))
  verificar_pdf(i, arch)
}
cat("  páginas por PDF (100 PDF): ", paste(names(table(npag)), table(npag), sep = " págs = ", collapse = " · "), " PDF\n", sep = "")
}

if (!exists("SOLO_DEFINICIONES")) {
## ================================ DOCX (solo la semilla 7: la canónica) ==============================================
cat("\n=== DOCX (docx/cloze_s0071.docx, semilla 7) ===\n")
dx <- system2("pandoc", c("-f", "docx", "-t", "plain", "--wrap=none", shQuote(file.path(SAL, "docx", "cloze_s0071.docx"))), stdout = TRUE)
dxt <- nb(paste(dx, collapse = "\n"))
e7 <- ex[[7]][[1]]; q1 <- nb(e7$question); ql1 <- nb(e7$questionlist); ep7 <- esperados_p2(e7)
chk("DOCX", "ningún ##ANSWERi## sin resolver (exams2pandoc no procesa los gaps del cloze: DECLARADO, el DOCX no es formato de entrega del CLOZE)", 7L,
    !grepl("##ANSWER", dxt, fixed = TRUE), paste("marcadores visibles:", lengths(regmatches(dxt, gregexpr("##ANSWER[0-9]##", dxt)))), decl = TRUE)
i_o1 <- regexpr(substr(ql1[1], 1, 40), dxt, fixed = TRUE)
chk("DOCX", "las opciones de la Parte 1 salen justo después de su pregunta (exams2pandoc las agrupa tras la Parte 6: DECLARADO)", 7L,
    i_o1 > 0 && i_o1 < regexpr("Parte 2\\.", dxt), paste("posición de las opciones de la Parte 1:", i_o1, "· posición de «Parte 2.»:", regexpr("Parte 2\\.", dxt)), decl = TRUE)
cat("  (informativo) las opciones aparecen agrupadas tras la Parte 6, con rótulos (a), (b), (c): ", grepl("\\(a\\) La sede", dxt) && grepl("\\(c\\) Si el grado", dxt), "\n")
chk("DOCX", "las seis filas de las dos tablas (cabecera y dos filas, completas y en orden)", 7L, all(filas_en_orden(cel_md(q1), nrm(dxt))))
chk("DOCX", "la Solution lleva el encabezado de la Parte 2, sus tres viñetas y la línea de «Respuestas»; nada del tipo retirado", 7L,
    lengths(regmatches(dxt, gregexpr(H2, dxt, fixed = TRUE))) == 1L && all(vapply(ep7$bullets, function(b) grepl(ws(b), ws(dxt), fixed = TRUE), NA)) &&
    grepl(ws(paste0("Parte 2. «", ep7$clave, "»")), ws(dxt), fixed = TRUE) && !grepl(RETIRADO_FMT, dxt))
}

## ================================ MOODLE (XML) ====================================================================
## `sem_de(k)` da el número de semilla (XML por semilla) o el de pregunta (XML del aula) que se imprime junto a cada comprobación.
verificar_moodle <- function(arch, fmt, sem_de) {
  r <- read_xml(arch, encoding = "UTF-8")
  qs <- xml_find_all(r, "//question[@type='cloze']")
  crudo <- paste(readLines(arch, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  for (k in seq_along(qs)) {
    s <- sem_de(k); f <- function(id, ok, det = "") chk(fmt, id, s, ok, det)
    html <- xml_text(xml_find_first(qs[[k]], "./questiontext/text"))
    doc <- read_html(paste0("<div>", html, "</div>"))
    tabs <- xml_find_all(doc, "//table")
    f("dos tablas, cada una de 3 filas × 3 columnas", length(tabs) == 2L && all(vapply(tabs, function(t) length(xml_find_all(t, ".//tr")) == 3L, NA)))
    tc <- lapply(tabs, function(t) lapply(xml_find_all(t, ".//tr"), function(rr) limpiar(xml_text(xml_find_all(rr, "./th|./td")))))
    cel <- unlist(tc, recursive = FALSE)
    fila <- function(z) suppressWarnings(c(n = as.numeric(sub(" cursos$", "", z[2])), a = as.numeric(sub(" kilos$", "", z[3]))))
    n1 <- c(fila(cel[[2]])[["n"]], fila(cel[[3]])[["n"]]); a1 <- c(fila(cel[[2]])[["a"]], fila(cel[[3]])[["a"]])
    n2 <- c(fila(cel[[5]])[["n"]], fila(cel[[6]])[["n"]]); a2 <- c(fila(cel[[5]])[["a"]], fila(cel[[6]])[["a"]])
    fil1 <- c(cel[[2]][1], cel[[3]][1]); fil2 <- c(cel[[5]][1], cel[[6]][1])
    mat2 <- sub("^Promedio de (\\S+) recogido por curso$", "\\1", cel[[4]][3])
    ps <- vapply(xml_find_all(doc, "//p"), function(p) trimws(nb(xml_text(p))), "")
    ps <- ps[nzchar(ps)]
    f("ningún ##ANSWERi## sin resolver", !any(grepl("##ANSWER", ps, fixed = TRUE)))
    pos <- vapply(1:6, function(j) which(startsWith(ps, sprintf("Parte %d.", j)))[1], 1L)
    f("las seis preguntas aparecen en orden", !anyNA(pos) && !is.unsorted(pos))
    gap <- vapply(1:6, function(j) ps[pos[j] + 1L], "")
    f("cada gap va en el párrafo siguiente a su pregunta", all(grepl("^\\{[0-9]+:(MULTICHOICE_V|MULTIRESPONSE|NUMERICAL):", gap)))
    tipo <- sub("^\\{[0-9]+:([A-Z_]+):.*$", "\\1", gap); peso <- as.numeric(sub("^\\{([0-9]+):.*$", "\\1", gap))
    f("tipos de gap: MULTICHOICE_V, MULTICHOICE_V, NUMERICAL ×3, MULTIRESPONSE", identical(tipo, c("MULTICHOICE_V", "MULTICHOICE_V", "NUMERICAL", "NUMERICAL", "NUMERICAL", "MULTIRESPONSE")), paste(tipo, collapse = ","))
    f("pesos de los gaps 3|1|1|1|1|1 (F-3)", identical(peso, c(3, 1, 1, 1, 1, 1)), paste(peso, collapse = "|"))
    f("defaultgrade = 8", as.numeric(xml_text(xml_find_first(qs[[k]], "./defaultgrade"))) == 8)
    cuerpo <- function(g) sub("^\\{[0-9]+:[A-Z_]+:", "", sub("\\}$", "", g))
    opcs <- function(g) { it <- strsplit(cuerpo(g), "~", fixed = TRUE)[[1]]
      marca <- startsWith(it, "=") | (startsWith(it, "%") & !grepl("^%-", it))
      list(txt = sub("^(=|%-?[0-9.]+%)", "", it), ok = marca) }
    o1 <- opcs(gap[1]); o2 <- opcs(gap[2]); o6 <- opcs(gap[6])
    f("cuatro opciones en cada gap de elección", length(o1$txt) == 4L && length(o2$txt) == 4L && length(o6$txt) == 4L)
    ## Parte 1: recalculada desde la tabla 1 del XML
    en1 <- grep("La tabla muestra la cantidad de cursos", ps, value = TRUE)[1]
    m1 <- regmatches(en1, regexec("por curso en (.+) y en (.+)\\.$", en1))[[1]]; nm1 <- m1[2:3]
    f("los nombres del enunciado = las filas de la tabla 1", endsWith(tolower(nm1[1]), tolower(fil1[1])) && endsWith(tolower(nm1[2]), tolower(fil1[2])))
    ev1 <- lapply(o1$txt, eval_p1, nm = nm1, n = n1, a = a1)
    v1 <- vapply(ev1, function(z) isTRUE(z$v), NA)
    f("Parte 1: una sola verdadera, y es la marcada en el XML", all(vapply(ev1, function(z) isTRUE(z$reconocida), NA)) && sum(v1) == 1L && identical(v1, o1$ok), paste("marcas", paste(o1$ok, collapse = " "), "recalculado", paste(v1, collapse = " ")))
    ## Parte 2: la pregunta cita el material; las cuatro opciones salen de mi banco; una sola marcada y es la que juzgué correcta
    pr2 <- sub("^Parte 2\\. ", "", ps[pos[2]]); m_pr2 <- regmatches(pr2, regexec(P2_RE_PREG, pr2))[[1]]
    f("Parte 2: la pregunta es la vigente y cita el material de la tabla del otro colegio", length(m_pr2) == 2L && identical(m_pr2[2], mat2), paste(pr2, "|", mat2))
    f("Parte 2: las cuatro opciones salen de mi banco, una sola marcada y es la que juzgué correcta",
      all(o2$txt %in% POOL_P2$texto) && identical(o2$ok, juicio_p2(o2$txt) %in% "correcta") && sum(o2$ok) == 1L, paste("marcas:", paste(o2$ok, collapse = " "), "| juicio:", paste(juicio_p2(o2$txt), collapse = " ")))
    ## Partes 3 a 5
    t2 <- n2 * a2; rr <- function(j) suppressWarnings(as.numeric(sub("^=(-?[0-9.]+):.*$", "\\1", cuerpo(gap[j]))))
    g3 <- sub("^.*en total el grado (\\S+)\\?$", "\\1", ps[pos[3]]); g4 <- sub("^.*en total el grado (\\S+)\\?$", "\\1", ps[pos[4]])
    f("Partes 3 y 4: el total del grado preguntado, leído de la tabla 2", isTRUE(rr(3) == t2[match(g3, tolower(fil2))] && rr(4) == t2[match(g4, tolower(fil2))] && setequal(c(g3, g4), tolower(fil2))),
      paste("Partes 3 y 4:", rr(3), rr(4), "esperado", t2[match(g3, tolower(fil2))], t2[match(g4, tolower(fil2))]))
    f("Parte 5: la diferencia de los dos totales de la tabla 2", isTRUE(rr(5) == max(t2) - min(t2)), paste("Parte 5:", rr(5), "esperado", max(t2) - min(t2)))
    f("tolerancia 0 en las tres Partes numéricas", all(grepl(":0\\}?$", gap[3:5])))
    ## Parte 6
    ev6 <- lapply(o6$txt, eval_p6, noms = fil2, n = n2, a = a2); v6 <- vapply(ev6, function(z) z$v, NA)
    f("Parte 6: las marcas del XML = el veredicto recalculado (al menos una verdadera y una falsa)", !anyNA(v6) && identical(as.logical(v6), o6$ok) && any(v6) && any(!v6), paste("marcas", paste(o6$ok, collapse = " "), "recalculado", paste(v6, collapse = " ")))
    ## retroalimentación general = la Solution, con la Parte 2 correcta
    gf <- xml_text(xml_find_first(qs[[k]], "./generalfeedback/text"))
    mf <- regmatches(gf, gregexpr("\\\\\\(.*?\\\\\\)", gf, perl = TRUE))[[1]]; tf <- gsub("^\\\\\\(|\\\\\\)$", "", mf)
    f("generalfeedback: las fórmulas van como \\(…\\), ninguna como $…$ (Moodle con MathJax) y hay al menos 10", length(mf) >= 10L && !grepl("$", gf, fixed = TRUE), length(mf))
    f("generalfeedback: toda fórmula \\(…\\) es una operación bien calculada, con su unidad y su signo (si los lleva) dentro de ella (L-6, p. ej. \\text{kilos.})",
      length(tf) >= 10L && all(vapply(tf, function(z) { r <- lee_f(paste0("$", z, "$")); !is.null(r) && isTRUE(r$ok) }, NA)) && any(grepl("\\text{kilos.}", tf, fixed = TRUE)), paste(tf[!vapply(tf, function(z) { r <- lee_f(paste0("$", z, "$")); !is.null(r) && isTRUE(r$ok) }, NA)], collapse = " | "))
    f("generalfeedback: ningún signo de puntuación pegado tras el cierre de una fórmula (L-6)", !grepl("\\\\\\)</span>[.,:;!?)»”…]", gf, perl = TRUE), paste(regmatches(gf, regexpr(".{25}\\\\\\)</span>[.,:;!?)»”…]", gf, perl = TRUE)), collapse = " | "))
    f("generalfeedback con texto y sin ##ANSWER", nzchar(gf) && !grepl("##ANSWER", gf, fixed = TRUE))
    gdoc <- read_html(paste0("<div>", gf, "</div>"))
    gps <- vapply(xml_find_all(gdoc, "//p"), function(p) ws(xml_text(p)), "")
    f("generalfeedback: un único encabezado «Parte 2 · …», el vigente", sum(gps == H2) == 1L && sum(startsWith(gps, "Parte 2 ·")) == 1L)
    gl <- vapply(xml_find_all(gdoc, sprintf("//p[normalize-space(.)='%s']/following-sibling::ul[1]/li", H2)), function(z) ws(xml_text(z)), "")
    inc <- o2$txt[!(juicio_p2(o2$txt) %in% "correcta")]
    cab <- sub("^(«[^»]+»).*$", "\\1", gl)
    f("generalfeedback: las tres viñetas de la Parte 2 son las tres opciones incorrectas del XML, cada una con la explicación propia de su opción",
      length(gl) == 3L && setequal(cab, paste0("«", inc, "»")) &&
      all(vapply(seq_along(gl), function(b) { id <- POOL_P2$id[match(sub("^«([^»]+)».*$", "\\1", gl[b]), POOL_P2$texto)]; !is.na(id) && grepl(POOL_P2$ancla[POOL_P2$id == id], gl[b], fixed = TRUE) }, NA)), paste(gl, collapse = " ## "))
    ok_cl <- vapply(gl, function(b) {                        # lo que dice cada viñeta que da la operación, recalculado con la tabla 2 del XML
      id <- POOL_P2$id[match(sub("^«([^»]+)».*$", "\\1", b), POOL_P2$texto)]
      if (is.na(id)) return(FALSE)
      f_op <- P2_OP[[id]]; if (is.null(f_op)) return(TRUE)   # «la mayoría de los cursos»: la tabla no permite calcularla
      v <- vapply(1:2, function(g) f_op(n2[g], a2[g], sum(n2)), 1)
      afirma <- regmatches(b, gregexpr(P2_RE_AFIRMA, b, perl = TRUE))[[1]]
      cl_da <- regmatches(b, gregexpr("\\bda\\b.*?(?=\\bda\\b|[,:.;]|$)", b, perl = TRUE))[[1]]      # ninguna cláusula «da …» que el oráculo no sepa recalcular
      length(afirma) >= 1L && all(vapply(cl_da, function(z) any(startsWith(z, grep("^da ", names(P2_AFIRMA), value = TRUE))), NA)) &&
        all(vapply(afirma, function(z) all(vapply(1:2, function(g) P2_AFIRMA[[z]](v[g], n2[g], a2[g]), NA)), NA))
    }, NA)
    f("generalfeedback: lo que cada viñeta de la Parte 2 dice que da la operación es cierto con las dos filas de la tabla del XML", length(gl) == 3L && all(ok_cl), paste(gl[!ok_cl], collapse = " ## "))
    resp2 <- vapply(xml_find_all(gdoc, "//li[starts-with(normalize-space(.), 'Parte 2.')]"), function(z) ws(xml_text(z)), "")
    f("generalfeedback: «Respuestas» cita la opción que el XML marca como correcta en la Parte 2", length(resp2) == 1L && identical(resp2, ws(paste0("Parte 2. «", o2$txt[o2$ok], "»"))), paste(resp2, collapse = " | "))
    f("generalfeedback: «Procedimiento» define el promedio por curso", PROC2 %in% gps)
    f("sin texto del tipo retirado de la Parte 2 en la pregunta ni en la retroalimentación", !grepl(RETIRADO_FMT, paste(html, gf)))
    nom <- xml_text(xml_find_first(qs[[k]], "./name/text"))
    f("el nombre de la pregunta no lleva ruta", !grepl("_home_|/home/", nom), nom)
  }
  ## guardas de exportar_moodle_cloze.R sobre el archivo entero
  nq <- length(qs)
  cuenta <- function(p) lengths(regmatches(crudo, gregexpr(p, crudo)))
  g <- c(desplegable = cuenta("\\{[0-9]+:MULTICHOICE:"), vertical = cuenta("\\{[0-9]+:MULTICHOICE_V:"), multirespuesta = cuenta("\\{[0-9]+:MULTIRESPONSE:"), numerico = cuenta("\\{[0-9]+:NUMERICAL:"))
  chk(fmt, "guarda del archivo: 0 desplegables, 2 MULTICHOICE_V + 1 MULTIRESPONSE + 3 NUMERICAL por pregunta", 0L, g[["desplegable"]] == 0L && g[["vertical"]] == 2L * nq && g[["multirespuesta"]] == nq && g[["numerico"]] == 3L * nq, paste(names(g), g, collapse = " "))
  chk(fmt, "guarda del archivo: ninguna imagen ni @@PLUGINFILE@@ en todo el XML", 0L, !grepl("<img|@@PLUGINFILE@@", crudo))
  list(n = nq, gaps = g)
}
if (!exists("SOLO_DEFINICIONES")) {
cat("\n=== MOODLE moodle/s001..s100.xml (un XML por semilla, set.seed(s) + n = 1) ===\n")
for (i in SEM) {
  arch <- file.path(SAL, "moodle", sprintf("s%03d.xml", i))
  r <- verificar_moodle(arch, "MOODLE", function(k) i)
  chk("MOODLE", "una sola pregunta cloze por archivo", i, r$n == 1L)
  ## cruce informativo con lo que R/exams emite para la misma semilla: mismas dos tablas
  rd <- read_xml(arch, encoding = "UTF-8"); h <- xml_text(xml_find_first(rd, "//question[@type='cloze']/questiontext/text"))
  cm <- lapply(xml_find_all(read_html(paste0("<div>", h, "</div>")), "//table"), function(t) lapply(xml_find_all(t, ".//tr"), function(r) limpiar(xml_text(xml_find_all(r, "./th|./td")))))
  chk("MOODLE", "las celdas de las dos tablas del XML = las que R/exams emite (xexams) para la misma semilla", i, identical(unlist(cm), unlist(cel_md(nb(ex[[i]][[1]]$question)))))
  gfm <- xml_text(xml_find_first(rd, "//question[@type='cloze']/generalfeedback/text")); mfm <- gsub("^\\\\\\(|\\\\\\)$", "", regmatches(gfm, gregexpr("\\\\\\(.*?\\\\\\)", gfm, perl = TRUE))[[1]])
  chk("MOODLE", "las fórmulas del XML (\\(…\\)) son exactamente las de la Solution y la retroalimentación emitidas por R/exams para la misma semilla (unidad y signo dentro, L-6)", i,
      identical(sort(mfm), sort(c(fs_tex(ex[[i]][[1]]$solution), fs_tex(ex[[i]][[1]]$solutionlist)))), paste(setdiff(c(fs_tex(ex[[i]][[1]]$solution), fs_tex(ex[[i]][[1]]$solutionlist)), mfm), collapse = " | "))
}
cat("\n=== MOODLE moodle_aula/totales_promedios_cloze.xml (30 versiones en un XML: la ruta real del aula) ===\n")
ra <- verificar_moodle(file.path(SAL, "moodle_aula", "totales_promedios_cloze.xml"), "MOODLE_AULA", function(k) k)
chk("MOODLE_AULA", "30 preguntas cloze", 0L, ra$n == 30L, ra$n)
pesos_aula <- vapply(strsplit(paste(readLines(file.path(SAL, "moodle_aula", "totales_promedios_cloze.xml"), encoding = "UTF-8", warn = FALSE), collapse = "\n"), "<question type=\"cloze\">", fixed = TRUE)[[1]][-1],
                     function(qq) paste(regmatches(qq, gregexpr("(?<=\\{)[0-9]+(?=:(MULTICHOICE|MULTIRESPONSE|NUMERICAL))", qq, perl = TRUE))[[1]], collapse = "|"), "")
chk("MOODLE_AULA", "secuencia de pesos 3|1|1|1|1|1 en cada una de las 30 preguntas (como la guarda del exportador)", 0L, length(pesos_aula) == 30L && all(pesos_aula == "3|1|1|1|1|1"), unique(pesos_aula))
## etiquetas HTML dentro de los gaps (Incidente G): el script del proyecto, en Python aislado, sobre los 130 XML
xmls <- c(file.path(SAL, "moodle", sprintf("s%03d.xml", SEM)), file.path(SAL, "moodle_aula", "totales_promedios_cloze.xml"))
etq <- system2("python3", c("-I", normalizePath(file.path(DIR_S, "..", "..", "verificar_gaps_xml.py")), shQuote(xmls)), stdout = TRUE)
tot <- grep("^TOTAL:", etq, value = TRUE); cat("  ", tot, "\n")
n_etq <- as.integer(sub("^.*etiquetas dentro de gaps: ([0-9]+).*$", "\\1", tot)); n_gap <- as.integer(sub("^TOTAL: gaps ([0-9]+),.*$", "\\1", tot))
chk("MOODLE", "ninguna etiqueta HTML dentro de un gap (verificar_gaps_xml.py sobre los 101 XML)", 0L, length(n_etq) == 1L && n_etq == 0L && n_gap == 6L * 130L, tot)

## ================================ resumen =========================================================================
cat("\n=== RESUMEN POR COMPROBACIÓN (OK/total; entre paréntesis, las semillas o preguntas que fallan) ===\n")
claves <- unique(REG[, c("fmt", "id")])
for (k in seq_len(nrow(claves))) {
  sub <- REG[REG$fmt == claves$fmt[k] & REG$id == claves$id[k], ]
  mal <- sub[!sub$ok, ]
  cat(sprintf("  [%s] %-12s %3d/%-3d %s%s\n", if (nrow(mal) == 0L) "OK   " else if (all(sub$decl)) "DECL " else "FALLA", claves$fmt[k], sum(sub$ok), nrow(sub), claves$id[k],
              if (nrow(mal)) paste0("  (", paste(head(mal$sem, 12), collapse = ","), if (nrow(mal) > 12) ",…" else "", ") -> ", mal$det[1]) else ""))
}
real <- REG[!REG$ok & !REG$decl, ]; decl <- REG[!REG$ok & REG$decl, ]
cat(sprintf("\nComprobaciones ejecutadas entre formatos: %d (HTML %d · PDF %d · DOCX %d · MOODLE %d · MOODLE_AULA %d)\n", nrow(REG),
            sum(REG$fmt == "HTML"), sum(REG$fmt == "PDF"), sum(REG$fmt == "DOCX"), sum(REG$fmt == "MOODLE"), sum(REG$fmt == "MOODLE_AULA")))
cat("Archivos verificados: HTML 100 · PDF 100 · DOCX 1 · XML de Moodle 100 (una pregunta) + 1 del aula (30 preguntas)\n")
cat("TOTAL de verificaciones fallidas entre formatos (sin las declaradas):", nrow(real), "\n")
cat("Verificaciones fallidas DECLARADAS (DOCX de un cloze, no es formato de entrega):", nrow(decl), "\n")
saveRDS(REG, file.path(DIR_S, "resultados_formatos.rds"))
}
