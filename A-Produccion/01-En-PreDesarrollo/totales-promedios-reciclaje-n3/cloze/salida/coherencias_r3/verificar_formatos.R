## verificar_formatos.R — Paso 8 (coherencias_5), ronda 3: coherencia entre lo que R/exams emite (xexams) y los ARCHIVOS
## ya renderizados (HTML, PDF, DOCX, Moodle), más los dos XML de Moodle (5 + 30 versiones) recalculados desde cero.
## Reutiliza los evaluadores independientes de verificar_coherencias.R (solo sus funciones y pools, sin ejecutarlo).
## Uso (desde esta carpeta): TMPDIR=<tmp> Rscript --no-init-file verificar_formatos.R
suppressMessages({library(exams); library(xml2)})
SAL <- normalizePath("..")                    # cloze/salida
F <- normalizePath("../../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
cat("md5 del .Rmd:", unname(tools::md5sum(F)), "\n")
## importar solo definiciones (nb, limpiar, num, pools, evaluadores) del verificador principal
for (e in parse("verificar_coherencias.R", encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in%
      c("nb", "limpiar", "num", "P2_PREG", "P2_OK", "P2_MAL", "P2_ANCLA", "eval_p1", "eval_p6")) eval(e, globalenv())
SEM <- c(canonica = 7L, CxC = 3L, CxP = 2L, PxC = 1L, PxP = 4L)     # cloze1..cloze5 (orden de render_cloze.R)
ex <- xexams(F, n = 5, seed = unname(SEM), driver = list(sweave = list(quiet = TRUE, pdf = FALSE, png = TRUE, encoding = "UTF-8"),
                                                          read = NULL, transform = NULL, write = NULL))
fallos <- character(0)
nota <- function(id, ok, det = "") { det <- paste(det, collapse = " "); cat(sprintf("  [%s] %s%s\n", if (isTRUE(ok)) "OK   " else "FALLA", id, if (!isTRUE(ok) && nzchar(det)) paste0("  -> ", substr(det, 1, 300)) else ""))
                                      if (!isTRUE(ok)) fallos <<- c(fallos, id) }
cel_md <- function(q) { tl <- grep("^\\|", q, value = TRUE); tl <- tl[!grepl("^\\|[-:| ]+\\|$", tl)]
  lapply(tl, function(r) limpiar(strsplit(sub("\\|\\s*$", "", sub("^\\|", "", r)), "|", fixed = TRUE)[[1]])) }

## ================================ HTML =============================================================================
cat("\n=== HTML (cloze1..5.html) ===\n")
for (i in 1:5) {
  e <- ex[[i]][[1]]; q <- nb(e$question); ql <- nb(e$questionlist)
  doc <- read_html(file.path(SAL, "html", sprintf("cloze%d.html", i)), encoding = "UTF-8")
  tabs <- xml_find_all(doc, "//table")
  cel_html <- lapply(tabs, function(t) lapply(xml_find_all(t, ".//tr"), function(r) limpiar(xml_text(xml_find_all(r, "./th|./td")))))
  md <- cel_md(q)
  nota(sprintf("HTML %d (semilla %d): dos tablas, cada una de 3 filas × 3 columnas", i, SEM[[i]]), length(tabs) == 2L && all(lengths(cel_html) == 3L))
  nota(sprintf("HTML %d: las celdas de las dos tablas = las de la pregunta emitida por R/exams (misma versión)", i),
       identical(unlist(cel_html), unlist(md)), paste(setdiff(unlist(md), unlist(cel_html)), collapse = "|"))
  ps <- vapply(xml_find_all(doc, "//p"), function(p) trimws(nb(xml_text(p))), "")
  nota(sprintf("HTML %d: ningún ##ANSWERi## sin resolver", i), !any(grepl("##ANSWER", ps, fixed = TRUE)) && !grepl("##ANSWER", paste(xml_text(doc)), fixed = TRUE))
  pos <- vapply(1:6, function(k) which(startsWith(ps, sprintf("Parte %d.", k)))[1], 1L)
  nota(sprintf("HTML %d: las seis preguntas aparecen en orden", i), !anyNA(pos) && !is.unsorted(pos))
  rangos <- list(1:4, 5:8, NULL, NULL, NULL, 12:15)
  ok_gap <- vapply(1:6, function(k) {
    sig <- ps[pos[k] + 1L]
    if (is.null(rangos[[k]])) identical(sig, "") else identical(sig, paste(ql[rangos[[k]]], collapse = " / "))
  }, NA)
  nota(sprintf("HTML %d: cada gap (opciones o campo num) va en el párrafo siguiente a su pregunta, con las opciones del exshuffle", i), all(ok_gap), paste(which(!ok_gap), collapse = ","))
  xml_remove(xml_find_all(doc, "//annotation"))     # el MathML guarda el TeX en <annotation>: no se muestra
  txt <- nb(xml_text(doc))
  nota(sprintf("HTML %d: sin LaTeX crudo ni «$» sueltos en el texto visible", i), !grepl("\\\\times|\\$|\\\\nobreak|\\{=latex\\}", txt))
  nota(sprintf("HTML %d: las ecuaciones salieron como MathML/MathJax (cuenta ≥ las de la Solution)", i),
       length(xml_find_all(doc, "//math|//span[contains(@class,'math')]")) >= 10L, length(xml_find_all(doc, "//math|//span[contains(@class,'math')]")))
  tipo <- if (any(grepl("¿Qué indica, en la tabla del otro colegio", ps, fixed = TRUE))) "promedio" else "comparar"
  h_si <- if (tipo == "promedio") "Parte 2 · Lecturas del promedio que no sirven" else "Parte 2 · Comparaciones que no deciden qué grado ganó"
  h_no <- if (tipo == "promedio") "Parte 2 · Comparaciones que no deciden qué grado ganó" else "Parte 2 · Lecturas del promedio que no sirven"
  nota(sprintf("HTML %d: Solution con el encabezado de su tipo de Parte 2 («%s»)", i, h_si), sum(ps == h_si) == 1L && sum(ps == h_no) == 0L)
  nota(sprintf("HTML %d: la Solution no cita letras de opciones (regla #19)", i),
       !grepl("[Oo]pci[oó]n [A-D]\\b|\\([a-d]\\)|\\b[Ll]etra\\b", paste(ps[(which(ps == "Respuestas")[1]):length(ps)], collapse = " ")))
}

## ================================ PDF ==============================================================================
cat("\n=== PDF (cloze1..5.pdf) ===\n")
nrm <- function(x) gsub("[^[:alnum:]áéíóúüñ]", "", gsub("fi|fl|¿|¾", "", tolower(nb(x))))   # ligaturas fi/fl y «¿» salen mal en pdftotext
for (i in 1:5) {
  e <- ex[[i]][[1]]; q <- nb(e$question); ql <- nb(e$questionlist)
  pdf <- nrm(paste(system2("pdftotext", c("-raw", shQuote(file.path(SAL, "pdf", sprintf("cloze%d.pdf", i))), "-"), stdout = TRUE), collapse = " "))
  cru <- paste(system2("pdftotext", c("-raw", shQuote(file.path(SAL, "pdf", sprintf("cloze%d.pdf", i))), "-"), stdout = TRUE), collapse = " ")
  nota(sprintf("PDF %d: ningún ##ANSWERi##, «$» ni LaTeX crudo", i), !grepl("##ANSWER|\\$|\\\\times|\\\\nobreak|\\{=latex\\}", cru))
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
  nota(sprintf("PDF %d: cada pregunta va seguida, sin nada en medio, de sus opciones (Partes 1, 2, 6) o de la pregunta siguiente (3, 4, 5)", i), all(ok), det)
  ## las dos tablas, con sus celdas, en el texto del PDF
  cm <- unlist(cel_md(q)); nota(sprintf("PDF %d: las 18 celdas de las dos tablas aparecen en el PDF", i), all(vapply(cm, function(z) grepl(nrm(z), pdf, fixed = TRUE), NA)),
                                paste(cm[!vapply(cm, function(z) grepl(nrm(z), pdf, fixed = TRUE), NA)], collapse = "|"))
  nota(sprintf("PDF %d: páginas", i), TRUE, "")
}
cat("  páginas por PDF:", paste(vapply(1:5, function(i) as.integer(sub("Pages:\\s+", "", grep("Pages", system2("pdfinfo", shQuote(file.path(SAL, "pdf", sprintf("cloze%d.pdf", i))), stdout = TRUE), value = TRUE))), 1L), collapse = " "), "\n")

## ================================ DOCX (solo la canónica) ===========================================================
cat("\n=== DOCX (cloze1.docx, semilla 7) ===\n")
dx <- system2("pandoc", c("-f", "docx", "-t", "plain", "--wrap=none", shQuote(file.path(SAL, "docx", "cloze1.docx"))), stdout = TRUE)
dxt <- nb(paste(dx, collapse = "\n"))
nota("DOCX: ningún ##ANSWERi## sin resolver", !grepl("##ANSWER", dxt, fixed = TRUE), paste("marcadores visibles:", lengths(regmatches(dxt, gregexpr("##ANSWER[0-9]##", dxt)))))
q1 <- nb(ex[[1]][[1]]$question); ql1 <- nb(ex[[1]][[1]]$questionlist)
i_p6 <- regexpr("Parte 6\\.", dxt); i_o1 <- regexpr(substr(ql1[1], 1, 40), dxt, fixed = TRUE)
nota("DOCX: las opciones de la Parte 1 salen justo después de su pregunta", i_o1 > 0 && i_o1 < regexpr("Parte 2\\.", dxt), paste("posición de las opciones de la Parte 1:", i_o1, "· posición de «Parte 2.»:", regexpr("Parte 2\\.", dxt)))
cat("  (informativo) las opciones aparecen agrupadas tras la Parte 6, con rótulos (a), (b), (c): ", grepl("\\(a\\) La sede", dxt) && grepl("\\(c\\) Si el grado", dxt), "\n")
nota("DOCX: las dos tablas con sus celdas", all(vapply(unlist(cel_md(q1)), function(z) grepl(z, dxt, fixed = TRUE), NA)))

## ================================ MOODLE (XML) ====================================================================
verificar_moodle <- function(arch) {
  r <- read_xml(arch, encoding = "UTF-8")
  qs <- xml_find_all(r, "//question[@type='cloze']")
  res <- character(0); nq <- 0L
  for (k in seq_along(qs)) {
    nq <- nq + 1L; id <- sprintf("pregunta %d", k)
    f <- function(ok, msg) if (!isTRUE(ok)) res <<- c(res, paste(id, "—", msg))
    html <- xml_text(xml_find_first(qs[[k]], "./questiontext/text"))
    doc <- read_html(paste0("<div>", html, "</div>"))
    tabs <- xml_find_all(doc, "//table")
    f(length(tabs) == 2L, "no hay dos tablas")
    tc <- lapply(tabs, function(t) lapply(xml_find_all(t, ".//tr"), function(rr) limpiar(xml_text(xml_find_all(rr, "./th|./td")))))
    f(all(lengths(tc) == 3L), "tablas no 3 × 3")
    cel <- unlist(tc, recursive = FALSE)
    fila <- function(z) c(n = as.numeric(sub(" cursos$", "", z[2])), a = as.numeric(sub(" kilos$", "", z[3])))
    n1 <- c(fila(cel[[2]])[["n"]], fila(cel[[3]])[["n"]]); a1 <- c(fila(cel[[2]])[["a"]], fila(cel[[3]])[["a"]])
    n2 <- c(fila(cel[[5]])[["n"]], fila(cel[[6]])[["n"]]); a2 <- c(fila(cel[[5]])[["a"]], fila(cel[[6]])[["a"]])
    fil1 <- c(cel[[2]][1], cel[[3]][1]); fil2 <- c(cel[[5]][1], cel[[6]][1])
    ps <- vapply(xml_find_all(doc, "//p"), function(p) trimws(nb(xml_text(p))), "")
    ps <- ps[nzchar(ps)]
    f(!any(grepl("##ANSWER", ps, fixed = TRUE)), "queda un ##ANSWER")
    pos <- vapply(1:6, function(j) which(startsWith(ps, sprintf("Parte %d.", j)))[1], 1L)
    f(!anyNA(pos) && !is.unsorted(pos), "preguntas fuera de orden")
    gap <- vapply(1:6, function(j) ps[pos[j] + 1L], "")
    f(all(grepl("^\\{[0-9]+:(MULTICHOICE_V|MULTIRESPONSE|NUMERICAL):", gap)), "un gap no sigue a su pregunta")
    tipo <- sub("^\\{[0-9]+:([A-Z_]+):.*$", "\\1", gap); peso <- as.numeric(sub("^\\{([0-9]+):.*$", "\\1", gap))
    f(identical(tipo, c("MULTICHOICE_V", "MULTICHOICE_V", "NUMERICAL", "NUMERICAL", "NUMERICAL", "MULTIRESPONSE")), paste("tipos de gap:", paste(tipo, collapse = ",")))
    f(identical(peso, c(3, 1, 1, 1, 1, 1)), paste("pesos:", paste(peso, collapse = "|")))
    f(as.numeric(xml_text(xml_find_first(qs[[k]], "./defaultgrade"))) == 8, "defaultgrade distinto de 8")
    cuerpo <- function(g) sub("^\\{[0-9]+:[A-Z_]+:", "", sub("\\}$", "", g))
    opcs <- function(g) { it <- strsplit(cuerpo(g), "~", fixed = TRUE)[[1]]
      marca <- startsWith(it, "=") | (startsWith(it, "%") & !grepl("^%-", it))
      list(txt = sub("^(=|%-?[0-9.]+%)", "", it), ok = marca) }
    o1 <- opcs(gap[1]); o2 <- opcs(gap[2]); o6 <- opcs(gap[6])
    f(length(o1$txt) == 4L && length(o2$txt) == 4L && length(o6$txt) == 4L, "no hay 4 opciones en un gap de elección")
    ## Parte 1
    en1 <- grep("La tabla muestra la cantidad de cursos", ps, value = TRUE)[1]
    m1 <- regmatches(en1, regexec("por curso en (.+) y en (.+)\\.$", en1))[[1]]; nm1 <- m1[2:3]
    f(endsWith(tolower(nm1[1]), tolower(fil1[1])) && endsWith(tolower(nm1[2]), tolower(fil1[2])), "nombres del enunciado ≠ filas de la tabla 1")
    ev1 <- lapply(o1$txt, eval_p1, nm = nm1, n = n1, a = a1)
    v1 <- vapply(ev1, function(z) isTRUE(z$v), NA)
    f(all(vapply(ev1, function(z) isTRUE(z$reconocida), NA)) && sum(v1) == 1L && identical(v1, o1$ok), paste("Parte 1: marcas", paste(o1$ok, collapse = " "), "recalculado", paste(v1, collapse = " ")))
    ## Parte 2
    pr2 <- ps[pos[2]]; tp <- names(P2_PREG)[vapply(P2_PREG, function(p) grepl(p, sub("^Parte 2\\. ", "", pr2)), NA)]
    f(length(tp) == 1L, "Parte 2: tipo no reconocido")
    if (length(tp) == 1L) f(all(o2$txt %in% c(P2_OK[[tp]], P2_MAL[[tp]])) && identical(o2$ok, o2$txt %in% P2_OK[[tp]]) && sum(o2$ok) == 1L, paste("Parte 2:", paste(o2$ok, collapse = " ")))
    ## Partes 3 a 5
    t2 <- n2 * a2; r <- function(j) as.numeric(sub("^=(-?[0-9.]+):.*$", "\\1", cuerpo(gap[j])))
    g3 <- sub("^.*en total el grado (\\S+)\\?$", "\\1", ps[pos[3]]); g4 <- sub("^.*en total el grado (\\S+)\\?$", "\\1", ps[pos[4]])
    f(r(3) == t2[match(g3, tolower(fil2))] && r(4) == t2[match(g4, tolower(fil2))] && setequal(c(g3, g4), tolower(fil2)), paste("Partes 3 y 4:", r(3), r(4), "esperado", t2[match(g3, tolower(fil2))], t2[match(g4, tolower(fil2))]))
    f(r(5) == max(t2) - min(t2), paste("Parte 5:", r(5), "esperado", max(t2) - min(t2)))
    f(all(grepl(":0\\}?$", gap[3:5])), "tolerancia distinta de 0 en una Parte num")
    ## Parte 6
    ev6 <- lapply(o6$txt, eval_p6, noms = fil2, n = n2, a = a2); v6 <- vapply(ev6, function(z) z$v, NA)
    f(!anyNA(v6) && identical(as.logical(v6), o6$ok) && any(v6) && any(!v6), paste("Parte 6: marcas", paste(o6$ok, collapse = " "), "recalculado", paste(v6, collapse = " ")))
    ## retroalimentación general (Solution) presente y sin marcadores
    gf <- xml_text(xml_find_first(qs[[k]], "./generalfeedback/text"))
    f(nzchar(gf) && !grepl("##ANSWER", gf, fixed = TRUE), "generalfeedback vacío o con ##ANSWER")
  }
  list(n = nq, fallos = res)
}
for (arch in c(file.path(SAL, "moodle", "cloze.xml"), file.path(SAL, "moodle_aula", "totales_promedios_cloze.xml"))) {
  cat("\n=== MOODLE", sub(paste0(SAL, "/"), "", arch, fixed = TRUE), "===\n")
  r <- verificar_moodle(arch)
  cat(sprintf("  preguntas cloze: %d · fallos: %d\n", r$n, length(r$fallos)))
  if (length(r$fallos)) cat(paste0("   * ", r$fallos), sep = "\n")
  if (length(r$fallos)) fallos <- c(fallos, paste(basename(arch), r$fallos))
}
cat("\nTOTAL de verificaciones fallidas entre formatos:", length(fallos), "\n")
