## verificar_cuadernillos.R: guardas C1-C15 del motor de cuadernillos, y sus mutantes.
##
## Uso:
##   Rscript verificar_cuadernillos.R <dir_sesion>     verifica una sesión generada por generar_cuadernillos.R
##   Rscript verificar_cuadernillos.R --mutantes [M1,M7,…]   construye cada mutante en un tempdir y exige que SU guarda falle
##
## Exit (decidir_exit): 1 si hay algún FAIL; 2 si hay NO_VERIFICABLE; 0 si no. El exit 0 certifica C1-C9 y C11-C15.
## C10 (revisión visual en gris) es MANUAL: el verificador nunca lo cuenta como PASS; se cierra en el reporte de aprobación.
## Plan y criterios: PRPs/motor-cuadernillos-oficio-2col.md (raíz del repo). C13 (numeración impresa) y las ampliaciones
## de C1 (imágenes y tablas del Word dentro de su columna), C3 (lectura del log y tinta dentro del margen inferior), C4
## (fuentes Type 3), C14 (preguntas enteras) y C15 (retroalimentación solo para el docente) son del 2026-11-14: salieron
## de la corrida del examen real y están en el §10 de INVESTIGACION.md.

local({
  d <- Sys.getenv("CUADERNILLOS_MOTOR_DIR", "")
  if (!nzchar(d)) {
    f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    if (!length(f) || basename(f[1]) != "verificar_cuadernillos.R") stop("defina CUADERNILLOS_MOTOR_DIR")
    d <- dirname(normalizePath(f[1]))
    Sys.setenv(CUADERNILLOS_MOTOR_DIR = d)
  }
  source(file.path(d, "generar_cuadernillos.R"), local = globalenv())
})

## ---------------------------------------------------------------------------------------------------------------
## Utilidades
## ---------------------------------------------------------------------------------------------------------------
resultado <- function(id, estado, detalle = "") list(id = id, estado = estado, detalle = paste(detalle, collapse = "; "))

decidir_exit <- function(r) {
  e <- vapply(r, `[[`, "", "estado")
  if (any(e == "FAIL")) 1L else if (any(e == "NO_VERIFICABLE")) 2L else 0L
}

medir <- function(pdf, columnas, log = NULL) {
  args <- c("-I", shQuote(file.path(dir_motor(), "prototipo", "medir_pdf.py")), shQuote(pdf), "--columnas", columnas)
  if (!is.null(log)) args <- c(args, "--log", shQuote(log))
  out <- suppressWarnings(system2("python3", args, stdout = TRUE, stderr = TRUE))
  jsonlite::fromJSON(paste(out, collapse = "\n"), simplifyVector = FALSE)
}

## Un .log de LaTeX NO es UTF-8: pdflatex escribe tal cual los bytes de las fuentes T1 y TS1 (una «ó» es el byte F3).
## MEDIDO el 2026-11-14, leer ese archivo con readLines() falla de dos maneras, las dos en silencio:
##   - con options(encoding = "native.enc") (R sin .Rprofile), el texto llega con bytes inválidos y gregexpr(perl = TRUE)
##     solo AVISA («input string 1 is invalid UTF-8») y no devuelve coincidencias: C3 daba PASS con tres Overfull de
##     hasta 58,6 pt en el log;
##   - con options(encoding = "UTF-8") (la que fija el .Rprofile de la raíz del repo), readLines() CORTA la lectura en el
##     primer byte inválido («entrada inválida encontrada en la conexión de entrada»): todo Overfull posterior se pierde.
## Por eso se leen los BYTES (readBin no pasa por la opción `encoding`) y se convierten desde latin1, donde todo byte es
## un carácter. Se comprueba el resultado: si no queda UTF-8 válido, el log no se pudo leer y la guarda NO aprueba.
leer_log <- function(f) {
  b <- tryCatch(readBin(f, what = "raw", n = file.size(f)), error = function(e) NULL, warning = function(w) NULL)
  if (is.null(b) || !length(b)) return(NA_character_)
  x <- iconv(rawToChar(b[b != as.raw(0)]), from = "latin1", to = "UTF-8", sub = "?")
  if (is.na(x) || !validUTF8(x)) NA_character_ else x
}

overfull_pt <- function(x)
  as.numeric(regmatches(x, gregexpr("(?<=Overfull \\\\hbox \\()[0-9.]+(?=pt too wide)", x, perl = TRUE))[[1]])

paginas_pdf <- function(pdf) {
  x <- suppressWarnings(system2("pdfinfo", shQuote(pdf), stdout = TRUE, stderr = TRUE))
  as.integer(sub("^Pages:\\s+", "", grep("^Pages:", x, value = TRUE)))
}

texto_pdf <- function(pdf, desde = 1, hasta = 1)
  paste(suppressWarnings(system2("pdftotext", c("-f", desde, "-l", hasta, shQuote(pdf), "-"), stdout = TRUE, stderr = TRUE)),
        collapse = "\n")

docx_xml <- function(docx, parte) {
  paste(suppressWarnings(system2("unzip", c("-p", shQuote(docx), parte), stdout = TRUE, stderr = TRUE)), collapse = "\n")
}

texto_pdf_completo <- function(pdf)
  paste(suppressWarnings(system2("pdftotext", c(shQuote(pdf), "-"), stdout = TRUE, stderr = TRUE)), collapse = "\n")

## Margen inferior libre de cada página, en mm: distancia de la tinta más baja al borde de la hoja. Se rasteriza en gris y
## se pide a ImageMagick la caja de tinta («%@»). Una página en blanco (un solo color) da NA. Sirve para ver lo que el
## log no dice con claridad: algo que no se puede partir (una pregunta entera, una figura, una tabla) y es más alto que su
## columna se sale por abajo. MEDIDO el 2026-11-14 en el examen real con las preguntas enteras: 63 pt (2,2 cm) fuera.
MARGEN_INFERIOR_MIN_MM <- 10      # el margen de la plantilla es de 15 mm: tolera 5 mm de desborde
margen_inferior_mm <- function(pdf, ppp = 40) {
  td <- tempfile(); dir.create(td); on.exit(unlink(td, recursive = TRUE))
  system2("pdftoppm", c("-r", ppp, "-gray", "-png", shQuote(pdf), shQuote(file.path(td, "p"))), stdout = FALSE, stderr = FALSE)
  f <- sort(list.files(td, "\\.png$", full.names = TRUE))
  if (!length(f)) return(numeric(0))
  x <- suppressWarnings(system2("identify", c("-format", shQuote("%h %k %@\\n"), shQuote(f)), stdout = TRUE, stderr = FALSE))
  vapply(x[nzchar(x)], function(l) {
    v <- strsplit(l, " ", fixed = TRUE)[[1]]
    if (length(v) < 3 || v[2] == "1") return(NA_real_)
    g <- as.numeric(regmatches(v[3], gregexpr("[0-9]+", v[3]))[[1]])       # ancho, alto, x, y de la caja de tinta
    if (length(g) < 4) return(NA_real_)
    (as.numeric(v[1]) - (g[4] + g[2])) / ppp * 25.4
  }, 0, USE.NAMES = FALSE)
}

## ppi efectivos de las imágenes de un .docx: píxeles de word/media/* frente a su ancho en document.xml (cx en EMU).
ppi_docx <- function(docx) {
  doc <- docx_xml(docx, "word/document.xml"); rels <- docx_xml(docx, "word/_rels/document.xml.rels")
  dib <- regmatches(doc, gregexpr("<w:drawing>.*?</w:drawing>", doc, perl = TRUE))[[1]]
  if (!length(dib)) return(numeric(0))
  td <- tempfile(); dir.create(td)
  system2("unzip", c("-q", "-o", shQuote(docx), "word/media/*", "-d", shQuote(td)), stdout = FALSE, stderr = FALSE)
  vapply(dib, function(x) {
    cx <- as.numeric(sub('.*<wp:extent cx="(\\d+)".*', "\\1", x))
    rid <- sub('.*r:embed="([^"]+)".*', "\\1", x)
    destino <- sub(sprintf('.*Id="%s"[^>]*Target="([^"]+)".*', rid), "\\1", rels)
    if (identical(destino, rels)) destino <- sub(sprintf('.*Target="([^"]+)"[^>]*Id="%s".*', rid), "\\1", rels)
    f <- file.path(td, "word", destino)
    px <- as.numeric(suppressWarnings(system2("identify", c("-format", "%w", shQuote(paste0(f, "[0]"))), stdout = TRUE)))
    px / (cx / 914400)
  }, 0, USE.NAMES = FALSE)
}

## Umbral de C5: 300 ppi con 1 % de tolerancia. MEDIDO: una imagen generada a 300 ppp queda a 299,0 ppi en el Word, porque
## pandoc redondea hacia abajo la resolución guardada en el PNG (118,11 px/cm), y pdfimages -list redondea a enteros.
PPI_MIN <- 300 * 0.99
clasificar_ppi <- function(ppi) if (!length(ppi)) "PASS" else if (min(ppi) >= PPI_MIN) "PASS" else if (min(ppi) >= 150) "WARN" else "FAIL"

## Una cara «en blanco» no tiene tinta: se rasteriza en gris y debe quedar de un solo color. No sirve pdftotext: la
## plantilla de NOPS fuerza la cara 2 con un punto invisible, y pdftotext lo devuelve como «.» (MEDIDO).
cara_sin_tinta <- function(pdf, pagina) {
  td <- tempfile(); dir.create(td)
  system2("pdftoppm", c("-f", pagina, "-l", pagina, "-r", "40", "-gray", "-png", shQuote(pdf), shQuote(file.path(td, "c"))),
          stdout = FALSE, stderr = FALSE)
  f <- list.files(td, "\\.png$", full.names = TRUE)
  length(f) == 1 && identical(suppressWarnings(system2("identify", c("-format", "%k", shQuote(f)), stdout = TRUE)), "1")
}

peor <- function(estados) {
  orden <- c("FAIL", "NO_VERIFICABLE", "WARN", "MANUAL", "PASS")
  for (e in orden) if (e %in% estados) return(e)
  "PASS"
}

## ---------------------------------------------------------------------------------------------------------------
## Verificación de una sesión
## ---------------------------------------------------------------------------------------------------------------
verificar_sesion <- function(dir_ses) {
  dir_ses <- normalizePath(dir_ses, mustWork = TRUE)
  fp <- file.path(dir_ses, "firmas.rds")
  if (!file.exists(fp)) return(list(resultado("C0", "NO_VERIFICABLE", "no existe firmas.rds")))
  firmas <- readRDS(fp)
  ids <- firmas$ids; N <- length(ids); Q <- length(firmas$ejercicios)
  sal <- firmas$salidas
  r <- list()
  add <- function(x) r[[length(r) + 1L]] <<- x

  ## C9, C11, C12: pre-vuelo (se rejuzga desde preflight.log, independiente del motor)
  pf <- leer_preflight(file.path(dir_ses, "preflight.log"))
  jz <- juzgar_preflight(pf, "hoja" %in% sal)
  lim <- character(0)
  if (N < 1 || N > 100) lim <- c(lim, paste0("versiones fuera de 1..100: ", N))
  if (Q > 45) lim <- c(lim, paste0("más de 45 preguntas: ", Q))
  rest <- pf$tabla$j[!is.na(pf$tabla$estatico) & startsWith(pf$tabla$estatico, "RESTAURA")]
  add(resultado("C9", if (length(jz$C9)) "FAIL" else "PASS",
                if (length(jz$C9)) jz$C9 else sprintf("%d ejercicios con FIN; presupuesto por ejercicio en el log", sum(pf$tabla$fin))))
  add(resultado("C11", if (length(c(jz$C11, lim))) "FAIL" else "PASS",
                if (length(c(jz$C11, lim))) c(jz$C11, lim) else sprintf("N = %d, Q = %d, tipos y opciones admitidos", N, Q)))
  add(resultado("C12", if (length(jz$C12)) "FAIL" else "PASS",
                if (length(jz$C12)) jz$C12 else paste0("estado=igual en todos",
                  if (any(!is.na(pf$tabla$nuevos))) paste0("; motores añadidos por carga de paquetes (no es fuga): ",
                    paste(unique(unlist(strsplit(stats::na.omit(pf$tabla$nuevos), ","))), collapse = ", ")) else "",
                  if (length(rest)) paste0("; RESTAURA (opts_chunk/opts_knit/knit_hooks, knitr lo restaura) en ", length(rest),
                                           " ejercicio(s)") else "")))
  if (length(unlist(jz))) {
    for (g in c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C13", "C14", "C15"))
      add(resultado(g, "NO_VERIFICABLE", "el pre-vuelo está en rojo: no se generó nada"))
    return(r)
  }

  arch <- list(
    cuadernillos = if ("cuadernillo" %in% sal) file.path(dir_ses, "cuadernillos", paste0("cuadernillo_", ids, ".pdf")),
    docente = if ("cuadernillo" %in% sal) file.path(dir_ses, "docente", paste0("cuadernillo_docente_", ids, ".pdf")),
    hojas = if ("hoja" %in% sal) file.path(dir_ses, "hojas", paste0("hoja_respuestas_", ids, ".pdf")),
    retro = if ("retro" %in% sal) file.path(dir_ses, "retro", paste0("retro_", ids, ".pdf")),
    word = if ("word" %in% sal) file.path(dir_ses, "word", paste0("cuadernillo_", ids, ".docx")))
  arch <- Filter(Negate(is.null), arch)                 # solo las salidas pedidas; las demás se omiten y se dice
  faltan <- unlist(lapply(arch, function(a) a[!file.exists(a)]))
  if (length(faltan)) add(resultado("C0", "FAIL", paste0("faltan archivos: ", paste(basename(faltan), collapse = ", "))))
  logs <- function(tipo) file.path(dir_ses, "tex", tipo, ids, "tmp_1.log")
  med <- list()
  DOS_COL <- c("cuadernillos", "docente")            # los dos cuadernillos a doble columna: estudiante y docente
  CON_TEX <- c("cuadernillos", "docente", "retro")    # lo que compila el motor con sus plantillas
  for (k in c("cuadernillos", "docente", "hojas", "retro")) if (length(arch[[k]])) {
    lg <- if (k == "hojas") NULL else logs(k)
    med[[k]] <- lapply(seq_along(arch[[k]]), function(i)
      if (file.exists(arch[[k]][i])) medir(arch[[k]][i], if (k %in% DOS_COL) 2 else 1,
                                          if (!is.null(lg) && file.exists(lg[i])) lg[i] else NULL) else NULL)
  }

  ## C1 papel
  det <- character(0)
  papel <- function(k, w, h) for (i in seq_along(med[[k]])) { m <- med[[k]][[i]]; if (is.null(m)) next
    pp <- unlist(m$papeles_mm)
    if (abs(m$ancho_mm - w) > 1 || abs(m$alto_mm - h) > 1 || length(pp) != 1)
      det <<- c(det, sprintf("%s: %s mm (esperado %g x %g)", basename(arch[[k]][i]), paste(pp, collapse = " / "), w, h)) }
  papel("cuadernillos", 216, 330); papel("docente", 216, 330); papel("hojas", 216, 330); papel("retro", 90, 160)
  for (f in arch$word) if (file.exists(f)) {
    doc <- docx_xml(f, "word/document.xml")
    w <- as.numeric(sub('.*<w:pgSz[^>]*w:w="(\\d+)".*', "\\1", doc)); h <- as.numeric(sub('.*<w:pgSz[^>]*w:h="(\\d+)".*', "\\1", doc))
    if (is.na(w) || is.na(h) || abs(w - 12246) > 5 || abs(h - 18709) > 5) det <- c(det, sprintf("%s: pgSz %s x %s twips", basename(f), w, h))
    if (!grepl('<w:cols[^>]*w:num="2"', doc)) det <- c(det, paste0(basename(f), ": sin w:cols w:num=\"2\""))
    if (grepl("##", doc, fixed = TRUE)) det <- c(det, paste0(basename(f), ": quedó un marcador «##» de la plantilla"))
    ## ninguna imagen más ancha que el espacio de su columna (columna menos la sangría de su párrafo); tolerancia de 1 pt
    im <- tryCatch(docx_imagenes(f), error = function(e) e)
    if (inherits(im, "error")) det <- c(det, paste0(basename(f), ": no se pudo medir el ancho de las imágenes (", conditionMessage(im), ")"))
    else if (any(im$cx > im$disponible + 12700)) { k <- which.max(im$cx - im$disponible)
      det <- c(det, sprintf("%s: %d imagen(es) más anchas que su columna (la peor: %.2f cm en %.2f cm)", basename(f),
                            sum(im$cx > im$disponible + 12700), im$cx[k] / 360000, im$disponible[k] / 360000)) }
    ## ni ninguna tabla: la suma de su rejilla cabe en la columna menos su sangría (tolerancia: 20 twips = 1 pt)
    tb <- tryCatch(docx_tablas(f), error = function(e) e)
    if (inherits(tb, "error")) det <- c(det, paste0(basename(f), ": no se pudo medir el ancho de las tablas (", conditionMessage(tb), ")"))
    else if (any(tb$ancho > tb$disponible + 20)) { k <- which.max(tb$ancho - tb$disponible)
      det <- c(det, sprintf("%s: %d tabla(s) más anchas que su columna (la peor: %.2f cm en %.2f cm)", basename(f),
                            sum(tb$ancho > tb$disponible + 20), tb$ancho[k] / 567, tb$disponible[k] / 567)) }
  }
  omitidas <- setdiff(c("cuadernillo", "hoja", "retro", "word"), sal)
  nota <- if (length(omitidas)) paste0(" (no pedidas, se omiten: ", paste(omitidas, collapse = ", "), ")") else ""
  add(resultado("C1", if (length(det)) "FAIL" else "PASS", if (length(det)) det else
    paste0("papel correcto en: ", paste(names(arch), collapse = ", "), nota)))

  ## C2 fuentes incrustadas
  det <- character(0)
  for (k in names(med)) for (i in seq_along(med[[k]])) { m <- med[[k]][[i]]
    if (!is.null(m) && length(m$fuentes_no_incrustadas)) det <- c(det, sprintf("%s: %s", basename(arch[[k]][i]),
                                                                             paste(unlist(m$fuentes_no_incrustadas), collapse = ","))) }
  add(resultado("C2", if (length(det)) "FAIL" else "PASS", if (length(det)) det else "100 % de las fuentes incrustadas"))

  ## C3 Overfull > 1 pt (cuadernillos, docente y retro). Sin .log: se recompila el .tex en un directorio temporal.
  det <- character(0); nolog <- 0L
  for (k in intersect(CON_TEX, names(med))) {
    lg <- logs(k)
    for (i in seq_along(lg)) {
      f <- lg[i]
      if (!file.exists(f)) {
        tex <- sub("\\.log$", ".tex", f)
        if (!file.exists(tex)) { nolog <- nolog + 1L; next }
        td <- tempfile(); dir.create(td); file.copy(list.files(dirname(tex), full.names = TRUE), td)
        for (p in 1:2) system2("pdflatex", c("-interaction=nonstopmode", "-output-directory", shQuote(td),
                                             shQuote(file.path(td, basename(tex)))), stdout = FALSE, stderr = FALSE)
        f <- file.path(td, "tmp_1.log")
        if (!file.exists(f)) { nolog <- nolog + 1L; next }
      }
      x <- leer_log(f)
      if (is.na(x)) { nolog <- nolog + 1L; next }           # un log ilegible no aprueba
      over <- overfull_pt(x)
      if (any(over > 1)) det <- c(det, sprintf("%s/%s: %d Overfull, máximo %.1f pt", k, ids[i], sum(over > 1), max(over)))
    }
  }
  ## ... y nada se sale por abajo: la tinta de cada página de los dos cuadernillos termina a 10 mm o más del borde
  sinmedir <- 0L
  for (k in intersect(DOS_COL, names(arch))) for (i in seq_along(arch[[k]])) {
    f <- arch[[k]][i]; if (!file.exists(f)) next
    mi <- margen_inferior_mm(f)
    if (!length(mi) || all(is.na(mi))) { sinmedir <- sinmedir + 1L; next }
    mal <- which(!is.na(mi) & mi < MARGEN_INFERIOR_MIN_MM)
    if (length(mal)) det <- c(det, sprintf("%s: la tinta llega a %.1f mm del borde inferior en la página %s (mínimo %g mm): algo es más alto que su columna",
                                           basename(f), min(mi[mal]), paste(mal, collapse = ","), MARGEN_INFERIOR_MIN_MM))
  }
  add(resultado("C3", if (length(det)) "FAIL" else if (nolog || sinmedir) "NO_VERIFICABLE" else "PASS",
                if (length(det)) det else if (nolog) paste0(nolog, " compilaciones sin .log legible ni .tex") else
                if (sinmedir) paste0(sinmedir, " PDF sin poder medir su margen inferior") else
                  "0 Overfull > 1 pt; la tinta de cada página, a 10 mm o más del borde inferior"))

  ## C4 texto vectorial >= 7 pt (cuadernillos y retro). Racha de 1-2 caracteres entre 5,5 y 7 pt: WARN.
  ## El texto de una fuente Type 3 NO se puede medir (poppler estima su tamaño, ver medir_pdf.py): no aprueba ni reprueba,
  ## y deja C4 en NO_VERIFICABLE. El detalle se resume por archivo y por tamaño: una lista cortada en 12 escondió los
  ## rótulos de 6 pt de dos gráficas detrás de 12 signos de una fuente Type 3 (2026-11-14).
  fail <- character(0); warn <- character(0); t3 <- character(0)
  for (k in intersect(CON_TEX, names(med))) for (i in seq_along(med[[k]])) {
    m <- med[[k]][[i]]; if (is.null(m)) next
    b <- basename(arch[[k]][i])
    if (length(m$fuentes_type3))
      t3 <- c(t3, sprintf("%s: %s caracteres en fuentes Type 3 (%s)", b, m$texto_type3, paste(unlist(m$fuentes_type3), collapse = ", ")))
    ra <- Filter(function(x) !isTRUE(x$type3), m$rachas_bajo_min)
    if (!length(ra)) next
    pt <- vapply(ra, function(x) as.numeric(x$pt), 0); n <- vapply(ra, function(x) as.numeric(x$n), 0)
    txt <- vapply(ra, function(x) as.character(x$texto), "")
    leve <- pt >= 5.5 & n <= 2
    resumen <- function(sel) vapply(sort(unique(pt[sel])), function(v) {
      q <- sel & pt == v
      sprintf("%s: %d racha(s) de %g pt (p. ej. «%s»)", b, sum(q), v, txt[q][which.max(n[q])]) }, "")
    if (any(!leve)) fail <- c(fail, resumen(!leve))
    if (any(leve)) warn <- c(warn, resumen(leve))
  }
  if (length(t3)) t3 <- c(paste0("tamaño NO MEDIBLE en ", length(t3), " archivo(s): use fuentes escalables (en matplotlib, ",
                                 "plt.rcParams['pdf.fonttype'] = 42)"), t3)
  det <- c(fail, t3, if (!length(fail)) warn)
  if (length(det) > 20) det <- c(det[1:20], sprintf("… y %d líneas más", length(det) - 20))
  add(resultado("C4", if (length(fail)) "FAIL" else if (length(t3)) "NO_VERIFICABLE" else if (length(warn)) "WARN" else "PASS",
                if (length(det)) det else "texto >= 7 pt; sin fuentes Type 3"))

  ## C5 raster >= 300 ppi efectivos (cuadernillos, retro y Word)
  ppis <- list()
  for (k in intersect(CON_TEX, names(med))) for (i in seq_along(med[[k]])) {
    m <- med[[k]][[i]]; if (!is.null(m) && length(m$raster_ppi)) ppis[[basename(arch[[k]][i])]] <- unlist(m$raster_ppi) }
  for (f in arch$word) if (file.exists(f)) { p <- ppi_docx(f); if (length(p)) ppis[[basename(f)]] <- p }
  est <- vapply(ppis, clasificar_ppi, "")
  add(resultado("C5", peor(est), if (!length(ppis)) "sin imágenes raster" else
    paste(sprintf("%s: mín %.0f ppi", names(ppis), vapply(ppis, min, 0))[est != "PASS" | seq_along(est) <= 3], collapse = "; ")))

  ## C6 reproducibilidad entre NOPS, cuadernillo, retro y Word
  hs <- firmas$huella; cs <- firmas$clave
  det <- character(0)
  if (length(hs) < 2) {
    add(resultado("C6", "PASS", paste0("una sola salida con texto (", paste(names(hs), collapse = ","), "): nada que comparar")))
  } else {
    ref <- names(hs)[1]
    for (o in names(hs)[-1]) {
      d1 <- which(hs[[o]] != hs[[ref]], arr.ind = TRUE)
      d2 <- which(cs[[o]] != cs[[ref]], arr.ind = TRUE)
      if (nrow(d1)) det <- c(det, sprintf("texto %s vs %s: %d celdas (p. ej. versión %s, pregunta %d)", o, ref, nrow(d1),
                                          ids[d1[1, 1]], d1[1, 2]))
      if (nrow(d2)) det <- c(det, sprintf("clave %s vs %s: %d celdas (p. ej. versión %s, pregunta %d)", o, ref, nrow(d2),
                                          ids[d2[1, 1]], d2[1, 2]))
    }
    add(resultado("C6", if (length(det)) "FAIL" else "PASS", if (length(det)) det else
      sprintf("%s idénticas en texto normalizado y clave (%d x %d)", paste(names(hs), collapse = " = "), N, Q)))
  }

  ## C7 ID impreso = ID de la hoja NOPS
  det <- character(0)
  if (!is.null(firmas$ids_nops) && !identical(firmas$ids_nops, ids)) det <- c(det, "los IDs de firmas no son los de exams2nops")
  for (k in intersect(CON_TEX, names(arch))) for (i in seq_along(arch[[k]])) {
    f <- arch[[k]][i]; if (!file.exists(f)) next
    if (!grepl(ids[i], texto_pdf(f), fixed = TRUE)) det <- c(det, sprintf("%s no imprime %s", basename(f), ids[i]))
  }
  add(resultado("C7", if (length(det)) "FAIL" else if (is.null(firmas$ids_nops)) "WARN" else "PASS",
                if (length(det)) det else if (is.null(firmas$ids_nops)) "sin hoja NOPS: IDs calculados con Date2ID, no comparados"
                else sprintf("%d IDs impresos = names(exams2nops)", N)))

  ## C8 caras: cuadernillo par; hoja exactamente 2, la segunda en blanco
  det <- character(0)
  for (f in c(arch$cuadernillos, arch$docente)) if (file.exists(f)) { p <- paginas_pdf(f); if (p %% 2 != 0) det <- c(det, sprintf("%s: %d caras", basename(f), p)) }
  for (f in arch$hojas) if (file.exists(f)) {
    p <- paginas_pdf(f)
    if (p != 2) det <- c(det, sprintf("%s: %d caras (deben ser 2)", basename(f), p))
    else if (!cara_sin_tinta(f, 2)) det <- c(det, sprintf("%s: la cara 2 tiene tinta", basename(f)))
  }
  for (f in c(file.path(dir_ses, "cuadernillos", "cuadernillos_imprimir.pdf"), file.path(dir_ses, "docente", "docente_imprimir.pdf"),
              file.path(dir_ses, "hojas", "hojas_imprimir.pdf")))
    if (file.exists(f) && paginas_pdf(f) %% 2 != 0) det <- c(det, sprintf("%s: número impar de caras", basename(f)))
  add(resultado("C8", if (length(det)) "FAIL" else "PASS", if (length(det)) det else "cuadernillos con caras pares; hojas de 2 caras, la segunda sin tinta"))

  ## C13 numeración impresa: en cada cuadernillo y en cada retroalimentación, las preguntas llevan los números 1..Q.
  ## Testigo: las anclas qini.N, que la plantilla escribe con el MISMO contador que imprime el número. Dos preguntas con el
  ## mismo número dejan una sola ancla, así que cualquier número repetido o saltado rompe la igualdad con 1..Q.
  ## Origen (2026-11-14): pandoc escribe \setcounter{enumi}{N} cuando una lista del ejercicio no empieza en 1; con las
  ## preguntas numeradas por enumi, la retroalimentación del examen real numeró 1, 2, 3, 2, 4, 5… y ninguna guarda lo vio.
  det <- character(0)
  for (k in intersect(CON_TEX, names(med))) for (i in seq_along(med[[k]])) {
    m <- med[[k]][[i]]; if (is.null(m)) next
    nums <- as.integer(unlist(m$numeros))
    if (!identical(nums, seq_len(Q)))
      det <- c(det, sprintf("%s: números impresos %s (esperado 1..%d)", basename(arch[[k]][i]),
                            if (length(nums)) paste(nums, collapse = ",") else "sin anclas qini.N", Q))
  }
  hay13 <- length(intersect(CON_TEX, names(med))) > 0
  add(resultado("C13", if (length(det)) "FAIL" else "PASS", if (length(det)) det else if (!hay13)
    "sin cuadernillo ni retroalimentación: nada que numerar" else
    sprintf("preguntas numeradas 1..%d en: %s", Q, paste(intersect(CON_TEX, names(med)), collapse = ", "))))

  ## C14 preguntas enteras: solo cuando la configuración lo pide (partir_preguntas = FALSE). Entonces ninguna pregunta de
  ## ningún cuadernillo empieza en una columna y termina en otra (anclas qini.N / qfin.N). Por defecto se parten: es la LEY
  ## del cuadernillo (caben más preguntas), y entonces la guarda no aplica y solo informa cuántas quedaron partidas.
  partir <- firmas$partir_preguntas %||% TRUE
  det <- character(0); npart <- integer(0)
  for (k in intersect(DOS_COL, names(med))) for (i in seq_along(med[[k]])) {
    m <- med[[k]][[i]]; if (is.null(m)) next
    pt <- as.integer(unlist(m$partidas)); sf <- as.integer(unlist(m$sin_fin))
    if (k == "cuadernillos") npart <- c(npart, length(pt))
    if (isFALSE(partir) && (length(pt) || length(sf)))
      det <- c(det, sprintf("%s: preguntas partidas %s%s", basename(arch[[k]][i]), paste(pt, collapse = ","),
                            if (length(sf)) paste0("; sin ancla de fin: ", paste(sf, collapse = ",")) else ""))
  }
  add(resultado("C14", if (length(det)) "FAIL" else "PASS", if (length(det)) det else if (!length(intersect(DOS_COL, names(med))))
    "sin cuadernillos: nada que medir" else if (isFALSE(partir)) "la configuración pide preguntas enteras: ninguna quedó partida" else
    sprintf("no aplica: la configuración permite partir (se parten entre %d y %d de %d preguntas por cuadernillo)",
            min(npart), max(npart), Q)))

  ## C15 retroalimentación: NINGUNA en el cuadernillo del estudiante; la de cada pregunta en el del docente y en la retro.
  ## Se lee el texto del PDF y se buscan los testigos que dejó el motor (trozos de la Solution de cada pregunta que no
  ## están en ningún enunciado). Sin testigos no se puede certificar: NO_VERIFICABLE, nunca PASS.
  tes <- firmas$testigos
  det <- character(0); con <- 0L
  hay15 <- length(intersect(CON_TEX, names(arch))) > 0
  if (hay15 && !is.null(tes)) for (i in seq_len(N)) {
    ti <- tes[[i]]; tiene <- which(lengths(ti) > 0); con <- con + length(tiene)
    esta <- function(f) { tx <- solo_letras(texto_pdf_completo(f))
      vapply(ti, function(w) length(w) > 0 && any(vapply(w, grepl, TRUE, x = tx, fixed = TRUE)), TRUE) }
    if ("cuadernillos" %in% names(arch) && file.exists(arch$cuadernillos[i])) { e <- which(esta(arch$cuadernillos[i]))
      if (length(e)) det <- c(det, sprintf("%s MUESTRA la retroalimentación de la pregunta %s", basename(arch$cuadernillos[i]), paste(e, collapse = ","))) }
    for (k in intersect(c("docente", "retro"), names(arch))) if (file.exists(arch[[k]][i])) { e <- setdiff(tiene, which(esta(arch[[k]][i])))
      if (length(e)) det <- c(det, sprintf("%s NO trae la retroalimentación de la pregunta %s", basename(arch[[k]][i]), paste(e, collapse = ","))) }
  }
  add(resultado("C15", if (length(det)) "FAIL" else if (hay15 && (is.null(tes) || con == 0L)) "NO_VERIFICABLE" else "PASS",
                if (length(det)) det else if (!hay15) "sin cuadernillos ni retroalimentación: nada que medir" else
                if (is.null(tes) || con == 0L) "sin testigos de la retroalimentación: no se puede certificar que el estudiante no la recibe" else
                sprintf("el cuadernillo del estudiante no trae retroalimentación; %s sí (testigos en %d de %d preguntas)",
                        paste(intersect(c("docente", "retro"), names(arch)), collapse = " y "), con, N * Q)))

  ## C10 revisión visual: MANUAL, nunca PASS
  g <- file.path(dir_ses, c("revision_gris_v1.png", "revision_gris_vN.png"))
  g <- g[file.exists(g)]
  add(resultado("C10", "MANUAL", if (length(g)) paste0("REVISIÓN VISUAL PENDIENTE: ", paste(g, collapse = " y "),
    " (criterio: categorías distinguibles en gris a 50 ppp; rótulos legibles en las páginas a 150 ppp; aceptación final del profesor)")
    else "sin cuadernillos: no hay hoja en gris"))
  r
}

informe <- function(r, dir_ses) {
  orden <- c("C0", sprintf("C%d", 1:15))
  r <- r[order(match(vapply(r, `[[`, "", "id"), orden))]
  lineas <- c(sprintf("Verificación de %s (%s)", dir_ses, format(Sys.time(), "%Y-%m-%d %H:%M")),
              sprintf("%-4s %-15s %s", vapply(r, `[[`, "", "id"), vapply(r, `[[`, "", "estado"), vapply(r, `[[`, "", "detalle")))
  ex <- decidir_exit(r)
  lineas <- c(lineas, "", sprintf("EXIT %d. El exit 0 certifica C1-C9 y C11-C15. C10 queda pendiente de revisión visual.", ex))
  cat(lineas, sep = "\n")
  writeLines(lineas, file.path(dir_ses, "verificacion.txt"))
  jsonlite::write_json(r, file.path(dir_ses, "verificacion.json"), auto_unbox = TRUE, pretty = TRUE)
  ex
}

## ---------------------------------------------------------------------------------------------------------------
## Mutantes: cada uno construye una COPIA con un reemplazo EXACTO (o un fixture/config propio) y exige que SU guarda falle
## ---------------------------------------------------------------------------------------------------------------
FIX <- function(...) file.path(dir_motor(), "pruebas", "fixtures", paste0(c(...), ".Rmd"))

MUTANTES <- list(
  list(id = "M1", guarda = "C1", plantilla = "cuadernillo_oficio_2col.tex",
       de = "papersize={216mm,330mm}", a = "papersize={216mm,356mm}", fixtures = "f_texto", salidas = "cuadernillo"),
  list(id = "M2", guarda = "C2", fixtures = "m_plot_pdf", salidas = "cuadernillo"),
  list(id = "M3", guarda = "C3", plantilla = "cuadernillo_oficio_2col.tex",
       de = paste("\\renewcommand{\\includegraphics}[2][]{\\siempiezaopcion",
                  "  \\if@tempswa\\adjustbox{valign=t,max width=\\linewidth,center}{\\includegraphicsoriginal[#1]{#2}}%",
                  "  \\else\\adjustbox{max width=\\linewidth,\\ifvmode center\\fi}{\\includegraphicsoriginal[#1]{#2}}\\fi}", sep = "\n"),
       a = "", fixtures = "m_ancha", salidas = "cuadernillo"),
  ## M3b: C3 también en la retroalimentación y con un log que NO es UTF-8 (el bloque de código lleva «ó», «×» y «÷»)
  list(id = "M3b", guarda = "C3", plantilla = "retro_celular.tex",
       de = paste("\\IfFileExists{fvextra.sty}{%", "  \\usepackage{fvextra}",
                  "  \\renewenvironment{verbatim}{\\VerbatimEnvironment",
                  "    \\begin{Verbatim}[breaklines=true,breaksymbolleft={},breakindent=1em]}{\\end{Verbatim}}}{}", sep = "\n"),
       a = "", fixtures = "f_lista", salidas = "retro"),
  list(id = "M4", guarda = "C4", fixtures = "m_diminuta", salidas = "cuadernillo"),
  ## M4b: texto en una fuente Type 3: su tamaño no se puede medir, y C4 no puede quedar en PASS
  list(id = "M4b", guarda = "C4", fixtures = "m_type3", salidas = "cuadernillo", espera = "NO_VERIFICABLE"),
  list(id = "M5", guarda = "C5", fixtures = "m_png72", salidas = "cuadernillo"),
  list(id = "M6", guarda = "C6", fixtures = "m_semilla_nula", salidas = c("hoja", "cuadernillo")),
  list(id = "M7", guarda = "C7", motor = TRUE, de = "c(list(ID = id_version(cfg$fecha, ses$startid, i),",
       a = "c(list(ID = id_version(cfg$fecha, ses$startid + 1L, i),", fixtures = "f_texto", salidas = c("hoja", "cuadernillo")),
  list(id = "M8", guarda = "C8", plantilla = "cuadernillo_oficio_2col.tex",
       de = "\\ifodd\\value{page}\\else\\thispagestyle{empty}\\null\\clearpage\\fi", a = "", fixtures = "f_texto",
       salidas = "cuadernillo"),
  list(id = "M9", guarda = "C9", fixtures = c("f_texto", "m_bucle"), presupuesto_s = 20, salidas = "clave",
       extra = function(ds) {
         l <- readLines(file.path(ds, "preflight.log"), warn = FALSE)
         v <- grep("^terminado por vigilante: ejercicio 2 ", l, value = TRUE)
         length(v) == 1 && as.numeric(sub(".*tras=([0-9.]+).*", "\\1", v)) <= 30
       }),
  list(id = "M10b", guarda = "C8", motor = TRUE, de = '"--pages", shQuote(crudo), "1-2", "--"',
       a = '"--pages", shQuote(crudo), "1", "--"', fixtures = "f_texto", salidas = "hoja"),
  list(id = "M11", guarda = "C11", estudiantes = 101L, fixtures = "f_texto", salidas = "cuadernillo", espera_stop = "C11"),
  list(id = "M11b", guarda = "C11", fixtures = "m_seis_opciones", salidas = c("hoja", "clave")),
  list(id = "M12", guarda = "C12", fixtures = c("m_motor_python", "m_chunk_python"), salidas = "clave"),
  list(id = "M12b", guarda = "C12", fixtures = c("m_motor_indirecto", "m_chunk_python"), salidas = "clave",
       extra = function(ds) {   # la regex no lo ve; lo ve la instantánea por el código del motor
         l <- readLines(file.path(ds, "preflight.log"), warn = FALSE)
         any(grepl("^ESTATICO 1 igual", l)) && any(grepl("^FIN 1 .*estado=CAMBIA:.*codigo", l))
       }),
  list(id = "M13", guarda = "C1", plantilla = "pandoc_cuadernillo.tex", de = "\\textbf{##Titulo##}",
       a = "\\textbf{##Titulo##} ##NoExiste##", fixtures = "f_texto", salidas = "word"),
  list(id = "M14", guarda = "C11", ruta = TRUE),
  ## M15 y M15b: la plantilla vuelve a numerar las preguntas con enumi; la lista «3. … 4. …» de f_lista cambia el número
  ## de la pregunta siguiente (el defecto real del 2026-11-14), en el cuadernillo y en la retroalimentación
  ## (en el cuadernillo el ancla qini.N va en la etiqueta: el mutante la conserva, para que muera por el NÚMERO y no por
  ## quedarse sin anclas)
  list(id = "M15", guarda = "C13", plantilla = "cuadernillo_oficio_2col.tex",
       de = paste("\\newlist{preguntas}{enumerate}{1}",
                  paste0("\\setlist[preguntas]{label=\\protect\\hypertarget{qini.\\arabic*}{}\\arabic*.,leftmargin=*,",
                         "itemsep=8pt plus 2pt,topsep=0pt}"), sep = "\n"),
       a = paste0("\\makeatletter\\newenvironment{preguntas}{\\begin{enumerate}[label=\\protect\\hypertarget{qini.\\arabic*}{}",
                  "\\arabic*.,leftmargin=*,itemsep=8pt plus 2pt,topsep=0pt]}{\\end{enumerate}}",
                  "\\let\\c@preguntasi\\c@enumi\\makeatother"),
       fixtures = c("f_lista", "f_texto"), salidas = "cuadernillo"),
  list(id = "M15b", guarda = "C13", plantilla = "retro_celular.tex",
       de = paste("\\newlist{preguntas}{enumerate}{1}",
                  "\\setlist[preguntas]{label=\\textbf{\\arabic*.},wide=0pt,leftmargin=0pt,itemsep=10pt plus 2pt,topsep=0pt}", sep = "\n"),
       a = "\\makeatletter\\newenvironment{preguntas}{\\begin{enumerate}}{\\end{enumerate}}\\let\\c@preguntasi\\c@enumi\\makeatother",
       fixtures = c("f_lista", "f_texto"), salidas = "retro"),
  ## M16 y M16b: el motor deja de reducir a su columna las imágenes (f_plot) y las tablas (f_tabla) del Word
  list(id = "M16", guarda = "C1", motor = TRUE, de = "    ajustar_docx_a_columna(destino)\n", a = "", fixtures = "f_plot",
       salidas = "word"),
  list(id = "M16b", guarda = "C1", motor = TRUE, de = "    ajustar_docx_a_columna(destino)\n", a = "", fixtures = "f_tabla",
       salidas = "word"),
  ## M17: la configuración pide preguntas enteras y el motor no se lo dice a la plantilla. La pregunta de m_alta trae una
  ## figura que ocupa una columna entera: partida, empieza en una columna y termina en otra por construcción
  list(id = "M17", guarda = "C14", motor = TRUE, de = 'partir <- list(Partir = if (cfg$partir_preguntas) "1" else "0")',
       a = 'partir <- list(Partir = "1")', fixtures = c("f_texto", "m_alta"), salidas = "cuadernillo", partir = FALSE),
  ## M17b: una pregunta entera más alta que su columna se sale por abajo (la tinta pasa del margen inferior)
  list(id = "M17b", guarda = "C3", fixtures = c("f_texto", "m_alta"), salidas = "cuadernillo", partir = FALSE),
  ## M18: la plantilla deja ver la Solution en el cuadernillo del estudiante
  list(id = "M18", guarda = "C15", plantilla = "cuadernillo_oficio_2col.tex", de = "\\newenvironment{solution}{\\comment}{\\endcomment}",
       a = "\\newenvironment{solution}{\\par}{\\par}", fixtures = "f_texto", salidas = "cuadernillo"),
  ## M18b: el motor genera el cuadernillo del docente sin pedir la retroalimentación
  list(id = "M18b", guarda = "C15", motor = TRUE, de = 'c(partir, list(Docente = "1",', a = 'c(partir, list(Docente = "0",',
       fixtures = "f_texto", salidas = "cuadernillo")
)

escribir_config <- function(ruta, fixtures, N = 2L, presupuesto_s = NULL, salidas, dir_salida = NULL, partir = NULL) {
  l <- c("config <- list(",
         '  titulo = "Prueba & mutante_1", institucion = "I. E. de prueba", fecha = as.Date("2026-11-13"),',
         sprintf("  estudiantes = %dL, semilla = 20261113L,", as.integer(N)),
         if (!is.null(presupuesto_s)) sprintf("  presupuesto_s = %g,", presupuesto_s),
         if (!is.null(dir_salida)) sprintf("  dir_salida = %s,", deparse(dir_salida)),
         if (!is.null(partir)) sprintf("  partir_preguntas = %s,", deparse(partir)),
         sprintf("  salidas = %s,", paste(deparse(salidas), collapse = "")),
         sprintf('  sesiones = list(list(nombre = "Sesión 1", startid = 1L, ejercicios = %s)))',
                 paste(deparse(unname(FIX(fixtures))), collapse = "")))
  writeLines(l, ruta)
}

reemplazo_exacto <- function(archivo, de, a) {
  s <- paste(readLines(archivo, warn = FALSE), collapse = "\n")
  n <- lengths(regmatches(s, gregexpr(de, s, fixed = TRUE)))
  if (n != 1) stop("MUTANTE MAL CONSTRUIDO: el patrón aparece ", n, " veces en ", basename(archivo), ": ", de)
  writeLines(sub(de, a, s, fixed = TRUE), archivo)
}

correr_motor <- function(motor, args, plantillas, wd, timeout = 900) {
  p <- processx::run(file.path(R.home("bin"), "Rscript"), c(motor, args), wd = wd, error_on_status = FALSE,
                     timeout = timeout, stderr_to_stdout = TRUE,
                     env = c("current", CUADERNILLOS_MOTOR_DIR = dir_motor(), CUADERNILLOS_PLANTILLAS = plantillas))
  list(status = p$status, salida = p$stdout, timeout = isTRUE(p$timeout))
}

correr_mutante <- function(m) {
  B <- tempfile("mut_"); dir.create(B)
  pl <- file.path(B, "plantillas"); dir.create(pl)
  ## solo las cuatro del motor (perifericos/ tiene además las plantillas y los semilleros oficiales); nunca se escribe
  ## en las plantillas reales
  origen <- file.path(dir_plantillas(), PLANTILLAS_MOTOR)
  if (!all(file.exists(origen))) stop("faltan plantillas del motor en ", dir_plantillas(), ": ",
                                      paste(PLANTILLAS_MOTOR[!file.exists(origen)], collapse = ", "))
  file.copy(origen, pl)
  motor <- file.path(dir_motor(), "generar_cuadernillos.R")
  if (!is.null(m$plantilla)) reemplazo_exacto(file.path(pl, m$plantilla), m$de, m$a)
  if (isTRUE(m$motor)) { file.copy(motor, file.path(B, "generar_cuadernillos.R")); motor <- file.path(B, "generar_cuadernillos.R")
                         reemplazo_exacto(motor, m$de, m$a) }
  if (isTRUE(m$ruta)) return(correr_m14(B, motor, pl))
  cfg <- file.path(B, "config.R")
  escribir_config(cfg, m$fixtures, N = m$estudiantes %||% 2L, presupuesto_s = m$presupuesto_s, salidas = m$salidas, partir = m$partir)
  ds <- file.path(B, "salida", m$id)
  res <- correr_motor(motor, c(cfg, "--dir-salida", ds), pl, B)
  if (!is.null(m$espera_stop))
    return(list(muerto = res$status != 0 && grepl(m$espera_stop, res$salida, fixed = TRUE),
                obtenido = sprintf("exit %s, «%s» %s", res$status, m$espera_stop,
                                   if (grepl(m$espera_stop, res$salida, fixed = TRUE)) "en el mensaje" else "ausente")))
  vj <- file.path(ds, "sesion-1", "verificacion.json")
  if (!file.exists(vj)) return(list(muerto = FALSE, obtenido = paste0("sin verificacion.json (exit ", res$status, "): ",
                                                                      paste(utils::tail(strsplit(res$salida, "\n")[[1]], 3), collapse = " | "))))
  v <- jsonlite::fromJSON(vj, simplifyVector = FALSE)
  e <- vapply(Filter(function(x) x$id == m$guarda, v), `[[`, "", "estado")
  ok_extra <- if (is.null(m$extra)) TRUE else isTRUE(m$extra(file.path(ds, "sesion-1")))
  list(muerto = any(e == (m$espera %||% "FAIL")) && ok_extra,
       obtenido = paste0(m$guarda, "=", paste(e, collapse = ","), if (!is.null(m$extra)) paste0(", condición extra=", ok_extra)))
}

## M14: tres entradas que la guarda de ruta debe rechazar (exit 2, ERR_DIR_SALIDA) sin borrar nada. Corre en un tempdir.
correr_m14 <- function(B, motor, pl) {
  casos <- list()
  ## (a) --dir-salida .. --limpiar desde B/a/b: «..» es B/a, que tiene un testigo
  dir.create(file.path(B, "a", "b"), recursive = TRUE); writeLines("testigo", ta <- file.path(B, "a", "testigo.txt"))
  cfg <- file.path(B, "a", "b", "config.R"); escribir_config(cfg, "f_texto", salidas = "clave")
  casos$a <- c(correr_motor(motor, c(cfg, "--dir-salida", "..", "--limpiar"), pl, file.path(B, "a", "b")), testigo = ta)
  ## (b) config con dir_salida = "." (el directorio del config), con --limpiar
  dir.create(file.path(B, "cb")); writeLines("testigo", tb <- file.path(B, "cb", "testigo.txt"))
  cfg <- file.path(B, "cb", "config.R"); escribir_config(cfg, "f_texto", salidas = "clave", dir_salida = ".")
  casos$b <- c(correr_motor(motor, c(cfg, "--limpiar"), pl, B), testigo = tb)
  ## (c) un directorio ajeno con «salida» en la ruta, no vacío y SIN el marcador, con --limpiar
  dir.create(file.path(B, "salida", "ajena"), recursive = TRUE); writeLines("testigo", tc <- file.path(B, "salida", "ajena", "testigo.txt"))
  cfg <- file.path(B, "config_c.R"); escribir_config(cfg, "f_texto", salidas = "clave")
  casos$c <- c(correr_motor(motor, c(cfg, "--dir-salida", file.path(B, "salida", "ajena"), "--limpiar"), pl, B), testigo = tc)
  ok <- vapply(casos, function(x) x$status == 2 && grepl("ERR_DIR_SALIDA", x$salida, fixed = TRUE) && file.exists(x$testigo), TRUE)
  list(muerto = all(ok), obtenido = paste0(names(ok), "=", ifelse(ok, "rechazado", "NO rechazado"), collapse = ", "))
}

correr_mutantes <- function(cuales = NULL) {
  ms <- if (is.null(cuales)) MUTANTES else Filter(function(m) m$id %in% cuales, MUTANTES)
  filas <- lapply(ms, function(m) {
    t0 <- Sys.time()
    r <- tryCatch(correr_mutante(m), error = function(e) list(muerto = FALSE, obtenido = paste("ERROR:", conditionMessage(e))))
    s <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
    cat(sprintf("%-5s %-4s esperado=%-14s %-9s %s (%d s)\n", m$id, m$guarda, m$espera %||% "FAIL",
                if (r$muerto) "MUERE" else "SOBREVIVE", r$obtenido, s))
    data.frame(id = m$id, guarda = m$guarda, muerto = r$muerto, obtenido = r$obtenido, segundos = s)
  })
  d <- do.call(rbind, filas)
  cat(sprintf("\n%d de %d mutantes mueren.\n", sum(d$muerto), nrow(d)))
  if (all(d$muerto)) 0L else 1L
}

## ---------------------------------------------------------------------------------------------------------------
if (sys.nframe() == 0L) {
  a <- commandArgs(TRUE)
  st <- if (length(a) && a[1] == "--mutantes") {
    correr_mutantes(if (length(a) > 1) strsplit(a[2], ",", fixed = TRUE)[[1]] else NULL)
  } else if (length(a) == 1) {
    informe(verificar_sesion(a[1]), normalizePath(a[1]))
  } else {
    cat("Uso: Rscript verificar_cuadernillos.R <dir_sesion> | --mutantes [M1,M2,…]\n"); 2L
  }
  quit(save = "no", status = st)
}
