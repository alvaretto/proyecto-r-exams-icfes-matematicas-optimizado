## verificar_cuadernillos.R: guardas C1-C12 del motor de cuadernillos, y sus mutantes.
##
## Uso:
##   Rscript verificar_cuadernillos.R <dir_sesion>     verifica una sesión generada por generar_cuadernillos.R
##   Rscript verificar_cuadernillos.R --mutantes [M1,M7,…]   construye cada mutante en un tempdir y exige que SU guarda falle
##
## Exit (decidir_exit): 1 si hay algún FAIL; 2 si hay NO_VERIFICABLE; 0 si no. El exit 0 certifica C1-C9, C11 y C12.
## C10 (revisión visual en gris) es MANUAL: el verificador nunca lo cuenta como PASS; se cierra en el reporte de aprobación.
## Plan y criterios: PRPs/motor-cuadernillos-oficio-2col.md (raíz del repo).

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
    for (g in c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"))
      add(resultado(g, "NO_VERIFICABLE", "el pre-vuelo está en rojo: no se generó nada"))
    return(r)
  }

  arch <- list(
    cuadernillos = if ("cuadernillo" %in% sal) file.path(dir_ses, "cuadernillos", paste0("cuadernillo_", ids, ".pdf")),
    hojas = if ("hoja" %in% sal) file.path(dir_ses, "hojas", paste0("hoja_respuestas_", ids, ".pdf")),
    retro = if ("retro" %in% sal) file.path(dir_ses, "retro", paste0("retro_", ids, ".pdf")),
    word = if ("word" %in% sal) file.path(dir_ses, "word", paste0("cuadernillo_", ids, ".docx")))
  arch <- Filter(Negate(is.null), arch)                 # solo las salidas pedidas; las demás se omiten y se dice
  faltan <- unlist(lapply(arch, function(a) a[!file.exists(a)]))
  if (length(faltan)) add(resultado("C0", "FAIL", paste0("faltan archivos: ", paste(basename(faltan), collapse = ", "))))
  logs <- function(tipo) file.path(dir_ses, "tex", tipo, ids, "tmp_1.log")
  med <- list()
  for (k in c("cuadernillos", "hojas", "retro")) if (length(arch[[k]])) {
    lg <- if (k == "hojas") NULL else logs(k)
    med[[k]] <- lapply(seq_along(arch[[k]]), function(i)
      if (file.exists(arch[[k]][i])) medir(arch[[k]][i], if (k == "cuadernillos") 2 else 1,
                                          if (!is.null(lg) && file.exists(lg[i])) lg[i] else NULL) else NULL)
  }

  ## C1 papel
  det <- character(0)
  papel <- function(k, w, h) for (i in seq_along(med[[k]])) { m <- med[[k]][[i]]; if (is.null(m)) next
    pp <- unlist(m$papeles_mm)
    if (abs(m$ancho_mm - w) > 1 || abs(m$alto_mm - h) > 1 || length(pp) != 1)
      det <<- c(det, sprintf("%s: %s mm (esperado %g x %g)", basename(arch[[k]][i]), paste(pp, collapse = " / "), w, h)) }
  papel("cuadernillos", 216, 330); papel("hojas", 216, 330); papel("retro", 90, 160)
  for (f in arch$word) if (file.exists(f)) {
    doc <- docx_xml(f, "word/document.xml")
    w <- as.numeric(sub('.*<w:pgSz[^>]*w:w="(\\d+)".*', "\\1", doc)); h <- as.numeric(sub('.*<w:pgSz[^>]*w:h="(\\d+)".*', "\\1", doc))
    if (is.na(w) || is.na(h) || abs(w - 12246) > 5 || abs(h - 18709) > 5) det <- c(det, sprintf("%s: pgSz %s x %s twips", basename(f), w, h))
    if (!grepl('<w:cols[^>]*w:num="2"', doc)) det <- c(det, paste0(basename(f), ": sin w:cols w:num=\"2\""))
    if (grepl("##", doc, fixed = TRUE)) det <- c(det, paste0(basename(f), ": quedó un marcador «##» de la plantilla"))
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

  ## C3 Overfull > 1 pt (cuadernillos y retro). Sin .log: se recompila el .tex en un directorio temporal.
  det <- character(0); nolog <- 0L
  for (k in intersect(c("cuadernillos", "retro"), names(med))) {
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
      x <- paste(readLines(f, warn = FALSE), collapse = "\n")
      over <- as.numeric(regmatches(x, gregexpr("(?<=Overfull \\\\hbox \\()[0-9.]+(?=pt too wide)", x, perl = TRUE))[[1]])
      if (any(over > 1)) det <- c(det, sprintf("%s/%s: %d Overfull, máximo %.1f pt", k, ids[i], sum(over > 1), max(over)))
    }
  }
  add(resultado("C3", if (length(det)) "FAIL" else if (nolog) "NO_VERIFICABLE" else "PASS",
                if (length(det)) det else if (nolog) paste0(nolog, " compilaciones sin .log ni .tex") else "0 Overfull > 1 pt"))

  ## C4 texto vectorial >= 7 pt (cuadernillos y retro). Racha de 1-2 caracteres entre 5,5 y 7 pt: WARN.
  fail <- character(0); warn <- character(0)
  for (k in intersect(c("cuadernillos", "retro"), names(med))) for (i in seq_along(med[[k]])) {
    m <- med[[k]][[i]]; if (is.null(m)) next
    for (ra in m$rachas_bajo_min) {
      txt <- sprintf("%s: %gpt «%s»", basename(arch[[k]][i]), ra$pt, ra$texto)
      if (ra$pt >= 5.5 && ra$n <= 2) warn <- c(warn, txt) else fail <- c(fail, txt)
    }
  }
  add(resultado("C4", if (length(fail)) "FAIL" else if (length(warn)) "WARN" else "PASS",
                if (length(fail)) utils::head(fail, 12) else if (length(warn)) utils::head(warn, 12) else "texto >= 7 pt"))

  ## C5 raster >= 300 ppi efectivos (cuadernillos, retro y Word)
  ppis <- list()
  for (k in intersect(c("cuadernillos", "retro"), names(med))) for (i in seq_along(med[[k]])) {
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
  for (k in intersect(c("cuadernillos", "retro"), names(arch))) for (i in seq_along(arch[[k]])) {
    f <- arch[[k]][i]; if (!file.exists(f)) next
    if (!grepl(ids[i], texto_pdf(f), fixed = TRUE)) det <- c(det, sprintf("%s no imprime %s", basename(f), ids[i]))
  }
  add(resultado("C7", if (length(det)) "FAIL" else if (is.null(firmas$ids_nops)) "WARN" else "PASS",
                if (length(det)) det else if (is.null(firmas$ids_nops)) "sin hoja NOPS: IDs calculados con Date2ID, no comparados"
                else sprintf("%d IDs impresos = names(exams2nops)", N)))

  ## C8 caras: cuadernillo par; hoja exactamente 2, la segunda en blanco
  det <- character(0)
  for (f in arch$cuadernillos) if (file.exists(f)) { p <- paginas_pdf(f); if (p %% 2 != 0) det <- c(det, sprintf("%s: %d caras", basename(f), p)) }
  for (f in arch$hojas) if (file.exists(f)) {
    p <- paginas_pdf(f)
    if (p != 2) det <- c(det, sprintf("%s: %d caras (deben ser 2)", basename(f), p))
    else if (!cara_sin_tinta(f, 2)) det <- c(det, sprintf("%s: la cara 2 tiene tinta", basename(f)))
  }
  for (f in c(file.path(dir_ses, "cuadernillos", "cuadernillos_imprimir.pdf"), file.path(dir_ses, "hojas", "hojas_imprimir.pdf")))
    if (file.exists(f) && paginas_pdf(f) %% 2 != 0) det <- c(det, sprintf("%s: número impar de caras", basename(f)))
  add(resultado("C8", if (length(det)) "FAIL" else "PASS", if (length(det)) det else "cuadernillos con caras pares; hojas de 2 caras, la segunda sin tinta"))

  ## C10 revisión visual: MANUAL, nunca PASS
  g <- file.path(dir_ses, c("revision_gris_v1.png", "revision_gris_vN.png"))
  g <- g[file.exists(g)]
  add(resultado("C10", "MANUAL", if (length(g)) paste0("REVISIÓN VISUAL PENDIENTE: ", paste(g, collapse = " y "),
    " (criterio: categorías distinguibles en gris a 50 ppp; rótulos legibles en las páginas a 150 ppp; aceptación final del profesor)")
    else "sin cuadernillos: no hay hoja en gris"))
  r
}

informe <- function(r, dir_ses) {
  orden <- c("C0", sprintf("C%d", 1:12))
  r <- r[order(match(vapply(r, `[[`, "", "id"), orden))]
  lineas <- c(sprintf("Verificación de %s (%s)", dir_ses, format(Sys.time(), "%Y-%m-%d %H:%M")),
              sprintf("%-4s %-15s %s", vapply(r, `[[`, "", "id"), vapply(r, `[[`, "", "estado"), vapply(r, `[[`, "", "detalle")))
  ex <- decidir_exit(r)
  lineas <- c(lineas, "", sprintf("EXIT %d. El exit 0 certifica C1-C9, C11 y C12. C10 queda pendiente de revisión visual.", ex))
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
       de = "\\renewcommand{\\includegraphics}[2][]{\\adjustbox{max width=\\linewidth,\\ifvmode center\\fi}{\\includegraphicsoriginal[#1]{#2}}}",
       a = "", fixtures = "m_ancha", salidas = "cuadernillo"),
  list(id = "M4", guarda = "C4", fixtures = "m_diminuta", salidas = "cuadernillo"),
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
  list(id = "M14", guarda = "C11", ruta = TRUE)
)

escribir_config <- function(ruta, fixtures, N = 2L, presupuesto_s = NULL, salidas, dir_salida = NULL) {
  l <- c("config <- list(",
         '  titulo = "Prueba & mutante_1", institucion = "I. E. de prueba", fecha = as.Date("2026-11-13"),',
         sprintf("  estudiantes = %dL, semilla = 20261113L,", as.integer(N)),
         if (!is.null(presupuesto_s)) sprintf("  presupuesto_s = %g,", presupuesto_s),
         if (!is.null(dir_salida)) sprintf("  dir_salida = %s,", deparse(dir_salida)),
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
  file.copy(list.files(dir_plantillas(), full.names = TRUE), pl)          # nunca se escribe en las plantillas reales
  motor <- file.path(dir_motor(), "generar_cuadernillos.R")
  if (!is.null(m$plantilla)) reemplazo_exacto(file.path(pl, m$plantilla), m$de, m$a)
  if (isTRUE(m$motor)) { file.copy(motor, file.path(B, "generar_cuadernillos.R")); motor <- file.path(B, "generar_cuadernillos.R")
                         reemplazo_exacto(motor, m$de, m$a) }
  if (isTRUE(m$ruta)) return(correr_m14(B, motor, pl))
  cfg <- file.path(B, "config.R")
  escribir_config(cfg, m$fixtures, N = m$estudiantes %||% 2L, presupuesto_s = m$presupuesto_s, salidas = m$salidas)
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
  list(muerto = any(e == "FAIL") && ok_extra,
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
    cat(sprintf("%-5s %-4s esperado=FAIL  %-9s %s (%d s)\n", m$id, m$guarda, if (r$muerto) "MUERE" else "SOBREVIVE", r$obtenido, s))
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
