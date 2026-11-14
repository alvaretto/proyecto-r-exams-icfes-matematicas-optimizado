# =============================================================================
# Test de regresión — Motor de cuadernillos en oficio a doble columna (regla #26)
# =============================================================================
# Prueba Auxiliares/Examenes_Fin_de_Periodo/00-Cuadernillos-Oficio-2Col/:
#   generar_cuadernillos.R (motor), verificar_cuadernillos.R (guardas C1-C15 y sus
#   mutantes) y prototipo/medir_pdf.py (medidor).
# Plan: PRPs/motor-cuadernillos-oficio-2col.md. Mediciones: §10 de INVESTIGACION.md.
#
# Tres capas:
#   1. funciones puras (ID, clave, escape de LaTeX, huella, exit, lectura del log,
#      geometría del Word);
#   2. el medidor sobre PDF construidos aquí (anclas, numeración, fuentes Type 3);
#   3. el motor de punta a punta: camino feliz con 5 fixtures y las 5 salidas, la
#      guarda de ruta y la tanda completa de mutantes.
# La capa 3 tarda unos 3 minutos: la suite lleva `watch` en tests/run_all_tests.R.

library(testthat)

repo_root <- tryCatch({
  r <- system("git rev-parse --show-toplevel", intern = TRUE)
  if (length(r) == 1 && dir.exists(r)) r else getwd()
}, error = function(e) getwd())

DIR <- file.path(repo_root, "Auxiliares", "Examenes_Fin_de_Periodo", "00-Cuadernillos-Oficio-2Col")
FIXT <- file.path(DIR, "pruebas", "fixtures")
MEDIDOR <- file.path(DIR, "prototipo", "medir_pdf.py")
RSCRIPT <- file.path(R.home("bin"), "Rscript")

# El verificador carga el motor (en globalenv) al hacer source(); ninguno de los dos corre nada al cargarse.
Sys.setenv(CUADERNILLOS_MOTOR_DIR = DIR)
source(file.path(DIR, "verificar_cuadernillos.R"), local = TRUE)

herramientas <- c("pdflatex", "qpdf", "pdfjam", "pdfunite", "pdfinfo", "pdftotext", "pdftoppm", "pdftohtml",
                  "pdffonts", "pdfimages", "python3", "unzip", "zip", "pandoc", "montage", "identify")
faltan <- herramientas[!nzchar(Sys.which(herramientas))]
paquetes <- c("exams", "processx", "digest", "jsonlite")
faltan <- c(faltan, paquetes[!vapply(paquetes, requireNamespace, TRUE, quietly = TRUE)])
listo <- length(faltan) == 0
motivo <- paste("faltan:", paste(faltan, collapse = ", "))

medir_json <- function(pdf, ...) {
  out <- suppressWarnings(system2("python3", c("-I", shQuote(MEDIDOR), shQuote(pdf), ...), stdout = TRUE, stderr = TRUE))
  jsonlite::fromJSON(paste(out, collapse = "\n"), simplifyVector = FALSE)
}

compilar <- function(lineas, dir = tempfile("tex")) {
  dir.create(dir, showWarnings = FALSE)
  writeLines(lineas, file.path(dir, "d.tex"))
  st <- 0L
  for (k in 1:2) st <- system2("pdflatex", c("-interaction=nonstopmode", "-halt-on-error", "-output-directory", shQuote(dir),
                                             shQuote(file.path(dir, "d.tex"))), stdout = FALSE, stderr = FALSE)
  list(status = st, pdf = file.path(dir, "d.pdf"))
}

# -----------------------------------------------------------------------------
# 1. Funciones puras
# -----------------------------------------------------------------------------
test_that("id_version() sigue la fórmula Date2ID: fecha aammdd + cinco cifras desde startid", {
  expect_equal(id_version(as.Date("2026-11-20"), 1L, 1L), "26112000001")
  expect_equal(id_version("2026-11-20", 101L, 2L), "26112000102")
  expect_equal(id_version("2026-01-05", 1L, 100L), "26010500100")
})

test_that("id_version() es el nombre que da exams2nops() a cada versión", {
  skip_if_not(listo, motivo)
  d <- tempfile("nops"); dir.create(d)
  xn <- suppressWarnings(suppressMessages(exams::exams2nops(
    file.path(FIXT, "f_texto.Rmd"), n = 2, startid = 7L, date = as.Date("2026-11-20"), language = "es", duplex = TRUE,
    twocolumn = FALSE, dir = d, name = "nops_")))
  expect_identical(names(xn), vapply(1:2, function(i) id_version("2026-11-20", 7L, i), ""))
})

test_that("nombre_copia() rellena con ceros como R/exams: con 10 o más copias la primera es _01", {
  expect_identical(nombre_copia("nops_", 1L, 3L, "pdf"), "nops_1.pdf")
  expect_identical(nombre_copia("nops_", 9L, 9L, "pdf"), "nops_9.pdf")
  expect_identical(nombre_copia("nops_", 1L, 10L, "pdf"), "nops_01.pdf")
  expect_identical(nombre_copia("nops_", c(1L, 25L), 25L, "pdf"), c("nops_01.pdf", "nops_25.pdf"))
  expect_identical(nombre_copia("nops_", 7L, 100L, "pdf"), "nops_007.pdf")
  expect_identical(nombre_copia("tmp_", 1L, 1L, "docx"), "tmp_1.docx")
  skip_if_not(listo, motivo)
  # contra R/exams: los archivos que deja exams2nops() con 10 copias
  d <- tempfile("nops10"); dir.create(d)
  suppressWarnings(suppressMessages(exams::exams2nops(
    file.path(FIXT, "f_texto.Rmd"), n = 10, date = as.Date("2026-11-20"), language = "es", duplex = TRUE,
    twocolumn = FALSE, dir = d, name = "nops_")))
  expect_setequal(list.files(d, "\\.pdf$"), nombre_copia("nops_", 1:10, 10L, "pdf"))
})

test_that("leer_config(): el tope del pre-vuelo es 60 + 10 × N por defecto y respeta el que traiga la configuración", {
  d <- tempfile("cfg"); dir.create(file.path(d, "salida"), recursive = TRUE)
  cfg <- file.path(d, "config.R")
  escribir_config(cfg, "f_texto", N = 25L, salidas = "clave", dir_salida = "salida/x")
  expect_equal(leer_config(cfg)$presupuesto_s, 60 + 10 * 25)
  escribir_config(cfg, "f_texto", N = 2L, salidas = "clave", dir_salida = "salida/x")
  expect_equal(leer_config(cfg)$presupuesto_s, 80)
  escribir_config(cfg, "f_texto", N = 25L, presupuesto_s = 20, salidas = "clave", dir_salida = "salida/x")
  expect_equal(leer_config(cfg)$presupuesto_s, 20)
})

test_that("leer_config(): partir_preguntas es TRUE por defecto (la LEY: caben más) y solo admite TRUE o FALSE", {
  d <- tempfile("cfg"); dir.create(file.path(d, "salida"), recursive = TRUE)
  cfg <- file.path(d, "config.R")
  escribir_config(cfg, "f_texto", salidas = "clave", dir_salida = "salida/x")
  expect_true(leer_config(cfg)$partir_preguntas)
  escribir_config(cfg, "f_texto", salidas = "clave", dir_salida = "salida/x", partir = FALSE)
  expect_false(leer_config(cfg)$partir_preguntas)
  escribir_config(cfg, "f_texto", salidas = "clave", dir_salida = "salida/x", partir = "no")
  expect_error(leer_config(cfg), "ERR_CONFIG: partir_preguntas")
})

test_that("solo_letras() y testigos_solucion(): trozos de la Solution que no están en ningún enunciado", {
  expect_identical(solo_letras("La re-\nspuesta: es 12,5 \uFB01n."), "larespuestaes125fin")
  ex <- list(question = "¿Cuántas fichas hay en total en la caja del enunciado de hoy?", questionlist = c("36", "13"),
             solution = c("Se multiplica el número de filas por el número de fichas: $9 \\cdot 4 = 36$.", "",
                          "¿Cuántas fichas hay en total en la caja del enunciado de hoy?"),
             solutionlist = c("Verdadero: es el producto de las dos cifras.", "Falso."))
  t <- testigos_solucion(ex, excluir = solo_letras(c(ex$question, ex$questionlist)))
  expect_true("semultiplicaelnúmerodefilaspor" %in% t)              # 30 letras del trozo más largo
  expect_true("verdaderoeselproductodelasdosc" %in% t)              # cada elemento de la lista, por separado
  expect_false(any(grepl("cuántasfichashay", t, fixed = TRUE)))     # lo que repite el enunciado no sirve de testigo
  expect_true(all(nchar(t) >= 20 & nchar(t) <= 30))
  expect_length(testigos_solucion(list(solution = "Es 12.", solutionlist = NULL)), 0)   # sin texto bastante: sin testigo
})

test_that("margen_inferior_mm() mide la tinta más baja de cada página y da NA en una página en blanco", {
  skip_if_not(listo, motivo)
  r <- compilar(c("\\documentclass{article}", "\\usepackage[papersize={216mm,330mm},margin=20mm,bmargin=15mm]{geometry}",
                  "\\pagestyle{empty}", "\\begin{document}", "arriba\\vfill\\noindent\\rule{3cm}{1cm}", "\\newpage",
                  "\\null\\vfill\\noindent\\rule{3cm}{1cm}\\vspace*{-12mm}", "\\newpage\\null", "\\end{document}"))
  expect_equal(r$status, 0L)
  m <- margen_inferior_mm(r$pdf)
  expect_length(m, 3)
  expect_equal(m[1], 15, tolerance = 1.5 / 15)       # el filete termina en el margen inferior: 15 mm
  expect_lt(m[2], MARGEN_INFERIOR_MIN_MM)            # el de la página 2 baja 12 mm más: queda a unos 3 mm del borde
  expect_true(is.na(m[3]))                           # página en blanco
})

test_that("matriz_semillas() es reproducible, distinta por sesión y de N x Q", {
  a <- matriz_semillas(2025L, 1L, 3L, 11L)
  expect_equal(dim(a), c(3L, 11L))
  expect_identical(a, matriz_semillas(2025L, 1L, 3L, 11L))
  expect_false(identical(a, matriz_semillas(2025L, 2L, 3L, 11L)))
})

test_that("clave_desde() y clave_texto() leen la clave de metainfo$solution", {
  ex <- function(sol) list(metainfo = list(solution = sol))
  x <- list("26112000001" = list(ex(c(FALSE, TRUE, FALSE, FALSE)), ex(c(TRUE, FALSE, FALSE, FALSE))),
            "26112000002" = list(ex(c(FALSE, FALSE, FALSE, TRUE)), ex(c(FALSE, TRUE, TRUE, FALSE))))
  m <- clave_desde(x)
  expect_equal(dim(m), c(2L, 2L))
  expect_equal(unname(m[1, ]), c("b", "a"))
  expect_equal(unname(m[2, ]), c("d", "bc"))               # mchoice: las letras se unen
  expect_equal(rownames(m), names(x))
  expect_equal(clave_texto(m[1, ]), "1 b · 2 a")
  uno <- clave_desde(list(v = list(ex(c(FALSE, FALSE, TRUE)))))    # una sola pregunta sigue siendo matriz
  expect_equal(dim(uno), c(1L, 1L))
})

test_that("escapar_latex() escapa en una sola pasada y el resultado compila", {
  crudo <- "a & b_c 50 % #1 $ {x} ~ ^ \\"
  esperado <- "a \\& b\\_c 50 \\% \\#1 \\$ \\{x\\} \\textasciitilde{} \\textasciicircum{} \\textbackslash{}"
  expect_identical(escapar_latex(crudo), esperado)
  expect_identical(escapar_latex(c("Sesión 1", "A_B")), c("Sesión 1", "A\\_B"))     # vectorizada; las tildes no se tocan
  skip_if_not(listo, motivo)
  r <- compilar(c("\\documentclass{article}", "\\usepackage[T1]{fontenc}", "\\begin{document}", esperado, "\\end{document}"))
  expect_equal(r$status, 0L)
  # control: sin escapar, el mismo texto NO compila
  malo <- compilar(c("\\documentclass{article}", "\\begin{document}", crudo, "\\end{document}"))
  expect_false(identical(malo$status, 0L))
})

test_that("normalizar() iguala las figuras entre formatos y no esconde un número cambiado", {
  pdf <- list(question = "Mida el lado: \\includegraphics[width=5cm]{rectangulo_ab12cd.pdf} de 7 cm", questionlist = c("14", "9"))
  png <- list(question = "Mida el lado: \\includegraphics[width=5cm]{rectangulo_ab12cd.png} de 7 cm", questionlist = c("14", "9"))
  expect_identical(huella(pdf), huella(png))
  expect_identical(normalizar("![](fig_b.png){width=100%}"), "FIG")
  otro <- pdf; otro$question <- sub("7 cm", "8 cm", otro$question, fixed = TRUE)
  expect_false(identical(huella(pdf), huella(otro)))
  opc <- pdf; opc$questionlist <- c("14", "10")
  expect_false(identical(huella(pdf), huella(opc)))
})

test_that("decidir_exit(): FAIL manda, después NO_VERIFICABLE; WARN y MANUAL no bloquean", {
  r <- function(...) lapply(c(...), function(e) resultado("C", e))
  expect_equal(decidir_exit(r("PASS", "WARN", "MANUAL")), 0L)
  expect_equal(decidir_exit(r("PASS", "NO_VERIFICABLE", "WARN")), 2L)
  expect_equal(decidir_exit(r("NO_VERIFICABLE", "FAIL", "PASS")), 1L)
})

test_that("leer_log() ve los Overfull de un log que no es UTF-8, con cualquier options(encoding)", {
  f <- tempfile(fileext = ".log")
  # como lo escribe pdflatex: la «ó» de T1 es el byte F3 y el «×» de TS1 es D6. Hay un Overfull ANTES del primer byte
  # inválido y dos DESPUÉS
  writeBin(c(charToRaw("Overfull \\hbox (58.55467pt too wide) in paragraph at lines 42--42\n[]\\T1/phv/m/n/10.95 Fracci"),
             as.raw(0xf3), charToRaw("n \\TS1/cmtt/m/n/10.95 "), as.raw(0xd6),
             charToRaw("\nOverfull \\hbox (33.32738pt too wide) in paragraph at lines 50--52\n"),
             charToRaw("Overfull \\hbox (0.4pt too wide) in paragraph at lines 9--9\n")), f)
  esperado <- c(58.55467, 33.32738, 0.4)
  viejo <- getOption("encoding"); on.exit(options(encoding = viejo))
  ingenuo <- function() tryCatch(suppressWarnings(overfull_pt(paste(readLines(f, warn = FALSE), collapse = "\n"))),
                                 error = function(e) numeric(0))
  for (enc in c("native.enc", "UTF-8")) {
    options(encoding = enc)
    x <- leer_log(f)
    expect_true(validUTF8(x), info = enc)
    expect_equal(overfull_pt(x), esperado, info = enc)
    # control positivo: la lectura de antes pierde Overfull en ese mismo archivo, de una manera distinta en cada modo
    # (sin coincidencias con native.enc; cortada en el primer byte inválido con UTF-8)
    expect_lt(length(ingenuo()), length(esperado))
  }
  options(encoding = viejo)
  expect_true(is.na(leer_log(file.path(tempdir(), "no_existe.log"))))     # un log ilegible no aprueba
})

test_that("geometría del Word: ancho de la columna y sangría del párrafo", {
  sec <- function(cols) paste0('<w:body><w:sectPr><w:pgSz w:h="18709" w:w="12246" /><w:pgMar w:bottom="850" ',
                               'w:left="567" w:right="567" w:top="680" />', cols, "</w:sectPr></w:body>")
  expect_equal(docx_columna_twips(sec('<w:cols w:num="2" w:sep="1" w:space="340" />')), 5386)
  expect_equal(docx_columna_twips(sec("")), 12246 - 2 * 567)                 # sin w:cols: una columna
  expect_error(docx_columna_twips("<w:body></w:body>"), "sectPr")
  numbering <- paste0('<w:abstractNum w:abstractNumId="3"><w:lvl w:ilvl="0"><w:pPr><w:ind w:left="700" w:hanging="360" />',
                      '</w:pPr></w:lvl><w:lvl w:ilvl="1"><w:pPr><w:ind w:left="1500" w:hanging="360" /></w:pPr></w:lvl>',
                      '</w:abstractNum><w:num w:numId="7"><w:abstractNumId w:val="3" /></w:num>')
  par <- function(ppr) paste0("<w:p>", ppr, "<w:r><w:t>x</w:t></w:r></w:p>")
  lista <- function(ilvl, numid) par(sprintf('<w:pPr><w:numPr><w:ilvl w:val="%d" /><w:numId w:val="%d" /></w:numPr></w:pPr>', ilvl, numid))
  expect_equal(docx_sangria_twips(lista(0, 7), numbering), 700)              # la de numbering.xml
  expect_equal(docx_sangria_twips(lista(1, 7), numbering), 1500)
  expect_equal(docx_sangria_twips(lista(1, 99), numbering), 1440)            # lista sin declarar: 720 por nivel
  expect_equal(docx_sangria_twips(par('<w:pPr><w:ind w:left="300" /></w:pPr>'), numbering), 300)
  expect_equal(docx_sangria_twips(par(""), numbering), 0)
})

test_that("reducir_tablas() reparte la rejilla de una tabla que no cabe y no toca la que sí cabe", {
  tabla <- function(anchos, ind) paste0(
    '<w:tbl><w:tblPr><w:tblW w:type="auto" w:w="0" /><w:jc w:val="left" /><w:tblInd w:w="', ind, '" w:type="dxa" /></w:tblPr>',
    "<w:tblGrid>", paste0('<w:gridCol w:w="', anchos, '" />', collapse = ""), "</w:tblGrid><w:tr><w:tc><w:tcPr /><w:p /></w:tc></w:tr></w:tbl>")
  rejilla <- function(x) as.numeric(regmatches(x, gregexpr('(?<=<w:gridCol w:w=")\\d+', x, perl = TRUE))[[1]])
  # como la escribe pandoc: 7920 twips repartidos por igual, con la sangría de la lista; columna de 5386 twips
  ancha <- tabla(c(1980, 1980, 1980, 1980), 720)
  r <- reducir_tablas(paste0("<w:body>", ancha, "</w:body>"), 5386)
  expect_equal(r$n, 1L)
  expect_lte(sum(rejilla(r$doc)), 5386 - 720)
  expect_gte(sum(rejilla(r$doc)), 5386 - 720 - 4)            # no la encoge de más: solo el redondeo de 4 columnas
  expect_true(grepl(sprintf('<w:tblW w:type="dxa" w:w="%.0f" />', sum(rejilla(r$doc))), r$doc, fixed = TRUE))
  # proporciones: una rejilla desigual conserva su reparto
  des <- reducir_tablas(tabla(c(4000, 2000, 2000), 0), 5386)
  g <- rejilla(des$doc)
  expect_equal(g / sum(g), c(0.5, 0.25, 0.25), tolerance = 1e-3)
  # la que cabe queda idéntica, y una segunda pasada no cambia nada
  cabe <- tabla(c(1000, 1000), 720)
  expect_identical(reducir_tablas(cabe, 5386), list(doc = cabe, n = 0L))
  expect_equal(reducir_tablas(r$doc, 5386)$n, 0L)
})

# -----------------------------------------------------------------------------
# 2. El medidor
# -----------------------------------------------------------------------------
test_that("medir_pdf.py lee las anclas: numeración, columnas y preguntas partidas", {
  skip_if_not(listo, motivo)
  r <- compilar(c("\\documentclass{article}", "\\usepackage{multicol,hyperref}", "\\begin{document}", "\\begin{multicols}{2}",
                  "\\hypertarget{qini.1}{}Uno\\hypertarget{qfin.1}{}\\par",
                  "\\hypertarget{qini.2}{}Dos\\columnbreak\\par sigue\\hypertarget{qfin.2}{}\\par",
                  "\\hypertarget{qini.3}{}Tres\\hypertarget{qfin.3}{}", "\\end{multicols}", "\\end{document}"))
  expect_equal(r$status, 0L)
  m <- medir_json(r$pdf, "--columnas", "2")
  expect_identical(m$marcas, "dests")
  expect_equal(unlist(m$numeros), 1:3)
  expect_equal(unlist(m$partidas), 2L)                    # la 2 empieza en la columna 1 y termina en la 2
  expect_length(m$sin_fin, 0)
  # un número repetido deja una sola ancla: la numeración ya no es 1..3 (lo que mide C13)
  rep <- compilar(c("\\documentclass{article}", "\\usepackage{hyperref}", "\\begin{document}",
                    "\\hypertarget{qini.1}{}Uno\\par\\hypertarget{qini.2}{}Dos\\par\\hypertarget{qini.2}{}Otra vez dos",
                    "\\end{document}"))
  expect_false(identical(as.integer(unlist(medir_json(rep$pdf)$numeros)), 1:3))
})

test_that("medir_pdf.py aparta el texto de una fuente Type 3: su tamaño es una estimación", {
  skip_if_not(listo, motivo)
  m <- medir_json(file.path(FIXT, "m_type3_fig.pdf"))
  expect_length(m$fuentes_type3, 1)
  expect_gt(m$texto_type3, 0)
  expect_null(m$texto_min_pt)                             # ningún tamaño medible: no puede aprobar por tamaño
  # control: poppler le pone 8 pt a rótulos que miden 5 pt; por eso no se le cree
  x <- suppressWarnings(system2("pdftohtml", c("-xml", "-i", "-q", "-stdout", "-zoom", "1",
                                               shQuote(file.path(FIXT, "m_type3_fig.pdf"))), stdout = TRUE, stderr = FALSE))
  tam <- as.numeric(sub('.*size="([0-9.]+)".*', "\\1", grep("<fontspec", x, value = TRUE)))
  expect_true(length(tam) > 0 && all(tam >= 7))
})

# -----------------------------------------------------------------------------
# 3. El motor de punta a punta
# -----------------------------------------------------------------------------
BASE <- tempfile("cuadernillos_"); dir.create(BASE)
FELIZ <- file.path(BASE, "salida", "feliz")
CFG <- file.path(BASE, "config.R")
SES <- file.path(FELIZ, "sesion-1")
FIX_FELIZ <- c("f_lista", "f_texto", "f_tikz", "f_tabla", "f_plot")

motor <- function(...) processx::run(RSCRIPT, c(file.path(DIR, "generar_cuadernillos.R"), ...), wd = BASE,
                                     error_on_status = FALSE, timeout = 600, stderr_to_stdout = TRUE,
                                     env = c("current", CUADERNILLOS_MOTOR_DIR = DIR))

test_that("camino feliz: 5 fixtures, 2 versiones y las 5 salidas dan exit 0 con C1-C9 y C11-C15 en PASS", {
  skip_if_not(listo, motivo)
  escribir_config(CFG, FIX_FELIZ, N = 2L, salidas = c("cuadernillo", "hoja", "clave", "retro", "word"))
  p <- motor(CFG, "--dir-salida", FELIZ)
  expect_equal(p$status, 0L, info = paste(utils::tail(strsplit(p$stdout, "\n")[[1]], 25), collapse = "\n"))
  expect_true(file.exists(file.path(FELIZ, MARCADOR)))
  v <- jsonlite::fromJSON(file.path(SES, "verificacion.json"), simplifyVector = FALSE)
  est <- stats::setNames(vapply(v, `[[`, "", "estado"), vapply(v, `[[`, "", "id"))
  expect_setequal(names(est), sprintf("C%d", 1:15))
  expect_identical(unname(est["C10"]), "MANUAL")           # C10 nunca cuenta como PASS
  otras <- setdiff(names(est), "C10")
  expect_true(all(est[otras] == "PASS"), info = paste(names(est), est, collapse = " · "))
})

test_that("camino feliz: una versión por estudiante, con el mismo ID en todas las salidas", {
  skip_if_not(listo, motivo)
  skip_if_not(file.exists(file.path(SES, "firmas.rds")), "el camino feliz no terminó")
  f <- readRDS(file.path(SES, "firmas.rds"))
  ids <- vapply(1:2, function(i) id_version("2026-11-13", 1L, i), "")
  expect_identical(f$ids, ids)
  expect_identical(f$ids_nops, ids)
  esperados <- c(file.path("cuadernillos", paste0("cuadernillo_", ids, ".pdf")), file.path("cuadernillos", "cuadernillos_imprimir.pdf"),
                 file.path("docente", paste0("cuadernillo_docente_", ids, ".pdf")), file.path("docente", "docente_imprimir.pdf"),
                 file.path("hojas", paste0("hoja_respuestas_", ids, ".pdf")), file.path("hojas", "hojas_imprimir.pdf"),
                 file.path("retro", paste0("retro_", ids, ".pdf")), file.path("word", paste0("cuadernillo_", ids, ".docx")),
                 file.path("clave", c("clave.csv", "clave.pdf")), "revision_gris_v1.png", "revision_gris_vN.png",
                 "semillas.rds", "semillas.csv", file.path("nops", "nops_.rds"))
  expect_true(all(file.exists(file.path(SES, esperados))), info = paste(esperados[!file.exists(file.path(SES, esperados))], collapse = ", "))
  # las cuatro salidas con texto dan la misma huella y la misma clave (C6), y son las 5 preguntas de cada versión
  expect_setequal(names(f$huella), c("nops", "cuadernillo", "docente", "retro", "word"))
  for (o in c("cuadernillo", "docente", "retro", "word")) {
    expect_identical(f$huella[[o]], f$huella$nops)
    expect_identical(f$clave[[o]], f$clave$nops)
  }
  expect_equal(dim(f$huella$nops), c(2L, 5L))
  clave <- utils::read.csv(file.path(SES, "clave", "clave.csv"), colClasses = "character")
  expect_identical(clave$version, ids)
  expect_identical(unname(as.matrix(clave[, -1])), unname(f$clave$nops))
})

test_that("camino feliz: hojas de 2 caras exactas, cuadernillos pares y título con «&» y «_» impreso", {
  skip_if_not(listo, motivo)
  skip_if_not(file.exists(file.path(SES, "firmas.rds")), "el camino feliz no terminó")
  ids <- readRDS(file.path(SES, "firmas.rds"))$ids
  for (id in ids) {
    expect_equal(paginas_pdf(file.path(SES, "hojas", sprintf("hoja_respuestas_%s.pdf", id))), 2L)
    expect_equal(paginas_pdf(file.path(SES, "cuadernillos", sprintf("cuadernillo_%s.pdf", id))) %% 2L, 0L)
    t1 <- texto_pdf(file.path(SES, "cuadernillos", sprintf("cuadernillo_%s.pdf", id)))
    expect_true(grepl("Prueba & mutante_1", t1, fixed = TRUE))    # el título del config, escapado por el motor
    expect_true(grepl(id, t1, fixed = TRUE))
  }
  expect_equal(paginas_pdf(file.path(SES, "hojas", "hojas_imprimir.pdf")) %% 2L, 0L)
})

test_that("camino feliz: numeración 1..5 con listas que no empiezan en 1, sin Type 3 y sin Overfull", {
  skip_if_not(listo, motivo)
  skip_if_not(file.exists(file.path(SES, "firmas.rds")), "el camino feliz no terminó")
  ids <- readRDS(file.path(SES, "firmas.rds"))$ids
  for (tipo in c("cuadernillos", "docente", "retro")) for (id in ids) {
    pdf <- file.path(SES, tipo, sprintf(NOMBRE_PDF[[tipo]], id))
    m <- medir_json(pdf, "--columnas", if (tipo == "retro") "1" else "2")
    expect_equal(unlist(m$numeros), 1:5, info = pdf)
    expect_length(m$fuentes_type3, 0)
    expect_gte(m$texto_min_pt, 7)
    lg <- leer_log(file.path(SES, "tex", tipo, id, "tmp_1.log"))
    expect_false(is.na(lg))
    expect_false(any(overfull_pt(lg) > 1), info = pdf)
  }
  # la lista del enunciado conserva los números que escribió el autor (3 y 4), y la pregunta siguiente es la 2
  t1 <- texto_pdf(file.path(SES, "cuadernillos", sprintf("cuadernillo_%s.pdf", ids[1])))
  expect_true(grepl("3. Sumar las tres cantidades", t1, fixed = TRUE))
  expect_true(grepl("4. Anotar el total", t1, fixed = TRUE))
})

test_that("camino feliz: control negativo de C12, el pre-vuelo da estado=igual en los 5 fixtures", {
  skip_if_not(listo, motivo)
  skip_if_not(file.exists(file.path(SES, "preflight.log")), "el camino feliz no terminó")
  pf <- leer_preflight(file.path(SES, "preflight.log"))
  expect_equal(nrow(pf$tabla), 5L)
  expect_true(all(pf$tabla$fin))
  expect_true(all(pf$tabla$estado == "igual"))
  expect_true(all(pf$tabla$estatico == "igual"))
  expect_true(pf$completo)
})

test_that("camino feliz: la retroalimentación está en el cuadernillo del docente y NO en el del estudiante", {
  skip_if_not(listo, motivo)
  skip_if_not(file.exists(file.path(SES, "firmas.rds")), "el camino feliz no terminó")
  f <- readRDS(file.path(SES, "firmas.rds"))
  expect_true(f$partir_preguntas)
  expect_equal(sum(lengths(unlist(f$testigos, recursive = FALSE)) > 0), 10L)     # hay testigos en las 10 preguntas (2 x 5)
  for (i in seq_along(f$ids)) {
    est <- solo_letras(texto_pdf_completo(file.path(SES, "cuadernillos", sprintf("cuadernillo_%s.pdf", f$ids[i]))))
    doc <- solo_letras(texto_pdf_completo(file.path(SES, "docente", sprintf("cuadernillo_docente_%s.pdf", f$ids[i]))))
    for (w in f$testigos[[i]]) {
      expect_false(any(vapply(w, grepl, TRUE, x = est, fixed = TRUE)))
      expect_true(any(vapply(w, grepl, TRUE, x = doc, fixed = TRUE)))
    }
    expect_false(grepl("retroalimentación", est, fixed = TRUE))
    expect_true(grepl("cuadernillodeldocente", doc, fixed = TRUE))
    expect_true(grepl("clave1", doc, fixed = TRUE))                               # la clave va en su primera página
    expect_equal(paginas_pdf(file.path(SES, "docente", sprintf("cuadernillo_docente_%s.pdf", f$ids[i]))) %% 2L, 0L)
    expect_true(all(margen_inferior_mm(file.path(SES, "docente", sprintf("cuadernillo_docente_%s.pdf", f$ids[i]))) >=
                      MARGEN_INFERIOR_MIN_MM, na.rm = TRUE))
  }
})

test_that("partir_preguntas = FALSE deja cada pregunta entera; por defecto la misma pregunta se parte", {
  skip_if_not(listo, motivo)
  # f_texto y m_alta: la figura de m_alta ocupa una columna entera. Partida cabe; entera se sale de la primera página.
  correr <- function(nombre, partir) {
    d <- file.path(BASE, nombre); dir.create(d)
    cfg <- file.path(d, "config.R"); escribir_config(cfg, c("f_texto", "m_alta"), N = 1L, salidas = "cuadernillo", partir = partir)
    ds <- file.path(d, "salida", nombre)
    p <- correr_motor(file.path(DIR, "generar_cuadernillos.R"), c(cfg, "--dir-salida", ds), dir_plantillas(), d)
    ses <- file.path(ds, "sesion-1")
    v <- jsonlite::fromJSON(file.path(ses, "verificacion.json"), simplifyVector = FALSE)
    est <- stats::setNames(vapply(v, `[[`, "", "estado"), vapply(v, `[[`, "", "id"))
    pdf <- list.files(file.path(ses, "cuadernillos"), "^cuadernillo_2.*\\.pdf$", full.names = TRUE)
    list(status = p$status, est = est, m = medir_json(pdf, "--columnas", "2"), margen = margen_inferior_mm(pdf))
  }
  a <- correr("partida", NULL)                                  # por defecto: se parte
  expect_equal(a$status, 0L)
  expect_identical(unname(a$est[c("C3", "C14")]), c("PASS", "PASS"))
  expect_equal(unlist(a$m$partidas), 2L)                        # la pregunta 2 empieza en una columna y termina en otra
  expect_true(all(a$margen >= MARGEN_INFERIOR_MIN_MM, na.rm = TRUE))
  b <- correr("entera", FALSE)                                  # entera: no se parte, pero no cabe y C3 lo dice
  expect_equal(b$status, 1L)
  expect_length(b$m$partidas, 0)
  expect_identical(unname(b$est["C14"]), "PASS")
  expect_identical(unname(b$est["C3"]), "FAIL")
  expect_true(any(b$margen < MARGEN_INFERIOR_MIN_MM, na.rm = TRUE))
  # con preguntas que sí caben, enteras da exit 0 y ninguna partida
  d <- file.path(BASE, "enteras5"); dir.create(d)
  cfg <- file.path(d, "config.R"); escribir_config(cfg, FIX_FELIZ, N = 1L, salidas = "cuadernillo", partir = FALSE)
  ds <- file.path(d, "salida", "enteras5")
  p <- correr_motor(file.path(DIR, "generar_cuadernillos.R"), c(cfg, "--dir-salida", ds), dir_plantillas(), d)
  expect_equal(p$status, 0L, info = paste(utils::tail(strsplit(p$salida, "\n")[[1]], 12), collapse = "\n"))
  for (tipo in c("cuadernillos", "docente")) {
    pdf <- list.files(file.path(ds, "sesion-1", tipo), "^cuadernillo_.*2[0-9]+\\.pdf$", full.names = TRUE)
    expect_length(pdf, 1)
    m <- medir_json(pdf, "--columnas", "2")
    expect_length(m$partidas, 0)
    expect_equal(unlist(m$numeros), 1:5)
  }
})

test_that("camino feliz: el Word es oficio a dos columnas y ninguna imagen ni tabla se sale de su columna", {
  skip_if_not(listo, motivo)
  skip_if_not(file.exists(file.path(SES, "firmas.rds")), "el camino feliz no terminó")
  ids <- readRDS(file.path(SES, "firmas.rds"))$ids
  for (id in ids) {
    docx <- file.path(SES, "word", sprintf("cuadernillo_%s.docx", id))
    expect_equal(system2("unzip", c("-tq", shQuote(docx)), stdout = FALSE, stderr = FALSE), 0L)   # zip íntegro
    doc <- docx_parte(docx, "word/document.xml")
    expect_equal(docx_columna_twips(doc), 5386)
    expect_false(grepl("##", doc, fixed = TRUE))
    im <- docx_imagenes(docx)
    expect_gt(nrow(im), 0)
    expect_true(all(im$cx <= im$disponible + 12700))
    tb <- docx_tablas(docx)
    expect_gt(nrow(tb), 0)                                   # la de f_tabla
    expect_true(all(tb$ancho <= tb$disponible + 20))
    expect_true(all(ppi_docx(docx) >= PPI_MIN))
    # pandoc lo vuelve a leer: el .docx reescrito sigue siendo un documento válido
    expect_equal(system2("pandoc", c(shQuote(docx), "-t", "plain", "-o", shQuote(tempfile(fileext = ".txt"))),
                         stdout = FALSE, stderr = FALSE), 0L)
  }
})

test_that("ajustar_docx_a_columna() reduce imágenes y tablas sin deformar y no toca lo que ya cabe", {
  skip_if_not(listo, motivo)
  skip_if_not(file.exists(file.path(SES, "firmas.rds")), "el camino feliz no terminó")
  # un Word generado SIN la reducción (el mutante M16 del motor), con una gráfica de knitr y una tabla
  d <- file.path(BASE, "sin_ajustar"); dir.create(d)
  m16 <- Filter(function(m) m$id == "M16", MUTANTES)[[1]]
  mot <- file.path(d, "generar_cuadernillos.R")
  file.copy(file.path(DIR, "generar_cuadernillos.R"), mot)
  reemplazo_exacto(mot, m16$de, m16$a)
  cfg <- file.path(d, "config.R"); escribir_config(cfg, c("f_plot", "f_tabla"), N = 1L, salidas = "word")
  ds <- file.path(d, "salida", "w")
  p <- correr_motor(mot, c(cfg, "--dir-salida", ds), dir_plantillas(), d)
  docx <- list.files(file.path(ds, "sesion-1", "word"), "\\.docx$", full.names = TRUE)
  expect_length(docx, 1)
  antes <- docx_imagenes(docx); tantes <- docx_tablas(docx)
  expect_true(any(antes$cx > antes$disponible))            # la gráfica se sale: es lo que el motor corrige
  expect_true(any(tantes$ancho > tantes$disponible))       # y la tabla también
  n <- ajustar_docx_a_columna(docx)
  despues <- docx_imagenes(docx); tdespues <- docx_tablas(docx)
  expect_equal(n[["imagenes"]], sum(antes$cx > antes$disponible))
  expect_equal(n[["tablas"]], sum(tantes$ancho > tantes$disponible))
  expect_true(all(despues$cx <= despues$disponible))
  expect_true(all(tdespues$ancho <= tdespues$disponible))
  expect_equal(despues$cx / despues$cy, antes$cx / antes$cy, tolerance = 1e-3)     # misma proporción
  expect_equal(sum(ajustar_docx_a_columna(docx)), 0L)      # idempotente: ya no hay nada que reducir
  # el texto de la tabla sigue entero después de reescribir el .docx
  txt <- paste(suppressWarnings(system2("pandoc", c(shQuote(docx), "-t", "plain"), stdout = TRUE, stderr = FALSE)), collapse = "\n")
  expect_true(all(vapply(c("Lunes", "Martes", "Miércoles", "Ventas"), grepl, TRUE, x = txt, fixed = TRUE)))
})

test_that("un directorio de salida con contenido se rechaza sin --limpiar y no se borra", {
  skip_if_not(listo, motivo)
  skip_if_not(file.exists(file.path(SES, "firmas.rds")), "el camino feliz no terminó")
  antes <- file.info(file.path(SES, "firmas.rds"))$mtime
  p <- motor(CFG, "--dir-salida", FELIZ)
  expect_equal(p$status, 2L)
  expect_true(grepl("ERR_DIR_SALIDA", p$stdout, fixed = TRUE))
  expect_identical(file.info(file.path(SES, "firmas.rds"))$mtime, antes)
})

test_that("con 10 versiones (índice de dos cifras) el motor arma las 10 hojas y la clave", {
  skip_if_not(listo, motivo)
  # el defecto del 2026-11-14: con 10 o más versiones R/exams escribe nops_01.pdf y el motor buscaba nops_1.pdf
  d <- file.path(BASE, "diez"); dir.create(d)
  cfg <- file.path(d, "config.R"); escribir_config(cfg, "f_texto", N = 10L, salidas = c("hoja", "clave"))
  ds <- file.path(d, "salida", "diez")
  p <- correr_motor(file.path(DIR, "generar_cuadernillos.R"), c(cfg, "--dir-salida", ds), dir_plantillas(), d)
  expect_equal(p$status, 0L, info = paste(utils::tail(strsplit(p$salida, "\n")[[1]], 12), collapse = "\n"))
  ses <- file.path(ds, "sesion-1")
  ids <- vapply(1:10, function(i) id_version("2026-11-13", 1L, i), "")
  expect_identical(readRDS(file.path(ses, "firmas.rds"))$ids, ids)
  hojas <- file.path(ses, "hojas", paste0("hoja_respuestas_", ids, ".pdf"))
  expect_true(all(file.exists(hojas)))
  expect_true(all(vapply(hojas, paginas_pdf, 1L) == 2L))
  expect_equal(paginas_pdf(file.path(ses, "hojas", "hojas_imprimir.pdf")), 20L)
  expect_equal(nrow(utils::read.csv(file.path(ses, "clave", "clave.csv"))), 10L)
})

test_that("guarda de ruta (M14): «..», «.» y una carpeta ajena sin marcador se rechazan sin borrar nada", {
  skip_if_not(listo, motivo)
  salida <- utils::capture.output(st <- correr_mutantes("M14"))
  expect_equal(st, 0L, info = paste(salida, collapse = "\n"))
})

test_that("los 26 mutantes mueren: cada guarda falla cuando se rompe lo que vigila", {
  skip_if_not(listo, motivo)
  salida <- utils::capture.output(st <- correr_mutantes())
  expect_equal(st, 0L, info = paste(salida, collapse = "\n"))
  expect_true(any(grepl("^26 de 26 mutantes mueren", salida)), info = paste(salida, collapse = "\n"))
  # cada guarda del verificador que certifica el exit 0 tiene al menos un mutante
  guardas <- unique(vapply(MUTANTES, `[[`, "", "guarda"))
  expect_setequal(guardas, sprintf("C%d", c(1:9, 11:15)))
})

unlink(BASE, recursive = TRUE)
