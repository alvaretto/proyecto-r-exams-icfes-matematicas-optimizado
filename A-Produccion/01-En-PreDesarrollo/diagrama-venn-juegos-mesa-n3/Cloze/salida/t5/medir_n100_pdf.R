## Tanda 5 · medición única N = 100 (regla #23): el PDF real de generar_pdf() del semillero, con config$preguntas_pdf = 100,
## set.seed(20261109). Por pregunta: el gap de la Parte 1 en el orden I-IV y la clave que registra exams2pdf() en el diagrama
## que reproduce las viñetas impresas. Usa las funciones del verificador (extraídas de su texto) y no escribe en Cloze/.
suppressMessages(library(exams))
C <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze"
V <- parse(file.path(C, "verificar_espacio_cloze.R"), keep.source = FALSE, encoding = "UTF-8")
QUIERO <- c("ORDEN", "leer_vinetas", "USO_OBJETOS", "cargar_semillero", "USO_ORDEN_GAP", "USO_ARCHIVOS_N", "USO_ESPIA", "poner_espia_pdf",
            "quitar_espia_pdf", "gap1_pdf", "vinetas_latex", "sonda_clave_pdf", "COMO_MARCAR", "como_marcar_en_portada", "medir_pdf_varias")
for (x in V) if (is.call(x) && identical(x[[1L]], as.name("<-")) && is.name(x[[2L]]) && as.character(x[[2L]]) %in% QUIERO) eval(x, globalenv())
PAPEL_HOJA_MM <- c(216, 330)
stopifnot(all(QUIERO %in% ls(globalenv())))
RMD <- file.path(C, "diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_cloze_v1.Rmd")
cat("md5 .Rmd:", unname(tools::md5sum(RMD)), "· md5 semillero:", unname(tools::md5sum(file.path(C, "SemilleroCloze.R"))), "\n")
d <- tempfile("n100_"); dir.create(d); setwd(d)
file.copy(RMD, d); file.copy(file.path(C, "solpcielo_cloze_una_por_pagina.tex"), d)
es <- cargar_semillero(readLines(file.path(C, "SemilleroCloze.R"), encoding = "UTF-8"))
NQ <- 100L
es$config$dir_salida <- file.path(d, "salida_hibrida"); es$config$dir_ejercicios <- d; es$config$preguntas_pdf <- NQ
dir.create(file.path(es$config$dir_salida, "pdf"), recursive = TRUE)
t0 <- proc.time()[["elapsed"]]
cap <- poner_espia_pdf(); set.seed(20261109); ok <- FALSE
invisible(utils::capture.output(ok <- tryCatch(es$generar_pdf(), finally = quitar_espia_pdf())))
pdf <- list.files(file.path(es$config$dir_salida, "pdf"), "\\.pdf$", full.names = TRUE)
cat(sprintf("generar_pdf(): %s · %d PDF · %.0f s · exshuffle recibido por exams2pdf(): %s\n", ok, length(pdf), proc.time()[["elapsed"]] - t0, deparse(cap$exshuffle)))
g <- gap1_pdf(pdf); m <- medir_pdf_varias(pdf, NQ)
cat(sprintf("gap de la Parte 1 en el orden I-IV: %d de %d (preguntas en el PDF: %d)\n", sum(g == USO_ORDEN_GAP), NQ, length(g)))
cat(sprintf("hoja %s mm · preguntas con figuras partidas: %s · «Cómo marcar» en la portada: %s · páginas: %s\n", paste(round(m$hoja), collapse = " x "),
            m$partidas, m$marcar, sub("^Pages: *", "", grep("^Pages", system2("pdfinfo", shQuote(pdf), stdout = TRUE), value = TRUE))))
kc <- sonda_clave_pdf(cap, g, NQ)
cat("clave registrada por exams2pdf() en el diagrama correcto:", if (length(kc)) paste("NO ·", attr(kc, "detalle")) else sprintf("%d de %d", NQ, NQ), "\n")
if (!length(kc)) { k <- attr(kc, "claves"); cat("posición de la clave (a)-(d):", paste(names(table(k)), table(k), sep = "=", collapse = " "), "\n") }
## Parte 6: posición de las verdaderas en el orden impreso (internal order: exshuffle = FALSE).
s6 <- t(vapply(cap$res[[1]], function(q) as.integer(q$metainfo$solution[[6]]), integer(4)))
cat("Parte 6, verdaderas por posición (%):", paste(round(100 * colMeans(s6)), collapse = " "), "· patrones distintos:", nrow(unique(s6)), "\n")
file.copy(pdf, "/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/3530c5eb-2f14-461c-b6c9-761d2d82ab0b/scratchpad/t5/semillero_n100.pdf", overwrite = TRUE)
