# Semillero de diagrama-venn-juegos-mesa-n3 (SCHOICE, MAT-2026-07-014)
# Adaptado de A-Produccion/perifericos/SemilleroUnico_v2.R para imprimir en hojas oficio (216 × 330 mm).
# Las condiciones medidas están en HANDOFF.md y en la invariante I-5 de .claude/CLAUDE.md. La sección «uso»
# de verificar_espacio.R vigila los argumentos de impresión de este archivo.
#
# 2026-11-09: el 2026-11-07 este archivo se había reemplazado por la copia genérica de perifericos/, que
# imprime con solpcielo.tex (parte las cuatro figuras de las preguntas que empiezan a media página), deja el
# NOPS a una columna y no lo centra en oficio. Se restauró la versión del 2026-11-06 y se conservó lo que
# traía la copia genérica: 10 preguntas por versión y el cuestionario interactivo, que ahora funciona sin
# internet. No volver a reemplazarlo por la copia de perifericos/.
# Ejecutar con el directorio de trabajo en esta carpeta.
library(exams)

# Configurar modo generación de exámenes para evitar pruebas test_that
.exams_generation_mode <- TRUE

archivo_examen <- "diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_schoice_v1.Rmd"
copias <- 1    # Versiones a generar (exámenes impresos y preguntas de Moodle): máximo 100, decisión del profesor
numpreg <- 10  # Preguntas por versión
stopifnot("copias debe estar entre 1 y 100" = copias >= 1 && copias <= 100)
dir_salida <- "salida/semillero"
dir_ejercicios <- "."
dir.create(dir_salida, recursive = TRUE, showWarnings = FALSE)

nombre_sin_extension <- sub("\\.Rmd$", "", archivo_examen)
nombre_arch <- paste0(nombre_sin_extension, "_")
nombre_nops <- paste0(nombre_sin_extension, "_nops_")

# PDF en oficio. La plantilla empieza cada pregunta en una página nueva: así las cuatro opciones
# quedan juntas (con solpcielo.tex se parten las que empiezan a media página).
exams2pdf(rep(archivo_examen, numpreg),
          n = copias,
          name = nombre_arch,
          encoding = "UTF-8",
          template = "solpcielo_una_por_pagina",
          dir = dir_salida,
          edir = dir_ejercicios,
          verbose = TRUE)

# DOCX. No conserva las cuatro opciones en una página: no usarlo para imprimir este ítem.
exams2pandoc(rep(archivo_examen, numpreg),
             n = copias,
             name = nombre_arch,
             encoding = "UTF-8",
             template = "pcielo.tex",
             header = list(Date = Sys.Date()),
             dir = dir_salida,
             edir = dir_ejercicios,
             type = "docx")

# Moodle. La versión s usa la semilla s, la misma que validó el paso 9 (validar_stresstest.R, N = 100):
# se exporta lo validado. Moodle baraja las opciones por su cuenta.
exams2moodle(archivo_examen,
             n = copias,
             seed = matrix(seq_len(copias), ncol = 1),
             svg = TRUE,
             name = nombre_arch,
             encoding = "UTF-8",
             dir = dir_salida,
             edir = dir_ejercicios,
             mchoice = list(shuffle = TRUE,
                            answernumbering = "ABCD",
                            eval = list(partial = TRUE, rule = "none")),
             verbose = TRUE)

# NOPS. twocolumn y samepage dejan las cuatro opciones en una página; \raggedbottom evita el hueco
# entre el enunciado y las viñetas. Para imprimir por una sola cara: duplex = FALSE quita la página 2 en blanco.
exams2nops(rep(archivo_examen, numpreg),
           n = copias,
           name = nombre_nops,
           encoding = "UTF-8",
           dir = dir_salida,
           edir = dir_ejercicios,
           language = "es",
           title = "Evaluación de Matemáticas",
           institution = "I. E. Pedacito de Cielo",
           logo = NULL,
           date = Sys.Date(),
           replacement = FALSE,
           blank = 0,
           duplex = TRUE,
           twocolumn = TRUE,
           samepage = TRUE,
           header = "\\raggedbottom")

# exams2nops() siempre genera A4. Aquí se centra cada página en una hoja oficio, sin escalar: esos
# *_oficio.pdf se imprimen a tamaño real. nops_scan() leyó esa colocación en simulación; con cualquier otra
# hoja o colocación, pasar antes los escaneos por Auxiliares/exams2nops-escaner/normalizar_escaneos_nops.py.
for (pdf in list.files(dir_salida, paste0("^", nombre_nops, "[0-9]+\\.pdf$"), full.names = TRUE)) {
  estado <- system2("pdfjam", c("--papersize", shQuote("{216mm,330mm}"), "--noautoscale", "true", "--quiet",
                                shQuote(pdf), "-o", shQuote(sub("\\.pdf$", "_oficio.pdf", pdf))))
  stopifnot("pdfjam no generó la versión en oficio" = estado == 0L)
}

# Cuestionario interactivo (exams2forms) para celulares sin internet. exams2webquiz() carga MathJax desde
# internet, y sin red las fórmulas salen en TeX crudo. .claude/scripts/webquiz_autocontenido.R lo incrusta y
# comprueba en un navegador sin red que todo se compone (exit 0 = WEBQUIZ_OK). Vista del estudiante, con las
# versiones 1 a copias × numpreg.
raiz <- tryCatch(suppressWarnings(system2("git", c("rev-parse", "--show-toplevel"), stdout = TRUE, stderr = FALSE)),
                 error = function(e) character(0))
script_quiz <- file.path(raiz[1], ".claude", "scripts", "webquiz_autocontenido.R")
nombre_quiz <- paste0(nombre_sin_extension, "_interactivo")
if (length(raiz) == 1L && file.exists(script_quiz)) {
  estado_quiz <- system2(file.path(R.home("bin"), "Rscript"),
                         c("--no-init-file", shQuote(script_quiz), shQuote(archivo_examen), "--n", copias * numpreg,
                           "--dir", shQuote(dir_salida), "--nombre", shQuote(nombre_quiz),
                           "--titulo", shQuote("Evaluación Interactiva de Matemáticas ICFES")))
  if (estado_quiz != 0L) {
    warning("El cuestionario interactivo no pasó la comprobación sin internet (exit ", estado_quiz, "): no llevarlo al aula")
  } else if (interactive()) {
    html_quiz <- list.files(dir_salida, paste0("^", nombre_quiz, ".*\\.html$"), full.names = TRUE)
    if (length(html_quiz)) utils::browseURL(html_quiz[1])
  }
} else {
  warning("No se encontró .claude/scripts/webquiz_autocontenido.R: no se generó el cuestionario interactivo")
}
