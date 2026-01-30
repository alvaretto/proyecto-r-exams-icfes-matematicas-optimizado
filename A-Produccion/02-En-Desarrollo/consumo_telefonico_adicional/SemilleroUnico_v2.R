# Carga de la librería r-exams
library(exams)

# Configurar modo generación de exámenes para evitar pruebas test_that
.exams_generation_mode <- TRUE

# Definición del archivo de examen y configuración inicial
archivo_examen <- "consumo_telefonico_adicional_n2_v1.Rmd"
copias <- 1  # Número de versiones a generar
numpreg <- 3
semilla_base <- sample(100:1e8, 1)
# NO establecer semilla fija - cada versión usará semilla diferente
dir_salida <- "salida"
dir_ejercicios <- "."


# Nombre del archivo sin la extensión .Rmd
nombre_sin_extension <- sub("\\.Rmd$", "", archivo_examen)
nombre_arch <- paste0(nombre_sin_extension, "_")

################################################################################
# Generación de copias individuales para PDF, sólo 'copias', no importa 'numpreg'

# for(i in 1:copias) {
#   nombre_archivo <- sprintf("%s_copia%d_", nombre_sin_extension, i)
#   exams2pdf(archivo_examen,
#             n = 1,
#             name = nombre_archivo,
#             encoding = "UTF-8",
#             template = "solpcielo",
#             dir = dir_salida,
#             edir = dir_ejercicios,
#             verbose = TRUE)
# }

################################################################################
# Generación de copias individuales para Pandoc (docx), sólo 'copias',
# no importa 'numpreg

# for(i in 1:copias) {
#   nombre_archivo <- sprintf("%s_copia%d_", nombre_sin_extension, i)
#   exams2pandoc(archivo_examen,
#                n = 1,
#                name = nombre_archivo,
#                encoding = "UTF-8",
#                template = "plain.tex",
#                dir = dir_salida,
#                edir = dir_ejercicios,
#                format = "docx",
#                verbose = TRUE)
# }

################################################################################
# Creación del examen en formato HTML, sólo 'numpreg', 'copias' = 1

# exams2html(rep(archivo_examen, numpreg),
#            svg = FALSE,
#            verbose = TRUE,
#            template = "plain",
#            name = paste0(nombre_sin_extension, "_semillero"))

#################################################################################
# Generación de n copias en un solo archivo de salida para PDF

# NO establecer semilla fija - cada versión usará semilla diferente
exams2pdf(rep(archivo_examen, numpreg),
          n = copias,
          name = nombre_arch,
          encoding = "UTF-8",
          template = "solpcielo",
          dir = dir_salida,
          edir = dir_ejercicios,
          verbose = TRUE)

################################################################################
# Generación de n copias en un solo archivo .docx

# NO establecer semilla fija - cada versión usará semilla diferente
exams2pandoc(rep(archivo_examen, numpreg),
             n = copias,
             name = nombre_arch,
             encoding = "UTF-8",
             template = "pcielo.tex",
             header = list(Date = Sys.Date()),
             inputs = NULL,
             options = NULL,
             quiet = TRUE, # Consider removing or setting to FALSE if verbose is TRUE
             resolution = 100,
             width = 4,
             height = 4,
             svg = TRUE,
             dir = dir_salida,
             edir = dir_ejercicios,
             tdir = NULL,
             sdir = NULL,
             verbose = TRUE, # Added verbose
             points = NULL,
             exshuffle = NULL,
             type = "docx")

################################################################################
# Generación para Moodle, solo configura manualmente 'copias'
# no importa 'numpreg'

# set.seed(semilla)
# exams2moodle(archivo_examen,
#              n = copias,
#              svg = TRUE,
#              name = nombre_arch,
#              encoding = "UTF-8",
#              dir = "salida",
#              edir = "ejercicios",
#              mchoice = list(shuffle = TRUE,
#                             answernumbering = "ABCD",
#                             eval = list(partial = TRUE,
#                                         rule = "none")),
#              verbose = TRUE)

################################################################################
# Generación para NOPS (exámenes escaneables)

# NO establecer semilla fija - cada versión usará semilla diferente
exams2nops(rep(archivo_examen, numpreg),
           n = copias,
           name = paste0(nombre_sin_extension, "_nops_"),
           encoding = "UTF-8",
           dir = dir_salida,
           edir = dir_ejercicios,
           language = "es",                      # Idioma español
           title = "Evaluación de Matemáticas",  # Título del examen
           institution = "I. E. Pedacito de Cielo", # Nombre de la institución
           logo = NULL,                         # Sin logo (opcional)
           date = Sys.Date(),                   # Fecha actual
           replacement = FALSE,                 # Sin preguntas de reemplazo
           blank = 0,                           # Sin páginas adicionales
           duplex = TRUE,                       # Impresión a doble cara
           pages = NULL,                        # Número de páginas automático
           points = NULL,                       # Puntos por pregunta automático
           showpoints = FALSE,                  # No mostrar puntos en el examen
           verbose = TRUE)

################################################################################
# Generación para exams2forms (formularios HTML interactivos)
# NOTA: Requiere instalar el paquete exams2forms (ejecutar solo una vez)
# install.packages("exams2forms")

library(exams2forms)

# Generar archivos HTML standalone con exams2webquiz
# Esta función genera automáticamente los archivos CSS y JS necesarios
# Configuración: 1 pregunta por página, múltiples versiones
exams2webquiz(archivo_examen,  # Una pregunta por archivo HTML
              n = copias * numpreg,  # Total de versiones = copias × preguntas
              dir = dir_salida,
              name = paste0(nombre_sin_extension, "_interactivo"),
              edir = dir_ejercicios,
              encoding = "UTF-8",
              title = "Evaluación Interactiva de Matemáticas ICFES",
              solution = TRUE,        # Mostrar botón de solución
              shuffle = TRUE,         # Mezclar opciones de respuesta
              mathjax = TRUE,         # Habilitar MathJax para fórmulas
              browse = FALSE)         # No abrir navegador (para ejecución en terminal)

# # Opción 2: Generar archivos HTML en subdirectorio para embeber
# # Útil para integrar en documentos Rmd/Quarto o sitios web
# dir_forms_embed <- file.path(dir_salida, "forms_embed")
# dir.create(dir_forms_embed, recursive = TRUE, showWarnings = FALSE)
#
# exams2forms(archivo_examen,
#             n = 3,  # Generar 3 variaciones
#             dir = dir_forms_embed,
#             edir = dir_ejercicios,
#             encoding = "UTF-8",
#             title = "Ejercicio Interactivo",
#             verbose = TRUE,
#             solution = TRUE,
#             shuffle = TRUE,
#             mathjax = TRUE)
#
# # Copiar archivos CSS y JS al subdirectorio
# file.copy(system.file("webex.css", package = "exams2forms"),
#           file.path(dir_forms_embed, "webex.css"), overwrite = TRUE)
# file.copy(system.file("webex.js", package = "exams2forms"),
#           file.path(dir_forms_embed, "webex.js"), overwrite = TRUE)

################################################################################
# Generación de PDF con 10 versiones para GitHub Pages
# Este PDF se publica en docs/recursos/ para demostración pública

cat("\n================================================================================\n")
cat("GENERANDO PDF CON 10 VERSIONES PARA GITHUB PAGES\n")
cat("================================================================================\n\n")

dir_github_pages <- "../../../docs/recursos"

# Crear directorio si no existe
if (!dir.exists(dir_github_pages)) {
  dir.create(dir_github_pages, recursive = TRUE)
  cat("✓ Directorio creado:", dir_github_pages, "\n\n")
}

# Eliminar PDFs antiguos para evitar conflictos
archivos_viejos <- list.files(dir_github_pages,
                              pattern = "muestra_10_versiones_consumo_telefonico.*\\.pdf$",
                              full.names = TRUE)
if (length(archivos_viejos) > 0) {
  file.remove(archivos_viejos)
  cat("✓ Eliminados", length(archivos_viejos), "PDFs antiguos\n\n")
}

cat("Generando 10 versiones individuales en PDF...\n")

# Generar 10 PDFs individuales
exams2pdf(
  archivo_examen,
  n = 10,
  name = "muestra_10_versiones_consumo_telefonico",
  dir = dir_github_pages,
  edir = dir_ejercicios,
  encoding = "UTF-8",
  template = "plain.tex",
  solution = TRUE,
  verbose = FALSE
)

cat("✓ 10 PDFs individuales generados\n\n")

# Combinar todos los PDFs en uno solo
cat("Combinando PDFs en un solo archivo...\n")

archivos_pdf <- list.files(dir_github_pages,
                           pattern = "muestra_10_versiones_consumo_telefonico\\d+\\.pdf$",
                           full.names = TRUE)
archivos_pdf <- sort(archivos_pdf)

if (length(archivos_pdf) == 10) {
  # Usar pdfunite para combinar
  archivo_final <- file.path(dir_github_pages, "muestra_10_versiones_consumo_telefonico1.pdf")

  comando <- paste("pdfunite",
                   paste(archivos_pdf, collapse = " "),
                   archivo_final)

  system(comando)

  # Eliminar archivos individuales
  file.remove(archivos_pdf)

  cat("✅ PDF combinado exitosamente!\n")
  cat("📁 Ubicación:", archivo_final, "\n")

  # Verificar número de páginas
  info_cmd <- paste("pdfinfo", shQuote(archivo_final), "| grep Pages")
  cat("\n")
  system(info_cmd)
  cat("\n")
} else {
  cat("⚠️ Advertencia: Se esperaban 10 PDFs pero se encontraron", length(archivos_pdf), "\n")
}

cat("================================================================================\n\n")

################################################################################
