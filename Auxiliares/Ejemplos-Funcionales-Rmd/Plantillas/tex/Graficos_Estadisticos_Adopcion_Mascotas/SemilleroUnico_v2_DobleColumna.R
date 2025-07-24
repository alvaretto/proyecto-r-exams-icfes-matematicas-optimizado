# ===============================================================================
# SEMILLERO ÚNICO v2 - FORMATO DOBLE COLUMNA LEGAL
# ===============================================================================
# Generador de exámenes optimizado para:
# - Tamaño legal (8.5" x 14")
# - Doble columna con línea central
# - Salidas: PDF, NOPS y Pandoc únicamente
# - Gráficos adaptados a limitaciones de doble columna
# ===============================================================================

# Carga de la librería r-exams
library(exams)

# Configurar modo generación de exámenes para evitar pruebas test_that
.exams_generation_mode <- TRUE

# ===============================================================================
# CONFIGURACIÓN INICIAL
# ===============================================================================

# Definición del archivo de examen y configuración inicial
archivo_examen <- "ejemplo_doble_columna.Rmd"  # CAMBIAR POR TU ARCHIVO .Rmd
copias <- 5
numpreg <- 3
semilla <- sample(100:1e8, 1)
set.seed(semilla)
dir_salida <- "salida_doble_columna"
dir_ejercicios <- "."

# Crear directorio de salida si no existe
if (!dir.exists(dir_salida)) {
  dir.create(dir_salida, recursive = TRUE)
}

# Nombre del archivo sin la extensión .Rmd
nombre_sin_extension <- sub("\\.Rmd$", "", archivo_examen)
nombre_arch <- paste0(nombre_sin_extension, "_legal2col_")

# ===============================================================================
# CONFIGURACIÓN DE GRÁFICOS PARA DOBLE COLUMNA
# ===============================================================================

# Configurar knitr para gráficos optimizados en doble columna
knitr::opts_chunk$set(
  fig.width = 3.4,           # Ancho máximo para columnas
  fig.height = 2.5,          # Altura recomendada
  dpi = 300,                 # Alta resolución para impresión
  out.width = "\\columnwidth", # Ajustar al ancho de columna
  fig.align = "center",      # Centrar gráficos
  echo = FALSE,              # No mostrar código R
  warning = FALSE,           # No mostrar advertencias
  message = FALSE            # No mostrar mensajes
)

# ===============================================================================
# INFORMACIÓN DEL SISTEMA
# ===============================================================================

cat("===============================================================================\n")
cat("GENERADOR DE EXÁMENES - FORMATO DOBLE COLUMNA LEGAL\n")
cat("===============================================================================\n")
cat("Archivo de examen:", archivo_examen, "\n")
cat("Número de copias:", copias, "\n")
cat("Preguntas por copia:", numpreg, "\n")
cat("Semilla aleatoria:", semilla, "\n")
cat("Directorio de salida:", dir_salida, "\n")
cat("===============================================================================\n")

# ===============================================================================
# 1. GENERACIÓN PDF - FORMATO LEGAL DOBLE COLUMNA
# ===============================================================================

cat("🔄 Generando PDF con formato legal y doble columna...\n")

# Verificar si existe la plantilla personalizada
plantilla_pdf <- "plain_legal_2col.tex"
ruta_plantillas <- "Auxiliares/Ejemplos-Funcionales-Rmd/Plantillas/tex/Graficos_Estadisticos_Adopcion_Mascotas"
if (file.exists(file.path(ruta_plantillas, plantilla_pdf))) {
  # Usar plantilla personalizada si está disponible
  ruta_plantilla <- file.path(ruta_plantillas, plantilla_pdf)
  file.copy(ruta_plantilla, plantilla_pdf, overwrite = TRUE)
  cat("✅ Plantilla PDF copiada:", plantilla_pdf, "\n")
}

exams2pdf(rep(archivo_examen, numpreg),
          n = copias,
          name = paste0(nombre_arch, "pdf_"),
          encoding = "UTF-8",
          template = if(file.exists(plantilla_pdf)) plantilla_pdf else "plain",
          dir = dir_salida,
          edir = dir_ejercicios,
          verbose = TRUE)

cat("✅ PDF generado exitosamente\n")

# ===============================================================================
# 2. GENERACIÓN NOPS - EXÁMENES ESCANEABLES FORMATO LEGAL
# ===============================================================================

cat("🔄 Generando NOPS (exámenes escaneables) formato legal...\n")

set.seed(semilla)
exams2nops(rep(archivo_examen, numpreg),
           n = copias,
           name = paste0(nombre_arch, "nops_"),
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

cat("✅ NOPS generado exitosamente\n")

# ===============================================================================
# 3. GENERACIÓN PANDOC - DOCUMENTOS WORD FORMATO LEGAL DOBLE COLUMNA
# ===============================================================================

cat("🔄 Generando Pandoc (DOCX) con formato legal y doble columna...\n")

# Verificar si existe la plantilla personalizada para Pandoc
plantilla_pandoc <- "pandoc_legal_2col.tex"
if (file.exists(file.path(ruta_plantillas, plantilla_pandoc))) {
  # Usar plantilla personalizada si está disponible
  ruta_plantilla_pandoc <- file.path(ruta_plantillas, plantilla_pandoc)
  file.copy(ruta_plantilla_pandoc, plantilla_pandoc, overwrite = TRUE)
  cat("✅ Plantilla Pandoc copiada:", plantilla_pandoc, "\n")
}

exams2pandoc(rep(archivo_examen, numpreg),
             n = copias,
             name = paste0(nombre_arch, "docx_"),
             encoding = "UTF-8",
             template = if(file.exists(plantilla_pandoc)) plantilla_pandoc else "plain.tex",
             header = list(Date = Sys.Date()),
             inputs = NULL,
             options = NULL,
             quiet = FALSE,
             resolution = 300,              # Alta resolución para gráficos
             width = 3.4,                   # Ancho optimizado para doble columna
             height = 2.5,                  # Altura optimizada para doble columna
             svg = TRUE,
             dir = dir_salida,
             edir = dir_ejercicios,
             tdir = NULL,
             sdir = NULL,
             verbose = TRUE,
             points = NULL,
             exshuffle = NULL,
             type = "docx")

cat("✅ Pandoc (DOCX) generado exitosamente\n")

# ===============================================================================
# RESUMEN FINAL
# ===============================================================================

cat("===============================================================================\n")
cat("🎉 GENERACIÓN COMPLETADA EXITOSAMENTE\n")
cat("===============================================================================\n")
cat("📁 Directorio de salida:", dir_salida, "\n")
cat("📄 Archivos generados:\n")
cat("   • PDF (Legal, Doble Columna):", paste0(nombre_arch, "pdf_*.pdf"), "\n")
cat("   • NOPS (Escaneable Legal):", paste0(nombre_arch, "nops_*.pdf"), "\n")
cat("   • DOCX (Legal, Doble Columna):", paste0(nombre_arch, "docx_*.docx"), "\n")
cat("===============================================================================\n")
cat("📊 Configuración de gráficos aplicada:\n")
cat("   • Ancho: 3.4 pulgadas\n")
cat("   • Alto: 2.5 pulgadas\n")
cat("   • Resolución: 300 DPI\n")
cat("   • Ajuste: Ancho de columna\n")
cat("===============================================================================\n")

# ===============================================================================
# INSTRUCCIONES PARA ADAPTAR ARCHIVOS .RMD
# ===============================================================================

cat("📝 PARA ADAPTAR TUS ARCHIVOS .RMD:\n")
cat("1. Agregar al inicio del primer chunk de R:\n")
cat("   knitr::opts_chunk$set(\n")
cat("     fig.width = 3.4, fig.height = 2.5, dpi = 300,\n")
cat("     out.width = \"\\\\columnwidth\", fig.align = \"center\"\n")
cat("   )\n")
cat("2. Cambiar 'archivo_examen' en línea 20 por tu archivo .Rmd\n")
cat("3. Ejecutar este script\n")
cat("===============================================================================\n")
