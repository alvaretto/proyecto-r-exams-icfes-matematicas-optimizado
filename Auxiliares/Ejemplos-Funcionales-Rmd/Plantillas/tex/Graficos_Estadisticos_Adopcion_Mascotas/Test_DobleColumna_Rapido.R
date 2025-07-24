# ===============================================================================
# PRUEBA RÁPIDA - SISTEMA DOBLE COLUMNA LEGAL
# ===============================================================================
# Script para probar rápidamente el sistema de doble columna
# Genera una sola copia de cada formato para verificar funcionamiento
# ===============================================================================

# Carga de la librería r-exams
library(exams)

# Configurar modo generación de exámenes
.exams_generation_mode <- TRUE

cat("===============================================================================\n")
cat("🧪 PRUEBA RÁPIDA - SISTEMA DOBLE COLUMNA LEGAL\n")
cat("===============================================================================\n")

# ===============================================================================
# CONFIGURACIÓN DE PRUEBA
# ===============================================================================

archivo_examen <- "ejemplo_doble_columna.Rmd"
copias <- 1  # Solo una copia para prueba rápida
numpreg <- 1 # Solo una pregunta para prueba rápida
semilla <- 12345
set.seed(semilla)
dir_salida <- "prueba_doble_columna"
dir_ejercicios <- "."

# Crear directorio de salida
if (!dir.exists(dir_salida)) {
  dir.create(dir_salida, recursive = TRUE)
}

# Verificar que el archivo de ejemplo existe
if (!file.exists(archivo_examen)) {
  # Buscar en el directorio de plantillas
  ruta_ejemplo <- file.path("Auxiliares/Ejemplos-Funcionales-Rmd/Plantillas/tex/Graficos_Estadisticos_Adopcion_Mascotas", archivo_examen)
  if (file.exists(ruta_ejemplo)) {
    file.copy(ruta_ejemplo, archivo_examen, overwrite = TRUE)
    cat("✅ Archivo de ejemplo copiado\n")
  } else {
    stop("❌ Error: No se encuentra el archivo de ejemplo")
  }
}

nombre_arch <- "prueba_legal2col_"

# ===============================================================================
# CONFIGURACIÓN DE GRÁFICOS
# ===============================================================================

knitr::opts_chunk$set(
  fig.width = 3.4,
  fig.height = 2.5,
  dpi = 300,
  out.width = "\\columnwidth",
  fig.align = "center",
  echo = FALSE,
  warning = FALSE,
  message = FALSE
)

# ===============================================================================
# COPIAR PLANTILLAS
# ===============================================================================

cat("🔄 Copiando plantillas...\n")

ruta_plantillas <- "Auxiliares/Ejemplos-Funcionales-Rmd/Plantillas/tex/Graficos_Estadisticos_Adopcion_Mascotas"
plantillas <- c("plain_legal_2col.tex", "pandoc_legal_2col.tex", "pcielo_legal_2col.tex")

for (plantilla in plantillas) {
  ruta_origen <- file.path(ruta_plantillas, plantilla)
  if (file.exists(ruta_origen)) {
    file.copy(ruta_origen, plantilla, overwrite = TRUE)
    cat("✅", plantilla, "copiada\n")
  } else {
    cat("⚠️ ", plantilla, "no encontrada\n")
  }
}

# ===============================================================================
# PRUEBA 1: PDF
# ===============================================================================

cat("\n🔄 Probando PDF...\n")

tryCatch({
  exams2pdf(rep(archivo_examen, numpreg),
            n = copias,
            name = paste0(nombre_arch, "pdf_"),
            encoding = "UTF-8",
            template = if(file.exists("plain_legal_2col.tex")) "plain_legal_2col.tex" else "plain",
            dir = dir_salida,
            edir = dir_ejercicios,
            verbose = FALSE)
  cat("✅ PDF generado exitosamente\n")
}, error = function(e) {
  cat("❌ Error en PDF:", e$message, "\n")
})

# ===============================================================================
# PRUEBA 2: NOPS
# ===============================================================================

cat("\n🔄 Probando NOPS...\n")

tryCatch({
  set.seed(semilla)
  exams2nops(rep(archivo_examen, numpreg),
             n = copias,
             name = paste0(nombre_arch, "nops_"),
             encoding = "UTF-8",
             dir = dir_salida,
             edir = dir_ejercicios,
             language = "es",
             title = "Prueba Doble Columna",
             institution = "I. E. Pedacito de Cielo",
             logo = NULL,
             date = Sys.Date(),
             replacement = FALSE,
             blank = 0,
             duplex = TRUE,
             pages = NULL,
             points = NULL,
             showpoints = FALSE,
             verbose = FALSE)
  cat("✅ NOPS generado exitosamente\n")
}, error = function(e) {
  cat("❌ Error en NOPS:", e$message, "\n")
})

# ===============================================================================
# PRUEBA 3: PANDOC
# ===============================================================================

cat("\n🔄 Probando Pandoc...\n")

tryCatch({
  exams2pandoc(rep(archivo_examen, numpreg),
               n = copias,
               name = paste0(nombre_arch, "docx_"),
               encoding = "UTF-8",
               template = if(file.exists("pandoc_legal_2col.tex")) "pandoc_legal_2col.tex" else "plain.tex",
               header = list(Date = Sys.Date()),
               quiet = TRUE,
               resolution = 300,
               width = 3.4,
               height = 2.5,
               svg = TRUE,
               dir = dir_salida,
               edir = dir_ejercicios,
               verbose = FALSE,
               type = "docx")
  cat("✅ Pandoc generado exitosamente\n")
}, error = function(e) {
  cat("❌ Error en Pandoc:", e$message, "\n")
})

# ===============================================================================
# RESUMEN
# ===============================================================================

cat("\n===============================================================================\n")
cat("🎉 PRUEBA COMPLETADA\n")
cat("===============================================================================\n")
cat("📁 Directorio de salida:", dir_salida, "\n")
cat("📄 Archivos generados:\n")

# Listar archivos generados
archivos_generados <- list.files(dir_salida, full.names = FALSE)
if (length(archivos_generados) > 0) {
  for (archivo in archivos_generados) {
    cat("   •", archivo, "\n")
  }
} else {
  cat("   ⚠️  No se generaron archivos\n")
}

cat("===============================================================================\n")
cat("📝 Para usar el sistema completo, ejecutar:\n")
cat("   source('SemilleroUnico_v2_DobleColumna.R')\n")
cat("===============================================================================\n")
