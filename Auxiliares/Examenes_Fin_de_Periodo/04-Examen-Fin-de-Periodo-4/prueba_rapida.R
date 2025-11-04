################################################################################
# SCRIPT DE PRUEBA RÁPIDA
# Genera UNA SOLA versión del examen para verificar que todo funciona
# Útil para pruebas antes de generar las 5 versiones completas
################################################################################

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                    PRUEBA RÁPIDA - GENERACIÓN DE EXAMEN                    ║\n")
cat("║                         (Solo 1 versión de prueba)                         ║\n")
cat("╚════════════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# Carga de la librería r-exams
library(exams)

# Configuración inicial (SOLO 1 COPIA PARA PRUEBA)
copias <- 1  # Solo 1 versión para prueba rápida
numpreg_por_archivo <- 1
dir_salida <- "salida_prueba"
dir_ejercicios <- "."

# Crear directorio de salida si no existe
if (!dir.exists(dir_salida)) {
  dir.create(dir_salida, recursive = TRUE)
}

# Lista de archivos .Rmd del 001 al 015
archivo_examen <- c(
  "001-muestreo_sesgo_municipio_aleatorio_argumentacion_n2_v1.Rmd",
  "002-cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_2.Rmd",
  "003-cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd",
  "004-pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd",
  "005-estadistica_media_calificaciones_n2_v1.Rmd",
  "006-ganancias_comerciales_formulacion_ejecucion_n2_v1.Rmd",
  "007-proporcionalidad_empresarial_formulacion_ejecucion_n2_v1.Rmd",
  "008-funciones_lineales_interpretacion_grafica_v2.Rmd",
  "009-funciones_lineales_interpretacion_grafica_v1.Rmd",
  "010-empaques_tetra_pak_argumentacion_n3_v1.Rmd",
  "011-probabilidad_extraccion_bolas_v1.Rmd",
  "012-probabilidad_combinaciones_v1.Rmd",
  "013-parabrisas.Rmd",
  "014-parabrisas-2.Rmd",
  "015-volumen_cilindro_hueco_R_v1.Rmd"
)

# Verificar que todos los archivos existen
archivos_faltantes <- archivo_examen[!file.exists(archivo_examen)]
if (length(archivos_faltantes) > 0) {
  cat("\n⚠️  ADVERTENCIA: Los siguientes archivos no se encontraron:\n")
  cat(paste("  -", archivos_faltantes, collapse = "\n"), "\n")
  stop("Faltan archivos .Rmd necesarios para generar el examen.")
}

cat("✅ Todos los archivos .Rmd están disponibles\n")
cat("📊 Total de preguntas:", length(archivo_examen), "\n\n")

# Generar semilla única
semilla <- sample(100:1e8, 1)
cat("🎲 Semilla de prueba:", semilla, "\n\n")

################################################################################
# PRUEBA 1: EXAMS2PANDOC SIN SOLUCIONES
################################################################################

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("🧪 PRUEBA 1: Generando exams2pandoc SIN soluciones...\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

tryCatch({
  set.seed(semilla)
  exams2pandoc(
    rep(archivo_examen, each = numpreg_por_archivo),
    n = copias,
    name = "PRUEBA_pandoc_sin_soluciones",
    encoding = "UTF-8",
    template = "pcielo_nosol.tex",
    solution = FALSE,
    header = list(
      Date = format(Sys.Date(), "%d de %B de %Y"),
      Title = "PRUEBA - Examen Final de Periodo 4"
    ),
    quiet = FALSE,
    resolution = 150,
    width = 6,
    height = 6,
    svg = TRUE,
    dir = dir_salida,
    edir = dir_ejercicios,
    verbose = TRUE,
    type = "docx"
  )
  cat("✅ PRUEBA 1 EXITOSA\n\n")
}, error = function(e) {
  cat("❌ ERROR EN PRUEBA 1:", e$message, "\n\n")
})

################################################################################
# PRUEBA 2: EXAMS2PDF SIN SOLUCIONES
################################################################################

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("🧪 PRUEBA 2: Generando exams2pdf SIN soluciones...\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

tryCatch({
  set.seed(semilla)
  exams2pdf(
    rep(archivo_examen, each = numpreg_por_archivo),
    n = copias,
    name = "PRUEBA_pdf_sin_soluciones",
    encoding = "UTF-8",
    template = "exam",
    dir = dir_salida,
    edir = dir_ejercicios,
    verbose = TRUE,
    resolution = 150
  )
  cat("✅ PRUEBA 2 EXITOSA\n\n")
}, error = function(e) {
  cat("❌ ERROR EN PRUEBA 2:", e$message, "\n\n")
})

################################################################################
# RESUMEN
################################################################################

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                         PRUEBA RÁPIDA COMPLETADA                           ║\n")
cat("╚════════════════════════════════════════════════════════════════════════════╝\n")
cat("\n")
cat("📁 Archivos de prueba generados en: ./", dir_salida, "/\n")
cat("\n")
cat("Si las pruebas fueron exitosas, puedes ejecutar el script completo:\n")
cat("   > source('SemilleroFinDePeriodo4.R')\n")
cat("\n")
cat("Para limpiar archivos de prueba:\n")
cat("   > unlink('", dir_salida, "', recursive = TRUE)\n")
cat("\n")

