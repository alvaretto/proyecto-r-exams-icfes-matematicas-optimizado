# Script de prueba para verificar las correcciones de tablas en formato legal
library(exams)

# Configurar modo generación de exámenes
.exams_generation_mode <- TRUE

# Configuración de prueba
archivo_examen <- "empaques_tetra_pak_argumentacion_n3_v1.Rmd"
copias <- 1
numpreg <- 2  # Reducido para pruebas rápidas
semilla <- 12345
set.seed(semilla)
dir_salida <- "test_salida"
dir_ejercicios <- "."

# Crear directorio de salida si no existe
if (!dir.exists(dir_salida)) {
  dir.create(dir_salida)
}

nombre_sin_extension <- sub("\\.Rmd$", "", archivo_examen)

cat("=== INICIANDO PRUEBAS DE FORMATO LEGAL CON TABLAS ===\n\n")

################################################################################
# PRUEBA 1: PDF con plantilla estándar oficio
cat("PRUEBA 1: Generando PDF con oficio_solpcielo.tex...\n")

tryCatch({
  exams2pdf(rep(archivo_examen, numpreg),
            n = copias,
            name = paste0(nombre_sin_extension, "_test_estandar_"),
            encoding = "UTF-8",
            template = "oficio_solpcielo",
            dir = dir_salida,
            edir = dir_ejercicios,
            verbose = TRUE)
  cat("✅ PRUEBA 1 COMPLETADA: PDF estándar generado\n\n")
}, error = function(e) {
  cat("❌ PRUEBA 1 FALLÓ:", e$message, "\n\n")
})

################################################################################
# PRUEBA 2: PDF con plantilla especializada para tablas
cat("PRUEBA 2: Generando PDF con oficio_solpcielo_tablas.tex...\n")

tryCatch({
  exams2pdf(rep(archivo_examen, numpreg),
            n = copias,
            name = paste0(nombre_sin_extension, "_test_tablas_"),
            encoding = "UTF-8",
            template = "oficio_solpcielo_tablas",
            dir = dir_salida,
            edir = dir_ejercicios,
            verbose = TRUE)
  cat("✅ PRUEBA 2 COMPLETADA: PDF con tablas optimizadas generado\n\n")
}, error = function(e) {
  cat("❌ PRUEBA 2 FALLÓ:", e$message, "\n\n")
})

################################################################################
# PRUEBA 3: Pandoc con plantilla específica
cat("PRUEBA 3: Generando DOCX con oficio_pcielo_pandoc.tex...\n")

tryCatch({
  exams2pandoc(rep(archivo_examen, numpreg),
               n = copias,
               name = paste0(nombre_sin_extension, "_test_pandoc_"),
               encoding = "UTF-8",
               template = "oficio_pcielo_pandoc.tex",
               header = list(Date = Sys.Date()),
               resolution = 100,
               width = 3.0,
               height = 3.0,
               svg = TRUE,
               dir = dir_salida,
               edir = dir_ejercicios,
               verbose = TRUE,
               type = "docx")
  cat("✅ PRUEBA 3 COMPLETADA: DOCX con dos columnas generado\n\n")
}, error = function(e) {
  cat("❌ PRUEBA 3 FALLÓ:", e$message, "\n\n")
})

################################################################################
# PRUEBA 4: Simulación NOPS con formato legal
cat("PRUEBA 4: Generando simulación NOPS con nops_oficio.tex...\n")

tryCatch({
  exams2pdf(rep(archivo_examen, numpreg),
            n = copias,
            name = paste0(nombre_sin_extension, "_test_nops_"),
            encoding = "UTF-8",
            template = "nops_oficio",
            dir = dir_salida,
            edir = dir_ejercicios,
            verbose = TRUE)
  cat("✅ PRUEBA 4 COMPLETADA: Simulación NOPS legal generada\n\n")
}, error = function(e) {
  cat("❌ PRUEBA 4 FALLÓ:", e$message, "\n\n")
})

################################################################################
# PRUEBA 5: HTML para comparación
cat("PRUEBA 5: Generando HTML para comparación...\n")

tryCatch({
  exams2html(rep(archivo_examen, numpreg),
             svg = FALSE,
             verbose = TRUE,
             template = "plain",
             name = paste0(nombre_sin_extension, "_test_html"),
             dir = dir_salida)
  cat("✅ PRUEBA 5 COMPLETADA: HTML generado\n\n")
}, error = function(e) {
  cat("❌ PRUEBA 5 FALLÓ:", e$message, "\n\n")
})

################################################################################
# Resumen de archivos generados
cat("=== RESUMEN DE ARCHIVOS GENERADOS ===\n")
archivos_generados <- list.files(dir_salida, pattern = "_test_", full.names = FALSE)
if (length(archivos_generados) > 0) {
  cat("Archivos en", dir_salida, ":\n")
  for (archivo in archivos_generados) {
    cat("  -", archivo, "\n")
  }
} else {
  cat("No se generaron archivos de prueba.\n")
}

cat("\n=== INSTRUCCIONES DE VERIFICACIÓN ===\n")
cat("1. Revisar los archivos PDF para verificar:\n")
cat("   - Formato legal (8.5\" x 14\")\n")
cat("   - Dos columnas con separador visible\n")
cat("   - Tablas adaptadas al ancho de columna\n")
cat("   - Imágenes centradas y redimensionadas\n\n")

cat("2. Comparar entre las diferentes plantillas:\n")
cat("   - _test_estandar_: Plantilla estándar\n")
cat("   - _test_tablas_: Optimizada para tablas\n")
cat("   - _test_pandoc_: Para DOCX con dos columnas\n")
cat("   - _test_nops_: Simulación NOPS legal\n\n")

cat("3. Si las tablas aún no se adaptan:\n")
cat("   - Usar la plantilla _tablas_ \n")
cat("   - Verificar que adjustbox esté instalado\n")
cat("   - Considerar dividir tablas grandes\n\n")

cat("=== PRUEBAS COMPLETADAS ===\n")
