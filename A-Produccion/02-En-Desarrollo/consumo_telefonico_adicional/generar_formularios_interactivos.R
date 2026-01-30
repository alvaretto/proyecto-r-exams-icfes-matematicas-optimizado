################################################################################
# Script para generar formularios HTML interactivos con exams2forms
# Archivo: generar_formularios_interactivos.R
# Descripción: Genera formularios HTML interactivos a partir de ejercicios R/exams
################################################################################

# Limpiar entorno
rm(list = ls())

################################################################################
# 1. INSTALACIÓN Y CARGA DE PAQUETES
################################################################################

# Verificar e instalar exams2forms si no está instalado
if (!require("exams2forms")) {
  cat("Instalando paquete exams2forms...\n")
  install.packages("exams2forms")
}

# Cargar paquetes necesarios
library(exams2forms)
library(exams)

cat("✓ Paquetes cargados correctamente\n\n")

################################################################################
# 2. CONFIGURACIÓN DE RUTAS Y PARÁMETROS
################################################################################

# Archivo del ejercicio (sin extensión)
nombre_ejercicio <- "consumo_telefonico_adicional_n2_v1"
archivo_examen <- paste0(nombre_ejercicio, ".Rmd")

# Directorio de salida (mismo que otros formatos)
dir_salida <- "Salida"

# Crear directorio de salida si no existe
if (!dir.exists(dir_salida)) {
  dir.create(dir_salida, recursive = TRUE)
  cat("✓ Directorio de salida creado:", dir_salida, "\n")
}

# Número de variaciones a generar
num_variaciones <- 5  # Cambia este número para generar más o menos versiones

cat("✓ Configuración establecida\n")
cat("  - Ejercicio:", archivo_examen, "\n")
cat("  - Variaciones:", num_variaciones, "\n")
cat("  - Directorio salida:", dir_salida, "\n\n")

################################################################################
# 3. NOTA SOBRE ARCHIVOS CSS Y JS
################################################################################

cat("NOTA: exams2webquiz() genera automáticamente los archivos CSS y JS necesarios.\n")
cat("No es necesario copiarlos manualmente.\n\n")

################################################################################
# 4. GENERAR FORMULARIOS HTML INTERACTIVOS
################################################################################

cat("Generando formularios HTML interactivos...\n\n")

# Verificar que el archivo del ejercicio existe
if (!file.exists(archivo_examen)) {
  stop("Error: No se encuentra el archivo ", archivo_examen)
}

# Generar formularios HTML standalone usando exams2webquiz
tryCatch({
  cat("Usando exams2webquiz() para generar archivos HTML standalone...\n\n")

  # exams2webquiz genera archivos HTML completos automáticamente
  exams2webquiz(archivo_examen,
                n = num_variaciones,
                dir = dir_salida,
                name = paste0(nombre_ejercicio, "_"),
                edir = ".",
                encoding = "UTF-8",
                title = "Evaluación Interactiva de Matemáticas ICFES",
                solution = TRUE,
                shuffle = TRUE,
                mathjax = TRUE,
                browse = FALSE)  # No abrir navegador automáticamente

  cat("\n✓ Formularios generados exitosamente\n\n")

}, error = function(e) {
  cat("\n✗ Error al generar formularios:\n")
  cat("  ", conditionMessage(e), "\n\n")
})

################################################################################
# 5. RESUMEN DE ARCHIVOS GENERADOS
################################################################################

cat("═══════════════════════════════════════════════════════════════\n")
cat("RESUMEN DE ARCHIVOS GENERADOS\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Listar archivos HTML generados
archivos_html <- list.files(dir_salida, pattern = "\\.html$", full.names = FALSE)

if (length(archivos_html) > 0) {
  cat("Formularios HTML generados:\n")
  for (archivo in archivos_html) {
    cat("  ✓", archivo, "\n")
  }
  cat("\n")
} else {
  cat("  ✗ No se generaron archivos HTML\n\n")
}

# Listar archivos de soporte
cat("Archivos de soporte:\n")
cat("  ✓ webex.css\n")
cat("  ✓ webex.js\n\n")

cat("═══════════════════════════════════════════════════════════════\n")
cat("INSTRUCCIONES DE USO\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("1. Abre cualquiera de los archivos HTML en tu navegador web\n")
cat("2. Los formularios funcionan completamente offline\n")
cat("3. Características disponibles:\n")
cat("   - Botón ✓ : Verificar respuesta\n")
cat("   - Botón ? : Ver solución completa\n")
cat("   - Botón ↺ : Cambiar a siguiente variación\n\n")

cat("4. Para compartir:\n")
cat("   - Copia toda la carpeta '", dir_salida, "' (incluye CSS y JS)\n", sep = "")
cat("   - Los archivos HTML necesitan webex.css y webex.js en el mismo directorio\n\n")

cat("═══════════════════════════════════════════════════════════════\n\n")

# Abrir el primer formulario en el navegador (opcional)
if (length(archivos_html) > 0) {
  cat("¿Deseas abrir el primer formulario en el navegador? (s/n): ")
  respuesta <- readline()
  
  if (tolower(respuesta) == "s") {
    browseURL(file.path(dir_salida, archivos_html[1]))
    cat("\n✓ Formulario abierto en el navegador\n")
  }
}

cat("\n✓ Proceso completado exitosamente\n")

