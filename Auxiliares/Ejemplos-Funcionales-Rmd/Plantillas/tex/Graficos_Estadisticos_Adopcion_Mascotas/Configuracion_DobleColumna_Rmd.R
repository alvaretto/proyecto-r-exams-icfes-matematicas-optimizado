# ===============================================================================
# CONFIGURACIÓN PARA ADAPTAR ARCHIVOS .RMD A DOBLE COLUMNA
# ===============================================================================
# Este script ayuda a adaptar archivos .Rmd existentes para que funcionen
# correctamente con el formato de doble columna legal
# ===============================================================================

# ===============================================================================
# FUNCIÓN PARA ADAPTAR ARCHIVOS .RMD
# ===============================================================================

adaptar_rmd_doble_columna <- function(archivo_rmd, crear_backup = TRUE) {
  
  cat("🔄 Adaptando archivo:", archivo_rmd, "\n")
  
  # Verificar que el archivo existe
  if (!file.exists(archivo_rmd)) {
    stop("❌ Error: El archivo ", archivo_rmd, " no existe.")
  }
  
  # Crear backup si se solicita
  if (crear_backup) {
    archivo_backup <- paste0(archivo_rmd, ".backup")
    file.copy(archivo_rmd, archivo_backup, overwrite = TRUE)
    cat("💾 Backup creado:", archivo_backup, "\n")
  }
  
  # Leer el contenido del archivo
  contenido <- readLines(archivo_rmd, encoding = "UTF-8")
  
  # Configuración de doble columna para agregar
  config_doble_columna <- c(
    "# Configuración para doble columna legal",
    "knitr::opts_chunk$set(",
    "  fig.width = 3.4,           # Ancho optimizado para columnas", 
    "  fig.height = 2.5,          # Altura recomendada",
    "  dpi = 300,                 # Alta resolución para impresión",
    "  out.width = \"\\\\columnwidth\", # Ajustar al ancho de columna",
    "  fig.align = \"center\",      # Centrar gráficos",
    "  echo = FALSE,              # No mostrar código R",
    "  warning = FALSE,           # No mostrar advertencias", 
    "  message = FALSE            # No mostrar mensajes",
    ")"
  )
  
  # Buscar el primer chunk de R
  primer_chunk <- which(grepl("^```\\{r", contenido))[1]
  
  if (is.na(primer_chunk)) {
    cat("⚠️  Advertencia: No se encontró ningún chunk de R. Agregando configuración al inicio.\n")
    # Agregar al inicio del archivo
    contenido_nuevo <- c("```{r setup, include=FALSE}", config_doble_columna, "```", "", contenido)
  } else {
    # Buscar el final del primer chunk
    fin_chunk <- which(grepl("^```$", contenido[primer_chunk:length(contenido)]))[1] + primer_chunk - 1
    
    # Insertar la configuración después de la línea del chunk
    contenido_nuevo <- c(
      contenido[1:primer_chunk],
      config_doble_columna,
      contenido[(primer_chunk + 1):length(contenido)]
    )
  }
  
  # Comentar chunks de Python si existen
  contenido_nuevo <- gsub("^```\\{python", "# ```{python", contenido_nuevo)
  contenido_nuevo <- gsub("^library\\(reticulate\\)", "# library(reticulate)", contenido_nuevo)
  
  # Escribir el archivo modificado
  writeLines(contenido_nuevo, archivo_rmd, useBytes = TRUE)
  
  cat("✅ Archivo adaptado exitosamente\n")
  cat("📝 Cambios realizados:\n")
  cat("   • Configuración de gráficos para doble columna agregada\n")
  cat("   • Chunks de Python comentados (si existían)\n")
  cat("   • Configuración de resolución optimizada\n")
  
  return(TRUE)
}

# ===============================================================================
# FUNCIÓN PARA VERIFICAR PLANTILLAS
# ===============================================================================

verificar_plantillas <- function() {
  
  cat("🔍 Verificando plantillas de doble columna...\n")
  
  # Directorio de plantillas
  dir_plantillas <- "Auxiliares/Ejemplos-Funcionales-Rmd/Plantillas/tex/Graficos_Estadisticos_Adopcion_Mascotas"
  
  # Plantillas requeridas
  plantillas_requeridas <- c(
    "plain_legal_2col.tex",
    "pandoc_legal_2col.tex", 
    "pcielo_legal_2col.tex"
  )
  
  plantillas_encontradas <- c()
  plantillas_faltantes <- c()
  
  for (plantilla in plantillas_requeridas) {
    ruta_completa <- file.path(dir_plantillas, plantilla)
    if (file.exists(ruta_completa)) {
      plantillas_encontradas <- c(plantillas_encontradas, plantilla)
      cat("✅", plantilla, "- Encontrada\n")
    } else {
      plantillas_faltantes <- c(plantillas_faltantes, plantilla)
      cat("❌", plantilla, "- No encontrada\n")
    }
  }
  
  if (length(plantillas_faltantes) > 0) {
    cat("⚠️  Advertencia: Algunas plantillas no están disponibles.\n")
    cat("   El sistema usará plantillas estándar como alternativa.\n")
  } else {
    cat("🎉 Todas las plantillas están disponibles!\n")
  }
  
  return(list(
    encontradas = plantillas_encontradas,
    faltantes = plantillas_faltantes
  ))
}

# ===============================================================================
# FUNCIÓN PARA COPIAR PLANTILLAS AL DIRECTORIO ACTUAL
# ===============================================================================

copiar_plantillas <- function() {
  
  cat("📋 Copiando plantillas al directorio actual...\n")
  
  # Directorio de plantillas
  dir_plantillas <- "Auxiliares/Ejemplos-Funcionales-Rmd/Plantillas/tex/Graficos_Estadisticos_Adopcion_Mascotas"
  
  # Plantillas a copiar
  plantillas <- c(
    "plain_legal_2col.tex",
    "pandoc_legal_2col.tex",
    "pcielo_legal_2col.tex"
  )
  
  copiadas <- 0
  
  for (plantilla in plantillas) {
    ruta_origen <- file.path(dir_plantillas, plantilla)
    ruta_destino <- plantilla
    
    if (file.exists(ruta_origen)) {
      file.copy(ruta_origen, ruta_destino, overwrite = TRUE)
      cat("✅ Copiada:", plantilla, "\n")
      copiadas <- copiadas + 1
    } else {
      cat("❌ No encontrada:", plantilla, "\n")
    }
  }
  
  cat("📊 Total copiadas:", copiadas, "de", length(plantillas), "\n")
  
  return(copiadas)
}

# ===============================================================================
# EJEMPLO DE USO
# ===============================================================================

cat("===============================================================================\n")
cat("CONFIGURACIÓN PARA DOBLE COLUMNA - EJEMPLOS DE USO\n")
cat("===============================================================================\n")
cat("Para adaptar un archivo .Rmd:\n")
cat("  adaptar_rmd_doble_columna('mi_archivo.Rmd')\n")
cat("\n")
cat("Para verificar plantillas disponibles:\n")
cat("  verificar_plantillas()\n")
cat("\n")
cat("Para copiar plantillas al directorio actual:\n")
cat("  copiar_plantillas()\n")
cat("===============================================================================\n")

# ===============================================================================
# CONFIGURACIÓN MANUAL PARA AGREGAR A ARCHIVOS .RMD
# ===============================================================================

cat("📝 CONFIGURACIÓN MANUAL PARA ARCHIVOS .RMD:\n")
cat("Agregar al inicio del primer chunk de R:\n\n")
cat("```{r setup, include=FALSE}\n")
cat("# Configuración para doble columna legal\n")
cat("knitr::opts_chunk$set(\n")
cat("  fig.width = 3.4,           # Ancho optimizado para columnas\n") 
cat("  fig.height = 2.5,          # Altura recomendada\n")
cat("  dpi = 300,                 # Alta resolución para impresión\n")
cat("  out.width = \"\\\\columnwidth\", # Ajustar al ancho de columna\n")
cat("  fig.align = \"center\",      # Centrar gráficos\n")
cat("  echo = FALSE,              # No mostrar código R\n")
cat("  warning = FALSE,           # No mostrar advertencias\n")
cat("  message = FALSE            # No mostrar mensajes\n")
cat(")\n")
cat("```\n")
cat("===============================================================================\n")
