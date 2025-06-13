# ===============================================================================
# APLICADOR AUTOMÁTICO DE CORRECCIONES A ARCHIVOS .RMD EXISTENTES
# ===============================================================================
# Archivo: aplicador_automatico.R
# Propósito: Aplicar el sistema de corrección a archivos .Rmd existentes
# Uso: source("Auxiliares/sistema_correccion_global/aplicador_automatico.R")
# ===============================================================================

# Cargar el sistema de corrección
source("Auxiliares/sistema_correccion_global/funciones_correccion.R")

#' Aplicar correcciones automáticas a un archivo .Rmd
#' @param ruta_archivo Ruta al archivo .Rmd
#' @param crear_backup Si crear backup del archivo original
#' @return TRUE si se aplicaron correcciones, FALSE si no
aplicar_correcciones_a_archivo <- function(ruta_archivo, crear_backup = TRUE) {
  
  if (!file.exists(ruta_archivo)) {
    cat("❌ Error: Archivo no encontrado:", ruta_archivo, "\n")
    return(FALSE)
  }
  
  cat("🔧 Procesando archivo:", ruta_archivo, "\n")
  
  # Crear backup si se solicita
  if (crear_backup) {
    backup_path <- paste0(ruta_archivo, ".backup.", format(Sys.time(), "%Y%m%d_%H%M%S"))
    file.copy(ruta_archivo, backup_path)
    cat("💾 Backup creado:", backup_path, "\n")
  }
  
  # Leer el archivo
  contenido <- readLines(ruta_archivo, warn = FALSE, encoding = "UTF-8")
  contenido_original <- contenido
  
  # Aplicar correcciones línea por línea
  correcciones_aplicadas <- 0
  
  for (i in seq_along(contenido)) {
    linea_original <- contenido[i]
    linea_corregida <- aplicar_correcciones_completas(linea_original)
    
    if (linea_original != linea_corregida) {
      contenido[i] <- linea_corregida
      correcciones_aplicadas <- correcciones_aplicadas + 1
      cat("✏️  Línea", i, "corregida\n")
      cat("   Antes: ", linea_original, "\n")
      cat("   Después:", linea_corregida, "\n\n")
    }
  }
  
  # Verificar si se necesita agregar el sistema de corrección embebido
  necesita_sistema <- !any(str_detect(contenido, "corregir_concordancia_genero|SISTEMA DE CORRECCIÓN EMBEBIDO"))

  if (necesita_sistema) {
    # Encontrar la posición después de las librerías
    pos_insercion <- which(str_detect(contenido, "library\\(|require\\("))
    if (length(pos_insercion) > 0) {
      pos_insercion <- max(pos_insercion) + 1
    } else {
      # Si no hay librerías, insertar después del chunk de inicio
      pos_inicio <- which(str_detect(contenido, "```\\{r.*inicio"))
      if (length(pos_inicio) > 0) {
        pos_insercion <- pos_inicio + 1
      } else {
        pos_insercion <- 1
      }
    }

    # Insertar el sistema embebido mejorado
    lineas_sistema <- c(
      "",
      "# ===============================================================================",
      "# SISTEMA DE CORRECCIÓN EMBEBIDO - VERSIÓN MEJORADA (Compatible con R-exams)",
      "# ===============================================================================",
      "",
      "# Diccionario de elementos por género",
      "elementos_genero <- list(",
      "  masculinos = c(\"vehículos\", \"hogares\", \"estudiantes\", \"centros médicos\"),",
      "  femeninos = c(\"familias\", \"empresas\", \"instituciones\", \"organizaciones\")",
      ")",
      "",
      "# Diccionario de adjetivos con formas",
      "adjetivos_formas <- list(",
      "  \"matriculado\" = list(masc = \"matriculados\", fem = \"matriculadas\"),",
      "  \"registrado\" = list(masc = \"registrados\", fem = \"registradas\"),",
      "  \"certificado\" = list(masc = \"certificados\", fem = \"certificadas\"),",
      "  \"beneficiario\" = list(masc = \"beneficiarios\", fem = \"beneficiarias\"),",
      "  \"asegurado\" = list(masc = \"asegurados\", fem = \"aseguradas\")",
      ")",
      "",
      "# Función para obtener la forma correcta de un adjetivo",
      "obtener_forma_adjetivo <- function(elemento, adjetivo_base) {",
      "  es_femenino <- elemento %in% elementos_genero$femeninos",
      "  if (adjetivo_base %in% names(adjetivos_formas)) {",
      "    formas <- adjetivos_formas[[adjetivo_base]]",
      "    return(if (es_femenino) formas$fem else formas$masc)",
      "  }",
      "  return(adjetivo_base)",
      "}",
      "",
      "# Función para crear variables textuales contextuales",
      "crear_variables_textuales <- function(elemento, condicion) {",
      "  es_femenino <- elemento %in% elementos_genero$femeninos",
      "  variables <- list(",
      "    elemento = elemento, condicion = condicion,",
      "    registrados = obtener_forma_adjetivo(elemento, \"registrado\"),",
      "    matriculados = obtener_forma_adjetivo(elemento, \"matriculado\"),",
      "    certificados = obtener_forma_adjetivo(elemento, \"certificado\"),",
      "    beneficiarios = obtener_forma_adjetivo(elemento, \"beneficiario\"),",
      "    es_femenino = es_femenino, es_masculino = !es_femenino",
      "  )",
      "  return(variables)",
      "}",
      "",
      "# Función para validar coherencia",
      "validar_coherencia <- function(entidad, elemento, condicion, total) {",
      "  errores <- c()",
      "  if (entidad == \"ICBF\" && elemento != \"familias\") {",
      "    errores <- c(errores, \"ICBF debe manejar familias\")",
      "  }",
      "  if (total < 1000 || total > 50000000) {",
      "    errores <- c(errores, \"Total fuera de rango realista\")",
      "  }",
      "  return(errores)",
      "}",
      "",
      "# Función para aplicar correcciones a datos (VERSIÓN MEJORADA)",
      "aplicar_correcciones_datos <- function(datos) {",
      "  variables_texto <- crear_variables_textuales(datos$elemento, datos$condicion)",
      "  datos$variables_texto <- variables_texto",
      "  datos$elemento_texto <- variables_texto$elemento",
      "  datos$condicion_texto <- variables_texto$condicion",
      "  return(datos)",
      "}",
      ""
    )

    contenido <- append(contenido, lineas_sistema, after = pos_insercion)
    correcciones_aplicadas <- correcciones_aplicadas + 1
    cat("📦 Sistema de corrección embebido agregado al archivo\n")
  }
  
  # Escribir el archivo corregido
  if (correcciones_aplicadas > 0) {
    writeLines(contenido, ruta_archivo, useBytes = TRUE)
    cat("✅ Archivo actualizado con", correcciones_aplicadas, "correcciones\n")
    return(TRUE)
  } else {
    cat("ℹ️  No se encontraron errores para corregir\n")
    return(FALSE)
  }
}

#' Aplicar correcciones a todos los archivos .Rmd en un directorio
#' @param directorio Directorio a procesar
#' @param recursivo Si procesar subdirectorios
#' @param patron Patrón de archivos a procesar
#' @return Número de archivos procesados
aplicar_correcciones_masivas <- function(directorio = ".", 
                                        recursivo = TRUE, 
                                        patron = "\\.Rmd$") {
  
  cat("🔍 Buscando archivos .Rmd en:", directorio, "\n")
  
  # Encontrar todos los archivos .Rmd
  archivos <- list.files(directorio, 
                        pattern = patron, 
                        recursive = recursivo, 
                        full.names = TRUE)
  
  if (length(archivos) == 0) {
    cat("❌ No se encontraron archivos .Rmd\n")
    return(0)
  }
  
  cat("📁 Encontrados", length(archivos), "archivos .Rmd\n\n")
  
  # Procesar cada archivo
  archivos_procesados <- 0
  archivos_corregidos <- 0
  
  for (archivo in archivos) {
    cat("" %+% rep("=", 60) %+% "\n")
    
    if (aplicar_correcciones_a_archivo(archivo)) {
      archivos_corregidos <- archivos_corregidos + 1
    }
    archivos_procesados <- archivos_procesados + 1
    
    cat("\n")
  }
  
  # Resumen final
  cat("" %+% rep("=", 60) %+% "\n")
  cat("📊 RESUMEN DE PROCESAMIENTO:\n")
  cat("   Archivos procesados:", archivos_procesados, "\n")
  cat("   Archivos corregidos:", archivos_corregidos, "\n")
  cat("   Archivos sin cambios:", archivos_procesados - archivos_corregidos, "\n")
  cat("✅ Procesamiento completado\n")
  
  return(archivos_procesados)
}

#' Validar archivos .Rmd en busca de errores de concordancia
#' @param directorio Directorio a validar
#' @param recursivo Si validar subdirectorios
#' @return Lista de archivos con errores
validar_archivos_masivo <- function(directorio = ".", recursivo = TRUE) {
  
  cat("🔍 Validando archivos .Rmd en:", directorio, "\n")
  
  archivos <- list.files(directorio, 
                        pattern = "\\.Rmd$", 
                        recursive = recursivo, 
                        full.names = TRUE)
  
  archivos_con_errores <- list()
  
  for (archivo in archivos) {
    contenido <- readLines(archivo, warn = FALSE, encoding = "UTF-8")
    contenido_texto <- paste(contenido, collapse = " ")
    
    # Buscar errores comunes
    errores_encontrados <- c()
    
    # Errores de concordancia
    if (str_detect(contenido_texto, "familias matriculados|familias registrados|familias certificados")) {
      errores_encontrados <- c(errores_encontrados, "Error de concordancia con 'familias'")
    }
    
    if (str_detect(contenido_texto, "empresas matriculados|empresas registrados|empresas certificados")) {
      errores_encontrados <- c(errores_encontrados, "Error de concordancia con 'empresas'")
    }
    
    if (length(errores_encontrados) > 0) {
      archivos_con_errores[[archivo]] <- errores_encontrados
      cat("❌", archivo, ":", paste(errores_encontrados, collapse = ", "), "\n")
    }
  }
  
  if (length(archivos_con_errores) == 0) {
    cat("✅ No se encontraron errores de concordancia\n")
  } else {
    cat("📊 Total de archivos con errores:", length(archivos_con_errores), "\n")
  }
  
  return(archivos_con_errores)
}

# Operador de concatenación para strings
`%+%` <- function(a, b) paste0(a, b)

cat("🚀 Aplicador Automático de Correcciones cargado\n")
cat("📋 Funciones disponibles:\n")
cat("   - aplicar_correcciones_a_archivo(ruta_archivo)\n")
cat("   - aplicar_correcciones_masivas(directorio)\n")
cat("   - validar_archivos_masivo(directorio)\n")
cat("💡 Ejemplo de uso:\n")
cat("   aplicar_correcciones_masivas('Lab/')\n")
cat("   validar_archivos_masivo('.')\n\n")
