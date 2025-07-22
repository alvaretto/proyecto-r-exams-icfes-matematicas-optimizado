# ===============================================================================
# GENERADOR UNIVERSAL DE EXÁMENES R/EXAMS - TODOS LOS FORMATOS
# ===============================================================================
# Archivo: SemilleroTotal_v1.R
# Propósito: Generar exámenes en todos los formatos disponibles con verbose
#           completo e interactividad con el usuario
# Basado en: SemilleroCloze.R (mejoras de verbose e interactividad)
# Autor: Transformación Pedagógica R-Exams
# Fecha: 2025-01-22
# ===============================================================================

# Cargar librerías necesarias
library(exams)
library(tools)

# ===============================================================================
# FUNCIONES ESPECÍFICAS PARA PREGUNTAS TIPO CLOZE
# ===============================================================================

# Función de redondeo matemático correcto (hacia arriba para .5)
redondear_matematico <- function(x, digits = 0) {
  # CORRECCIÓN IMPORTANTE: R usa "redondeo bancario" donde .5 se redondea al número par más cercano
  # Ejemplo problemático: round(36.5, 0) = 36 (incorrecto matemáticamente)
  # Esta función implementa redondeo matemático estándar: .5 siempre hacia arriba
  # Ejemplos corregidos: 36.5 -> 37, 38.5 -> 39, 40.5 -> 41
  factor <- 10^digits
  return(floor(x * factor + 0.5) / factor)
}

# Función de formato estándar para números (sin separador de miles, coma decimal)
formato_estandar <- function(x, decimales = 0) {
  # FORMATO ESTÁNDAR: Sin separador de miles, coma como separador decimal
  # Ejemplos monetarios: 16850 (enteros), 16850,32 (con decimales)
  # Ejemplos numéricos: 36,5000 (con decimales)
  if (decimales == 0) {
    return(as.character(as.integer(x)))
  } else {
    # Para decimales, usar coma como separador
    resultado <- sprintf(paste0("%.", decimales, "f"), x)
    return(gsub("\\.", ",", resultado))
  }
}

# Función para detectar tipo de examen
detectar_tipo_examen <- function(archivo_path) {
  if (!file.exists(archivo_path)) {
    return("desconocido")
  }

  contenido <- readLines(archivo_path, warn = FALSE)

  # Detectar Cloze
  tiene_cloze <- any(grepl("##ANSWER", contenido))

  # Detectar Schoice
  tiene_schoice <- any(grepl("answerlist", contenido)) ||
                   any(grepl("extype.*schoice", contenido))

  # Detectar Mchoice
  tiene_mchoice <- any(grepl("extype.*mchoice", contenido))

  # Detectar Num
  tiene_num <- any(grepl("extype.*num", contenido))

  # Detectar String
  tiene_string <- any(grepl("extype.*string", contenido))

  if (tiene_cloze) {
    return("cloze")
  } else if (tiene_schoice) {
    return("schoice")
  } else if (tiene_mchoice) {
    return("mchoice")
  } else if (tiene_num) {
    return("num")
  } else if (tiene_string) {
    return("string")
  } else {
    return("desconocido")
  }
}

# Función para configurar entorno según tipo de examen
configurar_entorno_examen <- function(tipo_examen) {
  cat("🔧 Configurando entorno para tipo:", toupper(tipo_examen), "\n")

  if (tipo_examen == "cloze") {
    cat("  ✅ Funciones de redondeo matemático activadas\n")
    cat("  ✅ Funciones de formato estándar activadas\n")
    cat("  ✅ Configuración específica para Cloze\n")

    # Configurar modo generación de exámenes para Cloze
    .exams_generation_mode <<- TRUE

    # Asegurar que las funciones estén disponibles globalmente
    assign("redondear_matematico", redondear_matematico, envir = .GlobalEnv)
    assign("formato_estandar", formato_estandar, envir = .GlobalEnv)

  } else if (tipo_examen == "schoice") {
    cat("  ✅ Configuración específica para Schoice\n")
    cat("  ✅ Opciones de respuesta múltiple activadas\n")

  } else if (tipo_examen == "mchoice") {
    cat("  ✅ Configuración específica para Mchoice\n")
    cat("  ✅ Opciones de respuesta múltiple activadas\n")

  } else if (tipo_examen == "num") {
    cat("  ✅ Configuración específica para Num\n")
    cat("  ✅ Respuestas numéricas activadas\n")

    # Para preguntas numéricas, también activar funciones de formato
    assign("redondear_matematico", redondear_matematico, envir = .GlobalEnv)
    assign("formato_estandar", formato_estandar, envir = .GlobalEnv)

  } else {
    cat("  ⚠️  Tipo de examen no reconocido, usando configuración genérica\n")
  }
}

# Función para mostrar información específica del tipo de examen
mostrar_info_tipo_examen <- function(tipo_examen) {
  cat("\n📚 INFORMACIÓN DEL TIPO DE EXAMEN:\n")

  switch(tipo_examen,
    "cloze" = {
      cat("  Tipo: CLOZE (Respuesta construida)\n")
      cat("  Características:\n")
      cat("    • Múltiples campos de respuesta en una pregunta\n")
      cat("    • Soporte para respuestas numéricas y de texto\n")
      cat("    • Redondeo matemático correcto implementado\n")
      cat("    • Formato estándar de números (sin separador de miles)\n")
      cat("    • Ideal para problemas matemáticos paso a paso\n")
    },
    "schoice" = {
      cat("  Tipo: SCHOICE (Selección única)\n")
      cat("  Características:\n")
      cat("    • Una sola respuesta correcta\n")
      cat("    • Múltiples opciones de distractor\n")
      cat("    • Ideal para conceptos y definiciones\n")
      cat("    • Evaluación automática\n")
    },
    "mchoice" = {
      cat("  Tipo: MCHOICE (Selección múltiple)\n")
      cat("  Características:\n")
      cat("    • Múltiples respuestas correctas posibles\n")
      cat("    • Evaluación parcial disponible\n")
      cat("    • Ideal para preguntas complejas\n")
    },
    "num" = {
      cat("  Tipo: NUM (Respuesta numérica)\n")
      cat("  Características:\n")
      cat("    • Respuesta numérica única\n")
      cat("    • Tolerancia configurable\n")
      cat("    • Redondeo matemático implementado\n")
      cat("    • Formato estándar activado\n")
    },
    {
      cat("  Tipo: DESCONOCIDO\n")
      cat("  ⚠️  No se pudo determinar el tipo específico\n")
      cat("  💡 Usando configuración genérica\n")
    }
  )
}

# ===============================================================================
# CONFIGURACIÓN PRINCIPAL
# ===============================================================================

# Archivo de examen (se puede cambiar según necesidad)
archivo_examen <- "consumo_gas_natural_porcentaje_maximo_aleatorio_interpretacion_representacion_n2_cloze_v1.Rmd"

# Configuración por defecto
config <- list(
  archivos = 5,                    # Número de versiones por formato
  semilla = sample(100:1e8, 1),      # Semilla aleatoria
  dir_salida = "salida_universal",   # Directorio de salida
  dir_ejercicios = ".",              # Directorio de ejercicios
  encoding = "UTF-8",                # Codificación
  formatos = c("html", "moodle")     # Formatos por defecto
)

# Configurar modo generación de exámenes
.exams_generation_mode <- TRUE

# Establecer semilla
set.seed(config$semilla)
cat("🎲 Semilla aleatoria establecida:", config$semilla, "\n")

# Crear directorio de salida si no existe
if (!dir.exists(config$dir_salida)) {
  dir.create(config$dir_salida, recursive = TRUE)
  cat("📁 Directorio de salida creado:", config$dir_salida, "\n")
}

# Nombre base para archivos generados
nombre_sin_extension <- sub("\\.Rmd$", "", archivo_examen)
nombre_base <- paste0(nombre_sin_extension, "_")

# ===============================================================================
# FUNCIONES DE GENERACIÓN CON VERBOSE COMPLETO
# ===============================================================================

# Función para generar HTML
generar_html <- function() {
  cat("🌐 Generando archivos HTML...\n")
  cat("📁 Directorio de salida:", file.path(config$dir_salida, "html"), "\n")
  cat("📄 Archivo base:", archivo_examen, "\n")
  cat("🔢 Número de versiones:", config$archivos, "\n")
  cat("🎲 Semilla:", config$semilla, "\n")

  tryCatch({
    set.seed(config$semilla)
    cat("⏳ Iniciando generación HTML...\n")

    resultado <- exams2html(archivo_examen,
                           n = config$archivos,
                           name = paste0(nombre_base, "_html"),
                           dir = file.path(config$dir_salida, "html"),
                           edir = config$dir_ejercicios,
                           encoding = config$encoding,
                           verbose = TRUE)

    cat("✅ Archivos HTML generados exitosamente\n")

    # Mostrar archivos generados
    archivos_html <- list.files(file.path(config$dir_salida, "html"), pattern = "\\.html$")
    cat("📊 Archivos generados:", length(archivos_html), "\n")
    for(i in seq_along(archivos_html)) {
      cat("  ", i, ":", archivos_html[i], "\n")
    }

    return(TRUE)

  }, error = function(e) {
    cat("❌ Error generando HTML:", e$message, "\n")
    return(FALSE)
  })
}

# Función para crear versión PDF-compatible de archivos Cloze
crear_version_pdf_compatible <- function(archivo_original) {
  if (!file.exists(archivo_original)) {
    return(NULL)
  }

  contenido <- readLines(archivo_original, warn = FALSE)

  # Crear versión simplificada para PDF
  contenido_pdf <- contenido

  # Reemplazar campos ##ANSWER## con espacios en blanco
  contenido_pdf <- gsub("##ANSWER[0-9]+##", "_______________", contenido_pdf)

  # Simplificar gráficos complejos
  contenido_pdf <- gsub("ggplot.*", "# Gráfico simplificado para PDF", contenido_pdf)

  # Crear archivo temporal
  archivo_pdf <- gsub("\\.Rmd$", "_pdf_compatible.Rmd", archivo_original)
  writeLines(contenido_pdf, archivo_pdf)

  cat("📝 Versión PDF-compatible creada:", archivo_pdf, "\n")
  return(archivo_pdf)
}

# Función para generar PDF
generar_pdf <- function() {
  cat("📄 Generando archivos PDF...\n")
  cat("📁 Directorio de salida:", file.path(config$dir_salida, "pdf"), "\n")
  cat("🔢 Número de versiones:", config$archivos, "\n")
  cat("🎲 Semilla:", config$semilla, "\n")

  # Detectar tipo de examen para advertencias específicas
  tipo_examen <- detectar_tipo_examen(archivo_examen)

  if (tipo_examen == "cloze") {
    cat("⚠️  ADVERTENCIA: Archivo tipo CLOZE detectado\n")
    cat("   Los archivos Cloze pueden tener limitaciones en PDF\n")
    cat("   Recomendado: HTML o Moodle para mejor compatibilidad\n")
    cat("   ¿Desea continuar con PDF? (puede fallar o verse mal)\n")
  }

  tryCatch({
    set.seed(config$semilla)

    # Verificar disponibilidad de LaTeX
    cat("🔍 Verificando disponibilidad de LaTeX...\n")
    pdflatex_disponible <- Sys.which("pdflatex") != ""

    if (!pdflatex_disponible) {
      stop("❌ pdflatex no está disponible. Instale: sudo apt install texlive-latex-extra")
    }

    cat("✅ pdflatex disponible\n")
    cat("⏳ Iniciando generación PDF...\n")

    # Configuración mejorada para diferentes tipos de examen
    if (tipo_examen == "cloze") {
      cat("🔧 Aplicando configuración especial para Cloze...\n")

      # Para Cloze, usar configuración más robusta
      resultado <- exams2pdf(archivo_examen,
                            n = min(config$archivos, 10),  # Limitar para Cloze
                            name = paste0(nombre_base, "_pdf_cloze"),
                            dir = file.path(config$dir_salida, "pdf"),
                            edir = config$dir_ejercicios,
                            encoding = "UTF-8",
                            template = "plain",
                            verbose = TRUE,
                            converter = "pandoc",
                            base64 = FALSE)
    } else {
      # Para otros tipos, configuración estándar
      resultado <- exams2pdf(archivo_examen,
                            n = config$archivos,
                            name = paste0(nombre_base, "_pdf"),
                            dir = file.path(config$dir_salida, "pdf"),
                            edir = config$dir_ejercicios,
                            encoding = config$encoding,
                            template = "plain",
                            verbose = TRUE)
    }

    cat("✅ Archivos PDF generados exitosamente\n")

    # Mostrar archivos generados
    archivos_pdf <- list.files(file.path(config$dir_salida, "pdf"), pattern = "\\.pdf$")
    cat("📊 Archivos generados:", length(archivos_pdf), "\n")
    for(i in seq_along(archivos_pdf)) {
      cat("  ", i, ":", archivos_pdf[i], "\n")
      # Mostrar tamaño para verificar que se generó correctamente
      tamaño <- file.size(file.path(config$dir_salida, "pdf", archivos_pdf[i]))
      cat("     Tamaño:", tamaño, "bytes\n")
    }

    if (tipo_examen == "cloze") {
      cat("💡 NOTA IMPORTANTE PARA CLOZE:\n")
      cat("   Los campos ##ANSWER## aparecen como texto plano en PDF\n")
      cat("   Para funcionalidad completa de Cloze, use HTML o Moodle\n")
    }

    return(TRUE)

  }, error = function(e) {
    cat("❌ Error generando PDF:", e$message, "\n")

    if (tipo_examen == "cloze") {
      cat("💡 SOLUCIÓN PARA CLOZE:\n")
      cat("   1. Los archivos Cloze complejos no son compatibles con PDF\n")
      cat("   2. Use HTML para visualización interactiva\n")
      cat("   3. Use Moodle para evaluación automática\n")
      cat("   4. Para PDF, considere convertir a Schoice\n")
    } else {
      cat("💡 POSIBLES SOLUCIONES:\n")
      cat("   1. Verificar que LaTeX esté instalado correctamente\n")
      cat("   2. Revisar caracteres especiales en el archivo\n")
      cat("   3. Probar con un archivo más simple\n")
    }

    return(FALSE)
  })
}

# Función para generar Pandoc (DOCX)
generar_pandoc <- function() {
  cat("📝 Generando archivos Pandoc (DOCX)...\n")
  cat("📁 Directorio de salida:", file.path(config$dir_salida, "pandoc"), "\n")
  cat("📄 Archivo base:", archivo_examen, "\n")
  cat("🔢 Número de versiones:", config$archivos, "\n")
  cat("🎲 Semilla:", config$semilla, "\n")

  tryCatch({
    set.seed(config$semilla)
    cat("⏳ Iniciando generación Pandoc (DOCX)...\n")

    resultado <- exams2pandoc(archivo_examen,
                             n = config$archivos,
                             name = paste0(nombre_base, "_pandoc"),
                             dir = file.path(config$dir_salida, "pandoc"),
                             edir = config$dir_ejercicios,
                             encoding = config$encoding,
                             template = "plain.tex",
                             type = "docx",
                             verbose = TRUE)

    cat("✅ Archivos Pandoc generados exitosamente\n")

    # Mostrar archivos generados
    archivos_pandoc <- list.files(file.path(config$dir_salida, "pandoc"), pattern = "\\.docx$")
    cat("📊 Archivos generados:", length(archivos_pandoc), "\n")
    for(i in seq_along(archivos_pandoc)) {
      cat("  ", i, ":", archivos_pandoc[i], "\n")
    }

    return(TRUE)

  }, error = function(e) {
    cat("❌ Error generando Pandoc:", e$message, "\n")
    cat("💡 Nota: Verifique que Pandoc esté instalado correctamente\n")
    return(FALSE)
  })
}

# Función para generar Moodle XML
generar_moodle <- function() {
  cat("🎓 Generando archivos para Moodle...\n")
  cat("📁 Directorio de salida:", file.path(config$dir_salida, "moodle"), "\n")
  cat("📄 Archivo base:", archivo_examen, "\n")
  cat("🔢 Número de versiones:", config$archivos, "\n")
  cat("🎲 Semilla:", config$semilla, "\n")

  tryCatch({
    set.seed(config$semilla)
    cat("⏳ Iniciando generación Moodle XML...\n")

    resultado <- exams2moodle(archivo_examen,
                             n = config$archivos,
                             name = paste0(nombre_base, "_moodle"),
                             dir = file.path(config$dir_salida, "moodle"),
                             edir = config$dir_ejercicios,
                             encoding = config$encoding,
                             svg = TRUE,
                             verbose = TRUE)

    cat("✅ Archivos Moodle generados exitosamente\n")

    # Mostrar archivos generados
    archivos_moodle <- list.files(file.path(config$dir_salida, "moodle"), pattern = "\\.xml$")
    cat("📊 Archivos generados:", length(archivos_moodle), "\n")
    for(i in seq_along(archivos_moodle)) {
      cat("  ", i, ":", archivos_moodle[i], "\n")
    }

    return(TRUE)

  }, error = function(e) {
    cat("❌ Error generando Moodle:", e$message, "\n")
    return(FALSE)
  })
}

# Función para generar NOPS (exámenes escaneables)
generar_nops <- function() {
  cat("📋 Generando archivos NOPS (escaneables)...\n")
  cat("📁 Directorio de salida:", file.path(config$dir_salida, "nops"), "\n")
  cat("📄 Archivo base:", archivo_examen, "\n")
  cat("🔢 Número de versiones:", config$archivos, "\n")
  cat("🎲 Semilla:", config$semilla, "\n")

  tryCatch({
    set.seed(config$semilla)
    cat("⏳ Iniciando generación NOPS...\n")

    resultado <- exams2nops(archivo_examen,
                           n = config$archivos,
                           name = paste0(nombre_base, "_nops"),
                           dir = file.path(config$dir_salida, "nops"),
                           edir = config$dir_ejercicios,
                           encoding = config$encoding,
                           language = "es",
                           title = "Evaluación de Matemáticas",
                           institution = "I. E. Pedacito de Cielo",
                           date = Sys.Date(),
                           verbose = TRUE)

    cat("✅ Archivos NOPS generados exitosamente\n")

    # Mostrar archivos generados
    archivos_nops <- list.files(file.path(config$dir_salida, "nops"), pattern = "\\.(pdf|zip)$")
    cat("📊 Archivos generados:", length(archivos_nops), "\n")
    for(i in seq_along(archivos_nops)) {
      cat("  ", i, ":", archivos_nops[i], "\n")
    }

    return(TRUE)

  }, error = function(e) {
    cat("❌ Error generando NOPS:", e$message, "\n")
    cat("💡 Nota: NOPS requiere LaTeX y configuración específica\n")
    return(FALSE)
  })
}

# ===============================================================================
# FUNCIÓN PRINCIPAL DE GENERACIÓN CON VERBOSE
# ===============================================================================

generar_todos_formatos <- function(formatos = c("html", "moodle")) {
  cat("\n🏭 INICIANDO GENERACIÓN UNIVERSAL\n")
  cat("📄 Archivo:", archivo_examen, "\n")
  cat("🎯 Formatos seleccionados:", paste(formatos, collapse = ", "), "\n")
  cat("🔢 Versiones por formato:", config$archivos, "\n")
  cat("🎲 Semilla:", config$semilla, "\n")
  cat("📁 Directorio de salida:", config$dir_salida, "\n")
  cat("⏰ Hora de inicio:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat(strrep("-", 60), "\n\n")

  # Verificar que el archivo existe
  cat("🔍 Verificando archivo de entrada...\n")
  if (!file.exists(archivo_examen)) {
    stop("❌ Error: No se encuentra el archivo ", archivo_examen)
  }
  cat("✅ Archivo encontrado:", archivo_examen, "\n")

  # Verificar tamaño del archivo
  tamaño_archivo <- file.size(archivo_examen)
  cat("📊 Tamaño del archivo:", round(tamaño_archivo / 1024, 2), "KB\n")

  resultados <- list()
  tiempo_inicio <- Sys.time()

  # Crear directorios de salida
  cat("\n📁 Creando directorios de salida...\n")
  crear_directorios()

  cat("\n🎯 INICIANDO GENERACIÓN POR FORMATOS\n")
  cat(strrep("=", 50), "\n")

  # Generar cada formato solicitado
  if ("html" %in% formatos) {
    posicion <- which(formatos == "html")
    cat("\n[", posicion, "/", length(formatos), "] 🌐 FORMATO HTML\n")
    cat(strrep("-", 30), "\n")
    tiempo_formato <- Sys.time()
    resultados$html <- generar_html()
    tiempo_transcurrido <- difftime(Sys.time(), tiempo_formato, units = "secs")
    cat("⏱️  Tiempo HTML:", round(tiempo_transcurrido, 2), "segundos\n")
  }

  if ("pdf" %in% formatos) {
    posicion <- which(formatos == "pdf")
    cat("\n[", posicion, "/", length(formatos), "] 📄 FORMATO PDF\n")
    cat(strrep("-", 30), "\n")
    tiempo_formato <- Sys.time()
    resultados$pdf <- generar_pdf()
    tiempo_transcurrido <- difftime(Sys.time(), tiempo_formato, units = "secs")
    cat("⏱️  Tiempo PDF:", round(tiempo_transcurrido, 2), "segundos\n")
  }

  if ("pandoc" %in% formatos) {
    posicion <- which(formatos == "pandoc")
    cat("\n[", posicion, "/", length(formatos), "] 📝 FORMATO PANDOC\n")
    cat(strrep("-", 30), "\n")
    tiempo_formato <- Sys.time()
    resultados$pandoc <- generar_pandoc()
    tiempo_transcurrido <- difftime(Sys.time(), tiempo_formato, units = "secs")
    cat("⏱️  Tiempo Pandoc:", round(tiempo_transcurrido, 2), "segundos\n")
  }

  if ("moodle" %in% formatos) {
    posicion <- which(formatos == "moodle")
    cat("\n[", posicion, "/", length(formatos), "] 🎓 FORMATO MOODLE\n")
    cat(strrep("-", 30), "\n")
    tiempo_formato <- Sys.time()
    resultados$moodle <- generar_moodle()
    tiempo_transcurrido <- difftime(Sys.time(), tiempo_formato, units = "secs")
    cat("⏱️  Tiempo Moodle:", round(tiempo_transcurrido, 2), "segundos\n")
  }

  if ("nops" %in% formatos) {
    posicion <- which(formatos == "nops")
    cat("\n[", posicion, "/", length(formatos), "] 📋 FORMATO NOPS\n")
    cat(strrep("-", 30), "\n")
    tiempo_formato <- Sys.time()
    resultados$nops <- generar_nops()
    tiempo_transcurrido <- difftime(Sys.time(), tiempo_formato, units = "secs")
    cat("⏱️  Tiempo NOPS:", round(tiempo_transcurrido, 2), "segundos\n")
  }

  # Calcular tiempo total
  tiempo_total <- difftime(Sys.time(), tiempo_inicio, units = "secs")

  cat("\n", strrep("=", 50), "\n")
  cat("🎉 GENERACIÓN COMPLETADA\n")
  cat("⏰ Hora de finalización:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("⏱️  Tiempo total:", round(tiempo_total, 2), "segundos\n")

  # Resumen de resultados
  cat("\n📊 RESUMEN DE GENERACIÓN:\n")
  exitosos <- sum(unlist(resultados), na.rm = TRUE)
  total <- length(resultados)
  cat("✅ Formatos exitosos:", exitosos, "de", total, "\n")

  for (formato in names(resultados)) {
    estado <- if (resultados[[formato]]) "✅ EXITOSO" else "❌ FALLÓ"
    cat("  ", toupper(formato), ":", estado, "\n")
  }

  return(resultados)
}

# ===============================================================================
# FUNCIONES DE UTILIDAD
# ===============================================================================

# Función para crear directorios de salida
crear_directorios <- function() {
  # Crear directorio principal
  if (!dir.exists(config$dir_salida)) {
    dir.create(config$dir_salida, recursive = TRUE)
    cat("📁 Directorio principal creado:", config$dir_salida, "\n")
  }

  # Crear subdirectorios para cada formato
  formatos_dirs <- c("html", "pdf", "pandoc", "moodle", "nops")
  for (formato in formatos_dirs) {
    dir_formato <- file.path(config$dir_salida, formato)
    if (!dir.exists(dir_formato)) {
      dir.create(dir_formato, recursive = TRUE)
      cat("📂 Subdirectorio creado:", dir_formato, "\n")
    }
  }
  cat("✅ Estructura de directorios lista\n")
}

# Función para mostrar información del archivo
mostrar_info_archivo <- function() {
  cat("\n📋 INFORMACIÓN DEL ARCHIVO:\n")
  cat("  Archivo:", archivo_examen, "\n")

  if (file.exists(archivo_examen)) {
    info <- file.info(archivo_examen)
    cat("  Tamaño:", info$size, "bytes\n")
    cat("  Modificado:", format(info$mtime, "%Y-%m-%d %H:%M:%S"), "\n")

    # Contar líneas
    contenido <- readLines(archivo_examen, warn = FALSE)
    cat("  Líneas:", length(contenido), "\n")

    # Detectar tipo de examen usando la nueva función
    tipo_examen <- detectar_tipo_examen(archivo_examen)
    cat("  Tipo:", toupper(tipo_examen), "\n")

    # Detectar aleatorización
    tiene_sample <- any(grepl("sample\\(", contenido))
    cat("  Aleatorización:", ifelse(tiene_sample, "Detectada", "No detectada"), "\n")

    # Configurar entorno según el tipo
    configurar_entorno_examen(tipo_examen)

    # Mostrar información específica del tipo
    mostrar_info_tipo_examen(tipo_examen)

    # Retornar el tipo para uso posterior
    return(tipo_examen)
  } else {
    cat("  ❌ Archivo no encontrado\n")
    return("desconocido")
  }
}

# Función para mostrar menú de formatos
mostrar_menu_formatos <- function() {
  cat("\n🎯 FORMATOS DISPONIBLES:\n")
  cat("  1. HTML      - Páginas web interactivas\n")
  cat("  2. PDF       - Documentos imprimibles\n")
  cat("  3. Pandoc    - Documentos Word (DOCX)\n")
  cat("  4. Moodle    - XML para plataforma Moodle\n")
  cat("  5. NOPS      - Exámenes escaneables\n")
  cat("  6. Todos     - Generar todos los formatos\n")
  cat("  0. Salir     - Terminar programa\n")
}

# Función para leer selección del usuario
leer_seleccion <- function() {
  cat("\n📝 Ingrese su selección (números separados por comas, ej: 1,4,5): ")
  seleccion <- readline()
  return(seleccion)
}

# Función para procesar selección del usuario
procesar_seleccion <- function(seleccion) {
  # Limpiar y dividir la selección
  opciones <- as.numeric(unlist(strsplit(gsub(" ", "", seleccion), ",")))
  opciones <- opciones[!is.na(opciones)]

  # Mapear números a formatos
  mapa_formatos <- c("html", "pdf", "pandoc", "moodle", "nops")
  formatos_seleccionados <- c()

  for (opcion in opciones) {
    if (opcion == 0) {
      return("salir")
    } else if (opcion == 6) {
      return(mapa_formatos)  # Todos los formatos
    } else if (opcion >= 1 && opcion <= 5) {
      formatos_seleccionados <- c(formatos_seleccionados, mapa_formatos[opcion])
    } else {
      cat("⚠️  Opción inválida:", opcion, "\n")
    }
  }

  return(unique(formatos_seleccionados))
}

# Función para confirmar configuración
confirmar_configuracion <- function(formatos) {
  cat("\n📋 CONFIGURACIÓN SELECCIONADA:\n")
  cat("  Archivo:", archivo_examen, "\n")
  cat("  Formatos:", paste(formatos, collapse = ", "), "\n")
  cat("  Versiones:", config$archivos, "\n")
  cat("  Semilla:", config$semilla, "\n")
  cat("  Directorio:", config$dir_salida, "\n")

  cat("\n❓ ¿Desea continuar? (s/n): ")
  respuesta <- tolower(readline())
  return(respuesta %in% c("s", "si", "sí", "y", "yes"))
}

# Función para configurar número de archivos
configurar_archivos <- function() {
  cat("\n🔢 CONFIGURACIÓN DE VERSIONES:\n")
  cat("  Actual:", config$archivos, "versiones por formato\n")
  cat("  ¿Desea cambiar el número? (s/n): ")

  respuesta <- tolower(readline())
  if (respuesta %in% c("s", "si", "sí", "y", "yes")) {
    cat("  Ingrese el nuevo número de versiones: ")
    nuevo_numero <- as.numeric(readline())

    if (!is.na(nuevo_numero) && nuevo_numero > 0 && nuevo_numero <= 1000) {
      config$archivos <<- nuevo_numero
      cat("  ✅ Configurado a", nuevo_numero, "versiones\n")
    } else {
      cat("  ❌ Número inválido. Manteniendo", config$archivos, "\n")
    }
  }
}

# Función para mostrar recomendaciones de formato según tipo de examen
mostrar_recomendaciones_formato <- function(tipo_examen) {
  cat("\n💡 RECOMENDACIONES DE FORMATO PARA", toupper(tipo_examen), ":\n")

  switch(tipo_examen,
    "cloze" = {
      cat("  🎯 Formatos recomendados:\n")
      cat("    1. HTML    - Excelente para visualizar campos de respuesta\n")
      cat("    4. Moodle  - Soporte completo para Cloze\n")
      cat("  ⚠️  Formatos con limitaciones:\n")
      cat("    2. PDF     - Campos de respuesta como texto plano\n")
      cat("    5. NOPS    - No compatible con Cloze\n")
      cat("  ✅ Funciones especiales activadas:\n")
      cat("    • redondear_matematico() - Redondeo correcto\n")
      cat("    • formato_estandar() - Formato sin separador de miles\n")
    },
    "schoice" = {
      cat("  🎯 Formatos recomendados:\n")
      cat("    1. HTML    - Visualización clara de opciones\n")
      cat("    2. PDF     - Excelente para exámenes impresos\n")
      cat("    4. Moodle  - Evaluación automática\n")
      cat("    5. NOPS    - Ideal para exámenes escaneables\n")
      cat("  ✅ Todos los formatos son compatibles\n")
    },
    "mchoice" = {
      cat("  🎯 Formatos recomendados:\n")
      cat("    1. HTML    - Selección múltiple interactiva\n")
      cat("    4. Moodle  - Evaluación parcial automática\n")
      cat("  ⚠️  Formatos con limitaciones:\n")
      cat("    5. NOPS    - Limitado para respuestas múltiples\n")
    },
    "num" = {
      cat("  🎯 Formatos recomendados:\n")
      cat("    1. HTML    - Entrada numérica validada\n")
      cat("    4. Moodle  - Tolerancia automática\n")
      cat("  ✅ Funciones especiales activadas:\n")
      cat("    • redondear_matematico() - Redondeo correcto\n")
      cat("    • formato_estandar() - Formato numérico estándar\n")
    },
    {
      cat("  ⚠️  Tipo no reconocido, todos los formatos disponibles\n")
      cat("  💡 Se recomienda verificar el archivo manualmente\n")
    }
  )
}

# ===============================================================================
# LÓGICA PRINCIPAL INTERACTIVA
# ===============================================================================

# Función principal interactiva
main_interactivo <- function() {
  cat("🚀 GENERADOR UNIVERSAL DE EXÁMENES R/EXAMS\n")
  cat("==========================================\n")
  cat("Versión: SemilleroTotal_v1.R\n")
  cat("Fecha:", format(Sys.Date(), "%Y-%m-%d"), "\n")
  cat("==========================================\n")

  # Mostrar información del archivo y detectar tipo
  tipo_examen <- mostrar_info_archivo()

  # Configurar número de archivos
  configurar_archivos()

  # Mostrar recomendaciones según el tipo de examen
  mostrar_recomendaciones_formato(tipo_examen)

  # Bucle principal
  repeat {
    # Mostrar menú
    mostrar_menu_formatos()

    # Leer selección
    seleccion <- leer_seleccion()

    # Procesar selección
    formatos <- procesar_seleccion(seleccion)

    # Verificar si el usuario quiere salir
    if (length(formatos) == 1 && formatos == "salir") {
      cat("\n👋 ¡Hasta luego!\n")
      break
    }

    # Verificar que hay formatos seleccionados
    if (length(formatos) == 0) {
      cat("\n❌ No se seleccionaron formatos válidos. Intente de nuevo.\n")
      next
    }

    # Confirmar configuración
    if (confirmar_configuracion(formatos)) {
      # Ejecutar generación
      cat("\n🎬 INICIANDO GENERACIÓN...\n")
      resultados <- generar_todos_formatos(formatos)

      # Mostrar resumen final
      cat("\n🎊 PROCESO COMPLETADO\n")
      cat("====================\n")

      exitosos <- sum(unlist(resultados), na.rm = TRUE)
      total <- length(resultados)

      if (exitosos > 0) {
        cat("✅ Generación exitosa:", exitosos, "de", total, "formatos\n")
        cat("📁 Archivos disponibles en:", config$dir_salida, "\n")

        # Mostrar estadísticas por formato
        for (formato in names(resultados)) {
          if (resultados[[formato]]) {
            dir_formato <- file.path(config$dir_salida, formato)
            if (dir.exists(dir_formato)) {
              archivos <- list.files(dir_formato)
              cat("  📂", toupper(formato), ":", length(archivos), "archivos\n")
            }
          }
        }
      } else {
        cat("❌ No se generaron archivos exitosamente\n")
        cat("💡 Revise los mensajes de error anteriores\n")
      }

      # Preguntar si desea continuar
      cat("\n❓ ¿Desea generar más formatos? (s/n): ")
      continuar <- tolower(readline())
      if (!continuar %in% c("s", "si", "sí", "y", "yes")) {
        cat("\n👋 ¡Hasta luego!\n")
        break
      }
    } else {
      cat("\n❌ Operación cancelada\n")
    }
  }
}

# ===============================================================================
# EJECUCIÓN PRINCIPAL
# ===============================================================================

# Verificar si el archivo de examen existe
if (!file.exists(archivo_examen)) {
  cat("❌ ERROR: No se encuentra el archivo de examen:\n")
  cat("   ", archivo_examen, "\n")
  cat("💡 Verifique que el archivo existe en el directorio actual\n")
  cat("📁 Directorio actual:", getwd(), "\n")
  stop("Archivo de examen no encontrado")
}

# Ejecutar interfaz interactiva
main_interactivo()
