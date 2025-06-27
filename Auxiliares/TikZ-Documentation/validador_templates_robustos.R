# 🔧 VALIDADOR AUTOMÁTICO DE TEMPLATES TIKZ ROBUSTOS
# Versión: 1.0
# Propósito: Validar compatibilidad multi-formato de templates TikZ
# Basado en: AUDITORIA_TEMPLATES_TIKZ_2025.md

# === CONFIGURACIÓN INICIAL ===
library(exams)
library(testthat)

# Configurar entorno de pruebas
options(warn = -1)  # Suprimir warnings durante pruebas
Sys.setlocale(category = "LC_NUMERIC", locale = "C")

# === FUNCIÓN PRINCIPAL DE VALIDACIÓN ===
validar_template_tikz <- function(template_path, test_name = "test_template") {
  
  cat("🔍 Validando template:", basename(template_path), "\n")
  
  # Crear archivo .Rmd temporal para pruebas
  rmd_content <- crear_rmd_prueba(template_path, test_name)
  rmd_file <- paste0(test_name, ".Rmd")
  writeLines(rmd_content, rmd_file)
  
  # Resultados de validación
  resultados <- list(
    template = basename(template_path),
    fecha = Sys.time(),
    formatos = list(),
    errores = list(),
    warnings = list(),
    tiempo_total = 0
  )
  
  tiempo_inicio <- Sys.time()
  
  # === PRUEBAS POR FORMATO ===
  formatos_prueba <- c("pdf", "html", "moodle")
  
  for (formato in formatos_prueba) {
    cat("  📋 Probando formato:", formato, "... ")
    
    resultado_formato <- probar_formato(rmd_file, formato, test_name)
    resultados$formatos[[formato]] <- resultado_formato
    
    if (resultado_formato$exito) {
      cat("✅\n")
    } else {
      cat("❌\n")
      resultados$errores[[formato]] <- resultado_formato$error
    }
  }
  
  resultados$tiempo_total <- as.numeric(difftime(Sys.time(), tiempo_inicio, units = "secs"))
  
  # Limpiar archivos temporales
  limpiar_archivos_temporales(test_name)
  
  return(resultados)
}

# === CREAR ARCHIVO RMD DE PRUEBA ===
crear_rmd_prueba <- function(template_path, test_name) {
  
  # Detectar tipo de template por nombre
  tipo_template <- detectar_tipo_template(template_path)
  
  # Generar variables según el tipo
  variables_r <- generar_variables_prueba(tipo_template)
  
  # Crear contenido del .Rmd
  rmd_content <- c(
    "---",
    "output:",
    "  html_document: default",
    "  pdf_document: default",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "library(exams)",
    "options(tikzLatex = 'pdflatex')",
    "options(tikzXelatex = FALSE)",
    "typ <- match_exams_device()",
    "```",
    "",
    "```{r variables, include=FALSE}",
    variables_r,
    "```",
    "",
    "```{r generar_tikz, echo=FALSE, results='asis'}",
    paste0("template_lines <- readLines('", template_path, "')"),
    "include_tikz(template_lines,",
    paste0("             name = '", test_name, "',"),
    "             format = typ,",
    "             packages = c('tikz', 'colortbl'),",
    "             width = '6cm')",
    "```",
    "",
    "Question",
    "========",
    "Este es un test de validación del template TikZ.",
    "",
    "Answerlist",
    "==========",
    "* Opción A",
    "* Opción B", 
    "* Opción C",
    "* Opción D",
    "",
    "Solution",
    "========",
    "Template validado correctamente.",
    "",
    "Answerlist",
    "==========",
    "* Falso",
    "* Falso",
    "* Verdadero", 
    "* Falso",
    "",
    "Meta-information",
    "================",
    "extype: schoice",
    "exsolution: 0010",
    "exname: test_template"
  )
  
  return(rmd_content)
}

# === DETECTAR TIPO DE TEMPLATE ===
detectar_tipo_template <- function(template_path) {
  nombre <- tolower(basename(template_path))
  
  if (grepl("cilindro", nombre)) return("cilindro")
  if (grepl("venn", nombre)) return("venn")
  if (grepl("tabla", nombre)) return("tabla")
  if (grepl("circular", nombre)) return("circular")
  
  return("generico")
}

# === GENERAR VARIABLES DE PRUEBA ===
generar_variables_prueba <- function(tipo) {
  
  switch(tipo,
    "cilindro" = c(
      "altura_cilindro <- 3",
      "radio_interno <- 1.2", 
      "grosor_pared <- 0.5",
      "escala_grafico <- 1.0"
    ),
    "venn" = c(
      "conjunto_A <- 'Rock'",
      "conjunto_B <- 'Pop'",
      "valor_solo_A <- 15",
      "valor_solo_B <- 20", 
      "valor_A_B <- 8",
      "valor_ninguno <- 12",
      "escala_venn <- 0.8"
    ),
    "tabla" = c(
      "encabezado1 <- 'Año'",
      "encabezado2 <- 'Ventas'",
      "datos_fila1 <- c('2020', '15.2')",
      "datos_fila2 <- c('2021', '18.7')",
      "datos_fila3 <- c('2022', '22.1')",
      "color_encabezado <- 'blue'",
      "intensidad_color <- 20"
    ),
    "circular" = c(
      "categorias <- c('A', 'B', 'C', 'D')",
      "valores <- c(30, 25, 25, 20)",
      "colores <- c('blue', 'red', 'green', 'orange')",
      "intensidades <- c(60, 60, 60, 60)",
      "radio_grafico <- 2.5",
      "mostrar_porcentajes <- TRUE",
      "mostrar_leyenda <- TRUE"
    ),
    # Genérico
    c("# Variables genéricas", "escala <- 1.0")
  )
}

# === PROBAR FORMATO ESPECÍFICO ===
probar_formato <- function(rmd_file, formato, test_name) {
  
  resultado <- list(exito = FALSE, error = NULL, archivo_salida = NULL)
  
  tryCatch({
    
    if (formato == "pdf") {
      exams2pdf(rmd_file, n = 1, name = test_name, dir = "temp_test")
      resultado$archivo_salida <- paste0("temp_test/", test_name, "1.pdf")
      
    } else if (formato == "html") {
      exams2html(rmd_file, n = 1, name = test_name, dir = "temp_test")
      resultado$archivo_salida <- paste0("temp_test/", test_name, "1.html")
      
    } else if (formato == "moodle") {
      exams2moodle(rmd_file, n = 1, name = test_name, dir = "temp_test")
      resultado$archivo_salida <- paste0("temp_test/", test_name, ".xml")
    }
    
    # Verificar que el archivo se creó
    if (file.exists(resultado$archivo_salida)) {
      resultado$exito <- TRUE
    } else {
      resultado$error <- "Archivo de salida no generado"
    }
    
  }, error = function(e) {
    resultado$error <- e$message
  })
  
  return(resultado)
}

# === LIMPIAR ARCHIVOS TEMPORALES ===
limpiar_archivos_temporales <- function(test_name) {
  
  # Archivos a limpiar
  archivos_limpiar <- c(
    paste0(test_name, ".Rmd"),
    paste0(test_name, ".png"),
    paste0(test_name, ".pdf")
  )
  
  # Directorio temporal
  if (dir.exists("temp_test")) {
    unlink("temp_test", recursive = TRUE)
  }
  
  # Archivos individuales
  for (archivo in archivos_limpiar) {
    if (file.exists(archivo)) {
      file.remove(archivo)
    }
  }
}

# === GENERAR REPORTE DE VALIDACIÓN ===
generar_reporte_validacion <- function(resultados_lista) {
  
  cat("\n" , "="*60, "\n")
  cat("📊 REPORTE DE VALIDACIÓN DE TEMPLATES TIKZ\n")
  cat("="*60, "\n")
  cat("Fecha:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Templates validados:", length(resultados_lista), "\n\n")
  
  for (resultado in resultados_lista) {
    
    cat("🎯 Template:", resultado$template, "\n")
    cat("   Tiempo total:", round(resultado$tiempo_total, 2), "segundos\n")
    
    # Estado por formato
    for (formato in names(resultado$formatos)) {
      estado <- if (resultado$formatos[[formato]]$exito) "✅" else "❌"
      cat("   ", formato, ":", estado, "\n")
      
      if (!resultado$formatos[[formato]]$exito) {
        cat("      Error:", resultado$formatos[[formato]]$error, "\n")
      }
    }
    cat("\n")
  }
  
  # Resumen general
  total_pruebas <- length(resultados_lista) * 3  # 3 formatos por template
  pruebas_exitosas <- sum(sapply(resultados_lista, function(r) {
    sum(sapply(r$formatos, function(f) f$exito))
  }))
  
  cat("📈 RESUMEN GENERAL\n")
  cat("Pruebas exitosas:", pruebas_exitosas, "/", total_pruebas, "\n")
  cat("Tasa de éxito:", round(pruebas_exitosas/total_pruebas*100, 1), "%\n")
}

# === FUNCIÓN PRINCIPAL PARA VALIDAR TODOS LOS TEMPLATES ===
validar_todos_templates <- function() {
  
  # Directorio de templates robustos
  dir_templates <- "Auxiliares/TikZ-Documentation/templates-rexams/robustos/"
  
  if (!dir.exists(dir_templates)) {
    stop("❌ Directorio de templates no encontrado: ", dir_templates)
  }
  
  # Encontrar todos los archivos .tikz
  templates <- list.files(dir_templates, pattern = "\\.tikz$", full.names = TRUE)
  
  if (length(templates) == 0) {
    stop("❌ No se encontraron templates .tikz en: ", dir_templates)
  }
  
  cat("🚀 Iniciando validación de", length(templates), "templates...\n\n")
  
  # Validar cada template
  resultados <- list()
  for (i in seq_along(templates)) {
    test_name <- paste0("test_", i)
    resultados[[i]] <- validar_template_tikz(templates[i], test_name)
  }
  
  # Generar reporte
  generar_reporte_validacion(resultados)
  
  return(resultados)
}

# === EJEMPLO DE USO ===
# Para validar todos los templates:
# resultados <- validar_todos_templates()
#
# Para validar un template específico:
# resultado <- validar_template_tikz("path/to/template.tikz", "mi_test")
