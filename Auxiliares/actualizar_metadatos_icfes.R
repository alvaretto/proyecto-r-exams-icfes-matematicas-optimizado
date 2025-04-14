# Script para actualizar metadatos ICFES en ejercicios existentes
# Este script busca archivos .Rmd en el repositorio y les añade los metadatos ICFES

# Función para añadir metadatos ICFES a un archivo .Rmd
add_icfes_metadata <- function(file_path, metadata) {
  # Leer el contenido del archivo
  content <- readLines(file_path, warn = FALSE)
  
  # Verificar si ya tiene metadatos ICFES
  if (any(grepl("# Metadatos ICFES", content))) {
    message("El archivo ya tiene metadatos ICFES: ", file_path)
    return(FALSE)
  }
  
  # Verificar si tiene encabezado YAML
  has_yaml <- FALSE
  yaml_end <- 0
  
  if (content[1] == "---") {
    for (i in 2:length(content)) {
      if (content[i] == "---") {
        has_yaml <- TRUE
        yaml_end <- i
        break
      }
    }
  }
  
  # Preparar los metadatos ICFES
  icfes_metadata <- c(
    "",
    "# Metadatos ICFES",
    "icfes:",
    paste0("  competencia: "),
    paste0("    - ", metadata$competencia[1]),
    if (length(metadata$competencia) > 1) paste0("    - ", metadata$competencia[2]) else NULL,
    if (length(metadata$competencia) > 2) paste0("    - ", metadata$competencia[3]) else NULL,
    paste0("  nivel_dificultad: ", metadata$nivel_dificultad),
    "  contenido:",
    paste0("    categoria: ", metadata$contenido$categoria),
    paste0("    tipo: ", metadata$contenido$tipo),
    paste0("  contexto: ", metadata$contexto),
    paste0("  eje_axial: ", metadata$eje_axial),
    paste0("  componente: ", metadata$componente),
    ""
  )
  
  # Insertar los metadatos ICFES
  if (has_yaml) {
    # Si tiene encabezado YAML, insertar después del encabezado
    new_content <- c(
      content[1:yaml_end],
      icfes_metadata,
      content[(yaml_end+1):length(content)]
    )
  } else {
    # Si no tiene encabezado YAML, crear uno nuevo
    new_content <- c(
      "---",
      "output:",
      "  pdf_document: default",
      "  html_document: default",
      "---",
      icfes_metadata,
      content
    )
  }
  
  # Escribir el nuevo contenido al archivo
  writeLines(new_content, file_path)
  message("Metadatos ICFES añadidos a: ", file_path)
  return(TRUE)
}

# Función para buscar archivos .Rmd en un directorio y sus subdirectorios
find_rmd_files <- function(dir_path) {
  files <- list.files(dir_path, pattern = "\\.Rmd$", recursive = TRUE, full.names = TRUE)
  return(files)
}

# Función para clasificar automáticamente un ejercicio basado en su contenido
classify_exercise <- function(file_path) {
  # Leer el contenido del archivo
  content <- paste(readLines(file_path, warn = FALSE), collapse = " ")
  
  # Clasificación por defecto
  metadata <- list(
    competencia = c("interpretacion_representacion"),
    nivel_dificultad = 2,
    contenido = list(
      categoria = "estadistica",
      tipo = "generico"
    ),
    contexto = "matematico",
    eje_axial = "eje4",
    componente = "aleatorio"
  )
  
  # Clasificación por ruta del archivo
  if (grepl("Estadística-Y-Probabilidad", file_path)) {
    metadata$contenido$categoria <- "estadistica"
    metadata$componente <- "aleatorio"
    metadata$eje_axial <- "eje4"
  } else if (grepl("Geometria-Analitica", file_path)) {
    metadata$contenido$categoria <- "geometria"
    metadata$componente <- "geometrico_metrico"
    metadata$eje_axial <- "eje2"
  } else if (grepl("Funciones", file_path)) {
    metadata$contenido$categoria <- "algebra_calculo"
    metadata$componente <- "numerico_variacional"
    metadata$eje_axial <- "eje3"
  } else if (grepl("Numeros-Reales", file_path)) {
    metadata$contenido$categoria <- "algebra_calculo"
    metadata$componente <- "numerico_variacional"
    metadata$eje_axial <- "eje3"
  }
  
  # Clasificación por contenido
  if (grepl("Venn|probabilidad|conjunto", content, ignore.case = TRUE)) {
    metadata$contenido$categoria <- "estadistica"
    metadata$componente <- "aleatorio"
  } else if (grepl("triángulo|círculo|esfera|paralelogramo", content, ignore.case = TRUE)) {
    metadata$contenido$categoria <- "geometria"
    metadata$componente <- "geometrico_metrico"
  } else if (grepl("función|ecuación|variable|expresión", content, ignore.case = TRUE)) {
    metadata$contenido$categoria <- "algebra_calculo"
    metadata$componente <- "numerico_variacional"
  }
  
  # Clasificación por nivel de dificultad
  if (grepl("justifica|argumenta|valida|refuta", content, ignore.case = TRUE)) {
    metadata$nivel_dificultad <- 4
    metadata$competencia <- c("argumentacion")
  } else if (grepl("modela|resuelve problema|estrategia", content, ignore.case = TRUE)) {
    metadata$nivel_dificultad <- 3
    metadata$competencia <- c("formulacion_ejecucion")
  }
  
  # Clasificación por contexto
  if (grepl("familia|hogar|salud|recreación", content, ignore.case = TRUE)) {
    metadata$contexto <- "familiar"
  } else if (grepl("trabajo|empleo|ocupación|profesión", content, ignore.case = TRUE)) {
    metadata$contexto <- "laboral"
  } else if (grepl("sociedad|comunidad|política|economía|ambiente", content, ignore.case = TRUE)) {
    metadata$contexto <- "comunitario"
  }
  
  return(metadata)
}

# Función principal para actualizar metadatos en todos los archivos .Rmd
update_all_metadata <- function(dir_path) {
  # Buscar archivos .Rmd
  rmd_files <- find_rmd_files(dir_path)
  
  # Contador de archivos actualizados
  updated_count <- 0
  
  # Procesar cada archivo
  for (file_path in rmd_files) {
    # Clasificar el ejercicio
    metadata <- classify_exercise(file_path)
    
    # Añadir metadatos ICFES
    if (add_icfes_metadata(file_path, metadata)) {
      updated_count <- updated_count + 1
    }
  }
  
  message("Proceso completado. ", updated_count, " archivos actualizados de ", length(rmd_files), " archivos .Rmd encontrados.")
}

# Ejemplo de uso:
update_all_metadata("RepositorioMatematicasICFES_R_Exams")
