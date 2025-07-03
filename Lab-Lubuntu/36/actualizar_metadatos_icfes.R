# Script para actualizar metadatos ICFES en ejercicios existentes
# Este script busca archivos .Rmd en el repositorio y les añade los metadatos ICFES

# Función para añadir metadatos ICFES a un archivo .Rmd
add_icfes_metadata <- function(file_path, metadata) {
  # Leer el contenido del archivo
  content <- readLines(file_path, warn = FALSE)

  # Verificar si ya tiene metadatos ICFES
  if (any(grepl("# Metadatos ICFES", content))) {
    # Leer el contenido para verificar si los metadatos son correctos
    content_text <- paste(content, collapse = " ")

    # Verificar si es un problema de geometría pero está clasificado como estadística
    if (grepl("volumen|cilindro|cubo|esfera|cono|pirámide|prisma", content_text, ignore.case = TRUE) &&
        grepl("categoria: estadistica", content_text, ignore.case = TRUE)) {
      message("Corrigiendo metadatos incorrectos en: ", file_path)
      # Eliminar los metadatos existentes para reemplazarlos
      start_idx <- which(grepl("# Metadatos ICFES", content))
      end_idx <- start_idx
      for (i in (start_idx + 1):length(content)) {
        if (grepl("^```", content[i])) {
          end_idx <- i - 1
          break
        }
        if (i == length(content)) {
          end_idx <- i
          break
        }
      }
      content <- content[-(start_idx:end_idx)]
    } else {
      message("El archivo ya tiene metadatos ICFES: ", file_path)
      return(FALSE)
    }
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

  # Preparar los metadatos ICFES en formato YAML
  icfes_metadata <- c(
    "",
    "# Metadatos ICFES",
    "icfes:",
    "  competencia: ",
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

  # Clasificación por defecto según el sistema de etiquetado ICFES
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

  # Clasificación por contenido para determinar categoría y componente
  # Primero verificamos si hay palabras clave específicas de geometría
  if (grepl("cilindro|volumen|área|superficie|geometría|figura|triángulo|círculo|esfera|paralelogramo|cubo|prisma|pirámide|cono|radio|diámetro",
            content, ignore.case = TRUE)) {
    metadata$contenido$categoria <- "geometria"
    metadata$componente <- "geometrico_metrico"
    metadata$eje_axial <- "eje2"
  }
  # Luego verificamos palabras clave de álgebra y cálculo
  else if (grepl("función|ecuación|variable|expresión|álgebra|número|operación|polinomio|derivada|integral|límite",
                 content, ignore.case = TRUE)) {
    metadata$contenido$categoria <- "algebra_calculo"
    metadata$componente <- "numerico_variacional"
    metadata$eje_axial <- "eje3"
  }
  # Finalmente verificamos palabras clave de estadística
  else if (grepl("Venn|probabilidad|conjunto|estadística|datos|gráfico|diagrama|frecuencia|media|mediana|moda|desviación",
                 content, ignore.case = TRUE)) {
    metadata$contenido$categoria <- "estadistica"
    metadata$componente <- "aleatorio"
    metadata$eje_axial <- "eje4"
  }

  # Verificación adicional para casos específicos
  # Si menciona "volumen" o "cilindro", es casi seguro que es geometría
  if (grepl("volumen|cilindro|cubo|esfera|cono|pirámide|prisma", content, ignore.case = TRUE)) {
    metadata$contenido$categoria <- "geometria"
    metadata$componente <- "geometrico_metrico"
    metadata$eje_axial <- "eje2"
  }

  # Clasificación por nivel de dificultad
  if (grepl("justifica|argumenta|valida|refuta|demuestra|prueba|verifica",
            content, ignore.case = TRUE)) {
    metadata$nivel_dificultad <- 4
    metadata$competencia <- c("argumentacion")
  } else if (grepl("modela|resuelve problema|estrategia|plantea|formula|ejecuta",
                   content, ignore.case = TRUE)) {
    metadata$nivel_dificultad <- 3
    metadata$competencia <- c("formulacion_ejecucion")
  } else if (grepl("compara|identifica|reconoce|representa",
                   content, ignore.case = TRUE)) {
    metadata$nivel_dificultad <- 2
    metadata$competencia <- c("interpretacion_representacion")
  } else if (grepl("lee|observa|extrae", content, ignore.case = TRUE)) {
    metadata$nivel_dificultad <- 1
    metadata$competencia <- c("interpretacion_representacion")
  }

  # Clasificación por contexto
  if (grepl("familia|hogar|salud|recreación|personal", content, ignore.case = TRUE)) {
    metadata$contexto <- "familiar"
  } else if (grepl("trabajo|empleo|ocupación|profesión|laboral",
                   content, ignore.case = TRUE)) {
    metadata$contexto <- "laboral"
  } else if (grepl("sociedad|comunidad|política|economía|ambiente|social",
                   content, ignore.case = TRUE)) {
    metadata$contexto <- "comunitario"
  } else {
    metadata$contexto <- "matematico"
  }

  # Clasificación por tipo de contenido
  if (grepl("ciudadano|cotidiano|diario|común", content, ignore.case = TRUE)) {
    metadata$contenido$tipo <- "generico"
  } else {
    metadata$contenido$tipo <- "no_generico"
  }

  # Clasificación por ruta del archivo (si hay información específica)
  if (grepl("Estadística|Probabilidad", file_path, ignore.case = TRUE)) {
    metadata$contenido$categoria <- "estadistica"
    metadata$componente <- "aleatorio"
    metadata$eje_axial <- "eje4"
  } else if (grepl("Geometria|Analitica", file_path, ignore.case = TRUE)) {
    metadata$contenido$categoria <- "geometria"
    metadata$componente <- "geometrico_metrico"
    metadata$eje_axial <- "eje2"
  } else if (grepl("Funciones|Algebra", file_path, ignore.case = TRUE)) {
    metadata$contenido$categoria <- "algebra_calculo"
    metadata$componente <- "numerico_variacional"
    metadata$eje_axial <- "eje3"
  } else if (grepl("Numeros|Reales", file_path, ignore.case = TRUE)) {
    metadata$contenido$categoria <- "algebra_calculo"
    metadata$componente <- "numerico_variacional"
    metadata$eje_axial <- "eje3"
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
# Para ejecutar en el directorio actual:
update_all_metadata(".")
