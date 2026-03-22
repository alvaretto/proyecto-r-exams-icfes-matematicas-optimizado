#!/usr/bin/env Rscript
# =============================================================================
# Script: corregir_ortografia_espanol.R
# Propósito: Corrección automática de tildes en archivos .Rmd
# Uso: Rscript corregir_ortografia_espanol.R archivo.Rmd [--fix]
#      --fix: Aplica correcciones (sin este flag solo reporta)
# =============================================================================

# Diccionario de correcciones: palabra_sin_tilde -> palabra_con_tilde
diccionario_tildes <- c(

  # Sustantivos y términos técnicos
  "informacion" = "información",
  "descripcion" = "descripción",
  "explicacion" = "explicación",
  "configuracion" = "configuración",
  "solucion" = "solución",
  "validacion" = "validación",
  "clasificacion" = "clasificación",
  "ecuacion" = "ecuación",
  "dimension" = "dimensión",
  "version" = "versión",
  "seleccion" = "selección",
  "seccion" = "sección",
  "funcion" = "función",
  "relacion" = "relación",
  "distribucion" = "distribución",
  "variacion" = "variación",
  "dispersion" = "dispersión",
  "combinacion" = "combinación",
  "iteracion" = "iteración",
  "compilacion" = "compilación",
  "instalacion" = "instalación",
  "documentacion" = "documentación",
  "retroalimentacion" = "retroalimentación",
  "elongacion" = "elongación",
  "deformacion" = "deformación",
  "inclinacion" = "inclinación",
  "operacion" = "operación",
  "multiplicacion" = "multiplicación",
  "division" = "división",
  "adicion" = "adición",
  "sustraccion" = "sustracción",
  "fraccion" = "fracción",
  "proporcion" = "proporción",
  "razon" = "razón",
  "evaluacion" = "evaluación",
  "medicion" = "medición",
  "observacion" = "observación",
  "experimentacion" = "experimentación",
  "interpretacion" = "interpretación",
  "representacion" = "representación",
  "argumentacion" = "argumentación",
  "formulacion" = "formulación",
  "ejecucion" = "ejecución",

 # Términos matemáticos/científicos esdrújulos
  "grafica" = "gráfica",
  "grafico" = "gráfico",
  "graficas" = "gráficas",
  "graficos" = "gráficos",
  "matematico" = "matemático",
  "matematica" = "matemática",
  "estadistica" = "estadística",
  "estadistico" = "estadístico",
  "cientifico" = "científico",
  "cientifica" = "científica",
  "parabolico" = "parabólico",
  "parabolica" = "parabólica",
  "geometrico" = "geométrico",
  "geometrica" = "geométrica",
  "numerico" = "numérico",
  "numerica" = "numérica",
  "teorico" = "teórico",
  "teorica" = "teórica",
  "unico" = "único",
  "unica" = "única",
  "dinamico" = "dinámico",
  "dinamica" = "dinámica",
  "automatico" = "automático",
  "automatica" = "automática",
  "semantico" = "semántico",
  "semantica" = "semántica",
  "cuadratico" = "cuadrático",
  "cuadratica" = "cuadrática",
  "logaritmico" = "logarítmico",
  "logaritmica" = "logarítmica",
  "exponencial" = "exponencial",
  "periodico" = "periódico",
  "periodica" = "periódica",
  "simetrico" = "simétrico",
  "simetrica" = "simétrica",
  "asimetrico" = "asimétrico",
  "asimetrica" = "asimétrica",

  # Sustantivos comunes
  "codigo" = "código",
  "proposito" = "propósito",
  "analisis" = "análisis",
  "numero" = "número",
  "numeros" = "números",
  "angulo" = "ángulo",
  "angulos" = "ángulos",
  "calculo" = "cálculo",
  "calculos" = "cálculos",
  "metodo" = "método",
  "metodos" = "métodos",
  "exito" = "éxito",
  "patron" = "patrón",
  "maximo" = "máximo",
  "maxima" = "máxima",
  "minimo" = "mínimo",
  "minima" = "mínima",
  "area" = "área",
  "areas" = "áreas",
  "perimetro" = "perímetro",
  "diametro" = "diámetro",
  "radio" = "radio",
  "triangulo" = "triángulo",
  "triangulos" = "triángulos",
  "rectangulo" = "rectángulo",
  "rectangulos" = "rectángulos",
  "circulo" = "círculo",
  "circulos" = "círculos",
  "piramide" = "pirámide",
  "energia" = "energía",
  "periodo" = "período",
  "vehiculo" = "vehículo",
  "vehiculos" = "vehículos",
  "pagina" = "página",
  "paginas" = "páginas",
  "linea" = "línea",
  "lineas" = "líneas",
  "termino" = "término",
  "terminos" = "términos",
  "limite" = "límite",
  "limites" = "límites",
  "hipotesis" = "hipótesis",
  "sintesis" = "síntesis",
  "tesis" = "tesis",
  "enfasis" = "énfasis",

  # Adverbios y conectores
  "mas" = "más",
  "tambien" = "también",
  "asi" = "así",
  "aqui" = "aquí",
  "ahi" = "ahí",
  "alla" = "allá",
  "despues" = "después",
  "segun" = "según",
  "ademas" = "además",
  "todavia" = "todavía",
  "quiza" = "quizá",
  "quizas" = "quizás",
  "dificil" = "difícil",
  "facil" = "fácil",
  "util" = "útil",
  "debil" = "débil",
  "agil" = "ágil",
  "fertil" = "fértil",
  "esteril" = "estéril",
  "movil" = "móvil",
  "habil" = "hábil",
  "fragil" = "frágil",
  "optima" = "óptima",
  "optimo" = "óptimo",
  # "cual"/"cuales" EXCLUIDOS — ambiguos:
  # "lo cual" (pronombre relativo, SIN tilde) vs "¿cuál?" (interrogativo, CON tilde)
  # El script no puede distinguir el contexto gramatical.

  # Verbos conjugados comunes
  # NOTA: "esta/estan" EXCLUIDOS - confunden pronombre demostrativo con verbo
  "sera" = "será",
  "seran" = "serán",
  "estara" = "estará",
  "estaran" = "estarán",
  "podra" = "podrá",
  "podran" = "podrán",
  "tendra" = "tendrá",
  "tendran" = "tendrán",
  "hara" = "hará",
  "haran" = "harán",
  "debera" = "deberá",
  "deberan" = "deberán",
  "habra" = "habrá",
  "habran" = "habrán"
)

# Palabras excluidas cuando son nombres de variables R
# Estas palabras NO deben corregirse si aparecen en contexto de código R
palabras_excluir_en_codigo <- c(
  "solucion", "angulos", "angulo", "funcion", "numero",
  "grafica", "grafico", "calculo", "codigo", "metodo",
  "area"  # Variable R frecuente (par_sel$area, ctx$area)
)

# Campos de metadatos R-exams que DEBEN permanecer ASCII
# Estos campos usan identificadores que no deben tener tildes
campos_rexams_ascii <- c(

  "exname",        # Nombre del ejercicio (identificador)
  "exsection",    # Sección/categoría (ruta con /)
  "extype",       # Tipo: schoice, mchoice, cloze, num, string
  "exsolution",   # Solución (código binario o valor)
  "exshuffle",    # TRUE/FALSE
  "extol",        # Tolerancia numérica
  "exextra"       # Metadatos extra (cualquier exextra[...])
)

# Función para verificar si una línea es metadato R-exams que debe ser ASCII
es_metadato_rexams_ascii <- function(linea) {
  # Verificar si la línea comienza con algún campo R-exams

  for (campo in campos_rexams_ascii) {
    # Patrón para campo exacto o con corchetes (exextra[Type])
    if (grepl(paste0("^", campo, "(\\[.*\\])?\\s*:"), linea, ignore.case = TRUE)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Función para verificar si una línea es código R (no texto/comentario)
es_codigo_r <- function(linea) {
  # Detectar asignaciones de variables (var <- valor, var = valor)
  if (grepl("^\\s*[a-zA-Z_][a-zA-Z0-9_]*\\s*(<-|=)\\s*", linea)) {
    return(TRUE)
  }
  # Detectar llamadas a funciones con argumentos nombrados
  if (grepl("\\w+\\s*=\\s*\\w+", linea) && !grepl("^#", trimws(linea))) {
    return(TRUE)
  }
  # Detectar definiciones de función
  if (grepl("function\\s*\\(", linea)) {
    return(TRUE)
  }
  return(FALSE)
}

# Función para verificar si es un nombre de variable R
es_nombre_variable <- function(linea, palabra) {
  # Patrones que indican que la palabra es un nombre de variable
  patrones_variable <- c(
    paste0("\\b", palabra, "\\s*<-"),       # var <-
    paste0("\\b", palabra, "\\s*=\\s*c\\("), # var = c(
    paste0("\\b", palabra, "\\s*=\\s*list"), # var = list
    paste0("\\$", palabra, "\\b"),           # datos$var
    paste0("\\b", palabra, "\\$"),           # var$campo
    paste0("\\[\\[\"", palabra, "\"\\]\\]"), # [["var"]]
    paste0("\\bfunction\\s*\\([^)]*", palabra), # argumento de función
    paste0("\\b", palabra, "\\s*=\\s*[^\"']"),  # argumento nombrado
    paste0("^\\s*", palabra, "\\s*<-")       # inicio de línea con asignación
  )

  for (patron in patrones_variable) {
    if (grepl(patron, linea, perl = TRUE)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Función para verificar si la palabra está dentro de un string R
esta_en_string <- function(linea, palabra) {
  # Buscar la palabra dentro de comillas
  if (grepl(paste0('["\'][^"\']*', palabra, '[^"\']*["\']'), linea, perl = TRUE)) {
    return(TRUE)
  }
  return(FALSE)
}

# Función para verificar si la palabra está en código R inline
esta_en_codigo_inline <- function(linea, palabra) {
  # Buscar la palabra dentro de `r ... `
  if (grepl(paste0('`r[^`]*', palabra, '[^`]*`'), linea, perl = TRUE)) {
    return(TRUE)
  }
  return(FALSE)
}

# Función para verificar si la palabra es un nombre de variable
es_palabra_excluida_en_codigo <- function(linea, palabra) {
  if (!(palabra %in% palabras_excluir_en_codigo)) {
    return(FALSE)
  }
  # Verificar si está en contexto de código R
  if (esta_en_codigo_inline(linea, palabra)) {
    return(TRUE)
  }
  # Verificar si es nombre de variable (asignación, argumento, etc.)
  patrones <- c(
    paste0("\\b", palabra, "\\s*<-"),        # var <-
    paste0("<-\\s*", palabra, "\\b"),        # <- var
    paste0("\\b", palabra, "\\s*="),         # var = (asignación o argumento)
    paste0("\\$", palabra, "\\b"),           # datos$var
    paste0("\\b", palabra, "\\$"),           # var$campo
    paste0("\\(", palabra, ","),             # función(var,
    paste0(",\\s*", palabra, "\\)"),         # , var)
    paste0(",\\s*", palabra, ","),           # , var,
    paste0("\\bfunction\\s*\\([^)]*", palabra)  # argumento de función
  )
  for (patron in patrones) {
    if (grepl(patron, linea, perl = TRUE)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Función para verificar si es un comentario
es_comentario <- function(linea) {
  return(grepl("^\\s*#", linea))
}

# Función para verificar si estamos en sección Markdown (Question/Solution)
# Esta función se llama con el contexto de todas las líneas
en_seccion_markdown <- function(lineas, num_linea) {
  # Buscar hacia atrás la última sección
  for (i in num_linea:1) {
    if (grepl("^(Question|Solution|Answerlist)\\s*$", lineas[i])) {
      return(TRUE)
    }
    if (grepl("^```\\{r", lineas[i])) {
      return(FALSE)  # Estamos en un chunk de R
    }
    if (grepl("^```$", lineas[i]) && i < num_linea) {
      # Buscar si el ``` anterior era apertura o cierre
      for (j in (i-1):1) {
        if (grepl("^```\\{r", lineas[j])) {
          return(TRUE)  # El ``` era cierre, estamos en Markdown
        }
        if (grepl("^```$", lineas[j])) {
          break
        }
      }
    }
  }
  return(FALSE)
}

# Función para detectar y corregir
corregir_archivo <- function(archivo, aplicar_fix = FALSE) {
  if (!file.exists(archivo)) {
    stop(paste("Archivo no encontrado:", archivo))
  }

  # Leer contenido
  contenido <- readLines(archivo, encoding = "UTF-8", warn = FALSE)
  contenido_original <- contenido

  errores_encontrados <- list()

  # Buscar cada palabra del diccionario
  for (i in seq_along(diccionario_tildes)) {
    palabra_mal <- names(diccionario_tildes)[i]
    palabra_bien <- diccionario_tildes[i]

    # Crear patrón que busque la palabra completa
    patron <- paste0("\\b", palabra_mal, "\\b")

    for (num_linea in seq_along(contenido)) {
      linea <- contenido[num_linea]

      # Buscar coincidencias (case insensitive para la búsqueda)
      if (grepl(patron, linea, ignore.case = TRUE, perl = TRUE)) {

        # FILTROS: No corregir en ciertos contextos

        # 1. No corregir metadatos R-exams (DEBEN ser ASCII)
        if (es_metadato_rexams_ascii(linea)) {
          next
        }

        # 2. No corregir nombres de variables R ni código inline
        if (es_nombre_variable(linea, palabra_mal)) {
          next
        }

        # 3. No corregir palabras excluidas en contexto de código R
        if (es_palabra_excluida_en_codigo(linea, palabra_mal)) {
          next
        }

        # 5. No corregir en metadatos YAML genéricos (excepto en valores de texto)
        if (grepl("^\\s*\\w+:", linea) && !grepl(':\\s*["\']', linea)) {
          # Es una clave YAML sin comillas, puede ser identificador
          if (grepl(paste0(":\\s*", palabra_mal, "\\s*$"), linea)) {
            next  # Es un valor de identificador, no corregir
          }
        }

        # 6. Sí corregir: comentarios, strings, y secciones Markdown
        debe_corregir <- FALSE

        if (es_comentario(linea)) {
          debe_corregir <- TRUE  # Comentarios sí deben tener tildes
        } else if (esta_en_string(linea, palabra_mal)) {
          debe_corregir <- TRUE  # Strings sí deben tener tildes
        } else if (en_seccion_markdown(contenido, num_linea)) {
          debe_corregir <- TRUE  # Secciones Markdown sí
        } else if (grepl("^\\s*\\*\\s+", linea)) {
          debe_corregir <- TRUE  # Listas Markdown
        }

        if (!debe_corregir) {
          next
        }

        # Guardar error encontrado
        errores_encontrados[[length(errores_encontrados) + 1]] <- list(
          linea = num_linea,
          texto = linea,
          mal = palabra_mal,
          bien = palabra_bien
        )

        if (aplicar_fix) {
          # Reemplazar preservando mayúsculas/minúsculas del original
          contenido[num_linea] <- gsub(
            paste0("\\b", palabra_mal, "\\b"),
            palabra_bien,
            contenido[num_linea],
            perl = TRUE
          )
          # También manejar versión con primera mayúscula
          palabra_mal_cap <- paste0(toupper(substr(palabra_mal, 1, 1)),
                                    substr(palabra_mal, 2, nchar(palabra_mal)))
          palabra_bien_cap <- paste0(toupper(substr(palabra_bien, 1, 1)),
                                     substr(palabra_bien, 2, nchar(palabra_bien)))
          contenido[num_linea] <- gsub(
            paste0("\\b", palabra_mal_cap, "\\b"),
            palabra_bien_cap,
            contenido[num_linea],
            perl = TRUE
          )
        }
      }
    }
  }

  # Reportar resultados
  if (length(errores_encontrados) > 0) {
    cat("\n========================================\n")
    cat("ERRORES ORTOGRÁFICOS ENCONTRADOS:", length(errores_encontrados), "\n")
    cat("Archivo:", archivo, "\n")
    cat("========================================\n\n")

    for (error in errores_encontrados) {
      cat(sprintf("Línea %d: '%s' → '%s'\n",
                  error$linea, error$mal, error$bien))
      cat(sprintf("  Contexto: %s\n\n",
                  substr(error$texto, 1, min(80, nchar(error$texto)))))
    }

    if (aplicar_fix) {
      # Guardar archivo corregido
      writeLines(contenido, archivo, useBytes = TRUE)
      cat("\n✓ CORRECCIONES APLICADAS Y GUARDADAS\n")
      cat("  Total de correcciones:", length(errores_encontrados), "\n")
    } else {
      cat("\n⚠ Para aplicar correcciones, ejecute con --fix:\n")
      cat(sprintf("  Rscript corregir_ortografia_espanol.R %s --fix\n", archivo))
    }

    return(invisible(FALSE))
  } else {
    cat("\n✓ No se encontraron errores ortográficos en:", archivo, "\n")
    return(invisible(TRUE))
  }
}

# Función para procesar múltiples archivos
corregir_directorio <- function(directorio, patron = "\\.Rmd$", aplicar_fix = FALSE) {
  archivos <- list.files(directorio, pattern = patron,
                         recursive = TRUE, full.names = TRUE)

  if (length(archivos) == 0) {
    cat("No se encontraron archivos .Rmd en:", directorio, "\n")
    return(invisible(NULL))
  }

  cat("Procesando", length(archivos), "archivos...\n\n")

  resultados <- sapply(archivos, function(f) {
    corregir_archivo(f, aplicar_fix)
  })

  # Resumen
  cat("\n========================================\n")
  cat("RESUMEN\n")
  cat("========================================\n")
  cat("Archivos procesados:", length(archivos), "\n")
  cat("Archivos con errores:", sum(!resultados), "\n")
  cat("Archivos correctos:", sum(resultados), "\n")
}

# Ejecución desde línea de comandos
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) == 0) {
    cat("Uso: Rscript corregir_ortografia_espanol.R <archivo.Rmd> [--fix]\n")
    cat("     Rscript corregir_ortografia_espanol.R <directorio> [--fix]\n")
    cat("\nOpciones:\n")
    cat("  --fix    Aplica las correcciones (sin esto solo reporta)\n")
    quit(status = 1)
  }

  objetivo <- args[1]
  aplicar_fix <- "--fix" %in% args

  if (dir.exists(objetivo)) {
    corregir_directorio(objetivo, aplicar_fix = aplicar_fix)
  } else if (file.exists(objetivo)) {
    corregir_archivo(objetivo, aplicar_fix = aplicar_fix)
  } else {
    cat("Error: No se encontró el archivo o directorio:", objetivo, "\n")
    quit(status = 1)
  }
}
