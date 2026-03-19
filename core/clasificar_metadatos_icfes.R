#!/usr/bin/env Rscript
# clasificar_metadatos_icfes.R
# Agrega metadatos ICFES faltantes (exextra[Competencia], etc.) a ejercicios .Rmd
# Infiere la clasificacion desde la ruta del directorio y el nombre del archivo.
#
# Uso:
#   Rscript core/clasificar_metadatos_icfes.R              # Dry-run (solo reporta)
#   Rscript core/clasificar_metadatos_icfes.R --apply      # Aplica los cambios

# --- Configuracion ---
BASE_DIR <- "A-Produccion/03-En-Produccion"
SKIP_PATTERNS <- c("Ejemplos-Funcionales-Rmd", "Plantillas", "/salida/", "/rsconnect/")

args <- commandArgs(trailingOnly = TRUE)
APPLY <- "--apply" %in% args

# --- Funciones de inferencia ---

inferir_componente <- function(ruta) {
  if (grepl("01-Numeros-Reales|02-Funciones|03-Algebra|04-Aritmetica", ruta))
    return("Numerico-Variacional")
  if (grepl("05-Geometr", ruta))
    return("Espacial-Metrico")
  if (grepl("06-Estad", ruta))
    return("Aleatorio")
  return(NA_character_)
}

inferir_competencia <- function(ruta, nombre, meta_existente) {
  # Primero intentar desde el nombre del archivo (mas preciso)
  nombre_lower <- tolower(nombre)
  ruta_lower <- tolower(ruta)

  if (grepl("argumentacion", nombre_lower)) return("Argumentacion")
  if (grepl("interpretacion|representacion", nombre_lower)) return("Interpretacion")
  if (grepl("formulacion|ejecucion", nombre_lower)) return("Formulacion")
  if (grepl("resolucion", nombre_lower)) return("Resolucion")

  # Intentar desde exsection existente
  exsection <- meta_existente[["exsection"]]
  if (!is.null(exsection)) {
    exs_lower <- tolower(exsection)
    if (grepl("interpretaci|representaci", exs_lower)) return("Interpretacion")
    if (grepl("formulaci|ejecuci", exs_lower)) return("Formulacion")
    if (grepl("argumentaci", exs_lower)) return("Argumentacion")
    if (grepl("resoluci|problem", exs_lower)) return("Resolucion")
  }

  # Intentar desde exextra[Type] existente (algunos usan Type para competencia)
  tipo <- meta_existente[["exextra[Type]"]]
  if (!is.null(tipo)) {
    tipo_lower <- tolower(tipo)
    if (grepl("interpretaci|representaci", tipo_lower)) return("Interpretacion")
    if (grepl("formulaci|ejecuci", tipo_lower)) return("Formulacion")
    if (grepl("argumentaci", tipo_lower)) return("Argumentacion")
  }

  # Intentar desde nombre del directorio padre
  if (grepl("interpretacion|representacion", ruta_lower)) return("Interpretacion")
  if (grepl("formulacion|ejecucion", ruta_lower)) return("Formulacion")
  if (grepl("argumentacion", ruta_lower)) return("Argumentacion")

  # Fallback: inferir desde el tema del ejercicio
  # Ejercicios de graficos/diagramas/tablas → Interpretacion
  if (grepl("accidentalidad|adopcion|mascotas|exportacion|pasteleria|poblacion|grafic", ruta_lower))
    return("Interpretacion")
  # Ejercicios de variacion lineal con graficas → Interpretacion
  if (grepl("variaci.*lineal|vuelo.*acrobatico|auto.*viajero", ruta_lower))
    return("Interpretacion")
  # Ejercicios de calculo directo (mediana, fracciones, probabilidad) → Formulacion
  if (grepl("fracci|reparto|bolas.*colores|confint", ruta_lower))
    return("Formulacion")
  if (grepl("mediana.*farmac|moda.*farmac", ruta_lower))
    return("Formulacion")
  # Diagramas de Venn → Interpretacion (requieren leer el diagrama)
  if (grepl("venn|generos.*musicales", ruta_lower))
    return("Interpretacion")

  return(NA_character_)
}

inferir_nivel <- function(nombre, meta_existente) {
  nombre_lower <- tolower(nombre)

  # Desde nombre: _n1_, _n2_, Nivel2, etc.
  if (grepl("_n1_|_n1\\.|nivel.?1", nombre_lower)) return("1")
  if (grepl("_n2_|_n2\\.|nivel.?2", nombre_lower)) return("2")
  if (grepl("_n3_|_n3\\.|nivel.?3", nombre_lower)) return("3")
  if (grepl("_n4_|_n4\\.|nivel.?4", nombre_lower)) return("4")

  # Desde exextra[Level] existente
  level <- meta_existente[["exextra[Level]"]]
  if (!is.null(level)) return(as.character(level))

  # Default: Nivel 2 (el mas comun en ejercicios ICFES de este repositorio)
  return("2")
}

inferir_tipo <- function(meta_existente) {
  extype <- meta_existente[["extype"]]
  if (is.null(extype)) return(NA_character_)
  extype_clean <- trimws(gsub("`r.*`", "", extype))
  switch(tolower(extype_clean),
    "schoice" = "SCHOICE",
    "cloze" = "CLOZE",
    "mchoice" = "MCHOICE",
    "num" = "NUM",
    toupper(extype_clean)
  )
}

inferir_afirmacion <- function(ruta, componente) {
  ruta_lower <- tolower(ruta)
  # Generar afirmacion basada en el tema del directorio
  if (grepl("mediana", ruta_lower))
    return("Interpreta y calcula la mediana de un conjunto de datos")
  if (grepl("media|promedio", ruta_lower))
    return("Interpreta y calcula la media de un conjunto de datos")
  if (grepl("moda", ruta_lower))
    return("Identifica la moda en un conjunto de datos")
  if (grepl("tendencia.central|01-mediamedianamoda", ruta_lower))
    return("Compara e interpreta medidas de tendencia central")
  if (grepl("diagrama.*caja|boxplot|medidas.*posici", ruta_lower))
    return("Interpreta diagramas de caja y medidas de posicion")
  if (grepl("dispersi", ruta_lower))
    return("Calcula e interpreta medidas de dispersion")
  if (grepl("probabilidad.*condicion", ruta_lower))
    return("Calcula probabilidades condicionadas e independencia de sucesos")
  if (grepl("probabilidad|conteo|aditivo|multiplicativo", ruta_lower))
    return("Aplica principios de conteo y calcula probabilidades")
  if (grepl("venn", ruta_lower))
    return("Interpreta diagramas de Venn y operaciones entre conjuntos")
  if (grepl("frecuencia|cualitativa|cuantitativa", ruta_lower))
    return("Interpreta distribuciones de frecuencia y representaciones graficas")
  if (grepl("lineal|exponencial|variaci|funcion", ruta_lower))
    return("Interpreta y analiza funciones y razones de cambio")
  if (grepl("fracci|reparto", ruta_lower))
    return("Resuelve problemas con fracciones y repartos proporcionales")
  if (grepl("conversi.*unidad|area.*formulacion", ruta_lower))
    return("Resuelve problemas de conversion de unidades y areas")
  if (grepl("gr.fic.*estad|exportacion", ruta_lower))
    return("Interpreta graficos estadisticos y extrae conclusiones")
  if (grepl("accidentalidad|vial", ruta_lower))
    return("Interpreta datos de frecuencia en contextos reales")
  return("Resuelve problemas matematicos en contextos reales")
}

inferir_evidencia <- function(ruta, competencia) {
  ruta_lower <- tolower(ruta)
  comp <- if (is.null(competencia) || is.na(competencia)) "Interpretacion" else competencia

  # Evidencias basadas en competencia + tema
  if (comp == "Interpretacion") {
    if (grepl("gr.fic|diagrama|caja|barras|lineas", ruta_lower))
      return("Lee e interpreta informacion presentada en graficos y tablas")
    return("Interpreta y compara datos presentados en diferentes representaciones")
  }
  if (comp == "Formulacion") {
    if (grepl("mediana|media|moda", ruta_lower))
      return("Selecciona y aplica procedimientos para calcular medidas estadisticas")
    return("Plantea y ejecuta procedimientos para resolver problemas matematicos")
  }
  if (comp == "Argumentacion") {
    return("Justifica propiedades y relaciones matematicas usando argumentos validos")
  }
  return("Establece relaciones entre datos y representaciones matematicas")
}

# --- Leer Meta-information de un .Rmd ---

leer_meta <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)
  # Buscar seccion Meta-information
  meta_start <- grep("^Meta-information", lines)
  if (length(meta_start) == 0) return(list(meta = list(), start = NA, end = NA))

  # La linea de separacion (=====) esta justo despues
  sep_line <- meta_start[1] + 1

  # Leer campos hasta el final o hasta un chunk de codigo
  meta <- list()
  i <- sep_line + 1
  end_line <- length(lines)
  while (i <= length(lines)) {
    line <- lines[i]
    # Detener si encontramos chunk R o linea vacia seguida de chunk
    if (grepl("^```\\{r", line)) {
      end_line <- i - 1
      break
    }
    if (trimws(line) == "" && i < length(lines) && grepl("^```\\{r", lines[i + 1])) {
      end_line <- i - 1
      break
    }
    # Parsear campo: key: value
    if (grepl("^[a-zA-Z]", line)) {
      parts <- regmatches(line, regexec("^([^:]+):\\s*(.*)", line))[[1]]
      if (length(parts) == 3) {
        key <- trimws(parts[2])
        val <- trimws(parts[3])
        meta[[key]] <- val
      }
    }
    if (trimws(line) == "" && i > sep_line + 5) {
      end_line <- i
      break
    }
    i <- i + 1
    end_line <- i
  }

  list(meta = meta, start = meta_start[1], sep = sep_line, end = min(end_line, length(lines)))
}

# --- Procesar un archivo ---

procesar_archivo <- function(filepath) {
  nombre <- basename(filepath)
  info <- leer_meta(filepath)
  meta <- info$meta

  if (is.na(info$start)) {
    return(list(
      archivo = filepath,
      estado = "SIN_META_SECTION",
      cambios = character(0)
    ))
  }

  # Verificar que campos faltan
  tiene_competencia <- !is.null(meta[["exextra[Competencia]"]])
  tiene_componente <- !is.null(meta[["exextra[Componente]"]])
  tiene_nivel <- !is.null(meta[["exextra[Nivel]"]])
  tiene_tipo <- !is.null(meta[["exextra[Type]"]])
  tiene_afirmacion <- !is.null(meta[["exextra[Afirmacion]"]])
  tiene_evidencia <- !is.null(meta[["exextra[Evidencia]"]])

  if (tiene_competencia && tiene_componente && tiene_nivel &&
      tiene_tipo && tiene_afirmacion && tiene_evidencia) {
    return(list(
      archivo = filepath,
      estado = "COMPLETO",
      cambios = character(0)
    ))
  }

  # Inferir campos faltantes
  cambios <- list()

  if (!tiene_componente) {
    val <- inferir_componente(filepath)
    if (!is.na(val)) cambios[["exextra[Componente]"]] <- val
  }

  if (!tiene_competencia) {
    val <- inferir_competencia(filepath, nombre, meta)
    if (!is.na(val)) cambios[["exextra[Competencia]"]] <- val
  }

  if (!tiene_nivel) {
    val <- inferir_nivel(nombre, meta)
    if (!is.na(val)) cambios[["exextra[Nivel]"]] <- val
  }

  if (!tiene_tipo) {
    val <- inferir_tipo(meta)
    if (!is.na(val)) cambios[["exextra[Type]"]] <- val
  }

  if (!tiene_afirmacion) {
    comp <- cambios[["exextra[Competencia]"]]
    if (is.null(comp)) comp <- meta[["exextra[Competencia]"]]
    val <- inferir_afirmacion(filepath, comp)
    cambios[["exextra[Afirmacion]"]] <- val
  }

  if (!tiene_evidencia) {
    comp <- cambios[["exextra[Competencia]"]]
    if (is.null(comp)) comp <- meta[["exextra[Competencia]"]]
    val <- inferir_evidencia(filepath, comp)
    cambios[["exextra[Evidencia]"]] <- val
  }

  # Campos que no se pudieron inferir
  no_inferidos <- character(0)
  for (campo in c("exextra[Competencia]", "exextra[Componente]", "exextra[Nivel]")) {
    if (!tiene_competencia && campo == "exextra[Competencia]" && is.null(cambios[[campo]]))
      no_inferidos <- c(no_inferidos, campo)
    if (!tiene_componente && campo == "exextra[Componente]" && is.null(cambios[[campo]]))
      no_inferidos <- c(no_inferidos, campo)
    if (!tiene_nivel && campo == "exextra[Nivel]" && is.null(cambios[[campo]]))
      no_inferidos <- c(no_inferidos, campo)
  }

  list(
    archivo = filepath,
    estado = if (length(no_inferidos) > 0) "PARCIAL" else "INFERIDO",
    cambios = cambios,
    no_inferidos = no_inferidos,
    meta_start = info$start,
    meta_sep = info$sep,
    meta_end = info$end
  )
}

# --- Aplicar cambios a un archivo ---

aplicar_cambios <- function(filepath, resultado) {
  if (length(resultado$cambios) == 0) return(FALSE)

  lines <- readLines(filepath, warn = FALSE)

  # Encontrar la ultima linea de metadatos existentes (antes de chunk o EOF)
  # Insertar las nuevas lineas justo despues del ultimo campo existente
  meta_start <- resultado$meta_start
  sep_line <- resultado$meta_sep

  # Encontrar la ultima linea con un campo meta valido
  last_meta_line <- sep_line
  for (i in (sep_line + 1):min(length(lines), sep_line + 30)) {
    if (grepl("^(ex[a-z]|exextra)", lines[i])) {
      last_meta_line <- i
    } else if (trimws(lines[i]) == "") {
      # Linea vacia - podria ser fin de meta
      break
    } else if (grepl("^```\\{r", lines[i])) {
      break
    }
  }

  # Generar lineas nuevas
  nuevas_lineas <- character(0)
  for (campo in names(resultado$cambios)) {
    nuevas_lineas <- c(nuevas_lineas, paste0(campo, ": ", resultado$cambios[[campo]]))
  }

  # Insertar despues del ultimo campo
  lines_antes <- lines[1:last_meta_line]
  lines_despues <- if (last_meta_line < length(lines)) lines[(last_meta_line + 1):length(lines)] else character(0)

  lines_nuevas <- c(lines_antes, nuevas_lineas, lines_despues)

  writeLines(lines_nuevas, filepath)
  return(TRUE)
}

# --- Main ---

cat("=== Clasificador Automatico de Metadatos ICFES ===\n")
cat("Modo:", if (APPLY) "APLICAR CAMBIOS" else "DRY-RUN (solo reportar)", "\n\n")

# Buscar archivos sin exextra[Competencia]
all_rmd <- list.files(BASE_DIR, pattern = "\\.Rmd$", recursive = TRUE, full.names = TRUE)

# Filtrar
rmd_filtrados <- all_rmd
for (pat in SKIP_PATTERNS) {
  rmd_filtrados <- rmd_filtrados[!grepl(pat, rmd_filtrados)]
}

# Filtrar copias
rmd_filtrados <- rmd_filtrados[!grepl("^Copia de ", basename(rmd_filtrados))]

cat("Archivos .Rmd en produccion (sin plantillas/copias):", length(rmd_filtrados), "\n")

# Procesar cada archivo
resultados <- lapply(rmd_filtrados, procesar_archivo)

# Clasificar resultados
completos <- Filter(function(r) r$estado == "COMPLETO", resultados)
inferidos <- Filter(function(r) r$estado == "INFERIDO", resultados)
parciales <- Filter(function(r) r$estado == "PARCIAL", resultados)
sin_meta <- Filter(function(r) r$estado == "SIN_META_SECTION", resultados)

cat("\n--- RESUMEN ---\n")
cat("Ya completos:", length(completos), "\n")
cat("Inferidos automaticamente:", length(inferidos), "\n")
cat("Parciales (falta info):", length(parciales), "\n")
cat("Sin seccion Meta-information:", length(sin_meta), "\n")

if (length(inferidos) > 0) {
  cat("\n--- EJERCICIOS AUTO-CLASIFICADOS ---\n")
  for (r in inferidos) {
    cat("\n", r$archivo, "\n")
    for (campo in names(r$cambios)) {
      cat("  +", campo, ":", r$cambios[[campo]], "\n")
    }
  }
}

if (length(parciales) > 0) {
  cat("\n--- EJERCICIOS PARCIALES (requieren revision manual) ---\n")
  for (r in parciales) {
    cat("\n", r$archivo, "\n")
    for (campo in names(r$cambios)) {
      cat("  +", campo, ":", r$cambios[[campo]], "\n")
    }
    cat("  !", "No inferidos:", paste(r$no_inferidos, collapse = ", "), "\n")
  }
}

if (length(sin_meta) > 0) {
  cat("\n--- SIN SECCION META-INFORMATION ---\n")
  for (r in sin_meta) {
    cat(" ", r$archivo, "\n")
  }
}

# Aplicar cambios si --apply
if (APPLY) {
  cat("\n--- APLICANDO CAMBIOS ---\n")
  n_aplicados <- 0
  for (r in c(inferidos, parciales)) {
    if (length(r$cambios) > 0) {
      exito <- aplicar_cambios(r$archivo, r)
      if (exito) {
        n_aplicados <- n_aplicados + 1
        cat("  OK:", r$archivo, "\n")
      } else {
        cat("  FALLO:", r$archivo, "\n")
      }
    }
  }
  cat("\nTotal modificados:", n_aplicados, "\n")
} else {
  n_pendientes <- length(inferidos) + length(parciales)
  if (n_pendientes > 0) {
    cat("\n--- Para aplicar cambios, ejecutar: ---\n")
    cat("Rscript core/clasificar_metadatos_icfes.R --apply\n")
  }
}
