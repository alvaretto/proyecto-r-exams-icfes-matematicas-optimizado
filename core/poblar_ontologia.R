# core/poblar_ontologia.R
# Pobla la ontología Fuseki leyendo YAML de todos los .Rmd en producción.
# Protocolo: Graph Store Protocol (GSP) — HTTP PUT a /icfes/data
#
# Uso:
#   source("core/poblar_ontologia.R")
#   poblar_ontologia()                         # Usa defaults
#   poblar_ontologia(verbose = FALSE)          # Silencioso
#   poblar_ontologia(directorio = "ruta/alt")  # Directorio alternativo

suppressPackageStartupMessages({
  library(httr2)
  library(yaml)
})

FUSEKI_BASE   <- "http://localhost:3030"
GRAPH_URI     <- "http://icfes.matematicas.edu.co/ejercicios"
ONTOLOGIA_IRI <- "http://icfes.matematicas.edu.co/ontologia#"
PENDING_FILE  <- "ontologia/.pending_registration"

# ── Normalización de campos YAML ─────────────────────────────────────────────

normalizar_competencia <- function(comp) {
  if (is.null(comp) || is.na(comp)) return(NULL)
  comp_lower <- tolower(trimws(as.character(comp)))
  comp_lower <- gsub("[áàä]", "a", comp_lower)
  comp_lower <- gsub("[éèë]", "e", comp_lower)
  comp_lower <- gsub("[íìï]", "i", comp_lower)
  comp_lower <- gsub("[óòö]", "o", comp_lower)
  comp_lower <- gsub("[úùü]", "u", comp_lower)
  comp_lower <- gsub("[ñ]",   "n", comp_lower)
  if (grepl("interpretaci|interpret|representaci", comp_lower))
    return(paste0("<", ONTOLOGIA_IRI, "InterpretacionRepresentacion>"))
  if (grepl("formulaci|ejecuci|formula", comp_lower))
    return(paste0("<", ONTOLOGIA_IRI, "FormulacionEjecucion>"))
  if (grepl("argumentaci|argument", comp_lower))
    return(paste0("<", ONTOLOGIA_IRI, "Argumentacion>"))
  if (grepl("resoluci|resolucion|problem", comp_lower))
    return(paste0("<", ONTOLOGIA_IRI, "ResolucionProblemas>"))
  NULL
}

normalizar_componente <- function(comp) {
  if (is.null(comp) || is.na(comp)) return(NULL)
  comp_lower <- tolower(trimws(as.character(comp)))
  comp_lower <- gsub("[áàä]", "a", comp_lower)
  comp_lower <- gsub("[éèë]", "e", comp_lower)
  comp_lower <- gsub("[íìï]", "i", comp_lower)
  comp_lower <- gsub("[óòö]", "o", comp_lower)
  comp_lower <- gsub("[úùü]", "u", comp_lower)
  comp_lower <- gsub("[ñ]",   "n", comp_lower)
  if (grepl("numeric|variaci", comp_lower))
    return(paste0("<", ONTOLOGIA_IRI, "NumericoVariacional>"))
  if (grepl("espaci|metric", comp_lower))
    return(paste0("<", ONTOLOGIA_IRI, "EspacialMetrico>"))
  if (grepl("aleatori|estadistic|probabili|datos", comp_lower))
    return(paste0("<", ONTOLOGIA_IRI, "AleatorioEstadistico>"))
  NULL
}

normalizar_nivel <- function(nivel) {
  if (is.null(nivel) || is.na(nivel)) return(NULL)
  nivel_str <- tolower(trimws(as.character(nivel)))
  if (nivel_str %in% c("1", "basico", "básico"))
    return(paste0("<", ONTOLOGIA_IRI, "DOK1>"))
  if (nivel_str %in% c("2", "medio"))
    return(paste0("<", ONTOLOGIA_IRI, "DOK2>"))
  if (nivel_str %in% c("3", "alto"))
    return(paste0("<", ONTOLOGIA_IRI, "DOK3>"))
  if (nivel_str %in% c("4", "avanzado"))
    return(paste0("<", ONTOLOGIA_IRI, "DOK4>"))
  NULL
}

# ── Operador %||% (null-coalesce) ─────────────────────────────────────────────

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ── Lectura de YAML desde .Rmd ────────────────────────────────────────────────

leer_yaml_rmd <- function(path) {
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) NULL)
  if (is.null(lines) || length(lines) < 3) return(NULL)

  # Buscar seccion Meta-information (formato R-exams real)
  meta_idx <- which(grepl("^Meta-information\\s*$", lines, ignore.case = TRUE))
  if (length(meta_idx) > 0) {
    start_idx <- meta_idx[1] + 1
    # Saltar linea de separadores (===, ---, etc.)
    while (start_idx <= length(lines) &&
           grepl("^[=\\-]{3,}\\s*$", lines[start_idx])) {
      start_idx <- start_idx + 1
    }
    meta_lines <- lines[start_idx:length(lines)]
    meta_lines <- meta_lines[nchar(trimws(meta_lines)) > 0]

    result <- list()
    for (line in meta_lines) {
      parts <- regmatches(line, regexec("^([^:]+):\\s*(.*)", line, perl = TRUE))[[1]]
      if (length(parts) < 3) next
      key   <- trimws(parts[2])
      value <- trimws(parts[3])
      value <- gsub("`r [^`]+`", "", value)
      value <- trimws(value)
      if (nzchar(value)) result[[key]] <- value
    }

    if (length(result) > 0 && !is.null(result[["exname"]])) {
      return(result)
    }
  }

  # Fallback: YAML front matter
  if (!grepl("^---", lines[1])) return(NULL)
  end_idx <- which(grepl("^---", lines[-1]))[1] + 1
  if (is.na(end_idx)) return(NULL)
  yaml_text <- paste(lines[2:(end_idx - 1)], collapse = "\n")
  tryCatch(yaml::yaml.load(yaml_text), error = function(e) NULL)
}

# ── Generación de Turtle para un ejercicio ────────────────────────────────────

ejercicio_a_turtle <- function(meta, ruta_archivo) {
  exname <- meta[["exname"]]
  if (is.null(exname) || nchar(trimws(exname)) == 0) return(NULL)

  # Sanitizar exname para usar como IRI local
  iri_local <- gsub("[^a-zA-Z0-9_\\-]", "_", exname)
  ejercicio_iri <- paste0("<", ONTOLOGIA_IRI, "ej_", iri_local, ">")

  # Campos normalizados
  competencia_iri <- normalizar_competencia(
    meta[["exextra[Competencia]"]] %||% meta[["Competencia"]] %||% meta[["competencia"]]
  )
  componente_iri <- normalizar_componente(
    meta[["exextra[Componente]"]] %||% meta[["Componente"]] %||% meta[["componente"]]
  )
  nivel_iri <- normalizar_nivel(
    meta[["exextra[Nivel]"]] %||% meta[["Nivel"]] %||% meta[["nivel"]]
  )
  bloom_val  <- meta[["exextra[Bloom]"]]  %||% meta[["Bloom"]]
  solo_val   <- meta[["exextra[SOLO]"]]   %||% meta[["SOLO"]]

  # Construir triples
  triples <- character(0)

  triples <- c(triples,
    paste0(ejercicio_iri, " a <", ONTOLOGIA_IRI, "Ejercicio> ;")
  )

  ruta_escaped <- gsub('"', '\\\\"', ruta_archivo)
  triples <- c(triples,
    paste0('  <', ONTOLOGIA_IRI, 'rutaArchivo> "', ruta_escaped, '" ;')
  )

  if (!is.null(competencia_iri))
    triples <- c(triples, paste0("  <", ONTOLOGIA_IRI, "tieneCompetencia> ", competencia_iri, " ;"))
  if (!is.null(componente_iri))
    triples <- c(triples, paste0("  <", ONTOLOGIA_IRI, "tieneComponente> ", componente_iri, " ;"))
  if (!is.null(nivel_iri))
    triples <- c(triples, paste0("  <", ONTOLOGIA_IRI, "tieneNivel> ", nivel_iri, " ;"))
  if (!is.null(bloom_val) && !is.na(bloom_val))
    triples <- c(triples, paste0('  <', ONTOLOGIA_IRI, 'nivelBloom> "', bloom_val, '" ;'))
  if (!is.null(solo_val) && !is.na(solo_val))
    triples <- c(triples, paste0('  <', ONTOLOGIA_IRI, 'nivelSOLO> "', solo_val, '" ;'))

  # Terminar el último triple correctamente (reemplazar ; por .)
  n <- length(triples)
  triples[n] <- sub(" ;$", " .", triples[n])

  paste(triples, collapse = "\n")
}

# ── Operaciones Fuseki ────────────────────────────────────────────────────────

fuseki_ping <- function() {
  resp <- tryCatch(
    request(FUSEKI_BASE) |>
      req_url_path("/$/ping") |>
      req_timeout(3) |>
      req_perform(),
    error = function(e) NULL
  )
  !is.null(resp) && resp_status(resp) == 200
}

fuseki_cargar_turtle <- function(turtle_text, graph_uri = GRAPH_URI) {
  resp <- tryCatch(
    request(FUSEKI_BASE) |>
      req_url_path("/icfes/data") |>
      req_url_query("graph" = graph_uri) |>
      req_method("PUT") |>
      req_body_raw(turtle_text, type = "text/turtle; charset=utf-8") |>
      req_timeout(30) |>
      req_perform(),
    error = function(e) {
      message("Error al conectar con Fuseki: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(resp)) return(FALSE)
  status <- resp_status(resp)
  if (status %in% c(200L, 201L, 204L)) return(TRUE)
  message("Fuseki respondio con status ", status)
  FALSE
}

# ── Función principal ─────────────────────────────────────────────────────────

poblar_ontologia <- function(directorio = "A-Produccion/03-En-Produccion",
                              verbose = TRUE) {
  if (!fuseki_ping()) {
    message("Fuseki no esta disponible en ", FUSEKI_BASE)
    message("Inicie Fuseki: systemctl --user start fuseki.service")
    return(invisible(FALSE))
  }

  archivos <- list.files(
    path       = directorio,
    pattern    = "\\.Rmd$",
    recursive  = TRUE,
    full.names = TRUE
  )

  if (length(archivos) == 0) {
    message("No se encontraron archivos .Rmd en: ", directorio)
    return(invisible(FALSE))
  }

  if (verbose) message("Procesando ", length(archivos), " archivos .Rmd...")

  # Prefijos Turtle para el grafo completo
  prefixes <- paste0(
    '@prefix :     <', ONTOLOGIA_IRI, '> .\n',
    '@prefix owl:  <http://www.w3.org/2002/07/owl#> .\n',
    '@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n',
    '@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .\n\n'
  )

  triples_all <- character(0)
  n_ok <- 0L
  n_skip <- 0L

  for (ruta in archivos) {
    meta <- leer_yaml_rmd(ruta)
    if (is.null(meta)) { n_skip <- n_skip + 1L; next }
    turtle_bloque <- ejercicio_a_turtle(meta, ruta)
    if (is.null(turtle_bloque)) { n_skip <- n_skip + 1L; next }
    triples_all <- c(triples_all, turtle_bloque)
    n_ok <- n_ok + 1L
  }

  if (length(triples_all) == 0) {
    message("No se generaron triples. Verifique el formato YAML de los .Rmd.")
    return(invisible(FALSE))
  }

  turtle_completo <- paste0(prefixes, paste(triples_all, collapse = "\n\n"))

  # Guardar localmente
  dir.create("ontologia", showWarnings = FALSE)
  writeLines(turtle_completo, "ontologia/ejercicios.ttl")
  if (verbose) message("Guardado: ontologia/ejercicios.ttl")

  # Subir a Fuseki
  ok <- fuseki_cargar_turtle(turtle_completo)
  if (ok) {
    if (verbose) {
      message("Ontologia cargada en Fuseki")
      message("   Ejercicios procesados: ", n_ok)
      message("   Omitidos (sin YAML valido): ", n_skip)
    }
    # Drenar pending queue si existe
    if (file.exists(PENDING_FILE)) {
      file.remove(PENDING_FILE)
      if (verbose) message("pending_registration limpiado")
    }
    return(invisible(TRUE))
  } else {
    message("Error al cargar en Fuseki")
    return(invisible(FALSE))
  }
}
