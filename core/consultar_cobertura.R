# core/consultar_cobertura.R
# Consultas SPARQL para análisis de cobertura de la ontología ICFES.
# Requiere Fuseki activo en localhost:3030
#
# Uso:
#   source("core/consultar_cobertura.R")
#   cobertura_por_competencia()
#   cobertura_por_nivel()
#   cobertura_competencia_x_nivel()
#   brechas_conceptos()

suppressPackageStartupMessages(library(httr2))

SPARQL_ENDPOINT <- "http://localhost:3030/icfes/sparql"
ONTOLOGIA_IRI   <- "http://icfes.matematicas.edu.co/ontologia#"

# ── Función base de consulta SPARQL ─────────────────────────────────────────

sparql_query <- function(query) {
  resp <- tryCatch(
    request(SPARQL_ENDPOINT) |>
      req_method("POST") |>
      req_body_form(query = query) |>
      req_headers(Accept = "application/sparql-results+json") |>
      req_timeout(10) |>
      req_perform(),
    error = function(e) {
      message("Error al conectar con Fuseki: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(resp)) return(NULL)
  if (resp_status(resp) != 200L) {
    message("SPARQL respondió con status ", resp_status(resp))
    return(NULL)
  }
  result <- tryCatch(resp_body_json(resp), error = function(e) NULL)
  if (is.null(result)) return(NULL)

  # Convertir JSON SPARQL a data.frame
  vars    <- result$head$vars
  bindings <- result$results$bindings
  if (length(bindings) == 0) return(data.frame())

  rows <- lapply(bindings, function(row) {
    vals <- lapply(vars, function(v) {
      if (!is.null(row[[v]])) row[[v]]$value else NA_character_
    })
    names(vals) <- vars
    as.data.frame(vals, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# ── Helpers ──────────────────────────────────────────────────────────────────

# Extrae el nombre local de un IRI (parte después del #)
iri_local <- function(iri) {
  sub(".*#", "", iri)
}

# ── Consultas de cobertura ────────────────────────────────────────────────────

#' Cuenta ejercicios por competencia ICFES
cobertura_por_competencia <- function() {
  q <- sprintf('
PREFIX : <%s>
SELECT ?competencia (COUNT(?ej) AS ?n_ejercicios)
WHERE {
  ?ej a :Ejercicio .
  OPTIONAL { ?ej :tieneCompetencia ?competencia }
}
GROUP BY ?competencia
ORDER BY DESC(?n_ejercicios)
', ONTOLOGIA_IRI)

  df <- sparql_query(q)
  if (is.null(df) || nrow(df) == 0) {
    message("Sin resultados. ¿Está Fuseki activo y la ontología cargada?")
    return(invisible(NULL))
  }
  df$competencia <- ifelse(is.na(df$competencia), "(sin competencia)",
                           iri_local(df$competencia))
  df$n_ejercicios <- as.integer(df$n_ejercicios)
  df
}

#' Cuenta ejercicios por nivel DOK
cobertura_por_nivel <- function() {
  q <- sprintf('
PREFIX : <%s>
SELECT ?nivel (COUNT(?ej) AS ?n_ejercicios)
WHERE {
  ?ej a :Ejercicio .
  OPTIONAL { ?ej :tieneNivel ?nivel }
}
GROUP BY ?nivel
ORDER BY ?nivel
', ONTOLOGIA_IRI)

  df <- sparql_query(q)
  if (is.null(df) || nrow(df) == 0) {
    message("Sin resultados.")
    return(invisible(NULL))
  }
  df$nivel <- ifelse(is.na(df$nivel), "(sin nivel)", iri_local(df$nivel))
  df$n_ejercicios <- as.integer(df$n_ejercicios)
  df
}

#' Tabla cruzada competencia × nivel DOK
cobertura_competencia_x_nivel <- function() {
  q <- sprintf('
PREFIX : <%s>
SELECT ?competencia ?nivel (COUNT(?ej) AS ?n_ejercicios)
WHERE {
  ?ej a :Ejercicio .
  OPTIONAL { ?ej :tieneCompetencia ?competencia }
  OPTIONAL { ?ej :tieneNivel ?nivel }
}
GROUP BY ?competencia ?nivel
ORDER BY ?competencia ?nivel
', ONTOLOGIA_IRI)

  df <- sparql_query(q)
  if (is.null(df) || nrow(df) == 0) {
    message("Sin resultados.")
    return(invisible(NULL))
  }
  df$competencia  <- ifelse(is.na(df$competencia),  "(sin competencia)",  iri_local(df$competencia))
  df$nivel        <- ifelse(is.na(df$nivel),        "(sin nivel)",        iri_local(df$nivel))
  df$n_ejercicios <- as.integer(df$n_ejercicios)
  df
}

#' Conceptos sin cobertura (ningún ejercicio los cubre)
brechas_conceptos <- function() {
  q <- sprintf('
PREFIX : <%s>
SELECT ?concepto ?label
WHERE {
  ?concepto a :ConceptoMatematico .
  OPTIONAL { ?concepto <http://www.w3.org/2000/01/rdf-schema#label> ?label }
  FILTER NOT EXISTS { ?ej a :Ejercicio ; :cubreConcepto ?concepto }
}
ORDER BY ?label
', ONTOLOGIA_IRI)

  df <- sparql_query(q)
  if (is.null(df) || nrow(df) == 0) {
    message("Todos los conceptos tienen cobertura (o la ontología no está cargada).")
    return(invisible(data.frame()))
  }
  df$concepto <- iri_local(df$concepto)
  df
}
