# core/visualizar_grafo.R
# Genera un grafo interactivo de conceptos matemáticos usando visNetwork.
# Color coding: rojo = sin ejercicios, amarillo = 1-2, verde = 3+
#
# Uso:
#   source("core/visualizar_grafo.R")
#   visualizar_grafo()                # Abre en navegador
#   visualizar_grafo(guardar = TRUE)  # Guarda como HTML

suppressPackageStartupMessages({
  library(httr2)
  library(visNetwork)
})

SPARQL_ENDPOINT <- "http://localhost:3030/icfes/sparql"
ONTOLOGIA_IRI   <- "http://icfes.matematicas.edu.co/ontologia#"

# Reutilizar sparql_query si ya está cargado, o definir una local
if (!exists("sparql_query")) {
  source("core/consultar_cobertura.R", local = FALSE)
}

# ── Consultas ────────────────────────────────────────────────────────────────

#' Obtiene todos los conceptos con su cantidad de ejercicios
obtener_conceptos_cobertura <- function() {
  q <- sprintf('
PREFIX : <%s>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?concepto ?label (COUNT(?ej) AS ?n_ejercicios)
WHERE {
  ?concepto a :ConceptoMatematico .
  OPTIONAL { ?concepto rdfs:label ?label }
  OPTIONAL { ?ej a :Ejercicio ; :cubreConcepto ?concepto }
}
GROUP BY ?concepto ?label
', ONTOLOGIA_IRI)
  sparql_query(q)
}

#' Obtiene todas las relaciones esPrerequisitoDe
obtener_prerequisitos <- function() {
  q <- sprintf('
PREFIX : <%s>
SELECT ?origen ?destino
WHERE {
  ?origen :esPrerequisitoDe ?destino .
}
', ONTOLOGIA_IRI)
  sparql_query(q)
}

# ── Color coding ─────────────────────────────────────────────────────────────

color_por_cobertura <- function(n) {
  n <- as.integer(n)
  if (is.na(n) || n == 0) return(list(background = "#FF6B6B", border = "#C0392B"))  # rojo
  if (n <= 2)             return(list(background = "#F1C40F", border = "#F39C12"))  # amarillo
  return(list(background = "#2ECC71", border = "#27AE60"))                           # verde
}

# ── Función principal ─────────────────────────────────────────────────────────

#' Genera y muestra el grafo interactivo de conceptos matemáticos.
#'
#' @param guardar Logical. Si TRUE, guarda como HTML en docs/grafo-conceptos.html
#' @param titulo  Título del grafo
visualizar_grafo <- function(guardar = FALSE,
                              titulo  = "Mapa de Conceptos Matemáticos ICFES") {

  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    stop("Instalar visNetwork: install.packages('visNetwork')")
  }

  # Obtener datos de Fuseki
  df_conceptos <- obtener_conceptos_cobertura()
  df_prereqs   <- obtener_prerequisitos()

  if (is.null(df_conceptos) || nrow(df_conceptos) == 0) {
    message("Sin datos. Verifique que Fuseki esté activo y la ontología cargada.")
    message("Iniciar: systemctl --user start fuseki.service")
    message("Cargar:  source('core/poblar_ontologia.R'); poblar_ontologia()")
    return(invisible(NULL))
  }

  # Extraer nombre local del IRI
  iri_local <- function(iri) sub(".*#", "", iri)

  # ── Nodos ──────────────────────────────────────────────────────────────────
  nodos <- data.frame(
    id    = iri_local(df_conceptos$concepto),
    label = ifelse(is.na(df_conceptos$label) | df_conceptos$label == "",
                   iri_local(df_conceptos$concepto),
                   df_conceptos$label),
    title = paste0(
      "<b>", ifelse(is.na(df_conceptos$label), iri_local(df_conceptos$concepto),
                    df_conceptos$label), "</b><br>",
      "Ejercicios: ", df_conceptos$n_ejercicios
    ),
    value       = pmax(as.integer(df_conceptos$n_ejercicios), 1L),
    n_ejercicios = as.integer(df_conceptos$n_ejercicios),
    stringsAsFactors = FALSE
  )

  # Aplicar color coding
  colores <- lapply(nodos$n_ejercicios, color_por_cobertura)
  nodos$color.background <- sapply(colores, `[[`, "background")
  nodos$color.border     <- sapply(colores, `[[`, "border")
  nodos$font.size        <- 12L
  nodos$shape            <- "ellipse"

  # ── Aristas ────────────────────────────────────────────────────────────────
  aristas <- NULL
  if (!is.null(df_prereqs) && nrow(df_prereqs) > 0) {
    aristas <- data.frame(
      from   = iri_local(df_prereqs$origen),
      to     = iri_local(df_prereqs$destino),
      arrows = "to",
      color  = list(color = "#95A5A6", highlight = "#2C3E50"),
      stringsAsFactors = FALSE
    )
    # Filtrar aristas cuyo origen o destino no está en los nodos
    aristas <- aristas[
      aristas$from %in% nodos$id & aristas$to %in% nodos$id,
    ]
  }

  # ── Leyenda ────────────────────────────────────────────────────────────────
  leyenda <- data.frame(
    label = c("Sin ejercicios", "1-2 ejercicios", "3+ ejercicios"),
    color = c("#FF6B6B",        "#F1C40F",         "#2ECC71"),
    stringsAsFactors = FALSE
  )

  # ── Opciones de visualización ─────────────────────────────────────────────
  opciones <- list(
    physics = list(
      enabled      = TRUE,
      stabilization = list(enabled = TRUE, iterations = 200),
      barnesHut    = list(gravitationalConstant = -3000, springLength = 150)
    ),
    layout  = list(improvedLayout = TRUE),
    edges   = list(smooth = list(type = "curvedCW", roundness = 0.2)),
    nodes   = list(borderWidth = 2),
    interaction = list(
      hover       = TRUE,
      tooltipDelay = 100,
      navigationButtons = TRUE,
      keyboard = TRUE
    )
  )

  # ── Construir grafo ───────────────────────────────────────────────────────
  grafo <- visNetwork(nodos,
                      if (!is.null(aristas) && nrow(aristas) > 0) aristas else data.frame(),
                      main = titulo,
                      width = "100%", height = "700px") |>
    visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
      nodesIdSelection = list(enabled = TRUE, useLabels = TRUE)
    ) |>
    visLegend(
      addNodes = lapply(seq_len(nrow(leyenda)), function(i) {
        list(label = leyenda$label[i],
             color = leyenda$color[i],
             shape = "ellipse")
      }),
      useGroups = FALSE
    )

  if (guardar) {
    dir.create("docs", showWarnings = FALSE)
    ruta_html <- "docs/grafo-conceptos.html"
    visNetwork::visSave(grafo, file = ruta_html)
    message("✅ Grafo guardado en: ", ruta_html)
    return(invisible(ruta_html))
  }

  grafo  # Abre en Viewer/navegador
}
