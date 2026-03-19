#!/usr/bin/env Rscript
# core/registrar_ejercicio.R
# Registra UN archivo .Rmd individual en Fuseki.
# Uso: Rscript core/registrar_ejercicio.R ruta/al/archivo.Rmd
#
# Si Fuseki no está disponible, encola en ontologia/.pending_registration

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  cat("Uso: Rscript core/registrar_ejercicio.R <archivo.Rmd>\n")
  quit(status = 1)
}

ruta <- args[1]
if (!file.exists(ruta)) {
  cat("Error: archivo no encontrado:", ruta, "\n")
  quit(status = 1)
}

# Cargar funciones de ontología
script_dir <- normalizePath(dirname(sys.frame(1)$ofile), mustWork = FALSE)
if (!nzchar(script_dir) || is.na(script_dir)) {
  script_dir <- "core"
}
source(file.path(script_dir, "poblar_ontologia.R"), local = TRUE)

# Registrar el ejercicio individual
registrar_un_ejercicio <- function(ruta_rmd) {
  if (!fuseki_ping()) {
    # Encolar para registro posterior
    dir.create("ontologia", showWarnings = FALSE)
    pending <- PENDING_FILE
    cat(ruta_rmd, "\n", file = pending, append = TRUE)
    cat("⚠️  Fuseki no disponible. Ejercicio encolado:", ruta_rmd, "\n")
    return(invisible(FALSE))
  }

  meta <- leer_yaml_rmd(ruta_rmd)
  if (is.null(meta)) {
    cat("⚠️  No se pudo leer YAML de:", ruta_rmd, "\n")
    return(invisible(FALSE))
  }

  turtle_bloque <- ejercicio_a_turtle(meta, ruta_rmd)
  if (is.null(turtle_bloque)) {
    cat("⚠️  No se generaron triples para:", ruta_rmd, "\n")
    return(invisible(FALSE))
  }

  prefixes <- paste0(
    "@prefix :     <http://icfes.matematicas.edu.co/ontologia#> .\n",
    "@prefix owl:  <http://www.w3.org/2002/07/owl#> .\n",
    "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n",
    "@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .\n\n"
  )

  # POST (agregar) al grafo existente usando content-type text/turtle
  # Usamos POST para no reemplazar el grafo completo (a diferencia de PUT)
  resp <- tryCatch(
    httr2::request(FUSEKI_BASE) |>
      httr2::req_url_path("/icfes/data") |>
      httr2::req_url_query("graph" = GRAPH_URI) |>
      httr2::req_method("POST") |>
      httr2::req_body_raw(
        paste0(prefixes, turtle_bloque),
        type = "text/turtle; charset=utf-8"
      ) |>
      httr2::req_timeout(15) |>
      httr2::req_perform(),
    error = function(e) NULL
  )

  if (!is.null(resp) && httr2::resp_status(resp) %in% c(200L, 201L, 204L)) {
    cat("✅ Registrado en Fuseki:", ruta_rmd, "\n")
    return(invisible(TRUE))
  } else {
    cat("❌ Error al registrar:", ruta_rmd, "\n")
    return(invisible(FALSE))
  }
}

registrar_un_ejercicio(ruta)
