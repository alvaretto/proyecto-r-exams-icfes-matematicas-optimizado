# =============================================================================
# etiquetar_moodle_xml.R — paso posterior a exams2moodle(): exextra -> <tags>
# =============================================================================
#
# PROBLEMA QUE RESUELVE
# ---------------------
# exams2moodle() (R/exams 2.4.2) descarta los exextra del .Rmd: ninguna pregunta
# llega etiquetada a Moodle, y hubo que etiquetarlas después, directo en la base
# (Moodle/etiquetado/aplicar_etiquetas.php). Esta función escribe las etiquetas en
# el XML a partir de los exextra del .Rmd, y así cada lote llega etiquetado.
#
# ETIQUETAS (las mismas que ya usa el banco de icfes)
# ---------------------------------------------------
#   area::Matematicas                     siempre (constante: todo ejercicio de este repo)
#   competencia::<exextra[Competencia]>   solo si el .Rmd la trae y se reconoce
#   nivel::<número de exextra[Nivel]>     solo si el .Rmd la trae (1 a 4)
#   componente::<exextra[Componente]>     solo si el .Rmd la trae y se reconoce
#
# Nunca se inventa una etiqueta: lo que el .Rmd no trae, o trae con un valor que no
# se reconoce, queda sin etiqueta (lo segundo con un warning).
#
# DECISIÓN A REVISAR: VOCABULARIO DEL BANCO
# -----------------------------------------
# Los .Rmd de 03-En-Produccion escriben el mismo valor de varias maneras
# (Interpretacion / Interpretación y representación; Numerico-Variacional / Numérico
# variacional; Aleatorio y sistemas de datos), pero el banco usa UN texto por valor
# (competencia::Interpretación y representación, componente::Numérico-variacional,
# componente::Aleatorio...). Copiar el exextra tal cual crearía etiquetas nuevas que
# no coinciden con las ya puestas. Por eso la competencia y el componente se llevan
# al texto del banco con la MISMA regla de Moodle/etiquetado/informe_etiquetas.py
# (sin tildes ni mayúsculas, la palabra clave al inicio de palabra). Los textos
# están en .ETQ_VOCAB_COMPETENCIA y .ETQ_VOCAB_COMPONENTE; si se prefiere copiar el
# valor literal, es el único punto que hay que cambiar.
#
# USO (1 o 2 líneas de R)
# -----------------------
#   source(file.path(system("git rev-parse --show-toplevel", intern = TRUE),
#                    "A-Produccion", "perifericos", "etiquetar_moodle_xml.R"), encoding = "UTF-8")
#   etiquetar_moodle_xml("salida/ejercicio_.xml", "ejercicio.Rmd")
#
# LÍNEA QUE HABRÍA QUE AGREGAR DESPUÉS DE CADA exams2moodle()
# -----------------------------------------------------------
# exams2moodle() devuelve la lista de ejercicios, no la ruta del XML: la ruta es
# dir + name + ".xml". La línea usa el mismo archivo_examen del Semillero. Ningún
# Semillero se modifica en este commit; la integración la decide quien los mantiene.
#
#   SemilleroMoodle_v2.R y SemilleroUnico_v2.R (name = nombre_arch, dir = "salida"):
#     etiquetar_moodle_xml(file.path("salida", paste0(nombre_arch, ".xml")), archivo_examen)
#
#   SemilleroCloze.R (dentro de generar_moodle(); name = paste0(nombre_base, "_moodle")):
#     etiquetar_moodle_xml(file.path(config$dir_salida, "moodle",
#                                    paste0(nombre_base, "_moodle.xml")), archivo_examen)
#
#   Y, una sola vez antes de la primera llamada, el source() de la sección USO.
#
# QUÉ HACE Y QUÉ NO
# -----------------
# - Cada <question> se cruza con su .Rmd por el título que escribe exams2moodle() en
#   <name> («R1 Q1 : <título>»): el extitle del .Rmd o, si no hay, el nombre del archivo
#   sin extensión (con «/ ~ \ :» cambiados por «_», como hace R/exams).
# - Las <question type="category"> quedan intactas, sin <tags>.
# - Una pregunta que no se puede cruzar con un .Rmd único queda intacta y se avisa.
# - Solo AGREGA: no quita ni reemplaza etiquetas que el XML ya traiga. La comparación
#   no distingue mayúsculas (Moodle tampoco), así que volver a correrla no duplica.
# - El resto del XML queda byte a byte igual (se verificó con un XML real de
#   exams2moodle()): solo se agregan los <tags>. Si no hay nada que agregar, ni se
#   reescribe el archivo.
# - Lee los exextra con la sintaxis de exams:::extract_extra(): «exextra[Nivel]: 3»,
#   con tipo opcional («exextra[Nivel, numeric]: 3»), valor recortado y, si la clave
#   está repetida, gana la última. Solo mira la sección «Meta-information».
#
# REQUIERE: xml2 (ya está en renv.lock) y R base.
# DEVUELVE (invisible): c(preguntas, agregadas, sin_fuente) como enteros.
# =============================================================================

# --- Constantes --------------------------------------------------------------
.ETQ_AREA <- "area::Matematicas"

# Palabra clave sin tildes -> texto del banco (igual que informe_etiquetas.py).
# Los textos llevan \u para no depender de la codificación con que se lea este archivo.
.ETQ_VOCAB_COMPETENCIA <- c(
  interpretacion = "Interpretación y representación",  # Interpretación y representación
  formulacion    = "Formulación y ejecución",           # Formulación y ejecución
  argumentacion  = "Argumentación")                           # Argumentación

.ETQ_VOCAB_COMPONENTE <- c(
  numerico    = "Numérico-variacional",                       # Numérico-variacional
  variacional = "Numérico-variacional",
  geometrico  = "Geométrico-métrico",                    # Geométrico-métrico
  espacial    = "Geométrico-métrico",
  metrico     = "Geométrico-métrico",
  aleatorio   = "Aleatorio")

# --- Auxiliares (prefijo .etq_ para no pisar nombres del Semillero) ----------

# Minúsculas y sin tildes, sin depender de la configuración regional.
.etq_plano <- function(x) {
  x <- enc2utf8(x)
  tolower(chartr("áéíóúüñÁÉÍÓÚÜÑ",
                 "aeiouunAEIOUUN", x))
}

# Texto del banco si el valor nombra exactamente UNO de los valores del vocabulario;
# NA si no nombra ninguno o nombra varios distintos.
.etq_buscar <- function(vocab, texto) {
  plano <- .etq_plano(texto)
  esta <- vapply(names(vocab), function(k) grepl(paste0("(^|[^a-z])", k), plano), logical(1L))
  hallados <- unique(unname(vocab[esta]))
  if (length(hallados) == 1L) hallados else NA_character_
}

# extitle y exextra[Competencia|Nivel|Componente] de la sección Meta-information.
.etq_leer_metainfo <- function(rmd) {
  lineas <- sub("\r$", "", readLines(rmd, warn = FALSE, encoding = "UTF-8"))
  i <- grep("^Meta-information\\s*$", lineas)
  bloque <- if (length(i)) utils::tail(lineas, -i[1L]) else character(0)
  campo <- function(clave) {
    patron <- sprintf("^\\s*%s\\s*:\\s*(.*?)\\s*$", clave)
    hit <- grep(patron, bloque, perl = TRUE, value = TRUE)
    if (!length(hit)) return(NULL)
    valor <- sub(patron, "\\1", utils::tail(hit, 1L), perl = TRUE)
    if (nzchar(valor)) valor else NULL
  }
  extra <- function(nombre) campo(sprintf("exextra\\[%s\\s*(?:,[^]]*)?\\]", nombre))
  list(titulo = campo("extitle"), competencia = extra("Competencia"),
       nivel = extra("Nivel"), componente = extra("Componente"))
}

# Títulos con los que exams2moodle() puede haber nombrado las preguntas de este .Rmd.
.etq_claves_titulo <- function(rmd, extitle) {
  if (!is.null(extitle)) return(extitle)
  sin_ext <- tools::file_path_sans_ext(rmd)
  unique(c(gsub("(/|~|\\\\|:)+", "_", sin_ext), basename(sin_ext)))
}

# Claves de título y etiquetas de un .Rmd (avisa de lo que no se reconoce).
.etq_fuente <- function(rmd) {
  info <- .etq_leer_metainfo(rmd)
  tags <- .ETQ_AREA
  avisar <- function(clave_rmd, clave_tag, valor) {
    warning(sprintf("%s: exextra[%s] = '%s' no se reconoce o es ambiguo; queda sin etiqueta %s::",
                    basename(rmd), clave_rmd, valor, clave_tag), call. = FALSE)
  }
  if (!is.null(info$competencia)) {
    v <- .etq_buscar(.ETQ_VOCAB_COMPETENCIA, info$competencia)
    if (is.na(v)) avisar("Competencia", "competencia", info$competencia)
    else tags <- c(tags, paste0("competencia::", v))
  }
  if (!is.null(info$nivel)) {
    n <- regmatches(info$nivel, gregexpr("[0-9]+", info$nivel))[[1L]]
    if (length(n) == 1L && n %in% as.character(1:4)) tags <- c(tags, paste0("nivel::", n))
    else avisar("Nivel", "nivel", info$nivel)
  }
  if (!is.null(info$componente)) {
    v <- .etq_buscar(.ETQ_VOCAB_COMPONENTE, info$componente)
    if (is.na(v)) avisar("Componente", "componente", info$componente)
    else tags <- c(tags, paste0("componente::", v))
  }
  list(claves = .etq_claves_titulo(rmd, info$titulo), tags = tags)
}

# --- Función pública ---------------------------------------------------------

#' @param xml   ruta del XML que escribió exams2moodle() (dir + name + ".xml").
#' @param rmd   el/los .Rmd de donde salió, como se pasaron a exams2moodle() (vector o lista).
#' @param salida dónde escribir el XML etiquetado; por defecto reemplaza a `xml`
#'               (escritura atómica: o queda entero o queda el original).
etiquetar_moodle_xml <- function(xml, rmd, salida = xml) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("etiquetar_moodle_xml() necesita el paquete 'xml2'.", call. = FALSE)
  }
  rmd <- unique(unlist(rmd, use.names = FALSE))
  if (!file.exists(xml)) stop("No existe el XML de Moodle: ", xml, call. = FALSE)
  if (any(!file.exists(rmd))) {
    stop("No existe el .Rmd: ", paste(rmd[!file.exists(rmd)], collapse = ", "), call. = FALSE)
  }

  # 1. Una fuente por .Rmd. Un título que dos .Rmd distintos comparten no sirve para cruzar.
  fuentes <- lapply(rmd, .etq_fuente)
  claves <- unlist(lapply(seq_along(fuentes), function(i) {
    stats::setNames(rep(i, length(fuentes[[i]]$claves)), fuentes[[i]]$claves)
  }))
  claves <- claves[!(names(claves) %in% names(claves)[duplicated(names(claves))])]

  # 2. Cada pregunta (menos las categorías) recibe las etiquetas de su .Rmd.
  doc <- xml2::read_xml(xml, options = "HUGE")
  preguntas <- xml2::xml_find_all(doc, "//question[not(@type='category')]")
  agregadas <- 0L
  sin_fuente <- character(0)
  for (k in seq_along(preguntas)) {
    q <- preguntas[[k]]
    nombre <- xml2::xml_text(xml2::xml_find_first(q, "./name/text"))
    titulo <- NA_character_
    if (!is.na(nombre)) {
      m <- regmatches(nombre, regexec("^\\s*(?:R[0-9]+\\s+)?Q[0-9]+\\s+:\\s+(.*?)\\s*$",
                                      nombre, perl = TRUE))[[1L]]
      if (length(m) == 2L) titulo <- m[2L]
    }
    i <- if (is.na(titulo)) NA_integer_ else unname(claves[titulo])
    if (is.na(i)) {
      sin_fuente <- c(sin_fuente, if (is.na(titulo)) "(nombre ilegible)" else titulo)
      next
    }
    tags <- fuentes[[i]]$tags
    nodo <- xml2::xml_find_first(q, "./tags")
    sin_nodo <- inherits(nodo, "xml_missing")
    actuales <- if (sin_nodo) character(0) else xml2::xml_text(xml2::xml_find_all(nodo, "./tag/text"))
    nuevas <- tags[!(tolower(tags) %in% tolower(trimws(actuales)))]
    if (!length(nuevas)) next
    if (sin_nodo) nodo <- xml2::xml_add_child(q, "tags")
    for (etiqueta in nuevas) {
      nodo_tag <- xml2::xml_add_child(nodo, "tag")
      nodo_texto <- xml2::xml_add_child(nodo_tag, "text")
      xml2::xml_set_text(nodo_texto, etiqueta)
    }
    agregadas <- agregadas + length(nuevas)
  }
  if (length(sin_fuente)) {
    warning(sprintf("%d pregunta(s) del XML no se cruzan con un .Rmd único de los dados (%s): quedan sin etiquetas.",
                    length(sin_fuente), paste(unique(sin_fuente), collapse = ", ")), call. = FALSE)
  }

  # 3. Escritura. Sin cambios y en el mismo archivo, no se toca. Si no, a un temporal en la
  #    misma carpeta y renombrado: un corte a medias no deja un XML truncado.
  mismo <- identical(normalizePath(xml), normalizePath(salida, mustWork = FALSE))
  if (agregadas > 0L || !mismo) {
    tmp <- tempfile(pattern = ".etiquetar_", tmpdir = dirname(salida), fileext = ".xml")
    on.exit(unlink(tmp), add = TRUE)
    xml2::write_xml(doc, tmp, options = c("as_xml", "no_empty_tags"), encoding = "UTF-8")
    if (!file.rename(tmp, salida)) stop("No se pudo escribir ", salida, call. = FALSE)
  }
  resumen <- c(preguntas = length(preguntas), agregadas = agregadas, sin_fuente = length(sin_fuente))
  message(sprintf("etiquetar_moodle_xml: %d preguntas, %d etiquetas agregadas, %d sin .Rmd (%s)",
                  resumen[["preguntas"]], agregadas, resumen[["sin_fuente"]], basename(salida)))
  invisible(resumen)
}
