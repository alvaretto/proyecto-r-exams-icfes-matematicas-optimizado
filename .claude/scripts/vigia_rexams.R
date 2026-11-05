#!/usr/bin/env Rscript
# =============================================================================
# vigia_rexams.R
# Vigía de versiones de R/exams: responde con fuentes primarias qué versión
# hay instalada, cuál hay en CRAN y en desarrollo, y qué ítems del NEWS son
# posteriores a la instalada y tocan el pipeline del repositorio.
#
# Uso:
#   Rscript .claude/scripts/vigia_rexams.R [--instalada exams=2.4-2,exams2forms=0.2-0]
#           [--lock renv.lock] [--offline DIR] [--exigir-al-dia] [--json]
#
# Por defecto lee las versiones instaladas con utils::packageVersion() y el
# renv.lock de la raíz del repo. --instalada sustituye a las instaladas (lo
# usan los tests para no depender de la máquina).
#
# Fuentes sin --offline (cada descarga con options(timeout = 30) y envuelta
# en tryCatch):
#   CRAN:       utils::available.packages(repos = "https://cloud.r-project.org"),
#               columna Version.
#   Desarrollo: https://zeileis.r-universe.dev/api/packages/<paquete>,
#               campo JSON Version.
#   NEWS:       https://codeberg.org/zeileis/<paquete>/raw/branch/main/NEWS.md.
#
# Con --offline DIR no se usa la red: se leen DIR/cran.json y DIR/dev.json
# (objetos con la forma exams:"2.4-5", exams2forms:"0.2-2") y
# DIR/exams_NEWS.md, DIR/exams2forms_NEWS.md.
#
# Códigos de salida:
#   0 = informe emitido
#   1 = solo con --exigir-al-dia, si CRAN tiene una versión mayor que la instalada
#   2 = NO VERIFICABLE (argumentos inválidos o alguna fuente no se pudo leer)
#
# Regla clave: si una fuente falla, jamás se dice «al día»: se imprime
# VIGIA_NO_VERIFICABLE: <fuente> y se sale con 2. La última línea de la salida
# normal es exactamente VIGIA_AL_DIA, VIGIA_HAY_VERSION_NUEVA o
# VIGIA_NO_VERIFICABLE.
# =============================================================================

# Paquetes que vigila el script (nombrados para que vapply conserve el nombre).
PAQUETES_VIGIA <- c(exams = "exams", exams2forms = "exams2forms")

# Temas que tocan el pipeline del repo. Un ítem puede tener varios; si ninguno
# coincide, clasificar_item() devuelve character(0).
TEMAS <- data.frame(
  tema = c("pandoc-latex", "plantillas", "figuras", "moodle", "cloze",
           "metadatos", "helpers-rmd", "nops", "webquiz", "stress"),
  patron = c(
    "pandoc|LTcaptype|longtable|\\\\usepackage|pdflatex",
    "template|plain\\.tex|exam\\.tex|solution\\.tex",
    "includegraphics|graphics file|supplement",
    "moodle",
    "cloze|exclozetype",
    "exsolution|exshuffle|extol|read_metainfo|meta-information",
    "answerlist|num_to_schoice|match_exams|include_supplement",
    "nops|exams2nops",
    "exams2forms|webquiz|webex",
    "stresstest"),
  regla = c("#18, #20", "#20", "#4", "exportación", "CLOZE", "#13",
            "código .Rmd", "NOPS", "webquiz", "F1"),
  stringsAsFactors = FALSE)

# version_mayor(a, b): TRUE si a > b. package_version() entiende 2.4-5 y 2.4.2
# con el mismo esquema (el guion y el punto separan igual), de modo que
# version_mayor("2.4-10", "2.4-5") es TRUE y version_mayor("2.4.2", "2.4-2")
# es FALSE.
version_mayor <- function(a, b) {
  package_version(a) > package_version(b)
}

# parsear_news(lineas): data.frame(paquete, version, item) desde un NEWS.md.
# Los encabezados son de nivel 1 («# exams 2.4-5», un solo #) y su segundo
# token debe ser una versión válida: así un título suelto («# Notas») no se
# confunde con un encabezado de versión. Cada ítem empieza con «* » en la
# columna 0 y sigue en las líneas con sangría, que se unen con un espacio; los
# sub-ítems («  * ...») forman parte del ítem padre y no crean fila propia.
# Se ignoran las líneas vacías y el texto anterior al primer encabezado.
parsear_news <- function(lineas) {
  paquete_actual <- NA_character_
  version_actual <- NA_character_
  item_actual <- NA_character_
  filas <- list()

  .cerrar <- function() {
    if (!is.na(item_actual)) {
      filas[[length(filas) + 1L]] <<- list(
        paquete = paquete_actual, version = version_actual, item = item_actual)
      item_actual <<- NA_character_
    }
  }

  for (linea in lineas) {
    if (grepl("^#\\s+\\S+\\s+\\S+", linea)) {
      partes <- strsplit(sub("^#\\s+", "", linea), "\\s+")[[1L]]
      if (!is.na(suppressWarnings(package_version(partes[2L])))) {
        .cerrar()
        paquete_actual <- partes[1L]
        version_actual <- partes[2L]
      }
      next
    }
    if (is.na(paquete_actual)) next
    if (grepl("^\\* ", linea)) {
      .cerrar()
      item_actual <- trimws(sub("^\\* ", "", linea))
    } else if (grepl("^[[:space:]]+", linea) && !is.na(item_actual)) {
      item_actual <- paste(item_actual, trimws(linea))
    } else if (nzchar(trimws(linea)) && !is.na(item_actual)) {
      # Línea en columna 0 que no es ítem ni encabezado (p. ej. un bloque de
      # código): cierra el ítem abierto.
      .cerrar()
    }
  }
  .cerrar()

  data.frame(
    paquete = vapply(filas, function(f) f$paquete, character(1)),
    version = vapply(filas, function(f) f$version, character(1)),
    item = vapply(filas, function(f) f$item, character(1)),
    stringsAsFactors = FALSE)
}

# items_nuevos(news, instalada): filas con versión mayor que la instalada del
# mismo paquete. Un paquete sin entrada en instalada se trata como 0.0-0.
items_nuevos <- function(news, instalada) {
  es_nuevo <- vapply(seq_len(nrow(news)), function(i) {
    v <- unname(instalada[news$paquete[i]])
    if (length(v) != 1L || is.na(v)) v <- "0.0-0"
    version_mayor(news$version[i], v)
  }, logical(1))
  news[es_nuevo, , drop = FALSE]
}

# clasificar_item(texto): temas según la tabla TEMAS, sin distinguir
# mayúsculas. Devuelve character(0) si ninguno coincide.
clasificar_item <- function(texto) {
  toca <- vapply(seq_len(nrow(TEMAS)), function(i) {
    grepl(TEMAS$patron[i], texto, ignore.case = TRUE)
  }, logical(1))
  TEMAS$tema[toca]
}

# leer_lock(ruta): versiones de Packages$<paq>$Version del renv.lock
# (JSON); NULL si el archivo no existe.
leer_lock <- function(ruta) {
  if (!file.exists(ruta)) return(NULL)
  l <- tryCatch(jsonlite::fromJSON(ruta, simplifyVector = TRUE),
                error = function(e) NULL)
  if (is.null(l) || is.null(l$Packages) || length(l$Packages) == 0L) return(NULL)
  vapply(l$Packages, function(p) {
    x <- p[["Version"]]
    if (is.null(x) || length(x) != 1L || is.na(x)) NA_character_ else as.character(x)
  }, character(1))
}

# informe(...): compone el resultado como lista con versiones (data.frame:
# paquete, instalada, lock, cran, dev), nuevos (data.frame con la columna
# adicional temas, primero los ítems que tienen tema) y codigo (0 al día,
# 1 con --exigir-al-dia y CRAN mayor que la instalada, 2 no verificable).
informe <- function(instalada, lock, cran, dev, news,
                    fallas = character(0), exigir_al_dia = FALSE) {
  paquetes <- names(instalada)
  if (is.null(paquetes)) {
    paquetes <- PAQUETES_VIGIA
    instalada <- stats::setNames(rep(NA_character_, length(paquetes)), paquetes)
  }
  .fuente <- function(x) {
    if (is.null(x)) return(rep(NA_character_, length(paquetes)))
    vapply(paquetes, function(p) {
      y <- unname(x[p])
      if (length(y) != 1L || is.na(y)) NA_character_ else y
    }, character(1))
  }
  versiones <- data.frame(
    paquete = paquetes,
    instalada = unname(instalada[paquetes]),
    lock = if (is.null(lock)) rep(NA_character_, length(paquetes)) else .fuente(lock),
    cran = .fuente(cran),
    dev = .fuente(dev),
    stringsAsFactors = FALSE)

  inst_eff <- ifelse(is.na(versiones$instalada), "0.0-0", versiones$instalada)
  nuevos <- items_nuevos(news, stats::setNames(inst_eff, paquetes))
  nuevos$temas <- lapply(nuevos$item, clasificar_item)
  con_tema <- lengths(nuevos$temas) > 0L
  nuevos <- nuevos[c(which(con_tema), which(!con_tema)), , drop = FALSE]

  codigo <- 0L
  if (length(fallas) > 0L) {
    codigo <- 2L
  } else if (isTRUE(exigir_al_dia) && isTRUE(any(
      version_mayor(ifelse(is.na(versiones$cran), "0.0-0", versiones$cran),
                    inst_eff), na.rm = TRUE))) {
    codigo <- 1L
  }

  list(versiones = versiones, nuevos = nuevos, codigo = codigo)
}

# --- Fuentes -----------------------------------------------------------------

# Descarga el texto de una URL con el timeout configurado por el llamador
# (options(timeout = 30)). NULL si no se pudo leer.
.descargar_texto <- function(url) {
  con <- tryCatch(url(url, open = "r"), error = function(e) NULL)
  if (is.null(con)) return(NULL)
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  txt <- tryCatch(readLines(con, warn = FALSE, encoding = "UTF-8"),
                  error = function(e) NULL)
  if (is.null(txt)) return(NULL)
  paste(txt, collapse = "\n")
}

# Versión de CRAN (cloud.r-project.org). NULL si no se pudo leer.
.cran_online <- function() {
  res <- tryCatch(
    suppressWarnings(utils::available.packages(repos = "https://cloud.r-project.org")),
    error = function(e) NULL)
  if (is.null(res)) return(NULL)
  v <- res[PAQUETES_VIGIA, "Version"]
  if (anyNA(v) || length(v) != length(PAQUETES_VIGIA)) return(NULL)
  v
}

# Versión en desarrollo (zeileis.r-universe.dev, campo JSON Version).
# NULL si no se pudo leer.
.dev_online <- function() {
  v <- vapply(PAQUETES_VIGIA, function(pkg) {
    txt <- .descargar_texto(paste0("https://zeileis.r-universe.dev/api/packages/", pkg))
    if (is.null(txt)) return(NA_character_)
    l <- tryCatch(jsonlite::fromJSON(txt), error = function(e) NULL)
    if (is.null(l) || is.null(l[["Version"]]) || length(l[["Version"]]) != 1L) {
      return(NA_character_)
    }
    as.character(l[["Version"]])
  }, character(1))
  if (anyNA(v)) NULL else v
}

# NEWS.md de Codeberg. NULL si no se pudo leer o no trae ítems.
.news_online <- function(paquete) {
  txt <- .descargar_texto(
    paste0("https://codeberg.org/zeileis/", paquete, "/raw/branch/main/NEWS.md"))
  if (is.null(txt)) return(NULL)
  df <- parsear_news(strsplit(txt, "\n", fixed = TRUE)[[1L]])
  if (nrow(df) == 0L) NULL else df
}

# JSON local (--offline) con la forma exams:"2.4-5", exams2forms:"0.2-2".
# NULL si falta el archivo, no trae los paquetes esperados o trae versiones
# inválidas.
.leer_json <- function(ruta) {
  if (!file.exists(ruta)) return(NULL)
  v <- tryCatch(jsonlite::fromJSON(ruta, simplifyVector = TRUE),
                error = function(e) NULL)
  if (is.null(v) || is.null(names(v))) return(NULL)
  faltan <- setdiff(PAQUETES_VIGIA, names(v))
  if (length(faltan) > 0L) return(NULL)
  v <- v[PAQUETES_VIGIA]
  if (is.list(v)) {
    v <- vapply(v, function(x) {
      if (length(x) == 1L) as.character(x) else NA_character_
    }, character(1))
  } else {
    v <- as.character(v)
    names(v) <- PAQUETES_VIGIA
  }
  if (anyNA(v) || any(!nzchar(v))) return(NULL)
  if (any(is.na(suppressWarnings(package_version(v))))) return(NULL)
  v
}

# NEWS.md local (--offline). NULL si falta el archivo o no trae ítems.
.leer_news_offline <- function(ruta) {
  if (!file.exists(ruta)) return(NULL)
  lineas <- tryCatch(readLines(ruta, warn = FALSE, encoding = "UTF-8"),
                     error = function(e) NULL)
  if (is.null(lineas) || length(lineas) == 0L) return(NULL)
  df <- parsear_news(lineas)
  if (nrow(df) == 0L) NULL else df
}

# --- CLI ---------------------------------------------------------------------

.raiz_repo <- function() {
  r <- tryCatch(
    suppressWarnings(system("git rev-parse --show-toplevel",
                            intern = TRUE, ignore.stderr = TRUE)),
    error = function(e) character(0))
  if (length(r) == 1L && nzchar(r)) return(r)
  normalizePath(".", mustWork = FALSE)
}

.parsear_instalada <- function(valor) {
  partes <- strsplit(valor, ",", fixed = TRUE)[[1L]]
  es_valido <- all(grepl("=", partes, fixed = TRUE))
  claves <- if (es_valido) sub("=.*$", "", partes) else character(0)
  valores <- if (es_valido) sub("^[^=]*=", "", partes) else character(0)
  versiones_ok <- suppressWarnings(!is.na(package_version(valores)))
  if (!es_valido || any(!nzchar(claves)) || any(!nzchar(valores)) ||
      anyDuplicated(claves) > 0L || any(!versiones_ok)) {
    stop("--instalada mal formado: ", valor)
  }
  stats::setNames(valores, claves)
}

.parser <- function(args) {
  instalada <- NULL
  lock <- NULL
  offline <- NULL
  exigir <- FALSE
  json <- FALSE
  i <- 1L
  while (i <= length(args)) {
    a <- args[i]
    if (a == "--instalada") {
      i <- i + 1L
      if (i > length(args)) stop("--instalada requiere un valor")
      instalada <- args[i]
    } else if (a == "--lock") {
      i <- i + 1L
      if (i > length(args)) stop("--lock requiere una ruta")
      lock <- args[i]
    } else if (a == "--offline") {
      i <- i + 1L
      if (i > length(args)) stop("--offline requiere un directorio")
      offline <- args[i]
    } else if (a == "--exigir-al-dia") {
      exigir <- TRUE
    } else if (a == "--json") {
      json <- TRUE
    } else {
      stop("argumento no reconocido: ", a)
    }
    i <- i + 1L
  }
  if (is.null(instalada)) {
    instalada <- vapply(PAQUETES_VIGIA, function(p) {
      tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
    }, character(1))
  } else {
    instalada <- .parsear_instalada(instalada)
  }
  if (is.null(lock)) lock <- file.path(.raiz_repo(), "renv.lock")
  list(instalada = instalada, lock = lock, offline = offline,
       exigir = exigir, json = json)
}

# ¿Alguna fuente (CRAN o desarrollo) trae una versión mayor que la instalada?
.hay_nueva <- function(cran, dev, instalada) {
  paquetes <- names(instalada)
  if (is.null(paquetes)) {
    paquetes <- PAQUETES_VIGIA
    instalada <- stats::setNames(rep(NA_character_, length(paquetes)), paquetes)
  }
  inst_eff <- ifelse(is.na(unname(instalada[paquetes])), "0.0-0",
                     unname(instalada[paquetes]))
  .efectiva <- function(x) {
    if (is.null(x)) return(rep("0.0-0", length(paquetes)))
    v <- unname(x[paquetes])
    ifelse(is.na(v), "0.0-0", v)
  }
  .supera <- function(x, y) {
    r <- suppressWarnings(package_version(x) > package_version(y))
    isTRUE(any(r, na.rm = TRUE))
  }
  .supera(.efectiva(cran), inst_eff) || .supera(.efectiva(dev), inst_eff)
}

# Recorta un ítem a 160 caracteres (con «…» si se recortó) y normaliza los
# espacios que dejó la unión de las líneas.
.recortar <- function(x) {
  x <- gsub("[[:space:]]+", " ", x)
  if (nchar(x) > 160L) paste0(substr(x, 1L, 159L), "…") else x
}

.imprimir <- function(inf, fallas, hay_nueva) {
  cat("Vigía de versiones de R/exams\n\n")
  tabla <- data.frame(
    Paquete = inf$versiones$paquete,
    Instalada = inf$versiones$instalada,
    renv.lock = inf$versiones$lock,
    CRAN = inf$versiones$cran,
    Desarrollo = inf$versiones$dev,
    check.names = FALSE, stringsAsFactors = FALSE)
  print(tabla, row.names = FALSE)
  cat("\n")
  for (i in seq_len(nrow(inf$versiones))) {
    f <- inf$versiones[i, ]
    # package_version: `2.4.2` y `2.4-2` son la misma versión escrita de dos formas.
    if (!is.na(f$lock) && !is.na(f$instalada) && package_version(f$lock) != package_version(f$instalada)) {
      cat(sprintf("LOCK_DIVERGENTE: %s instalada %s, renv.lock %s\n",
                  f$paquete, f$instalada, f$lock))
    }
  }
  if (nrow(inf$nuevos) > 0L) {
    cat("\nNovedades posteriores a la instalada:\n")
    for (i in seq_len(nrow(inf$nuevos))) {
      f <- inf$nuevos[i, ]
      temas <- f$temas[[1L]]
      prefijo <- if (length(temas) > 0L) {
        paste0("[", paste(temas, collapse = " "), "] ")
      } else {
        ""
      }
      cat(sprintf("  %s %s %s%s\n", f$paquete, f$version, prefijo, .recortar(f$item)))
    }
  }
  cat("\n")
  if (length(fallas) > 0L) {
    for (f in fallas) cat("VIGIA_NO_VERIFICABLE: ", f, "\n", sep = "")
    cat("VIGIA_NO_VERIFICABLE\n")
  } else if (hay_nueva) {
    cat("Antes de actualizar: Rscript .claude/scripts/comparar_version_exams.R --lib <biblioteca-con-la-version-nueva>\n\n")
    cat("VIGIA_HAY_VERSION_NUEVA\n")
  } else {
    cat("VIGIA_AL_DIA\n")
  }
}

# El CLI se ejecuta solo bajo Rscript (no con source()), que es lo que permite
# cargar las funciones desde los tests sin disparar la descarga de fuentes.
if (!interactive() && sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  op <- tryCatch(.parser(args), error = function(e) e)
  if (inherits(op, "error")) {
    cat("Uso: Rscript .claude/scripts/vigia_rexams.R [--instalada exams=2.4-2,exams2forms=0.2-0] [--lock renv.lock] [--offline DIR] [--exigir-al-dia] [--json]\n")
    cat("VIGIA_NO_VERIFICABLE: argumentos inválidos\n")
    cat("VIGIA_NO_VERIFICABLE\n")
    quit(status = 2L)
  }

  opciones <- options(timeout = 30)
  on.exit(options(opciones), add = TRUE)

  fallas <- character(0)
  if (!is.null(op$offline)) {
    cran <- .leer_json(file.path(op$offline, "cran.json"))
    if (is.null(cran)) fallas <- c(fallas, "cran.json")
    dev <- .leer_json(file.path(op$offline, "dev.json"))
    if (is.null(dev)) fallas <- c(fallas, "dev.json")
    news_exams <- .leer_news_offline(file.path(op$offline, "exams_NEWS.md"))
    if (is.null(news_exams)) fallas <- c(fallas, "NEWS de exams")
    news_forms <- .leer_news_offline(file.path(op$offline, "exams2forms_NEWS.md"))
    if (is.null(news_forms)) fallas <- c(fallas, "NEWS de exams2forms")
  } else {
    cran <- .cran_online()
    if (is.null(cran)) fallas <- c(fallas, "CRAN")
    dev <- .dev_online()
    if (is.null(dev)) fallas <- c(fallas, "desarrollo")
    news_exams <- .news_online("exams")
    if (is.null(news_exams)) fallas <- c(fallas, "NEWS de exams")
    news_forms <- .news_online("exams2forms")
    if (is.null(news_forms)) fallas <- c(fallas, "NEWS de exams2forms")
  }

  partes_news <- list(news_exams, news_forms)
  partes_news <- partes_news[!vapply(partes_news, is.null, logical(1))]
  news <- if (length(partes_news) > 0L) do.call(rbind, partes_news) else {
    data.frame(paquete = character(0), version = character(0),
               item = character(0), stringsAsFactors = FALSE)
  }

  lock <- leer_lock(op$lock)
  inf <- informe(op$instalada, lock, cran, dev, news, fallas, op$exigir)
  hay_nueva <- .hay_nueva(cran, dev, op$instalada)

  if (op$json) {
    cat(jsonlite::toJSON(inf, auto_unbox = TRUE), "\n", sep = "")
  } else {
    .imprimir(inf, fallas, hay_nueva)
  }
  quit(status = inf$codigo)
}
