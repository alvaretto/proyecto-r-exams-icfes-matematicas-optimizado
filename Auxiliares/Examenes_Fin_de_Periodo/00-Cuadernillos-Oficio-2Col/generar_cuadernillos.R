## generar_cuadernillos.R: motor de cuadernillos Saber 11 Matemáticas en papel oficio a doble columna.
##
## Uso:
##   Rscript generar_cuadernillos.R <config.R> [--salidas cuadernillo,hoja,clave,retro,word] [--dir-salida <ruta>] [--limpiar]
##
## Desde UNA matriz de semillas por sesión produce, para N estudiantes (una versión por estudiante, N ≤ 100):
##   cuadernillos/cuadernillo_<ID>.pdf (+ cuadernillos_imprimir.pdf)   hojas/hoja_respuestas_<ID>.pdf (+ hojas_imprimir.pdf)
##   docente/cuadernillo_docente_<ID>.pdf (+ docente_imprimir.pdf): el mismo cuadernillo a dos columnas, con la
##     retroalimentación y la clave. Sale SIEMPRE junto con el del estudiante (salida «cuadernillo»).
##   clave/clave.csv y clave/clave.pdf   retro/retro_<ID>.pdf (90 x 160 mm)   word/cuadernillo_<ID>.docx
##   revision_gris_v1.png y revision_gris_vN.png (C10), firmas.rds, semillas.rds/.csv, preflight.log, verificacion.txt
## y termina con verificar_cuadernillos.R sobre cada sesión. Exit: 0 (C1-C9 y C11-C15 en verde), 1 (algún FAIL),
## 2 (ERR_DIR_SALIDA o NO VERIFICABLE).
##
## Opciones de la configuración (además de las de configuraciones/examen_real_11.R):
##   partir_preguntas = TRUE (por defecto) | FALSE. TRUE: una pregunta puede partirse entre columnas o páginas; es la LEY
##     del cuadernillo (acomodar la mayor cantidad de preguntas). FALSE: cada pregunta queda entera en una columna.
##   presupuesto_s: segundos por ejercicio en el pre-vuelo (por defecto 60 + 10 × N).
##
## source() de este archivo NO tiene efectos: solo define funciones. main() corre solo con Rscript.
## Plan y criterios: PRPs/motor-cuadernillos-oficio-2col.md (raíz del repo).

SALIDAS_VALIDAS <- c("cuadernillo", "hoja", "clave", "retro", "word")
`%||%` <- function(a, b) if (is.null(a)) b else a

## ---------------------------------------------------------------------------------------------------------------
## Rutas del motor y de las plantillas
## ---------------------------------------------------------------------------------------------------------------
dir_motor <- function() {
  d <- Sys.getenv("CUADERNILLOS_MOTOR_DIR", "")
  if (nzchar(d)) return(normalizePath(d, mustWork = TRUE))
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(f) && basename(f[1]) %in% c("generar_cuadernillos.R", "verificar_cuadernillos.R"))
    return(dirname(normalizePath(f[1], mustWork = TRUE)))
  stop("no se pudo ubicar el directorio del motor: defina CUADERNILLOS_MOTOR_DIR")
}

raiz_git <- function(desde = dir_motor()) {
  r <- suppressWarnings(system2("git", c("-C", shQuote(desde), "rev-parse", "--show-toplevel"), stdout = TRUE, stderr = FALSE))
  if (length(r) != 1 || !nzchar(r)) stop("no se encontró la raíz git desde ", desde)
  normalizePath(r)
}

## Directorio de plantillas, una sola variable (la leen el motor, el verificador y los mutantes).
## Por defecto, <raíz git>/A-Produccion/perifericos: las cuatro plantillas del motor son versión oficial desde el
## 2026-11-14 (regla #26) y ya no hay copia en este proyecto. CUADERNILLOS_PLANTILLAS lo sustituye: lo usan los mutantes,
## que copian las plantillas a un directorio temporal y dañan la copia, nunca las oficiales.
PLANTILLAS_MOTOR <- c("cuadernillo_oficio_2col.tex", "retro_celular.tex", "pandoc_cuadernillo.tex", "referencia_oficio_2col.docx")
dir_plantillas_defecto <- function() file.path(raiz_git(), "A-Produccion", "perifericos")
dir_plantillas <- function() {
  d <- Sys.getenv("CUADERNILLOS_PLANTILLAS", unset = "")
  normalizePath(if (nzchar(d)) d else dir_plantillas_defecto(), mustWork = TRUE)
}
plantilla <- function(nombre) {
  p <- file.path(dir_plantillas(), nombre)
  if (!file.exists(p)) stop("falta la plantilla ", p)
  p
}

es_absoluta <- function(p) grepl("^(/|~)", p)

## ---------------------------------------------------------------------------------------------------------------
## Guarda de ruta (C11, ERR_DIR_SALIDA): antes de escribir o borrar
## ---------------------------------------------------------------------------------------------------------------
## normalizePath(mustWork = FALSE) devuelve sin resolver una ruta que no existe (con sus «..»). Se resuelve el ancestro
## EXISTENTE más largo y se le añade el resto; un «.» o «..» que quede en el resto se rechaza.
normalizar_ruta <- function(p) {
  p <- path.expand(p)
  if (!es_absoluta(p)) stop("ERR_DIR_SALIDA: ruta no absoluta: ", p)
  partes <- strsplit(p, "/", fixed = TRUE)[[1]]
  partes <- partes[nzchar(partes)]
  for (i in rev(seq_len(length(partes) + 1L) - 1L)) {
    anc <- paste0("/", paste(partes[seq_len(i)], collapse = "/"))
    if (dir.exists(anc)) {
      resto <- partes[seq_len(length(partes) - i) + i]
      if (any(resto %in% c(".", ".."))) stop("ERR_DIR_SALIDA: «.» o «..» en una parte que no existe: ", p)
      base <- normalizePath(anc, mustWork = TRUE)
      return(if (length(resto)) do.call(file.path, as.list(c(base, resto))) else base)
    }
  }
  stop("ERR_DIR_SALIDA: no se pudo normalizar ", p)
}

es_ancestro_o_igual <- function(a, b) identical(a, b) || startsWith(paste0(b, "/"), paste0(a, "/"))

guarda_ruta <- function(p) {
  enlace <- Sys.readlink(path.expand(p))       # "" si no es enlace; NA si la ruta no existe todavía
  if (!is.na(enlace) && nzchar(enlace)) stop("ERR_DIR_SALIDA: es un enlace simbólico: ", p)
  ds <- normalizar_ruta(p)
  comp <- strsplit(ds, "/", fixed = TRUE)[[1]]
  comp <- comp[nzchar(comp)]
  k <- which(comp == "salida")
  if (!length(k) || all(k == length(comp)))
    stop("ERR_DIR_SALIDA: la ruta debe quedar DENTRO de una carpeta «salida»: ", ds)
  prohibidas <- unique(c(tryCatch(raiz_git(), error = function(e) NULL), dir_motor()))
  for (q in prohibidas) if (es_ancestro_o_igual(ds, q))
    stop("ERR_DIR_SALIDA: la ruta es la raíz git, el motor o un ancestro de alguno: ", ds)
  ds
}

MARCADOR <- ".cuadernillos_salida"

## Rechaza un directorio no vacío salvo con --limpiar, y --limpiar solo borra si está el marcador del motor.
preparar_salida <- function(ds, limpiar = FALSE) {
  if (dir.exists(ds)) {
    contenido <- list.files(ds, all.files = TRUE, no.. = TRUE)
    if (length(contenido)) {
      if (!limpiar) stop("ERR_DIR_SALIDA: ", ds, " no está vacío; repita con --limpiar")
      if (!file.exists(file.path(ds, MARCADOR)))
        stop("ERR_DIR_SALIDA: ", ds, " no está vacío y no tiene el marcador ", MARCADOR, ": no se borra")
      unlink(file.path(ds, contenido), recursive = TRUE, force = TRUE)
    }
  } else {
    dir.create(ds, recursive = TRUE)
  }
  file.create(file.path(ds, MARCADOR))
  invisible(ds)
}

## ---------------------------------------------------------------------------------------------------------------
## Configuración
## ---------------------------------------------------------------------------------------------------------------
leer_config <- function(ruta, dir_salida_cli = NULL) {
  ruta <- normalizePath(ruta, mustWork = TRUE)
  env <- new.env(parent = globalenv())
  assign("DIR_CONFIG", dirname(ruta), envir = env)     # un config puede leer a otro: source(file.path(DIR_CONFIG, …), local = TRUE)
  sys.source(ruta, envir = env)
  if (!exists("config", envir = env, inherits = FALSE)) stop("ERR_CONFIG: ", ruta, " no define la lista `config`")
  cfg <- get("config", envir = env)
  falta <- setdiff(c("titulo", "institucion", "fecha", "estudiantes", "semilla", "sesiones"), names(cfg))
  if (length(falta)) stop("ERR_CONFIG: faltan campos: ", paste(falta, collapse = ", "))
  N <- cfg$estudiantes
  if (!is.numeric(N) || length(N) != 1 || is.na(N) || N != round(N) || N < 1 || N > 100)
    stop("C11: estudiantes debe estar entre 1 y 100 (vale ", paste(N, collapse = " "), ")")
  cfg$estudiantes <- as.integer(N)
  cfg$fecha <- as.Date(cfg$fecha)
  cfg$semilla <- as.integer(cfg$semilla)
  base <- dirname(ruta)
  if (!length(cfg$sesiones)) stop("ERR_CONFIG: no hay sesiones")
  cfg$sesiones <- lapply(seq_along(cfg$sesiones), function(k) {
    s <- cfg$sesiones[[k]]
    ej <- vapply(s$ejercicios, function(e) if (es_absoluta(e)) path.expand(e) else file.path(base, e), "", USE.NAMES = FALSE)
    no <- !file.exists(ej)
    if (any(no)) stop("ERR_CONFIG: no existen: ", paste(ej[no], collapse = ", "))
    ej <- normalizePath(ej)
    if (length(ej) > 45) stop("C11: como máximo 45 preguntas por sesión (sesión ", k, ": ", length(ej), ")")
    list(k = k, slug = sprintf("sesion-%d", k), nombre = s$nombre %||% sprintf("Sesión %d", k),
         startid = as.integer(s$startid %||% (1L + 100L * (k - 1L))), ejercicios = ej)
  })
  ## dos sesiones con la misma fecha no pueden compartir IDs
  rangos <- lapply(cfg$sesiones, function(s) s$startid + seq_len(cfg$estudiantes) - 1L)
  if (anyDuplicated(unlist(rangos))) stop("C11: los rangos de ID de las sesiones se cruzan; use startid distintos (1, 101)")
  cfg$salidas <- cfg$salidas %||% SALIDAS_VALIDAS
  if (length(setdiff(cfg$salidas, SALIDAS_VALIDAS)))
    stop("ERR_CONFIG: salidas desconocidas: ", paste(setdiff(cfg$salidas, SALIDAS_VALIDAS), collapse = ", "))
  ## dir_salida: la de la CLI es relativa al directorio de trabajo; la del config, al directorio del config
  ds <- if (!is.null(dir_salida_cli)) {
    if (es_absoluta(dir_salida_cli)) dir_salida_cli else file.path(getwd(), dir_salida_cli)
  } else {
    if (is.null(cfg$dir_salida)) stop("ERR_CONFIG: falta dir_salida (en el config o con --dir-salida)")
    if (es_absoluta(cfg$dir_salida)) cfg$dir_salida else file.path(base, cfg$dir_salida)
  }
  cfg$dir_salida <- guarda_ruta(ds)
  ## Tope del pre-vuelo por ejercicio (C9): 60 + 10 × N segundos (decisión del profesor, 2026-11-14). El del PRP era
  ## 30 + 5 × N; MEDIDO con N = 25: el ejercicio más pesado del examen real (I_1796473-Opc-A2, una tabla TikZ y tres
  ## gráficas por versión) tardó 143,1 y 141,4 s de los 155 s que le daba, y desde unas 44 versiones lo habría cortado
  ## sin estar colgado. Un ejercicio de verdad colgado se corta igual; solo tarda el doble en cortarse.
  cfg$presupuesto_s <- as.numeric(cfg$presupuesto_s %||% (60 + 10 * cfg$estudiantes))
  ## Partir las preguntas es la regla (decisión del profesor, 2026-11-13 y 2026-11-14: caben más). FALSE las deja enteras.
  pp <- cfg$partir_preguntas %||% TRUE
  if (!is.logical(pp) || length(pp) != 1L || is.na(pp)) stop("ERR_CONFIG: partir_preguntas debe ser TRUE o FALSE")
  cfg$partir_preguntas <- pp
  cfg$ruta_config <- ruta
  cfg
}

## ---------------------------------------------------------------------------------------------------------------
## Funciones puras
## ---------------------------------------------------------------------------------------------------------------
matriz_semillas <- function(semilla, s, N, Q) {
  set.seed(semilla + s)
  matrix(sample.int(1e6, N * Q), nrow = N)
}

## La fórmula de Date2ID de exams2nops (R/exams 2.4.5): fecha %y%m%d + número de 5 cifras desde startid.
id_version <- function(fecha, startid, i) paste0(format(as.Date(fecha), "%y%m%d"), formatC(startid + i - 1L, width = 5, flag = "0"))

## Nombre que R/exams da a la copia i de n: rellena el índice con ceros hasta el ancho de n (código de
## exams:::make_exams_write_pdf, 2.4.5: formatC(id, width = floor(log10(n)) + 1L, flag = "0")). Con n = 25 la primera hoja
## es nops_01.pdf, no nops_1.pdf. MEDIDO el 2026-11-14: el motor buscaba nops_1.pdf y la corrida de capacidad (25
## versiones) abortó en las hojas; con menos de 10 versiones los dos nombres coinciden y no se veía.
nombre_copia <- function(nombre, i, n, ext) paste0(nombre, formatC(i, width = floor(log10(n)) + 1L, flag = "0"), ".", ext)

## R/exams escribe los valores de `header` tal cual en el .tex: el motor los escapa. Carácter a carácter, en una sola
## pasada, para que las llaves de \textbackslash{} no se escapen otra vez.
escapar_latex <- function(x) {
  tabla <- c("\\" = "\\textbackslash{}", "&" = "\\&", "%" = "\\%", "$" = "\\$", "#" = "\\#", "_" = "\\_",
             "{" = "\\{", "}" = "\\}", "~" = "\\textasciitilde{}", "^" = "\\textasciicircum{}")
  vapply(x, function(s) {
    ch <- strsplit(s, "", fixed = TRUE)[[1]]
    i <- ch %in% names(tabla)
    ch[i] <- tabla[ch[i]]
    paste0(ch, collapse = "")
  }, "", USE.NAMES = FALSE)
}

## Huella del texto de una pregunta, con cada referencia de figura sustituida por «FIG» (pdf en exams2pdf, png en Word).
normalizar <- function(txt) {
  txt <- gsub("\\\\includegraphics(\\[[^]]*\\])?\\{[^}]*\\}", "FIG", txt, perl = TRUE)
  gsub("!\\[[^]]*\\]\\([^)]*\\)(\\{[^}]*\\})?", "FIG", txt, perl = TRUE)
}
huella <- function(ex) digest::digest(normalizar(c(ex$question, unlist(ex$questionlist))))

clave_letras <- function(ex) paste(letters[which(ex$metainfo$solution)], collapse = "")

## Texto reducido a letras y cifras en minúscula: así se comparan un .tex y lo que pdftotext lee del PDF, sin que
## importen los saltos de renglón, la partición de palabras, los espacios ni las ligaduras («ﬁ» = «fi»).
solo_letras <- function(x) {
  x <- paste(x, collapse = " ")
  for (l in list(c("ﬃ", "ffi"), c("ﬄ", "ffl"), c("ﬀ", "ff"), c("ﬁ", "fi"), c("ﬂ", "fl")))
    x <- gsub(l[1], l[2], x, fixed = TRUE)
  tolower(gsub("[^[:alnum:]]", "", x))
}

## Testigos de la retroalimentación de una pregunta: hasta `n` trozos de texto llano de su Solution (de 20 a 30 letras
## cada uno) que NO aparecen en ningún enunciado del examen (`excluir` = solo_letras de todas las preguntas de la versión).
## Con ellos la guarda C15 comprueba, leyendo el PDF, que el cuadernillo del estudiante no trae la retroalimentación y
## que el del docente y la retro sí. Se corta en toda marca de LaTeX o de fórmula: lo que queda se imprime tal cual.
## Cada elemento de la lista de la Solution va por separado: en el PDF los separa su letra («a)», «b)»), y un trozo que
## cruzara dos elementos no se encontraría (medido el 2026-11-14: falso FAIL en un ejercicio de prueba).
testigos_solucion <- function(ex, excluir = "", n = 3L, largo = 30L, minimo = 20L) {
  corte <- "\\\\[A-Za-z]+\\*?|[\\\\$_^{}&%~<>|]|\\[|\\]|\n[ \t]*\n"
  partes <- c(paste(ex$solution, collapse = "\n"), as.character(unlist(ex$solutionlist)))
  trozos <- unlist(lapply(partes, function(x) strsplit(x, corte, perl = TRUE)[[1]]))
  t <- vapply(trozos, solo_letras, "", USE.NAMES = FALSE)
  t <- t[nchar(t) >= minimo]
  if (!length(t)) return(character(0))
  t <- unique(substr(t[order(-nchar(t))], 1L, largo))          # los trozos más largos primero
  t <- t[!vapply(t, grepl, TRUE, x = excluir, fixed = TRUE)]
  utils::head(t, n)
}

## Matriz N x Q de claves (letras minúsculas, como las casillas de la hoja NOPS) desde un objeto de R/exams.
clave_desde <- function(x) {
  m <- t(vapply(x, function(v) vapply(v, clave_letras, ""), character(length(x[[1]]))))
  if (length(x[[1]]) == 1L) m <- matrix(m, ncol = 1)
  dimnames(m) <- list(names(x), sprintf("p%02d", seq_len(ncol(m))))
  m
}

clave_texto <- function(fila) paste(sprintf("%d %s", seq_along(fila), fila), collapse = " · ")

## ---------------------------------------------------------------------------------------------------------------
## Registro
## ---------------------------------------------------------------------------------------------------------------
.reg <- new.env()
registrar <- function(...) {
  linea <- paste0(format(Sys.time(), "%H:%M:%S"), " ", paste0(...))
  cat(linea, "\n")
  if (!is.null(.reg$log)) cat(linea, "\n", file = .reg$log, append = TRUE)
}
etapa <- function(nombre, expr) {
  t0 <- Sys.time()
  registrar("INICIA ", nombre)
  v <- force(expr)
  registrar("TERMINA ", nombre, " (", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), " s)")
  invisible(v)
}

## file.rename() falla entre sistemas de archivos distintos (tempdir en /tmp y la salida en /home, MEDIDO): copiar y borrar.
mover <- function(de, a) {
  if (!file.exists(de)) stop("R/exams no dejó el archivo esperado: ", de)
  if (!file.copy(de, a, overwrite = TRUE)) stop("no se pudo copiar ", de, " a ", a)
  unlink(de)
  invisible(a)
}

sistema <- function(cmd, args, ...) {
  r <- suppressWarnings(system2(cmd, args, stdout = TRUE, stderr = TRUE, ...))
  st <- attr(r, "status") %||% 0L
  if (st != 0) stop(cmd, " falló (", st, "): ", paste(utils::tail(r, 5), collapse = " | "))
  invisible(r)
}

## ---------------------------------------------------------------------------------------------------------------
## Pre-vuelo (C9, C11, C12)
## ---------------------------------------------------------------------------------------------------------------
## Escaneo estático de C12. MEDIDO (knitr 1.51, código de knitr::knit): al terminar cada documento, knit() restaura
## opts_chunk, opts_current, knit_code y opts_knit, y devuelve knit_hooks a los valores por defecto; knit_engines NO se
## restaura (la fuga real del 2026-11-13). Por eso solo knit_engines$set( es FAIL; las otras tres llamadas se informan como
## RESTAURA (10 de los 11 ejercicios del examen real usan opts_chunk$set) y las vigila la instantánea dinámica.
PATRON_FUGA <- "knit_engines\\$set\\("
PATRONES_RESTAURADOS <- c("opts_chunk\\$set\\(", "opts_knit\\$set\\(", "knit_hooks\\$set\\(")
PERMITIDO <- "^\\s*(knitr::)?knit_engines\\$set\\(\\s*python\\s*=\\s*reticulate::eng_python\\s*\\)\\s*;?\\s*(#.*)?$"

escaneo_estatico <- function(ruta) {
  l <- readLines(ruta, warn = FALSE, encoding = "UTF-8")
  activa <- !grepl("^\\s*#", l)
  fuga <- which(activa & grepl(PATRON_FUGA, l) & !grepl(PERMITIDO, l, perl = TRUE))
  rest <- which(activa & Reduce(`|`, lapply(PATRONES_RESTAURADOS, grepl, x = l)))
  if (length(fuga)) return(paste0("CAMBIA:", paste0("línea", fuga, collapse = ",")))
  if (length(rest)) return(paste0("RESTAURA:", paste0("línea", rest, collapse = ",")))
  "igual"
}

## Lee preflight.log. Devuelve un data.frame por ejercicio y cómo terminó el hijo.
leer_preflight <- function(log) {
  l <- if (file.exists(log)) trimws(readLines(log, warn = FALSE)) else character(0)
  campo <- function(linea, k) sub(paste0("^.*\\b", k, "=(\\S+).*$"), "\\1", linea, perl = TRUE)
  ini <- grep("^INICIO ", l, value = TRUE); fin <- grep("^FIN ", l, value = TRUE)
  est <- grep("^ESTATICO ", l, value = TRUE); err <- grep("^ERROR ", l, value = TRUE)
  num <- function(v) as.integer(sub("^\\S+ (\\d+).*$", "\\1", v))
  Q <- max(c(0L, num(est), num(ini)))
  d <- data.frame(j = seq_len(Q), ejercicio = NA_character_, t_inicio = NA_real_, fin = FALSE, tipos = NA_character_,
                  max_opciones = NA_integer_, estado = NA_character_, nuevos = NA_character_, estatico = NA_character_,
                  stringsAsFactors = FALSE)
  for (x in est) d$estatico[num(x)] <- sub("^ESTATICO \\d+ (\\S+).*$", "\\1", x)
  for (x in ini) { j <- num(x); d$t_inicio[j] <- as.numeric(campo(x, "t")); d$ejercicio[j] <- sub("^.*t=\\S+ ", "", x) }
  for (x in fin) {
    j <- num(x); d$fin[j] <- TRUE; d$tipos[j] <- campo(x, "tipos")
    d$max_opciones[j] <- as.integer(campo(x, "max_opciones")); d$estado[j] <- campo(x, "estado")
    if (grepl("\\bnuevos=", x, perl = TRUE)) d$nuevos[j] <- campo(x, "nuevos")
  }
  list(tabla = d, terminado = grep("^terminado por ", l, value = TRUE), error = err,
       completo = any(l == "TERMINADO"))
}

## Juicio del pre-vuelo: lista de FAIL por guarda. con_hoja = la sesión lleva hoja NOPS.
juzgar_preflight <- function(pf, con_hoja) {
  d <- pf$tabla
  f9 <- character(0); f11 <- character(0); f12 <- character(0)
  if (length(pf$terminado)) f9 <- c(f9, paste0(pf$terminado, collapse = "; "))
  if (length(pf$error)) f9 <- c(f9, paste0("el pre-vuelo terminó con error: ", paste(pf$error, collapse = "; ")))
  sin_fin <- d$j[!is.na(d$t_inicio) & !d$fin]
  if (length(sin_fin)) f9 <- c(f9, paste0("ejercicio ", sin_fin, " sin FIN: ", basename(d$ejercicio[sin_fin]), collapse = "; "))
  if (!pf$completo && !length(f9)) f9 <- "el pre-vuelo no llegó a TERMINADO"
  if (con_hoja) {
    malos <- d$j[d$fin & !grepl("^(schoice|mchoice)(/(schoice|mchoice))*$", d$tipos)]
    if (length(malos)) f11 <- c(f11, paste0("tipo no admitido por la hoja NOPS en el ejercicio ", malos, " (", d$tipos[malos], ")",
                                            collapse = "; "))
    muchas <- d$j[d$fin & d$max_opciones > 5]
    if (length(muchas)) f11 <- c(f11, paste0("más de 5 opciones en el ejercicio ", muchas, " (", d$max_opciones[muchas], ")",
                                             collapse = "; "))
  }
  cam <- d$j[d$fin & d$estado != "igual"]
  if (length(cam)) f12 <- c(f12, paste0("instantánea: ejercicio ", cam, " ", basename(d$ejercicio[cam]), " ", d$estado[cam],
                                        collapse = "; "))
  est <- d$j[!is.na(d$estatico) & startsWith(d$estatico, "CAMBIA")]
  if (length(est)) f12 <- c(f12, paste0("escaneo estático: ejercicio ", est, " ", d$estatico[est], collapse = "; "))
  list(C9 = f9, C11 = f11, C12 = f12)
}

preflight <- function(cfg, ses, S, dir_ses) {
  log <- file.path(dir_ses, "preflight.log")
  for (j in seq_along(ses$ejercicios))
    cat("ESTATICO", j, escaneo_estatico(ses$ejercicios[j]), "\n", file = log, append = TRUE)
  man <- file.path(dir_ses, "preflight_manifiesto.rds")
  saveRDS(list(ejercicios = ses$ejercicios, S = S, N = cfg$estudiantes, log = log), man)
  pres <- cfg$presupuesto_s
  respaldo <- length(ses$ejercicios) * pres
  p <- processx::process$new("Rscript", c(file.path(dir_motor(), "preflight_hijo.R"), man),
                             stdout = file.path(dir_ses, "preflight_hijo.out"), stderr = "2>&1", cleanup_tree = TRUE)
  t0 <- Sys.time()
  repeat {
    if (!p$is_alive()) break
    Sys.sleep(2)
    pf <- leer_preflight(log)
    d <- pf$tabla
    abierto <- d$j[!is.na(d$t_inicio) & !d$fin]
    if (length(abierto)) {
      j <- max(abierto)
      if (as.numeric(Sys.time()) - d$t_inicio[j] > pres) {
        p$kill_tree()
        cat("terminado por vigilante: ejercicio", j, basename(d$ejercicio[j]),
            paste0("tras=", round(as.numeric(Sys.time()) - d$t_inicio[j], 1)), paste0("presupuesto=", pres), "\n",
            file = log, append = TRUE)
        break
      }
    }
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > respaldo) {
      p$kill_tree()
      cat("terminado por respaldo", paste0("(", respaldo, " s en total)"), "\n", file = log, append = TRUE)
      break
    }
  }
  juzgar_preflight(leer_preflight(log), "hoja" %in% cfg$salidas)
}

## ---------------------------------------------------------------------------------------------------------------
## Salidas
## ---------------------------------------------------------------------------------------------------------------
cabecera <- function(cfg, ses, i, extra = list()) {
  c(list(ID = id_version(cfg$fecha, ses$startid, i), Date = format(cfg$fecha),
         Titulo = escapar_latex(cfg$titulo), Institucion = escapar_latex(cfg$institucion),
         Sesion = escapar_latex(ses$nombre)), extra)
}

generar_hojas <- function(cfg, ses, S, dir_ses) {
  N <- cfg$estudiantes
  dn <- file.path(dir_ses, "nops"); dh <- file.path(dir_ses, "hojas")
  dir.create(dn); dir.create(dh)
  xn <- exams::exams2nops(ses$ejercicios, n = N, seed = S, startid = ses$startid, date = cfg$fecha, language = "es",
                          institution = escapar_latex(cfg$institucion), title = escapar_latex(cfg$titulo), duplex = TRUE,
                          twocolumn = FALSE, dir = dn, name = "nops_")
  ids <- names(xn)
  for (i in seq_len(N)) {
    crudo <- file.path(dn, nombre_copia("nops_", i, N, "pdf"))
    tmp <- tempfile(fileext = ".pdf")
    ## la hoja y su reverso en blanco (MEDIDO: con duplex = TRUE la página 2 sale en blanco)
    sistema("qpdf", c("--warning-exit-0", shQuote(crudo), "--pages", shQuote(crudo), "1-2", "--", shQuote(tmp)))
    sistema("pdfjam", c("--papersize", shQuote("{216mm,330mm}"), "--noautoscale", "true", "--quiet",
                        shQuote(tmp), "-o", shQuote(file.path(dh, paste0("hoja_respuestas_", ids[i], ".pdf")))))
  }
  sistema("pdfunite", c(shQuote(file.path(dh, paste0("hoja_respuestas_", ids, ".pdf"))), shQuote(file.path(dh, "hojas_imprimir.pdf"))))
  xn
}

escribir_clave <- function(cfg, ses, clave, dir_ses) {
  dc <- file.path(dir_ses, "clave"); dir.create(dc)
  utils::write.csv(data.frame(version = rownames(clave), clave, check.names = FALSE), file.path(dc, "clave.csv"),
                   row.names = FALSE)
  Q <- ncol(clave)
  apaisado <- Q > 20
  filas <- paste0(rownames(clave), " & ", apply(clave, 1, paste, collapse = " & "), " \\\\")
  tex <- c("\\documentclass[10pt]{article}",
           sprintf("\\usepackage[papersize={216mm,330mm},margin=12mm%s]{geometry}", if (apaisado) ",landscape" else ""),
           "\\usepackage{helvet}\\renewcommand{\\familydefault}{\\sfdefault}\\usepackage[T1]{fontenc}",
           "\\usepackage{longtable,booktabs}\\pagestyle{empty}", "\\begin{document}",
           sprintf("\\textbf{%s} \\textperiodcentered{} %s \\textperiodcentered{} Clave por versión\\par\\medskip",
                   escapar_latex(cfg$titulo), escapar_latex(ses$nombre)),
           "\\footnotesize\\setlength{\\tabcolsep}{3pt}",
           sprintf("\\begin{longtable}{l%s}", strrep("c", Q)), "\\toprule",
           paste0("Versión & ", paste(seq_len(Q), collapse = " & "), " \\\\"), "\\midrule\\endhead",
           filas, "\\bottomrule", "\\end{longtable}", "\\end{document}")
  td <- tempfile(); dir.create(td)
  writeLines(tex, file.path(td, "clave.tex"))
  for (k in 1:2) sistema("pdflatex", c("-interaction=nonstopmode", "-halt-on-error", "-output-directory", shQuote(td),
                                       shQuote(file.path(td, "clave.tex"))))
  file.copy(file.path(td, "clave.pdf"), file.path(dc, "clave.pdf"), overwrite = TRUE)
}

## Una llamada por versión: exams2pdf agrega el índice de copia a `name`, así que se genera con name = "tmp_" y se renombra.
NOMBRE_PDF <- c(cuadernillos = "cuadernillo_%s.pdf", docente = "cuadernillo_docente_%s.pdf", retro = "retro_%s.pdf")
generar_pdf_por_version <- function(cfg, ses, S, dir_ses, ids, tipo, plantilla_tex, extra_header = NULL) {
  dd <- file.path(dir_ses, tipo); dir.create(dd)
  objs <- vector("list", cfg$estudiantes)
  for (i in seq_len(cfg$estudiantes)) {
    tmpd <- tempfile(); dir.create(tmpd)
    texdir <- file.path(dir_ses, "tex", tipo, ids[i])
    dir.create(dirname(texdir), recursive = TRUE, showWarnings = FALSE)   # exams2pdf crea texdir sin recursive
    extra <- if (is.null(extra_header)) list() else extra_header(i)
    x <- exams::exams2pdf(ses$ejercicios, n = 1, seed = S[i, , drop = FALSE], template = plantilla_tex,
                          header = cabecera(cfg, ses, i, extra), texdir = texdir, dir = tmpd, name = "tmp_")
    destino <- file.path(dd, sprintf(NOMBRE_PDF[[tipo]], ids[i]))
    mover(file.path(tmpd, nombre_copia("tmp_", 1L, 1L, "pdf")), destino)
    objs[[i]] <- x[[1]]
  }
  names(objs) <- ids
  objs
}

## ---------------------------------------------------------------------------------------------------------------
## Word: ninguna imagen ni tabla más ancha que el espacio que le deja su columna
## ---------------------------------------------------------------------------------------------------------------
## MEDIDO el 2026-11-14 con el examen real: pandoc calcula el ancho de una imagen contra el ancho ÚTIL DE LA PÁGINA
## (pgSz menos márgenes), no contra la columna ni contra la sangría de la lista. Con el Word a dos columnas, una imagen
## con {width=100%} salía a 19,58 cm en una columna de 9,50 cm, y una gráfica de 9,17 cm se salía de los 8,23 cm que deja
## la sangría de la pregunta. El motor reduce cada imagen al espacio disponible, sin deformarla; nunca la amplía. Con las
## tablas pasa lo mismo (ver docx_tablas): se les reparte la rejilla para que quepan.
## Unidades: twips en sectPr y numbering.xml; EMU en los dibujos (1 twip = 635 EMU; 1 cm = 360000 EMU).
EMU_POR_TWIP <- 635

docx_parte <- function(docx, parte) {
  x <- suppressWarnings(system2("unzip", c("-p", shQuote(docx), shQuote(parte)), stdout = TRUE, stderr = FALSE))
  x <- paste(x, collapse = "\n"); Encoding(x) <- "UTF-8"
  x
}

attr_num <- function(x, patron) {
  m <- regmatches(x, regexec(patron, x, perl = TRUE))[[1]]
  if (length(m) < 2) NA_real_ else as.numeric(m[2])
}

## Ancho de UNA columna, en twips, leído de la sección del propio documento.
docx_columna_twips <- function(doc) {
  sec <- regmatches(doc, regexpr("(?s)<w:sectPr\\b.*?</w:sectPr>", doc, perl = TRUE))
  if (!length(sec)) stop("el Word no trae sectPr")
  w <- attr_num(sec, '<w:pgSz\\b[^>]*\\bw:w="(\\d+)"')
  izq <- attr_num(sec, '<w:pgMar\\b[^>]*\\bw:left="(\\d+)"'); der <- attr_num(sec, '<w:pgMar\\b[^>]*\\bw:right="(\\d+)"')
  n <- attr_num(sec, '<w:cols\\b[^>]*\\bw:num="(\\d+)"'); esp <- attr_num(sec, '<w:cols\\b[^>]*\\bw:space="(\\d+)"')
  if (is.na(n)) n <- 1
  if (is.na(esp)) esp <- 720                       # separación por defecto de Word
  if (anyNA(c(w, izq, der))) stop("el Word no declara pgSz o pgMar")
  (w - izq - der - esp * (n - 1)) / n
}

## Sangría izquierda (twips) del párrafo: la suya propia (w:ind) o la del nivel de su lista en numbering.xml.
## Si numbering.xml no la declara, la regla de pandoc: 720 twips por nivel (MEDIDO: 720, 1440, 2160…).
docx_sangria_twips <- function(parrafo, numbering) {
  ppr <- regmatches(parrafo, regexpr("(?s)<w:pPr>.*?</w:pPr>", parrafo, perl = TRUE))
  if (!length(ppr)) return(0)
  propia <- attr_num(ppr, '<w:ind\\b[^>]*\\bw:left="(\\d+)"')
  if (!is.na(propia)) return(propia)
  ilvl <- attr_num(ppr, '<w:ilvl\\b[^>]*\\bw:val="(\\d+)"'); numid <- attr_num(ppr, '<w:numId\\b[^>]*\\bw:val="(\\d+)"')
  if (is.na(ilvl) || is.na(numid)) return(0)
  regla <- 720 * (ilvl + 1)
  num <- regmatches(numbering, regexpr(sprintf('(?s)<w:num\\b[^>]*\\bw:numId="%d"[^>]*>.*?</w:num>', numid), numbering, perl = TRUE))
  if (!length(num)) return(regla)
  abs_id <- attr_num(num, '<w:abstractNumId\\b[^>]*\\bw:val="(\\d+)"')
  if (is.na(abs_id)) return(regla)
  ab <- regmatches(numbering, regexpr(sprintf('(?s)<w:abstractNum\\b[^>]*\\bw:abstractNumId="%d"[^>]*>.*?</w:abstractNum>', abs_id),
                                      numbering, perl = TRUE))
  if (!length(ab)) return(regla)
  lvl <- regmatches(ab, regexpr(sprintf('(?s)<w:lvl\\b[^>]*\\bw:ilvl="%d"[^>]*>.*?</w:lvl>', ilvl), ab, perl = TRUE))
  if (!length(lvl)) return(regla)
  izq <- attr_num(lvl, '<w:ind\\b[^>]*\\bw:left="(\\d+)"')
  if (is.na(izq)) regla else izq
}

PATRON_PARRAFO <- "(?s)<w:p(?: [^>]*)?>.*?</w:p>"
PATRON_DIBUJO <- "(?s)<w:drawing>.*?</w:drawing>"

## Una fila por imagen: ancho y alto (EMU) y el espacio disponible en su columna (EMU). La usan el motor y la guarda C1.
docx_imagenes <- function(docx) {
  doc <- docx_parte(docx, "word/document.xml"); numbering <- docx_parte(docx, "word/numbering.xml")
  col <- docx_columna_twips(doc)
  filas <- list()
  for (par in regmatches(doc, gregexpr(PATRON_PARRAFO, doc, perl = TRUE))[[1]]) {
    dib <- regmatches(par, gregexpr(PATRON_DIBUJO, par, perl = TRUE))[[1]]
    if (!length(dib)) next
    disp <- (col - docx_sangria_twips(par, numbering)) * EMU_POR_TWIP
    for (d in dib) filas[[length(filas) + 1L]] <- data.frame(
      cx = attr_num(d, '<wp:extent\\b[^>]*\\bcx="(\\d+)"'), cy = attr_num(d, '<wp:extent\\b[^>]*\\bcy="(\\d+)"'), disponible = disp)
  }
  if (length(filas)) do.call(rbind, filas) else data.frame(cx = numeric(0), cy = numeric(0), disponible = numeric(0))
}

PATRON_TABLA <- "(?s)<w:tbl>.*?</w:tbl>"

## Una fila por tabla: ancho (suma de su rejilla), sangría y espacio disponible en su columna, en twips.
## MEDIDO el 2026-11-14: pandoc escribe toda tabla con una rejilla de 7920 twips (13,97 cm) repartida por igual entre sus
## columnas, más la sangría de la lista (720). En una columna de 9,50 cm, LibreOffice muestra la tabla cortada: a la de
## la pregunta 3 del examen real (4 columnas) solo se le veía una columna de datos.
docx_tablas <- function(docx) {
  doc <- docx_parte(docx, "word/document.xml")
  col <- docx_columna_twips(doc)
  tabs <- regmatches(doc, gregexpr(PATRON_TABLA, doc, perl = TRUE))[[1]]
  if (!length(tabs)) return(data.frame(ancho = numeric(0), sangria = numeric(0), disponible = numeric(0)))
  do.call(rbind, lapply(tabs, function(t) {
    g <- as.numeric(regmatches(t, gregexpr('(?<=<w:gridCol w:w=")\\d+', t, perl = TRUE))[[1]])
    ind <- attr_num(t, '<w:tblInd\\b[^>]*\\bw:w="(\\d+)"'); if (is.na(ind)) ind <- 0
    data.frame(ancho = sum(g), sangria = ind, disponible = col - ind)
  }))
}

reducir_imagenes <- function(doc, numbering, col) {
  reducidas <- 0L
  mp <- gregexpr(PATRON_PARRAFO, doc, perl = TRUE)
  pars <- regmatches(doc, mp)[[1]]
  for (k in seq_along(pars)) {
    md <- gregexpr(PATRON_DIBUJO, pars[k], perl = TRUE)
    dib <- regmatches(pars[k], md)[[1]]
    if (!length(dib)) next
    disp <- floor((col - docx_sangria_twips(pars[k], numbering)) * EMU_POR_TWIP)
    for (j in seq_along(dib)) {
      cx <- attr_num(dib[j], '<wp:extent\\b[^>]*\\bcx="(\\d+)"'); cy <- attr_num(dib[j], '<wp:extent\\b[^>]*\\bcy="(\\d+)"')
      if (is.na(cx) || is.na(cy) || cx <= disp) next
      viejo <- sprintf('cx="%.0f" cy="%.0f"', cx, cy)
      nuevo <- sprintf('cx="%.0f" cy="%.0f"', disp, round(cy * disp / cx))
      if (!grepl(viejo, dib[j], fixed = TRUE)) stop("Word: no se encontró ", viejo, " en el dibujo ", j)
      dib[j] <- gsub(viejo, nuevo, dib[j], fixed = TRUE)      # wp:extent y a:ext llevan el mismo par
      reducidas <- reducidas + 1L
    }
    regmatches(pars[k], md) <- list(dib)
  }
  if (reducidas) regmatches(doc, mp) <- list(pars)
  list(doc = doc, n = reducidas)
}

## Reparte la rejilla de cada tabla que no cabe, en la misma proporción, y fija su ancho (w:tblW) en el nuevo total.
reducir_tablas <- function(doc, col) {
  reducidas <- 0L
  mt <- gregexpr(PATRON_TABLA, doc, perl = TRUE)
  tabs <- regmatches(doc, mt)[[1]]
  for (k in seq_along(tabs)) {
    mg <- gregexpr('<w:gridCol w:w="\\d+"', tabs[k], perl = TRUE)
    cols <- regmatches(tabs[k], mg)[[1]]
    if (!length(cols)) next
    g <- as.numeric(sub('.*"(\\d+)"$', "\\1", cols))
    ind <- attr_num(tabs[k], '<w:tblInd\\b[^>]*\\bw:w="(\\d+)"'); if (is.na(ind)) ind <- 0
    disp <- col - ind
    if (sum(g) <= disp) next
    g2 <- floor(g * disp / sum(g))
    regmatches(tabs[k], mg) <- list(sprintf('<w:gridCol w:w="%.0f"', g2))
    tabs[k] <- sub("<w:tblW\\b[^>]*/>", sprintf('<w:tblW w:type="dxa" w:w="%.0f" />', sum(g2)), tabs[k], perl = TRUE)
    reducidas <- reducidas + 1L
  }
  if (reducidas) regmatches(doc, mt) <- list(tabs)
  list(doc = doc, n = reducidas)
}

## Reduce en el propio .docx lo que no cabe en su columna. Devuelve cuántas imágenes y cuántas tablas redujo.
ajustar_docx_a_columna <- function(docx) {
  doc <- docx_parte(docx, "word/document.xml"); numbering <- docx_parte(docx, "word/numbering.xml")
  col <- docx_columna_twips(doc)
  ri <- reducir_imagenes(doc, numbering, col)
  rt <- reducir_tablas(ri$doc, col)
  hecho <- c(imagenes = ri$n, tablas = rt$n)
  if (!sum(hecho)) return(hecho)
  td <- tempfile(); dir.create(td)
  sistema("unzip", c("-q", "-o", shQuote(docx), "-d", shQuote(td)))
  writeBin(charToRaw(enc2utf8(rt$doc)), file.path(td, "word", "document.xml"))
  nuevo <- tempfile(fileext = ".docx")
  ## [Content_Types].xml primero, como lo escribe Word; -X: sin atributos extra del sistema de archivos
  sistema("sh", c("-c", shQuote(sprintf("cd %s && zip -q -X %s '[Content_Types].xml' && zip -q -X -r %s . -x '[Content_Types].xml'",
                                        shQuote(td), shQuote(nuevo), shQuote(nuevo)))))
  if (!file.copy(nuevo, docx, overwrite = TRUE)) stop("no se pudo reescribir ", docx)
  unlink(c(td, nuevo), recursive = TRUE)
  hecho
}

generar_word <- function(cfg, ses, S, dir_ses, ids) {
  dw <- file.path(dir_ses, "word"); dir.create(dw)
  ref <- normalizePath(plantilla("referencia_oficio_2col.docx"))      # ANTES: options se evalúa tras cambiar de directorio
  tpl <- normalizePath(plantilla("pandoc_cuadernillo.tex"))
  objs <- vector("list", cfg$estudiantes)
  for (i in seq_len(cfg$estudiantes)) {
    tmpd <- tempfile(); dir.create(tmpd)
    ## en el Word el título va SIN escapar: pandoc lee el .tex y conserva «&» y «_» (MEDIDO el 2026-11-13)
    x <- exams::exams2pandoc(ses$ejercicios, n = 1, seed = S[i, , drop = FALSE], type = "docx", template = tpl,
                             solution = FALSE, resolution = 300, options = paste0("--reference-doc=", ref),
                             header = list(ID = id_version(cfg$fecha, ses$startid, i), Titulo = cfg$titulo),
                             dir = tmpd, name = "tmp_")
    destino <- file.path(dw, paste0("cuadernillo_", ids[i], ".docx"))
    mover(file.path(tmpd, nombre_copia("tmp_", 1L, 1L, "docx")), destino)
    ajustar_docx_a_columna(destino)
    objs[[i]] <- x[[1]]
  }
  names(objs) <- ids
  objs
}

hojas_gris <- function(dir_ses, ids) {
  for (v in unique(c(1L, length(ids)))) {
    etiqueta <- if (v == 1L) "v1" else "vN"
    pdf <- file.path(dir_ses, "cuadernillos", paste0("cuadernillo_", ids[v], ".pdf"))
    td <- tempfile(); dir.create(td)
    sistema("pdftoppm", c("-r", "50", "-gray", "-png", shQuote(pdf), shQuote(file.path(td, "p"))))
    pags <- sort(list.files(td, "^p-.*\\.png$", full.names = TRUE))
    sistema("montage", c(shQuote(pags), "-tile", paste0(min(4, length(pags)), "x"), "-geometry", "+6+6",
                         "-background", "white", shQuote(file.path(dir_ses, paste0("revision_gris_", etiqueta, ".png")))))
    sistema("pdftoppm", c("-r", "150", "-gray", "-png", shQuote(pdf), shQuote(file.path(td, "q"))))
    for (q in list.files(td, "^q-.*\\.png$", full.names = TRUE)) {
      k <- as.integer(sub("^q-0*(\\d+)\\.png$", "\\1", basename(q)))
      file.copy(q, file.path(dir_ses, sprintf("revision_gris_%s_p%d.png", etiqueta, k)), overwrite = TRUE)
    }
  }
}

## ---------------------------------------------------------------------------------------------------------------
## Una sesión completa
## ---------------------------------------------------------------------------------------------------------------
generar_sesion <- function(cfg, ses) {
  dir_ses <- file.path(cfg$dir_salida, ses$slug); dir.create(dir_ses, recursive = TRUE)
  .reg$log <- file.path(dir_ses, "motor.log")
  N <- cfg$estudiantes; Q <- length(ses$ejercicios)
  registrar("sesión ", ses$k, " (", ses$nombre, "): ", Q, " preguntas, ", N, " versiones, salidas = ",
            paste(cfg$salidas, collapse = ","))
  S <- matriz_semillas(cfg$semilla, ses$k, N, Q)
  saveRDS(S, file.path(dir_ses, "semillas.rds"))
  utils::write.csv(S, file.path(dir_ses, "semillas.csv"), row.names = FALSE)
  writeLines(cfg$salidas, file.path(dir_ses, "salidas.txt"))
  fp <- etapa("pre-vuelo", preflight(cfg, ses, S, dir_ses))
  firmas <- list(ids = NULL, ids_nops = NULL, ejercicios = ses$ejercicios, salidas = cfg$salidas, huella = list(),
                 clave = list(), preflight = fp, startid = ses$startid, fecha = cfg$fecha,
                 partir_preguntas = cfg$partir_preguntas, testigos = NULL)
  if (length(unlist(fp))) {
    registrar("PRE-VUELO EN ROJO: no se genera nada. ", paste(names(fp)[lengths(fp) > 0], collapse = ", "))
    firmas$ids <- vapply(seq_len(N), function(i) id_version(cfg$fecha, ses$startid, i), "")
    saveRDS(firmas, file.path(dir_ses, "firmas.rds"))
    return(dir_ses)
  }
  ids <- vapply(seq_len(N), function(i) id_version(cfg$fecha, ses$startid, i), "")
  guardar <- function(nombre, objs) {
    firmas$huella[[nombre]] <<- t(vapply(objs, function(v) vapply(v, huella, ""), character(Q)))
    firmas$clave[[nombre]] <<- clave_desde(objs)
    if (Q == 1L) firmas$huella[[nombre]] <<- matrix(firmas$huella[[nombre]], ncol = 1)
  }
  if ("hoja" %in% cfg$salidas) {
    xn <- etapa("hojas NOPS", generar_hojas(cfg, ses, S, dir_ses))
    ids <- names(xn); firmas$ids_nops <- ids
    guardar("nops", xn)
  }
  firmas$ids <- ids
  if ("clave" %in% cfg$salidas) {
    xc <- if (!is.null(firmas$clave$nops)) firmas$clave$nops else NULL
    if (is.null(xc)) {      # sin hoja: la clave sale de una pasada de exams2pdf sin compilar (xexams)
      x <- exams::xexams(ses$ejercicios, n = N, seed = S, dir = tempfile())
      names(x) <- ids; xc <- clave_desde(x)
    }
    etapa("clave", escribir_clave(cfg, ses, xc, dir_ses))
  }
  if ("cuadernillo" %in% cfg$salidas) {
    ## La impresión a doble columna da SIEMPRE dos cuadernillos por versión, con la misma plantilla y las mismas semillas:
    ## el del estudiante (sin retroalimentación) y el del docente (con ella y con la clave). Decisión del profesor, 2026-11-14.
    partir <- list(Partir = if (cfg$partir_preguntas) "1" else "0")
    xa <- etapa("cuadernillos del estudiante", generar_pdf_por_version(cfg, ses, S, dir_ses, ids, "cuadernillos",
                  plantilla("cuadernillo_oficio_2col.tex"), extra_header = function(i) partir))
    guardar("cuadernillo", xa)
    sistema("pdfunite", c(shQuote(file.path(dir_ses, "cuadernillos", paste0("cuadernillo_", ids, ".pdf"))),
                          shQuote(file.path(dir_ses, "cuadernillos", "cuadernillos_imprimir.pdf"))))
    cld <- firmas$clave$nops %||% firmas$clave$cuadernillo
    xd <- etapa("cuadernillos del docente", generar_pdf_por_version(cfg, ses, S, dir_ses, ids, "docente",
                  plantilla("cuadernillo_oficio_2col.tex"),
                  extra_header = function(i) c(partir, list(Docente = "1", Clave = clave_texto(cld[i, ])))))
    guardar("docente", xd)
    sistema("pdfunite", c(shQuote(file.path(dir_ses, "docente", paste0("cuadernillo_docente_", ids, ".pdf"))),
                          shQuote(file.path(dir_ses, "docente", "docente_imprimir.pdf"))))
  }
  ## testigos de la retroalimentación (C15): de cualquier salida de exams2pdf o del Word, que traen la Solution en el objeto
  xt <- if ("cuadernillo" %in% cfg$salidas) xa else NULL
  if ("retro" %in% cfg$salidas) {
    cl <- firmas$clave$nops %||% firmas$clave$cuadernillo
    xr <- etapa("retroalimentación", generar_pdf_por_version(
      cfg, ses, S, dir_ses, ids, "retro", plantilla("retro_celular.tex"),
      extra_header = function(i) if (is.null(cl)) list() else list(Clave = clave_texto(cl[i, ]))))
    guardar("retro", xr)
    if (is.null(xt)) xt <- xr
  }
  if (!is.null(xt)) firmas$testigos <- lapply(xt, function(v) {
    preguntas <- solo_letras(unlist(lapply(v, function(e) c(e$question, unlist(e$questionlist)))))
    lapply(v, testigos_solucion, excluir = preguntas)
  })
  if ("word" %in% cfg$salidas) {
    xw <- etapa("Word", generar_word(cfg, ses, S, dir_ses, ids))
    guardar("word", xw)
  }
  saveRDS(firmas, file.path(dir_ses, "firmas.rds"))
  if ("cuadernillo" %in% cfg$salidas) etapa("hojas en gris (C10)", hojas_gris(dir_ses, ids))
  dir_ses
}

## ---------------------------------------------------------------------------------------------------------------
## CLI
## ---------------------------------------------------------------------------------------------------------------
parsear_args <- function(args) {
  op <- list(config = NULL, salidas = NULL, dir_salida = NULL, limpiar = FALSE)
  i <- 1L
  while (i <= length(args)) {
    a <- args[i]
    if (a == "--limpiar") op$limpiar <- TRUE
    else if (a == "--salidas") { i <- i + 1L; op$salidas <- strsplit(args[i], ",", fixed = TRUE)[[1]] }
    else if (a == "--dir-salida") { i <- i + 1L; op$dir_salida <- args[i] }
    else if (startsWith(a, "--")) stop("ERR_CONFIG: opción desconocida ", a)
    else op$config <- a
    i <- i + 1L
  }
  if (is.null(op$config)) stop("ERR_CONFIG: uso: Rscript generar_cuadernillos.R <config.R> [--salidas …] [--dir-salida <ruta>] [--limpiar]")
  op
}

main <- function(args = commandArgs(TRUE)) {
  st <- tryCatch({
    op <- parsear_args(args)
    cfg <- leer_config(op$config, op$dir_salida)
    if (!is.null(op$salidas)) {
      if (length(setdiff(op$salidas, SALIDAS_VALIDAS))) stop("ERR_CONFIG: salidas desconocidas: ", paste(op$salidas, collapse = ","))
      cfg$salidas <- op$salidas
    }
    preparar_salida(cfg$dir_salida, op$limpiar)
    suppressMessages(library(exams))
    Sys.setenv(MPLBACKEND = "Agg")
    assign(".exams_generation_mode", TRUE, envir = globalenv())
    options(tinytex.clean = FALSE)                               # conserva el .log de cada compilación (C3)
    suppressMessages(requireNamespace("tinytex", quietly = TRUE))  # MEDIDO: si no, el primer include_tikz imprime
                                                                   # «Loading required namespace: tinytex» en el PDF
    salidas_ses <- vapply(cfg$sesiones, function(ses) generar_sesion(cfg, ses), "")
    verif <- file.path(dir_motor(), "verificar_cuadernillos.R")
    codigos <- vapply(salidas_ses, function(d) {
      r <- system2(file.path(R.home("bin"), "Rscript"), c(shQuote(verif), shQuote(d)), env = paste0("CUADERNILLOS_MOTOR_DIR=", dir_motor()))
      as.integer(r)
    }, 0L)
    if (any(codigos == 1L)) 1L else if (any(codigos != 0L)) 2L else 0L
  }, error = function(e) {
    msg <- conditionMessage(e)
    cat("ERROR:", msg, "\n", file = stderr())
    if (startsWith(msg, "ERR_DIR_SALIDA")) 2L else 1L
  })
  quit(save = "no", status = st)
}

if (sys.nframe() == 0L) main()
