## verificar_coherencias.R — Paso 8 (coherencias_5), ronda 9, .Rmd md5 f98f9dda47e4f3cafc4e8b4e82cc956b
##
## Verificador INDEPENDIENTE del generador: no usa ningún objeto interno del data_generation (ni `verdad()`, ni `verdad_p6()`,
## ni `afs`, ni P2, valor_p2, op2, expl2, clave2_expl) y NO abre el .Rmd para reconstruir el pool de la Parte 2. Toma lo que
## R/exams EMITE para cada semilla (`xexams()`: pregunta en Markdown con sus dos tablas, opciones tras el exshuffle, vector
## `exsolution`, Solution y retroalimentación) y lo recalcula desde las tablas MOSTRADAS: totales, ganador, diferencia, veredicto
## de cada afirmación leída del texto y valor de cada operación de la Parte 2.
## Cambios respecto de la ronda 3 (md5 42f725bf…): la Parte 2 tiene un solo tipo («¿Qué indica … el promedio …?») con un banco de
## 3 claves y 5 distractores que juzgo yo (POOL_P2, más abajo); el tipo «comparar» (pregunta, encabezado de la Solution y párrafo
## de «Procedimiento») se retiró y ahora es una comprobación NEGATIVA (RETIRADO_TIPO). Las Partes 1, 3, 4, 5 y 6, la Solution, las
## ecuaciones y los formatos conservan todas las comprobaciones de la ronda 3.
## Incluye mutaciones de control: si una alteración deliberada no hace fallar al verificador POR SU PROPIA SONDA, no vale.
##
## Uso (desde esta carpeta): TMPDIR=<tmp> Rscript --no-init-file verificar_coherencias.R [semillas...]
##   sin argumentos: la muestra estándar N = 100 (semillas 1:100; regla #23), que incluye las 5 versiones renderizadas.
suppressMessages(library(exams))
if (!exists("DIR_S")) DIR_S <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))))   # carpeta de este script (coherencias_r10); el directorio de trabajo está FUERA del repo
F <- normalizePath(file.path(DIR_S, "..", "..", "totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"))
cat("md5 del .Rmd:", unname(tools::md5sum(F)), "\n")
arg <- as.integer(commandArgs(trailingOnly = TRUE))
sem <- if (length(arg)) arg else 1:100
EN_PROFUNDIDAD <- c(canonica = 7L, CxC = 3L, CxP = 2L, PxC = 1L, PxP = 4L)

## ---------------------------------------------------------------------------------------------------------------
NBSP <- intToUtf8(160L)
nb <- function(x) gsub(NBSP, " ", x, fixed = TRUE)
limpiar <- function(x) trimws(nb(gsub("**", "", gsub("<[^>]+>", "", x), fixed = TRUE)))
num <- function(s) as.numeric(unlist(regmatches(s, gregexpr("[0-9]+", s))))

## ---------------------------------------------------------------------------------------------------------------
## PARTE 2 — MI juicio semántico, escrito leyendo cada texto contra la pregunta vigente
##   «¿Qué indica, en la tabla del otro colegio, el promedio de <material> recogido por curso de un grado?»
## Criterio mío: el promedio por curso de un grado es lo que recogió el grado dividido entre el número de SUS cursos (lo que le
## tocaría a cada curso si el total se repartiera por igual). Es CORRECTA la opción que describe esa cantidad; es INCORRECTA la que
## describe otra cantidad (otra operación) o algo que la tabla no da y que el promedio no indica.
## Los ocho textos salen de pool_parte2_emitido.R (lo que R/exams emite en 1:100). `juicio`, `por_que` y `ancla` son míos.
## `ancla` = la frase que debe llevar la explicación de esa opción, escrita desde mi juicio (qué da realmente esa operación).
## P2_OP = la operación que describe cada texto, con n = cursos del grado, a = su promedio mostrado, T = n·a y N = cursos de los
## dos grados; se recalcula con la tabla MOSTRADA de cada versión (sin entrada = la tabla no permite calcularla).
P2_RE_PREG <- "^¿Qué indica, en la tabla del otro colegio, el promedio de (\\S+) recogido por curso de un grado\\?$"
## --- El juicio (correcta/incorrecta), el porqué y la operación salen de MI juicio ciego, escrito ANTES de ver las marcas: juicio_parte2_ciego.tsv.
## Aquí solo se les pone un identificador y la frase `ancla` que debe llevar la explicación de cada opción (mi lectura de qué da realmente esa operación).
TSV <- read.delim(file.path(DIR_S, "juicio_parte2_ciego.tsv"), sep = "\t", quote = "", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
P2_ID <- c("Lo que recogió ese grado dividido en partes iguales entre sus cursos." = "k1", "Lo que recogió ese grado dividido entre el número de sus cursos." = "k2",
           "Lo que recogió ese grado dividido entre la cantidad de cursos de ese mismo grado." = "k3", "Lo que recogió ese grado dividido en partes iguales entre sus cursos y luego entre dos." = "d_dos",
           "Lo que recogió ese grado dividido entre el número de sus cursos al cuadrado." = "d_cuad", "Lo que recogió ese grado dividido entre el promedio de sus cursos." = "d_prom",
           "Lo que recogió ese grado dividido entre la cantidad de cursos de ambos grados." = "d_ambos", "Lo que recogió la mayoría de los cursos de ese mismo grado." = "d_moda")
P2_ANCLA <- c(k1 = "dividido entre la cantidad de cursos", k2 = "dividido entre la cantidad de cursos", k3 = "dividido entre la cantidad de cursos", d_dos = "da la mitad del promedio",
              d_cuad = "da el promedio dividido otra vez entre los cursos", d_prom = "da la cantidad de cursos", d_ambos = "da menos que el promedio", d_moda = "la moda")
stopifnot("el TSV del juicio ciego tiene los ocho textos del banco" = nrow(TSV) == 8L && setequal(TSV$texto, names(P2_ID)) && all(TSV$juicio %in% c("correcta", "incorrecta")))
POOL_P2 <- data.frame(id = unname(P2_ID[TSV$texto]), texto = TSV$texto, juicio = TSV$juicio, por_que = TSV$por_que, ancla = unname(P2_ANCLA[P2_ID[TSV$texto]]), stringsAsFactors = FALSE)
## La operación de cada texto es la del TSV (n = cursos del grado, a = promedio mostrado, T = n·a, N = cursos de los dos grados); «sin operación» = la tabla no permite calcularla.
P2_OP <- lapply(setNames(TSV$operacion, P2_ID[TSV$texto]), function(op) { if (grepl("^sin operaci", op)) return(NULL)
  ex_ <- parse(text = sub(" = .*$", "", op)); function(n, a, N) { T <- n * a; eval(ex_) } })
P2_OP <- P2_OP[!vapply(P2_OP, is.null, NA)]                                          # d_moda: sin operación
## Una cuarta paráfrasis CORRECTA que escribo yo, fuera del banco emitido: solo la usa la mutación «dos correctas».
P2_EXTRA_CORRECTA <- "Lo que recogió ese grado repartido por igual entre cada uno de sus cursos."
juicio_p2 <- function(txt) { j <- POOL_P2$juicio[match(txt, POOL_P2$texto)]; j[is.na(j) & txt %in% P2_EXTRA_CORRECTA] <- "correcta"; j }
## Lo que una explicación puede afirmar sobre lo que da una operación, y cómo se recalcula (v = valor de la operación; n, a = cursos y promedio).
P2_AFIRMA <- list(
  "da la mitad del promedio"                          = function(v, n, a) abs(v - a / 2) < 1e-9,
  "da el doble del promedio"                          = function(v, n, a) abs(v - 2 * a) < 1e-9,
  "da menos que el promedio"                          = function(v, n, a) v < a - 1e-9,
  "da más que el promedio"                            = function(v, n, a) v > a + 1e-9,
  "da la cantidad de cursos"                          = function(v, n, a) abs(v - n) < 1e-9,
  "da el promedio dividido otra vez entre los cursos" = function(v, n, a) abs(v - a / n) < 1e-9,
  "da el promedio"                                    = function(v, n, a) abs(v - a) < 1e-9,
  "no el promedio"                                    = function(v, n, a) abs(v - a) > 1e-9)
P2_RE_AFIRMA <- paste0("(", paste(names(P2_AFIRMA)[order(-nchar(names(P2_AFIRMA)))], collapse = "|"), ")")
## Texto del tipo retirado en la tanda 8, y textos del banco anterior de la Parte 2 (ronda 3): no deben quedar en ningún texto emitido.
RETIRADO_TIPO <- c("Comparaciones que no deciden", "¿Qué se compara", "decidir qué grado ganó", "Gana el grado que recogió más en total",
  "La suma de lo recogido por los cursos de un grado y la suma del otro grado.",
  "El producto de cursos por promedio de un grado y el del otro grado.",
  "La cantidad de cursos multiplicada por el promedio de un grado y lo mismo del otro grado.",
  "El promedio por curso de un grado y el del otro grado.",
  "La cantidad de cursos participantes de un grado y la del otro grado.",
  "La diferencia entre el promedio por curso de un grado y el del otro grado.",
  "La suma de la cantidad de cursos y el promedio por curso de un grado y la suma del otro grado.",
  "El promedio dividido entre la cantidad de cursos de un grado y el del otro grado.")
## Frases de las explicaciones del tipo retirado. Se buscan SOLO en el texto propio de la Parte 2 (retroalimentación de sus opciones y
## viñetas de su bloque de la Solution): «resta de los promedios» aparece legítimamente en el análisis ERR-RESTA de la Parte 1.
RETIRADO_ANCLAS_P2 <- c("resta de los promedios", "Sumar la cantidad de cursos", "Dividir el promedio", "Un promedio mayor", "Más cursos")
OBSOLETO_BANCO <- c("Lo que recogió el grado dividido entre el número de sus cursos participantes.",
  "Lo que recogió ese grado dividido entre sus cursos.",
  "Lo que recogió el mejor de sus cursos dentro de ese grado.", "Lo que recogió el grado sumando lo de sus cursos.",
  "Lo que recogió cualquiera de sus cursos porque en el grado recogieron lo mismo.",
  "Lo que recogió el que queda en el medio al ordenar sus cursos en ese grado.",
  "Lo que recogió la mayoría de sus cursos en ese grado.")

## Lector de fórmulas de la Solution y de la retroalimentación: lee `$a op b = c<resto>$` y devuelve operandos, unidad y signo que la cierra.
## El resto admitido es vacío, `\ \text{kilos}` con su signo opcional dentro (`. , : ;`) o `\text{.}` (una diferencia de promedios sin unidad, L-6).
lee_f <- function(f) {
  g <- gsub("^\\$|\\$$", "", f)
  m <- regmatches(g, regexec("^(\\d+) (\\\\times|-|\\+) (\\d+) = (\\d+)(.*)$", g))[[1]]
  if (length(m) != 6L) return(NULL)
  a <- as.numeric(m[2]); b <- as.numeric(m[4]); cc <- as.numeric(m[5]); op <- m[3]; resto <- m[6]
  r <- switch(op, "\\times" = a * b, "-" = a - b, "+" = a + b)
  u <- NA_character_; sg <- NA_character_
  if (resto == "") { u <- ""; sg <- "" }
  else if (grepl("^\\\\ \\\\text\\{kilos[.,:;]?\\}$", resto)) { u <- "kilos"; sg <- sub("^\\\\ \\\\text\\{kilos([.,:;]?)\\}$", "\\1", resto) }
  else if (grepl("^\\\\text\\{[.,:;]\\}$", resto)) { u <- ""; sg <- sub("^\\\\text\\{([.,:;])\\}$", "\\1", resto) }
  list(ok = isTRUE(r == cc) && !is.na(u), op = op, a = a, b = b, c = cc, unidad = u, signo = sg)
}
frm <- function(x) lapply(regmatches(x, gregexpr("\\$[^$]+\\$", x))[[1]], lee_f)          # todas las fórmulas de UNA línea, leídas
sh <- function(x) gsub("\\$[^$]+\\$", "<F>", x)                                               # la línea con cada fórmula reducida a <F>
fok <- function(z, op, a, b, c, u, s) !is.null(z) && isTRUE(z$ok) && z$op == op && z$a == a && z$b == b && z$c == c && z$unidad == u && z$signo == s
## Parte 6 del Procedimiento: el tramo general juzgado (criterio vigente, sin cifras) y la forma del tramo con cifras. Las cuatro redacciones anteriores están PROHIBIDAS.
P6_GENERAL <- paste0("Parte 6. Cada hipótesis cambia un solo dato de la tabla. Un curso más con el mismo promedio suma ese promedio al total, y un curso menos lo resta. ",
  "Por eso, para saber si el grado que perdió habría ganado con un curso más, o si el que ganó habría perdido con uno menos, basta comparar el promedio de cada uno con la diferencia entre los totales: ",
  "si el promedio es mayor, sí; si es menor, no. ")
P6_RE_TRAMO <- paste0("^Con un curso más, el grado (\\S+) habría sumado a su total (\\d+) kilos, (más|menos) que la diferencia de (\\d+) kilos: (habría ganado|no le habría alcanzado para ganar)\\. ",
  "Con un curso menos, el grado (\\S+) habría restado a su total (\\d+) kilos, (más|menos) que la diferencia de (\\d+) kilos: (habría perdido|habría seguido ganando)\\.$")
P6_PROHIBIDOS <- c(   # copiados de `git show` (0a26d4fd trae la vigente): 32243d0a, ef2c96dc, 6f6f1b13 y 27f789dc («si y solo si») + la redacción vieja de H3 («solo si … fuera mayor»)
  "Por eso, si el grado que perdió tiene un curso más o el que ganó tiene uno menos, basta comparar el promedio de ese grado con la diferencia entre los totales: si el promedio es mayor, el ganador cambia; si es menor, no.",
  "Por eso, con un curso más o uno menos, basta comparar ese promedio con la diferencia entre los totales: si el promedio es mayor, el ganador cambia; si es menor, no.",
  "Por eso basta comparar ese promedio con la diferencia entre los totales: si es mayor, el ganador cambia; si es menor, no.",
  "si y solo si", "habría ganado con un curso más solo si", "habría perdido con un curso menos solo si", "el ganador cambia")

## Evaluador de la Parte 1: lee el TEXTO de una afirmación y la juzga con la tabla mostrada.
## lectura = "diferencia" (la cifra es T_S - T_O, la natural) o "total" (la cifra es T_S; lectura alterna declarada).
eval_p1 <- function(txt, nm, n, a, lectura = "diferencia") {
  tt <- n * a
  s <- which(vapply(nm, function(z) startsWith(tolower(txt), tolower(z)), NA))
  if (length(s) != 1L) return(list(v = NA, reconocida = FALSE))
  o <- 3L - s
  x <- NA_real_; xp <- NA_real_
  if (grepl(" en [0-9]+ kilos\\.$", txt)) x <- num(sub(".* en ([0-9]+ kilos)\\.$", "\\1", txt))
  else if (grepl("recogió [0-9]+ kilos más que", txt)) x <- num(sub(".*recogió ([0-9]+ kilos) más que.*", "\\1", txt))
  else if (grepl("fue de [0-9]+ kilos más\\.$", txt)) x <- num(sub(".*fue de ([0-9]+ kilos) más\\.$", "\\1", txt))
  if (grepl("con [0-9]+ kilos de más\\.$", txt)) xp <- num(sub(".*con ([0-9]+ kilos) de más\\.$", "\\1", txt))
  sin_sust <- grepl("cursos más grandes|un curso que recogió más que los otros|más alumnos para recoger|reciclaron durante más días", txt)
  v <- tt[s] > tt[o] && !sin_sust &&
    (!grepl("participaron más cursos", txt) || n[s] > n[o]) &&
    (!grepl("participaron menos cursos", txt) || n[s] < n[o]) &&
    (!grepl("tuvo mayor promedio", txt) || a[s] > a[o]) &&
    (!grepl("(su|el) promedio fue menor", txt) || a[s] < a[o]) &&
    (is.na(x) || x == (if (lectura == "total") tt[s] else tt[s] - tt[o])) &&
    (is.na(xp) || xp == a[s] - a[o])
  list(v = v, reconocida = !is.na(x) || !is.na(xp) || sin_sust, sede = s, x = x, xp = xp)
}

## Evaluador de la Parte 6: lee la hipótesis, modifica la tabla mostrada y vuelve a comparar los totales.
eval_p6 <- function(txt, noms, n, a) {
  tt <- n * a; w <- which.max(tt); l <- 3L - w
  if (grepl("^Ganó el concurso el grado ", txt))
    return(list(v = tolower(sub("^Ganó el concurso el grado (.*)\\.$", "\\1", txt)) == tolower(noms[w]), suj = NA))
  suj <- if (grepl("^Si (el|al) grado que perdió", txt)) l else if (grepl("^Si (el|al) grado que ganó", txt)) w else NA
  if (is.na(suj)) return(list(v = NA, suj = NA))
  ot <- 3L - suj; n2 <- n; a2 <- a
  if (grepl("con un curso más,", txt, fixed = TRUE)) n2[suj] <- n[suj] + 1
  else if (grepl("le hubiera faltado un curso,", txt, fixed = TRUE)) n2[suj] <- n[suj] - 1
  else if (grepl("con tantos cursos como el que", txt, fixed = TRUE)) n2[suj] <- n[ot]
  else if (grepl("el mismo promedio por curso que el que", txt, fixed = TRUE)) a2[suj] <- a[ot]
  else return(list(v = NA, suj = suj))
  t2 <- n2 * a2
  v <- if (grepl("habría ganado el concurso\\.$", txt)) t2[suj] > t2[ot] else
       if (grepl("habría perdido el concurso\\.$", txt)) t2[suj] < t2[ot] else NA
  list(v = v, suj = suj, t_suj = t2[suj], t_otro = t2[ot])
}

## ---------------------------------------------------------------------------------------------------------------
## verificar(e): devuelve fallos (vector de texto) y la ficha numérica de la versión.
verificar <- function(e) {
  fallos <- character(0); reg <- character(0)
  nota <- function(id, ok, det = "") { reg <<- c(reg, id); det <- paste(det, collapse = " "); if (!isTRUE(ok)) fallos <<- c(fallos, if (nzchar(det)) paste0(id, " — ", det) else id) }
  q <- nb(e$question); ql <- nb(e$questionlist); sl <- nb(e$solutionlist); sol <- nb(e$solution)
  mi <- e$metainfo; marcas <- mi$solution
  fic <- list()

  ## ===== (2) VISUAL-TEXTO: tablas, nombres, material, marcadores =========================================
  tl <- grep("^\\|", q, value = TRUE); tl <- tl[!grepl("^\\|[-:| ]+\\|$", tl)]
  nota("V4 dos tablas de cabecera + 2 filas", length(tl) == 6L, paste("filas de tabla:", length(tl)))
  if (length(tl) != 6L) return(list(fallos = fallos, ficha = fic, checks = reg))
  cel <- lapply(tl, function(r) limpiar(strsplit(sub("\\|\\s*$", "", sub("^\\|", "", r)), "|", fixed = TRUE)[[1]]))
  nota("V4 tres columnas por fila", all(lengths(cel) == 3L))
  fila <- function(i) suppressWarnings(c(n = as.numeric(sub(" cursos$", "", cel[[i]][2])), a = as.numeric(sub(" kilos$", "", cel[[i]][3]))))
  n1 <- c(fila(2)[["n"]], fila(3)[["n"]]); a1 <- c(fila(2)[["a"]], fila(3)[["a"]])
  n2 <- c(fila(5)[["n"]], fila(6)[["n"]]); a2 <- c(fila(5)[["a"]], fila(6)[["a"]])
  nota("V4 cifras enteras con sus unidades", !anyNA(c(n1, a1, n2, a2)))
  fil1 <- c(cel[[2]][1], cel[[3]][1]); fil2 <- c(cel[[5]][1], cel[[6]][1])
  mat1 <- sub("^Promedio de (\\S+) (recogido|recolectado) por curso$", "\\1", cel[[1]][3])
  mat2 <- sub("^Promedio de (\\S+) recogido por curso$", "\\1", cel[[4]][3])
  nota("V6 mismo material en las dos tablas", identical(mat1, mat2), paste(mat1, mat2))
  nota("V2 encabezados de la tabla 1", cel[[1]][2] == "Cantidad de cursos" && cel[[1]][1] %in% c("Sede", "Jornada"))
  nota("V2 encabezados de la tabla 2", cel[[4]][1] == "Grado" && cel[[4]][2] == "Cantidad de cursos")
  canon <- identical(cel[[1]][3], "Promedio de papel recolectado por curso")
  nota("V2 verbo de la cabecera (canónica «recolectado», las demás «recogido»)",
       canon || !grepl("recolect", paste(c(q, ql, sol), collapse = " ")))

  idx <- grep("La tabla muestra la cantidad de cursos que participaron", q, fixed = TRUE)
  nota("V1 dos párrafos «La tabla muestra»", length(idx) == 2L)
  en1 <- q[idx[1]]; en2 <- q[idx[2]]
  m1 <- regmatches(en1, regexec("por curso en (.+) y en (.+)\\.$", en1))[[1]]
  nota("V1 enunciado nombra las dos filas de la tabla 1", length(m1) == 3L)
  nm1 <- m1[2:3]
  ## cada nombre del enunciado termina en el rótulo de su fila, en el mismo orden de la tabla
  nota("V1 nombres del enunciado = filas de la tabla 1 (mismo orden)",
       endsWith(tolower(nm1[1]), tolower(fil1[1])) && endsWith(tolower(nm1[2]), tolower(fil1[2])),
       paste(paste(nm1, collapse = " | "), "<->", paste(fil1, collapse = " | ")))
  nota("V1 el enunciado cita el material de la tabla 1", grepl(paste0("mayor cantidad de ", mat1), en1, fixed = TRUE) &&
       grepl(paste0("promedio de ", mat1, " recogido por curso"), en1, fixed = TRUE))
  m2 <- regmatches(en2, regexec("en el grado (\\S+) y en el grado (\\S+)\\.$", en2))[[1]]
  nota("V1 la introducción nombra los dos grados de la tabla 2 (mismo orden)",
       length(m2) == 3L && identical(tolower(m2[2:3]), tolower(fil2)), paste(m2[2:3], collapse = "|"))
  nota("V1 la introducción cita el material de la tabla 2",
       grepl(paste0("ganaba el que recogiera más ", mat2, "."), en2, fixed = TRUE) &&
       grepl(paste0("promedio de ", mat2, " recogido por curso"), en2, fixed = TRUE))
  nota("V1 los grados van en orden ascendente (sexto < séptimo < octavo < noveno)",
       match(tolower(fil2[1]), c("sexto", "séptimo", "octavo", "noveno")) < match(tolower(fil2[2]), c("sexto", "séptimo", "octavo", "noveno")))

  ## marcadores ##ANSWERi##: seis, en orden, cada uno justo tras el párrafo de su parte; ninguno en la Solution
  ans <- grep("^##ANSWER[0-9]+##$", q)
  nota("V3 seis marcadores ##ANSWERi## en orden", identical(q[ans], paste0("##ANSWER", 1:6, "##")))
  for (i in 1:6) {
    p <- grep(paste0("^\\*\\*Parte ", i, "\\.\\*\\* "), q)
    nota(sprintf("V3 Parte %d: un párrafo de pregunta", i), length(p) == 1L)
    if (length(p) == 1L) {
      sig <- q[(p + 1):length(q)]; sig <- sig[nzchar(trimws(sig))][1]
      nota(sprintf("V3 Parte %d: su ##ANSWER%d## va justo después de su pregunta", i, i), identical(sig, paste0("##ANSWER", i, "##")))
    }
  }
  nota("V3 sin ##ANSWER en la Solution ni en la retroalimentación", !any(grepl("##ANSWER", c(sol, sl), fixed = TRUE)))
  pregunta <- function(i) sub(paste0("^\\*\\*Parte ", i, "\\.\\*\\* "), "", grep(paste0("^\\*\\*Parte ", i, "\\.\\*\\* "), q, value = TRUE))
  nota("V3 listas alineadas (15 opciones y 15 retroalimentaciones; num = vacío en la pregunta)",
       length(ql) == 15L && length(sl) == 15L && all(!nzchar(ql[9:11])) && all(nzchar(ql[c(1:8, 12:15)])))

  ## ===== (3) MATEMÁTICA ==================================================================================
  t1 <- n1 * a1; t2 <- n2 * a2
  w1 <- which.max(t1); w2 <- which.max(t2); l2 <- 3L - w2; D1 <- max(t1) - min(t1); D2 <- max(t2) - min(t2)
  fic <- list(t1 = rbind(fil1, n1, a1, t1), t2 = rbind(fil2, n2, a2, t2), gana1 = fil1[w1], D1 = D1, gana2 = fil2[w2], D2 = D2,
              rama1 = if (n1[w1] > n1[3 - w1]) "CURSOS" else "PROMEDIO", rama2 = if (n2[w2] > n2[l2]) "CURSOS" else "PROMEDIO",
              mat = mat1)
  nota("M0 sin dominancia en ninguna tabla (cursos y promedio en sentidos opuestos)",
       (n1[1] - n1[2]) * (a1[1] - a1[2]) < 0 && (n2[1] - n2[2]) * (a2[1] - a2[2]) < 0)
  ## Parte 1: una sola verdadera y es la marcada
  ev1 <- lapply(ql[1:4], eval_p1, nm = nm1, n = n1, a = a1)
  nota("M1 el evaluador reconoce las cuatro afirmaciones de la Parte 1", all(vapply(ev1, function(z) isTRUE(z$reconocida), NA)))
  v1 <- vapply(ev1, function(z) isTRUE(z$v), NA)
  nota("M1 la Parte 1 tiene exactamente una afirmación verdadera", sum(v1) == 1L, paste(v1, collapse = " "))
  nota("M1 la marca exsolution de la Parte 1 coincide con el veredicto recalculado", identical(as.logical(marcas[[1]]), v1),
       paste("marcas:", paste(marcas[[1]], collapse = " "), "| recalculado:", paste(v1, collapse = " ")))
  ## la sede de las dos afirmaciones que dan el triunfo a G es 2 + 2 (L-1)
  sedes <- vapply(ev1, function(z) if (is.null(z$sede)) NA_integer_ else z$sede, 1L)
  nota("M1 reparto 2 + 2 entre la sede que gana y la que pierde", sum(sedes == w1, na.rm = TRUE) == 2L && sum(sedes != w1, na.rm = TRUE) == 2L)
  fic$p1_total_leido_como_total <- sum(vapply(ql[1:4], function(z) isTRUE(eval_p1(z, nm1, n1, a1, "total")$v), NA))
  ## Parte 2: único tipo vigente. Banco de ocho textos juzgados por mí (POOL_P2); recálculo con la tabla del otro colegio
  pr2 <- pregunta(2)
  m_pr2 <- regmatches(pr2, regexec(P2_RE_PREG, pr2))[[1]]
  nota("S2 la pregunta de la Parte 2 es la vigente («¿Qué indica, en la tabla del otro colegio, el promedio de … recogido por curso de un grado?»)", length(m_pr2) == 2L, pr2)
  nota("V1 la pregunta de la Parte 2 cita el material de la tabla del otro colegio", length(m_pr2) == 2L && identical(m_pr2[2], mat2), paste(m_pr2[2], "|", mat2))
  op <- ql[5:8]
  jui <- juicio_p2(op)
  nota("S2 las cuatro opciones de la Parte 2 salen del banco de ocho textos que juzgué (ninguna de otro tipo)", all(op %in% POOL_P2$texto), paste(op[!op %in% POOL_P2$texto], collapse = " | "))
  nota("S2 opciones únicas", !anyDuplicated(op))
  nota("S2 exactamente una opción correcta (mi juicio)", sum(jui %in% "correcta") == 1L, paste(jui, collapse = " "))
  nota("S2 las otras tres opciones son incorrectas (mi juicio)", sum(jui %in% "incorrecta") == 3L, paste(jui, collapse = " "))
  nota("M2 la marca de la Parte 2 coincide con mi juicio semántico", identical(as.logical(marcas[[2]]), jui %in% "correcta"),
       paste("marcas:", paste(marcas[[2]], collapse = " "), "| mi juicio:", paste(jui, collapse = " ")))
  ## recálculo con la tabla MOSTRADA: lo que da cada operación, en las dos filas, frente al promedio impreso
  N2 <- sum(n2); id_op <- POOL_P2$id[match(op, POOL_P2$texto)]; no_calc <- 0L
  for (j in 1:4) {
    if (is.na(id_op[j])) next
    f_op <- P2_OP[[id_op[j]]]
    if (is.null(f_op)) { no_calc <- no_calc + 1L; next }
    v <- vapply(1:2, function(g) f_op(n2[g], a2[g], N2), 1)
    igual <- abs(v - a2) < 1e-9
    nota(sprintf("M2 la operación de la opción %d de la Parte 2 %s el promedio impreso en las dos filas de la tabla del otro colegio", j, if (jui[j] %in% "correcta") "da" else "NO da"),
         if (jui[j] %in% "correcta") all(igual) else !any(igual), paste("operación =", paste(round(v, 3), collapse = ","), "| promedios =", paste(a2, collapse = ",")))
  }
  fic$p2_no_calculables <- no_calc
  ## retroalimentación de cada opción de la Parte 2: «Verdadero.» en la correcta; «Falso.» y SU explicación en las incorrectas
  for (j in 1:4) {
    f <- sl[4 + j]; k <- match(op[j], POOL_P2$texto)
    if (is.na(k)) next
    if (POOL_P2$juicio[k] == "correcta") {
      nota(sprintf("S7 Parte 2 opción %d (correcta): «Verdadero.» y define el promedio (total entre cursos, reparto por igual)", j),
           startsWith(f, "Verdadero.") && grepl(POOL_P2$ancla[k], f, fixed = TRUE) && grepl("repartiera por igual", f, fixed = TRUE), f)
    } else {
      nota(sprintf("S7 Parte 2 opción %d (incorrecta): «Falso.» y su explicación propia (%s)", j, POOL_P2$ancla[k]),
           startsWith(f, "Falso.") && grepl(POOL_P2$ancla[k], f, fixed = TRUE), f)
      otras <- POOL_P2$ancla[POOL_P2$juicio == "incorrecta" & POOL_P2$id != POOL_P2$id[k]]
      nota(sprintf("S7 Parte 2 opción %d (incorrecta): su explicación no usa la frase de otra opción", j), !any(vapply(otras, function(z) grepl(z, f, fixed = TRUE), NA)), f)
      ## lo que la explicación dice que da la operación, recalculado con las dos filas de la tabla del otro colegio
      f_op <- P2_OP[[POOL_P2$id[k]]]
      if (!is.null(f_op)) {
        v <- vapply(1:2, function(g) f_op(n2[g], a2[g], N2), 1)
        afirma <- regmatches(f, gregexpr(P2_RE_AFIRMA, f, perl = TRUE))[[1]]
        ver <- vapply(afirma, function(z) all(vapply(1:2, function(g) P2_AFIRMA[[z]](v[g], n2[g], a2[g]), NA)), NA)
        cl_da <- regmatches(f, gregexpr("\\bda\\b.*?(?=\\bda\\b|[,:.;]|$)", f, perl = TRUE))[[1]]
        conocida <- vapply(cl_da, function(z) any(startsWith(z, grep("^da ", names(P2_AFIRMA), value = TRUE))), NA)
        nota(sprintf("M2 Parte 2 opción %d: toda cláusula «da …» de su explicación es una afirmación que sé recalcular con la tabla", j), length(cl_da) >= 1L && all(conocida), paste(cl_da[!conocida], collapse = " | "))
        nota(sprintf("M2 Parte 2 opción %d: todo lo que su explicación dice que da la operación es cierto con las dos filas de la tabla (%s)", j, paste(afirma, collapse = "; ")),
             length(afirma) >= 1L && all(ver), paste("operación =", paste(round(v, 3), collapse = ","), "| n =", paste(n2, collapse = ","), "| a =", paste(a2, collapse = ","), "|", f))
      }
    }
  }
  ## la Solution: un único encabezado vigente, las tres incorrectas de esta versión con su explicación, y nada del tipo retirado
  h_p2 <- "**Parte 2 · Lecturas del promedio que no sirven**"
  nota("G3 la Solution lleva un único encabezado «Parte 2 · …», el de las lecturas del promedio que no sirven", sum(sol == h_p2) == 1L && sum(startsWith(sol, "**Parte 2")) == 1L,
       paste(grep("^\\*\\*Parte 2", sol, value = TRUE), collapse = " | "))
  i_h <- which(sol == h_p2)
  nota("S6 existe un único encabezado de la Parte 2", length(i_h) == 1L)
  bullets <- character(0)
  if (length(i_h) == 1L) {
    sig_h <- which(startsWith(sol, "**") & seq_along(sol) > i_h)[1]; if (is.na(sig_h)) sig_h <- length(sol) + 1L
    bullets <- sol[(i_h + 1):(sig_h - 1)]; bullets <- bullets[startsWith(bullets, "- «")]
    esperados <- vapply(which(!(jui %in% "correcta")), function(j) paste0("- «", op[j], "» ", sub("^Falso\\. ", "", sl[4 + j])), "")
    nota("S6 la Solution de la Parte 2 enumera las tres incorrectas de esta versión, con la explicación de su retroalimentación",
         length(bullets) == 3L && setequal(bullets, esperados), paste(setdiff(esperados, bullets), collapse = " ; "))
  }
  nota("S6 la Parte 2 de la sección «Procedimiento» define el promedio por curso y no promete lo que recogió cada curso",
       any(sol == "Parte 2. El promedio por curso es el total del grado dividido entre la cantidad de cursos: lo que le tocaría a cada curso si el total se repartiera por igual. No dice cuánto recogió cada curso en particular.") &&
       !any(grepl("^Parte 2\\. Gana", sol)) && sum(startsWith(sol, "Parte 2. ")) == 1L, paste(grep("^Parte 2\\. ", sol, value = TRUE), collapse = " | "))
  todo_txt <- c(q, ql, sol, sl)
  p2_propio <- c(sl[5:8], bullets)
  hit <- c(RETIRADO_TIPO[vapply(RETIRADO_TIPO, function(p) any(grepl(p, todo_txt, fixed = TRUE)), NA)],
           RETIRADO_ANCLAS_P2[vapply(RETIRADO_ANCLAS_P2, function(p) any(grepl(p, p2_propio, fixed = TRUE)), NA)])
  nota("S6 ni el enunciado, ni las opciones, ni la Solution, ni la retroalimentación conservan texto del tipo retirado de la Parte 2 («comparar»)", length(hit) == 0L, paste(hit, collapse = " | "))
  hit <- OBSOLETO_BANCO[vapply(OBSOLETO_BANCO, function(p) any(grepl(p, todo_txt, fixed = TRUE)), NA)]
  nota("S6 ni conservan textos del banco anterior de la Parte 2 (ronda 3)", length(hit) == 0L, paste(hit, collapse = " | "))
  nota("V11 la retroalimentación de las opciones de la Parte 2 no imprime cifras", !any(grepl("[0-9]", sl[5:8])), paste(sl[5:8][grepl("[0-9]", sl[5:8])], collapse = " | "))
  ## Partes 3 a 5
  g3 <- sub("^.*en total el grado (\\S+)\\?$", "\\1", pregunta(3)); g4 <- sub("^.*en total el grado (\\S+)\\?$", "\\1", pregunta(4))
  nota("S3 las Partes 3 y 4 piden los dos grados de la tabla, uno cada una", setequal(c(g3, g4), tolower(fil2)), paste(g3, g4))
  nota("S3 las Partes 3 a 5 citan el material de la tabla", all(vapply(3:5, function(i) grepl(mat2, pregunta(i), fixed = TRUE), NA)))
  nota("S4 la Parte 5 pregunta por los kilos de más del ganador", grepl("¿Cuántos kilos más recogió?", pregunta(5), fixed = TRUE) && startsWith(pregunta(5), "El grado que ganó recogió más"))
  esp3 <- t2[match(g3, tolower(fil2))]; esp4 <- t2[match(g4, tolower(fil2))]
  nota("M3 Parte 3 = n × a del grado preguntado, leído de la tabla", identical(as.numeric(marcas[[3]]), as.numeric(esp3)), paste("exsolution", marcas[[3]], "| recalculado", esp3))
  nota("M3 Parte 4 = n × a del grado preguntado, leído de la tabla", identical(as.numeric(marcas[[4]]), as.numeric(esp4)), paste("exsolution", marcas[[4]], "| recalculado", esp4))
  nota("M3 Parte 5 = diferencia de los dos totales de la tabla", identical(as.numeric(marcas[[5]]), as.numeric(D2)), paste("exsolution", marcas[[5]], "| recalculado", D2))
  ## Parte 6: composición, marcas recalculadas, retroalimentación
  op6 <- ql[12:15]
  ev6 <- lapply(op6, eval_p6, noms = fil2, n = n2, a = a2)
  v6 <- vapply(ev6, function(z) z$v, NA)
  nota("M4 el evaluador reconoce las cuatro afirmaciones de la Parte 6", !anyNA(v6))
  nota("M4 las marcas de la Parte 6 coinciden con el veredicto recalculado", identical(as.logical(marcas[[6]]), as.logical(v6)),
       paste("marcas:", paste(marcas[[6]], collapse = " "), "| recalculado:", paste(v6, collapse = " ")))
  nota("M4 la Parte 6 tiene al menos una verdadera y una falsa", any(v6) && any(!v6))
  nota("M4 cuatro afirmaciones distintas", !anyDuplicated(op6))
  hp <- grepl("con un curso más,", op6, fixed = TRUE); hg <- grepl("le hubiera faltado un curso,", op6, fixed = TRUE)
  ga <- startsWith(op6, "Ganó el concurso el grado ")
  vn <- grepl("con tantos cursos como el que|el mismo promedio por curso que el que", op6)
  nota("S5 composición de la Parte 6: HP, HG, el ganador y una de ventaja", sum(hp) == 1L && sum(hg) == 1L && sum(ga) == 1L && sum(vn) == 1L)
  nota("S5 sin pares complementarios: la de ventaja y la del ganador llevan el mismo veredicto", v6[vn] == v6[ga])
  ## sin cifras ni nombres de la Parte 1 en el texto propio de las Partes 2 a 6 (V11)
  prop <- c(en2, pregunta(2), op, pregunta(3), pregunta(4), pregunta(5), pregunta(6), op6)
  prop <- gsub("Las Partes 2 a 6", "", prop, fixed = TRUE)
  nota("V11 el texto de las Partes 2 a 6 no imprime cifras (solo su tabla)", !any(grepl("[0-9]", prop)))
  nota("V11 las Partes 2 a 6 no nombran sedes ni jornadas de la Parte 1",
       !any(grepl(paste0("\\b(", paste(c(tolower(fil1), "sede", "jornada"), collapse = "|"), ")\\b"), tolower(prop), perl = TRUE)))
  cifras1 <- unique(c(n1, a1, num(ql[1:4]), t1, D1))
  nota("V11 las respuestas de las Partes 3 a 5 no son cifras que la Parte 1 muestre o resuelva", !any(c(esp3, esp4, D2) %in% cifras1),
       paste(intersect(c(esp3, esp4, D2), cifras1), collapse = ","))

  ## ===== retroalimentación y Solution: aritmética y cifras ===================================================
  todo <- c(sl, sol)
  eq <- unlist(regmatches(todo, gregexpr("\\$[^$]+\\$", todo)))
  eq <- gsub("^\\$|\\$$", "", eq)
  ok_eq <- vapply(eq, function(z) { r <- lee_f(paste0("$", z, "$")); if (is.null(r)) NA else r$ok }, NA)
  nota("M5 toda ecuación $a op b = c$ de la Solution y la retroalimentación está bien calculada", !anyNA(ok_eq) && all(ok_eq),
       paste(eq[is.na(ok_eq) | !ok_eq], collapse = " ; "))
  fic$n_ecuaciones <- length(eq)
  ## retroalimentación de las Partes 3 y 4: n y a del grado, y su producto, son los de la tabla
  for (i in 3:4) {
    g <- if (i == 3) g3 else g4; r <- match(g, tolower(fil2))
    exp <- sprintf("En el grado %s participaron %d cursos y cada uno recogió, en promedio, %d kilos: $%d \\times %d = %d$ kilos.", g, n2[r], a2[r], n2[r], a2[r], t2[r])
    nota(sprintf("M5 retroalimentación de la Parte %d = n × a de la fila del grado %s", i, g), identical(sl[8 + (i - 2)], exp), sl[8 + (i - 2)])
  }
  exp5 <- sprintf("Ganó el grado %s: $%d - %d = %d$ kilos más que el grado %s.", tolower(fil2[w2]), max(t2), min(t2), D2, tolower(fil2[l2]))
  nota("M5 retroalimentación de la Parte 5 nombra al ganador y calcula la diferencia", identical(sl[11], exp5), sl[11])
  ## Parte 1: la retroalimentación de la afirmación verdadera cita los dos totales y la diferencia de la tabla
  clave1 <- which(v1)
  l1 <- 3L - w1; fb1 <- sl[clave1]; zf <- if (length(clave1) == 1L) frm(fb1) else list()
  nota("M5 retroalimentación de la clave de la Parte 1 = totales y diferencia de la tabla 1 (fórmulas leídas: cifras, unidad y signo dentro)",
       length(clave1) == 1L && identical(sh(fb1), sprintf("Verdadero. %s recogió <F> y %s, <F> ganó por <F>", sub("^la", "La", nm1[w1]), nm1[l1])) && length(zf) == 3L &&
       fok(zf[[1]], "\\times", n1[w1], a1[w1], t1[w1], "kilos", "") && fok(zf[[2]], "\\times", n1[l1], a1[l1], t1[l1], "kilos", ":") && fok(zf[[3]], "-", t1[w1], t1[l1], D1, "kilos", "."), fb1)
  for (j in setdiff(1:4, clave1)) {
    f <- sl[j]
    nota(sprintf("S7 Parte 1 opción %d (falsa): empieza «Falso.»", j), startsWith(f, "Falso."))
    ## toda cifra que cita está entre las de la tabla, sus totales, D o la cifra de la propia afirmación
    permitidas <- c(n1, a1, t1, D1, ev1[[j]]$x, num(ql[j]))
    nota(sprintf("M5 Parte 1 opción %d: la retroalimentación solo cita cifras de la tabla 1 o de su propia afirmación", j),
         all(num(f) %in% permitidas), paste(setdiff(num(f), permitidas), collapse = ","))
  }
  ## Parte 6: la retroalimentación de cada afirmación cita los dos totales comparados y su veredicto
  for (j in 1:4) {
    f <- sl[11 + j]; z <- ev6[[j]]
    nota(sprintf("S7 Parte 6 opción %d: empieza «%s»", j, if (v6[j]) "Verdadero." else "Falso."), startsWith(f, if (v6[j]) "Verdadero." else "Falso."))
    if (!is.na(z$suj)) {
      nota(sprintf("M5 Parte 6 opción %d: cita el total del sujeto tras el cambio (%d) y el del otro (%d)", j, z$t_suj, z$t_otro),
           grepl(paste0("= ", z$t_suj, "\\$"), f) && grepl(paste0("\\b", z$t_otro, " del grado"), f), f)
      gano <- z$t_suj > z$t_otro
      nota(sprintf("S7 Parte 6 opción %d: el desenlace de la retroalimentación es coherente con los totales", j),
           if (gano) grepl("habría ganado\\.$|habría seguido ganando\\.$", f) else grepl("habría perdido\\.$|habría seguido perdiendo\\.$", f), f)
    } else {
      nota(sprintf("M5 Parte 6 opción %d (ganador): nombra al ganador y sus totales", j),
           grepl(sprintf("^(Verdadero|Falso)\\. Ganó el grado %s, con %d kilos frente a los %d del grado %s\\.$", tolower(fil2[w2]), max(t2), min(t2), tolower(fil2[l2])), f), f)
    }
  }
  ## Solution: Respuestas, procedimiento y caso específico contra las tablas
  resp <- c(sprintf("- Parte 3. %d kilos, el total del grado %s.", esp3, g3), sprintf("- Parte 4. %d kilos, el total del grado %s.", esp4, g4),
            sprintf("- Parte 5. %d kilos.", D2))
  nota("M6 «Respuestas» de las Partes 3 a 5 = lo recalculado desde la tabla", all(resp %in% sol), paste(resp[!resp %in% sol], collapse = " ; "))
  nota("M6 «Respuestas» de la Parte 1 cita la afirmación verdadera por su contenido",
       any(sol == paste0("- Parte 1. La respuesta correcta es la que afirma: «", ql[clave1], "»")))
  nota("M6 «Respuestas» de la Parte 2 cita la opción correcta", any(sol == paste0("- Parte 2. «", ql[5:8][as.logical(marcas[[2]])], "»")))
  ver6 <- paste0("    - «", op6[v6], "»"); fal6 <- paste0("    - «", op6[!v6], "»")
  i_v <- which(sol == "- Parte 6. Son verdaderas:"); i_f <- which(sol == "- Parte 6. Son falsas:")
  nota("M6 «Respuestas» de la Parte 6 agrupa por veredicto exactamente lo recalculado (como conjunto: la prosa conserva el orden previo al exshuffle)",
       length(i_v) == 1L && length(i_f) == 1L && setequal(sol[(i_v + 1):(i_f - 1)], ver6) && length(sol[(i_v + 1):(i_f - 1)]) == length(ver6) &&
       setequal(sol[(i_f + 1):(i_f + length(fal6))], fal6))
  b1 <- vapply(1:2, function(k) sprintf("- %s: <F>", sub("^la", "La", nm1[k])), "")
  ok_b <- vapply(1:2, function(k) { ls_ <- sol[sh(sol) == b1[k]]; length(ls_) == 1L && length(frm(ls_)) == 1L && fok(frm(ls_)[[1]], "\\times", n1[k], a1[k], t1[k], "kilos", ".") }, NA)
  nota("M6 «Procedimiento» Parte 1: los dos totales de la tabla 1 (fórmulas leídas; la unidad y el punto, dentro)", all(ok_b), paste(b1[!ok_b], collapse = " ; "))
  fila_p2 <- sprintf("- Grado %s: $%d \\times %d = %d$ kilos.", tolower(fil2), n2, a2, t2)
  nota("M6 «Procedimiento» Partes 3 a 5: los dos totales de la tabla 2", all(fila_p2 %in% sol), paste(fila_p2[!fila_p2 %in% sol], collapse = " ; "))
  gl1 <- sol[startsWith(sh(sol), sprintf("Ganó %s, con <F> más. ", nm1[w1]))]
  nota("M6 «Procedimiento» Parte 1: nombra al ganador y la diferencia de la tabla 1 (fórmula leída)", length(gl1) == 1L && length(frm(gl1)) == 1L && fok(frm(gl1)[[1]], "-", t1[w1], t1[l1], D1, "kilos", ""),
       sprintf("Ganó %s, con <F> más.", nm1[w1]))
  ganol2 <- sprintf("Ganó el grado %s, con $%d - %d = %d$ kilos más. ", tolower(fil2[w2]), t2[w2], t2[l2], D2)
  nota("M6 «Procedimiento» Partes 3 a 5: nombra al ganador y la diferencia de la tabla 2", any(startsWith(sol, ganol2)), ganol2)
  ## la frase sobre el perfil (cursos/promedio) es coherente con la rama
  perfil1 <- if (n1[w1] > n1[3 - w1]) sprintf("Su promedio era menor (%d kilos frente a %d), pero participaron más cursos (%d frente a %d), y eso pesó más.", a1[w1], a1[3 - w1], n1[w1], n1[3 - w1]) else
             sprintf("Participaron menos cursos (%d frente a %d), pero su promedio era mayor (%d kilos frente a %d), y eso pesó más.", n1[w1], n1[3 - w1], a1[w1], a1[3 - w1])
  nota("M6 «Procedimiento» Parte 1: la frase del perfil de la sede ganadora coincide con la tabla", any(grepl(perfil1, sol, fixed = TRUE)), perfil1)
  perfil2 <- if (n2[w2] > n2[l2]) sprintf("Su promedio era menor (%d kilos frente a %d), pero participaron más cursos (%d frente a %d).", a2[w2], a2[l2], n2[w2], n2[l2]) else
             sprintf("Participaron menos cursos (%d frente a %d), pero su promedio era mayor (%d kilos frente a %d).", n2[w2], n2[l2], a2[w2], a2[l2])
  nota("M6 «Procedimiento» Partes 3 a 5: la frase del perfil del grado ganador coincide con la tabla", any(grepl(perfil2, sol, fixed = TRUE)), perfil2)
  ## ---- Parte 6 del Procedimiento: el criterio y las dos comparaciones (H3 de la r4; los 10 puntos de la r5 con los cambios de la r6) ----
  solR <- e$solution                                                       # CRUDO (con U+00A0): L-6 se mide aquí; lo demás, normalizado
  p6i <- grep("^Parte 6\\. Cada hipótesis", sol)
  nota("P6 un único párrafo «Parte 6. Cada hipótesis…» en el Procedimiento", length(p6i) == 1L, length(p6i))
  if (length(p6i) == 1L) {
    p6 <- sol[p6i]; p6R <- solR[p6i]; marca_corte <- "Con un curso más, el grado "
    ncorte <- lengths(regmatches(p6, gregexpr(marca_corte, p6, fixed = TRUE)))
    nota("P6 el párrafo lleva una sola vez el corte «Con un curso más, el grado » (con mayúscula, no el «con un curso más,» del criterio)", ncorte == 1L, p6)
    if (ncorte == 1L) {
      cut <- regexpr(marca_corte, p6, fixed = TRUE)[[1]]; general <- substr(p6, 1, cut - 1L); tramo <- substr(p6, cut, nchar(p6))
      nota("P6 el tramo general es el criterio literal vigente (sin cifras)", identical(general, P6_GENERAL) && !grepl("[0-9]", sub("^Parte 6\\. ", "", general)), general)
      mt <- regmatches(tramo, regexec(P6_RE_TRAMO, tramo))[[1]]
      nota("P6 el tramo con cifras tiene la forma juzgada: dos comparaciones con cifras y dos conclusiones (texto no juzgado = fallo)", length(mt) == 11L, tramo)
      nota("P6 exactamente dos comparaciones con cifras y dos conclusiones en el tramo",
           lengths(regmatches(tramo, gregexpr("(más|menos) que la diferencia de", tramo))) == 2L && lengths(regmatches(tramo, gregexpr("habría ganado|no le habría alcanzado para ganar|habría perdido|habría seguido ganando", tramo))) == 2L, tramo)
      if (length(mt) == 11L) {
        aP <- as.numeric(mt[3]); aG <- as.numeric(mt[8]); DP <- as.numeric(mt[5]); DG <- as.numeric(mt[10])
        nota("P6 «un curso más»: el grado que perdió según la tabla, su promedio y D = |T1 - T2|", tolower(mt[2]) == tolower(fil2[l2]) && aP == a2[l2] && DP == D2, paste(mt[2], aP, DP, "| tabla:", fil2[l2], a2[l2], D2))
        nota("P6 «un curso menos»: el grado que ganó según la tabla, su promedio y el mismo D", tolower(mt[7]) == tolower(fil2[w2]) && aG == a2[w2] && DG == D2, paste(mt[7], aG, DG, "| tabla:", fil2[w2], a2[w2], D2))
        nota("P6 los promedios citados no igualan a D (el criterio «mayor/menor» no deja un empate sin resolver)", aP != DP && aG != DG, paste(aP, DP, aG, DG))
        nota("P6 cada «más»/«menos» es verdadero con sus dos cifras", (mt[4] == "más") == (aP > DP) && (mt[9] == "más") == (aG > DG), paste(aP, mt[4], DP, "|", aG, mt[9], DG))
        hp_gana <- (n2[l2] + 1) * a2[l2] > t2[w2]; hg_pierde <- (n2[w2] - 1) * a2[w2] < t2[l2]      # contrafactual desde la tabla
        nota("P6 la conclusión de «un curso más» es verdadera con el contrafactual (n_P + 1)·a_P frente a T_G y coherente con su «más/menos»", (mt[6] == "habría ganado") == hp_gana && ((mt[4] == "más") == hp_gana), paste(mt[6], "| contrafactual:", (n2[l2] + 1) * a2[l2], "frente a", t2[w2]))
        nota("P6 la conclusión de «un curso menos» es verdadera con el contrafactual (n_G - 1)·a_G frente a T_P y coherente con su «más/menos»", (mt[11] == "habría perdido") == hg_pierde && ((mt[9] == "más") == hg_pierde), paste(mt[11], "| contrafactual:", (n2[w2] - 1) * a2[w2], "frente a", t2[l2]))
        ## HP y HG identificadas por su TEXTO en la questionlist tras la mezcla (nunca por rol_p6); «sí» del criterio = promedio > D = marca VERDADERA
        o6 <- ql[12:15]; ihp <- which(grepl("con un curso más,", o6, fixed = TRUE)); ihg <- which(grepl("le hubiera faltado un curso,", o6, fixed = TRUE))
        m6 <- as.logical(marcas[[6]])
        if (length(ihp) == 1L && length(ihg) == 1L) {
          nota("P6 «sí» para el grado que perdió (promedio > D) = la marca emitida de la opción «con un curso más»", m6[ihp] == (aP > DP) && m6[ihp] == hp_gana && m6[ihp] == (mt[6] == "habría ganado"), paste("marca", m6[ihp], "| promedio > D:", aP > DP))
          nota("P6 «sí» para el grado que ganó (promedio > D) = la marca emitida de la opción «le hubiera faltado un curso»", m6[ihg] == (aG > DG) && m6[ihg] == hg_pierde && m6[ihg] == (mt[11] == "habría perdido"), paste("marca", m6[ihg], "| promedio > D:", aG > DG))
        } else nota("P6 las opciones «con un curso más» y «le hubiera faltado un curso» existen una vez cada una en la questionlist", FALSE, paste(ihp, ihg))
      }
    }
    nota("P6 L-6 en el párrafo (crudo): cada cifra va unida a «kilos» con U+00A0 (4 pares) y las dos últimas palabras también",
         !grepl("[0-9] kilos", p6R) && lengths(regmatches(p6R, gregexpr(paste0("[0-9]", NBSP, "kilos"), p6R))) == 4L && grepl(paste0(NBSP, "(perdido|ganando)\\.$"), p6R), gsub(NBSP, "~", p6R, fixed = TRUE))
    nota("P6 ninguna de las cuatro redacciones anteriores, ni la vieja de H3, en el párrafo", !any(vapply(P6_PROHIBIDOS, function(p) grepl(p, p6, fixed = TRUE), NA)), p6)
  }
  nota("P6 ninguna redacción prohibida del criterio en todo el texto emitido de la versión", !any(vapply(P6_PROHIBIDOS, function(p) any(grepl(p, c(q, ql, sol, sl), fixed = TRUE)), NA)))
  c1l <- sol[sh(sol) == sprintf("En la Parte 1, %s recogió <F> y %s recogió <F> La diferencia es <F> a favor de %s.", nm1[w1], nm1[l1], nm1[w1])]
  nota("M6 «Caso específico» Parte 1 = tabla 1 (fórmulas leídas)", length(c1l) == 1L && length(frm(c1l)) == 3L &&
       fok(frm(c1l)[[1]], "\\times", n1[w1], a1[w1], t1[w1], "kilos", "") && fok(frm(c1l)[[2]], "\\times", n1[l1], a1[l1], t1[l1], "kilos", ".") && fok(frm(c1l)[[3]], "-", t1[w1], t1[l1], D1, "kilos", ""), c1l)
  caso2 <- sprintf("En el otro colegio, el grado %s recogió $%d \\times %d = %d$ kilos y el grado %s, $%d \\times %d = %d$ kilos: la diferencia es %d kilos.",
                   tolower(fil2[w2]), n2[w2], a2[w2], t2[w2], tolower(fil2[l2]), n2[l2], a2[l2], t2[l2], D2)
  nota("M6 «Caso específico» otro colegio = tabla 2", any(sol == caso2), caso2)

  ## ===== añadidas en la ronda 9: frases de la Solution que la ronda 3 no verificaba ========================================
  ## (a) la frase de «Procedimiento» sobre las dos afirmaciones de cada sede nombra a la que perdió, a la que ganó y la diferencia de la tabla 1
  fr <- grep("^Las dos afirmaciones que dan el triunfo a ", sol, value = TRUE)
  mfr <- if (length(fr) == 1L) regmatches(fr, regexec("^Las dos afirmaciones que dan el triunfo a (.+) son falsas, porque (?:esa|ese) \\S+ no ganó\\. De las dos que se lo dan a (.+), solo una cita la diferencia correcta, ([0-9]+) kilos\\.$", fr, perl = TRUE))[[1]] else character(0)
  nota("M6 «Procedimiento» Parte 1: la frase de las dos afirmaciones de cada sede nombra a la sede que perdió, a la que ganó y la diferencia de la tabla 1",
       length(mfr) == 4L && identical(mfr[2], nm1[3 - w1]) && identical(mfr[3], nm1[w1]) && as.numeric(mfr[4]) == D1, paste(fr, collapse = " | "))
  ## (b) el bloque «Parte 6 · Afirmaciones falsas» de la Solution = las falsas de esta versión con la explicación de su retroalimentación
  i_f6 <- which(sol == "**Parte 6 · Afirmaciones falsas**")
  nota("S6 existe un único encabezado «Parte 6 · Afirmaciones falsas»", length(i_f6) == 1L)
  if (length(i_f6) == 1L) {
    sig6 <- which(startsWith(sol, "**") & seq_along(sol) > i_f6)[1]; if (is.na(sig6)) sig6 <- length(sol) + 1L
    b6 <- sol[(i_f6 + 1):(sig6 - 1)]; b6 <- b6[startsWith(b6, "- «")]
    esp6 <- vapply(which(v6 %in% FALSE), function(j) paste0("- «", op6[j], "» ", sub("^Falso\\. ", "", sl[11 + j])), "")
    nota("S6 la Solution de la Parte 6 enumera las afirmaciones falsas de esta versión, con la explicación de su retroalimentación", length(b6) == sum(v6 %in% FALSE) && setequal(b6, esp6), paste(setdiff(esp6, b6), collapse = " ; "))
  }
  ## (c) todo el texto de la versión nombra su material y ninguno de los otros dos
  ajeno <- setdiff(c("papel", "cartón", "plástico"), mat1)
  usa <- ajeno[vapply(ajeno, function(m) any(grepl(m, tolower(c(q, ql, sol, sl)), fixed = TRUE)), NA)]
  nota("V12 el texto de la versión (enunciado, opciones, Solution y retroalimentación) nombra solo su material, no los otros dos", length(usa) == 0L, paste(usa, collapse = ","))
  ## (d) la Solution tiene sus doce encabezados, en orden
  enc <- sol[startsWith(sol, "**") & endsWith(sol, "**")]
  pat_enc <- c("^\\*\\*Respuestas\\*\\*$", "^\\*\\*Procedimiento correcto\\*\\*$", "^\\*\\*Análisis de los distractores\\*\\*$", rep("^\\*\\*Parte 1 · ERR-[A-Z]+ — .+\\*\\*$", 3),
               "^\\*\\*Parte 2 · Lecturas del promedio que no sirven\\*\\*$", "^\\*\\*Parte 6 · Afirmaciones falsas\\*\\*$", "^\\*\\*Propiedades del concepto\\*\\*$",
               "^\\*\\*Caso específico\\*\\*$", "^\\*\\*Reflexión metacognitiva\\*\\*$", "^\\*\\*Estrategia para evitar el error\\*\\*$")
  nota("G la Solution tiene los doce encabezados esperados, en este orden", length(enc) == length(pat_enc) && all(mapply(grepl, pat_enc, enc)), paste(enc, collapse = " | "))

  ## ===== L-6 (firmada 2026-11-02 y ampliada 2026-11-03) sobre el texto CRUDO, con U+00A0 =================================================
  ## Escribo el juicio desde la regla, no desde el texto: (a) espacio duro entre cifra y unidad y en «El Bosque»/«La Pradera», en toda la versión; (b) en la Parte 1,
  ## la unidad y el signo que cierran una fórmula van DENTRO de ella. Las Partes 2 a 6 no están obligadas por (b) (su unidad va fuera, con U+00A0, y eso se mide en (a)).
  crudo <- c(e$question, e$questionlist, e$solution, e$solutionlist); fuera_tabla <- crudo[!startsWith(crudo, "|")]
  n_nb <- sum(lengths(regmatches(fuera_tabla, gregexpr(paste0("[0-9]", NBSP, "(kilos?|cursos?)\\b"), fuera_tabla, perl = TRUE))))
  mal_sp <- fuera_tabla[grepl("[0-9] (kilos?|cursos?)\\b|\\$ (kilos?|cursos?)\\b", fuera_tabla, perl = TRUE)]
  nota("L6a ninguna cifra ni fórmula separada de «kilos»/«cursos» por un espacio normal (fuera de las tablas)", length(mal_sp) == 0L, paste(gsub(NBSP, "~", mal_sp, fixed = TRUE), collapse = " | "))
  nota("L6a control positivo: el patrón encuentra los pares cifra⍽unidad con U+00A0 (al menos 8 por versión)", n_nb >= 8L, n_nb)
  nom2 <- fil1[grepl(" ", fil1)]
  mal_nom <- nom2[vapply(nom2, function(z) any(grepl(z, fuera_tabla, fixed = TRUE)), NA)]
  nota("L6a los nombres de dos palabras («El Bosque», «La Pradera») no llevan espacio normal fuera de las tablas", length(mal_nom) == 0L, paste(mal_nom, collapse = " | "))
  nota("L6a control positivo: cada nombre de dos palabras de la tabla 1 aparece con U+00A0 en el texto", all(vapply(nom2, function(z) any(grepl(sub(" ", NBSP, z, fixed = TRUE), fuera_tabla, fixed = TRUE)), NA)), paste(nom2, collapse = " | "))
  s_cr <- e$solution
  i_a <- grep("^Parte 1\\. El concurso premia", s_cr); i_b <- grep("^Parte 2\\. El promedio por curso es", s_cr)
  j_a <- grep("^\\*\\*Parte 1 · ERR", s_cr)[1]; j_b <- grep("^\\*\\*Parte 2 · Lecturas", s_cr); k_c <- grep("^En la Parte 1,", s_cr)
  nota("L6b ubico la Parte 1 en la Solution (Procedimiento, tres análisis ERR y Caso específico)", length(i_a) == 1L && length(i_b) == 1L && length(j_b) == 1L && length(k_c) == 1L && !is.na(j_a), paste(i_a, i_b, j_a, j_b, k_c))
  if (length(i_a) == 1L && length(i_b) == 1L && length(j_b) == 1L && length(k_c) == 1L && !is.na(j_a)) {
    p1_scope <- c(e$solutionlist[1:4], s_cr[i_a:(i_b - 1L)], s_cr[j_a:(j_b - 1L)], s_cr[k_c])
    n_f <- 0L; malf <- character(0)
    for (lin_ in p1_scope) {
      fs <- regmatches(lin_, gregexpr("\\$[^$]+\\$", lin_))[[1]]; pos <- gregexpr("\\$[^$]+\\$", lin_)[[1]]
      for (u in seq_along(fs)) {
        n_f <- n_f + 1L; z <- lee_f(fs[u]); sig <- substr(lin_, pos[u] + nchar(fs[u]), nchar(lin_)); fin_dentro <- !is.null(z) && z$ok && (z$unidad == "kilos" || nzchar(z$signo))
        sin_unidad_fuera <- !grepl(paste0("^[ ", NBSP, "]*(kilos?|cursos?)\\b"), sig, perl = TRUE)
        sin_signo_fuera <- !grepl("^[.,:;!?)»”…\\]]", sig, perl = TRUE) && !grepl("^[.,:;!?)»”…\\]]", sub(paste0("^[ ", NBSP, "]+"), "", sig), perl = TRUE) | grepl("^ [a-záéíóú]", sig, perl = TRUE)
        bare_ok <- !is.null(z) && z$ok && !fin_dentro && z$unidad == "" && z$signo == "" && grepl("^ [a-záéíóú]", sig, perl = TRUE) && !grepl("^ (kilos?|cursos?)\\b", sig, perl = TRUE)
        if (!((fin_dentro && sin_unidad_fuera && !grepl("^[.,:;!?)»”…\\]]", sig, perl = TRUE)) || bare_ok)) malf <- c(malf, paste0(gsub(NBSP, "~", fs[u], fixed = TRUE), " ⏎ sigue: «", substr(gsub(NBSP, "~", sig, fixed = TRUE), 1, 12), "»"))
      }
    }
    nota("L6b (Parte 1) la unidad y el signo que cierran una fórmula van dentro de ella: ninguna unidad ni signo fuera", length(malf) == 0L, paste(malf, collapse = " | "))
    nota("L6b control positivo: la Parte 1 de la versión tiene al menos 10 fórmulas leídas", n_f >= 10L, n_f)
  }

  ## ===== (4) CÓDIGO: metadatos y regla #19 ==================================================================
  nota("K1 exclozetype = schoice|schoice|num|num|num|mchoice", identical(mi$clozetype, c("schoice", "schoice", "num", "num", "num", "mchoice")))
  nota("K1 extype = cloze y seis entradas de exsolution", identical(mi$type, "cloze") && length(marcas) == 6L)
  nota("K1 forma de exsolution: schoice/mchoice lógico de 4; num escalar", all(vapply(marcas[c(1, 2, 6)], function(z) is.logical(z) && length(z) == 4L, NA)) &&
       all(vapply(marcas[3:5], function(z) is.numeric(z) && length(z) == 1L, NA)))
  nota("K1 una sola marca en cada schoice; al menos una en el mchoice", sum(marcas[[1]]) == 1L && sum(marcas[[2]]) == 1L && sum(marcas[[6]]) >= 1L)
  nota("K1 extol = 0 en los seis gaps", identical(as.numeric(mi$tolerance), rep(0, 6)))
  nota("K1 expoints = 3|1|1|1|1|1", identical(as.numeric(mi$points), c(3, 1, 1, 1, 1, 1)))
  nota("K1 exshuffle = TRUE", isTRUE(as.logical(mi$shuffle)))
  nota("K1 exname", identical(mi$name, "totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1"))
  ## regla #19: la Solution y la retroalimentación no identifican opciones por letra o posición
  txt19 <- c(sol, sl)
  pat19 <- "[Oo]pci[oó]n(es)? [A-Da-d]\\b|\\([a-d]\\)|\\blas? (A|B|C|D)\\b|\\b[Ll]etra\\b|\\b[Aa]lternativa\\b|\\b(primera|segunda|tercera|cuarta|última|ultima) (opci[oó]n|afirmaci[oó]n)\\b|\\b(arriba|abajo)\\b|\\b(la|el) (primera|segunda|tercera|cuarta)\\b"
  nota("K2 (regla #19) ni la Solution ni la retroalimentación citan letras ni posiciones de opciones", !any(grepl(pat19, txt19, perl = TRUE)),
       paste(txt19[grepl(pat19, txt19, perl = TRUE)], collapse = " ; "))
  list(fallos = fallos, ficha = fic, checks = reg)
}

## ---------------------------------------------------------------------------------------------------------------
t0 <- Sys.time()
ex <- xexams(F, n = length(sem), seed = sem,
             driver = list(sweave = list(quiet = TRUE, pdf = FALSE, png = TRUE, encoding = "UTF-8"),
                           read = NULL, transform = NULL, write = NULL))
cat(sprintf("xexams: %d versiones en %.0f s\n", length(sem), as.numeric(difftime(Sys.time(), t0, units = "secs"))))
res <- lapply(seq_along(sem), function(i) verificar(ex[[i]][[1]]))
names(res) <- sem
nf <- vapply(res, function(r) length(r$fallos), 1L)
nchk <- vapply(res, function(r) length(r$checks), 1L)
cat(sprintf("\nVersiones verificadas: %d · con algún fallo: %d · fallos totales: %d\n", length(sem), sum(nf > 0), sum(nf)))
cat(sprintf("Comprobaciones por versión: mínimo %d · máximo %d · total %d\n", min(nchk), max(nchk), sum(nchk)))
for (s in names(res)[nf > 0]) { cat("\n--- semilla", s, "\n"); cat(paste0("  * ", res[[s]]$fallos), sep = "\n") }

## ---- comprobaciones por coherencia (prefijo del identificador de cada comprobación) -------------------------------
COHE <- c(S = "Semántica", V = "Visual-Texto", M = "Matemática", K = "Código", G = "General")
prefijo <- function(x) substr(x, 1, 1)
tc <- t(vapply(res, function(r) vapply(names(COHE), function(p) sum(prefijo(r$checks) == p), 1L), setNames(integer(length(COHE)), names(COHE))))
tf <- t(vapply(res, function(r) vapply(names(COHE), function(p) sum(prefijo(r$fallos) == p), 1L), setNames(integer(length(COHE)), names(COHE))))
resumen <- rbind(`mín. por versión` = apply(tc, 2, min), `máx. por versión` = apply(tc, 2, max), `total (100 versiones)` = colSums(tc), `fallos` = colSums(tf))
colnames(resumen) <- COHE[colnames(resumen)]
cat("\nComprobaciones por coherencia (S = Semántica, V = Visual-Texto, M = Matemática, K = Código, G = General):\n"); print(resumen)

## ---- cobertura de la muestra --------------------------------------------------------------------------------
fi <- lapply(res, `[[`, "ficha")
cat("\nCobertura de la muestra (semillas", range(sem), "):\n")
cat("  Parte 1 × otro colegio:\n"); print(table(Parte1 = vapply(fi, function(z) z$rama1, ""), OtroColegio = vapply(fi, function(z) z$rama2, "")))
cat("  Material:\n"); print(table(vapply(fi, function(z) z$mat, "")))
cat("  Fila (en la tabla 1) de la sede que gana:  ", paste(names(table(vapply(fi, function(z) z$gana1, ""))), table(vapply(fi, function(z) z$gana1, "")), sep = "=", collapse = " "), "\n")
cat("  Ecuaciones aritméticas comprobadas: ", sum(vapply(fi, function(z) z$n_ecuaciones, 1L)), "\n")
tot_alt <- vapply(fi, function(z) z$p1_total_leido_como_total, 1L)
cat(sprintf("  Lectura alterna de la cifra como «total» (residuo declarado de O-1): versiones con ≥ 1 afirmación verdadera bajo esa lectura: %d de %d; con exactamente una: %d\n",
            sum(tot_alt >= 1), length(tot_alt), sum(tot_alt == 1)))
cat(sprintf("  Opciones de la Parte 2 sin operación calculable con la tabla (la mayoría de los cursos): %d apariciones en %d versiones\n",
            sum(vapply(fi, function(z) z$p2_no_calculables, 1L)), sum(vapply(fi, function(z) z$p2_no_calculables, 1L) > 0)))

## ---- comprobaciones globales de la Parte 2 (las versiones juntas) --------------------------------------------------
glob <- character(0)
gnota <- function(id, ok, det = "") { det <- paste(det, collapse = " "); cat(sprintf("  [%s] %s%s\n", if (isTRUE(ok)) "OK   " else "FALLA", id, if (!isTRUE(ok) && nzchar(det)) paste0("  -> ", substr(det, 1, 300)) else ""))
                                      if (!isTRUE(ok)) glob <<- c(glob, id) }
cat("\n==== Comprobaciones globales de la Parte 2 (", length(sem), "versiones juntas) ====\n")
op_ver <- lapply(ex, function(m) nb(m[[1]]$questionlist)[5:8]); mk_ver <- lapply(ex, function(m) as.logical(m[[1]]$metainfo$solution[[2]]))
usados <- unique(unlist(op_ver))
gnota("S2 las opciones emitidas son EXACTAMENTE los ocho textos que juzgué (ni uno más, ni uno menos)", setequal(usados, POOL_P2$texto) && length(usados) == 8L,
      paste("fuera del banco:", paste(setdiff(usados, POOL_P2$texto), collapse = " | "), "· ausentes:", paste(setdiff(POOL_P2$texto, usados), collapse = " | ")))
claves_ver <- unlist(Map(function(o, m) o[m], op_ver, mk_ver)); dist_ver <- unlist(Map(function(o, m) o[!m], op_ver, mk_ver))
gnota("S2 los textos que R/exams marca como clave son exactamente los tres que juzgué correctos", setequal(unique(claves_ver), POOL_P2$texto[POOL_P2$juicio == "correcta"]), paste(unique(claves_ver), collapse = " | "))
gnota("S2 los textos que R/exams deja como distractor son exactamente los cinco que juzgué incorrectos", setequal(unique(dist_ver), POOL_P2$texto[POOL_P2$juicio == "incorrecta"]), paste(unique(dist_ver), collapse = " | "))
gnota("S2 mi juicio da 3 correctas y 5 incorrectas", sum(POOL_P2$juicio == "correcta") == 3L && sum(POOL_P2$juicio == "incorrecta") == 5L)
cat("  Frecuencia de cada texto en las", length(sem), "versiones (como clave / como distractor):\n")
for (k in seq_len(nrow(POOL_P2))) cat(sprintf("    %-8s %-10s clave %3d · distractor %3d · %s\n", POOL_P2$id[k], paste0("(", POOL_P2$juicio[k], ")"),
                                              sum(claves_ver == POOL_P2$texto[k]), sum(dist_ver == POOL_P2$texto[k]), POOL_P2$texto[k]))
cat("  Posición de la clave de la Parte 2 tras el exshuffle (1 a 4):", paste(names(table(vapply(mk_ver, which, 1L))), table(vapply(mk_ver, which, 1L)), sep = "=", collapse = " "), "\n")
gnota("S2 en ninguna versión coinciden dos claves o dos distractores iguales", all(vapply(op_ver, function(o) !anyDuplicated(o), NA)))
## las cinco versiones renderizadas conservan los casos que dicen ser (canónica, CxC, CxP, PxC, PxP)
esperado <- c(canonica = "CURSOS|CURSOS", CxC = "CURSOS|CURSOS", CxP = "CURSOS|PROMEDIO", PxC = "PROMEDIO|CURSOS", PxP = "PROMEDIO|PROMEDIO")
for (nmv in names(EN_PROFUNDIDAD)) { s <- as.character(EN_PROFUNDIDAD[[nmv]]); z <- fi[[s]]; if (is.null(z)) next
  gnota(sprintf("G semilla %s sigue siendo el caso «%s» (Parte 1 | otro colegio)", s, nmv), identical(paste(z$rama1, z$rama2, sep = "|"), unname(esperado[nmv])) &&
        (nmv != "canonica" || (identical(z$t1["fil1", ], c("Primaria", "Secundaria")) && identical(z$t1["t1", ], c("200", "180")))), paste(z$rama1, z$rama2)) }

## ---- fichas de las cinco versiones en profundidad -------------------------------------------------------------
cat("\n==== Fichas de las 5 versiones renderizadas (semillas de render_cloze.R) ====\n")
for (nmv in names(EN_PROFUNDIDAD)) {
  s <- as.character(EN_PROFUNDIDAD[[nmv]]); z <- fi[[s]]
  if (is.null(z)) next
  i <- match(as.integer(s), sem); clave2 <- POOL_P2$id[match(op_ver[[i]][mk_ver[[i]]], POOL_P2$texto)]
  cat(sprintf("\n[%s · semilla %s] Parte 1 (%s): ", nmv, s, z$rama1)); cat(paste(z$t1["fil1", ], z$t1["n1", ], "×", z$t1["a1", ], "=", z$t1["t1", ], collapse = " | "), "→ gana", z$gana1, "por", z$D1, "\n")
  cat(sprintf("   Otro colegio (%s): ", z$rama2)); cat(paste(z$t2["fil2", ], z$t2["n2", ], "×", z$t2["a2", ], "=", z$t2["t2", ], collapse = " | "), "→ gana", z$gana2, "por", z$D2, "· Parte 2: clave", clave2, "\n")
  cat("   fallos:", length(res[[s]]$fallos), "\n")
}

## ---- mutaciones de control: cada alteración deliberada DEBE hacer fallar al verificador POR SU PROPIA SONDA -------------
cat("\n==== Mutaciones de control (cada una debe producir ≥ 1 fallo, y entre ellos el de su sonda propia) ====\n")
k0 <- match(7L, sem); if (is.na(k0)) k0 <- 1L
e0 <- ex[[k0]][[1]]   # canónica si la semilla 7 está en la muestra
H_P2 <- "**Parte 2 · Lecturas del promedio que no sirven**"
dis <- function(e) which(!as.logical(e$metainfo$solution[[2]]))      # posiciones (1 a 4) de los distractores de la Parte 2
cla <- function(e) which(as.logical(e$metainfo$solution[[2]]))
e_con <- function(id) { t <- POOL_P2$texto[POOL_P2$id == id]; for (m in ex) { e <- m[[1]]; if (t %in% nb(e$questionlist)[5:8][dis(e)]) return(e) }; e0 }
cambiar_op <- function(e, de, a) { e$solutionlist <- gsub(de, a, e$solutionlist, fixed = TRUE); e$solution <- gsub(de, a, e$solution, fixed = TRUE); e }
mover <- function(m) { i <- which(m)[1]; j <- which(!m)[1]; m[c(i, j)] <- m[c(j, i)]; m }
mk <- function(f, espera, base = NULL) list(f = f, espera = espera, base = base)
mut <- list(
  "Parte 5 + 1" = mk(function(e) { e$metainfo$solution[[5]] <- e$metainfo$solution[[5]] + 1; e }, "^M3 Parte 5"),
  "Parte 3 y 4 intercambiadas" = mk(function(e) { s <- e$metainfo$solution; e$metainfo$solution[[3]] <- s[[4]]; e$metainfo$solution[[4]] <- s[[3]]; e }, "^M3 Parte [34]"),
  "una marca de la Parte 1 movida" = mk(function(e) { e$metainfo$solution[[1]] <- mover(e$metainfo$solution[[1]]); e }, "^M1 la marca exsolution de la Parte 1"),
  "una marca de la Parte 6 invertida" = mk(function(e) { m <- e$metainfo$solution[[6]]; m[1] <- !m[1]; e$metainfo$solution[[6]] <- m; e }, "^M4 las marcas de la Parte 6"),
  "marca de la Parte 2 movida" = mk(function(e) { e$metainfo$solution[[2]] <- mover(e$metainfo$solution[[2]]); e }, "^M2 la marca de la Parte 2"),
  "cifra de una celda de la tabla 2 alterada" = mk(function(e) { i <- grep("cursos</span>", e$question)[5]; e$question[i] <- sub("([0-9]+) cursos", "99 cursos", e$question[i]); e }, "^M3 Parte [345]"),
  "nombre de grado de la Parte 3 cambiado" = mk(function(e) { e$question <- sub("en total el grado (\\S+)\\?", "en total el grado quinto?", e$question); e }, "^S3 las Partes 3 y 4"),
  "##ANSWER3## movido tras la Parte 4" = mk(function(e) { q <- e$question; i <- which(q == "##ANSWER3##"); j <- which(q == "##ANSWER4##"); q[c(i, j)] <- q[c(j, i)]; e$question <- q; e }, "^V3 Parte [34]: su ##ANSWER"),
  "retroalimentación de la Parte 5 con otra diferencia" = mk(function(e) { e$solutionlist[11] <- sub("= [0-9]+\\$", "= 99$", e$solutionlist[11]); e }, "^M5 retroalimentación de la Parte 5"),
  "una letra en la Solution (regla #19)" = mk(function(e) { e$solution <- c(e$solution, "La opción B es la correcta."); e }, "^K2"),
  "encabezado de la Parte 2 del tipo retirado («Comparaciones que no deciden…»)" = mk(function(e) { s <- e$solution; s[s == H_P2] <- "**Parte 2 · Comparaciones que no deciden qué grado ganó**"; e$solution <- s; e }, "^G3"),
  "opción de la Parte 2 del tipo retirado" = mk(function(e) { e$questionlist[6] <- "La diferencia entre el promedio por curso de un grado y el del otro grado."; e }, "^S2 las cuatro opciones de la Parte 2 salen del banco"),
  "afirmación de la Parte 1 con otra cifra" = mk(function(e) { i <- grep("[ \u00a0]kilos más\\.$|[ \u00a0]en [0-9]+[ \u00a0]kilos\\.$", e$questionlist)[1]; e$questionlist[i] <- sub("[0-9]+([ \u00a0]kilos)", "7\\1", e$questionlist[i]); e }, "^M1 la Parte 1 tiene exactamente una|^M1 el evaluador|^S7 Parte 1|^M5 Parte 1"),
  ## ---- nuevas de la ronda 9 (Parte 2 con un solo tipo) ----
  "Parte 2: una opción incorrecta sustituida por OTRA CLAVE del banco (dos correctas)" = mk(function(e) { d <- dis(e)[1]; k <- POOL_P2$texto[POOL_P2$juicio == "correcta"]; e$questionlist[4 + d] <- setdiff(k, nb(e$questionlist)[4 + cla(e)])[1]; e }, "^S2 exactamente una opción correcta"),
  "Parte 2: una opción incorrecta sustituida por una CUARTA PARÁFRASIS CORRECTA escrita por mí (dos correctas)" = mk(function(e) { d <- dis(e)[1]; e$questionlist[4 + d] <- P2_EXTRA_CORRECTA; e }, "^S2 exactamente una opción correcta"),
  "Parte 2: retroalimentación de dos opciones incorrectas intercambiada" = mk(function(e) { d <- dis(e); i <- 4 + d[1]; j <- 4 + d[2]; s <- e$solutionlist; e$solutionlist[c(i, j)] <- s[c(j, i)]; e }, "^S7 Parte 2 opción [1-4] \\(incorrecta\\): «Falso\\.» y su explicación propia"),
  "Parte 2: encabezado de la Solution duplicado" = mk(function(e) { s <- e$solution; i <- which(s == H_P2); e$solution <- append(s, s[i], after = i); e }, "^G3"),
  "Parte 2: explicación con la operación cambiada («la mitad» → «el doble del promedio»)" = mk(function(e) cambiar_op(e, "da la mitad del promedio", "da el doble del promedio"), "^M2 Parte 2 opción [1-4]: todo lo que su explicación dice"),
  "Parte 2: explicación con la operación cambiada («menos que» → «más que el promedio»)" = mk(function(e) cambiar_op(e, "da menos que el promedio", "da más que el promedio"), "^M2 Parte 2 opción [1-4]: todo lo que su explicación dice"),
  "Parte 2: explicación con la operación cambiada («la cantidad de cursos» → «el promedio»)" = mk(function(e) cambiar_op(e, "da la cantidad de cursos", "da el promedio"), "^M2 Parte 2 opción [1-4]: todo lo que su explicación dice", base = function() e_con("d_prom")),
  "Parte 2: explicación con una afirmación añadida que el oráculo no sabe recalcular («y da el triple del promedio»)" = mk(function(e) cambiar_op(e, "da la mitad del promedio", "da la mitad del promedio y da el triple del promedio"), "^M2 Parte 2 opción [1-4]: toda cláusula"),
  "Parte 2: texto del tipo retirado añadido a la Solution" = mk(function(e) { e$solution <- c(e$solution, "Parte 2. Gana el grado que recogió más en total."); e }, "^S6 ni el enunciado"),
  "Parte 2: texto del tipo retirado en el enunciado" = mk(function(e) { i <- grep("^\\*\\*Parte 2\\.\\*\\* ", e$question); e$question[i] <- "**Parte 2.** ¿Qué se compara en la tabla del otro colegio al decidir qué grado ganó el concurso?"; e }, "^S2 la pregunta de la Parte 2 es la vigente"),
  "Parte 2: pregunta con otro material" = mk(function(e) { m <- regmatches(e$question, regexec("el promedio de (papel|cartón|plástico) recogido por curso de un grado", e$question)); m <- m[lengths(m) == 2L][[1]][2]
                                                      e$question <- sub(paste0("el promedio de ", m, " recogido por curso de un grado"), paste0("el promedio de ", setdiff(c("papel", "cartón", "plástico"), m)[1], " recogido por curso de un grado"), e$question, fixed = TRUE); e },
                                        "^V1 la pregunta de la Parte 2 cita el material"),
  "Parte 2: «Respuestas» apunta a un distractor" = mk(function(e) { i <- grep("^- Parte 2\\. «", e$solution); e$solution[i] <- paste0("- Parte 2. «", nb(e$questionlist)[4 + dis(e)[1]], "»"); e }, "^M6 «Respuestas» de la Parte 2"),
  "Parte 2: «Procedimiento» con el párrafo del tipo retirado" = mk(function(e) { i <- grep("^Parte 2\\. El promedio por curso es", e$solution); e$solution[i] <- "Parte 2. Gana el grado que recogió más en total, no el de mayor promedio."; e }, "^S6 la Parte 2 de la sección «Procedimiento»"),
  "Parte 2: dos opciones iguales" = mk(function(e) { d <- dis(e); e$questionlist[4 + d[2]] <- e$questionlist[4 + d[1]]; e }, "^S2 opciones únicas"),
  "Parte 2: dos marcas verdaderas en el exsolution" = mk(function(e) { m <- e$metainfo$solution[[2]]; m[which(!m)[1]] <- TRUE; e$metainfo$solution[[2]] <- m; e }, "^M2 la marca de la Parte 2"),
  "Parte 2: cifra impresa en una opción" = mk(function(e) { e$questionlist[5] <- sub("\\.$", " de 3 cursos.", e$questionlist[5]); e }, "^V11 el texto de las Partes 2 a 6"),
  "Parte 2: retroalimentación de la clave con «Falso.»" = mk(function(e) { k <- 4 + cla(e); e$solutionlist[k] <- sub("^Verdadero\\.", "Falso.", e$solutionlist[k]); e }, "^S7 Parte 2 opción [1-4] \\(correcta\\)"),
  "Parte 2: retroalimentación de un distractor con «Verdadero.»" = mk(function(e) { k <- 4 + dis(e)[1]; e$solutionlist[k] <- sub("^Falso\\.", "Verdadero.", e$solutionlist[k]); e }, "^S7 Parte 2 opción [1-4] \\(incorrecta\\)"),
  "Parte 2: opción del banco anterior (ronda 3)" = mk(function(e) { e$questionlist[6] <- "Lo que recogió el mejor de sus cursos dentro de ese grado."; e }, "^S2 las cuatro opciones de la Parte 2 salen del banco"),
  "Parte 2: una viñeta de la Solution con la explicación de otra opción" = mk(function(e) { b <- which(startsWith(e$solution, "- «Lo que recogió")); stopifnot(length(b) >= 2L)
                                                      t1 <- sub("^(- «[^»]+» ).*$", "\\1", e$solution[b[1]]); x2 <- sub("^- «[^»]+» ", "", e$solution[b[2]]); e$solution[b[1]] <- paste0(t1, x2); e }, "^S6 la Solution de la Parte 2 enumera"),
  "Procedimiento: la frase de las dos afirmaciones de cada sede con otra diferencia" = mk(function(e) { e$solution <- sub("solo una cita la diferencia correcta, [0-9]+[\u00a0 ]kilos", "solo una cita la diferencia correcta, 99 kilos", e$solution); e }, "^M6 «Procedimiento» Parte 1: la frase de las dos afirmaciones"),
  "Procedimiento: la frase de las dos afirmaciones con las sedes intercambiadas" = mk(function(e) { i <- grep("^Las dos afirmaciones que dan el triunfo a ", e$solution); a <- sub("^Las dos afirmaciones que dan el triunfo a (.+) son falsas.*$", "\\1", e$solution[i]); b <- sub("^.*se lo dan a (.+), solo una cita.*$", "\\1", e$solution[i])
                                                                    e$solution[i] <- sub(paste0("se lo dan a ", b, ","), paste0("se lo dan a ", "@@", ","), sub(paste0("triunfo a ", a, " son"), paste0("triunfo a ", b, " son"), e$solution[i], fixed = TRUE), fixed = TRUE); e$solution[i] <- sub("@@", a, e$solution[i], fixed = TRUE); e }, "^M6 «Procedimiento» Parte 1: la frase de las dos afirmaciones"),
  "Parte 6 · Afirmaciones falsas: una cifra de la explicación cambiada" = mk(function(e) { i <- grep("^- «Si el grado que perdió hubiera tenido el mismo promedio por curso", e$solution)[1]; e$solution[i] <- sub("= [0-9]+\\$", "= 157$", e$solution[i]); e }, "^S6 la Solution de la Parte 6 enumera"),
  "Parte 6 · Afirmaciones falsas: una viñeta omitida" = mk(function(e) { i <- grep("^- «Ganó el concurso el grado", e$solution)[1]; e$solution <- e$solution[-i]; e }, "^S6 la Solution de la Parte 6 enumera"),
  "otro material en la Solution (papel → cartón)" = mk(function(e) { e$solution <- sub("mayor cantidad de papel", "mayor cantidad de cartón", e$solution, fixed = TRUE); e }, "^V12"),
  "orden de los encabezados de la Solution alterado" = mk(function(e) { s <- e$solution; i <- which(s == "**Caso específico**"); j <- which(s == "**Reflexión metacognitiva**"); s[c(i, j)] <- s[c(j, i)]; e$solution <- s; e }, "^G la Solution tiene los doce encabezados"),
  "Parte 2: una viñeta de la Solution omitida" = mk(function(e) { b <- which(startsWith(e$solution, "- «Lo que recogió")); e$solution <- e$solution[-b[1]]; e }, "^S6 la Solution de la Parte 2 enumera"))
## ---- nuevas de la r10 (tanda C2): L-6 en la Parte 1 y criterio de la Parte 6. Son mutaciones del TEXTO EMITIDO; cada una declara su sonda ----
P6_ORIG_GEN <- P6_GENERAL
mp6 <- function(e, f) { s <- e$solution; i <- grep("^Parte 6\\. Cada hipótesis", nb(s)); s[i] <- f(s[i]); e$solution <- s; e }     # muta el párrafo (crudo)
e_con_nombre <- function(nom) { for (m in ex) { e <- m[[1]]; if (any(grepl(nom, e$questionlist[1:4], fixed = TRUE))) return(e) }; e0 }
mut_r10 <- list(
  "MC50: «más» → «menos» en la comparación de HP (rama verdadera)" = mk(function(e) mp6(e, function(p) sub("(, )más( que la diferencia de)", "\\1menos\\2", p)), "^P6 cada «más»/«menos»|^P6 la conclusión de «un curso más»"),
  "MC51: vuelve a la redacción de 27f789dc («si y solo si…») en lugar del criterio vigente" = mk(function(e) mp6(e, function(p) {
        g <- regmatches(p, regexec("Con un curso más, el grado (\\S+) habría sumado a su total ([0-9]+)\u00a0kilos.*, ([0-9]+)\u00a0kilos: habría", p))[[1]]
        paste0("Parte 6. Cada hipótesis cambia un solo dato de la tabla. Un curso más con el mismo promedio suma ese promedio al total, y un curso menos lo resta. Por eso el grado ", g[2], " habría ganado con un curso más si y solo si su promedio por curso, ", g[3], "\u00a0kilos, es mayor que la diferencia, ", g[4], "\u00a0kilos.") }), "^P6 un único párrafo|^P6 el párrafo lleva una sola vez el corte|^P6 ninguna de las cuatro redacciones"),
  "MC54: la comparación de HP cita el promedio del grado que ganó" = mk(function(e) mp6(e, function(p) { a_g <- sub("^.*habría restado a su total ([0-9]+)\u00a0kilos.*$", "\\1", p); sub("(habría sumado a su total )[0-9]+", paste0("\\1", a_g), p) }), "^P6 «un curso más»: el grado que perdió según la tabla"),
  "solo la conclusión de HP cambia («habría ganado» → «no le habría alcanzado para ganar»)" = mk(function(e) mp6(e, function(p) sub("habría ganado\\.", "no le habría alcanzado para ganar.", p)), "^P6 la conclusión de «un curso más»"),
  "solo la conclusión de HG cambia («habría perdido» → «habría seguido ganando»)" = mk(function(e) mp6(e, function(p) sub("habría\u00a0perdido\\.", "habría seguido\u00a0ganando.", p)), "^P6 la conclusión de «un curso menos»", base = function() e_con_hg_pierde()),
  "criterio con la frase de 32243d0a" = mk(function(e) mp6(e, function(p) sub("Por eso, para saber si el grado que perdió habría ganado con un curso más, o si el que ganó habría perdido con uno menos, basta comparar el promedio de cada uno con la diferencia entre los totales: si el promedio es mayor, sí; si es menor, no. ",
        "Por eso, si el grado que perdió tiene un curso más o el que ganó tiene uno menos, basta comparar el promedio de ese grado con la diferencia entre los totales: si el promedio es mayor, el ganador cambia; si es menor, no. ", p, fixed = TRUE)), "^P6 el tramo general es el criterio literal vigente"),
  "criterio con «sí» y «no» intercambiados" = mk(function(e) mp6(e, function(p) sub("si el promedio es mayor, sí; si es menor, no.", "si el promedio es mayor, no; si es menor, sí.", p, fixed = TRUE)), "^P6 el tramo general es el criterio literal vigente"),
  "redacción vieja de H3 («solo si … fuera mayor»)" = mk(function(e) mp6(e, function(p) { g <- regmatches(p, regexec("Con un curso más, el grado (\\S+) habría sumado a su total ([0-9]+)\u00a0kilos.*, ([0-9]+)\u00a0kilos: habría", p))[[1]]
        paste0("Parte 6. Cada hipótesis cambia un solo dato de la tabla. Un curso más con el mismo promedio suma ese promedio al total, y un curso menos lo resta. Por eso el grado ", g[2], " habría ganado con un curso más solo si su promedio por curso, ", g[3], "\u00a0kilos, fuera mayor que la diferencia, ", g[4], "\u00a0kilos.") }), "^P6 un único párrafo|^P6 el párrafo lleva una sola vez el corte|^P6 ninguna de las cuatro redacciones"),
  "texto no juzgado en el tramo con cifras (una cláusula añadida a la conclusión)" = mk(function(e) mp6(e, function(p) sub("habría ganado\\.", "habría ganado, y además sobraría papel.", p)), "^P6 el tramo con cifras tiene la forma juzgada"),
  "L-6: una espacio normal entre la cifra y «kilos» en el párrafo de la Parte 6" = mk(function(e) mp6(e, function(p) sub("([0-9])\u00a0kilos", "\\1 kilos", p)), "^P6 L-6 en el párrafo|^L6a ninguna cifra"),
  "L-6: la unidad fuera de una fórmula de la Parte 1 (Procedimiento)" = mk(function(e) { s <- e$solution; i <- grep("^- .*: \\$[0-9]+ \\\\times [0-9]+ = [0-9]+\\\\ \\\\text\\{kilos\\.\\}\\$$", s)[1]; s[i] <- sub("\\\\ \\\\text\\{kilos\\.\\}\\$$", paste0("$\u00a0kilos."), s[i]); e$solution <- s; e }, "^L6b \\(Parte 1\\) la unidad y el signo"),
  "L-6: el signo fuera de la fórmula en la retroalimentación de la Parte 1" = mk(function(e) { k <- which(startsWith(nb(e$solutionlist[1:4]), "Verdadero.")); e$solutionlist[k] <- sub("\\\\ \\\\text\\{kilos\\.\\}\\$$", "\\\\ \\\\text{kilos}$.", e$solutionlist[k]); e }, "^L6b \\(Parte 1\\) la unidad y el signo"),
  "L-6: el espacio normal entre la cifra y «kilos» en una opción de la Parte 1" = mk(function(e) { i <- grep("[0-9]\u00a0kilos", e$questionlist[1:4])[1]; e$questionlist[i] <- sub("([0-9])\u00a0kilos", "\\1 kilos", e$questionlist[i]); e }, "^L6a ninguna cifra"),
  "L-6: «El Bosque» con espacio normal en una opción de la Parte 1" = mk(function(e) { i <- grep("El\u00a0Bosque", e$questionlist[1:4])[1]; e$questionlist[i] <- sub("El\u00a0Bosque", "El Bosque", e$questionlist[i]); e }, "^L6a los nombres de dos palabras", base = function() e_con_nombre("El\u00a0Bosque")))
e_con_hg_pierde <- function() { for (m in ex) { e <- m[[1]]; if (grepl("habría\u00a0perdido\\.$", e$solution[grep("^Parte 6\\. Cada hipótesis", nb(e$solution))])) return(e) }; e0 }
mut <- c(mut, mut_r10)

n_ok <- 0L; n_prop <- 0L
for (nm in names(mut)) {
  m <- mut[[nm]]; base <- if (is.null(m$base)) e0 else m$base()
  mutada <- m$f(base)
  if (identical(mutada, base)) { cat(sprintf("  %-104s MUTACIÓN SIN EFECTO (no altera nada: no vale como control)  <<<<<<\n", nm)); next }
  r <- verificar(mutada); disp <- grep(m$espera, r$fallos, value = TRUE)
  ok <- length(r$fallos) >= 1L; prop <- length(disp) >= 1L; n_ok <- n_ok + ok; n_prop <- n_prop + prop
  cat(sprintf("  %-104s %s · sonda propia: %s (%d fallos)\n", nm, if (ok) "CAZADA" else "NO CAZADA  <<<<<<", if (prop) "sí" else "NO  <<<<<<", length(r$fallos)))
  if (prop) cat(sprintf("        ↳ %s\n", substr(disp[1], 1, 150)))
}
cat(sprintf("\nMutaciones: %d de %d cazadas · %d de %d por su sonda propia\n", n_ok, length(mut), n_prop, length(mut)))
ctrl <- c(sin_mutar_semilla_base = length(verificar(e0)$fallos), sin_mutar_con_d_prom = length(verificar(e_con("d_prom"))$fallos))
cat("Control sin mutar (debe dar 0 fallos):", paste(names(ctrl), ctrl, sep = " = ", collapse = " · "), "\n")
cat(sprintf("\nTOTAL: fallos por versión = %d · fallos globales = %d · mutaciones sin cazar o sin su sonda = %d\n", sum(nf), length(glob), length(mut) - n_prop + sum(ctrl)))
saveRDS(list(sem = sem, res = res, glob = glob), file.path(DIR_S, "resultados_verificacion.rds"))
