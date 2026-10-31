## verificar_coherencias.R — Paso 8 (coherencias_5), ronda 3, .Rmd md5 42f725bf191b81784f76b858c26f7733
##
## Verificador INDEPENDIENTE del generador: no usa ningún objeto interno del data_generation (ni `verdad()`, ni
## `verdad_p6()`, ni `afs`). Toma lo que R/exams EMITE para cada semilla (`xexams()`: pregunta en Markdown con sus
## dos tablas, opciones tras el exshuffle, vector `exsolution`, Solution y retroalimentación) y lo recalcula
## desde las tablas MOSTRADAS: totales, ganador, diferencia, veredicto de cada afirmación leída del texto.
## Incluye mutaciones de control: si una alteración deliberada no hace fallar al verificador, el verificador no vale.
##
## Uso (desde esta carpeta): TMPDIR=<tmp> Rscript --no-init-file verificar_coherencias.R [semillas...]
##   sin argumentos: la muestra estándar N = 100 (semillas 1:100; regla #23), que incluye las 5 versiones renderizadas.
suppressMessages(library(exams))
F <- normalizePath("../../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
cat("md5 del .Rmd:", unname(tools::md5sum(F)), "\n")
arg <- as.integer(commandArgs(trailingOnly = TRUE))
sem <- if (length(arg)) arg else 1:100
EN_PROFUNDIDAD <- c(canonica = 7L, CxC = 3L, CxP = 2L, PxC = 1L, PxP = 4L)

## ---------------------------------------------------------------------------------------------------------------
nb <- function(x) gsub(" ", " ", x, fixed = TRUE)
limpiar <- function(x) trimws(nb(gsub("**", "", gsub("<[^>]+>", "", x), fixed = TRUE)))
num <- function(s) as.numeric(unlist(regmatches(s, gregexpr("[0-9]+", s))))

## Pool de la Parte 2 con MI juicio semántico (correcta / incorrecta respecto de la pregunta de su tipo).
P2_PREG <- c(promedio = "^¿Qué indica, en la tabla del otro colegio, el promedio de (papel|cartón|plástico) recogido por curso de un grado\\?$",
             comparar = "^¿Qué se compara en la tabla del otro colegio al decidir qué grado ganó el concurso\\?$")
P2_OK <- list(
  promedio = c("Lo que recogió el grado dividido entre el número de sus cursos participantes.",
               "Lo que recogió ese grado dividido en partes iguales entre sus cursos.",
               "Lo que recogió ese grado dividido entre sus cursos."),
  comparar = c("La suma de lo recogido por los cursos de un grado y la suma del otro grado.",
               "El producto de cursos por promedio de un grado y el del otro grado.",
               "La cantidad de cursos multiplicada por el promedio de un grado y lo mismo del otro grado."))
P2_MAL <- list(
  promedio = c("Lo que recogió el mejor de sus cursos dentro de ese grado.",
               "Lo que recogió el grado sumando lo de sus cursos.",
               "Lo que recogió cualquiera de sus cursos porque en el grado recogieron lo mismo.",
               "Lo que recogió el que queda en el medio al ordenar sus cursos en ese grado.",
               "Lo que recogió la mayoría de sus cursos en ese grado."),
  comparar = c("El promedio por curso de un grado y el del otro grado.",
               "La cantidad de cursos participantes de un grado y la del otro grado.",
               "La diferencia entre el promedio por curso de un grado y el del otro grado.",
               "La suma de la cantidad de cursos y el promedio por curso de un grado y la suma del otro grado.",
               "El promedio dividido entre la cantidad de cursos de un grado y el del otro grado."))
## Frase de la retroalimentación que corresponde a cada opción incorrecta de la Parte 2 (palabra ancla).
P2_ANCLA <- c("Lo que recogió el mejor de sus cursos dentro de ese grado." = "curso que más recogió",
              "Lo que recogió el grado sumando lo de sus cursos." = "total del grado",
              "Lo que recogió cualquiera de sus cursos porque en el grado recogieron lo mismo." = "todos los cursos recogieron lo mismo",
              "Lo que recogió el que queda en el medio al ordenar sus cursos en ese grado." = "mediana",
              "Lo que recogió la mayoría de sus cursos en ese grado." = "moda",
              "El promedio por curso de un grado y el del otro grado." = "Un promedio mayor",
              "La cantidad de cursos participantes de un grado y la del otro grado." = "Más cursos",
              "La diferencia entre el promedio por curso de un grado y el del otro grado." = "resta de los promedios",
              "La suma de la cantidad de cursos y el promedio por curso de un grado y la suma del otro grado." = "Sumar la cantidad de cursos",
              "El promedio dividido entre la cantidad de cursos de un grado y el del otro grado." = "Dividir el promedio")

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
  fallos <- character(0)
  nota <- function(id, ok, det = "") { det <- paste(det, collapse = " "); if (!isTRUE(ok)) fallos <<- c(fallos, if (nzchar(det)) paste0(id, " — ", det) else id) }
  q <- nb(e$question); ql <- nb(e$questionlist); sl <- nb(e$solutionlist); sol <- nb(e$solution)
  mi <- e$metainfo; marcas <- mi$solution
  fic <- list()

  ## ===== (2) VISUAL-TEXTO: tablas, nombres, material, marcadores =========================================
  tl <- grep("^\\|", q, value = TRUE); tl <- tl[!grepl("^\\|[-:| ]+\\|$", tl)]
  nota("V4 dos tablas de cabecera + 2 filas", length(tl) == 6L, paste("filas de tabla:", length(tl)))
  if (length(tl) != 6L) return(list(fallos = fallos, ficha = fic))
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
  ## Parte 2
  tipo <- names(P2_PREG)[vapply(P2_PREG, function(p) grepl(p, pregunta(2)), NA)]
  nota("S2 la pregunta de la Parte 2 es de uno de los dos tipos", length(tipo) == 1L, pregunta(2))
  if (length(tipo) == 1L) {
    op <- ql[5:8]
    nota("S2 las cuatro opciones salen del pool de SU tipo (sin mezcla entre tipos)", all(op %in% c(P2_OK[[tipo]], P2_MAL[[tipo]])), paste(op[!op %in% c(P2_OK[[tipo]], P2_MAL[[tipo]])], collapse = " | "))
    nota("S2 exactamente una opción correcta", sum(op %in% P2_OK[[tipo]]) == 1L)
    nota("M2 la marca de la Parte 2 coincide con mi juicio semántico", identical(as.logical(marcas[[2]]), op %in% P2_OK[[tipo]]))
    nota("S2 opciones únicas", !anyDuplicated(op))
    ## retroalimentación de cada opción de la Parte 2
    for (j in 1:4) {
      f <- sl[4 + j]
      if (op[j] %in% P2_OK[[tipo]]) nota(sprintf("S7 Parte 2 opción %d (correcta): retroalimentación «Verdadero.»", j), startsWith(f, "Verdadero."))
      else nota(sprintf("S7 Parte 2 opción %d (incorrecta): «Falso.» y su explicación propia", j),
                startsWith(f, "Falso.") && grepl(P2_ANCLA[[op[j]]], f, fixed = TRUE), f)
    }
    ## encabezado de la Solution según el tipo (H3 de la FASE 2C r1)
    h_si <- if (tipo == "promedio") "**Parte 2 · Lecturas del promedio que no sirven**" else "**Parte 2 · Comparaciones que no deciden qué grado ganó**"
    h_no <- if (tipo == "promedio") "**Parte 2 · Comparaciones que no deciden qué grado ganó**" else "**Parte 2 · Lecturas del promedio que no sirven**"
    nota("G3 la Solution lleva el encabezado de su tipo de Parte 2", sum(sol == h_si) == 1L && sum(sol == h_no) == 0L, paste("tipo", tipo))
    ## y lista exactamente las tres opciones incorrectas de esa versión, cada una con la explicación de su retroalimentación
    i_h <- which(sol == h_si)
    nota("S6 existe un único encabezado de la Parte 2 de su tipo", length(i_h) == 1L)
    if (length(i_h) == 1L) {
      sig_h <- which(startsWith(sol, "**") & seq_along(sol) > i_h)[1]
      bullets <- sol[(i_h + 1):(sig_h - 1)]; bullets <- bullets[startsWith(bullets, "- «")]
      esperados <- vapply(which(!(op %in% P2_OK[[tipo]])), function(j) paste0("- «", op[j], "» ", sub("^Falso\\. ", "", sl[4 + j])), "")
      nota("S6 la Solution de la Parte 2 enumera las tres incorrectas de esta versión, con la explicación de su retroalimentación",
           length(bullets) == 3L && setequal(bullets, esperados), paste(setdiff(esperados, bullets), collapse = " ; "))
    }
    nota("S6 la Parte 2 de la sección «Procedimiento» corresponde a su tipo",
         if (tipo == "promedio") any(grepl("^Parte 2\\. El promedio por curso es el total del grado dividido entre la cantidad de cursos", sol)) else
                                 any(grepl("^Parte 2\\. Gana el grado que recogió más en total", sol)))
    fic$tipo_p2 <- tipo
  }
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
  ok_eq <- vapply(eq, function(z) {
    m <- regmatches(z, regexec("^(\\d+) (\\\\times|-|\\+) (\\d+) = (\\d+)$", z))[[1]]
    if (length(m) != 5L) return(NA)
    x <- as.numeric(m[2]); y <- as.numeric(m[4]); r <- switch(m[3], "\\times" = x * y, "-" = x - y, "+" = x + y)
    r == as.numeric(m[5])
  }, NA)
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
  exp1 <- sprintf("Verdadero. %s recogió $%d \\times %d = %d$ kilos y %s, $%d \\times %d = %d$ kilos: ganó por $%d - %d = %d$ kilos.",
                  sub("^la", "La", nm1[w1]), n1[w1], a1[w1], t1[w1], nm1[3 - w1], n1[3 - w1], a1[3 - w1], t1[3 - w1], t1[w1], t1[3 - w1], D1)
  nota("M5 retroalimentación de la clave de la Parte 1 = totales y diferencia de la tabla 1", length(clave1) == 1L && identical(sl[clave1], exp1), sl[clave1])
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
  fila_p1 <- c(sprintf("- %s: $%d \\times %d = %d$ kilos.", sub("^la", "La", nm1[1]), n1[1], a1[1], t1[1]),
               sprintf("- %s: $%d \\times %d = %d$ kilos.", sub("^la", "La", nm1[2]), n1[2], a1[2], t1[2]))
  nota("M6 «Procedimiento» Parte 1: los dos totales de la tabla 1", all(fila_p1 %in% sol), paste(fila_p1[!fila_p1 %in% sol], collapse = " ; "))
  fila_p2 <- sprintf("- Grado %s: $%d \\times %d = %d$ kilos.", tolower(fil2), n2, a2, t2)
  nota("M6 «Procedimiento» Partes 3 a 5: los dos totales de la tabla 2", all(fila_p2 %in% sol), paste(fila_p2[!fila_p2 %in% sol], collapse = " ; "))
  ganol1 <- sprintf("Ganó %s, con $%d - %d = %d$ kilos más. ", nm1[w1], t1[w1], t1[3 - w1], D1)
  nota("M6 «Procedimiento» Parte 1: nombra al ganador y la diferencia de la tabla 1", any(startsWith(sol, ganol1)), ganol1)
  ganol2 <- sprintf("Ganó el grado %s, con $%d - %d = %d$ kilos más. ", tolower(fil2[w2]), t2[w2], t2[l2], D2)
  nota("M6 «Procedimiento» Partes 3 a 5: nombra al ganador y la diferencia de la tabla 2", any(startsWith(sol, ganol2)), ganol2)
  ## la frase sobre el perfil (cursos/promedio) es coherente con la rama
  perfil1 <- if (n1[w1] > n1[3 - w1]) sprintf("Su promedio era menor (%d kilos frente a %d), pero participaron más cursos (%d frente a %d), y eso pesó más.", a1[w1], a1[3 - w1], n1[w1], n1[3 - w1]) else
             sprintf("Participaron menos cursos (%d frente a %d), pero su promedio era mayor (%d kilos frente a %d), y eso pesó más.", n1[w1], n1[3 - w1], a1[w1], a1[3 - w1])
  nota("M6 «Procedimiento» Parte 1: la frase del perfil de la sede ganadora coincide con la tabla", any(grepl(perfil1, sol, fixed = TRUE)), perfil1)
  perfil2 <- if (n2[w2] > n2[l2]) sprintf("Su promedio era menor (%d kilos frente a %d), pero participaron más cursos (%d frente a %d).", a2[w2], a2[l2], n2[w2], n2[l2]) else
             sprintf("Participaron menos cursos (%d frente a %d), pero su promedio era mayor (%d kilos frente a %d).", n2[w2], n2[l2], a2[w2], a2[l2])
  nota("M6 «Procedimiento» Partes 3 a 5: la frase del perfil del grado ganador coincide con la tabla", any(grepl(perfil2, sol, fixed = TRUE)), perfil2)
  umbral <- sprintf("el grado %s habría ganado con un curso más solo si su promedio por curso, %d kilos, fuera mayor que la diferencia, %d kilos; y el grado %s habría perdido con un curso menos solo si su promedio, %d kilos, fuera mayor que esa diferencia.",
                    tolower(fil2[l2]), a2[l2], D2, tolower(fil2[w2]), a2[w2])
  nota("M6 «Procedimiento» Parte 6: el umbral (promedio del grado vs diferencia) usa las cifras de la tabla", any(grepl(umbral, sol, fixed = TRUE)), umbral)
  caso1 <- sprintf("En la Parte 1, %s recogió $%d \\times %d = %d$ kilos y %s recogió $%d \\times %d = %d$ kilos. La diferencia es $%d - %d = %d$ kilos a favor de %s.",
                   nm1[w1], n1[w1], a1[w1], t1[w1], nm1[3 - w1], n1[3 - w1], a1[3 - w1], t1[3 - w1], t1[w1], t1[3 - w1], D1, nm1[w1])
  nota("M6 «Caso específico» Parte 1 = tabla 1", any(sol == caso1), caso1)
  caso2 <- sprintf("En el otro colegio, el grado %s recogió $%d \\times %d = %d$ kilos y el grado %s, $%d \\times %d = %d$ kilos: la diferencia es %d kilos.",
                   tolower(fil2[w2]), n2[w2], a2[w2], t2[w2], tolower(fil2[l2]), n2[l2], a2[l2], t2[l2], D2)
  nota("M6 «Caso específico» otro colegio = tabla 2", any(sol == caso2), caso2)

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
  list(fallos = fallos, ficha = fic)
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
cat(sprintf("\nVersiones verificadas: %d · con algún fallo: %d · fallos totales: %d\n", length(sem), sum(nf > 0), sum(nf)))
for (s in names(res)[nf > 0]) { cat("\n--- semilla", s, "\n"); cat(paste0("  * ", res[[s]]$fallos), sep = "\n") }

## ---- cobertura de la muestra --------------------------------------------------------------------------------
fi <- lapply(res, `[[`, "ficha")
cat("\nCobertura de la muestra (semillas", range(sem), "):\n")
cat("  Parte 1 × otro colegio:\n"); print(table(Parte1 = vapply(fi, function(z) z$rama1, ""), OtroColegio = vapply(fi, function(z) z$rama2, "")))
cat("  Tipo de Parte 2:\n"); print(table(vapply(fi, function(z) if (is.null(z$tipo_p2)) NA_character_ else z$tipo_p2, "")))
cat("  Material:\n"); print(table(vapply(fi, function(z) z$mat, "")))
cat("  Fila (en la tabla 1) de la sede que gana:  ", paste(names(table(vapply(fi, function(z) z$gana1, ""))), table(vapply(fi, function(z) z$gana1, "")), sep = "=", collapse = " "), "\n")
cat("  Ecuaciones aritméticas comprobadas: ", sum(vapply(fi, function(z) z$n_ecuaciones, 1L)), "\n")
tot_alt <- vapply(fi, function(z) z$p1_total_leido_como_total, 1L)
cat(sprintf("  Lectura alterna de la cifra como «total» (residuo declarado de O-1): versiones con ≥ 1 afirmación verdadera bajo esa lectura: %d de %d; con exactamente una: %d\n",
            sum(tot_alt >= 1), length(tot_alt), sum(tot_alt == 1)))

## ---- fichas de las cinco versiones en profundidad -------------------------------------------------------------
cat("\n==== Fichas de las 5 versiones renderizadas (semillas de render_cloze.R) ====\n")
for (nmv in names(EN_PROFUNDIDAD)) {
  s <- as.character(EN_PROFUNDIDAD[[nmv]]); z <- fi[[s]]
  if (is.null(z)) next
  cat(sprintf("\n[%s · semilla %s] Parte 1 (%s): ", nmv, s, z$rama1)); cat(paste(z$t1["fil1", ], z$t1["n1", ], "×", z$t1["a1", ], "=", z$t1["t1", ], collapse = " | "), "→ gana", z$gana1, "por", z$D1, "\n")
  cat(sprintf("   Otro colegio (%s): ", z$rama2)); cat(paste(z$t2["fil2", ], z$t2["n2", ], "×", z$t2["a2", ], "=", z$t2["t2", ], collapse = " | "), "→ gana", z$gana2, "por", z$D2, "· Parte 2:", z$tipo_p2, "\n")
  cat("   fallos:", length(res[[s]]$fallos), "\n")
}

## ---- mutaciones de control: cada alteración deliberada DEBE hacer fallar al verificador ---------------------------
cat("\n==== Mutaciones de control (cada una debe producir ≥ 1 fallo) ====\n")
k0 <- match(7L, sem); if (is.na(k0)) k0 <- 1L
e0 <- ex[[k0]][[1]]   # canónica si la semilla 7 está en la muestra
mut <- list(
  "Parte 5 + 1" = function(e) { e$metainfo$solution[[5]] <- e$metainfo$solution[[5]] + 1; e },
  "Parte 3 y 4 intercambiadas" = function(e) { s <- e$metainfo$solution; e$metainfo$solution[[3]] <- s[[4]]; e$metainfo$solution[[4]] <- s[[3]]; e },
  "una marca de la Parte 1 movida" = function(e) { m <- e$metainfo$solution[[1]]; e$metainfo$solution[[1]] <- rev(m); e },
  "una marca de la Parte 6 invertida" = function(e) { m <- e$metainfo$solution[[6]]; m[1] <- !m[1]; e$metainfo$solution[[6]] <- m; e },
  "marca de la Parte 2 movida" = function(e) { m <- e$metainfo$solution[[2]]; e$metainfo$solution[[2]] <- rev(m); e },
  "cifra de una celda de la tabla 2 alterada" = function(e) { i <- grep("cursos</span>", e$question)[5]; e$question[i] <- sub("([0-9]+) cursos", "99 cursos", e$question[i]); e },
  "nombre de grado de la Parte 3 cambiado" = function(e) { e$question <- sub("en total el grado (\\S+)\\?", "en total el grado quinto?", e$question); e },
  "##ANSWER3## movido tras la Parte 4" = function(e) { q <- e$question; i <- which(q == "##ANSWER3##"); j <- which(q == "##ANSWER4##"); q[c(i, j)] <- q[c(j, i)]; e$question <- q; e },
  "retroalimentación de la Parte 5 con otra diferencia" = function(e) { e$solutionlist[11] <- sub("= [0-9]+\\$", "= 99$", e$solutionlist[11]); e },
  "una letra en la Solution (regla #19)" = function(e) { e$solution <- c(e$solution, "La opción B es la correcta."); e },
  "encabezado de la Parte 2 de otro tipo" = function(e) { s <- e$solution; i <- which(grepl("^\\*\\*Parte 2 · (Lecturas|Comparaciones)", s)); s[i] <- ifelse(grepl("Lecturas", s[i]), "**Parte 2 · Comparaciones que no deciden qué grado ganó**", "**Parte 2 · Lecturas del promedio que no sirven**"); e$solution <- s; e },
  "opción de la Parte 2 de otro tipo" = function(e) { e$questionlist[6] <- "La diferencia entre el promedio por curso de un grado y el del otro grado."; e },
  "afirmación de la Parte 1 con otra cifra" = function(e) { i <- grep(" kilos más\\.$| en [0-9]+ kilos\\.$", e$questionlist)[1]; e$questionlist[i] <- sub("[0-9]+ kilos", "7 kilos", e$questionlist[i]); e })
for (nm in names(mut)) {
  r <- verificar(mut[[nm]](e0))
  cat(sprintf("  %-55s %s (%d fallos)\n", nm, if (length(r$fallos) >= 1L) "CAZADA" else "NO CAZADA  <<<<<<", length(r$fallos)))
}
cat("\nControl sin mutar (debe dar 0 fallos):", length(verificar(e0)$fallos), "\n")
saveRDS(list(sem = sem, res = res), "resultados_verificacion.rds")
