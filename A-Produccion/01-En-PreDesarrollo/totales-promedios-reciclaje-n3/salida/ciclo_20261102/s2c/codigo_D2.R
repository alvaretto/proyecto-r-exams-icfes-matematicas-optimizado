## =============================================================================
## MAT-2026-07-050 — pregunta impresa 50, SAI «Nivelación 6», grado 10, Sesión 2.
## Fuente verbatim: ITEM_OFICIAL.md. Invariantes L-1..L-5: .claude/CLAUDE.md local.
## Tabla Markdown con tabla_responsiva() (Flujo B = n, decisión del profesor, 2026-10-29).
## Notación: G = la sede que gana (mayor total), P = la que pierde; n = cursos,
## a = promedio por curso, T = n * a, D = T_G - T_P (la cifra de la clave).
## Ramas (L-3): CURSOS (gana la de más cursos y menor promedio, la del impreso) y
## PROMEDIO (gana la de mayor promedio y menos cursos).
## =============================================================================
library(exams)

## --- Espacio duro (S1, ciclo 2026-11-02) ------------------------------------------
## U+00A0 entre cifra y unidad («15 kilos», «4 cursos») en las opciones y en la Solution, y entre
## las dos palabras de «El Bosque» y «La Pradera» también fuera de la tabla, para que un renglón no
## parta la cifra de su unidad ni el nombre de la sede (BAJA 1 y 2 de la 6b del CLOZE hermano). Va
## siempre como esta variable, nunca como carácter literal; pandoc lo pasa a «~» en LaTeX. Las
## celdas de cifras de la tabla conservan el espacio normal.
## Umbral medido en el Moodle del colegio (2026-10-30, salida/r4b/medicion_qtext_moodle.md, N = 100):
## la caja de la pregunta (.qtext) mide 270 px a 320 px y 310 px a 360 px. El espacio duro de las
## celdas de «El Bosque» y «La Pradera» sube el ancho mínimo de la tabla hasta +21,4 px (esas tablas
## llegan a 209,9 px; la más ancha, la canónica, a 225,6 px, con Roboto), y eso solo pesaba con cajas
## de menos de 240 px: en el colegio, 0/100 tablas fuera de su caja a 320 y a 360 px.
ED <- "\u00a0"
## Tanda S1b (2026-11-02, decisión del profesor «Arreglarlo ahora»): cuando la cifra cierra una fórmula, la unidad va
## DENTRO de ella, «$a \\times b = T\\ \\text{kilos}$», y no fuera con ED: Chromium parte el renglón entre la fórmula
## y el espacio duro (6b de S1, N = 100: 132 de 1158 a 360 px y 259 de 1158 a 320 px). El PDF ya no lo partía.
## Tanda S1b relanzada (2026-11-03, decisión del profesor «V1' (Recomendada)»): el signo que sigue a la unidad entra
## también en la fórmula, «$... = T\\ \\text{kilos.}$»: con el signo fuera, Chromium partía el renglón entre la
## fórmula y el signo (6b de V1, N = 100: el punto solo en 34 semillas a 360 px y en 5 a 320 px).
## Tanda S2a (2026-11-03, decisión del profesor «Cerrarlo ahora (Recomendada)»): el signo que sigue a una fórmula SIN
## unidad también entra en ella, «$... = 5\\text{.}$». Es la explicación de ERR-RESTA, la única: con el punto fuera,
## Chromium podía partir el renglón entre la fórmula y el punto (42 casos en 100 versiones; H2 de la FASE 2C r5).

## --- Utilidades (familia F5: muestreo seguro) --------------------------------
pick  <- function(x) x[sample.int(length(x), 1L)]     # vectores, también de longitud 1
pick1 <- function(x) x[[sample.int(length(x), 1L)]]   # listas
cap1  <- function(s) paste0(toupper(substr(s, 1L, 1L)), substring(s, 2L))
pond  <- function(s) { u <- utf8ToInt(s); sum(seq_along(u) * as.numeric(u)) }   # suma ponderada por posición
participaron_txt <- function(x) if (x == 1) paste0("participó 1", ED, "curso") else paste0("participaron ", x, ED, "cursos")
## H1 (FASE 2C, ronda 1): cifras en kilos que salen de un cálculo directo con la tabla (pura).
## La razón sin sustento no cita ninguna, para que su diagnóstico no engañe.
calculos_tabla <- function(p) unique(c(p$delta, p$aG + p$aP, p$nG * p$delta, p$nP * p$delta,
  p$k * p$aG, p$k * p$aP, p$TG, p$TP, p$nG * p$aP, p$nP * p$aG))

## --- Familia F2: tabla responsiva (copia local de la de .claude/scripts/snippets_familias_rmd.R,
## sin su rama is_latex_output(), que bajo R/exams nunca se ejecuta). Aborta si el ancho
## de contenido supera 85 caracteres: en el PDF una tabla pipe no parte sus celdas.
## Ronda 4 (B1 del 6b, ronda 3): la tabla de R/exams y la de Moodle no traen relleno de celda, y
## las cabeceras quedaban a 4 px entre sí. Cada celda va en un <span> con estilo propio, puesto
## después de medir el ancho: pandoc lo conserva en HTML y Moodle, y en PDF y DOCX deja solo el
## texto. El estilo va en línea, y no en un <style>, para no tocar las demás tablas de la página de
## Moodle. Desde la regla #21 v1.4 (2026-10-29) la función canónica también tiene los dos argumentos
## de abajo, con el mismo <span>, pero desactivados por defecto (el primero vale NULL): en otro
## ejercicio la misma variante saca la tabla de su caja a 360 px. Esta copia los deja activados por
## defecto, con los valores medidos aquí.
## Medido en 100 versiones (salida/r4/medicion_relleno_tabla.md): 0,6 em por lado en cada celda hacía
## que la tabla no cupiera en su caja a 360 px en 49/100 versiones (0/100 sin relleno). Con 0,5 em
## solo a la izquierda de las columnas 2 y 3 y las cabeceras al 90 %, 0/100 a 360 px y 9/100 a
## 320 px, igual que sin relleno, con 11 a 13,5 px entre cabeceras.
tabla_responsiva <- function(df, align = NULL, bold_rows = integer(0), ancho_max_pdf = 85,
                             separacion = "0.5em", tam_cabecera = "0.9em") {
  df2 <- data.frame(lapply(df, as.character), stringsAsFactors = FALSE, check.names = FALSE)
  if (length(bold_rows)) for (r in bold_rows)
    df2[r, ] <- lapply(df2[r, , drop = FALSE], function(x) paste0("**", x, "**"))
  anchos_col <- vapply(seq_along(df2), function(j)
    max(nchar(c(names(df2)[j], df2[[j]]))), numeric(1))
  ancho <- sum(anchos_col) + 2L * length(anchos_col) + 1L
  if (!is.null(ancho_max_pdf) && ancho > ancho_max_pdf)
    stop(sprintf(paste0(
      "tabla_responsiva(): la tabla mide %d caracteres de ancho y en el PDF de R/exams ",
      "una tabla pipe no parte el texto: se saldría del margen (límite medido: %d). ",
      "Use lista_responsiva(), acorte las celdas o, si ya revisó el PDF, pase ",
      "ancho_max_pdf = NULL."), as.integer(ancho), ancho_max_pdf), call. = FALSE)
  if (!is.null(separacion)) {
    envolver <- function(x, j, cab) paste0("<span style=\"display:inline-block;",
      if (j > 1L) paste0(" padding-left:", separacion, ";") else "",
      if (cab) paste0(" font-size:", tam_cabecera, ";") else "", "\">", x, "</span>")
    for (j in seq_along(df2)) df2[[j]] <- envolver(df2[[j]], j, FALSE)
    names(df2) <- vapply(seq_along(df2), function(j) envolver(names(df2)[j], j, TRUE), "")
  }
  md <- paste(knitr::kable(df2, format = "markdown", align = align), collapse = "\n")
  paste0("::: {style=\"overflow-x:auto; -webkit-overflow-scrolling:touch; max-width:100%; margin:0.6em 0;\"}\n\n",
         md, "\n\n:::\n")
}

## --- Instancia canónica (L-1, L-5): literales del impreso --------------------
CANON_ENUNCIADO <- paste(
  "En un colegio se realiza un concurso de reciclaje que premiará a la sede que recoja la mayor cantidad",
  "de papel. La tabla muestra la cantidad de cursos que participaron y el promedio de papel recogido por",
  "curso en la sede primaria y en la sede secundaria.")
PREGUNTA <- "De acuerdo con la información anterior, ¿cuál de las siguientes afirmaciones es VERDADERA?"
CANON_OPCIONES <- c(
  A = paste0("La sede primaria ganó el concurso porque recogió 40", ED, "kilos más que la sede de secundaria."),
  B = paste("La sede primaria ganó el concurso porque participaron más cursos y aunque el promedio fue menor,",
            paste0("el total fue de 20", ED, "kilos más.")),
  C = paste0("La sede secundaria ganó porque, aunque participaron menos cursos, tuvo mayor promedio con 5", ED,
             "kilos de más."),
  D = paste("La sede secundaria ganó el concurso porque en ella hubo un curso que recogió más que los otros,",
            "aumentando la cantidad de papel recogida en todo el colegio."))

## --- Contextos (regla #11: ocho plantillas, ocho estructuras) -----------------
CONJUNTOS <- list(
  list(tipo = "sede", encab = "Sede", nombres = c("la sede primaria", "la sede secundaria"),
       filas = c("Primaria", "Secundaria")),
  list(tipo = "sede", encab = "Sede", nombres = c("la sede norte", "la sede sur"), filas = c("Norte", "Sur")),
  list(tipo = "sede", encab = "Sede", nombres = c("la sede urbana", "la sede rural"), filas = c("Urbana", "Rural")),
  list(tipo = "sede", encab = "Sede", nombres = c("la sede central", "la sede campestre"),
       filas = c("Central", "Campestre")),
  ## Ronda 4 (B2 del 6b, ronda 3): espacio duro en las celdas, para que a 360 px «El Bosque» no se
  ## parta en dos renglones. S1 (ciclo 2026-11-02): también en los nombres, que salen en el enunciado,
  ## las opciones y la Solution; y con ED, no con el carácter literal.
  list(tipo = "sede", encab = "Sede", nombres = c(paste0("la sede El", ED, "Bosque"), paste0("la sede La", ED, "Pradera")),
       filas = c(paste0("El", ED, "Bosque"), paste0("La", ED, "Pradera"))),
  list(tipo = "jornada", encab = "Jornada", nombres = c("la jornada de la mañana", "la jornada de la tarde"),
       filas = c("Mañana", "Tarde")))
MATERIALES <- c("papel", "cartón", "plástico")
PROTAGONISTAS <- c("la profesora Marta", "el profesor Andrés", "la coordinadora del proyecto ambiental",
                   "la personera estudiantil", "el grupo ecológico", "el rector")
## Toda plantilla declara el criterio del concurso: gana la mayor cantidad TOTAL (guarda abajo).
contextos <- list(
  list(plantilla = function(t, m, p) {   # estructura: institucional
    paste0("En un colegio se realiza un concurso de reciclaje que premiará a la ", t,
           " que recoja la mayor cantidad de ", m, ".") }),
  list(plantilla = function(t, m, p) {   # estructura: periodistica
    paste0("El periódico escolar anunció un concurso de reciclaje: el premio será para la ", t,
           " que recoja la mayor cantidad de ", m, ".") }),
  list(plantilla = function(t, m, p) {   # estructura: dialogo
    paste0("—Ganará la ", t, " que recoja la mayor cantidad de ", m, " —anunció ", p,
           " al abrir el concurso de reciclaje del colegio.") }),
  list(plantilla = function(t, m, p) {   # estructura: perspectiva_estudiante
    paste0("Imagina que haces parte del comité ambiental de tu colegio, que organizó un concurso de ",
           "reciclaje para premiar a la ", t, " que recoja la mayor cantidad de ", m, ".") }),
  list(plantilla = function(t, m, p) {   # estructura: situacion_problema
    paste0(cap1(p), " debe entregar el premio del concurso de reciclaje del colegio a la ", t,
           " que recoja la mayor cantidad de ", m, ".") }),
  list(plantilla = function(t, m, p) {   # estructura: pregunta_retorica
    paste0("¿Qué ", t, " del colegio ganará el concurso de reciclaje? El premio será para la que recoja ",
           "la mayor cantidad de ", m, ".") }),
  list(plantilla = function(t, m, p) {   # estructura: accion_en_curso
    paste0(cap1(p), " está revisando los resultados del concurso de reciclaje del colegio, que premiará a la ",
           t, " que recoja la mayor cantidad de ", m, ".") }),
  list(plantilla = function(t, m, p) {   # estructura: descubrimiento
    paste0("Al revisar las planillas del concurso de reciclaje, ", p, " recordó que el premio es para la ", t,
           " que recoja la mayor cantidad de ", m, ".") }))

## S2c: parámetros del candidato (en memoria)
Q_DELTA_P  <- c(CURSOS = 0, PROMEDIO = 0)
VEHICULO   <- c(CURSOS = "padv", PROMEDIO = "psin")
RESTA_G_OK <- c(CURSOS = TRUE, PROMEDIO = TRUE)
D2_SIN_CIFRA <- TRUE
## --- Espacio de parámetros (familia F1: se enumera y se sortea; nada se remuestrea)
N_CURSOS  <- 3:8
PROMEDIOS <- 20:60
espacio_rama <- function(rama, xP_delta = FALSE) {
  g <- expand.grid(nG = N_CURSOS, nP = N_CURSOS, aG = PROMEDIOS, aP = PROMEDIOS)
  g <- if (rama == "CURSOS") g[g$nG > g$nP & g$aG < g$aP, ] else g[g$nG < g$nP & g$aG > g$aP, ]
  g$k <- abs(g$nG - g$nP); g$delta <- abs(g$aG - g$aP)
  g$TG <- g$nG * g$aG; g$TP <- g$nP * g$aP; g$D <- g$TG - g$TP
  ## Ventaja parcial: lo que aporta el punto fuerte de cada sede. D = parcialG - parcialP.
  g$parcialG <- if (rama == "CURSOS") g$k * g$aG else g$nG * g$delta
  g$parcialP <- if (rama == "CURSOS") g$nP * g$delta else g$k * g$aP
  ok <- g$k <= 3 & g$delta >= 2 & g$delta <= 15 & g$D >= 6 & g$D <= 0.3 * g$TP
  ok <- ok & g$D != g$aG & g$D != g$aP                 # L-2: el dato aislado de G nunca es D
  g$xP <- if (xP_delta) g$delta else g$parcialP        # S2c: cifra de padv
  ok <- ok & g$D != g$delta & g$D != g$xP              # L-2 y canal 8: la resta y la cifra de P tampoco
  g <- g[ok, ]
  ## Canal 3: si la cifra de la ventaja de P ya es un número de la tabla (k = 1 en la rama
  ## PROMEDIO: k * aP = aP), el distractor de G no cita otro dato aislado.
  g$p_tabla <- g$xP == g$aG | g$xP == g$aP | g$xP == g$nG | g$xP == g$nP
  ## Canal 2: ¿hay una cifra falsa del lado de G por debajo de D y distinta de la de P?
  g$hay_debajo <- (RESTA_G_OK[[rama]] & !xP_delta & g$delta < g$D & g$delta != g$xP) |
                  (!g$p_tabla & ((g$aG < g$D & g$aG != g$xP) | (g$aP < g$D & g$aP != g$xP)))
  rownames(g) <- NULL
  g
}

## --- Moldes de las afirmaciones (L-1). Cada molde declara lo que afirma (L-2) ---
## perfil M = más cursos y menor promedio; perfil P = menos cursos y mayor promedio.
## Las cuatro opciones comparten el cierre «su total fue más alto que el de O en x kilos»: todas
## citan una cifra y nombran las dos sedes (canales 4 y 5). Ronda 3 (variante C, decisión del
## profesor en el WAIT_USER #3 de la ronda 2): la cifra va en el complemento del comparativo, que
## mide la diferencia. El cierre anterior, «su total fue de x kilos más que el de O», traía la
## cláusula completa «su total fue de x kilos», que admitía esa otra lectura (regla #13 v1.1).
## Medido en la ronda 2: «superó en x kilos al de O» abría un canal léxico (+18,1 pp en la rama
## CURSOS, «la que MÁS comparte»; +14,3 pp en la rama PROMEDIO, «la que MENOS comparte») y «fue mayor
## que», otro (+21,1 / +16,4 pp); este cierre conserva «más». Los moldes tienen largos parecidos; en
## la canónica, en cambio, A es la más corta y D la más larga.
cierre <- function(x, O) if (is.na(x)) paste0("su total fue más alto que el de ", O,
  " en lo mismo que se diferencian los dos promedios.") else paste0("su total fue más alto que el de ", O, " en ", x, ED, "kilos.")
## Regla #13 v1.1: toda lectura razonable cuenta. lecturas_de() da las lecturas que admite el TEXTO
## de una afirmación: siempre «diferencia» (la cifra es T_S - T_O) y, si el texto trae una cláusula
## completa con la cifra («fue de N kilos», «recogió N kilos»), también «total» (la cifra es T_S).
## El cierre de arriba no la trae; la clave de la canónica sí («el total fue de 20 kilos más»).
## S1: entre la cifra y «kilos» puede ir el espacio duro; la lectura se detecta con los dos espacios.
lecturas_de <- function(texto) c("diferencia", if (grepl(paste0("(fue de|recogió) [0-9]+[ ", ED, "]kilos"), texto)) "total")
MOLDES <- list(
  M_a = list(perfil = "M", afirma = "dif_total", f = function(S, O, x) paste0(cap1(S),
    " ganó el concurso porque participaron más cursos y, aunque el promedio fue menor, ", cierre(x, O))),
  M_b = list(perfil = "M", afirma = "dif_total", f = function(S, O, x) paste0(cap1(S),
    " ganó el concurso porque, aunque su promedio fue menor, participaron más cursos y ", cierre(x, O))),
  P_a = list(perfil = "P", afirma = "dif_total", f = function(S, O, x) paste0(cap1(S),
    " ganó el concurso porque, aunque participaron menos cursos, tuvo mayor promedio y ", cierre(x, O))),
  P_b = list(perfil = "P", afirma = "dif_total", f = function(S, O, x) paste0(cap1(S),
    " ganó el concurso porque tuvo mayor promedio y, aunque participaron menos cursos, ", cierre(x, O))),
  N_a = list(perfil = NA, afirma = "dif_total", f = function(S, O, x) paste0(cap1(S),
    " ganó el concurso porque, al sumar lo que recogieron todos y cada uno de sus cursos, ", cierre(x, O))),
  N_b = list(perfil = NA, afirma = "dif_total", f = function(S, O, x) paste0(cap1(S),
    " ganó el concurso porque, después de juntar lo que recogieron todos sus cursos, ", cierre(x, O))),
  U_a = list(perfil = NA, afirma = "sin_sustento", f = function(S, O, x) paste0(cap1(S),
    " ganó el concurso porque, al sumar solo lo que recogieron sus cursos más grandes, ", cierre(x, O))),
  U_b = list(perfil = NA, afirma = "sin_sustento", f = function(S, O, x) paste0(cap1(S),
    " ganó el concurso porque en ella hubo un curso que recogió más que los otros, y ", cierre(x, O))),
  U_c = list(perfil = NA, afirma = "sin_sustento", f = function(S, O, x) paste0(cap1(S),
    " ganó el concurso porque en sus cursos hay más alumnos para recoger, y por eso ", cierre(x, O))),
  U_d = list(perfil = NA, afirma = "sin_sustento", f = function(S, O, x) paste0(cap1(S),
    " ganó el concurso porque sus cursos reciclaron durante más días que los demás, y ", cierre(x, O))),
  ## Solo para la estructura de la opción C de la canónica (su texto es el del impreso).
  P_ven = list(perfil = "P", afirma = "dif_prom", f = function(S, O, x) paste0(cap1(S),
    " ganó el concurso porque, aunque participaron menos cursos, tuvo mayor promedio con ", x,
    ED, "kilos de más.")))

## --- Función de verdad PURA (L-2): sin sample() ni estado --------------------
## Evalúa cada parte de la afirmación con los datos y con la lectura indicada (regla #13 v1.1:
## «diferencia» o «total», ver lecturas_de()); una razón que la tabla no puede sustentar vale NA,
## y una afirmación solo es verdadera si TODAS sus partes lo son.
verdad <- function(af, d, lectura = "diferencia") {
  stopifnot(lectura %in% c("diferencia", "total"))
  n <- c(G = d$nG, P = d$nP); a <- c(G = d$aG, P = d$aP); tt <- n * a
  S <- af$sede; O <- setdiff(c("G", "P"), S)
  v <- c(gana = tt[[S]] > tt[[O]])
  if (!is.na(af$perfil)) {
    v <- c(v, perfil = if (af$perfil == "M") n[[S]] > n[[O]] && a[[S]] < a[[O]] else
                                             n[[S]] < n[[O]] && a[[S]] > a[[O]])
  }
  v <- c(v, parte = switch(af$afirma,
    dif_total    = if (lectura == "total") tt[[S]] == af$x else tt[[S]] - tt[[O]] == af$x,
    dif_prom     = a[[S]] - a[[O]] == af$x,
    dif_igual_delta = tt[[S]] - tt[[O]] == abs(a[["G"]] - a[["P"]]),
    sin_sustento = NA,
    stop("verdad(): afirmación de tipo desconocido: ", af$afirma)))
  isTRUE(all(v))
}

## --- Pool de errores conceptuales (regla #1; al menos 4, L-1) -----------------------
## calcula(): la cifra que produce la lectura errónea (pura; usa solo los datos y la
## presentación elegida, nunca el azar). explica(): análisis con los números de la versión.
ventaja_txt <- function(p) if (p$rama == "CURSOS") "tener mayor promedio" else "tener más cursos"
no_gano <- function(p) paste0(cap1(p$nomP), " no ganó: recogió ", p$TP, ED, "kilos, menos que los ", p$TG,
                              " de ", p$nomG, ".")
errores_conceptuales <- list(
  list(codigo = "ERR-DATO", lado = "G", nombre = "Toma un dato aislado de la tabla",
       descripcion_corta = "Toma un número que ya está en la tabla como si fuera la diferencia entre los totales.",
       descripcion_larga = paste("Los promedios de la tabla describen un curso típico de cada grupo; cuánto más",
                                 "recogió un grupo que el otro solo se sabe al calcular y restar los totales."),
       causa_raiz = "Busca la respuesta entre los datos impresos en lugar de calcular con ellos.",
       precondicion = function(p) isTRUE(p$fuente_dato %in% c("aG", "aP")) && p[[p$fuente_dato]] != p$D,
       calcula = function(p) {
         if (!isTRUE(p$fuente_dato %in% c("aG", "aP"))) stop("calcula(ERR-DATO): fuera de contexto")
         p[[p$fuente_dato]] },
       explica = function(p, af) paste0("La cifra ", af$x, ED, "kilos es el promedio de ",
         if (p$fuente_dato == "aG") p$nomG else p$nomP, ", un dato de la tabla, no la diferencia entre los ",
         "totales. ", cap1(p$nomG), " recogió $", p$nG, " \\times ", p$aG, " = ", p$TG, "\\ \\text{kilos}$ y ", p$nomP,
         ", $", p$nP, " \\times ", p$aP, " = ", p$TP, "\\ \\text{kilos:}$ la diferencia es ", p$D, ED, "kilos.")),
  list(codigo = "ERR-RESTA", lado = "G", nombre = "Resta los promedios",
       descripcion_corta = "Resta los dos promedios y toma esa resta como la diferencia entre los totales.",
       descripcion_larga = paste("La resta de los promedios compara un curso típico con otro; cuando los grupos",
                                 "tienen distinta cantidad de cursos, la diferencia entre los totales es otra."),
       causa_raiz = "Opera con los promedios como si fueran totales.",
       precondicion = function(p) p$delta != p$D,
       calcula = function(p) { if (p$delta == p$D) stop("calcula(ERR-RESTA): fuera de contexto"); p$delta },
       explica = function(p, af) paste0(if (is.na(af$x)) "Esa afirmación toma la resta de los promedios como la diferencia entre los totales: $" else
         paste0("La cifra ", af$x, " sale de restar los promedios: $"),
         max(p$aG, p$aP), " - ", min(p$aG, p$aP), " = ", p$delta, "\\text{.}$ Esa resta compara un curso típico de cada ",
         p$tipo, ", no las ", p$tipo, "s completas: como participaron ", p$nG, " y ", p$nP,
         ED, "cursos, la diferencia entre los totales es $", p$TG, " - ", p$TP, " = ", p$D, "\\ \\text{kilos.}$")),
  list(codigo = "ERR-PARCIAL", lado = "G", nombre = "Cuenta solo la ventaja de quien gana",
       descripcion_corta = paste("Calcula lo que aporta el punto fuerte del grupo que gana y olvida lo que aporta",
                                 "el punto fuerte del otro."),
       descripcion_larga = paste("Cada grupo tiene una ventaja: uno, más cursos; el otro, mayor promedio. La",
                                 "diferencia entre los totales es lo que aporta una ventaja menos lo que aporta",
                                 "la otra."),
       causa_raiz = "Considera solo una de las dos cantidades que forman el total.",
       precondicion = function(p) p$parcialG != p$D,
       calcula = function(p) { if (p$parcialG == p$D) stop("calcula(ERR-PARCIAL): fuera de contexto"); p$parcialG },
       explica = function(p, af) {
         if (p$rama == "CURSOS") {
           paste0("La cifra $", p$k, " \\times ", p$aG, " = ", af$x, "$ cuenta solo lo que ",
             if (p$k == 1) "aporta el curso" else paste0("aportan los ", p$k, ED, "cursos"), " de más de ", p$nomG,
             " y olvida que en ", p$nomP, " cada curso recogió, en promedio, ", p$delta, ED, "kilos más: $", p$nP,
             " \\times ", p$delta, " = ", p$parcialP, "\\ \\text{kilos}$ que compensan parte de esa ventaja. La diferencia ",
             "es $", af$x, " - ", p$parcialP, " = ", p$D, "\\ \\text{kilos.}$")
         } else {
           paste0("La cifra $", p$nG, " \\times ", p$delta, " = ", af$x, "$ cuenta solo la ventaja de promedio de ",
             p$nomG, " en sus ", p$nG, ED, "cursos y olvida que en ", p$nomP, " ", participaron_txt(p$k),
             if (p$k == 1) " más, que aporta $" else " más, que aportan $", p$k, " \\times ", p$aP, " = ",
             p$parcialP, "\\ \\text{kilos.}$ La diferencia es $",
             af$x, " - ", p$parcialP, " = ", p$D, "\\ \\text{kilos.}$")
         } }),
  ## ERR-TOTAL (x = T_G): salió en la ronda 2, porque con el cierre «su total fue de x kilos más que
  ## el de O» la lectura «su total fue de x kilos» lo volvía verdadero. Repuesto en la ronda 3 con el
  ## cierre del comparativo, que no admite esa lectura (guarda de la regla #13 v1.1, abajo).
  list(codigo = "ERR-TOTAL", lado = "G", nombre = "Confunde el total con la diferencia",
       descripcion_corta = "Calcula el total del grupo que gana y lo presenta como la diferencia con el otro.",
       descripcion_larga = paste("El total de un grupo dice cuánto recogió; para saber cuánto más recogió que el",
                                 "otro hay que restarle el total del otro."),
       ## Ronda 4 (H3 de la FASE 2C, ronda 3): la afirmación sí compara; lo que falta es restar.
       causa_raiz = "Se detiene a mitad del procedimiento: calcula un total, pero no le resta el del otro grupo.",
       precondicion = function(p) p$TG != p$D,
       calcula = function(p) { if (p$TG == p$D) stop("calcula(ERR-TOTAL): fuera de contexto"); p$TG },
       ## Ronda 4 (B3 del 6b, ronda 3): sin paréntesis alrededor de la fórmula, que a 360 px dejaba
       ## el «(» solo al final del renglón.
       explica = function(p, af) paste0("La cifra ", af$x, " es el total de ", p$nomG, ": $", p$nG, " \\times ",
         p$aG, " = ", p$TG, "\\ \\text{kilos.}$ No es la diferencia: falta restarle el total de ", p$nomP, ", y $", p$TG,
         " - ", p$TP, " = ", p$D, "\\ \\text{kilos.}$")),
  list(codigo = "ERR-PROM", lado = "P", nombre = "Compara solo los promedios",
       descripcion_corta = paste("Ve el mayor promedio y concluye que ganó quien lo tiene, sin tener en cuenta que",
                                 "allí participaron menos cursos."),
       descripcion_larga = paste("Un promedio más alto no garantiza un total más alto: el total también depende",
                                 "de cuántos cursos participaron."),
       causa_raiz = "Toma el promedio como criterio del concurso, cuando el criterio es la cantidad total.",
       precondicion = function(p) p$rama == "CURSOS" && isTRUE(p$fuente_padv %in% c("delta", "parcialP")),
       calcula = function(p) {
         if (!isTRUE(p$fuente_padv %in% c("delta", "parcialP"))) stop("calcula(ERR-PROM): fuera de contexto")
         p[[p$fuente_padv]] },
       explica = function(p, af) {
         if (af$molde == "P_ven" || identical(p$fuente_padv, "delta")) {
           paste0("Es cierto que el promedio de ", p$nomP, " es ", p$delta, ED, "kilos mayor, pero allí participaron ",
             "menos cursos: su total, $", p$nP, " \\times ", p$aP, " = ", p$TP, "\\ \\text{kilos,}$ es menor que el de ",
             p$nomG, ", ", p$TG, ED, "kilos. ", cap1(p$nomP), " no ganó.")
         } else {
           paste0("Es cierto que el promedio de ", p$nomP, " es ", p$delta, ED, "kilos mayor. La cifra $", p$nP,
             " \\times ", p$delta, " = ", af$x, "$ suma esa ventaja en sus ", p$nP, ED, "cursos, pero olvida que en ",
             p$nomG, " ", participaron_txt(p$k), if (p$k == 1) " más, que aporta $" else " más, que aportan $",
             p$k, " \\times ", p$aG, " = ",
             p$parcialG, "\\ \\text{kilos.}$ ", no_gano(p))
         } }),
  list(codigo = "ERR-CURSOS", lado = "P", nombre = "Compara solo la cantidad de cursos",
       descripcion_corta = paste("Ve que en un grupo participaron más cursos y concluye que ganó, sin tener en",
                                 "cuenta que su promedio es menor."),
       descripcion_larga = paste("Más cursos no garantizan un total más alto: el total también depende de cuánto",
                                 "recogió, en promedio, cada curso."),
       causa_raiz = "Toma la cantidad de cursos como criterio del concurso, cuando el criterio es la cantidad total.",
       precondicion = function(p) p$rama == "PROMEDIO" && identical(p$fuente_padv, "parcialP"),
       calcula = function(p) {
         if (!identical(p$fuente_padv, "parcialP")) stop("calcula(ERR-CURSOS): fuera de contexto")
         p$parcialP },
       explica = function(p, af) paste0("Es cierto que en ", p$nomP, " ", participaron_txt(p$k),
         " más. La cifra $", p$k, " \\times ", p$aP, " = ", af$x, "$ cuenta solo lo que ",
         if (p$k == 1) "aporta el curso" else "aportan los cursos", " de más, pero olvida que en ", p$nomG,
         " cada curso recogió, en promedio, ", p$delta, ED, "kilos más: $", p$nG, " \\times ", p$delta, " = ",
         p$parcialG, "\\ \\text{kilos.}$ ", no_gano(p))),
  list(codigo = "ERR-INVENTA", lado = "P", nombre = "Inventa una explicación que no sale de la tabla",
       descripcion_corta = paste("Elige una razón que suena posible, aunque la tabla no da ningún dato para",
                                 "sostenerla."),
       descripcion_larga = paste("La tabla solo informa cuántos cursos participaron y cuánto recogió, en",
                                 "promedio, cada curso. No dice qué recogió un curso en particular, cuántos",
                                 "alumnos tiene ni cuántos días recicló: una afirmación apoyada en eso no se",
                                 "puede justificar con la tabla."),
       causa_raiz = "Completa con suposiciones la información que la tabla no da, en lugar de calcular con la que sí da.",
       precondicion = function(p) TRUE,
       calcula = function(p) {
         if (!is.na(p$y) && (p$y == p$D || (p$y %in% calculos_tabla(p) && !(isTRUE(p$s2c_psin_delta) && p$y == p$delta))))
           stop("calcula(ERR-INVENTA): fuera de contexto")
         p$y },
       explica = function(p, af) {
         razon <- switch(af$molde,
           U_a = paste0("Sobre lo recogido, la tabla da solo el promedio por curso de cada ", p$tipo,
                        "; no dice cuánto recogió cada ",
                        "curso ni cuántos alumnos tiene, así que no permite saber cuáles son los más grandes ni sumar ",
                        "solo lo que ellos recogieron."),
           U_b = paste0("Sobre lo recogido, la tabla da solo el promedio por curso de cada ", p$tipo,
                        "; no dice cuánto recogió cada ",
                        "curso, así que no permite afirmar que hubo un curso destacado."),
           U_c = paste0("La tabla no dice cuántos alumnos tiene cada curso, y aunque lo dijera, el concurso se ",
                        "decide por los kilos recogidos."),
           U_d = paste0("La tabla no dice durante cuántos días recicló cada curso, y el concurso se decide por los ",
                        "kilos recogidos."))
         ## H1 (FASE 2C, ronda 1): «no sale de ningún cálculo con la tabla» era falso en el 2,84 % de
         ## las versiones. Esta frase la garantiza la construcción (la cifra nunca es D).
         cifra <- if (is.na(af$x)) "" else paste0(" La cifra de ", af$x, ED, "kilos tampoco es la diferencia ",
                                                  "entre los totales, que es de ", p$D, ED, "kilos.")
         paste0(razon, cifra, " Además, ", p$nomP, " no ganó: recogió ", p$TP, ED, "kilos, menos que los ", p$TG,
                " de ", p$nomG, ".")
       }))
names(errores_conceptuales) <- vapply(errores_conceptuales, function(e) e$codigo, character(1))
stopifnot("regla #1: pool de al menos 4 errores" = length(errores_conceptuales) >= 4)

## --- Sorteo de la versión ----------------------------------------------------
## L-5: la canónica sale con probabilidad 0,10 (decisión del profesor, 2026-10-29).
P_CANON <- 0.10
stopifnot("L-5: P_CANON <= 0,10" = P_CANON <= 0.10)
es_canonica <- sample(c(TRUE, FALSE), 1, prob = c(P_CANON, 1 - P_CANON))
## L-3: la canónica va en la rama CURSOS; las demás la sortean con 4/9 para CURSOS, de modo
## que la frecuencia global de CURSOS sea 0,10 + 0,90 * 4/9 = 0,50.
rama <- if (es_canonica) "CURSOS" else sample(c("CURSOS", "PROMEDIO"), 1, prob = c(4, 5) / 9)
if (es_canonica) {
  conj <- CONJUNTOS[[1]]; mat <- "papel"; gi <- 1L
  d <- list(nG = 5, nP = 4, aG = 40, aP = 45)
  ## S7 (tanda S1b relanzada, 2026-11-03): la canónica no usa lado_gdis; rasgos() de verificar_espacio.R calcula
  ## su lado de las cifras (5 < 20 (D) < 40). rec() del CLOZE la deja de leer en el re-empalme.
} else {
  conj <- pick1(CONJUNTOS); mat <- pick(MATERIALES); gi <- pick(1:2)   # P4: la fila de G se sortea
  ## Canal de magnitud (§P7, «cifra mediana»): el lugar de D entre las cuatro cifras citadas se
  ## sortea. b_debajo = cuántas cifras de distractor quedan por debajo de D (lugar de D = b + 1).
  ## La cifra de la ventaja de P (parcialP) la fija el dato, así que se filtran las combinaciones
  ## compatibles con b (F1: se filtra un conjunto finito; nada se remuestrea).
  b_debajo <- pick(0:3)
  s2c_padv_delta <- rama == "CURSOS" && VEHICULO[["CURSOS"]] == "padv" && Q_DELTA_P[["CURSOS"]] > 0 &&
    runif(1) < Q_DELTA_P[["CURSOS"]]
  esp <- espacio_rama(rama, xP_delta = s2c_padv_delta)
  pa_deb <- esp$xP < esp$D
  compatible <- switch(as.character(b_debajo), "0" = !pa_deb, "1" = rep(TRUE, nrow(esp)),
                       "2" = pa_deb | esp$hay_debajo, "3" = pa_deb & esp$hay_debajo)
  esp <- esp[compatible, ]
  stopifnot("F1: espacio de parámetros no vacío" = nrow(esp) > 0)
  f <- esp[sample.int(nrow(esp), 1L), ]
  d <- list(nG = f$nG, nP = f$nP, aG = f$aG, aP = f$aP)
  ctx <- pick1(contextos)
}
d$rama <- rama
d$k <- abs(d$nG - d$nP); d$delta <- abs(d$aG - d$aP)
d$TG <- d$nG * d$aG; d$TP <- d$nP * d$aP; d$D <- d$TG - d$TP
d$parcialG <- if (rama == "CURSOS") d$k * d$aG else d$nG * d$delta
d$parcialP <- if (rama == "CURSOS") d$nP * d$delta else d$k * d$aP
perfil_G <- if (rama == "CURSOS") "M" else "P"
perfil_P <- if (rama == "CURSOS") "P" else "M"
nomG <- conj$nombres[gi]; nomP <- conj$nombres[3L - gi]
stopifnot("L-3: cursos y promedio en sentidos opuestos" = (d$nG - d$nP) * (d$aG - d$aP) < 0,
          "L-3: gana G" = d$TG > d$TP)
if (es_canonica) stopifnot("L-3: la canónica va en la rama CURSOS" = rama == "CURSOS")

afirmacion <- function(molde, sede, x, codigo) {
  mo <- MOLDES[[molde]]
  S <- if (sede == "G") nomG else nomP
  O <- if (sede == "G") nomP else nomG
  list(molde = molde, sede = sede, x = x, codigo = codigo, perfil = mo$perfil, afirma = mo$afirma,
       texto = mo$f(S, O, x))
}

if (es_canonica) {
  fuente_dato <- "aG"; fuente_padv <- "delta"          # D de la canónica no cita cifra: no hay y
  afs <- list(
    clave = afirmacion("M_a", "G", d$D, "CLAVE"),
    gdis  = afirmacion("N_a", "G", d$aG, "ERR-DATO"),
    padv  = afirmacion("P_ven", "P", d$delta, "ERR-PROM"),
    psin  = afirmacion("U_b", "P", NA_real_, "ERR-INVENTA"))
  ## Textos del impreso, con sus rarezas («la sede de secundaria», «ganó porque»).
  afs$clave$texto <- CANON_OPCIONES[["B"]]; afs$gdis$texto <- CANON_OPCIONES[["A"]]
  afs$padv$texto  <- CANON_OPCIONES[["C"]]; afs$psin$texto <- CANON_OPCIONES[["D"]]
} else {
  ## Moldes: como en el impreso, de las dos afirmaciones de G una da el perfil de la sede y la
  ## otra no; cuál de las dos es la clave se sortea. Así las de perfil son siempre dos (una de
  ## cada sede) y las que no lo dan, otras dos (la de G y la sin sustento): ni el tipo de molde
  ## ni el vocabulario compartido delatan la clave (L-1; §P7, «la que más comparte»).
  moldes_de <- function(t, p) if (t == "neutro") c("N_a", "N_b") else if (p == "M") c("M_a", "M_b") else c("P_a", "P_b")
  tipo_clave <- pick(c("perfil", "neutro")); tipo_gdis <- if (tipo_clave == "perfil") "neutro" else "perfil"
  molde_clave <- pick(moldes_de(tipo_clave, perfil_G))
  molde_gdis  <- pick(moldes_de(tipo_gdis, perfil_G))
  ## Lados respecto de D: el de la ventaja de P lo fija el dato; G y U se reparten el resto.
  lado_padv <- if (f$xP < d$D) "debajo" else "encima"
  faltan <- b_debajo - (lado_padv == "debajo")
  repartos <- list(c(gdis = "encima", psin = "encima"), c(gdis = "debajo", psin = "encima"),
                   c(gdis = "encima", psin = "debajo"), c(gdis = "debajo", psin = "debajo"))
  repartos <- Filter(function(z) sum(z == "debajo") == faltan && (z[["gdis"]] == "encima" || f$hay_debajo),
                     repartos)
  stopifnot("F1: reparto de lados posible" = length(repartos) > 0)
  s2c_quiere_psin <- rama == "PROMEDIO" && VEHICULO[["PROMEDIO"]] == "psin" && Q_DELTA_P[["PROMEDIO"]] > 0 &&
    runif(1) < Q_DELTA_P[["PROMEDIO"]]
  s2c_psin_delta <- FALSE; s2c_fallback <- FALSE
  if (s2c_quiere_psin) {
    lado_delta <- if (d$delta < d$D) "debajo" else "encima"
    xs_g <- setdiff(c(if (!f$p_tabla) c(d$aG, d$aP), d$parcialG, d$TG), d$D)
    rep_d <- Filter(function(z) z[["psin"]] == lado_delta &&
                      (if (z[["gdis"]] == "debajo") any(xs_g < d$D) else any(xs_g > d$D)), repartos)
    if (length(rep_d)) { repartos <- rep_d; s2c_psin_delta <- TRUE } else s2c_fallback <- TRUE
  }
  lados <- pick1(repartos); lado_gdis <- lados[["gdis"]]
  ## Cifra falsa del lado de G: tipo de error sorteado entre los aplicables (regla #1).
  cand_g <- data.frame(codigo = c("ERR-DATO", "ERR-DATO", "ERR-RESTA", "ERR-PARCIAL", "ERR-TOTAL"),
                       fuente = c("aG", "aP", "delta", "parcialG", "TG"),
                       x = c(d$aG, d$aP, d$delta, d$parcialG, d$TG), stringsAsFactors = FALSE)
  cand_g <- cand_g[cand_g$x != d$D, ]                               # L-2: la cifra falsa nunca es D
  if (s2c_padv_delta || s2c_psin_delta || !RESTA_G_OK[[rama]]) cand_g <- cand_g[cand_g$codigo != "ERR-RESTA", ]
  ## Canal 3: un solo dato de la tabla entre las opciones. Esto también impide que la cifra de G
  ## repita la de P (canal 8): parcialP solo coincide con un candidato de G cuando es aG o aP.
  if (f$p_tabla) cand_g <- cand_g[cand_g$codigo != "ERR-DATO", ]
  lado_ok <- if (lado_gdis == "debajo") cand_g$x < d$D else cand_g$x > d$D
  stopifnot("canal 2: hay cifra falsa del lado sorteado" = any(lado_ok))
  cand_g <- cand_g[lado_ok, ]
  cod_gdis <- pick(unique(cand_g$codigo))
  fila_g <- cand_g[cand_g$codigo == cod_gdis, ]
  fila_g <- fila_g[sample.int(nrow(fila_g), 1L), ]
  fuente_dato <- if (cod_gdis == "ERR-DATO") fila_g$fuente else NA_character_
  ## Ventaja de P (la única que P sí tiene en la tabla), con su ventaja parcial como cifra.
  cod_padv <- if (rama == "CURSOS") "ERR-PROM" else "ERR-CURSOS"
  molde_padv <- pick(moldes_de("perfil", perfil_P)); fuente_padv <- if (s2c_padv_delta) "delta" else "parcialP"
  ## Razón sin sustento, con una cifra inventada del lado que toca y que no repite ninguna otra
  ## cifra de las opciones ni de la tabla.
  molde_psin <- pick(c("U_a", "U_b", "U_c", "U_d"))
  ## H1 (FASE 2C, ronda 1): tampoco cita una cifra que salga de un cálculo directo con la tabla.
  usadas <- c(d$D, fila_g$x, d$parcialP, f$xP, d$nG, d$nP, d$aG, d$aP, calculos_tabla(d))
  rango_y <- setdiff(if (lados[["psin"]] == "debajo") 2:(d$D - 1) else (d$D + 1):(d$D + 40), usadas)
  stopifnot("F1: hay cifra para la razón sin sustento" = length(rango_y) > 0)
  y <- if (s2c_psin_delta) d$delta else pick(rango_y)
  stopifnot("H1: la cifra de la razón sin sustento no sale de un cálculo con la tabla" =
              s2c_psin_delta || !(y %in% calculos_tabla(d)))
  afs <- list(
    clave = afirmacion(molde_clave, "G", d$D, "CLAVE"),
    gdis  = if (D2_SIN_CIFRA && cod_gdis == "ERR-RESTA")
              modifyList(afirmacion(molde_gdis, "G", NA_real_, cod_gdis), list(afirma = "dif_igual_delta")) else
              afirmacion(molde_gdis, "G", fila_g$x, cod_gdis),
    padv  = afirmacion(molde_padv, "P", f$xP, cod_padv),
    psin  = afirmacion(molde_psin, "P", y, "ERR-INVENTA"))
}

## Regla #13 v1.1 (H2 de la FASE 2C ronda 1, adaptada al cierre de la ronda 3): ninguna lectura que
## el texto admite cambia el veredicto de una opción. La lectura «diferencia» la vigila L-2, más abajo;
## aquí se comparan las otras con ella. La canónica es verbatim (L-1, regla #24 H-2): su clave admite
## «el total fue de 20 kilos», que la vuelve falsa sin volver verdadera ninguna otra opción
## (declarado en el HANDOFF); por eso solo su clave queda exceptuada.
cambia_veredicto <- function(a) {
  alt <- setdiff(lecturas_de(a$texto), "diferencia")
  length(alt) > 0L && any(vapply(alt, function(le) verdad(a, d, le), logical(1)) != verdad(a, d))
}
cambian <- vapply(afs, cambia_veredicto, logical(1))
if (es_canonica) cambian[["clave"]] <- FALSE
stopifnot("regla #13 v1.1: ninguna lectura que admite el texto cambia el veredicto" = !any(cambian))
## En las no canónicas la cifra aparece una sola vez y solo en el complemento del comparativo.
if (!es_canonica) stopifnot("regla #13 v1.1: la cifra va solo en el complemento «en x kilos»" =
  all(vapply(afs, function(a) is.na(a$x) || grepl(paste0(" en ", a$x, ED, "kilos\\.$"), a$texto) &&
                                lengths(regmatches(a$texto, gregexpr("[0-9]+", a$texto))) == 1L, logical(1))))

## --- Enunciado y tabla -------------------------------------------------------
if (es_canonica) {
  enunciado <- CANON_ENUNCIADO
} else {
  enunciado <- paste0(ctx$plantilla(conj$tipo, mat, pick(PROTAGONISTAS)),
                      " La tabla muestra la cantidad de cursos que participaron y el promedio de ", mat,
                      " recogido por curso en ", conj$nombres[1], " y en ", conj$nombres[2], ".")
}
pregunta <- PREGUNTA
n_fila <- if (gi == 1L) c(d$nG, d$nP) else c(d$nP, d$nG)
a_fila <- if (gi == 1L) c(d$aG, d$aP) else c(d$aP, d$aG)
tabla_df <- data.frame(conj$filas, paste(n_fila, "cursos"), paste(a_fila, "kilos"), stringsAsFactors = FALSE)
## L-1 (decisión del profesor tras la FASE 2C): la canónica conserva la rareza del impreso
## («recogido» en el enunciado, «recolectado» en el encabezado); las demás usan un solo verbo.
verbo_tabla <- if (es_canonica) "recolectado" else "recogido"
names(tabla_df) <- c(conj$encab, "Cantidad de cursos", paste("Promedio de", mat, verbo_tabla, "por curso"))
if (!es_canonica) stopifnot("L-1: un solo verbo en el enunciado y en el encabezado" =
                              !grepl("recolect", paste(c(enunciado, names(tabla_df)), collapse = " ")))
tabla_md_df <- tabla_df
tabla_md_df[[1]] <- paste0("**", tabla_md_df[[1]], "**")             # la primera columna va en negrita
tabla_md <- tabla_responsiva(tabla_md_df, align = c("c", "c", "c"))

## --- Guardas (las invariantes del subproyecto, en ejecución) -----------------
textos <- vapply(afs, function(a) a$texto, character(1))   # sin acentos graves: el arsenal 2D corta los chunks en el primero
verdaderas <- vapply(afs, verdad, logical(1), d = d)
stopifnot("L-2: una sola afirmación verdadera, la clave" = sum(verdaderas) == 1L && isTRUE(verdaderas[["clave"]]))
nombra <- ifelse(startsWith(textos, cap1(nomG)), "G", ifelse(startsWith(textos, cap1(nomP)), "P", NA))
stopifnot("L-1: reparto 2 + 2" = identical(unname(nombra), c("G", "G", "P", "P")),
          "L-1: sin vocabulario de error" =
            !any(grepl("estudiante|error|equivoc|confundi|olvid", textos, ignore.case = TRUE)),
          "regla #10: opciones únicas" = length(unique(textos)) == 4L)
cifras <- unlist(lapply(afs, function(a) if (is.na(a$x)) NULL else a$x))
stopifnot("canal 8: cifras distintas entre opciones" = !anyDuplicated(cifras))
## S1: ninguna cifra separada de su unidad por un espacio normal, ni «El Bosque» o «La Pradera» (la
## tabla, que conserva el espacio normal en las cifras, no entra).
stopifnot("S1: espacio duro entre cifra y unidad y en los nombres de dos palabras" =
            !any(grepl("[0-9] (kilos?|cursos?)\\b|El Bosque|La Pradera", c(textos, enunciado), perl = TRUE)))
stopifnot("criterio: gana la mayor cantidad total" =
            grepl(paste0("la mayor cantidad", if (es_canonica) "" else paste0(" de ", mat)), enunciado, fixed = TRUE))
## La pregunta es la del impreso en todas las versiones: 90 y 380886 salen de ITEM_OFICIAL.md.
stopifnot("pregunta del impreso" = nchar(pregunta) == 90L && pond(pregunta) == 380886)
if (es_canonica) {
  ## Contra sumas de control calculadas desde ITEM_OFICIAL.md, no contra las constantes de
  ## las que se copió: compararlas consigo mismas no podría fallar.
  celdas <- paste(c(names(tabla_df), as.vector(t(as.matrix(tabla_df)))), collapse = "|")
  stopifnot("L-1: canónica verbatim (enunciado)" = nchar(enunciado) == 254L && pond(enunciado) == 2996052,
            "L-1: canónica verbatim (tabla)" = nchar(celdas) == 119L && pond(celdas) == 695221,
            "L-1: canónica verbatim (opciones)" =
              all(vapply(textos, nchar, 0L) == c(126L, 88L, 105L, 156L)) &&
              all(vapply(gsub(ED, " ", textos, fixed = TRUE), pond, 0) == c(750351, 373876, 532900, 1156926)),
            ## S1 (H-2): la canónica conserva las palabras del impreso y solo cambia el espacio entre la
            ## cifra y «kilos», tres veces; la guarda de arriba lo compara sin él.
            "S1: en la canónica, el espacio duro solo entre cifra y unidad" =
              sum(lengths(regmatches(textos, gregexpr(ED, textos, fixed = TRUE)))) == 3L &&
              !any(grepl(paste0("[^0-9]", ED), textos)))
}
params <- c(d, list(nomG = nomG, nomP = nomP, tipo = conj$tipo, mat = mat, fuente_dato = fuente_dato,
                    fuente_padv = fuente_padv, y = if (es_canonica) NA_real_ else y,
                    s2c_psin_delta = if (es_canonica) FALSE else s2c_psin_delta))
sel <- errores_conceptuales[c(afs$gdis$codigo, afs$padv$codigo, afs$psin$codigo)]
stopifnot("regla #1: precondiciones de los errores elegidos" =
            all(vapply(sel, function(e) isTRUE(e$precondicion(params)), logical(1))),
          "regla #1: calcula() reproduce la cifra de cada distractor" =
            identical(unname(vapply(sel, function(e) as.numeric(e$calcula(params)), 0)),
                      as.numeric(c(if (is.na(afs$gdis$x)) params$delta else afs$gdis$x, afs$padv$x, afs$psin$x))))

## --- Mezcla interna (R/exams vuelve a mezclar con exshuffle: TRUE) -----------
o <- sample(4L)
opciones <- unname(textos[o])
sol <- unname(verdaderas[o])
rol_opcion <- names(afs)[o]
respuesta_correcta <- opciones[sol]
fb <- c(
  clave = paste0("Verdadero. ", cap1(nomG), " recogió $", d$nG, " \\times ", d$aG, " = ", d$TG, "\\ \\text{kilos}$ y ",
                 nomP, ", $", d$nP, " \\times ", d$aP, " = ", d$TP, "\\ \\text{kilos:}$ ganó por $", d$TG, " - ", d$TP, " = ",
                 d$D, "\\ \\text{kilos.}$"),
  gdis  = paste0("Falso. ", cap1(nomG), " sí ganó, pero por ", d$D, ED, "kilos, no por ", if (is.na(afs$gdis$x)) paste0("lo que se diferencian los promedios, ",
                 d$delta, ED, "kilos") else afs$gdis$x, "."),
  padv  = paste0("Falso. ", cap1(nomP), " recogió ", d$TP, ED, "kilos, menos que los ", d$TG, " de ", nomG, ": ",
                 ventaja_txt(d), " no le bastó."),
  psin  = paste0("Falso. ", cap1(nomP), " no ganó (", d$TP, ED, "kilos frente a ", d$TG, "), y la razón que da no ",
                 "sale de la tabla, que solo informa la cantidad de cursos y el promedio por curso de cada ",
                 conj$tipo, "."))
feedback <- unname(fb[rol_opcion])
## L-6 (tanda S1b): ninguna fórmula del feedback ni del análisis de los distractores cierra en «$» seguido de la
## unidad; la unidad va dentro de la fórmula. explica() es pura (regla #1), así que llamarla aquí no mueve el RNG.
stopifnot("L-6: la unidad que cierra una fórmula va dentro de la fórmula" =
            !any(grepl(paste0("\\$[ ", ED, "]+(kilos?|cursos?)\\b"),
                       c(fb, vapply(c("gdis", "padv", "psin"), function(r)
                         errores_conceptuales[[afs[[r]]$codigo]]$explica(params, afs[[r]]), "")), perl = TRUE)))
## L-6 (tanda S1b relanzada, V1'): el signo que sigue a la unidad de una fórmula va dentro de ella, «\\text{kilos.}$».
## Tanda S2a (2026-11-03, decisión del profesor «Cerrarlo ahora»): también el de toda fórmula sin unidad, «= 5\\text{.}$»
## (ERR-RESTA), y con cualquier carácter que no sea letra ni cifra entre el cierre de la fórmula y el signo.
stopifnot("L-6: el signo que sigue a una fórmula, con unidad o sin ella, va dentro de la fórmula" =
            !any(grepl("\\$[^A-Za-z0-9$\\n]{0,3}[.,:;!?)\\]\u00bb\u201d\u2026]",
                       c(fb, vapply(c("gdis", "padv", "psin"), function(r)
                         errores_conceptuales[[afs[[r]]$codigo]]$explica(params, afs[[r]]), "")), perl = TRUE)))

reflexiones <- c(
  "Antes de comparar, pregúntate qué mide cada número: un promedio describe un curso típico; un total, el grupo completo.",
  "Cuando cada grupo tiene una ventaja distinta, no decidas por la que más llama la atención: calcula cuánto aporta cada una.",
  "Una afirmación puede apoyarse en un dato cierto y llegar a una conclusión falsa: revisa las dos partes.",
  "Si una afirmación cita una cifra, compruébala con tus cálculos antes de aceptarla.",
  "Si la tabla no trae un dato, ninguna afirmación puede justificarse con él.")
reflexion <- pick(reflexiones)
