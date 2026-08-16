#!/usr/bin/env Rscript
# =============================================================================
# auditoria_propia.R — verificador del ejercicio
#   pendiente_rectas_paralelas_..._n4_schoice_v1
#
# Cubre lo que el arsenal compartido NO puede cubrir aqui:
#   (A) CORRECTITUD EXHAUSTIVA sobre el espacio completo (p/q, H, t), no muestreada.
#   (B) Unicidad de la clave: ningun distractor alcanza la pendiente objetivo,
#       para TODA combinacion de error, k04 y t admisible.
#   (C) Diagnosticidad por CONTENIDO (longitud, firma sin digitos).
#   (D) §P4 regla #22: predictibilidad POSICIONAL de la clave (cuadrante).
#   (E) Variedad del pool de errores (Error 27 / Incidente N).
#   (F) Pase ESTRATIFICADO por pendiente objetivo.
#   (G) Instancia canonica por igualdad exacta.
#   (H) Texto emitido: contracciones y dobles espacios.
#   (I) GEOMETRIA RELATIVA A H  <-- sonda anadida tras la FASE 2C.
#       Ninguna sonda del arsenal (H1 longitud, H2 primera palabra, H3/H3b
#       veredicto) ni de (C)/(D) usa H. Con pendiente ENTERA la clave estaba
#       siempre a |dx| in {1,2} de H y los distractores llegaban a 10: eliminar
#       "las lejanas en x" resolvia el item sin calcular pendiente alguna
#       (25 % -> 44-59 %). (I) mide el rango de |dx| de la clave y una bateria
#       de reglas de eliminacion puramente geometricas, incluida la RAZON
#       |dy| vs |dx|, que era el canal ciego de la 2a pasada de la FASE 2C.
#   (J) CANAL SEMANTICO DEL SIGNO  <-- sonda anadida en la 2a pasada FASE 2C.
#       (I) es puramente geometrica (|dx|, |dy|, distancias: magnitudes SIN
#       signo) y el arsenal mira longitud, primera palabra y veredicto: nadie
#       miraba el SIGNO de la pendiente de cada opcion.
#   (L) REGLAS CONDICIONADAS POR |m|  <-- sonda anadida en la 4a pasada FASE 2C.
#       (I) mide reglas INCONDICIONALES y (K) sus conjunciones de dos. Ninguna
#       podia ver el atajo real, porque el estudiante LEE la pendiente pedida en
#       el enunciado y puede CONDICIONAR la regla a ella: "quedarse con las
#       opciones cuyo |dy| esta del mismo lado de 1 que |m|" cambia de sentido
#       segun |m| y por eso no es ninguna regla de (I) ni conjuncion de dos.
#       Medida sobre el pool de 7 anterior: SIGNO+BUCKET 73,4 % (azar 25 %).
#
# Muestra estandar N = 100 (regla #23). El pase estratificado amplia la muestra
# —excepcion DECLARADA de la regla #23— hasta que todo estrato alcance n >= 20.
# =============================================================================

N_STD       <- 100L
MIN_ESTRATO <- 20L
N_STRAT     <- 300L
RMD <- "pendiente_rectas_paralelas_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd"

# --- CORTES DEL EXCESO: UNA sola fuente de verdad (2026-08-15) ----------------
# Este archivo y el helper §P7 juzgaban el MISMO artefacto con umbrales distintos
# (45 % aqui, 70 % alli) y daban veredictos opuestos. La medicion sobre 468 items
# oficiales mostro por que: la tasa absoluta crece con el numero de reglas, asi
# que un umbral absoluto mide, en parte, el tamano de la bateria. Lo comparable es
# el EXCESO sobre el techo nulo, y sus cortes se leen del helper para que no puedan
# volver a divergir. Ver `.claude/rules/diversidad-sustantiva.md` §P7.
REPO_ROOT <- tryCatch(suppressWarnings(system("git rev-parse --show-toplevel",
                                              intern = TRUE, ignore.stderr = TRUE)),
                      error = function(e) character(0))
# Candidatas en orden: raiz del repo y los dos saltos relativos conocidos. Si
# ninguna existe se ABORTA: duplicar los cortes aqui es justo la divergencia que
# este cambio cerro, asi que un fallback silencioso seria peor que no correr.
CANDIDATAS <- c(if (length(REPO_ROOT) == 1L && nzchar(REPO_ROOT))
                  file.path(REPO_ROOT, ".claude/scripts/bateria_eliminacion.R"),
                "../../../.claude/scripts/bateria_eliminacion.R",
                ".claude/scripts/bateria_eliminacion.R")
HELPER_P7 <- CANDIDATAS[file.exists(CANDIDATAS)][1]
if (is.na(HELPER_P7))
  stop("No se encontro el helper §P7 en ninguna de: ",
       paste(CANDIDATAS, collapse = " | "), ". Los cortes del exceso NO pueden ",
       "duplicarse aqui: es la divergencia que se cerro el 2026-08-15.")
source(HELPER_P7)
# Replicas del nulo en los bloques que enumeran decenas de miles de conjunciones.
K_PERM <- as.integer(Sys.getenv("R_K4_PERM", "8"))

# =============================================================================
# RESIDUO ACEPTADO — decision del profesor (2026-08-16), tras la 4a pasada de
# endurecimiento y una medicion sobre 468 items oficiales del ICFES.
#
# NO ES UN INTERRUPTOR DEL GATE. Fija la MEDICION VIGENTE del 2026-08-15
# (commit 56849d565) como LINEA BASE, con una TOLERANCIA de ruido, y el gate
# SIGUE VIVO contra esa linea base: una corrida futura que mida POR ENCIMA de
# linea_base + tolerancia vuelve a fallar. No se toca ningun umbral compartido
# (CORTE_CANAL/CORTE_RUIDO siguen siendo los del helper §P7); lo que cambia es
# contra QUE se compara el exceso una vez que YA cruzo ese corte.
#
# MOTIVO (no "se deja pasar"). Sobre 468 items oficiales, en TODO lo comparable
# el ejercicio esta EN LINEA con el corpus: vara universal, oficiales +0,4 pp
# vs este item -1,1 pp; vara de valor, oficiales -2,1 pp vs este item +0,9 pp —
# las cuatro cifras son ruido alrededor del techo nulo (ver cabecera del helper
# §P7). El residuo del 67 % (K4 cruzado) NO TIENE CONTRAPARTE MEDIBLE: la regla
# que lo produce exige la estructura "punto H + pendiente p/q", y de 468 items
# oficiales EXISTE EXACTAMENTE UNO con esa estructura — no es que los oficiales
# pasen la prueba, es que la prueba no se les puede aplicar. Es una propiedad
# ESTRUCTURAL de los bancos parametricos (cien versiones del mismo item tienen
# regularidades que un item unico, visto una sola vez en un examen real, no
# tiene), no un defecto propio de este item. El ejercicio MEJORO en cada pasada
# (maximo 66,3 % -> 47,0 %, exceso +31,0 -> +13,4 pp) y NUNCA empeoro; lo que
# crecio fue la bateria, de 19 a 91 reglas.
#
# Lo que sigue verificado y NO entra en este residuo: correctitud 12.320/12.320
# ((A)+(B) arriba), unicidad sin colisiones, 5 formatos en R limpio, arsenal
# compartido N=100 en verde, multisemilla 100/100, literalidad ICFES caracter a
# caracter, instancia canonica verbatim ((G) abajo). Un fallo en cualquiera de
# esos bloques SIGUE bloqueando: este residuo cubre unicamente las 4 cifras de
# diagnosticidad por CONJUNCION que se citan abajo, medidas y no recalculadas.
#
# TOLERANCIA = +3 pp sobre cada exceso medido (~1,4 sd del ruido a N=100,
# sd=2,2 pp segun la calibracion del helper §P7 sobre 500 replicas de H0).
# La MISMA constante se declara en `bateria_p7.R`, que corre en un proceso R
# separado y no comparte estado con este archivo: es la unica forma de que las
# dos corridas apliquen la misma linea base sin volver a duplicar los cortes
# compartidos (CORTE_CANAL/CORTE_RUIDO SI siguen viniendo del helper §P7,
# fuente unica; solo la linea base de ESTE residuo se repite a proposito).
# =============================================================================
FECHA_RESIDUO <- "2026-08-16"
TOL_RESIDUO   <- 0.03
RESIDUO_ACEPTADO <- list(
  p7_atomico  = list(max = 0.470, techo = 0.336, exceso = 0.134),  # bateria_p7.R
  l_celda     = list(max = 0.470, techo = 0.328, exceso = 0.142),  # bloque (L)
  k3_triplete = list(max = 0.617, techo = 0.358, exceso = 0.259),  # bloque (K3)
  k4_cruzado  = list(max = 0.670, techo = 0.363, exceso = 0.308),  # bloque (K4) GATE 1
  k4_marginal = list(marginal = 0.218),                            # bloque (K4) GATE 2
  # (K) pares simples. Anadida el 2026-08-16 por decision explicita del profesor.
  # POR QUE TIENE ENTRADA PROPIA Y NO SE DEDUCE DE (K3): es cierto que (K) esta
  # dominado por (K3) POR CONSTRUCCION -- ambos buscan sobre `reglas_k`, y anadir
  # una tercera regla a una conjuncion solo puede restringir mas el conjunto
  # superviviente, asi que max(tripletes) >= max(pares) siempre (medido: 61,7 %
  # vs 51,0 %; +25,9 pp vs +17,0 pp). Pero deducirlo en cascada dejaba a (K) sin
  # vigilancia propia: si algun dia (K3) baja, (K) podria quedar por encima sin
  # que nada lo notara, porque su unica referencia era la linea base ajena.
  # Con entrada propia, (K) se compara consigo mismo y vuelve a bloquear si
  # empeora, aunque (K3) siga igual.
  k_pares     = list(max = 0.510, techo = 0.343, exceso = 0.170)    # bloque (K)
)
# Devuelve TRUE (residuo aceptado, no bloquea) o FALSE (excede la linea base:
# SIGUE bloqueando). Imprime SIEMPRE la comparacion, se acepte o no, para que
# la cifra jamas quede oculta detras de un veredicto silencioso.
residuo_ok <- function(clave, campo, valor_actual, etiqueta) {
  base_valor <- RESIDUO_ACEPTADO[[clave]][[campo]]
  tope <- base_valor + TOL_RESIDUO
  cat(sprintf("    [RESIDUO ACEPTADO %s | %s.%s] linea base %+.1f pp + tolerancia %.1f pp -> tope %+.1f pp | medido ahora %+.1f pp\n",
              FECHA_RESIDUO, clave, campo, 100 * base_valor, 100 * TOL_RESIDUO, 100 * tope, 100 * valor_actual))
  ok <- valor_actual <= tope
  cat(sprintf("    -> %s: %s\n", etiqueta,
              if (ok) "dentro de linea base + tolerancia -> RESIDUO ACEPTADO, no bloquea"
              else "POR ENCIMA de linea base + tolerancia -> EXCEDE lo aceptado, SIGUE bloqueando"))
  ok
}

errores <- character(0); avisos <- character(0)
fail <- function(...) errores <<- c(errores, paste0(...))
warn <- function(...) avisos  <<- c(avisos,  paste0(...))

stopifnot(file.exists(RMD))
rmd <- readLines(RMD, warn = FALSE)
i0  <- grep("^```\\{r data_generation", rmd)[1]
stopifnot(!is.na(i0))
i1  <- i0 + which(grepl("^```[[:space:]]*$", rmd[(i0 + 1):length(rmd)]))[1]
f_dg <- tempfile(fileext = ".R"); writeLines(rmd[(i0 + 1):(i1 - 1)], f_dg)

correr <- function(semilla) { set.seed(semilla); e <- new.env(); sys.source(f_dg, envir = e); e }

cat("=============================================================\n")
cat(" AUDITORIA PROPIA — pendiente_rectas_paralelas_n4_schoice_v1\n")
cat("=============================================================\n\n")

e0 <- correr(1L)
simplificar <- e0$simplificar
errores_conceptuales <- e0$errores_conceptuales
PEND_POOL <- e0$PEND_POOL
CAND_T    <- e0$CAND_T
LIM       <- e0$LIM_COORD
# Tope del soporte de t, LEIDO del .Rmd. Alimenta la regla "|dx| en q*(1..T)" de
# (K) para que la sonda siga al artefacto en vez de quedarse clavada en 3.
TMAX_T    <- max(abs(e0$CAND_T_CLAVE))
stopifnot(setequal(abs(e0$CAND_T), abs(e0$CAND_T_CLAVE)))  # invariante de soporte
HX <- -3L:3L; HY <- -4L:4L

# =============================================================================
# (A)+(B) CORRECTITUD Y UNICIDAD — ENUMERACION EXHAUSTIVA
# =============================================================================
n_clave <- 0L; n_clave_ok <- 0L; n_dist <- 0L; n_col <- 0L
# n_dx0 hace VERIFICABLE la afirmacion del .Rmd de que la guarda `d[1] == 0` es
# estructuralmente inalcanzable. Antes esta enumeracion descartaba esos casos en
# silencio con un `next`, asi que NO acreditaba nada sobre ellos.
n_cand <- 0L; n_dx0 <- 0L
# n_apl hace VERIFICABLE la otra guarda que el .Rmd declara inalcanzable:
# `if (length(idx_aplicables) < 3L) next`. Se registra el minimo y el maximo del
# numero de errores aplicables sobre TODO el espacio (p/q, hy), no sobre una
# muestra: si el minimo es >= 3, la guarda no puede disparar nunca.
apl_min <- .Machine$integer.max; apl_max <- 0L
for (pq in PEND_POOL) { p_m <- pq[1]; q_m <- pq[2]; obj <- simplificar(p_m, q_m)
  for (hx in HX) for (hy in HY) {
    for (t in CAND_T) {
      kx <- hx + q_m * t; ky <- hy + p_m * t
      if (max(abs(c(kx, ky))) > LIM) next
      n_clave <- n_clave + 1L
      if (all(simplificar(ky - hy, kx - hx) == obj)) n_clave_ok <- n_clave_ok + 1L
      else fail("CLAVE INCORRECTA: p/q=", p_m, "/", q_m, " H=(", hx, ",", hy, ") t=", t)
    }
    params <- list(p = p_m, q = q_m, hy = hy)
    n_apl <- sum(vapply(errores_conceptuales,
                        function(e) isTRUE(e$precondicion(params)), logical(1)))
    apl_min <- min(apl_min, n_apl); apl_max <- max(apl_max, n_apl)
    for (k04 in c(2L, 3L, 5L)) for (err in errores_conceptuales) {
      if (!isTRUE(err$precondicion(params))) next
      for (t in CAND_T) {
        d <- err$calcula(hx, hy, p_m, q_m, t, k04)
        n_cand <- n_cand + 1L
        if (d[1] == 0) { n_dx0 <- n_dx0 + 1L; next }
        px <- hx + d[1]; py <- hy + d[2]
        if (max(abs(c(px, py))) > LIM) next
        n_dist <- n_dist + 1L
        if (all(simplificar(d[2], d[1]) == obj)) { n_col <- n_col + 1L
          fail("DISTRACTOR IGUALA LA CLAVE: ", err$codigo, " p/q=", p_m, "/", q_m,
               " H=(", hx, ",", hy, ") t=", t, " k04=", k04) }
      }
    }
  }
}
cat("(A) Correctitud de la clave — espacio COMPLETO (p/q, H, t):\n")
cat("    casos enumerados : ", n_clave, "\n")
cat("    pendiente == p/q : ", n_clave_ok, sprintf(" (%.1f %%)\n", 100 * n_clave_ok / n_clave))
cat("(B) Unicidad — TODO error x k04 x t admisible:\n")
cat("    candidatos enumerados     : ", n_dist, "\n")
cat("    que igualan la pendiente  : ", n_col, "\n")
cat("    (dx = 0) guarda del .Rmd  : ", n_dx0, " de ", n_cand,
    " -> la guarda es INALCANZABLE, no una defensa activa\n", sep = "")
if (n_dx0 > 0L) fail("La guarda dx==0 SI se alcanza (", n_dx0, " casos): el .Rmd la declara inalcanzable")
cat("    errores aplicables por (p/q, hy): min ", apl_min, ", max ", apl_max,
    " -> la guarda `length(idx_aplicables) < 3` es INALCANZABLE\n\n", sep = "")
if (apl_min < 3L) fail("La guarda idx_aplicables<3 SI se alcanza (min = ", apl_min, ")")

# =============================================================================
# (N) EL TEXTO REPRODUCE EL PUNTO EMITIDO — anadido en la 5a pasada FASE 2C.
#     A-2: `descripcion_corta` de FUN-PEN-09 decia "descuenta una unidad al
#     avance vertical y otra al horizontal", que describe (q-1, |p|-1) y NO el
#     (2q-1, 2|p|-1) que el codigo emite. Los dos solo coincidirian si |p| = q,
#     que PEND_POOL excluye: la descripcion no reproducia el punto en NINGUNA
#     version, y con q = 1 la operacion descrita divide por cero. FUN-PEN-08
#     tenia el mismo defecto para m < 0 ("suma al numerador" cuando lo que suma
#     es la MAGNITUD).
#
#     NADA lo detectaba. El arsenal valida que el distractor sea distinto de la
#     clave (unicidad) y que el texto compile y lleve tildes, pero NADIE
#     comprobaba que la prosa describa la operacion que produjo el punto. Es el
#     gemelo semantico del Incidente O: un campo que SI se emite pero que ningun
#     check confronta con el codigo.
#
#     COMO SE EVITA LA CIRCULARIDAD: la pendiente esperada se re-deriva AQUI a
#     partir de lo que dice el TEXTO, no llamando a `calcula()`. Si alguien
#     cambia la formula sin tocar la prosa (o al reves), las dos derivaciones
#     divergen y este bloque falla nombrando el codigo.
# =============================================================================
sg <- function(z) if (z > 0) 1L else if (z < 0) -1L else 0L
# Pendiente que AFIRMA la prosa de cada error, escrita desde el texto:
pend_del_texto <- list(
  # "Divide la diferencia de abscisas entre la de ordenadas" -> reciproco q/p
  "FUN-PEN-01" = function(p, q, k) c(q, p),
  # "valor absoluto correcto pero con el signo contrario"    -> -p/q
  "FUN-PEN-02" = function(p, q, k) c(-p, q),
  # "reciproco cambiado de signo"                            -> -q/p
  "FUN-PEN-03" = function(p, q, k) c(-q, p),
  # "suma una unidad al avance vertical y otra al horizontal"
  "FUN-PEN-08" = function(p, q, k) c(sg(p) * (abs(p) + 1L), q + 1L),
  # "duplica los dos avances y luego descuenta una unidad a cada uno"
  "FUN-PEN-09" = function(p, q, k) c(sg(p) * (2L * abs(p) - 1L), 2L * q - 1L),
  # "cuenta un cuadro de mas al medir el MENOR de los dos avances": con |m| > 1
  # el menor es el horizontal, con |m| < 1 el vertical. La prosa fija la
  # pendiente exacta en cada rama, asi que se comprueba como valor, no como
  # propiedad debil.
  "FUN-PEN-10" = function(p, q, k)
    if (abs(p) > q) c(p, q + 1L) else c(sg(p) * (abs(p) + 1L), q)
)
# Errores cuya prosa no fija un valor sino una PROPIEDAD: se comprueba esa.
prop_del_texto <- list(
  # "acepta cualquier pendiente del mismo signo sin comprobar el valor"
  "FUN-PEN-04" = function(fr, p, q) sg(fr[1]) == sg(p) && !all(fr == simplificar(p, q)),
  # "...que aqui ni siquiera conserva el signo"
  "FUN-PEN-05" = function(fr, p, q) sg(fr[1]) == -sg(p),
  # "avanza lo mismo en horizontal que en vertical -> pendiente 1"
  "FUN-PEN-06" = function(fr, p, q) all(fr == c(1L, 1L))
)
n_txt <- 0L; malos <- character(0)
for (pq in PEND_POOL) { p_m <- pq[1]; q_m <- pq[2]
  for (hy in HY) { params <- list(p = p_m, q = q_m, hy = hy)
    for (k04 in c(2L, 3L, 5L)) for (err in errores_conceptuales) {
      if (!isTRUE(err$precondicion(params))) next
      for (t in CAND_T) {
        d <- err$calcula(0L, hy, p_m, q_m, t, k04)
        if (d[1] == 0) next
        fr <- simplificar(d[2], d[1])
        cd <- err$codigo
        if (!is.null(pend_del_texto[[cd]])) {
          n_txt <- n_txt + 1L
          esp <- do.call(simplificar, as.list(pend_del_texto[[cd]](p_m, q_m, k04)))
          if (!all(fr == esp)) malos <- union(malos, cd)
        } else if (!is.null(prop_del_texto[[cd]])) {
          n_txt <- n_txt + 1L
          if (!isTRUE(prop_del_texto[[cd]](fr, p_m, q_m))) malos <- union(malos, cd)
        }
      }
    }
  }
}
# FUN-PEN-07 no afirma una pendiente sino una IGUALDAD sobre el punto: dice que
# el estudiante ajusta K hasta que (y2 + y1)/(x2 - x1) coincide con la pendiente
# pedida. Se comprueba esa igualdad, que es lo que la prosa promete.
n_07 <- 0L; mal_07 <- 0L
e07 <- errores_conceptuales[[which(vapply(errores_conceptuales,
        function(e) e$codigo == "FUN-PEN-07", logical(1)))]]
for (pq in PEND_POOL) { p_m <- pq[1]; q_m <- pq[2]
  for (hx in HX) for (hy in HY) { if (hy == 0) next
    for (t in CAND_T) {
      d <- e07$calcula(hx, hy, p_m, q_m, t, 2L); if (d[1] == 0) next
      px <- hx + d[1]; py <- hy + d[2]; n_07 <- n_07 + 1L
      if (!all(simplificar(py + hy, px - hx) == simplificar(p_m, q_m))) mal_07 <- mal_07 + 1L
    }
  }
}
cat("(N) La prosa de cada error reproduce la PENDIENTE del punto emitido (A-2):\n")
cat("    El titulo dice PENDIENTE y no PUNTO a proposito: el codigo escala por\n")
cat(sprintf("    t en +-1..+-%d y la prosa no menciona escala, asi que lo que debe\n", TMAX_T))
cat("    coincidir es la RAZON entre los avances, que es lo que la Solution\n")
cat("    imprime reducida. Declararlo evita leer mas de lo que se mide.\n")
cat(sprintf("    casos enumerados (%d de %d errores) : %d\n",
            length(pend_del_texto) + length(prop_del_texto),
            length(errores_conceptuales), n_txt))
cat("    errores cuya prosa NO lo reproduce: ",
    if (length(malos)) paste(sort(malos), collapse = ", ") else "ninguno", "\n")
cat("    FUN-PEN-07 (y2+y1)/(x2-x1) == p/q : ", n_07 - mal_07, " de ", n_07, "\n", sep = "")

# --- COBERTURA POR CAMPO Y POR ERROR (la ceguera se declara, regla #22 v1.4) --
# Un "ninguno" de arriba significa "los campos que miro estan bien", no "la
# prosa esta bien". Se declara con que criterio se comprobo cada error y que
# campos quedan SIN medir, para que nadie lea el verde como cobertura total.
cat("    cobertura, error por error:\n")
for (err in errores_conceptuales) {
  cd <- err$codigo
  crit <- if (!is.null(pend_del_texto[[cd]])) "pendiente exacta derivada del texto"
          else if (!is.null(prop_del_texto[[cd]])) "solo PROPIEDAD debil (signo / valor 1)"
          else if (cd == "FUN-PEN-07") "identidad (y2+y1)/(x2-x1) == p/q"
          else "NO MEDIDO"
  cat(sprintf("      %-12s : %s\n", cd, crit))
}
cat("    campos cubiertos : nombre, descripcion_corta, descripcion_larga\n")
cat("      (los tres afirman la MISMA operacion aritmetica y los tres SE EMITEN)\n")
cat("    campos NO medidos: causa_raiz (es una hipotesis pedagogica sobre el\n")
cat("      origen del error, no una afirmacion aritmetica falsable)\n")

# --- REGRESION LITERAL sobre `nombre` (O-1 de la FASE 2C) --------------------
# `nombre` SE EMITE como titular del distractor. A-2 corrigio las dos
# `descripcion_*` y dejo `nombre` con la MISMA prosa falsada, viva en el 73,5 %
# de las versiones; el bloque (N) no lo veia porque solo derivaba de los campos
# `descripcion_*`. Aqui se prohibe literalmente la formulacion falsada.
prohibidas <- c("lo mismo arriba y abajo", "al numerador y al denominador")
nombres_pool <- vapply(errores_conceptuales, function(e) e$nombre, character(1))
textos_pool  <- c(nombres_pool,
                  vapply(errores_conceptuales, function(e) e$descripcion_corta, character(1)))
hits <- unlist(lapply(prohibidas, function(pp) textos_pool[grepl(pp, textos_pool, fixed = TRUE)]))
cat("    regresion literal en `nombre`/`descripcion_corta` : ",
    if (length(hits)) paste(hits, collapse = " | ") else "ninguna formulacion prohibida", "\n")
if (length(hits)) fail("Prosa FALSADA por A-2 sigue viva en el pool: ", paste(hits, collapse = " | "))
# Control positivo del detector literal.
if (!any(grepl(prohibidas[1], c("Cree que sumar lo mismo arriba y abajo conserva la razon"), fixed = TRUE)))
  fail("CONTROL POSITIVO ROTO: el detector literal de prosa falsada no dispara")
cat("    control positivo del detector literal: dispara sobre la prosa vieja -> vivo\n")
if (length(malos)) fail("La prosa NO reproduce el punto emitido en: ", paste(sort(malos), collapse = ", "))
if (mal_07 > 0L) fail("FUN-PEN-07: la igualdad que promete su prosa falla en ", mal_07, " casos")
# CONTROL POSITIVO: se codifica la prosa ANTERIOR de FUN-PEN-09 —"descuenta una
# unidad al avance vertical y otra al horizontal" = (q-1, |p|-1)— y se exige que
# ESTA sonda la habria rechazado. Sin este control, un "ninguno" no distingue
# "la prosa es fiel" de "la sonda no mira" (Incidente P / INC-MUTANTE-SONDA).
prosa_vieja_09 <- function(p, q) c(sg(p) * (abs(p) - 1L), q - 1L)
caza <- 0L; casos_v <- 0L
e09 <- errores_conceptuales[[which(vapply(errores_conceptuales,
        function(e) e$codigo == "FUN-PEN-09", logical(1)))]]
for (pq in PEND_POOL) { p_m <- pq[1]; q_m <- pq[2]
  if (q_m - 1L == 0L) next          # la prosa vieja divide por cero con q = 1
  d <- e09$calcula(0L, 0L, p_m, q_m, 1L, 2L)
  casos_v <- casos_v + 1L
  if (!all(simplificar(d[2], d[1]) ==
           do.call(simplificar, as.list(prosa_vieja_09(p_m, q_m))))) caza <- caza + 1L
}
if (caza != casos_v)
  fail("CONTROL POSITIVO ROTO: (N) no habria cazado la prosa ANTERIOR de FUN-PEN-09 (",
       caza, " de ", casos_v, ")")
cat("    control positivo: la prosa ANTERIOR de FUN-PEN-09 se rechaza en ", caza, " de ",
    casos_v, " pendientes con q>1 (y con q=1 divide por cero) -> sonda viva\n\n", sep = "")

# =============================================================================
# MEDICION SOBRE N = 100 (regla #23)
# =============================================================================
recolectar <- function(semillas) lapply(semillas, function(sm) {
  e <- correr(sm); j <- which(e$sol == 1L); pc <- e$puntos[[j]]
  dx <- vapply(e$puntos, function(z) abs(z[1] - e$hx), numeric(1))
  dy <- vapply(e$puntos, function(z) abs(z[2] - e$hy), numeric(1))
  dd <- sqrt(dx^2 + dy^2)
  L  <- nchar(e$opciones)
  list(p = e$p_m, q = e$q_m, hy = e$hy, canon = e$es_canonica,
       opciones = e$opciones, sol = e$sol, codigos = e$codigos, j = j,
       kx = pc[1], ky = pc[2], dx = dx, dy = dy, dd = dd, L = L,
       sg = vapply(e$pend_frac, function(z) sign(z[1]), numeric(1)),
       sg_obj = sign(e$p_m), hx = e$hx,
       # |m| de cada opcion y de la clave: sostienen las reglas CONDICIONADAS
       # por |m| del bloque (L), que ni (I) ni (K) pueden expresar.
       mab = vapply(e$pend_frac, function(z) abs(z[1]) / z[2], numeric(1)),
       mobj = abs(e$p_m) / e$q_m,
       intento = if (is.null(e$intento)) NA_integer_ else as.integer(e$intento),
       ptx = vapply(e$puntos, function(z) z[1], numeric(1)),
       pty = vapply(e$puntos, function(z) z[2], numeric(1)),
       mas_larga = (L[j] == max(L) && sum(L == max(L)) == 1L),
       mas_corta = (L[j] == min(L) && sum(L == min(L)) == 1L),
       firma = length(unique(gsub("[0-9]", "", e$opciones))),
       texto = c(e$enunciado_intro, e$frase_borrado))
})
reg <- recolectar(seq_len(N_STD) * 7919L)

# POLITICA DE CANONICAS (D-5, 2026-08-15b). La instancia canonica reproduce el
# item oficial verbatim y NO es corregible; pesa 1/12 y arrastra los agregados.
# El bloque (F) ya aplicaba sus checks BLOQUEANTES al subconjunto SIN canonicas,
# que es lo unico corregible, mientras (I)/(J)/(K)/(L) los aplicaban al agregado
# CON canonicas: dos politicas distintas en el mismo archivo. Se ALINEA a la de
# (F) — se PUBLICAN siempre las dos columnas y se BLOQUEA por la de sin canonicas.
reg_nc <- reg[!vapply(reg, function(r) isTRUE(r$canon), logical(1))]
stopifnot(length(reg_nc) > 0L)
N_NC <- length(reg_nc)

if (any(vapply(reg, function(r) sum(r$sol), integer(1)) != 1L)) fail("Version sin exactamente 1 correcta")
if (any(vapply(reg, function(r) length(unique(r$opciones)), integer(1)) != 4L)) fail("Version con opciones duplicadas")

cat("(C) Diagnosticidad por contenido (N = ", N_STD, "):\n", sep = "")
pl <- mean(vapply(reg, function(r) r$mas_larga, logical(1)))
pc <- mean(vapply(reg, function(r) r$mas_corta, logical(1)))
cat(sprintf("    clave UNICA mas larga : %5.1f %%\n", 100 * pl))
cat(sprintf("    clave UNICA mas corta : %5.1f %%\n", 100 * pc))
cat("    firmas distintas tras quitar digitos: ",
    paste(sort(unique(vapply(reg, function(r) r$firma, integer(1)))), collapse = ", "), "\n")
if (pl >= 0.90 || pc >= 0.90) fail("Clave identificable por longitud en >=90 %")
if (pl >= 0.70 || pc >= 0.70) warn("Clave identificable por longitud en >=70 %")

cat("\n(D) Posicion de la clave en el plano (regla #22 §P4):\n")
cuad <- vapply(reg, function(r) {
  if (r$kx == 0 || r$ky == 0) "eje"
  else if (r$kx > 0 && r$ky > 0) "I" else if (r$kx < 0 && r$ky > 0) "II"
  else if (r$kx < 0 && r$ky < 0) "III" else "IV" }, character(1))
tc <- table(cuad)
for (nm in names(tc)) cat(sprintf("    %-4s : %3d  (%.1f %%)\n", nm, tc[[nm]], 100 * tc[[nm]] / N_STD))
if (max(tc) / N_STD >= 0.90) fail("Clave siempre en la misma region (>=90 %)")
if (max(tc) / N_STD >= 0.70) warn("Clave concentrada en una region (>=70 %)")

# =============================================================================
# (I) GEOMETRIA RELATIVA A H — sonda anadida tras la FASE 2C
# =============================================================================
cat("\n(I) Geometria relativa a H (canal que NINGUNA sonda del arsenal mide):\n")
rango_dx <- vapply(reg, function(r) as.integer(rank(r$dx, ties.method = "min")[r$j]), integer(1))
tr <- table(factor(rango_dx, levels = 1:4))
cat("    rango de |dx| de la CLAVE entre las 4 opciones (uniforme = 25 % cada uno):\n")
for (k in 1:4) cat(sprintf("      rango %d : %3d  (%.1f %%)\n", k, tr[[k]], 100 * tr[[k]] / N_STD))
if (tr[[4]] == 0L) fail("La clave NUNCA es la mas lejana de H en x: descartarla es gratis siempre")
for (k in 1:4) if (tr[[k]] / N_STD >= 0.60)
  fail("El rango ", k, " de |dx| concentra el ", round(100 * tr[[k]] / N_STD), " % (>=60 %)")

cat("    |dx| de la clave, valores observados: ",
    paste(sort(unique(vapply(reg, function(r) r$dx[r$j], numeric(1)))), collapse = ", "), "\n")

# Bateria de reglas de eliminacion PURAMENTE GEOMETRICAS (azar = 25 %)
acierto <- function(reg, sobrevive) {
  v <- vapply(reg, function(r) {
    S <- sobrevive(r); if (!any(S)) return(0.25)
    if (S[r$j]) 1 / sum(S) else 0 }, numeric(1))
  mean(v)
}
# MCD elemento a elemento de (|dx|, |dy|). Se define UNA vez y se reusa en (I),
# (K) y (L): antes estaba duplicado en dos lambdas inline, y una familia de
# reglas que se mide en tres bloques no puede depender de tres copias del mismo
# algoritmo.
gcd_vec <- function(r) mapply(function(a, b) {
  a <- abs(a); b <- abs(b)
  while (b > 0) { t <- b; b <- a %% b; a <- t }
  a }, r$dx, r$dy)
# Cociente ENTERO |dx| / q, o NA si q no divide a |dx|. Es el numero de pasos que
# la opcion representaria si su pendiente fuera la pedida; para la CLAVE vale
# exactamente |t_c|. Sostiene la familia de reglas de paridad y extremo del
# cociente, que la bateria anterior no tenia.
coc_vec <- function(r) ifelse(r$dx %% r$q == 0, r$dx / r$q, NA_real_)

reglas <- list(
  "descartar |dx| maximo"      = function(r) r$dx <  max(r$dx),
  "descartar |dx| > 2"         = function(r) r$dx <= 2,
  "quedarse con |dx| minimo"   = function(r) r$dx == min(r$dx),
  "descartar |dy| minimo"      = function(r) r$dy >  min(r$dy),
  "descartar la mas lejana"    = function(r) r$dd <  max(r$dd),
  "descartar la mas cercana"   = function(r) r$dd >  min(r$dd),
  "|dx|<=2 y |dy|>=2"          = function(r) r$dx <= 2 & r$dy >= 2,
  # RAZON |dy| vs |dx| — canal CIEGO hasta la 2a pasada de la FASE 2C. Como la
  # clave es K = H + t*(q, p), cumple |dy| > |dx| exactamente cuando |m| > 1, y
  # PEND_POOL estaba 14:6 a favor de |m| > 1: la regla llegaba al 37 % sin estar
  # medida por ninguna sonda (ni de este archivo ni del arsenal).
  "quedarse |dy|>|dx|"         = function(r) r$dy >  r$dx,
  "quedarse |dy|<|dx|"         = function(r) r$dy <  r$dx,
  # Cierre SISTEMATICO de la familia min/max. La leccion de la 2a pasada de la
  # FASE 2C es que la regla con mas acierto era justo la que faltaba: una
  # bateria incompleta no mide "sin senal", mide "sin sonda".
  "quedarse con |dx| maximo"   = function(r) r$dx == max(r$dx),
  "quedarse con |dy| maximo"   = function(r) r$dy == max(r$dy),
  "quedarse con |dy| minimo"   = function(r) r$dy == min(r$dy),
  "descartar |dy| maximo"      = function(r) r$dy <  max(r$dy),
  "descartar |dx| minimo"      = function(r) r$dx >  min(r$dx),
  # Abscisa/ordenada UNICA entre las opciones: es un rasgo perceptible sin
  # calcular nada (se observo una version con tres opciones de abscisa -3 y la
  # clave como unica abscisa distinta).
  "quedarse abscisa unica"     = function(r) vapply(seq_along(r$ptx),
                                    function(i) sum(r$ptx == r$ptx[i]) == 1L, logical(1)),
  "descartar abscisa unica"    = function(r) vapply(seq_along(r$ptx),
                                    function(i) sum(r$ptx == r$ptx[i]) >  1L, logical(1)),
  "quedarse ordenada unica"    = function(r) vapply(seq_along(r$pty),
                                    function(i) sum(r$pty == r$pty[i]) == 1L, logical(1)),
  # Familia DIVISIBILIDAD/GCD. CAND_T_CLAVE llega a |t| = 3 y CAND_T solo a 2,
  # asi que |dx| de la clave (= q*|t|) accede a multiplos de 3 que los
  # distractores casi no alcanzan. Canal detectado en la 3a pasada FASE 2C.
  "quedarse |dx| multiplo de 3"= function(r) r$dx %% 3 == 0,
  "quedarse |dx| par"          = function(r) r$dx %% 2 == 0,
  "descartar |dx| par"         = function(r) r$dx %% 2 != 0,
  "quedarse |dy| par"          = function(r) r$dy %% 2 == 0,
  "quedarse ambas pares"       = function(r) r$dx %% 2 == 0 & r$dy %% 2 == 0,
  "descartar ambas pares"      = function(r) !(r$dx %% 2 == 0 & r$dy %% 2 == 0),
  "quedarse |dy| multiplo de 3"= function(r) r$dy %% 3 == 0,
  "quedarse gcd(dx,dy)>1"      = function(r) gcd_vec(r) > 1,
  "descartar gcd(dx,dy)>1"     = function(r) !(gcd_vec(r) > 1),
  # Cierre de la familia DIVISIBILIDAD por min/max, simetrico al que (I) ya hace
  # con |dx| y |dy|. Faltaba justamente el extremo: "quedarse con la de mayor
  # factor comun". Una bateria que mide `gcd>1` pero no `max gcd` no mide la
  # familia, mide un punto de ella.
  "quedarse gcd maximo"        = function(r) { g <- gcd_vec(r); g == max(g) },
  "quedarse gcd minimo"        = function(r) { g <- gcd_vec(r); g == min(g) },
  "descartar coordenada 0"     = function(r) !(r$ptx == 0 | r$pty == 0),
  "quedarse coordenada 0"      = function(r)  (r$ptx == 0 | r$pty == 0),
  "comparte ordenada con H"    = function(r) r$pty == r$hy,
  # --- BATERIA AMPLIADA (2026-08-15b) ------------------------------------------
  # Familia MAGNITUD: los DOS extremos a la vez. Ni (I) ni (L) tenian la regla
  # "descartar los extremos y quedarse con el resto", que es la que explota que
  # la clave nunca sea ni la mayor ni la menor. Medida en la linea base: 45,1 %
  # sin canonicas, por encima del umbral de FALLO de este mismo bloque.
  "descartar |m| extremos"     = function(r) r$mab > min(r$mab) & r$mab < max(r$mab),
  "descartar |dx| extremos"    = function(r) r$dx  > min(r$dx)  & r$dx  < max(r$dx),
  "descartar |dy| extremos"    = function(r) r$dy  > min(r$dy)  & r$dy  < max(r$dy),
  # Familia DIVISIBILIDAD: la PARIDAD del cociente dx/q y su extremo. No es
  # deducible del enunciado (el estudiante no puede saber que |t| esta acotado),
  # asi que es artefacto puro. En la linea base "cociente impar" daba 44,6 %.
  "cociente dx/q par"          = function(r) { v <- coc_vec(r); !is.na(v) & v %% 2 == 0 },
  "cociente dx/q impar"        = function(r) { v <- coc_vec(r); !is.na(v) & v %% 2 == 1 },
  "cociente dx/q maximo"       = function(r) { v <- coc_vec(r)
                                   if (all(is.na(v))) return(rep(TRUE, length(v)))
                                   !is.na(v) & v == max(v, na.rm = TRUE) },
  "cociente dx/q minimo"       = function(r) { v <- coc_vec(r)
                                   if (all(is.na(v))) return(rep(TRUE, length(v)))
                                   !is.na(v) & v == min(v, na.rm = TRUE) }
)
cat("    acierto de reglas de eliminacion geometricas (azar = 25.0 %):\n")
peor <- 0
for (nm in names(reglas)) {
  a <- acierto(reg, reglas[[nm]]); peor <- max(peor, a)
  cat(sprintf("      %-26s : %5.1f %%\n", nm, 100 * a))
}
# El FALLO se decide SIN canonicas (politica alineada con (F), D-5): la canonica
# reproduce el item oficial y no es corregible. Se publican las dos columnas.
peor_sc <- 0; peor_sc_nm <- ""
for (nm in names(reglas)) { a <- acierto(reg_nc, reglas[[nm]])
  if (a > peor_sc) { peor_sc <- a; peor_sc_nm <- nm } }
cat(sprintf("      MAXIMO con canonicas %.1f %%  |  SIN canonicas %.1f %% (%s)\n",
            100 * peor, 100 * peor_sc, peor_sc_nm))
if (peor_sc >= 0.45) fail("Una regla geometrica alcanza ", round(100 * peor_sc),
                          " % sin canonicas (azar 25 %): ", peor_sc_nm)
if (peor_sc >= 0.35) warn("Una regla geometrica alcanza ", round(100 * peor_sc),
                          " % sin canonicas (azar 25 %): ", peor_sc_nm)
# Control positivo: una regla que SIEMPRE deja sola a la clave debe dar 100 %
if (abs(acierto(reg, function(r) seq_along(r$dx) == r$j) - 1) > 1e-9)
  fail("CONTROL POSITIVO ROTO: la bateria no puntua 100 % una regla omnisciente")
cat("      control positivo (regla omnisciente): 100.0 % -> bateria viva\n")

# --- DEDUCCION LEGITIMA, medida aparte y NO contada como defecto --------------
# `q | dx` NO es un atajo superficial: es una CONSECUENCIA del enunciado. Si
# dy/dx ha de valer p/q con p/q irreducible y las coordenadas son enteras,
# entonces q divide a dx. Un estudiante que la aplica esta haciendo matematicas
# correctas, no explotando un artefacto del generador, y NO puede eliminarse sin
# romper el ejercicio. Se mide y se publica para que nadie la "corrija" luego.
# La version fija "multiplo de 3" que si esta en la bateria de arriba es su
# sombra: solo rinde por encima del azar cuando q = 3 (46,2 % frente a 23,7 %
# cuando q != 3), asi que mide esta misma deduccion, no un canal aparte.
# M-2: se declaraba `q | dx` y se OMITIA su mitad simetrica `|p| | dy`. Las dos
# salen del mismo hecho —si dy/dx = p/q con p/q irreducible y las coordenadas son
# enteras, entonces dx = q*t y dy = p*t— asi que publicar solo una daba una
# imagen incompleta de cuanto acota el enunciado por si mismo. Se publican las
# dos y su conjuncion, que es la deduccion completa que un estudiante puede hacer
# leyendo el enunciado y sin dividir nada.
a_q  <- acierto(reg, function(r) r$dx %% r$q == 0)
a_p  <- acierto(reg, function(r) r$dy %% abs(r$p) == 0)
# SIN canonicas (politica D-5, O-3): este valor es el denominador del marginal
# que decide un check BLOQUEANTE, asi que tiene que salir del mismo subconjunto.
a_qp <- acierto(reg_nc, function(r) r$dx %% r$q == 0 & r$dy %% abs(r$p) == 0)
cat("    [deducciones LEGITIMAS, fuera del veredicto]\n")
cat(sprintf("      q | dx                     : %5.1f %%\n", 100 * a_q))
cat(sprintf("      |p| | dy                   : %5.1f %%\n", 100 * a_p))
cat(sprintf("      ambas (q | dx  y  |p| | dy): %5.1f %%\n", 100 * a_qp))
cat("      Son condiciones NECESARIAS de la pendiente pedida, no atajos: acotan\n")
cat("      el campo pero no identifican la clave, y eliminarlas exigiria que la\n")
cat("      clave incumpliera su propia definicion. Un estudiante que las aplica\n")
cat("      esta haciendo matematicas correctas.\n")

# =============================================================================
# (J) CANAL SEMANTICO DEL SIGNO — sonda anadida en la 2a pasada de la FASE 2C.
#     Ninguna sonda del arsenal (H1 longitud, H2 primera palabra, H3/H3b
#     veredicto) mira el SIGNO de la pendiente de cada opcion, y la bateria (I)
#     es PURAMENTE geometrica: trabaja con |dx|, |dy| y distancias, que son
#     magnitudes sin signo. El canal quedaba, por tanto, sin medir en las dos
#     capas a la vez. "Quedarse con las opciones cuya pendiente tiene el signo
#     pedido" acertaba el 42 % (azar 25 %) y el 100 % en el 7 % de versiones en
#     que la clave era la UNICA de su signo. Es ademas el atajo que el propio
#     item declara erroneo: FUN-PEN-04 ES ese error.
# =============================================================================
cat("\n(J) Canal semantico: elegir por el SIGNO de la pendiente:\n")
a_sig <- acierto(reg, function(r) r$sg == r$sg_obj)
cat(sprintf("    acierto de la regla SOLO-SIGNO : %5.1f %%  (azar 25.0 %%)\n", 100 * a_sig))
tsg <- table(factor(vapply(reg, function(r) sum(r$sg == r$sg_obj), integer(1)), levels = 1:4))
for (k in 1:4)
  cat(sprintf("      %d opcion(es) con el signo de la clave : %3d  (%.1f %%)\n",
              k, tsg[[k]], 100 * tsg[[k]] / N_STD))
cat("    cota teorica si >=3 opciones comparten el signo: 33.3 %\n")
if (tsg[[1]] > 0L)
  fail("En ", tsg[[1]], " version(es) la clave es la UNICA de su signo: ahi SOLO-SIGNO acierta el 100 %")
a_sig_sc <- acierto(reg_nc, function(r) r$sg == r$sg_obj)
cat(sprintf("    SOLO-SIGNO sin canonicas       : %5.1f %%\n", 100 * a_sig_sc))
if (a_sig_sc >= 0.40) fail("La regla SOLO-SIGNO alcanza ", round(100 * a_sig_sc), " % sin canonicas")
if (a_sig_sc >= 0.35) warn("La regla SOLO-SIGNO alcanza ", round(100 * a_sig_sc), " % sin canonicas")
# Control positivo: sobre un conjunto de signos FABRICADO en que la clave es la
# unica de su signo, la sonda debe puntuar 100 %. Sin el, un 25 % no distingue
# "sin senal" de "sonda que no mide" (Incidente P / INC-MUTANTE-SONDA).
reg_ctrl <- lapply(reg, function(r) {
  r$sg <- rep(-1, length(r$sg)); r$sg[r$j] <- 1; r$sg_obj <- 1; r })
if (abs(acierto(reg_ctrl, function(r) r$sg == r$sg_obj) - 1) > 1e-9)
  fail("CONTROL POSITIVO ROTO: la sonda de signo no puntua 100 % una clave unica en su signo")
cat("    control positivo (clave unica de su signo): 100.0 % -> sonda viva\n")

# =============================================================================
# (K) CIERRE POR PARES — sonda anadida en la 3a pasada de la FASE 2C.
#     (I) y (J) miden reglas ATOMICAS y aplican su umbral a cada una por
#     separado. Ninguna capa evaluaba la CONJUNCION de dos reglas que
#     individualmente pasan: SOLO-SIGNO (32,7 %) y |dy|>|dx| (31,2 %) pasan las
#     dos y su conjuncion alcanza el 44,2 %, a 0,8 pp del umbral de FALLO que
#     este mismo archivo fija para reglas simples.
#
#     UMBRAL ABSOLUTO -> CRITERIO POR EXCESO (2026-08-16, decision del
#     profesor). Este bloque juzgaba con un umbral ABSOLUTO (fail 50 %, warn
#     40 %) sin techo nulo — el MISMO criterio que la medicion sobre 468 items
#     oficiales del ICFES demostro que mide, en parte, el TAMANO de la
#     bateria (45 reglas aqui frente a las 19-91 medidas en esa poblacion).
#     Es la deuda que quedo declarada al cerrar (K3)/(K4)/(L) el 2026-08-15:
#     "el cierre por pares y tripletes de auditoria_propia.R no incluia las
#     reglas intra-celda de (L)" llevaba implicito que (K) seguia con el
#     criterio viejo mientras sus vecinos ya usaban el nuevo. Mismo protocolo
#     que (K3)/(L)/(K4): permutar cual opcion es la clave dejando las reglas
#     intactas, MEDIA sobre K_PERM replicas (nunca max — sobreestimar el techo
#     REBAJA el exceso, el mismo sesgo a favor del artefacto que se corrigio
#     en (K4)), juzgado por los cortes calibrados del helper §P7.
# =============================================================================
cat("\n(K) Cierre por PARES sobre (I)+(J) (azar 25.0 %):\n")
# Se incluye tambien la regla CONDICIONADA por |m| (bloque L): asi el cierre por
# pares cubre la conjuncion SOLO-SIGNO AND BUCKET, que es el atajo de dos bits.
# Sin ella, (K) solo combinaba reglas incondicionales y no podia alcanzarlo.
reglas_k <- c(reglas, list(
  "SOLO-SIGNO"         = function(r) r$sg == r$sg_obj,
  "BUCKET condicional" = function(r) if (r$mobj > 1) r$dy > r$dx else r$dy < r$dx,
  # O-4 de la FASE 2C: ni (I) ni (K) ni (L) tenian UNA SOLA regla condicionada por
  # `q` o por `|p|` POR SEPARADO, que son los dos numeros que el enunciado imprime.
  # (L) condiciona solo por |m|. La version FIJA de esta familia si estaba
  # ("|dx| multiplo de 3") y el propio .Rmd reconocia que solo rinde cuando q = 3
  # -> se sabia cual era la version relevante y no se media. Sin estas entradas el
  # cierre por pares y por tripletes estaba ciego a su propia familia mas fuerte.
  "q | dx"             = function(r) r$dx %% r$q == 0,
  "|p| | dy"           = function(r) r$dy %% abs(r$p) == 0,
  # EL TOPE SALE DEL .Rmd, NO ESTA CLAVADO (2026-08-15). Esta regla mide el
  # ARTEFACTO "el |dx| de la clave es q por un entero PEQUENO", y lo que la hace
  # artefacto es el tope del soporte de t, que el estudiante no puede deducir.
  # Escrito a mano como `<= 3 * q`, la sonda dejaba de medir el artefacto en
  # cuanto el soporte cambiaba: con |t| <= 5 habria reportado 35,7 % en vez del
  # 44,4 % real, es decir, ampliar el soporte habria "aprobado" la regla sin que
  # el canal se cerrara. Una sonda que se desactiva justo cuando se toca lo que
  # vigila no es una sonda: es una puerta amanada. Ahora TMAX_T se deriva de
  # CAND_T_CLAVE del propio .Rmd y la etiqueta se imprime con el valor real.
  "|dx| en q*(1..T)"   = function(r) r$dx %% r$q == 0 & r$dx <= TMAX_T * r$q,
  "|dx| == q"          = function(r) r$dx == r$q,
  "|dy| == |p|"        = function(r) r$dy == abs(r$p)))
nk <- names(reglas_k); pares <- utils::combn(length(reglas_k), 2L)

# --- Las reglas CONDICIONADAS por q / |p| tambien se miden ATOMICAMENTE -------
# Estan en `reglas_k` para que entren en el cierre, pero una regla simple se juzga
# con el umbral de regla simple (45 %), no solo dentro de conjunciones.
cat("    reglas condicionadas por q / |p| (medidas como ATOMICAS, azar 25 %):\n")
cat(sprintf("      (el tope T de `|dx| en q*(1..T)` se lee del .Rmd: T = %d)\n", TMAX_T))
cond_nm <- c("q | dx", "|p| | dy", "|dx| en q*(1..T)", "|dx| == q", "|dy| == |p|")
peor_c <- 0; peor_c_nm <- ""
for (nm in cond_nm) {
  a <- acierto(reg, reglas_k[[nm]])
  if (a > peor_c) { peor_c <- a; peor_c_nm <- nm }
  cat(sprintf("      %-22s : %5.1f %%\n", nm, 100 * a))
}
cat("      NOTA: `q | dx` y `|p| | dy` son consecuencias NECESARIAS del enunciado\n")
cat("      (dx = q*t, dy = p*t), asi que su acierto NO es un defecto corregible:\n")
cat("      quitarlo exigiria que la clave incumpliera su definicion. Las tres\n")
cat(sprintf("      restantes SI son artefacto del generador (acotan |t| <= %d, que el\n", TMAX_T))
cat("      estudiante no puede deducir) y se juzgan con el umbral normal.\n")
art_nm <- c("|dx| en q*(1..T)", "|dx| == q", "|dy| == |p|")
peor_a <- max(vapply(art_nm, function(nm) acierto(reg, reglas_k[[nm]]), numeric(1)))
if (peor_a >= 0.45) fail("Una regla condicionada por q/|p| que es ARTEFACTO alcanza ",
                         round(100 * peor_a), " % (azar 25 %)")
if (peor_a >= 0.40) warn("Una regla condicionada por q/|p| que es ARTEFACTO alcanza ",
                         round(100 * peor_a), " % (azar 25 %)")
res_k <- apply(pares, 2L, function(ij)
  acierto(reg, function(r) reglas_k[[ij[1]]](r) & reglas_k[[ij[2]]](r)))
ord <- order(res_k, decreasing = TRUE)
cat("    pares evaluados: ", ncol(pares), "  |  top 5:\n", sep = "")
for (k in ord[1:5])
  cat(sprintf("      %-26s AND %-26s : %5.1f %%\n",
              nk[pares[1, k]], nk[pares[2, k]], 100 * res_k[k]))
peor_k <- max(res_k)
res_k_sc  <- apply(pares, 2L, function(ij)
  acierto(reg_nc, function(r) reglas_k[[ij[1]]](r) & reglas_k[[ij[2]]](r)))
ord_sc    <- order(res_k_sc, decreasing = TRUE)
peor_k_sc <- res_k_sc[ord_sc[1]]
cat(sprintf("    MAXIMO con canonicas %.1f %%  |  SIN canonicas %.1f %%\n",
            100 * peor_k, 100 * peor_k_sc))
cat(sprintf("    [historico, NO decide] umbral absoluto anterior (fail 50 %%, warn 40 %%) -> %s\n",
            if (peor_k_sc >= 0.50) "fail lo hubiera cruzado"
            else if (peor_k_sc >= 0.40) "warn lo hubiera cruzado"
            else "ninguno se hubiera cruzado"))
# --- TECHO NULO DE (K): de UMBRAL ABSOLUTO a CRITERIO POR EXCESO (2026-08-16,
# decision del profesor). Hasta esta fecha (K) era el UNICO de los cuatro
# cierres de esta bateria ((K),(K3),(K4),(L)) que seguia juzgando con un
# umbral ABSOLUTO (fail 50 %, warn 40 %) sin techo nulo -- el mismo criterio
# que la medicion sobre 468 items oficiales del ICFES (cabecera del helper
# §P7) demostro que mide, en parte, el TAMANO de la propia bateria: la misma
# poblacion da 27,4 % con 19 reglas y 34,8 % con 25, y el techo nulo se mueve
# con ella. Mismo protocolo que (K3)/(L)/(K4): se permuta cual opcion es la
# clave dejando las reglas intactas, se toma la MEDIA sobre K_PERM replicas
# (NUNCA `max` de un punado de replicas: el maximo de pocas replicas es un
# estimador SESGADO AL ALZA del techo, y sobreestimar el techo REBAJA el
# exceso -- el mismo sesgo a favor del artefacto auditado que se encontro y
# corrigio en (K4)), con su `sd` siempre publicada.
set.seed(20260815L)
nulos_k <- vapply(seq_len(K_PERM), function(i) {
  perm  <- sample.int(4L, length(reg_nc), replace = TRUE)
  reg_p <- lapply(seq_along(reg_nc), function(z) { r <- reg_nc[[z]]; r$j <- perm[z]; r })
  max(apply(pares, 2L, function(ij)
    acierto(reg_p, function(r) reglas_k[[ij[1]]](r) & reglas_k[[ij[2]]](r))))
}, numeric(1))
techo_nulo_k <- mean(nulos_k)
sd_k <- if (K_PERM > 1L) stats::sd(nulos_k) else NA_real_
exceso_k <- peor_k_sc - techo_nulo_k
cat(sprintf("    techo NULO (K) sin canonicas (clave permutada, %d replicas, media sobre %d pares) : %5.1f %%  (sd %.1f pp)\n",
            K_PERM, ncol(pares), 100 * techo_nulo_k, 100 * sd_k))
cat(sprintf("    EXCESO sin canonicas sobre el nulo : %+5.1f pp  (corte de canal %+.1f pp, corte de ruido %+.1f pp)\n",
            100 * exceso_k, 100 * CORTE_CANAL, 100 * CORTE_RUIDO))
if (!is.na(sd_k) && exceso_k >= CORTE_CANAL && exceso_k - K_SIGMA * sd_k <= CORTE_RUIDO) {
  warn("Una CONJUNCION de dos reglas alcanza ", round(100 * peor_k_sc), " % sin canonicas, exceso ",
       round(100 * exceso_k), " pp, pero el ruido (sd ", round(100 * sd_k, 1),
       " pp) lo devuelve a la zona gris: NO CONCLUYENTE — ",
       nk[pares[1, ord_sc[1]]], " AND ", nk[pares[2, ord_sc[1]]])
  k_aceptado <- NA
} else if (exceso_k >= CORTE_CANAL) {
  # (K) TIENE ENTRADA PROPIA en RESIDUO_ACEPTADO desde el 2026-08-16, por
  # decision explicita del profesor, y se juzga CONTRA ELLA -- nunca en cascada
  # desde (K3). El argumento de dominancia es cierto (max(tripletes) >=
  # max(pares) por construccion sobre el mismo `reglas_k`: medido 61,7 % vs
  # 51,0 %), pero deducir la aceptacion de (K3) dejaba a (K) SIN VIGILANCIA
  # PROPIA: si (K3) bajara, (K) podria quedar por encima sin que nada lo notara,
  # porque su unica referencia era una linea base ajena. Con entrada propia,
  # (K) se compara consigo mismo y vuelve a bloquear si empeora, aunque (K3)
  # siga igual. Ver el comentario de `k_pares` en RESIDUO_ACEPTADO.
  k_aceptado <- residuo_ok("k_pares", "exceso", exceso_k,
                           "(K) cierre por pares (exceso sobre su techo nulo)")
  if (!k_aceptado) {
    fail("Una CONJUNCION de dos reglas alcanza ", round(100 * peor_k_sc),
         " % sin canonicas (techo nulo ", round(100 * techo_nulo_k),
         " %, exceso ", round(100 * exceso_k), " pp, corte de canal ",
         round(100 * CORTE_CANAL), " pp), y EXCEDE su PROPIA linea base de ",
         "residuo aceptado: ",
         nk[pares[1, ord_sc[1]]], " AND ", nk[pares[2, ord_sc[1]]])
  }
} else {
  k_aceptado <- NA
  cat(sprintf("    -> SIN canal: exceso %+.1f pp por debajo del corte de canal (%+.1f pp) -> (K) no bloquea\n",
              100 * exceso_k, 100 * CORTE_CANAL))
}
if (exceso_k > CORTE_RUIDO && exceso_k < CORTE_CANAL)
  warn("Una CONJUNCION de dos reglas queda en ZONA GRIS: ", round(100 * peor_k_sc),
       " % sin canonicas, exceso ", round(100 * exceso_k), " pp — ",
       nk[pares[1, ord_sc[1]]], " AND ", nk[pares[2, ord_sc[1]]])
# Control positivo: la conjuncion de la regla omnisciente consigo misma da 100 %.
omni <- function(r) seq_along(r$dx) == r$j
if (abs(acierto(reg, function(r) omni(r) & omni(r)) - 1) > 1e-9)
  fail("CONTROL POSITIVO ROTO: el cierre por pares no puntua 100 % una conjuncion omnisciente")
cat("    control positivo (omnisciente AND omnisciente): 100.0 % -> cierre vivo\n")

# =============================================================================
# (K3) CIERRE POR TRIPLETES — anadido en la 5a pasada de la FASE 2C (A-1).
#      (K) cerraba por PARES. El atajo de dos bits es SIGNO and BUCKET, asi que
#      cualquier refinamiento suyo por un tercer rasgo ("dentro de la celda,
#      quedarse con las de factor comun") es un TRIPLETE y quedaba fuera del
#      cierre. Se enumeran los C(n,3) tripletes.
#
#      EL UMBRAL NO SE INVENTA: se calibra contra un NULO. Con miles de
#      combinaciones, el MAXIMO sobre todas ellas esta inflado por seleccion
#      —siempre habra alguna que, por azar de la muestra, contenga a la clave—.
#      El nulo permuta que opcion es la clave, deja las reglas intactas y vuelve
#      a tomar el maximo: mide cuanto sube el estadistico SIN que haya canal.
#      Un maximo observado por debajo del techo nulo no es evidencia de nada.
# =============================================================================
cat("\n(K3) Cierre por TRIPLETES sobre (I)+(J)+BUCKET (azar 25.0 %):\n")
# POLITICA D-5 APLICADA AQUI TAMBIEN (O-3 de la 4a pasada de la FASE 2C). Este
# bloque bloqueaba con el agregado CON canonicas mientras (I)/(J)/(K)/(L) ya
# bloqueaban sin ellas: dos politicas distintas en el mismo archivo. La canonica
# es justamente la instancia en que la clave es la UNICA de su celda, asi que
# infla mecanicamente cualquier conjuncion que contenga el atajo de dos bits.
Jkey  <- vapply(reg_nc, function(r) r$j, integer(1))
Smat  <- function(f) t(vapply(reg_nc, function(r) as.logical(f(r)), logical(4)))
acierto_M <- function(M, jj = Jkey) {
  ns  <- rowSums(M)
  hit <- M[cbind(seq_len(nrow(M)), jj)]
  mean(ifelse(ns == 0L, 0.25, ifelse(hit, 1 / pmax(ns, 1L), 0)))
}
MATS <- lapply(reglas_k, Smat)
trip <- utils::combn(length(reglas_k), 3L)
val_trip <- function(jj) apply(trip, 2L, function(z)
  acierto_M(MATS[[z[1]]] & MATS[[z[2]]] & MATS[[z[3]]], jj))
res_t <- val_trip(Jkey)
ordt  <- order(res_t, decreasing = TRUE)
cat("    tripletes evaluados: ", ncol(trip), "  |  top 5:\n", sep = "")
for (k in ordt[1:5])
  cat(sprintf("      %-24s AND %-24s AND %-24s : %5.1f %%\n",
              nk[trip[1, k]], nk[trip[2, k]], nk[trip[3, k]], 100 * res_t[k]))
peor_t <- max(res_t)
# Techo nulo por MEDIA, no por maximo de replicas (2026-08-15): el maximo de un
# puñado de replicas sobreestima el techo, y sobreestimar el techo REBAJA el
# exceso — sesgo a favor del artefacto auditado. Cortes leidos del helper §P7:
# este bloque ya juzgaba por exceso, pero con cortes propios (+10/+5) que nadie
# habia calibrado contra nada.
set.seed(20260815L)
nulos_t <- vapply(seq_len(K_PERM), function(i)
  max(val_trip(sample.int(4L, length(reg_nc), replace = TRUE))), numeric(1))
techo_nulo <- mean(nulos_t)
sd_t <- if (K_PERM > 1L) stats::sd(nulos_t) else NA_real_
cat(sprintf("    maximo observado : %5.1f %%\n", 100 * peor_t))
cat(sprintf("    techo NULO (clave permutada, %d replicas, media) : %5.1f %%  (sd %.1f pp)\n",
            K_PERM, 100 * techo_nulo, 100 * sd_t))
cat(sprintf("    EXCESO : %+5.1f pp  (corte de canal %+.1f pp, corte de ruido %+.1f pp)\n",
            100 * (peor_t - techo_nulo), 100 * CORTE_CANAL, 100 * CORTE_RUIDO))
cat("      El techo nulo es cuanto alcanza el MEJOR de los ", ncol(trip),
    " tripletes cuando NO\n", sep = "")
cat("      hay canal alguno. Restarlo es lo que separa senal de seleccion.\n")
exceso_k3 <- peor_t - techo_nulo
if (exceso_k3 >= CORTE_CANAL) {
  k3_aceptado <- residuo_ok("k3_triplete", "exceso", exceso_k3, "(K3) cierre por tripletes")
  if (!k3_aceptado)
    fail("Un TRIPLETE alcanza ", round(100 * peor_t), " %, ",
         round(100 * exceso_k3), " pp por encima del techo nulo (",
         round(100 * techo_nulo), " %), EXCEDE la linea base de residuo aceptado: ",
         nk[trip[1, ordt[1]]], " AND ", nk[trip[2, ordt[1]]], " AND ", nk[trip[3, ordt[1]]])
} else {
  k3_aceptado <- NA
}
if (exceso_k3 >= CORTE_RUIDO && exceso_k3 < CORTE_CANAL)
  warn("Un TRIPLETE queda en ZONA GRIS: ", round(100 * peor_t), " %, ",
       round(100 * exceso_k3), " pp sobre el techo nulo: ",
       nk[trip[1, ordt[1]]], " AND ", nk[trip[2, ordt[1]]], " AND ", nk[trip[3, ordt[1]]])
# Control positivo: el triplete omnisciente consigo mismo debe dar 100 %.
Mo <- Smat(omni)
if (abs(acierto_M(Mo & Mo & Mo) - 1) > 1e-9)
  fail("CONTROL POSITIVO ROTO: el cierre por tripletes no puntua 100 % un triplete omnisciente")
cat("    control positivo (omnisciente x3): 100.0 % -> cierre vivo\n")

# =============================================================================
# (L) REGLAS CONDICIONADAS POR |m| — sonda anadida en la 4a pasada de la FASE 2C.
#     El estudiante LEE la pendiente pedida, asi que puede condicionar la regla a
#     ella. Las dos comprobaciones baratas son: el SIGNO de dy/dx y si |dy| es
#     mayor o menor que |dx| (o sea, de que lado de 1 cae la pendiente). Ninguna
#     exige dividir. Su conjuncion es el "atajo de dos bits" que sobre el pool de
#     7 anterior resolvia el 73,4 % de las versiones (azar 25 %), sin que (I),
#     (J) ni (K) pudieran verlo: (I)/(K) solo expresan reglas incondicionales.
#     La celda queda ahora poblada por construccion con FUN-PEN-04, -08 y -09.
# =============================================================================
cat("\n(L) Reglas CONDICIONADAS por |m| (azar 25.0 %):\n")
f_buck <- function(r) if (r$mobj > 1) r$dy > r$dx else r$dy < r$dx
f_sign <- function(r) r$sg == r$sg_obj
f_cell <- function(r) f_sign(r) & f_buck(r)
# Distancia a 1 en escala logaritmica: monotona y simetrica para |m| y 1/|m|.
# pmax evita -Inf en las opciones de pendiente 0, que nunca estan en la celda.
d1 <- function(r) abs(log(pmax(r$mab, 1e-9)))
extrema_en <- function(r, C, v, cual) {
  if (!any(C)) return(C)
  C & (v == (if (cual == "max") max(v[C]) else min(v[C])))
}
# CIERRE POR FAMILIAS DE DIMENSION (A-1, 5a pasada de la FASE 2C) -------------
# La version anterior de este bloque enumeraba SEIS reglas intra-celda y las
# seis pertenecian a la misma familia: MAGNITUD (distancia a 1, |dx|, |dy|).
# Ninguna tocaba la DIVISIBILIDAD, y ahi habia canal: la clave usa el vector
# PRIMITIVO (q, p), asi que LIM_COORD le deja llegar a |t| = 2 o 3 y su
# gcd(dx, dy) = |t| sale > 1 a menudo; sus companeros de celda (04, 08, 09)
# llevan vectores mayores, LIM_COORD los empuja a |t| = 1 y su gcd es el de dos
# numeros sin factor comun garantizado. "Quedarse con la de mayor factor comun
# dentro de la celda" explotaba esa asimetria.
#
# LECCION GENERICA: una bateria de reglas de eliminacion necesita cierre por
# FAMILIAS DE DIMENSION (magnitud, divisibilidad, signo, posicion), no solo por
# min/max dentro de la familia que a uno se le ocurrio primero. Enumerar a mano
# reproduce el sesgo de quien enumera: las cuatro pasadas anteriores de la FASE
# 2C encontraron, cada una, que la regla con mas acierto era justo la que
# faltaba. Por eso las reglas intra-celda se GENERAN por producto cartesiano
# {familia} x {max, min} y {familia} x {predicado}, no se escriben una a una.
vals_l <- list(
  "dist.a 1"   = d1,                      # familia MAGNITUD (relativa a 1)
  "|dx|"       = function(r) r$dx,        # familia MAGNITUD
  "|dy|"       = function(r) r$dy,        # familia MAGNITUD
  "dist.a H"   = function(r) r$dd,        # familia MAGNITUD (euclidea)
  "gcd(dx,dy)" = gcd_vec,                 # familia DIVISIBILIDAD  <-- faltaba
  # AMPLIACION 2026-08-15b: el cociente entero |dx|/q dentro de la celda. Es la
  # forma intra-celda de la familia que (I) ahora mide de forma atomica.
  "coc dx/q"   = function(r) { v <- coc_vec(r); ifelse(is.na(v), -1, v) }
)
preds_l <- list(
  "gcd>1"       = function(r) gcd_vec(r) > 1,      # familia DIVISIBILIDAD
  "gcd=1"       = function(r) gcd_vec(r) == 1,
  "|dx| par"    = function(r) r$dx %% 2 == 0,
  "|dy| par"    = function(r) r$dy %% 2 == 0,
  "ambas pares" = function(r) r$dx %% 2 == 0 & r$dy %% 2 == 0,
  "coord. 0"    = function(r) r$ptx == 0 | r$pty == 0,  # familia POSICION
  "coc par"     = function(r) { v <- coc_vec(r); !is.na(v) & v %% 2 == 0 },
  "coc impar"   = function(r) { v <- coc_vec(r); !is.na(v) & v %% 2 == 1 }
)
mk_extrema <- function(v, cual) { force(v); force(cual)
  function(r) extrema_en(r, f_cell(r), v(r), cual) }
mk_pred <- function(pf) { force(pf)
  function(r) f_cell(r) & pf(r) }

reglas_l <- list(
  "BUCKET condicional"        = f_buck,
  "SIGNO+BUCKET (2 bits)"     = f_cell,
  "complemento de la celda"   = function(r) !f_cell(r)
)
# EL TERCER PUNTO DE LA FAMILIA: ni max ni min, sino EL DEL MEDIO. Faltaba, y
# ahi estaba el canal mas grande del item: con FUN-PEN-08 acercando a 1 y
# FUN-PEN-04/-09 alejando, la clave quedaba en el centro de su celda en el 66,3 %
# de las versiones (N = 100 sin canonicas, techo nulo 35 %). Una bateria que mide
# los dos extremos de una familia y no su interior no mide la familia: mide sus
# bordes. Vale para CUALQUIER magnitud, no solo para la distancia a 1.
mk_medio <- function(v) { force(v)
  function(r) { C <- f_cell(r); if (!any(C)) return(C)
    vv <- v(r); C & vv > min(vv[C]) & vv < max(vv[C]) } }
for (nm in names(vals_l)) {
  reglas_l[[paste0("celda + ", nm, " max")]]   <- mk_extrema(vals_l[[nm]], "max")
  reglas_l[[paste0("celda + ", nm, " min")]]   <- mk_extrema(vals_l[[nm]], "min")
  reglas_l[[paste0("celda + ", nm, " medio")]] <- mk_medio(vals_l[[nm]])
}
for (nm in names(preds_l)) reglas_l[[paste0("celda + ", nm)]] <- mk_pred(preds_l[[nm]])
peor_l <- 0; peor_l_nm <- ""
for (nm in names(reglas_l)) {
  a <- acierto(reg, reglas_l[[nm]])
  if (a > peor_l) { peor_l <- a; peor_l_nm <- nm }
  cat(sprintf("      %-26s : %5.1f %%\n", nm, 100 * a))
}
# Ocupacion de la celda: es lo que fija la cota del atajo (1/n si hay n opciones).
occ <- vapply(reg, function(r) sum(f_cell(r)), integer(1))
to <- table(factor(occ, levels = 0:4))
cat("    opciones dentro de la celda de la clave:\n")
for (k in 0:4) cat(sprintf("      %d : %3d  (%.1f %%)  -> el atajo acierta %s\n",
                           k, to[[k + 1L]], 100 * to[[k + 1L]] / N_STD,
                           if (k == 0) "25.0 % (nadie sobrevive)" else sprintf("%.1f %%", 100 / k)))
# UMBRAL DEDICADO del atajo de dos bits, que es el que motiva todo este bloque.
# Es una conjuncion de DOS comprobaciones que no exigen dividir, asi que se le
# aplica el umbral de una regla simple.
a_comb <- acierto(reg, f_cell)
a_comb_sc <- acierto(reg_nc, f_cell)
cat(sprintf("    SIGNO+BUCKET sin canonicas : %5.1f %%\n", 100 * a_comb_sc))
if (a_comb_sc >= 0.45) fail("SIGNO+BUCKET alcanza ", round(100 * a_comb_sc), " % sin canonicas")
if (a_comb_sc >= 0.40) warn("SIGNO+BUCKET alcanza ", round(100 * a_comb_sc), " % sin canonicas")
# Las reglas intra-celda ("celda + ...") son conjunciones de TRES condiciones y
# ademas exigen ORDENAR magnitudes, que ya es casi calcular las pendientes; por
# eso llevan umbral propio, mas alto que el de dos bits.
#
# TENSION DECLARADA en "celda + |dx| maximo". Es la peor de esta familia y no
# baja mas sin reabrir un canal ya cerrado: el filtro `rango_objetivo` obliga a
# que el rango de |dx| de la clave entre las CUATRO opciones sea uniforme
# (28/24/25/23 % medido), justamente para que "descartar las lejanas en x" no
# funcione. Uniforme entre cuatro significa que la clave es la de |dx| mayor en
# ~1/4 de las versiones; y como la opcion que queda FUERA de la celda suele ser
# la del reciproco, que tiene |dx| grande, quitarla asciende a la clave. Forzar
# que la clave nunca sea la extrema de su celda crearia la senal inversa
# ("descartar la extrema de la celda" acertaria el 100 %). Se deja medida.
# DEDUCCION PARCIAL, **NO** UN PISO — retractacion medida (O-2 de la FASE 2C).
# Una version anterior de este bloque presentaba `q|dx AND |p||dy` como un "piso
# legitimo" y sostenia que una regla por debajo de el no supera al razonamiento
# correcto. ERA FALSO, por dos razones que se comprobaron:
#   1. NO IDENTIFICA NINGUNA LINEA BASE: es el criterio exacto TRUNCADO, y su
#      valor depende de donde se corte -> q|dx solo ~39 %, |p||dy solo ~43 %,
#      los dos ~51 %, anadiendo dx/q == dy/|p| ~91 %, y con el signo 100 %.
#   2. LA PREMISA "SIN DIVIDIR" NO DISTINGUE NADA: el criterio EXACTO tampoco
#      exige dividir, porque dy/dx = p/q equivale a dy*q == dx*p, que son dos
#      multiplicaciones por opcion. La deduccion parcial es una estrategia
#      ESTRICTAMENTE DOMINADA: mismo coste, la mitad del acierto.
# Ademas su poder discriminante no lo fija el enunciado sino el POOL (hay errores
# que la cumplen siempre y otros casi nunca), asi que tampoco es inevitable.
# Lo que SI es informativo es el MARGINAL de cada atajo por encima de ella: mide
# cuanto aporta el artefacto a quien ya razona, que es la pregunta pedagogica.
f_piso <- function(r) r$dx %% r$q == 0 & r$dy %% abs(r$p) == 0
cat(sprintf("    deduccion PARCIAL q|dx AND |p||dy : %5.1f %%  (NO es un piso: ver arriba)\n",
            100 * a_qp))
for (nm in c("celda (signo+bucket)", "solo signo", "solo bucket")) {
  g  <- switch(nm, "celda (signo+bucket)" = f_cell, "solo signo" = f_sign, "solo bucket" = f_buck)
  am <- acierto(reg_nc, local({ gg <- g; function(r) f_piso(r) & gg(r) }))
  cat(sprintf("      + %-22s : %5.1f %%  (marginal %+5.1f pp)\n", nm, 100 * am, 100 * (am - a_qp)))
  if (am - a_qp >= 0.10)
    fail("El atajo '", nm, "' anade ", round(100 * (am - a_qp)),
         " pp SOBRE la deduccion parcial (", round(100 * am), " % absoluto)")
}
# TECHO NULO DE (L) — la calibracion que (K3) tenia y (L) NO (2026-08-15).
# `peor_l` es un MAXIMO sobre 19 reglas a N = 100: como en (K3), parte de su
# valor es SELECCION, no senal, y sin nulo no se distingue una de otra. Mismo
# protocolo que (K3): se permuta la clave y se toma el mejor de las 19 reglas.
#
# TRES CORRECCIONES (2026-08-15b), todas en la direccion SEVERA:
#   1. El nulo se calculaba sobre `reg` (CON canonicas) y se comparaba contra
#      `peor_l_sc`, que es SIN canonicas: nulo y observado salian de poblaciones
#      distintas. La politica D-5 dice que lo bloqueante opera sobre `reg_nc`, asi
#      que el nulo pasa a calcularse ahi tambien.
#   2. `max` sobre replicas -> `mean`: el maximo de un puñado de replicas
#      sobreestima el techo, y un techo alto REBAJA el exceso.
#   3. El veredicto pasa del 45 % ABSOLUTO al EXCESO, con los cortes del helper.
#      No es bajar la puerta: la medicion sobre 468 items oficiales mostro que la
#      tasa absoluta crece con el numero de reglas y por eso no es comparable —
#      este mismo bloque paso de 6 reglas a 19 sin que su umbral se moviera. El
#      45 % se sigue IMPRIMIENDO como referencia historica, pero no decide.
set.seed(20260815L)
nulos_l <- vapply(seq_len(K_PERM), function(i) {
  perm  <- sample.int(4L, length(reg_nc), replace = TRUE)
  reg_p <- lapply(seq_along(reg_nc), function(z) { r <- reg_nc[[z]]; r$j <- perm[z]; r })
  max(vapply(reglas_l, function(f) acierto(reg_p, f), numeric(1)))
}, numeric(1))
techo_nulo_l <- mean(nulos_l)
sd_l <- if (K_PERM > 1L) stats::sd(nulos_l) else NA_real_
peor_l_sc <- 0; peor_l_sc_nm <- ""
for (nm in names(reglas_l)) { a <- acierto(reg_nc, reglas_l[[nm]])
  if (a > peor_l_sc) { peor_l_sc <- a; peor_l_sc_nm <- nm } }
exceso_l <- peor_l_sc - techo_nulo_l
cat(sprintf("    techo NULO (L) sin canonicas (clave permutada, %d replicas, media sobre %d reglas) : %5.1f %%  (sd %.1f pp)\n",
            K_PERM, length(reglas_l), 100 * techo_nulo_l, 100 * sd_l))
cat(sprintf("    MAXIMO con canonicas %.1f %%  |  SIN canonicas %.1f %% (%s)\n",
            100 * peor_l, 100 * peor_l_sc, peor_l_sc_nm))
cat(sprintf("    EXCESO sin canonicas sobre el nulo : %+5.1f pp  (corte de canal %+.1f pp)\n",
            100 * exceso_l, 100 * CORTE_CANAL))
cat(sprintf("    [historico, NO decide] umbral absoluto 45 %% -> %s\n",
            if (peor_l_sc >= 0.45) "lo cruzaria" else "no lo cruzaria"))
if (!is.na(sd_l) && exceso_l >= CORTE_CANAL && exceso_l - K_SIGMA * sd_l <= CORTE_RUIDO) {
  warn("Una regla condicionada alcanza ", round(100 * peor_l_sc), " % sin canonicas, exceso ",
       round(100 * exceso_l), " pp, pero el ruido (sd ", round(100 * sd_l, 1),
       " pp) lo devuelve a la zona gris: NO CONCLUYENTE — ", peor_l_sc_nm)
  l_aceptado <- NA
} else if (exceso_l >= CORTE_CANAL) {
  l_aceptado <- residuo_ok("l_celda", "exceso", exceso_l, "(L) intra-celda")
  if (!l_aceptado)
    fail("Una regla condicionada alcanza ", round(100 * peor_l_sc),
         " % sin canonicas (techo nulo ", round(100 * techo_nulo_l),
         " %, exceso ", round(100 * exceso_l), " pp, corte ", round(100 * CORTE_CANAL),
         " pp), EXCEDE la linea base de residuo aceptado: ", peor_l_sc_nm)
} else {
  l_aceptado <- NA
}
if (exceso_l > CORTE_RUIDO && exceso_l < CORTE_CANAL) {
  warn("Una regla condicionada queda en ZONA GRIS: ", round(100 * peor_l_sc),
       " % sin canonicas, exceso ", round(100 * exceso_l), " pp — ", peor_l_sc_nm)
}
# O-11: el estadistico es un MAXIMO sobre ~19 reglas y a N=100 arrastra varios pp
# de ruido, asi que un veredicto decidido por 1-2 pp no es reproducible: reparticiones
# distintas de la misma muestra caen a los dos lados del umbral. Se DECLARA la
# incertidumbre en vez de subir N (la regla #23 fija N=100 y sus excepciones).
# O-11 reformulado sobre el EXCESO (2026-08-15): la incertidumbre ya no se mide
# contra un umbral absoluto sino contra los cortes, y su anchura NO es una
# constante inventada — es el `sd` del propio techo nulo medido en esta corrida.
if (!is.na(sd_l) && (abs(exceso_l - CORTE_CANAL) < K_SIGMA * sd_l ||
                     abs(exceso_l - CORTE_RUIDO) < K_SIGMA * sd_l))
  cat(sprintf("    [INCERTIDUMBRE] el exceso (%+.1f pp) esta a menos de %.1f sd del ruido\n      (sd %.1f pp) de uno de los cortes, y es un maximo sobre %d reglas: a N=%d el\n      veredicto de este bloque NO es reproducible tirada a tirada. Leer con el\n      marginal, no solo con el corte.\n",
              100 * exceso_l, K_SIGMA, 100 * sd_l, length(reglas_l), N_STD))
# Control positivo: sin el, un 33 % no distingue "sin senal" de "sonda muerta"
# (Incidente P / INC-MUTANTE-SONDA). Se fabrica un registro en que la clave es la
# UNICA de su celda; ahi SIGNO+BUCKET debe puntuar exactamente 100 %.
reg_ctrl_l <- lapply(reg, function(r) {
  r$sg <- rep(-1, length(r$sg)); r$sg[r$j] <- 1; r$sg_obj <- 1
  r$mobj <- 2; r$dy <- rep(1, length(r$dy)); r$dx <- rep(9, length(r$dx))
  r$dy[r$j] <- 9; r$dx[r$j] <- 1; r })
if (abs(acierto(reg_ctrl_l, f_cell) - 1) > 1e-9)
  fail("CONTROL POSITIVO ROTO: la sonda SIGNO+BUCKET no puntua 100 % una clave sola en su celda")
cat("    control positivo (clave sola en su celda): 100.0 % -> sonda viva\n")

# =============================================================================
# (K4) CIERRE CRUZADO (I)+(J)+(K) x (L) — anadido en la 4a pasada de la FASE 2C
#      a peticion del detractor independiente (objecion O-1).
#
#      EL AGUJERO QUE CIERRA. (K) enumeraba pares y (K3) tripletes, pero AMBOS
#      recorrian solo `reglas_k`. Las reglas INTRA-CELDA de (L) —las 29 que se
#      generan por producto cartesiano— se median UNICAMENTE como atomos y no
#      entraban en ninguna conjuncion. Es el mismo modo de fallo que motivo la
#      existencia de (L) —una bateria incompleta no mide "sin senal", mide SIN
#      SONDA (regla #22 §P7)— repetido un piso mas arriba: (L) se anadio con
#      umbral atomico propio y nadie le extendio el cierre.
#
#      MEDIDO por el detractor sobre 5 muestras independientes ANTES de existir
#      este bloque: el mejor triplete cruzado alcanza 67,0-72,8 %, es decir
#      +21,0 a +27,6 pp sobre la deduccion NECESARIA `q|dx AND |p||dy`, entre 2 y
#      3 veces el umbral de +10 pp que el propio bloque (L) aplica. La regla
#      ganadora es la misma en las cinco muestras:
#         celda + gcd(dx,dy)>1  AND  |p| | dy  AND  |dx| en q*(1..T)
#
#      EL VEREDICTO SE DA POR MARGINAL, no por valor absoluto: dos de los tres
#      componentes de esa terna son consecuencias necesarias de que la pendiente
#      sea p/q con coordenadas enteras, asi que un umbral absoluto mezclaria
#      matematica correcta con artefacto. El umbral es el mismo +10 pp de (L),
#      no uno nuevo, y el denominador es `a_qp`, calculado sin canonicas.
#      Politica D-5: todo este bloque opera sobre `reg_nc`.
# =============================================================================
cat("\n(K4) Cierre CRUZADO (I)+(J)+(K) x (L) (azar 25.0 %):\n")
reglas_x <- c(reglas_k, reglas_l)
nx  <- names(reglas_x)
MX  <- lapply(reglas_x, Smat)          # Smat ya opera sobre reg_nc (politica D-5)
parx <- utils::combn(length(reglas_x), 2L)
val_par_x <- function(jj) apply(parx, 2L, function(z) acierto_M(MX[[z[1]]] & MX[[z[2]]], jj))
res_px <- val_par_x(Jkey); op <- which.max(res_px)
cat(sprintf("    pares cruzados evaluados: %d  |  maximo %5.1f %%  (%s AND %s)\n",
            ncol(parx), 100 * max(res_px), nx[parx[1, op]], nx[parx[2, op]]))

# Tripletes cruzados: C(74,3) = 64.824. Se enumeran COMPLETOS, sin muestreo, para
# que el maximo sea el maximo y no una cota inferior con nombre de maximo.
tripx <- utils::combn(length(reglas_x), 3L)
val_trip_x <- function(jj) apply(tripx, 2L, function(z)
  acierto_M(MX[[z[1]]] & MX[[z[2]]] & MX[[z[3]]], jj))
res_tx <- val_trip_x(Jkey); ot <- order(res_tx, decreasing = TRUE)
cat(sprintf("    tripletes cruzados evaluados: %d  |  top 3:\n", ncol(tripx)))
for (k in ot[1:3])
  cat(sprintf("      %-22s AND %-22s AND %-22s : %5.1f %%\n",
              nx[tripx[1, k]], nx[tripx[2, k]], nx[tripx[3, k]], 100 * res_tx[k]))
peor_x <- max(res_tx)
# TECHO NULO DEL CIERRE CRUZADO — la laguna concreta que quedaba (2026-08-15).
# Habia dos defectos, y el segundo se comia al primero:
#   1. Se estimaba con `max` sobre 3 replicas. El maximo de un puñado de replicas
#      es un estimador SESGADO AL ALZA del techo: infla el nulo y por tanto REBAJA
#      el exceso, es decir, favorece al artefacto auditado. Pasa a `mean` sobre
#      K_PERM replicas, con su `sd` publicada — el mismo protocolo del helper §P7.
#   2. Se imprimia y NO DECIDIA NADA: el veredicto lo daba el marginal sobre la
#      deduccion necesaria. Un 67 % contra el azar del 25 % no significa lo mismo
#      que contra su propio techo, y este cierre es el mas ancho del archivo
#      (2701 pares + 64.824 tripletes), asi que su techo nulo es el mas alto.
# Ahora el exceso DECIDE, con los cortes leidos del helper (fuente unica).
set.seed(20260815L)
nulos_x <- vapply(seq_len(K_PERM), function(i)
  max(val_trip_x(sample.int(4L, length(reg_nc), replace = TRUE))), numeric(1))
nulo_x   <- mean(nulos_x)
sd_x     <- if (K_PERM > 1L) stats::sd(nulos_x) else NA_real_
exceso_x <- peor_x - nulo_x
marg_x   <- peor_x - a_qp
cat(sprintf("    maximo observado          : %5.1f %%\n", 100 * peor_x))
cat(sprintf("    techo NULO (clave permutada, %d replicas, media) : %5.1f %%  (sd %.1f pp, max %.1f %%)\n",
            K_PERM, 100 * nulo_x, 100 * sd_x, 100 * max(nulos_x)))
cat(sprintf("    EXCESO sobre el techo nulo               : %+5.1f pp  (corte de canal %+.1f pp)\n",
            100 * exceso_x, 100 * CORTE_CANAL))
cat(sprintf("    deduccion NECESARIA q|dx AND |p||dy      : %5.1f %%\n", 100 * a_qp))
cat(sprintf("    MARGINAL sobre la deduccion necesaria    : %+5.1f pp  (umbral 10 pp)\n",
            100 * marg_x))
cat("      Los dos criterios responden a preguntas DISTINTAS y ninguno sustituye al\n")
cat("      otro: el EXCESO dice si hay canal (existencia estadistica, comparable\n")
cat("      entre bloques y poblaciones); el MARGINAL dice cuanto anade el atajo a\n")
cat("      quien ya razona (pregunta pedagogica). Cualquiera de los dos puede\n")
cat("      RECHAZAR por su cuenta; ninguno absuelve de lo que dice el otro.\n")
# GATE 1 — EXISTENCIA DEL CANAL: exceso sobre el techo nulo, cortes del helper.
if (!is.na(sd_x) && exceso_x >= CORTE_CANAL && exceso_x - K_SIGMA * sd_x <= CORTE_RUIDO) {
  warn("El cierre CRUZADO alcanza ", round(100 * peor_x), " %, exceso ",
       round(100 * exceso_x), " pp, pero el ruido del techo nulo (sd ",
       round(100 * sd_x, 1), " pp) lo devuelve a la zona gris: NO CONCLUYENTE")
  k4x_aceptado <- NA
} else if (exceso_x >= CORTE_CANAL) {
  k4x_aceptado <- residuo_ok("k4_cruzado", "exceso", exceso_x,
                             "(K4) cierre cruzado, GATE 1 (existencia del canal)")
  if (!k4x_aceptado)
    fail("El cierre CRUZADO alcanza ", round(100 * peor_x), " %, ", round(100 * exceso_x),
         " pp SOBRE su techo nulo (", round(100 * nulo_x), " %; corte ",
         round(100 * CORTE_CANAL), " pp), EXCEDE la linea base de residuo aceptado: ",
         nx[tripx[1, ot[1]]], " AND ", nx[tripx[2, ot[1]]], " AND ", nx[tripx[3, ot[1]]])
} else {
  k4x_aceptado <- NA
  if (exceso_x > CORTE_RUIDO)
    warn("El cierre CRUZADO queda en ZONA GRIS: exceso ", round(100 * exceso_x),
         " pp sobre el techo nulo (", round(100 * nulo_x), " %) — ni cero medido ni canal")
}
# GATE 2 — APORTE PEDAGOGICO: marginal sobre la deduccion necesaria (independiente).
if (marg_x >= 0.10) {
  k4m_aceptado <- residuo_ok("k4_marginal", "marginal", marg_x,
                             "(K4) cierre cruzado, GATE 2 (aporte pedagogico)")
  if (!k4m_aceptado)
    fail("El cierre CRUZADO alcanza ", round(100 * peor_x), " %, ", round(100 * marg_x),
         " pp SOBRE la deduccion necesaria (", round(100 * a_qp), " %; techo nulo ",
         round(100 * nulo_x), " %), EXCEDE la linea base de residuo aceptado: ",
         nx[tripx[1, ot[1]]], " AND ", nx[tripx[2, ot[1]]], " AND ", nx[tripx[3, ot[1]]])
} else {
  k4m_aceptado <- NA
  if (marg_x >= 0.05)
    warn("El cierre CRUZADO alcanza ", round(100 * peor_x), " %, ", round(100 * marg_x),
         " pp sobre la deduccion necesaria")
}
# Control positivo: la regla omnisciente esta en `reglas_x`? NO — se inyecta a
# proposito, porque un cierre que no puntua 100 % una terna omnisciente esta
# muerto y su "sin hallazgos" no distinguiria de "sin sonda".
if (abs(acierto_M(Mo & Mo & Mo) - 1) > 1e-9)
  fail("CONTROL POSITIVO ROTO: el cierre cruzado no puntua 100 % una terna omnisciente")
cat("    control positivo (omnisciente x3): 100.0 % -> cierre cruzado vivo\n")

# =============================================================================
# (R) RESIDUO ACEPTADO — resumen, SIEMPRE impreso (decision del profesor,
#     FECHA_RESIDUO, ver comentario junto a RESIDUO_ACEPTADO al inicio del
#     archivo). Se imprime pase o falle cada bloque individual, para que la
#     cifra nunca dependa de haber entrado en la rama de fallo.
# =============================================================================
cat("\n(R) RESIDUO ACEPTADO — decision del profesor (", FECHA_RESIDUO, "):\n", sep = "")
cat("    linea base = medicion del commit 56849d565 (2026-08-15) + tolerancia ",
    sprintf("%.1f pp\n", 100 * TOL_RESIDUO), sep = "")
imprimir_residuo_resumen <- function(clave, campo, base, valor_actual, aceptado) {
  cat(sprintf("      %-11s : base %+5.1f pp  tope %+5.1f pp  medido %+5.1f pp  -> %s\n",
              clave, 100 * base, 100 * (base + TOL_RESIDUO), 100 * valor_actual,
              if (is.na(aceptado)) "no alcanzo el corte de canal (no se evalua)"
              else if (aceptado) "RESIDUO ACEPTADO"
              else "EXCEDE LO ACEPTADO"))
}
imprimir_residuo_resumen("l_celda",     "exceso",   RESIDUO_ACEPTADO$l_celda$exceso,     exceso_l, l_aceptado)
imprimir_residuo_resumen("k3_triplete", "exceso",   RESIDUO_ACEPTADO$k3_triplete$exceso, exceso_k3, k3_aceptado)
imprimir_residuo_resumen("k4_cruzado",  "exceso",   RESIDUO_ACEPTADO$k4_cruzado$exceso,  exceso_x, k4x_aceptado)
imprimir_residuo_resumen("k4_marginal", "marginal", RESIDUO_ACEPTADO$k4_marginal$marginal, marg_x, k4m_aceptado)
# (K) tiene ENTRADA PROPIA en RESIDUO_ACEPTADO desde el 2026-08-16: pasa de
# umbral absoluto a criterio por exceso y se juzga contra SU PROPIA linea base,
# no en cascada desde (K3). Ver el comentario de `k_pares` en RESIDUO_ACEPTADO.
cat(sprintf("      %-11s : base %+5.1f pp  tope %+5.1f pp  medido %+5.1f pp  -> %s\n",
            "k_pares", 100 * RESIDUO_ACEPTADO$k_pares$exceso,
            100 * (RESIDUO_ACEPTADO$k_pares$exceso + TOL_RESIDUO), 100 * exceso_k,
            if (is.na(k_aceptado)) "no alcanzo el corte de canal (no se evalua)"
            else if (k_aceptado) "RESIDUO ACEPTADO"
            else "EXCEDE LO ACEPTADO"))
cat("    'no alcanzo el corte de canal' es BUENA noticia (mejor que la linea base),\n")
cat("    no una casilla vacia: significa que esa corrida no necesito el residuo.\n")
cat("    §P7 atomico (91 reglas) se mide en bateria_p7.R, PROCESO SEPARADO: ejecutar\n")
cat("    ese script para ver su propia comparacion contra la misma linea base.\n")
cat("    Bloques que NO entran en NINGUN residuo (si fallan, BLOQUEAN sin excepcion):\n")
cat("    (A)-(H), (I) atomico simple (umbral absoluto, deuda declarada y NO cerrada),\n")
cat("    (J) signo, (M)-(N).\n")

# --- (I)/(J)/(K)/(L) SIN la instancia canonica -------------------------------
# La canonica reproduce el item oficial y no es corregible, pero pesa 1/12 y
# arrastra los agregados. Se publican LAS DOS cifras para no confundir una
# propiedad del ITEM OFICIAL con una del generador.
# `reg_nc` se define arriba, junto a la politica de canonicas (D-5). Aqui solo se
# publica el resumen; los checks BLOQUEANTES ya se aplicaron sobre el en cada bloque.
if (length(reg_nc) > 0L && length(reg_nc) < length(reg)) {
  cat(sprintf("\n    [sin canonicas, n = %d de %d]\n", length(reg_nc), length(reg)))
  cat(sprintf("      SOLO-SIGNO                 : %5.1f %%\n",
              100 * acierto(reg_nc, function(r) r$sg == r$sg_obj)))
  cat(sprintf("      SIGNO+BUCKET (2 bits)      : %5.1f %%\n",
              100 * acierto(reg_nc, f_cell)))
  peor_nc <- max(vapply(reglas, function(f) acierto(reg_nc, f), numeric(1)))
  cat(sprintf("      peor regla geometrica      : %5.1f %%\n", 100 * peor_nc))
  peor_lnc <- max(vapply(reglas_l, function(f) acierto(reg_nc, f), numeric(1)))
  cat(sprintf("      peor regla condicionada    : %5.1f %%\n", 100 * peor_lnc))
  peor_knc <- max(apply(pares, 2L, function(ij)
    acierto(reg_nc, function(r) reglas_k[[ij[1]]](r) & reglas_k[[ij[2]]](r))))
  cat(sprintf("      peor conjuncion de dos     : %5.1f %%\n", 100 * peor_knc))
  cat("    La canonica NO es corregible: reproduce el item oficial verbatim, y en\n")
  cat("    el la clave es la unica de su celda. Es la fuente del residuo del\n")
  cat("    agregado; las cifras 'sin canonicas' son las del GENERADOR.\n")
}

# =============================================================================
# (M) DOS PROPIEDADES DEL GENERADOR que el resto de bloques no publica.
#     No son sondas de diagnosticidad: son cifras que estaban afirmadas en
#     comentarios del .Rmd sin salir impresas en ninguna parte.
# =============================================================================
cat("\n(M) Propiedades del generador:\n")
p_pos    <- mean(vapply(reg,    function(r) r$p > 0, logical(1)))
p_pos_nc <- mean(vapply(reg_nc, function(r) r$p > 0, logical(1)))
cat(sprintf("    P(pendiente objetivo > 0) : %5.1f %%  (sin canonicas %5.1f %%)\n",
            100 * p_pos, 100 * p_pos_nc))
cat("      La canonica tiene m = -2 fija y pesa 1/12, asi que arrastra el\n")
cat("      agregado unos 4 pp hacia el signo negativo. El sesgo pedagogicamente\n")
cat("      relevante es el del generador (columna sin canonicas).\n")
its <- vapply(reg_nc, function(r) r$intento, integer(1))
its <- its[!is.na(its)]
if (length(its)) {
  cat(sprintf("    intentos de configuracion : media %.2f, max %d, RANGO_LIBRE = %d\n",
              mean(its), max(its), e0$RANGO_LIBRE))
  n_libre <- sum(its > e0$RANGO_LIBRE)
  cat(sprintf("      versiones que llegaron a soltar el equilibrio de rango: %d de %d\n",
              n_libre, length(its)))
  if (n_libre == 0L)
    cat("      -> la rama RANGO_LIBRE NO se ejercita; es una garantia de termino,\n",
        "         no un camino vivo. Declarada, no verificada por uso.\n", sep = "")
}

# =============================================================================
cat("\n(E) Reparto de los errores conceptuales usados:\n")
cods <- unlist(lapply(reg, function(r) r$codigos[r$codigos != "CORRECTA"]))
te <- sort(table(cods), decreasing = TRUE)
for (nm in names(te)) cat(sprintf("    %-12s : %3d (%.1f %%)\n", nm, te[[nm]], 100 * te[[nm]] / length(cods)))
if (length(te) < 4L) fail("Menos de 4 errores distintos en N=", N_STD)

# =============================================================================
cat("\n(F) Pase ESTRATIFICADO por pendiente objetivo (N = ", N_STRAT, "):\n", sep = "")
regs <- recolectar(100000L + seq_len(N_STRAT) * 31L)
clave_pend <- vapply(regs, function(r) paste0(r$p, "/", r$q), character(1))
tm <- table(clave_pend); cortos <- character(0)
# M-3: este bloque NO excluia las instancias canonicas, mientras (I)/(J)/(K)/(L)
# si publican las dos cifras. La canonica es deterministica (m = -2/1, H = (1,-2),
# puntos fijos) y cae ENTERA en el estrato -2/1, de modo que ahi mezclaba una
# propiedad del ITEM OFICIAL con una del generador. Ahora se imprimen las dos
# lineas y los checks BLOQUEANTES se aplican al subconjunto SIN canonicas, que es
# lo unico corregible; si el agregado cruza un umbral que el generador no cruza,
# se emite un aviso nombrando a la canonica como causa.
metricas <- function(sub) {
  n <- length(sub)
  if (n == 0L) return(NULL)
  cu <- vapply(sub, function(r) { if (r$kx == 0 || r$ky == 0) "eje"
    else if (r$kx > 0 && r$ky > 0) "I" else if (r$kx < 0 && r$ky > 0) "II"
    else if (r$kx < 0 && r$ky < 0) "III" else "IV" }, character(1))
  list(n = n,
       a   = mean(vapply(sub, function(r) r$mas_larga, logical(1))),
       b   = mean(vapply(sub, function(r) r$mas_corta, logical(1))),
       dom = max(table(cu)) / n,
       r4  = mean(vapply(sub, function(r) rank(r$dx, ties.method = "min")[r$j] == 4, logical(1))))
}
cat("      p/q    n   mas-larga  mas-corta  region-dom  rango4-dx  estado\n")
for (nm in names(tm)) {
  sub    <- regs[clave_pend == nm]
  es_can <- vapply(sub, function(r) isTRUE(r$canon), logical(1))
  M  <- metricas(sub)
  Mn <- metricas(sub[!es_can])
  est <- if (M$n < MIN_ESTRATO) { cortos <- c(cortos, nm); "NO CONCLUYENTE" } else "medible"
  cat(sprintf("    %6s %4d   %6.1f %%   %6.1f %%    %6.1f %%   %6.1f %%   %s\n",
              nm, M$n, 100*M$a, 100*M$b, 100*M$dom, 100*M$r4, est))
  if (any(es_can)) {
    if (is.null(Mn)) {
      cat("           (estrato compuesto SOLO por instancias canonicas)\n")
    } else {
      cat(sprintf("    %6s %4d   %6.1f %%   %6.1f %%    %6.1f %%   %6.1f %%   sin canonicas (%d retiradas)\n",
                  "", Mn$n, 100*Mn$a, 100*Mn$b, 100*Mn$dom, 100*Mn$r4, sum(es_can)))
    }
  }
  # Los checks bloqueantes miran el GENERADOR (sin canonicas); si no queda
  # muestra sin canonicas, se cae al agregado y se declara.
  Mc <- if (!is.null(Mn) && Mn$n >= MIN_ESTRATO) Mn else M
  if (Mc$n >= MIN_ESTRATO) {
    if (Mc$a >= 0.90 || Mc$b >= 0.90) fail("Estrato ", nm, ": clave identificable por longitud >=90 %")
    if (Mc$dom >= 0.90) fail("Estrato ", nm, ": clave en la misma region >=90 %")
    if (!is.null(Mn) && M$n >= MIN_ESTRATO &&
        (M$dom >= 0.90 || M$a >= 0.90 || M$b >= 0.90) &&
        !(Mc$dom >= 0.90 || Mc$a >= 0.90 || Mc$b >= 0.90))
      warn("Estrato ", nm, ": el AGREGADO cruza un umbral que el generador no cruza; ",
           "la causa son las ", sum(es_can), " instancias canonicas, que no son corregibles.")
  }
}
if (length(cortos) > 0) {
  n_nec <- ceiling(N_STRAT * MIN_ESTRATO / min(unlist(tm)))
  cat("    -> estratos con n<20: ", paste(cortos, collapse = ", "), " | N necesario: ", n_nec, "\n", sep = "")
  warn("Estratos por pendiente con n<20: ", paste(cortos, collapse = ", "),
       " -> NO CONCLUYENTE individualmente (N necesario = ", n_nec, "). ",
       "El agregado de (C)(D)(I) sobre N=", N_STD, " y N=", N_STRAT, " si es concluyente.")
}
n_canon <- sum(vapply(regs, function(r) isTRUE(r$canon), logical(1)))
cat(sprintf("    canonica %4d   (deterministica: se verifica por igualdad exacta en (G))\n", n_canon))

# =============================================================================
cat("\n(G) Instancia canonica (ERA-2026-S2-P43 = MAT-2026-1-129) — igualdad exacta:\n")
hallada <- FALSE
for (sm in 1:400) {
  e <- correr(sm)
  if (isTRUE(e$es_canonica)) {
    esp <- sort(c("$(0,\\, {0})$", "$(-4,\\, {0})$", "$(-1,\\, {-1})$", "$(6,\\, {5})$"))
    ok_op <- identical(esp, sort(e$opciones))
    ok_cl <- identical(e$opciones[which(e$sol == 1L)], "$(0,\\, {0})$")
    ok_pa <- (e$p_m == -2L && e$q_m == 1L && e$hx == 1L && e$hy == -2L)
    cat("    semilla ", sm, ": opciones oficiales=", ok_op, " clave (0,0)=", ok_cl,
        " m=-2,H=(1,-2)=", ok_pa, "\n", sep = "")
    if (!(ok_op && ok_cl && ok_pa)) fail("La instancia canonica no reproduce el item oficial")
    # --- Atribucion de codigos en la canonica: GENERABLE o CONCEPTUAL ---------
    # Los tres distractores de la canonica son los del cuadernillo oficial, no
    # salen de `calcula()`. Se comprueba, error por error, si el codigo atribuido
    # PODRIA haber generado ese punto con algun t y algun k04 admisibles. El
    # resultado se IMPRIME en vez de asumirse: parte de las atribuciones son
    # CONCEPTUALES (el error que el distractor ilustra) y no generativas, y eso
    # tiene que estar a la vista, no escondido en un comentario.
    cat("    atribucion de codigos (el punto es generable por SU codigo?):\n")
    for (jj in seq_along(e$opciones)) {
      if (e$sol[jj] == 1L) next
      cod <- e$codigos[jj]; pz <- e$puntos[[jj]]
      err <- errores_conceptuales[[which(vapply(errores_conceptuales,
                                   function(x) x$codigo == cod, logical(1)))]]
      gen <- FALSE
      for (k04 in c(2L, 3L, 5L)) for (t in CAND_T) {
        d <- err$calcula(e$hx, e$hy, e$p_m, e$q_m, t, k04)
        if (d[1] != 0 && all(c(e$hx + d[1], e$hy + d[2]) == pz)) gen <- TRUE
      }
      cat(sprintf("      (%d, %d) -> %s : %s\n", pz[1], pz[2], cod,
                  if (gen) "GENERABLE" else "CONCEPTUAL (no generable con estos parametros)"))
    }
    hallada <- TRUE; break
  }
}
if (!hallada) fail("No se alcanzo la instancia canonica en 400 semillas")
# Contrapartida: en las versiones NO canonicas la atribucion es generativa por
# construccion (el punto sale de `calcula()` del error atribuido). Se comprueba.
n_mal <- 0L
for (r in reg_nc) {
  for (jj in seq_along(r$codigos)) {
    if (r$codigos[jj] == "CORRECTA") next
    if (!(r$codigos[jj] %in% vapply(errores_conceptuales, function(x) x$codigo, character(1))))
      n_mal <- n_mal + 1L
  }
}
cat("    versiones no canonicas con un codigo fuera del pool: ", n_mal, "\n", sep = "")
if (n_mal > 0L) fail("Hay ", n_mal, " distractores con un codigo que no existe en el pool")

# =============================================================================
cat("\n(H) Texto emitido — contracciones y dobles espacios:\n")
textos <- unlist(lapply(regs, function(r) r$texto))
for (pat in c("de el ", "a el ", "de la la ", "en el el ", "  ")) {
  k <- sum(grepl(pat, textos, fixed = TRUE))
  cat(sprintf("    %-12s : %3d\n", paste0("'", pat, "'"), k))
  if (k > 0) fail("Texto emitido contiene '", pat, "' (", k, " veces)")
}
if (!grepl("de el ", "copio de el tablero", fixed = TRUE))
  fail("CONTROL POSITIVO ROTO: la sonda de contracciones no dispara")
cat("    control positivo ('copio de el tablero'): DISPARA -> sonda viva\n")
cat("    textos distintos: ", length(unique(textos)), "\n", sep = "")

# =============================================================================
cat("\n=============================================================\n")
if (length(avisos)) { cat("AVISOS:\n"); for (a in avisos) cat("  - ", a, "\n", sep = "") }
if (length(errores)) {
  cat("RESULTADO: RECHAZADO (", length(errores), " errores)\n", sep = "")
  for (x in unique(errores)) cat("  * ", x, "\n", sep = "")
  quit(status = 1)
}
cat("RESULTADO: APROBADO (0 errores)\n")
cat("=============================================================\n")
