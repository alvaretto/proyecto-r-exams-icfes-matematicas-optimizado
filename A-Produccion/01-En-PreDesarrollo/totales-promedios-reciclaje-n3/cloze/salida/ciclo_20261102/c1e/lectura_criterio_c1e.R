## lectura_criterio_c1e.R — tanda C1e (2026-11-03). Solo lee. Semillas 1 a 1000, data_generation y chunk solucion de e16842c1.
## Adaptación de c1d/lectura_criterio_c1d.R. La frase nueva nombra dos hipótesis: «si el grado que perdió habría ganado con un curso más»
## (HP) y «si el que ganó habría perdido con uno menos» (HG). El alcance literal son esas dos, nada más.
##   (1) Ninguna afirmación de ventaja por cursos (BC) entra en el alcance. Solo entraría si dijera lo mismo que HP o HG: BC_perdio con
##       nG - nP = 1 (el que perdió suma un curso) o BC_gano con nG - nP = 1 (el que ganó pierde uno). Se cuenta, no se supone.
##   (2) BC fuera del alcance, como registro: las cifras de la C1c y la C1d (495, 421, 273, 238, 222 y 183).
##   (3) HP y HG: en las 1000, el criterio, el veredicto calculado desde la tabla y la marca de sol_p6 coinciden. Empates: se cuentan.
##       Control positivo: con el promedio del otro grado, HP se aparta en 306 de 1000.
##   (4) Control contra la r5: semillas 1 a 205, 102 BC y 80 con el veredicto contrario, sin acotar.
##   (5) Lecturas de la frase nueva, medidas y no corregidas (la frase es literal del profesor):
##       a. «de cada uno» cruzado: cada hipótesis con el promedio del otro grado;
##       b. «de cada uno» + «si el promedio» en singular: un solo veredicto para las dos preguntas;
##       c. «sí» y «no» como respuesta a cada una de las dos preguntas: cuatro celdas, contra la tabla, la marca y la frase resuelta;
##       d. «si el promedio es mayor» leído como «el de mayor promedio», comparando los dos promedios entre sí;
##       e. «con un curso más» y «con uno menos» leídos como comparación con el otro grado de la tabla, y no como un curso añadido o quitado;
##       f. «la diferencia entre los totales» leída como la que queda después del cambio, no la de la tabla.
## Uso: cd cloze/salida && Rscript --no-init-file ciclo_20261102/c1e/lectura_criterio_c1e.R
suppressMessages(library(exams))
F <- normalizePath("../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
MD5 <- unname(tools::md5sum(F)); cat("md5 del .Rmd:", MD5, "\n"); stopifnot(MD5 == "e16842c1157b3bffe35b8f21682ed6ad")
F_NUEVA <- "Por eso, para saber si el grado que perdió habría ganado con un curso más, o si el que ganó habría perdido con uno menos, basta comparar el promedio de cada uno con la diferencia entre los totales: si el promedio es mayor, sí; si es menor, no."
l <- readLines(F, encoding = "UTF-8")
trozo <- function(nm) { a <- grep(paste0("^```\\{r ", nm, "[,}]"), l)[1]; b <- which(l == "```" & seq_along(l) > a)[1]; paste(l[(a + 1):(b - 1)], collapse = "\n") }
EX <- parse(text = trozo("data_generation"), encoding = "UTF-8"); SOL <- parse(text = trozo("solucion"), encoding = "UTF-8")
fila <- function(s) { set.seed(s); e <- new.env(); eval(EX, envir = e); d <- e$dp; id <- e$c6$id
  TP <- d$nP * d$aP; TG <- d$nG * d$aG; D <- TG - TP
  stopifnot(D == d$D, D > 0)
  marca <- function(r) isTRUE(e$sol_p6[e$rol_p6 == r])
  pr <- regmatches(t0 <- paste(capture.output(eval(SOL, envir = new.env(parent = e))), collapse = "\n"), regexpr("Parte 6\\. Cada hipótesis[^\\n]*", t0, perl = TRUE))
  stopifnot(length(pr) == 1L); pr <- gsub(intToUtf8(160L), " ", pr, fixed = TRUE)
  res <- function(x) grepl(x, pr, fixed = TRUE)
  data.frame(s = s, rama = e$rama_par, id = id, tipo = substr(id, 1, 2), perd = grepl("perdio$", id), k = abs(d$nG - d$nP), dif = d$nG - d$nP,
             nP = d$nP, nG = d$nG, aP = d$aP, aG = d$aG, TP = TP, TG = TG, D = D, v_c = e$v_c, real_tabla = d$aP > d$aG,
             hp_real = TP + d$aP > TG, hg_real = TG - d$aG < TP, hp_marca = marca("hp"), hg_marca = marca("hg"),
             hp_emp = TP + d$aP == TG, hg_emp = TG - d$aG == TP, canon = isTRUE(e$es_canonica),
             lit = lengths(regmatches(pr, gregexpr(F_NUEVA, pr, fixed = TRUE))),
             hp_si = res(": habría ganado."), hp_no = res(": no le habría alcanzado para ganar."),
             hg_si = res(": habría perdido."), hg_no = res(": habría seguido ganando."),
             dif_cita = lengths(regmatches(pr, gregexpr(paste0("que la diferencia de ", D, " kilos"), pr, fixed = TRUE))), stringsAsFactors = FALSE) }
T <- do.call(rbind, lapply(1:1000, fila))
cat(sprintf("Semillas 1 a 1000 (%d canónicas). Afirmación de ventaja: BP %d · BC %d\n", sum(T$canon), sum(T$tipo == "BP"), sum(T$tipo == "BC")))
cat(sprintf("La frase literal, una vez por párrafo emitido: %d de 1000\n", sum(T$lit == 1L))); stopifnot(all(T$lit == 1L))
B <- T[T$tipo == "BC", ]
stopifnot("v_c coincide con el veredicto calculado desde la tabla" = identical(B$v_c, B$real_tabla))
B$crit <- ifelse(B$perd, B$aP > B$D, B$aG > B$D)                      # el criterio aplicado al grado que nombra la afirmación
B$dentro <- B$dif == 1L                                                # BC_perdio: el que perdió, un curso más; BC_gano: el que ganó, uno menos
B$sentido <- ifelse(B$dif > 0, "previsto", "inverso")
## (1)
cat(sprintf("\n(1) BC que dirían lo mismo que HP o HG (dentro del alcance literal, las dos hipótesis que nombra la frase): %d versiones · el criterio da el veredicto contrario en %d\n",
            sum(B$dentro), sum(B$dentro & B$crit != B$v_c)))
stopifnot("ninguna BC dentro del alcance" = sum(B$dentro) == 0L)
## Por qué es 0: con k = 1 y el ganador con más cursos, tantos cursos como el otro ES un curso más o uno menos, así que HP y HG serían la misma
## afirmación, ambas verdaderas, y L-C1 exige una falsa: la de ventaja sale BP. Se comprueba, no se supone.
K1C <- T[T$rama == "CURSOS" & T$k == 1L, ]
cat(sprintf("    Comprobación: rama CURSOS con k = 1: %d versiones; afirmación de ventaja BP en %d; HP y HG verdaderas en %d\n",
            nrow(K1C), sum(K1C$tipo == "BP"), sum(K1C$hp_real & K1C$hg_real)))
cat(sprintf("    BP cambia el promedio, no los cursos: tampoco la nombra la frase (%d versiones)\n", sum(T$tipo == "BP")))
## (2)
K <- B[B$k == 1L, ]
cat(sprintf("\n(2) BC fuera del alcance, como registro: %d\n", sum(!B$dentro)))
cat(sprintf("    k >= 2: %d · el criterio, aplicado igual, da el contrario en %d\n", sum(B$k >= 2L), sum(B$k >= 2L & B$crit != B$v_c)))
for (se in c("previsto", "inverso")) { X <- K[K$sentido == se, ]
  cat(sprintf("    k = 1, sentido %s: %d · da el veredicto real en %d · da el contrario en %d\n", se, nrow(X), sum(X$crit == X$v_c), sum(X$crit != X$v_c))) }
cat(sprintf("    Sin acotar (frase de 6f6f1b13), aplicado a toda BC: contrario en %d de %d\n", sum(B$crit != B$v_c), nrow(B)))
stopifnot("cifras de la C1c y la C1d" = nrow(B) == 495L, sum(B$crit != B$v_c) == 421L, sum(B$k >= 2L) == 273L, sum(B$k >= 2L & B$crit != B$v_c) == 238L,
          nrow(K) == 222L, sum(K$crit != K$v_c) == 183L)
cat("    Control: las seis cifras de la C1c y la C1d (495, 421, 273, 238, 222 y 183), reproducidas\n")
## (3)
crit_hp <- T$aP > T$D; crit_hg <- T$aG > T$D
cat(sprintf("\n(3) HP: el criterio coincide con el veredicto calculado desde la tabla en %d de 1000 y con la marca en %d de 1000\n",
            sum(crit_hp == T$hp_real), sum(crit_hp == T$hp_marca)))
cat(sprintf("    HG: el criterio coincide con el veredicto calculado desde la tabla en %d de 1000 y con la marca en %d de 1000\n",
            sum(crit_hg == T$hg_real), sum(crit_hg == T$hg_marca)))
cat(sprintf("    Empates (promedio = diferencia, que «si es mayor» y «si es menor» no cubrirían): HP %d · HG %d\n", sum(T$hp_emp), sum(T$hg_emp)))
stopifnot("HP y HG, en las 1000" = all(crit_hp == T$hp_real), all(crit_hg == T$hg_real), all(crit_hp == T$hp_marca), all(crit_hg == T$hg_marca),
          "sin empates" = sum(T$hp_emp) == 0L, sum(T$hg_emp) == 0L)
## Control positivo de la sonda de (3): un criterio con el grado cambiado sí se aparta.
cat(sprintf("    Control positivo: con el promedio del otro grado, HP se aparta del veredicto real en %d de 1000\n", sum((T$aG > T$D) != T$hp_real)))
stopifnot("control positivo" = sum((T$aG > T$D) != T$hp_real) == 306L)
## (4)
B205 <- B[B$s <= 205L, ]
cat(sprintf("\n(4) Control contra la r5, semillas 1 a 205: %d BC, y el criterio sin acotar da el contrario en %d (la r5 publicó 102 y 80).\n", nrow(B205), sum(B205$crit != B205$v_c)))
stopifnot(nrow(B205) == 102L, sum(B205$crit != B205$v_c) == 80L)
## (5) Lecturas de la frase nueva
cat("\n(5) Lecturas de la frase nueva, medidas y no corregidas (frase literal del profesor)\n")
cat(sprintf("  a. «de cada uno» cruzado, cada hipótesis con el promedio del otro grado: HP sale contraria en %d de 1000 y HG en %d de 1000.\n",
            sum((T$aG > T$D) != T$hp_real), sum((T$aP > T$D) != T$hg_real)))
dist <- T$hp_real != T$hg_real
cat(sprintf("  b. Un solo veredicto para las dos preguntas: HP y HG tienen veredictos distintos en %d de 1000 (ahí ningún «sí» o «no» común es verdadero para las dos). Dos verdaderas: %d · dos falsas: %d.\n",
            sum(dist), sum(T$hp_real & T$hg_real), sum(!T$hp_real & !T$hg_real)))
cat(sprintf("     Las dos frases resueltas responden por separado: conclusión de HP y de HG, una de cada una, en %d de 1000.\n",
            sum((T$hp_si != T$hp_no) & (T$hg_si != T$hg_no))))
celda <- function(nom, crit, real, marca, res_si, res_no, si, no) {
  for (v in c(TRUE, FALSE)) { i <- crit == v
    cat(sprintf("     %s, «%s» (promedio %s que la diferencia): %d versiones · veredicto de la tabla: «%s» en %d · marca: «%s» en %d · frase resuelta «%s» en %d\n",
                nom, if (v) "sí" else "no", if (v) "mayor" else "menor", sum(i), if (v) si else no, sum(real[i] == v), if (v) "verdadera" else "falsa", sum(marca[i] == v),
                if (v) si else no, sum((if (v) res_si else res_no)[i]))) } }
cat("  c. «sí» y «no» como respuesta a cada pregunta:\n")
celda("HP", crit_hp, T$hp_real, T$hp_marca, T$hp_si, T$hp_no, "habría ganado", "no le habría alcanzado para ganar")
celda("HG", crit_hg, T$hg_real, T$hg_marca, T$hg_si, T$hg_no, "habría perdido", "habría seguido ganando")
stopifnot("c: la frase resuelta dice lo mismo que el criterio" = all(T$hp_si == crit_hp), all(T$hp_no == !crit_hp), all(T$hg_si == crit_hg), all(T$hg_no == !crit_hg))
cat(sprintf("     En las dos preguntas «sí» es que el ganador cambia: HP sí y P supera a G en %d de %d; HG sí y G queda por debajo de P en %d de %d.\n",
            sum(crit_hp & (T$TP + T$aP > T$TG)), sum(crit_hp), sum(crit_hg & (T$TG - T$aG < T$TP)), sum(crit_hg)))
cat(sprintf("  d. «si el promedio es mayor» como «el de mayor promedio» (los dos promedios entre sí): HP sale contraria en %d de 1000 y HG en %d de 1000.\n",
            sum((T$aP > T$aG) != T$hp_real), sum((T$aG > T$aP) != T$hg_real)))
CMP <- T[T$dif == -1L, ]                                               # la tabla muestra al que perdió con exactamente un curso más que el que ganó
cat(sprintf("  e. «con un curso más» como comparación con el otro grado de la tabla: el que perdió ya tiene un curso más que el que ganó (y el que ganó, uno menos) en %d de 1000 (todas de la rama %s).\n",
            nrow(CMP), paste(unique(CMP$rama), collapse = "/")))
cat(sprintf("     Quien lea ahí «ya lo tenía y perdió», responde «no»; el veredicto de la hipótesis es verdadero en HP %d y en HG %d de esas %d. Con la frase anterior era «tiene un curso más»; ahora es condicional. De esas %d, %d traen una BC en sentido inverso (fuera del alcance).\n",
            sum(CMP$hp_real), sum(CMP$hg_real), nrow(CMP), nrow(CMP), sum(CMP$tipo == "BC")))
hp_otro <- (T$nG + 1L) * T$aP > T$TG; hg_otro <- (T$nP - 1L) * T$aG < T$TP
cat(sprintf("     «Un curso más que el otro grado» y «uno menos que el otro grado», calculado para toda versión (no un curso añadido): HP sale contraria en %d de 1000 y HG en %d de 1000.\n",
            sum(hp_otro != T$hp_real), sum(hg_otro != T$hg_real)))
cat(sprintf("  f. «la diferencia entre los totales» como la que queda después del cambio (D - promedio): HP sale contraria en %d de 1000 y HG en %d de 1000. Las dos frases resueltas citan la diferencia de la tabla, dos veces, en %d de 1000.\n",
            sum((T$aP > T$D - T$aP) != T$hp_real), sum((T$aG > T$D - T$aG) != T$hg_real), sum(T$dif_cita == 2L)))
## Cruces, como en la C1d
cat("\nRama × k (todas las versiones):\n"); print(table(T$rama, pmin(T$k, 3), dnn = c("rama", "k (3 = 3 o más)")))
cat("\nRama × tipo de afirmación de ventaja × marca:\n"); print(ftable(table(T$rama, T$id, T$v_c, dnn = c("rama", "id", "marca"))))
