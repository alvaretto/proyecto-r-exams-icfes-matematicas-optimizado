## lectura_criterio_c1d.R — tanda C1d (2026-11-03). Solo lee. Semillas 1 a 1000, data_generation de 32243d0a.
## Adaptación de c1c/lectura_criterio_c1c.R. La frase nueva acota el criterio a «si el grado que perdió tiene un curso más o el que
## ganó tiene uno menos». El alcance literal es, entonces: el que perdió con un curso más o el que ganó con uno menos.
##   (1) Afirmación de ventaja por cursos (BC) DENTRO de ese alcance: BC_perdio con nG - nP = 1 (el que perdió suma un curso) o BC_gano
##       con nG - nP = 1 (el que ganó pierde uno). Cuántas hay y en cuántas el criterio da el veredicto contrario.
##   (2) BC FUERA del alcance: k >= 2, y k = 1 en sentido inverso (el que perdió pierde un curso o el que ganó suma uno, rama PROMEDIO).
##       El control exige las cifras de la C1c: 495 BC, 421 sin acotar, 273 y 238 con k >= 2, 222 y 183 con k = 1.
##   (3) HP y HG, que sí están dentro: en las 1000, el criterio, el veredicto calculado desde la tabla y la marca de sol_p6 coinciden.
##       Empates (promedio = diferencia, «ni mayor ni menor»): se cuentan.
##   (4) Control contra la r5: semillas 1 a 205, 102 BC y 80 con el veredicto contrario, sin acotar.
##   (5) Dos lecturas de la frase nueva que el encargo no pedía, medidas para la r6, no corregidas (la frase es literal del profesor):
##       a. «ese grado» tomado como el último nombrado, «el que ganó», también para HP;
##       b. «tiene un curso más» leído como comparación con el otro grado de la tabla, y no como un curso añadido.
suppressMessages(library(exams))
F <- normalizePath("../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
MD5 <- unname(tools::md5sum(F)); cat("md5 del .Rmd:", MD5, "\n"); stopifnot(MD5 == "32243d0a6e6c499a4c6b1213a49e95cd")
l <- readLines(F, encoding = "UTF-8"); a <- grep("^```\\{r data_generation", l)[1]; b <- which(l == "```" & seq_along(l) > a)[1]
EX <- parse(text = paste(l[(a + 1):(b - 1)], collapse = "\n"), encoding = "UTF-8")
fila <- function(s) { set.seed(s); e <- new.env(); eval(EX, envir = e); d <- e$dp; id <- e$c6$id
  TP <- d$nP * d$aP; TG <- d$nG * d$aG; D <- TG - TP
  stopifnot(D == d$D, D > 0)
  marca <- function(r) isTRUE(e$sol_p6[e$rol_p6 == r])
  data.frame(s = s, rama = e$rama_par, id = id, tipo = substr(id, 1, 2), perd = grepl("perdio$", id), k = abs(d$nG - d$nP), dif = d$nG - d$nP,
             aP = d$aP, aG = d$aG, D = D, v_c = e$v_c, real_tabla = d$aP > d$aG,
             hp_real = TP + d$aP > TG, hg_real = TG - d$aG < TP, hp_marca = marca("hp"), hg_marca = marca("hg"),
             hp_emp = TP + d$aP == TG, hg_emp = TG - d$aG == TP, canon = isTRUE(e$es_canonica), stringsAsFactors = FALSE) }
T <- do.call(rbind, lapply(1:1000, fila))
cat(sprintf("Semillas 1 a 1000 (%d canónicas). Afirmación de ventaja: BP %d · BC %d\n", sum(T$canon), sum(T$tipo == "BP"), sum(T$tipo == "BC")))
B <- T[T$tipo == "BC", ]
stopifnot("v_c coincide con el veredicto calculado desde la tabla" = identical(B$v_c, B$real_tabla))
B$crit <- ifelse(B$perd, B$aP > B$D, B$aG > B$D)                      # el criterio aplicado al grado que nombra la afirmación
B$dentro <- B$dif == 1L                                                # BC_perdio: el que perdió, un curso más; BC_gano: el que ganó, uno menos
B$sentido <- ifelse(B$dif > 0, "previsto", "inverso")
## (1)
cat(sprintf("\n(1) BC dentro del alcance literal («el que perdió con un curso más o el que ganó con uno menos»): %d versiones · el criterio da el veredicto contrario en %d\n",
            sum(B$dentro), sum(B$dentro & B$crit != B$v_c)))
## Por qué es 0: con k = 1 y el ganador con más cursos, tantos cursos como el otro ES un curso más o uno menos, así que HP y HG serían la misma
## afirmación, ambas verdaderas, y L-C1 exige una falsa: la de ventaja sale BP. Se comprueba, no se supone.
K1C <- T[T$rama == "CURSOS" & T$k == 1L, ]
cat(sprintf("    Comprobación: rama CURSOS con k = 1: %d versiones; afirmación de ventaja BP en %d; HP y HG verdaderas en %d\n",
            nrow(K1C), sum(K1C$tipo == "BP"), sum(K1C$hp_real & K1C$hg_real)))
## (2)
K <- B[B$k == 1L, ]
cat(sprintf("\n(2) BC fuera del alcance: %d\n", sum(!B$dentro)))
cat(sprintf("    k >= 2: %d · el criterio, aplicado igual, da el contrario en %d\n", sum(B$k >= 2L), sum(B$k >= 2L & B$crit != B$v_c)))
for (se in c("previsto", "inverso")) { X <- K[K$sentido == se, ]
  cat(sprintf("    k = 1, sentido %s: %d · da el veredicto real en %d · da el contrario en %d\n", se, nrow(X), sum(X$crit == X$v_c), sum(X$crit != X$v_c))) }
cat(sprintf("    Sin acotar (frase de 6f6f1b13), aplicado a toda BC: contrario en %d de %d\n", sum(B$crit != B$v_c), nrow(B)))
stopifnot("cifras de la C1c" = nrow(B) == 495L, sum(B$crit != B$v_c) == 421L, sum(B$k >= 2L) == 273L, sum(B$k >= 2L & B$crit != B$v_c) == 238L,
          nrow(K) == 222L, sum(K$crit != K$v_c) == 183L)
cat("    Control: las seis cifras de la C1c (495, 421, 273, 238, 222 y 183), reproducidas\n")
## (3)
crit_hp <- T$aP > T$D; crit_hg <- T$aG > T$D
cat(sprintf("\n(3) HP, dentro del alcance: el criterio coincide con el veredicto calculado desde la tabla en %d de 1000 y con la marca en %d de 1000\n",
            sum(crit_hp == T$hp_real), sum(crit_hp == T$hp_marca)))
cat(sprintf("    HG, dentro del alcance: el criterio coincide con el veredicto calculado desde la tabla en %d de 1000 y con la marca en %d de 1000\n",
            sum(crit_hg == T$hg_real), sum(crit_hg == T$hg_marca)))
cat(sprintf("    Empates (promedio = diferencia, que «si es mayor» y «si es menor» no cubrirían): HP %d · HG %d\n", sum(T$hp_emp), sum(T$hg_emp)))
stopifnot("HP y HG, en las 1000" = all(crit_hp == T$hp_real), all(crit_hg == T$hg_real), all(crit_hp == T$hp_marca), all(crit_hg == T$hg_marca))
## Control positivo de la sonda de (3): un criterio con el grado cambiado sí se aparta.
cat(sprintf("    Control positivo: con el promedio del otro grado, HP se aparta del veredicto real en %d de 1000\n", sum((T$aG > T$D) != T$hp_real)))
## (4)
B205 <- B[B$s <= 205L, ]
cat(sprintf("\n(4) Control contra la r5, semillas 1 a 205: %d BC, y el criterio sin acotar da el contrario en %d (la r5 publicó 102 y 80).\n", nrow(B205), sum(B205$crit != B205$v_c)))
stopifnot(nrow(B205) == 102L, sum(B205$crit != B205$v_c) == 80L)
## (5) Lecturas de la frase nueva
cat("\n(5) Lecturas de la frase nueva, medidas y no corregidas (frase literal del profesor)\n")
cat(sprintf("  a. «ese grado» = el último nombrado («el que ganó»), también para HP: el veredicto de HP sale contrario en %d de 1000. Las frases resueltas que siguen citan el promedio de cada grado.\n",
            sum((T$aG > T$D) != T$hp_real)))
CMP <- T[T$dif == -1L, ]                                               # la tabla muestra al que perdió con exactamente un curso más que el que ganó
cat(sprintf("  b. «tiene un curso más» leído como comparación entre los dos grados: la tabla lo cumple en %d de 1000 versiones (todas de la rama %s).\n",
            nrow(CMP), paste(unique(CMP$rama), collapse = "/")))
cat(sprintf("     De ellas, %d tienen una BC en sentido inverso; quien además le aplique el criterio a esa BC obtiene el contrario en %d de 1000. Son las mismas 222 y 183 de (2), fuera del alcance en la lectura buscada.\n",
            sum(CMP$tipo == "BC"), sum(K$sentido == "inverso" & K$crit != K$v_c)))
## Cruces, como en la C1c
cat("\nRama × k (todas las versiones):\n"); print(table(T$rama, pmin(T$k, 3), dnn = c("rama", "k (3 = 3 o más)")))
cat("\nRama × tipo de afirmación de ventaja × marca:\n"); print(ftable(table(T$rama, T$id, T$v_c, dnn = c("rama", "id", "marca"))))
