## lectura_criterio_c1c.R — tanda C1c (2026-11-03). Solo lee. Semillas 1 a 1000, data_generation de ef2c96dc.
## El criterio nuevo se acota a «con un curso más o uno menos». ¿En cuántas versiones un estudiante que lo aplique a la afirmación
## de ventaja «por cursos» (BC_perdio o BC_gano) obtiene el veredicto equivocado? La afirmación dice que el ganador cambia; el
## criterio responde «cambia» si el promedio del grado que cambia supera la diferencia. Con k = 1 el cambio es de un curso:
##   sentido previsto: el que perdió suma un curso o el que ganó pierde uno (es decir, igual a HP o a HG);
##   sentido inverso: el que perdió pierde un curso o el que ganó suma uno (rama PROMEDIO): el ganador no puede cambiar.
suppressMessages(library(exams))
F <- normalizePath("../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
stopifnot(unname(tools::md5sum(F)) == "ef2c96dc1c2b667fa904ea231329b4dc")
l <- readLines(F, encoding = "UTF-8"); a <- grep("^```\\{r data_generation", l)[1]; b <- which(l == "```" & seq_along(l) > a)[1]
EX <- parse(text = paste(l[(a + 1):(b - 1)], collapse = "\n"), encoding = "UTF-8")
fila <- function(s) { set.seed(s); e <- new.env(); eval(EX, envir = e); d <- e$dp; id <- e$c6$id
  if (!startsWith(id, "BC_")) return(data.frame(s = s, tipo = "BP", k = NA, sentido = NA, crit = NA, real = e$v_c))
  perd <- id == "BC_perdio"; suma <- if (e$rama_par == "CURSOS") perd else !perd
  a_q <- if (perd) d$aP else d$aG
  data.frame(s = s, tipo = "BC", k = abs(d$nG - d$nP), sentido = if (perd == suma) "previsto" else "inverso", crit = a_q > d$D, real = e$v_c) }
T <- do.call(rbind, lapply(1:1000, fila))
cat(sprintf("Semillas 1 a 1000. Afirmación de ventaja: BP %d · BC %d\n", sum(T$tipo == "BP"), sum(T$tipo == "BC")))
B <- T[T$tipo == "BC", ]
cat(sprintf("BC con k >= 2 (fuera del alcance de «un curso más o uno menos»): %d\n", sum(B$k >= 2)))
K <- B[B$k == 1, ]
for (se in c("previsto", "inverso")) { X <- K[K$sentido == se, ]
  cat(sprintf("BC con k = 1, sentido %s: %d versiones · el criterio da el veredicto real en %d · da el contrario en %d\n", se, nrow(X), sum(X$crit == X$real), sum(X$crit != X$real))) }
cat(sprintf("Versiones donde el criterio, aplicado dentro de su alcance literal, da el veredicto contrario: %d de 1000\n", sum(K$crit != K$real)))
## Control de la tabla: rama del otro colegio × k × tipo, y qué dice la marca. Si la rama CURSOS no tuviera ninguna versión con
## k = 1, el «sentido previsto» vacío sería un hecho del generador y no un fallo de este script.
fila2 <- function(s) { set.seed(s); e <- new.env(); eval(EX, envir = e); d <- e$dp
  data.frame(rama = e$rama_par, k = abs(d$nG - d$nP), id = e$c6$id, v_c = e$v_c, signo = sign(d$nG - d$nP), aP_D = d$aP > d$D, aG_D = d$aG > d$D) }
U <- do.call(rbind, lapply(1:1000, fila2))
cat("\nRama × k (todas las versiones):\n"); print(table(U$rama, pmin(U$k, 3), dnn = c("rama", "k (3 = 3 o más)")))
cat("\nRama × tipo de afirmación de ventaja × marca:\n"); print(ftable(table(U$rama, U$id, U$v_c, dnn = c("rama", "id", "marca"))))
cat("\nsign(nG - nP) por rama:\n"); print(table(U$rama, U$signo))
## Comparación con la H1(b) de la r5 (allí, 80 de 102 BC en 205 versiones): sin la acotación, el criterio se aplica a toda BC.
cat(sprintf("\nSin la acotación (frase de 6f6f1b13), el criterio aplicado a toda BC da el veredicto contrario en %d de %d BC (%d de 1000 versiones).\n",
            sum(B$crit != B$real), nrow(B), sum(B$crit != B$real)))
cat(sprintf("Con la acotación, las BC con k >= 2 quedan fuera de su alcance (%d, de las que %d daban el contrario), y quedan dentro las %d con k = 1, todas en sentido inverso (%d dan el contrario).\n",
            sum(B$k >= 2), sum(B$k >= 2 & B$crit != B$real), nrow(K), sum(K$crit != K$real)))
## Control positivo contra la cifra publicada por la r5 (su H1, BC: 102 de 205 versiones y 80 con el veredicto equivocado).
B205 <- B[B$s <= 205, ]
cat(sprintf("\nControl contra la r5, semillas 1 a 205: %d BC, y el criterio sin acotar da el contrario en %d (la r5 publicó 102 y 80).\n", nrow(B205), sum(B205$crit != B205$real)))
