## semillas_render_c1c.R — las mismas semillas que render_cloze.R: la primera canónica y una no canónica por cruce de ramas.
lin <- readLines(F, encoding = "UTF-8"); a <- grep("^```\\{r data_generation", lin)[1]; b <- which(lin == "```" & seq_along(lin) > a)[1]
cod <- parse(text = paste(lin[(a + 1):(b - 1)], collapse = "\n"), encoding = "UTF-8")
info <- function(s) { set.seed(s); e <- new.env(); eval(cod, envir = e); c(can = as.character(isTRUE(e$es_canonica)), rama = e$rama, par = e$rama_par) }
I <- t(vapply(1:300, info, character(3))); primera <- function(cond) which(cond)[1]
SEM <- c(canonica = primera(I[, "can"] == "TRUE"), CxC = primera(I[, "can"] == "FALSE" & I[, "rama"] == "CURSOS" & I[, "par"] == "CURSOS"),
         CxP = primera(I[, "can"] == "FALSE" & I[, "rama"] == "CURSOS" & I[, "par"] == "PROMEDIO"), PxC = primera(I[, "can"] == "FALSE" & I[, "rama"] == "PROMEDIO" & I[, "par"] == "CURSOS"),
         PxP = primera(I[, "can"] == "FALSE" & I[, "rama"] == "PROMEDIO" & I[, "par"] == "PROMEDIO"))
stopifnot(!anyNA(SEM)); cat("Semillas:", paste(names(SEM), SEM, sep = "=", collapse = " "), "\n")
