library(exams)
.exams_generation_mode <- TRUE
ej <- list.files("ej", pattern = "\\.Rmd$")
n <- 3; fecha <- as.Date("2026-11-13")
set.seed(2026); S <- matrix(sample(1:1e6, n * length(ej)), nrow = n)
id <- function(i) paste0(format(fecha, "%y%m%d"), formatC(i, width = 5, flag = "0"))
for (d in c("nops", "a", "b")) dir.create(d, showWarnings = FALSE)
xn <- exams2nops(ej, n = n, seed = S, dir = "nops", edir = "ej", name = "nops_", date = fecha, language = "es",
                 institution = "I. E. Pedacito de Cielo", title = "Evaluación de Matemáticas", duplex = TRUE,
                 twocolumn = FALSE)  # solo se usa la hoja de respuestas; twocolumn = TRUE falla con tablas (longtable)
xa <- exams2pdf(ej, n = n, seed = S, dir = "a", edir = "ej", name = "cuad_", template = "cuadernillo_oficio_2col",
                header = list(Date = format(fecha), ID = id), texdir = "a/tex")
xb <- exams2pdf(ej, n = n, seed = S, dir = "b", edir = "ej", name = "cuad_", template = "cuadernillo_oficio_2col",
                header = list(Date = format(fecha), ID = id))
firma <- function(e) paste(c(e$question, unlist(e$questionlist)), collapse = "\n")
res <- do.call(rbind, lapply(seq_len(n), function(i) do.call(rbind, lapply(seq_along(ej), function(j) {
  a <- xa[[i]][[j]]; b <- xb[[i]][[j]]; z <- xn[[i]][[j]]
  data.frame(version = id(i), archivo = substr(ej[j], 1, 45),
             texto_nops = identical(firma(a), firma(z)), clave_nops = identical(a$metainfo$solution, z$metainfo$solution),
             repro = identical(firma(a), firma(b)), clave_unica = sum(a$metainfo$solution) == 1)
}))))
print(res, row.names = FALSE)
cat("\nTOTAL pares:", nrow(res), "| texto=", sum(res$texto_nops), "| clave=", sum(res$clave_nops),
    "| repro=", sum(res$repro), "| clave_unica=", sum(res$clave_unica), "\n")
