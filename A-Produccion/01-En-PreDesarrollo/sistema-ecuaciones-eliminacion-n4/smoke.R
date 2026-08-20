rmd <- "sistema_ecuaciones_eliminacion_numerico_variacional_argumentacion_n4_schoice_v1.Rmd"
lin <- readLines(rmd, warn = FALSE)
ini <- grep("^```\\{r data_generation", lin)[1] + 1
fin <- ini - 1 + which(grepl("^```\\s*$", lin[ini:length(lin)]))[1] - 1
code <- lin[ini:fin]
N <- 100L
res <- vector("list", N); fallos <- 0L
for (i in seq_len(N)) {
  set.seed(1000L + 7L*i)
  env <- new.env()
  ok <- tryCatch({ eval(parse(text = code), envir = env); TRUE },
                 error = function(e) { cat("FALLO semilla", i, ":", conditionMessage(e), "\n"); FALSE })
  if (!ok) { fallos <- fallos + 1L; next }
  res[[i]] <- list(paso = env$paso_real, cod = env$err_real$codigo,
                   canon = env$is_canonical, R = env$par$R, L = env$par$L,
                   ops = env$opciones, sol = env$sol,
                   clave = env$opciones[which(env$sol == 1L)],
                   vm = env$valor_mostrado, ctx = env$ctx$id)
}
res <- Filter(Negate(is.null), res)
cat("\n=== corridas OK:", length(res), " fallos:", fallos, "===\n")
cat("estratos por paso:\n"); print(table(vapply(res, function(r) r$paso, integer(1))))
cat("codigos de error real:\n"); print(table(vapply(res, function(r) r$cod, character(1))))
cat("canonicas:", sum(vapply(res, function(r) r$canon, logical(1))), "\n")
cat("contextos:\n"); print(table(vapply(res, function(r) r$ctx, character(1))))
cat("claves unicas:", length(unique(vapply(res, function(r) r$clave, character(1)))), "/", length(res), "\n")
cat("valores R unicos:", length(unique(vapply(res, function(r) r$R, numeric(1)))), "\n")
# H1 manual: longitud de la clave vs rivales
lg <- t(vapply(res, function(r) {
  L <- nchar(r$ops); k <- which(r$sol==1L)
  c(es_max = as.numeric(L[k] == max(L) && sum(L==max(L))==1),
    es_min = as.numeric(L[k] == min(L) && sum(L==min(L))==1),
    margen = (L[k] - max(L[-k]))/max(L[-k]))
}, numeric(3)))
cat("\nH1 manual -> clave es la UNICA mas larga:", round(100*mean(lg[,1]),1), "%",
    "| unica mas corta:", round(100*mean(lg[,2]),1), "%",
    "| margen mediano:", round(100*median(lg[,3]),1), "%\n")
cat("\nEjemplo canonico (si hay):\n")
cn <- Filter(function(r) r$canon, res)
if (length(cn)) { for (o in cn[[1]]$ops) cat("  * ", o, "\n", sep=""); cat("  clave:", which(cn[[1]]$sol==1), "\n") }
