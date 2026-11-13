## Verificador de las guardas de texto (FASE 2C r1 H7). Contrato de mutación (INC-MUTANTE-SONDA): cada mutante declara su
## sonda, su patrón aparece exactamente una vez, se aplica en memoria sobre el chunk data_generation, recorre semillas hasta
## la primera muerte (tope 600) y la línea base sin mutar corre sobre las mismas semillas.
rmd <- list.files(".", pattern = "_schoice_v1\\.Rmd$")[1]
l <- readLines(rmd, encoding = "UTF-8"); i0 <- grep("^```\\{r data_generation", l); i1 <- which(l == "```")[which(l == "```") > i0][1]
src <- paste(l[(i0 + 1):(i1 - 1)], collapse = "\n")
correr <- function(code_txt, s) { set.seed(s); e <- new.env()
  tryCatch({ eval(parse(text = code_txt, encoding = "UTF-8"), e); "" }, error = function(err) conditionMessage(err)) }
MUT <- list(
  M1 = list(sonda = "el término mostrado vale lo que dice el texto",
            de = 'ult_tex_signo <- paste0(if (ult %% 2L == 1L) "-" else "", ult_tex)', a = 'ult_tex_signo <- ult_tex'),
  M2 = list(sonda = "los signos mostrados son los que el texto describe",
            de = 'paste0(l0, if (s[i] < 0) " - " else " + ", t)', a = 'paste0(l0, if (coef[i] < 0) " - " else " + ", t)'),
  M3 = list(sonda = "la diferencia redondeada que se imprime es menor que la cota impresa",
            de = 'format = "fg", digits = 3)', a = 'format = "fg", digits = 2)'),
  M4 = list(sonda = "la trampa de la calculadora está entre los distractores válidos",
            de = 'calcula = function(p) round(sin(p$x) * p$D)', a = 'calcula = function(p) numerador(p, p$n, fact_impar, function(k) alt(k, p$n))'),
  M5 = list(sonda = "a partir del último término usado, cada término es menor que el anterior",
            de = '(n - 2L):(n + 5L)', a = '0:(n + 5L)'),
  M6 = list(sonda = "a partir del último término usado, cada término es menor que el anterior",
            de = 'n <- pick(3:4)', a = 'n <- pick(2:4)'),
  M7 = list(sonda = "ningún distractor del banco pasa de 4 en valor absoluto",
            de = 'TOPE_VALOR <- 4', a = 'TOPE_VALOR <- Inf'))
TOPE <- 600L; filas <- list()
for (id in names(MUT)) {
  m <- MUT[[id]]; nveces <- lengths(regmatches(src, gregexpr(m$de, src, fixed = TRUE)))
  if (nveces != 1L) { filas[[id]] <- data.frame(mutante = id, sonda = m$sonda, semilla = NA, veredicto = paste0("mal_construido (patrón x", nveces, ")")); next }
  mut <- sub(m$de, m$a, src, fixed = TRUE); stopifnot(!identical(mut, src))
  ver <- "no_detectado"; sem <- NA
  for (s in seq_len(TOPE)) { err <- correr(mut, s); if (nzchar(err)) {
      base <- correr(src, s)
      ver <- if (nzchar(base)) "invalido_base_falla" else if (grepl(m$sonda, err, fixed = TRUE)) "cazado_por_su_sonda" else paste0("cazado_por_otra: ", substr(err, 1, 80))
      sem <- s; break } }
  filas[[id]] <- data.frame(mutante = id, sonda = m$sonda, semilla = sem, veredicto = ver)
}
base_fallos <- sum(vapply(seq_len(TOPE), function(s) nzchar(correr(src, s)), TRUE))
R <- do.call(rbind, filas); print(R, row.names = FALSE); cat("línea base sin mutar: fallos en", TOPE, "semillas =", base_fallos, "\n")
quit(status = if (all(R$veredicto == "cazado_por_su_sonda") && base_fallos == 0L) 0L else 1L)
