## FAMILIA 4 — marca-vs-verdad sobre el XML EXPORTADO. La verdad se RECOMPUTA
## del texto del propio XML; no se declara. Toda sonda lleva control positivo.
desent <- function(s){s<-gsub("&lt;","<",s);s<-gsub("&gt;",">",s);s<-gsub("&amp;","&",s);gsub("&quot;","\"",s)}
opciones_de <- function(gap) {
  cuerpo <- sub("^\\{1:[A-Z]+:", "", sub("\\}$", "", gap))
  strsplit(cuerpo, "~", fixed = TRUE)[[1]]
}
es_clave <- function(op) startsWith(op, "=")
## --- CONTROL POSITIVO de la sonda (debe dar 1 clave y 3 no-clave) -----------
ctrl <- "{1:MULTICHOICE:a~=b~%-33.33333%c~d}"
stopifnot(length(opciones_de(ctrl)) == 4L, sum(vapply(opciones_de(ctrl), es_clave, TRUE)) == 1L)
## --- CONTROL NEGATIVO: un gap sin ninguna marca debe dar 0 ------------------
stopifnot(sum(vapply(opciones_de("{1:MULTICHOICE:a~b~c~d}"), es_clave, TRUE)) == 0L)
cat("controles de la sonda: OK\n\n")

nn <- function(s) suppressWarnings(as.numeric(gsub("[.]", "", s)))
x  <- paste(readLines(list.files("salida_moodle100c","\\.xml$",full.names=TRUE)[1], warn=FALSE,
                      encoding="UTF-8"), collapse="\n")
qs <- regmatches(x, gregexpr("<question type=\"cloze\">.*?</question>", x))[[1]]
err <- 0L
for (qi in seq_along(qs)) {
  q <- desent(qs[qi])
  g <- regmatches(q, gregexpr("\\{1:[A-Z]+:[^}]*\\}", q))[[1]]
  tipos <- sub("^\\{1:([A-Z]+):.*$", "\\1", g)
  ok_tipos <- identical(tipos, c("MULTICHOICE","NUMERICAL","NUMERICAL",
                                 "MULTICHOICE","MULTIRESPONSE","NUMERICAL"))
  n1c <- sum(vapply(opciones_de(g[1]), es_clave, TRUE))
  n4c <- sum(vapply(opciones_de(g[4]), es_clave, TRUE))
  n5c <- sum(vapply(opciones_de(g[5]), es_clave, TRUE))
  n1t <- length(opciones_de(g[1])); n4t <- length(opciones_de(g[4])); n5t <- length(opciones_de(g[5]))
  ## --- verdad numerica recomputada del sistema IMPRESO en el enunciado ------
  ecs <- regmatches(q, gregexpr("[0-9]+[A-Z] \\+ [0-9]+[A-Z] &= [0-9.]+", q))[[1]]
  co  <- lapply(ecs, function(s) nn(regmatches(s, gregexpr("[0-9][0-9.]*", s))[[1]]))
  val <- function(gp) nn(sub("^\\{1:NUMERICAL:=([0-9]+).*$", "\\1", gp))
  Rv <- val(g[2]); Lv <- val(g[3]); V6 <- val(g[6])
  e1 <- e2 <- NA
  if (length(co) == 2L) {
    e1 <- co[[1]][1]*Lv + co[[1]][2]*Rv == co[[1]][3]
    e2 <- co[[2]][1]*Lv + co[[2]][2]*Rv == co[[2]][3]
  }
  p6 <- regmatches(q, regexpr("Parte 6.*?\\{1:NUMERICAL", q))
  pr <- strsplit(sub("^.*por ([0-9]+) ([^0-9]+) y ([0-9]+) .*$", "\\1|\\3", p6), "|", fixed = TRUE)[[1]]
  e6 <- if (length(pr) == 2L && !any(is.na(suppressWarnings(as.numeric(pr)))))
          as.numeric(pr[1])*Lv + as.numeric(pr[2])*Rv == V6 else NA
  malo <- !ok_tipos || n1c != 1L || n4c != 1L || n5c != 3L ||
          n1t != 4L || n4t != 4L || n5t != 6L || !isTRUE(e1) || !isTRUE(e2) || !isTRUE(e6)
  if (malo) err <- err + 1L
  cat(sprintf("q%d %s tipos=%s p1=%d/%d p4=%d/%d p5=%d/%d E1=%s E2=%s P6=%s\n",
      qi, if (malo) "FALLA" else "OK  ", if (ok_tipos) "ok" else "MAL",
      n1c, n1t, n4c, n4t, n5c, n5t, e1, e2, e6))
}
cat("\nMISMATCHES marca-vs-verdad:", err, "de", length(qs), "\n")
