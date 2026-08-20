## ============================================================================
## BATERÍA DE REFERENCIA ICFES — la vara con la que se calibra §P7
## ============================================================================
## Es la batería con la que se midió la vara oficial sobre 426 ítems de
## Matemáticas deduplicados (2026-08-19). Vivía sólo en un scratchpad, así que
## las cifras publicadas en la regla #22 §P7-A NO eran reproducibles: se versiona
## aquí para que lo sean.
##
## CIFRAS DE REFERENCIA (34 reglas, tras corregir la ceguera de la v1):
##   Corpus oficial completo (n=426) : exceso +4,6 pp  (techo nulo 28,3 · sd 1,11)
##   Control oficial         (n=399) : exceso +5,2 pp  (techo nulo 28,4 · sd 1,15)
##   Ítems con opciones-ecuación (27): exceso -0,7 pp
##
## ⚠️ CEGUERA CORREGIDA EN LA v2 — leer antes de añadir reglas de valor.
## La v1 extraía el valor con `n1()`, el PRIMER número de la opción. En cualquier
## molde que empiece por un numeral —«En el paso 2, la ecuación debió ser: 90R =
## 630.000»— eso lee el número de PASO, no el valor. Medido: aplicabilidad 0 % en
## las cinco reglas de divisibilidad, con un canal real del 41,9 % pasando por
## debajo, y un `PASS` de -4,2 pp sobre un ítem que con la sonda correcta mide
## +9,4 pp. El corpus oficial contiene ítems con ese molde, así que la ceguera
## afectaba también a la vara.
## Regla práctica: para el VALOR usa `nlast()` (último número); `n1()` sólo sirve
## cuando la opción es una cifra desnuda.
##
## CONTROL DE NO-RELLENO (§P7-C): al ampliar de 28 a 34 reglas, el exceso de las
## poblaciones oficiales NO se movió (+5,3 -> +5,2 y +4,6 -> +4,6). Las sondas
## nuevas no inflan el techo de nadie: destapan lo que estaba tapado. Repite este
## control ANTES de promover cualquier ampliación futura.
##
## Origen: medición de la vara + 5.ª auditoría de detractor sobre
## `sistema-ecuaciones-eliminacion-n4`. Ver `ref_vara_p7_items_ecuacion`.
## ============================================================================

source("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/bateria_eliminacion.R")

nums  <- function(s) suppressWarnings(as.numeric(gsub("\\.","",regmatches(s, gregexpr("[0-9]+(?:[.,][0-9]+)?", s))[[1]])))
n1    <- function(s){v <- nums(s); if(length(v)) v[1] else NA_real_}
toks  <- function(s) unique(tolower(regmatches(s, gregexpr("[A-Za-zÀ-ÿ]{3,}", s))[[1]]))
ntok  <- function(s) length(strsplit(trimws(s), "\\s+")[[1]])
solo1 <- function(l) if (sum(l, na.rm=TRUE) == 1L) which(l) else NA_integer_
grp   <- function(l){ l[is.na(l)] <- FALSE
  if (all(l) || !any(l)) return(NA_integer_)
  if (sum(l) <= length(l)/2) which(l) else which(!l) }   # el grupo MINORITARIO

B <- list(
 # ---------------- magnitud ----------------
 nueva_regla("la mas larga (chars)","magnitud", function(o) which.max(nchar(o))),
 nueva_regla("la mas corta (chars)","magnitud", function(o) which.min(nchar(o))),
 nueva_regla("el numero mayor","magnitud", function(o){v<-vapply(o,n1,0);if(all(is.na(v)))NA else which.max(v)}),
 nueva_regla("el numero menor","magnitud", function(o){v<-vapply(o,n1,0);if(all(is.na(v)))NA else which.min(v)}),
 nueva_regla("mas digitos","magnitud", function(o) which.max(nchar(gsub("[^0-9]","",o)))),
 # ---------------- divisibilidad ----------------
 nueva_regla("unica con decimal","divisibilidad", function(o) solo1(grepl("[0-9][.,][0-9]",o))),
 nueva_regla("unica sin decimal","divisibilidad", function(o) solo1(!grepl("[0-9][.,][0-9]",o))),
 nueva_regla("primer numero PAR","divisibilidad", function(o){v<-vapply(o,n1,0)
   if(all(is.na(v))||any(v!=floor(v),na.rm=TRUE))return(NA); grp(v%%2==0)}),
 nueva_regla("unica multiplo de 5","divisibilidad", function(o){v<-vapply(o,n1,0)
   if(all(is.na(v)))return(NA); solo1(!is.na(v)&v%%5==0)}),
 nueva_regla("unica no multiplo de 10","divisibilidad", function(o){v<-vapply(o,n1,0)
   if(all(is.na(v)))return(NA); solo1(!is.na(v)&v%%10!=0)}),
 # ---------------- signo ----------------
 nueva_regla("unica con negativo","signo", function(o) solo1(grepl("(^|[ (=+*/])[-\u2212][0-9A-Za-z]",o))),
 nueva_regla("grupo minoritario negativo","signo", function(o) grp(grepl("(^|[ (=+*/])[-\u2212][0-9A-Za-z]",o))),
 nueva_regla("unica con negacion lexica","signo", function(o)
   solo1(grepl("\\b([Nn]o|[Nn]unca|[Nn]ing|sin)\\b",o))),
 nueva_regla("grupo desigualdad estricta","signo", function(o){
   e<-grepl("[<>]",o)&!grepl("[\u2264\u2265]",o); ne<-grepl("[\u2264\u2265]",o)
   if(!any(e|ne))return(NA); grp(e)}),
 # ---------------- posicion ----------------
 nueva_regla("siempre la 1a","posicion", function(o) 1L),
 nueva_regla("siempre la 2a","posicion", function(o) 2L),
 nueva_regla("siempre la 3a","posicion", function(o) 3L),
 nueva_regla("siempre la ultima","posicion", function(o) length(o)),
 # ---------------- formato ----------------
 nueva_regla("unica con fraccion /","formato", function(o) solo1(grepl("/",o))),
 nueva_regla("unica con parentesis","formato", function(o) solo1(grepl("\\(",o))),
 nueva_regla("unica con '='","formato", function(o) solo1(grepl("=",o))),
 nueva_regla("unica con potencia","formato", function(o) solo1(grepl("\\^|[\u00b2\u00b3\u2074]",o))),
 nueva_regla("molde atipico (n tokens)","formato", function(o){k<-vapply(o,ntok,0L)
   d<-abs(k-stats::median(k)); if(all(d==0))NA else which.max(d)}),
 # ---------------- lexico ----------------
 nueva_regla("unica con token exclusivo","lexico", function(o){T<-lapply(o,toks)
   ex<-vapply(seq_along(T),function(i) length(setdiff(T[[i]],unlist(T[-i])))>0,logical(1)); solo1(ex)}),
 nueva_regla("la que MAS comparte","lexico", function(o){T<-lapply(o,toks)
   sc<-vapply(seq_along(T),function(i) length(intersect(T[[i]],unlist(T[-i]))),0L)
   if(all(sc==0))NA else which.max(sc)}),
 nueva_regla("la que MENOS comparte","lexico", function(o){T<-lapply(o,toks)
   sc<-vapply(seq_along(T),function(i) length(intersect(T[[i]],unlist(T[-i]))),0L)
   if(all(sc==0))NA else which.min(sc)}),
 nueva_regla("primer token unico","lexico", function(o){
   p<-tolower(vapply(strsplit(trimws(o),"\\s+"),function(z) if(length(z))z[1] else "",""))
   solo1(vapply(seq_along(p),function(i) !(p[i] %in% p[-i]),logical(1)))}),
 nueva_regla("unica sin token repetido interno","lexico", function(o){
   rep<-vapply(o,function(s){w<-tolower(regmatches(s,gregexpr("[A-Za-zÀ-ÿ0-9]+",s))[[1]])
     length(w)>0 && any(duplicated(w))},logical(1)); solo1(!rep)})
)

## --- v2: seis sondas sobre el ÚLTIMO número (corrigen la ceguera) ---
## BATERIA UNIVERSAL V2 = las 28 congeladas + 6 sondas sobre el ULTIMO numero.
## Motivo (auditoria 5): en moldes que empiezan por "En el paso N, ...", n1() lee el
## NUMERO DE PASO, no el valor -> las 5 reglas de valor quedan con aplicabilidad 0 %.
## §P7-C: se anade la sonda y se RE-MIDE el historico completo con la bateria nueva.
## Las 6 nuevas son genericas (ningun umbral elegido mirando este ejercicio salvo el
## "redondo" = multiplo de 50, que es la unidad de precio mas gruesa habitual).
## [ELIMINADO al versionar] Aqui el archivo del scratchpad hacia source("../bateria.R"),
## ruta relativa que NO resuelve desde ningun cwd (mismo modo de fallo que el Error 31).
## Es innecesario: las reglas base estan definidas ARRIBA, en este mismo archivo.
nlast <- function(s){ z <- nums(s); if(!length(z)) NA_real_ else z[length(z)] }
## nums() de bateria.R es ciego al signo: se anade una version con signo para la sonda.
nlast_s <- function(s){ z <- suppressWarnings(as.numeric(gsub("\\.","",
             regmatches(s, gregexpr("-?[0-9]+(?:[.,][0-9]+)?", s))[[1]])))
           if(!length(z)) NA_real_ else z[length(z)] }
B2 <- c(B, list(
  nueva_regla("ultimo numero mayor","magnitud", function(o){v<-vapply(o,nlast,0)
    if(all(is.na(v)))NA else which.max(v)}),
  nueva_regla("ultimo numero menor","magnitud", function(o){v<-vapply(o,nlast,0)
    if(all(is.na(v)))NA else which.min(v)}),
  nueva_regla("ultimo multiplo de 10","divisibilidad", function(o){v<-vapply(o,nlast,0)
    if(all(is.na(v)))return(NA); grp(!is.na(v)&v%%10==0)}),
  nueva_regla("ultimo multiplo de 100","divisibilidad", function(o){v<-vapply(o,nlast,0)
    if(all(is.na(v)))return(NA); grp(!is.na(v)&v%%100==0)}),
  nueva_regla("ultimo redondo (mult 50) y positivo","divisibilidad", function(o){
    v<-vapply(o,nlast_s,0); if(all(is.na(v)))return(NA)
    s<-which(!is.na(v)&v>0&v%%50==0); if(!length(s)||length(s)==length(o))NA else s}),
  nueva_regla("descartar ultimo negativo","signo", function(o){v<-vapply(o,nlast_s,0)
    if(all(is.na(v)))return(NA); s<-which(!is.na(v)&v>0)
    if(!length(s)||length(s)==length(o))NA else s})
))
