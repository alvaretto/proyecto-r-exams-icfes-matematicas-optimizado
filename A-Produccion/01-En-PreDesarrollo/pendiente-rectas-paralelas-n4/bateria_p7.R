#!/usr/bin/env Rscript
# =============================================================================
# bateria_p7.R — corrida del helper §P7 sobre ESTE ejercicio
#
# POR QUE EXISTE (objecion O-5 del detractor, 4a pasada de la FASE 2C). El
# veredicto del helper `.claude/scripts/bateria_eliminacion.R` se habia citado en
# un reporte sin dejar artefacto en el repositorio: no era ni verificable ni
# repetible, que es exactamente lo que la regla #23 prohibe. Este archivo es esa
# corrida, con sus reglas declaradas por familia y su semilla fija.
#
# QUE ACREDITA Y QUE NO — leer antes de citar su veredicto:
#   · RECONCILIADO 2026-08-15. Antes este bloque declaraba que su cifra y la de
#     `auditoria_propia.R` NO eran comparables, por dos razones que ya no existen:
#     (a) umbrales distintos (70 % aqui, 45 % alli) — los dos eran ABSOLUTOS y la
#     medicion sobre 468 items oficiales mostro que un umbral absoluto mide, en
#     parte, el tamano de la bateria; ahora ambos juzgan por el EXCESO sobre el
#     techo nulo, con los mismos cortes leidos del helper; y (b) convenciones de
#     puntuacion distintas (0/1 aqui, 1/|S| alli) — el helper adopto la de 1/|S|,
#     que es la unica con nulo EXACTO (E[score] = 1/n para toda regla), asi que
#     estas reglas se le pasan tal cual, sin sortear entre supervivientes.
#   · mide ATOMOS. El cierre por conjunciones vive en (K)/(K3)/(K4) del auditor,
#     y ahi es donde este item falla (67 % cruzado, +31 pp sobre su techo nulo).
#     Un PASS aqui NO acredita ausencia de canal: acredita ausencia de canal
#     ATOMICO, que es una afirmacion estrictamente mas debil.
#
#   Uso:  Rscript bateria_p7.R [ruta_al_.Rmd] [N]      (por defecto: este, N=100)
# =============================================================================
#!/usr/bin/env Rscript
# =============================================================================
# medir.R <ruta_rmd> <N> <etiqueta>
# Harness A/B para el factor de escala comun `s`.
# Mide con la BATERIA AMPLIADA (reglas de auditoria_propia.R + las 3 familias
# que el detractor senalo como ausentes) y con el helper §P7.
# =============================================================================
args <- commandArgs(trailingOnly = TRUE)
RMD <- if (length(args) >= 1) args[1] else
  "pendiente_rectas_paralelas_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd"
N   <- if (length(args) >= 2) as.integer(args[2]) else 100L   # regla #23
ETQ <- if (length(args) >= 3) args[3] else "vigente"
REPO <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
source(file.path(REPO, ".claude/scripts/bateria_eliminacion.R"))

rmd <- readLines(RMD, warn = FALSE)
i0  <- grep("^```\\{r data_generation", rmd)[1]
i1  <- i0 + which(grepl("^```[[:space:]]*$", rmd[(i0 + 1):length(rmd)]))[1]
f_dg <- tempfile(fileext = ".R"); writeLines(rmd[(i0 + 1):(i1 - 1)], f_dg)
correr <- function(sm) { set.seed(sm); e <- new.env(); sys.source(f_dg, envir = e); e }

t0 <- Sys.time()
reg <- lapply(seq_len(N) * 7919L, function(sm) {
  e <- correr(sm); j <- which(e$sol == 1L)
  dx <- vapply(e$puntos, function(z) abs(z[1] - e$hx), numeric(1))
  dy <- vapply(e$puntos, function(z) abs(z[2] - e$hy), numeric(1))
  list(p = e$p_m, q = e$q_m, hy = e$hy, hx = e$hx, canon = isTRUE(e$es_canonica),
       j = j, dx = dx, dy = dy, dd = sqrt(dx^2 + dy^2), L = nchar(e$opciones),
       sg = vapply(e$pend_frac, function(z) sign(z[1]), numeric(1)),
       sg_obj = sign(e$p_m),
       mab = vapply(e$pend_frac, function(z) abs(z[1]) / z[2], numeric(1)),
       mobj = abs(e$p_m) / e$q_m,
       ptx = vapply(e$puntos, function(z) z[1], numeric(1)),
       pty = vapply(e$puntos, function(z) z[2], numeric(1)),
       s   = if (is.null(e$s_esc)) NA_integer_ else as.integer(e$s_esc),
       tc  = if (is.null(e$t_c))   NA_integer_ else as.integer(e$t_c),
       cod = e$codigos, kx = e$puntos[[j]][1], ky = e$puntos[[j]][2],
       intento = if (is.null(e$intento)) NA_integer_ else as.integer(e$intento))
})
cat(sprintf("\n### %s  (N = %d, %.0f s)\n", ETQ, N, as.numeric(difftime(Sys.time(), t0, units = "secs"))))

gcd2 <- function(a, b) { a <- abs(a); b <- abs(b); while (b > 0) { t <- b; b <- a %% b; a <- t }; a }
gcd_vec <- function(r) mapply(gcd2, r$dx, r$dy)

# --- metrica de ESPERANZA (la misma de auditoria_propia.R, comparable) --------
acierto <- function(reg, f) mean(vapply(reg, function(r) {
  S <- f(r); if (!any(S)) return(0.25); if (S[r$j]) 1 / sum(S) else 0 }, numeric(1)))

f_buck <- function(r) if (r$mobj > 1) r$dy > r$dx else r$dy < r$dx
f_sign <- function(r) r$sg == r$sg_obj
f_cell <- function(r) f_sign(r) & f_buck(r)
d1     <- function(r) abs(log(pmax(r$mab, 1e-9)))
coc    <- function(r) ifelse(r$dx %% r$q == 0, r$dx / r$q, NA_real_)
extrema_en <- function(r, C, v, cual) { if (!any(C)) return(C)
  C & (v == (if (cual == "max") max(v[C]) else min(v[C]))) }

R <- list()
add <- function(nm, fam, f) R[[nm]] <<- list(fam = fam, f = f)

# ---- MAGNITUD ---------------------------------------------------------------
add("dx max",            "magnitud", function(r) r$dx == max(r$dx))
add("dx min",            "magnitud", function(r) r$dx == min(r$dx))
add("desc dx max",       "magnitud", function(r) r$dx <  max(r$dx))
add("desc dx min",       "magnitud", function(r) r$dx >  min(r$dx))
add("dy max",            "magnitud", function(r) r$dy == max(r$dy))
add("dy min",            "magnitud", function(r) r$dy == min(r$dy))
add("desc dy max",       "magnitud", function(r) r$dy <  max(r$dy))
add("desc dy min",       "magnitud", function(r) r$dy >  min(r$dy))
add("dx<=2",             "magnitud", function(r) r$dx <= 2)
add("dx<=2 y dy>=2",     "magnitud", function(r) r$dx <= 2 & r$dy >= 2)
add("mas lejana",        "magnitud", function(r) r$dd == max(r$dd))
add("desc mas lejana",   "magnitud", function(r) r$dd <  max(r$dd))
add("desc mas cercana",  "magnitud", function(r) r$dd >  min(r$dd))
add("dy>dx",             "magnitud", function(r) r$dy >  r$dx)
add("dy<dx",             "magnitud", function(r) r$dy <  r$dx)
add("|m| max",           "magnitud", function(r) r$mab == max(r$mab))
add("|m| min",           "magnitud", function(r) r$mab == min(r$mab))
# N-1 del detractor: descartar los DOS extremos de |m| (45,1 % reportado)
add("desc |m| extremos", "magnitud", function(r) r$mab > min(r$mab) & r$mab < max(r$mab))
add("desc d1 extremos",  "magnitud", function(r) { v <- d1(r); v > min(v) & v < max(v) })
add("d1 max",            "magnitud", function(r) { v <- d1(r); v == max(v) })
add("d1 min",            "magnitud", function(r) { v <- d1(r); v == min(v) })
add("mas larga",         "magnitud", function(r) r$L == max(r$L))
add("mas corta",         "magnitud", function(r) r$L == min(r$L))
# ---- DIVISIBILIDAD ----------------------------------------------------------
add("gcd>1",             "divisibilidad", function(r) gcd_vec(r) > 1)
add("gcd=1",             "divisibilidad", function(r) gcd_vec(r) == 1)
add("gcd max",           "divisibilidad", function(r) { g <- gcd_vec(r); g == max(g) })
add("gcd min",           "divisibilidad", function(r) { g <- gcd_vec(r); g == min(g) })
add("desc gcd max",      "divisibilidad", function(r) { g <- gcd_vec(r); g <  max(g) })
add("desc gcd extremos", "divisibilidad", function(r) { g <- gcd_vec(r); g > min(g) & g < max(g) })
add("dx par",            "divisibilidad", function(r) r$dx %% 2 == 0)
add("dx impar",          "divisibilidad", function(r) r$dx %% 2 != 0)
add("dy par",            "divisibilidad", function(r) r$dy %% 2 == 0)
add("ambas pares",       "divisibilidad", function(r) r$dx %% 2 == 0 & r$dy %% 2 == 0)
add("desc ambas pares",  "divisibilidad", function(r) !(r$dx %% 2 == 0 & r$dy %% 2 == 0))
add("dx mult 3",         "divisibilidad", function(r) r$dx %% 3 == 0)
add("dy mult 3",         "divisibilidad", function(r) r$dy %% 3 == 0)
add("q|dx",              "divisibilidad", function(r) r$dx %% r$q == 0)
add("|p||dy",            "divisibilidad", function(r) r$dy %% abs(r$p) == 0)
add("dx==q",             "divisibilidad", function(r) r$dx == r$q)
add("dy==|p|",           "divisibilidad", function(r) r$dy == abs(r$p))
# N-2 del detractor: paridad del cociente dx/q (44,6 % reportado)
add("coc dx/q par",      "divisibilidad", function(r) { v <- coc(r); !is.na(v) & v %% 2 == 0 })
add("coc dx/q impar",    "divisibilidad", function(r) { v <- coc(r); !is.na(v) & v %% 2 == 1 })
# detractor: quedarse con el cociente dx/q entero MAXIMO (38,9 % / 48,6 %)
add("coc dx/q max",      "divisibilidad", function(r) { v <- coc(r)
  if (all(is.na(v))) return(rep(TRUE, length(v))); !is.na(v) & v == max(v, na.rm = TRUE) })
add("coc dx/q min",      "divisibilidad", function(r) { v <- coc(r)
  if (all(is.na(v))) return(rep(TRUE, length(v))); !is.na(v) & v == min(v, na.rm = TRUE) })
add("coc dx/q ==1",      "divisibilidad", function(r) { v <- coc(r); !is.na(v) & v == 1 })
# ---- SIGNO ------------------------------------------------------------------
add("SOLO-SIGNO",        "signo", f_sign)
add("signo contrario",   "signo", function(r) r$sg == -r$sg_obj)
add("dy y dx mismo signo","signo", function(r) r$sg > 0)
# ---- POSICION ---------------------------------------------------------------
add("abscisa unica",     "posicion", function(r) vapply(seq_along(r$ptx),
                            function(i) sum(r$ptx == r$ptx[i]) == 1L, logical(1)))
add("desc abscisa unica","posicion", function(r) vapply(seq_along(r$ptx),
                            function(i) sum(r$ptx == r$ptx[i]) >  1L, logical(1)))
add("ordenada unica",    "posicion", function(r) vapply(seq_along(r$pty),
                            function(i) sum(r$pty == r$pty[i]) == 1L, logical(1)))
add("coord 0",           "posicion", function(r)  (r$ptx == 0 | r$pty == 0))
add("desc coord 0",      "posicion", function(r) !(r$ptx == 0 | r$pty == 0))
add("comparte ord con H","posicion", function(r) r$pty == r$hy)
add("ambas coord pos",   "posicion", function(r) r$ptx > 0 & r$pty > 0)
# ---- FORMATO ----------------------------------------------------------------
add("dx de 1 cifra",     "formato", function(r) r$dx < 10)
add("ambas de 1 cifra",  "formato", function(r) abs(r$ptx) < 10 & abs(r$pty) < 10)
add("sin signo menos",   "formato", function(r) r$ptx >= 0 & r$pty >= 0)
# ---- LEXICO -----------------------------------------------------------------
add("mas digitos",       "lexico", function(r) { d <- nchar(gsub("[^0-9]", "", format(r$ptx)))+
                            nchar(gsub("[^0-9]", "", format(r$pty))); d == max(d) })
# ---- CONDICIONADAS por |m| (intra-celda) ------------------------------------
add("BUCKET",            "magnitud", f_buck)
add("SIGNO+BUCKET",      "signo",    f_cell)
add("compl. celda",      "signo",    function(r) !f_cell(r))
for (nm in c("d1", "dx", "dy", "dd", "gcd", "coc")) {
  vf <- switch(nm, d1 = d1, dx = function(r) r$dx, dy = function(r) r$dy,
               dd = function(r) r$dd, gcd = gcd_vec,
               coc = function(r) { v <- coc(r); ifelse(is.na(v), -1, v) })
  fam <- if (nm %in% c("gcd", "coc")) "divisibilidad" else "magnitud"
  local({ v <- vf; n <- nm; fa <- fam
    add(paste0("celda+", n, " max"), fa, function(r) extrema_en(r, f_cell(r), v(r), "max"))
    add(paste0("celda+", n, " min"), fa, function(r) extrema_en(r, f_cell(r), v(r), "min"))
    add(paste0("celda+", n, " medio"), fa, function(r) { C <- f_cell(r); if (!any(C)) return(C)
      vv <- v(r); C & vv > min(vv[C]) & vv < max(vv[C]) })
  })
}
for (nm in c("gcd>1", "gcd=1", "dx par", "dy par", "ambas pares", "coord 0",
             "coc dx/q par", "coc dx/q impar")) {
  local({ n <- nm; pf <- R[[n]]$f
    add(paste0("celda+", n), R[[n]]$fam, function(r) f_cell(r) & pf(r)) })
}

# =============================================================================
# SALIDA
# =============================================================================
reg_nc <- reg[!vapply(reg, function(r) r$canon, logical(1))]
tabla <- function(rg, etq) {
  a <- vapply(R, function(x) acierto(rg, x$f), numeric(1))
  o <- order(a, decreasing = TRUE)
  cat(sprintf("\n-- TOP 12 reglas [%s, n = %d] --\n", etq, length(rg)))
  for (k in o[1:12]) cat(sprintf("   %-22s [%-13s] %5.1f %%\n", names(R)[k], R[[k]]$fam, 100 * a[k]))
  cat(sprintf("   MAXIMO = %.1f %% (%s)\n", 100 * max(a), names(R)[which.max(a)]))
  invisible(a)
}
a_con <- tabla(reg,    "CON canonicas")
a_sin <- tabla(reg_nc, "SIN canonicas")

cat("\n-- Canales vigilados (SIN canonicas) --\n")
for (nm in c("SOLO-SIGNO","SIGNO+BUCKET","celda+gcd max","celda+gcd min","gcd max","gcd min",
             "desc |m| extremos","coc dx/q par","coc dx/q impar","coc dx/q max",
             "dy>dx","q|dx","dx==q","celda+dx max","celda+d1 max","celda+d1 min"))
  cat(sprintf("   %-22s : %5.1f %%   (con canon: %5.1f %%)\n", nm, 100*a_sin[[nm]], 100*a_con[[nm]]))

cat("\n-- Distribuciones del generador (SIN canonicas) --\n")
pr <- function(v, etq) { t <- table(v)
  cat(sprintf("   %-16s : %s\n", etq,
      paste(sprintf("%s=%.1f%%", names(t), 100*as.integer(t)/length(v)), collapse = "  "))) }
if (!all(is.na(vapply(reg_nc, function(r) r$s, integer(1)))))
  pr(vapply(reg_nc, function(r) r$s, integer(1)), "factor s")
pr(vapply(reg_nc, function(r) abs(r$tc), integer(1)), "|t_c| clave")
pr(vapply(reg_nc, function(r) as.integer(rank(r$dx, ties.method="min")[r$j]), integer(1)), "rango |dx|")
pr(vapply(reg_nc, function(r) sum(f_cell(r)), integer(1)), "ocup. celda")
pr(vapply(reg_nc, function(r) gcd_vec(r)[r$j], numeric(1)), "gcd de la clave")
cat(sprintf("   %-16s : %s\n", "|dx| clave",
    paste(sort(unique(vapply(reg_nc, function(r) r$dx[r$j], numeric(1)))), collapse=",")))
cat(sprintf("   %-16s : media %.1f  max %d\n", "intentos bucle",
    mean(vapply(reg_nc, function(r) r$intento, integer(1)), na.rm = TRUE),
    max(vapply(reg_nc, function(r) r$intento, integer(1)), na.rm = TRUE)))
cat(sprintf("   %-16s : %.1f %%\n", "P(p>0)", 100*mean(vapply(reg_nc, function(r) r$p > 0, logical(1)))))
ncod <- length(unique(unlist(lapply(reg_nc, function(r) r$cod[r$cod != "CORRECTA"]))))
cat(sprintf("   %-16s : %d errores distintos, %d ternas distintas\n", "variedad pool", ncod,
    length(unique(vapply(reg_nc, function(r) paste(sort(r$cod[r$cod!="CORRECTA"]), collapse="+"), character(1))))))
cat(sprintf("   %-16s : max coord %d\n", "caja",
    max(vapply(reg_nc, function(r) max(abs(c(r$ptx, r$pty))), numeric(1)))))

# ---- helper §P7 -------------------------------------------------------------
set.seed(4242L)
# Las reglas se pasan TAL CUAL: devuelven el conjunto superviviente (logico) y el
# helper lo puntua 1/|S|. Antes se sorteaba una opcion entre los supervivientes
# para encajar en la convencion 0/1, lo que anadia varianza gratis y era la razon
# declarada de que esta cifra y la de auditoria_propia.R fueran incomparables.
mk <- function(x) nueva_regla(names(R)[x], R[[x]]$fam, local({ ff <- R[[x]]$f
  function(op) as.logical(ff(attr(op, "reg"))) }))
reglas_p7 <- lapply(seq_along(R), mk)
mk_op <- function(rg) lapply(rg, function(r) structure(as.character(seq_along(r$dx)), reg = r))
for (par in list(list(reg, "CON canonicas"), list(reg_nc, "SIN canonicas"))) {
  res <- evaluar_bateria(reglas_p7, mk_op(par[[1]]),
                         vapply(par[[1]], function(r) r$j, integer(1)))
  cat(sprintf("\n>>> HELPER §P7 [%s]: %s | max %.1f %% (%s) | nulo %.1f %% | exceso %+.1f pp (ruido %.1f pp, corte %+.1f pp)\n",
              par[[2]], res$veredicto, 100*res$max_obs, res$regla_top,
              100*res$techo_nulo, 100*res$exceso, 100*res$ruido, 100*res$corte_canal))
  if (identical(par[[2]], "SIN canonicas")) assign("res_p7", res)
}
cat(sprintf("    familias con sonda: %s | sin sonda: %s\n",
            paste(res_p7$familias_con_sonda, collapse=", "),
            if (length(res_p7$familias_sin_sonda)) paste(res_p7$familias_sin_sonda, collapse=", ") else "(ninguna)"))
cat("\nEXIT: el veredicto que gobierna este ejercicio es el de auditoria_propia.R,\n")
cat("que cierra por CONJUNCIONES. Este bloque acredita solo la parte ATOMICA, con\n")
cat("el mismo criterio (exceso sobre el techo nulo) y los mismos cortes: si los dos\n")
cat("discrepan ahora, es porque miden ambitos distintos, no varas distintas.\n")
quit(status = exit_bateria(res_p7))
