#!/usr/bin/env python3
"""editar_verificador_c1b.py — tanda C1b del ciclo 2026-11-02 del CLOZE (2026-11-03).

Sobre verificar_espacio_cloze.R (md5 de partida 6e76321a22e2026adaa3a4b5f576f070, comprobado):
  1. Cabecera: la tanda C1b.
  2. Sonda H1-Sol (H1 de la FASE 2C r4) sobre el párrafo «Parte 6.» de la Solution emitida, con control exacto.
  3. MC52 en E6: la guarda del data_generation que ata la comparación a v_hp y v_hg.
  4. MC39 (H2 de la r4): línea base h2_exacta(COD_C, 1L) y veredicto con el conteo de pendientes fuera de j39.
  5. MC50, MC51, MC53 y MC54 (H1-Sol) y la tabla de mutantes.
Los bloques del párrafo viejo y del nuevo se leen de los archivos (copia de 27f789dc y .Rmd nuevo), no se transcriben.
Uso: python3 -I editar_verificador_c1b.py <verificador> <copia 27f789dc> <.Rmd nuevo>
"""
import hashlib
import sys

ver_p, viejo_p, nuevo_p = sys.argv[1:4]
leer = lambda p: open(p, encoding="utf-8").read()
txt = leer(ver_p)
md5 = hashlib.md5(txt.encode("utf-8")).hexdigest()
assert md5 == "6e76321a22e2026adaa3a4b5f576f070", f"md5 de partida inesperado: {md5}"
assert hashlib.md5(leer(viejo_p).encode("utf-8")).hexdigest() == "27f789dc6327f7d6bda72cded61502cd"


def bloque(t):
    i = t.index('cat("Parte 6. Cada hipótesis')
    assert t.count('cat("Parte 6. Cada hipótesis') == 1
    j = t.index('sep = "")', i) + len('sep = "")')
    b = t[i:j]
    assert ")---" not in b
    return b


P6_VIEJO = bloque(leer(viejo_p))
P6_NUEVO = bloque(leer(nuevo_p))
assert "si y solo si" in P6_VIEJO and "hp_supera" in P6_NUEVO

CAB_DE = r'''##       U+2009, U+202F ni U+2060 en el .Rmd ni en este verificador: MC44.
'''
CAB_A = CAB_DE + r'''##       Tanda C1b (2026-11-03): H1 de la FASE 2C r4, la sonda H1-Sol sobre el párrafo «Parte 6.» de la Solution emitida
##       (cada comparación con cifras, verdadera leída del texto; control exacto de dos por versión): MC50, MC51, MC53 y
##       MC54; la guarda del data_generation que ata la comparación a v_hp y v_hg: MC52. H2 de la r4: MC39 con línea base
##       (h2_exacta sin mutar) y con el conteo de los pendientes que abre fuera de j39.
'''

H1_DE = r'''   "C1 · L-6: ni el .Rmd ni este verificador llevan U+00A0, U+2009, U+202F o U+2060 literales")
'''
H1_A = H1_DE + r'''## ---- C1b (tanda C1b, 2026-11-03): H1 de la FASE 2C r4, el párrafo «Parte 6.» de la Solution ------------------------------
## Sonda H1-Sol, sobre la Solution emitida (el chunk «solucion» en las N versiones de la mezcla, como la de L-6). Lee del TEXTO,
## con U+00A0 como espacio, cada comparación con cifras del párrafo «Parte 6.»: una cifra con «kilos», «más», «menos», «mayor»
## o «menor» seguido de «que» y otra cifra con «kilos», sin cruzar un punto, dos puntos ni punto y coma. La tabla del otro
## colegio se lee como se muestra (tabla_leida(), sin las funciones del .Rmd). Por comparación cuenta:
##   falsa  la desigualdad que dice el texto no se cumple con las dos cifras que cita;
##   cifra  la primera cifra no es el promedio del grado que nombra su frase, la segunda no es la diferencia entre los totales
##          de la tabla, o el grado no es el que perdió («un curso más») o el que ganó («un curso menos»);
##   concl  la conclusión de su frase no se sigue del «más» o «menos» escrito, o falta, o hay dos;
##   marca  la conclusión contradice la marca de HP o de HG en exsolution;
##   esp    en el párrafo, una cifra seguida de «kilos» con espacio normal (L-6, sobre el texto crudo).
## Control EXACTO: dos comparaciones por versión, una con «un curso más» y otra con «un curso menos». Mutantes: MC50 a MC54.
P6_CONCL <- list(mas = c("habría ganado" = TRUE, "no le habría alcanzado para ganar" = FALSE),
                 menos = c("habría perdido" = TRUE, "habría seguido ganando" = FALSE))
RE_CMP <- "([0-9]+) kilos[^0-9.:;]*?\\b(más|menos|mayor|menor) que [^0-9.:;]*?([0-9]+) kilos"
COMP_H1 <- c("falsa", "cifra", "concl", "marca", "esp")
h1_p6 <- function(sol_txt, envs = ENVS_NAT) {
  ex <- parse(text = sol_txt, encoding = "UTF-8")
  z <- c(falsa = 0, cifra = 0, concl = 0, marca = 0, esp = 0, n_cmp = 0, n_mas = 0, n_menos = 0, sin_parrafo = 0, err = 0)
  ramas <- c(mas_si = 0, mas_no = 0, menos_si = 0, menos_no = 0)
  for (en in envs) {
    t <- tryCatch(paste(capture.output(eval(ex, envir = new.env(parent = en))), collapse = "\n"), error = function(e) NA_character_)
    if (is.na(t)) { z[["err"]] <- z[["err"]] + 1; next }
    pr <- regmatches(t, regexpr("Parte 6\\. Cada hipótesis[^\\n]*", t, perl = TRUE))
    if (length(pr) != 1L) { z[["sin_parrafo"]] <- z[["sin_parrafo"]] + 1; next }
    z[["esp"]] <- z[["esp"]] + cuenta_re("[0-9] kilos?\\b", pr)
    tb <- tabla_leida(en$tabla_par_md); tb$T <- tb$n * tb$a; tb$nombre <- tolower(tb$nombre)
    gan <- tb$nombre[which.max(tb$T)]; per <- tb$nombre[which.min(tb$T)]; Dt <- max(tb$T) - min(tb$T)
    for (cl in strsplit(norm_ed(pr), "[.;]")[[1]]) {
      m <- regmatches(cl, regexec(RE_CMP, cl, perl = TRUE))[[1]]
      if (!length(m)) next
      z[["n_cmp"]] <- z[["n_cmp"]] + 1
      x <- as.numeric(m[2]); y <- as.numeric(m[4]); sube <- m[3] %in% c("más", "mayor")
      if (!(if (sube) x > y else x < y)) z[["falsa"]] <- z[["falsa"]] + 1
      papel <- if (grepl("un curso más", cl, fixed = TRUE)) "mas" else if (grepl("un curso menos", cl, fixed = TRUE)) "menos" else NA_character_
      g <- regmatches(cl, regexec("el grado (\\S+)", cl, perl = TRUE))[[1]]
      quien <- if (identical(papel, "mas")) per else if (identical(papel, "menos")) gan else NA_character_
      if (is.na(papel) || length(g) != 2L || !identical(g[2], quien) || !isTRUE(x == tb$a[tb$nombre == quien]) || y != Dt)
        z[["cifra"]] <- z[["cifra"]] + 1
      if (is.na(papel)) { z[["concl"]] <- z[["concl"]] + 1; next }
      z[[paste0("n_", papel)]] <- z[[paste0("n_", papel)]] + 1
      hay <- vapply(names(P6_CONCL[[papel]]), function(k) grepl(k, cl, fixed = TRUE), NA)
      if (sum(hay) != 1L) { z[["concl"]] <- z[["concl"]] + 1; next }
      cv <- unname(P6_CONCL[[papel]][hay])
      if (cv != sube) z[["concl"]] <- z[["concl"]] + 1
      if (!identical(cv, isTRUE(en$sol_p6[en$rol_p6 == if (papel == "mas") "hp" else "hg"]))) z[["marca"]] <- z[["marca"]] + 1
      k <- paste0(papel, if (sube) "_si" else "_no"); ramas[[k]] <- ramas[[k]] + 1
    }
  }
  list(z = z, ramas = ramas)
}
H1S <- h1_p6(SOL_C)
ok(H1S$z[["err"]] == 0 && H1S$z[["sin_parrafo"]] == 0 && all(H1S$z[COMP_H1] == 0),
   sprintf("C1b · H1-Sol, párrafo «Parte 6.» de la Solution (%d versiones), leído del texto: comparaciones falsas %d · cifra o grado que no son los de la tabla %d · conclusión que no se sigue del «más» o «menos» %d · conclusión contra la marca de HP o HG %d · cifra-kilos con espacio normal %d · versiones sin el párrafo %d",
           nv, as.integer(H1S$z[["falsa"]]), as.integer(H1S$z[["cifra"]]), as.integer(H1S$z[["concl"]]), as.integer(H1S$z[["marca"]]),
           as.integer(H1S$z[["esp"]]), as.integer(H1S$z[["sin_parrafo"]])))
ok(H1S$z[["n_cmp"]] == 2 * nv && H1S$z[["n_mas"]] == nv && H1S$z[["n_menos"]] == nv,
   sprintf("C1b · H1-Sol, control exacto: %d comparaciones con cifras (esperadas %d), %d con «un curso más» y %d con «un curso menos» (esperadas %d y %d); por rama: «un curso más» y «más» %d, «un curso más» y «menos» %d, «un curso menos» y «más» %d, «un curso menos» y «menos» %d",
           as.integer(H1S$z[["n_cmp"]]), 2L * nv, as.integer(H1S$z[["n_mas"]]), as.integer(H1S$z[["n_menos"]]), nv, nv,
           as.integer(H1S$ramas[["mas_si"]]), as.integer(H1S$ramas[["mas_no"]]), as.integer(H1S$ramas[["menos_si"]]), as.integer(H1S$ramas[["menos_no"]])))
'''

MC52_DE = r'''       llego = function(e) isTRUE(length(e$opciones_p6) == 4L && !any(grepl("^Ganó el concurso el grado", e$opciones_p6)))))
filas_mut <- list()
'''
MC52_A = r'''       llego = function(e) isTRUE(length(e$opciones_p6) == 4L && !any(grepl("^Ganó el concurso el grado", e$opciones_p6)))),
  ## MC52 (C1b, H1 de la FASE 2C r4): la comparación de la Solution de la Parte 6 toma el promedio del grado que ganó para
  ## «un curso más». Sonda: la guarda del data_generation que la ata a los veredictos sorteados.
  list(id = "MC52", sonda = "H1 r4: la comparación de la Solution de la Parte 6 coincide",
       desc = "hp_supera compara el promedio del grado que ganó con la diferencia",
       cambios = list(c("hp_supera <- dp$aP > dp$D", "hp_supera <- dp$aG > dp$D")),
       llego = function(e) isTRUE(e$hp_supera == (e$dp$aG > e$dp$D))))
filas_mut <- list()
'''

MC39_DE = r'''  ver <- if (!llego) "mal_construido" else if (nrow(j39)) "cazado_por_su_sonda" else if (any(VM$veredicto == "pendiente")) "cazado_por_otra" else "no_detectado"
  d <- if (nrow(j39)) paste(sprintf("%s · %s %s %+.1f pp (firmada %+.1f, tope %+.1f) -> pendiente con la regla por regla; cubierta con la de estrato",
                                    j39$estrato, j39$id, j39$papel, 100 * j39$exceso,
                                    vapply(seq_len(nrow(j39)), function(i) firma_p6_f(j39$estrato[i], j39$id[i], j39$papel[i]), 0),
                                    unname(FIRMA_P6_TOKEN[j39$estrato])), collapse = "; ") else
    sprintf("ninguna regla «perdió hubiera tenido» de la lista quedó pendiente y cubierta por estrato (masa con error %.3f)", if (is.null(HM$A)) NA else HM$masa_err)
  ok(ver == "cazado_por_su_sonda", sprintf("MC39 (L-C7, «o que pase de su cifra»: BP_gano gana «perdió hubiera tenido»): %s — %s", d, ver))
'''
MC39_A = r'''  ## H2 de la FASE 2C r4 (tanda C1b, 2026-11-03): línea base en la misma medida (h2_exacta sin mutar, con la Parte 1 de la
  ## semilla 1), donde las filas de j39 tienen que salir «cubierto»; y el veredicto cuenta los pendientes que el mutante abre
  ## fuera de j39 (los que ya lo eran en la línea base no cuentan), como juzgar_s1() del SCHOICE desde la S2b. El detractor
  ## r4 dedujo de los textos 6 n-gramas fuera de la lista («tenido conservando»… y «perdió conservando»…): el veredicto
  ## esperado es cazado_por_su_sonda_y_otra.
  HB <- h2_exacta(COD_C, 1L)
  VB <- if (is.null(HB$A)) NULL else p6_token_tabla(HB$A); if (!is.null(VB)) VB$veredicto <- p6_token_juzgar(VB)
  b39 <- if (is.null(VB)) VT7[0, ] else VB[clave_fila(VB) %in% clave_fila(j39), , drop = FALSE]
  base_ok <- nrow(j39) > 0L && nrow(b39) == nrow(j39) && all(b39$veredicto == "cubierto")
  pend_b <- if (is.null(VB)) character(0) else clave_fila(VB)[VB$veredicto == "pendiente"]
  o39 <- if (llego) VM[VM$veredicto == "pendiente" & !clave_fila(VM) %in% c(clave_fila(j39), pend_b), , drop = FALSE] else VT7[0, ]
  ver <- if (!llego || (nrow(j39) && !base_ok)) "mal_construido" else if (nrow(j39)) (if (nrow(o39)) "cazado_por_su_sonda_y_otra" else "cazado_por_su_sonda") else
    if (any(VM$veredicto == "pendiente")) "cazado_por_otra" else "no_detectado"
  bx <- function(k, campo) { j <- which(clave_fila(b39) == k); if (!length(j)) return(if (campo == "v") "sin fila" else NA_real_)
    if (campo == "v") b39$veredicto[j[1]] else 100 * b39$exceso[j[1]] }
  d <- if (nrow(j39)) paste(sprintf("%s · %s %s %+.1f pp (firmada %+.1f, tope %+.1f; línea base %+.1f, «%s») -> pendiente con la regla por regla; cubierta con la de estrato",
                                    j39$estrato, j39$id, j39$papel, 100 * j39$exceso,
                                    vapply(seq_len(nrow(j39)), function(i) firma_p6_f(j39$estrato[i], j39$id[i], j39$papel[i]), 0),
                                    unname(FIRMA_P6_TOKEN[j39$estrato]),
                                    vapply(clave_fila(j39), bx, 0, campo = "x"), vapply(clave_fila(j39), bx, "", campo = "v")), collapse = "; ") else
    sprintf("ninguna regla «perdió hubiera tenido» de la lista quedó pendiente y cubierta por estrato (masa con error %.3f)", if (is.null(HM$A)) NA else HM$masa_err)
  d <- paste0(d, sprintf(" · pendientes nuevos fuera de j39: %d%s (la línea base, sin mutar, tiene %d pendientes)", nrow(o39),
                         if (nrow(o39)) paste0(": ", paste(head(unique(o39$id), 8), collapse = ", ")) else "", length(pend_b)))
  ok(ver == "cazado_por_su_sonda_y_otra", sprintf("MC39 (L-C7, «o que pase de su cifra»: BP_gano gana «perdió hubiera tenido»): %s — %s (esperado: cazado_por_su_sonda_y_otra)", d, ver))
'''

MUT_DE = r'''cat("  Decisión 2 a: las nueve reglas y sondas al 100 % (+75,0 pp) no pueden subir; ningún mutante puede sacarlas de «cubierto» y se declara.\n")
'''
MUT_A = r'''## MC50, MC51, MC53 y MC54 (C1b): H1-Sol, sobre el chunk «solucion» (en memoria, patrón único). MC50, MC53 y MC54 son
## ADITIVOS: el control exacto no se mueve. MC51 vuelve a la redacción de 27f789dc, que deja una sola comparación con dos
## cifras por versión: muere por H1-Sol y también por el control, y se declara así. El párrafo viejo y el nuevo, literales.
P6_NUEVO <- r"---(@@P6_NUEVO@@)---"
P6_VIEJO <- r"---(@@P6_VIEJO@@)---"
mut_h1 <- function(id, de, a, comp, desc, esperado = "cazado_por_su_sonda") {
  n <- lengths(regmatches(SOL_C, gregexpr(de, SOL_C, fixed = TRUE)))
  if (n != 1L) { falla(id, " MAL CONSTRUIDO: el patrón aparece ", n, " veces en el chunk solucion"); filas_mut[[id]] <<- c(id, paste("C1b · H1-Sol:", comp), "-", "mal_construido", NA); return(invisible()) }
  R <- h1_p6(sub(de, a, SOL_C, fixed = TRUE)); dif <- R$z[COMP_H1] - H1S$z[COMP_H1]; hits <- names(dif)[dif != 0]
  ctrl <- R$z[["n_cmp"]] != 2 * nv || R$z[["n_mas"]] != nv || R$z[["n_menos"]] != nv || R$z[["sin_parrafo"]] > 0
  ver <- if (R$z[["err"]] > 0) "cazado_por_otra" else if (!comp %in% hits) (if (length(hits) || ctrl) "cazado_por_otra" else "no_detectado") else
    if (ctrl) "cazado_por_su_sonda_y_control" else "cazado_por_su_sonda"
  d <- sprintf("%s; control: %d comparaciones (%d «un curso más», %d «un curso menos»; esperadas %d, %d y %d)",
               if (length(hits)) paste(sprintf("%s %+d", hits, as.integer(dif[hits])), collapse = ", ") else "ninguna componente",
               as.integer(R$z[["n_cmp"]]), as.integer(R$z[["n_mas"]]), as.integer(R$z[["n_menos"]]), 2L * nv, nv, nv)
  ok(ver == esperado, sprintf("%s (%s): %s — %s (esperado: %s)", id, desc, d, ver, esperado))
  filas_mut[[id]] <<- c(id, paste("C1b · H1-Sol:", comp), d, ver, NA)
}
mut_h1("MC50", 'if (hp_supera) "más" else "menos", " que la diferencia de ",', 'if (hp_supera) "menos" else "menos", " que la diferencia de ",',
       "falsa", "«un curso más» dice «menos» también cuando el promedio supera la diferencia")
mut_h1("MC51", P6_NUEVO, P6_VIEJO, "falsa", "vuelve a la redacción de 27f789dc", esperado = "cazado_por_su_sonda_y_control")
mut_h1("MC53", 'dp$D, ED, "kilos: ", if (hp_supera)', 'dp$D, " kilos: ", if (hp_supera)', "esp",
       "«un curso más»: espacio normal entre la diferencia y «kilos»")
mut_h1("MC54", '" habría sumado a su total ", dp$aP, ED,', '" habría sumado a su total ", dp$aG, ED,', "cifra",
       "«un curso más» cita el promedio del grado que ganó")
''' + MUT_DE

TM_DE = r'''"MC44", "MC45", "MC46", "MC47", "MC48", "MC49"), c("id", "veredicto", "sonda_esperada", "sonda_real")], quote = FALSE)'''
TM_A = r'''"MC44", "MC45", "MC46", "MC47", "MC48", "MC49", "MC50", "MC51", "MC52", "MC53", "MC54"), c("id", "veredicto", "sonda_esperada", "sonda_real")], quote = FALSE)'''

MUT_A = MUT_A.replace("@@P6_NUEVO@@", P6_NUEVO).replace("@@P6_VIEJO@@", P6_VIEJO)
for de, a in [(CAB_DE, CAB_A), (H1_DE, H1_A), (MC52_DE, MC52_A), (MC39_DE, MC39_A), (MUT_DE, MUT_A), (TM_DE, TM_A)]:
    n = txt.count(de)
    assert n == 1, f"el patrón aparece {n} veces (se exige 1): {de[:70]!r}"
    txt = txt.replace(de, a, 1)
with open(ver_p, "w", encoding="utf-8") as f:
    f.write(txt)
print("md5 de partida", md5, "· nuevo", hashlib.md5(txt.encode("utf-8")).hexdigest())
