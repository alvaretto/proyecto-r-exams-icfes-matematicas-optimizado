#!/usr/bin/env python3
"""editar_verificador_c1c.py — tanda C1c del ciclo 2026-11-02 del CLOZE (2026-11-03).

Sobre verificar_espacio_cloze.R (md5 de partida 6d37faad687a5fc11e69b763429b6624, comprobado):
  1. Cabecera: la tanda C1c.
  2. Comentario de «marca»: dice lo que hace la línea que la calcula (sol_p6 y rol_p6 del entorno). H3 de la r5.
  3. Sonda Crit-P6 (H1 de la r5), aparte de H1-Sol: el criterio literal, una vez por versión; sin cifras en el tramo general;
     control exacto.
  4. mut_h1(): la convención _y_otra y _y_control (H3 de la r5), con sus esperados declarados: MC50 y MC54, _y_otra; MC51,
     _y_otra_y_control; MC53, sin cambio.
  5. P6_NUEVO: la redacción nueva, leída del .Rmd (no se transcribe).
  6. MC55 (mutante del entorno, marca de HP en una copia), MC56 (vuelve a la frase de 6f6f1b13) y MC57 (cifra en el tramo
     general, con el literal intacto), y la tabla de mutantes.
Cada patrón se exige exactamente una vez.
Uso: python3 -I editar_verificador_c1c.py <verificador> <copia 6f6f1b13> <.Rmd nuevo>
"""
import hashlib
import sys

ver_p, viejo_p, nuevo_p = sys.argv[1:4]
leer = lambda p: open(p, encoding="utf-8").read()
txt = leer(ver_p)
md5 = hashlib.md5(txt.encode("utf-8")).hexdigest()
assert md5 == "6d37faad687a5fc11e69b763429b6624", f"md5 de partida inesperado: {md5}"
assert hashlib.md5(leer(viejo_p).encode("utf-8")).hexdigest() == "6f6f1b1311b0d681bd974c719ecd75d5"


def bloque(t):
    assert t.count('cat("Parte 6. Cada hipótesis') == 1
    i = t.index('cat("Parte 6. Cada hipótesis')
    j = t.index('sep = "")', i) + len('sep = "")')
    b = t[i:j]
    assert ")---" not in b
    return b


P6_ANTES = bloque(leer(viejo_p))
P6_AHORA = bloque(leer(nuevo_p))
assert "Por eso basta" in P6_ANTES and "Por eso, con un curso más o uno menos" in P6_AHORA


def cambia(t, de, a):
    n = t.count(de)
    assert n == 1, f"el patrón aparece {n} veces (se exige 1): {de[:80]!r}"
    return t.replace(de, a, 1)


# 1. Cabecera
CAB = "##       (h2_exacta sin mutar) y con el conteo de los pendientes que abre fuera de j39.\n"
txt = cambia(txt, CAB, CAB + r'''##       Tanda C1c (2026-11-03): H1 de la FASE 2C r5, la sonda Crit-P6 sobre el criterio general del párrafo «Parte 6.» (el
##       literal una vez por versión y ninguna cifra en el tramo general; control exacto): MC56 y MC57. H3 de la r5: mut_h1()
##       con la convención _y_otra y _y_control, y MC55, mutante del entorno que prueba sola la componente «marca».
''')

# 2. Comentario de «marca»
txt = cambia(txt, "##   marca  la conclusión contradice la marca de HP o de HG en exsolution;\n",
             r'''##   marca  la conclusión contradice la marca de HP o de HG, tomada de sol_p6 y rol_p6 del entorno, no del exsolution emitido
##          (esa lectura la hace el oráculo r10 del paso 8; H3 de la FASE 2C r5);
''')
txt = cambia(txt, "«un curso menos». Mutantes: MC50 a MC54.\n", "«un curso menos». Mutantes: MC50 a MC55.\n")

# 3. Sonda Crit-P6, antes de E5
E5 = "## ---- E5 ---"
txt = cambia(txt, E5, r'''## ---- C1c (tanda C1c, 2026-11-03): H1 de la FASE 2C r5, el criterio general del párrafo «Parte 6.» ----------------------------
## Sonda Crit-P6, sobre la Solution emitida y sobre el texto crudo. H1-Sol no ve el criterio general, porque solo lee las
## comparaciones con cifras; por eso esta sonda va aparte y su mutante no mueve H1-Sol. Cuenta por versión:
##   falta  el párrafo no contiene el criterio literal exactamente una vez;
##   cifra  el tramo general del párrafo (sin «Parte 6.», hasta «Con un curso más, el grado») lleva alguna cifra.
## Control EXACTO: el literal aparece una vez por versión, nv apariciones en total. Mutantes: MC56 (vuelve a la frase de
## 6f6f1b13) y MC57 (una cifra en el tramo general, con el literal intacto).
P6_CRITERIO <- "Por eso, con un curso más o uno menos, basta comparar ese promedio con la diferencia entre los totales: si el promedio es mayor, el ganador cambia; si es menor, no."
COMP_CRIT <- c("falta", "cifra")
crit_p6 <- function(sol_txt, envs = ENVS_NAT, lit = P6_CRITERIO) {
  ex <- parse(text = sol_txt, encoding = "UTF-8")
  z <- c(falta = 0, cifra = 0, n_crit = 0, sin_parrafo = 0, err = 0)
  for (en in envs) {
    t <- tryCatch(paste(capture.output(eval(ex, envir = new.env(parent = en))), collapse = "\n"), error = function(e) NA_character_)
    if (is.na(t)) { z[["err"]] <- z[["err"]] + 1; next }
    pr <- regmatches(t, regexpr("Parte 6\\. Cada hipótesis[^\\n]*", t, perl = TRUE))
    if (length(pr) != 1L) { z[["sin_parrafo"]] <- z[["sin_parrafo"]] + 1; next }
    n <- lengths(regmatches(pr, gregexpr(lit, pr, fixed = TRUE)))
    z[["n_crit"]] <- z[["n_crit"]] + n
    if (n != 1L) z[["falta"]] <- z[["falta"]] + 1
    gen <- sub("Con un curso más, el grado.*$", "", sub("^Parte 6\\. ", "", pr), perl = TRUE)
    if (grepl("[0-9]", gen)) z[["cifra"]] <- z[["cifra"]] + 1
  }
  z
}
CRIT <- crit_p6(SOL_C)
ok(CRIT[["err"]] == 0 && CRIT[["sin_parrafo"]] == 0 && all(CRIT[COMP_CRIT] == 0),
   sprintf("C1c · Crit-P6, criterio general del párrafo «Parte 6.» (%d versiones), leído del texto crudo: sin el literal exactamente una vez %d · con cifras en el tramo general %d · versiones sin el párrafo %d",
           nv, as.integer(CRIT[["falta"]]), as.integer(CRIT[["cifra"]]), as.integer(CRIT[["sin_parrafo"]])))
ok(CRIT[["n_crit"]] == nv, sprintf("C1c · Crit-P6, control exacto: %d apariciones del criterio literal (esperadas %d)", as.integer(CRIT[["n_crit"]]), nv))

''' + E5)

# 5. P6_NUEVO (el literal de MC51), leído de los archivos
txt = cambia(txt, 'P6_NUEVO <- r"---(' + P6_ANTES + ')---"', 'P6_NUEVO <- r"---(' + P6_AHORA + ')---"')

# Comentario de los mutantes de H1-Sol
txt = cambia(txt, "## cifras por versión: muere por H1-Sol y también por el control, y se declara así. El párrafo viejo y el nuevo, literales.\n",
             r'''## cifras por versión: muere por H1-Sol y también por el control, y se declara así. El párrafo viejo y el nuevo, literales.
## H3 de la FASE 2C r5 (tanda C1c): el veredicto sigue la convención de mut_l6 (MC45 a MC48). cazado_por_su_sonda, si su
## componente es la única afectada; cazado_por_su_sonda_y_otra, si hay otras; _y_control, si además se mueve el control exacto.
## MC50 mueve falsa y concl; MC54, cifra y falsa; MC51, falsa, marca y el control; MC53, solo esp. La componente «marca» no cae
## sola con ningún mutante de texto: la prueba MC55, sobre una copia de los entornos.
''')

# 4 y 6. mut_h1(), sus esperados y los mutantes nuevos
ini = 'mut_h1 <- function(id, de, a, comp, desc, esperado = "cazado_por_su_sonda") {'
fin = 'cat("  Decisión 2 a: las nueve reglas'
assert txt.count(ini) == 1 and txt.count(fin) == 1
i, j = txt.index(ini), txt.index(fin)
viejo = txt[i:j]
assert viejo.count('mut_h1("MC5') == 4 and "MC55" not in viejo
NUEVO = r'''juzga_h1 <- function(id, R, comp, desc, esperado) {
  dif <- R$z[COMP_H1] - H1S$z[COMP_H1]; hits <- names(dif)[dif != 0]
  ctrl <- R$z[["n_cmp"]] != 2 * nv || R$z[["n_mas"]] != nv || R$z[["n_menos"]] != nv || R$z[["sin_parrafo"]] > 0
  ver <- if (R$z[["err"]] > 0) "cazado_por_otra" else if (!comp %in% hits) (if (length(hits) || ctrl) "cazado_por_otra" else "no_detectado") else
    paste0(if (identical(hits, comp)) "cazado_por_su_sonda" else "cazado_por_su_sonda_y_otra", if (ctrl) "_y_control")
  d <- sprintf("%s; control: %d comparaciones (%d «un curso más», %d «un curso menos»; esperadas %d, %d y %d)",
               if (length(hits)) paste(sprintf("%s %+d", hits, as.integer(dif[hits])), collapse = ", ") else "ninguna componente",
               as.integer(R$z[["n_cmp"]]), as.integer(R$z[["n_mas"]]), as.integer(R$z[["n_menos"]]), 2L * nv, nv, nv)
  ok(ver == esperado, sprintf("%s (%s): %s — %s (esperado: %s)", id, desc, d, ver, esperado))
  filas_mut[[id]] <<- c(id, paste("C1b · H1-Sol:", comp), d, ver, NA)
}
mut_h1 <- function(id, de, a, comp, desc, esperado = "cazado_por_su_sonda") {
  n <- lengths(regmatches(SOL_C, gregexpr(de, SOL_C, fixed = TRUE)))
  if (n != 1L) { falla(id, " MAL CONSTRUIDO: el patrón aparece ", n, " veces en el chunk solucion"); filas_mut[[id]] <<- c(id, paste("C1b · H1-Sol:", comp), "-", "mal_construido", NA); return(invisible()) }
  juzga_h1(id, h1_p6(sub(de, a, SOL_C, fixed = TRUE)), comp, desc, esperado)
}
mut_h1("MC50", 'if (hp_supera) "más" else "menos", " que la diferencia de ",', 'if (hp_supera) "menos" else "menos", " que la diferencia de ",',
       "falsa", "«un curso más» dice «menos» también cuando el promedio supera la diferencia", esperado = "cazado_por_su_sonda_y_otra")
mut_h1("MC51", P6_NUEVO, P6_VIEJO, "falsa", "vuelve a la redacción de 27f789dc", esperado = "cazado_por_su_sonda_y_otra_y_control")
mut_h1("MC53", 'dp$D, ED, "kilos: ", if (hp_supera)', 'dp$D, " kilos: ", if (hp_supera)', "esp",
       "«un curso más»: espacio normal entre la diferencia y «kilos»")
mut_h1("MC54", '" habría sumado a su total ", dp$aP, ED,', '" habría sumado a su total ", dp$aG, ED,', "cifra",
       "«un curso más» cita el promedio del grado que ganó", esperado = "cazado_por_su_sonda_y_otra")
## MC55 (C1c, H3 de la r5): mutante del ENTORNO, no del texto. Voltea la marca de HP en una COPIA de los entornos: la Solution
## emite el mismo párrafo y solo puede caer «marca». Las guardas miran las copias (obligación 3 del contrato de mutación): el
## volteo llegó, a una sola entrada por versión, y los entornos originales siguen intactos.
SOL6_0 <- lapply(ENVS_NAT, function(en) en$sol_p6)
E55 <- lapply(ENVS_NAT, function(en) { e <- list2env(as.list(en, all.names = TRUE), parent = parent.env(en))
  i <- e$rol_p6 == "hp"; e$sol_p6[i] <- !e$sol_p6[i]; e })
llego55 <- all(mapply(function(o, m) { i <- o$rol_p6 == "hp"
  sum(i) == 1L && !identical(o, m) && isTRUE(m$sol_p6[i] == !o$sol_p6[i]) && identical(m$sol_p6[!i], o$sol_p6[!i]) && identical(m$rol_p6, o$rol_p6) }, ENVS_NAT, E55)) &&
  identical(SOL6_0, lapply(ENVS_NAT, function(en) en$sol_p6))
if (!llego55) { falla("MC55 MAL CONSTRUIDO: el volteo de la marca de HP no llegó a la copia o tocó los entornos originales")
  filas_mut[["MC55"]] <- c("MC55", "C1b · H1-Sol: marca", "-", "mal_construido", NA)
} else juzga_h1("MC55", h1_p6(SOL_C, envs = E55), "marca", "la marca de HP, volteada en una copia de los entornos", "cazado_por_su_sonda")
## MC56 y MC57 (C1c, H1 de la r5): Crit-P6, sobre el chunk «solucion» (en memoria, patrón único). Cada uno declara su componente,
## y el veredicto exige además que H1-Sol y su control no se muevan: MC56 toca solo el criterio, no las comparaciones con cifras.
P6_CRIT_VIEJO <- "Por eso basta comparar ese promedio con la diferencia entre los totales: si es mayor, el ganador cambia; si es menor, no."
mut_crit <- function(id, de, a, comp, desc, lit_mut = NULL) {
  et <- paste("C1c · Crit-P6:", comp); n <- lengths(regmatches(SOL_C, gregexpr(de, SOL_C, fixed = TRUE)))
  if (n != 1L) { falla(id, " MAL CONSTRUIDO: el patrón aparece ", n, " veces en el chunk solucion"); filas_mut[[id]] <<- c(id, et, "-", "mal_construido", NA); return(invisible()) }
  txt <- sub(de, a, SOL_C, fixed = TRUE)
  if (!is.null(lit_mut) && crit_p6(txt, lit = lit_mut)[["n_crit"]] != nv) { falla(id, " MAL CONSTRUIDO: el texto mutado no emite lo que el mutante declara"); filas_mut[[id]] <<- c(id, et, "-", "mal_construido", NA); return(invisible()) }
  C <- crit_p6(txt); R <- h1_p6(txt); dif <- C[COMP_CRIT] - CRIT[COMP_CRIT]; hits <- names(dif)[dif != 0]
  cmp_h1 <- c(COMP_H1, "n_cmp", "n_mas", "n_menos", "sin_parrafo", "err"); h1_mov <- !identical(R$z[cmp_h1], H1S$z[cmp_h1])
  ver <- if (C[["err"]] > 0) "cazado_por_otra" else if (!comp %in% hits) (if (length(hits) || h1_mov) "cazado_por_otra" else "no_detectado") else
    if (identical(hits, comp) && !h1_mov) "cazado_por_su_sonda" else "cazado_por_su_sonda_y_otra"
  d <- sprintf("%s; control: %d apariciones del literal (sin mutar %d); H1-Sol %s",
               if (length(hits)) paste(sprintf("%s %+d", hits, as.integer(dif[hits])), collapse = ", ") else "ninguna componente",
               as.integer(C[["n_crit"]]), as.integer(CRIT[["n_crit"]]), if (h1_mov) "se mueve" else "no se mueve")
  ok(ver == "cazado_por_su_sonda", sprintf("%s (%s): %s — %s", id, desc, d, ver))
  filas_mut[[id]] <<- c(id, et, d, ver, NA)
}
mut_crit("MC56", r"---(Por eso, con un curso más o uno menos, basta comparar ese promedio ",
    "con la diferencia entre los totales: si el promedio es mayor, el ganador cambia; si es menor, no. ",)---",
  r"---(Por eso basta comparar ese promedio ",
    "con la diferencia entre los totales: si es mayor, el ganador cambia; si es menor, no. ",)---",
  "falta", "vuelve a la frase de 6f6f1b13, sin tocar las comparaciones", lit_mut = P6_CRIT_VIEJO)
mut_crit("MC57", '"promedio al total, y un curso menos lo resta. Por eso, con un curso más', '"promedio al total, y un curso menos lo resta (", dp$D, "). Por eso, con un curso más',
         "cifra", "una cifra de la versión en el tramo general, con el literal intacto")
'''
txt = txt[:i] + NUEVO + txt[j:]

# Tabla de mutantes
txt = cambia(txt, '"MC53", "MC54"), c("id", "veredicto", "sonda_esperada", "sonda_real")]',
             '"MC53", "MC54", "MC55", "MC56", "MC57"), c("id", "veredicto", "sonda_esperada", "sonda_real")]')

assert not any(ch in txt for ch in (" ", " ", " ", "⁠"))
with open(ver_p, "w", encoding="utf-8") as f:
    f.write(txt)
print("md5 de partida", md5, "· nuevo", hashlib.md5(txt.encode("utf-8")).hexdigest())
