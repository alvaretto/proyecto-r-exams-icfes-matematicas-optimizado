import sys, re
r2_path, cur_path, out_path = sys.argv[1:4]
t = open(r2_path, encoding="utf-8").read()
cur = open(cur_path, encoding="utf-8").read()
def rep(old, new, s):
    n = s.count(old)
    assert n == 1, f"patrón aparece {n} veces: {old[:80]!r}"
    return s.replace(old, new)
# (1) Cabecera: glosa provisional (se reescribe con las cifras exactas tras la enumeración)
i = t.index("##   - Selección (regla #22 §P7; FASE 2C r1, hallazgo H1)")
j = t.index("esas formas baja de ahí: o la clave es la central, o es la más larga en el código LaTeX.\n") + len("esas formas baja de ahí: o la clave es la central, o es la más larga en el código LaTeX.\n")
t = t[:i] + "##   - GLOSA_PROVISIONAL\n" + t[j:]
# (2) Feedback de VL-F: arreglo de la r3 (falso positivo de «cuanto» en el corrector), no es de escritura
old_f = ('    feedback = paste0("Incorrecta. Divide entre la distancia: según esta expresión, cuanto más largo el recorrido, ",\n'
         '      "menos vueltas, y debe ser al revés."),\n')
k = cur.index('    feedback = paste0("Incorrecta. Divide entre la distancia: según esta expresión, un recorrido más largo necesitaría ",')
k2 = cur.index("\n", cur.index("\n", k) + 1) + 1
t = rep(old_f, cur[k:k2], t)
# (3) Guarda de la tabla de pesos: r3 sin las marcas que solo servían a sus formas; topes por (nivel, atajo)
gi = t.index("## Guarda de la tabla de pesos (FASE 2C r1, H1).")
gj_txt = 'if (length(fallan) > 0L) stop("diagnosticidad: tabla de pesos — por encima del tope: ", paste(fallan, collapse = ", "))\n'
gj = t.index(gj_txt) + len(gj_txt)
ci = cur.index("ANCHO_GLIFO <- c(")
cj = cur.index("bigr_r <- function(t)")
cj = cur.index("\n", cj) + 1
modelo = cur[ci:cj]   # ANCHO_GLIFO, ancho_tex, tok_r, bigr_r (de la r3, sin cambios)
guarda = (
"## Guarda de la tabla de pesos (FASE 2C r1 H1, r2 H1 y r3 H4). Cinco atajos: centralidad por bigramas de símbolos,\n"
"## medoide por distancia de edición y «usa los tres datos» (detractor r1); «la que más comparte» (batería congelada);\n"
"## «la más ancha» con el ancho del render de TeX (detractor r2). Se calculan una vez por celda con una instancia de\n"
"## referencia (100, 150, P) y se ponderan con la tabla. En las formas de esta tabla los cinco dependen solo de la\n"
"## estructura de las expresiones: salida/verificar_vueltas.R lo comprueba celda por celda contra la enumeración\n"
"## exacta del diseño. Crédito de la clave: 1/(número de empatadas) si está entre ellas; si el atajo no señala ninguna\n"
"## opción, 1/4 (azar), como en la batería. Topes por (nivel, atajo): la cifra del diseño más 0,5 pp en cada forma y\n"
"## más 1 pp en el agregado; una tabla que suba cualquiera aborta el render. «La más ancha» vuelve con la escritura de la\n"
"## ronda 2 (decisión del profesor del 2026-11-10, «Volver a la de la ronda 2») y se firma en la pausa 3 (§P7-E).\n"
+ modelo +
"credito <- function(ganan) if (!length(ganan)) 0.25 else if (1L %in% ganan) 1 / length(ganan) else 0   # la clave va primera\n"
'ATAJOS <- c("centralidad", "medoide", "tres_datos", "mas_comparte", "ancho")\n'
"atajos_celda <- function(terna, est) {\n"
'  cods <- c("C", sub("VL-", "", strsplit(terna, "+", fixed = TRUE)[[1L]], fixed = TRUE))\n'
'  op <- vapply(cods, expr_de, "", est = est, k = "100", n = "150", v = "P")\n'
'  ts <- lapply(op, tok_r); u <- unique(unlist(ts)); ch <- intToUtf8(seq_along(u) + 64L, multiple = TRUE)\n'
'  sL <- rowSums(adist(vapply(ts, function(t) paste(ch[match(t, u)], collapse = ""), "")))\n'
"  J <- sapply(ts, function(a) sapply(ts, function(z)\n"
"    length(intersect(bigr_r(a), bigr_r(z))) / length(union(bigr_r(a), bigr_r(z)))))\n"
"  sJ <- colSums(J) - 1\n"
'  tk <- lapply(op, function(s) unique(tolower(regmatches(s, gregexpr("[A-Za-z]{3,}", s))[[1L]])))\n'
"  sM <- vapply(seq_along(tk), function(i) length(intersect(tk[[i]], unlist(tk[-i]))), 0L)\n"
"  w <- vapply(op, ancho_tex, 0)\n"
"  c(centralidad = credito(which(abs(sJ - max(sJ)) < 1e-12)), medoide = credito(which(sL == min(sL))),\n"
'    tres_datos = credito(which(vapply(ts, function(t) all(c("100", "150", "P") %in% t), TRUE))),\n'
"    mas_comparte = credito(which(sM == max(sM))),\n"
"    ancho = credito(which(abs(w - max(w)) < 0.01)))\n"
"}\n"
"TOPES <- TOPES_PROVISIONALES\n"
"revisar_tabla <- function(A, w, est) {   # pares «nivel: atajo» por encima de su tope\n"
"  w <- w / sum(w); fallan <- character(0); t_ag <- colSums(w * A)\n"
'  f_ag <- names(t_ag)[t_ag > TOPES["agregado", names(t_ag)] + 1e-9]; if (length(f_ag) > 0L) fallan <- paste("agregado:", f_ag)\n'
"  for (e in unique(est)) {\n"
"    en <- est == e; t_e <- colSums(w[en] * A[en, , drop = FALSE]) / sum(w[en])\n"
'    f_e <- names(t_e)[t_e > TOPES[e, names(t_e)] + 1e-9]; if (length(f_e) > 0L) fallan <- c(fallan, paste0(e, ": ", f_e))\n'
"  }\n"
"  fallan\n"
"}\n"
"activas <- which(CELDAS$peso > 0)\n"
"A_cel <- t(vapply(activas, function(i) atajos_celda(CELDAS$terna[i], CELDAS$estilo[i]), setNames(numeric(length(ATAJOS)), ATAJOS)))\n"
"w_cel <- CELDAS$peso[activas] / sum(CELDAS$peso[activas])\n"
"fallan <- revisar_tabla(A_cel, w_cel, CELDAS$estilo[activas])\n"
+ gj_txt)
t = t[:gi] + guarda + t[gj:]
# (4) Separación vertical entre opciones en HTML y Moodle (arreglo de la r3, no es de escritura)
ai = cur.index("## Separación vertical entre opciones en HTML y Moodle")
aj = cur.index("\n", cur.index('answerlist(paste0("<span', ai)) + 1
t = rep('answerlist(opciones, markup = "markdown")\n', cur[ai:aj], t)
open(out_path, "w", encoding="utf-8").write(t)
print("OK", len(t.splitlines()), "líneas")
