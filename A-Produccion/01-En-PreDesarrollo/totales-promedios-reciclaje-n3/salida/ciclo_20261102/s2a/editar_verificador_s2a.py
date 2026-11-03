"""Tanda S2a (2026-11-03): verificar_espacio.R con H1, H2 (sonda) y H5 de la FASE 2C r5.
Las regiones se ubican con anclas de texto (cada una exige una sola aparición); el texto nuevo no lleva escapes de
Unicode ni caracteres especiales literales: los construye intToUtf8() en R.
Uso: python3 -I editar_verificador_s2a.py <verificar_espacio.R> <copia_de_respaldo>"""
import sys, shutil, hashlib
ver, copia = sys.argv[1], sys.argv[2]
shutil.copyfile(ver, copia)
t = open(ver, encoding="utf-8").read()
antes = hashlib.md5(t.encode("utf-8")).hexdigest()


def una(s):
    n = t.count(s)
    if n != 1:
        sys.exit(f"ABORTO: el ancla aparece {n} veces (se exige 1): {s[:80]!r}")
    return t.index(s)


def reemplaza(de, a):
    global t
    una(de)
    t = t.replace(de, a)


def region(ini, fin, a):
    """Sustituye desde el comienzo de `ini` hasta el final de `fin` (incluido)."""
    global t
    i = una(ini); j = una(fin)
    assert j > i, "ancla final antes que la inicial"
    t = t[:i] + a + t[j + len(fin):]


# A1. Cabecera
reemplaza("##        canónica ya no asigna lado_gdis y rasgos() calcula su lado de las cifras.\n",
r"""##        canónica ya no asigna lado_gdis y rasgos() calcula su lado de las cifras.
## Tanda S2a (2026-11-03), FASE 2C r5. H2, decisión del profesor «Cerrarlo ahora»: la sonda S1-Sol signo cubre toda
##        fórmula, con unidad o sin ella (M37 para la guarda del .Rmd; M38 para la sonda). H1: controles positivos
##        exactos (1158, 730 y el signo dentro de una fórmula sin unidad, uno por versión con ERR-RESTA), cada uno con un
##        mutante que quita un solo sitio (M34, M35, M36), y separadores ampliados a todo carácter que no sea letra ni
##        cifra (M39 y M41, con U+2009, U+202F y U+2060). H5: guarda real del canal 2 sobre lado_gdis (M40); fuera el
##        ok() tautológico de S7.
""")

# A2. rasgos(): lado es el sorteado; NA en la canónica
reemplaza("""## S7 (tanda S1b relanzada, decisión del profesor del 2026-11-03): la canónica ya no asigna lado_gdis, que no usaba; su
## lado se calcula de sus cifras (x del distractor de G frente a D). En las demás versiones se lee lado_gdis, como antes.
""", """## S7 (tanda S1b relanzada, decisión del profesor del 2026-11-03): la canónica ya no asigna lado_gdis, que no usaba.
## H5 de la FASE 2C r5 (tanda S2a): lado es el lado SORTEADO para la cifra falsa de G (lado_gdis), y lo consume la
## guarda real del canal 2 (E2). La canónica no sortea lado: NA, y la guarda la deja fuera.
""")
reemplaza("""  lado = if (isTRUE(es_canonica)) (if (afs$gdis$x > d$D) "encima" else "debajo") else lado_gdis))""",
          """  lado = if (isTRUE(es_canonica)) NA_character_ else lado_gdis))""")

# A3. El ok() tautológico de S7, sustituido por la guarda real del canal 2
region("## S7: en la canónica, el lado sale de sus cifras y tiene que seguir dando «encima» (5 < 20 (D) < 40).\n",
       """CANON$X, CANON$D, CANON$lado))\n""",
r"""## H5 de la FASE 2C r5 (tanda S2a): el ok() de S7 era tautológico (el lado de la canónica salía de sus cifras, y la
## guarda verbatim ya fija x = 40 y D = 20). Lo sustituye una guarda real del canal 2 en las no canónicas de E1 y de los
## estratos: la cifra falsa de G cae del lado de D que se sorteó para ella (lado_gdis). M40 quita el filtro
## «cand_g <- cand_g[lado_ok, ]» y la tiene que ver esta guarda.
lado_cumple <- function(x) isTRUE(x$canon) || isTRUE((x$X < x$D) == (x$lado == "debajo"))
VL <- Filter(function(x) !isTRUE(x$canon), c(NAT, CU$CURSOS, CU$PROMEDIO))
n_lado_mal <- sum(!vapply(c(NAT, CU$CURSOS, CU$PROMEDIO), lado_cumple, TRUE))
ok(n_lado_mal == 0L, sprintf("canal 2 (H5 r5): en las %d no canónicas de E1 y de los estratos, la cifra falsa de G cae del lado de D sorteado para ella (debajo en %d, encima en %d); fuera de su lado: %d",
                             length(VL), sum(vapply(VL, function(x) identical(x$lado, "debajo"), TRUE)),
                             sum(vapply(VL, function(x) identical(x$lado, "encima"), TRUE)), n_lado_mal))
""")

# A4. M33 con el mensaje nuevo de la guarda, y M37
reemplaza("""  list(id = "M33", sonda = "L-6: el signo que sigue a la unidad de una fórmula va dentro de la fórmula",""",
          """  list(id = "M33", sonda = "L-6: el signo que sigue a una fórmula, con unidad o sin ella, va dentro de la fórmula",""")
reemplaza(r"""       llego = function(e) grepl("\\\\text\\{kilos\\}\\$\\.$", e$fb[["clave"]])))""",
r"""       llego = function(e) grepl("\\\\text\\{kilos\\}\\$\\.$", e$fb[["clave"]])),
  ## Tanda S2a (H2 de la FASE 2C r5): M37 devuelve el punto de la explicación de ERR-RESTA fuera de su fórmula sin
  ## unidad («= 5$.»), como en 6c9d7705. La guarda del signo de la S1b solo miraba las fórmulas con unidad: no lo veía.
  list(id = "M37", sonda = "L-6: el signo que sigue a una fórmula, con unidad o sin ella, va dentro de la fórmula",
       desc = "la explicación de ERR-RESTA deja el punto fuera de su fórmula sin unidad (revierte H2)",
       cambios = list(c('p$delta, "\\\\text{.}$ Esa resta', 'p$delta, "$. Esa resta')),
       llego = function(e) grepl("= 5$. Esa resta", e$errores_conceptuales[["ERR-RESTA"]]$explica(
         list(aG = 40, aP = 45, delta = 5, tipo = "sede", nG = 5, nP = 4, TG = 200, TP = 180, D = 20), list(x = 5)),
         fixed = TRUE)))""")

# A5. M40, mutante de medición de la guarda real del canal 2
reemplaza("""ok(n_c3 > 0, sprintf("M15 (quita el filtro del canal 3): la guarda del canal 3 lo detecta en %d de %d versiones", n_c3, length(V)))\n""",
r"""ok(n_c3 > 0, sprintf("M15 (quita el filtro del canal 3): la guarda del canal 3 lo detecta en %d de %d versiones", n_c3, length(V)))
## Mutante de medición M40 (H5 de la FASE 2C r5): sin el filtro del lado sorteado, la cifra falsa de G puede caer del
## otro lado de D. La guarda real del canal 2 (E2) lo tiene que ver, sin que ninguna guarda del .Rmd aborte el render.
mm <- mutar(list(c("cand_g <- cand_g[lado_ok, ]", "cand_g <- cand_g")))
if (!is.null(mm$error)) falla(paste("M40 MAL CONSTRUIDO:", mm$error)) else {
  V <- list(); err40 <- 0L; llego40 <- FALSE
  for (s in seq_len(N)) {
    r <- correr(s, mm$expr); if (!is.null(r$err)) { err40 <- err40 + 1L; next }
    if (!isTRUE(r$env$es_canonica) && nrow(r$env$cand_g) > sum(r$env$lado_ok)) llego40 <- TRUE
    V[[length(V) + 1]] <- rasgos(r$env)
  }
  n40 <- sum(!vapply(V, lado_cumple, TRUE))
  ver40 <- if (!llego40) "mal_construido" else if (err40 > 0L) "cazado_por_otra" else if (n40 > 0L) "cazado_por_su_sonda" else "no_detectado"
  ok(ver40 == "cazado_por_su_sonda", sprintf("M40 (quita el filtro cand_g[lado_ok, ]): la guarda real del canal 2 lo detecta en %d de %d versiones de E1 (%d con error) — %s",
                                             n40, length(V), err40, ver40))
}
""")

# A6. s1_sol con el texto emitido en memoria y el data_generation del mismo texto
region("""s1_sol <- function(lin, semillas, pat = PAT_S1) {\n""",
       """    lengths(regmatches(t, gregexpr(pat, t, perl = TRUE))) }, 0))\n}\n""",
r"""## Tanda S2a (H1 de la FASE 2C r5): el texto emitido se calcula una sola vez por versión del .Rmd (memoria por
## contenido), y el data_generation sale del mismo texto que las otras tres partes, así que un mutante del
## data_generation (M34, M35, M36) llega también a la Solution. Con el .Rmd sin mutar es el mismo EXPR de siempre.
MEMO_SOL <- list()
textos_sol <- function(lin, semillas) {
  for (m in MEMO_SOL) if (identical(m$lin, lin) && identical(m$sem, semillas)) return(m$t)
  ex <- chunk_de(lin, "data_generation")
  ch <- lapply(c("opciones", "solucion", "feedback"), function(nm) chunk_de(lin, nm))
  t <- vapply(semillas, function(s) { r <- correr(s, ex); if (!is.null(r$err)) return(NA_character_)
    paste(unlist(lapply(ch, function(e) capture.output(eval(e, envir = r$env)))), collapse = "\n") }, "")
  MEMO_SOL[[length(MEMO_SOL) + 1L]] <<- list(lin = lin, sem = semillas, t = t)
  t
}
cuenta_v <- function(t, pat) vapply(t, function(x) if (is.na(x)) NA_real_ else
  as.numeric(lengths(regmatches(x, gregexpr(pat, x, perl = TRUE)))), 0, USE.NAMES = FALSE)
s1_sol <- function(lin, semillas, pat = PAT_S1) sum(cuenta_v(textos_sol(lin, semillas), pat))
s1_sol_v <- function(lin, semillas, pat) cuenta_v(textos_sol(lin, semillas), pat)
""")

# A7. Sonda S1-Sol fórmula: separadores ampliados y control exacto
region("## Tanda S1b, sonda S1-Sol fórmula: ninguna fórmula cierra en «$» seguido de espacio (normal o duro) y la unidad. Control\n",
       """N, n_s1f, n_dentro))\n""",
r"""## Tanda S1b, sonda S1-Sol fórmula: ninguna fórmula cierra en «$» seguido de la unidad. M31 revierte la variante en
## «kilos a favor de» y tiene que verlo esta sonda, no S1-Sol (con el espacio duro, S1-Sol no lo ve).
## Tanda S2a (H1 de la FASE 2C r5):
##  - Separadores. Entre el cierre de una fórmula y la unidad, o entre el cierre y el signo (S1-Sol signo), las sondas
##    admiten hasta tres caracteres cualesquiera que no sean letra ni cifra: espacio normal, U+00A0, U+2009, U+202F,
##    U+2060… La clase anterior era una lista negra (espacio normal o U+00A0) y no veía el espacio fino (M39, M41).
##  - Controles positivos exactos, deterministas con las semillas 1..100. Con un umbral (>= N, > 0) no veían que su
##    cuenta bajara: la r5 sacó la unidad de la lista de totales con U+2009, y el control pasó de 1158 a 958 en OK.
##    Cada uno lleva un mutante que quita un solo sitio (M34, M35, M36).
SIG_S1 <- "[.,:;!?)\\]»”…]"
SEP_S1 <- "[^A-Za-z0-9$\\n]{0,3}"
PAT_S1F <- paste0("\\$", SEP_S1, "(kilos?|cursos?)\\b")
PAT_S1F_VIEJA <- paste0("\\$[ ", intToUtf8(0xa0), "]+(kilos?|cursos?)\\b")
PAT_DENTRO <- paste0("\\\\ \\\\text\\{(kilos?|cursos?)", SIG_S1, "?\\}\\$")
CTRL_DENTRO <- 1158; CTRL_SIGNO <- 730; CTRL_SIN_U <- 42
n_s1f <- s1_sol(l, 1:N, PAT_S1F); n_dentro <- s1_sol(l, 1:N, PAT_DENTRO)
ok(identical(n_s1f, 0) && isTRUE(n_dentro == CTRL_DENTRO),
   sprintf("S1-Sol fórmula: en las %d versiones de E1, ninguna fórmula cierra antes de su unidad, con ningún separador (%s); unidad dentro de la fórmula: %s (control exacto: %s)",
           N, n_s1f, n_dentro, CTRL_DENTRO))
""")

# A8. Sonda S1-Sol signo, generalizada, con sus dos controles exactos
region("## Tanda S1b relanzada (2026-11-03, V1'), sonda S1-Sol signo: ninguna fórmula que cierra en su unidad deja fuera el\n",
       """N, n_s1s, sum(v_dentro_s > 0, na.rm = TRUE), N, sum(v_dentro_s)))\n""",
r"""## Sonda S1-Sol signo (tanda S1b relanzada, V1'; generalizada en la tanda S2a por la decisión del profesor «Cerrarlo
## ahora»): ninguna fórmula, con unidad o sin ella, deja fuera el signo que la sigue. Con el signo fuera, Chromium parte
## el renglón entre la fórmula y el signo (6b de V1: el punto solo en 34 semillas a 360 px y en 5 a 320 px). Hasta la
## S2a la sonda pedía la unidad («\\text{kilos}$.») y no veía el «= 5$.» de ERR-RESTA (42 casos en 100 versiones).
## Controles exactos: la unidad con el signo dentro («\\text{kilos.}$», 730) y el signo dentro de una fórmula sin
## unidad («\\text{.}$»), uno por versión con ERR-RESTA como distractor de G (42 en E1). M32, M38 y M41 prueban la sonda.
PAT_S1S <- paste0("\\$", SEP_S1, SIG_S1)
PAT_S1S_VIEJA <- paste0("\\\\text\\{(kilos?|cursos?)\\}\\$", SIG_S1)
PAT_SIGNO_DENTRO <- paste0("\\\\text\\{(kilos?|cursos?)", SIG_S1, "\\}\\$")
PAT_SIN_U <- paste0("\\\\text\\{", SIG_S1, "\\}\\$")
n_s1s <- s1_sol(l, 1:N, PAT_S1S)
v_dentro_s <- s1_sol_v(l, 1:N, PAT_SIGNO_DENTRO)
ok(identical(n_s1s, 0) && !anyNA(v_dentro_s) && all(v_dentro_s > 0) && isTRUE(sum(v_dentro_s) == CTRL_SIGNO),
   sprintf("S1-Sol signo: en las %d versiones de E1, ninguna fórmula, con unidad o sin ella, deja fuera el signo que la sigue (%s); unidad con el signo dentro en %d de %d versiones (%s en total; control exacto: %s)",
           N, n_s1s, sum(v_dentro_s > 0, na.rm = TRUE), N, sum(v_dentro_s), CTRL_SIGNO))
es_resta <- vapply(NAT, function(x) identical(x$cod_gdis, "ERR-RESTA"), TRUE)
v_sin_u <- s1_sol_v(l, 1:N, PAT_SIN_U)
ok(!anyNA(v_sin_u) && isTRUE(sum(v_sin_u) == CTRL_SIN_U) && sum(es_resta) == CTRL_SIN_U && identical(v_sin_u > 0, es_resta),
   sprintf("S1-Sol signo, fórmula sin unidad: el signo dentro («\\text{.}$») sale %s veces, en las %d versiones de E1 con ERR-RESTA como distractor de G y solo en ellas (control exacto: %s)",
           sum(v_sin_u), sum(es_resta), CTRL_SIN_U))
""")

# A9. Mutantes nuevos de las sondas y de los controles, tras M32
reemplaza("""           n_p32, n_m32, n_m32_otras))\n""",
r"""           n_p32, n_m32, n_m32_otras))
## Tanda S2a: mutantes de las sondas y de los controles exactos de S1-Sol, sobre el texto del .Rmd en memoria
## (mut_lin() exige que el patrón aparezca una sola vez). Cada uno declara su sonda: la que tiene que cazarlo, con las
## otras dos sondas en 0. Los de control quitan un solo sitio, y solo los caza su control exacto. Los caracteres
## especiales se insertan con intToUtf8(), nunca literales.
mut_lin <- function(de, a) { tx <- paste(l, collapse = "\n"); n <- lengths(regmatches(tx, gregexpr(de, tx, fixed = TRUE)))
  list(n = n, lin = if (n == 1L) strsplit(sub(de, a, tx, fixed = TRUE), "\n", fixed = TRUE)[[1]] else NULL) }
SONDAS_S1 <- c("S1" = PAT_S1, "S1-Sol fórmula" = PAT_S1F, "S1-Sol signo" = PAT_S1S)
CONTROLES_S1 <- list("unidad dentro" = list(pat = PAT_DENTRO, n = CTRL_DENTRO),
                     "unidad con el signo dentro" = list(pat = PAT_SIGNO_DENTRO, n = CTRL_SIGNO),
                     "signo dentro sin unidad" = list(pat = PAT_SIN_U, n = CTRL_SIN_U))
juzgar_s1 <- function(id, desc, de, a, sonda, vieja = NULL) {
  m <- mut_lin(de, a)
  if (is.null(m$lin)) return(invisible(falla(sprintf("%s MAL CONSTRUIDO: el patrón «%s» aparece %d veces (se exige 1)", id, substr(de, 1, 40), m$n))))
  t <- textos_sol(m$lin, 1:N)
  if (anyNA(t)) return(invisible(falla(sprintf("MUTANTE %s CAZADO POR LA SONDA EQUIVOCADA: el data_generation aborta en %d versiones", id, sum(is.na(t))))))
  s <- vapply(SONDAS_S1, function(p) sum(cuenta_v(t, p)), 0)
  cc <- vapply(CONTROLES_S1, function(z) sum(cuenta_v(t, z$pat)), 0)
  cmal <- cc != vapply(CONTROLES_S1, function(z) z$n, 0)
  ver <- if (identical(m$lin, l)) "mal_construido" else if (sonda %in% names(SONDAS_S1)) {
    if (s[[sonda]] == 0) "no_detectado" else if (any(s[names(s) != sonda] > 0)) "cazado_por_otra" else "cazado_por_su_sonda"
  } else {
    if (any(s > 0) || any(cmal[names(cmal) != sonda])) "cazado_por_otra" else if (!cmal[[sonda]]) "no_detectado" else "cazado_por_su_sonda"
  }
  v_txt <- if (is.null(vieja)) "" else sprintf("; la clase anterior: %s", sum(cuenta_v(t, vieja)))
  ok(ver == "cazado_por_su_sonda",
     sprintf("%s (%s): S1 %s, S1-Sol fórmula %s, S1-Sol signo %s%s; controles: %s — sonda esperada «%s»: %s",
             id, desc, s[[1]], s[[2]], s[[3]], v_txt,
             paste(sprintf("%s %s%s", names(cc), cc, ifelse(cmal, " (falla)", "")), collapse = ", "), sonda, ver))
}
juzgar_s1("M34", "la explicación de ERR-PARCIAL, rama CURSOS, saca la unidad de su fórmula «… que compensan»; quita un solo sitio",
          'p$parcialP, "\\\\ \\\\text{kilos}$ que compensan', 'p$parcialP, "$ que compensan', "unidad dentro")
juzgar_s1("M35", "la explicación de ERR-PROM con el molde P_ven pierde la coma de su fórmula; quita un solo sitio",
          '"\\\\ \\\\text{kilos,}$ es menor que el de "', '"\\\\ \\\\text{kilos}$ es menor que el de "', "unidad con el signo dentro")
juzgar_s1("M36", "la explicación de ERR-RESTA pierde el punto de su fórmula sin unidad; quita el único sitio",
          'p$delta, "\\\\text{.}$ Esa resta', 'p$delta, "$ Esa resta', "signo dentro sin unidad")
juzgar_s1("M38", "la lista de totales pierde la unidad y deja el punto fuera de la fórmula, «= 200$.»",
          PAT_M32, '"$.\\n", sep = "")', "S1-Sol signo", vieja = PAT_S1S_VIEJA)
for (cp in list(c("a", "2009", "espacio fino"), c("b", "202f", "espacio duro estrecho"), c("c", "2060", "unión de palabras"))) {
  juzgar_s1(paste0("M39", cp[1]), sprintf("la lista de totales saca la unidad de la fórmula con U+%s, %s (mutante de la r5)", toupper(cp[2]), cp[3]),
            PAT_M32, sprintf('"$", intToUtf8(0x%s), "kilos.\\n", sep = "")', cp[2]), "S1-Sol fórmula", vieja = PAT_S1F_VIEJA)
  juzgar_s1(paste0("M41", cp[1]), sprintf("la lista de totales deja el punto fuera de la fórmula tras U+%s, %s", toupper(cp[2]), cp[3]),
            PAT_M32, sprintf('"\\\\ \\\\text{kilos}$", intToUtf8(0x%s), ".\\n", sep = "")', cp[2]), "S1-Sol signo", vieja = PAT_S1S_VIEJA)
}
""")

# A10. Ni el .Rmd ni el verificador llevan los espacios especiales literales
reemplaza("""ok(!identical(l_m29, l) && !lit_ok(l_m29), "M29 (ED con el carácter literal): la guarda L-6 lo detecta")\n""",
r"""ok(!identical(l_m29, l) && !lit_ok(l_m29), "M29 (ED con el carácter literal): la guarda L-6 lo detecta")
## Tanda S2a (H1 y H4 de la FASE 2C r5): ni el .Rmd ni este verificador llevan U+00A0, U+2009, U+202F ni U+2060
## literales; los mutantes de S1-Sol los construyen con intToUtf8().
ESP_LIT <- paste0("[", intToUtf8(c(0xa0, 0x2009, 0x202f, 0x2060)), "]")
yo_ver <- if (length(arg_file)) normalizePath(sub("^--file=", "", arg_file[1])) else NA_character_
ok(!any(grepl(ESP_LIT, l, perl = TRUE)) && !is.na(yo_ver) && !any(grepl(ESP_LIT, readLines(yo_ver, encoding = "UTF-8"), perl = TRUE)),
   "L-6 (S2a): ni el .Rmd ni el verificador llevan U+00A0, U+2009, U+202F ni U+2060 literales")
""")

for ch in (0xA0, 0x2009, 0x202F, 0x2060):
    assert chr(ch) not in t, f"carácter U+{ch:04X} literal en el verificador"
open(ver, "w", encoding="utf-8").write(t)
print("md5 antes:", antes, "· después:", hashlib.md5(t.encode("utf-8")).hexdigest())
