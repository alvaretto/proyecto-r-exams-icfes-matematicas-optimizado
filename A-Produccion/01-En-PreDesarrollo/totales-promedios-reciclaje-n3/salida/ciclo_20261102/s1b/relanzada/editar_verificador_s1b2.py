r"""Tanda S1b relanzada (2026-11-03): ediciones de verificar_espacio.R (md5 833a1e43… -> nuevo).
- Cabecera: línea de la tanda.
- S7: rasgos() calcula el lado de la canónica de sus cifras (la canónica ya no asigna lado_gdis), con su ok().
- M30 adaptado a V1' («\\text{kilos.}$»); M33 nuevo (guarda «L-6: el signo que sigue…» del data_generation).
- Sonda S1-Sol signo (texto emitido: opciones, Solution y feedback) y su mutante M32 (en la Solution).
Uso: python3 -I editar_verificador_s1b2.py <verificar_espacio.R>"""
import sys
p = sys.argv[1]
s = open(p, encoding='utf-8').read()

def una(s, viejo, nuevo):
    assert s.count(viejo) == 1, (s.count(viejo), viejo[:90])
    return s.replace(viejo, nuevo)

# 1. Cabecera
s = una(s, "##        S1-Sol fórmula, guarda «L-6: la unidad que cierra una fórmula…» (M30) y M31, que revierte la variante.\n",
        "##        S1-Sol fórmula, guarda «L-6: la unidad que cierra una fórmula…» (M30) y M31, que revierte la variante.\n"
        "## Tanda S1b relanzada (2026-11-03): V1', el signo que sigue a la unidad entra en la fórmula («\\\\text{kilos.}$»):\n"
        "##        sonda S1-Sol signo (M32), guarda «L-6: el signo que sigue a la unidad…» (M33) y M30 adaptado. S7 cerrado: la\n"
        "##        canónica ya no asigna lado_gdis y rasgos() calcula su lado de las cifras.\n")

# 2. S7 en rasgos()
s = una(s, "rasgos <- function(env) with(env, list(\n",
        "## S7 (tanda S1b relanzada, decisión del profesor del 2026-11-03): la canónica ya no asigna lado_gdis, que no usaba; su\n"
        "## lado se calcula de sus cifras (x del distractor de G frente a D). En las demás versiones se lee lado_gdis, como antes.\n"
        "rasgos <- function(env) with(env, list(\n")
s = una(s, "tabla = c(d$nG, d$nP, d$aG, d$aP), gi = gi, lado = lado_gdis))\n",
        "tabla = c(d$nG, d$nP, d$aG, d$aP), gi = gi,\n"
        "  lado = if (isTRUE(es_canonica)) (if (afs$gdis$x > d$D) \"encima\" else \"debajo\") else lado_gdis))\n")
s = una(s, 'ok(!is.null(CANON), "se encontró la instancia canónica")\n',
        'ok(!is.null(CANON), "se encontró la instancia canónica")\n'
        '## S7: en la canónica, el lado sale de sus cifras y tiene que seguir dando «encima» (5 < 20 (D) < 40).\n'
        'ok(identical(CANON$lado, "encima") && isTRUE(CANON$X > CANON$D),\n'
        '   sprintf("S7: en la canónica, el lado del distractor de G sale de sus cifras (x = %s, D = %s): «%s»", CANON$X, CANON$D, CANON$lado))\n')

# 3. M30 adaptado y M33
M30_OLD = ("       cambios = list(c('d$D, \"\\\\\\\\ \\\\\\\\text{kilos}$.\"),', 'd$D, \"$\", ED, \"kilos.\"),')),\n")
s = una(s, M30_OLD, "       cambios = list(c('d$D, \"\\\\\\\\ \\\\\\\\text{kilos.}$\"),', 'd$D, \"$\", ED, \"kilos.\"),')),\n")
LLEGO30 = '       llego = function(e) grepl("\\\\$\\u00a0kilos\\\\.$", e$fb[["clave"]])))\n'
s = una(s, LLEGO30,
        '       llego = function(e) grepl("\\\\$\\u00a0kilos\\\\.$", e$fb[["clave"]])),\n'
        '  ## Tanda S1b relanzada: M33 saca el punto de la fórmula en el feedback de la clave («\\\\text{kilos}$.», como en V1).\n'
        '  list(id = "M33", sonda = "L-6: el signo que sigue a la unidad de una fórmula va dentro de la fórmula",\n'
        '       desc = "el feedback de la clave deja el punto fuera de la fórmula (revierte V1\')",\n'
        "       cambios = list(c('d$D, \"\\\\\\\\ \\\\\\\\text{kilos.}$\"),', 'd$D, \"\\\\\\\\ \\\\\\\\text{kilos}$.\"),')),\n"
        '       llego = function(e) grepl("\\\\\\\\text\\\\{kilos\\\\}\\\\$\\\\.$", e$fb[["clave"]])))\n')

# 4. Sonda S1-Sol signo y M32, tras el ok() de M31
ANCLA = 'ok(!is.na(n_m31) && n_m31 > 0 && identical(n_m31_s1, 0), sprintf("M31 ('
i = s.index(ANCLA); j = s.index('\n', i) + 1
assert s.count(ANCLA) == 1
BLOQUE = r'''## Tanda S1b relanzada (2026-11-03, V1'), sonda S1-Sol signo: ninguna fórmula que cierra en su unidad deja fuera el
## signo que la sigue («\\text{kilos}$.»): con el signo fuera, Chromium parte el renglón entre la fórmula y el signo (6b
## de V1: el punto solo en 34 semillas a 360 px y en 5 a 320 px). Control positivo: la unidad con el signo dentro
## («\\text{kilos.}$») aparece en todas las versiones. M32 saca el punto de la fórmula en la lista de totales del
## procedimiento correcto, y tiene que verlo esta sonda, no S1-Sol ni S1-Sol fórmula.
SIG_S1 <- "[.,:;!?)\\]»”…]"
PAT_S1S <- paste0("\\\\text\\{(kilos?|cursos?)\\}\\$", SIG_S1)
s1_sol_v <- function(lin, semillas, pat) {
  ch <- lapply(c("opciones", "solucion", "feedback"), function(nm) chunk_de(lin, nm))
  vapply(semillas, function(s) { r <- correr(s); if (!is.null(r$err)) return(NA_real_)
    t <- paste(unlist(lapply(ch, function(e) capture.output(eval(e, envir = r$env)))), collapse = "\n")
    lengths(regmatches(t, gregexpr(pat, t, perl = TRUE))) }, 0)
}
n_s1s <- s1_sol(l, 1:N, PAT_S1S)
v_dentro_s <- s1_sol_v(l, 1:N, paste0("\\\\text\\{(kilos?|cursos?)", SIG_S1, "\\}\\$"))
ok(identical(n_s1s, 0) && !anyNA(v_dentro_s) && all(v_dentro_s > 0),
   sprintf("S1-Sol signo: en las %d versiones de E1, ninguna fórmula que cierra en su unidad deja fuera el signo que la sigue (%s); unidad con el signo dentro en %d de %d versiones (%s en total)",
           N, n_s1s, sum(v_dentro_s > 0, na.rm = TRUE), N, sum(v_dentro_s)))
PAT_M32 <- '"\\\\ \\\\text{kilos.}$\\n", sep = "")'
n_p32 <- sum(lengths(regmatches(l, gregexpr(PAT_M32, l, fixed = TRUE))))
l_m32 <- sub(PAT_M32, '"\\\\ \\\\text{kilos}$.\\n", sep = "")', l, fixed = TRUE)
n_m32 <- if (n_p32 != 1L || identical(l_m32, l)) NA else s1_sol(l_m32, 1:N, PAT_S1S)
n_m32_otras <- if (is.na(n_m32)) NA else s1_sol(l_m32, 1:N) + s1_sol(l_m32, 1:N, PAT_S1F)
ok(n_p32 == 1L && !is.na(n_m32) && n_m32 > 0 && identical(n_m32_otras, 0),
   sprintf("M32 (la lista de totales del procedimiento correcto saca el punto de la fórmula; el patrón aparece %d vez): la sonda S1-Sol signo lo detecta en %s casos, y S1-Sol y S1-Sol fórmula en %s (cazado por su sonda)",
           n_p32, n_m32, n_m32_otras))
'''
s = s[:j] + BLOQUE + s[j:]
assert ' ' not in s and '⁠' not in s, 'carácter literal'
open(p, 'w', encoding='utf-8').write(s)
print('verificador editado:', p)
