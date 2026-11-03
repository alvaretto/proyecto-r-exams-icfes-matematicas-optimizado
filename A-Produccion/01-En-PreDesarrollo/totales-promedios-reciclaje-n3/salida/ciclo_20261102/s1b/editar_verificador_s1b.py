"""Tanda S1b: adapta L-6 al caso «fórmula y unidad» en verificar_espacio.R (sonda S1-Sol fórmula, M27 sobre el sitio
nuevo, M30 y M31). Sin U+00A0 literal: se escribe siempre el escape \\u00a0."""
import sys
p = sys.argv[1]; s = open(p, encoding='utf-8').read()
def rep(old, new):
    global s
    assert s.count(old) == 1, (s.count(old), old[:70]); s = s.replace(old, new, 1)
# 1) cabecera
rep('## Ciclo 2026-11-02, tanda S1: espacio duro (S1)',
    '## Ciclo 2026-11-02, tanda S1b: la unidad que cierra una fórmula va DENTRO de ella («= T\\\\ \\\\text{kilos}$»): sonda\n'
    '##        S1-Sol fórmula, guarda «L-6: la unidad que cierra una fórmula…» (M30) y M31, que revierte la variante.\n'
    '## Ciclo 2026-11-02, tanda S1: espacio duro (S1)')
# 2) M30 en la lista de mutantes de data_generation, tras M26
old_m26_end = '''       llego = function(e) !isTRUE(e$es_canonica) && grepl(" en [0-9]+ kilos\\\\.$", e$afs$clave$texto)))'''
rep(old_m26_end, old_m26_end[:-1] + ''',
  ## Tanda S1b: M30 devuelve la unidad del feedback de la clave fuera de la fórmula, como en S1 («$», ED, «kilos»).
  list(id = "M30", sonda = "L-6: la unidad que cierra una fórmula va dentro de la fórmula",
       desc = "el feedback de la clave cierra la fórmula antes de «kilos» (revierte la variante V1)",
       cambios = list(c('d$D, "\\\\\\\\ \\\\\\\\text{kilos}$."),', 'd$D, "$", ED, "kilos."),')),
       llego = function(e) grepl("\\\\$\\u00a0kilos\\\\.$", e$fb[["clave"]])))''')
# 3) S1-Sol: patrón como argumento, sonda de fórmula, M27 en el sitio nuevo y M31
rep('s1_sol <- function(lin, semillas) {', 's1_sol <- function(lin, semillas, pat = PAT_S1) {')
rep('    lengths(regmatches(t, gregexpr(PAT_S1, t, perl = TRUE))) }, 0))',
    '    lengths(regmatches(t, gregexpr(pat, t, perl = TRUE))) }, 0))')
rep('''l_m27 <- sub('d$D,\\n    "$", ED, "kilos a favor de "', 'd$D,\\n    "$ kilos a favor de "', paste(l, collapse = "\\n"), fixed = TRUE)''',
    '''l_m27 <- sub('d$D,\\n    "\\\\\\\\ \\\\\\\\text{kilos}$ a favor de "', 'd$D,\\n    "$ kilos a favor de "', paste(l, collapse = "\\n"), fixed = TRUE)''')
anchor = '''ok(!is.na(n_m27) && n_m27 > 0, sprintf("M27 (la Solution pierde el espacio duro de «kilos a favor de»): la sonda S1-Sol lo detecta en %s casos", n_m27))'''
rep(anchor, anchor + '''
## Tanda S1b, sonda S1-Sol fórmula: ninguna fórmula cierra en «$» seguido de espacio (normal o duro) y la unidad. Control
## positivo: la unidad dentro de la fórmula («\\\\ \\\\text{kilos}$») aparece en todas las versiones. M31 revierte la
## variante en «kilos a favor de» y tiene que verlo esta sonda, no S1-Sol (con el espacio duro, S1-Sol no lo ve).
PAT_S1F <- "\\\\$[ \\u00a0]+(kilos?|cursos?)\\\\b"
n_s1f <- s1_sol(l, 1:N, PAT_S1F); n_dentro <- s1_sol(l, 1:N, "\\\\\\\\ \\\\\\\\text\\\\{(kilos?|cursos?)\\\\}\\\\$")
ok(identical(n_s1f, 0) && n_dentro >= N, sprintf("S1-Sol fórmula: en las %d versiones de E1, ninguna fórmula cierra antes de su unidad (%s); unidad dentro de la fórmula: %s", N, n_s1f, n_dentro))
l_m31 <- sub('d$D,\\n    "\\\\\\\\ \\\\\\\\text{kilos}$ a favor de "', 'd$D,\\n    "$", ED, "kilos a favor de "', paste(l, collapse = "\\n"), fixed = TRUE)
l_m31 <- strsplit(l_m31, "\\n", fixed = TRUE)[[1]]
n_m31 <- if (identical(l_m31, l)) NA else s1_sol(l_m31, 1:N, PAT_S1F)
n_m31_s1 <- if (identical(l_m31, l)) NA else s1_sol(l_m31, 1:N)
ok(!is.na(n_m31) && n_m31 > 0 && identical(n_m31_s1, 0), sprintf("M31 (la Solution vuelve a cerrar la fórmula antes de «kilos a favor de», con el espacio duro): la sonda S1-Sol fórmula lo detecta en %s casos y S1-Sol en %s (cazado por su sonda)", n_m31, n_m31_s1))''')
assert ' ' not in s and '⁠' not in s
open(p, 'w', encoding='utf-8').write(s); print('ok')
