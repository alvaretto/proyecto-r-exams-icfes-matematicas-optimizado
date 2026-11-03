#!/usr/bin/env python3
"""Ciclo 2026-11-02, tanda S1: ediciones del .Rmd del SCHOICE (S1, S5, S6, S7).
Cada reemplazo exacto declara cuántas veces debe aparecer; si no, aborta sin escribir."""
import re, sys
ruta = sys.argv[1]
src = open(ruta, encoding="utf-8").read()
NB = "\u00a0"
orig = src

def rep(old, new, n=1):
    global src
    c = src.count(old)
    if c != n:
        sys.exit(f"ABORTA: «{old[:70]}» aparece {c} veces (se esperaban {n})")
    src = src.replace(old, new)

# --- S1 + S6: definición de ED, después de library(exams) ---------------------------------------
rep("library(exams)\n\n## --- Utilidades",
    "library(exams)\n\n"
    "## --- Espacio duro (S1, ciclo 2026-11-02) ------------------------------------------\n"
    "## U+00A0 entre cifra y unidad («15 kilos», «4 cursos») en las opciones y en la Solution, y entre\n"
    "## las dos palabras de «El Bosque» y «La Pradera» también fuera de la tabla, para que un renglón no\n"
    "## parta la cifra de su unidad ni el nombre de la sede (BAJA 1 y 2 de la 6b del CLOZE hermano). Va\n"
    "## siempre como esta variable, nunca como carácter literal; pandoc lo pasa a «~» en LaTeX. Las\n"
    "## celdas de cifras de la tabla conservan el espacio normal.\n"
    "## Umbral medido en el Moodle del colegio (2026-10-30, salida/r4b/medicion_qtext_moodle.md, N = 100):\n"
    "## la caja de la pregunta (.qtext) mide 270 px a 320 px y 310 px a 360 px. El espacio duro de las\n"
    "## celdas de «El Bosque» y «La Pradera» sube el ancho mínimo de la tabla hasta +21,4 px (esas tablas\n"
    "## llegan a 209,9 px; la más ancha, la canónica, a 225,6 px, con Roboto), y eso solo pesaba con cajas\n"
    "## de menos de 240 px: en el colegio, 0/100 tablas fuera de su caja a 320 y a 360 px.\n"
    "ED <- \"\\u00a0\"\n\n"
    "## --- Utilidades")

rep('participaron_txt <- function(x) if (x == 1) "participó 1 curso" else paste("participaron", x, "cursos")',
    'participaron_txt <- function(x) if (x == 1) paste0("participó 1", ED, "curso") else paste0("participaron ", x, ED, "cursos")')

# --- S5: comentario de la copia local de tabla_responsiva() -------------------------------------
rep("## --- Familia F2: tabla responsiva (copia de .claude/scripts/snippets_familias_rmd.R,\n"
    "## sin su rama is_latex_output(), que bajo R/exams nunca se ejecuta). Aborta si el ancho\n",
    "## --- Familia F2: tabla responsiva (copia local de la de .claude/scripts/snippets_familias_rmd.R,\n"
    "## sin su rama is_latex_output(), que bajo R/exams nunca se ejecuta). Aborta si el ancho\n")
rep("## Moodle. Variante local: la función de snippets_familias_rmd.R no lo tiene.\n",
    "## Moodle. Desde la regla #21 v1.4 (2026-10-29) la función canónica también tiene `separacion` y\n"
    "## `tam_cabecera`, con el mismo <span>, pero desactivados por defecto (separacion = NULL): en otro\n"
    "## ejercicio la misma variante saca la tabla de su caja a 360 px. Esta copia los deja activados por\n"
    "## defecto, con los valores medidos aquí.\n")

# --- S1: opciones de la canónica (H-2: mismas palabras; cambia solo el espacio) -----------------
rep('A = "La sede primaria ganó el concurso porque recogió 40 kilos más que la sede de secundaria.",',
    'A = paste0("La sede primaria ganó el concurso porque recogió 40", ED, "kilos más que la sede de secundaria."),')
rep('            "el total fue de 20 kilos más."),',
    '            paste0("el total fue de 20", ED, "kilos más.")),')
rep('C = "La sede secundaria ganó porque, aunque participaron menos cursos, tuvo mayor promedio con 5 kilos de más.",',
    'C = paste0("La sede secundaria ganó porque, aunque participaron menos cursos, tuvo mayor promedio con 5", ED,\n'
    '             "kilos de más."),')

# --- S7: el campo tipo de cada plantilla (la estructura queda como comentario, regla #11) --------
for t in ["institucional", "periodistica", "dialogo", "perspectiva_estudiante", "situacion_problema",
          "pregunta_retorica", "accion_en_curso", "descubrimiento"]:
    rep(f'  list(tipo = "{t}", plantilla = function(t, m, p) {{\n',
        f'  list(plantilla = function(t, m, p) {{   # estructura: {t}\n')

# --- S1: «El Bosque» y «La Pradera» (las celdas ya lo tenían, como carácter literal) ----------------
rep("  ## Ronda 4 (B2 del 6b, ronda 3): espacio duro en las celdas, para que a 360 px «El Bosque» no se\n"
    "  ## parta en dos renglones. Pandoc lo pasa a «~» en LaTeX.\n"
    '  list(tipo = "sede", encab = "Sede", nombres = c("la sede El Bosque", "la sede La Pradera"),\n'
    f'       filas = c("El{NB}Bosque", "La{NB}Pradera")),\n',
    "  ## Ronda 4 (B2 del 6b, ronda 3): espacio duro en las celdas, para que a 360 px «El Bosque» no se\n"
    "  ## parta en dos renglones. S1 (ciclo 2026-11-02): también en los nombres, que salen en el enunciado,\n"
    "  ## las opciones y la Solution; y con ED, no con el carácter literal.\n"
    '  list(tipo = "sede", encab = "Sede", nombres = c(paste0("la sede El", ED, "Bosque"), paste0("la sede La", ED, "Pradera")),\n'
    '       filas = c(paste0("El", ED, "Bosque"), paste0("La", ED, "Pradera"))),\n')

# --- S1: cierre, lecturas_de() y el molde de la opción C -----------------------------------------
rep('cierre <- function(x, O) paste0("su total fue más alto que el de ", O, " en ", x, " kilos.")',
    'cierre <- function(x, O) paste0("su total fue más alto que el de ", O, " en ", x, ED, "kilos.")')
rep('lecturas_de <- function(texto) c("diferencia", if (grepl("(fue de|recogió) [0-9]+ kilos", texto)) "total")',
    '## S1: entre la cifra y «kilos» puede ir el espacio duro; la lectura se detecta con los dos espacios.\n'
    'lecturas_de <- function(texto) c("diferencia", if (grepl(paste0("(fue de|recogió) [0-9]+[ ", ED, "]kilos"), texto)) "total")')

# --- S7: parámetro m de MOLDES (no lo usaba ningún molde) ---------------------------------------
rep("f = function(S, O, x, m)", "f = function(S, O, x)", 11)
rep("texto = mo$f(S, O, x, mat))", "texto = mo$f(S, O, x))")

# --- S7: rama dif_cursos de verdad() (ningún molde la usa); un afirma desconocido aborta ---------
rep("    dif_cursos   = n[[S]] - n[[O]] == af$x,\n    sin_sustento = NA))\n",
    "    sin_sustento = NA,\n    stop(\"verdad(): afirmación de tipo desconocido: \", af$afirma)))\n")

# --- S7: b_debajo y contexto_tipo en la rama canónica; contexto_tipo en la otra ------------------
rep('  b_debajo <- 1L; lado_gdis <- "encima"; contexto_tipo <- "canonica"    # cifras 5 < 20 (D) < 40\n',
    '  ## lado_gdis no se usa en la canónica (cifras 5 < 20 (D) < 40), pero lo leen rasgos() de\n'
    '  ## verificar_espacio.R y rec() de cloze/verificar_espacio_cloze.R: se quita en la tanda del\n'
    '  ## re-empalme, junto con esa lectura (S7, ciclo 2026-11-02).\n'
    '  lado_gdis <- "encima"\n')
rep("  ctx <- pick1(contextos); contexto_tipo <- ctx$tipo\n", "  ctx <- pick1(contextos)\n")

# --- S1: guarda del complemento, con el espacio duro --------------------------------------------
# (la transformación T2 de abajo la convierte; aquí solo se comprueba que exista)
if src.count('grepl(paste0(" en ", a$x, " kilos\\\\.$"), a$texto)') != 1:
    sys.exit("ABORTA: guarda del complemento no encontrada")

# --- S1: guarda de la canónica verbatim, sin el espacio duro -------------------------------------
rep('            "L-1: canónica verbatim (opciones)" =\n'
    '              all(vapply(textos, nchar, 0L) == c(126L, 88L, 105L, 156L)) &&\n'
    '              all(vapply(textos, pond, 0) == c(750351, 373876, 532900, 1156926)))\n',
    '            "L-1: canónica verbatim (opciones)" =\n'
    '              all(vapply(textos, nchar, 0L) == c(126L, 88L, 105L, 156L)) &&\n'
    '              all(vapply(gsub(ED, " ", textos, fixed = TRUE), pond, 0) == c(750351, 373876, 532900, 1156926)),\n'
    '            ## S1 (H-2): la canónica conserva las palabras del impreso y solo cambia el espacio entre la\n'
    '            ## cifra y «kilos», tres veces; la guarda de arriba lo compara sin él.\n'
    '            "S1: en la canónica, el espacio duro solo entre cifra y unidad" =\n'
    '              sum(lengths(regmatches(textos, gregexpr(ED, textos, fixed = TRUE)))) == 3L &&\n'
    '              !any(grepl(paste0("[^0-9]", ED), textos)))\n')

# --- S1: guarda general (opciones y enunciado) -------------------------------------------------
rep('stopifnot("canal 8: cifras distintas entre opciones" = !anyDuplicated(cifras))\n',
    'stopifnot("canal 8: cifras distintas entre opciones" = !anyDuplicated(cifras))\n'
    '## S1: ninguna cifra separada de su unidad por un espacio normal, ni «El Bosque» o «La Pradera» (la\n'
    '## tabla, que conserva el espacio normal en las cifras, no entra).\n'
    'stopifnot("S1: espacio duro entre cifra y unidad y en los nombres de dos palabras" =\n'
    '            !any(grepl("[0-9] (kilos?|cursos?)\\\\b|El Bosque|La Pradera", c(textos, enunciado), perl = TRUE)))\n')

# --- S1: transformaciones T1-T3 (cifra o fórmula seguida de la unidad) ---------------------------
n1 = len(re.findall(r'"\$ kilos', src)); src = re.sub(r'"\$ kilos', '"$", ED, "kilos', src)
n2 = len(re.findall(r',(\s*)" kilos', src)); src = re.sub(r',(\s*)" kilos', r',\1ED, "kilos', src)
n3 = len(re.findall(r',(\s*)" cursos', src)); src = re.sub(r',(\s*)" cursos', r',\1ED, "cursos', src)
print(f"T1 «\"$ kilos»: {n1} · T2 «, \" kilos»: {n2} · T3 «, \" cursos»: {n3}")
if NB in src:
    sys.exit("ABORTA: queda un U+00A0 literal en el .Rmd")
open(ruta, "w", encoding="utf-8").write(src)
print("escrito", ruta)
