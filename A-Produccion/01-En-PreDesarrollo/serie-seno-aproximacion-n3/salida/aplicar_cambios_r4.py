## Cambios de la FASE 2C r4 (APROBAR_CON_CAMBIOS) y de la 6b/coherencias r4 (signo menos «$-$» literal). Conserva el YAML del Knit.
import sys
def aplicar(p, R, yaml=False):
    s = open(p, encoding="utf-8").read(); y0 = s[:s.index("```{r data_generation")] if yaml else ""
    for a, b in R:
        c = s.count(a)
        if c != 1: sys.exit("NO COINCIDE (%d) en %s: %s" % (c, p, a[:90]))
        s = s.replace(a, b)
    if yaml: assert s.startswith(y0)
    open(p, "w", encoding="utf-8").write(s); print(p, "->", len(R), "cambios")
aplicar("serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd", [
 ('dec_txt     <- function(v) paste0(if (v < 0) "$-$" else "", dec(abs(v)))       # decimal en texto: en MathML la coma {,} sale separada\ndec_sig_txt <- function(v) paste0(if (v < 0) "$-$" else "", dec_sig(abs(v)))   # el signo menos va en modo matemático (FASE 2C r1 H8)',
  '## Decimal con coma, sin espacio tras la coma en MathML: en texto si es positivo; si es negativo, $-\\text{...}$ (un «$-$» pegado a un\n## dígito no lo reconoce pandoc como matemática: 6b y coherencias r4). Probado en HTML, PDF, NOPS y DOCX.\ndec_txt     <- function(v) if (v < 0) paste0("$-\\\\text{", dec(abs(v)), "}$") else dec(v)\ndec_sig_txt <- function(v) if (v < 0) paste0("$-\\\\text{", dec_sig(abs(v)), "}$") else dec_sig(v)'),
 ('''    return(paste0("Correcta. Usa los primeros ", PALABRA[n], " términos, con los signos alternados y los factoriales impares: $",
                  ck$l1, " = ", frs(num_clave, D), "$."))''',
  '''    return(paste0("Correcta. Usa los primeros ", PALABRA[n], " términos, con los signos alternados y los factoriales impares; ",
                  "el resultado es $", frs(num_clave, D), "$."))'''),
 ('delante de todos los términos, aunque en la serie los signos se alternan.', 'delante de todos los términos que siguen a $x$, aunque en la serie los signos se alternan.'),
 ('paste0("alterna los signos empezando al revés: suma el segundo término y resta el tercero",\n           if (n >= 4) ", y suma el cuarto" else ""),',
  'paste0("alterna los signos al revés (suma el segundo término y resta el tercero",\n           if (n >= 4) ", y suma el cuarto" else "", ")"),'),
 ('es el tope más estricto que deja al menos 3 errores elegibles además de', 'es el tope entero más estricto (mínimo exacto 3,525) con al menos 3 errores elegibles además de'),
 ('Una igualdad por ecuación en display (360 y 320 px). Decimales en texto, con coma.', 'Una igualdad por ecuación en display. Decimales con coma.'),
 ('##     salida/p9_bateria_p7.txt (N = 100) y la tabla exacta del H1 de salida/fase2c_r3_detractor.md. Varios canales son estructurales de\n##     D1: la trampa queda siempre junto a la clave. Sin alcance firmado cableado todavía (FASE 2C r2 H2).',
  '##     salida/p9_bateria_p7.txt (enumeración exacta, 21 reglas; control N = 100). Varios canales son estructurales de D1: la trampa\n##     queda siempre junto a la clave. Alcance cableado en salida/verificar_bateria_p7.R, PENDIENTE DE FIRMA.'),
], yaml=True)
aplicar("ITEM_OFICIAL.md", [("Es el tope\n  más estricto que deja", "Es el tope\n  entero más estricto (el mínimo exacto es 3,525) que deja")])
aplicar("salida/verificar_bateria_p7.R", [
 ('m_q <- Sys.getenv("MUTANTE_QUITAR"); m_t <- Sys.getenv("MUTANTE_TOPE")',
  '## MUTANTE_FIRMA_FICTICIA=1 rellena una firma de prueba (NO es del profesor) para probar el camino ROJO_CUBIERTO (FASE 2C r4 H6).\nif (nzchar(Sys.getenv("MUTANTE_FIRMA_FICTICIA"))) { FIRMA <- list(fecha = "MUTANTE", texto = "firma ficticia de prueba: no es del profesor"); cat("MUTANTE: firma ficticia\\n") }\nm_q <- Sys.getenv("MUTANTE_QUITAR"); m_t <- Sys.getenv("MUTANTE_TOPE")'),
])
