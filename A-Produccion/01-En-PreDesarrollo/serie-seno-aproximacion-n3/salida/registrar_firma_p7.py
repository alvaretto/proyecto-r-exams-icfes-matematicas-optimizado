## Registra en salida/verificar_bateria_p7.R la firma del profesor del 2026-11-13 (§P7-E), atada al md5 del .Rmd. Cada patrón, una vez.
import sys
p = "salida/verificar_bateria_p7.R"; s = open(p, encoding="utf-8").read()
R = [
("## Batería §P7 (regla #22) del ejercicio, con el mecanismo de ALCANCE FIRMADO de §P7-E en estado PENDIENTE DE FIRMA.",
 "## Batería §P7 (regla #22) del ejercicio, con el ALCANCE FIRMADO de §P7-E: firma del profesor del 2026-11-13, atada al md5 del .Rmd."),
("""##                            profesor registrada (fecha y texto literal).""",
 """##                            profesor registrada (fecha y texto literal) para el md5 del archivo medido.
##   FIRMA_NO_CUBRE  (exit 4) hay reglas por encima de +8 pp y una firma, pero la firma es de otro md5: no cubre este archivo.
##                            Nunca se informa como «cubierto»."""),
("""##   MUTANTE_TOPE=<regla>    baja el tope de la regla en 1 pp: el verificador debe decir «POR ENCIMA DE SU TOPE: <regla>».""",
 """##   MUTANTE_TOPE=<regla>    baja el tope de la regla en 1 pp: el verificador debe decir «POR ENCIMA DE SU TOPE: <regla>».
##   VERIFICAR_RMD=<ruta>    mide otro archivo (p. ej. una copia con un cambio): con otro md5 debe salir FIRMA_NO_CUBRE."""),
('rmd <- list.files(".", pattern = "_schoice_v1\\\\.Rmd$")[1]',
 'rmd <- Sys.getenv("VERIFICAR_RMD", list.files(".", pattern = "_schoice_v1\\\\.Rmd$")[1])'),
("""## --- Firma del profesor: VACÍA. Solo se llena con su texto literal y la fecha en que firme (§P7-E, regla por regla). ---
FIRMA <- list(fecha = "", texto = "")""",
 """## --- Firma del profesor (§P7-E, regla por regla), textos literales transmitidos por el coordinador. Atada al md5 del .Rmd firmado
## (incluye el YAML del Knit de RStudio, que el profesor decidió dejar: «No, dejarlas»). ---
FIRMA <- list(fecha = "2026-11-13", md5 = "b9d489a9dd838630875a4958146b9bc8",
  texto = paste("1. Tras ver la tabla de atajos anterior a la pasada 4: «Firmar los atajos de la tabla y aprobar».",
                "2. Tras ver las seis cifras que cambiaron o eran nuevas tras la pasada 4: «Firmar también estos seis y aprobar»."))
## Firmadas FUERA de la batería congelada (no se amplía, §P7-C): medidas por el detractor de la ronda 4.
FIRMADAS_FUERA_BATERIA <- data.frame(stringsAsFactors = FALSE,
  atajo = c("la más cercana a la opción de la calculadora", "entre las de numerador múltiplo de x, la más cercana al seno",
            "si x es positivo la mayor, si es negativo la menor"),
  cifra_pp = c(75.0, 50.0, 8.3),
  nota = c("", "", "acierto del 100 % en la rama x = ±3 con tres términos"),
  fuente = "salida/fase2c_r4_detractor.md (H2)")"""),
("""## MUTANTE_FIRMA_FICTICIA=1 rellena una firma de prueba (NO es del profesor) para probar el camino ROJO_CUBIERTO (FASE 2C r4 H6).
if (nzchar(Sys.getenv("MUTANTE_FIRMA_FICTICIA"))) { FIRMA <- list(fecha = "MUTANTE", texto = "firma ficticia de prueba: no es del profesor"); cat("MUTANTE: firma ficticia\\n") }
""", ""),
("## --- Alcance pendiente de firma: cada regla", "## --- Alcance FIRMADO el 2026-11-13: cada regla"),
("""firmada <- nzchar(FIRMA$fecha) && nzchar(FIRMA$texto)""",
 """firmada <- nzchar(FIRMA$fecha) && nzchar(FIRMA$texto) && nzchar(FIRMA$md5)
md5_actual <- unname(tools::md5sum(rmd))"""),
("""if (nrow(sobre) == 0L) { cat("\\nVEREDICTO_P7: PASS\\n"); quit(status = 0L) }""",
 """cat("\\n-- Firmadas fuera de la batería --\\n"); for (i in seq_len(nrow(FIRMADAS_FUERA_BATERIA))) with(FIRMADAS_FUERA_BATERIA[i, ],
  cat(sprintf("%s: %+.1f pp%s (%s)\\n", atajo, cifra_pp, if (nzchar(nota)) paste0("; ", nota) else "", fuente)))
cat(sprintf("\\nFirma: %s · md5 firmado %s · md5 medido %s\\n", if (firmada) FIRMA$fecha else "(ninguna)", FIRMA$md5, md5_actual))
if (nrow(sobre) == 0L) { cat("\\nVEREDICTO_P7: PASS\\n"); quit(status = 0L) }
if (firmada && !identical(md5_actual, FIRMA$md5)) {
  cat(sprintf("\\nVEREDICTO_P7: FIRMA_NO_CUBRE — la firma del %s es del md5 %s; este archivo tiene md5 %s\\n", FIRMA$fecha, FIRMA$md5, md5_actual))
  quit(status = 4L)
}"""),
]
for a, b in R:
    c = s.count(a)
    if c != 1: sys.exit("NO COINCIDE (%d): %s" % (c, a[:90]))
    s = s.replace(a, b)
open(p, "w", encoding="utf-8").write(s); print("firma registrada:", len(R), "cambios")
