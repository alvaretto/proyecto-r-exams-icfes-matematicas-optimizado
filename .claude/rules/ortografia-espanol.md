# Regla #7 — Ortografía Española (versión compacta)

> Diccionario completo y tabla de errores frecuentes: `.claude/docs/reglas/ortografia-espanol.md`. **Copiar palabras tal cual; no improvisar.**

**Regla de generación.** Todo texto visible al estudiante lleva tildes correctas DESDE EL PRIMER BORRADOR: `paste0()` de contextos y errores (`descripcion_corta/larga`, `causa_raiz`), reflexiones, `sol_texts`, Markdown de Question/Solution y encabezados.

**Frecuentes:** más, según, así, después, también, además · ángulo, gráfica, función, número, cálculo, método, código, análisis, máximo, mínimo · solución, ecuación, relación, información, distribución, sección, versión, opción, intersección, exclusión, unión, omisión, confusión, reflexión, operación, expresión, región, interpretación, formulación, argumentación · matemático, estadística, único, numérico · realizó, preguntó, organizó, descubrió, publicó, encuestó, identificó, excluyó, confundió, cometió, aplicó, interpretó, calculó · cafetería, boletín, periódico, compañeros, jóvenes, área, comité · ¿Cuál?, ¿Qué?, cómo (interrogativo).

**Sin tilde (excepciones):** nombres de variables R; identificadores de R-exams (`exname`, `exsection`, `extype`, `exsolution`, `exshuffle`, `extol`) y las **claves** `exextra[Nombre]`, siempre ASCII; inglés técnico; demostrativos "Esta/Este".

**Valores de `exextra`: literales del catálogo ICFES** (2026-10-22). Se copian tal cual del catálogo canónico o de la ficha, con tildes, mayúsculas y signos: `exextra[Competencia]: Argumentación`, `exextra[GradoSugerido]: 6°-7°`. No son identificadores: R/exams los guarda como texto UTF-8 y los cuatro formatos renderizan. Los valores en ASCII que ya existen se corrigen al tocar el ejercicio, sin migración en bloque.

**Validar:** `Rscript .claude/scripts/corregir_ortografia_espanol.R archivo.Rmd` (`--fix` para aplicar). Su "limpio" no prueba que haya tildes. **NUNCA** `git commit --no-verify`.
