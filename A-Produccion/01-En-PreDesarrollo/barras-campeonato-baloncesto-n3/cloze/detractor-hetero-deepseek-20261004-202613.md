# Revisión Detractor heterogénea (FASE 2C-bis)

- **Objetivo**: /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/barras-campeonato-baloncesto-n3/cloze/barras_campeonato_baloncesto_aleatorio_interpretacion_representacion_n3_cloze_v1.Rmd
- **Motor / modelo**: deepseek / deepseek-v4-flash
- **Fecha**: 2026-10-04 20:34:19 · **Duración**: 486s · **Intentos previos**: ninguno
- **Familia distinta de Anthropic**: sí — complemento del AgenteDetractor de FASE 2C, no lo sustituye

---

⚠ claude.ai connectors are disabled because ANTHROPIC_API_KEY or another auth source is set and takes precedence over your claude.ai login · Unset it to load your organization's connectors
"deepseek-v4-flash" isn't described by this version's model catalog; update Claude Code, or map it with behavesAs on a modelPicker row (or modelOverrides, if it is a provider id of a model this version knows). Until then auto-compact keeps this session within 200k tokens (the context window it assumes); if the model accepts more, append [1m] to the model name for 1M, or set CLAUDE_CODE_MAX_CONTEXT_TOKENS to its real window; CLAUDE_CODE_DISABLE_UNKNOWN_MODEL_WINDOW_ENFORCEMENT=1 restores the previous wait-for-the-API behavior.
[claude-code:unrecognized_model] {"model":"deepseek-v4-flash","query_source":"sdk"}
He cerrado la última verificación: el XML de Moodle está correcto (los 6 gaps con el tipo y la marca correctos), los dos falsos positivos que yo mismo había anotado se descartan, y encontré un desajuste documental en D-17. Emito el reporte.

---

# Revisión Detractor — FASE 2C-bis (heterogénea)

**Fecha**: 2026-10-04
**Modalidad**: Auditoría adversarial, segundo detractor (familia de modelos distinta)
**Objetivo**: `A-Produccion/01-En-PreDesarrollo/barras-campeonato-baloncesto-n3/cloze/barras_campeonato_baloncesto_aleatorio_interpretacion_representacion_n3_cloze_v1.Rmd`
**md5 del artefacto auditado**: `17df2bfc99ebad7aaae008f5fb9775cb` (coincide con la línea base del HANDOFF)
**Acompañantes leídos**: `cloze/HANDOFF.md`, `cloze/ejercicio_state.json`, `cloze/solpcielo_cloze.tex`, `cloze/verificar_dibujo_clave_cloze.R`, `../HANDOFF.md` (SCHOICE hermano) y los render de `cloze/salida/` (PDF, HTML, DOCX, Moodle XML)
**Restricción respetada**: sólo lectura. No escribí ni edité ningún archivo.

---

## 1. Resumen ejecutivo

**No encontré ninguna CORRECCIÓN.** No hay clave falsa, ni segunda clave, ni Solution falsa, ni distractor que resulte correcto en alguna rama. Lo verifiqué por enumeración (300 versiones) y por lectura semántica de las seis partes, no por confianza en el HANDOFF.

**Encontré un hallazgo de DIAGNOSTICIDAD que obliga**, y que el primer detractor no señaló porque el HANDOFF lo presenta como cerrado: el residuo declarado de **D-16 («formato: +13,2 pp») supera el umbral de +8 pp de §P7-A**, y §P7-E es explícita en que aceptar un canal por encima de ese umbral *es un OVERRIDE humano firmado (H-5), con la batería en rojo*. El HANDOFF lo cierra como «VERDE con residuo declarado» y, además, el propio estado reconoce que **queda una pasada de §P7-D sin consumir** (`p7d_cloze: 2 pasadas de 3`). No es un defecto de la clave: es un cierre de procedimiento prematuro sobre un canal que la regla manda tratar de otra forma.

Añado un desajuste documental verificado (**D-17 está desactualizado**: sólo 1 de los 3 Semilleros es realmente el problema declarado) y descarto, con evidencia, dos falsos positivos que yo mismo había levantado durante la auditoría.

El ejercicio está en condiciones de pasar a la firma del profesor, pero **no** en condiciones de declarar §P7 cerrado como está.

---

## 2. Objeciones por severidad

### Objeción 1 — El residuo de §P7 de la Parte 1/§P7-E se cierra sin cumplir la regla

**Qué se cuestiona**: la línea D-16 del HANDOFF — *«→ formato: +13,2 pp TODO de la canónica … La canónica ya se reconoce por el enunciado (§P7-E)»* — cerrada como «VERDE con residuo declarado».

**Por qué** (Fuente Nivel 1 — norma interna del repo, `.claude/rules/diversidad-sustantiva.md` §P7-A y §P7-E):

> **§P7-A**: «Aceptable **≤ +5,3 pp**; obliga sólo **> +8 pp**.»
> **§P7-E**: «Medir todo canal también sobre la instancia canónica (enumeración exacta). **Aceptar un canal > +8 pp es un OVERRIDE humano firmado (H-5); la batería queda en rojo.**»

El canal medido es **+13,2 pp**, es decir **+5,2 pp por encima del umbral que obliga**. La regla no admite «VERDE con residuo declarado» para un canal que supera +8 pp: admite dos salidas y sólo dos — seguir corrigiendo, o firmar el override y **dejar la batería en rojo**. El HANDOFF no hizo ni una ni la otra: la declaró verde.

**Riesgo concreto**: la mitigación que se invoca («la canónica ya se reconoce por el enunciado») es precisamente el argumento que §P7-E ya descartó *a priori* —por eso exige la medición sobre la instancia canónica y no la acepta por diseño. Y hay una pasada de corrección disponible que no se consumió: el propio `ejercicio_state.json` dice `"p7d_cloze": "2 pasadas de 3 consumidas"` y el HANDOFF lo repite en §«§P7-D del CLOZE». Con una pasada intacta y un canal sobre umbral, el cierre como verde no está sostenido por la norma.

**Alternativa propuesta** (una de las dos, no ambas):

- **A. Consumir la tercera pasada §P7-D** atacando el canal «formato». El canal nace de que el vocabulario de la canónica («décimo»/«undécimo», y grados que excluyen sexto/séptimo del caso de práctica) es exclusivo de la rama canónica; el objetivo es que el token visible deje de predecir el formato. Si el canal resulta **estructural** —una sola combinación viable— entonces §P7-F obliga a **enumerar el espacio de diseño y documentarlo** antes de declararlo: hoy esa enumeración no está en el HANDOFF.
- **B. Si A es inviable por diseño**, obtener la firma humana H-5 **por escrito** (como ya se hizo con `exshuffle: FALSE`) y consignar **§P7 en rojo** tanto en `HANDOFF.md` (D-16) como en `ejercicio_state.json`, con la cifra y el umbral al lado. Un rojo firmado es un estado legítimo; un verde sin firma, no.

**Veredicto**: MODIFICAR (dejar §P7 en rojo firmado, o agotar la pasada y re-medir).

---

### Objeción 2 — D-17 está desactualizado: declara 3 Semilleros rojos, sólo 1 lo está

**Qué se cuestiona**: la línea D-17 del HANDOFF — *«**ROJO** | `SemilleroUnico_v2.R` → `.Rmd` SCHOICE (y NOPS); `SemilleroCloze.R` y `SemilleroMoodle_v2.R` → teorema de Pitágoras»*.

**Por qué** (Fuente Nivel 1 — verificación directa sobre el disco, ejecutada por mí):

```
SemilleroUnico_v2.R:9   archivo_examen <- "barras_campeonato_baloncesto_..._cloze_v1.Rmd"   [mtime 2026-10-04 20:27]
SemilleroMoodle_v2.R:8  archivo_examen <- "barras_campeonato_baloncesto_..._cloze_v1.Rmd"   [mtime 2026-10-04 20:28]
SemilleroCloze.R:27     archivo_examen <- "01-teorema_pitagoras_..._cloze_v1.Rmd"           [mtime 2025-11-02]
```

Contra lo que dice el HANDOFF, `SemilleroUnico_v2.R` **no** apunta al SCHOICE: apunta al CLOZE correcto. `SemilleroMoodle_v2.R` también. Es decir, **el ROJO es 1/3 real y 2/3 documentación vieja**, y los dos arreglos son posteriores a la línea base (mtime 20:27/20:28 contra la línea base de 20:07) sin que el HANDOFF se actualizara.

**Riesgo concreto**: dos riesgos, en direcciones opuestas. (i) **Real**: `SemilleroCloze.R` sigue siendo un archivo heredado (mtime 2025-11-02) que renderiza **otro ejercicio** — un profesor que lo ejecute obtiene el teorema de Pitágoras creyendo que genera este CLOZE. (ii) **De confianza**: mientras D-17 diga «los 3» cuando sólo falla 1, cualquier auditor futuro que verifique el HANDOFF encontrará una afirmación falsa y no sabrá si el resto del documento merece crédito.

**Alternativa propuesta**: corregir o retirar `SemilleroCloze.R` (sustituirlo por la copia de `SemilleroUnico_v2.R` renombrada, o borrarlo si es un residuo de otro subproyecto), y **reescribir D-17** con la medición actual: 2/3 verdes, 1/3 rojo, con el mtime de cada uno. Tarea mecánica, no de diseño.

**Veredicto**: MODIFICAR.

---

### Objeción 3 — Paso 11 abierto: el ejercicio no es promovible todavía

**Qué se cuestiona**: `ejercicio_state.json` → `aprobacion_usuario.completado: false`.

**Por qué** (Fuente Nivel 2 — `workflow-state-enforcement.md`, paso 11 del flujo de 11): el estado se sella **sólo al ejecutarse**; auditar por artefacto, no por flag. Aquí el flag es coherente con el artefacto: no hay firma del profesor.

**Riesgo concreto**: ninguno técnico; es la compuerta humana. Lo consigno para que el veredicto de este reporte no se lea como «listo para promover». Un detractor que aprueba no promueve.

**Alternativa propuesta**: que el profesor revise `salida/` y responda la PREGUNTA_AL_RETOMAR, incluida la **firma de las 10 afirmaciones de la Parte 5** (que el HANDOFF ya declara como oráculo humano pendiente: `VERDAD_P5` del verificador es un juicio humano, no una medición).

**Veredicto**: MANTENER (abierto por diseño; requiere acción humana, no corrección).

---

## 3. Falsos positivos que levanto y descarto (para que no se reabran)

Registro aquí dos cosas que **parecían** defectos y que verifiqué hasta descartar. Las dejo escritas porque un auditor posterior puede tropezar con las mismas y el trabajo de refutarlas ya está hecho.

**3.1. «El feedback de la Parte 4 está pegado a la opción equivocada»** — descartado. Al leer la página 1 del PDF en imagen creí ver la marca en la 3.ª opción, mientras la Solution enumera el feedback «Correcto» en la 4.ª. `pdftotext -layout` sobre la hoja de respuestas resuelve la duda en contra de mi lectura:

```
1. a)  (a)  (b)  (c) X (d)      ← P1: clave en (c)
   d)  (a)  (b)  (c)  (d) X     ← P4: clave en (d)
   e)  (a) X (b) X (c)  (d)  (e) ← P5: 2 marcas
```

La opción marcada de P4 es la 4.ª, que en la Answerlist de Solution es el feedback «Correcto. E1 convierte los datos reales en los de la gráfica del estudiante». **Alineado.** Mi lectura de imagen era la parte no confiable, no el archivo.

**3.2. «El XML de Moodle renumera la Parte 6 como Parte 4»** — descartado. Un `grep 'Parte 4\.[^<]*'` devolvía el texto de la Parte 6 bajo la etiqueta «Parte 4.». La inspección del XML completo muestra que es la **referencia explícita del enunciado**, no un renumerado:

```html
<p><strong>Parte 6.</strong> Considera de nuevo los datos reunidos correctamente
en la Parte 4. Indica si la siguiente afirmación es verdadera o falsa: …</p>
```

Las seis etiquetas aparecen en orden correcto en las 5 copias. Era un artefacto de mi propio grep, no un defecto.

---

## 4. Verificación por dominio (los 8 de la regla #9 + overrides)

| Dominio | Veredicto | Evidencia |
|---|---|---|
| **código R/exams** | VERDE | Enumeración en memoria N=300: 0 errores de generación; en 300/300 la clave es única (exactamente una matriz de opciones igual a `M`, y es la marcada); las 4 opciones son distintas entre sí dos a dos; el reparto de formato es siempre 2+2 (`stopifnot` ejecutado); `exsolution` con 6 campos; `fig_id` único 300/300. Guardas de la regla #20 (`\newcounter{none}`), #18 (`{width=…}&#8203;`) y #4 v6.1 (sufijo por versión) presentes. |
| **pedagógico** | VERDE | Las 6 partes son progresivas (identificar → calcular → evaluar → transferir) y ninguna es procedimental pura. La Parte 5 mantiene pares concepto con un miembro verdadero y uno falso, y la opción con matiz no delata a la verdadera (medido: 41/100 en el ciclo 1 → corregido). La Solution contiene las siete secciones metacognitivas. |
| **visual** | VERDE (Familia B, N declarado) | Leí el PDF renderizado: la instancia canónica coincide con el ítem impreso (Gráfica I = E1 apilada; Gráfica II conserva el literal 4,5 apilada; Gráfica III = clave agrupada; Gráfica IV = E4 agrupada). El rótulo «Gráfica I…IV» se dibuja **dentro** de cada figura (no se separa por salto de página). **N de renderizado = 2 copias leídas visualmente** (canónica y rama E8); el resto de las versiones queda cubierto por el verificador, no por ojo. |
| **gramática / ortografía** | VERDE | `validar_glifos_latex.R` → «OK: sin glifos que rompan pdflatex». Sin glifos prohibidos por la regla #25. La ortografía del texto visible es correcta; la concordancia partidos/partidas está heredada del SCHOICE. |
| **coherencia matemática** | VERDE | `validar_coherencia_matematica.R` → un único error, `ERR_C4: exshuffle debe ser TRUE`, que es **el override firmado** (ver más abajo), no un defecto oculto. Niveles 4 y 5, coherencia cloze, matemática y código: OK. Recalculé a mano la clave de las seis partes desde lo que muestran la tabla y las gráficas: coinciden. |
| **ICFES metacognitivo** | VERDE | `DOK 2` / `Nivel 3` no viola `DOK ≥ 3 ⇒ Nivel ≥ 3` (la implicación va en la otra dirección). Las 11 `exextra` son idénticas al SCHOICE y literales en el catálogo. Competencia/Componente/Afirmación/Evidencia coherentes. |
| **testing** | VERDE | `verificar_dibujo_clave_cloze.R` (N=100) → 100/100, y **14/14 mutantes detectados**. Leí el script: cubre el dibujo de la gráfica de la Solution (líneas 138-139), que la Solution nombre la gráfica y el formato correctos (142-143), que **los números que cita la Solution igualen la matriz real** (144), la Answerlist de Solution de la Parte 1 (156) y de las Partes 4, 5 y 6 (219-221), la alineación P4 con `exsolution` (210) y la no-inversión del valor de verdad de P6 (212). **Sospeché que la prosa de la Solution de la Parte 1 quedaba sin guardia y no es así**: está cubierta. |
| **semántico (Nivel 4)** | VERDE | Leí las cinco afirmaciones de la Parte 5 y las etiqueté yo mismo, sin mirar `verdad_pre`: (1) V, (2) V, (3) F, (4) F, (5) F. Coincide con el patrón del XML (`MULTIRESPONSE` con dos `=`) y con la hoja (`e) (a) X (b) X`). La distinción agrupadas→apiladas (representación) frente a intercambiar la leyenda (atribución) es conceptualmente correcta; la afirmación 3 es exactamente la trampa E8. |
| **overrides firmados e invariantes locales** (regla #9 v1.5) | VERDE | `exshuffle: FALSE` está **firmado (H-5)** y su justificación es verificable de forma independiente: con `TRUE` R/exams mezclaba la lista «Gráfica I–IV» del gap mientras la hoja del PDF pide (a)–(d) **por posición**, de modo que quien identificaba bien la gráfica quedaba mal calificado en ~3 de cada 4 versiones. La mezcla se hace internamente (`orden`, `perm4`, `perm5`). Verifiqué en el XML que **ningún gap usa `_S`**, así que Moodle no vuelve a mezclar: el override es coherente de punta a punta. `ERR_C4` está declarado, no silenciado. |

### Verificación específica del XML de Moodle (no es de los 8 dominios, pero es donde vive un modo de fallo típico)

Inspeccioné las 5 copias del `moodle.xml` (3,6 MB) parte por parte:

| Parte | Tipo en XML | Marca correcta | ¿Correcto? |
|---|---|---|---|
| 1 | `MULTICHOICE` | `=Gráfica III` | ✓ |
| 2 | `NUMERICAL` | `=13` | ✓ |
| 3 | `NUMERICAL` | `=6` | ✓ |
| 4 | `MULTICHOICE` | `=Grados intercambiados` | ✓ |
| 5 | **`MULTIRESPONSE`** | varios `=` (2 verdaderas) | ✓ |
| 6 | `MULTICHOICE` | `=Verdadero` | ✓ |

El punto crítico es la Parte 5: si se hubiera exportado como `MULTICHOICE` (selección única) en lugar de `MULTIRESPONSE` (casillas), el estudiante **no podría marcar dos opciones** y la parte sería irresoluble en Moodle. Exporta como `MULTIRESPONSE`. Ningún gap lleva `_S` ni markup, y los nombres de archivo son neutrales.

---

## 5. Cifras de la compensación posicional (confirmación independiente)

Reconté la posición de la clave en 300 versiones: **76 / 68 / 72 / 84** frente a 75 esperado por posición. La compensación documentada en el comentario del `data_generation` (`1/8 + 7/8 × 1/7 = 1/4 ; 7/8 × 2/7 = 1/4`) **funciona empíricamente**, y con ello queda confirmado el arreglo de una objeción del ciclo anterior. Igualmente confirmé que la frase `nota_borde` de la Solution es verdadera **exactamente** en la rama E8 (89 versiones = las de `rama_e8`), es decir, la Solution nunca afirma algo falso sobre el borde superior.

---

## 6. Veredicto global por dominio

- **CORRECCIONES (binarias, bloqueantes): 0.** Nada falsifica la clave, ninguna segunda clave, ninguna Solution falsa, ningún distractor correcto en ninguna rama.
- **DIAGNOSTICIDAD que obliga: 1** (Objeción 1, §P7: +13,2 pp sin firma y con una pasada §P7-D disponible).
- **Documentación/empaquetado: 1** (Objeción 2, D-17 desactualizado) y **1 paso humano abierto** (Objeción 3).

El ejercicio está listo para la revisión del profesor; no está listo para declarar §P7 cerrado ni para promover.

---

## 7. Próximos pasos priorizados

1. **Decidir §P7 por escrito** (Objeción 1): consumir la tercera pasada §P7-D sobre el canal «formato», o firmar el override H-5 y marcar §P7 en rojo en `HANDOFF.md` D-16 y en `ejercicio_state.json`. Si se concluye que el canal es estructural, documentar antes la **enumeración del espacio de diseño** que exige §P7-F.
2. **Arreglar `SemilleroCloze.R`** (Objeción 2) y reescribir D-17 con la medición de hoy: 2/3 verdes.
3. **Firma del profesor** de las 10 afirmaciones de la Parte 5 (`VERDAD_P5` es juicio humano, no medición) — es la deuda que el HANDOFF ya declara y que ninguna automatización cierra.
4. Sólo después: `workflow-state.sh complete <dir> aprobacion_usuario`, evidencia de aula y `/promover-ejercicio`.

---

**Dominios no auditados**: ninguno. Los 8 dominios de `.claude/rules/detractor-obligatorio.md` y el bloque de overrides firmados/invariantes locales quedan cubiertos, con el único límite declarado de la **Familia B** (la revisión visual de la Solution se hizo sobre 2 copias renderizadas: canónica y rama E8; el resto de las versiones queda cubierto por el verificador programático, no por inspección visual).

VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
