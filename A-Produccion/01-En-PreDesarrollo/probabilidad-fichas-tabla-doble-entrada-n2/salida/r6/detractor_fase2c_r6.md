# Auditoría Detractor FASE 2C (r6): `probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd`

**Fecha:** 2026-10-27
**Versión auditada:** md5 `4fa520bb5f7ce85cdaa3313ca6f2c8ab`. Comprobé el md5 al empezar y al terminar.
**Independencia:** soy un detractor nuevo. No escribí ni corregí este ejercicio y no audité versiones anteriores.
**Objeciones:** 4. Ninguna de CORRECCIÓN; 1 ALTA de diagnosticidad, 1 MEDIA y 2 BAJA.

## Resumen ejecutivo

- **Corrección: sin hallazgos.**
  - La clave es verdadera en la canónica (26 = 26) y en todas las ramas no canónicas.
  - Lo dicho en la Solution y el feedback es verdadero en cada rama.
  - Se cumplen la regla #19 y la regla #25.
- **La pasada 2 cerró lo que debía cerrar.** Las reglas de magnitud del H2 de r5 ya no superan el corte.
  - Midiendo con 12 sondas, **ningún canal nace de la pasada 2**: todas bajan o se quedan igual respecto de la copia r5.
- **La regla «celdas más juntas», sola, da +7,6 pp ± 0,4.** Es el valor exacto, medido en una depuración con M = 50 000 declarada.
  - Coincide con el +8,0 que se declaró como residuo.
  - El +9,2 de E4c es ruido de una muestra de 200.
- **Hay un canal que las firmas no cubren (H1).** Las dos firmas vigentes y el residuo declarado se pueden **encadenar**:
  - Primero se aplica la regla firmada (descartar la pareja que repite, o la compuesta).
  - Después se elige la pareja con las celdas más juntas.
  - El acierto llega al **47,6 %** (repite + juntas) y al **64,8 %** (compuesta + juntas) en las no canónicas.
  - Eso son +14 pp por encima de cada firma. En el ítem oficial ambas cadenas dan 0 %.
  - El canal ya existía en r5 (62,0 % y 79,8 %). La pasada 2 lo bajó unos 15 pp, pero no lo cerró.

## Estado de lo señalado por r5

| r5 | Estado en r6 | Cómo lo verifiqué |
|---|---|---|
| H1: la regla compuesta llega a 1/2 | Firmada por el profesor (CLAUDE.md:162). El código no cambia esa estructura. E4c la ancla a ≤ 1/2 | `verificar_espacio.txt:193` |
| H2: asimetría de rango 5..20 frente a 3..24 | **Resuelto.** La línea 202 saca `a` y `b` de forma independiente de `r_resto`, y la 203 filtra `c_ok` con \|b − cc\| ≤ 15 | Diff contra la copia r5, más sondas con M = 50 000 (tabla de H1) |
| H3: registro | Corregido en CLAUDE.md (I-8 cita +7,1…+8,4, y E4 da +8,4). En el HANDOFF quedan restos | H3 |
| H4: conjuntos vacíos en E1 y guarda duplicada | **Resuelto.** E1 cuenta `aborto_vacio` (0) y la guarda duplicada ya no está | Diff del verificador |

## Hallazgos

### H1. La cadena «firma vigente + celdas más juntas» supera las firmas (ALTA, DIAGNOSTICIDAD; no nace de la pasada 2)

**Qué se cuestiona.** La regla compuesta está firmada en ≤ 1/2 («repite + su gemela de molde»). Siempre deja solo dos opciones: la clave y el espejo. Si entre esas dos se elige la de celdas más juntas (rango de las 4 celdas), se acierta mucho más que 1/2.

**Mecanismo.** La clave tiene sumas iguales: a + b = cc + d. Por eso sus dos pares de celdas comparten el centro s/2, y su rango es max(|a − b|, |cc − d|). El espejo no tiene esa estructura.
- Rango mediano de las 4 celdas (N = 100): clave 9, espejo 13, PROB-UNA 10, PROB-CER 12.
- P(rango de la clave < rango del espejo) = 0,70, con 0,02 de empates.

**Medición con el generador real** (chunk completo; N = 100 no canónicas, semillas 1..109; 9 canónicas por el camino):

| Regla | No canónica, N = 100 | Depuración M = 50 000 (± 2 SE) | Misma regla en la copia r5 (M = 50 000) | Canónica / ítem oficial | Mezcla al 10 % |
|---|---|---|---|---|---|
| Celdas más juntas | 37,3 % | **32,6 % (+7,6 ± 0,4)** | 45,0 % | 0 % | 29,4 % (+4,4) |
| Repite + juntas | 53,3 % | **47,6 % (+22,6 ± 0,4)**; +14,2 sobre la firma de 1/3 | 62,0 % | 0 % | 42,8 % (+17,8) |
| Compuesta + juntas | 71,0 % | **64,8 % (+39,8 ± 0,4)**; +14,8 sobre la firma de 1/2 | 79,8 % | 0 % | 58,3 % (+33,3) |

Cómo se hizo la depuración:
- Usé una réplica abstracta de `construir_cubo()`, extraída por `parse()` del propio `.Rmd`.
- Las reglas no dependen de `perm` ni de `flip`, así que la réplica basta.
- La cifra de N = 100 del generador real cae dentro del ruido de la depuración.

Otras sondas (M = 50 000, valor de r5 → valor de r6). Todas están dentro de ±3 pp del azar en r6, y ninguna sube por la pasada 2:
- Menor máximo: 38,2 → 26,2.
- Sin los extremos de la tabla: 34,4 → 22,5.
- Contiene el máximo: 13,5 → 24,0.
- Contiene el mínimo: 23,4 → 24,3.
- Mayor mínimo: 21,7 → 25,5.
- Un tipo con sus dos celdas iguales: 29,0 → 27,7. Esta era la candidata a nacer de que `a` y `b` sean ahora independientes, y no lo hace.
- Juntas medida por desviación típica: 45,0 → 32,9.

**¿La usaría un estudiante?**
- **«Celdas más juntas», sola, sí.** Es la lectura ingenua «misma probabilidad ↔ números parecidos», la misma que la ficha ancla en C. El ítem oficial la castiga (lleva a C, 0 %). Las no canónicas la premian un poco: +7,6, dentro del residuo declarado. PROB-CER atrapa esa lectura en las sumas, pero no en las celdas.
- **Las dos cadenas exigen aprender entre versiones** que la pareja que repite nunca es la clave (y, en la compuesta, que las opciones van en parejas de molde). Son heurísticas de alguien que ya conoce el examen. En un Moodle con muchas versiones se pueden aprender, igual que la compuesta que ya se firmó.

**Por qué ninguna firma lo cubre.**
- La firma de CLAUDE.md:162 y E4c son **por cifra**: compuesta ≤ 1/2 y repite 1/3.
- «Celdas más juntas» se declaró aparte, con +8,0.
- Nadie midió la cadena. Su último paso añade un hecho que no es «el mismo que el 1/3»: el centro común de una clave de sumas iguales.

**¿Es estructural? Lo probé con un prototipo** (depuración por rechazo, solo para estimar la distribución objetivo). Si la diferencia de sumas del espejo se fija en 2..8 (en el oficial es 8):
- «Juntas» sola baja a 24,6 %.
- La cadena compuesta + juntas apenas se mueve: 61,8 %.
- Si además se restringe así la diferencia de sumas de PROB-UNA: 61,7 %.

La brecha de sumas no es la palanca. No aislé la causa, así que no afirmo cuál es. Según §P7-F, es candidata a canal estructural.

**Riesgo concreto.** En la mezcla que ve el aula, un estudiante que ya conoce el examen acierta el 58,3 % con la cadena compuesta + juntas (+33,3 pp) y el 42,8 % con repite + juntas (+17,8). Las firmas vigentes dan 45 % y 33,3 %.

**Alternativas** (decide el profesor, H-5):
- **(a) Seguir la regla de parada que fija el propio HANDOFF** («Si el detractor de r6 abre otro canal: parar y firmar»).
  - Firmar **por alcance**: «cualquier encadenamiento de las firmas de repetición con reglas de magnitud sobre las celdas».
  - Dejar escritas las cifras exactas: 47,6 % y 64,8 % en las no canónicas; +17,8 y +33,3 en la mezcla; 0 % en el oficial.
  - Medir ambas cadenas en E4c (ver H2).
- **(b) Usar la pasada 3, la última de §P7-D.**
  - Antes de tocar código, enumerar el espacio de diseño del espejo (§P7-F).
  - Objetivo: P(rango de la clave < rango del espejo) ≈ 0,5.
  - Simular el cambio sobre una copia antes de aplicarlo. El prototipo de arriba ya descarta el ajuste más obvio.

**Veredicto:** MODIFICAR (registro y firma, o pasada 3).

### H2. E4c casi no puede fallar y no mide cadenas (MEDIA, guarda/registro, no afecta al estudiante)

- La línea 358 marca «CANAL» solo si `ex_nc − 2·SE > 0,08`. Con N = 200 y 2·SE = 6,4 pp (`verificar_espacio.txt:197`), «celdas más juntas» tendría que superar unos +14,6 pp para que E4c fallara.
  - Por debajo de eso queda en «zona del corte (no concluyente)», aunque el valor exacto sea +7,6.
  - El residuo declarado podría derivar al doble sin que el verificador lo note.
- E4c no tiene ninguna regla encadenada (H1).

**Alternativa.**
- Calcular las tasas de E4c **exactas**, ponderando la enumeración de E1 con las probabilidades del generador. La regla #23 admite la enumeración exhaustiva de un espacio finito.
- Hacer fallar el verificador si una tasa supera la cifra declarada más una tolerancia fija.
- Añadir `repite+juntas` y `compuesta+juntas` como reglas congeladas. §P7-C exige además volver a medir el histórico.

### H3. El HANDOFF va por detrás del verificador (BAJA, registro)

La fila r6 de `HANDOFF.md:25` tiene dos problemas:
- Sigue diciendo «en curso al escribir esto».
- Atribuye a «E4c» las cifras de la depuración N = 2000 (−1,9 / −1,3 / −1,5 / +8,0). El E4c del verificador imprime otras: +1,6 / −1,7 / +0,9 / +9,2.

Ninguna de las dos series es la exacta. **Alternativa:** registrar la cifra exacta de cada regla (por ejemplo, +7,6 para «celdas más juntas») y dar el origen de cada serie.

### H4. Los encabezados de la Solution salen numerados «0.0.1» en el PDF (BAJA, presentación)

- `cat("### …")` (líneas 698 y siguientes) produce `\subsubsection` numeradas en la plantilla de `exams2pdf`.
- En `preview_s1-1.png` se ve «0.0.1 Respuesta correcta» y «0.0.2 Procedimiento correcto».
- En HTML y Moodle no pasa.

**Alternativa:** `### Respuesta correcta {-}`. Según la Pandoc User's Guide (extensión `header_attributes`), un guion en los atributos equivale a `.unnumbered`. Antes de aplicarlo, hay que renderizar un PDF y comprobarlo.

## Respuestas al encargo

**1. Corrección**
- **Canónica:** B = 26 / 26, A = 20 / 28, C = 11 / 10, D = 8 / 20. Las cuatro opciones y la tabla se comparan contra literales.
- **No canónicas:**
  - La clave se construye: a + b = cc + d.
  - E1 recorre de forma exacta las 10.401.300 combinaciones: 0 segundas igualdades y 0 pares complementarios. La clave es la única igualdad con las tres lecturas (al menos una, exactamente una, ambas).
- **Textos, rama por rama:**
  - `explica_una`: la posición k y el rótulo de la dimensión libre coinciden, porque `celdas_tipo` ordena por esa dimensión.
  - `explica_cer`: la diferencia es exactamente 1 por construcción.
  - `explica_sim`: el espejo tiene sumas distintas.
  - En la canónica, C va con PROB-CER y D con `TXT_CANON`, sin sumas.
  - «En cada extracción» y «se saque una ficha o se saquen dos» son verdaderas: con R2, la probabilidad marginal y la de «al menos una» solo dependen de n.
- **Semilla 1 en el PDF:** 3 + 6 = 4 + 5 = 9, total 81, y cada suma de la Solution coincide con la tabla.
- **Regla #19:** «La respuesta correcta es la que afirma…».
- **Orden de la prosa:** los distractores se recorren con `sort(codigos_sel)`, no en el orden mostrado.
- **Glifos:** en el fuente solo aparecen `§` y `…`, ambos seguros. El hook 2O da OK.
- **Tensión, no falsedad:** la Estrategia dice «Una celda que se repite no decide nada, ni a favor ni en contra». En las no canónicas, la pareja que repite es siempre trampa. Ya lo señaló r5.

**2. `construir_cubo()` frente a la réplica del verificador**
- Coinciden. Las ANCLAS son literales, `C_OK` usa la misma expresión, `EH` y `FG` tienen los mismos conjuntos de exclusión y `pertenece()` usa m = e − h.
- Ninguna exclusión sobra ni falta. Cada una evita exactamente una repetición o una igualdad; lo comprobé a mano en las cuatro ramas.
- No hay abortos alcanzables:
  - Como d − a = b − cc, se cumple |k| ≤ 16. Los conjuntos desplazados conservan al menos 6 − 4 = 2 valores.
  - `deltas` nunca queda vacío, gracias a cc ≠ b y m ≠ m_malo.
  - `stopifnot(length(c_ok) > 0)` (línea 204) no se puede alcanzar: E1 da `aborto_c_ok = 0`. Es defensiva e inocua.

**3. Guardas**
- Los 25 mutantes son cazados por su sonda, y las guardas no tautológicas pueden fallar.
- Hay tres guardas que son trampas contra ediciones, no contra datos: `P_CANON <= 0.10`, pool ≥ 4 y `c_ok > 0`.
- `rowSums < N` está implicada por `w >= 1`, y el código lo declara.
- La debilidad de E4c está en H2.

**4. Registro**
- Las firmas coinciden con el código: P_CANON 0,10; PROB-CRU solo en la canónica; C con PROB-CER; compuesta ≤ 1/2; pasada 2 con 3..24 (líneas 196, 202 y 203).
- El +8,0 declarado coincide con el exacto (+7,6).
- La explicación del falso positivo 2N es coherente con el `set -o pipefail` de la línea 29 del hook. No reproduje el SIGPIPE.
- Los desfases del HANDOFF están en H3.

**5. Severidad**
- No hay ninguna objeción de CORRECCIÓN.
- H1 afecta a lo que el estudiante puede explotar y supera en más de +8 pp las firmas. Bloquea la FASE 3 hasta que el profesor firme o se use la pasada 3.
- H2, H3 y H4 solo endurecen guardas, registro o presentación.

## Scripts y salidas (en el scratchpad; el repositorio no se tocó)

- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0569ece5-0795-4f06-b915-ded47d71f2a8/scratchpad/detractor_r6/cadena_chunk.R` y su salida `cadena_chunk_r6.txt` en la misma carpeta: generador real, N = 100.
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0569ece5-0795-4f06-b915-ded47d71f2a8/scratchpad/detractor_r6/canales.R`, con las salidas `canales_r6_50k.txt` y `canales_r5_50k.txt`: depuración declarada, M = 50 000.
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0569ece5-0795-4f06-b915-ded47d71f2a8/scratchpad/detractor_r6/prototipo.R` y su salida `prototipo.txt`: prototipo de pasada 3, depuración por rechazo.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:202 | a <- pick(r_resto); b <- pick(r_resto[a + r_resto >= 8 & a + r_resto <= 46]); s <- a + b
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:221 | delta <- pick(deltas); k <- d - a + delta
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/.claude/CLAUDE.md:162 | la regla compuesta «descarta la pareja que repite y su gemela de molde»: 1/2 en las no canónicas
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/verificar_espacio.R:219 | celdas_mas_juntas = { m <- which(rg == min(rg)); if (k %in% m) 1 / length(m) else 0 })
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/verificar_espacio.R:358 | if (ex_nc - 2 * se > .08) "CANAL" else if (ex_nc + 2 * se >= .08) "zona del corte (no concluyente)" else "ok"
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/salida/r6/verificar_espacio.txt:197 | zona del corte (no concluyente)
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/verificar_espacio.R | AUSENTE: repite_juntas
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/HANDOFF.md:25 | en curso al escribir esto
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/HANDOFF.md:25 | E4c (reglas que ven la tabla): fuera de 5
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:698 | ### Respuesta correcta
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS