# Auditoría del detractor, FASE 2C (r5): `probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd`

**Fecha:** 2026-10-27 · **Versión auditada:** md5 `afc260eb8f2d1e29ae581978750a08b6` (comprobado en disco) · **Detractor nuevo:** no escribí, no corregí ni audité versiones previas.
**Hallazgos:** 4 en total: 2 ALTA (diagnosticidad), 2 BAJA (registro y guardas). **Ninguno es de CORRECCIÓN.**

## Resumen

- **Corrección: sin objeciones.** La clave es única y verdadera, tanto en la canónica como en las no canónicas.
  - E1 lo comprueba de forma exacta sobre 3.982.576 combinaciones.
  - Mi barrido de 101 versiones (100 no canónicas + la canónica) da exactamente una línea «Son iguales» por versión, 0 fracciones n/N sin «extracción/extraída», 0 referencias a letras y 0 glifos prohibidos emitidos.
  - El fuente solo usa estos caracteres no ASCII: `— ¿ … « » ° á é í ñ ó ú`.
  - Leí `explica_sim`, `explica_una`, `explica_cer` y `explica_cru` en cada rama: sus afirmaciones son verdaderas por construcción (`k` posicional, |Δ| = 1, espejo desigual).
- **La reapertura 7 cumple lo que prometía.** La regla base «descarta la pareja que repite» da exactamente 1/3. Pero **cerrar ese canal dejó otros dos en el estímulo**, y la batería §P7 no puede verlos porque no mira la tabla:
  - **H1 (estructural):** la misma señal que se firmó a 1/3 llega al **50 %** en todas las no canónicas si se encadena un segundo paso.
  - **H2 (paramétrico):** las celdas de la clave nunca salen de 5..20, mientras que las demás llegan a 3..24. «Descarta las parejas con algún número < 5 o > 20» nunca elimina la clave.

## Estado de lo señalado por r4

| r4 | Estado | Evidencia |
|---|---|---|
| H1, canal «la que repite» 66,4 % | **Resuelto para esa regla**: 1/3 exacto (E1: `repite_fuera_de_UNA = 0`). Sigue la vía compuesta (H1 nuevo) | `salida/r5/verificar_espacio.txt:21`. Mi réplica: 33,3 % con N=100 y con N=2000 |
| H2, espejo también trampa de cruce | **Resuelto**: el espejo ya no repite entre tipos y PROB-CRU no sale fuera de la canónica | Residuo menor: el espejo difiere en 1 ficha en 1/100 versiones y PROB-UNA en 7/100. Son «cercanas» que la Solution no menciona. No es falso, y no lo eleva a hallazgo |
| H3, «en cada extracción» sin guarda | **Resuelto** | Guarda en líneas 402-404 (feedback y análisis). El texto estático del chunk `solucion` lo cubre E3. Barrido: 0 excepciones |
| H4, HANDOFF atrasado | **Parcial** | Ver H3 nuevo |

El diff r4 → r5 no toca las zonas de los hallazgos de r1-r3 (textos de celda suelta, guarda léxica, sumas de control), salvo la línea de estrategia 752, que revisé: es verdadera como enunciado general.

## Hallazgos

| ID | Severidad | Tipo | ¿Lo ve o explota el estudiante? | Resumen |
|---|---|---|---|---|
| H1 | **ALTA** | DIAGNOSTICIDAD, +25,0 pp no canónica / +20,0 pp mezcla, estructural | Sí | La copia de PROB-UNA siempre une una celda de la clave con una del espejo, y la trampa siempre comparte molde con la otra trampa: 50 % exacto |
| H2 | **ALTA** | DIAGNOSTICIDAD, +18,2 pp (N=100) / +21,8 pp (depuración), paramétrico | Sí | `r_clave <- 5:20` frente a `r_resto <- 3:24`: la clave nunca contiene un 3, 4 ni 21-24 |
| H3 | BAJA | Registro | No | I-8 dice «+7,1 medidos, bajo el corte de +8», pero r5 mide +8,0. I-8 todavía describe PROB-CRU agregada. Las firmas llaman «canal de estímulo» a una sola regla |
| H4 | BAJA | Guardas y verificador | No | E1 no cuenta como aborto un conjunto de elección vacío. Hay una guarda duplicada que no puede fallar |

### H1. La señal firmada a 1/3 llega a 1/2 con un segundo paso (ALTA, DIAGNOSTICIDAD, estructural)

**Mecanismo (leído del código):**
- PROB-UNA copia siempre una celda de la clave en una celda del espejo: `e <- d`, `h <- a`, `f <- cc` o `g <- b` (líneas 212, 214, 227, 229).
- Además, P y Q comparten molde `{X,Y}`, y la clave y el espejo comparten `{Y,Z}` (líneas 307-310).
- El molde se lee en el texto de cada opción: «roja con un triángulo», «de categoría oro», «de categoría oro con un…».

Hay dos rutas equivalentes:
- **Regla compuesta «repite + su molde»:** descarta la pareja que repite **y la otra pareja redactada igual**. Quedan siempre la clave y el espejo.
- **Regla «una sola copia»:** quédate con las parejas que contienen **una sola** de las dos celdas iguales. Son la clave y el espejo, salvo duplicados casuales.

**Medición** (`canales.R`; N=100 estándar, depuración N=2000 declarada):

| Regla | No canónica N=100 | Depuración N=2000 | Canónica | Mezcla 10 % |
|---|---|---|---|---|
| Base «descarta la que repite» (firmada) | 33,3 % | 33,3 % | 33,3 % | 33,3 % (+8,3) |
| Compuesta «repite + su molde» | **50,0 %** (en cada versión, por construcción) | 50,0 % | 0 % | **45,0 % (+20,0)** |
| «Una sola copia» | 44,0 % | 42,9 % [42,6; 43,3] | 0 % | 38,6 % (+13,6) |

**Por qué es estructural (§P7-F):**
- Una repetición *posicional* dentro de P o de Q siempre empareja una celda de la clave (a o d; b o cc) con una libre.
- La única forma de que la pareja de molde de la trampa sea a veces la clave es poner la trampa UNA en el espejo. Eso exige un tercer error que no repita para P/Q, y el pool actual no lo tiene (PROB-CRU repite por definición).

**Lo que firmó el profesor no cubre esto.** La firma dice 1/3. El registro presenta esa cifra como «el canal de estímulo» (E1: «acierta 1/3»; CLAUDE.md:158: «el mismo nivel del ítem oficial»), pero esa es solo la regla base. Hay además una tensión pedagógica (no es falsedad): la Estrategia dice «Una celda que se repite no decide nada, ni a favor ni en contra», y en este generador la pareja que repite es la trampa en el 100 % de las versiones no canónicas.

**Alternativas:**
- (a) **Firma explícita y ampliada (H-5, humana):** «residuo estructural de la señal de repetición: hasta 1/2 en no canónicas (+25 pp), 45 % en la mezcla (+20 pp)». Debe constar que, a diferencia del 1/3, supera el corte de +8. Plausibilidad: la regla compuesta encadena un paso de meta-lectura entre versiones. Si el profesor juzga que ningún estudiante lo hace, este es el camino coherente con §P7-F.
- (b) **Rediseño** (pasada 2 de 3 de §P7-D):
  - Con p = 1/2, la trampa UNA va en el espejo (`e == g` o `f == h`), diagnosticada como SIM + UNA, como pidió r4 en su H2.
  - P/Q llevan PROB-CER y un error nuevo que no repita.
  - Con eso las reglas compuesta y de una sola copia bajan a ≈ 25 % y la base sigue en 1/3.
  - Requiere ampliar el pool y volver a enumerar en E1.

### H2. Asimetría de rango: la clave nunca contiene valores extremos (ALTA, DIAGNOSTICIDAD, no estructural)

**Mecanismo.** `r_clave <- 5:20` y `a_ok` encierran las cuatro celdas de la clave en 5..20. Las celdas libres salen de `r_resto <- 3:24`. Medido en la depuración N=2000:
- Celdas de la clave: media 12,17, sd 4,42, **0 %** fuera de 5..20.
- Celdas libres: media 13,35, sd 5,87, **20,5 %** fuera.

**Reglas sobre la tabla y las opciones:**

| Regla | N=100 | Depuración N=2000 [IC 95 %] | Canónica | Mezcla |
|---|---|---|---|---|
| «Descarta parejas con algún número < 5 o > 20» (nunca elimina la clave) | 43,2 % (+18,2) | 46,8 % [45,8; 47,8] (+21,8) | 25 % | +19,6 |
| «La pareja cuyas 4 celdas están más juntas» | 41,3 % (+16,3) | 46,0 % (+21,0) | 0 % | +16,4 |
| «Descarta las parejas con el número mayor de la tabla» | 36,5 % (+11,5) | 35,4 % [34,4; 36,5] (+10,4) | 50 % | +11,9 |
| «… con el mayor o el menor» | 29,2 % (+4,2) | 35,3 % (+10,3) | 50 % | +11,7 |

Son hipótesis a priori, sacadas del código y no del dato. El nulo atómico de una regla de descarte con la clave permutada es exactamente 25 %. Aun así probé 13 reglas: con N=100, el exceso de «número mayor» queda a menos de 5 pp del corte (frontera). Las reglas de extremos y de celdas juntas superan el corte con margen de +8 a +14 pp.

La canónica también da 50 % con «número mayor»: el 20 del impreso no está en B. Pero una sola instancia no es una vara de población. La vara del repo es +5,3 pp (§P7-A).

**Alternativa (paramétrica, sin ampliar el pool):** que las celdas de la clave y las libres salgan del mismo conjunto. Por ejemplo:
- `r_clave <- 3:24`, con `s` y `a_ok` ajustados para que |d − a| ≤ 15 y |b − cc| ≤ 15.
- Así los conjuntos desplazados de PROB-CER (22 − 16 − 4 exclusiones) conservan al menos 2 valores.

Después, volver a enumerar en E1, contando también los conjuntos vacíos (H4), y medir las cuatro reglas de la tabla con N=100. Objetivo: ≤ +5,3 pp en las plausibles («número mayor» y «mayor o menor»). Añadirlas al verificador como E4c; §P7-C exige volver a medir el histórico si se amplía la batería.

### H3. Registro desfasado (BAJA)

- En `.claude/CLAUDE.md:141`, I-8 dice «+7,1 medidos, bajo el corte de +8». En r5, la mezcla de `cruzada_en_su_molde` da **+8,0** (`verificar_espacio.txt:172`), justo en el corte. La firma es por alcance (P_CANON ≤ 0,10) y se cumple, pero la frase ya no es cierta.
- I-8 (líneas 129-130) sigue describiendo trampas agregadas «cuyas celdas cruzadas coinciden (cruce sin sumar)», lo que contradice el I-6 nuevo (PROB-CRU solo en la canónica).
- HANDOFF: la fila r5 dice «en curso al escribir esto», y la tabla «Estado» describe r3. Lo declara con una nota, pero sigue desfasada.
- La fila de decisiones (línea 158), el JSON y E1 llaman «canal de estímulo exacto: 1/3» al valor de una sola regla (ver H1).

**Alternativa:** actualizar las cifras y redactar «la regla base acierta 1/3; la compuesta, ver la firma o el rediseño de H1».

### H4. Guardas y verificador (BAJA)

- **E1 no ve conjuntos vacíos.** Si `EH()` o `FG()` devuelven 0 filas, el `for (i in seq_len(nrow(eh)))` se salta en silencio y E1 sigue reportando `aborto = 0`, aunque `pick()` sobre un vector vacío aborta el generador. Hoy no se alcanza (mínimo 2 elementos con los rangos actuales), pero es justo lo que cambiaría la corrección de H2. **Alternativa:** contar `nrow(eh) == 0` y `nrow(fg) == 0` como abortos.
- **Guardas que no pueden fallar:**
  - `length(errores_conceptuales) >= 4` (línea 336) repite la de la línea 170 sin que la lista cambie entre ambas.
  - `pre_sim` queda implicada por `sum(iguales) == 1`.
  - `all(rowSums(cuentas) < N)` queda implicada por `all(w >= 1)` mientras las parejas sean disjuntas. Su mutante M3 solo la caza porque cambia la estructura; eso es válido como trampa estructural, pero conviene declararlo.

## Respuestas al encargo

1. **Corrección:** sin hallazgos (ver Resumen).
   - Regla #19: la Solution dice «La respuesta correcta es la que afirma…», y los distractores se recorren con `sort(codigos_sel)`, que es el orden de los códigos y no el orden mostrado.
   - Regla #25: 0 glifos. El hook 2O también da OK.
2. **`construir_cubo` frente a EH/FG:** coinciden línea a línea. Las ANCLAS hacen fallar el enumerador si el generador cambia.
   - No hay abortos alcanzables: los conjuntos de PROB-UNA tienen ≥ 18 valores, los desplazados de PROB-CER ≥ 2, y `deltas` nunca queda vacío gracias a `m_malo` y `cc ≠ b`. 0 abortos en 2.226 semillas.
   - Comprobé a mano que cada exclusión evita exactamente una repetición o una igualdad.
   - **Sesgos de las exclusiones:** en posiciones y moldes no hay (E2: 75/60/65). Las celdas iguales dentro de un mismo tipo no favorecen a la clave (23,5 %). La magnitud sí está sesgada (H2), y la vinculación copia–clave también (H1).
3. **I-1…I-8:**
   - Todas las guardas nuevas pueden fallar: `repite` (M21), pool antes del sorteo (M8/M22) y «extracción» (M20).
   - Las tautológicas están en H4.
   - La sonda de M21 prueba lo que dice: quitar `h` hace que el espejo repita y salta `repite`.
4. **Registro:** las firmas del profesor coinciden con el código (P_CANON 0,10, PROB-CRU solo en la canónica, C → PROB-CER, 96,8 %). Las cifras desfasadas están en H3.
5. **Severidad:**
   - H1 y H2 son DIAGNOSTICIDAD por encima de +8 pp y afectan lo que el estudiante puede explotar. Bloquean la FASE 3 hasta que se corrijan o el profesor las firme.
   - §P7-D: la reapertura 7 fue la pasada 1. Quedan 2.
   - H3 y H4 solo endurecen el registro y las guardas.

Scripts y salidas (solo lectura del repo):
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0569ece5-0795-4f06-b915-ded47d71f2a8/scratchpad/detractor_r5/canales.R`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0569ece5-0795-4f06-b915-ded47d71f2a8/scratchpad/detractor_r5/barrido.R`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0569ece5-0795-4f06-b915-ded47d71f2a8/scratchpad/detractor_r5/depuracion_n2000.txt`

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:212 | e <- d; h <- pick(setdiff(r_resto, c(a, d, d - m_malo)))
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:229 | g <- b; f <- pick(setdiff(r_resto, c(cc, b - m, b, h)))
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:308 | espejo = list(tp(c(2, 3), c(1, 1)), tp(c(2, 3), c(2, 2)))
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:309 | list(tp(c(1, 2), c(1, 1)), tp(c(1, 2), c(2, 2)))
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:752 | Una celda que se repite no decide nada, ni a favor ni en contra
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/salida/r5/verificar_espacio.txt:21 | la regla «descarta la que repite» acierta 1/3 (+8,3 pp)
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/.claude/CLAUDE.md:158 | el mismo nivel del ítem oficial
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:196 | r_clave <- 5:20
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:197 | r_resto <- 3:24
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:199 | a_ok <- r_clave[r_clave >= s - 20 & r_clave <= s - 5]
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/.claude/CLAUDE.md:141 | +7,5 pp esperados y +7,1 medidos, bajo el corte de +8
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/salida/r5/verificar_espacio.txt:172 | cruzada_en_su_molde (relacional)       33.0 %             +8.0
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/.claude/CLAUDE.md:130 | o cuyas celdas cruzadas coinciden (cruce sin sumar)
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/HANDOFF.md:23 | en curso al escribir esto
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/verificar_espacio.R:146 | for (i in seq_len(nrow(eh))) {
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:170 | stopifnot(length(errores_conceptuales) >= 4)
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:336 | length(errores_conceptuales) >= 4,
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS