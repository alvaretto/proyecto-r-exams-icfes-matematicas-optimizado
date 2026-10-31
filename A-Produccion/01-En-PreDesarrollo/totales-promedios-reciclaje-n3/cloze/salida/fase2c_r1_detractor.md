# Auditoría del detractor: FASE 2C, ronda 1, del CLOZE `totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1`

**Fecha:** 2026-10-31
**Artefacto:** comprobé el md5 del `.Rmd`: `96cfcf356c01326d42f10ee4c36545b7`, coincide. El SCHOICE padre da `e293d5c812b1f9790e2bb777b62de9a5`, también coincide.
**Independencia:** no escribí ni corregí ninguno de estos archivos. Trabajé en solo lectura y no edité nada del repo. Mis scripts están en `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/de2a4b7e-30f1-462f-99eb-f555afbbd5c4/scratchpad/det_r1/` (`chk.R` a `chk5.R`).
**Hallazgos:** 4 en total: 2 de severidad ALTA (diagnosticidad), 2 BAJA. Ninguno de CORRECCIÓN.

## Resumen

Las claves y las marcas están bien en todas las versiones que probé:

- Un oráculo propio, que lee el texto y la tabla mostrados sin usar las funciones del `.Rmd`, revisó 1500 semillas (158 canónicas). No encontró ninguna discrepancia en las Partes 3 a 6.
- La Solution no afirma nada falso.
- Los pesos `3|1|1|1|1|1` coinciden en el `.Rmd`, en E7 y en la guarda de exportación.
- Las cuatro firmas de L-C3 están cableadas regla por regla, con su mutante, y nunca salen como PASS.
- Repetí el verificador: exit 4 en 69 s, con la misma salida que `verificar_espacio_cloze_r3.txt`.

Sí encontré **dos canales nuevos por encima de +8 pp**. Ninguno tiene sonda en el verificador y ninguno está firmado:

- **H1 (Parte 2, léxico):** «elige la opción que contiene “total”» acierta el **100 %** en el tipo «comparar» y el **70 %** en «promedio». Es una enumeración exacta.
- **H2 (estímulo del otro colegio):** la diferencia de cursos *k*′, que se lee en la tabla sin multiplicar, predice qué grado ganó en el **80,0 %** (N = 100) y en el **82,5 %** de la canónica (exacto). También predice el veredicto de HG en el **74,0 %**. Eso decide 2 o 3 de las 4 marcas de la Parte 6.

Los dos están en las Partes 2 a 6 y se corrigen **sin tocar la Parte 1 ni el bloque trasplantado**. Según el HANDOFF, de las tres pasadas de §P7-D para esas partes queda una usada. Mientras sigan abiertos bloquean la FASE 3, por eso el veredicto es RECHAZAR.

## Hallazgos

### H1 · ALTA · DIAGNOSTICIDAD: la Parte 2 tiene un canal léxico («total» y forma verbal) que nada mide

**Qué se cuestiona:**
- Las 6 claves de la Parte 2 contienen la palabra «total» (líneas 677-679 y 688-690).
- En el tipo «comparar», ninguno de los 5 distractores la contiene. En el tipo «promedio», solo uno («Lo que recogió el grado en total.»).
- Las Partes 3 y 4, que se muestran a la vez, preguntan «¿Cuántos kilos … recogió **en total** el grado…?» (línea 708).
- El verificador solo sondea el largo de la Parte 2 (líneas 291-304). Mide esa familia contra +8 pp gap a gap, pero no tiene la familia léxica.

**Evidencia:** enumeración exacta, 3 claves × 10 tríos de distractores por tipo (espacio finito; excepción declarada de la regla #23).

| Tipo | Regla | Acierto | Exceso sobre 25 % |
|---|---|---|---|
| comparar | contiene «total» | **100,0 %** | **+75,0 pp** |
| comparar | más palabras en común con las Partes 3 a 5 | 70,0 % | +45,0 pp |
| comparar | más palabras en común con su propia pregunta | 66,7 % | +41,7 pp |
| comparar | más palabras con la clave de la Parte 1 (canónica) | 52,2 % | +27,2 pp |
| comparar | más palabras con las opciones de la Parte 1 (canónica) | 45,8 % | +20,8 pp |
| comparar | descarta las que empiezan por «Únicamente» | 37,5 % | +12,5 pp |
| promedio | contiene «total» | **70,0 %** | **+45,0 pp** |
| promedio | descarta las que empiezan por «Lo que recogió » | 70,0 % | +45,0 pp |
| promedio | condicional en -ría («recogería», «tocaría») | 50,0 % | +25,0 pp |
| promedio | más palabras con la clave de la Parte 1 (canónica) | 50,0 % | +25,0 pp |
| promedio | más palabras en común con su propia pregunta | 45,8 % | +20,8 pp |

- La tabla trae todas las reglas que, en cada estrato, pasan de +8 pp.
- Con los dos tipos al 50 %, «contiene “total”» acierta el **85,0 % (+60 pp)**.
- No es un artefacto de selección. Era la hipótesis previa, y para esa sola regla el techo nulo es 25 %, porque en «comparar» hay exactamente una opción con «total».
- Es la pista que la literatura prohíbe: «clang associations», es decir, palabras del enunciado repetidas en la clave, y falta de homogeneidad gramatical. Fuente (nivel 1): Haladyna, Downing y Rodriguez (2002), *Applied Measurement in Education* 15(3), 309-333, guía «Avoid giving clues to the right answer», doi:10.1207/S15324818AME1503_5.

**Riesgo:** el gap vale 1/8 de la pregunta. Quien empareja la palabra «total», que las Partes 3 y 4 ponen a la vista, acierta sin haber entendido qué indica un promedio ni qué se compara. Así la Parte 2 deja de distinguir la concepción errónea («Únicamente el promedio…») de la correcta.

**Alternativa propuesta (sin tocar la Parte 1):**
1. Equilibrar «total»: por tipo, al menos dos distractores lo usan mal. Por ejemplo, «El total de cursos de los dos grados.» o «La suma de los dos promedios, tomada como el total.». Y una o dos claves lo evitan, por ejemplo «Lo que reunió un grado entre sus cursos: la cantidad de cursos por el promedio.».
2. Homogeneizar la forma verbal en «promedio»: no todos los distractores empiezan por «Lo que recogió», y las claves no son las únicas en condicional.
3. Añadir a E3 una batería léxica de la Parte 2 por tipo, congelada antes de medir (§P7-C), con `bateria_eliminacion.R` (las seis familias más el techo nulo) y un mutante que revierta el equilibrio.
4. Cambiar los textos de la Parte 2 mueve X5, X6 y X10 en la canónica. Hay que volver a correr `enumerar_canonica_cloze.R`. Si X6 o X10 pasan de +16,7 pp, vuelve al profesor (L-C3).

**Veredicto de la objeción:** MODIFICAR (pasada 2 de §P7-D en las Partes 2 a 6).

### H2 · ALTA · DIAGNOSTICIDAD: la tabla del otro colegio delata quién ganó y el veredicto de HG sin calcular

**Qué se cuestiona:**
- La rama del otro colegio y la combinación de veredictos se sortean primero. Después, la fila se toma uniforme dentro de un espacio filtrado (líneas 581-591 y 636).
- Las condiciones de ese espacio (*D*′ ≥ 6, *D*′ ≤ 0,3 *T*′_P, δ′ de 2 a 15, *k*′ ≤ 3) concentran la rama CURSOS en *k*′ ≥ 2 y la rama PROMEDIO en *k*′ = 1.
- E3 comprueba solo las tasas **marginales** («50 % ± 12», línea 279) y la fila del ganador. No comprueba ningún rasgo **observable** de la tabla contra el veredicto. Es el punto ciego conocido: la batería no ve el estímulo.

**Evidencia:**
- **N = 100** (semillas 1-100, regla #23):
  - R_k, «si *k*′ ≥ 2 ganó el grado de más cursos; si *k*′ = 1, el de mayor promedio»: **80,0 %** frente al 50 % del azar (**+30,0 pp**). Por rama: CURSOS 77,4 % (n = 53), PROMEDIO 83,0 % (n = 47).
  - R_δ, «si δ′ ≤ 4 ganó el de más cursos»: se aplica en 14 versiones y acierta el 100 %.
  - R_hg, «HG es verdadera si y solo si *k*′ = 1»: **74,0 %** (+24,0 pp). P(hg | *k*′ = 1) = 72,5 % con n = 51; P(hg | *k*′ ≥ 2) = 24,5 % con n = 49.
- **Canónica, exacto** (rama 1/2 × combinación 1/3 × fila uniforme, con la Parte 1 fija): R_k **82,5 %**, R_hg **73,3 %**; R_δ se aplica en el 14,9 % y acierta el 99,6 %.
- **Análisis estratificado** (1500 semillas, porque el estrato *k*′ = 3 tiene n = 19 con N = 100 y no concluye):

  | | CURSOS | PROMEDIO | P(hg) |
  |---|---|---|---|
  | *k*′ = 1 | 148 | 692 | 72,4 % |
  | *k*′ = 2 | 370 | 99 | 32,8 % |
  | *k*′ = 3 | 189 | 2 | 5,2 % |

  Si se elige el mejor patrón completo de la Parte 6 en cada celda observable (*k*′ × tipo de la afirmación de ventaja), se acierta la Parte 6 entera en el **56,9 %**. Con solo conocer el diseño, sin el estímulo, sería el 34,1 %.
- **Alcance:** la afirmación de ventaja y «Ganó el concurso el grado X» comparten la misma proposición (ver «Verificado sin hallazgo»). R_k decide entonces **2 de las 4 marcas** de la Parte 6, y R_hg una tercera.
- **Fuente (nivel 1 local):** es el mismo mecanismo que L-3 prohíbe para la Parte 1, «quien ve varias versiones aprende que… y descarta sin calcular» (`.claude/CLAUDE.md` del subproyecto, L-3 «Por qué»). Además, L-3 se declaró endurecida en las Partes 2 a 6, «sin dominancia en el caso paralelo». R_δ es una dominancia de hecho en el 14 % de las versiones. Aplica también la regla #22 §P4: el validador por valor da PASS aunque la clave sea predecible por un rasgo de la escena.

**Riesgo:** «el veredicto se sortea primero» (L-C1, guarda MC13) fija la marginal, pero no impide que la tabla lo delate. La Parte 6 (1/8) se resuelve en buena parte sin calcular *D*′, y eso anula la finalidad que F-4 le da.

**Alternativa propuesta (sin tocar la Parte 1):**
1. Tras sortear la rama y la combinación, sortear *k*′ (y una franja de δ′) con la misma distribución en las dos ramas. Otra opción es ponderar las filas por 1/P(*k*′, franja δ′ | rama, combinación). Después, la fila se toma uniforme dentro de la celda.
2. Hay un acople estructural: en CURSOS con *k*′ = 1, HG es siempre verdadera, porque *D*′ = *a*′_G − *n*′_P·δ′ < *a*′_G. Por eso hay que enumerar antes el espacio (§P7-F). Si no existe un reparto que deje R_k, R_δ y R_hg en ≤ +8 pp por rama, el canal es estructural y va al profesor como override firmado, con estas cifras (H-5).
3. Guarda nueva en E3: R_k, R_δ, R_hg y R_hp por rama del otro colegio (N = 100) y exactas en la canónica. Mutante: volver a la fila uniforme, que debe quedar PENDIENTE.
4. X6 a X11 de la canónica no dependen de la fila, así que no deberían moverse. X1 a X4 sí, y hay que volver a enumerarlas.

**Nota sobre la Parte 1:** allí hay el mismo predictor (R_k = 74,4 % en las 887 no canónicas de 1000 semillas). Pero queda **dominado** por S_U, que ya identifica a la sede ganadora en el 100 % y está firmado en «O-1 corregido». No hay canal nuevo en la Parte 1. Lo dejo anotado para el próximo ciclo del SCHOICE.

**Veredicto de la objeción:** MODIFICAR, o firmar como override si §P7-F muestra que es estructural.

### H3 · BAJA · OTRO (coherencia de la Solution): encabezado que no corresponde en el tipo «comparar»

- `encabezado("Parte 2 · Lecturas del promedio que no sirven")` es fijo (línea 1012).
- En el tipo «comparar» (743 de 743 versiones), los distractores que lista no son lecturas del promedio: «Únicamente la cantidad de cursos…», «Lo que recogió el curso que más recogió en un grado».
- No afirma nada falso sobre los datos, pero rotula mal el error.
- **Propuesta:** encabezado según el tipo, por ejemplo «Parte 2 · Comparaciones que no deciden el concurso» para «comparar». No toca la Parte 1.

### H4 · BAJA · OTRO (robustez de la puerta L-C3): la enumeración reconoce los sorteos por el texto de la llamada

- `enumerar_canonica_cloze.R` dice: «Cualquier otro sorteo después del marcador del bloque P1 hace fallar la hoja» (línea 24).
- Pero `libre()` solo comprueba que el texto de la llamada esté en `SORTEOS_LIBRES` (línea 63). Un sorteo nuevo que decida una regla y repita el texto de uno libre pasaría como libre: por ejemplo, reescribir `v_c` como `sample.int(2L, 1L) == 1L`, que es el texto de `gi_par` y `m3`. Ese sorteo no se integraría.
- La «cifra exacta» quedaría mal sin ningún error, y el mutante correspondiente no moriría.
- Hoy el código no tiene ese caso, y la lógica de las 192 hojas es correcta (ver abajo).
- **Propuesta:** fijar en cada hoja cuántas veces aparece cada sorteo libre (`sample.int(2L, 1L)` ×2, `sample.int(4L)` ×2, el resto ×1) y fallar ante cualquier desvío. Otra opción es identificar el sorteo por la variable a la que se asigna.

## Verificado sin hallazgo

- **CORRECCIÓN, Partes 3 a 6:** 0 discrepancias de 1500 contra mi oráculo. Leí *n*′ y *a*′ de la tabla mostrada y evalué cada redacción con mi propia lectura:
  - HP: (*n*′_P + 1)·*a*′_P > *T*′_G.
  - HG: (*n*′_G − 1)·*a*′_G < *T*′_P.
  - Las cuatro de ventaja con su igualación.
  - El ganador.

  No hay empates posibles: *a*′ ≠ *D*′, δ′ ≥ 2 y *k*′ ≥ 1.
- **Lecturas alternativas:**
  - «con un curso más», «le hubiera faltado un curso» y «con tantos cursos como» solo admiten la lectura del código.
  - La afirmación de ventaja y la del ganador son lógicamente equivalentes dado lo que se ve. Con ventaja «por cursos», «Ganó…» nombra siempre al grado de más cursos; con ventaja «por promedio», al de mayor promedio. No forman un par complementario y no delatan el veredicto por sí solas: P(verdadera | tipo) = 50 %. El problema es el estímulo (H2).
- **Retroalimentación alineada:** R/exams 2.4.2 amplía la lista de preguntas a 15 entradas con `""` para los gaps numéricos. Así se cumple `ssol`, y la lista de la Solution se permuta junto con las opciones (lo comprobé en `read_exercise`). El contrato N ≠ M no es un bug.
- **Solution:** sin letras A-D, 0 de 1500. La Parte 6 se agrupa por veredicto, sin enumerar en orden. Las Partes 3 a 5 coinciden con la tabla. «solo si» es verdadero. No encontré prosa falsa.
- **Espacios duros (U+00A0):**
  - 0 en las 211 500 líneas de la Parte 1 de la Solution.
  - 0 casos de cifra + espacio normal + unidad en las 24 012 líneas de las Partes 2 a 6.
  - 0 en la tabla del otro colegio y 0 en las retroalimentaciones de las Partes 3 a 6 con espacio normal.
  - La tabla de la Parte 1 trae 2 espacios duros en el 14,8 % de las versiones, heredados del SCHOICE (E1 lo prueba). El CLOZE no añade ninguno.
- **Pesos (F-3):** `expoints: 3|1|1|1|1|1` en el `.Rmd`. E7 tiene 4 mutantes muertos. La guarda de `exportar_moodle_cloze.R` exige `all(pesos == "3|1|1|1|1|1")` en cada pregunta. En el XML del aula los gaps salen como 3, 1, 1, 1, 1, 1, sin barras invertidas dentro de los gaps.
- **L-C3:**
  - `FIRMA_CANON` lleva 4 cifras regla por regla y compara con `round(…, 1)`.
  - Dentro de lo firmado, «cubierto» (exit 4). Por encima, o con cualquier otra regla > +8 pp, «pendiente» (exit 3).
  - MC18 a MC21 sacan cada uno su regla de «cubierto», cazados por su sonda.
  - Las 13 cifras de «O-1 corregido» se cablean en E4. La de S_U usa `0.167 + 1e-9`, más estricta que el redondeo firmado; no es un defecto.
- **Enumeración de la canónica:** son 192 hojas = 2 ramas × 6 órdenes de las combinaciones × 2 tipos × 3 claves × (2 redacciones de ventaja, o 2 × 2 cuando las hipótesis difieren). Masa 1, sin errores, igual a la transcripción e invariante entre semillas. X1 a X4 y X5 se integran exactamente, y los sorteos libres restantes no afectan a ninguna regla. Limitación en H4.
- **Batería cruzada por estratos observables** (no canónicas, n = 1066): X6, X8, X9, X10, X11 y S_U por rama del otro colegio y por tipo de la Parte 2, todos a ±4,5 pp de su base. La mejor regla en cada celda observable da +4,4 pp (CURSOS) y +2,8 pp (PROMEDIO) sobre S_U. Sin canal nuevo.
- **`exextra`:** coinciden literalmente con `ITEM_OFICIAL.md`. Nivel 3 con DOK 2 es válido (`DOK ≥ 3 ⇒ Nivel ≥ 3`).
- **Ortografía:** el corrector del repo no encuentra errores. Lo leí además a mano en las Partes 2 a 6.

## No auditado (declarado)

- Revisión visual de PDF, HTML y DOCX: no volví a mirar las capturas.
- Los mutantes MC1 a MC16, uno por uno: solo leí la salida de MC17 a MC21.
- Re-derivación propia de E1: confío en el OK del verificador.
- Lectura línea a línea de `x_canonica_exacta.R`: solo vi su coincidencia con la enumeración por código.

## Veredicto global: RECHAZAR

No hay ningún defecto de CORRECCIÓN. Rechazo por H1 y H2: dos canales de diagnosticidad nuevos, muy por encima de +8 pp (§P7-A), en familias sin sonda. Bloquean la FASE 3 hasta que se corrijan en la pasada 2 de §P7-D de las Partes 2 a 6, o hasta que el profesor los firme con estas cifras si §P7-F muestra que H2 es estructural. Ninguna corrección toca la Parte 1 ni el bloque P1. Después de corregir: volver a correr `verificar_espacio_cloze.R` y la enumeración de la canónica, y hacer una FASE 2C nueva.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd:688 | El total de un grado, que es la cantidad de cursos
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd:678 | El total del grado repartido por igual entre sus cursos.
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd:708 | recogió en total el grado
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/verificar_espacio_cloze.R:291 | Parte 2: canal de largo
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/HANDOFF.md:117 | aplicarla gap a gap no está calibrado para CLOZE
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd:636 | fp <- ep[sample.int(nrow(ep), 1L), ]
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd:586 | g$delta >= 2 & g$delta <= 15 & g$D >= 6
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/verificar_espacio_cloze.R:276 | ninguna plantilla con veredicto fijo
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/.claude/CLAUDE.md:354 | E3: hp y hg verdaderas en el 50
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/.claude/CLAUDE.md:256 | sin dominancia en el caso paralelo
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd:1012 | Lecturas del promedio que no sirven
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd:693 | Únicamente la cantidad de cursos de los dos grados
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/enumerar_canonica_cloze.R:24 | Cualquier otro sorteo después del marcador del bloque P1 hace fallar la hoja
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/enumerar_canonica_cloze.R:63 | sorteo no reconocido después del bloque P1
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: RECHAZAR
