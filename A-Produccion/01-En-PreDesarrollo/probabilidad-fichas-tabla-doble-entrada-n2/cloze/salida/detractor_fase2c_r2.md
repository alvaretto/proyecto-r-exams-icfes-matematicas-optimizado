# Auditoría detractora FASE 2C, pasada 2: CLOZE `probabilidad-fichas-tabla-doble-entrada-n2`

**Fecha:** 2026-10-28
**Artefacto:** `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd` (versión de las 12:56)
**Independencia:** no escribí ni corregí el ejercicio y no hice la pasada 1.
**Hallazgos:** 2. Ninguno es crítico ni alto; hay 1 de severidad media y 1 baja. Ninguno es de CORRECCIÓN.

## Resumen ejecutivo

Las seis claves son correctas y ningún gap admite una segunda clave. Lo comprobé con un analizador propio que lee el evento del texto visible y cuenta en la tabla, sobre 300 versiones: 100 canónicas, 100 con la dimensión D nombrada en las 4 opciones y 100 con D en 2 opciones. Resultado: 0 discrepancias. Las marcas de `exsolution` y del XML de Moodle coinciden con esa verdad.

R3 se cumple, verificado por diff. Las correcciones de la pasada 1 están bien aplicadas y no abren ningún defecto nuevo.

También busqué si la dimensión de las Partes 2 a 6 amplifica algún residuo firmado de la Parte 1. La regla cruzada con mayor tasa en el verificador era «celdas más juntas entre las opciones que no nombran D»: 35,6 %, es decir, +10,6 pp sobre el azar, con n = 300. Una medición de depuración con N = 4000 da 31,7 %. El incremento sobre su base firmada es −1,1 pp (error típico 0,7) y el exceso en la mezcla, +7,0 pp. La cifra de 35,6 % era ruido de muestreo: no hay canal nuevo.

Los dos hallazgos son de proceso y de texto:
- **H1:** el paso de renderizado se selló sin inspeccionar la versión corregida. El DOCX tiene los marcadores `##ANSWERi##` literales y la FASE 2B no generó preview.
- **H2:** la Solution afirma una frecuencia («suele») sin evidencia.

## Lo que verifiqué por mi cuenta

1. **R3 por diff.** Comparé el chunk del SCHOICE (líneas 1-679) con las líneas 1-686 del CLOZE. Solo difieren en tres cosas:
   - las 6 líneas de comentario del encabezado;
   - los renombres `opciones→opciones_p1`, `sol→sol_p1` y `feedback→feedback_p1`, y sus usos en `respuesta_correcta` y `txt_frac`;
   - el marcador de fin de bloque.

   Todas las líneas de guarda son idénticas: `P_CANON <= 0.10`, canónica verbatim (28217, 306 y 4314626), `nchar(pregunta) == 123L`, `rowSums(cuentas) < N`, `sum(iguales) == 1`, PROB-CRU solo en la canónica y opciones agregadas fuera de ella. Delante del bloque no hay nada que consuma el RNG.

2. **Verdad independiente** (script en `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/ca84678d-286e-418b-a9bc-e7a8cd20624b/scratchpad/detractor_r2/audit.R`). Recorrí las semillas 1 a 879 hasta tener 100 versiones en cada estrato, sin errores de generación. En cada versión comprobé:
   - la Parte 1 desde el texto de cada opción;
   - la Parte 2 como `sum(w)`;
   - las Partes 3 y 4 a partir del evento «sacar …» del enunciado;
   - la Parte 5 como la afirmación de cada opción frente a los conteos, con exactamente una verdadera;
   - la Parte 6 con cada evento leído y el «no» convertido en N − n;
   - los 6 componentes de `exsolution`.

   **0 fallos en 300 versiones.**

3. **La Solution, en esas mismas 300 versiones:**
   - 0 apariciones de «nivel» o «dimensi».
   - Toda línea con `\frac{` de la Solution o de las 13 retroalimentaciones dice «extracci» o «ficha extraída».
   - La lista de verdaderas de «Parte 6» en «Respuestas» es exactamente el conjunto de opciones verdaderas mostradas.
   - No hay dobles espacios, espacio antes de signo, «un un» ni «de de».
   - Leí una Solution completa (figura, semilla 11) y otra parcial (categoría, semilla 1). Todas sus afirmaciones son verdaderas y las tildes están bien.

4. **R/exams 2.4.2 (`read_exercise`).** Completa el questionlist del cloze con `""` en los gaps `num`: quedan 13 entradas, igual que el solutionlist. Por eso `ssol` vale TRUE y la mezcla de opciones y retroalimentación queda alineada. Confirma lo dicho en la pasada 1.

5. **XML de Moodle**, pregunta canónica:
   - Tipos: `MULTICHOICE_V`, tres `NUMERICAL` (`=100:0`, `=59:0`, `=41:0`), `MULTICHOICE_V` y `MULTIRESPONSE`.
   - La marca `=` cae en «Una ficha blanca con un cuadrado y una ficha negra con un círculo.».
   - En la Parte 5, la marca va en «par» (59 > 41).
   - En la Parte 6, la marca va en las dos parejas complementarias y `%-50%` en las otras dos.
   - No hay retroalimentación `#` dentro de las subpreguntas, así que las llaves de `\frac` no rompen la sintaxis cloze.

6. **PDF actual** (`pdf/cloze1.pdf`, 12:57, canónica), páginas 1 y 2 a 80 ppp:
   - La tabla coincide celda a celda con I-2 (12/11, 8/15, 16/20, 10/8).
   - Se ven las comillas « » y las fracciones.
   - La Solution trae los textos ya corregidos.

7. **Estratos:**
   - D en la canónica: color 38, categoría 29, figura 33.
   - nT = 1 en la Parte 6: 42 % (canónica), 51 % (D en 2), 52 % (D en 4).
   - |n1 − n2| = 1: 0 %, 8 % y 0 %, respectivamente.
   - Las plantillas verdaderas de la Parte 6 son siempre `si1_no2` y `si2_no1`: 71 versiones solo la primera, 74 solo la segunda y 155 ambas.

8. **Diagnosticidad de la Parte 5.** Ni `paso9_diagnosticidad.txt` ni las sondas del arsenal la miden: solo aparecen p1 y p6. Por construcción no tiene canal superficial:
   - sus dos opciones usan las mismas palabras en otro orden, con el mismo largo y la misma primera palabra;
   - en las no canónicas, «el nivel que aparece primero en la tabla es el mayor» acierta exactamente el 50 %, por la simetría del sorteo `flip`;
   - en la canónica el mayor es el segundo nivel en color y figura y el primero en categoría, así que esa regla acierta 2/3 allí y solo suma +1,7 pp en la mezcla.

9. **R8 y R12: depuración de la peor regla cruzada** (`cruce.R`; la regla #23 admite N distinto de 100 en depuración). Dato del verificador con N = 100 por estrato: 37,0 % con D en 2 frente a 32,0 % de su base.

   Con N = 4000 no canónicas:

   | Regla | Tasa | Error típico |
   |---|---|---|
   | Base «celdas más juntas» | 32,8 % | 0,7 |
   | Con el filtro «no nombra D» | 31,7 % | 0,7 |
   | Incremento pareado | −1,1 pp | 0,7 |

   Por rol de D, la regla con filtro acierta 66,9 % cuando D = X, 31,3 % cuando D = Y y 0,0 % cuando D = Z. Se compensa porque D = X y D = Z salen con frecuencias parecidas (31,0 % y 33,9 %). En la mezcla, la regla cruzada llega a +7,0 pp.

   El verificador decide con «base + 3 errores típicos», el mismo criterio que la E4c del SCHOICE (detractor r6). Con n = 300, esa banda mide unos 8 pp, así que por sí sola no detecta amplificaciones menores. La depuración cierra ese hueco para la regla máxima.

10. **Las 6 advertencias de 2E y 2F del arsenal** («Tarea», tabla de metadatos, descripción de evaluación, «Es posible que los estudiantes…», `letra_correcta`, análisis de distractores) son heurísticas. La Solution sí tiene «### Análisis de los distractores», y `letra_correcta` está prohibida por la regla #19. No requieren acción.

## Requisitos del encargo

| Código | Estado | Evidencia |
|---|---|---|
| R3 | Cumple | Diff del punto 1. E1 del verificador: 100 semillas más 11 canónicas de cuota, con `.Random.seed` idéntico |
| R4 | Cumple | `schoice\|num\|num\|num\|schoice\|mchoice`, `##ANSWER1##`…`##ANSWER6##` en orden (líneas 868-898). La Parte 1 va primero porque R3 lo exige |
| R5 | Cumple | Las Partes 2 a 6 usan N y una sola dimensión. Los complementos son eventos simples de la misma bolsa; no hay condicional ni otra bolsa |
| R6 | Cumple | `exextra[Nivel]: 2` y `exextra[DOK]: 2`, con descriptor D2.3 |
| R7 | Cumple | Las opciones de las Partes 5 y 6 son afirmaciones y parejas de eventos. Guarda en las líneas 807-808 |
| R8 | Cumple hacia la Parte 1 | Sin cifras y con una sola dimensión (guardas y 300 versiones). D es independiente de los roles (E3). La peor regla cruzada se depuró en el punto 9. Entre partes solo queda el punto 3 de la pasada 1, que sigue pendiente |
| R10 | Cumple, con H2 | Respuesta por parte y por contenido. La Parte 6 se agrupa por veredicto y en un orden independiente del que se muestra. Están las 6 secciones de la regla #1. Las cifras n/N van con «en cada extracción» o «una ficha extraída» (punto 3) |
| R11 | Cumple | El diff de los `exextra` solo difiere en `Type: CLOZE`; `exshuffle: TRUE` |
| R12 | Cumple | Código byte a byte idéntico. E4 dentro de «firma + 3 errores típicos», el mismo criterio del SCHOICE. La amplificación por D se descarta en el punto 9 |
| R13 | Cumple | 23/23 mutantes cazados por su sonda; 100 por estrato; los estratos con n < 20 se marcan NO CONCLUYENTE. El estrato «otro» está vacío por construcción: la clave y el espejo comparten (Y, Z), y P y Q comparten (X, Y) |
| Clave por gap y marca en Moodle | Correctas | Puntos 2 y 5 |
| Segunda clave | No hay | Puntos 2 y 7. Las Partes 5 y 6 nunca empatan, por `dims_ok` |
| Solution verdadera | Sí, salvo H2 | Punto 3 |
| Ortografía | Correcta | Corrector limpio, lectura de dos Solutions y PDF |
| Moodle `MULTICHOICE_V` | Correcto | Punto 5; `f4_moodle_xml.txt`: 65/65 |

## Estado de los hallazgos de la pasada 1

- **Punto 1, jerga «nivel» y «dimensión».** Corregido: 0 apariciones en 300 versiones. La minúscula tras «En las Partes 2 a 6,» también está bien.
- **Punto 2, «esas dos parejas».** Corregido. El texto nombra las dos parejas por evento y es verdadero tanto con nT = 1 como con nT = 2.
- **Punto 3, la Parte 5 presupone el veredicto de tres plantillas de la Parte 6.** Sin cambios: el veredicto de cada plantilla sigue siendo invariante en el 100 % de los casos. Añado un matiz: la Parte 6 se resuelve sin mirar la tabla, solo con la lógica del complemento y n1 ≠ n2, que es justo lo que delatan las opciones de la Parte 5. Sigue pendiente de decisión del profesor, sin declarar en el HANDOFF ni en el `.claude/CLAUDE.md` local.
- **Punto 4, mutantes.** Corregido: M19 a M23 se añadieron y los 23 se cazan.
- **Punto 5, «casos posibles en cada extracción».** Corregido en las Partes 2 a 4 y en la Solution, que ahora dicen «al extraer una ficha». Las Partes 5 y 6 conservan «En cada extracción», que es la redacción firmada en I-7 y es verdadera en la lectura marginal. La lectura condicional podría empatar solo con |n1 − n2| = 1 (8/100 con D en 2, unos 4,6 % de la mezcla). No obliga a nada; cambiarlo sería endurecer.
- **Punto 6, el PDF muestra las opciones en línea.** Sigue igual (página 1) y es declarativo; falta declararlo.
- **Punto 7, `WARN_DIV_BAJA` en la Parte 5 (22/100).** Sigue igual y es declarativo; falta declararlo.
- **Punto 8, los gaps pesan lo mismo.** Pendiente del profesor; sigue sin `expoints`.

## Hallazgos

### H1. El renderizado se selló sin inspeccionar la versión corregida: DOCX con marcadores literales y FASE 2B sin preview

- **Dominio:** visual y testing. **Severidad:** MEDIA. No es de CORRECCIÓN y no bloquea la FASE 3 por sí solo.
- **Qué se cuestiona:** `renderizado_4_formatos` registra `"docx": "OK"`, y la nota del arsenal enumera las fases aprobadas sin decir que la 2B no corrió.
- **Por qué** (regla #6, `ciclo-validacion.md`, fuente local de nivel 1): «tras CUALQUIER cambio a un `.Rmd`: re-renderizar, mostrar preview…». Prohíbe «Validación ciega ("el PDF se generó") o marcar completado sin inspección visual real». Además, `codigo-rmd.md` prohíbe omitir alguno de los 4 formatos.
- **Evidencia:**
  - **DOCX.** Convertí `cloze/salida/docx/cloze1.docx` (12:57) a texto con pandoc. Muestra los seis marcadores `##ANSWER1##`…`##ANSWER6##` literales. Las opciones de las Partes 1, 5 y 6 quedan al final de la pregunta como (a), (b) y (c), unidas con « / » y separadas de su parte. No es un defecto introducido aquí: los DOCX cloze de `excedente-almuerzo-proporcional-n4` y `migraciones-exteriores-lineas-n2` también tienen 6 marcadores literales cada uno. Es una limitación de `exams2pandoc` 2.4.2 compartida por todo el repo. Aun así, el estado la registra como OK y no la declara.
  - **FASE 2B.** El informe del hook de las 13:07 dice «PDF no encontrado»: corrió con cwd = `cloze/salida` y los PDF están en `pdf/`. Los únicos previews son `preview/pdf_canon-1..5.png` (12:27) y `preview_…-1.png` (12:31). Los dos son anteriores al `.Rmd` corregido (12:56).
  - Yo inspeccioné las páginas 1 y 2 de `pdf/cloze1.pdf` y no vi defectos. Nadie ha mirado todavía, para esta versión:
    - las páginas 3 a 5: análisis de distractores, análisis de las Partes 5 y 6, Propiedades, Caso específico, Reflexión y Estrategia, que es donde cayeron las correcciones;
    - los PDF no canónicos.
- **Riesgo concreto:** bajo para la corrección. Lo que está en juego es la trazabilidad: el estado afirma una inspección que no consta en ningún artefacto.
- **Alternativa:**
  1. Generar las imágenes de `pdf/cloze1..3.pdf` (por ejemplo, con `pdftoppm -r 80`), revisar todas las páginas con Read antes de sellar `coherencias_5` y anotar la hora en el estado.
  2. Declarar en el estado y en el HANDOFF: «DOCX del cloze: no apto para estudiantes (marcadores `##ANSWERi##` literales; limitación de exams2pandoc 2.4.2 compartida con los demás cloze del repo)». Es la misma vía que el punto 6 de la pasada 1 para el PDF. Si hace falta una versión imprimible, usar el SCHOICE.
- **Veredicto:** MODIFICAR (proceso). No toca el `.Rmd`.

### H2. «suele venir de una suma mal hecha»: una frecuencia afirmada sin evidencia en el análisis de la Parte 5

- **Dominio:** pedagógico y semántico (verdad de la Solution). **Severidad:** BAJA.
- **Qué se cuestiona:** la línea 960, «La afirmación contraria suele venir de una suma mal hecha en las Partes 3 o 4». Es texto fijo y sale en el 100 % de las versiones.
- **Por qué:** el encargo exige que lo que afirma la Solution sea verdadero, y «suele» es una afirmación de frecuencia. El ejercicio no tiene evidencia de aula: `aprobacion_usuario` sigue en `false`. La frase siguiente describe otra ruta al error: suponer que dos posibilidades son igual de probables. Ese sesgo de equiprobabilidad está documentado en un trabajo revisado por pares (Lecoutre, M.-P., 1992, «Cognitive models and problem spaces in "purely random" situations», *Educational Studies in Mathematics*, 23, 557-568). Presentar la suma mal hecha como causa habitual no tiene ese respaldo.
- **Riesgo concreto:** es menor. Le da al estudiante una causa probable que nadie ha medido.
- **Alternativa:** «La afirmación contraria puede venir de una suma mal hecha en las Partes 3 o 4.» Es una sola palabra; no cambia el RNG ni las guardas, y la guarda F-4 no la toca.
- **Veredicto:** MODIFICAR (texto).

## Verificado sin objeción

- La corrección de los seis gaps, incluida la marca en Moodle; no hay segunda clave.
- La alineación de la mezcla en el cloze, comprobada en el código de R/exams 2.4.2.
- R3 byte a byte y las guardas de la Parte 1.
- I-1 a I-8 en el CLOZE.
- Que los residuos firmados no se amplifican: E4 y la depuración del punto 9.
- La independencia de D respecto de los roles.
- La regla #19 y que la Solution no sigue el orden en que se muestran las opciones.
- Las seis secciones de la regla #1.
- n/N con «en cada extracción».
- La ortografía.
- Las glosas I-7 de la Solution.
- La sintaxis cloze de Moodle con `\frac`.
- Arsenal: 0 errores.

**No auditado:**
- Vista HTML a 360 px.
- Páginas 3 a 5 del PDF canónico y los PDF no canónicos (forma parte de H1).
- La pregunta cargada en un Moodle real: dibujo de `MULTIRESPONSE` y penalizaciones.

## Veredicto global: APROBAR_CON_CAMBIOS

Ningún hallazgo es de CORRECCIÓN ni de severidad crítica o alta, así que nada bloquea la FASE 3. La pasada 2 de diagnosticidad no encuentra canales nuevos por encima de +8 pp.

**Próximos pasos:**
1. **H1:** generar y revisar con Read las imágenes de la versión actual y declarar la limitación del DOCX. Hacerlo antes de sellar `coherencias_5`.
2. **H2:** cambiar «suele venir» por «puede venir». Después, volver a renderizar y correr `verificar_espacio_cloze.R`.
3. **Llevar al profesor** los puntos 3 y 8 de la pasada 1 (y el 5, si quiere cerrarlo del todo).
4. **Declarar** los puntos 3, 6 y 7 de la pasada 1 en el HANDOFF, que hoy no tiene sección del CLOZE, y en el `.claude/CLAUDE.md` local, si el profesor los firma como residuo.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/salida/arsenal_hook.txt:18 | PDF no encontrado - ejecutar exams2pdf() primero
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/ejercicio_state.json:64 | 2A APROBADO, 2G 100/100, 2H PASA, 2I/2J/2L/2N/2O OK
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/ejercicio_state.json:56 | Re-render tras la pasada 1 (13:0x) desde cloze/salida con render_cloze.R
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/render_cloze.R:24 | exams2pandoc(F, n = 1, type = "docx", dir = "docx", name = "cloze"
H1 | SIN_ARCHIVO | cloze/salida/docx/cloze1.docx es binario; convertido con pandoc -t plain contiene seis marcadores ##ANSWERi## literales y las opciones agrupadas al final como (a), (b), (c); los DOCX cloze de excedente-almuerzo-proporcional-n4 y migraciones-exteriores-lineas-n2 también tienen 6
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:960 | La afirmación contraria suele venir de una suma mal hecha en las
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:961 | los dos eventos son igual de probables, no
R3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:397 | opciones_p1 <- unname(opciones_base[o])
R3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:686 | ## ==== FIN DEL BLOQUE P1 ====
R3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/salida/verificar_espacio_cloze.txt:5 | 100 semillas (9 canónicas) + 11 canónicas de cuota
R4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:813 | Gaps del cloze (regla #14: seis tipos, seis soluciones, seis ##ANSWERi## en orden)
R5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:692 | el total N y UNA sola dimensión de la misma bolsa (I-4, D2.3)
R5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:745 | Ese número es la cantidad de casos posibles al
R6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:1014 | exextra[Nivel]: 2
R6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:1019 | exextra[DOK]: 2
R6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:1015 | exextra[Descriptor]: D2.3 — Compara la probabilidad de eventos simples
R7 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:807 | I-1 y R7: sin vocabulario de error en las opciones
R8 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:802 | V11 por valor: las Partes 2 a 6 no imprimen cifras
R8 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/salida/verificar_espacio_cloze.txt:33 | Incremento máximo sobre la base atribuible a D: +3.0 pp (juntas_entre_no_nombra_D)
R8 | SIN_ARCHIVO | depuración propia con N = 4000 no canónicas (scratchpad cruce.R): juntas_entre_no_nombra_D 31,7 % frente a la base 32,8 %, incremento pareado -1,1 pp (EE 0,7), mezcla +7,0 pp
R10 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:912 | La respuesta correcta es la que afirma: «
R10 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:917 | **Parte 6.** Tienen la misma probabilidad
R11 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:1009 | exextra[Type]: CLOZE
R11 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:1008 | exshuffle: TRUE
R12 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/salida/verificar_espacio_cloze.txt:48 | compuesta_y_juntas: 68.0 % en las no canónicas, dentro de su firma (64.8 %)
R12 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/verificar_espacio_cloze.R:294 | ok <- R12[[rg]][["nc"]] <= FIRMADAS[[rg]] + 3 * R12[[rg]][["se"]] + 1e-9
R13 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/salida/verificar_espacio_cloze.txt:106 | 23/23 mutantes cazados por su propia sonda
R13 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/verificar_espacio_cloze.R:35 | N_EST <- 100L; TOPE <- 4000L
R13 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/verificar_espacio_cloze.R:113 | NO CONCLUYENTE: estrato
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
