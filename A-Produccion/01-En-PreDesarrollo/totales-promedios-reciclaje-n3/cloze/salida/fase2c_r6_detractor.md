<!-- Reporte íntegro del AgenteDetractor de la FASE 2C r6 del CLOZE (ciclo 2026-11-02, tanda C1e; .Rmd e16842c1), copiado de su transcripción por el coordinador el 2026-11-03 (mensaje final: 2026-11-04T01:25:22.860Z). Su evidencia está en `~/.cache/fase2c_r6_cloze/`, y sus scripts y salidas de texto, copiados en `cloze/salida/fase2c_r6/`. -->

# FASE 2C r6: auditoría del CLOZE `totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1`

**Fecha:** 2026-11-03. **Objeciones:** 2 (0 críticas, 0 altas, 0 medias, 2 bajas), sin CORRECCIÓN.

**Objeto auditado:** `.Rmd` md5 `e16842c1157b3bffe35b8f21682ed6ad` y verificador md5 `1cbc770f15fbf9140cb0ef294c7a0aae`, los dos comprobados con `md5sum`. El encargo decía «sin commit», pero durante la auditoría entró el commit `0a26d4fd` (20:04:40), que trae este mismo md5. El objeto no cambió.

## Resumen ejecutivo

- No hay CORRECCIÓN en ninguna de las 6 partes.
- La frase nueva es verdadera en todo el espacio, enumerado de forma exhaustiva.
- Sus lecturas alternativas quedan contenidas por las frases vecinas; ninguna pasa de BAJA de redacción.
- Los dos hallazgos no tocan el `.Rmd`. Son de exactitud en lo declarado sobre la 6b.
- La regla de parada no se activa. Mi opinión: no hace falta otra FASE 2C.

## Hallazgos

### H1. BAJA · visual, declaración del residuo: la tabla del residuo de cortes no trae el inventario completo

**Evidencia.** Las cifras declaradas se reproducen, pero el inventario tiene dos huecos. Lo medí con código propio, palabra a palabra, sobre `~/.cache/c1e_6b/out.json` (N = 100 por versión y ancho):

| Corte | `e16842c1` 360 px | `e16842c1` 320 px | `32243d0a` 360 px | `32243d0a` 320 px |
|---|---|---|---|---|
| «habría ⏎ restado» (no declarado) | 0/100 | **56/100** | 12/100 | 35/100 |
| «no le habría ⏎ alcanzado» (no declarado) | 0/43 | **28/43** | 43/43 | 0/43 |
| Páginas con algún corte en las dos frases resueltas | 32/100 | **68/100** | 71/100 | 100/100 |
| «Páginas con alguno de los dos», como se declara | 32/100 | 33/100 | 19/100 | 84/100 |

- La tabla mide «habría ⏎ sumado», pero nunca su gemelo «habría ⏎ restado».
- El 0/43 de «no le ⏎ habría alcanzado» parece una mejora a 320 px, pero el corte solo se corrió a la palabra siguiente.
- «A 320 px mejora y a 360 px empeora» vale solo para el subconjunto de dos cortes. Con el inventario completo, en las frases resueltas mejoran los dos anchos (71 → 32 y 100 → 68). Lo que empeora a 360 px es el corte fijo del tramo general, «habría ⏎ ganado con», 100/100.

**Por qué importa.** El profesor pidió declarar este residuo «con su frecuencia real». El `.Rmd` no se viola y no hay ninguna ley rota.

**Alternativa.** Añadir las dos filas y la de páginas a la tabla, en `.claude/CLAUDE.md` y en `cloze/HANDOFF.md`, y corregir esa frase. Para el futuro, contar «habría ⏎ X» por palabra y no por frases fijas: mi script es `/home/bootcamp/.cache/fase2c_r6_cloze/inventario_cortes_r6.py`. No cambia el `.Rmd` ni se pone espacio duro.

### H2. BAJA · exactitud del informe de la 6b: el §F describe cortes que no existen

**Evidencia.** El §F dice que «si el promedio es mayor, sí;» queda entero en un renglón a 360 px (s001) y a 320 px (s001, s005). Los renglones del instrumento dicen otra cosa, en 100 de 100 páginas:
- A 360 px: «si el promedio ⏎ es mayor, sí; si es menor, no.»
- A 320 px: «si ⏎ el promedio es mayor, sí; si es ⏎ menor, no.»

El recorte `n_s001_360.png`, que leí, confirma el corte de 360 px. También es falso que s010 y s011 corten distinto a 320 px: las semillas 1, 5 y 10 cortan igual, y el conteo da 100 de 100. Las métricas del informe no cambian, y esta afirmación no pasó al registro.

**Alternativa.** Una fe de erratas del §F en el registro de la tanda.

## Lecturas de la frase nueva

Las seis de la tabla se reproducen exactas, y ninguna es CORRECCIÓN:

| Lectura | Cifra reproducida (semillas 1 a 1000) | Valoración |
|---|---|---|
| a. «de cada uno» cruzado | HP 306 · HG 306 | Contenida por las frases resueltas |
| b. Un solo veredicto para las dos preguntas | 306 mixtas · 344 VV · 350 FF | La más débil, por el singular «si el promedio». Contenida: cada hipótesis se resuelve por separado |
| c. «sí» y «no» por pregunta | 476, 524, 518 y 482 | Verdadera |
| d. Comparar los dos promedios entre sí | HP 346 · HG 348 | Contenida por «con la diferencia entre los totales:» |
| e. Comparación con el otro grado | 460 · HP 149 · HG 292 · 346 y 652 | Contenida por «Un curso más con el mismo promedio…» |
| f. Diferencia después del cambio | HP 479 · HG 481 | Contenida: la diferencia se cita dos veces con su cifra |

- **Fuera de la tabla.** Aplicar el criterio a la afirmación BP, que tampoco está en el alcance, da el contrario en 413 de 505. Es solo registro: la frase nombra un curso más o uno menos.
- **Otras lecturas.** No encontré ninguna otra lectura razonable que cambie la verdad de la frase.
- **Sintaxis, concordancia y tiempos.** Correctos: el condicional compuesto coincide con el de las frases resueltas y con el de la opción.
- **Residuo.** Por la regla de parada, todo esto queda como residuo declarado.

## Comprobado sin hallazgo

Evidencia en `/home/bootcamp/.cache/fase2c_r6_cloze/`. Usé un oráculo propio en Python que solo lee el texto emitido y recalcula desde las tablas mostradas.

**CORRECCIÓN (foco 8).** 111 versiones emitidas con `xexams` (semillas 1 a 100 y 11 de cuota; 20 canónicas), 0 fallos:
- Partes 1, 2 y 6: clave única, con la marca del `exsolution` emitido. Identifiqué HP y HG por su texto en la `questionlist` después de la mezcla.
- Partes 3 a 5: valores correctos.
- Solution: Respuestas, Procedimiento, párrafo «Parte 6.», análisis de distractores de las Partes 1, 2 y 6, y Caso específico.
- Feedback de las 15 entradas, alineado con cada opción tras la mezcla.
- 1827 igualdades en la Solution y 999 en el feedback, ninguna falsa.
- `##ANSWER1##` a `##ANSWER6##`, en orden y uno tras su parte. Tipos, puntos y tolerancia correctos.
- La canónica coincide con `ITEM_OFICIAL.md` en 20 de 20: enunciado, tabla celda a celda, pregunta y opciones.
- Moodle: 100 versiones nuevas más las 5 del render, 0 fallos en marcas y valores. La frase está en la retroalimentación general en las 105.

**Frase (foco 1).**
- Texto emitido: 222 comparaciones y 222 conclusiones verdaderas contra el contrafactual calculado de la tabla. Salieron las cuatro combinaciones (41, 21, 13 y 36).
- 1000 semillas: literal 1000; HP y HG coinciden con la tabla, la marca y la frase resuelta en 1000 de 1000; 0 empates.
- Enumeración exhaustiva del espacio paralelo: 3529 filas en CURSOS y 1813 en PROMEDIO (espacio ampliado), más el base (2149 y 976): 0 discrepancias y 0 empates. Las cuatro cuentas coinciden con `espacio_paralelo()` del `.Rmd`.

**Alcance (foco 2).**
- `dif == 1` es correcta: `BC_perdio` y `BC_gano` con *k* = 1 en CURSOS equivalen a HP y HG.
- El 0 y 0 no es azar. En CURSOS con *k* = 1, toda fila da HP y HG verdaderas (901 filas en el espacio base y 1060 en el ampliado), así que la afirmación de ventaja es BP.
- Reproducido: 495 BC, 222 con 183, y 273 con 238 (estas últimas mezclan 230/201 con *dif* ≥ 2 y 43/37 con *dif* ≤ −2).

**Crit-P6 y H1-Sol (foco 3).**
- Crit-P6 lee la salida del chunk sin normalizar. Su control exacto da 100 de 100.
- El corte distingue mayúsculas, así que «con un curso más,» no lo dispara.
- H1-Sol solo mira cláusulas con dos cifras, y el tramo general no tiene ninguna: sus «habría ganado» y «habría perdido» no cuentan. Control: 200.
- Mi corrida confirma las dos cosas.

**Mutantes (foco 4).** Corrí el verificador completo, y cada uno muere por la sonda que declara:
- MC50: falsa +57, concl +57; veredicto `_y_otra`.
- MC51: falsa +43, marca +43, control 100; veredicto `_y_otra_y_control`.
- MC53: esp +100; veredicto `cazado_por_su_sonda`.
- MC54: falsa +30, cifra +100; veredicto `_y_otra`.
- MC55: marca +100, sola, control 200; veredicto `cazado_por_su_sonda`.
- MC56: falta +100, control a 0, H1-Sol no se mueve; veredicto `cazado_por_su_sonda`.
- MC57: cifra +100, control 100, H1-Sol no se mueve; veredicto `cazado_por_su_sonda`.

**Invariancia (foco 5).**
- La cadena de md5 se comprobó: `6f6f1b13`, `ef2c96dc` y `32243d0a`.
- El diff frente a `6f6f1b13` es solo el `cat()` de la frase.
- Contra el texto emitido que auditó la r5, en 111 semillas: 0 diferencias en `question`, `questionlist` y `solutionlist`; `solution` idéntica al cambiar una frase por la otra.
- La Parte 1 emitida es igual a la del SCHOICE en las 111: estímulo, opciones, clave y feedback.

**Verificador.** Exit 4 en 469 s, con 368 cifras cubiertas. Las 1447 líneas de resultado son idénticas a las de la C1e. La copia del coordinador también coincide.

**6b (foco 6).**
- El instrumento sigue siendo el mismo (md5 `fff70a89…`).
- La referencia reproduce la C1d.
- Con mi código, las cifras de `contar_h2.py` y `contar_cortes_frase_c1e.py` son correctas: 32/51, 33/51, 100/100 en «habría ⏎ ganado con» y 0 en los demás cortes medidos.
- El problema es de inventario, y va en H1.

**Sellos (foco 7).**

| Tanda | Pasos 3, 4, 5 y 6 (hora) | Artefactos |
|---|---|---|
| C1c | 18:32:18,418 · 18:33:01,054 · 18:32:18,605 · 18:32:18,779 | Render 18:24, hook 18:25:33, invariancia 18:25:58, verificador 18:30:36. El paso 4 va después del 5 y del 6: desviación declarada, y exacta |
| C1d | 19:13:14 · 19:13:25 · 19:13:35 · 19:13:48 | Anteriores a sus sellos, y `ORIGEN.txt` lleva el md5 de la tanda |
| C1e | 19:44:54 · 19:45:08 · 19:45:17 · 19:45:31 | Anteriores a sus sellos, y `ORIGEN.txt` lleva el md5 de la tanda |

- La copia del informe del hook es idéntica al original.
- `detractor_fase2c` sigue sin sellar, y hay 9 reaperturas.
- `render.log` se tocó más tarde porque recibe las comprobaciones finales: a las 19:41:05 en la C1e y a 15 ms del sello en la C1d. No es un hallazgo.

**Sospecha que descarté.** R/exams escribe `=` en cada verdadera de la Parte 6, también con 2 o 3 verdaderas. Parecía dar nota completa con una sola marcada, pero Moodle 5.1 normaliza: `public/question/type/multianswer/questiontype.php`, línea 569 y anteriores, divide cada positiva entre el total y conserva las negativas. Las notas reales son:

| Verdaderas en la versión | Una sola verdadera marcada | Las cuatro marcadas |
|---|---|---|
| 1 | 1 | 0 |
| 2 | 0,5 | 0 |
| 3 | 0,333 | 0,5 |

Leí el código, no lo probé en el servidor del colegio. `notas_p6_r6.py` asumía que no había normalización y queda sin valor; el válido es `notas2_r6.py`. La línea de fallos de Moodle en `moodle_enum_r6.txt` es un error de mi analizador; la válida está en `moodle2_r6.txt`.

**Siguen de antes.** L-8 sigue sin cablear: el verificador lo declara. F-3 no cambió.

## Paso 8: la lista de 10 puntos sí necesita cambios

1. **Puntos 2 y 8.** El literal es el de `e16842c1`. Como literales prohibidos, añadir las tres redacciones anteriores (`6f6f1b13`, `ef2c96dc` y `32243d0a`).
2. **Puntos 3, 5 y 6.** Cortar primero el tramo general en «Con un curso más, el grado», con mayúscula. El tramo lleva ahora «habría ganado», «habría perdido», «con un curso más,» y un «;». Un conteo sobre el párrafo entero daría conclusiones de más, y un corte por «;» parte el criterio.
3. **Punto nuevo.** Juzgar el criterio, no solo su presencia: en cada versión y para HP y HG, «sí» debe coincidir con promedio > *D*, con el contrafactual y con la marca del `exsolution` emitido.
4. **Punto 10.** Añadir dos mutantes: uno que vuelva a la frase de `32243d0a` y otro que intercambie «sí» y «no». Los dos deben morir por texto no juzgado.

Sigue pendiente el endurecimiento de L-C8: el volcado del pool sin las marcas de R/exams.

## No auditado o no repetido

- No rendericé PDF ni DOCX. Para Moodle generé el XML, sin importarlo a un servidor.
- No repetí la 6b: usé su `out.json` y leí un solo recorte.
- No volví a medir §P7 aparte. El texto no cambió, y las 368 cifras firmadas salen iguales en el verificador.
- N = 100 más 11 semillas de cuota en el texto emitido. Las 1000 semillas solo sirvieron para reproducir las cifras declaradas (regla #23).
- `git status`: mi corrida no añadió archivos al repo. La única diferencia son los 4 archivos que `0a26d4fd` sacó de «modificados».

## Veredicto global

APROBAR CON CAMBIOS. Los cambios son documentales (H1 y H2) y del oráculo del paso 8. El `.Rmd` `e16842c1` no necesita tocarse.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/.claude/CLAUDE.md:2993 | Páginas con alguno de los dos
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/.claude/CLAUDE.md:2994 | «no le ⏎ habría alcanzado»
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/.claude/CLAUDE.md:2999 | A 320 px mejora y a 360 px empeora
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/.claude/CLAUDE.md | AUSENTE: habría ⏎ restado
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/salida/ciclo_20261102/c1c/6b_c1c/contar_h2.py:22 | "D": ("habría", "sumado", "habría sumado")
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/salida/ciclo_20261102/c1e/6b_c1e/informe_6b_c1e_e16842c1.md:35 | «si el promedio es mayor, sí;» queda entero en un renglón en 360 (s001) y 320 (s001, s005)
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
