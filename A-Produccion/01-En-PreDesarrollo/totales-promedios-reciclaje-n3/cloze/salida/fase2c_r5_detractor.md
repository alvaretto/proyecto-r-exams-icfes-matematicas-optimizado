<!-- Reporte íntegro del AgenteDetractor de la FASE 2C r5 del CLOZE (ciclo 2026-11-02, tanda C1b; .Rmd 6f6f1b13), copiado de su transcripción por el coordinador el 2026-11-03 (mensaje final: 2026-11-03T22:18:34.928Z). Su evidencia está en `~/.cache/fase2c_r5_cloze/`, y sus scripts y salidas de texto, copiados en `cloze/salida/fase2c_r5/`. -->

# Auditoría Detractor, FASE 2C r5: CLOZE `totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1` (tanda C1b)

**Fecha:** 2026-11-03
**Objeciones:** 3, todas BAJA (0 críticas, 0 altas, 0 medias). **CORRECCIÓN: ninguna.**

**Artefactos auditados (md5 comprobados):**
- `.Rmd`: `6f6f1b1311b0d681bd974c719ecd75d5`
- verificador: `6d37faad687a5fc11e69b763429b6624`
- SCHOICE: `63e2713e6ac0bdbf250f4dc33901b2ec`

El diff del `.Rmd` toca solo los dos sitios declarados.

## Resumen ejecutivo

La C1b cumple lo que se le pidió.
- **El párrafo nuevo es verdadero.** Lo leí del texto que emite R/exams 2.4.2 (`xexams` + `read_exercise`, después de `exshuffle`) en 205 versiones (semillas 1 a 205, que incluyen las 100 de E1 y sus 20 canónicas). Ninguna de las 410 comparaciones con cifras es falsa, y ninguna de las 410 conclusiones es falsa. Las conclusiones las evalué con el contrafactual calculado desde la tabla mostrada, no desde la palabra «más» o «menos».
- **Las seis partes son correctas en todas las versiones.** No hay fallos de CORRECCIÓN en ninguna de las 205.
- **Lo que pide cambios es menor:**
  - El criterio general nuevo admite dos lecturas que lo vuelven falso (H1).
  - El BAJO de la 6b a 320 px no es ocasional: es sistemático, y también aparece a 360 px (H2).
  - El juicio de los mutantes MC50, MC51 y MC54 no sigue la convención `_y_otra` que la C1b aplicó a MC39 (H3).

## Hallazgos

### H1: el criterio general admite dos lecturas que lo vuelven falso
**Severidad:** BAJA. **Tipo:** precisión de la Solution (regla #13 v1.1).

**Qué se cuestiona:** «Por eso basta comparar ese promedio con la diferencia entre los totales: si es mayor, el ganador cambia; si es menor, no.» (`.Rmd`, líneas 1067-1068).

**Evidencia:**
- **(a) Sujeto elidido.** «si es mayor» tiene dos antecedentes posibles. Los dos son singulares y compatibles con «mayor»: «ese promedio» y «la diferencia», que además es el más cercano.
  - Leído como «si la diferencia es mayor, el ganador cambia», el criterio se invierte: es falso en el 100 % de las versiones.
  - La lectura buscada es la más natural y es verdadera en 205 de 205. Las frases que siguen la desambiguan.
- **(b) Alcance.** El párrafo abre con «Cada hipótesis cambia un solo dato de la tabla» y el «Por eso» presenta el criterio como consecuencia general.
  - En la redacción vieja el «Por eso» llevaba su alcance explícito («habría ganado con un curso más si y solo si…»). En la nueva, el alcance solo lo da, de forma implícita, «ese promedio».
  - La afirmación de ventaja de la Parte 6 también es una hipótesis. Si un estudiante le aplica el criterio:
    - En las de cursos (BC, 102 de 205), le da el veredicto equivocado en 80 de 102. Con `k ≥ 2`, o cuando en la rama PROMEDIO el perdedor *pierde* cursos, lo que hay que comparar es `k·a` con *D*′, no *a* con *D*′.
    - En las de promedio (BP, 103), el criterio no aplica.
- No es CORRECCIÓN: con la lectura buscada, el párrafo es verdadero en 205 de 205, y el feedback de la afirmación de ventaja da el cálculo correcto.

**Alternativa:** cambiar una sola frase. Así resuelve las dos lecturas:
> «Por eso, con un curso más o uno menos, basta comparar ese promedio con la diferencia entre los totales: si el promedio es mayor, el ganador cambia; si es menor, no.»

El criterio sigue sin cifras, así que la sonda H1-Sol no cambia. Lo que sí hay que actualizar es el literal `P6_NUEVO` del verificador (MC51): si no se actualiza, sale «MAL CONSTRUIDO», a la vista. Requiere reapertura desde `generacion_rmd` y medir la 6b otra vez. Lo decide el profesor.

**Veredicto:** MODIFICAR.

### H2: el BAJO de la 6b es sistemático a 320 px y también aparece a 360 px
**Severidad:** BAJA. **Tipo:** visual; el residuo está mal declarado.

**Qué se cuestiona:** el residuo declarado «puede partir la conclusión … en 3 de los 6 recortes … no se midió su frecuencia» (`SUB/.claude/CLAUDE.md`, líneas 2371-2373).

**Evidencia (medición propia, N = 100):**
- Semillas 1 a 100, `set.seed` + `exams2html`.
- Instrumento: `cap7.py` de la 6b con un solo campo más, los renglones del párrafo. Chromium headless por CDP, `innerWidth` 320 y 360 comprobado, red bloqueada.
- 200 páginas, 0 errores.

| Corte en la conclusión | 320 px | 360 px |
|---|---|---|
| «habría ⏎ ganado.» (HP verdadera) | **57 / 57** | 0 / 57 |
| «habría ⏎ seguido ganando.» (HG falsa) | 43 / 51 | **29 / 51** |
| Páginas con alguno de los dos | **84 / 100** | **29 / 100** |

- Además, dentro de las frases:
  - «no le ⏎ habría alcanzado»: 43 de 43 a 320 px.
  - «habría ⏎ sumado»: 100 de 100 a 320 px y 41 de 100 a 360 px.
- El espacio duro entre «seguido» y «ganando» empuja el corte delante de «seguido». Por eso el corte B también aparece a 360 px, y la 6b no lo menciona.
- Ninguna ley se viola: L-6 y la palabra sola en el último renglón siguen en 0. Es un corte normal de un tiempo compuesto, pero el residuo está declarado por debajo de su frecuencia real.

**Alternativa:**
- Corregir la declaración del residuo en el registro con estas cifras, sin tocar el `.Rmd`.
- Si el profesor quiere quitarlo, poner espacio duro entre «habría» y su participio en las dos conclusiones y medirlo pareado a 360 y 320 px. El riesgo es que queden unidades más largas y renglones más desiguales.

**Veredicto:** MANTENER el `.Rmd` y MODIFICAR la declaración.

### H3: los mutantes de H1-Sol se juzgan sin la convención `_y_otra`, y la componente «marca» no lee del texto
**Severidad:** BAJA. **Tipo:** cobertura y precisión del verificador. Endurecerlo es autónomo (H-5).

**Evidencia:**
- **Veredictos de los mutantes.** `mut_h1()` (línea 1542) devuelve `cazado_por_su_sonda` en cuanto su componente está entre las afectadas, aunque haya otras. La convención del mismo verificador (línea 1500) reserva `cazado_por_su_sonda` para cuando la sonda es la única afectada, y es la que la C1b aplicó a MC39 por el H2 de la r4. Según la salida de la C1b:
  - MC50: «falsa +57, concl +57».
  - MC54: «cifra +100, falsa +30».
  - MC51: «falsa +43, marca +43», más el control.
  - Ninguna componente queda probada en aislamiento salvo `esp`, con MC53.
- **Componente «marca».** El comentario de la línea 712 dice «la marca … en exsolution». La línea 748 la lee de las variables del entorno, `en$sol_p6` y `en$rol_p6`, no del `exsolution` emitido.
  - Lo comprobé por mi lado: en las 205 versiones, las conclusiones del párrafo coinciden con las marcas del `exsolution` emitido después de la mezcla. No hay defecto en el `.Rmd`, pero la sonda no es «leída del texto» en esa componente.

**Alternativa:**
- En `mut_h1()`: `identical(hits, comp)` → `cazado_por_su_sonda`; si `comp %in% hits` con otras → `cazado_por_su_sonda_y_otra` (con `_y_control` si toca).
- Declarar los esperados así: MC50 y MC54 `_y_otra`, MC51 `_y_otra_y_control`, MC53 sin cambio.
- Leer la marca del `exsolution` emitido, o corregir el comentario de la línea 712.

**Veredicto:** MODIFICAR el verificador.

## Lo que comprobé sin hallazgo (con cifras)

**1. Redacción nueva.**
- Criterio general:
  - Con la lectura buscada, el bicondicional es verdadero en 205 de 205.
  - No hay empates: *a*′_P ≠ *D*′ y *a*′_G ≠ *D*′.
  - Salen las seis combinaciones de veredictos:
    - CURSOS: (F, F) 33, (V, F) 30, (V, V) 34.
    - PROMEDIO: (F, F) 36, (F, V) 39, (V, V) 33.
- Comparaciones («habría sumado a su total *N* kilos, más/menos que la diferencia de *D* kilos»):
  - No encontré ninguna otra lectura razonable que cambie su verdad. La aposición se refiere a «*N* kilos». «menos que» lleva coma, así que no se lee como *D* − *N*. «que» y no «de» es correcto, porque el segundo término es un sintagma nominal.
  - «restar a» está aceptado.
- Tiempos:
  - El condicional compuesto concuerda con las opciones («habría ganado el concurso») y con el marco pasado («Ganó el grado…»). Es la apódosis normativa de un contrafactual pasado.
  - «no le habría alcanzado para ganar» es correcto. El «no le alcanzaría» de la decisión del profesor iba como «Por ejemplo», así que el cambio no relaja nada (H-5).
- Concordancia: el sujeto es el grado, en singular, y «le» se refiere a él.
- L-6: 0 pares cifra-kilos con espacio normal, y el espacio duro final está en 205 de 205.
- Regla #19: 0. Regla #25: ningún carácter fuera de Latin-1 en el párrafo. Tildes bien.

**2. Guarda y sonda nuevas.**
- Guarda «H1 r4»:
  - En mi emisión, la primera semilla en que `v_hp ≠ v_hg` es la 5, como declara MC52 («cazado … en la semilla 5»).
  - La guarda solo puede fallar en esas semillas: 69 de 205.
- Sonda H1-Sol:
  - Su control exacto da 200 (salida, línea 478).
  - Su patrón, aplicado por mí al párrafo viejo emitido, reconoce 100 comparaciones en 100 versiones: solo la de «un curso más». La de «un curso menos» no tiene segunda cifra y solo la caza el control. Eso es coherente con `cazado_por_su_sonda_y_control`, declarado para MC51.
  - Sobre el texto nuevo emitido: 410 comparaciones, el doble de las 205 versiones.

**3. MC39 (H2 de la r4).**
- La línea base `h2_exacta(COD_C, 1L)` está en la misma medida que el mutante.
- Las filas de `j39`:
  - en la línea base: +12,5 pp, «cubierto»;
  - con el mutante: +25,0 pp, «pendiente» con la firma regla por regla y cubiertas con la de estrato.
- El mutante abre 33 pendientes nuevos, y los 100 que ya tenía la línea base no cuentan. El veredicto es `cazado_por_su_sonda_y_otra`.
- Prueba lo que dice. Matiz: el mutante cae justo en el tope (+25,0 = +25,0), así que fija además el «≤» de la puerta de estrato. Con un «<», `j39` quedaría vacía y el `ok()` fallaría a la vista, sin dar un falso verde.

**4. Invariancia.** Con mi emisión `xexams` de `6f6f1b13` frente a la de `27f789dc`, semillas 1 a 100:
- Salen idénticos en las 100:
  - el enunciado;
  - las `questionlist` y `solutionlist`;
  - la metainformación, salvo `file`, que codifica la ruta de la copia;
  - la Solution fuera del párrafo.
- Las cifras y los grados del párrafo son los mismos que en el viejo, también en 100 de 100.

**5. CORRECCIÓN, en 205 versiones, con funciones de verdad propias sobre el texto emitido: 0 fallos.**
- Parte 1:
  - La clave es la única verdadera y coincide con la marca en 205 de 205. Revisé a mano 56 esqueletos de opción. Los distractores que dan el triunfo al ganador son falsos solo por citar una cifra distinta de *D*. Los que se lo dan al perdedor son falsos porque esa sede no ganó.
  - Canónica: 20 de 20 iguales al impreso (enunciado, pregunta, encabezado «recolectado», celdas, las cuatro opciones literales y la marca en el contenido de B).
- Parte 2: una sola clave, igual a la marca, en 205 de 205.
- Partes 3 a 5: totales y diferencia iguales a los de la tabla mostrada en 205 de 205.
- Parte 6: las marcas coinciden con mis funciones de verdad (siete tipos de afirmación) en 205 de 205, con al menos una verdadera y una falsa, y sin empates contrafactuales.
- Feedback:
  - el prefijo «Verdadero.»/«Falso.» coincide con la verdad en las 12 entradas de elección de cada versión;
  - el contenido de HP, HG, ventaja y ganador está bien en 205 de 205 cada uno;
  - en la Parte 1, todo cuadra: «recogió *N* kilos» (205), «los *N* de» (595), «frente a» (615) y «por *D*, no por *E*» (205).
- Solution, en 205 de 205:
  - las Respuestas de las seis partes;
  - las frases «Ganó …» con su razón, en la Parte 1 y en las Partes 3 a 5;
  - los dos «Caso específico»;
  - el análisis de distractores de las Partes 1, 2 y 6.
- Aritmética: 5251 igualdades en el texto emitido, ninguna falsa. Ningún NA ni NULL.
- Estructura:
  - `##ANSWER1##` a `##ANSWER6##`, en orden y una vez cada uno, en 205 de 205;
  - `clozetype` schoice|schoice|num|num|num|mchoice;
  - puntos 3|1|1|1|1|1 y tolerancia 0;
  - `questionlist` de 15 entradas, con 3 vacías para las partes num.

## Paso 8 (L-C8, H3 de la r4): qué debe comprobar el oráculo r10 en el párrafo nuevo

1. Leer el párrafo del texto emitido (`xexams` o HTML), no de las variables del entorno, y normalizar U+00A0 solo para comparar. L-6 se mide sobre el texto crudo.
2. Exigir exactamente un párrafo «Parte 6. Cada hipótesis…» y el criterio literal sin cifras (el nuevo, si se aplica H1). Si el texto no está juzgado, fallar.
3. Exigir exactamente dos comparaciones con cifras por versión:
   - «un curso más»: el grado que perdió según la tabla, su promedio y *D* = |*T*₁ − *T*₂|;
   - «un curso menos»: el grado que ganó, su promedio y el mismo *D*.
4. Comprobar que cada «más» o «menos» es verdadero con sus dos cifras, con la aserción de que *a* ≠ *D*.
5. Comprobar que cada conclusión es verdadera con el contrafactual calculado desde la tabla, (*n*_P + 1)·*a*_P frente a *T*_G y (*n*_G − 1)·*a*_G frente a *T*_P, y que es coherente con el «más» o «menos».
6. Comprobar que cada conclusión coincide con la marca de HP y de HG en el `exsolution` emitido, identificando HP y HG por su texto en la `questionlist` después de la mezcla (no por `rol_p6`).
7. Comprobar L-6 en el párrafo: cada par cifra-kilos y las dos últimas palabras llevan U+00A0.
8. Sin literales de la redacción vieja, como los de las líneas 360, 398, 403 y 414 del r9.
9. No escribir nada en el directorio de trabajo.
10. Mutantes que deben matarlo: MC50, MC51 (por texto no juzgado o por el conteo), MC54 y uno que cambie solo la conclusión.

## No auditado o no repetido (declarado)

- No volví a correr el verificador completo (480 s): usé su salida y mis comprobaciones independientes. Tampoco volví a medir §P7: la C1b no toca opciones ni enunciados (invariancia 100 de 100).
- No rendericé PDF, DOCX ni Moodle. No volví a calcular la métrica de «una sola palabra en el último renglón» de la 6b.
- No usé subagentes. Todas mis salidas están en `/home/bootcamp/.cache/fase2c_r5_cloze/`:
  - `emitir.R`, `emit.rds`, `verdad.R` → `verdad.txt`, `verdad2.R`, `invariancia.R`;
  - `6b/render.R`, `6b/cap7_r5.py`, `6b/out.json`.
- R/exams teje en `tdir`, también dentro de `~/.cache`. No comprobé con `git status` que el repo quedara sin cambios.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd:1066 | Cada hipótesis cambia un solo dato de la tabla.
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd:1067 | Por eso basta comparar ese promedio con la diferencia entre los
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd:1068 | totales: si es mayor, el ganador cambia; si es menor, no.
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/.claude/CLAUDE.md:2372 | en 3 de los 6 recortes del NUEVO
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/.claude/CLAUDE.md:2373 | no se midió su frecuencia en las 100 semillas
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd:1070 | if (hp_supera) "habría ganado" else "no le habría alcanzado para ganar"
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/verificar_espacio_cloze.R:1542 | if (ctrl) "cazado_por_su_sonda_y_control" else "cazado_por_su_sonda"
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/verificar_espacio_cloze.R:1500 | else if (identical(hits, sonda)) "cazado_por_su_sonda" else if (sonda %in% hits) "cazado_por_su_sonda_y_otra"
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/salida/ciclo_20261102/c1b/verificar_espacio_cloze_c1b.txt:935 | falsa +57, concl +57; control: 200 comparaciones
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/salida/ciclo_20261102/c1b/verificar_espacio_cloze_c1b.txt:938 | falsa +30, cifra +100; control: 200 comparaciones
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/verificar_espacio_cloze.R:712 | la conclusión contradice la marca de HP o de HG en exsolution
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/verificar_espacio_cloze.R:748 | isTRUE(en$sol_p6[en$rol_p6 == if (papel == "mas") "hp" else "hg"])
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
