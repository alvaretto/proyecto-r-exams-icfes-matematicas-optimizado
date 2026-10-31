# Auditoría Detractor, FASE 2C ronda 2: CLOZE `totales-promedios-reciclaje-n3`

**Objeto:** `.Rmd` md5 `42f725bf191b81784f76b858c26f7733`, batería L-C4 md5 `de20e0a986f86d44f0d3412517d2f07b` (comprobada)
**Fecha:** 2026-10-31 · **Modo:** solo lectura; scripts propios en `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/de2a4b7e-30f1-462f-99eb-f555afbbd5c4/scratchpad/`
**Hallazgos:** 4 (0 CORRECCIÓN, 0 críticos, 0 altos, 3 medios, 1 bajo)

## Resumen ejecutivo

- **Corrección:** no encontré ningún defecto. Claves, distractores, Solution y Parte 6 están bien en los dos tipos.
- **Las cifras del orquestador se reproducen con código propio.**
  - Las 20 reglas de L-C4 en la Parte 2 quedan en +2,8 pp o menos en los dos tipos.
  - Las 69 celdas de «H2 a» siguen idénticas tras la pasada.
  - Pesos, espacios duros y canónicas están en orden.
- **H1 de la ronda 1 se cerró dentro de la batería congelada, pero el defecto no desapareció: se desplazó.**
  - En el tipo «promedio», las tres claves dicen «dividido entre» y ningún distractor lo dice.
  - Elegir «la que dice dividido», «la que dice entre» o «la que más palabras comparte con el párrafo del otro colegio» acierta el 100 %, es decir, +75,0 pp exactos.
  - En «comparar» queda «descarta las que dicen *por curso*», +25,0 pp.
  - Ninguna sonda los ve: L-C4 no incluye el párrafo `intro_par` como estímulo.
- **Por §P7-C no obligan en este ciclo,** pero hay que declararlos con su cifra y llevarlos al profesor antes del paso 11.

## Verificaciones pedidas

**1. Corrección de la Parte 2 nueva: sin hallazgo.**
- **«promedio».** Las tres claves son T/n: verdaderas. Los cinco distractores son falsos con cualquier lectura: máximo, total, «todos recogieron lo mismo», mediana y valor mayoritario.
- **«comparar».** Las tres claves son T′ de cada grado (suma, producto, cursos × promedio). Los cinco distractores son a, n, Δa, n + a y a/n.
- **Una sola clave por versión,** por construcción: `pick(q2$clave)` más 3 del pool de distractores, y la guarda «Parte 2: una sola clave».
- **Lecturas alternativas examinadas, sin que ninguna invierta un veredicto:**
  - «dividido en partes iguales» leído como «el total ya repartido» sigue describiendo la parte igual.
  - «¿Qué se compara *en la tabla*…?» leído como «qué dato de la tabla» no vuelve verdadero ningún distractor: ni a ni n solos deciden.
  - En la rama PROMEDIO, comparar a (o a/n) da el ganador por coincidencia, pero no es el criterio. Su feedback dice «no garantiza», lo cual es cierto.
- **Parte 6 y Solution.**
  - HP ⟺ a′_P > D′ y HG ⟺ a′_G > D′, sin empates por guarda.
  - BC/BP ⟺ rama, con V10 recalculado desde el texto.
  - «solo si … fuera mayor que la diferencia» es cierto.
  - `expl6` es coherente con la marca (guarda de las líneas 880-881).

**2. Cierre de H1 y H3: hecho en L-C4, con un canal nuevo fuera de ella (H1 y H2 abajo).**
- Mi enumeración exacta propia: 720 versiones por tipo, repetida con los 3 materiales y los 6 pares de grados, con rango 0,0 pp entre ellos.
- **«promedio».** Máximo P2-F3 +2,8; R1, R2 y R3 +2,5.
- **«comparar».** Máximo P2-F3 +2,8; R2 −10,6.
- Coinciden con `p2_redaccion_42f725bf.txt`, que midió un solo material y un solo par de grados.
- **H3 de la ronda 1, cerrado.** El encabezado depende del tipo (líneas 1019-1020), y «cociente» tiene su `expl2` (línea 845).

**3. Override de H2: bien cableado y sin cambios.**
- Las 69 celdas de `FIRMA_H2` coinciden con la columna `ex` de `h2_exacta_96cfcf35.txt`: 0 sin pareja y 0 distintas.
- Volví a correr `medir_h2_exacta.R` sobre `42f725bf`:
  - 3200 hojas, 15 892 filas, masa con error 0,0000;
  - **0 celdas distintas de lo firmado, 0 por encima, 0 pendientes fuera de la firma.**

**4. RL1 y RL2: reproducidas analíticamente.**
- **RL2 = 5/6.** Con hp = hg (probabilidad 2/3), las dos hipótesis llevan el veredicto contrario a `v_d`. Con hp ≠ hg (probabilidad 1/3), aciertan la mitad de las veces. Resultado: 83,3 %, +33,3 pp.
- **RL1 en CURSOS.** Acierto ½ · ⅔ = 33,3 %, nulo ½ · (⅔ · ⅓ + ⅓ · ⅔) = 22,2 %: +11,1 pp. Coincide con lo firmado.

**5. La batería L-C4 y sus reglas del estímulo.**
- La integridad está bien.
- El estímulo de la Parte 2 que registra es la pregunta, las Partes 3 a 5 y los encabezados. **Falta el párrafo del otro colegio:** es el H3 de abajo.

**6. V11 «a un paso»: residuo declarado, sin objeción.** En la mezcla, 11,0 % y 49,0 %, con n = 100. Las Partes 3 a 5 son numéricas con tolerancia 0, y esas coincidencias salen de la densidad de operaciones entre enteros pequeños: no indican qué operación hacer.

**7. Canónicas: dentro de lo firmado.**
- X6 22,2 % (−2,8 pp) y X10 50,0 % (+0,0 pp): ya no están en rojo.
- X9 75,0 % (+25,0 pp) y X11 62,5 % (+12,5 pp): cubiertas.
- E1 en verde. Mi diff textual: el bloque P1 difiere del SCHOICE solo en las 4 líneas de los tres renombres.

**8. Pesos 3|1|1|1|1|1: correctos.**
- Línea 1070 del `.Rmd`, más E7 con 4 mutantes.
- La guarda de `exportar_moodle_cloze.R:35`.
- `exportar_r3.log`, escrito después del mtime del `.Rmd`: 30 versiones, 180 gaps, «secuencias de pesos: 3|1|1|1|1|1».

**9. Espacios duros: correctos.**
- U+00A0 literal solo en tres líneas:
  - la 87, dentro del bloque P1 e idéntica a la línea 87 del SCHOICE;
  - las 598 y 789, que son código que normaliza los espacios.
- `ED` solo en las líneas 577 a 1043, en los textos de las Partes 3 a 6 y su Solution.
- Las secciones de la Parte 1 en la Solution (líneas 956, 967-984 y 1038-1040) llevan espacio normal, como fija la decisión 2 a.

**10. V9 en p6:** es el H4 de abajo.

## Hallazgos

### H1 · MEDIA · DIAGNOSTICIDAD: en el tipo «promedio», la clave se reconoce por «dividido entre» (+75,0 pp exactos)

**Evidencia.**
- Las tres claves dicen «dividido entre». Ninguno de los cinco distractores dice «dividido» ni «entre».
- El párrafo del otro colegio contiene «entre dos grados». Por eso la clave comparte siempre una palabra más con el estímulo.

| Regla, fuera de L-C4 (exacta, 720 versiones; igual con los 3 materiales y los 6 pares) | Exceso |
|---|---|
| elige la que dice «dividido» | +75,0 pp |
| elige la que dice «entre» | +75,0 pp |
| más palabras en común con `intro_par` | +75,0 pp |
| más palabras en común con todo el estímulo de la Parte 2 (párrafo, pregunta y encabezados) | +75,0 pp |
| «número» o «participantes» (solo la clave 1) | +25,0 pp |

**Lectura.**
- Es H1 de la ronda 1 desplazado: antes, «total» en las claves de «comparar», +75 pp; ahora, «dividido entre» en las claves de «promedio».
- La evidencia del orquestador («PEOR exceso de la Parte 2: +2,8 pp», más 5 sondas EXTRA) no mide vocabulario exclusivo de la clave ni el párrafo del otro colegio.
- Esto es el atajo de asociación entre enunciado y opción («clang association»). Fuente Nivel 1: Haladyna, Downing y Rodriguez (2002), *Applied Measurement in Education* 15(3), guía «avoid clues… clang associations» y «keep choices homogeneous in content».
- **Trade-off legítimo.** «Promedio = dividir» es parte del constructo. Aun así, sin ningún distractor que divida, el ítem mide si el estudiante reconoce la palabra, no si sabe qué se divide entre qué.

**Propuesta.**
- Por §P7-C no obliga en este ciclo. Declararlo con su cifra en el HANDOFF y llevarlo al profesor antes del paso 11. Él decide entre gastar la pasada 3 de §P7-D o aceptarlo como residuo.
- Si se gasta la pasada 3: dar «dividido entre» a 4 de los 5 distractores, con operandos equivocados y siempre falsos.
  - Con eso, «elige dividido» baja a 30 %, +5,0 pp; con los 5, a +0,0.
  - Ejemplos:
    - «Lo que recogió el mejor de sus cursos dividido entre el número de sus cursos.»
    - «Lo que recogió ese grado dividido entre los cursos del otro grado.»
    - «Lo que recogieron los dos grados dividido entre los cursos de ese grado.»
  - Cada uno con su `expl2`.
  - Después, volver a medir L-C4, estas sondas, X5, X6 y X10, y la canónica.

### H2 · MEDIA · DIAGNOSTICIDAD: en «comparar», «por curso» marca distractor (+25,0 pp exactos)

**Evidencia.**
- Tres distractores dicen «promedio por curso»: «promedio», «resta» y «suma». Ninguna clave lo dice.
- «Descarta las que dicen *curso* en singular» da +25,0 pp exactos.
- «recogido» y «los», que solo están en la clave 1, dan +25,0 pp.
- La contraria de la regla del párrafo («descarta la que más comparte con el párrafo») da +6,1 pp: zona gris.

**Propuesta.** Declararlo con H1. En la pasada 3, quitar «por curso» de esos distractores: por ejemplo, «El promedio de un grado y el del otro grado.» Eso deja la regla en +0,0; después, volver a medir.

### H3 · MEDIA · OTRO (cobertura del verificador, §P7-G): L-C4 no incluye el párrafo del otro colegio

**Evidencia.**
- El registro de la Parte 2 en la batería solo lleva `preg = texto_p2, otros = c(texto_p3, texto_p4, texto_p5)`.
- `intro_par` no aparece en la batería, aunque se muestra justo encima de la tabla y de la Parte 2.
- Por ese hueco entró H1.

**Propuesta.**
- En este ciclo, declarar la cobertura parcial del estímulo.
- En la próxima batería (L-C4 v2, que obliga a volver a medir todo el histórico), añadir «más palabras en común con todo el estímulo de la Parte 2» y una sonda de vocabulario exclusivo de la clave, calibrada contra el techo nulo.

### H4 · BAJA · OTRO (proceso): el ERR_DIAG_SUPERFICIAL de p6 es un artefacto de la sonda, y hay que dejarlo escrito

**Evidencia.**
- `validar_diagnosticidad.R` sale con exit 1 por «la correcta empieza por *si* en el 100 % de 17 versiones».
- Las versiones con una sola verdadera (17 de 100) son, por L-C1, las que tienen hp ≠ hg con «ventaja» y «Ganó…» falsas. En ellas, la única verdadera es siempre una hipótesis.
- Pero 3 de las 4 afirmaciones empiezan por «Si»: HP, HG y las cuatro redacciones de «ventaja».

**Mi juicio, con cifras: el orquestador tiene razón.**
- La regla «las que empiezan por *Si* son verdaderas» predice un valor constante por papel. Con el nulo de la batería, p·q + (1 − p)(1 − q), su exceso es **0,0 pp exactos**.
- Marcar las tres «Si» y dejar fuera «Ganó…» **nunca** coincide con la clave, porque «ventaja» y «Ganó…» llevan el mismo veredicto.
- La sonda condiciona por el número de verdaderas, que el estudiante no ve.
- El canal real condicionado es RL2 (+33,3 pp), y ya está firmado.

**Propuesta.**
- No sellar el paso 9 sin anotar en el HANDOFF esta justificación con sus cifras.
- Como tarea aparte del repo: la sonda H3 de `validar_diagnosticidad.R` no debe aplicarse a gaps mchoice. Su propio encabezado dice «gaps de selección única».

## Medido y sin objeción

- **Largo.** «La tercera en largo» da +15,0 pp en los dos tipos. Los márgenes con las opciones vecinas van del 1,3 % al 8 %: por §P7-B es inexplotable.
- **Prefijo.** Las dos primeras palabras únicas dan +0,0 y −2,5 pp.
- **Molde.** Las tres últimas palabras dan ±7,5 pp.
- **Cruzadas** (n = 100): X5, X6 y X10 llegan como máximo a +6,0 pp, en la rama PROMEDIO.
- **Mutantes.** MC7, MC19, MC20 y MC23 quedan cazados por su sonda en `verificar_espacio_cloze_r6b.txt`.

**No auditado:** el render visual (PDF, HTML y Moodle a 320-360 px) y la ortografía.

## Veredicto global

**APROBAR CON CAMBIOS.**
- No hay ningún defecto de corrección.
- Todo lo que obliga queda dentro de L-C4 o de lo firmado.
- Los cambios son declarativos: H1, H2 y H3 con sus cifras ante el profesor (pasada 3 de §P7-D o residuo), y H4 por escrito antes de sellar el paso 9.

## Próximos pasos

1. Anotar H1 (+75,0), H2 (+25,0) y H3 en el HANDOFF y llevarlos al profesor.
2. Si él decide gastar la pasada 3: los distractores con «dividido entre» y «promedio» sin «por curso», volviendo a medir con L-C4, las sondas de H1 y H2, las cruzadas y la canónica.
3. Dejar escrito H4 en el sello del paso 9.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd:684 | Lo que recogió el grado dividido entre el número de sus cursos participantes.
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd:686 | Lo que recogió ese grado dividido entre sus cursos.
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd:689 | Lo que recogió el grado sumando lo de sus cursos.
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd:660 | donde el concurso se hizo entre dos grados
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/salida/p2_redaccion_42f725bf.txt:38 | PEOR exceso de la Parte 2: +2.8 pp
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd:699 | El promedio por curso de un grado y el del otro grado.
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd:696 | El producto de cursos por promedio de un grado y el del otro grado.
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/bateria_partes_2_a_6.R:251 | preg = texto_p2, otros = c(texto_p3, texto_p4, texto_p5)
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/bateria_partes_2_a_6.R:99 | más palabras en común con su pregunta
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/bateria_partes_2_a_6.R | AUSENTE: intro_par
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/salida/diagnosticidad_r3.txt:1-25 | la correcta empieza por "si" en el 100% de 17 versiones
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd:744 | v_c <- if (v_hg == v_hp) !v_hg else sample(c(TRUE, FALSE), 1L)
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd:733 | Si el grado que perdió hubiera participado con tantos cursos como el que ganó
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
