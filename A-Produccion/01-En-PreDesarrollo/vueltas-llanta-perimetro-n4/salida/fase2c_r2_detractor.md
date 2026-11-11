# Auditoría del detractor, FASE 2C r2: `vueltas-llanta-perimetro-n4` (SCHOICE)

**Fecha**: 2026-11-10
**Artefacto**: `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd`. Comprobé en disco que su md5 es `4389766601257f9e715c274e82d66d2c`.
**Independencia**: soy un detractor nuevo. No escribí ni corregí el artefacto, y no soy el detractor de la r1.

**Objeciones**: 5.
- Ninguna de CORRECCIÓN.
- 1 ALTA de DIAGNOSTICIDAD.
- 1 MEDIA de DIAGNOSTICIDAD.
- 1 MEDIA de testing.
- 2 BAJAS.

## Resumen

**La clave es verdadera y única en todas las versiones.** Lo comprobé con un oráculo propio que lee el texto visible en 2000 semillas (210 de ellas canónicas): 0 problemas. Las correcciones de H3, H4 y H5 de la r1 están bien hechas.

**El canal de la r1 no se cerró: se desplazó a la anchura de las opciones.** En la forma `fraccion_unica`, que es el 70 % de las versiones no canónicas, la clave es la fracción más ancha a la vista siempre que no esté VL-G:
- en el agregado acierta el 67,5 %, frente a un techo nulo de 24,9 % (+42,6 pp);
- dentro de esa forma acierta el 80,0 % (+54,9 pp).

La batería congelada no lo ve porque mide «la más larga» en caracteres del código LaTeX. En esa forma, la clave, VL-B y VL-F tienen siempre los mismos caracteres, solo que en otro orden.

El mismo defecto de medición afecta lo que va a firma en las formas `izq`:
- «la más larga (chars)», con 54-55 %, es un artefacto: `\times` tiene dos caracteres más que `\div`, pero en pantalla × y ÷ ocupan lo mismo. Medida a la vista, da 27,0 % y 27,5 %.
- El piso de esas formas medido a la vista es 40 %, no 50 %.
- Hay una forma de escritura que el programa lineal no exploró y que baja el piso a 33,9 %.

## 1. CORRECCIÓN: lo que verifiqué ejecutando

**Oráculo**: `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0e74dd34-847c-4a81-ab53-a25af8b1f7ff/scratchpad/detractor_r2/oraculo_r2.R`, con salida en `oraculo_r2.txt` en la misma carpeta. Sobre 2000 semillas lee del texto la constante, las unidades, la distancia y la letra, comprueba que la constante es la conversión real y evalúa cada opción LaTeX en tres perímetros con un analizador propio de llaves.

| Comprobación | Resultado |
|---|---|
| Errores de `data_generation` | 0 de 2000 |
| Exactamente una opción correcta, y es la marcada | 2000 de 2000 |
| La constante K coincide con la conversión de las unidades del texto; unidades de la pregunta y del contexto | 0 discrepancias |
| «Caso específico» (aritmética y «da lo mismo») | 0 discrepancias |
| «Respuesta correcta … equivale a $K·d/V$» con la letra de la versión | 0 discrepancias |
| Cifras del feedback de cada opción frente a mi evaluación | 0 discrepancias |
| El diagnóstico describe exactamente las 3 opciones incorrectas mostradas | 2000 de 2000 |
| Letras de opción en la Solution (regla #19) | 0 |
| Concordancia (llanta/rueda frente a rodillo; del/de la) | 0 errores |
| Verificador del orquestador (`paso9_verificador.txt`, completo, `exit=3`) | oráculo 401 de 401; control sin mutar: 0 muertes en 600 semillas |
| Glifos (`validar_glifos_latex.R`) | `OK: sin glifos que rompan pdflatex` |
| Literalidad de exextra (`validar_literalidad_exextra.py --item`) | `TOTAL fallos: 0` |

**Prosa nueva de la Solution (H3 de la r1).** La frase «Cada una de las tres expresiones incorrectas de esta pregunta falla al menos una de las dos» es verdadera en las 20 ternas:
- los seis errores del banco fallan la prueba de unidades: dan cm/m², m², 1/m, m/cm, 1/m² o cm/m, nunca vueltas;
- VL-A, VL-B, VL-D y VL-F fallan además la prueba de monotonía.

Lo comprobé analíticamente para los seis códigos, sin ejecutarlo. La salvedad «Pasar las dos pruebas no basta» corrige la universalización que señaló la r1.

**H4 y H5 de la r1**, resueltos:
- H4: «la distancia, escrita en la misma unidad del perímetro, dividida entre el perímetro» ya no valida VL-E.
- H5: la atribución literal a la Guía queda solo en la canónica; en las demás versiones se habla del «error análogo».

## 2. Objeciones

### H1. Desplazamiento del canal: en `fraccion_unica` la clave es la expresión más ancha, y la batería no lo mide (DIAGNOSTICIDAD, ALTA)

**Qué se cuestiona.** En `fraccion_unica` las tres expresiones que usan K, d y la letra se escriben así (líneas 141-143):
- clave: `\dfrac{K \times d}{V}`, con dos números en el numerador;
- VL-B: `\dfrac{d \times V}{K}`;
- VL-F: `\dfrac{K}{V \times d}`.

En pantalla una fracción ocupa el ancho de su parte más ancha. Por eso la clave es estrictamente la más ancha siempre que la terna no tenga VL-G. Ejemplo real, terna VL-B+VL-D+VL-F:
- `100×500/P` (clave);
- `100/P`;
- `100/(P×500)`;
- `500×P/100`.

En código LaTeX las tres expresiones completas tienen 27 caracteres, así que la regla congelada `which.max(nchar(o))` las da por empatadas.

La tabla de pesos pone en esa forma tres celdas sin VL-G: ABF 10, BDF 10 y BDE 1, frente a BFG 7. Es decir, el 75 % de la forma por construcción.

**Medición.** Uso la misma muestra por cuotas que `verificar_vueltas.R`, con dos modelos de ancho:
- modelo 1: cada glifo vale 1;
- modelo 2: dígito 1, letra 1,2 y operador con su espaciado 2,4.

El techo nulo sale de 2000 permutaciones de la clave. Script: `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0e74dd34-847c-4a81-ab53-a25af8b1f7ff/scratchpad/detractor_r2/ancho_visual.R`, salida en `ancho_visual.txt`.

| Muestra | «La más ancha a la vista» | Techo nulo (p95) | Exceso | Clave única más ancha | Margen mediano (modelo 1 / modelo 2) |
|---|---|---|---|---|---|
| Agregado (100 no canónicas) | **67,5 %** | 24,9 % (31,5 %) | **+42,6 pp** | 60 de 100 | 50 % / 32 % |
| `fraccion_unica` (100) | **80,0 %** | 25,1 % (32,0 %) | **+54,9 pp** | 80 de 100 | 50 % / 32 % |
| `fraccion_izq` (100) | 27,0 % | 25,2 % | +1,8 pp | 0 (empate con VL-F) | — |
| `numero_izq` (100) | 27,5 % | 25,0 % | +2,5 pp | 0 (empate con VL-F) | — |
| Canónica (24 órdenes) | 50,0 % | 24,9 % | heredado del impreso (empate de B y C; H-2) | 0 | — |

- Los dos modelos dan las mismas tasas.
- El margen supera el 15 %, así que el canal es explotable (§P7-B).
- En el mismo estrato, la batería congelada da «la mas larga (chars)» 26,0 % (`paso9_verificador.txt`, línea 33) y un veredicto PASS.

**Por qué es un defecto**
- Regla #22 §P7: «una batería incompleta no mide "sin señal", mide "sin sonda"». La batería tiene la sonda de longitud, pero la calcula sobre el código fuente, que el estudiante no ve. Para opciones con `\dfrac` eso es ceguera, no ausencia de canal.
- §P7-A: obliga por encima de +8 pp, y aquí son +42,6 pp.
- Es el «ciclo de desplazamiento de canal»: la pasada 2 bajó la centralidad del 81 % al 25 %, y el atajo pasó a la anchura.
- Fuente externa: Haladyna, Downing y Rodriguez (2002), *Applied Measurement in Education* 15(3), 309-333, pauta «Keep the length of choices about equal». No la consulté en esta sesión; la norma que obliga es la del repositorio.

**Riesgo concreto.** En unas 2 de cada 3 versiones no canónicas, un estudiante que elige «la expresión más grande» acierta sin modelar. Es más que el máximo de cualquier regla del ALCANCE, y el verificador declara «0 rojos sin alcance».

**Alternativa (pasada 3 de 3, §P7-D).** Medí todo por celda con N = 100 y las 34 reglas: B2 sin las de posición ni K/V, más el vector de la r1. En todos los casos sustituí la longitud en caracteres por la longitud visual (`celdas_vis.csv`, `celdas_div.csv`).
- **Opción 1, con las mismas tres formas.** Volver a correr `pesos_lp.py` con la columna visual y las mismas restricciones da un máximo agregado de 37,9 % (38,5 % redondeado a 40 partes), frente al 67,5 % actual (`lp_visual.txt`).
- **Opción 2, con una forma no explorada que llamo `division`:**
  - clave `d \div \dfrac{V}{K}`;
  - VL-B `d \times \dfrac{V}{K}`, que es literalmente la B de la Guía;
  - VL-F `\dfrac{K}{V} \div d`;
  - VL-G `K \times d \div \dfrac{V}{K}`.

  El programa lineal conjunto (`lp_conjunto.py`), con un tope de 70 % por contenido de la clave para la sonda H3b y las demás restricciones del orquestador, da:
  - máximo agregado 35,3 %;
  - tope por forma 40 %;
  - K/V 13,3 %;
  - formas: `fraccion_izq` 30 % y `division` 70 %;
  - errores: VL-A 10 %, VL-B 61 %, VL-D 56 %, VL-E 39 %, VL-F 100 %, VL-G 34 %.

  Además, esa clave es el «cálculo directo» que recomienda la propia Solution.
- Son estimaciones por celda. Antes de sellar, hay que validarlas con el verificador sobre el diseño real, añadir la anchura visual a la guarda «diagnosticidad: tabla de pesos» con su mutante, y comprobar que la clave sigue siendo verdadera.

**Encuadre procedimental (§P7-C).** El canal apareció a mitad de ciclo. Que obligue en este ciclo lo decide el profesor, igual que en la r1. Con estas cifras, la FASE 3 queda bloqueada hasta que decida.

**Veredicto**: MODIFICAR.

### H2. El «canal estructural» de las formas `izq` está mal demostrado, y a firma va una regla que el estudiante no ve (DIAGNOSTICIDAD, MEDIA)

**Qué se cuestiona.** La cabecera (línea 25) y el pedido de firma dicen: «o la clave es la central, o es la más larga en el código LaTeX». El ALCANCE incluye «la mas larga (chars)» con 0,5405 y 0,5505 (líneas 22-23).

**Lo que medí**
- Con la longitud en caracteres, el piso por forma es 50,0 % en las dos formas `izq`, y lo sostienen a la vez varias reglas: centralidad, medoide, «usa los tres datos», «la más larga» y, en `numero_izq`, «el número menor». Eso confirma la cifra del orquestador con esa vara (`lp_div.py`).
- Con la longitud visual, el piso de las dos formas `izq` es 40,0 %, con BFG 0,4 + DEF 0,6. Lo sostienen la centralidad, el medoide, «usa los tres datos» y «contiene K y d».
- «La más larga» a la vista da 27 %. No es canal.
- El programa lineal solo exploró las formas de la línea 153. `division` tiene un piso de 33,9 % (H1).
- §P7-F pide enumerar el espacio de diseño antes de declarar un canal estructural.

**Riesgo.** El profesor firmaría:
- una regla de 54-55 % con «margen 8 %» que en pantalla es un empate (margen visual 0);
- un piso de 50 % que, medido sobre lo que ve el estudiante, es 40 %;
- y no firmaría el canal real de H1.

**Alternativa**
- Quitar «la mas larga (chars)» de las formas `izq` del ALCANCE, o firmarla declarada como artefacto del código fuente.
- Añadir la anchura visual por forma y en la canónica (50 %, H-2).
- Corregir el comentario de las líneas 23-25.
- Rehacer el pedido de firma regla por regla después de la pasada 3.

**Procedencia, sin objeción.** `pesos_celdas.log` registra el md5 `a38fb7…`, pero repetí `pesos_celdas.R` sobre `43897666…` y el CSV sale idéntico (diferencia máxima 0,0).

**Veredicto**: MODIFICAR.

### H3. Contrato de mutación: la obligación 7 está mal etiquetada y faltan mutantes (testing, MEDIA)

- **Etiqueta equivocada.** M8a y M8b tumban cuatro componentes cada uno: el agregado y las tres formas. Aun así salen `cazado_por_su_sonda`, porque la línea 171 compara solo el nombre del atajo, no el nivel. Ningún mutante mueve solo un componente por forma:
  - M9 mueve centralidad y medoide de `fraccion_izq` juntos (`_y_otra`);
  - `numero_izq` y `fraccion_unica` no tienen un mutante propio.
- **Mutantes del alcance.** V1 y V2 solo clasifican `ROJAS` (la batería, línea 97). Los rojos del vector del detractor (`RD`) no tienen mutante.
- **Cobertura mínima.** Estas guardas no tienen mutante, y el verificador no las declara `sin_prueba_de_deteccion`:
  - opciones distintas;
  - precondiciones;
  - número entero de vueltas;
  - errata 101/120;
  - pregunta, opciones y clave de la canónica;
  - textos sin NA;
  - signo menos tipográfico;
  - perímetro de referencia del diseño.

**Alternativa**: veredicto por par (nivel, atajo), un mutante por componente por forma, un mutante V3 sobre `RD`, y la lista explícita de lo que queda `sin_prueba_de_deteccion`.

**Veredicto**: MODIFICAR.

### H4. El corrector de ortografía sale con EXIT 1 por un comentario (BAJA)

`corregir_ortografia_espanol.R` marca en la línea 291 «MAS → más». Es un comentario que cita el nombre ASCII de la regla, y el gate del pre-commit rechazaría el commit.

La alerta de la línea 258 («cuanto más largo …, menos») es un falso positivo: el correlativo no lleva tilde.

**Alternativa**: escribir «la que más comparte» en el comentario.

### H5. VL-F aparece en el 97,4 % de las versiones (variedad; transparencia ante el profesor, BAJA)

Medido en 2000 semillas:

| Error | % de las versiones no canónicas |
|---|---|
| VL-A | 32,5 |
| VL-B | 85,6 |
| VL-D | 43,5 |
| VL-E | 9,4 |
| VL-F | 97,4 |
| VL-G | 31,5 |

«Se sortean 3 de un banco de 6» (`ITEM_OFICIAL.md`, línea 171) es en la práctica «VL-F más 2 de 5». Al profesor solo se le informó del coste en VL-E y VL-G.

No lo considero un defecto de diagnosticidad: descartar VL-F mide el constructo. Pero hay que mostrarlo con su cifra en la pausa 3. Las alternativas de H1 lo dejan en 100 %.

## 3. Respuestas al encargo

- **Celdas, mezclas o formas no exploradas**: sí. La forma `division` y la medición visual; ver H1 y H2.
- **El agregado**: el cálculo de la batería es correcto. Repetí el CSV y el verificador tiene salida completa. Pero tiene dos limitaciones:
  - es ciego a la anchura (H1);
  - excluye la canónica. Con su peso del 10 %, «usa los tres datos» sería ≈ 0,9 × 34,4 + 0,1 × 50,0 = 36,0 %, es decir +8,2 pp sobre 27,8 %. No lo objeto, porque la canónica se firma aparte, pero hay que declararlo.
- **El ALCANCE**: cubre exactamente los 13 rojos que se midieron, y V1 y V2 los sacan del «cubierto». No cubre el canal que no se midió (H1).
- **Diversidad sustantiva**: 63 claves distintas de 100; 268 versiones únicas de 300.

## 4. Cobertura de los dominios

| Dominio | Estado |
|---|---|
| Código R/exams | F1 y F5 correctos; guardas activas. H3 |
| Pedagógico | H3, H4 y H5 de la r1 resueltos |
| Visual | Sin figura. No inspeccioné capturas: la anchura se midió con un modelo geométrico (dos variantes) |
| Gramática | Concordancia correcta en 2000 versiones. H4 |
| Coherencia matemática | 0 fallos |
| ICFES y exextra | 0 fallos |
| Testing | H3 |
| Semántico | El diagnóstico coincide con las opciones mostradas |
| Invariantes locales (v1.5) | Comentario de cabecera (H2); no hay `.claude/` propio |

## 5. Conclusión

**APROBAR CON CAMBIOS.** No hay hallazgos de CORRECCIÓN. Cambios necesarios:
1. Llevar H1 al profesor (§P7-C), en lenguaje llano: «en 2 de cada 3 versiones acierta quien elige la expresión más grande». Si decide corregir, aplicar la pasada 3 con la longitud visual en el vector y en la guarda; la forma `division` es opcional.
2. Rehacer el ALCANCE y el pedido de firma (H2) después de esa pasada.
3. Completar los mutantes (H3).
4. Corregir la línea 291 (H4).
5. Informar la cifra de VL-F (H5).

La FASE 2C sigue abierta para el md5 que salga de la pasada 3.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:141 | fraccion_unica = list(C = function(k, n, v) fr(paste0(k,
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:142 | B = function(k, n, v) fr(paste0(n,
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:143 | F = function(k, n, v) fr(k, paste0(v,
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:160 | "VL-B+VL-D+VL-F|fraccion_unica" = 10
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/bateria_referencia_icfes.R:47 | function(o) which.max(nchar(o))
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/paso9_verificador.txt:29-33 | fraccion_unica (n = 100): veredicto PASS
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:25 | o la clave es la central, o es la más larga en el código LaTeX
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/verificar_vueltas.R:22 | regla = "la mas larga (chars)", tasa = 0.5405
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/verificar_vueltas.R:23 | regla = "la mas larga (chars)", tasa = 0.5505
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:153 | ESTILOS <- c("fraccion_izq", "numero_izq", "fraccion_unica")
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/pesos_lp.txt:78 | Etapa por estrato: tope condicional mínimo por forma 50.00
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/verificar_vueltas.R:171 | otras <- setdiff(unique(unlist(lapply(lista
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/paso9_verificador.txt:70-80 | M8a | sonda esperada: centralidad | cazado_por_su_sonda
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/verificar_vueltas.R:97 | v1 <- clasificar(ROJAS, alc_v1); v2 <- clasificar(ROJAS, alc_v2)
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:326 | f_est <- c("centralidad", "medoide")
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/verificar_vueltas.R | AUSENTE: sin_prueba_de_deteccion
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:291 | que MAS comparte» de la batería congelada
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/pesos_lp.txt:102 | error VL-F en 97.5
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/ITEM_OFICIAL.md:171 | se sortean 3 de un banco de 6
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
