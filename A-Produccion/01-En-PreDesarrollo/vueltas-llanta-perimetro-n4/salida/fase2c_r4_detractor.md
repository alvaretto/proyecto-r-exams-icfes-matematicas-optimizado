# Auditoría del detractor, FASE 2C ronda 4: `vueltas-llanta-perimetro-n4` (SCHOICE)

- **Fecha:** 2026-11-10
- **Artefacto:** `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd`. Comprobé en disco que su md5 es `85773027d040e2ce4b45eda03afa362c`. La copia de la r2 tiene md5 `4389766601257f9e715c274e82d66d2c` y la de la r3, `6a51d76af76a3a1352812c2adc2f8a77`; las dos coinciden con lo declarado.
- **Independencia:** soy un detractor nuevo. No escribí ni corregí el artefacto, y no soy ninguno de los detractores de las rondas 1, 2 y 3. Trabajé solo en lectura.
- **Carpeta de trabajo:** `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0e74dd34-847c-4a81-ab53-a25af8b1f7ff/scratchpad/detractor_r4/`. Ahí están `oraculo_r4.R`, sus salidas `oraculo_r4.txt` y `oraculo_r4b.txt`, `celdas_rng.R` y `enum_r4.rds`.

**Objeciones: 4.** Ninguna de CORRECCIÓN, ninguna crítica ni alta, 1 media y 3 bajas.

## Resumen

1. **CORRECCIÓN: sin hallazgos.** Lo comprobé con un oráculo propio que lee el texto visible:
   - recorrió las 1248 combinaciones del diseño, forzadas a través del chunk real;
   - más 48 canónicas y 1500 semillas reales sin tocar el código;
   - resultado: 0 problemas.
2. **Restauración fiel.** Quedó todo lo que se declaró conservar de la r3 y no queda nada que solo sirviera a las formas de la r3.
3. **La enumeración modela el generador.** Reproduje 10 reglas en 5 niveles con un camino distinto, y las 50 cifras coinciden con `r4_exacto.csv`.
4. **El alcance está completo.** Tiene las 36 reglas en rojo, cada una con su cifra exacta. El contrato de mutación se cumple; H4 es una nota menor.
5. **Las cifras de la tabla de firma y de la glosa de cabecera son correctas.** Lo que falla son tres explicaciones del texto que lee el profesor (H1) y varias precisiones menores (H3).

## 1. CORRECCIÓN (encargo a)

**Cómo trabaja el oráculo.** `oraculo_r4.R` evalúa los chunks del `.Rmd` y analiza solo lo que imprimen `enunciado`, `opciones`, `solucion` y `feedback`. El analizador de LaTeX es propio, de descenso recursivo; no es el `tex2r` del `.Rmd` ni el del verificador.

**Cobertura:**
- **Forzada.** Sustituí en memoria los cinco sorteos (`runif` de la canónica, objeto, distancia, letra y celda), así que pasan por el chunk real las 1248 combinaciones de objeto × distancia × letra × celda. A eso se suman 48 canónicas.
- **Real.** 1500 semillas con el código intacto: 144 canónicas; cm/m 616, mm/m 447 y m/km 293; P y p.

**Qué comprueba, con 0 problemas en total:**
- **Enunciado:**
  - K es la conversión entre las unidades que dice el texto (metro→centímetros 100, metro→milímetros 1000, kilómetro→metros 1000);
  - la abreviatura es la de la unidad;
  - la concordancia de género (llanta y rueda frente a rodillo) es correcta;
  - toda cifra con unidad de distancia del contexto es d.
- **Opciones:**
  - exactamente una vale K·d/P en P = 37, 211,5 y 1234, y es la que marca `exsolution`;
  - cada incorrecta corresponde a un único mecanismo del banco, identificado por su función y no por el código;
  - no hay opciones repetidas;
  - ninguna muestra «101/120».
- **Solution:**
  - la expresión de «Respuesta correcta» es la marcada, y su equivalente vale K·d/V;
  - los tres pasos del procedimiento son verdaderos, con sus ecuaciones;
  - caso específico: K·d, la división exacta, la clave con P_ref sustituido y ningún distractor igual a la clave en P_ref;
  - análisis: tres párrafos, con las expresiones mostradas como incorrectas, y en cada uno el código VL-x coincide con el mecanismo que se deduce de la expresión;
  - «K veces menor» aparece solo en VL-E y «K veces mayor» solo en VL-G, con el K correcto;
  - las fracciones K/V, V/K y K × d son correctas.
- **Feedback:**
  - «Correcta» va solo en la marcada, con sus datos;
  - en las incorrectas, la cifra en P_ref es exacta, o «aproximadamente» bien redondeada y nunca aplicada a un valor con 4 decimales o menos;
  - la razón que se da corresponde al mecanismo;
  - «No contiene el perímetro» aparece solo cuando la expresión no lo contiene.
- **Regla #19:** no aparece «opción X» ni «letra».
- **Canónica:**
  - regla y pregunta copiadas literalmente de `ITEM_OFICIAL.md`;
  - las cuatro opciones de la Guía;
  - la clave es `\dfrac{100}{P} \times 150`.

**Aviso sobre mi propio oráculo.** En la primera corrida salieron 312 + 81 «problemas». Todos venían de un falso positivo mío: la comprobación de K/V marcaba como errónea la propia expresión de VL-E (d/V). Lo corregí y repetí la corrida: 0 problemas.

**Frecuencia de las celdas.** En mi primera tanda de semillas (20001-21000) salió chi² p = 0,003. Para comprobarlo reproduje la secuencia de sorteos del generador en 100 000 semillas: p = 0,146, con frecuencias observadas iguales a la tabla de pesos hasta la tercera cifra. Fue azar de esa ventana.

**Visual.** Miré la plana 1 de `render_r4/pdf/v2.pdf` (tractor, m/km, `numero_izq`, celda B-F-G):
- las opciones y la Solution se componen bien;
- el caso específico («4 kilómetros son 4000 metros, y 4000 ÷ 5 = 800») es correcto;
- el separador `inline-block` aparece 40 veces en el XML de Moodle (4 × 10) y no se filtra al texto del PDF.

## 2. Fidelidad de la restauración (encargo b)

Comparé las tres versiones: r2 contra la actual y r3 contra la actual.

**De la r3 quedan exactamente:**
- el feedback reescrito de VL-F;
- el `<span>` de separación;
- `ANCHO_GLIFO` y `ancho_tex`;
- el crédito 1/4;
- `revisar_tabla()`, ahora con los pesos normalizados.

**Se añaden:** `TOPES` por (nivel, atajo) y el nombre del `stopifnot` de `all(PESOS > 0)`.

**Se retiran todos los elementos de la r3 que dependían de la forma:**
- `TXT_CLAVE`, `PROC_FORMA`, `B_CORTA`, `G_CORTA` y `G_LARGA`;
- la variante «Equivale a multiplicar» del feedback de VL-B;
- `arriba()`, `contiene_div` y `letra_arriba`;
- las formas `cociente`, `cociente_b` y `fraccion_nv`.

Las formas, `ESTILOS` y `PESOS` son idénticos a los de la copia de la r2. No se perdió ningún cambio de la r3 ajeno a la escritura.

Lo único que queda inerte es el crédito 1/4: con los cinco atajos actuales no se ejecuta nunca (H4).

## 3. Enumeración exacta (encargo c)

**Pesos, canónica, objetos, distancias y letras.** El verificador enumera:
- objeto uniforme entre 9;
- distancia uniforme dentro del objeto;
- letra P o p con probabilidad 1/2;
- celda con probabilidad PESOS/40;
- canónica con P_CANON = 0,10, en sus 24 órdenes.

Ese es el orden de sorteo del generador, sin otros sorteos intermedios. Mi enumeración forzada usa las opciones que produce el chunk real, no `expr_de`.

**Cifras reproducidas por mi cuenta.** Los anchos los medí con mi propia corrida de pdflatex (1182 cadenas). Todas coinciden con las de la enumeración:

| Regla | Agregado | `fraccion_izq` | `numero_izq` | `fraccion_unica` | Canónica |
|---|---|---|---|---|---|
| la más ancha (TeX) | 60,00 | 25,00 | 25,00 | 75,00 | 50,00 |
| ancho TeX (tol 1 pt) | 60,00 | 25,00 | 25,00 | 75,00 | 50,00 |
| letra abajo (conjunto) | 56,46 | 33,33 | 29,17 | 67,26 | 50,00 |
| contiene × (conjunto) | 42,29 | 66,67 | 66,67 | 31,85 | 50,00 |
| centralidad | 32,50 | 50,00 | 50,00 | 25,00 | 0,00 |
| estímulo K/V + otra operación | 30,00 | 41,67 | 41,67 | 25,00 | 100,00 |

También coinciden medoide, tres datos, «empieza por la distancia» y «única con producto».

**Otras cifras de la tabla que confirmé:**
- H3b exacta: 70,0 % de las versiones nuevas («dfrac times p»).
- Pesos de las formas: 15 / 15 / 70 %.
- Margen de anchura: la clave es la única más ancha en el 52,5 % de las nuevas, con un margen mínimo del 16,1 % sobre la más ancha (19,2 % sobre la segunda). Sostiene «en todas esas versiones el margen es de 15 % o más».
- El umbral de H3b que cita la tabla («avisa desde el 90 %») es el de `validar_diagnosticidad.R`. La cifra del 70 % que daba la cabecera de la r3 era errónea.

**Por qué las cifras de la guarda coinciden con las exactas.** No es casualidad, es estructural:
- en las formas `izq`, la clave, VL-B y VL-F tienen anchura idéntica en TeX (fracciones simétricas, y × y ÷ ocupan lo mismo);
- en `fraccion_unica`, la clave es más ancha que VL-B y VL-F porque w(K) ≥ 15 pt es mayor que w(V) ≤ 7,81 pt.

Por eso el control «guarda = exacta» da 5,55e-16.

## 4. Alcance y contrato de mutación (encargo d)

**Las 36 reglas en rojo están cableadas.** `alcance_r4.R` y las líneas 54-89 de `paso9_verificador.txt` coinciden fila por fila. Cada cifra es la exacta redondeada hacia arriba a 6 decimales.

**Sondas propias fuera de las familias:**

| Sonda | Resultado | Lectura |
|---|---|---|
| «producto arriba y la letra sola abajo» | 87,5 % en `fraccion_unica` | Es la prueba de monotonía que enseña la propia Solution, es decir, el constructo. No es residuo que firmar: va en la glosa (H1c) |
| «contiene a otra opción» | 36,95 % en `fraccion_izq` (+7,95 pp sobre el techo de V3) | Zona gris, por debajo de +8 pp |
| «la más larga» en caracteres del código | 50,0 % en las dos formas `izq` | Ver H3c |

**Contrato de mutación (Incidente P, 8 obligaciones):**
- **Obligaciones 1, 2, 4 y 5:** las 20 guardas con nombre tienen su mutante G, que declara su sonda, usa un patrón único y recorre semillas hasta la primera muerte. La única excepción es la guarda de la clave canónica, declarada `sin_prueba_de_deteccion` con una justificación válida.
- **Obligación 7:** los 20 pares (nivel, atajo) se cazan solos, mutando una copia del entorno. Los mutantes T-* salen `cazado_por_su_sonda_y_otra`, pero la «otra» es el mismo atajo en otros niveles, así que la prueba se supera.
- **Obligación 8:** el control sin mutar no deja ningún par por encima del tope.
- **Mutantes del alcance:** V1, V2 y V3 sacan una regla de «cubierto».
- **Obligación 6:** H4.

## 5. Tabla de firma y glosa de cabecera (encargo e)

**Cifras.**
- Todas las tasas, techos y excesos de `tabla_firma_p7.md` coinciden con la salida del verificador.
- Los 36 renglones son los de `alcance_r4.R`.
- Las líneas citadas son correctas, una por una: verificador 134, 138, 153-154, 158, 160-162 y 165; `reglas_r3` 22, 25, 28, 31, 33, 35 y 37; batería 47, 84 y 114.
- La cabecera del `.Rmd` (líneas 15-26) da cifras exactas: 60,0 %, 75,0 %, 56,5 %, 42,3 %, el reparto 15/15/70 y el rango del 37,5 % al 75,0 %.

**Explicaciones.** Las fallas están en el texto explicativo: H1 y H3.

## 6. Objeciones

### H1. Tres explicaciones de la tabla de firma no corresponden a la enumeración (transparencia ante la firma, MEDIA)

**a) El signo × en las formas `izq`.**
- La tabla dice: «la clave es la que lleva el signo × entre dos distractores con ÷ o sin signo».
- Eso solo es cierto en la mitad de las versiones de cada forma: las celdas A-D-F y D-E-F.
- En la otra mitad (B-F-G, con peso 3 de 6), VL-B y VL-G también llevan ×. Medido: la clave es la única con × en el 50,0 % de `fraccion_izq` y en el 50,0 % de `numero_izq`.
- Además, los distractores son tres, no dos.
- Es justo la diferencia entre el 62,5 % de «la única con ×» y el 66,7 % de «contiene ×».

**b) «Centralidad: por §P7-B es inexplotable».**
- §P7-B exime lo imperceptible, no lo contable. Su texto íntegro (línea 454) lo dice así.
- El margen del 3-9 % es el de una suma de índices de Jaccard, no una magnitud que el estudiante compare.
- La convergencia (cada distractor es la clave con un solo cambio) sí se puede contar.
- La misma señal, medida como medoide, tiene margen ≥ 15 % en el 50 % de las versiones de las formas `izq`.
- Fuente de nivel 1 sobre la estrategia de convergencia: el artículo de *Journal of Educational Measurement* citado en Fuentes.
- Riesgo: el profesor descuenta como «inexplotable» un canal del 50 %.

**c) La glosa de constructo queda a medias.**
- La r3 (H2) pidió llevar cada regla a firma «con su glosa de constructo». La tabla solo la da para la regla del estímulo.
- «Letra abajo» (56,5 % y 67,3 %) coincide con «a más perímetro, menos vueltas», la propiedad que enuncia la Solution.
- «Contiene ×» coincide con «hay que multiplicar por la distancia».
- Ancho, «empieza por la distancia», las marcas únicas, la centralidad y el medoide son rasgos de superficie.
- Para decidir, el profesor necesita esa distinción.

**Alternativa (cambia solo la tabla, no el md5):**
- Corregir la frase de a): «en la mitad de sus versiones la clave es la única con ×; en la otra mitad la llevan también otras dos, y la delata la convergencia (centralidad y medoide, 50 %)».
- Sustituir b) por «explotable: el medoide, que mide lo mismo, tiene margen ≥ 15 % en esas versiones».
- Añadir una columna «¿coincide con razonar bien?», regla por regla, e incluir la nota de la sonda compuesta (87,5 %, constructo).

**Veredicto:** MODIFICAR (la tabla).

### H2. Dos comentarios del `.Rmd` describen algo que el código no hace (invariantes locales, regla #9 v1.5, BAJA)

- **Líneas 125-127:** «En cada forma, la clave se escribe igual que los distractores completos (VL-B, VL-F, VL-G): la forma no la distingue de ellos dentro de la versión».
  - En `numero_izq`, VL-F empieza por una fracción (línea 140) mientras la clave, VL-B y VL-G empiezan por un número.
  - No puede ser de otro modo: escrito «d ÷ K/V», VL-F valdría lo mismo que VL-B.
  - Por eso allí la forma sí distingue la clave: «empieza por la distancia» 75,0 % y «única sin empieza_fraccion» 62,5 %, las dos a firma.
- **Línea 7:** da el contenido de la clave como `\dfrac{K}{V} \times d`. Eso solo vale en la canónica y en `fraccion_izq`.

**Alternativa:** reescribir los dos comentarios. Cambia el md5, así que hay que repetir el verificador y la tabla (4,7 min). Se puede posponer al próximo cambio del `.Rmd`.

### H3. Precisión de la tabla (BAJA)

**a) Versiones contadas dos veces.**
- Cada versión de la muestra del agregado se añade también a la de su forma (líneas 42-43), y el oráculo une las dos listas (66-68).
- Las «802 versiones reales» son 602 distintas: 300 por tanda más las 2 canónicas.
- Las «tandas de 400» son de 300.
- Las «800» de pertenencia son 600.

**b) Glosas fijadas en la canónica.**
- «100 dividido entre el perímetro» y «les falta el 100»: K = 1000 en el 55,6 % de las versiones nuevas.
- «La P sola»: V = p en el 50 %.
- «Los metros del recorrido»: kilómetros en 2 de los 9 objetos.

**c) Las reglas de caracteres no se informan.**
- La familia B sustituye las dos reglas de caracteres (verificador 138).
- El HANDOFF de la r3 prometía informarlas aparte, como artefacto. La tabla no lo hace.
- Medido: «la más larga (caracteres)» acierta el 50,0 % en las dos formas `izq` y el 32,9 % en el agregado.
- Solo la vería un estudiante si las fórmulas salieran en TeX crudo (celulares sin red, regla #26).

**Alternativa:**
- a) contar semillas distintas;
- b) usar glosas genéricas: «la constante del enunciado (100 o 1000)», «la letra del perímetro», «la distancia»;
- c) añadir una línea de artefacto, sin firma.

### H4. El crédito 1/4 que se conservó de la r3 no se ejecuta en la guarda (testing, BAJA)

- Con los cinco atajos, ningún conjunto queda vacío:
  - el máximo y el mínimo siempre existen;
  - la clave siempre contiene 100, 150 y P (línea 330).
- Así que la rama `if (!length(ganan)) 0.25` (línea 316) no se ejecuta nunca y ningún mutante puede probarla (obligación 6).
- Además no figura en la lista `sin_prueba_de_deteccion` del verificador.

**Alternativa:** declararla en el verificador como `sin_prueba_de_deteccion (inalcanzable con los cinco atajos)` (no cambia el md5), o retirarla en el próximo cambio del `.Rmd`.

### Observaciones sin objeción

- **Códigos VL-A, VL-B y VL-D.** En la canónica coinciden con las letras oficiales A, B y D, que el barajado cambia de lugar. La regla #19 admite identificar por código, y la expresión aparece justo después.
- **Advertencias del hook post-render.** Son 6 y son genéricas («Variable 'letra_correcta' no encontrada», «Falta: tabla de metadatos»…).
- **Sin vista previa del hook.** El hook no generó la vista previa de la FASE 2B («PDF no encontrado»), así que la evidencia visual es el paso 6b (HTML, N = 24 declarado) más la plana que miré yo.
- **HANDOFF desactualizado.** Todavía describe la r3 (líneas 109-111). Hay que actualizarlo antes de la pausa 3.

## 7. Dominios

| Dominio | Estado |
|---|---|
| Código R/exams | F1 y F5 correctos; 0 muertes en 2796 corridas; H4 |
| Pedagógico | H1c (constructo) |
| Visual | Plana del PDF correcta; separador sin filtrarse. No auditado: anchos en MathJax ni el purificador de Moodle |
| Gramática | Concordancia correcta en todas las combinaciones |
| Coherencia matemática | 0 fallos |
| ICFES (canónica) | Literal |
| Testing | H4 y H3a |
| Semántico | El diagnóstico coincide con el mecanismo en 2796 de 2796 |
| Invariantes locales | H2 |

## 8. Veredicto

No hay ningún defecto de CORRECCIÓN, y §P7-D está agotado: ninguna de estas objeciones pide otra pasada al generador. Antes de enseñar la tabla en la pausa 3:

1. Corregir H1 y H3 en `tabla_firma_p7.md`. No cambia el md5.
2. H2 y H4 son opcionales. Si se tocan comentarios del `.Rmd`, cambia el md5 y hay que repetir el verificador y regenerar la tabla.

## Fuentes

- [Converging on correct answers: A peculiarity of multiple choice items, *Journal of Educational Measurement* 19(3), 211-220 (1982), DOI 10.1111/j.1745-3984.1982.tb00129.x](https://scilit.com/publications/83573fe43543b263827fff365b1c89c6). Se suele atribuir a J. K. Smith; no confirmé el autor en esta sesión.
- [White y Zammarelli (1981), Convergence principles, *Applied Psychological Measurement*](https://conservancy.umn.edu/items/4aae3158-ea3d-4318-b011-39b100493aea)

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/tabla_firma_p7.md:102 | la clave es la que lleva el signo × entre dos distractores con ÷ o sin signo
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:135 | B = function(k, n, v) paste0(fr(v, k), " \\times ", n),
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:137 | G = function(k, n, v) paste0(fr(k, v), " \\times ", k, " \\times ", n)),
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:158 | "VL-B+VL-F+VL-G|fraccion_izq" = 3
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/tabla_firma_p7.md:96 | Por §P7-B es inexplotable, aunque figure en rojo.
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/docs/reglas/diversidad-sustantiva.md:454 | que exime lo imperceptible, no lo contable
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/paso9_verificador.txt:28 | medoide 29 % (en el 50.0 % de las versiones; 50.0 % con margen >= 15 %)
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/tabla_firma_p7.md:103 | Coincide con el razonamiento correcto
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/fase2c_r3_detractor.md:148 | llevarlas a firma regla por regla, cada una con su glosa de constructo
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/tabla_firma_p7.md:38 | Elegir una expresión con la P sola debajo de una raya de fracción
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:127 | distractores completos (VL-B, VL-F, VL-G): la forma no la distingue de ellos dentro de la versión.
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:140 | F = function(k, n, v) paste0(fr(k, v), " \\div ", n),
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/paso9_verificador.txt:79 | empieza por la distancia
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/paso9_verificador.txt:83 | marca: unica sin empieza_fraccion
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:7 | (\dfrac{K}{V} \times d); la letra no se fija
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/verificar_vueltas.R:42 | if (length(nc) < 100L) nc[[length(nc) + 1L]] <- e
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/verificar_vueltas.R:43 | if (length(porf[[e$estilo]]) < 100L) porf[[e$estilo]][[length(porf[[e$estilo]]) + 1L]] <- e
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/verificar_vueltas.R:66 | nc_de <- function(T) c(T$nc, unlist(T$porf, recursive = FALSE))
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/paso9_verificador.txt:4 | ORÁCULO de texto: 802 versiones
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/tabla_firma_p7.md:104 | y en 802 versiones reales leídas del texto
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/tabla_firma_p7.md:15 | Dos tandas de 400 versiones reales caen dentro de 1,62 errores típicos
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/tabla_firma_p7.md:58 | Buscar la expresión que contiene «100 dividido entre el perímetro»
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/tabla_firma_p7.md:59 | Descartar las expresiones a las que les falta el 100, el perímetro o la distancia
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/tabla_firma_p7.md:65 | Elegir una expresión que empieza por los metros del recorrido
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:56 | mm_m = list(K = 1000, up = "mm", up_pl = "milímetros",
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/verificar_vueltas.R:138 | if (nom(r) == "la mas larga (chars)")
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/HANDOFF.md:142 | en caracteres del código se informan aparte, como artefacto, y no se firman
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/tabla_firma_p7.md:20 | Batería congelada, con la anchura medida en la hoja y no en el código
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:316 | if (!length(ganan)) 0.25
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:330 | all(c("100", "150", "P") %in% t)
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/paso9_verificador.txt:122 | sin_prueba_de_deteccion: «canónica: clave en el contenido de la C oficial»
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
