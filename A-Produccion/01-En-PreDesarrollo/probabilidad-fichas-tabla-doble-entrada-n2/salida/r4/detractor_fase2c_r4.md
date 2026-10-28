# Auditoría del detractor, FASE 2C (r4): `probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd`

**Fecha:** 2026-10-27
**Artefacto auditado:** md5 `bc4b370bb4bf1fd2f658999988aa85f4`. Lo comprobé al empezar y al terminar, y no cambió.
**Independencia:** no escribí ni corregí este ejercicio y no audité ninguna versión anterior.
**Modo:** solo lectura. No edité ningún archivo del repositorio. Mis scripts están en `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0569ece5-0795-4f06-b915-ded47d71f2a8/scratchpad/detractor_r4/`: `sweep.R`, `canal_estimulo.R`, `espejo_cru.R` y `ejemplo.R`.

**Objeciones:** 4. Ninguna es de CORRECCIÓN. Hay 1 ALTA (diagnosticidad), 1 MEDIA y 2 BAJA.

## Resumen

- **La clave es correcta y única en todas las ramas.** La Solution y el feedback no afirman nada falso. Lo comprobé con un barrido propio de 2.000 semillas, de las que 203 son canónicas. El conteo es independiente: reconstruyo la tabla a partir del texto alternativo y analizo el texto de cada opción, sin usar `pares` ni `w`. Resultado: 0 fallos.
- **Los cuatro cambios que pidió r3 están aplicados y no rompieron nada.** Ningún cambio consume números aleatorios, así que las versiones no canónicas son idénticas a las de r3.
- **Lo nuevo y grave (H1):** existe un canal de eliminación **en el estímulo**, que la batería §P7 no puede ver porque solo mira las opciones. La regla es: «descarta la pareja cuyos dos tipos tienen una celda de igual valor».
  - La clave **nunca** tiene esa coincidencia, por razones estructurales.
  - Las trampas PROB-UNA y PROB-CRU la tienen siempre. El espejo también la tiene cada vez que aparece PROB-CRU.
  - La regla acierta el **66,6 %** en las versiones no canónicas, **+41,6 pp** sobre el azar. Acierta el **100 %** en 1 de cada 3 versiones.
  - La propia Solution enseña esa regla: «Desconfía de las coincidencias de una sola celda».
- **H2:** el mismo mecanismo hace que, en el 100 % de las versiones con PROB-CRU, el espejo sea **también** una trampa PROB-CRU. Sin embargo, la Solution lo diagnostica solo como PROB-SIM.
- **H3 y H4** son de guarda y de registro.

## Estado de los hallazgos de r3

| r3 | Estado en r4 | Evidencia |
|---|---|---|
| H1 (n/N con dos extracciones) | **Resuelto** | El diff toca el procedimiento, la conclusión, el caso específico, `explica_cer` y el feedback de la clave. En mi barrido, las **8.798 fracciones** emitidas tienen «extracción» o «extraída» en su misma línea, con 0 excepciones. El denominador siempre es N y el numerador siempre es la cuenta de un tipo |
| H2 (vocabulario de agregación) | **Resuelto** | La regex ampliada está en las líneas 372-373. M18 y M19 los caza su sonda. En 2.000 semillas no hay ningún aborto: la lista no da falsos positivos sobre los textos actuales |
| H3 (anclar el sorteo) | **Resuelto, con potencia limitada** | Está el ancla `prob = c(P_CANON, 1 - P_CANON)`, más la prueba binomial de E2 (11,6 %, z = +1,4). Ver la observación sobre guardas |
| H4 (registro) | **Resuelto** | `nota_tipo_metacognicion` en `generacion_rmd`; líneas «Guardia:» en I-6 (77-82) e I-7 (95-96) del CLAUDE.md local |

## Lo que verifiqué sin objeción

**Barrido propio de 2.000 semillas** (`sweep.R`). Resultado: 0 fallos y 0 abortos. Comprobé:
- una sola clave, que coincide con `sol`;
- las tres lecturas de «se eligen dos fichas» (al menos una, exactamente una, ambas): solo empata la clave;
- ninguna opción no canónica tiene celdas sueltas;
- 27.594 líneas «tipo: a + b = s», todas contra la tabla;
- «Son iguales» aparece exactamente una vez;
- los bloques de error: PROB-UNA (1.180 bloques, valores iguales), PROB-CRU (1.421 bloques, cruces iguales o celda suelta sin vocabulario de agregación), PROB-CER (1.399 bloques, diferencia de 1) y PROB-SIM (2.000 bloques);
- ningún bloque de error recae sobre la clave;
- el feedback: Verdadero o Falso y sus cifras, en las 4 opciones;
- la regla #19: 0 letras.

**Otras comprobaciones:**
- **Glifos (regla #25):** los únicos caracteres no ASCII visibles son `— « » á é í ñ ó ú`. Todos son seguros.
- **Orden de la prosa:** primero la clave y después `sort(codigos_sel)`. No sigue el orden de las opciones.
- **PDF canónico, páginas 1 y 2** (`salida/r4/preview_canonica-{1,2}.png`), leídas a ojo:
  - la tabla coincide celda a celda con `ITEM_OFICIAL.md`;
  - las fracciones 26/100, 11/100 y 10/100 aparecen con «en cada extracción»;
  - los bloques PROB-CER (C), PROB-CRU (D) y PROB-SIM (A) son verdaderos.
- **Verificador** (`salida/r4/verificar_espacio.txt`):
  - E1: 12.993.248 combinaciones sin violaciones;
  - E3: la canónica es verbatim;
  - E4: −0,6 pp en las no canónicas y +7,1 pp en la mezcla, dentro del override firmado;
  - E5: 22 de 22 mutantes cazados.
- **Hook:** 0 errores y 7 advertencias. No inspeccioné una por una las de 2B, 2E y 2F.

### Guardas, una por una (punto 2)

| Guarda | ¿Puede fallar? |
|---|---|
| I-1 (léxico de error en las opciones) | Sí (M5). Solo es de regresión: hoy las etiquetas no pueden contener esas palabras |
| I-2 (frases, celdas, enunciado simple y ponderado) | Sí (M6, M7, M11) |
| I-5 (una sola clave) | Sí (M1, M2) |
| Opciones únicas | **No**: es estructural, y el verificador lo declara así |
| I-7 R2 `rowSums(cuentas) < N` | Solo si se cambia la estructura (M3). En las no canónicas está implicada por I-8 junto con «celdas ≥ 1»: cada opción cubre 4 de las 8 celdas. Es defensa en profundidad |
| `pre_sim` | **No**: equivale a «suma distinta», que ya garantiza la guarda de I-5 de la línea 334. No está declarada como de consistencia (la de `calcula()` sí lo está) |
| `pre_una`, `pre_cru` (no canónica), `pre_cer` | Sí (M12, M17) |
| Celdas sueltas, estructural y léxica | Sí (M10a-d, M13, M14, M18, M19) |
| Pregunta literal | Sí (M15) |
| `P_CANON` | La constante, sí (M16). El sorteo, parcialmente (ver la observación) |
| «en cada extracción» (I-7) | **No existe** (H3) |

**Observación sobre `P_CANON`, sin ID.** La prueba binomial de E2, con 688 semillas y |z| ≤ 3, solo detecta proporciones fuera de [6,6 %, 13,4 %]. No detecta el cruce de la firma, que ocurre en 0,107: con p = 0,107 se espera z ≈ +0,6. La guarda que de verdad protege es el ancla textual, y un comentario que contenga esa misma cadena también la satisfaría. Es aceptable si se declara así.

## Hallazgos

| ID | Severidad | Clase | ¿Lo ve el estudiante? | Resumen |
|---|---|---|---|---|
| H1 | **ALTA** | DIAGNOSTICIDAD (+41,6 pp) | Sí | Regla de estímulo «descarta la pareja con una celda repetida»: 66,6 % de acierto, 100 % en 1/3 de las versiones |
| H2 | MEDIA | Diagnosticidad del distractor | Sí | Con PROB-CRU presente, el espejo también es una trampa PROB-CRU (602 de 602), y la Solution lo diagnostica solo como PROB-SIM |
| H3 | BAJA | Guarda | No | La invariante «en cada extracción» de I-7 no tiene guarda ejecutable |
| H4 | BAJA | Registro | No | El HANDOFF va por detrás de la reapertura 6 |

### H1. Canal de eliminación en el estímulo: «la clave es la única sin celda repetida» (ALTA, DIAGNOSTICIDAD)

**Qué se cuestiona.** La construcción de `construir_cubo()` produce una asimetría visible en la tabla.

- **La clave nunca tiene una celda repetida entre sus dos tipos.** `c_ok <- setdiff(a_ok, c(a, b))` (línea 197) excluye cc ∈ {a, b}. Como a + b = cc + d, eso también excluye d ∈ {a, b}. Además, es estructural: cualquier coincidencia en la clave (a = cc implica b = d; a = d implica b = cc) la volvería legible con una sola celda.
- **PROB-UNA y PROB-CRU tienen siempre una coincidencia**, porque es su definición (líneas 204-209 y 221-227).
- **PROB-CRU además la transfiere al espejo.** `e <- h` (línea 209) y `f <- g` (línea 227) ponen las celdas iguales en los tipos (Y,Z) = (1,1) y (2,2), que son justamente los del espejo (línea 304). Y el espejo va siempre (línea 290).

**Evidencia ejecutada** (`canal_estimulo.R`):
- Muestra no canónica por cuotas de N = 100 por estrato (600 versiones), más las 80 canónicas de esas mismas semillas.
- Regla: eliminar las opciones cuyos dos tipos comparten un valor de celda y elegir al azar entre las que quedan.
- Su techo nulo es exactamente 25 %. Es una sola regla, fijada antes de medir a partir del código y del texto de la Solution, no la máxima de una batería. Con la clave permutada, la esperanza es 1/4 por linealidad.

| Estrato | Acierto | Espejo con coincidencia | Opciones que quedan |
|---|---|---|---|
| PROB-CRU > PROB-UNA | **100,0 %** | 100 % | 1,00 |
| PROB-UNA > PROB-CRU | **100,0 %** | 100 % | 1,00 |
| PROB-CRU > PROB-CER | 57,5 % | 100 % | 1,85 |
| PROB-CER > PROB-CRU | 52,5 % | 100 % | 1,95 |
| PROB-CER > PROB-UNA | 45,0 % | 22 % | 2,54 |
| PROB-UNA > PROB-CER | 44,5 % | 24 % | 2,57 |

- **No canónica:** 66,6 %, **+41,6 pp**. El margen sobre el corte de +8 es de +33,6 pp.
- **Canónica** (el ítem del ICFES): 33,3 %, +8,3 pp. Solo A tiene coincidencia (8 «impar» = 8).
- **Mezcla con P_CANON = 0,10:** 63,3 %, +38,3 pp.
- La clave tiene una coincidencia en **0 de 600** versiones.

**Ejemplo, semilla 6** (`ejemplo.R`). Clave «negra con rombo / blanca con triángulo»: 17+10 | 13+14, sin valores repetidos. Los tres distractores comparten un valor:
- PROB-SIM: 10+5 | 5+4;
- PROB-CRU: 17+5 | 5+14;
- PROB-UNA: 10+13 | 10+4.

**Por qué no lo vio nadie.** La batería congelada (§P7-C) solo recibe el texto de las opciones. Es el punto ciego que ya está registrado: «La batería §P7 no ve el estímulo». El −0,6 pp de E4 no cubre este canal.

**Realismo, dicho explícitamente.** Para aplicar la regla hay que ubicar las dos celdas de cada tipo, que es el obstáculo de PROB-UNA. La regla se salta «sumar y comparar», que es el «¿Qué evalúa?» de la ficha, pero no el reconocimiento de que un tipo ocupa dos celdas. Quien tiene la idea errónea sigue cayendo en la trampa (elige la coincidencia), así que el ítem sigue discriminando a esos estudiantes. Quien saca provecho es el estudiante que desconfía de las coincidencias, y eso es exactamente lo que enseña la línea 742 tras cada intento en Moodle. Aunque el efecto real fuera la mitad del medido, seguiría por encima de +8 pp.

**Análisis del espacio de diseño (§P7-F).** Supongamos que además se excluyen las coincidencias casuales en el espejo y en el par PROB-CER. Los valores siguientes los derivé algebraicamente, no los medí:
- UNA + CER: 1/3, es decir +8,3 pp, el mismo nivel que el ítem oficial.
- CRU + CER: 1/2, es decir +25 pp. Es estructural, porque el cruce de PROB-CRU cae siempre en el espejo o en la clave.
- CRU + UNA: 100 %.

Con la arquitectura actual (la clave y el espejo comparten molde), PROB-CRU no puede quedar por debajo de +25 pp.

**Alternativas**, que decide el profesor por H-5, §P7-D y §P7-F:
1. **Rediseño.** Excluir las coincidencias casuales en el espejo y en el par PROB-CER, y además una de estas dos:
   - **(a)** no combinar PROB-CRU con PROB-UNA, y sacar el cruce de PROB-CRU del molde del espejo (por ejemplo, colocando el espejo en otro molde cuando aparece PROB-CRU);
   - **(b)** dejar PROB-CRU solo en la canónica. Esto toca el anclaje de I-6 fuera de la canónica.
2. **Override firmado** con la cifra medida (+41,6 pp en las no canónicas, 100 % en 1/3 de las versiones), igual que el de I-8.
3. **En cualquier caso**, y es un endurecimiento autónomo: neutralizar la línea 742, por ejemplo «Una celda repetida no decide nada, ni a favor ni en contra: suma siempre las dos celdas de cada tipo.»

La regla de parada del HANDOFF no se aplica aquí: este hallazgo cambia lo que mide el ítem, no solo una guarda.

**Veredicto:** MODIFICAR. Bloquea el paso 11 hasta que el profesor decida.

### H2. El espejo también es una trampa PROB-CRU, y la Solution solo lo atribuye a PROB-SIM (MEDIA)

**Evidencia** (`espejo_cru.R`, 1.000 semillas):
- En las versiones con PROB-CRU, el espejo cumple el patrón de `pre_cru` (línea 101), celdas cruzadas iguales, en **602 de 602**.
- Sin PROB-CRU, lo cumple en 52 de 285.

Así, en dos tercios de las versiones no canónicas, quien tiene la idea errónea «compara celdas cruzadas» encuentra **dos** opciones que la confirman. Si elige el espejo, la Solution le dice «Elige la pareja que se ve simétrica a la correcta, sin contar sus fichas» (línea 143), que no es la lectura que hizo él.

No es CORRECCIÓN: ninguna frase es falsa. Pero ese distractor deja de corresponder a un solo error.

**Alternativa.** Se arregla de raíz con el rediseño de H1. Mientras tanto, como endurecimiento autónomo: si el espejo tiene un cruce igual, `explica_sim` añade «Además, dos de sus celdas que se cruzan valen lo mismo (v y v): quien compara celdas cruzadas sin sumar también puede elegirla.»

**Veredicto:** MODIFICAR.

### H3. «En cada extracción» es una invariante sin guarda (BAJA)

El CLAUDE.md local (líneas 92-93) dice que toda cifra n/N «dice «en cada extracción» o «una ficha extraída»». Ni el `.Rmd` ni `verificar_espacio.R` lo comprueban: la cadena «extracci» aparece 0 veces en el verificador. Hoy se cumple (8.798 de 8.798), pero si alguien lo quita del feedback, nada lo avisa. Es la misma clase de defecto que los Errores 35-37.

**Alternativa.**
- Añadir en E3 o en E5 una comprobación que renderice la Solution y el feedback y exija «extracci|extraída» en la línea de cada `\frac`.
- Añadir un mutante que lo retire del feedback de la clave.

### H4. El HANDOFF va por detrás de la reapertura 6 (BAJA)

La fila r4 del historial está al día, pero estas secciones no:
- la tabla «Estado» dice «3-6 ✅ (reapertura 5) | `salida/r3/`» y «Detractor nuevo sobre r3»;
- la sección se titula «Cambios de código de r3»;
- dice «20/20 cazados», cuando ya son 22.

El propio archivo dice que, si discrepa, manda el JSON, y el JSON sí está al día.

**Alternativa:** actualizar las secciones Estado y Cambios de código a r4.

## Respuestas al encargo

1. **Corrección:** la clave, la Solution, el feedback y los distractores son verdaderos en la canónica y en las no canónicas con cada par de errores. La lectura de «dos fichas» quedó bien acotada. La regla #19, el orden de la prosa y los glifos están bien.
2. **Guardas:** ver la tabla. `pre_sim` y la de opciones únicas no pueden fallar. La de R2 solo falla si se cambia la estructura. «En cada extracción» no tiene guarda (H3).
3. **Registro:** las firmas del profesor (96,8 %, C → PROB-CER, P_CANON ≤ 0,10, ancho de la tabla y TipoMetacognicion) coinciden con el código y con el JSON. El HANDOFF va por detrás (H4).
4. **Severidad:**
   - No hay CORRECCIÓN.
   - **H1 es DIAGNOSTICIDAD con +41,6 pp de exceso** (margen de +33,6 sobre el corte de +8). Obliga según §P7-A, salvo override firmado.
   - Lo que ve o aprende el estudiante: H1 y H2. Lo que solo endurece guardas o registro: H3 y H4.

**Dominios cubiertos:** código R/exams, coherencia matemática, semántico, pedagógico, ICFES metacognitivo, testing y gramática (por el conjunto de caracteres). Lo visual lo cubrí parcialmente: páginas 1 y 2 del PDF canónico. No revisé a ojo el HTML a 360 px ni el DOCX ni el NOPS.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:197 | c_ok <- setdiff(a_ok, c(a, b))
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:209 | e <- h <- pick(r_resto)
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:227 | f <- g <- pick(r_resto)
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:290 | el espejo de la clave va siempre
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:304 | espejo = list(tp(c(2, 3), c(1, 1)), tp(c(2, 3), c(2, 2)))
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:742 | Desconfía de las coincidencias de una sola celda
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/verificar_espacio.R | AUSENTE: coincid
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:101 | x$c1[1] == x$c2[2] || x$c1[2] == x$c2[1]
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:110 | Esta pareja cruza las mismas características que la correcta
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:143 | Elige la pareja que se ve simétrica a la correcta, sin contar sus fichas.
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/.claude/CLAUDE.md:92 | toda cifra n/N de la Solution y del feedback dice «en cada extracción»
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/verificar_espacio.R | AUSENTE: extracci
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/HANDOFF.md:52 | Detractor nuevo sobre r3
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/HANDOFF.md:36 | ## Cambios de código de r3
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/HANDOFF.md:44 | 20/20 cazados por su sonda
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS