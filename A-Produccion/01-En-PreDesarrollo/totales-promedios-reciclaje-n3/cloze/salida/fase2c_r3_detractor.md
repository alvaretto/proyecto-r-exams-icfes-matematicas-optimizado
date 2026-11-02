# FASE 2C r3: detractor del CLOZE `totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1`

**Fecha:** 2026-10-31.
**Artefacto:** `.Rmd` con md5 `f98f9dda47e4f3cafc4e8b4e82cc956b`. Lo comprobé y coincide con HEAD `e11dc058`.
**Independencia:** soy un detractor nuevo. No escribí ni corregí nada de este ejercicio y no participé en r1 ni en r2.
**Objeciones:** 3, ninguna crítica ni alta: 2 medias y 1 baja. Ninguna es de CORRECCIÓN.

## Resumen ejecutivo

No encontré ningún defecto de CORRECCIÓN en las Partes 2 a 6. Lo comprobé con código propio sobre las semillas 1:100, compilando el `.Rmd` de verdad con `knitr::knit` y releyendo la tabla del otro colegio tal como se muestra:
- **Parte 2:** solo la clave es verdadera, con cualquier lectura razonable de cada opción.
- **Partes 3 a 5:** los valores numéricos son correctos.
- **Parte 6:** los veredictos que calculé desde la tabla coinciden con `sol_p6`.
- **Retroalimentación y Solution:** están alineadas con la marca, y la aritmética y las comparaciones de todas las explicaciones son correctas.

También reproduje con código propio estas cifras, sin diferencias:
- E1, en 120 versiones, 29 de ellas canónicas.
- La conservación del RNG frente a `898608dd`.
- L-C6 exacta: P2-R1 +13,6, P2-R2 +21,9 y P2-R3 +7,5 pp.
- La lista de L-C7: 216 reglas, cada una igual a su cifra medida.
- L-C2 exacta: las 9 celdas, más el desglose por clave.
- X6 y X10 en la canónica.

Los tres hallazgos:
- **H1:** «exacta» en L-C2 significa exacta sobre el sorteo de la Parte 2, pero muestral sobre la Parte 1. Entre muestras disjuntas de N = 100, X6 cambia tanto como el margen al umbral. Leída clave por clave, pasa de +8 pp también en la rama CURSOS, cosa que el registro no dice.
- **H2:** ningún mutante prueba la segunda mitad de la decisión «Va» 2: una regla de la lista que pase de su propia cifra sin llegar al tope de su estrato.
- **H3:** la Solution de la Parte 6 explica las hipótesis con un «solo si» en subjuntivo, que da solo la condición necesaria y en el caso verdadero se lee como contrafáctica.

## Tabla de hallazgos

| ID | Severidad | Tipo | Cifra exacta y fuente | ¿Bajo override? |
|---|---|---|---|---|
| H1 | MEDIA | DIAGNOSTICIDAD (precisión de una cifra que va a firma) | **X6 por rama**, en 5 réplicas disjuntas de N = 100 (mi código):<br>CURSOS: +1,3, +6,7, +3,0, +7,3 y +3,0 pp.<br>PROMEDIO: +6,0, +5,7, +7,0, +2,0 y +5,0 pp.<br>**X6 de las claves 1 y 2:**<br>CURSOS: +2,0, +10,0, +4,5, +11,0 y +4,5.<br>PROMEDIO: +9,0, +8,5, +10,5, +3,0 y +7,5.<br>**Clave 3:** +0,0 en todas.<br>La réplica 1 reproduce exactamente al verificador. | No: X6 fuera de la canónica no tiene override (L-C3 solo cubre la canónica). Leída por clave, queda pendiente de firma. |
| H2 | MEDIA | Otro: la guarda de L-C7 no tiene mutante para una de sus dos cláusulas | La cláusula se aplica a 32 reglas de la lista (8 por estrato) cuya cifra firmada es la mitad del tope: +12,5 contra 25,0 (CURSOS y PROMEDIO), +8,2 contra 16,4 (*k*′ = 1) y +8,4 contra 16,7 (*k*′ ≥ 2). Fuente: la salida `va` cruzada con el TSV. | Dentro de L-C7 (decisión «Va» 2). |
| H3 | BAJA | Otro: precisión de la Solution, que no afirma nada falso | En las 100 semillas, la explicación general de la Parte 6 usa «solo si … fuera mayor», cuando la relación es «si y solo si». | No aplica. |

## H1: L-C2 «exacta» lo es solo sobre la Parte 2, y el desglose por clave no es solo de la rama PROMEDIO

**Qué se cuestiona.** `medir_exacta_p2()` integra exactamente las 30 combinaciones de la Parte 2, pero sobre **una sola muestra** de Partes 1 por estrato:
- mezcla: semillas 1:100;
- CURSOS y PROMEDIO: las primeras 100 no canónicas de cada rama desde la semilla 1001.

El registro de la tanda «Va» declara «X6 por clave: … en CURSOS +2,0 / +2,0 / +0,0; en PROMEDIO +9,0 / +9,0 / +0,0» como si fueran cifras exactas. La regla X6 lee solo palabras de las opciones de la Parte 1, así que su valor depende de qué moldes y contextos de la Parte 1 salieron en la muestra.

**Medido con código propio** (`x6_replicas.R`):
- **Reproducción.** Mi implementación da exactamente lo del verificador.
  - Mezcla: X5 −4,2, X6 −0,1 y X10 −7,3, con X6 de +1,0 / +1,0 / −2,2 por clave.
  - CURSOS, réplica 1: X5 −2,9, X6 +1,3 y X10 −4,0 (+2 / +2 / 0 por clave).
  - PROMEDIO, réplica 1: X5 −6,4, X6 +6,0 y X10 +2,7 (+9 / +9 / 0 por clave).
- **Réplicas disjuntas** (semillas 1001 a 2258, N = 100 cada una):
  - Por rama, X6 va de +1,3 a +7,3 en CURSOS (desviación típica entre réplicas ≈ 2,6 pp) y de +2,0 a +7,0 en PROMEDIO (≈ 1,9 pp).
  - Por clave, las claves 1 y 2 pasan de +8 pp en 2 de 5 réplicas en CURSOS y en 3 de 5 en PROMEDIO.
  - Con las 500 juntas (solo depuración; la regla #23 obliga a reportar las réplicas de 100): CURSOS +4,3 (claves 1 y 2: +6,4) y PROMEDIO +5,1 (+7,7).
  - X5 y X10 quedan por debajo de +8 en todas las réplicas (X10 llega como máximo a +6,0).

**Por qué importa.**
- **Fuente:** regla #22 §P7-E, aclaración iii ampliada: toda cifra que se lleve a firma sale del código «con todos sus filtros». La decisión «Va» 1 deja pendiente la lectura clave por clave («Si el profesor quiere leerla clave por clave, la decisión vuelve a él»).
- **El profesor decidiría con datos incompletos.** Si lee clave por clave, solo tiene la cifra de PROMEDIO, de una sola muestra, y el dato de CURSOS (+2,0) es el más bajo de cinco réplicas.
- **Leído por rama, el veredicto «ok» se mantiene en las 5 réplicas** (máximo +7,3). Pero el margen al umbral (2,0 pp en la cifra declarada) es del orden de la dispersión entre muestras.
- **Contexto que el profesor debería tener:**
  - Por clave dentro de una rama es un estrato de diseño: el alumno ve la clave de la Parte 2, pero no puede ver la rama sin calcular.
  - Lo que el alumno puede aprovechar es la mezcla por clave: +1,0 / +1,0 / −2,2 en la muestra del verificador, y alrededor de +4 pp según mi estimación con las 500 (la canónica, con X6 = 0 %, pesa 0,10 × −25 pp).

**Alternativa propuesta.**
1. Llevar al profesor X6 de las claves 1 y 2 **en las dos ramas**, con las cinco réplicas: CURSOS de +2,0 a +11,0 y PROMEDIO de +3,0 a +10,5. Si quiere una cifra exacta, X6 solo lee letras, así que se puede enumerar el espacio discreto de textos de la Parte 1 (contexto × molde × sede ganadora × orden). Eso es la excepción de enumeración exhaustiva de la regla #23.
2. En E3, rotular la cifra como «exacta sobre la Parte 2, muestral sobre la Parte 1 (N = 100)». Además, imprimir el desglose por clave en cada rama como NO CONCLUYENTE cuando quede a menos de una dispersión de réplica de +8 pp.
3. El canal lo lleva una palabra de las claves 1 y 2, no de la 3: las réplicas dan siempre X6 igual en las claves 1 y 2 y +0,0 en la 3. Por eso un eventual cambio de redacción es localizable. Pero §P7-D está agotado, así que esto es solo para declarar el residuo.

**Veredicto:** MODIFICAR la declaración, no el `.Rmd`.

## H2: el mutante MC38 solo prueba la mitad «fuera de la lista» de L-C7

**Qué se cuestiona.** La decisión «Va» 2 exige dos cosas. Queda pendiente una regla «que no esté en la lista, **o que pase de su cifra**, … aunque no llegue a la cifra del estrato». La guarda implementa las dos cláusulas en `p6_token_juzgar()` (línea 1036). Pero ningún mutante prueba la segunda:
- MC38 selecciona solo reglas «justamente», que están fuera de la lista.
- MC36 («duda») está fuera de la lista y además en la mezcla, cuyo tope es 0,0, así que también lo atraparía el juez anterior, el de estrato.
- En la corrida base, cada una de las 216 reglas de la lista sale exactamente igual a su cifra (cotejado: 0 diferencias). La cláusula no se ejerce nunca.

**Riesgo concreto.** Si se borrara `round(ex, 1) <= f + 1e-9 &`, el verificador seguiría en verde. Entonces, cualquiera de las 32 reglas cuya cifra firmada es la mitad del tope podría subir hasta ese tope (+25,0 en las ramas; +16,4 y +16,7 en *k*′) sin firma nueva. Es justo lo que la decisión «Va» 2 quería cerrar.

**Fuentes:**
- Contrato de mutación del subproyecto (leyes L-C: «cada una tiene su guarda ejecutable y un mutante que la mata por su propia sonda»).
- La doctrina del repositorio «una invariante sin guardia no es una invariante».
- DeMillo, Lipton y Sayward, «Hints on Test Data Selection», *IEEE Computer* 11(4), 1978: un mutante que nadie mata señala código sin probar.

**Alternativa propuesta.** Una sonda directa en E8, al lado de MC38:

```r
mitad <- FIRMA_P6_PALABRAS[FIRMA_P6_PALABRAS$firmado_pp < unname(FIRMA_P6_TOKEN[FIRMA_P6_PALABRAS$estrato]) - 1e-9, ]
f39 <- VT7[clave_fila(VT7) == clave_fila(mitad[1, ]), , drop = FALSE]
f39$exceso <- (mitad$firmado_pp[1] + 1) / 100      # sube 1 pp sobre su cifra, sigue bajo el tope del estrato
ok(p6_token_juzgar(f39) == "pendiente" && p6_token_juzgar_estrato(f39) == "cubierto",
   "MC39 (L-C7, «o que pase de su cifra»): una regla de la lista por encima de su cifra y bajo el tope queda pendiente")
```

Lo ideal es completarla con un mutante de texto que suba de verdad una de esas 32 reglas.

**Veredicto:** MODIFICAR (es el verificador; el `.Rmd` no cambia).

## H3: «solo si … fuera mayor» en la Solution de la Parte 6

**Qué se cuestiona.** El texto emitido es, por ejemplo (semilla 2): «el grado sexto habría ganado con un curso más solo si su promedio por curso, 35 kilos, fuera mayor que la diferencia, 15 kilos». Hay dos problemas:
- **Solo da la condición necesaria.** La relación real es «si y solo si» (*a*′_P > *D*′ ⇔ *T*′_P + *a*′_P > *T*′_G). Con «solo si», de 35 > 15 no se deduce que hubiera ganado.
- **El subjuntivo confunde.** En los casos verdaderos (35 > 15), «fuera mayor» se lee como que no lo es.

No es falso, y la retroalimentación de cada opción sí justifica cada veredicto con sus números.

**Alternativa propuesta.**

```r
cat("… Por eso el grado ", per_par, " habría ganado con un curso más exactamente cuando su promedio por curso supera la diferencia: ",
    dp$aP, ED, "kilos frente a ", dp$D, ED, "kilos, así que ", if (v_hp) "habría ganado" else "habría seguido perdiendo", "; …")
```

Y lo mismo con HG. La Solution no la ve el alumno mientras responde, así que el cambio no toca la batería ni consume pasada de §P7-D.

**Veredicto:** MODIFICAR (opcional).

## Lo que verifiqué sin hallazgo

1. **Parte 2, tras la tanda 8** (solo el tipo «promedio»):
   - Mi función de verdad, propia, prueba todas las lecturas: «al cuadrado» como *T*/*n*² y como (*T*/*n*)², y «luego entre dos» como *T*/*n*/2 y como *T*/(*n*/2). En las 100 semillas y para los dos grados, solo la clave da *a*, y la da con todas las lecturas.
   - Prueba analítica sobre todo el espacio paralelo (*n* de 3 a 10, *a* de 20 a 60): *T*/*a* = *n* ≠ *a*; *a*/2 ≠ *a*; *a*/*n* ≠ *a*; *a*² ≠ *a*; *na*/(*n* + *n*₂) < *a*; el testigo de la moda da *a* − 1.
   - `feedback_p2` está alineado con `sol_p2`, y cada «Falso.» lleva el `expl2` de su propio distractor.
   - Lo que dicen `clave2_expl` y los cinco `expl2` es correcto. La redacción actual de la moda («si más de la mitad recogió lo mismo, es la moda, y puede ser distinto del promedio») cierra el BAJA anterior: la condición vuelve verdadera la frase.
   - `exsolution` está bien.
2. **Tipo retirado:**
   - Ni la Solution ni el texto visible lo nombran ni dependen de él. `tipo_p2` solo se asigna. En el XML de Moodle de la tanda 8 no aparece «Comparaciones que no deciden», «qué se compara» ni «cotej».
   - `invisible(sample.int(2L, size = 1L))` no deja nada visible y consume el RNG igual que el `sample(names(P2), 1L)` de `898608dd`. Comparé las dos versiones en las semillas 1:100 y hay 0 diferencias en: la tabla del otro colegio, los textos y claves de las Partes 3 a 5, opciones, marcas y retroalimentación de la Parte 6, posición de la clave de la Parte 2 y `.Random.seed` final. Además, las 41 semillas que antes eran «promedio» tienen ahora opciones y retroalimentación de la Parte 2 idénticas.
3. **L-C2:** las 9 celdas y el desglose por clave, reproducidos exactamente (ver H1). Lo declarado es lo firmado:
   - X5, X6 y X10 se integran sobre las 30 combinaciones; para X6 y X10 integrar sobre los tríos no cambia nada.
   - Más de +8 pp da «pendiente» (`veredicto_cruzado`).
   - La única desviación, E3 en lugar de E8, está documentada.
4. **L-C6:** con 720 versiones exactas y mi código, P2-R1 da +13,6, P2-R2 +21,9 y P2-R3 +7,5 pp. Son las cifras firmadas y declaradas, y las mismas en `898608dd` y en `f98f9dda`. El desglose por clave de las reglas de la Parte 2 no es un estrato que el alumno pueda ver: mientras responde no sabe cuál es la clave.
5. **L-C7:**
   - La lista tiene el md5 `2aa8e742…` que comprueba el código y 216 reglas: en cada estrato, 46 en el tope y 8 en la mitad, ninguna en la mezcla.
   - Cada cifra de la lista coincide con la medida en la salida `va` (216 reglas, 0 diferencias).
   - MC38 prueba lo que dice («justamente»: pendiente con el juez regla por regla y cubierto con el de estrato), con el límite señalado en H2.
   - MC36 y MC37 están en su sitio. MC37 juzga los tres estratos, y en CURSOS da +38,2 → pendiente.
6. **L-C5 (RL1 y RL2):** las cifras de la mezcla y de las ramas salen de la estructura del sorteo de veredictos (tres combinaciones equiprobables por rama):
   - RL2: 5/6 de aciertos frente a 1/2 → +33,3.
   - RL1 (métrica del par): (2/3 − 1/2)/2 = +8,3 en la mezcla y (2/3 − 4/9)/2 = +11,1 en cada rama.
   - Coinciden con lo firmado.
7. **E1** con mi código: 120 versiones, 29 canónicas. 0 diferencias en objetos, `opciones_p1`, `sol_p1`, `feedback_p1` y `.Random.seed` al cierre del bloque P1. El CLOZE completo no altera el enunciado, la tabla, la pregunta ni la respuesta de la Parte 1.
8. **Canónica (L-C3):** X6 da 0,0 % y X10 16,7 %, exactas sobre la Parte 2, en 30 canónicas (mi código). Coinciden con la puerta.
9. **V11 «a un paso»:** está declarado como residuo con sus cifras (mezcla 11,0 % / 49,0 %; CURSOS 12,0 / 63,0; PROMEDIO 17,0 / 52,0; canónica 16,7 / 33,3). No es una regla de selección: para aprovecharlo habría que elegir la operación correcta entre decenas, con tolerancia 0. Lo acepto como residuo.
10. **Pesos:** `expoints: 3|1|1|1|1|1` (línea 1069). E7 pasa y mata sus tres mutantes. La guarda de `exportar_moodle_cloze.R` exige esa secuencia en cada pregunta, y el XML de la tanda 8 trae `{3:MULTICHOICE_V…}` y `{1:MULTICHOICE_V…}` sin retroalimentación dentro de los gaps.
11. **Espacio duro (U+00A0)**, en 100 compilaciones con `knit`:
    - Aparece solo en la Solution y en la retroalimentación de las Partes 3 a 6.
    - No aparece en el texto visible de las Partes 2 a 6 (no lleva cifras) ni en las celdas de la tabla del otro colegio.
    - Ninguna línea de las Partes 2 a 6 tiene una cifra seguida de espacio normal y «kilos» o «cursos».
    - La Parte 1 usa espacio normal (decisión 2 a).
    - **Una precisión sobre el encargo:** en 12 de las 100 versiones, la tabla de la Parte 1 sí lleva U+00A0 en las filas «El Bosque» y «La Pradera». Viene del SCHOICE aprobado (`filas`, línea 87, literal) y es idéntico por E1. No hace daño (pandoc lo vuelve `~` en el PDF) y concuerda con el pendiente del SCHOICE, que solo habla de «fuera de la tabla». No lo cuento como hallazgo.
12. **Gramática y tildes** del texto emitido de las Partes 2 a 6 y de su Solution en las semillas 1 a 3: correctas. Metadatos: DOK 2 con Nivel 3 es coherente (la regla solo exige DOK ≥ 3 ⇒ Nivel ≥ 3).

## Lo que no verifiqué

- **Las 69 celdas de L-C5 en los estratos de *k*′, y las 268 reglas de la Parte 2 aparte de R1 a R3.** Solo las leí en la salida del verificador. Repetir la enumeración exacta de `enumerar_h2_cloze.R` no cabía en el presupuesto.
- **Las corridas de MC31 a MC35, MP1 y MP2.** Solo las leí en la salida `va`.
- **Los cuatro formatos y la revisión visual.** Fuera del alcance de esta ronda.

## Mis artefactos (fuera del repositorio)

Todos en `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/7ecb76b3-2641-4f28-a0dc-aba5add06161/scratchpad/detractor_r3/`:
- `h1_correccion.R` y `h1_correccion.txt`: barrido de corrección y de espacio duro.
- `x6_replicas.R` y `x6_replicas.txt`: L-C2 y las réplicas.
- `e1_p2r.R`: E1 propio y L-C6 exacta.
- `rng_canon.R`: RNG frente a `898608dd` y la canónica.
- `knit/s001.md` a `knit/s003.md`: markdown emitido.

## Veredicto global

**APROBAR_CON_CAMBIOS.** No hay CORRECCIÓN, y nada crítico ni alto bloquea la FASE 3.

Próximos pasos:
1. **H1:** llevar al profesor X6 por clave en las dos ramas, con sus réplicas o con la enumeración de la Parte 1, y corregir en el registro el rótulo «exacta».
2. **H2:** añadir la sonda MC39 al verificador.
3. **H3:** reescribir la frase de la Solution de la Parte 6 (opcional).

CITAS_DETRACTOR
H1 | A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/verificar_espacio_cloze.R:383 | medir_exacta_p2 <- function(L, regla, banco) mean(vapply(L, function(x) mean(vapply(combinaciones_p2(banco)
H1 | A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/verificar_espacio_cloze.R:160 | NAT <- list(); for (z in 1:N) { en <- correr(EXPR_C, z)
H1 | A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/.claude/CLAUDE.md:996 | en CURSOS, +2,0 / +2,0 / +0,0; en PROMEDIO, +9,0 / +9,0 / +0,0 pp
H1 | A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/.claude/CLAUDE.md:946 | profesor quiere leerla clave por clave, la decisión vuelve a él.
H2 | A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/verificar_espacio_cloze.R:1036 | round(ex, 1) <= f + 1e-9 & round(ex, 1) <= fe + 1e-9
H2 | A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/verificar_espacio_cloze.R:1096 | VM$veredicto == "pendiente" & VM$v_estrato == "cubierto"
H2 | A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/verificar_espacio_cloze.R:1067 | pd <- pend[grepl("duda", pend$id, fixed = TRUE), , drop = FALSE]
H2 | A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/.claude/CLAUDE.md:952 | que no esté en la lista, o que pase de su cifra, queda pendiente (exit 3)
H3 | A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd:1005 | "solo si su promedio por curso, ", dp$aP, ED, "kilos, fuera mayor que la diferencia
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
