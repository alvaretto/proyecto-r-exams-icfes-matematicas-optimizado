# Encargo: cablear dos decisiones firmadas en `verificar_espacio_cloze.R`

Eres un programador de R. Trabajas en el repositorio `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams`. Tu
directorio de trabajo es `A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/` (lo llamo `cloze/`). Escribe en
español y con tildes, también en los comentarios del código.

## Reglas

- **Editas un solo archivo: `cloze/verificar_espacio_cloze.R`.** No toques el `.Rmd`, `firma_p6_palabras_f98f9dda.tsv`, el
  `HANDOFF.md`, `ejercicio_state.json` ni ningún otro archivo. No hagas commit.
- Imita el estilo del archivo: comentarios `##` en español, las funciones `ok()`, `falla()`, `pendiente()`, `firmado()` y
  `residuo()` (líneas 64-69), líneas largas como las que ya hay.
- El verificador se corre siempre desde `cloze/salida`. Tarda unos 6-7 minutos:
  `cd salida && Rscript --no-init-file ../verificar_espacio_cloze.R > verificador_va.txt 2>&1; echo "exit=$?"`
- Hoy da exit 3. El único pendiente es X6 en la rama PROMEDIO (L-C2), con +8,5 pp medidos en la muestra. La salida de
  referencia es `cloze/salida/verificar_espacio_cloze_r8b.txt`.

## Cambio 1: X5, X6 y X10 exactas sobre el sorteo de la Parte 2 (batería cruzada L-C2, líneas ~331-366)

Hoy la matriz `XT` se calcula con la clave y los distractores de la Parte 2 tal como salieron en cada versión de la muestra.
El profesor decidió que **X5, X6 y X10**, las tres reglas que leen el sorteo de la Parte 2, se midan **exactas sobre ese sorteo**
en los estratos `mezcla` (`NAT`), `CURSOS` (`CU$CURSOS`) y `PROMEDIO` (`CU$PROMEDIO`). Para eso:

1. El banco de la Parte 2 es fijo: 3 claves (paráfrasis) y 5 distractores, en `correr(EXPR_C, 1L)$P2$promedio`, con los campos
   `$clave` y `$distractores`. Es el mismo objeto que `Q_P2` (línea ~863; ver cómo se define `EN_P2`). Cada versión muestra una
   clave, sorteada uniforme entre las 3, y 3 de los 5 distractores (C(5,3) = 10 tríos equiprobables). Es el mismo supuesto de
   `medir_p2_exacta()` en `cloze/bateria_partes_2_a_6.R`, línea 323. Defínelo antes del bloque L-C2 y comprueba el supuesto con
   `ok()`: en todas las versiones de `NAT`, `CU$CURSOS` y `CU$PROMEDIO`, la clave mostrada (`x$opciones_p2[x$sol_p2]`) está en
   `$clave` y los distractores mostrados están en `$distractores`.
2. Escribe **una sola función** para la medida exacta y úsala en la puerta y en el mutante MC37:
   - **X6**: media sobre las versiones `x` de la media sobre las 3 claves `k` de `sc(mas_comun(x$opciones, k), x$clave)`.
   - **X10**: igual, con `sc(dentro(par_su(x), mas_comun(x$opciones, k)), x$clave)`.
   - **X5**: media sobre las 30 combinaciones (clave × trío) de `sc(mas_comun(x$opciones, c(x$intro, x$texto_p2, <clave y trío>,
     x$texto_p3, x$texto_p4, x$texto_p5, x$texto_p6, x$opciones_p6)), x$clave)`. El orden de las opciones no importa, porque la
     regla cuenta palabras.
   - El script `cloze/salida/x6_estratos_t8.R` ya hace esto para X6 y sirve de modelo.
3. En el bucle de veredictos de L-C2, esas 9 celdas (3 reglas × 3 estratos) se juzgan con `veredicto_cruzado()` sobre la cifra
   **exacta**. El mensaje dice «exacta sobre el sorteo de la Parte 2 (decisión «Va» del profesor, 2026-10-31)» y muestra también
   la cifra de la muestra. Las demás reglas y la columna `canonica` no cambian; la canónica la cubre L-C3.
4. Imprime X6 exacta por clave en cada estrato, como `x6_estratos_t8.R`.

**Criterio:** con el `.Rmd` actual, X6 exacta debe dar lo que dice `cloze/salida/x6_estratos_t8.txt`: mezcla 24,9 %, CURSOS 26,3 %
y PROMEDIO 31,0 % (+6,0 pp); por clave, +1,0/+1,0/−2,2, +2,0/+2,0/+0,0 y +9,0/+9,0/+0,0. Si no coincide, averigua por qué
antes de seguir: las muestras deberían ser las mismas.

**Mutante MC37 (L-C2 exacta).** Usa las mismas tres sustituciones de MC19 (línea ~682): las tres claves ganan « aunque fue
menor». Con `mutar()`, construye muestras como hace `medir_mut()` (semillas 1:200) y mide X6 exacta con la misma función y con el
banco del código mutado. Comprueba que llegó: todas las claves del banco mutado terminan en «aunque fue menor.».
- Resultado `cazado_por_su_sonda` si `veredicto_cruzado()` da «pendiente» para X6 exacta.
- Si con esa mutación X6 exacta no pasa de +8 pp, no inventes otra en silencio. Dilo en el informe con la cifra y propón una
  mutación del banco de claves que sí llegue.
- Agrega la fila en `filas_mut[["MC37"]]` y en la lista de la tabla final (línea ~1076).

## Cambio 2: L-C7 regla por regla (líneas ~944-983)

Hoy `p6_token_juzgar()` cubre toda regla por palabra que no pase la cifra de su estrato (`FIRMA_P6_TOKEN`). El profesor firmó
**regla por regla** la lista `cloze/firma_p6_palabras_f98f9dda.tsv`: 216 filas, separadas por tabulador, en UTF-8, con las
columnas `estrato`, `id`, `papel`, `firmado_pp` y `aplica_pct`. Los `id` llevan «» y espacios, por ejemplo `P6-V «hubiera
tenido»c`.

1. Lee la lista con su md5 fijado: `stopifnot(unname(tools::md5sum(f)) == "2aa8e7426b325d8fed9f8c6144608164")`. Usa
   `utils::read.delim(f, quote = "", stringsAsFactors = FALSE, encoding = "UTF-8")` y comprueba que tiene 216 filas.
2. Nuevo `p6_token_juzgar(VT)`, con `ex <- 100 * VT$exceso`; `f`, la cifra firmada de esa misma (estrato, id, papel) o NA si no
   está; y `fe <- FIRMA_P6_TOKEN[estrato]`, el tope del estrato, que se conserva:
   - `ex <= 8 + 1e-9` → «ok»;
   - `!is.na(f) & round(ex, 1) <= f + 1e-9 & round(ex, 1) <= fe + 1e-9` → «cubierto»;
   - en otro caso → «pendiente».
3. Mensajes:
   - Cubierta: `firmado(m, sprintf(" (firmada regla por regla: %+.1f pp)", f), por = "«Va» (L-C7, regla por regla)")`.
   - Pendiente fuera de la lista: «, fuera de la lista firmada (L-C7 regla por regla)».
   - Pendiente que pasa su cifra: «, por encima de lo firmado (%+.1f pp)».
4. Comprobaciones con `ok()`: las 216 reglas de la lista existen en `VT7` y todas salen «cubierto», y en `VT7` no hay ninguna
   «cubierto» que no esté en la lista.
5. MC36 tiene que seguir cazado por su sonda. Su regla «duda» no está en la lista.
6. **Mutante MC38 (L-C7 regla por regla).** Prueba que el cambio sostiene la puerta.
   - Mutación: en el `.Rmd`, la redacción «hubiera tenido el mismo promedio por curso» (líneas 743 y 746 del `.Rmd`; léelas)
     gana una palabra nueva que no aparece en ninguna otra afirmación, por ejemplo «justamente»: «hubiera tenido justamente el
     mismo promedio por curso». Haz la sustitución en las dos líneas.
   - Mídelo como MC36: `h2_exacta(mm$txt, 1L)`, `p6_token_tabla()` y el `p6_token_juzgar()` nuevo.
   - Comprueba que llegó: la palabra aparece en las afirmaciones de ventaja de esa redacción.
   - Calcula además el veredicto con la regla anterior, por estrato. Para eso deja una función `p6_token_juzgar_estrato()` con
     el código viejo, solo para este contraste.
   - Resultado `cazado_por_su_sonda` si alguna regla con «justamente» sale «pendiente» con la función nueva y «cubierto» con la
     vieja. Es justo lo que la firma por estrato dejaba pasar.
   - Agrega su fila a `filas_mut` y a la tabla final.

## Cabecera y salida

- Actualiza la cabecera (líneas 1-49) con el estilo de las entradas de las tandas.
  - E8: «Tras la tanda 8 (decisión «Va» del profesor, 2026-10-31): X5, X6 y X10 exactas sobre el sorteo de la Parte 2 (MC37);
    L-C7 regla por regla (firma_p6_palabras_f98f9dda.tsv, MC38)».
  - Cambia «L-C7: la regla por token de la Parte 6, por estrato» para reflejar «regla por regla».
- Actualiza también los textos de las líneas ~1080 y ~1085 que nombran L-C7.

## Criterio de aceptación (compruébalo tú, corriendo el verificador)

1. Exit 4: «EN ROJO BAJO OVERRIDE FIRMADO», con 0 fallos y 0 pendientes.
2. X6 exacta con las cifras de arriba. X5 y X10 exactas, con su cifra, por debajo de +8 pp. Si alguna pasa de +8 pp, déjala
   pendiente (no la cubras) e infórmalo.
3. 216 reglas de L-C7 cubiertas regla por regla. Son las mismas 216 que en `verificar_espacio_cloze_r8b.txt`.
4. MC36, MC37 y MC38 salen `cazado_por_su_sonda`, y los demás mutantes con el mismo veredicto que en r8b.
5. `git status --porcelain` muestra solo `cloze/verificar_espacio_cloze.R` modificado. Ignora lo que ya estaba modificado fuera
   de `totales-promedios-reciclaje-n3/` antes de empezar.

## Informe (tu respuesta final)

- Qué cambiaste, con números de línea.
- El exit del verificador y el nombre del archivo de salida.
- Las cifras exactas de X5, X6 y X10 por estrato, y X6 por clave.
- Cuántas reglas de L-C7 quedaron cubiertas.
- Las filas de MC36, MC37 y MC38.
- Todo lo que no se cumplió del criterio, con su cifra. Sin minimizar: si hay un problema, lo nombras.
