# Auditoría del detractor, FASE 2C, pasada 2 de la reapertura (2026-10-19): CLOZE informacion-insuficiente-lote-n4

Objeciones: 0 de CORRECCIÓN, 1 ALTA nueva (fuga entre partes que introdujo la corrección), 1 MEDIA (la objeción 4 queda cerrada solo a medias) y 2 BAJAS.

Muestra:
- Generé los datos con `set.seed(s)` y el chunk `data_generation` de la copia de trabajo, en dos muestras:
  - semillas 1-1017, con N = 100 por rama más 68 canónicas (`cur.rds`);
  - semillas 1-1200, con todas las versiones (`cur_0..5.rds`).
- Repetí lo mismo con HEAD (`head.rds`) como control.
- Hubo 0 errores de generación.
- Los scripts son `harness.R`, `harness_chunk.R`, `analizar.R` y `rangos.R`, todos en este directorio.

## 1. Objeciones del reporte FASE2C_REAPERTURA

### Objeción 1 (CORRECCIÓN): CERRADA
Recorrí los 14 pares (rama, dato quitado) a los que se puede llegar:
- -T-N: N y T.
- -TAN: A, N y T.
- canónica: L y N.
- L--N: L y N.
- LT--: L y T.
- LTA-: A, L y T.

Para cada versión hice un oráculo bidireccional. Primero obtuve k a partir del texto del blockquote (`metros de largo`, `franjas iguales, todas paralelas`, `Las franjas se asignan`, `<n> <beneficiarios>`). Después comprobé que la clave no es determinable con ese k y que los tres distractores sí lo son.
- k(texto) ≠ k_p6: 0 de 568.
- Violaciones (clave o distractor): 0 de 568, canónica incluida (68).
- El razonamiento a mano por par coincide. La única inferencia posible era la del reporte anterior (lado + área ⇒ el otro lado; «la que da a la carretera» ⇒ asignación), y ya no aplica, porque el blockquote no conserva ni el lado en metros ni la frase de asignación.
- `rel_fijo`: correcto en los 180 hipotéticos sin L y con T. De ellos, 41 dicen «más largo»: vienen de LT--/LTA- con `corte_fijo` sobre el lado mayor y el texto describe el mismo lote.
- La canónica no tiene carretera, ni en el enunciado ni en el hipotético.
- La plantilla de diálogo (empieza con raya) se ve bien dentro del blockquote en los 4 formatos (render_v2/html2).
- Los valores de los distractores (`valores_p6`) concuerdan con k: por ejemplo, POR-08 = «el lado más largo/corto» sin L y «el lado de # m» con L.

### Objeción 2 (P5 LTA-, longitud), parte explotable: CERRADA
Parte 5 con N = 100 por rama. Una regla es explotable cuando su margen es ≥ 15 % (§P7-B).

| rama | clave = la más larga (bruto) | techo nulo | explotable, actual | explotable, HEAD |
|---|---|---|---|---|
| LTA- | 58 % | 21,5 % | +0,8 pp (1/100) | +9,2 pp (13/100) |
| L--N | 40 % | 23,5 % | +0,0 pp | +0,0 pp |
| demás | ≤ 29 % | — | ≤ 0 pp | — |

- Cuando la clave es la más larga en LTA-, el margen mediano es del 8,5 % (cociente 0,915; p10 0,862, es decir 13,8 %). Es inexplotable por §P7-B. Este residuo bruto (+36,5 pp sin margen) se declara, no se persigue.
- El caso sin restricción (`p5_sin_largo`) se da en 1/100 de LTA- y 0 en las demás ramas.
- Corrección de la clave en P5 (§P7-D): en 568 versiones hay 0 claves que no sean indeterminadas-y-dependientes de h_p5 y 0 distractores que lo sean (oráculo DEP). La restricción solo rechaza paquetes; no cambia el predicado de unicidad, así que no puede crear una segunda clave.
- Canales inversos (semillas 1-1200, con margen ≥ 15 % en cada salto):
  - «descartar la más larga»: ≤ +2,6 pp (canónica). En LTA- es −16,1 pp.
  - «la segunda más larga»: ≤ +0,9 pp. En bruto sube en la canónica de 44 % a 60 %, pero sin margen.
  - «la más corta»: ≤ 0 pp en todas las ramas. LT-- llega a +13,2 pp en bruto, igual que en HEAD, y es explotable en 0 %.
- Concentración de claves en P5: LTA- pasa de 14 % a 21 % (POR-03). Las demás ramas siguen como en HEAD.
- El orden de la cascada (primero exclusiones, luego longitud) no abre canal medible.

### Objeción 3: CERRADA
- El motivo sale de `banco_p6`.
- POR-01..05 nunca son clave de P6 (0 de 568). Es estructural: ninguna rama viva declara L, T y N a la vez. La rama (L,T,-,N) fue retirada y la canónica es L--N.
- UBI-01 nunca aparece en P6 de -TAN/LTA- (0). En las demás ramas tiene DEP TAN sin T∧A∧N, así que no está determinada y no puede ser opción de P6.
- La excepción declarada para `motivo_porcion`/`contra_porcion` es cierta.
- Revisé a mano los 24 motivos distintos (hecho, clave): todos nombran el dato quitado y ninguno culpa a uno que siga declarado.

### Objeción 4: CERRADA A MEDIAS (ver O-2)

## 2. Objeciones

### O-1 (ALTA, nueva, la introdujo esta corrección): la Parte 6 imprime la respuesta de la Parte 2 o 3
**Qué se cuestiona.** Cuando `hecho_p6 = "L"`, el blockquote dice «… de 3.600 metros cuadrados de área …». Las Partes 2 y 3 pueden pedir FIJ-01 («¿Cuál es el área del lote?») o FIJ-06 (área total de las porciones), cuyo `num_val` es esa misma área. En las ramas con L el estudiante debía calcularla (largo × ancho). Ahora la ve escrita en la Parte 6, que se muestra en la misma pantalla.

**Evidencia.**
- render_v2/tex/exercise1.tex (semilla 10, L--N): el enunciado dice «30 metros de largo y 120 metros de ancho». La Parte 2 pregunta «¿Cuál es el área del lote? Responda en metros cuadrados…». El blockquote de la Parte 6 dice «tiene 3.600 metros cuadrados de área».
- Frecuencia (N = 100 por rama):

| rama | versiones con la fuga |
|---|---|
| L--N | 16/100 |
| LTA- | 17/100 |
| LT-- | 8/100 |
| canónica | 0/68 |
| -T-N y -TAN | 0 (no hay hecho L) |

- Ponderado por la distribución natural (semillas 1-1200): (186·0,16 + 116·0,08 + 347·0,17)/1200 ≈ 8,2 % de todas las versiones.
- HEAD no lo tenía: `frase_quitar` no imprimía el número. Es el único número nuevo que el hipotético puede introducir: 172 hipotéticos tienen algún número nuevo, y en los 172 es el área con hecho L.

**Por qué ALTA y no CORRECCIÓN.** La clave de P2/P3 sigue siendo verdadera. Lo que se rompe es la medición de un hueco: uno de cada seis se responde copiando, sin hacer la operación que se pide. El propio archivo trata esto como defecto: la guarda C-1 y el último escalón de P4 vetan `par_23` porque «pedir que se calcule una magnitud demuestra que esa opción … se puede responder» (líneas 1469-1474; esa fuga se midió en 22,5 % y se corrigió).

**Atenuante.** En -T-N/-TAN el área ya viene en el enunciado y P2/P3 la piden en el 65-72 % de las versiones. El CLOZE ya admite que P2 sea una lectura directa. Por eso el daño marginal es convertir el 16-17 % de L--N/LTA- en ese mismo nivel. Decide el profesor si basta declararlo, pero esto contradice la invariante C-1 del archivo.

**Alternativa propuesta.**
1. En la cascada de la Parte 6 (líneas 1145-1152), dejar `L` al final cuando P2/P3 piden el área, en lugar de prohibirlo, para no arriesgar fallos de generación. Si aun así sale `L`, contarlo y declararlo, igual que `p5_sin_largo`:
```r
area_en_p23 <- any(cods[par_23] %in% c("GEO-INF-FIJ-01", "GEO-INF-FIJ-06"))
ord_h <- function(v) { v <- safe_sample(v, length(v)); if (area_en_p23) c(setdiff(v, "L"), intersect(v, "L")) else v }
...  for (h in ord_h(declarados)) {
p6_fuga_area <- hecho_p6 == "L" && area_en_p23
```
   En todas las ramas con L hay otro hecho declarado: L--N tiene N, LT-- tiene T y LTA- tiene T y A. Que `elegir_p6` tenga éxito con ellos en esas versiones no está medido; hay que medirlo.
2. Añadir la guarda G12, aplicable a cualquier número y no solo al área: ningún `num_val` de P2/P3 aparece formateado en `enunciado_p6` si no aparece también en `enunciado`.
```r
nuevo_en_p6 <- function(v) !is.na(v) && grepl(fmt_mil(v), enunciado_p6, fixed = TRUE) && !grepl(fmt_mil(v), enunciado, fixed = TRUE)
stopifnot("G12: la Parte 6 imprime la respuesta de la Parte 2 o 3" = p6_fuga_area || !any(vapply(c(q_p2$num_val, q_p3$num_val), nuevo_en_p6, logical(1))))
```
   Añadir también un mutante (`ord_h` → `safe_sample`) que tiene que morir por G12 en alguna de las semillas 10, 44 o 48.

**Criterios de aceptación.** Con N = 100 por rama, deben cumplirse todos:
- 0 errores de generación;
- `p6_fuga_area` declarado (objetivo 0);
- repetir la distribución de `clave_p6` y `hecho_p6` por rama, porque en L--N con área en P2/P3 la clave pasa a ser siempre FIJ-xx: hay que reportar la cuota máxima por rama;
- repetir el oráculo bidireccional de la Parte 6 (0 violaciones).

### O-2 (MEDIA, objeción 4 cerrada a medias): el oráculo de `verificar_p6_ubi01.R` solo mira en una dirección y no recorre la canónica
**Qué se cuestiona.**
- `conserva()` (líneas 43-47) solo comprueba que el dato quitado no aparezca en el texto, y es casi la misma regex que G11. No comprueba que los datos que siguen declarados estén en el texto, ni que la clave sea indeterminada y los distractores determinados según el texto.
- Un fallo del constructor que borrara de más (por ejemplo, la frase del trazado cuando k_p6$T = TRUE) dejaría distractores sin respuesta. Ni G6b ni ese oráculo lo verían, porque ambos parten de los flags y no del texto.
- La línea `if (g("is_canonical")) next` excluye la canónica, que tiene dos pares (L y N) propios y sin carretera.

**Por qué.** Regla #24 H-3, inventario bidireccional: un chequeo de una sola dirección no ve lo que sobra o falta en la otra.

**Alternativa.** Sustituir `conserva()` por el oráculo bidireccional de `analizar.R` (sección A): k a partir del texto, `identical(k_texto, k_p6)`, más `det_bajo(DEP[clave], k_texto) == FALSE` y `det_bajo(DEP[d], k_texto) == TRUE`. Contar también las canónicas. El mutante «quitar la frase del trazado con k_p6$T = TRUE» debe morir.

### O-3 (BAJA): «del dato que se quitó» en singular cuando se quitan dos
Con hecho T e `info_A`, `frase_quitar` dice que se quitan el trazado «y, por lo tanto», la asignación. Pero la Solution (`Las otras tres … no dependen del dato que se quitó`) y el feedback de los distractores (`no depende del dato que se quita`) hablan de un único dato. No es falso, porque la asignación se pierde como consecuencia, pero es impreciso. Alternativa: «de los datos que se quitaron» cuando `hecho_p6 == "T" && info_A`.

### O-4 (BAJA, estilo): «únicamente» repetido
«…sino únicamente su área, es decir, que dijera únicamente esto:». Alternativa: «…sino solo su área, es decir, que dijera esto:».

## 3. Verificado sin objeción
- El blockquote se ve correctamente en los 4 formatos: HTML (`<blockquote>`), PDF (`\begin{quote}`, página revisada visualmente en s3-1.png), DOCX (bloque de cita) y Moodle (7 `<blockquote>` en el XML; el hueco MULTICHOICE va después del texto).
- «Resumen de la Parte 1» como lista, m² y viñetas de «Estrategia»: correctos en HTML y PDF.
- Glifos: OK (`validar_glifos_latex.R`). Ortografía: 0 correcciones; quedan 9 casos ambiguos, todos en comentarios, y son los mismos de antes.
- FASE 2J: la Solution no depende de letras. No aparece «Opción [A-D]» en el render.
- Fugas entre partes, salvo O-1: el hipotético es un subconjunto del enunciado más `rel_fijo`, que se deduce de L. No delata nada de las Partes 1, 4 y 5.
- G8, G9, G10 y G11: sin falsos positivos en 1200 semillas (0 errores).
- P6, longitud (sin cambios en esta pasada): exceso explotable ≤ +7,5 pp (L--N). Sigue bajo +8 pp.

## Veredicto global
APROBAR_CON_CAMBIOS.
- Las objeciones de CORRECCIÓN y la parte explotable de la diagnosticidad están cerradas y comprobadas con un oráculo independiente.
- O-1 es ALTA y la introdujo esta misma corrección. Bloquea la FASE 3 (regla #9) hasta que se aplique con su guarda y su mutante y se cumplan los criterios de aceptación de arriba.
- `detractor_fase2c` no debe sellarse antes de eso. Si la medición de O-1 cambia la distribución de claves de la Parte 6 más allá de lo declarado, o aparece algún fallo de generación, se vuelve a la FASE 2C.

VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
