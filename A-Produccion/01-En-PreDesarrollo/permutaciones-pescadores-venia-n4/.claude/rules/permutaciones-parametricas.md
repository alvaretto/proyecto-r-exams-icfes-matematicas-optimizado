# Regla local — Contrato de la familia paramétrica de permutaciones

## Principio

**Las cuatro opciones del ítem no son cuatro números cualesquiera: son la evaluación de siete
fórmulas fijas sobre un único parámetro `n`. La respuesta correcta ES `n!`. Cualquier cambio en
`N_POOL`, en las fórmulas del pool o en la lógica de selección debe preservar, simultáneamente,
las siete invariantes de abajo — verificadas por enumeración exhaustiva, no por muestreo.**

Análogo local a `plano-cartesiano-barco-n2/.claude/rules/barco-parametrico.md`, donde la clave es
el bounding box del dibujo. Aquí la clave es el valor de una fórmula, así que el riesgo no es
geométrico sino combinatorio: una fórmula nueva puede colisionar con otra y producir dos opciones
idénticas sin ningún error de sintaxis.

---

## El contrato

### Espacio de parámetros

```
n ∈ {4, 5, 6}                      # N_POOL (localizar con: grep -n 'N_POOL <-')
correcta = n!                      # 24, 120, 720
```

### Pool de fórmulas (7 errores, se eligen 3 por versión)

| Código | Fórmula | n=4 | n=5 | n=6 | ¿> `n!`? | Origen |
|---|---|---:|---:|---:|:---:|---|
| `EST-PER-01` | `n^(n-1)` | 64 | 625 | 7776 | **sí** | ficha oficial, opción A |
| `EST-PER-02` | `n²` | 16 | 25 | 36 | no | ficha oficial, opción C |
| `EST-PER-03` | `n` | 4 | 5 | 6 | no | ficha oficial, opción D |
| `EST-PER-04` | `(n-1)!` | 6 | 24 | 120 | no | ampliación propia (regla #1) |
| `EST-PER-05` | `n(n+1)/2` | 10 | 15 | 21 | no | ampliación propia (regla #1) |
| `EST-PER-06` | `(n+1)!` | 120 | 720 | 5040 | **sí** | ampliación propia (decisión D4) |
| `EST-PER-07` | `2·n!` | 48 | 240 | 1440 | **sí** | ampliación propia (decisión D4) |

La columna «¿> `n!`?» **no es informativa: es estructural**. La selección exige al menos una fórmula
de esas tres en cada terna (I-7), y el código la deriva de `calcula()` en tiempo de generación
(`es_mayor`), nunca de una lista de códigos hardcoded — así una fórmula nueva cae en el grupo correcto
sin tocar nada más y no hay dos fuentes de verdad que se desincronicen.

### Las siete invariantes

| # | Condición | Dónde se comprueba | Consecuencia si se rompe |
|---|---|---|---|
| **I-1** | Las 4 opciones son distintas | `stopifnot(length(unique(all_vals)) == 4L)` | `ERR_ANS_C`: dos opciones idénticas; el ítem pasa a tener dos "respuestas" |
| **I-2** | Ningún distractor iguala a `n!` | `stopifnot(!any(unname(vals) == correcta_val))` | `ERR_ANS_E`: un distractor es correcto |
| **I-3** | `max(all_vals) / n! ≤ 15` | `stopifnot(max(all_vals) / correcta_val <= 15)` | Regla #22 P5: el distractor mayor se descarta por magnitud sin razonar |
| **I-4** | Todas las opciones son enteros positivos | `stopifnot(all(all_vals > 0L), ...)` | Opciones con decimales o negativas, imposibles como conteo |
| **I-5** | Exactamente una marcada y coincide con `n!` | `stopifnot(identical(opciones[which(sol == 1L)], correcta))` | La clave apunta a un distractor: el estudiante correcto se califica mal |
| **I-6** | En la instancia canónica el conjunto es `{24, 64, 16, 4}` | `if (es_canonica) stopifnot(setequal(...))` | El ejercicio deja de reproducir el ítem ICFES `MAT-2026-1-004` |
| **I-7** | Al menos un distractor supera a `n!` ⇒ la clave nunca es la opción mayor | `stopifnot(any(unname(vals) > correcta_val))` + el filtro `legales` de la selección | Vuelve el hallazgo H1: «elegir el número mayor» acierta el 40 % y descartar las 2 menores deja una adivinanza al 50 % |

**I-5 es la crítica.** Es la única que, si se desactiva, produce un ejercicio que compila, renderiza
en los cuatro formatos y pasa el resto de validadores del repo **con la clave falsa**. Verificado
por mutación el 2026-07-29 (ver abajo).

**I-7 es la que ningún validador genérico puede dar, ni siquiera en principio.** No es una propiedad
de corrección —el ítem con la clave en 4.º lugar es matemáticamente impecable— sino de **calidad
psicométrica**: mide si la clave se puede acertar por magnitud sin razonar. I-3 no la cubre porque es
unilateral (cuando la clave es el máximo, su ratio vale 1,0× y pasa trivialmente). Por eso I-7 vive en
tres capas: el `stopifnot` del chunk, `V6` sobre el espacio (93/105 ternas legales) y `V9` sobre la
selección real (240 semillas). V6 y V9 **no son redundantes**: V6 mide el espacio, V9 la selección; si
alguien borrara el filtro `legales`, V6 seguiría verde.

---

## Verificación exhaustiva, no muestreada

El espacio completo es pequeño y se enumera entero: **3 valores de `n` × C(7,3) = 35 ternas = 105
combinaciones**. `verificar_render.R` V6 las recorre todas, y V9 comprueba además sobre 240 semillas
que la selección real del chunk se queda en el subespacio legal.

Resultado medido (2026-07-30, pool de 7 + I-7):

```
V6  105/105 ternas: 4 opciones únicas, ninguna == correcta,
                    razón máx/clave en [1,0x, 10,8x] (umbral 15x)
    espacio legal 93/105 (12 descartadas por I-7: ninguna fórmula > n!)
    rango de la clave: 1/2/3 (NUNCA 4.º) | mitad baja 41,9 %
    'elegir el mayor' acierta 0,0 % | clave/mayor distractor máx 0,50x
V9  240/240 versiones: toda terna con >=1 distractor > n!, clave nunca la mayor
    84 ternas (n:códigos) distintas alcanzadas | mitad baja observada 35,8 %
```

Las tres cifras de la segunda mitad (mitad baja > 0, «elegir el mayor» == 0, rango no constante) son
**guardas de no-regresión que FALLAN**, no avisos: si alguien toca el pool o el filtro sin re-medir,
V6 devuelve FAIL en vez de degradar el ítem en silencio. Es la lección del code-review del
2026-07-29, donde tres de diez defectos eran verificadores que se citaban como evidencia verde
estando vacíos.

**Regla operativa: si añades o cambias una fórmula del pool, el número de ternas cambia y V6 debe
volver a dar 100 %.** No basta con que "los casos que probé funcionan". El barrido que fijó la
configuración actual está en [`../../docs/BLUEPRINT.md`](../../docs/BLUEPRINT.md) §3.1: encoger el
pool a 6 baja la cobertura de mitad baja del 41,9 % al 25 %, y a 5 la deja en 0 % con el rango de la
clave clavado en 3.º.

---

## Prueba de mutación (por qué I-5 necesita dos capas)

Ejecutada el 2026-07-29 sobre copias en un directorio temporal:

| Mutación | Resultado |
|---|---|
| Marcar como correcta la opción `n²` (`sol <- as.integer(nombres == "cuadrado")`) | **Abortada en generación** por I-5; el `.Rmd` ni siquiera renderiza |
| La misma mutación **+ desactivar I-5** | Renderiza sin error, pero **V5 la detecta en 8/8 preguntas** del XML de Moodle |

Es decir: la guarda interna atrapa el caso normal, y el verificador externo atrapa el caso en que
alguien "limpia" la guarda interna. Ninguna de las dos capas es redundante.

---

## Qué protege y qué NO protege el validador genérico del repo

**Actualizado el 2026-07-29 tras el code-review de alta intensidad.** La afirmación anterior de esta
sección —«`validar_coherencia_matematica.R` da APROBADO incluso con la clave falsa»— era **cierta,
pero por una razón distinta de la que estaba escrita**, y ya no lo es.

La razón real no era sólo que las 21 keywords semánticas de la Capa B sean de estadística
descriptiva (eso sigue siendo verdad y sigue siendo un punto ciego en combinatoria). Era que el
validador busca **nombres de variable fijos** y este ejercicio usaba otros, de modo que sus
comprobaciones ni se ejecutaban:

| Comprobación | Nombre que busca | Nombre que había | Efecto |
|---|---|---|---|
| Capa A / Capa C | `error_sel` / `error_seleccionado` | `errores_sel` (lista de 3) | `return()` temprano |
| Nivel 5B (cross-check de la clave) | `valor_correcto` | `correcta_val` | `return()` temprano |
| Nivel 5B (valores de opción) | `opciones_valores` antes que `opciones` | sólo `opciones` (cadenas de `fmt()`) | se salta por `!is.numeric` |

El `.Rmd` expone ahora esos tres alias (`valor_correcto`, `opciones_valores`, `error_sel`) al final
del bloque de mezcla, con un `stopifnot` que verifica que `opciones_valores` siga alineado con
`opciones`/`sol`. **No los borres**: sin ellos el APROBADO de FASE 2A vuelve a ser vacuo.

**Medido por mutación el 2026-07-29** (misma clave falsa en los dos casos, guardas internas
desactivadas):

| Mutante | Alias | Veredicto de `validar_coherencia_matematica.R` |
|---|---|---|
| clave falsa | **sin** alias (estado previo) | `APROBADO (0 errores)` · 5B imprime «OK» estando ciego |
| clave falsa | **con** alias | `ERR_ANS_B: Opción marcada (valor=7776) NO coincide con valor_correcto (720)` |

**Cobertura resultante:** 5B cubre la clave; la Capa A cubre la precondición de **uno** de los tres
errores seleccionados (el validador espera un error, no una terna); la Capa C sigue inerte a
propósito (necesitaría un `valor_erroneo` único, que con 3 distractores sería una ficción, y la
relación que comprobaría —distractor ≠ clave— ya la garantiza I-2). Las tres a la vez sólo las
cubren **I-1..I-7, V5/V6/V9 y `test_permutaciones_invariantes.R`**; el validador genérico es ahora una
capa adicional real, no la principal.

Mismo patrón que documentó el hermano del barco: *si la clave es una propiedad calculada, ningún
validador genérico la protege; hace falta un test propio que enumere el espacio completo*.

---

## Cómo verificar tras cualquier cambio

```bash
cd A-Produccion/01-En-PreDesarrollo/permutaciones-pescadores-venia-n4
Rscript verificar_render.R                    # V1-V9, exit 1 si algo falla
cd ../../..
Rscript .claude/scripts/validar_coherencia_matematica.R <ruta_al_rmd>
Rscript .claude/scripts/validar_diversidad_sustantiva.R <ruta_al_rmd> --n 40
Rscript .claude/scripts/corregir_ortografia_espanol.R <ruta_al_rmd>
```

---

## Referencias

- [`../CLAUDE.md`](../CLAUDE.md) — particularidades operativas del subproyecto
- [`../../docs/BLUEPRINT.md`](../../docs/BLUEPRINT.md) — decisiones D1, D2, D3 y D4; §3.1 barrido del pool
- [`../../HANDOFF.md`](../../HANDOFF.md) — reanudación
- Repo raíz: `.claude/rules/diversidad-sustantiva.md` (P5), `.claude/rules/ejercicios-metacognitivos.md` (pool: mínimo 4-6, aquí 7 por D4), `.claude/rules/solution-letter-independence.md` (#19)
- Hermano análogo: `plano-cartesiano-barco-n2/.claude/rules/barco-parametrico.md`

---

**Versión**: 2.0 (pool 5 → 7, invariante I-7 y V9 tras la decisión D4 que cerró el hallazgo H1)
**Fecha**: 2026-07-30
**Estado**: ACTIVO
