# Regla local — Contrato de la familia paramétrica de permutaciones

## Principio

**Las cuatro opciones del ítem no son cuatro números cualesquiera: son la evaluación de cinco
fórmulas fijas sobre un único parámetro `n`. La respuesta correcta ES `n!`. Cualquier cambio en
`N_POOL`, en las fórmulas del pool o en la lógica de selección debe preservar, simultáneamente,
las seis invariantes de abajo — verificadas por enumeración exhaustiva, no por muestreo.**

Análogo local a `plano-cartesiano-barco-n2/.claude/rules/barco-parametrico.md`, donde la clave es
el bounding box del dibujo. Aquí la clave es el valor de una fórmula, así que el riesgo no es
geométrico sino combinatorio: una fórmula nueva puede colisionar con otra y producir dos opciones
idénticas sin ningún error de sintaxis.

---

## El contrato

### Espacio de parámetros

```
n ∈ {4, 5, 6}                      # N_POOL, línea ~47 del .Rmd
correcta = n!                      # 24, 120, 720
```

### Pool de fórmulas (5 errores, se eligen 3 por versión)

| Código | Fórmula | n=4 | n=5 | n=6 | Origen |
|---|---|---:|---:|---:|---|
| `EST-PER-01` | `n^(n-1)` | 64 | 625 | 7776 | ficha oficial, opción A |
| `EST-PER-02` | `n²` | 16 | 25 | 36 | ficha oficial, opción C |
| `EST-PER-03` | `n` | 4 | 5 | 6 | ficha oficial, opción D |
| `EST-PER-04` | `(n-1)!` | 6 | 24 | 120 | ampliación propia (regla #1) |
| `EST-PER-05` | `n(n+1)/2` | 10 | 15 | 21 | ampliación propia (regla #1) |

### Las seis invariantes

| # | Condición | Dónde se comprueba | Consecuencia si se rompe |
|---|---|---|---|
| **I-1** | Las 4 opciones son distintas | `stopifnot(length(unique(all_vals)) == 4L)` | `ERR_ANS_C`: dos opciones idénticas; el ítem pasa a tener dos "respuestas" |
| **I-2** | Ningún distractor iguala a `n!` | `stopifnot(!any(unname(vals) == correcta_val))` | `ERR_ANS_E`: un distractor es correcto |
| **I-3** | `max(all_vals) / n! ≤ 15` | `stopifnot(max(all_vals) / correcta_val <= 15)` | Regla #22 P5: el distractor mayor se descarta por magnitud sin razonar |
| **I-4** | Todas las opciones son enteros positivos | `stopifnot(all(all_vals > 0L), ...)` | Opciones con decimales o negativas, imposibles como conteo |
| **I-5** | Exactamente una marcada y coincide con `n!` | `stopifnot(identical(opciones[which(sol == 1L)], correcta))` | La clave apunta a un distractor: el estudiante correcto se califica mal |
| **I-6** | En la instancia canónica el conjunto es `{24, 64, 16, 4}` | `if (es_canonica) stopifnot(setequal(...))` | El ejercicio deja de reproducir el ítem ICFES `MAT-2026-1-004` |

**I-5 es la crítica.** Es la única que, si se desactiva, produce un ejercicio que compila, renderiza
en los cuatro formatos y pasa el resto de validadores del repo **con la clave falsa**. Verificado
por mutación el 2026-07-29 (ver abajo).

---

## Verificación exhaustiva, no muestreada

El espacio completo es pequeño y se enumera entero: **3 valores de `n` × C(5,3) = 10 ternas = 30
combinaciones**. `verificar_render.R` V6 las recorre todas.

Resultado medido (2026-07-29):

```
30/30 ternas: 4 opciones únicas, ninguna == correcta,
              razón máx/clave en [1,0x, 10,8x] (umbral 15x)
rango de la correcta por magnitud: 3.º o 4.º  (no invariante)
```

**Regla operativa: si añades o cambias una fórmula del pool, el número de ternas cambia y V6 debe
volver a dar 100 %.** No basta con que "los casos que probé funcionan".

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

## Qué NO protege ningún validador del repo

`validar_coherencia_matematica.R` da APROBADO con 0 errores tanto en el ejercicio correcto como
—si se desactivara I-5— en uno con la clave falsa: sus 21 reglas de keywords semánticas cubren
propiedades de conjuntos de datos estadísticos (paridad, cuartiles, outliers), no combinatoria.
Es un punto ciego conocido para este dominio. La correctitud combinatoria de este ejercicio la
sostienen **I-1..I-6 y V5/V6**, no el validador genérico.

Mismo patrón que documentó el hermano del barco: *si la clave es una propiedad calculada, ningún
validador genérico la protege; hace falta un test propio que enumere el espacio completo*.

---

## Cómo verificar tras cualquier cambio

```bash
cd A-Produccion/01-En-PreDesarrollo/permutaciones-pescadores-venia-n4
Rscript verificar_render.R                    # V1-V8, exit 1 si algo falla
cd ../../..
Rscript .claude/scripts/validar_coherencia_matematica.R <ruta_al_rmd>
Rscript .claude/scripts/validar_diversidad_sustantiva.R <ruta_al_rmd> --n 40
Rscript .claude/scripts/corregir_ortografia_espanol.R <ruta_al_rmd>
```

---

## Referencias

- [`../CLAUDE.md`](../CLAUDE.md) — particularidades operativas del subproyecto
- [`../../docs/BLUEPRINT.md`](../../docs/BLUEPRINT.md) — decisiones D1, D2 y D3
- [`../../HANDOFF.md`](../../HANDOFF.md) — reanudación
- Repo raíz: `.claude/rules/diversidad-sustantiva.md` (P5), `.claude/rules/ejercicios-metacognitivos.md` (pool 4-6), `.claude/rules/solution-letter-independence.md` (#19)
- Hermano análogo: `plano-cartesiano-barco-n2/.claude/rules/barco-parametrico.md`

---

**Versión**: 1.0
**Fecha**: 2026-07-29
**Estado**: ACTIVO
