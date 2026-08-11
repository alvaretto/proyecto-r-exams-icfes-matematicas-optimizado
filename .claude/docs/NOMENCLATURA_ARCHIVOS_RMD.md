# Nomenclatura Oficial para Archivos .Rmd

> **FUENTE ÚNICA DE VERDAD.** Cualquier otro archivo del repo que describa el formato de nombre
> (skills, comandos, reglas, agentes) es una **cita** de este documento, no una fuente paralela.
> Si alguno diverge, gana éste y el divergente es un defecto que hay que corregir. La razón está
> en el §Historial: durante seis meses convivieron **dos formatos rivales en nueve sitios** y nada
> los reconciliaba.

## REGLA OBLIGATORIA

**Todo archivo .Rmd DEBE seguir este formato de nomenclatura:**

```
[ejercicio]_[componente]_[competencia]_n[nivel]_[tipo]_v[version].Rmd
```

Con el sufijo opcional `_neg` inmediatamente antes de `_v[version]` cuando aplique la regla #10:

```
[ejercicio]_[componente]_[competencia]_n[nivel]_[tipo]_neg_v[version].Rmd
```

## Componentes del Nombre

### 1. [ejercicio] - Nombre Descriptivo
- Usar snake_case (guiones bajos)
- Describir el contenido matemático del ejercicio
- Sin tildes ni caracteres especiales
- Máximo 40 caracteres

**Ejemplos válidos:**
- `series_temporales_poblacion`
- `diagrama_venn_generos_musicales`
- `mediana_datos_farmaceuticos`
- `informacion_insuficiente_lote`

**Ejemplos inválidos:**
- `poblacion_paises` (muy genérico)
- `ejercicio1` (no descriptivo)
- `SeriesTemporales` (camelCase prohibido)
- `series-temporales` (guiones prohibidos)
- `mediana_metacognitivo` (⚠️ `metacognitivo` NO va aquí ni en ninguna ranura — ver §Historial)

### 2. [componente] - Componente ICFES
Usar exactamente uno de estos valores:

| Valor | Descripción |
|-------|-------------|
| `geometrico_metrico` | Geometría, medición, espacial |
| `numerico_variacional` | Números, álgebra, funciones |
| `aleatorio` | Estadística, probabilidad |

### 3. [competencia] - Competencia ICFES
Usar exactamente uno de estos valores, en su **forma oficial larga**:

| Valor | Descripción |
|-------|-------------|
| `interpretacion_representacion` | Leer, interpretar gráficos/tablas |
| `formulacion_ejecucion` | Plantear y resolver problemas |
| `argumentacion` | Justificar, validar procedimientos |

⚠️ `interpretacion` y `formulacion` a secas **no son válidas**: son las formas cortas que introdujo
la deriva de 2026-02. La competencia del nombre debe coincidir con `exextra[Competencia]`.

### 4. n[nivel] - Nivel de Dificultad
Usar `n1`, `n2`, `n3` o `n4`:

| Nivel | Descripción |
|-------|-------------|
| `n1` | Básico - Reconocimiento directo |
| `n2` | Intermedio - Aplicación simple |
| `n3` | Avanzado - Múltiples pasos |
| `n4` | Superior - Análisis complejo |

Debe ser coherente con el DOK (ver `ejercicios-metacognitivos.md`): si DOK ≥ 3 → Nivel ≥ 3.

### 5. [tipo] - Tipo de Ítem R-exams
Usar exactamente uno de estos valores:

| Valor | Descripción |
|-------|-------------|
| `schoice` | Selección única (`extype: schoice`) |
| `cloze` | Pregunta compuesta multi-gap (`extype: cloze`) |

**Esta ranura es obligatoria y no es decorativa.** El repo tiene 13 familias de ejercicios con dos
o tres variantes del mismo enunciado (`schoice`, `cloze`, `_neg`); sin la ranura de tipo, esas
variantes colisionan en el mismo nombre de archivo. Debe coincidir con `extype` del `.Rmd`.

### 6. `_neg` - Sufijo de Lógica Negativa (opcional)
Se añade **solo** cuando el ejercicio cumple la regla #10
(`validacion-neg-opciones-repetidas.md`): (N−1) opciones equivalentes + 1 con el error, con su
test de patrón correspondiente.

⚠️ **No basta con que el enunciado tenga tallo negativo** («¿cuál NO…?»). El sufijo es un
**disparador mecánico**: `validar_5c_unicidad` calcula `es_negativo <- grepl("_neg_", basename(...))`
y, si lo encuentra, valida por la rama negativa (que exige (N−1) opciones equivalentes) en vez de
la positiva (que exige las N distintas). Ponerlo en un ítem que no cumple la regla #10 hace que el
validador compruebe lo contrario de lo que el ejercicio necesita. El énfasis del tallo negativo
vive en el `***NO***` del enunciado, no en el nombre del archivo.

### 7. v[version] - Versión del Ejercicio
- Empezar siempre con `v1`
- Incrementar al crear variantes: `v2`, `v3`, etc.
- Las variantes son ejercicios diferentes, no versiones dinámicas

### Sufijo `_interactivo`
Los derivados HTML interactivos llevan `_interactivo` **después** de `_v[version]`:
`..._n3_schoice_v1_interactivo.Rmd`. No son ejercicios nuevos, son una salida del mismo.

## Ejemplos Completos

```
# Estadística, interpretación, nivel 2, selección única
series_temporales_poblacion_aleatorio_interpretacion_representacion_n2_schoice_v1.Rmd

# Estadística, formulación, nivel 3, cloze
diagrama_venn_generos_musicales_aleatorio_formulacion_ejecucion_n3_cloze_v1.Rmd

# Variacional, interpretación, nivel 2, selección única
funcion_lineal_auto_viajero_numerico_variacional_interpretacion_representacion_n2_schoice_v1.Rmd

# Geométrico-métrico, argumentación, nivel 4, selección única
informacion_insuficiente_lote_geometrico_metrico_argumentacion_n4_schoice_v1.Rmd

# El MISMO ejercicio en sus dos variantes: la ranura de tipo las distingue
area_jardin_lote_geometrico_metrico_argumentacion_n4_schoice_v1.Rmd
area_jardin_lote_geometrico_metrico_argumentacion_n4_cloze_v1.Rmd

# Lógica negativa (cumple la regla #10)
diagrama_caja_estaturas_aleatorio_interpretacion_representacion_n2_schoice_neg_v1.Rmd
```

## Correspondencia con exname

El campo `exname` en Meta-information DEBE coincidir exactamente con el nombre del archivo (sin extensión):

```yaml
Meta-information
================
exname: informacion_insuficiente_lote_geometrico_metrico_argumentacion_n4_schoice_v1
extype: schoice
...
```

## Correspondencia con Metadatos ICFES

Los metadatos ICFES del archivo DEBEN coincidir con el nombre. Es la razón de ser del formato: el
nombre **declara la clasificación ICFES** del ítem, no solo lo identifica.

| Ranura del nombre | Campo del `.Rmd` |
|---|---|
| `[componente]` | `exextra[Componente]` (`Geométrico-métrico` ↔ `geometrico_metrico`) |
| `[competencia]` | `exextra[Competencia]` (`Argumentación` ↔ `argumentacion`) |
| `n[nivel]` | `exextra[Nivel]` |
| `[tipo]` | `extype` |

Los `exextra[...]` llevan tildes y mayúsculas (literalidad ICFES); el nombre de archivo va en ASCII
y minúsculas. La correspondencia es semántica, no carácter a carácter.

## Validación Automática

| Capa | Qué hace | Dónde |
|---|---|---|
| **Gate PreToolUse** | Bloquea la escritura de un `.Rmd` cuyo nombre no cumple el formato, **antes** de crearlo | `.claude/hooks/pre-write-rmd-gate.sh` |
| **Test de regresión** | Recorre `01-`, `02-` y `03-` y falla ante cualquier `.Rmd` fuera de formato que no esté en el allowlist legacy | `tests/testthat/test_nomenclatura_rmd.R` |
| **Coherencia de citas** | Verifica que los sitios que citan el formato (skills, comandos, reglas) coincidan con este documento | mismo test |

El allowlist legacy es **finito y decreciente**: contiene los archivos anteriores a 2026-08-10 y no
admite altas. Cuando un ejercicio legacy se toque por cualquier motivo, se renombra y sale de la lista.

## Estructura de Carpetas

Cada ejercicio vive en su propio directorio bajo la zona que le corresponde:

| Zona | Ruta | Estado |
|---|---|---|
| Pre-desarrollo | `A-Produccion/01-En-PreDesarrollo/<subproyecto>/` | Experimentación |
| Desarrollo | `A-Produccion/02-En-Desarrollo/<subproyecto>/` | En proceso / aprobado para aula |
| Producción | `A-Produccion/03-En-Produccion/<categoría ICFES>/<subproyecto>/` | Validados con estudiantes (Nivel 3) |

El nombre del **directorio** (`<subproyecto>`) usa kebab-case corto y descriptivo
(`area-jardin-lote-porcentaje-n4`), y **no** está obligado a repetir el nombre completo del `.Rmd`.
Es una convención distinta y deliberada: el nombre largo informa la clasificación ICFES del ítem,
el corto sirve para navegar.

## Errores Comunes a Evitar

| ❌ | Por qué |
|---|---|
| `poblacion_paises.Rmd` | Faltan componente, competencia, nivel, tipo, versión |
| `Series_Temporales_Aleatorio_n2_v1.Rmd` | Mayúsculas prohibidas; falta competencia y tipo |
| `series-temporales_aleatorio_n2_schoice_v1.Rmd` | Guiones medios prohibidos, usar guiones bajos |
| `mediana_metacognitivo_argumentacion_n3_schoice_v1.Rmd` | `metacognitivo` ocupa la ranura del componente ICFES sin informar nada |
| `grafica_lineal_aleatorio_interpretacion_n2_schoice_v1.Rmd` | Competencia en forma corta; la oficial es `interpretacion_representacion` |
| `area_lote_geometrico_metrico_argumentacion_n4_v1.Rmd` | Falta la ranura de tipo → colisiona con su variante cloze |
| `... _schoice_neg_v1.Rmd` en un ítem que no cumple la regla #10 | El `_neg_` vuelca al validador a la rama equivocada |

## Historial — por qué este documento lleva una advertencia al principio

Entre 2026-02-06 y 2026-08-10 el repositorio tuvo **dos formatos de nombre rivales**:

- el de este documento, `[ejercicio]_[componente]_[competencia]_n[nivel]_v[version]`, que **no
  estaba cableado en ninguna comprobación ejecutable**;
- uno de facto, `[ejercicio]_metacognitivo_[competencia_corta]_n[nivel]_[tipo]_v[N]`, nacido con la
  regla de ejercicios metacognitivos (v3.1) y **sí cableado**: en el regex de los dos comandos
  orquestadores, en los dos skills generadores y en la regla #10.

Resultado medido el 2026-08-10 sobre los 142 `.Rmd` tocados desde febrero: **53 seguían el de facto
y 10 el documentado**. El de facto ganaba porque era el que la maquinaria comprobaba, y este
documento era inerte.

**Qué se perdió por el camino:** la palabra `metacognitivo` es constante en todos los ejercicios
desde que la regla la hizo obligatoria, de modo que ocupaba la ranura del **componente ICFES** sin
aportar un bit de información; y la competencia se acortó, perdiendo la forma oficial.

**Decisión (2026-08-10, del profesor):** vuelve a regir el formato de este documento, **extendido
con la ranura de tipo** —imprescindible: sin ella, 13 familias de ejercicios con variantes
schoice/cloze colisionan— y sin la palabra `metacognitivo`. Que un ejercicio sea metacognitivo lo
dice la regla #1, que es universal; no hace falta repetirlo en cada nombre.

**Alcance:** de aquí en adelante. Lo existente entra en un allowlist legacy que se vacía cuando
cada ejercicio se toca por otro motivo. Los 7 archivos de `03-En-Produccion/` son inmutables
(regla #2) y permanecen en el allowlist de forma indefinida.

**La lección, que es la que justifica la advertencia de la cabecera:** un formato que solo vive en
un documento pierde siempre contra uno que vive en un regex. Por eso ahora el formato tiene un
**gate PreToolUse** que bloquea antes de escribir, y un test que verifica que las citas del resto
del repo no vuelvan a divergir de este archivo.

## Referencias

- Gate: `.claude/hooks/pre-write-rmd-gate.sh`
- Test: `tests/testthat/test_nomenclatura_rmd.R`
- Regla #10 (sufijo `_neg`): `.claude/rules/validacion-neg-opciones-repetidas.md`
- Regla #1 (metacognitivos): `.claude/rules/ejercicios-metacognitivos.md`
- Skills generadores: `.claude/skills/generar-schoice/SKILL.md`, `.claude/skills/generar-cloze/SKILL.md`
- Orquestadores: `.claude/commands/orquestador-schoice.md`, `.claude/commands/orquestador-cloze.md`

---

**Versión:** 2.0
**Fecha:** 2026-08-10
**Estado:** ACTIVO Y OBLIGATORIO — fuente única de verdad del formato de nombre
**Cambios v2.0:** ranura `[tipo]` obligatoria; sufijos `_neg` e `_interactivo` documentados con su
semántica de disparador; competencia en forma oficial larga; `metacognitivo` prohibido en el nombre;
correspondencia nombre ↔ `exextra[...]` tabulada; rutas actualizadas a `01-/02-/03-`; convención de
directorio kebab-case declarada como deliberadamente distinta; gate + test como capas de validación;
§Historial con la medición de la deriva y la decisión que la cierra.
