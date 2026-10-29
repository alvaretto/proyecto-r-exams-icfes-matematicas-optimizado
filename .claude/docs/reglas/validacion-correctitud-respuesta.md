> **Texto íntegro** de la regla. La versión compacta que Claude Code carga en cada sesión es `.claude/rules/validacion-correctitud-respuesta.md`.
> Si cambias la norma, actualiza **ambas**; el historial y las mediciones viven aquí (política de contexto v3.28.0).

# Regla: Validación de Correctitud de Respuesta (Nivel 5)

## Principio Fundamental

**El sistema DEBE verificar automáticamente que la respuesta marcada como correcta ES realmente la correcta matemáticamente, que los distractores son únicos y diferentes de la respuesta correcta, y que los valores están en rangos válidos. NO hay excepciones.**

---

## Contexto

Los Niveles 1-4 de validación verifican:
- **Nivel 1 (Sintáctico)**: El código R ejecuta sin errores
- **Nivel 2 (Numérico)**: Sin NA/NaN/Inf
- **Nivel 3 (Estructural)**: Metadatos completos, exsolution correcto
- **Nivel 4 (Semántico)**: Descripciones de errores coherentes con datos

**Brecha crítica**: Ninguno de estos niveles verifica que la opción marcada como correcta tenga el valor correcto calculado, ni que los distractores sean únicos en tiempo de ejecución.

---

## Sub-niveles de Validación

### 5A: Evaluación de exsolution dinámico

Cuando `exsolution` contiene `` `r expr` ``, el Nivel 3 lo salta. El Nivel 5A:
1. Extrae la expresión R del inline
2. La evalúa en el entorno del ejercicio
3. Valida el resultado como si fuera estático

**Código de error**: `ERR_ANS_A`
**Severidad**: Bloqueante

### 5B: Cross-check respuesta marcada vs valor correcto

Busca en el entorno del ejercicio:
- Vector `sol` (posiciones correctas)
- `opciones_mezcladas` / `opciones` (valores de las opciones)
- `valor_correcto` / `mediana_calc` / `respuesta_correcta` (valor esperado)

Verifica que `opciones[which(sol==1)]` corresponda al valor correcto.

**Código de error**: `ERR_ANS_B`
**Severidad**: Bloqueante

### 5C: Unicidad de opciones en runtime

- Para SCHOICE normal: todas las opciones deben ser diferentes (`digest::digest()`)
- Para ejercicios `_neg_`: patrón (N-1) iguales + 1 diferente

**Código de error**: `ERR_ANS_C`
**Severidad**: Bloqueante

### 5D: Validación de rangos matemáticos

- Mediana dentro de [min(datos), max(datos)]
- Cuartiles ordenados: min ≤ Q1 ≤ Q2 ≤ Q3 ≤ max
- Probabilidades en [0, 1]
- Porcentajes en [0, 100]

**Código de error**: `ERR_ANS_D`
**Severidad**: Bloqueante

### 5E: Distractor ≠ respuesta correcta

Verifica con `digest::digest()` que ningún distractor sea idéntico a la respuesta correcta.

**Código de error**: `ERR_ANS_E`
**Severidad**: Bloqueante

### Nivel 4 — Capa D: Determinismo de calcula()

Verifica que `calcula()` de CADA error en el pool sea una función pura:

1. **Análisis estático**: Escanea `deparse(calcula)` buscando `sample(`, `runif(`, `rnorm(`, etc.
2. **Test empírico**: Ejecuta `calcula()` dos veces con los mismos argumentos y compara resultados.

Si el error es el seleccionado → `ERR_SEM_D` (bloqueante).
Si el error está en el pool pero no seleccionado → `WARN_SEM_D` (bug latente).

**Código de error**: `ERR_SEM_D` / `WARN_SEM_D`
**Severidad**: ERR bloqueante, WARN informativo

**Bug original (2026-02-14)**: EST-MTC-03 usaba `sample(datos_ord)` dentro de `calcula()`.
El `set.seed()` del multi-semilla enmascaraba el problema: cada semilla producía un resultado
determinista, pero ese resultado NO correspondía a los datos que el estudiante veía en la tabla.
La Capa D habría detectado `sample(` en el análisis estático y bloqueado el ejercicio.

---

## Validación Multi-semilla

El script `validar_multisemilla.R` ejecuta el .Rmd N veces con diferentes semillas:

```
Para semilla en 1:N (dispersas con primos):
  1. set.seed(semilla)
  2. Ejecutar todos los chunks R
  3. Validar Nivel 5A-5E completo
  4. Validar coherencia general y semántica
  5. Registrar fallos

Reportar:
  - Semillas que fallaron
  - Tipo de error por semilla
  - Tasa de éxito (debe ser 100%)
```

### Modos

| Modo | Semillas | Uso |
|------|----------|-----|
| Estándar (default) | **100** | Hook automático (FASE 2G) y toda medición reportada |
| Exhaustivo | 100 | Pre-promoción — `max(n, 100)`, hoy idéntico al estándar |

> Desde la **regla #23** (`muestra-estandar-validacion.md`, 2026-08-13) el tamaño de muestra
> es **único: N = 100**. El antiguo «modo rápido de 20» desapareció: era uno de los cinco
> números rivales que convivían sin fuente única. Invocar sin `--n` ya da el estándar.

### Ejecución

```bash
# Rápido (hook automático)
Rscript .claude/scripts/validar_multisemilla.R archivo.Rmd --n 100

# Exhaustivo (pre-promoción)
Rscript .claude/scripts/validar_multisemilla.R archivo.Rmd --modo exhaustivo
```

---

## Integración con el Hook

La FASE 2G se ejecuta automáticamente en `post-exams2-validation.sh`:

```
FASE 2A: Coherencia matemática (Niveles 1-4)
FASE 2B: Preview visual (PDF → PNG)
FASES 2C-2F: Arsenal estático
FASE 2G: Multi-semilla (100 semillas, Nivel 5; regla #23)
```

La FASE 2G solo se ejecuta si las fases anteriores no tienen errores.

---

## Códigos de Error

| Código | Nivel | Descripción | Severidad |
|--------|-------|-------------|-----------|
| `ERR_ANS_A` | 5A | exsolution dinámico evalúa a formato inválido | Bloqueante |
| `ERR_ANS_B` | 5B | Respuesta marcada no coincide con valor correcto | Bloqueante |
| `ERR_ANS_C` | 5C | Opciones duplicadas en SCHOICE | Bloqueante |
| `ERR_ANS_D` | 5D | Valor fuera de rango matemático válido | Bloqueante |
| `ERR_ANS_E` | 5E | Distractor idéntico a respuesta correcta | Bloqueante |
| `ERR_SEM_D` | 4D | calcula() contiene función aleatoria o no es determinista | Bloqueante |
| `WARN_SEM_D` | 4D | calcula() de error no seleccionado tiene aleatoriedad (bug latente) | Informativo |

---

## Variables Detectadas Automáticamente

El Nivel 5 busca estas convenciones de nombres en el entorno:

### Vectores de solución
- `sol`, `solucion`, `solucion_vector`

### Valor correcto
- `valor_correcto`, `mediana_calc`, `respuesta_correcta`
- `media_correcta`, `mediana_correcta`, `moda_correcta`

### Opciones
- `opciones_mezcladas`, `opciones_graficos`, `opciones_valores`
- `opciones`, `opciones_num`, `opciones_texto`

### Datos
- `datos_ord`, `datos`

### Cuartiles
- `cuartiles_correctos` (lista con `q1`, `mediana`, `q3`)

### Probabilidades y porcentajes
- Variables con `prob` en el nombre → rango [0, 1]
- Variables con `porcentaje`, `pct`, `percent` → rango [0, 100]

---

## Tests

Suite: `tests/testthat/test_correctitud_respuesta.R`

- 14 tests cubriendo todos los sub-niveles (5A-5E)
- Tests de detección de errores Y de aprobación de archivos válidos
- Test de integración con múltiples errores simultáneos

---

## Afirmaciones y prosa de la Solution (v1.1, 2026-10-29)

Los sub-niveles 5A-5E comparan **valores**; ninguno evalúa si un **texto** es verdadero. Esta
sección fija lo que hay que exigir cuando la clave o la Solution son proposiciones.

### Origen

- Ejercicio `A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/` (MAT-2026-07-050,
  SCHOICE de evaluación de afirmaciones), **FASE 2C ronda 1 del 2026-10-29: `RECHAZAR`**. El
  arsenal post-render había dado **0 errores**. Reporte íntegro del detractor en
  `salida/fase2c_r1_detractor.md` de ese subproyecto (20 citas verificadas).
- **H1 (CORRECCIÓN).** La Solution afirmaba que la cifra de la «razón sin sustento» «no sale de
  ningún cálculo con la tabla», y en el 2,84 % de las versiones no canónicas sí salía de uno (una
  ventaja parcial, una suma o un producto). En la semilla 109 la Solution se contradecía: usaba
  ese número unas líneas antes como resultado de un cálculo.
- **H2.** «Su total fue de 318 kilos más que el de…» admitía la lectura «su total fue de 318
  kilos» cuando la cifra era el total de la sede ganadora; con esa lectura un distractor quedaba
  verdadero y la clave falsa. Pasaba en el 17 % de las versiones.

### Por qué no lo ve el Nivel 5

`ERR_ANS_B..E` comparan la opción marcada con `sol`, `opciones*` o `valor_correcto`, y la
unicidad por valor o por hash; ninguno lee el contenido proposicional de un texto. La regla #9
(`detractor-obligatorio.md`) ya clasifica «Solution falsa» como CORRECCIÓN binaria, pero solo la
detecta el detractor.

### Las tres exigencias

- **Opciones-afirmación** (`evaluacion_afirmacion`). Cada afirmación lleva una **función de verdad
  pura** sobre los datos de la versión, y `data_generation` incluye `stopifnot(sum(verdaderas) == 1, verdaderas[clave])`. La unicidad de
  la verdadera se **construye** de forma determinista (familia F1 de la regla #21) en vez de
  resamplear hasta que se cumpla, y se **verifica** enumerando el espacio de parámetros cuando es
  finito; si no lo es, con la muestra estándar N = 100 (regla #23). La verificación necesita un
  **mutante** que revierta la construcción: si el mutante no hace fallar la verificación, esta no
  prueba nada.
- **Toda lectura razonable cuenta.** Si una frase admite otra lectura que cambia su valor de
  verdad («fue de N kilos más que» leída como «fue de N kilos»), se reescribe de modo que solo
  tenga una («superó en N kilos a»). No basta con que la lectura prevista sea correcta: el
  estudiante puede leer la otra, y entonces la clave o un distractor cambia de signo.
- **Afirmaciones absolutas de la Solution.** Frases como «no sale de ningún cálculo» o «no está en
  la tabla», dichas sobre cifras sorteadas, se garantizan **por construcción** con su propio
  `stopifnot`, o se reescriben en términos que se puedan verificar. Un «ninguno» o un «siempre»
  sobre una cifra sorteada es una afirmación sobre todo el espacio de versiones: o se prueba, o
  no se escribe.

### Lo que ya estaba verificado en ese ítem (invariante local L-2)

Para las opciones, la enumeración de 3116 combinaciones no halló ninguna con dos afirmaciones
verdaderas, y 12 mutantes murieron por su sonda. El detractor encontró una sola afirmación
verdadera, la marcada, en 2000 de 2000 versiones; un oráculo que solo lee el HTML, en 401 de 401.
Aun así el detractor halló H1 y H2: esas verificaciones evalúan la lectura prevista de cada
afirmación, y ninguna cubría las lecturas alternativas de una frase (H2) ni las afirmaciones
absolutas de la Solution (H1). El arreglo de H1 y H2 se verifica en la ronda 2 de su FASE 2C (ver
el `HANDOFF.md` del subproyecto).

### v1.2 (2026-10-29): la instancia canónica y las reescrituras medidas

Dos hallazgos del mismo ítem, en sus rondas 2 y 3, precisaron la segunda exigencia.

- **Una reescritura puede abrir un canal.** En la ronda 2 se probó a reescribir el cierre como «su total
  superó en *x* kilos al de *O*». La batería §P7 congelada subió a +18,1 pp en la rama CURSOS y a +14,3 pp
  en la rama PROMEDIO: un canal léxico. La reescritura que se adoptó en la ronda 3, «su total fue más alto
  que el de *O* en *x* kilos», no abrió ninguno. En 89 916 textos no canónicos, ninguno admite la lectura
  «total». Por eso la exigencia ya no propone una frase fija: pide reescribir y medir la reescritura con la
  batería.
- **La instancia canónica no se reescribe.** Su clave verbatim, «el total fue de 20 kilos más», admite la
  lectura «el total fue de 20 kilos», que la volvería falsa sin volver verdadera ninguna otra opción. La
  v1.1 pedía reescribirla, y la regla #24 H-2 lo prohíbe; las dos decían «sin excepciones». El profesor
  decidió el 2026-10-29 que **prevalece H-2**. La lectura alternativa de la canónica se declara como
  residuo, siempre que la función de verdad confirme que no crea una segunda clave. En el verificador de
  ese ítem, la canónica es la única excepción de la guarda de lecturas; ningún mutante la retira, y eso
  queda declarado.

### Relación con otras reglas

- Regla #9: la CORRECCIÓN es binaria (`RECHAZAR`); una Solution falsa o una segunda clave no se
  compensan con diagnosticidad.
- Regla #21, familia F1: la unicidad se construye de forma determinista, no se busca por
  reintento.

---

**Versión**: 1.2
**Fecha**: 2026-10-29 (v1.2 — la canónica conserva el impreso y las reescrituras se miden; v1.1 — afirmaciones y prosa de la Solution; v1.0 — 2026-02-14)
**Estado**: ACTIVO Y OBLIGATORIO
**Excepciones**: NINGUNA
