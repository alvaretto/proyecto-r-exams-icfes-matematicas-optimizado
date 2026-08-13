# Regla #23 — Muestra Estándar de Validación: **N = 100 versiones**

## Principio Fundamental

**Toda medición estadística sobre versiones de un ejercicio usa exactamente `N = 100`. Ese número
es único, vive en código ejecutable y NO se elige por sesión, por agente ni por handoff. Está
PROHIBIDO reportar una medición con otro N sin que esta regla lo autorice explícitamente.**

Aplica a los validadores del arsenal, a los verificadores propios de cada ejercicio, a los smokes
y a cualquier cifra que un agente escriba en un reporte con la forma «medido sobre N versiones».

---

## Origen: la deriva de los cinco números (2026-08-13)

El repositorio tenía **cinco tamaños de muestra rivales conviviendo**, y ninguno era fuente única.
Medido con `grep` sobre `.claude/` antes de esta regla:

| N | Dónde vivía | Ocurrencias |
|---|---|---|
| 40 | `validar_diversidad_sustantiva.R` (default), reglas #21/#22, ambos orquestadores, hook FASE 2N | 19 |
| 30 | `validar_diagnosticidad.R` (default), skill `stress-test-visual` | 3 |
| 20 | `validar_multisemilla.R` (default), regla #13, hook FASE 2G | 5 |
| 10 | pre-flight de ambos orquestadores, hook | 4 |
| 5  | `commands/validate.md` | 1 |

Ninguno de esos números estaba justificado por una medición: eran valores heredados de la sesión
que creó cada archivo. La consecuencia es que **cada agente elegía el suyo** —y algunos elegían
400, muy por encima de lo necesario, gastando minutos de cómputo por corrida sin ganar precisión
que nadie hubiera pedido—. El profesor lo había aclarado verbalmente varias veces; la instrucción
no se sostuvo porque **no existía en ningún sitio que se ejecutara**.

Es exactamente la deriva que este repositorio ya sufrió con la nomenclatura de archivos (v3.20.8):
*un valor que solo vive en la prosa pierde contra uno que vive en un flag*. Por eso esta regla no
se limita a declarar el número: lo cablea en los defaults, lo propaga a cada invocación y lo fija
con un test que falla si reaparece otro.

---

## El número y su alcance

### Familia A — mediciones sobre `data_generation` (NO renderizan): **N = 100, sin excepción**

Son baratas porque solo evalúan el chunk `data_generation`, sin `pdflatex` ni pandoc.

| Herramienta | Default anterior | Default vigente |
|---|---|---|
| `validar_diagnosticidad.R` | 30 | **100** |
| `validar_diversidad_sustantiva.R` | 40 | **100** |
| `validar_multisemilla.R` | 20 | **100** |
| Verificadores propios del ejercicio (`auditoria_propia.R`, `verificar_render.R`, smokes) | ad-hoc (250–400) | **100** |

Invocarlas **sin** `--n` ya produce el estándar. Pasar `--n 100` explícitamente es redundante pero
válido; pasar cualquier otro valor exige la justificación de la sección siguiente.

Coste medido sobre `informacion_insuficiente_..._cloze_v1` (el ejercicio con el motor más pesado
del repositorio, banco de 25 preguntas con reintentos acotados), 2026-08-13:

| Herramienta | N anterior | N = 100 |
|---|---|---|
| `validar_diversidad_sustantiva.R` | 20 s (n=40) | **54 s** |
| `validar_diagnosticidad.R` | 15 s (n=30) | **55 s** |
| `validar_multisemilla.R` | — | **61 s** |

### Familia B — muestras de RENDERIZADO real: N = 100 cuando el tiempo lo permita, **declarado siempre**

Cada unidad cuesta un PDF compilado o una captura de navegador, así que el coste no es comparable:
`stress_test_visual.R` y `auditor-visual-html` renderizan de verdad. Para ellas:

1. El objetivo sigue siendo **100**.
2. Si se usa menos, el reporte **DEBE declarar la cifra y la razón** en la misma línea en que da el
   resultado. Un «auditoría visual OK» sin número es un reporte incompleto, no un aprobado.
3. **Nunca** se presenta una muestra reducida como si acreditara lo mismo que el estándar.

### Cuándo se permite un N distinto de 100

Solo tres casos, y los tres **exigen declarar el número en el reporte**:

- **Depuración durante el desarrollo**: mientras se itera un fix se puede usar un N bajo para ganar
  vueltas. La medición que se REPORTA es la de 100.
- **Familia B**, según lo anterior.
- **Enumeración exhaustiva**: cuando el espacio de versiones es finito y pequeño, se enumera
  **completo** y se dice cuántas son. Un espacio de 318 combinaciones se recorre entero; muestrear
  100 de 318 sería perder cobertura gratis.

- **Análisis ESTRATIFICADO** (por rama, por gap, por cualquier partición): ver la sección
  siguiente. El N global se reparte entre los estratos, así que 100 puede dejar el estrato más
  ligero sin muestra suficiente.

Subir de 100 «por si acaso» NO es uno de los casos. Si una medición necesita más de 100 para ser
concluyente, el problema es la sonda —o es un análisis estratificado, y entonces se aplica lo
siguiente.

---

## Análisis estratificado: el N global no es el N del estrato

La regla #22 v1.3 exige medir **condicionando por rama** cuando un ítem tiene ramas
estructuralmente distintas, porque el agregado no acredita. Pero al condicionar, **el N se
reparte**: con 5 ramas de pesos desiguales, N=100 puede dejar la más ligera en n≈7.

**Una tasa sobre 7 casos no es una medición.** Medido en `informacion_insuficiente_..._cloze_v1`
el 2026-08-13, con el umbral de fuga léxica en el 70 %:

| Muestra | n de la rama `LT--` | Tasa del token `área` | Veredicto |
|---|---|---|---|
| N = 100 | 7 | **71,4 %** (5 de 7) | RECHAZADO ← **falso** |
| N = 400 | 29 | **48,3 %** (14 de 29) | APROBADO |

Un solo caso movía la tasa 14 puntos y cruzaba el umbral. **Un rojo falso es peor que no medir**:
se aprende a ignorarlo, que es exactamente cómo la FASE 2G llegó a llevar meses en rojo permanente
sin que nadie la mirara.

**Regla:** un estrato con **n < 20** no se declara ni verde ni rojo. Se declara **NO CONCLUYENTE**,
se nombra, y se calcula el N que lo dictaminaría:

```
N_necesario = N_actual × 20 / n_del_estrato_más_pequeño
```

El verificador **no puede sellar** con estratos sin medir: su veredicto es `SIN VEREDICTO`
(exit 1), nunca `APROBADO`. Está PROHIBIDO presentar «0 hallazgos en lo medido» como aprobación.

Esto NO reabre la elección libre del N: el número sigue saliendo de una fórmula con una entrada
medida (el peso del estrato menor), no del criterio de la sesión. Implementación de referencia:
`auditoria_propia.R` del CLOZE `informacion-insuficiente-lote-n4` (`MIN_ESTRATO`, `marcar_corto()`).

---

## Lo que NO cambia esta regla

**El requisito de producto de 250+ versiones únicas sobre 300 intentos** (regla #3 de
`codigo-rmd.md`, paso 9 del workflow) es otra cosa y sigue igual. Ahí el número no es un tamaño de
muestra para medir una propiedad: es el umbral de diversidad que el ejercicio debe alcanzar. No
confundir «cuántas versiones genero para MEDIR» (100) con «cuántas versiones únicas debe ser capaz
de producir el ejercicio» (250 de 300).

---

## Defensa Automática (3 capas)

### Capa 1 — Defaults en el código ejecutable

Los tres validadores de la Familia A tienen `100` como valor por defecto. Un agente que invoque sin
`--n` obtiene el estándar sin saber que existe esta regla.

⚠️ `validar_multisemilla.R` es un **symlink**: el archivo real es
`SOURCES/scripts_validacion/validar_multisemilla.R` (invariante I-10). Editar la ruta de
`.claude/scripts/` no surte efecto.

### Capa 2 — Cableado de cada invocación

Toda invocación en `.claude/{agents,commands,rules,hooks,skills}` usa `--n 100`. El hook
`post-exams2-validation.sh` incluido: su timeout subió de 120 s a **300 s** para que el estándar
quepa (170 s medidos en el peor ejercicio + el resto de fases). Subir ese timeout es legítimo;
**acortarlo NO** (regla #4 del CLAUDE.md raíz).

### Capa 3 — Test de regresión

`tests/testthat/test_muestra_estandar.R` barre `.claude/` y falla si:

- aparece `--n <valor>` distinto de 100 junto a un validador de la Familia A;
- alguno de los tres defaults deja de ser 100;
- el timeout del hook baja de 300 s.

Lleva sus controles positivos: el test se verificó por mutación (introduciendo un `--n 40`) y falla
nombrando el archivo y la línea.

---

## Antipatrones PROHIBIDOS

| Antipatrón | Por qué |
|---|---|
| Elegir el N «según lo pesado que parezca el ejercicio» | Es la deriva que originó esta regla: cinco números y ninguna razón escrita |
| Copiar el N de un HANDOFF o de un reporte anterior | Los handoffs no son fuente de verdad ejecutable; el default del script sí |
| Reportar «medido sobre 400 versiones» | Gasta minutos por corrida sin ganar nada que se haya pedido |
| Dar una cifra de auditoría visual sin decir cuántas versiones se miraron | Un porcentaje sin denominador no es una medición |
| Cambiar el 250/300 de diversidad creyendo que es lo mismo | Son cosas distintas: umbral de producto ≠ tamaño de muestra |

---

## Tests Asociados

| Test | Verifica |
|---|---|
| `tests/testthat/test_muestra_estandar.R` | Defaults = 100, invocaciones = 100, timeout del hook ≥ 300 s |
| `tests/testthat/test_infraestructura_claude.R` | I-1..I-10 siguen en verde tras el cambio |

---

**Versión:** 1.0
**Fecha:** 2026-08-13
**Estado:** ACTIVO Y OBLIGATORIO
**Excepciones:** las tres declaradas en «Cuándo se permite un N distinto de 100», todas con
obligación de declarar la cifra en el reporte.
**Aplica a:** todo agente, skill, comando, hook, script y reporte de este repositorio.
