# PRE-REGISTRO — vara §P7 para ítems cuyas OPCIONES SON ECUACIONES
Fijado ANTES de calcular ningún exceso. Fecha: 2026-08-19.

## 0. Qué se inspeccionó antes de fijar esto (declarado)
Se inspeccionaron FORMAS de opciones (una muestra de 40 líneas `- X: ...` de 2026-1) para
poder escribir un clasificador. NO se calculó ninguna tasa, ningún techo nulo y ningún
exceso antes de fijar los puntos 1-3. Escribir un criterio sin mirar la forma del dato
sería imposible; mirar tasas antes de fijarlo sería el sesgo que este protocolo evita.

## 1. CRITERIO DE PERTENENCIA A LA FAMILIA (operacional, sin juicio)

Normalización de cada opción (tras quitar el prefijo `  - X: `):
  - se retira markup LaTeX: `\(`, `\)`, `$`, `\dfrac{a}{b}`->`a/b`, `\frac`->`/`,
    `\times`->`x`, `\cdot`->`.`, `\le`->`<=`, `\ge`->`>=`, `\left`,`\right`, llaves;
  - se colapsan espacios.

EXCLUSIÓN PREVIA (el ítem sale de AMBOS grupos, no de uno solo):
  - alguna opción contiene `[FIGURA:` o una imagen Markdown `![`  -> el contenido real
    no es texto y no se puede clasificar con fidelidad;
  - el ítem no tiene exactamente 4 opciones, o no tiene clave parseable en A-D.

Una OPCIÓN es ALGEBRAICA si cumple LAS TRES:
  (a) contiene un SÍMBOLO DE VARIABLE: letra latina aislada
      regex `(?<![A-Za-zÀ-ÿ0-9])[A-Za-z](?![A-Za-zÀ-ÿ0-9])`
      que NO esté en {y,o,u,e,a,Y,O,U,E,A}  (conjunciones/preposiciones/artículos
      españoles y letras de rótulo);
  (b) contiene un OPERADOR: uno de  = + − - × · * / ^ < > ≤ ≥ √ ;
  (c) es MAYORMENTE SIMBÓLICA: como máximo 2 palabras alfabéticas de ≥4 letras.

Un ÍTEM pertenece a la FAMILIA-ECUACIÓN si LAS 4 opciones son ALGEBRAICAS.
En caso contrario pertenece al CONTROL (valores, prosa, o mezcla).

Este criterio NO se cambia tras ver resultados. Si resultara mal calibrado, se declara
como CORRECCIÓN EXPLÍCITA con las DOS cifras (antes y después).

## 2. BATERÍA — la MISMA para los dos grupos. Cierre por las 6 familias.
28 reglas genéricas sobre el vector de 4 opciones (definidas en `bateria.R`):
  magnitud (5) · divisibilidad (5) · signo (4) · posicion (4) · formato (5) · lexico (5)
Ninguna familia queda sin sonda -> no se invoca `familias_no_aplicables`.
La comparación FAMILIA vs CONTROL es apples-to-apples porque la batería es idéntica.

## 3. N, TECHO NULO Y CORTES
  - N = TODOS los ítems del grupo tras dedup (sin submuestreo).
  - Dedup por (enunciado normalizado + las 4 opciones normalizadas).
  - Techo nulo: `evaluar_bateria()` del helper, n_perm = 400, clave permutada
    UNIFORMEMENTE sobre posiciones, reglas intactas. sd = sd de los 400 máximos nulos.
  - Cortes: los del helper SIN TOCAR (ruido +2 pp, canal +8 pp, k_sigma = 2).
  - SOPORTE MÍNIMO: si un grupo tiene < 20 ítems -> NO CONCLUYENTE por soporte
    (regla #23). No se redondea a conclusión.

## 4. FUENTES INCLUIDAS / EXCLUIDAS (fijado antes de medir)
INCLUIDAS (las que el encargo nombra, todas ICFES Saber 11 Colombia):
  2025-2 · 2026-1 · Marzo-2026 · Kafir · SAI-CND10S223 · Individuales-2026 (I26B)
  · Simulacro-2026 · ERA-2026 (Simulacros/)
EXCLUIDAS con razón:
  - `Copia de Alineacion-...-2025-2.md`  -> duplicado literal del mismo archivo;
  - `Enem-20`   -> ENEM brasileño, no es ítem ICFES;
  - `QSQS-G7`   -> Quiero Ser Quiero Saber grado 7, no es Saber 11.

## 5. LIMITACIÓN DE LA MUESTRA, DECLARADA DE ANTEMANO
Las claves de este corpus NO son oficiales del ICFES: se derivaron por triangulación
de modelos (Claude+Gemini+ChatGPT). Cualquier propiedad medida puede ser de los modelos
y no del ICFES. Se reporta como limitación, no se atribuye al ICFES.
