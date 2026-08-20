# Reglas locales — sistema-ecuaciones-eliminacion-n4

Invariantes de ESTE subproyecto. Prevalecen sobre el criterio genérico de cualquier agente
dentro de este directorio. Si una contradice una regla del repo raíz, gana la del raíz y el
conflicto se REPORTA, no se resuelve en silencio.

Origen: `MAT-2026-1-044` (ERA-2026 Sesión 2, pregunta impresa **44**, `pagina_013.jpg`),
verbatim de MAT-2026-1-130.

## L-1 — La opción D del cuadernillo NO tiene errata. NO "corregirla".

Un diagnóstico previo afirmó que `60L + 100R = 1.300.000` era errata del ICFES y que lo
correcto sería `40L`. **Es falso, y medido:** D dice «debió **sumar las ecuaciones**», y las
ecuaciones que el Paso 2 opera son E1 (`30L+60R=900.000`) y la del Paso 1 (`30L+40R=400.000`).
`30+30=60` · `60+40=100` · `900.000+400.000=1.300.000`. Exacto.

El `40L` sale de sumar E1 con E2 **original**, que no es lo que D describe.

Consecuencia: el señuelo de `ERR-ALG-05` se calcula SIEMPRE sobre la ecuación **mostrada en el
Paso 1** (`2a·L + (b+d_p)R = P1+P2_p`), nunca sobre E2 original. Un refactor que lo derive de
E2 original rompe la fidelidad al cuadernillo Y la aritmética. Blindado con `stopifnot()`.

## L-2 — Prohibido el distractor «multiplicar toda la ecuación por un factor de signo opuesto y SUMAR»

Es una ruta **matemáticamente válida**: ×(−k) toda E2 y sumar da el mismo R correcto
(verificado: L=20.000, R=5.000 por las dos rutas). Sería una segunda clave correcta.

La opción A canónica NO cae aquí: multiplica por −k **solo el primer término**, lo que sigue
violando la propiedad uniforme. Consérvala verbatim.

## L-3 — La instancia canónica viola `b > k·d`; es deliberado

Las versiones paramétricas exigen `b > k·d` para que el Paso 2 correcto no arroje coeficientes
negativos. El canónico tiene `b=60 < k·d=120` y se exceptúa por fidelidad (regla #24 H-2). No
"normalizar" el canónico para que cumpla la restricción general.

## L-4 — Ceguera declarada de sondas

Las 4 opciones comparten primera palabra («En»). Por construcción: **H2 = 0 % y H3 no se
imprime**. El relevo es **H3b**, que borra los dígitos de la firma — por eso lo que DEBE variar
entre versiones es el **vocabulario del procedimiento**, no sólo el número del paso.
Parametrizar sólo el número del paso produciría un PASS falso.

## L-5 — La Solution NO lleva nota sobre la opción D ni sobre el cuadernillo

Decisión del profesor: verbatim, sin nota editorial.

## L-6 — `calcula()` / `ejecutar_proc()` son funciones PURAS

Prohibido `sample`/`runif`/`rnorm` dentro (Capa D, `ERR_SEM_D`). El veredicto de cada opción se
obtiene EJECUTANDO su propuesta, no declarándolo.
