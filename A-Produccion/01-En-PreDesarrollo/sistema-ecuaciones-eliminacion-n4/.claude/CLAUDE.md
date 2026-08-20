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

## L-7 — Guarda contra verificación semántica vacua

Si el `proc` de un señuelo saliera idéntico al del estímulo (`proc == pr_show`), `ejecutar_prop()`
comprobaría «el estímulo no llega a R», que ya está aseverado antes: la sonda se verificaría a sí
misma. Medido vacuo en 27/100 versiones antes de la guarda. `proc_de_opcion()` aborta si ocurre.

## L-8 — La batería de §P7 está CONGELADA en `bateria_congelada.R`. No se le añaden reglas.

Pre-registro §P7-C del 2026-08-20. Cubre las seis familias y mide **por estrato**, porque las tres
ramas de este ítem son estructuralmente distintas y el agregado esconde canales al 100 %.

Si una auditoría descubre una familia sin sonda, se añade y **se re-mide el histórico completo**, o
se declara que las cifras anteriores no son comparables. Encadenar pasadas con baterías distintas y
tratar sus excesos como una serie es exactamente lo que §P7-C prohíbe, y ya pasó una vez aquí: la
batería anterior dio V4 por bueno en todo cuando había empeorado tres reglas.

Los cortes salen del helper `.claude/scripts/bateria_eliminacion.R`, **fuente única**. Este script
aborta si no lo encuentra en vez de inventarlos.

## L-9 — `V1` (divisor = coeficiente de su propia ecuación) es TECHO ESTRUCTURAL, no un canal más.

Medido: **100 % en el estrato del paso 3**, exceso +75,0 pp, margen 50-75 %.

**No se corrige eligiendo otros divisores.** Dividir entre el coeficiente *es* la operación
correcta: si la opción muestra la ecuación **y** el divisor, comprobar su coherencia identifica la
clave sin resolver nada. La única salida es no mostrar la ecuación — variante V3, medida, que
reintroduce el canal de escala (`M5` −15,5 pp con margen del 68,6 %).

**No gastar pasadas persiguiéndolo.** Es la decisión estructural de `HANDOFF.md` §10.7.1(c) y le
corresponde al profesor, no al verificador.

## L-10 — Tras un borrado por patrón, comprobar PRESENCIA DE SÍMBOLOS, no que el código parsee.

El 2026-08-20 un regex demasiado amplio borró cinco funciones vivas (`proc_de_opcion`,
`ejecutar_prop`, `paso_de`, `txt_clave`, `txt_senuelo`) y **el chunk seguía parseando**. Una
comprobación de sintaxis no lo habría visto. Lo cazó `grep -c '^<fn> <- function'`.
