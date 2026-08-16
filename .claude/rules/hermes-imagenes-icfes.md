# Regla #24 — Hermes: triaje y fidelidad de figuras de cuadernillo ICFES

## Principio Fundamental

**Antes de reproducir CUALQUIER figura de un cuadernillo ICFES escaneado —sea redibujándola con el
Graficador Experto (TikZ/Python/R, regla #3) o generándola— hay que MIRAR el recorte del escaneo y
clasificarla. La descripción textual del ítem (`[FIGURA: …]`, `¿Qué evalúa?`, `Tema Específico`)
SOBRE-clasifica sistemáticamente: describe como "gráfica de líneas" lo que es una tabla, y como
"ilustración" lo que lleva cifras answer-critical.**

Esta regla importa al repositorio ICFES R/exams la estrategia **Hermes**, desarrollada y validada en
el proyecto hermano Todo-Pajaro entre 2026-07-03 y 2026-08-05 sobre lotes reales de Saber 11. NO es
teoría: cada lección de abajo nació de un incidente medido, y varias de ellas describen modos de
fallo en que **un artefacto correcto a la vista destruye la respuesta del ítem**.

## Qué es Hermes y qué parte de él rige aquí

Hermes es un harness autónomo `generar → juzgar(PVE) → afinar → iterar` que redibuja figuras de
cuadernillos escaneados, con un **gate PVE de fidelidad** que decide si la reproducción respeta
rótulos, cifras, posiciones y conteos. Su motor ejecutable vive en Todo-Pajaro:

```
MOTOR_HERMES=/home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Mejorando-Imagenes/motor-hermes
```

| Pieza | Ruta | Qué hace |
|---|---|---|
| Harness | `$MOTOR_HERMES/harness_mejora.py` | Loop generar→juzgar→afinar sobre un manifest de ítems |
| Triaje visual | `$MOTOR_HERMES/clasificar_visual.py` | El juez-visión MIRA el crop y dictamina `{tipo, riesgo, features_criticos}` |
| Gate de fidelidad | `$MOTOR_HERMES/gate_fidelidad.py` | 4 ramas: numérica · celda-a-celda · inventario de geometría · checklist visual |
| Preparación | `$MOTOR_HERMES/auto_prep.py` | Recorte al borde del CONTENIDO + screening de trampas |
| Política aprendida | `$MOTOR_HERMES/entrenamiento/politica_aprendida.json` | Datos, no código; el motor la lee al arrancar |
| Estrategia completa | `.claude/skills/hermes-imagenes/SKILL.md` (copia congelada aquí) | Las 13 lecciones íntegras |

**Reparto de responsabilidades en ESTE repositorio.** El generativo de Hermes NO produce las figuras
de nuestros `.Rmd`: aquí las figuras son **vectoriales y dinámicas** (TikZ/Python/R, regla #3), porque
deben regenerarse por versión con parámetros aleatorios (regla #22). Lo que Hermes aporta y aquí es
obligatorio son sus **dos gates**:

1. **Gate de triaje** — qué es realmente la figura y qué de ella es answer-critical.
2. **Gate de fidelidad** — que nuestra reproducción vectorial no altere ni un rótulo del original.

Es decir: en el vocabulario de Hermes, **este repositorio siempre está en la rama `tipo:"datos"` →
vector**. Lo que importamos es su criterio para no romper el ítem al redibujarlo.

## Las 5 lecciones de Hermes que son OBLIGATORIAS aquí

### H-1 — Gate visual: MIRA el recorte, no confíes en la descripción

Antes de decidir si el ejercicio requiere Flujo B (regla #2) y qué debe dibujar, hay que **leer el JPG
del cuadernillo** con la herramienta Read y describir lo que se ve, no lo que el `.md` de alineación
dice que hay. Incidente original: `Q143` "tres recipientes sobre estufas" parecía ilustración; el JPG
mostraba una **tabla de solubilidad**. `Q053` "alambres" traía dimensiones A=1/2 cm², 20/10 cm — todas
answer-critical.

Consecuencia directa sobre la decisión `flujo_b.requerido`: un recuadro de texto con una fórmula NO es
una gráfica (se reproduce con Markdown + LaTeX, F3 `eq_display()`); una gráfica con cifras rotuladas SÍ
exige Graficador Experto. Declarar cuál de los dos es, con la evidencia del JPG citada.

### H-2 — ⛔ La TRAMPA o el error deliberado ES la pregunta: NUNCA "corregir"

Muchos ítems ICFES incluyen inconsistencias **deliberadas** en el estímulo (barras que no cuadran con
su tabla, un total errado, una etiqueta mal escrita) y la pregunta pide detectarlas. Al redibujar, la
tentación —humana y de cualquier modelo— es "arreglar" la inconsistencia, y eso **destruye el ítem**:
vuelve verdadera una opción que era falsa.

**Reproduce la figura tal cual, INCLUIDOS sus errores. Jamás normalices ni hagas coincidir dos
representaciones.** Incidente `Q067`: la gráfica discrepaba de su tabla a propósito (clave C = "la
gráfica presenta MAL la cantidad"); la reproducción la "corrigió" y habría hecho verdadera la opción B.

Screening obligatorio del enunciado antes de dibujar — si aparece cualquiera de estos patrones, el ítem
es **fidelidad-crítico** y su figura NO se toca sin verificación humana contra el escaneo:

```
"cuál es el error" · "misma información" · "no coincide" · "presenta mal" · "inconsistente"
"¿es correcta la gráfica/tabla?" · "X afirma que… ¿es verdadera la afirmación?"
```

Los dos últimos son el patrón `CORRIGE_REP` y la marca `VALIDACION`, añadidos tras `q107` y `Q058`.

### H-3 — Gate de fidelidad POR TIPO, no un checklist único

Un solo criterio de "se parece" produce falsos negativos y, peor, falsos positivos. Hermes usa cuatro
ramas según lo que la figura sea, y aquí se aplican al comparar nuestra figura vectorial contra el JPG:

| Tipo de figura | Cómo se verifica | Falla que atrapa |
|---|---|---|
| Cifras rotuladas (barras) | Comparar 3 fuentes: reproducción vs descripción vs escaneo | Valor alterado |
| Tabla | **Celda a celda** (misma fila/columna/encabezado) | Ecuaciones `n!`, intervalos `15≤p<20`, unidades, comas decimales, columna perdida |
| Geometría / física rotulada | **Inventario BIDIRECCIONAL de rótulos** | Etiqueta **agregada** que no está en el original |
| Curvas / dispersión | Checklist dirigido: forma, cortes de eje, tramos, intersecciones, **estilo de línea por serie** | Estilos sólida/punteada intercambiados |

**Por qué el inventario bidireccional importa aquí:** el checklist de forma daba FIEL a una reproducción
que había **agregado** un rótulo espurio (`q090`: una "A" inventada en el vértice de una pirámide,
copiada de la opción vecina). Un checklist que solo pregunta "¿se parece?" no enumera rótulos, así que
no ve lo que sobra. Hay que enumerar las etiquetas de AMBAS y comparar en las dos direcciones.

**Corolario para nosotros:** la regla #22 §P6 ya prohíbe que el nombre del archivo delate el rol de la
opción. H-3 añade el gemelo: que la figura no delate ni contradiga nada por lo que DIBUJA de más.

### H-4 — Crop al borde del CONTENIDO y con el número IMPRESO como ancla

- **Ancla = el número impreso en el JPG, NO el índice del mapeo.** Los mapeos página↔pregunta acumulan
  desfase: la página que el mapeo daba como "Q143" imprimía "146". Verifica el número impreso.
- **El recorte va al borde del contenido, no a un % fijo.** Un crop al 86 % cortaba la 8.ª columna de las
  tablas anchas y el gate no podía certificarla. Si una figura ancha "no se puede verificar", **regenera
  el crop antes de sospechar del gate**.
- Una celda **ilegible en el escaneo cuenta como falla** (falla-seguro a humano), nunca como "asumamos
  que dice lo esperable".
- Si el crop cae en una página sin la figura (enunciado partido en dos páginas, `q038`), la reproducción
  se estaría **fabricando desde el texto**, sin original contra el cual verificar → PARAR.

### H-5 — Asimetría de seguridad (regla de oro)

**Solo se permite ENDURECER de forma autónoma** (añadir un patrón de trampa, subir el piso de riesgo de
un tipo que demostró ser frágil). **RELAJAR nunca es autónomo**: baja de riesgo, o dar por buena una
figura que un gate marcó dudosa, requiere aprobación humana explícita.

> Un falso positivo cuesta trabajo; un falso negativo corrompe un ítem.

Esta asimetría es la misma que ya rige en el repo para el detractor (regla #9) y para los estratos con
`n < 20` de la regla #23: ante duda, **NO CONCLUYENTE**, nunca verde.

## Integración con el workflow ICFES (dónde se ejecuta cada gate)

| Paso del workflow | Gate Hermes | Qué se exige |
|---|---|---|
| Pre-flight, antes del paso 1 | **H-1** + **H-4** | Leer el JPG, verificar el número impreso, describir lo que se ve |
| Paso 2 (`flujo_b`) | **H-1** | La decisión `requerido: true/false` se justifica con lo VISTO, no con el `.md` |
| Paso 3 (`generacion_rmd`) | **H-2** | Screening de trampa. Si dispara → la figura no se altera y se declara |
| Paso 8 (`coherencias_5`) | **H-3** | Coherencia Visual-Texto se verifica con la rama que corresponda al tipo |
| Paso 7 (`detractor_fase2c`) | **H-2** + **H-3** | El detractor audita explícitamente si la reproducción "corrigió" algo |

## Antipatrones PROHIBIDOS

| Antipatrón | Por qué |
|---|---|
| Decidir `flujo_b.requerido` leyendo solo la ficha `.md` | La descripción sobre-clasifica (H-1) |
| "Arreglar" una inconsistencia del original al redibujar | Destruye el ítem: vuelve verdadera una opción falsa (H-2) |
| Verificar una tabla comparando el conjunto de cifras | Aplana la estructura; no ve fórmulas, intervalos ni columnas perdidas (H-3) |
| Verificar geometría preguntando "¿se parece?" | No enumera rótulos → no ve el que se AGREGÓ (H-3) |
| Confiar en el mapeo página↔pregunta | Tiene desfase acumulado (H-4) |
| Dar por buena una figura que un gate marcó dudosa | Relajar no es autónomo (H-5) |
| Reproducir una figura cuyo crop no la contiene | Se fabrica desde el texto, sin original que verificar (H-4) |

## Excepciones

**NINGUNA** para H-2 y H-5. Para H-3, si la figura es puramente decorativa y ningún dato suyo entra en
el razonamiento del estudiante, se declara así explícitamente en el reporte y se salta la rama de
fidelidad — pero esa declaración es un juicio que hay que escribir, no un silencio.

## Referencias

- Estrategia completa (copia congelada): `.claude/skills/hermes-imagenes/SKILL.md`
- Motor ejecutable: `$MOTOR_HERMES/` en Todo-Pajaro (fuente única; **no forkear el código aquí** —
  dos copias divergentes del mismo gate es el modo de fallo del invariante I-10)
- Regla #2 `flujo-b-obligatorio.md` · Regla #3 `graficador-secuencial.md` · Regla #4
  `graficos-como-opciones.md` · Regla #22 `diversidad-sustantiva.md` §P6

---

**Versión:** 1.0
**Fecha:** 2026-08-15
**Estado:** ACTIVO Y OBLIGATORIO
**Origen:** importación permanente de la estrategia Hermes desde Todo-Pajaro
(`motor-hermes/skill-hermes/SKILL.md` v1.9.0, 13 lecciones validadas 2026-07-03 → 2026-08-05)
**Aplica a:** todo ejercicio `.Rmd` derivado de un ítem de cuadernillo ICFES que contenga figura.
