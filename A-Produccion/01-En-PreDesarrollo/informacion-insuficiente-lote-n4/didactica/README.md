# Material didáctico del ítem MAT-2026-1-010 («¿Alcanzan los datos?»)

Tres páginas HTML autocontenidas (sin internet) para enseñar en 10.° y 11.° la versión canónica
del ítem, la de `../ITEM_OFICIAL.md`, con poca intervención del docente.

| Archivo | Para quién | Qué es |
|---|---|---|
| `actividad_estudiante.html` | estudiantes | Actividad en 8 pasos, con lote interactivo, comparación, casos de transferencia e ítem propio. Funciona sola (guarda en el celular) y dentro de Moodle |
| `guia_estudiante.html` | estudiantes (sin la clave ni la palabra del paso 5) | Paso a paso para entrar a la actividad en Moodle y completarla: píldora de guardado, mensajes del docente, los 8 pasos con sus botones y qué hacer si algo falla. Revisada a 320 y 360 px, en claro y oscuro. Se sube como recurso «Archivo» |
| `guia_docente.html` | docente (contiene la clave) | Análisis del ítem, matriz de 21 métodos con su evidencia verificada, secuencia de dos sesiones, criterios de comprensión y plan B en papel |

## Dentro de Moodle: plugin `mod_aulaviva`

`actividad_estudiante.html` es la **fuente única** de la actividad `lote_d49` del plugin «Actividad
en vivo» del Moodle icfes. El plugin guarda cada respuesta a nombre del estudiante y le da al
docente un panel en vivo con retroalimentación escrita.

- Código del plugin: `/home/bootcamp/Proyectos-2026/Proyectos-Varios/Moodle/plugins/aulaviva/`
  (ver su `README.md`; diseño en `../PRPs/mod-aulaviva-actividades-en-vivo.md` de ese repo).
- Al editar `actividad_estudiante.html`:
  1. Sincronizar con `Proyectos-Varios/Moodle/plugins/sincronizar_actividades.sh`.
  2. Comprobar con su nivel 0 (`plugins/aulaviva/tests/local/verificar_local.sh`), que recorre la
     actividad sola y dentro de Moodle en un navegador sin interfaz.
  3. Desplegar con `plugins/desplegar.sh --apply --verificar`.
- La actividad usa `window.AulaViva` solo si existe. Sin Moodle sigue guardando en `localStorage`.
  Es el contrato del README del plugin.

## Estado

- Las páginas no dependen del `.Rmd` del subproyecto, que sigue en revisión. Usan el ítem oficial
  y cuatro casos de transferencia verificados a mano contra el modelo de determinación del `.Rmd`.
- Plugin instalado en producción el 2026-10-20. Falta la aceptación del profesor en el curso `lab`
  y la aplicación en aula.
- 2026-10-20: la segunda decisión queda fija al guardarla y el paso 5 (explicación y clave) se abre
  con la palabra «lindero», que el docente dice al terminar el cierre (guía, secciones 1 y 7). Antes
  se abría al guardar la segunda decisión y esta se podía cambiar después, así que el conteo del panel
  salía inflado. Desplegado el 2026-10-20 a las 16:22 (sha256 `8c452816…2aec1404`, sin deriva).
- 2026-10-20 17:43, provisional y desplegado: en modo Moodle ya no se vota con los dedos. El paso 1
  (`#p1vote`) dice «Espere en silencio a que todos guarden…», y lo mismo dicen la secuencia de la guía
  docente y el paso 1 de `guia_estudiante.html`. Sin Moodle se conserva el voto con los dedos (plan sin
  conexión). El arnés del nivel 0 del plugin vigila los dos textos.
- Pendiente: las barras del curso en cada celular, que el docente libera desde el panel. Su PRP es
  `Proyectos-Varios/Moodle/PRPs/mod-aulaviva-barras-del-curso.md`, sin ejecutar; antes hay que resolver
  su P-1. Cuando exista, los textos provisionales pasan a hablar de las barras.
