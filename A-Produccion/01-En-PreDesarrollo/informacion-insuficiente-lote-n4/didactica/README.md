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
- 2026-10-20, detractor independiente (APROBAR_CON_CAMBIOS, 5 hallazgos altos). Los puntos 1 y 2 de su
  plan están aplicados:
  - **H1:** la pregunta de Sara ya no presupone la clave: «¿Tiene razón Sara? Sí / No» y la
    justificación con un reparto (`p3.saraOK`).
  - **H5:** el caso C1 pregunta «¿Qué lugar del acta ocupa el asociado que recibe la franja que da a la
    carretera?» → el primero. Antes hubo tres versiones con dos lecturas, cada una detectada por un
    detractor: «¿Cuál de los asociados…?» (el nombre no se conoce), «¿Qué porción…?» (cuota) y
    «¿Cuál es la ubicación…?» (la región depende de cuántos asociados son). Preguntar por la posición en
    el acta esquiva las tres, y C1 queda como espejo de B2.
  - **H2:** en la sesión 2, la regla de memoria y la decisión de cada caso se guardan solas y quedan
    fijas antes de hablar (`regla2`, `casos[X].r1`).
  - **H3:** el panel muestra el primer intento: columnas «Casos (antes de hablar)» (`ok1`) y «Cambian
    de estado (antes de hablar)» (`c1`, E4).
  El arnés del nivel 0 vigila las cuatro, con mutantes probados. Pendientes del plan: H4 (cuestionario
  diferido con CBM), H6-H12.
- 2026-10-20, dos reauditorías (RECHAZAR y RECHAZAR) y sus correcciones:
  - **N1:** «Compare los casos» solo con las cuatro decisiones; títulos neutros hasta decidir.
  - **N6/M2:** E4 = B1, C1, C2 y D1. El caso A no cuenta, y la guardia del arnés decide bien A1 para
    que un mutante que lo vuelva a contar dé rojo.
  - **N3:** mientras se escribe la regla de memoria, los pasos 1 a 5 y el resumen se ocultan.
  - **N4:** la primera y la segunda decisión quedan fijas completas (letra, seguridad y razón), y el
    paso 3, al guardar la segunda.
  - **N5:** en Moodle no aparece «Ya hice la sesión 1 en otro celular»; sin Moodle pide la palabra.
  - **N7:** un caso comprobado sin decisión previa (estado viejo) no ofrece fijarla; en Moodle no
    aparece «Borrar todas mis respuestas».
  - **M3:** redacción de «Compare los casos» y guardia de los cuatro títulos.
  Cada corrección tiene su guardia en el arnés (5 páginas, una de «estado viejo»), y seis mutantes la
  ponen en rojo. Desplegado el 2026-10-20 (`activity.html` sha256 `7ae9d137dc09f78f…`, sin deriva).
- Pendiente: las barras del curso en cada celular, que el docente libera desde el panel. Su PRP es
  `Proyectos-Varios/Moodle/PRPs/mod-aulaviva-barras-del-curso.md`, sin ejecutar; antes hay que resolver
  su P-1. Cuando exista, los textos provisionales pasan a hablar de las barras.
