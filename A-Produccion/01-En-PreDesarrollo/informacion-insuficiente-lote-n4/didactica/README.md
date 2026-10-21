# Material didáctico del ítem MAT-2026-1-010 («¿Alcanzan los datos?»)

Tres páginas HTML autocontenidas (sin internet), más un cuestionario de verificación diferida, para enseñar en 10.° y 11.° la versión canónica
del ítem, la de `../ITEM_OFICIAL.md`, con poca intervención del docente.

| Archivo | Para quién | Qué es |
|---|---|---|
| `actividad_estudiante.html` | estudiantes | Actividad en 8 pasos, con lote interactivo, comparación, casos de transferencia e ítem propio. Funciona sola (guarda en el celular) y dentro de Moodle |
| `guia_estudiante.html` | estudiantes (sin la clave ni la palabra del paso 5) | Paso a paso para entrar a la actividad en Moodle y completarla: píldora de guardado, mensajes del docente, los 8 pasos con sus botones y qué hacer si algo falla. Revisada a 320 y 360 px, en claro y oscuro. Se sube como recurso «Archivo» |
| `guia_docente.html` | docente (contiene la clave) | Análisis del ítem, matriz de 21 métodos con su evidencia verificada, secuencia de dos sesiones, criterios de comprensión (E1-E7), verificación diferida y plan B en el cuaderno |
| `verificar_cuestionario.py` | quien mantiene el material | Comprueba el XML (una sola clave, la clave no es la más larga, sin «$» ni letras) y que la tabla 10b de la guía coincida con él. Lo corre el paso f2 del nivel 0 de `mod_aulaviva`, que lo busca en esta carpeta: no moverlo |
| `verificacion_diferida.xml` | docente (contiene la clave) | Las 6 preguntas del cuestionario de verificación diferida (H4), en XML de Moodle. Las crea en un curso `Moodle/cuestionarios/crear_verificacion_lote_d49.php` del repo Proyectos-Varios |

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
- 2026-10-20: ya no se vota con los dedos en ningún modo. A las 17:43 se quitó del modo Moodle y en la
  noche también del modo sin Moodle (archivo suelto o la dirección directa de la actividad), porque el
  profesor lo seguía viendo. El paso 1 (`#p1vote`) dice «Espere en silencio a que todos guarden…», y lo
  mismo dicen la guía docente y la del estudiante. El arnés del nivel 0 del plugin exige que ningún texto
  de la página lo mencione, con o sin Moodle.
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
- Tercera reauditoría (APROBAR_CON_CAMBIOS: M1, M2, M3, N5 y H5 cerrados; N4 cerrado en Moodle). Sus
  hallazgos, corregidos:
  - **P1:** la regla de memoria y el ocultamiento empiezan con «Empezar la sesión 2», en la clase
    siguiente. Al terminar la sesión 1 no se oculta nada (antes desaparecían la confirmación y el
    resumen, y la regla se escribía con la explicación recién leída).
  - **P4:** el paso 7, cuyo rótulo es casi la regla, también se oculta mientras se escribe.
  - **P2:** el estado viejo se reconoce por `ok` (persiste), no por `checked` (se borra al cambiar un
    radio).
  - **P3:** la guía docente ya no dice que la seguridad y la justificación se pueden corregir.
  - **P5:** la nota del ocultamiento es neutra y no aparece en pasos bloqueados.
  Arnés 6 de 6 páginas (nueva: «viejo2»), con mutantes en rojo. Desplegado el 2026-10-20
  (`activity.html` sha256 `4c1d95642ee4ef72…`, sin deriva; backup `20261020_195541`).
- O1 resuelto el 2026-10-20: los dos `.Rmd` del subproyecto ya no preguntan «¿Cuál de los X recibe la
  porción que da a la carretera?». Ahora preguntan «¿Cuál es el turno del X cuya porción da a la
  carretera?» → «el primero en el orden del acta», es decir, por la posición, como C1. No nombran el
  acta porque la mayoría de sus contextos no la mencionan. La FASE 2C dio APROBAR_CON_CAMBIOS, sin
  hallazgos de corrección (`../cloze/REPORTE_DETRACTOR_2026-10-20_UBI02_TURNO.md`).
- 2026-10-21, resto del plan del primer detractor (decisiones del profesor: todo el lote; P-1 = variante
  B; aulas provisionales sin tablero, proyector ni impresora):
  - **Barras del curso:** `mod_aulaviva` 0.2.0 (`2026102001`). El docente toca «Mostrar al curso» y cada
    celular que ya guardó esa decisión ve las barras A-D sin nombres. Con P-1 = B se muestran la primera
    y la segunda decisión juntas al minuto 35, no al minuto 8 (Perez et al. 2010).
  - **H6:** al inicio solo se ve «8 franjas verticales»; los demás sugeridos aparecen al guardar un
    reparto armado con «Dibujar» (`own`, columna «Repartos propios»).
  - **H7:** cierre escrito de 8 min en el paso 4 (qué cambió y qué no → tres repartos dibujados en el
    celular → la regla con sus palabras), fijo al guardarlo; la palabra se pide después y el paso 5
    tiene 10 min.
  - **H8:** cada caso trae la respuesta de un estudiante (Julián, Valeria, Andrés, Sebastián) que se
    valida en dos niveles; dos son válidas y dos no; columna «Validó respuestas de otros» (E6).
  - **H9:** cada integrante propone un reparto y los tres lo guardan; «Parte del hijo 1» es la tercera
    columna (el arnés mide que termine antes de 294 px).
  - **H11:** el ítem propio lleva cuatro preguntas; «Mostrar mi ítem a la otra pareja» lo muestra a
    pantalla completa sin respuestas; la revisión del ítem ajeno queda escrita y fija (columna en el panel).
  - **H12:** la guía ya no se contradice sobre el modo sin plugin; sin plugin, dos «Consulta»; sin Moodle,
    papelitos.
  - **H4:** cuestionario «¿Alcanzan los datos? — verificación» (CBM diferido, 1 intento, contraseña),
    creado en Lab (cmid 476) para que el profesor revise las 6 preguntas antes de usarlo.
  - Nivel 0: 9 de 9 páginas; 13 mutantes nuevos, todos muertos. Desplegado el 2026-10-21
    (`activity.html` sha256 `b0c6ac78…`, plugin sin deriva; backup `20261021_070448`); navegador real
    18 de 18; cuestionario verificado por API (nota CBM esperada) y por HTTPS (contraseña y certeza).
  - Reauditoría del mismo día (detractor nuevo, 31 citas verificadas): RECHAZAR por H1 (en AD-1, «la cerca
    que rodea el terreno» admitía la lectura de la puerta; con CBM costaba −6). Corregidos los 9 hallazgos:
    - H1: AD-1 y el caso A3 preguntan por el perímetro o sin puertas.
    - H2: la revisión del ítem ajeno se ve en sesión 2 aunque el ítem de la pareja esté en el otro celular.
    - H3: Valeria y Sebastián tienen una razón errada compatible con «Es válida».
    - H4: en el cierre, el hijo 1 cambia de lugar.
    - H5: sin pregunta oral en los cierres; la sesión 2 cierra con un solo ítem.
    - H6: el XML entra a git (excepción en `.gitignore`).
    - H7: la sección 10b ya no promete la retroalimentación de cada opción.
    - H8: «Ocultar al curso» y pila de recuadros con desplazamiento.
    - H9: la secuencia sin plugin.
    16 mutantes, todos muertos. El cuestionario se rehízo (0 intentos) con `--reemplazar`.
  - Primera confirmación (detractor nuevo, 27 citas): RECHAZAR. El H1 crítico era el mismo canal: Valeria
    decía «350 m de cerca» sin «sin dejar puertas». Se barrieron todos los materiales: ya no queda ninguna
    cantidad de cerca sin esa condición, salvo la clave de AD-3, que es indeterminada con cualquier lectura.
    Corregidos los 7 hallazgos:
    - Valeria habla de «los cortes».
    - El libro de calificaciones recorta la nota CBM a [0, 6].
    - El docente aplica la regla al cerrar la sesión 2.
    - Los minutos de las Consultas.
    - `p7` se marca con el ítem o con la revisión.
    - El paso 5 y la sección 3 muestran al hijo 1 en lugares distintos.
    - `verificar_cuestionario.py`, en el paso f2 del nivel 0.
  - Segunda confirmación (detractor nuevo, 12 citas): **APROBAR_CON_CAMBIOS**, sin errores de corrección.
    Sus 3 hallazgos bajos quedaron aplicados en la guía y en este README:
    - el cierre en dos ramas;
    - corregir las preguntas en el XML, no en Moodle;
    - la fila del verificador.
  - Estado final: nivel 0 con 9 de 9 páginas; 17 mutantes del arnés y 3 del verificador, todos muertos.
    Desplegado (`activity.html` sha256 `afa9c68f…`, plugin sin deriva, backup `20261021_074853`), con el
    navegador real en verde. Cuestionario en Lab, cmid 476, con 0 intentos.
  - Pendiente:
    - el profesor revisa las 6 preguntas;
    - la aceptación en `lab` (nivel 7);
    - el nivel 6 de las barras (carga), fuera del horario de clase;
    - opcional, un `--comparar` entre el XML y el cuestionario de Moodle.
