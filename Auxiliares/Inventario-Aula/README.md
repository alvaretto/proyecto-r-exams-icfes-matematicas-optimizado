# Inventario de aplicación en aula: cómo se llena

`aplicacion-en-aula.md` registra qué ejercicios validados se aplicaron en clase con **todos** los
estudiantes del grupo. Es el requisito de evidencia de Nivel 3 para promover un ejercicio a
`03-En-Produccion/`. La evidencia solo cuenta con el grupo completo, no con una muestra.

## Quién escribe qué

| Parte | La escribe | De dónde sale |
|---|---|---|
| Fecha de revisión y tabla **«Pendientes de aplicar»** | La rutina semanal (en la nube, no está en este repo) | Los `ejercicio_state.json` de `A-Produccion/` con los 11 pasos completos: el nombre sale del campo `ejercicio` y la fecha, de `aprobacion_usuario` |
| Tabla **«Ya aplicados en aula»** | El docente | La BD de Moodle icfes, con el método de abajo |

La rutina solo reescribe la fecha y la tabla de pendientes; este README y el `.sql` no los toca.
Si un `ejercicio_state.json` nombra un `.Rmd` que no existe, la rutina pide un ejercicio
fantasma (ver Error 43 en `.claude/docs/patrones-errores-conocidos.md`).

## Fuente de la evidencia

Moodle icfes (`https://icfes.tailebdb1f.ts.net/`). Acceso: `ssh alvaretto@100.97.113.70` y
`sudo mysql moodle`, **solo `SELECT`**. La documentación del servidor está en
`~/Proyectos-2026/Proyectos-Varios/Moodle/docs/00-RETOMAR.md`.

Las consultas están en [`evidencia-moodle.sql`](evidencia-moodle.sql) (A–E, probadas).

## Método

1. **Encontrar el ejercicio.** exams2moodle nombra cada pregunta `Rnnn Q1 : <exname>`.
   Compararlo exacto: `SUBSTRING_INDEX(name, ' : ', -1) = BINARY @ex`. Si no aparece, buscar
   variantes del nombre, pero un `_v2` con otra clave **es otro ejercicio**, no el mismo
   renombrado.
2. **Medir por intentos reales** (`question_attempts` → `quiz_attempts`), no por los slots del
   cuestionario: hay cuestionarios que sacan preguntas al azar de una categoría.
3. **Contar solo el cuestionario dedicado** al ejercicio («Actividad NN - …»). En los simulacros
   la pregunta sale al azar a unos sí y a otros no: eso no es aplicarlo al grupo.
4. **N y M** (consulta A). N = estudiantes del curso con un intento terminado; M = estudiantes
   matriculados activos. **¿TODOS? = «Sí» solo si N = M**; si no, «No (N/M)».
5. **Controles antes de escribir «Sí»:**
   - **Todos respondieron** (C): el estado final de la pregunta debe ser `gradedright`,
     `gradedwrong` o `gradedpartial`. Un `gaveup` o `todo` es una pregunta sin contestar.
   - **Nadie faltó por irse del grupo** (D): revisar las altas y bajas del curso. Las altas
     incluyen a la docente. Una baja por `cli` suele ser una cuenta de prueba; una baja anterior a
     la aplicación no cuenta.
   - **Se aplicó la versión aprobada** (E): las preguntas deben haberse importado después del
     último commit del `.Rmd` en `main` (`git log -1 --format=%ci -- <ruta>`). Si hay commits
     posteriores, revisar que no cambien nada que vea el estudiante.
6. **Escribir la fila**: fecha (el día con más intentos terminados), grupo (nombre corto del
   curso), «N de M», ¿TODOS? y, en Observaciones, el cuestionario con su id, los días de
   presentación (B) y los controles hechos.

## Trampas

- **MariaDB compara sin tildes**: `LIKE '%comité%'` también encuentra «comite». Para distinguir
  versiones por su texto, usar `LIKE BINARY`.
- **«Está en Moodle» no es «grupo completo»**: el 2026-10-05, de 5 ejercicios presentes en
  Moodle, solo 2 cumplían, y solo en 2 de 7 grupos.
- **No todos presentan el mismo día**: en «Barco - Avión» la mayoría lo hizo el día de la clase y
  el resto en las semanas siguientes. Queda escrito en Observaciones para que quien promueva el
  ejercicio decida si eso cuenta como «en clase».
- **El servidor es un portátil y a veces está apagado**: una consulta por `ssh` sin
  `-o ConnectTimeout=10` se queda colgada sin error. Antes de culpar a la consulta, ver
  `uptime -s` y `journalctl -b -1`.

## Historial

| Fecha | Qué se hizo |
|---|---|
| 2026-10-05 | Primera medición desde Moodle. Registrados `desplazamiento_avion_aeropuerto_…_n3_schoice_v1` y `coordenadas_vertices_plano_cartesiano_…_n2_schoice_v1` en 11b-mat (13/13) y p3c-mat (18/18). Corregidos los estados de `29-2025-2` y `Lentes-radio`; versionado el `_v1` aprobado de `distribucion-contagiados-v1` |
