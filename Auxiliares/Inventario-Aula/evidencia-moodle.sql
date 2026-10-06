-- Evidencia de aplicación en aula, medida en la BD de Moodle icfes. SOLO LECTURA.
--
-- Uso (desde este equipo; el servidor debe estar encendido):
--   ssh -o ConnectTimeout=10 alvaretto@100.97.113.70 'sudo mysql moodle' < evidencia-moodle.sql
--
-- Antes de correrlo, cambiar @ex (exname del .Rmd, sin extensión) y, para B–E,
-- @quiz (id del cuestionario dedicado al ejercicio, sacado de la consulta A).
-- Método y criterios: README.md de esta carpeta.
-- Probado contra el servidor el 2026-10-05 (3,5 s): reproduce 11b-mat 13/13.

SET @ex = 'desplazamiento_avion_aeropuerto_metacognitivo_interpretacion_n3_schoice_v1';

-- A) Cuestionarios donde se presentó el ejercicio: N (estudiantes con intento
--    terminado) y M (estudiantes matriculados activos en el curso).
SELECT c.shortname AS curso, z.id AS quiz, LEFT(z.name, 50) AS cuestionario,
       COUNT(DISTINCT za.userid) AS N,
       (SELECT COUNT(DISTINCT ue.userid)
          FROM mdl_user_enrolments ue
          JOIN mdl_enrol e ON e.id = ue.enrolid AND e.status = 0 AND e.courseid = c.id
          JOIN mdl_user u ON u.id = ue.userid AND u.deleted = 0 AND u.suspended = 0
          JOIN mdl_role_assignments ra2 ON ra2.userid = ue.userid AND ra2.contextid = cx.id
          JOIN mdl_role r2 ON r2.id = ra2.roleid AND r2.shortname = 'student'
         WHERE ue.status = 0 AND (ue.timeend = 0 OR ue.timeend > UNIX_TIMESTAMP())) AS M
  FROM mdl_question q
  JOIN mdl_question_attempts qa ON qa.questionid = q.id
  JOIN mdl_quiz_attempts za ON za.uniqueid = qa.questionusageid
                           AND za.state = 'finished' AND za.preview = 0
  JOIN mdl_quiz z ON z.id = za.quiz
  JOIN mdl_course c ON c.id = z.course
  JOIN mdl_context cx ON cx.contextlevel = 50 AND cx.instanceid = c.id
  JOIN mdl_role_assignments ra ON ra.userid = za.userid AND ra.contextid = cx.id
  JOIN mdl_role r ON r.id = ra.roleid AND r.shortname = 'student'
 WHERE SUBSTRING_INDEX(q.name, ' : ', -1) = BINARY @ex
 GROUP BY c.id, z.id
 ORDER BY c.shortname, z.id;

SET @quiz = 150;

-- B) Día del primer intento terminado de cada estudiante.
SELECT dia, COUNT(*) AS estudiantes FROM (
  SELECT userid, DATE(FROM_UNIXTIME(MIN(timefinish))) AS dia
    FROM mdl_quiz_attempts
   WHERE quiz = @quiz AND state = 'finished' AND preview = 0
   GROUP BY userid) t
 GROUP BY dia ORDER BY dia;

-- C) Estado final de la pregunta en cada intento terminado (todo debe ser graded*).
SELECT s.state, COUNT(DISTINCT za.userid) AS estudiantes
  FROM mdl_quiz_attempts za
  JOIN mdl_question_attempts qa ON qa.questionusageid = za.uniqueid
  JOIN mdl_question q ON q.id = qa.questionid
                     AND SUBSTRING_INDEX(q.name, ' : ', -1) = BINARY @ex
  JOIN mdl_question_attempt_steps s ON s.questionattemptid = qa.id
   AND s.sequencenumber = (SELECT MAX(s2.sequencenumber) FROM mdl_question_attempt_steps s2
                            WHERE s2.questionattemptid = qa.id)
 WHERE za.quiz = @quiz AND za.state = 'finished' AND za.preview = 0
 GROUP BY s.state;

-- D) Historial de matrículas del curso del cuestionario (altas y bajas por día).
SELECT DATE(FROM_UNIXTIME(l.timecreated)) AS dia, l.origin,
       REPLACE(l.eventname, '\\core\\event\\', '') AS evento,
       COUNT(DISTINCT l.relateduserid) AS usuarios
  FROM mdl_logstore_standard_log l
 WHERE l.courseid = (SELECT course FROM mdl_quiz WHERE id = @quiz)
   AND l.eventname IN ('\\core\\event\\user_enrolment_created',
                       '\\core\\event\\user_enrolment_deleted')
 GROUP BY dia, l.origin, evento ORDER BY dia;

-- E) Cuándo se importaron las preguntas (comparar con el último commit del .Rmd).
SELECT FROM_UNIXTIME(MIN(timecreated)) AS importada, COUNT(*) AS preguntas
  FROM mdl_question WHERE SUBSTRING_INDEX(name, ' : ', -1) = BINARY @ex;
