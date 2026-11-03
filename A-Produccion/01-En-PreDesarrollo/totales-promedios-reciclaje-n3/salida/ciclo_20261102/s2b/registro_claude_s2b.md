### Ciclo 2026-11-02 (SCHOICE), tanda S2b

> La hizo el `orquestador-schoice` el 2026-11-03, tras la FASE 2C r6 (veredicto del detractor r6: APROBAR_CON_CAMBIOS, sin
> ninguna CORRECCIÓN; reporte íntegro en `salida/fase2c_r6_detractor.md`, con las 17 citas verificadas por el coordinador).
> Solo cambian el verificador, el instrumento y el registro. El `.Rmd` sigue en `63e2713e6ac0bdbf250f4dc33901b2ec`, sin
> reapertura y sin sellos (6/11). `verificar_espacio.R` pasa de `bdd0daf0…` a **`9c65474801b5b838183a0507233b8ea1`**, y
> `cap_s1b2.py`, de `1d0117ed…` a `aaa1b49e8b3134cb4ed24bed5a263e83`. Evidencia: `salida/ciclo_20261102/s2b/`.

**Cómo se atendió cada hallazgo de la r6**

| Hallazgo | Qué se hizo | Resultado |
|---|---|---|
| H1 (ALTA, DIAGNOSTICIDAD): el canal «resta», sin medir | E4 mide «resta» sola y sus seis combinaciones «… + resta» con las pistas firmadas de «O-1 corregido», con el criterio de R_dif (primera cifra de cada opción y `sc()`), en la mezcla de E1, en las ramas CURSOS y PROMEDIO (N = 100 cada una) y en la canónica por enumeración exacta (24 órdenes). Ninguna está firmada, así que la que pasa de +8 pp queda pendiente. Además: el barrido del estímulo, M44 y `ESTIMULO_SC` corregida, que ahora dice qué canales del estímulo mide E4 y cuáles no | 27 de las 28 cifras de H1, pendientes. Reproducen las del detractor y las del coordinador. M44 (ERR-RESTA siempre que se pueda): su cuota en el distractor de G pasa de 45 a 63 % en CURSOS y de 57 a 60 % en PROMEDIO, y «resta» sube de 58,8 a 72,2 % y de 67,8 a 70,0 %: cazado por su sonda |
| H2 (BAJA): los mutantes de sonda de la S2a no aíslan su sonda | `juzgar_s1()` simétrico: un mutante de sonda que además hace fallar un control sale «cazado_por_su_sonda_y_control». M38, M39a-c y M41a-c lo declaran en `ESPERA_CONTROL`. M42a, M42b y M42c (sonda S1-Sol signo) y M43 (sonda S1-Sol fórmula), aditivos: la frase «Comprobación: $D$» antes de «Propiedades del concepto», seguida de U+2009, U+202F o U+2060 y un punto, o de U+2060 y «kilos». Los separadores y los saltos de línea, con `intToUtf8()` | M42a-c: S1-Sol signo 100, las otras dos sondas 0, los controles en 1158 / 730 / 42 y la clase anterior en 0. M43: S1-Sol fórmula 100, y lo demás igual. Los siete mutantes de la S2a salen ahora «cazado_por_su_sonda_y_control», que es lo que declaran |
| H3 (BAJA): la guarda de V1 sigue siendo una lista negra, y la tabla candidata no lo dice | Opción (b): la tabla candidata, corregida, más abajo. La (a), ampliar la guarda de las líneas 594-595 del `.Rmd`, no se aplica ahora: si el profesor decide rediseñar por H1, va en ese mismo cambio | — |
| H4 (BAJA): `cap_s1b2.py` documenta `python3 -I`, que aquí no puede importar `websockets` | La línea de uso pasa a `python3 -E -P cap_s1b2.py …`, con su porqué en la cabecera, y un assert, antes de importar `websockets`, comprueba que el directorio de trabajo está fuera del repo | `python3 -E -P -c "import websockets"` importa websockets 15.0.1 del site de usuario (exit 0; con `-I`, exit 1). El assert corta dentro del repo y deja pasar fuera |

**Verificador** (`s2b/verificar_espacio_s2b.txt`, 129 s): **exit 3, PENDIENTE DE FIRMA (36; 0 fallos)**. Los 36 pendientes son
los 27 de H1 y los 9 del barrido. Las 37 cifras cubiertas (13 de «O-1 corregido» y 24 de la decisión 2 a) salen idénticas, y
M28 sigue cazado por su sonda con su único pendiente nuevo. Frente a la salida de la S2a, de sus 297 líneas solo cambian 9: los
siete veredictos de M38, M39 y M41, el resultado y la línea de cierre (`s2b/diff_verificador_s2a_a_s2b.txt`). Ninguna cifra
firmada se mueve. M19 mueve también el barrido: con ERR-TOTAL siempre que se pueda, el producto de una fila sube en las dos
ramas («n1 * a1 (i)»: de 28,0 a 40,0 % en CURSOS y de 32,5 a 46,8 % en PROMEDIO; «n2 * a2 (i)»: de 28,7 a 42,2 % y de 30,2 a
37,8 %). Las tablas de la firma las saca `s2b/tabla_firma_s2b.py` de esa salida, sin transcribir ninguna cifra.

**El barrido del estímulo** (regla #22 §P7, cierre por familias; §P7-G).
- **Qué mide.** Para cada operación de un paso entre dos celdas de la tabla (suma, resta, producto y cociente cuando es
  entero), dos lecturas. En la (i), la sede de la opción que cita el resultado gana, y se elige la otra opción de esa sede. En
  la (ii), la opción que cita el resultado es falsa y se descarta. Si ninguna opción lo cita, azar. Se añaden, declaradas, las
  cuatro operaciones de dos pasos que usa el ítem: las ventajas parciales de `calculos_tabla()`.
- **Por fila de la tabla** (fila 1 y fila 2), no por G y P: el estudiante no sabe qué fila gana, y rotular las celdas por G y
  por P presupone la respuesta. En los dos pares de la misma columna las dos rotulaciones dan lo mismo.
- **Ninguna regla de un paso equivale a resolver el ítem.** Los dos productos de fila juntos son T_G y T_P, y compararlos es
  resolverlo. Cada uno por separado es «un solo total», y se mide (BE2; su gemela, «n2 * a2 (i)», da +7,5 / +3,7 / +5,2 pp).
- **Resultado:** 28 operaciones × 2 lecturas × 4 estratos, y pasan de +8 pp 9 cifras, en 8 reglas. «|a1 - a2| (i)» es
  «resta» (RS1): el verificador comprueba que da las mismas cifras y la juzga una sola vez.
- **Ninguna la cubre una sonda firmada.** Todas señalan una opción que ya marca una pista firmada, pero con otra lectura:
  - BE1, en la canónica, descarta la C, que ya descarta S_U (firmada en la canónica a +25,0 pp).
  - BE2 señala la opción que cita un total, que es ERR-TOTAL. Esa opción ya la descarta «atípica» en las combinaciones
    firmadas con S_U; leerla como «esa sede gana» es nuevo.
  - BE3 a BE6, en la canónica: con las cifras del impreso, la resta de 45 y 5 y el producto de 1 por 40 dan 40, la cifra de
    la A (ERR-DATO: el promedio de G), que ya descarta S1 (firmada en la canónica a +25,0 pp).
  - BE5 y BE7, en CURSOS, y BE8, en PROMEDIO: cuando la fila es la de G, la operación da su ventaja parcial (ERR-PARCIAL), que
    ya descarta «suma» en las combinaciones firmadas.
- **Lectura de las de dos pasos (BE5, BE7 y BE8).** En la otra rama, la misma operación da la ventaja de P (la que cita
  padv), y la lectura (i) se vuelve antipredictiva: de −10,8 a −14,2 pp. El estudiante no ve la rama sin calcular, y en la
  mezcla de E1 quedan entre −5,2 y +4,0 pp. Es la situación de S3 a S6, que el verificador juzga en la mezcla ponderada. Aquí
  se juzgan, como toda regla nueva, en los cuatro estratos, y quedan pendientes: juzgarlas solo en la mezcla sería relajar la
  guarda, y eso lo decide el profesor (H-5).
- **No se midió:** las combinaciones de las reglas del barrido con S_U y las pistas firmadas, y otras operaciones de dos o más
  pasos.

**Tabla de cifras para la firma** (§P7-E, regla por regla y por estrato; aclaración iii: cifras del código). Acierto, y entre
paréntesis su exceso sobre el 25 % del azar, en pp. Peso de la canónica en la mezcla: 0,10 × su exceso (L-5). Pesos de diseño
de los estratos (W del verificador): CURSOS 0,40, PROMEDIO 0,50 y canónica 0,10. Son reglas discretas (igualdad exacta), sin
margen en el sentido de §P7-B, como las de O-1.

@@TABLAS_FIRMA@@

**Alcance.**
- Por invariancia, estas cifras son también las de la versión aprobada para el aula (`e293d5c8`) y las de la Parte 1 del CLOZE
  (F-2), en `cloze/`, que no se tocó ni se midió aparte. La invariancia está registrada en S1, S1b y S2a, en las semillas 1 a
  1000: las opciones (salvo el espacio duro), la clave, la rama y la tabla no han cambiado desde `e293d5c8`.
- **F-3.** Los pesos 3|1|1|1|1|1 del CLOZE se firmaron con el atajo del 77,6 %, que daba a la Parte 1 +19,7 pp de la nota de la
  pregunta sobre el azar. Por la misma cuenta, con RS7 en la mezcla de E1 (93,0 %): (93,0 − 25) × 3/8 = +25,5 pp. Supera la
  premisa de F-3, así que también vuelve al profesor.
- Es un canal nuevo por encima de +8 pp, y por «O-1 corregido» § Alcance vuelve al profesor. No lo introdujo la S2a. No es
  CORRECCIÓN: la clave sigue siendo la única afirmación verdadera.

**Opciones del profesor, sin elegir**
- **(a) Firmar regla por regla, como override estructural, igual que O-1:** las 36 cifras de las tablas de arriba (27 de H1 y 9
  del barrido), con su peso en la mezcla. La tanda que lo cablee añade la firma al verificador: dentro de lo firmado, «ROJO,
  cubierto» (exit 4); por encima, pendiente. M44 tendrá que sacar «resta» de «cubierto»: hoy la sube en las dos ramas. Por
  F-2, la firma alcanza la Parte 1 del CLOZE, y F-3 se revisa con la cifra nueva.
- **(b) Rediseñar.** Por ejemplo, que la resta de los promedios salga también del lado de P, como en la C del impreso, para que
  citarla deje de nombrar a la sede que gana. Toca el `.Rmd`: reapertura desde `generacion_rmd`, pasos 3 a 6, 6b y FASE 2C r7
  con un detractor nuevo. Antes exige la enumeración del espacio de diseño de §P7-F (en la rama PROMEDIO, P no tiene el mayor
  promedio, así que la frase de la C no le sirve tal cual) y una pasada más allá de §P7-D, que solo autoriza el profesor. La
  ampliación de la guarda de V1 (H3, opción a) iría en ese mismo cambio, y el barrido de esta tanda sirve de vara para el
  rediseño.
- **(c) Otra.**

Con cualquiera de las tres, decide también el profesor si el paso 7 se sella con la r6 o con un detractor nuevo. La S2b cambió
el verificador y el instrumento, no el `.Rmd`. El precedente, la decisión 4 de la tanda 9a del CLOZE, se tomó caso por caso.

**Tabla candidata de L-6, corregida (H3 de la r6, opción b), pendiente de la firma del profesor.** Sustituye a la candidata de la
tanda S2a, que sigue escrita más arriba. La tabla firmada de L-6 no se toca.

| Exige | Guarda | Mutante |
|---|---|---|
| La unidad que cierra una fórmula («kilos», «curso(s)») va dentro de ella (V1; tanda S1b, decisión «Arreglarlo ahora») | `stopifnot` «L-6: la unidad que cierra una fórmula va dentro de la fórmula» (líneas 594-595 de `63e2713e`; feedback y análisis de los distractores). **Solo ve el espacio normal y U+00A0** entre el cierre de la fórmula y la unidad. El resto de los separadores (U+2009, U+202F, U+2060…) lo cubre solo la sonda S1-Sol fórmula de `verificar_espacio.R`, sobre opciones, Solution y feedback emitidos: entre el cierre y la unidad admite hasta 3 caracteres que no sean letra ASCII, cifra, `$` ni salto de línea. Control exacto: 1158 unidades dentro de la fórmula en E1. La guarda tampoco ve la Solution, que se arma en su propio chunk | Guarda: M30 (el feedback de la clave cierra la fórmula antes de «kilos», con el espacio duro). Sonda: M31 (la Solution, en «kilos a favor de», con el espacio duro); M39a, M39b y M39c (la lista de totales, con U+2009, U+202F o U+2060; también hacen fallar los controles: «cazado_por_su_sonda_y_control»); y M43 (aditivo: los controles no se mueven y la clase anterior da 0). Control: M34 (quita un sitio: 1158 → 1151) |
| El signo que sigue a una fórmula, con unidad o sin ella (`. , : ; ! ? ) ] » ”` y `…`), va dentro de ella (V1′, tanda S1b relanzada, decisión «V1′ (Recomendada)»; generalizada en la tanda S2a, decisión «Cerrarlo ahora (Recomendada)») | `stopifnot` «L-6: el signo que sigue a una fórmula, con unidad o sin ella, va dentro de la fórmula» (líneas 601-602; feedback y análisis de los distractores). Entre el cierre de la fórmula y el signo admite **hasta 3 caracteres que no sean letra ASCII, cifra, `$` ni salto de línea**: las letras con tilde y la ñ cuentan como separadores, y con 4 o más caracteres ya no se ve. La sonda S1-Sol signo usa la misma clase sobre opciones, Solution y feedback emitidos. Controles exactos: 730 unidades con el signo dentro y 42 signos dentro de una fórmula sin unidad, uno por versión de E1 con ERR-RESTA | Guarda: M33 y M37. Sonda: M32, M38, M41a, M41b y M41c (también hacen fallar un control: M38 y M41a-c salen «cazado_por_su_sonda_y_control»; M32 no pasa por `juzgar_s1()` y su comprobación no mira los controles); y M42a, M42b y M42c (aditivos: los controles no se mueven y la clase anterior da 0). Controles: M35 (730 → 721) y M36 (42 → 0) |

Queda declarado, sin guarda: la tabla del enunciado conserva el espacio normal en sus celdas de cifras (L-6 firmada). Por F-2,
las dos filas rigen también la Parte 1 del CLOZE tras el re-empalme.

**¿Ley local o regla?**
- **Local, candidatas** (endurecen; las confirma el profesor):
  - **L-7 · Barrido del estímulo.** E4 mide «resta», sus combinaciones con las pistas firmadas y el barrido de las operaciones de
    un paso entre dos celdas (más las cuatro de dos pasos), en las dos lecturas, por estrato y en la canónica exacta. Toda cifra
    por encima de +8 pp sin firma queda pendiente (exit 3). Guardas: `juzga_s2b()` y el cotejo de «|a1 - a2| (i)» con «resta».
    Mutantes: M44 y M19, también sobre el barrido.
  - La tabla candidata de L-6, corregida (arriba).
- **Del repo, candidatas.** Tocan `.claude/` (regla #17), así que las decide el profesor:
  1. §P7-G: una regla del estímulo que solo mira si una cifra de las opciones **está** en la tabla (S1) no cubre las
     **operaciones** con la tabla. La declaración del estímulo de este ítem pasó seis FASE 2C con un canal de +29 a +43 pp sin
     sonda. La familia del estímulo se cerraría con el barrido de las operaciones de un paso entre celdas, en las dos lecturas.
     Es general (todo ítem con una tabla numérica y cifras en las opciones), grave e invisible a los gates. Amplía §P7-G; no crea
     una regla nueva.
  2. Una declaración de cobertura (`estimulo_no_aplicable`, `familias_no_aplicables`) dice qué mide y qué no. La genérica («se
     mide aparte en E4») escondió el canal.
  3. `INC-MUTANTE-SONDA`: un mutante de sonda que además hace fallar un control prueba que la sonda es sensible, no que su clase
     ampliada sea necesaria. La necesidad la prueba un mutante aditivo, con los controles quietos. Es la primera vez, así que no
     cumple todavía el criterio de repetición.

**Restos fuera del repo.** La prueba del assert de `cap_s1b2.py` desde fuera del repo dejó `/tmp/x.json` y el perfil de
Chromium `/tmp/perfil_inexistente_s2b/`, sin ningún proceso vivo. El permiso no dejó borrarlos.
