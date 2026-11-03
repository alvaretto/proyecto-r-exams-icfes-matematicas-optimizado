## Ciclo 2026-11-02, tanda S2b (2026-11-03)

- **Origen.** La FASE 2C r6 (veredicto del detractor r6: APROBAR_CON_CAMBIOS, sin ninguna CORRECCIÓN;
  `salida/fase2c_r6_detractor.md`, 17 citas verificadas por el coordinador) dejó H1 (ALTA, DIAGNOSTICIDAD: el canal «resta»,
  sin medir) y H2 a H4 (BAJA). Esta tanda solo toca el verificador, el instrumento y el registro: el `.Rmd` sigue en
  `63e2713e6ac0bdbf250f4dc33901b2ec`, sin reapertura ni sellos (**6 de 11**).
- **Hecho.**
  - `verificar_espacio.R` pasa de `bdd0daf0…` a **`9c65474801b5b838183a0507233b8ea1`**. Añade «resta» y sus seis combinaciones
    con las pistas de «O-1 corregido»; el barrido del estímulo (28 operaciones entre celdas de la tabla, en dos lecturas); M44
    (ERR-RESTA siempre que se pueda) y M19, también sobre el barrido; `ESTIMULO_SC` corregida; `juzgar_s1()` simétrico; M42a-c
    y M43, aditivos; y M28, que juzga solo los pendientes nuevos del proceso sin normalizar.
  - Resultado (`salida/ciclo_20261102/s2b/verificar_espacio_s2b.txt`, 129 s): **exit 3, 36 pendientes y 0 fallos**. Los
    pendientes son los 27 de H1 y los 9 del barrido. Las 37 cifras cubiertas salen idénticas, y ninguna cifra firmada se mueve.
    Frente a la S2a, de sus 297 líneas solo cambian 9: los siete veredictos de M38, M39 y M41, que ahora salen
    «cazado_por_su_sonda_y_control», el resultado y la línea de cierre (`s2b/diff_verificador_s2a_a_s2b.txt`).
  - `cap_s1b2.py` pasa de `1d0117ed…` a `aaa1b49e…` (H4): uso `python3 -E -P`, y un assert de que el directorio de trabajo
    está fuera del repo. `python3 -E -P -c "import websockets"` funciona.
  - H3, opción (b): la tabla candidata de L-6, corregida, en el `.claude/CLAUDE.md` local. La opción (a), ampliar la guarda
    del `.Rmd`, iría con un rediseño por H1.
- **Cifras clave.** Las tablas completas, regla por regla, con la canónica exacta y su peso, están en el `.claude/CLAUDE.md`
  local, § «Ciclo 2026-11-02 (SCHOICE), tanda S2b».
  - «resta» sola: 54,2 / 58,8 / 67,8 % (mezcla de E1 / CURSOS / PROMEDIO); en la canónica, 0,0 % exacto. Aplica en el 51,
    el 45 y el 57 % de las versiones, y en las 144 no canónicas en que aplica, la opción que cita la resta es ERR-RESTA, del
    lado de G, en las 144.
  - «S_U + S1 + suma + atípica + resta»: 93,0 / 94,5 / 98,0 %; en la canónica, 100,0 %. Lo firmado sin «resta» era 81,5 % en
    CURSOS y 74,5 % en PROMEDIO.
  - Barrido: pasan de +8 pp 9 cifras, en 8 reglas. Un solo total (el producto de la fila 1) da +9,0 pp en la mezcla; las de dos
    pasos (ventajas parciales), +9,0 y +12,0 pp en una rama y antipredictivas en la otra; y en la canónica las cifras del
    impreso dan coincidencias de hasta +75,0 pp.
- **Alcance.** Por la invariancia registrada en S1, S1b y S2a, el canal está también en la versión aprobada para el aula
  (`e293d5c8`) y en la Parte 1 del CLOZE (F-2), que no se tocó. Supera la premisa de F-3: con el 93,0 %, la Parte 1 daría
  +25,5 pp de la nota de la pregunta sobre el azar, frente a los +19,7 pp con que se firmó.
- **Opciones del profesor, sin elegir.** (a) Firmar regla por regla, como override estructural, igual que O-1. (b) Rediseñar;
  por ejemplo, que la resta de los promedios salga también del lado de P, como en la C del impreso. Exige antes la enumeración
  de §P7-F y una pasada más allá de §P7-D, que solo autoriza el profesor. (c) Otra. Detalle de cada una en el `.claude/CLAUDE.md`
  local.
- **Próximo paso.**
  1. La decisión del profesor sobre H1 y el barrido, y la firma de la tabla candidata de L-6 corregida.
  2. Con (a): cablear la firma en el verificador (exit 4) y decidir si el paso 7 se sella con la r6 o con un detractor nuevo.
     Con (b): reapertura, pasos 3 a 6, 6b y FASE 2C r7 con un detractor nuevo y sin `name:`.
  3. Después, los pasos 8 a 10 y el WAIT_USER #3.
- **Restos fuera del repo:** `/tmp/x.json` y `/tmp/perfil_inexistente_s2b/`, de la prueba del assert de `cap_s1b2.py`. El
  permiso no dejó borrarlos.
