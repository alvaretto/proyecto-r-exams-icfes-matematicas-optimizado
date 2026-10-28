# HANDOFF — CLOZE `probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1`

> Frase disparadora: «Continúa con el CLOZE de probabilidad-fichas-tabla-doble-entrada-n2».
> Última actualización: 2026-10-28. Si este archivo y `ejercicio_state.json` discrepan, manda el JSON.
> El SCHOICE hermano está en `..`; sus invariantes y **todas las firmas**, también las del CLOZE, en
> [`../.claude/CLAUDE.md`](../.claude/CLAUDE.md). El ítem verbatim, en [`../ITEM_OFICIAL.md`](../ITEM_OFICIAL.md).

## Estado: 11/11, aprobado para el aula por el profesor el 2026-10-28

No está en producción: la promoción espera la evidencia de aula, como la del SCHOICE.
`.Rmd` final: md5 `310c314673c5d188d1385f2148b3b1b0`.

## Qué es

Versión CLOZE de la pregunta impresa 32 del simulacro SAI «Nivelación 6» de grado 10 (MAT-2026-07-032,
D2.3, Nivel 2, DOK 2). Tiene 6 partes, `schoice|num|num|num|schoice|mchoice`:

| Parte | Tipo | Qué pide | Peso |
|---|---|---|---|
| 1 | schoice | La pregunta del ítem. Por código es el SCHOICE de `..`: misma generación, mismo RNG (E1) | 3 |
| 2 | num | Casos posibles (N) | 1 |
| 3 | num | Casos favorables de un nivel de la característica sorteada | 1 |
| 4 | num | Casos favorables del otro nivel | 1 |
| 5 | schoice | Cuál de los dos niveles es más probable en cada extracción | 1 |
| 6 | mchoice | Afirmaciones sobre la misma bolsa y la misma característica | 1 |

Las Partes 2 a 6 usan una sola característica por versión, sorteada sin depender de los roles, y no
imprimen cifras.

El motivo es que la clave de la Parte 1 equivale a que coincidan los totales de dos características
distintas (en la canónica, blancas = círculos = 46). Una parte que pidiera esos dos totales delataría la
clave (V11).

## Uso en el aula

- **Moodle**, desde `cloze/salida`: `Rscript --no-init-file ../exportar_moodle_cloze.R [n]`.
  - El valor de `n` va de 1 a 100; por defecto es 30.
  - Los gaps de opción única salen como botones verticales (`MULTICHOICE_V`), que se leen a 360 px.
  - El script comprueba los pesos, que no haya imágenes en los gaps y que los nombres de las figuras
    sean neutrales.
  - Los `Semillero*.R` de esta carpeta hacen lo mismo, con 100 versiones como máximo.
- **La pregunta vale 8 puntos en Moodle, no 6** (detractor r4, H2), por los pesos `3|1|1|1|1|1`.
  - Dentro de la pregunta, la Parte 1 vale 3/8 y cada una de las otras 1/8.
  - Si el cuestionario mezcla esta pregunta con otras, fija en él la puntuación máxima de su espacio:
    Moodle conserva la proporción.
- **DOCX: no apto para estudiantes**, porque deja los `##ANSWERi##` a la vista (exams2pandoc 2.4.2).
  **Para papel, usar el SCHOICE.** NOPS es N/A para cualquier cloze.
- **Pendiente:** revisar la tabla en un celular real. A 360 px sus cifras miden unos 9-10 px (auditoría
  visual 6b).

## Decisiones del profesor (2026-10-28)

Registradas como firmas en `../.claude/CLAUDE.md`.

1. **Flujo B por herencia.** Se reutiliza la tabla Python del SCHOICE sin regenerar el graficador. La
   firma del 96,8 % cubre el CLOZE.
2. **K-1.** Se mantiene el pool efectivo heredado en la Parte 1: PROB-SIM, PROB-UNA y PROB-CER.
3. **H3 de r1, residuo estructural.**
   - La Parte 6 se resuelve sin la tabla, porque el veredicto de cada plantilla no cambia entre
     versiones.
   - La Parte 5 presupone n1 ≠ n2.
4. **Pesos `expoints: 3|1|1|1|1|1`** (I-9). Sus guardas:
   - E7 del verificador;
   - la guarda de pesos del exportador.
5. **Aprobado para el aula**, condicionado a la revalidación del cambio de pesos. Se cumplió con la
   FASE 2C r4.

## Evidencia (`salida/`)

| Qué | Archivo | Resultado |
|---|---|---|
| Detractores FASE 2C | `detractor_fase2c_r1.md` … `_r4.md` | Las cuatro pasadas dieron APROBAR_CON_CAMBIOS. Ninguna encontró defectos de CORRECCIÓN. Las citas de r2, r3 y r4 se verificaron con `verificar_citas_detractor.py` |
| Verificador propio | `verificar_espacio_cloze_r4b.txt` | APROBADO, 0 fallos. Imprime el md5 de la versión verificada. E1: P1 ≡ SCHOICE. E2: una sola clave por gap. E3: V11. E4: residuos. E5: 300/300 únicas. E6: 23 mutantes. E7: pesos, con 4 mutantes |
| Render | `render_r4.log` | HTML, PDF, DOCX y Moodle OK; NOPS N/A |
| Moodle del aula | `exportar_r4b.log`, `moodle_aula/` | 30/30 preguntas con pesos `3|1|1|1|1|1` y defaultgrade 8; 0 desplegables; 0 imágenes en gaps |
| Mutantes del exportador | `mutantes_exportador_r4.txt` | M1-M5 mueren. Con los patrones anteriores, M2 y M4 sobrevivían |
| Arsenal (hook v6.3) | `~/.cache/post-exams2-validation/probabilidad_…_cloze_v1/20261028-160113-595160/informe.txt` | 0 errores, 7 advertencias (6 heurísticas de 2E/2F y la 2B sin PDF); 2G 100/100; 2H PASA |
| Stress test | `stress_test_n100.txt` | N = 100, 0 anomalías |
| Posición de la clave de la Parte 5 | — | Uniforme por construcción, porque R/exams permuta cada gap. Medida en 47/100 (r4) y 141/300 (p = 0,33): no hay sesgo |

## Residuos declarados (no bloquean; §P7-D agotado, 3 de 3 pasadas)

- **Firmados del SCHOICE, medidos sobre la Parte 1 del CLOZE** (E4):
  - en «repite» y «compuesta», la misma tasa que en el SCHOICE;
  - en «celdas más juntas» y sus dos encadenamientos, entre 1 y 3 pp por encima de su firma. La depuración
    de los detractores r2 y r3 lo atribuye a ruido de muestreo.
- **H3 de r1:** la Parte 6 se resuelve sin la tabla; firmado.
- **Parte 5:** 22/100 valores únicos (gap binario).
- **Lectura condicional con |n1 − n2| = 1:** ≈ 4,6 %. La lectura firmada es la marginal, «en cada
  extracción» (I-7).
- **Exclusión de la característica que empata:** 5-6 % de las versiones.
- **Regla cruzada «celdas más juntas»:** +6,7 pp en depuración (N = 4000).
- **La vara §P7** está medida sobre ítems de opción múltiple simple, no sobre CLOZE.

## Cómo volver a verificar (siempre desde `cloze/salida`)

El `data_generation` dibuja `tabla_<id>.png` en el directorio actual, así que nunca hay que correrlo desde
`..`.

```bash
cd cloze/salida
Rscript --no-init-file ../verificar_espacio_cloze.R > verificar_espacio_cloze.txt   # E1-E7
Rscript --no-init-file ../render_cloze.R                                            # 4 formatos
Rscript --no-init-file ../exportar_moodle_cloze.R 30                                # Moodle del aula
```

Con el hook v6.3 (2026-10-28), un `exams2*("<.Rmd>")` dispara el arsenal solo:
- el ejercicio se deduce del `.Rmd`;
- el informe y sus artefactos van a `~/.cache/post-exams2-validation/`;
- no sella nada.

El paso 6 se sella a mano con la nota que sugiere el informe.

**Trampas:**
- Nada de comillas invertidas dentro de los chunks, ni siquiera en comentarios: la 2D da un falso
  positivo.
- En este shell, `cp` es `cp -i`: usar `/bin/cp -f`.

## Pendiente

- **Evidencia de aula (Nivel 3)** antes de promover. Destino previsto: el del SCHOICE, en
  `03-En-Produccion/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/07-Probabilidad_Principios-Aditivo-Multiplicativo-Conteo/`.
- **Revisar la tabla en un celular real.**
- **Versionar `cloze/`** cuando lo pida el profesor. Hoy está sin versionar entero. Van:
  - el `.Rmd`, el estado, este HANDOFF y los scripts;
  - las plantillas y los Semilleros.

  No va `salida/`.
