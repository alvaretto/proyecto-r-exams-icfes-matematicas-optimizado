# HANDOFF — serie-seno-aproximacion-n3 (MAT-2026-07-017, SCHOICE)

**Fecha:** 2026-11-13. **Estado: 11/11, aprobado y firmado por el profesor el 2026-11-13.** md5 firmado del `.Rmd`:
`b9d489a9dd838630875a4958146b9bc8`. 11/11 no quiere decir terminado: la promoción depende de la evidencia de aula.

**En el remoto desde el 2026-11-13** (rama `feat/plantillas-oficiales-rexams`): `7cd5eef0` trae este ejercicio y `b90044ff` la
regla #27 y el umbral de 100 únicas sobre 150, que salieron de este ciclo. Suite completa antes del push: 54 suites de 54.

**Segundo Knit, 2026-11-13 a las 12:01 (sin resolver).** Otro Knit de RStudio reordenó dos líneas del YAML (`html_document` y
`pdf_document`). El `.Rmd` en disco tiene md5 `35090792a857343326a9153a4411bb32`, y el cuerpo, sin las 6 líneas del YAML, es
idéntico al de `7cd5eef0`. `salida/verificar_bateria_p7.R` da exit 4 (`FIRMA_NO_CUBRE`) sobre ese archivo. La versión firmada
está en el commit: `git checkout 7cd5eef0 -- <el .Rmd>` la restaura. Falta la decisión del profesor: restaurarla, o atar la
firma al cuerpo sin el YAML para que un Knit no la anule.

## Artefactos

- **`.Rmd`:** `serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd`, md5 `b9d489a9…`.
  - **YAML del Knit de RStudio.** Empieza con un bloque `output:` que antepuso el Knit de RStudio el 2026-11-13. El profesor decidió
    dejarlo («No, dejarlas»), y el md5 firmado lo incluye. R/exams lo ignora: el HTML no lo muestra y los cuatro formatos compilan.
  - **Salidas del Knit.** El `.html`, el `.pdf` y el `.docx` de esta carpeta son del Knit. No son salidas de R/exams y no van al
    commit.
- **`ITEM_OFICIAL.md`:** transcripción, clasificación, triangulación de la regla #27, «Regla de uso», decisiones literales del profesor,
  firma de los atajos y residuo declarado.
- **FASE 2C:**

  | Ronda | md5 auditado | Veredicto | Archivo |
  |---|---|---|---|
  | 1 | `e227bb84…` | RECHAZAR | `salida/fase2c_r1_detractor.md` |
  | 2 | `faaed530…` | RECHAZAR | `salida/fase2c_r2_detractor.md` |
  | 3 | `c6468f30…` | APROBAR_CON_CAMBIOS | `salida/fase2c_r3_detractor.md` |
  | 4 | `b310061e…` | APROBAR_CON_CAMBIOS | `salida/fase2c_r4_detractor.md` |

  Las citas de las cuatro rondas se verificaron.
- **Verificador §P7:** `salida/verificar_bateria_p7.R`, que lee `reglas_p7.R` (44 reglas congeladas) y `enumeracion_exacta.R` (88
  configuraciones exactas).
  - **Firma.** Está registrada con fecha, textos y md5. Las 21 reglas del alcance están en su tope. Hay además 3 reglas firmadas
    fuera de la batería, con su fuente.
  - **Salidas:**

    | Situación | Salida | Exit |
    |---|---|---|
    | Estado real | ROJO_CUBIERTO | 3 |
    | Hay reglas fuera del alcance o por encima de su tope | ROJO_SIN_CUBRIR | 1 |
    | El md5 no es el firmado | FIRMA_NO_CUBRE | 4 |
    | Ninguna regla pasa de +8 pp | PASS | 0 |

  - **Mutantes:** `MUTANTE_QUITAR=<regla>`, `MUTANTE_TOPE=<regla>` y `VERIFICAR_RMD=<copia>`. En zsh, cada variable se escribe por
    separado.
- **Mutantes de texto:** M1 a M7, en `salida/verificar_mutantes_textos.R`.
- **Medición visual pareada:** `salida/p6b_desborde_360_320.txt` y `salida/p6b_desborde_moodle_270_310.txt`.
- **Tope de valor:** `salida/enumeracion_tope_r4.txt`.

## Decisiones del profesor (2026-11-13, literales)

| Pregunta | Respuesta literal |
|---|---|
| ¿El ejercicio lleva gráficas? | «No, sin gráficas» |
| Atajo de la calculadora | «No aceptarlo» · «Cambiar el 1 por 2 o 3» |
| Forma de las fracciones | «Otra opción sin simplificar» |
| Versiones de dos términos | «Quitarlas» |
| Umbral de versiones únicas | al menos 100 distintas de 150 renders |
| Pendientes menores | «Resuelve los pendientes menores. Commit. Push» |
| YAML del Knit | «No, dejarlas» |
| Firma, tabla anterior a la pasada 4 | «Firmar los atajos de la tabla y aprobar» |
| Firma, seis cifras nuevas o cambiadas | «Firmar también estos seis y aprobar» |

## Firma (§P7-E)

La tabla completa, con cada atajo en lenguaje llano, su regla, la cifra firmada y su origen, está en `ITEM_OFICIAL.md`, § «Firma de
los atajos». Son 21 reglas de la batería y 3 del detractor de la ronda 4.

## Residuo declarado

- **Clave en un extremo.** Con x = ±3 y tres términos, la clave es la opción extrema en el 100 % de esa rama; con cuatro términos, en
  el 50 %. El rango agregado queda en 27,1 / 22,9 / 22,9 / 27,1 %.
- **Ecuaciones con desplazamiento lateral.** En Moodle a 320 px (contenido de 270 px), 16 ecuaciones de la Solution en 14 de 100
  versiones se desplazan hasta 13 px dentro de su caja. La página no desborda.
- **8 claves distintas** (`WARN_DIV_BAJA`), consecuencia de D1 y D3.

## Siguientes pasos

1. **Evidencia de aula.** Aplicar el ejercicio y recoger los resultados: el aula es la que decide la promoción.
2. **FASE 2C-bis** (`/detractor-hetero`), recomendada antes de promover.
3. Promover con `/promover-ejercicio` solo con esa evidencia.

## Semilleros y papel oficio (2026-11-13)

- **Copiados de `A-Produccion/perifericos/`** (comprobados con `cmp`): `SemilleroUnico_v2.R`, `SemilleroMoodle_v2.R`, `SemilleroCloze.R`,
  `pcielo.tex`, `pcielo_nosol.tex`, `solpcielo.tex`, `solpcielo_cloze.tex` y `solpcielo_cloze_una_por_pagina.tex`. `SemilleroCloze.R` y las
  plantillas CLOZE van sin adaptar (el ejercicio es solo SCHOICE).
- **Único cambio en cada semillero:** `archivo_examen` apunta al `.Rmd` de este ejercicio. `copias`, el `stopifnot(copias <= 100)` y el `seed`
  de Moodle quedan como en `perifericos/`.
- **Añadido solo a `SemilleroUnico_v2.R`:** una cabecera de 7 líneas que declara la adaptación (la regla #26 la exige) y, tras `exams2nops()`,
  el bucle de `pdfjam` que deja `*_nops_N_oficio.pdf` (216 × 330 mm, `--noautoscale`, igual que el semillero de `diagrama-venn-juegos-mesa-n3`).
  No lleva `twocolumn`, `samepage` ni plantilla de una pregunta por página: no hay opciones gráficas. Con la cabecera, `archivo_examen` queda
  en la línea 15. El PDF de preguntas usa `solpcielo.tex`, que ya declara oficio.
- **Imprimir** el `*_oficio.pdf` a tamaño real. Con un escaneo de otra hoja o colocación, pasarlo antes por
  `Auxiliares/exams2nops-escaner/normalizar_escaneos_nops.py`. Falta la prueba con papel y escáner reales.
- **Guarda:** `Rscript --no-init-file salida/verificar_uso_semillero.R` (unos 4 s). Comprueba (a) literales de los dos semilleros, (b) un render
  real de 2 preguntas en un directorio temporal con las medidas de `pdfinfo`, y (c) un mutante sin el paso de `pdfjam`. Exit 0 verde; 1 literales;
  2 render o medidas; 3 mutante vivo; 4 sin herramienta o archivo. Última línea: `VEREDICTO: …`.
- **Verificación real (copia en el scratchpad, `MPLBACKEND=Agg`, desde la carpeta):** `SemilleroUnico_v2.R` exit 0 en 13 s: PDF de 10 preguntas
  de 20 páginas a 612,3 × 935,4 pt (oficio), DOCX, NOPS A4 (595,3 × 841,9 pt, 5 páginas) y NOPS oficio (612,3 × 935,4 pt, 5 páginas).
  `SemilleroMoodle_v2.R` exit 0 en 31 s: XML con 100 preguntas de opción múltiple. Fuera de un repo git el semillero no encuentra el cuestionario
  interactivo (advertencia esperada); ejecutado a mano con `webquiz_autocontenido.R --n 10`: `WEBQUIZ_OK`, 662 fórmulas compuestas sin red.
  El YAML del Knit no causó fallos.
