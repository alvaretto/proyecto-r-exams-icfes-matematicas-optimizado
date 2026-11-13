# Regla #27 — Clasificación impresa en el original: triangular y decidir (versión compacta)

> Texto íntegro —origen, concordancia medida del rótulo SAI, primer caso (`MAT-2026-07-017`), plantilla de la tabla—: `.claude/docs/reglas/triangulacion-clasificacion-impresa.md`.

**Principio.** Si el original escaneado trae impresa una clasificación del ítem (competencia, componente, afirmación o evidencia, nivel, descriptor, estándar, o códigos que los cifran), esa línea es un **voto**: ni se recorta como ruido ni se copia como verdad. Se triangula con las dos fuentes locales, dimensión por dimensión, y cada discrepancia se cierra con una **decisión**. «Observación para el profesor» no es un cierre.

- **T-1 Transcribir.** En el paso 1 se mira la franja bajo las opciones y el rótulo se copia literal en `ITEM_OFICIAL.md`. El rótulo del ítem es el que va **debajo** de sus opciones (regla #24 H-4): el que queda encima del número impreso es del ítem anterior. Si el original no trae ninguno, se declara («no trae»).
- **T-2 Decodificar.** Cada código se traduce al texto literal del catálogo canónico, citando la clave de lectura y cómo se comprobó («A: 2.3» = evidencia 3 de la competencia 2). Un código que no se puede decodificar no vota, y se declara.
- **T-3 Tres votos**, solo en las dimensiones que el impreso trae: **impreso** (E), **ficha** de alineación (F) y **clasificación propia a ciegas** (P), hecha por un agente distinto que ve el recorte sin el rótulo y el catálogo, y no ve la ficha. Las demás dimensiones siguen la ficha.
- **T-4 Decidir.**
  - 3 de 3: concordante.
  - 2 de 3: decide la mayoría. Si la ficha queda en minoría (E = P ≠ F), el ejercicio adopta la mayoría en `ITEM_OFICIAL.md` y en `exextra`, y la ficha se reporta al profesor como candidata a corrección: no se edita desde este repo.
  - 1-1-1, o sin voto P: no hay decisión autónoma. Se le pregunta al profesor en lenguaje llano, con el texto literal de cada candidato.
  - Si quien imprime es el ICFES (cuadernillo o guía oficial), su clasificación prevalece sobre la ficha; la de un editor comercial es un voto.
  - El valor decidido existe literal en el catálogo (regla #7). Si cambia competencia, componente o nivel con el `.Rmd` ya escrito, cambia el nombre del archivo: `reopen` y renombrado (regla #16).
- **T-5 Registrar.** Tabla «Triangulación de la clasificación» en `ITEM_OFICIAL.md` (`Dimensión | Impreso | Ficha | Propia | Decisión | Razón`; nunca de dos columnas, porque `validar_literalidad_exextra.py --item` las lee como campos) y la misma tabla en el reporte del paso 10. Sin esa tabla con todas sus decisiones no se sella `validar_icfes`.

**Defensa:** sin guarda mecánica. La aplican el paso 1 y el paso 10 de los orquestadores, y la audita el detractor (dominio ICFES). Los ejercicios anteriores se ajustan en el próximo ciclo de cada uno.

**Versión:** 1.0 (2026-11-13).
