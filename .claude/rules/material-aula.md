# Regla #26 — Material del aula: papel oficio, NOPS, Moodle y celulares sin internet (versión compacta)

> Texto íntegro —mediciones, incidentes del 2026-11-06 al 2026-11-09 y procedimientos—: `.claude/docs/reglas/material-aula.md`.

**Principio.** Lo que llega al aula se verifica en la salida real que usa el profesor: la hoja oficio impresa, la hoja NOPS escaneada, el XML de Moodle y el celular sin red. Que el `.Rmd` pase sus validaciones no prueba nada de eso.

- **Versión oficial de plantillas y semilleros:** la de `A-Produccion/perifericos/` **en disco** (decisión del profesor, 2026-11-09). Git la sigue, y las copias de cada ejercicio derivan de ella y se rehacen cuando cambia. No se edita sin instrucción del profesor.
- **Papel:** oficio de 216 × 330 mm, no legal (356 mm). Lo declaran `solpcielo.tex` y `solpcielo_cloze.tex`.
- **Varias preguntas con opciones gráficas en un PDF:** plantilla local = copia de la oficial con `\clearpage` al empezar cada pregunta (`solpcielo_una_por_pagina.tex`; en CLOZE, sobre `solpcielo_cloze.tex`). Con la compartida, las preguntas que empiezan a media página parten sus figuras.
- **NOPS con opciones gráficas:** `twocolumn = TRUE`, `samepage = TRUE` y `header = "\\raggedbottom"`. `exams2nops` fija A4, y `pdfjam` centra cada página en oficio sin escalar (`*_oficio.pdf`). Un escaneo con otra colocación pasa antes por `normalizar_escaneos_nops.py`.
- **CLOZE con gráficas rotuladas I-IV (Incidente G) que se imprime:** «Cómo marcar» pide la letra por posición, así que el gap del PDF va sin barajar: `exshuffle = FALSE` **solo** en la llamada `exams2pdf()` del semillero, nunca en el `.Rmd`. En exams 2.4.5, `exams2moodle()` y `exams2canvas()` no tienen ese argumento y abortan con él; `exams2html()` lo acepta y deja de barajar sin avisar (Error 42).
- **Exportaciones:** máximo 100 versiones (decisión del profesor del 2026-08-15, reafirmada el 2026-11-09). En Moodle, `seed = matrix(seq_len(n), ncol = 1)`: la versión s es la semilla s que valida el paso 9. No cambia el umbral de diversidad de 250 únicas sobre 300 (regla #3 de `codigo-rmd.md`).
- **Cuestionario interactivo:** con `.claude/scripts/webquiz_autocontenido.R` (exit 0 = `WEBQUIZ_OK`), nunca con `exams2webquiz()` solo: carga MathJax desde internet, y sin red las fórmulas salen en TeX crudo.
- **Semillero del ejercicio:** si el ejercicio tiene condiciones de impresión medidas, su semillero local las lleva con una cabecera que lo dice, y una sección `uso` del verificador las vigila con literales, un render real y mutantes. No se reemplaza por la copia genérica de `perifericos/`.
- **Knit de RStudio:** no se usa sobre un `.Rmd` de R/exams: le antepone YAML y cambia el md5 de una firma (Error 43). Si aparece, se quita solo con el visto bueno del profesor y después de comprobar que el cuerpo tiene el md5 firmado.

**Defensa:** no hay guarda global. Vigilan las secciones `uso` de cada ejercicio (en `diagrama-venn-juegos-mesa-n3`, SCHOICE y CLOZE) y los orquestadores al crear el semillero. Los semilleros aprobados antes de esta regla se ajustan en el próximo ciclo de su ejercicio.

**Versión:** 1.0 (2026-11-09).
