# Instrucciones para Agentes de IA en el Repositorio de R/Exams

Este documento proporciona una guía para que los agentes de IA (como GitHub Copilot) comprendan y trabajen eficazmente en este repositorio, que se centra en la generación de ejercicios de matemáticas para la prueba ICFES utilizando el framework R/exams.

## Arquitectura y Conceptos Clave

El objetivo principal de este proyecto es generar ejercicios de matemáticas dinámicos y parametrizados a partir de archivos de plantilla `.Rmd`.

- **Framework Principal**: [R/exams](http://www.r-exams.org/). Todo el contenido se crea dentro de este ecosistema.
- **Archivos de Plantilla (`.Rmd`)**: Cada archivo `.Rmd` representa una única pregunta. Contiene una mezcla de Markdown para el texto, código R para la lógica de aleatorización y LaTeX/TikZ para la generación de gráficos y tablas.
- **Generación de Datos**: Dentro de cada `.Rmd`, un chunk de R (generalmente el primero) genera aleatoriamente los parámetros de la pregunta (por ejemplo, `p_central`, `limite1`). Esto permite crear miles de variaciones únicas de la misma pregunta.
- **Versiones de Ejercicios**: El proyecto mantiene múltiples versiones de la misma pregunta, que varían en tecnología y dificultad:
    - `*_v1.Rmd`: Versiones que pueden usar Python/matplotlib para generar imágenes PNG.
    - `*_tikz_v1.Rmd`: Versiones más avanzadas que usan TikZ (nativo de LaTeX) para crear gráficos y tablas vectoriales de alta calidad.
    - `*_cloze_v1_2.Rmd`: Las versiones más complejas, que utilizan el formato `cloze` de R/exams para combinar múltiples sub-preguntas (numéricas, de opción múltiple) en un solo ejercicio.

## Flujo de Trabajo del Desarrollador

El flujo de trabajo típico para modificar o crear un ejercicio es el siguiente:

1.  **Modificar un archivo `.Rmd`**: Edita el texto de la pregunta, la lógica de generación de datos en los chunks de R, o las opciones de respuesta.
2.  **Generar los Resultados**: Utiliza los comandos del paquete `exams` en R para compilar las plantillas en formatos finales. Los archivos generados se guardan en el directorio `salida/`.
    - **Para generar un PDF**:
      ```r
      exams::exams2pdf("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1_2.Rmd", n = 1, dir = "salida")
      ```
    - **Para generar un paquete para Moodle**:
      ```r
      exams::exams2moodle("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1_2.Rmd", n = 5, dir = "salida")
      ```
3.  **Verificar los Resultados**: Revisa los archivos generados en el directorio `salida/` para asegurarte de que la pregunta se renderiza correctamente y la lógica es correcta.

## Convenciones y Patrones Específicos del Proyecto

- **Nomenclatura de Archivos**: Los archivos siguen una convención estricta para alinearse con los estándares de ICFES: `tema_subtema_tipo_competencia_componente_nivel_version.Rmd`.
- **Gráficos y Tablas con TikZ**: Se prefiere el uso de TikZ sobre las imágenes PNG para obtener una calidad vectorial superior y una mejor integración con LaTeX. El código TikZ se encuentra dentro de los chunks de R en los archivos `.Rmd`.
- **Scripts de Generación Masiva**:
    - `SemilleroUnico_v2.R`: Script para generar versiones individuales de los ejercicios.
    - `SemilleroMoodle_v2.R`: Script para generar paquetes de preguntas para Moodle.
- **Plantillas LaTeX**: La apariencia de los PDFs generados se controla mediante plantillas `.tex` como `pcielo.tex`.

## Archivos y Directorios Clave

- `*.Rmd`: Los archivos de plantilla de las preguntas. Son el corazón del repositorio.
- `salida/`: Directorio donde se guardan todos los ejercicios generados (PDF, HTML, XML de Moodle, etc.).
- `*.tex`: Plantillas de LaTeX que definen el estilo de los documentos PDF.
- `Semillero*.R`: Scripts de R para automatizar la generación de ejercicios.
- `README.md`: Contiene una descripción detallada de la estructura del proyecto y las diferentes versiones de los ejercicios.
- `WALKTHROUGH_INCREMENTO_DIFICULTAD.md`: Documentación específica sobre las mejoras y la lógica de las versiones más avanzadas.
