---
name: validar-diversidad-300
description: Ejecuta pruebas de testthat para confirmar la generación de 300+ versiones únicas (Fase 3 del workflow).
---
# Instrucciones de Validación
1. Una vez generado el archivo .Rmd en la carpeta `/A-Produccion/`, localiza su 
correspondiente test en `tests/testthat/` [2].
2. Ejecuta el comando local: `Rscript -e 'testthat::test_file("ruta/del/test.R")'`.
3. Informa al usuario si el ejercicio cumple con el criterio de aleatorización 
completa definido en la filosofía del sistema [3, 4].
