#!/usr/bin/env Rscript

library(exams)

# Crear un ejercicio de prueba simple
cat("Creando ejercicio de prueba...\n")

# Crear directorio de prueba
dir.create("test_exams", showWarnings = FALSE)

# Crear ejercicio simple
exercise_content <- '
---
output: 
  exams::exams2pdf:
    template: plain
---

# Ejercicio de Prueba - Matemáticas ICFES

¿Cuál es el resultado de $2 + 3$?

```{r, echo=FALSE, results="asis"}
# Generar opciones
correct <- 5
options <- c(correct, 3, 4, 6, 7)
options <- sample(options)

# Encontrar posición correcta
correct_pos <- which(options == correct)

# Mostrar opciones
for(i in 1:length(options)) {
  cat("\\n", LETTERS[i], ") ", options[i], sep="")
}
```

Meta-information
================
extype: schoice
exsolution: `r paste(rep("0", 5), collapse="")`
exsolution: `r paste(replace(rep("0", 5), correct_pos, "1"), collapse="")`
exname: suma_basica
'

# Escribir archivo
writeLines(exercise_content, "test_exams/suma_basica.Rmd")

# Probar compilación
cat("Probando compilación PDF...\n")
tryCatch({
  exams2pdf("test_exams/suma_basica.Rmd", n = 1, name = "prueba_icfes")
  cat("✓ Prueba PDF exitosa\n")
}, error = function(e) {
  cat("✗ Error en prueba PDF:", e$message, "\n")
})

cat("🎯 Prueba completada. Revisa los archivos generados.\n")
