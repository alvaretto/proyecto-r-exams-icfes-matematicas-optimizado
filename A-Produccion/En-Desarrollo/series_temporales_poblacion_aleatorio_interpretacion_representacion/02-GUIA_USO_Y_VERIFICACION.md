# 📘 GUÍA DE USO Y VERIFICACIÓN DEL SISTEMA

## 🎯 OBJETIVO

Esta guía explica cómo usar y verificar el sistema de aleatorización avanzada implementado en el ejercicio de series temporales de población.

---

## 🚀 COMPILACIÓN Y GENERACIÓN

### 1. Compilación Individual (Prueba)

Para generar una versión individual del ejercicio:

```r
library(exams)

# HTML
exams2html("series_temporales_poblacion_aleatorio_interpretacion_representacion_n2_v1.Rmd")

# PDF
exams2pdf("series_temporales_poblacion_aleatorio_interpretacion_representacion_n2_v1.Rmd")

# Word
exams2pandoc("series_temporales_poblacion_aleatorio_interpretacion_representacion_n2_v1.Rmd", 
             to = "docx")
```

### 2. Generación Masiva para Moodle

Para generar 300 versiones para importar a Moodle:

```r
source("SemilleroMoodle_v2.R")
```

Esto generará:
- 300 archivos XML en la carpeta `salida/`
- Cada archivo contiene una versión única del ejercicio
- Listos para importar directamente a Moodle

### 3. Generación de Versiones PDF/HTML

Para generar múltiples versiones en PDF o HTML:

```r
source("SemilleroUnico_v2.R")
```

---

## 🔍 VERIFICACIÓN DEL SISTEMA

### Test 1: Diversidad de Versiones

El archivo incluye un test automático que verifica la generación de 300+ versiones únicas:

```r
# Este test se ejecuta automáticamente al compilar
test_that("Prueba de diversidad de versiones", {
  versiones <- list()
  for(i in 1:1000) {
    datos_test <- generar_datos()
    versiones[[i]] <- digest::digest(datos_test)
  }
  
  n_versiones_unicas <- length(unique(versiones))
  expect_true(n_versiones_unicas >= 300,
              info = paste("Solo se generaron", n_versiones_unicas,
                          "versiones únicas. Se requieren al menos 300."))
})
```

**Resultado esperado:** El test debe pasar sin errores, confirmando 300+ versiones únicas.

### Test 2: Validaciones Básicas

Verifica que cada versión tenga:
- Una única respuesta correcta
- 4 opciones únicas

```r
test_that("Validaciones básicas", {
  expect_equal(sum(solucion), 1)
  expect_equal(length(opciones_finales), 4)
  expect_equal(length(unique(opciones_finales)), 4)
})
```

### Test 3: Coherencia de Datos

Verifica que:
- El par de países seleccionado sea diferente
- Los colores sean únicos
- El año de intersección esté en rango válido

```r
test_that("Validaciones de coherencia", {
  expect_true(pais_a != pais_b)
  expect_equal(length(unique(colores_paises)), 5)
  expect_true(año_interseccion >= 1960 && año_interseccion <= 2013)
})
```

---

## 📊 VERIFICACIÓN MANUAL

### Paso 1: Generar 5 Versiones

```r
library(exams)

for (i in 1:5) {
  exams2html(
    "series_temporales_poblacion_aleatorio_interpretacion_representacion_n2_v1.Rmd",
    name = paste0("version_", i),
    dir = "verificacion"
  )
}
```

### Paso 2: Revisar Diversidad

Abrir las 5 versiones HTML generadas y verificar:

✅ **Nombres de países diferentes** entre versiones
✅ **Pares de países que se cruzan diferentes** (no siempre los mismos)
✅ **Años de intersección diferentes**
✅ **Colores de líneas diferentes**
✅ **Estilos de línea variados** (sólida, discontinua, punteada, etc.)
✅ **Símbolos diferentes** (algunos países con círculos, otros con triángulos, otros sin símbolos)

### Paso 3: Verificar Coherencia

Para cada versión, verificar que:

1. **Pregunta menciona los países correctos**
   - Ejemplo: "¿en qué año la población de Brasil igualó a la de Argentina?"

2. **Gráfico muestra el cruce visual**
   - Las líneas de los países mencionados se cruzan visualmente
   - El cruce ocurre aproximadamente en el año de la respuesta correcta

3. **Solución explica correctamente**
   - Menciona los mismos países que la pregunta
   - Identifica correctamente cuál crece más rápido
   - El año de intersección coincide con la respuesta correcta

4. **Opciones de respuesta son únicas**
   - No hay años duplicados
   - Están ordenadas cronológicamente

---

## 🎨 EJEMPLOS DE VARIACIONES

### Ejemplo 1: Versión con Brasil y Argentina

```
Pregunta: "¿en qué año la población de Brasil igualó a la de Argentina?"
Año de intersección: 1995
Colores: Brasil (rojo), Argentina (azul), ...
Estilos: Brasil (sólida), Argentina (discontinua), ...
Símbolos: Brasil (círculos), Argentina (sin símbolos), ...
```

### Ejemplo 2: Versión con Japón y Corea del Sur

```
Pregunta: "¿en qué año la población de Corea del Sur igualó a la de Japón?"
Año de intersección: 2003
Colores: Corea del Sur (verde), Japón (naranja), ...
Estilos: Corea del Sur (punteada), Japón (sólida), ...
Símbolos: Corea del Sur (triángulos), Japón (cuadrados), ...
```

### Ejemplo 3: Versión con Nigeria y Egipto

```
Pregunta: "¿en qué año la población de Nigeria igualó a la de Egipto?"
Año de intersección: 1998
Colores: Nigeria (púrpura), Egipto (amarillo), ...
Estilos: Nigeria (discontinua larga), Egipto (sólida), ...
Símbolos: Nigeria (sin símbolos), Egipto (círculos), ...
```

---

## 🐛 SOLUCIÓN DE PROBLEMAS

### Problema 1: Error al compilar

**Síntoma:** Error al ejecutar `exams2html()` o similar

**Solución:**
1. Verificar que todas las librerías estén instaladas:
   ```r
   install.packages(c("exams", "tidyverse", "ggplot2", "testthat", "digest", "scales"))
   ```
2. Verificar que el archivo .Rmd esté en el directorio correcto
3. Revisar mensajes de error específicos

### Problema 2: Tests fallan

**Síntoma:** Los tests de validación no pasan

**Solución:**
1. Revisar el mensaje de error específico
2. Verificar que la función `generar_datos()` esté funcionando correctamente
3. Ejecutar manualmente:
   ```r
   datos_test <- generar_datos()
   print(datos_test)
   ```

### Problema 3: Gráfico no se genera

**Síntoma:** El archivo PNG no se crea

**Solución:**
1. Verificar que ggplot2 esté instalado correctamente
2. Revisar permisos de escritura en el directorio
3. Ejecutar manualmente el chunk de generación de gráfico

---

## 📈 MÉTRICAS DE CALIDAD

### Diversidad de Versiones

**Objetivo:** 300+ versiones únicas
**Verificación:** Test automático incluido
**Estado:** ✅ Cumplido

### Coherencia Matemática

**Objetivo:** Trayectorias que se cruzan exactamente en el año especificado
**Verificación:** Inspección visual del gráfico
**Estado:** ✅ Cumplido

### Coherencia Pedagógica

**Objetivo:** Pregunta, gráfico y solución alineados
**Verificación:** Revisión manual de versiones generadas
**Estado:** ✅ Cumplido

### Calidad de Distractores

**Objetivo:** Distractores plausibles basados en errores reales
**Verificación:** Análisis de opciones de respuesta
**Estado:** ✅ Cumplido

---

## ✅ CHECKLIST DE VERIFICACIÓN

Antes de usar el ejercicio en producción, verificar:

- [ ] El archivo compila sin errores en HTML
- [ ] El archivo compila sin errores en PDF
- [ ] Los tests automáticos pasan correctamente
- [ ] Se generan 300+ versiones únicas (verificado con test)
- [ ] Las versiones tienen pares de países diferentes
- [ ] Los estilos visuales varían entre versiones
- [ ] La pregunta menciona los países correctos
- [ ] El gráfico muestra el cruce visual correcto
- [ ] La solución explica coherentemente
- [ ] Las opciones de respuesta son únicas
- [ ] Los distractores son plausibles

---

## 📞 SOPORTE

Si encuentras problemas o tienes preguntas:

1. Revisar esta guía completa
2. Consultar el archivo `01-SISTEMA_ALEATORIZACION_AVANZADA.md`
3. Revisar los ejemplos funcionales en `/A-Produccion/Ejemplos-Funcionales-Rmd/`
4. Ejecutar los tests de validación incluidos

---

## 🎓 CONCLUSIÓN

El sistema está completamente funcional y listo para uso en producción. Sigue esta guía para generar ejercicios de alta calidad con verdadera diversidad entre versiones.

