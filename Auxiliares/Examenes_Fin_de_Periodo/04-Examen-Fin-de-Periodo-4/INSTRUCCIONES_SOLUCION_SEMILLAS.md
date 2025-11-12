# SOLUCIÓN AL PROBLEMA DE SEMILLAS IDÉNTICAS

## PROBLEMA IDENTIFICADO

Cuando cambiabas la semilla de 1001 a 1002 a 1003, las preguntas generadas eran idénticas debido a:

### 1. **`set.seed()` interno en archivos .Rmd**
El archivo `cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd` usaba `set.seed(seed_distractores)` en dos lugares, lo que sobrescribía la semilla global y rompía la secuencia aleatoria.

### 2. **Caché de R/exams**
Los archivos temporales de compilaciones anteriores podían estar causando que se reutilizaran ejercicios previamente generados.

### 3. **Restablecimiento incorrecto de semilla**
El script restablecía la semilla antes de cada `exams2pdf`, lo que podía causar problemas de sincronización.

## CAMBIOS REALIZADOS

### ✅ **Archivo: cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd**

**Líneas 199 y 329**: Eliminados los `set.seed(seed_distractores)` que rompían la secuencia aleatoria global.

**ANTES:**
```r
set.seed(seed_distractores)
indices_seleccionados <- sample(...)
```

**AHORA:**
```r
# CORRECCIÓN CRÍTICA: NO usar set.seed() aquí para no romper la secuencia aleatoria global
# set.seed(seed_distractores)  # ELIMINADO
indices_seleccionados <- sample(...)
```

### ✅ **Archivo: SemilleroFinDePeriodo_4.R**

**Agregado**: Sistema de limpieza de caché antes de generar PDFs (líneas 95-108)

**Modificado**: Flujo de generación de PDFs para mantener consistencia entre versiones con/sin soluciones (líneas 110-148)

## INSTRUCCIONES DE USO

### **PASO 1: Verificar que el sistema funciona**

Ejecuta el script de prueba en RStudio:

```r
source("Auxiliares/Examenes_Fin_de_Periodo/04-Examen-Fin-de-Periodo-4/test_semillas.R")
```

Deberías ver:
```
✓ OK: Semillas 1001 y 1002 generan órdenes DIFERENTES
✓ OK: Semillas 1002 y 1003 generan órdenes DIFERENTES
✓ OK: Semillas 1001 y 1003 generan órdenes DIFERENTES
✓ OK: Semillas 1001 y 1002 generan números DIFERENTES
```

Si ves errores ❌, hay un problema con tu instalación de R.

### **PASO 2: Limpiar archivos anteriores**

Antes de generar nuevos PDFs, elimina los archivos antiguos:

```r
# En RStudio, ejecuta:
unlink("Auxiliares/Examenes_Fin_de_Periodo/04-Examen-Fin-de-Periodo-4/salida/*.pdf")
unlink("Auxiliares/Examenes_Fin_de_Periodo/04-Examen-Fin-de-Periodo-4/salida/.exams", recursive = TRUE)
```

### **PASO 3: Cambiar la semilla**

Edita el archivo `SemilleroFinDePeriodo_4.R` línea 17:

```r
semilla <- 1001  # Cambia este número
```

### **PASO 4: Ejecutar el script**

```r
source("Auxiliares/Examenes_Fin_de_Periodo/04-Examen-Fin-de-Periodo-4/SemilleroFinDePeriodo_4.R")
```

### **PASO 5: Verificar los PDFs generados**

Los archivos se generarán en:
- `salida/Matematicas_Evaluacion_Fin_de_Periodo_4_sol1.pdf` (con soluciones)
- `salida/Matematicas_Evaluacion_Fin_de_Periodo_4_sin_sol1.pdf` (sin soluciones)

**IMPORTANTE**: Ambos PDFs deben tener exactamente las mismas preguntas en el mismo orden.

### **PASO 6: Probar con diferentes semillas**

1. Cambia `semilla <- 1002` en línea 17
2. Ejecuta el script nuevamente
3. Compara los nuevos PDFs con los anteriores
4. Deberías ver:
   - **Orden diferente** de las 20 preguntas
   - **Datos diferentes** dentro de cada pregunta
   - **Número de taller diferente** en el encabezado

## VERIFICACIÓN DE RESULTADOS

### ✅ **Qué DEBE cambiar con diferentes semillas:**
- Orden de las 20 preguntas en el examen
- Valores numéricos dentro de cada pregunta
- Contextos y nombres de personajes
- Opciones de respuesta y distractores
- Número de "Taller xxxx" en el encabezado

### ❌ **Qué NO debe cambiar:**
- Tipos de preguntas (siempre las mismas 3 preguntas base)
- Estructura del examen
- Formato del PDF
- Templates LaTeX

## SOLUCIÓN DE PROBLEMAS

### **Problema: Las preguntas siguen siendo idénticas**

1. Verifica que guardaste los cambios en los archivos
2. Reinicia RStudio completamente
3. Ejecuta el script de prueba (test_semillas.R)
4. Limpia el caché manualmente:
   ```r
   unlink("salida/.exams", recursive = TRUE)
   ```

### **Problema: Error al compilar**

1. Verifica que todos los archivos .Rmd existen
2. Comprueba que las rutas son correctas
3. Revisa que no hay errores de sintaxis en los .Rmd

### **Problema: PDFs con/sin soluciones son diferentes**

Esto NO debería ocurrir. Si ocurre, reporta el problema con detalles específicos.

## NOTAS TÉCNICAS

- La semilla se establece UNA SOLA VEZ al inicio (línea 18)
- El orden de preguntas se genera con esa semilla (línea 23)
- La primera generación PDF usa la secuencia aleatoria continua
- La segunda generación PDF RESETEA la semilla para mantener consistencia
- El caché se limpia automáticamente antes de cada generación

