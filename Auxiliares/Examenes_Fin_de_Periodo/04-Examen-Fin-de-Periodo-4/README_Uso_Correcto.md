# 📚 GUÍA DE USO: Generación de Exámenes Consistentes

## 🎯 OBJETIVO

Este directorio contiene un sistema corregido para generar exámenes R-exams con **consistencia garantizada** entre versiones con y sin soluciones.

---

## 📁 ARCHIVOS PRINCIPALES

### **Archivos de Ejercicio:**
- `cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd` - Ejercicio en formato R-exams

### **Scripts de Generación:**
- `SemilleroFinDePeriodo_4.R` - Script principal para generar todas las versiones

### **Scripts de Verificación:**
- `TEST_Verificacion_Consistencia.R` - Pruebas automáticas de consistencia

### **Documentación:**
- `SOLUCION_Consistencia_Versiones.md` - Explicación técnica de la solución
- `README_Uso_Correcto.md` - Este archivo

---

## 🚀 INSTRUCCIONES DE USO

### **PASO 1: Generar Exámenes**

Desde RStudio, ejecutar:

```r
source("SemilleroFinDePeriodo_4.R")
```

Esto generará **4 archivos** en la carpeta `salida/`:

1. `Matematicas_Evaluacion_Fin_de_Periodo_4-docx1.docx` - Versión DOCX **con soluciones**
2. `Matematicas_Evaluacion_Fin_de_Periodo_4_sin_sol1.docx` - Versión DOCX **sin soluciones**
3. `Matematicas_Evaluacion_Fin_de_Periodo_4_sol1.pdf` - Versión PDF **con soluciones**
4. `Matematicas_Evaluacion_Fin_de_Periodo_4_sin_sol1.pdf` - Versión PDF **sin soluciones**

### **PASO 2: Verificar Consistencia (Opcional)**

Para confirmar que todas las versiones tienen los mismos datos:

```r
source("TEST_Verificacion_Consistencia.R")
```

Deberías ver:
```
✅ TODAS LAS PRUEBAS PASARON
   La solución de consistencia está funcionando correctamente.
   Las versiones con y sin soluciones generarán los mismos datos.
```

---

## 🔧 CONFIGURACIÓN AVANZADA

### **Cambiar la Semilla para Generar Versión Diferente**

Editar `SemilleroFinDePeriodo_4.R`, línea 14:

```r
# Cambiar este número para generar una versión completamente diferente
semilla <- 123456  # ← Cambiar a cualquier otro número
```

**Ejemplos:**
- `semilla <- 654321` - Generará versión diferente pero consistente
- `semilla <- 111111` - Otra versión diferente
- `semilla <- as.numeric(Sys.Date())` - Versión basada en la fecha actual

### **Generar Múltiples Copias**

Editar `SemilleroFinDePeriodo_4.R`, línea 9:

```r
copias <- 5  # Generará 5 copias diferentes
```

**IMPORTANTE:** Cada copia tendrá datos diferentes, pero las versiones con/sin soluciones de la **misma copia** serán consistentes.

---

## ✅ VERIFICACIÓN MANUAL

### **Cómo Verificar que las Versiones son Consistentes:**

1. **Abrir ambos archivos** (con y sin soluciones) del mismo formato
2. **Comparar el enunciado de la pregunta:**
   - Debe mencionar el mismo nombre (ej: "Margarita", "Carlos", etc.)
   - Debe tener los mismos valores numéricos en el diagrama
   - Debe tener los mismos valores en los pasos 1 y 2
3. **Comparar las opciones de respuesta:**
   - Deben ser exactamente las mismas 4 opciones
   - En el mismo orden
4. **Única diferencia esperada:**
   - La versión "sin soluciones" NO muestra la sección "Solution"
   - La versión "con soluciones" SÍ muestra la sección "Solution"

### **Ejemplo de Consistencia Correcta:**

**Versión SIN soluciones:**
```
Margarita debe encontrar el valor del cateto x...
[Diagrama con valores: 3, √2]

Paso 1: Calcula √(x² + 3²) y obtiene √2
Paso 2: El resultado es 2

¿Cuál es el valor del cateto x?

A) x = 1
B) x = 3
C) x = √2
D) x = 2
```

**Versión CON soluciones:**
```
Margarita debe encontrar el valor del cateto x...
[Diagrama con valores: 3, √2]  ← MISMO DIAGRAMA

Paso 1: Calcula √(x² + 3²) y obtiene √2  ← MISMOS VALORES
Paso 2: El resultado es 2  ← MISMO RESULTADO

¿Cuál es el valor del cateto x?

A) x = 1  ← MISMAS OPCIONES
B) x = 3
C) x = √2
D) x = 2

[SECCIÓN SOLUTION AQUÍ]  ← ÚNICA DIFERENCIA
```

---

## ⚠️ ERRORES COMUNES Y SOLUCIONES

### **Error: "Las versiones tienen datos diferentes"**

**Causa:** Se modificó el archivo `.Rmd` y se agregó `set.seed()` dentro de él.

**Solución:** 
1. Verificar que el archivo `.Rmd` NO contenga `set.seed()` en el chunk `data_generation`
2. Verificar que el script `SemilleroFinDePeriodo_4.R` SÍ tenga `set.seed(semilla)` antes de cada llamada a `exams2*()`

### **Error: "Todas las copias son idénticas"**

**Causa:** La semilla se establece una sola vez y no se cambia entre copias.

**Solución:** Esto es el **comportamiento esperado** con la solución implementada. Para generar copias diferentes:

```r
# Opción 1: Cambiar semilla manualmente entre ejecuciones
semilla <- 123456  # Primera ejecución
# ... ejecutar script ...
semilla <- 654321  # Segunda ejecución
# ... ejecutar script ...

# Opción 2: Usar semilla basada en timestamp (genera copias diferentes cada vez)
semilla <- as.numeric(Sys.time())
```

---

## 📊 FLUJO DE TRABAJO RECOMENDADO

### **Para Exámenes de Producción:**

1. **Establecer semilla fija** en `SemilleroFinDePeriodo_4.R`
2. **Generar todas las versiones** (DOCX y PDF, con y sin soluciones)
3. **Verificar consistencia** manualmente o con script de prueba
4. **Distribuir versión sin soluciones** a estudiantes
5. **Guardar versión con soluciones** para corrección

### **Para Generar Múltiples Versiones Diferentes:**

```r
# Generar 10 versiones diferentes del examen
for (i in 1:10) {
  semilla <- 100000 + i
  set.seed(semilla)
  
  # Generar versión sin soluciones
  exams2pdf(archivo_examen, 
            n = 1,
            name = paste0("Examen_Version_", i),
            template = "exam",
            dir = "salida")
}
```

---

## 🎓 NOTAS TÉCNICAS

- La solución implementada es compatible con la filosofía ICFES de 300+ versiones únicas
- El control de semilla NO afecta la capacidad de generar versiones diversas
- La diversidad se mantiene a través de la función `generar_datos()` que usa `sample()` internamente
- Para más detalles técnicos, consultar `SOLUCION_Consistencia_Versiones.md`

