# Comparación de Versiones: v1 vs v2 - Diagramas de Caja

## 📊 **Resumen de Cambios**

Se ha creado una nueva versión (v2) del ejercicio de diagramas de caja que presenta las opciones de manera individual en lugar de mostrar los cuatro diagramas juntos.

## 📁 **Archivos Comparados**

- **Versión 1:** `estadistica_diagramas_caja_interpretacion_representacion_Nivel2_v1.Rmd`
- **Versión 2:** `estadistica_diagramas_caja_interpretacion_representacion_Nivel2_v2.Rmd`

## 🔄 **Principales Diferencias**

### **Presentación de Opciones**

#### **Versión 1 (v1) - Diagramas Combinados**
```markdown
A continuación se presentan cuatro diagramas de caja:

[IMAGEN ÚNICA CON 4 DIAGRAMAS LADO A LADO]

¿Cuál de los diagramas corresponde mejor a los datos de la tabla?

Answerlist
----------
- Diagrama A
- Diagrama B  
- Diagrama C
- Diagrama D
```

#### **Versión 2 (v2) - Opciones Individuales**
```markdown
¿Cuál de los siguientes diagramas corresponde mejor a los datos de la tabla?

Answerlist
----------
- [IMAGEN INDIVIDUAL DEL DIAGRAMA A]
- [IMAGEN INDIVIDUAL DEL DIAGRAMA B]
- [IMAGEN INDIVIDUAL DEL DIAGRAMA C]
- [IMAGEN INDIVIDUAL DEL DIAGRAMA D]
```

**Formato técnico implementado:**
```r
cat("-\n")
cat("![](diagrama_a.png){width=70%}\n\n")
cat("-\n")
cat("![](diagrama_b.png){width=70%}\n\n")
# ... etc
```

### **Generación de Gráficos**

#### **Versión 1 - Un Solo Archivo**
- **Archivos generados:** `diagramas_caja.png`, `diagramas_caja.pdf`
- **Estructura:** 4 subplots en una sola figura (1×4)
- **Tamaño:** 12×6 pulgadas
- **Título general:** "Diagramas de Caja y Bigotes"

#### **Versión 2 - Archivos Individuales**
- **Archivos generados:** 
  - `diagrama_a.png`, `diagrama_a.pdf`
  - `diagrama_b.png`, `diagrama_b.pdf`
  - `diagrama_c.png`, `diagrama_c.pdf`
  - `diagrama_d.png`, `diagrama_d.pdf`
- **Estructura:** 4 figuras individuales (1×1 cada una)
- **Tamaño:** 4×6 pulgadas cada una
- **Títulos individuales:** "Diagrama A", "Diagrama B", etc.

## 🎯 **Ventajas y Desventajas**

### **Versión 1 (Diagramas Combinados)**

#### ✅ **Ventajas:**
- **Comparación visual directa:** Los estudiantes pueden comparar fácilmente los 4 diagramas
- **Visión panorámica:** Permite ver patrones y diferencias de un vistazo
- **Menor espacio:** Una sola imagen ocupa menos espacio en el documento
- **Carga más rápida:** Un solo archivo de imagen para descargar

#### ❌ **Desventajas:**
- **Tamaño de imagen:** Los diagramas individuales pueden verse pequeños
- **Resolución limitada:** Detalles pueden ser difíciles de distinguir
- **Menos enfoque:** Puede ser abrumador ver todas las opciones juntas

### **Versión 2 (Opciones Individuales)**

#### ✅ **Ventajas:**
- **Mayor claridad visual:** Cada diagrama se ve más grande y claro
- **Enfoque individual:** Los estudiantes pueden analizar cada opción por separado
- **Mejor legibilidad:** Etiquetas y valores más fáciles de leer
- **Formato estándar:** Sigue el patrón común de exámenes con opciones visuales

#### ❌ **Desventajas:**
- **Comparación más difícil:** Requiere desplazarse para comparar opciones
- **Mayor espacio:** Ocupa más espacio en el documento
- **Más archivos:** 4 imágenes en lugar de 1
- **Carga más lenta:** Múltiples archivos para descargar

## 📚 **Consideraciones Pedagógicas**

### **Versión 1 - Enfoque Analítico**
- **Habilidad principal:** Comparación visual simultánea
- **Proceso cognitivo:** Análisis comparativo directo
- **Estrategia:** Eliminación por contraste visual
- **Nivel de dificultad:** Ligeramente más alto (requiere análisis visual fino)

### **Versión 2 - Enfoque Sistemático**
- **Habilidad principal:** Análisis individual y evaluación secuencial
- **Proceso cognitivo:** Verificación paso a paso contra criterios
- **Estrategia:** Evaluación individual de cada opción
- **Nivel de dificultad:** Más accesible (análisis detallado posible)

## 🎓 **Recomendaciones de Uso**

### **Usar Versión 1 cuando:**
- Los estudiantes tienen experiencia con diagramas de caja
- Se busca evaluar habilidades de comparación visual
- El espacio en el documento es limitado
- Se quiere un enfoque más desafiante

### **Usar Versión 2 cuando:**
- Los estudiantes están aprendiendo diagramas de caja
- Se busca máxima claridad visual
- Se quiere seguir formatos estándar de examen
- La accesibilidad visual es prioritaria

## 🔧 **Aspectos Técnicos**

### **Compatibilidad**
- **Ambas versiones:** Compatible con HTML, PDF, Moodle, Word
- **Aleatorización:** Idéntica en ambas versiones
- **Validaciones:** Mismas verificaciones matemáticas
- **Metadatos:** Información ICFES idéntica

### **Rendimiento**
- **Versión 1:** Menor uso de ancho de banda, carga más rápida
- **Versión 2:** Mayor uso de ancho de banda, carga más lenta

### **Mantenimiento**
- **Versión 1:** Más simple (1 archivo de imagen)
- **Versión 2:** Más complejo (4 archivos de imagen)

## 📊 **Métricas de Calidad**

### **Ambas Versiones Mantienen:**
- ✅ **Aleatorización:** 12+ parámetros, 300+ variantes
- ✅ **Validaciones:** Coherencia matemática completa
- ✅ **Distractores:** Errores conceptuales sofisticados
- ✅ **Competencias ICFES:** Nivel 2, interpretación y representación
- ✅ **Desafío pedagógico:** Datos desordenados en tabla

### **Diferencias en Experiencia de Usuario:**
- **v1:** Enfoque en comparación visual
- **v2:** Enfoque en análisis detallado

## 🎯 **Conclusiones y Recomendaciones**

### **Recomendación General**
**Usar Versión 2 (opciones individuales)** para la mayoría de casos porque:

1. **Mayor accesibilidad:** Diagramas más claros y legibles
2. **Formato estándar:** Sigue convenciones de exámenes
3. **Mejor experiencia:** Análisis más cómodo para estudiantes
4. **Flexibilidad:** Permite análisis tanto individual como comparativo

### **Casos Específicos para Versión 1**
- Evaluaciones avanzadas donde se busque mayor desafío visual
- Situaciones con limitaciones de espacio o ancho de banda
- Cuando se quiera evaluar específicamente habilidades de comparación visual

### **Implementación Sugerida**
- **Primaria/Secundaria básica:** Versión 2
- **Secundaria avanzada/Universidad:** Versión 1 o 2 según objetivos
- **Evaluaciones formativas:** Versión 2
- **Evaluaciones sumativas:** Versión 1 o 2 según nivel

## 📁 **Archivos Relacionados**

- `estadistica_diagramas_caja_interpretacion_representacion_Nivel2_v1.Rmd` - Versión original
- `estadistica_diagramas_caja_interpretacion_representacion_Nivel2_v2.Rmd` - Nueva versión
- `COMPARACION_VERSIONES_v1_vs_v2.md` - Este documento
- `REPORTE_CONTROL_CALIDAD.md` - Control de calidad aplicado
- `MEJORA_PEDAGOGICA_DESAFIO_ADICIONAL.md` - Documentación pedagógica

---

## 🔧 **Correcciones Implementadas**

### **Problema Inicial Detectado**
- **Síntoma:** Las imágenes no se mostraban correctamente en la versión v2
- **Causa:** Formato incorrecto del Answerlist con imágenes individuales

### **Solución Aplicada**
- **Referencia:** Patrón extraído de `Auxiliares/Ejemplos_Funcionales.md/Ejemplo_01.md`
- **Formato correcto implementado:**
  ```r
  cat("-\n")
  cat("![](imagen.png){width=70%}\n\n")
  ```
- **Resultado:** ✅ Imágenes ahora se muestran correctamente

### **Mejoras Adicionales Implementadas**

#### **Gráfica en Conclusión - Ambas Versiones**
- **v1:** Muestra nuevamente los 4 diagramas combinados en la conclusión
- **v2:** Muestra únicamente el diagrama correcto individual en la conclusión

#### **Código Implementado:**
**Versión 1:**
```r
# Mostrar los cuatro diagramas con énfasis en el correcto
cat("![](diagramas_caja.png){width=95%}")
```

**Versión 2:**
```r
# Mostrar solo el diagrama correcto
letra_correcta <- tolower(LETTERS[indice_correcto])
cat("![](diagrama_", letra_correcta, ".png){width=70%}")
```

### **Problema Crítico Resuelto**
- **Error original:** `File 'diagrama_ d .png' not found` (espacio extra en nombre)
- **Causa:** Interpolación incorrecta de variables en nombres de archivo
- **Solución:** Uso de `paste0()` para construcción segura de nombres
- **Código corregido:**
  ```r
  nombre_archivo <- paste0("diagrama_", letra_correcta, ".png")
  cat("![](", nombre_archivo, "){width=70%}")
  ```

### **Validación Final Completa**
- **Compilación HTML:** ✅ Exitosa
- **Compilación PDF:** ✅ Exitosa (problema resuelto)
- **Compilación Moodle:** ✅ Exitosa
- **Visualización:** ✅ Imágenes individuales visibles
- **Gráfica en solución:** ✅ Diagrama correcto mostrado en conclusión
- **Funcionalidad:** ✅ Completamente operativa

---

**Documento generado:** Junio 2024
**Última actualización:** Junio 2024
**Estado:** ✅ Ambas versiones funcionales y validadas
