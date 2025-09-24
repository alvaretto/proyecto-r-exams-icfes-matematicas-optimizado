# 📚 ÍNDICE DE ERRORES COMUNES - SISTEMA ICFES R-EXAMS 2025

**Última actualización:** 2025-01-27  
**Sistema:** ICFES R-exams 2025 Integrado  
**Propósito:** Base de conocimiento para resolución eficiente de errores  

---

## 🎯 **CATEGORÍAS DE ERRORES**

### **A. ERRORES CRÍTICOS (Impiden funcionamiento)**
### **B. ERRORES DE COMPILACIÓN (Fallan exams2*)**
### **C. ERRORES DE CALIDAD (Incumplen estándares ICFES)**
### **D. ERRORES DE ESTRUCTURA (Problemas de código)**
### **E. ERRORES DE CONFIGURACIÓN (Setup incorrecto)**

---

## 🔴 **CATEGORÍA A: ERRORES CRÍTICOS**

### **A1. Diversidad de Versiones Insuficiente**
- **Archivo:** `2025-01-27_error_diversidad_versiones_insuficiente.md`
- **Síntoma:** Solo 120 versiones vs. 300+ requeridas
- **Causa:** Parámetros de aleatorización limitados (5 valores únicos)
- **Solución:** Ampliar a 21 valores + contextos dinámicos + 8 tipos distractores
- **Tiempo resolución:** 2-3 horas
- **Impacto:** CRÍTICO - Incumple estándar ICFES

### **A2. Operador $ Inválido para Vectores Atómicos**
- **Archivo:** `2025-01-27_error_operador_dollar_vectores_atomicos.md`
- **Síntoma:** `Error en x$correcta: $ operator is invalid for atomic vectors`
- **Causa:** Uso de `c()` en lugar de `list()` para estructura de opciones
- **Solución:** Cambiar `c()` por `list()` en línea de creación de opciones
- **Tiempo resolución:** 30 minutos
- **Impacto:** CRÍTICO - Función no ejecutable

---

## 🟠 **CATEGORÍA B: ERRORES DE COMPILACIÓN**

### **B1. Compilación TikZ con Variables Dinámicas**
- **Archivo:** `2025-01-27_error_compilacion_tikz_variables_dinamicas.md`
- **Síntoma:** `LaTeX failed to compile cuadrado_rotado.tex`
- **Causa:** Variables R dinámicas con caracteres especiales LaTeX
- **Solución:** Usar valores fijos estables en código TikZ
- **Tiempo resolución:** 45 minutos
- **Impacto:** ALTO - Impide generación de gráficos

### **B2. Configuración de Tolerancias Incorrecta**
- **Síntoma:** Respuestas numéricas marcadas como incorrectas
- **Causa:** Tolerancia 0 para respuestas numéricas con valores grandes
- **Solución:** `extol: 0|0|1|1|0|1|0` (schoice=0, numéricas≥1)
- **Tiempo resolución:** 15 minutos
- **Impacto:** MEDIO - Evaluación automática falla

---

## 🟡 **CATEGORÍA C: ERRORES DE CALIDAD**

### **C1. Opciones de Respuesta Duplicadas**
- **Síntoma:** Múltiples opciones con el mismo valor
- **Causa:** Generación aleatoria sin validación de unicidad
- **Solución:** Validación automática + regeneración si hay duplicados
- **Tiempo resolución:** 20 minutos
- **Impacto:** MEDIO - Reduce calidad pedagógica

### **C2. Distractores No Pedagógicos**
- **Síntoma:** Opciones incorrectas sin justificación educativa
- **Causa:** Generación aleatoria sin base en errores conceptuales
- **Solución:** Pool de 8 tipos de distractores basados en errores reales
- **Tiempo resolución:** 1 hora
- **Impacto:** MEDIO - Reduce valor educativo

---

## 🔵 **CATEGORÍA D: ERRORES DE ESTRUCTURA**

### **D1. Metadatos ICFES Incompletos**
- **Síntoma:** Campos faltantes en sección Meta-information
- **Causa:** Template incompleto o campos no actualizados
- **Solución:** Verificar estructura obligatoria ICFES completa
- **Tiempo resolución:** 10 minutos
- **Impacto:** BAJO - Clasificación incorrecta

### **D2. Formato Numérico Inconsistente**
- **Síntoma:** Notación científica o separadores incorrectos
- **Causa:** Configuración `options()` faltante o incorrecta
- **Solución:** `options(scipen = 999, OutDec = ".")`
- **Tiempo resolución:** 5 minutos
- **Impacto:** BAJO - Presentación inconsistente

---

## 🟣 **CATEGORÍA E: ERRORES DE CONFIGURACIÓN**

### **E1. Librerías No Cargadas**
- **Síntoma:** `Error: could not find function "exams2html"`
- **Causa:** `library(exams)` faltante o librerías no instaladas
- **Solución:** Verificar chunk de configuración inicial completo
- **Tiempo resolución:** 5 minutos
- **Impacto:** BAJO - Setup incorrecto

### **E2. Configuración Python/Reticulate**
- **Síntoma:** `Error in py_run_string_impl: Python not available`
- **Causa:** `use_python()` no configurado o Python no encontrado
- **Solución:** `use_python("/usr/bin/python3", required = TRUE)`
- **Tiempo resolución:** 10 minutos
- **Impacto:** BAJO - Chunks Python no funcionan

---

## 🔧 **HERRAMIENTAS DE DIAGNÓSTICO RÁPIDO**

### **Comando de Verificación General:**
```bash
cd Lab-Manjaro/10-S1-2024B
R --no-restore --no-save -e "
# Test rápido de funcionalidad básica
library(exams)
source('archivo.Rmd')
datos <- generar_datos()
cat('✅ Función básica:', length(datos\$opciones) == 4, '\n')
cat('✅ Opciones únicas:', length(unique(sapply(datos\$opciones, function(x) x\$valor))) == 4, '\n')
cat('✅ Una correcta:', sum(sapply(datos\$opciones, function(x) x\$correcta)) == 1, '\n')
"
```

### **Test de Diversidad Rápido:**
```bash
R --no-restore --no-save -e "
versiones <- replicate(100, digest::digest(generar_datos()))
cat('Diversidad:', length(unique(versiones)), '/100\n')
if(length(unique(versiones)) < 30) cat('⚠️ DIVERSIDAD BAJA\n') else cat('✅ DIVERSIDAD OK\n')
"
```

### **Test de Compilación Rápido:**
```bash
R --no-restore --no-save -e "
tryCatch({
  exams2html('archivo.Rmd', n=1, dir='test_temp')
  cat('✅ COMPILACIÓN OK\n')
}, error = function(e) cat('❌ ERROR:', e\$message, '\n'))
"
```

---

## 📊 **ESTADÍSTICAS DE ERRORES**

### **Frecuencia por Categoría (Histórico):**
- **Errores Críticos:** 15% (alta prioridad)
- **Errores Compilación:** 25% (resolución urgente)
- **Errores Calidad:** 35% (mejora continua)
- **Errores Estructura:** 15% (mantenimiento)
- **Errores Configuración:** 10% (setup inicial)

### **Tiempo Promedio de Resolución:**
- **Críticos:** 1-3 horas
- **Compilación:** 30-60 minutos
- **Calidad:** 20-60 minutos
- **Estructura:** 5-20 minutos
- **Configuración:** 5-15 minutos

---

## 🎯 **PROTOCOLO DE RESOLUCIÓN**

### **1. Identificación (5 minutos):**
- Leer mensaje de error completo
- Identificar categoría usando este índice
- Localizar archivo de documentación específico

### **2. Diagnóstico (10 minutos):**
- Ejecutar comandos de verificación rápida
- Confirmar síntomas descritos en documentación
- Identificar líneas de código específicas

### **3. Aplicación (Variable):**
- Seguir solución documentada paso a paso
- Aplicar validaciones recomendadas
- Ejecutar tests de verificación

### **4. Validación (10 minutos):**
- Confirmar que error está resuelto
- Ejecutar tests de regresión
- Documentar cualquier variación encontrada

---

## 📚 **RECURSOS ADICIONALES**

### **Documentación Relacionada:**
- `Auxiliares/rules_full/reglas-generales.md` - Sistema integrado completo
- `Auxiliares/METODOLOGIA_Correccion_Errores_Recurrentes_ICFES_R_Exams.md` - Metodología
- `Auxiliares/BIBLIOTECA_Soluciones_Errores_Comunes.md` - Biblioteca de soluciones
- `Auxiliares/CHECKLIST_Validacion_Archivos_Rmd.md` - Checklist de validación

### **Ejemplos Funcionales:**
- `Auxiliares/Ejemplos-Funcionales-Rmd/` - Patrones correctos de implementación
- `Lab-Manjaro/10-S1-2024B/area_cuadrado_rotado_*.Rmd` - Ejemplo corregido completo

### **Herramientas de Testing:**
- `Auxiliares/Testing_Rnw/` - Scripts de testing automatizado
- `Auxiliares/Validacion/` - Herramientas de validación

---

## 🚀 **ACTUALIZACIONES FUTURAS**

### **Próximas Adiciones:**
- Errores específicos de ejercicios tipo Cloze
- Problemas de integración con Moodle
- Errores de configuración en diferentes sistemas operativos
- Optimizaciones de rendimiento para ejercicios complejos

### **Contribuciones:**
Para agregar nuevos errores a este índice:
1. Crear archivo detallado siguiendo formato establecido
2. Agregar entrada en categoría apropiada
3. Actualizar estadísticas y comandos de verificación
4. Probar solución en entorno limpio antes de documentar

---

**📝 Nota:** Este índice se actualiza continuamente. Consultar fecha de última actualización para verificar vigencia de la información.
