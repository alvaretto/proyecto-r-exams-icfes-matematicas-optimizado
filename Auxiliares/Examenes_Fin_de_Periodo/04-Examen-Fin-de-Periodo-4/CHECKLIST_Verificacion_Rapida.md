# ✅ CHECKLIST DE VERIFICACIÓN RÁPIDA

## 🎯 USO RÁPIDO

Usa este checklist para verificar que la solución de consistencia está funcionando correctamente.

---

## 📋 ANTES DE GENERAR EXÁMENES

### **1. Verificar Configuración del Script R**

Abrir `SemilleroFinDePeriodo_4.R` y verificar:

- [ ] Línea 14: `semilla <- 123456` (o cualquier número fijo)
- [ ] Línea 15: `set.seed(semilla)` está **descomentado**
- [ ] Línea 28: `set.seed(semilla)` está **descomentado** (antes de exams2pandoc con sol)
- [ ] Línea 55: `set.seed(semilla)` está **descomentado** (antes de exams2pandoc sin sol)
- [ ] Línea 84: `set.seed(semilla)` está **descomentado** (antes de exams2pdf con sol)
- [ ] Línea 98: `set.seed(semilla)` está **descomentado** (antes de exams2pdf sin sol)

### **2. Verificar Configuración del Archivo .Rmd**

Abrir `cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd` y verificar:

- [ ] Líneas 50-53: Comentario indica "NO establecer set.seed() aquí"
- [ ] Líneas 60-62: NO hay código `timestamp_seed <- as.numeric(Sys.time())`
- [ ] Línea 65: Existe `base_seed <- sample(1:1000000, 1)`
- [ ] Línea 131: Usa `base_seed` (NO `unique_seed`)

---

## 📊 DESPUÉS DE GENERAR EXÁMENES

### **3. Verificar Archivos Generados**

En la carpeta `salida/`, verificar que existen:

- [ ] `Matematicas_Evaluacion_Fin_de_Periodo_4-docx1.docx` (con soluciones)
- [ ] `Matematicas_Evaluacion_Fin_de_Periodo_4_sin_sol1.docx` (sin soluciones)
- [ ] `Matematicas_Evaluacion_Fin_de_Periodo_4_sol1.pdf` (con soluciones)
- [ ] `Matematicas_Evaluacion_Fin_de_Periodo_4_sin_sol1.pdf` (sin soluciones)

### **4. Verificación Manual de Consistencia**

Abrir **DOS archivos del mismo formato** (ej: DOCX con sol y DOCX sin sol):

#### **Verificar Enunciado:**
- [ ] Mismo nombre de personaje (ej: "Margarita", "Carlos", etc.)
- [ ] Mismo diagrama TikZ con mismos valores numéricos
- [ ] Mismo texto en Paso 1
- [ ] Mismo texto en Paso 2

#### **Verificar Opciones:**
- [ ] Opción A es idéntica en ambos archivos
- [ ] Opción B es idéntica en ambos archivos
- [ ] Opción C es idéntica en ambos archivos
- [ ] Opción D es idéntica en ambos archivos

#### **Verificar Diferencia Esperada:**
- [ ] Versión "sin soluciones" NO muestra sección "Solution"
- [ ] Versión "con soluciones" SÍ muestra sección "Solution"

---

## 🔬 VERIFICACIÓN AUTOMATIZADA (OPCIONAL)

### **5. Ejecutar Script de Prueba**

En RStudio:

```r
source("TEST_Verificacion_Consistencia.R")
```

Verificar resultado:

- [ ] Prueba 1: ✅ ÉXITO
- [ ] Prueba 2: ✅ ÉXITO
- [ ] Prueba 3: ✅ ÉXITO
- [ ] Mensaje final: "TODAS LAS PRUEBAS PASARON"

---

## ⚠️ SI ALGO FALLA

### **Problema: Versiones tienen datos diferentes**

**Verificar:**
1. [ ] ¿El script R tiene `set.seed(semilla)` descomentado?
2. [ ] ¿El archivo .Rmd NO tiene `set.seed()` en el chunk data_generation?
3. [ ] ¿El archivo .Rmd NO usa `Sys.time()` para generar semillas?

**Solución:**
- Revisar `SOLUCION_Consistencia_Versiones.md` sección "Cambios Implementados"
- Comparar con los archivos originales corregidos

### **Problema: Script de prueba falla**

**Verificar:**
1. [ ] ¿El archivo .Rmd está en el mismo directorio que el script de prueba?
2. [ ] ¿La librería `exams` está instalada?
3. [ ] ¿La librería `digest` está instalada?

**Solución:**
```r
install.packages("exams")
install.packages("digest")
```

### **Problema: No se generan archivos de salida**

**Verificar:**
1. [ ] ¿Existe la carpeta `salida/`?
2. [ ] ¿Los templates .tex están en el directorio?
3. [ ] ¿Hay errores en la consola de R?

**Solución:**
- Crear carpeta `salida/` manualmente
- Verificar que existen: `pcielo.tex`, `pcielo_nosol.tex`, `solpcielo.tex`, `exam.tex`
- Revisar errores en consola y corregir

---

## 📝 REGISTRO DE VERIFICACIÓN

**Fecha:** _______________

**Verificado por:** _______________

**Semilla usada:** _______________

**Resultados:**

- [ ] ✅ Todos los checks pasaron
- [ ] ⚠️  Algunos checks fallaron (especificar abajo)
- [ ] ❌ Verificación no completada

**Notas adicionales:**

```
_________________________________________________________________

_________________________________________________________________

_________________________________________________________________
```

---

## 🎓 REFERENCIA RÁPIDA

### **Archivos de Documentación:**
- `README_Uso_Correcto.md` - Guía de uso completa
- `SOLUCION_Consistencia_Versiones.md` - Explicación técnica detallada
- `RESUMEN_Cambios_Implementados.md` - Resumen de cambios

### **Archivos de Código:**
- `SemilleroFinDePeriodo_4.R` - Script principal de generación
- `cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd` - Ejercicio
- `TEST_Verificacion_Consistencia.R` - Script de pruebas

### **Comando Rápido:**
```r
# Generar exámenes
source("SemilleroFinDePeriodo_4.R")

# Verificar consistencia
source("TEST_Verificacion_Consistencia.R")
```

---

**Versión del Checklist:** 1.0  
**Última actualización:** 2025-11-04

