# 🚀 INICIO AQUÍ - Guía Rápida de Uso

## 📌 RESUMEN EJECUTIVO

Este directorio contiene un sistema **corregido y verificado** para generar exámenes R-exams con **consistencia garantizada** entre versiones con y sin soluciones.

---

## ⚡ USO RÁPIDO (3 PASOS)

### **PASO 1: Abrir RStudio**
Navegar a este directorio:
```r
setwd("Auxiliares/Examenes_Fin_de_Periodo/04-Examen-Fin-de-Periodo-4")
```

### **PASO 2: Generar Exámenes**
```r
source("SemilleroFinDePeriodo_4.R")
```

### **PASO 3: Verificar Resultados**
Los archivos generados estarán en la carpeta `salida/`:

- ✅ Versión DOCX con soluciones
- ✅ Versión DOCX sin soluciones
- ✅ Versión PDF con soluciones
- ✅ Versión PDF sin soluciones

**TODAS las versiones tendrán los mismos datos numéricos y opciones de respuesta.**

---

## 📚 DOCUMENTACIÓN DISPONIBLE

### **Para Usuarios Nuevos:**
1. **`README_Uso_Correcto.md`** ← Empieza aquí
   - Guía paso a paso completa
   - Instrucciones de configuración
   - Solución a errores comunes

### **Para Verificación:**
2. **`CHECKLIST_Verificacion_Rapida.md`**
   - Lista de verificación rápida
   - Checks antes y después de generar
   - Registro de verificación

### **Para Entender el Problema:**
3. **`SOLUCION_Consistencia_Versiones.md`**
   - Explicación técnica del problema
   - Detalles de la solución implementada
   - Diagrama de flujo de control

### **Para Ver los Cambios:**
4. **`RESUMEN_Cambios_Implementados.md`**
   - Lista completa de cambios
   - Comparación antes/después
   - Impacto de los cambios

---

## 🔧 ARCHIVOS PRINCIPALES

### **Scripts de Generación:**
- `SemilleroFinDePeriodo_4.R` - Script principal (ejecutar este)

### **Ejercicios:**
- `cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd` - Ejercicio de Pitágoras

### **Scripts de Prueba:**
- `TEST_Verificacion_Consistencia.R` - Verificación automatizada (opcional)

---

## ✅ VERIFICACIÓN RÁPIDA

### **¿Cómo saber si funciona correctamente?**

Después de ejecutar `SemilleroFinDePeriodo_4.R`:

1. **Abrir dos archivos** (ej: DOCX con sol y DOCX sin sol)
2. **Comparar el enunciado**: Debe tener el mismo nombre y valores numéricos
3. **Comparar las opciones**: Deben ser exactamente las mismas 4 opciones
4. **Única diferencia**: La versión "sin soluciones" no muestra la sección "Solution"

Si todo coincide → ✅ **Funcionando correctamente**

---

## 🎯 PROBLEMA QUE SE RESOLVIÓ

### **Antes:**
- Las preguntas eran DIFERENTES entre versiones con y sin soluciones
- Las opciones de respuesta eran DIFERENTES
- Imposible comparar versiones o crear claves de respuesta consistentes

### **Ahora:**
- ✅ Todas las versiones tienen los MISMOS datos
- ✅ Todas las versiones tienen las MISMAS opciones
- ✅ Solo difieren en mostrar o no la solución

---

## 🔄 PARA GENERAR VERSIÓN DIFERENTE

Si necesitas generar un examen completamente diferente:

1. **Editar `SemilleroFinDePeriodo_4.R`**, línea 14:
   ```r
   semilla <- 654321  # Cambiar este número
   ```

2. **Ejecutar nuevamente:**
   ```r
   source("SemilleroFinDePeriodo_4.R")
   ```

3. **Resultado:** Nueva versión con datos diferentes, pero consistente entre formatos

---

## ⚠️ IMPORTANTE: NO HACER

❌ **NO modificar el archivo .Rmd** para agregar `set.seed()`  
❌ **NO comentar las líneas `set.seed(semilla)`** en el script R  
❌ **NO usar `Sys.time()`** para generar semillas  

✅ **SÍ usar el script R** tal como está configurado  
✅ **SÍ cambiar la semilla** en el script R si necesitas versión diferente  
✅ **SÍ verificar consistencia** después de generar  

---

## 🆘 SOPORTE

### **Si algo no funciona:**

1. **Consultar:** `CHECKLIST_Verificacion_Rapida.md`
2. **Ejecutar:** `source("TEST_Verificacion_Consistencia.R")`
3. **Revisar:** Mensajes de error en la consola de R

### **Errores comunes:**

**Error: "Versiones diferentes"**
→ Verificar que `set.seed(semilla)` esté descomentado en el script R

**Error: "No se generan archivos"**
→ Verificar que existe la carpeta `salida/` y los templates .tex

**Error: "Pruebas fallan"**
→ Instalar librerías: `install.packages(c("exams", "digest"))`

---

## 📊 ESTRUCTURA DE ARCHIVOS

```
04-Examen-Fin-de-Periodo-4/
│
├── 00-INICIO_AQUI.md                    ← Estás aquí
├── README_Uso_Correcto.md               ← Guía completa
├── CHECKLIST_Verificacion_Rapida.md     ← Verificación
├── SOLUCION_Consistencia_Versiones.md   ← Detalles técnicos
├── RESUMEN_Cambios_Implementados.md     ← Cambios realizados
│
├── SemilleroFinDePeriodo_4.R            ← EJECUTAR ESTE
├── cateto_teorema_pitagoras_...Rmd      ← Ejercicio
├── TEST_Verificacion_Consistencia.R     ← Pruebas (opcional)
│
├── pcielo.tex                           ← Templates
├── pcielo_nosol.tex
├── solpcielo.tex
├── exam.tex
│
└── salida/                              ← Archivos generados
    ├── ...docx (con soluciones)
    ├── ...sin_sol.docx
    ├── ...sol.pdf
    └── ...sin_sol.pdf
```

---

## 🎓 PRÓXIMOS PASOS

1. ✅ **Leer:** `README_Uso_Correcto.md` para instrucciones detalladas
2. ✅ **Ejecutar:** `source("SemilleroFinDePeriodo_4.R")`
3. ✅ **Verificar:** Usar `CHECKLIST_Verificacion_Rapida.md`
4. ✅ **Opcional:** Ejecutar `TEST_Verificacion_Consistencia.R`

---

**Estado:** ✅ Sistema Operativo y Verificado  
**Versión:** 1.0  
**Fecha:** 2025-11-04  
**Compatibilidad:** R-exams, RStudio, Filosofía ICFES 2025

