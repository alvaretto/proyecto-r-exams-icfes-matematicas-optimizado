# 🔧 INSTRUCCIONES DE DIAGNÓSTICO - Generación de Exámenes

## 📋 SITUACIÓN ACTUAL

**Problema reportado:** Solo se generaron archivos DOCX, pero NO se generaron archivos PDF ni NOPS.

**Archivos generados exitosamente:**
- ✅ `Evaluacion_Fin_de_Periodo_4-docx1.docx` (con soluciones)
- ✅ `Evaluacion_Fin_de_Periodo_4_sin_sol1.docx` (sin soluciones)

**Archivos faltantes:**
- ❌ PDF con soluciones
- ❌ PDF sin soluciones
- ❌ NOPS con soluciones (15 archivos)
- ❌ NOPS sin soluciones (15 archivos)

---

## 🚀 SOLUCIÓN RÁPIDA - 3 PASOS

### **PASO 1: Ejecutar prueba rápida de formatos**

Esto identificará exactamente qué formatos funcionan y cuáles fallan:

```r
source("05-test_formatos.R")
```

**Resultado esperado:**
- Verás un resumen de 6 pruebas (DOCX, PDF, NOPS)
- Cada prueba mostrará ✓ ÉXITO o ✗ ERROR
- Se generará un archivo `resultados_prueba_formatos.txt` con los detalles

**Tiempo estimado:** 1-2 minutos

---

### **PASO 2: Ejecutar script con logs detallados**

Si la prueba rápida muestra errores, ejecuta el script completo con logs:

```r
source("03-SemilleroFinDePeriodo_v4_con_logs.R")
```

**Resultado esperado:**
- Se generará un archivo `log_generacion_examenes.txt`
- El log contendrá información detallada de cada error
- Podrás ver exactamente en qué punto falló cada formato

**Tiempo estimado:** 5-10 minutos (dependiendo del número de ejercicios)

---

### **PASO 3: Revisar logs y aplicar solución**

```r
# Leer el log generado
file.show("log_generacion_examenes.txt")

# O desde terminal:
# cat log_generacion_examenes.txt
```

**Buscar líneas con `[ERROR]`** y aplicar la solución correspondiente según el error.

---

## 🔍 ERRORES COMUNES Y SOLUCIONES RÁPIDAS

### **ERROR 1: "pdflatex not found" o similar**

**Causa:** LaTeX no está instalado.

**Solución:**
```bash
# En Manjaro/Arch Linux:
sudo pacman -S texlive-core texlive-latexextra texlive-fontsextra texlive-pictures

# Verificar instalación:
pdflatex --version
```

---

### **ERROR 2: "template not found" o "solpcielo.tex not found"**

**Causa:** Templates LaTeX faltantes.

**Solución:**
```r
# Verificar que existen los templates
file.exists("solpcielo.tex")  # Debe retornar TRUE
file.exists("exam.tex")        # Debe retornar TRUE

# Si retornan FALSE, copiar desde otro directorio o crear templates
```

---

### **ERROR 3: "only schoice questions allowed" (para NOPS)**

**Causa:** exams2nops solo funciona con preguntas de selección múltiple.

**Solución:**
- Verificar que todos los ejercicios seleccionados sean tipo `schoice`
- Excluir ejercicios tipo `cloze` o `num` del examen NOPS
- O generar solo formatos PDF y DOCX (que sí soportan todos los tipos)

---

### **ERROR 4: Error en ejercicio .Rmd específico**

**Causa:** Uno o más ejercicios tienen errores de sintaxis LaTeX/TikZ.

**Solución:**
```r
# Identificar qué ejercicio causa el error (revisar el log)
# Probar compilar ese ejercicio individualmente:
library(exams)
exams2pdf("NOMBRE_EJERCICIO_PROBLEMATICO.Rmd",
          n = 1,
          name = "test",
          dir = "salida",
          verbose = TRUE)

# Corregir el error en ese ejercicio
# O temporalmente excluirlo del examen
```

---

## 📊 INTERPRETACIÓN DE RESULTADOS

### **Si la prueba rápida muestra:**

#### **✓ Todos los formatos: ÉXITO**
```
¡Perfecto! Todos los formatos funcionan.
Ejecutar: source("SemilleroFinDePeriodo_v4.R")
```

#### **✓ DOCX: ÉXITO | ✗ PDF: ERROR**
```
Problema: LaTeX no instalado o mal configurado
Solución: Instalar LaTeX (ver ERROR 1 arriba)
```

#### **✓ DOCX y PDF: ÉXITO | ✗ NOPS: ERROR**
```
Problema: Ejercicios no compatibles con NOPS
Solución: Verificar que sean tipo schoice (ver ERROR 3 arriba)
```

#### **✗ Todos los formatos: ERROR**
```
Problema: Error en ejercicio .Rmd
Solución: Revisar log detallado (ver ERROR 4 arriba)
```

---

## 📁 ARCHIVOS CREADOS PARA DIAGNÓSTICO

| Archivo | Propósito | Cuándo usar |
|---------|-----------|-------------|
| `05-test_formatos.R` | Prueba rápida de cada formato | **PRIMERO** - Identificar qué falla |
| `03-SemilleroFinDePeriodo_v4_con_logs.R` | Script completo con logs | **SEGUNDO** - Ver detalles de errores |
| `04-DIAGNOSTICO_Y_SOLUCIONES.md` | Guía completa de soluciones | **TERCERO** - Aplicar soluciones |
| `log_generacion_examenes.txt` | Log detallado (generado) | Revisar errores específicos |
| `resultados_prueba_formatos.txt` | Resumen de pruebas (generado) | Ver qué formatos funcionan |

---

## 🎯 FLUJO DE TRABAJO RECOMENDADO

```
1. Ejecutar: source("05-test_formatos.R")
   ↓
2. ¿Todos los formatos funcionan?
   ├─ SÍ → Ejecutar: source("SemilleroFinDePeriodo_v4.R")
   └─ NO → Continuar al paso 3
   ↓
3. Ejecutar: source("03-SemilleroFinDePeriodo_v4_con_logs.R")
   ↓
4. Revisar: file.show("log_generacion_examenes.txt")
   ↓
5. Identificar error específico
   ↓
6. Aplicar solución según error (ver sección "ERRORES COMUNES")
   ↓
7. Volver al paso 1 para verificar
```

---

## 🛠️ COMANDOS ÚTILES DE VERIFICACIÓN

### **Verificar instalación de LaTeX:**
```bash
pdflatex --version
which pdflatex
```

### **Verificar templates disponibles:**
```r
file.exists("solpcielo.tex")
file.exists("exam.tex")
file.exists("pcielo.tex")
file.exists("pcielo_nosol.tex")
```

### **Verificar paquete exams:**
```r
library(exams)
packageVersion("exams")
```

### **Listar ejercicios disponibles:**
```r
list.files(pattern = "^[0-9]{3}-.*\\.Rmd$")
```

### **Verificar espacio en disco:**
```bash
df -h .
```

---

## 📞 INFORMACIÓN PARA SOPORTE

Si después de seguir estos pasos aún hay problemas, proporciona:

1. **Contenido del log:**
   ```r
   cat(readLines("log_generacion_examenes.txt"), sep = "\n")
   ```

2. **Resultados de la prueba rápida:**
   ```r
   cat(readLines("resultados_prueba_formatos.txt"), sep = "\n")
   ```

3. **Información del sistema:**
   ```r
   R.version.string
   packageVersion("exams")
   Sys.info()["sysname"]
   ```

4. **Verificación de LaTeX:**
   ```bash
   pdflatex --version
   ```

---

## ✅ CHECKLIST ANTES DE EJECUTAR

Antes de ejecutar el script completo, verificar:

```
□ LaTeX instalado (pdflatex --version)
□ Templates LaTeX disponibles (file.exists("solpcielo.tex"))
□ Paquete exams instalado (library(exams))
□ Directorio salida/ existe
□ Al menos 15 ejercicios .Rmd disponibles
□ Suficiente espacio en disco (df -h)
```

---

## 🚀 INICIO RÁPIDO

**Si tienes prisa, ejecuta esto:**

```r
# Prueba rápida (1-2 minutos)
source("05-test_formatos.R")

# Si todo funciona, ejecutar script completo
source("SemilleroFinDePeriodo_v4.R")

# Si algo falla, ejecutar con logs
source("03-SemilleroFinDePeriodo_v4_con_logs.R")

# Revisar log
file.show("log_generacion_examenes.txt")
```

---

## 📚 DOCUMENTACIÓN ADICIONAL

- **Guía completa de diagnóstico:** `04-DIAGNOSTICO_Y_SOLUCIONES.md`
- **Resumen de modificaciones:** `01-RESUMEN_MODIFICACIONES_SemilleroFinDePeriodo_v4.md`
- **Explicación de formatos:** `02-EXPLICACION_FORMATOS_Y_VERSIONES.md`

---

## 💡 CONSEJO FINAL

**La causa más común de que solo se generen DOCX es que LaTeX no está instalado o configurado correctamente.**

**Solución rápida:**
```bash
sudo pacman -S texlive-core texlive-latexextra texlive-fontsextra texlive-pictures
```

Luego ejecutar:
```r
source("05-test_formatos.R")
```

Si después de instalar LaTeX todos los formatos muestran ✓ ÉXITO, entonces ejecutar:
```r
source("SemilleroFinDePeriodo_v4.R")
```

---

**¡Buena suerte!** 🎯

