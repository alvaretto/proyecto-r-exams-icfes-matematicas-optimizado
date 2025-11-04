# EXPLICACIÓN: FORMATOS Y VERSIONES - SemilleroFinDePeriodo_v4.R

## 🎯 CONCEPTO CLAVE: ARCHIVOS vs VERSIONES

### **¿Qué significa "15 versiones"?**

Cuando el script genera **15 versiones**, significa que crea **15 exámenes diferentes** con:
- Las mismas 15 preguntas seleccionadas aleatoriamente
- Diferentes valores aleatorios en cada pregunta (números, contextos, distractores)
- Orden de opciones mezclado aleatoriamente

### **¿Cómo se organizan estas versiones?**

Depende del formato de salida:

---

## 📊 FORMATOS CONSOLIDADOS (1 archivo = 15 versiones)

### **Formatos: DOCX y PDF (exams2pandoc y exams2pdf)**

Estos formatos generan **1 archivo único** que contiene **15 versiones secuenciales**:

```
Evaluacion_Fin_de_Periodo_4-docx1.docx
├── Versión 1 (páginas 1-5)
├── Versión 2 (páginas 6-10)
├── Versión 3 (páginas 11-15)
├── ...
└── Versión 15 (páginas 71-75)
```

**Características:**
- ✅ **1 archivo consolidado** por formato
- ✅ **15 versiones** dentro del mismo archivo
- ✅ Separadas por saltos de página
- ✅ Ideal para imprimir todas las versiones de una vez
- ✅ Fácil distribución: un solo archivo para compartir

**Archivos generados:**
1. `Evaluacion_Fin_de_Periodo_4-docx1.docx` → 15 versiones con soluciones
2. `Evaluacion_Fin_de_Periodo_4_sin_sol1.docx` → 15 versiones sin soluciones
3. `Evaluacion_Fin_de_Periodo_4_sol1.pdf` → 15 versiones con soluciones
4. `Evaluacion_Fin_de_Periodo_41.pdf` → 15 versiones sin soluciones

**Total:** 4 archivos consolidados

---

## 📄 FORMATOS INDIVIDUALES (15 archivos = 15 versiones)

### **Formato: NOPS (exams2nops)**

Este formato genera **15 archivos separados**, uno por cada versión:

```
Evaluacion_Fin_de_Periodo_4_nops1.pdf       → Versión 1
Evaluacion_Fin_de_Periodo_4_nops2.pdf       → Versión 2
Evaluacion_Fin_de_Periodo_4_nops3.pdf       → Versión 3
...
Evaluacion_Fin_de_Periodo_4_nops15.pdf      → Versión 15
```

**Características:**
- ✅ **15 archivos individuales** (con soluciones)
- ✅ **15 archivos individuales** (sin soluciones)
- ✅ Cada archivo = 1 versión única
- ✅ Incluye hoja de respuestas escaneable
- ✅ Código de barras único por versión
- ✅ Ideal para corrección automática
- ✅ Distribución individual: un archivo por estudiante

**Archivos generados:**
1. `Evaluacion_Fin_de_Periodo_4_nops_sol1.pdf` a `..._sol15.pdf` → 15 archivos con soluciones
2. `Evaluacion_Fin_de_Periodo_4_nops1.pdf` a `..._nops15.pdf` → 15 archivos sin soluciones

**Total:** 30 archivos individuales (15 + 15)

---

## 📁 RESUMEN DE ARCHIVOS GENERADOS

### **Total de archivos: 34**

```
salida/
│
├── FORMATOS CONSOLIDADOS (4 archivos)
│   ├── Evaluacion_Fin_de_Periodo_4-docx1.docx          [15 versiones internas]
│   ├── Evaluacion_Fin_de_Periodo_4_sin_sol1.docx       [15 versiones internas]
│   ├── Evaluacion_Fin_de_Periodo_4_sol1.pdf            [15 versiones internas]
│   └── Evaluacion_Fin_de_Periodo_41.pdf                [15 versiones internas]
│
└── FORMATOS INDIVIDUALES (30 archivos)
    │
    ├── CON SOLUCIONES (15 archivos)
    │   ├── Evaluacion_Fin_de_Periodo_4_nops_sol1.pdf
    │   ├── Evaluacion_Fin_de_Periodo_4_nops_sol2.pdf
    │   ├── ...
    │   └── Evaluacion_Fin_de_Periodo_4_nops_sol15.pdf
    │
    └── SIN SOLUCIONES (15 archivos)
        ├── Evaluacion_Fin_de_Periodo_4_nops1.pdf
        ├── Evaluacion_Fin_de_Periodo_4_nops2.pdf
        ├── ...
        └── Evaluacion_Fin_de_Periodo_4_nops15.pdf
```

---

## 🔍 COMPARACIÓN DETALLADA

| Aspecto | DOCX/PDF (consolidado) | NOPS (individual) |
|---------|------------------------|-------------------|
| **Archivos generados** | 1 archivo por formato | 15 archivos por formato |
| **Versiones por archivo** | 15 versiones | 1 versión |
| **Total de archivos** | 4 archivos | 30 archivos |
| **Hoja de respuestas** | No incluida | Incluida y escaneable |
| **Código de barras** | No | Sí, único por versión |
| **Corrección automática** | No | Sí, mediante escaneo |
| **Ideal para** | Imprimir todas las versiones juntas | Distribución individual |
| **Uso típico** | Archivo maestro, backup | Aplicación en aula |

---

## 💡 CASOS DE USO

### **Caso 1: Imprimir todas las versiones para un grupo**

**Solución:** Usar archivos consolidados (DOCX o PDF)

```
1. Abrir: Evaluacion_Fin_de_Periodo_41.pdf
2. Imprimir: Todas las páginas (contiene 15 versiones)
3. Distribuir: Una versión por estudiante
```

**Ventajas:**
- Un solo archivo para manejar
- Impresión rápida de todas las versiones
- Fácil de compartir con colegas

---

### **Caso 2: Distribuir versiones individuales digitalmente**

**Solución:** Usar archivos NOPS individuales

```
1. Seleccionar: Evaluacion_Fin_de_Periodo_4_nops1.pdf
2. Enviar: A estudiante 1
3. Seleccionar: Evaluacion_Fin_de_Periodo_4_nops2.pdf
4. Enviar: A estudiante 2
...
```

**Ventajas:**
- Cada estudiante recibe solo su versión
- No puede ver otras versiones
- Control individual de distribución

---

### **Caso 3: Corrección automática con escáner**

**Solución:** Usar archivos NOPS sin soluciones

```
1. Imprimir: Evaluacion_Fin_de_Periodo_4_nops1.pdf a nops15.pdf
2. Distribuir: Una versión por estudiante
3. Aplicar: Examen en aula
4. Recolectar: Hojas de respuesta
5. Escanear: Con software compatible con NOPS
6. Obtener: Calificaciones automáticas
```

**Ventajas:**
- Corrección automática mediante escaneo
- Código de barras identifica la versión
- Ahorro de tiempo en corrección
- Resultados inmediatos

---

### **Caso 4: Revisar soluciones de todas las versiones**

**Solución:** Usar archivos consolidados con soluciones

```
1. Abrir: Evaluacion_Fin_de_Periodo_4_sol1.pdf
2. Revisar: Todas las versiones y sus soluciones
3. Verificar: Coherencia entre versiones
```

**Ventajas:**
- Todas las soluciones en un solo archivo
- Fácil comparación entre versiones
- Verificación de calidad

---

## 🎯 CONFIGURACIÓN DE VERSIONES

### **¿Cómo cambiar el número de versiones?**

Editar línea 59 del script:

```r
# Generar 15 versiones únicas del examen
copias <- 15  # Cambiar este número
```

**Ejemplos:**

```r
copias <- 5   # Genera 5 versiones
copias <- 10  # Genera 10 versiones
copias <- 30  # Genera 30 versiones
```

**Impacto:**

| Versiones | Archivos consolidados | Archivos NOPS | Total archivos |
|-----------|----------------------|---------------|----------------|
| 5         | 4                    | 10            | 14             |
| 10        | 4                    | 20            | 24             |
| 15        | 4                    | 30            | 34             |
| 30        | 4                    | 60            | 64             |

---

## ⚙️ FUNCIONAMIENTO TÉCNICO

### **¿Cómo se garantiza la diversidad entre versiones?**

```r
# 1. Semilla aleatoria única por ejecución
semilla <- sample(100:1e8, 1)

# 2. Configurar semilla antes de cada formato
set.seed(semilla)

# 3. Generar n versiones (copias)
exams2pdf(..., n = copias)  # copias = 15

# 4. Cada .Rmd genera valores aleatorios internamente
# Resultado: 15 versiones con diferentes valores
```

### **¿Por qué usar la misma semilla en todos los formatos?**

```r
# Garantiza que la Versión 1 sea idéntica en todos los formatos:
set.seed(semilla)
exams2pdf(...)      # Versión 1 en PDF

set.seed(semilla)
exams2pandoc(...)   # Versión 1 en DOCX (idéntica a PDF)

set.seed(semilla)
exams2nops(...)     # Versión 1 en NOPS (idéntica a PDF y DOCX)
```

**Beneficio:** Consistencia entre formatos para la misma versión

---

## 📋 VERIFICACIÓN DE SALIDA

### **Checklist después de ejecutar el script:**

```
✅ Verificar que existen 4 archivos consolidados:
   - Evaluacion_Fin_de_Periodo_4-docx1.docx
   - Evaluacion_Fin_de_Periodo_4_sin_sol1.docx
   - Evaluacion_Fin_de_Periodo_4_sol1.pdf
   - Evaluacion_Fin_de_Periodo_41.pdf

✅ Verificar que existen 30 archivos NOPS:
   - 15 archivos con soluciones (_nops_sol1.pdf a _nops_sol15.pdf)
   - 15 archivos sin soluciones (_nops1.pdf a _nops15.pdf)

✅ Abrir un archivo consolidado y verificar:
   - Contiene múltiples versiones separadas por saltos de página
   - Cada versión tiene 15 preguntas
   - Las preguntas tienen valores diferentes entre versiones

✅ Abrir un archivo NOPS y verificar:
   - Contiene solo 1 versión
   - Incluye hoja de respuestas escaneable
   - Tiene código de barras único
```

---

## 🚀 RESUMEN EJECUTIVO

**El script `SemilleroFinDePeriodo_v4.R` genera:**

- ✅ **15 versiones únicas** del examen
- ✅ **6 formatos diferentes** de salida
- ✅ **34 archivos totales**:
  - 4 archivos consolidados (cada uno con 15 versiones internas)
  - 30 archivos NOPS individuales (uno por versión)
- ✅ **Consistencia garantizada** entre formatos (misma semilla)
- ✅ **Diversidad garantizada** entre versiones (aleatorización multinivel)

**Cada versión contiene:**
- 15 preguntas seleccionadas aleatoriamente
- Valores aleatorios únicos por versión
- Orden de opciones mezclado
- Distractores variados

**Ideal para:**
- Evaluaciones masivas con múltiples versiones
- Prevención de copia entre estudiantes
- Corrección automática (formato NOPS)
- Distribución flexible (consolidada o individual)

