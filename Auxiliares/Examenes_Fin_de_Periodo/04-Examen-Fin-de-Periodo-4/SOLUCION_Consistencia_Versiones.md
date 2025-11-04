# 🔧 SOLUCIÓN: Consistencia entre Versiones con y sin Soluciones

## 📋 PROBLEMA IDENTIFICADO

### **Síntomas:**
- Las preguntas generadas eran DIFERENTES entre versiones con y sin soluciones
- Las opciones de respuesta eran DIFERENTES entre versiones
- El enunciado permanecía igual, pero los datos numéricos cambiaban
- Esto ocurría al comparar salidas de `exams2pandoc()` vs `exams2pdf()`

### **Causa Raíz:**
El archivo `.Rmd` generaba una **semilla basada en timestamp** en cada ejecución:

```r
# CÓDIGO PROBLEMÁTICO (ELIMINADO):
timestamp_seed <- as.numeric(Sys.time()) * 1000000
base_seed <- sample(1:1000000, 1)
unique_seed <- (timestamp_seed + base_seed) %% 1000000
```

**Consecuencia:** Cada llamada a `exams2pandoc()` o `exams2pdf()` ejecutaba el `.Rmd` en momentos diferentes, generando timestamps diferentes y por lo tanto datos completamente diferentes.

---

## ✅ SOLUCIÓN IMPLEMENTADA

### **Estrategia: Control de Semilla Global**

Se implementó un sistema de **semilla compartida** que garantiza que todas las versiones usen exactamente los mismos datos aleatorios.

### **Cambios en `SemilleroFinDePeriodo_4.R`:**

#### 1. **Semilla Global Única (líneas 11-13):**
```r
# SOLUCIÓN CRÍTICA: Establecer semilla global ÚNICA para todas las compilaciones
# Esto garantiza que exams2pandoc y exams2pdf generen exactamente los mismos datos
semilla <- 123456  # Semilla fija para reproducibilidad entre versiones
set.seed(semilla)
```

#### 2. **Restablecer Semilla Antes de Cada Generación:**
```r
# Antes de cada llamada a exams2pandoc() o exams2pdf():
set.seed(semilla)
```

Esto se aplicó en **4 lugares**:
- Línea 27: Antes de `exams2pandoc()` con soluciones (DOCX)
- Línea 54: Antes de `exams2pandoc()` sin soluciones (DOCX)
- Línea 83: Antes de `exams2pdf()` con soluciones (PDF)
- Línea 97: Antes de `exams2pdf()` sin soluciones (PDF)

### **Cambios en `cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd`:**

#### 1. **Eliminación de Generación de Semilla Basada en Timestamp (líneas 54-64):**
```r
# ANTES (PROBLEMÁTICO):
timestamp_seed <- as.numeric(Sys.time()) * 1000000
base_seed <- sample(1:1000000, 1)
unique_seed <- (timestamp_seed + base_seed) %% 1000000

# DESPUÉS (CORRECTO):
# SOLUCIÓN CRÍTICA: Eliminar generación de semilla basada en timestamp
# La semilla se controla desde el script R principal (SemilleroFinDePeriodo_4.R)
# Esto garantiza consistencia entre versiones con y sin soluciones

# Generar seed para distractores basado en valores aleatorios reproducibles
base_seed <- sample(1:1000000, 1)
```

#### 2. **Actualización de Referencia a Semilla (línea 131):**
```r
# ANTES:
seed_distractores <- unique_seed + factor_diversidad + variacion_contexto

# DESPUÉS:
seed_distractores <- base_seed + factor_diversidad + variacion_contexto
```

#### 3. **Comentario Explicativo en Chunk de Configuración (líneas 47-51):**
```r
# IMPORTANTE: NO establecer set.seed() aquí
# La semilla se controla desde el script R principal (SemilleroFinDePeriodo_4.R)
# Esto garantiza que todas las versiones (con/sin soluciones, PDF/DOCX) 
# generen exactamente los mismos datos aleatorios
```

---

## 🎯 RESULTADO ESPERADO

### **Comportamiento Correcto:**
1. ✅ **Mismos datos numéricos** en todas las versiones
2. ✅ **Mismas preguntas** en todas las versiones
3. ✅ **Mismas opciones de respuesta** en todas las versiones
4. ✅ **Única diferencia**: Presencia o ausencia de la sección Solution

### **Verificación:**
Al ejecutar `SemilleroFinDePeriodo_4.R`, todas las salidas generadas deben tener:
- Mismo contexto del problema
- Mismos valores de catetos e hipotenusa
- Mismas 4 opciones de respuesta en el mismo orden
- Solo difieren en mostrar o no la solución detallada

---

## 📊 FLUJO DE CONTROL DE SEMILLA

```
SemilleroFinDePeriodo_4.R
│
├─ set.seed(123456)  ← Semilla global inicial
│
├─ set.seed(123456)  ← Restablecer antes de exams2pandoc (con sol)
│   └─ Ejecuta .Rmd → Genera datos con semilla 123456
│
├─ set.seed(123456)  ← Restablecer antes de exams2pandoc (sin sol)
│   └─ Ejecuta .Rmd → Genera MISMOS datos con semilla 123456
│
├─ set.seed(123456)  ← Restablecer antes de exams2pdf (con sol)
│   └─ Ejecuta .Rmd → Genera MISMOS datos con semilla 123456
│
└─ set.seed(123456)  ← Restablecer antes de exams2pdf (sin sol)
    └─ Ejecuta .Rmd → Genera MISMOS datos con semilla 123456
```

---

## 🔄 PARA GENERAR VERSIONES DIFERENTES

Si deseas generar una versión completamente diferente del examen:

1. **Cambiar la semilla en `SemilleroFinDePeriodo_4.R`:**
```r
semilla <- 654321  # Cambiar este número
```

2. **Ejecutar el script nuevamente**

3. **Todas las versiones (con/sin soluciones) tendrán nuevos datos, pero serán consistentes entre sí**

---

## ⚠️ IMPORTANTE: NO HACER

❌ **NO establecer `set.seed()` dentro del archivo `.Rmd`**
❌ **NO usar `Sys.time()` para generar semillas**
❌ **NO comentar las líneas `set.seed(semilla)` en el script R**

✅ **SÍ controlar la semilla desde el script R principal**
✅ **SÍ restablecer la semilla antes de cada llamada a `exams2*()`**
✅ **SÍ usar una semilla fija para reproducibilidad**

---

## 📝 NOTAS ADICIONALES

- Esta solución es compatible con la filosofía ICFES de generar 300+ versiones únicas
- La diversidad se mantiene a través de la función `generar_datos()` que usa `sample()` internamente
- El control de semilla global solo garantiza que cada ejecución del script genere las mismas versiones
- Para generar múltiples versiones diferentes, ejecutar el script con diferentes valores de `semilla`

