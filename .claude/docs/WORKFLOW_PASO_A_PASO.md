# 🎯 Workflow Paso a Paso: De Imagen a Ejercicio R-Exams

**Guía completa para generar ejercicios ICFES R-Exams desde una imagen**

---

## 📋 Índice

1. [Paso 0: Preparar y Subir la Imagen](#paso-0-preparar-y-subir-la-imagen)
2. [Paso 1: Analizar la Imagen con /analizar-icfes](#paso-1-analizar-la-imagen)
3. [Paso 2: Interpretar el Análisis](#paso-2-interpretar-el-análisis)
4. [Paso 3: Generar el Ejercicio](#paso-3-generar-el-ejercicio)
5. [Paso 4: Revisar el Archivo .Rmd](#paso-4-revisar-el-archivo-rmd)
6. [Paso 5: Validar Diversidad](#paso-5-validar-diversidad)
7. [Paso 6: Compilar y Probar](#paso-6-compilar-y-probar)
8. [Paso 7: Promover a Producción](#paso-7-promover-a-producción)

---

## 📸 PASO 0: Preparar y Subir la Imagen

### 0.1 Preparar la Imagen

**Requisitos de la imagen:**

- ✅ Formato: PNG, JPG, o JPEG
- ✅ Resolución mínima: 800x600 píxeles
- ✅ Contenido: Ejercicio matemático ICFES completo
- ✅ Legibilidad: Texto claro y legible
- ✅ Incluir: Enunciado, opciones de respuesta (si aplica), gráficos (si aplica)

**Ubicación recomendada:**
```bash
# Crear directorio para imágenes si no existe
mkdir -p A-Produccion/imagenes-fuente

# Guardar imagen con nombre descriptivo
# Ejemplo: ejercicio_geometria_triangulos.png
```

### 0.2 Subir la Imagen en Claude Code

**Método 1: Arrastrar y Soltar**

1. Abrir Claude Code en VSCode
2. Arrastrar la imagen desde el explorador de archivos
3. Soltar en el chat de Claude Code
4. Esperar confirmación de carga

**Método 2: Usar el Botón de Adjuntar**

1. Hacer clic en el ícono de clip 📎 en el chat
2. Seleccionar "Adjuntar archivo"
3. Navegar a la ubicación de la imagen
4. Seleccionar y abrir

**Método 3: Referenciar Ruta**

1. Si la imagen ya está en el proyecto:
   ```
   /analizar-icfes A-Produccion/imagenes-fuente/ejercicio_geometria.png
   ```

**Verificación:**

- ✅ La imagen aparece en el chat
- ✅ Claude Code confirma que puede ver la imagen
- ✅ La imagen es legible en la vista previa

---

## 🔍 PASO 1: Analizar la Imagen

### 1.1 Ejecutar Comando de Análisis

**Comando:**
```bash
/analizar-icfes [ruta/a/imagen.png]
```

**Ejemplos:**
```bash
# Si la imagen está adjunta en el chat
/analizar-icfes

# Si la imagen está en el proyecto
/analizar-icfes A-Produccion/imagenes-fuente/ejercicio_estadistica.png
```

### 1.2 Qué Hace el Comando

El comando `/analizar-icfes` realiza:

1. **Análisis de contenido matemático**
2. **Clasificación según 6 dimensiones ICFES**:

   - C1: Nivel de Dificultad (1-4)
   - C2: Competencia (Interpretación, Formulación, Argumentación)
   - C3: Componente (Numérico, Geométrico, Aleatorio)
   - C4: Pensamiento (Numérico, Espacial, Métrico, Variacional, Aleatorio)
   - C5: Contenido Curricular (Álgebra, Geometría, Estadística)
   - C6: Eje Axial (Matemático, Aplicado)
3. **Detección de gráficos** (Flujo A vs Flujo B)
4. **Determinación del tipo** (SCHOICE vs CLOZE)

### 1.3 Tiempo Estimado

⏱️ **2-3 minutos** (dependiendo de la complejidad de la imagen)

---

## 📊 PASO 2: Interpretar el Análisis

### 2.1 Salida Esperada

**Ejemplo de salida completa:**

```
═══════════════════════════════════════════════════════════
ANÁLISIS ICFES COMPLETADO
═══════════════════════════════════════════════════════════

📋 CLASIFICACIÓN ICFES (6 Dimensiones)

1. **Nivel de Dificultad**: Nivel 3 (51-70 pts)
   - Justificación: Requiere aplicación de conceptos y razonamiento

2. **Competencia**: Formulación y Ejecución
   - Justificación: El estudiante debe plantear y resolver el problema

3. **Componente**: Geométrico-Métrico
   - Justificación: Involucra figuras geométricas y medidas

4. **Pensamiento**: Pensamiento Espacial
   - Justificación: Requiere visualización y razonamiento espacial

5. **Contenido Curricular**: Geometría (Contenidos No Genéricos)
   - Justificación: Teorema de Pitágoras, áreas de triángulos

6. **Eje Axial**: Aplicado/Contextualizado
   - Justificación: Problema en contexto real (construcción, diseño)

═══════════════════════════════════════════════════════════
ANÁLISIS VISUAL
═══════════════════════════════════════════════════════════

🎨 **Contenido Gráfico Detectado**: SÍ
   - Tipo: Diagrama geométrico (triángulo con medidas)
   - Complejidad: Media
   - Requiere TikZ: SÍ

📝 **Tipo de Ejercicio Recomendado**: SCHOICE
   - Razón: 4 opciones de respuesta únicas
   - Formato: Selección única

═══════════════════════════════════════════════════════════
DECISIÓN DE FLUJO
═══════════════════════════════════════════════════════════

🔀 **Flujo Seleccionado**: FLUJO B (Con Gráficas TikZ)

**Razón**: La imagen contiene un diagrama geométrico que debe ser
replicado con código TikZ para garantizar:

- ✅ Aleatorización de parámetros
- ✅ Generación de 300+ versiones únicas
- ✅ Calidad profesional en PDF y HTML

**Agente Activado**: Agente-Graficador Especializado TikZ

═══════════════════════════════════════════════════════════
PRÓXIMO PASO
═══════════════════════════════════════════════════════════

✅ Ejecutar: /generar-schoice

Este comando generará el archivo .Rmd completo con:
- Código TikZ para el diagrama
- Aleatorización de parámetros
- 4 opciones de respuesta con distractores
- Metadatos ICFES completos
```

### 2.2 Elementos Clave a Verificar

**Verificar que el análisis incluye:**

- ✅ **6 dimensiones ICFES** (C1-C6) con justificaciones
- ✅ **Detección de gráficos** (SÍ/NO)
- ✅ **Tipo de ejercicio** (SCHOICE o CLOZE)
- ✅ **Decisión de flujo** (Flujo A o Flujo B)
- ✅ **Próximo paso claro** (comando a ejecutar)

**Si falta alguna dimensión:**
```bash
# Solicitar análisis completo
Por favor, completa el análisis con las 6 dimensiones ICFES
```

---

## 🛠️ PASO 3: Generar el Ejercicio

### 3.1 Elegir el Comando Apropiado

**Según el tipo de ejercicio detectado:**

| Tipo Detectado | Comando a Ejecutar |
|----------------|-------------------|
| SCHOICE | `/generar-schoice` |
| CLOZE | `/generar-cloze` |

### 3.2 Ejecutar Generación

**Para SCHOICE:**
```bash
/generar-schoice
```

**Para CLOZE:**
```bash
/generar-cloze
```

### 3.3 Qué Hace el Comando

El comando de generación:

1. **Consulta ejemplos funcionales** en `/A-Produccion/Ejemplos-Funcionales-Rmd/`
2. **Genera código .Rmd completo** con:

   - Encabezado YAML con paquetes LaTeX
   - Chunk de configuración inicial
   - Chunk de generación de datos (aleatorización)
   - Chunk de prueba de diversidad (300+ versiones)
   - Chunks de gráficos (TikZ, Python, R según necesidad)
   - Sección Question con enunciado
   - Sección Solution con explicación detallada
   - Meta-information con metadatos ICFES
3. **Aplica metodologías**:

   - Sistema Condicional Automático
   - Metodología TikZ Avanzada (si Flujo B)
   - Corrección de Errores Recurrentes
   - Protocolo Anti-Errores

### 3.4 Ubicación del Archivo Generado

```
A-Produccion/En-Desarrollo/
└── [ejercicio]_[componente]_[competencia]_n[nivel]_v1.Rmd
```

**Ejemplo:**
```
A-Produccion/En-Desarrollo/
└── Triangulos_Geometrico_Formulacion_n3_v1.Rmd
```

### 3.5 Tiempo Estimado

⏱️ **5-10 minutos** (Flujo A) o **10-20 minutos** (Flujo B con TikZ)

---

## 📝 PASO 4: Revisar el Archivo .Rmd

### 4.1 Abrir el Archivo Generado

```bash
# Desde VSCode
code A-Produccion/En-Desarrollo/[nombre_archivo].Rmd

# O desde RStudio
# File > Open File > Navegar a la ubicación
```

### 4.2 Checklist de Revisión

**Estructura del archivo:**

- [ ] **Encabezado YAML completo**
  - [ ] `output: pdf_document, html_document`
  - [ ] `header-includes` con paquetes LaTeX
  
- [ ] **Chunk de configuración inicial** (`inicio`)
  - [ ] Librerías cargadas (exams, tidyverse, etc.)
  - [ ] Configuración de Python (si aplica)
  - [ ] Semilla aleatoria
  
- [ ] **Chunk de generación de datos** (`data_generation`)
  - [ ] Función `generar_datos()`
  - [ ] Validaciones matemáticas
  - [ ] Formato numérico estándar
  
- [ ] **Chunk de prueba de diversidad** (`version_diversity_test`)
  - [ ] Test de 300+ versiones únicas
  
- [ ] **Chunks de gráficos** (si aplica)
  - [ ] TikZ con `include_tikz()` (Flujo B)
  - [ ] Python con matplotlib (si aplica)
  - [ ] R con ggplot2 (si aplica)
  
- [ ] **Sección Question**
  - [ ] Enunciado claro y completo
  - [ ] Answerlist con 4 opciones (SCHOICE)
  
- [ ] **Sección Solution**
  - [ ] Explicación detallada del proceso
  - [ ] Justificación matemática
  - [ ] Answerlist con verdadero/falso
  
- [ ] **Meta-information**
  - [ ] `exname` descriptivo
  - [ ] `extype` correcto (schoice/cloze)
  - [ ] `exsolution` correcto
  - [ ] `exshuffle: TRUE`
  - [ ] Metadatos ICFES completos

**Contenido matemático:**

- [ ] **Aleatorización inteligente**
  - [ ] Rangos apropiados para parámetros
  - [ ] Diversidad matemáticamente relevante
  
- [ ] **Distractores plausibles**
  - [ ] Representan errores comunes
  - [ ] Valores diferentes entre sí
  
- [ ] **Coherencia matemática**
  - [ ] Cálculos correctos
  - [ ] Unidades consistentes
  - [ ] Precisión numérica apropiada

**Calidad gráfica (si Flujo B):**

- [ ] **Código TikZ funcional**
  - [ ] Sintaxis correcta
  - [ ] Parámetros aleatorizados
  - [ ] Etiquetas claras
  
- [ ] **Fidelidad visual**
  - [ ] Replica la imagen original (98%+)
  - [ ] Colores apropiados
  - [ ] Escalas correctas

### 4.3 Correcciones Comunes

**Si encuentras errores:**

```bash
# Para errores de sintaxis Python
# Consultar: A-Produccion/Ejemplos-Funcionales-Rmd/

# Para errores de TikZ
/corregir-error-imagen [archivo.Rmd]

# Para errores de compilación LaTeX
# Consultar: .claude/docs/patrones-errores-conocidos.md
```

### 4.4 Tiempo Estimado

⏱️ **10-15 minutos** (revisión detallada)

---

## ✅ PASO 5: Validar Diversidad

### 5.1 Ejecutar Validación de Diversidad

**Comando:**
```bash
/validar-diversidad-300 A-Produccion/En-Desarrollo/[archivo].Rmd
```

**Ejemplo:**
```bash
/validar-diversidad-300 A-Produccion/En-Desarrollo/Triangulos_Geometrico_Formulacion_n3_v1.Rmd
```

### 5.2 Salida Esperada

```
═══════════════════════════════════════════════════════════
VALIDACIÓN DE DIVERSIDAD - 300+ VERSIONES
═══════════════════════════════════════════════════════════

📊 Generando 1000 versiones de prueba...

✅ RESULTADO: 847 versiones únicas generadas

📈 ESTADÍSTICAS:

   - Versiones generadas: 1000
   - Versiones únicas: 847
   - Tasa de unicidad: 84.7%
   - Mínimo requerido: 300
   - Estado: ✅ APROBADO

═══════════════════════════════════════════════════════════
```

### 5.3 Si la Validación Falla

**Si se generan < 300 versiones únicas:**

```
❌ RESULTADO: 187 versiones únicas generadas

⚠️ PROBLEMA: No se alcanza el mínimo de 300 versiones

💡 SOLUCIONES:

1. Aumentar rangos de aleatorización en generar_datos()
2. Agregar más parámetros aleatorios
3. Diversificar distractores
```

**Acciones correctivas:**

1. Abrir el archivo .Rmd
2. Localizar función `generar_datos()`
3. Aumentar rangos de parámetros:
   ```r
   # Antes
   a <- sample(1:10, 1)
   
   # Después
   a <- sample(1:50, 1)
   ```
4. Volver a ejecutar `/validar-diversidad-300`

### 5.4 Tiempo Estimado

⏱️ **2-3 minutos** (si pasa) o **10-15 minutos** (si requiere correcciones)

---

## 🔨 PASO 6: Compilar y Probar

### 6.1 Abrir en RStudio

```r
# Abrir RStudio
# File > Open File > Seleccionar el .Rmd
```

### 6.2 Compilar a PDF

**Código en consola de R:**
```r
library(exams)

# Compilar a PDF
exams2pdf("A-Produccion/En-Desarrollo/[archivo].Rmd",
          n = 3,  # Generar 3 versiones
          name = "prueba",
          dir = "output_pdf")
```

**Verificar:**

- ✅ Compilación sin errores
- ✅ PDF generado correctamente
- ✅ Gráficos visibles y correctos
- ✅ Texto legible
- ✅ Formato apropiado

### 6.3 Compilar a HTML

**Código en consola de R:**
```r
# Compilar a HTML
exams2html("A-Produccion/En-Desarrollo/[archivo].Rmd",
           n = 3,
           name = "prueba",
           dir = "output_html")
```

**Verificar:**

- ✅ Compilación sin errores
- ✅ HTML generado correctamente
- ✅ Gráficos visibles
- ✅ Interactividad funcional

### 6.4 Compilar a Moodle (Opcional)

**Código en consola de R:**
```r
# Compilar a Moodle XML
exams2moodle("A-Produccion/En-Desarrollo/[archivo].Rmd",
             n = 5,
             name = "prueba_moodle")
```

### 6.5 Troubleshooting de Compilación

**Error común: Imagen faltante**
```bash
/corregir-error-imagen A-Produccion/En-Desarrollo/[archivo].Rmd
```

**Error común: LaTeX**
```bash
# Consultar patrones conocidos
cat .claude/docs/patrones-errores-conocidos.md | grep -A 10 "LaTeX"
```

**Error común: Python/Reticulate**
```bash
# Verificar configuración de Python
# Consultar: Auxiliares/Python-Documentation/Python-ICFES-Guide.md
```

### 6.6 Tiempo Estimado

⏱️ **5-10 minutos** (compilación exitosa) o **15-30 minutos** (con correcciones)

---

## 🚀 PASO 7: Promover a Producción

### 7.1 Verificar Criterios de Promoción

**Checklist obligatorio:**

- [ ] ✅ Diversidad validada (300+ versiones)
- [ ] ✅ Compilación PDF exitosa
- [ ] ✅ Compilación HTML exitosa
- [ ] ✅ Gráficos correctos (si aplica)
- [ ] ✅ Metadatos ICFES completos
- [ ] ✅ Calidad matemática verificada
- [ ] ✅ Sin errores de sintaxis
- [ ] ✅ Distractores plausibles

### 7.2 Ejecutar Promoción

**Comando:**
```bash
/promover-ejercicio A-Produccion/En-Desarrollo/[archivo].Rmd
```

**Ejemplo:**
```bash
/promover-ejercicio A-Produccion/En-Desarrollo/Triangulos_Geometrico_Formulacion_n3_v1.Rmd
```

### 7.3 Qué Hace el Comando

1. **Verifica criterios de calidad**
2. **Mueve el archivo** a:
   ```
   A-Produccion/Nuevos-Ejercicios/[archivo].Rmd
   ```
3. **Actualiza registro** de ejercicios en producción
4. **Genera reporte** de promoción

### 7.4 Ubicación Final

```
A-Produccion/Nuevos-Ejercicios/
└── Triangulos_Geometrico_Formulacion_n3_v1.Rmd
```

### 7.5 Tiempo Estimado

⏱️ **2-3 minutos**

---

## 📊 RESUMEN DEL WORKFLOW COMPLETO

### Diagrama de Flujo

```
📸 PASO 0: Subir Imagen (1 min)
    ↓
🔍 PASO 1: /analizar-icfes (2-3 min)
    ↓
📊 PASO 2: Interpretar Análisis (1 min)
    ↓
🛠️ PASO 3: /generar-schoice o /generar-cloze (5-20 min)
    ↓
📝 PASO 4: Revisar .Rmd (10-15 min)
    ↓
✅ PASO 5: /validar-diversidad-300 (2-15 min)
    ↓
🔨 PASO 6: Compilar PDF/HTML (5-30 min)
    ↓
🚀 PASO 7: /promover-ejercicio (2-3 min)
    ↓
✅ EJERCICIO EN PRODUCCIÓN
```

### Tiempo Total Estimado

| Escenario | Tiempo Mínimo | Tiempo Máximo |
|-----------|---------------|---------------|
| **Flujo A (sin gráficas)** | 28 minutos | 70 minutos |
| **Flujo B (con TikZ)** | 38 minutos | 90 minutos |

---

## 🎯 CASOS ESPECIALES

### Caso 1: Imagen con Múltiples Gráficos

**Si la imagen tiene varios gráficos:**

1. El análisis detectará "Múltiples gráficos"
2. Se activará Flujo B
3. Cada gráfico se replicará con TikZ separado
4. Tiempo estimado: +10-15 minutos por gráfico adicional

### Caso 2: Ejercicio Tipo CLOZE

**Si el análisis detecta CLOZE:**

1. Ejecutar `/generar-cloze` en lugar de `/generar-schoice`
2. Revisar configuración de `exclozetype` y `extol`
3. Consultar ejemplos en:
   ```
   06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/
   09-Probabilidad-Condicionada_Independencia-De-Sucesos/
   ```

### Caso 3: Imagen No es Formato ICFES

**Si la imagen NO es formato ICFES estándar:**

El sistema preguntará:
```
⚠️ La imagen no parece ser formato ICFES estándar.

¿Desea convertirla al formato ICFES?

Por favor proporcione:

1. Competencia ICFES deseada
2. Componente ICFES deseado
3. Nivel de dificultad deseado (1-4)
4. Tipo de pregunta (SCHOICE/CLOZE)
```

Responder con la información solicitada.

---

## ⚠️ TROUBLESHOOTING COMÚN

### Problema 1: El análisis no detecta las 6 dimensiones

**Solución:**
```bash
# Solicitar análisis completo
Por favor, completa el análisis ICFES con las 6 dimensiones:

1. Nivel de Dificultad
2. Competencia
3. Componente
4. Pensamiento
5. Contenido Curricular
6. Eje Axial
```

### Problema 2: Error de compilación PDF

**Solución:**
```bash
# Consultar patrones conocidos
cat .claude/docs/patrones-errores-conocidos.md

# Si es error de imagen
/corregir-error-imagen [archivo].Rmd
```

### Problema 3: Menos de 300 versiones únicas

**Solución:**

1. Aumentar rangos de aleatorización
2. Agregar más parámetros aleatorios
3. Diversificar distractores
4. Volver a validar

### Problema 4: Gráfico TikZ no se ve bien

**Solución:**
```bash
# Solicitar corrección de gráfico
El gráfico TikZ no replica bien la imagen original.
Por favor, mejora la fidelidad visual del código TikZ.
```

---

## 📚 DOCUMENTACIÓN DE REFERENCIA

### Comandos Principales

- `/analizar-icfes` - `.claude/commands/analizar-icfes.md`
- `/generar-schoice` - `.claude/commands/generar-schoice.md`
- `/generar-cloze` - `.claude/commands/generar-cloze.md`
- `/validar-diversidad-300` - `.claude/skills/validar-diversidad/`
- `/corregir-error-imagen` - `.claude/commands/corregir-error-imagen.md`
- `/promover-ejercicio` - `.claude/commands/promover-ejercicio.md`

### Guías y Metodologías

- **Guía de usuario**: `.claude/docs/GUIA_USUARIO.md`
- **Troubleshooting**: `.claude/TROUBLESHOOTING.md`
- **Patrones de errores**: `.claude/docs/patrones-errores-conocidos.md`
- **Metodología TikZ**: Documentada en reglas generales
- **Python ICFES**: `Auxiliares/Python-Documentation/Python-ICFES-Guide.md`

### Ejemplos Funcionales

- **SCHOICE**: `A-Produccion/Ejemplos-Funcionales-Rmd/`
- **CLOZE**: `06-Estadística-Y-Probabilidad/.../Probabilidad-Intervalos-Curva-13-S1-2024B/`

---

---

## 📋 CHECKLIST RÁPIDO

### Antes de Empezar

- [ ] Imagen preparada (PNG/JPG, legible, completa)
- [ ] Claude Code abierto en VSCode
- [ ] RStudio disponible para compilación

### Durante el Proceso

- [ ] Imagen subida correctamente
- [ ] `/analizar-icfes` ejecutado
- [ ] Análisis completo (6 dimensiones)
- [ ] Flujo A o B identificado
- [ ] Comando de generación ejecutado
- [ ] Archivo .Rmd revisado
- [ ] Diversidad validada (300+)
- [ ] PDF compilado sin errores
- [ ] HTML compilado sin errores
- [ ] Calidad matemática verificada

### Antes de Promover

- [ ] Todos los tests pasados
- [ ] Gráficos correctos (si aplica)
- [ ] Metadatos ICFES completos
- [ ] Sin errores de sintaxis
- [ ] Distractores plausibles

---

## 🎓 CONSEJOS Y MEJORES PRÁCTICAS

### Para Mejores Resultados

1. **Imágenes de calidad**: Usar imágenes claras y de alta resolución
2. **Nombres descriptivos**: Nombrar archivos de forma descriptiva
3. **Revisar ejemplos**: Consultar ejemplos funcionales antes de modificar
4. **Validar temprano**: Ejecutar validación de diversidad antes de compilar
5. **Compilar incremental**: Probar PDF y HTML por separado
6. **Documentar errores**: Si encuentras un error nuevo, documentarlo

### Errores Comunes a Evitar

1. ❌ **No validar diversidad**: Siempre ejecutar `/validar-diversidad-300`
2. ❌ **Modificar sin consultar ejemplos**: Revisar ejemplos funcionales primero
3. ❌ **Ignorar warnings**: Los warnings pueden indicar problemas futuros
4. ❌ **No probar en HTML**: Siempre compilar tanto PDF como HTML
5. ❌ **Promover sin verificar**: Completar checklist antes de promover

### Optimización de Tiempo

**Para acelerar el proceso:**

1. **Preparar imágenes en lote**: Tener varias imágenes listas
2. **Usar plantillas**: Basarse en ejercicios similares existentes
3. **Automatizar validación**: Usar scripts de validación
4. **Compilar en paralelo**: Mientras se genera uno, compilar otro

---

## 📞 SOPORTE Y AYUDA

### Si Necesitas Ayuda

1. **Consultar documentación**:

   - `.claude/docs/GUIA_USUARIO.md`
   - `.claude/TROUBLESHOOTING.md`
   - `.claude/docs/patrones-errores-conocidos.md`

2. **Revisar ejemplos funcionales**:
   - `A-Produccion/Ejemplos-Funcionales-Rmd/`

3. **Buscar en casos resueltos**:
   - `.claude/docs/casos-resueltos/`

4. **Solicitar ayuda a Claude Code**:
   ```
   Tengo un problema con [descripción del problema].
   El error es: [mensaje de error]
   Ya intenté: [acciones tomadas]
   ```

---

## 🔄 WORKFLOW ALTERNATIVO: Múltiples Ejercicios

### Para Procesar Varias Imágenes

**Método 1: Secuencial**
```
1. Subir imagen 1 → /analizar-icfes → /generar-schoice
2. Subir imagen 2 → /analizar-icfes → /generar-schoice
3. Subir imagen 3 → /analizar-icfes → /generar-schoice
...
N. Validar todos → Compilar todos → Promover todos
```

**Método 2: Por Lotes**
```
1. Subir todas las imágenes
2. Analizar todas con /analizar-icfes
3. Generar todos los .Rmd
4. Validar todos en lote
5. Compilar todos en lote
6. Promover todos en lote
```

**Recomendación**: Método 1 para 1-3 ejercicios, Método 2 para 4+ ejercicios

---

## 📊 MÉTRICAS DE CALIDAD

### Indicadores de un Buen Ejercicio

| Métrica | Valor Objetivo | Cómo Verificar |
|---------|----------------|----------------|
| **Diversidad de versiones** | ≥ 300 | `/validar-diversidad-300` |
| **Compilación PDF** | Sin errores | `exams2pdf()` |
| **Compilación HTML** | Sin errores | `exams2html()` |
| **Fidelidad gráfica** | ≥ 98% | Comparación visual |
| **Metadatos ICFES** | 6/6 completos | Revisar Meta-information |
| **Distractores únicos** | 4 valores diferentes | Revisar Answerlist |
| **Tiempo de compilación** | < 30 segundos | Medir en RStudio |

### Niveles de Calidad

**⭐ Básico (Mínimo Aceptable)**

- ✅ Compila sin errores
- ✅ 300+ versiones únicas
- ✅ Metadatos ICFES completos

**⭐⭐ Bueno (Recomendado)**

- ✅ Todo lo anterior
- ✅ Distractores plausibles
- ✅ Gráficos de calidad (si aplica)
- ✅ Explicación detallada en Solution

**⭐⭐⭐ Excelente (Ideal)**

- ✅ Todo lo anterior
- ✅ 500+ versiones únicas
- ✅ Fidelidad gráfica 98%+
- ✅ Distractores pedagógicos
- ✅ Contexto realista y relevante

---

## 🎯 OBJETIVOS DE APRENDIZAJE

### Al Completar Este Workflow, Serás Capaz De:

1. ✅ **Subir y preparar** imágenes de ejercicios ICFES
2. ✅ **Analizar y clasificar** ejercicios según 6 dimensiones ICFES
3. ✅ **Interpretar** el análisis y decidir el flujo apropiado
4. ✅ **Generar** archivos .Rmd completos y funcionales
5. ✅ **Revisar y validar** la calidad del código generado
6. ✅ **Compilar** ejercicios en múltiples formatos (PDF, HTML, Moodle)
7. ✅ **Troubleshoot** errores comunes de compilación
8. ✅ **Promover** ejercicios a producción con confianza

---

## 📈 PRÓXIMOS PASOS

### Después de Dominar Este Workflow

1. **Explorar variantes avanzadas**:

   - Ejercicios con múltiples gráficos
   - Ejercicios tipo CLOZE complejos
   - Integración de Python para visualizaciones

2. **Optimizar el proceso**:

   - Crear plantillas personalizadas
   - Automatizar pasos repetitivos
   - Desarrollar bibliotecas de distractores

3. **Contribuir al proyecto**:

   - Documentar nuevos patrones de errores
   - Crear ejemplos funcionales adicionales
   - Mejorar metodologías existentes

---

**Última actualización:** 2025-12-20
**Versión:** 1.0
**Autor:** Sistema Claude Code ICFES R-Exams

