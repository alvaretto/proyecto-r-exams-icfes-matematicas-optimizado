# 📚 FUNCIONAMIENTO DETALLADO DEL DIRECTORIO `.claude`

## 🎯 PROPÓSITO GENERAL

El directorio `.claude` es el **cerebro del sistema automatizado** para generar ejercicios matemáticos ICFES en formato R-exams (.Rmd). Funciona como un sistema de automatización inteligente que:

1. **Analiza imágenes** de ejercicios matemáticos ICFES
2. **Genera código .Rmd** siguiendo estándares estrictos
3. **Valida automáticamente** la calidad y funcionalidad
4. **Corrige errores** consultando ejemplos funcionales
5. **Documenta soluciones** para aprendizaje continuo

---

## 🏗️ ESTRUCTURA DEL DIRECTORIO

```
.claude/
├── 📊 Mermaid_Chart.txt          # Diagrama de flujo completo del sistema
├── ⚙️ settings.json              # Configuración de hooks y automatizaciones
├── 🔧 settings.local.json        # Permisos para skills
│
├── 📁 skills/                    # Skills automatizados (comandos /)
│   ├── analizar-icfes/          # Análisis de 6 dimensiones ICFES
│   ├── generar-schoice/         # Generador de selección única
│   ├── generar-cloze/           # Generador de respuesta abierta
│   ├── promover-ejercicio/      # Promoción a producción
│   ├── corregir-error-imagen/   # Corrector de errores TikZ
│   ├── validar-diversidad/      # Validador de 300+ versiones
│   └── validar-icfes/           # Validador de metadatos
│
├── 📁 agents/                    # Agentes especializados
│   ├── clasificador-icfes.md    # Clasificación automática
│   ├── graficador-tikz.md       # Replicación visual TikZ
│   ├── corrector-coherencia.md  # Validación de coherencia
│   ├── diagnosticador-errores.md # Diagnóstico de errores
│   └── validador-visual.md      # Validación visual
│
├── 📁 docs/                      # Documentación técnica
│   ├── README.md                # Índice principal
│   ├── WORKFLOW_PASO_A_PASO.md  # Guía completa del flujo
│   ├── GUIA_RAPIDA_VISUAL.md    # Referencia visual rápida
│   ├── TRES_NIVELES_VALIDACION.md # Metodología de validación
│   ├── TROUBLESHOOTING.md       # Solución de problemas
│   ├── CHANGELOG.md             # Historial de cambios
│   ├── COMMANDS_VS_SKILLS.md    # Filosofía commands vs skills
│   ├── ACTUALIZACION_DOCUMENTACION.md # Actualización de documentación
│   ├── patrones-errores-conocidos.md # Base de conocimiento
│   └── casos-resueltos/         # Historial de casos
│
├── 📁 hooks/                     # Hooks de automatización
├── 📁 scripts/                   # Scripts de utilidad
├── 📁 tests/                     # Tests de validación
├── 📁 backups/                   # Respaldos
├── 📁 logs/                      # Registros de ejecución
└── 📁 deprecated/                # Archivos obsoletos
```

---

## 🔄 FLUJO DE TRABAJO COMPLETO (SEGÚN MERMAID_CHART.TXT)

El diagrama `Mermaid_Chart.txt` define un **sistema de 3 fases obligatorias** con ciclo de retroalimentación automático:

### 📥 **ENTRADA: Análisis de Imagen ICFES**

Cuando se proporciona una imagen de ejercicio matemático, el sistema analiza **6 dimensiones**:

#### 1️⃣ **Nivel de Dificultad**

- Nivel 1: 0-35 puntos (básico)
- Nivel 2: 36-50 puntos (intermedio)
- Nivel 3: 51-70 puntos (avanzado)
- Nivel 4: 71-100 puntos (superior)

#### 2️⃣ **Competencia ICFES**

- **Interpretación y Representación** (34%)
- **Formulación y Ejecución** (43%)
- **Argumentación** (23%)

#### 3️⃣ **Componente**

- Numérico-Variacional
- Geométrico-Métrico
- Aleatorio

#### 4️⃣ **Tipo de Pensamiento**

- Pensamiento Numérico
- Pensamiento Espacial
- Pensamiento Métrico
- Pensamiento Variacional
- Pensamiento Aleatorio

#### 5️⃣ **Contenido Curricular**

- Álgebra y Cálculo (Genéricos/No Genéricos)
- Geometría (Genéricos/No Genéricos)
- Estadística

#### 6️⃣ **Eje Axial Disciplinar**

- Puramente Matemático
- Aplicado/Contextualizado

---

### 📝 **CLASIFICACIÓN DE TIPO DE EJERCICIO**

El sistema determina automáticamente:

**Tipo de pregunta:**

- **SCHOICE**: Selección única (4 opciones)
- **CLOZE**: Pregunta compuesta (múltiples respuestas)

**Análisis visual del enunciado:**

- Con gráficos matemáticos (TikZ, pgfplots)
- Con gráficos no matemáticos (imágenes)
- Sin gráficos

**Análisis de opciones:**

- Opciones textuales/numéricas
- Opciones con gráficos
- Opciones mixtas

---

## 🔄 **FASE 1: RENDERIZADO INICIAL OBLIGATORIO**

**Objetivo:** Generar el archivo .Rmd y compilarlo en todos los formatos soportados.

### Proceso:
```r
# Renderizado completo automático
exams2html("archivo.Rmd", n = 1)   # Formato web
exams2pdf("archivo.Rmd", n = 1)    # Formato PDF
exams2docx("archivo.Rmd", n = 1)   # Formato Word
exams2nops("archivo.Rmd", n = 1)   # Formato escaneable
```

### Captura de errores:

- ✅ Errores de compilación LaTeX
- ✅ Advertencias de R
- ✅ Problemas de encoding
- ✅ Errores de TikZ/Python/Reticulate
- ✅ Problemas de metadatos

**Resultado:** Lista completa de errores y advertencias para análisis.

---

## 🔍 **FASE 2: VALIDACIÓN VISUAL Y FUNCIONAL SISTEMÁTICA**

**Objetivo:** Inspección exhaustiva de coherencia en 4 dimensiones.

### 1️⃣ **Coherencia Matemática**

- ✓ Fórmulas correctas
- ✓ Cálculos verificados
- ✓ Respuesta correcta validada
- ✓ Distractores plausibles

### 2️⃣ **Coherencia Imagen-Texto**

- ✓ Descripción coincide con gráfico
- ✓ Valores sincronizados (texto ↔ código)
- ✓ Etiquetas correctas en gráficos
- ✓ Unidades consistentes

### 3️⃣ **Coherencia de Código**

- ✓ Variables R ↔ Python sincronizadas
- ✓ Variables R ↔ TikZ sincronizadas
- ✓ Variables Python ↔ TikZ sincronizadas
- ✓ Datos compartidos correctamente

### 4️⃣ **Renderizado en 4 Formatos**

- ✓ HTML correcto y funcional
- ✓ PDF correcto y funcional
- ✓ DOCX correcto y funcional
- ✓ NOPS correcto y funcional

**Resultado:** Consolidación de todos los resultados de validación.

---

## ⚡ **FASE 3: DECISIÓN Y ACCIÓN**

**Objetivo:** Decidir si el ejercicio está listo o requiere corrección.

### 🔀 **Punto de Decisión: ¿Errores encontrados?**

#### ❌ **NO HAY ERRORES** → Flujo de Éxito

1. **Documentar éxito** en logs
2. **Validación exitosa** confirmada
3. **Promoción a producción** (`/promover-ejercicio`)

#### ✅ **SÍ HAY ERRORES** → Ciclo de Corrección

---

### 📚 **SUBFASE 3A: CORRECCIÓN BASADA EN EJEMPLOS**

**Principio fundamental:** Los ejemplos funcionales son la **fuente de verdad absoluta**.

#### Proceso automático:

1. **Consultar automáticamente** ejemplos funcionales:
   ```
   📁 /A-Produccion/Ejemplos-Funcionales-Rmd/
   ```

2. **Identificar patrones de solución:**
   - Buscar archivos similares al problema actual
   - Extraer patrones exitosos de código
   - Analizar estructuras funcionales

3. **Aplicar correcciones** basadas en ejemplos validados

#### Clasificación de errores:

**🖼️ Errores de Gráficos:**

- Gráficas no visualizadas → Verificar `include_tikz()` y rutas
- Gráficas solapadas → Ajustar posicionamiento y márgenes
- Renderizado incorrecto → Revisar código TikZ/Python/R
- Tamaño inadecuado → Ajustar `scale`, `width`, `height`

**📝 Errores de Texto/Formato:**

- LaTeX no compila → Revisar caracteres especiales
- Encoding incorrecto → Verificar UTF-8
- Metadatos faltantes → Completar YAML

**🏗️ Errores de Estructura:**

- Opciones incorrectas → Validar 4 opciones únicas
- Solución no coincide → Verificar `exsolution`

**🔗 Errores de Coherencia:**

- Coherencia matemática → Revisar fórmulas y cálculos
- Coherencia imagen-texto → Alinear descripción con gráfico
- Coherencia de código → Sincronizar variables R/Python/TikZ

---

### 🔄 **SUBFASE 3B: CICLO DE REVALIDACIÓN OBLIGATORIA**

**⚠️ REGLA CRÍTICA:** Después de corregir, **SIEMPRE volver a FASE 1**.

```
┌─────────────────────────────────────┐
│  ✏️ Correcciones aplicadas          │
│         ↓                           │
│  🔄 VOLVER AUTOMÁTICAMENTE A FASE 1 │
│         ↓                           │
│  📋 Renderizar 4 formatos           │
│         ↓                           │
│  🔍 Validar nuevamente              │
│         ↓                           │
│  ⚡ Decidir: ¿Errores resueltos?    │
└─────────────────────────────────────┘
```

**NO SE TERMINA** hasta resolver **TODOS** los errores.

---

### 📊 **SUBFASE 3C: GESTIÓN DE RESULTADOS**

**Solo se ejecuta cuando:** Hubo corrección previa Y ahora NO hay errores.

#### Documentación obligatoria:

1. **Registrar en base de conocimiento:**
   ```
   📁 .claude/docs/patrones-errores-conocidos.md
   ```

2. **Contenido del registro:**
   - ❌ Error encontrado (mensaje exacto)
   - 🔍 Causa raíz identificada
   - ✅ Solución aplicada (código completo)
   - 📁 Ejemplo funcional utilizado
   - 🧪 Validación exitosa (PDF + HTML)
   - 📅 Fecha y contexto

3. **Actualizar historial** de casos resueltos

---

## ⛔ **CONDICIONES CRÍTICAS DEL SISTEMA**

El sistema tiene **4 reglas inquebrantables**:

1. ❌ **NO terminar con errores sin resolver**
   - El ciclo continúa hasta solución completa

2. ❌ **NUNCA proceder con errores pendientes**
   - No se promociona a producción con errores

3. ✅ **Documentar SOLO después de solución confirmada**
   - Principio de documentación verificada

4. ✅ **Ejemplos funcionales = Fuente de verdad absoluta**
   - Siempre consultar antes de improvisar

---

## 🤖 **SKILLS AUTOMATIZADOS**

Los **skills** son comandos que ejecutan flujos completos:

### `/analizar-icfes`

- Analiza imagen según 6 dimensiones ICFES
- Clasifica tipo de ejercicio
- Identifica componentes visuales

### `/generar-schoice`

- Genera ejercicio de selección única
- Crea 4 opciones con distractores plausibles
- Incluye metadatos ICFES completos

### `/generar-cloze`

- Genera ejercicio de respuesta abierta
- Configura tolerancias apropiadas
- Valida coherencia de respuestas

### `/promover-ejercicio`

- Mueve archivo a carpeta de producción
- Verifica validaciones previas
- Actualiza índices

### `/corregir-error-imagen`

- Corrige errores de `include_tikz()`
- Aplica renderizado condicional
- Valida en PDF y HTML

### `/validar-diversidad`

- Genera 1000 versiones de prueba
- Verifica mínimo 300 únicas
- Reporta estadísticas

### `/validar-icfes`

- Verifica metadatos completos
- Valida formato YAML
- Confirma competencias válidas

---

## 🎯 **AGENTES ESPECIALIZADOS**

Los **agents** son módulos de IA especializados:

### `clasificador-icfes.md`

- Experto en taxonomía ICFES
- Clasifica ejercicios automáticamente
- Asigna metadatos correctos

### `graficador-tikz.md`

- Replica imágenes con TikZ
- Fidelidad visual 98%+
- Genera código LaTeX optimizado

### `corrector-coherencia.md`

- Valida coherencia matemática
- Detecta inconsistencias
- Sugiere correcciones

### `diagnosticador-errores.md`

- Analiza mensajes de error
- Identifica causa raíz
- Propone soluciones

### `validador-visual.md`

- Compara imagen original vs generada
- Mide fidelidad visual
- Valida elementos gráficos

---

## 📖 **DOCUMENTACIÓN TÉCNICA**

### `WORKFLOW_PASO_A_PASO.md`
Guía completa de 7 pasos desde imagen hasta producción.

### `TRES_NIVELES_VALIDACION.md`
Metodología de validación en 3 niveles:

- **Nivel 1:** RStudio (Run > Run all)
- **Nivel 2:** Generación masiva (SemilleroUnico_v2.R)
- **Nivel 3:** Validación en aula con estudiantes

### `patrones-errores-conocidos.md`
Base de conocimiento con soluciones verificadas:

- Errores de compilación LaTeX
- Problemas de TikZ
- Errores de reticulate (R-Python)
- Inconsistencias de metadatos

### `GUIA_RAPIDA_VISUAL.md`
Referencia visual rápida con diagramas y ejemplos.

---

## 🔧 **HOOKS Y AUTOMATIZACIONES**

El archivo `settings.json` define **hooks automáticos**:

### PreToolUse (Antes de editar)
```json
{
  "matcher": "Write|Edit|MultiEdit",
  "command": "echo '⚠️ REGLA DE ORO: Consulta ejemplos funcionales antes de escribir código .Rmd'"
}
```

### PostToolUse (Después de ejecutar)
```json
{
  "matcher": "Bash",
  "command": "echo '✅ Comando ejecutado. Verifica errores de compilación.'"
}
```

---

## 📊 **MÉTRICAS DE CALIDAD**

El sistema garantiza:

- ✅ **300+ versiones únicas** por ejercicio
- ✅ **4 formatos funcionales** (HTML, PDF, DOCX, NOPS)
- ✅ **98%+ fidelidad visual** en gráficos TikZ
- ✅ **100% coherencia matemática** validada
- ✅ **Metadatos ICFES completos** y correctos
- ✅ **Documentación verificada** de soluciones

---

## 🎓 **FILOSOFÍA DEL SISTEMA**

### Principio de Documentación Verificada
**Solo se documenta lo que está 100% verificado y funcionando.**

### Ciclo de Mejora Continua
Cada error resuelto se convierte en conocimiento reutilizable.

### Ejemplos Funcionales como Fuente de Verdad
Antes de improvisar, **siempre consultar ejemplos probados**.

### Validación Exhaustiva
No se promociona a producción sin pasar las 3 fases completas.

---

## 🚀 **RESUMEN EJECUTIVO**

El directorio `.claude` es un **sistema de automatización inteligente** que:

1. **Transforma imágenes** en ejercicios R-exams funcionales
2. **Valida automáticamente** en 4 dimensiones de coherencia
3. **Corrige errores** consultando ejemplos funcionales
4. **Documenta soluciones** para aprendizaje continuo
5. **Garantiza calidad** mediante ciclo de 3 fases obligatorias

**El resultado:** Ejercicios ICFES de alta calidad, completamente funcionales, con 300+ versiones únicas, listos para producción.

---

## 📋 **DIAGRAMA DE FLUJO VISUAL**

El archivo `Mermaid_Chart.txt` contiene el diagrama completo del sistema que puede visualizarse en:

- Editores Markdown con soporte Mermaid
- Herramientas online como [Mermaid Live Editor](https://mermaid.live/)
- VSCode con extensión Mermaid

### Estructura del diagrama:

```
ENTRADA (Imagen ICFES)
    ↓
ANÁLISIS (6 dimensiones)
    ↓
CLASIFICACIÓN (Tipo de ejercicio)
    ↓
GENERACIÓN (.Rmd)
    ↓
FASE 1: RENDERIZADO (4 formatos)
    ↓
FASE 2: VALIDACIÓN (4 coherencias)
    ↓
FASE 3: DECISIÓN
    ├─→ Sin errores → PRODUCCIÓN ✅
    └─→ Con errores → SUBFASE 3A (Consultar ejemplos)
                   → SUBFASE 3B (Revalidar - volver a FASE 1)
                   → SUBFASE 3C (Documentar solución)
                   → PRODUCCIÓN ✅
```

---

## 🔗 **REFERENCIAS RÁPIDAS**

### Archivos clave:

- **Diagrama completo:** `.claude/Mermaid_Chart.txt`
- **Configuración:** `.claude/settings.json`
- **Guía de usuario:** `.claude/docs/GUIA_USUARIO.md`
- **Workflow completo:** `.claude/docs/WORKFLOW_PASO_A_PASO.md`
- **Base de conocimiento:** `.claude/docs/patrones-errores-conocidos.md`

### Directorios importantes:

- **Skills:** `.claude/skills/`
- **Agentes:** `.claude/agents/`
- **Documentación:** `.claude/docs/`
- **Ejemplos funcionales:** `/A-Produccion/Ejemplos-Funcionales-Rmd/`

---

**Última actualización:** 2025-12-28
**Versión del documento:** 1.0
**Estado:** ✅ Completo y verificado

