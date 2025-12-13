# 🤖 GUÍA COMPLETA: Creación de Agentes en AnythingLLM

## 📋 ÍNDICE

1. [Diferencia entre Agentes y Workflows](#diferencia-entre-agentes-y-workflows)
2. [¿Qué es un Agente en AnythingLLM?](#qué-es-un-agente-en-anythingllm)
3. [Acceso a la Configuración de Agentes](#acceso-a-la-configuración-de-agentes)
4. [Configuración Paso a Paso](#configuración-paso-a-paso)
5. [Herramientas Disponibles para Agentes](#herramientas-disponibles-para-agentes)
6. [Agentes para ICFES R-Exams](#agentes-para-icfes-r-exams)
7. [Cómo Usar los Agentes](#cómo-usar-los-agentes)
8. [Consejos y Mejores Prácticas](#consejos-y-mejores-prácticas)
9. [Solución de Problemas](#solución-de-problemas)

---

## 🔄 DIFERENCIA ENTRE AGENTES Y WORKFLOWS

### ⚠️ ACLARACIÓN IMPORTANTE

**Agentes** y **Workflows (Flows)** son dos conceptos diferentes en AnythingLLM, aunque ambos pueden trabajar juntos.

### 📊 Tabla Comparativa

| **Característica** | **Agentes (AI Agents)** | **Workflows (Flows)** |
|-------------------|------------------------|----------------------|
| **Naturaleza** | Conversacional e interactivo | Estructurado y automatizado |
| **Configuración** | A nivel de workspace | Creación visual con bloques |
| **Activación** | Con `@agent` en el chat | Como skill del agente o directamente |
| **Flexibilidad** | Alta - se adapta al contexto | Media - sigue secuencia definida |
| **Uso principal** | Interacción dinámica con usuario | Automatización de tareas repetitivas |
| **Herramientas** | Predefinidas (RAG, Web, Files, etc.) | Bloques personalizables |
| **Complejidad** | Baja - configuración simple | Media - requiere diseño de flujo |
| **Código requerido** | No | No (no-code) |
| **Mejor para** | Asistencia conversacional | Procesos paso a paso |
| **Ejemplo de uso** | "Busca información sobre X" | "Validar archivo → Corregir → Guardar" |

### 🎯 ¿Cuándo usar cada uno?

#### Usa **AGENTES** cuando:

- ✅ Necesitas interacción conversacional
- ✅ El usuario hace preguntas abiertas
- ✅ Requieres búsqueda web o RAG
- ✅ La tarea es exploratoria
- ✅ Necesitas flexibilidad en las respuestas

**Ejemplo**:  *"@agent busca información sobre estadística descriptiva y explícamela"*

#### Usa **WORKFLOWS** cuando:

- ✅ Tienes un proceso definido paso a paso
- ✅ Necesitas automatizar tareas repetitivas
- ✅ Requieres secuencias predecibles
- ✅ Quieres integrar APIs o archivos
- ✅ Necesitas procesamiento estructurado

**Ejemplo**:  *Flow que valida → corrige → compila un archivo .Rmd*

### 🔗 Trabajando Juntos

**Los Flows pueden ser skills de los Agentes**: 

- Creas un Flow para una tarea específica
- El Agente puede llamar ese Flow como una herramienta
- El usuario interactúa con el Agente, que usa Flows internamente

**Ejemplo**: 

```
Usuario: "@agent valida este archivo .Rmd"
Agente: [Llama al Flow "Validador ICFES"]
Flow: [Ejecuta: Leer → Validar → Reportar]
Agente: "He validado el archivo. Encontré 3 errores..."
```

---

## 🤖 ¿QUÉ ES UN AGENTE EN ANYTHINGLLM?

### Definición

Un **Agente** en AnythingLLM es un LLM (Large Language Model) con acceso a herramientas específicas que le permiten:

- 🔍 **Buscar información** en documentos embebidos (RAG)
- 🌐 **Navegar por internet** (Web Browsing)
- 📄 **Extraer contenido** de sitios web (Web Scraping)
- 💾 **Guardar archivos** en tu máquina local
- 📊 **Generar gráficos** y visualizaciones
- 🗄️ **Consultar bases de datos** SQL
- 📚 **Resumir documentos** largos
- 🔄 **Ejecutar Flows** personalizados

### Características Clave

1. **Conversacional**:  Interactúas con el agente mediante chat natural
2. **Contextual**:  Mantiene el contexto de la conversación
3. **Multi-herramienta**:  Puede usar múltiples tools en una sola sesión
4. **Workspace-específico**:  Cada workspace tiene su propia configuración de agente
5. **Extensible**:  Puedes agregar Flows personalizados como skills

### Arquitectura

```
┌─────────────────────────────────────────┐
│         Usuario en Workspace            │
│                                         │
│  "@agent genera un ejercicio ICFES"    │
└──────────────┬──────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────┐
│          Agente (LLM + Tools)           │
│                                         │
│  ┌─────────────────────────────────┐   │
│  │  LLM (GPT-4, Claude, etc.)      │   │
│  └─────────────────────────────────┘   │
│                                         │
│  ┌─────────────────────────────────┐   │
│  │  Herramientas Disponibles:      │   │
│  │  • RAG Search                   │   │
│  │  • Web Browsing                 │   │
│  │  • Web Scraping                 │   │
│  │  • Save Files                   │   │
│  │  • List Documents               │   │
│  │  • Summarize Documents          │   │
│  │  • Chart Generation             │   │
│  │  • SQL Agent                    │   │
│  │  • Custom Flows (tus workflows) │   │
│  └─────────────────────────────────┘   │
└─────────────────────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────┐
│         Resultado al Usuario            │
│                                         │
│  "He generado el ejercicio y lo he     │
│   guardado en Lab-Manjaro/..."         │
└─────────────────────────────────────────┘
```

---

## 🚀 ACCESO A LA CONFIGURACIÓN DE AGENTES

### Paso 1: Abrir Workspace Settings

1. Abre AnythingLLM en http://localhost:3001
2. Selecciona tu workspace **"ICFES R-Exams"**
3. Haz clic en el ícono de **configuración** (⚙️) en la esquina superior derecha del workspace

### Paso 2: Navegar a Agent Configuration

1. En el menú lateral de configuración, busca la sección **"Agent Configuration"**
2. Haz clic en **"Agent Configuration"**
3. Verás la interfaz de configuración del agente

**Descripción visual de la interfaz**: 

```
┌────────────────────────────────────────────────────┐
│  Workspace Settings                                │
│                                                    │
│  ┌──────────────────┐                             │
│  │ General          │                             │
│  │ Chat Settings    │                             │
│  │ Vector Database  │                             │
│  │ ► Agent Config   │ ← Aquí                      │
│  │ Members          │                             │
│  └──────────────────┘                             │
│                                                    │
│  Agent Configuration                               │
│  ┌──────────────────────────────────────────────┐ │
│  │ LLM Provider: [Dropdown ▼]                   │ │
│  │ Model: [Dropdown ▼]                          │ │
│  │                                              │ │
│  │ [Update workspace agent]                     │ │
│  │                                              │ │
│  │ [Configure Agent Skills]                     │ │
│  └──────────────────────────────────────────────┘ │
└────────────────────────────────────────────────────┘
```

---

## ⚙️ CONFIGURACIÓN PASO A PASO

### PASO 1: Seleccionar LLM Provider y Model

**¿Qué es esto?**

- Define qué modelo de IA usará el agente
- Puede ser diferente al LLM del workspace normal

**Configuración**: 

1. **LLM Provider**:  Selecciona el proveedor
   - OpenAI (GPT-4, GPT-3.5)
   - Anthropic (Claude 3.5 Sonnet, Claude 3 Opus)
   - Google (Gemini Pro)
   - Local (Ollama, LM Studio)
   - Otros (Groq, Together AI, etc.)

2. **Model**:  Selecciona el modelo específico
   - Para agentes, se recomienda modelos potentes
   - Modelos pequeños o con baja cuantización pueden fallar

**Recomendaciones para ICFES R-Exams**: 

| **Tarea** | **Modelo Recomendado** | **Razón** |
|-----------|----------------------|-----------|
| Generación de código | GPT-4 Turbo / Claude 3.5 Sonnet | Mejor comprensión de código |
| Validación | GPT-4 / Claude 3 Opus | Precisión en detección de errores |
| Clasificación | GPT-3.5 Turbo / Gemini Pro | Suficiente para categorización |
| TikZ | Claude 3.5 Sonnet | Excelente con código LaTeX |

**Configuración recomendada para ICFES R-Exams**: 

```yaml
LLM Provider: OpenAI
Model: gpt-4-turbo-preview
```

3. **Haz clic en "Update workspace agent"** para guardar la configuración

⚠️ **IMPORTANTE**:  Si no haces clic en "Update workspace agent", los cambios NO se guardarán.

---

### PASO 2: Configurar Agent Skills

**¿Qué son los Agent Skills?**

- Son las herramientas que el agente puede usar
- Algunos son obligatorios (Default), otros opcionales
- También puedes agregar tus Flows personalizados

**Acceso**: 

1. Después de actualizar el LLM, haz clic en **"Configure Agent Skills"**
2. Se abrirá una ventana con todas las skills disponibles

**Interfaz de Agent Skills**: 

```
┌────────────────────────────────────────────────────┐
│  Agent Skills Configuration                        │
│                                                    │
│  Default Skills (siempre activos):                │
│  ✓ RAG Search                                     │
│  ✓ Summarize Documents                            │
│  ✓ Scrape Websites                                │
│                                                    │
│  Optional Skills:                                  │
│  ☐ Web Browsing (requiere Search Provider)       │
│  ☐ Save Files to Browser                         │
│  ☐ List Documents                                │
│  ☐ Chart Generation                              │
│  ☐ SQL Agent                                     │
│                                                    │
│  Custom Flows:                                     │
│  ☐ Generador Completo ICFES                      │
│  ☐ Validador y Corrector ICFES                   │
│  ☐ Optimizador de Diversidad ICFES              │
│                                                    │
│  [Save Configuration]                              │
└────────────────────────────────────────────────────┘
```

**Skills Recomendadas para ICFES R-Exams**: 

| **Skill** | **Activar** | **Uso en ICFES** |
|-----------|------------|------------------|
| RAG Search | ✅ (Default) | Consultar ejemplos funcionales |
| Summarize Documents | ✅ (Default) | Resumir documentación |
| Scrape Websites | ✅ (Default) | Obtener documentación R-exams |
| Web Browsing | ✅ | Buscar soluciones a errores |
| Save Files to Browser | ✅ | Guardar archivos .Rmd generados |
| List Documents | ✅ | Ver ejemplos disponibles |
| Chart Generation | ❌ | No necesario para este proyecto |
| SQL Agent | ❌ | No necesario para este proyecto |
| Custom Flows | ✅ | Tus workflows personalizados |

---

### PASO 3: Configurar Search Provider (Opcional)

**¿Qué es el Search Provider?**

- Permite al agente buscar en internet (Web Browsing)
- Requiere API key de un servicio de búsqueda

**Proveedores soportados**: 

- **Google Search** (100 búsquedas gratis/día)
- **SearchApi** (múltiples motores)
- **Serper**
- **Bing Search**
- **Serply**

**Configuración recomendada**: 

```yaml
Search Provider: Google Search
API Key: [Tu API key de Google Custom Search]
```

**Cómo obtener Google Search API Key**: 

1. Ve a https://console.cloud.google.com/
2. Crea un proyecto nuevo
3. Habilita "Custom Search API"
4. Crea credenciales (API Key)
5. Copia la API key en AnythingLLM

⚠️ **Nota**:  Si no configuras Search Provider, el agente no podrá usar Web Browsing.

---

### PASO 4: Configurar Parámetros del LLM (Avanzado)

**Ubicación**:  Workspace Settings → Chat Settings

**Parámetros importantes**: 

| **Parámetro** | **Valor Recomendado** | **Descripción** |
|--------------|---------------------|----------------|
| Temperature | 0.3 | Más determinista, menos creativo |
| Max Tokens | 4000 | Respuestas largas para código |
| Top P | 0.95 | Diversidad de respuestas |
| Frequency Penalty | 0.0 | Sin penalización por repetición |
| Presence Penalty | 0.0 | Sin penalización por temas |

**Configuración para ICFES R-Exams**: 

```yaml
Temperature: 0.3  # Precisión sobre creatividad
Max Tokens: 4000  # Código .Rmd puede ser largo
Top P: 0.95       # Balance adecuado
```

---

### PASO 5: Guardar y Activar

1. **Guardar configuración**:  Haz clic en todos los botones "Save" o "Update"
2. **Verificar activación**:  El agente está activo automáticamente
3. **Probar**:  Escribe `@agent hola` en el chat para verificar

---

## 🛠️ HERRAMIENTAS DISPONIBLES PARA AGENTES

### 1️⃣ RAG Search (Default)

**Función**:  Busca información en documentos embebidos en el workspace

**Uso**: 

```
@agent ¿qué ejemplos funcionales tenemos de ejercicios de estadística?
```

**Cómo funciona**: 

1. El agente busca en los documentos del workspace
2. Encuentra información relevante
3. Responde basándose en esa información

**Configuración necesaria**: 

- Documentos embebidos en el workspace
- Vector database configurada

---

### 2️⃣ Web Browsing (Opcional)

**Función**:  Busca información en internet

**Uso**: 

```
@agent busca información sobre cómo usar pgfplots en LaTeX
```

**Cómo funciona**: 

1. El agente usa el Search Provider configurado
2. Realiza búsqueda en Google/Bing/etc.
3. Extrae información relevante
4. Responde con la información encontrada

**Configuración necesaria**: 

- Search Provider configurado
- API Key válida

---

### 3️⃣ Web Scraping (Default)

**Función**:  Extrae contenido de un sitio web específico

**Uso**: 

```
@agent extrae el contenido de https://www.r-exams.org/intro/ y resúmelo
```

**Cómo funciona**: 

1. El agente accede a la URL
2. Extrae el contenido HTML
3. Embebe el contenido en el workspace
4. Responde basándose en ese contenido

**Nota**:  El contenido se embebe temporalmente y puede ser consultado después.

---

### 4️⃣ Save Files to Browser (Opcional)

**Función**:  Guarda archivos en tu máquina local

**Uso**: 

```
@agent guarda este código como ejercicio_estadistica.Rmd
```

**Cómo funciona**: 

1. El agente prepara el contenido
2. Muestra un popup para elegir ubicación
3. Guarda el archivo en la ubicación seleccionada

**Nota**:  Requiere interacción del usuario para elegir ubicación.

---

### 5️⃣ List Documents (Opcional)

**Función**:  Lista todos los documentos embebidos en el workspace

**Uso**: 

```
@agent ¿qué documentos tienes disponibles?
```

**Cómo funciona**: 

1. El agente consulta la base de datos vectorial
2. Lista todos los documentos embebidos
3. Muestra nombres y metadatos

---

### 6️⃣ Summarize Documents (Default)

**Función**:  Resume documentos largos

**Uso**: 

```
@agent resume el contenido de METODOLOGIA_Correccion_Errores_Recurrentes_ICFES_R_Exams.md
```

**Cómo funciona**: 

1. El agente busca el documento
2. Lee el contenido completo
3. Genera un resumen conciso

---

### 7️⃣ Chart Generation (Opcional)

**Función**:  Genera gráficos y visualizaciones

**Uso**: 

```
@agent grafica y=2x+3 para x de -10 a 10
```

**Cómo funciona**: 

1. El agente interpreta la solicitud
2. Genera código de gráfico
3. Renderiza el gráfico
4. Muestra la imagen

**Nota**:  No es muy útil para ICFES R-Exams, ya que generamos gráficos en .Rmd.

---

### 8️⃣ SQL Agent (Opcional)

**Función**:  Consulta bases de datos SQL

**Uso**: 

```
@agent consulta la tabla ejercicios y muestra los de nivel 3
```

**Cómo funciona**: 

1. El agente se conecta a la base de datos
2. Genera query SQL
3. Ejecuta la consulta
4. Presenta los resultados

**Nota**:  No es necesario para ICFES R-Exams a menos que uses base de datos.

---

### 9️⃣ Custom Flows (Tus Workflows)

**Función**:  Ejecuta tus workflows personalizados como skills

**Uso**: 

```
@agent valida el archivo ejercicio_mediana_v1.Rmd
```

**Cómo funciona**: 

1. El agente identifica que debe usar un Flow
2. Llama al Flow "Validador y Corrector ICFES"
3. El Flow ejecuta su secuencia
4. El agente presenta los resultados

**Configuración**: 

- Crear Flows primero
- Activarlos en Agent Skills
- El agente los usará automáticamente cuando sea apropiado

---

## 🎯 AGENTES PARA ICFES R-EXAMS

**⚠️ ACLARACIÓN IMPORTANTE**: 

En AnythingLLM, **NO se crean múltiples agentes separados**. En su lugar:

1. **Un solo agente por workspace** con todas las herramientas
2. **Diferentes Flows** para tareas específicas
3. **El agente usa los Flows** según la necesidad

**Arquitectura recomendada para ICFES R-Exams**: 

```
┌─────────────────────────────────────────────────┐
│  Workspace: ICFES R-Exams                       │
│                                                 │
│  ┌───────────────────────────────────────────┐ │
│  │  AGENTE ÚNICO                             │ │
│  │  (GPT-4 Turbo / Claude 3.5 Sonnet)        │ │
│  │                                           │ │
│  │  Skills Activas:                          │ │
│  │  • RAG Search                             │ │
│  │  • Web Browsing                           │ │
│  │  • Web Scraping                           │ │
│  │  • Save Files                             │ │
│  │  • List Documents                         │ │
│  │  • Summarize Documents                    │ │
│  │  • Flow: Generador Completo ICFES        │ │
│  │  • Flow: Validador y Corrector ICFES     │ │
│  │  • Flow: Optimizador de Diversidad       │ │
│  └───────────────────────────────────────────┘ │
└─────────────────────────────────────────────────┘
```

### Configuración del Agente Único ICFES

**Configuración del LLM**: 

```yaml
LLM Provider: OpenAI
Model: gpt-4-turbo-preview
Temperature: 0.3
Max Tokens: 4000
Top P: 0.95
```

**Skills Activadas**: 

- ✅ RAG Search (consultar ejemplos funcionales)
- ✅ Web Browsing (buscar documentación)
- ✅ Web Scraping (extraer contenido de R-exams.org)
- ✅ Save Files (guardar archivos .Rmd)
- ✅ List Documents (ver ejemplos disponibles)
- ✅ Summarize Documents (resumir documentación)
- ✅ Flow: Generador Completo ICFES
- ✅ Flow: Validador y Corrector ICFES
- ✅ Flow: Optimizador de Diversidad ICFES

**Search Provider**: 

```yaml
Provider: Google Search
API Key: [Tu API key]
```

---

### Prompt del Sistema (System Prompt)

**Ubicación**:  Workspace Settings → Chat Settings → System Prompt

**Prompt recomendado para ICFES R-Exams**: 

```markdown
Eres un experto en generar ejercicios matemáticos para el sistema ICFES usando el framework R-exams.

Tu conocimiento incluye:

1. Estructura completa de archivos .Rmd
2. Metadatos ICFES obligatorios (competencia, nivel, componente)
3. Metodologías TikZ avanzadas para gráficos
4. Integración Python-R con reticulate
5. Sistema de validación y corrección de errores
6. Generación de 300+ versiones únicas por ejercicio

REGLAS OBLIGATORIAS:

- SIEMPRE consulta los ejemplos funcionales en /A-Produccion/Ejemplos-Funcionales-Rmd/ antes de generar código
- NUNCA improvises configuraciones técnicas
- SIEMPRE incluye test de diversidad de versiones (300+ estándar)
- OBLIGATORIO incluir metadatos ICFES completos
- NUNCA uses set.seed() fijo - debe ser aleatorio
- SIEMPRE valida coherencia matemática

HERRAMIENTAS DISPONIBLES:

- Usa RAG Search para consultar ejemplos funcionales
- Usa Web Browsing para buscar documentación oficial
- Usa Web Scraping para extraer contenido de r-exams.org
- Usa Save Files para guardar archivos .Rmd generados
- Usa tus Flows personalizados para tareas específicas:

  * "Generador Completo ICFES" para generar ejercicios desde imágenes
  * "Validador y Corrector ICFES" para validar y corregir archivos
  * "Optimizador de Diversidad" para mejorar diversidad de versiones

FORMATO DE RESPUESTA:

- Responde siempre en español
- Sé preciso y técnico
- Proporciona código completo y funcional
- Explica decisiones técnicas importantes
- Sugiere mejoras cuando sea apropiado

COMPETENCIAS ICFES:

- interpretacion_representacion
- formulacion_ejecucion
- argumentacion

COMPONENTES ICFES:

- geometrico_metrico
- numerico_variacional
- aleatorio

NIVELES: 1 (básico) a 4 (superior)
```

**Cómo configurar el System Prompt**: 

1. Ve a Workspace Settings → Chat Settings
2. Busca el campo "System Prompt"
3. Pega el prompt anterior
4. Haz clic en "Update workspace"

---

## 💬 CÓMO USAR LOS AGENTES

### Activar el Agente

**Sintaxis**:  `@agent [tu pregunta o solicitud]`

**Ejemplos**: 

```
@agent hola
@agent ¿qué ejemplos funcionales tienes de estadística?
@agent genera un ejercicio de mediana nivel 2
@agent valida el archivo ejercicio_estadistica.Rmd
```

### Sesión de Agente

**Inicio de sesión**: 

- Cuando escribes `@agent`, se inicia una sesión
- Verás el mensaje: `Agent @agent invoked`

**Durante la sesión**: 

- NO necesitas escribir `@agent` en cada mensaje
- El agente mantiene el contexto
- Puedes hacer preguntas de seguimiento

**Fin de sesión**: 

- Escribe `/exit` para terminar la sesión
- Verás el mensaje: `Agent session completed`

**Ejemplo de conversación**: 

```
Usuario: @agent genera un ejercicio de estadística nivel 2
Agente: [Agent @agent invoked]
        Claro, voy a generar un ejercicio de estadística nivel 2.
        ¿Qué tema específico prefieres? (mediana, moda, media, etc.)

Usuario: mediana
Agente: Perfecto. Voy a consultar los ejemplos funcionales...
        [Usa RAG Search]
        He encontrado ejemplos de mediana. Generando ejercicio...
        [Usa Flow: Generador Completo ICFES]
        ✅ Ejercicio generado: ejercicio_mediana_aleatorio_n2_v1.Rmd
        ¿Quieres que lo valide?

Usuario: sí, valídalo
Agente: [Usa Flow: Validador y Corrector ICFES]
        ✅ Validación completada. El archivo está correcto.
        ¿Necesitas algo más?

Usuario: /exit
Agente: [Agent session completed]
```

### Comandos Útiles

| **Comando** | **Función** |
|------------|------------|
| `@agent` | Iniciar sesión de agente |
| `/exit` | Terminar sesión de agente |
| `/reset` | Reiniciar conversación |
| `/help` | Ver comandos disponibles |

---

## 💡 CONSEJOS Y MEJORES PRÁCTICAS

### ✅ Configuración del Agente

1. **Usa modelos potentes**:  GPT-4, Claude 3.5 Sonnet
   - Modelos pequeños fallan en tool-calling
   - Cuantización alta (8-bit) es mejor que baja (4-bit)

2. **Temperatura baja**:  0.2-0.3 para precisión
   - Código requiere determinismo
   - Creatividad no es prioridad

3. **Max tokens alto**:  4000+ para código largo
   - Archivos .Rmd pueden ser extensos
   - Evita respuestas cortadas

4. **System Prompt detallado**:  Incluye reglas específicas
   - Mejora consistencia
   - Reduce errores

### ✅ Uso del Agente

1. **Sé específico en tus solicitudes**: 

   - ❌ "genera un ejercicio"
   - ✅ "genera un ejercicio de mediana, nivel 2, componente aleatorio"

2. **Usa el contexto de la sesión**: 

   - No repitas información ya proporcionada
   - El agente recuerda la conversación

3. **Aprovecha las herramientas**: 

   - Pide al agente que consulte ejemplos funcionales
   - Solicita búsquedas web cuando sea necesario

4. **Valida siempre**: 

   - Pide al agente que valide archivos generados
   - Compila en RStudio para verificar

### ✅ Integración con Flows

1. **Flows para tareas repetitivas**: 

   - Validación siempre sigue los mismos pasos
   - Usa Flows para automatizar

2. **Agente para decisiones**: 

   - El agente decide qué Flow usar
   - Interpreta resultados de Flows

3. **Combina ambos**: 

   - Agente conversacional + Flows estructurados
   - Mejor experiencia de usuario

---

## 🔧 SOLUCIÓN DE PROBLEMAS

### ❌ El agente no responde

**Posibles causas**: 

- Agente no configurado
- LLM no seleccionado
- API key inválida

**Solución**: 

1. Verifica configuración en Workspace Settings → Agent Configuration
2. Asegúrate de haber hecho clic en "Update workspace agent"
3. Verifica que la API key sea válida

---

### ❌ El agente dice que no puede usar herramientas

**Posibles causas**: 

- Modelo LLM inadecuado (muy pequeño o baja cuantización)
- Skills no activadas
- Search Provider no configurado (para Web Browsing)

**Solución**: 

1. Usa un modelo más potente (GPT-4, Claude 3.5)
2. Verifica que las skills estén activadas en "Configure Agent Skills"
3. Configura Search Provider si necesitas Web Browsing

---

### ❌ El agente alucina (inventa información)

**Posibles causas**: 

- Temperatura muy alta
- No usa RAG Search
- Modelo inadecuado

**Solución**: 

1. Reduce temperatura a 0.2-0.3
2. Pide explícitamente que consulte documentos: "consulta los ejemplos funcionales"
3. Usa modelos más precisos

---

### ❌ El agente no guarda archivos

**Posibles causas**: 

- Skill "Save Files" no activada
- El agente alucina que guardó (no llamó la herramienta)

**Solución**: 

1. Activa "Save Files to Browser" en Agent Skills
2. Sé explícito: "usa la herramienta save-file-to-browser para guardar"
3. Verifica en los logs si realmente llamó la herramienta

---

### ❌ El agente no encuentra documentos

**Posibles causas**: 

- Documentos no embebidos en el workspace
- Vector database no configurada

**Solución**: 

1. Embebe los documentos necesarios en el workspace
2. Verifica configuración de Vector Database
3. Re-indexa el workspace si es necesario

---

## 📚 RECURSOS ADICIONALES

### Documentación Oficial

- **AI Agents Overview**:  https://docs.anythingllm.com/agent/overview
- **Agent Setup**:  https://docs.anythingllm.com/agent/setup
- **Agent Usage**:  https://docs.anythingllm.com/agent/usage
- **Custom Skills**:  https://docs.anythingllm.com/agent/custom/introduction

### Archivos del Proyecto

- **Ejemplos funcionales**:  `/A-Produccion/Ejemplos-Funcionales-Rmd/`
- **Biblioteca de soluciones**:  `/Auxiliares/BIBLIOTECA_Soluciones_Errores_Comunes.md`
- **Guía de workflows**:  `AnythingLLM-Config/02-GUIA_Creacion_Workflows_AnythingLLM.md`
- **README AnythingLLM**:  `AnythingLLM-Config/01-README_AnythingLLM_ICFES.md`

---

## 🎯 PRÓXIMOS PASOS

1. ✅ Configurar el agente único en tu workspace "ICFES R-Exams"
2. ✅ Activar todas las skills recomendadas
3. ✅ Configurar el System Prompt personalizado
4. ✅ Embeber documentación del proyecto en el workspace
5. ✅ Crear los 3 Flows principales (si no los has creado)
6. ✅ Activar los Flows como skills del agente
7. ✅ Probar el agente con casos reales
8. ✅ Iterar y mejorar basado en resultados

---

## 📝 RESUMEN EJECUTIVO

### Lo que aprendiste:

1. **Diferencia entre Agentes y Workflows**: 

   - Agentes: Conversacionales, flexibles
   - Workflows: Estructurados, automatizados
   - Trabajan juntos: Agente usa Flows como skills

2. **Configuración de Agentes**: 

   - Un solo agente por workspace
   - Configuración en Workspace Settings → Agent Configuration
   - Seleccionar LLM, activar skills, configurar search provider

3. **Herramientas disponibles**: 

   - RAG Search, Web Browsing, Web Scraping
   - Save Files, List Documents, Summarize Documents
   - Custom Flows (tus workflows)

4. **Uso del Agente**: 

   - Activar con `@agent`
   - Mantiene contexto durante la sesión
   - Terminar con `/exit`

5. **Mejores prácticas**: 

   - Modelos potentes, temperatura baja
   - System Prompt detallado
   - Combinar Agente + Flows

---

**¡Ahora estás listo para usar Agentes en AnythingLLM para tu proyecto ICFES R-Exams!** 🚀

**Versión**:  1.0.0\
**Fecha**:  2025-11-06\
**Ubicación**:  `AnythingLLM-Config/03-GUIA_Creacion_Agentes_AnythingLLM.md`\
**Autor**:  Sistema ICFES R-Exams


