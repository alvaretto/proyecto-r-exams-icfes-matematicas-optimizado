# 📋 GUÍA: Cómo Aplicar la Configuración del Workspace

## ⚠️ ACLARACIÓN IMPORTANTE

El archivo `02-workspace-config.json` **NO se importa ni se carga** en AnythingLLM.

Es un **archivo de referencia** que documenta cómo debería estar configurado tu workspace "ICFES R-Exams".

Toda la configuración se hace **manualmente** a través de la interfaz web de AnythingLLM.

---

## 📋 ÍNDICE

1. [Información del Workspace](#información-del-workspace)
2. [Configuración del LLM](#configuración-del-llm)
3. [Configuración de Embeddings](#configuración-de-embeddings)
4. [Configuración del Agente](#configuración-del-agente)
5. [Documentos a Embeber](#documentos-a-embeber)
6. [System Prompt del Agente](#system-prompt-del-agente)
7. [Workflows (Flows)](#workflows-flows)

---

## 🏢 INFORMACIÓN DEL WORKSPACE

### Del archivo JSON:

```json
"workspace": {
  "name": "ICFES R-Exams",
  "description": "Sistema integral para generar ejercicios matemáticos ICFES usando R-exams",
  "slug": "icfes-r-exams"
}
```

### Dónde configurar en AnythingLLM:

**Ubicación**: 

Workspace Settings → General Settings

**Pasos**:

1. Abre tu workspace "ICFES R-Exams"
2. Haz clic en el ícono de configuración (⚙️) en la esquina superior derecha
3. Selecciona **"General Settings"**
4. Configura:

   - **Workspace Name**: `ICFES R-Exams`
   - **Workspace Description**: `Sistema integral para generar ejercicios matemáticos ICFES usando R-exams`
5. Haz clic en **"Update workspace"**

---

## 🤖 CONFIGURACIÓN DEL LLM

### Del archivo JSON:

```json
"llm_config": {
  "provider": "openai",
  "model": "gpt-4-turbo-preview",
  "temperature": 0.3,
  "max_tokens": 4000,
  "top_p": 0.95,
  "frequency_penalty": 0.0,
  "presence_penalty": 0.0
}
```

### Dónde configurar en AnythingLLM:

**Ubicación**: 

Workspace Settings → Chat Settings → LLM Preference

**Pasos**:

1. Ve a Workspace Settings → **Chat Settings**
2. En la sección **"LLM Preference"**:

   - **LLM Provider**: Selecciona `OpenAI`
   - **Chat Model**: Selecciona `gpt-4-turbo-preview`
3. En **"Chat Settings"** (parámetros avanzados):

   - **Temperature**: `0.3`
   - **Max Tokens**: `4000`
   - **Top P**: `0.95`
   - **Frequency Penalty**: `0.0`
   - **Presence Penalty**: `0.0`
4. Haz clic en **"Update workspace"**

**Nota**: 

Algunos parámetros pueden estar en secciones diferentes según la versión de AnythingLLM.

---

## 📊 CONFIGURACIÓN DE EMBEDDINGS

### Del archivo JSON:

```json
"embedding_config": {
  "provider": "openai",
  "model": "text-embedding-3-small",
  "chunk_size": 1000,
  "chunk_overlap": 200,
  "similarity_threshold": 0.7
}
```

### Dónde configurar en AnythingLLM:

**Ubicación**: 

Workspace Settings → Vector Database

**Pasos**:

1. Ve a Workspace Settings → **Vector Database**
2. Configura:

   - **Embedding Provider**: `OpenAI`
   - **Embedding Model**: `text-embedding-3-small`
3. En **"Document Processing"** (si está disponible):

   - **Chunk Size**: `1000`
   - **Chunk Overlap**: `200`
4. Haz clic en **"Update workspace"**

**Nota**: 

`similarity_threshold` no es configurable en la interfaz. Es un parámetro interno.

---

## 🤖 CONFIGURACIÓN DEL AGENTE

### Del archivo JSON:

```json
"agent_config": {
  "mode": "chat",
  "memory": true,
  "memory_depth": 10,
  "search_provider": "vector",
  "top_k": 5,
  "enable_web_search": false,
  "enable_code_execution": false
}
```

### Dónde configurar en AnythingLLM:

**Ubicación**: 

Workspace Settings → Agent Configuration

**Pasos**:

1. Ve a Workspace Settings → **Agent Configuration**
2. Selecciona el **LLM Provider** y **Model** para el agente:

   - **LLM Provider**: `OpenAI`
   - **Model**: `gpt-4-turbo-preview`
3. Haz clic en **"Update workspace agent"**
4. Haz clic en **"Configure Agent Skills"**
5. Activa las skills necesarias (ver sección siguiente)

**Nota**:

Los parámetros `memory`, `memory_depth`, `top_k` no son configurables directamente. Son valores por defecto del sistema.

---

## 📚 DOCUMENTOS A EMBEBER

### Del archivo JSON:

```json
"documents": {
  "priority_high": [
    ".augment/rules/reglas-generales.md",
    ".augment/rules/siempre.md",
    ".agent.md",
    "Auxiliares/Agentes-IA/01-GUIA_COMPLETA_Agentes_Workflows_ICFES_R_Exams.md",
    "AnythingLLM-Config/01-README_AnythingLLM_ICFES.md"
  ],
  "priority_medium": [
    "Auxiliares/METODOLOGIA_Correccion_Errores_Recurrentes_ICFES_R_Exams.md",
    "Auxiliares/BIBLIOTECA_Soluciones_Errores_Comunes.md",
    "Auxiliares/CHECKLIST_Validacion_Archivos_Rmd.md",
    ...
  ],
  "examples": [
    "Auxiliares/Ejemplos-Funcionales-Rmd/*.Rmd"
  ]
}
```

### Dónde configurar en AnythingLLM:

**Ubicación**:

Workspace → Upload Documents

**Pasos**:

1. Abre tu workspace "ICFES R-Exams"
2. En el panel izquierdo, haz clic en **"Upload Documents"** o el ícono de carpeta
3. Haz clic en **"Upload"** o arrastra archivos
4. Navega a tu proyecto: `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams`
5. Sube los documentos en este orden:

**Prioridad Alta** (subir primero):

- `.augment/rules/reglas-generales.md`
- `.augment/rules/siempre.md`
- `.agent.md`
- `Auxiliares/Agentes-IA/01-GUIA_COMPLETA_Agentes_Workflows_ICFES_R_Exams.md`
- `AnythingLLM-Config/01-README_AnythingLLM_ICFES.md`
- `AnythingLLM-Config/02-GUIA_Creacion_Workflows_AnythingLLM.md`
- `AnythingLLM-Config/03-GUIA_Creacion_Agentes_AnythingLLM.md`

**Prioridad Media** (subir después):

- `Auxiliares/METODOLOGIA_Correccion_Errores_Recurrentes_ICFES_R_Exams.md`
- `Auxiliares/BIBLIOTECA_Soluciones_Errores_Comunes.md`
- `Auxiliares/CHECKLIST_Validacion_Archivos_Rmd.md`
- `Auxiliares/README_AGENTE_TIKZ.md`
- `Auxiliares/plantilla_metadatos_icfes.md`
- `Auxiliares/matriz_alineacion_icfes.md`
- `Auxiliares/guia_implementacion_icfes.md`

**Ejemplos Funcionales** (subir carpeta completa):

- Sube toda la carpeta `Auxiliares/Ejemplos-Funcionales-Rmd/`
- AnythingLLM procesará todos los archivos .Rmd automáticamente

**Documentación Técnica** (opcional):

- `Auxiliares/Python-Documentation/` (toda la carpeta)
- `Auxiliares/Documentacion/` (toda la carpeta)

6. Espera a que AnythingLLM procese y embeba los documentos
7. Verás una barra de progreso durante el procesamiento

**Consejos**:

- Sube documentos en lotes pequeños (5-10 archivos a la vez)
- Espera a que termine el procesamiento antes de subir más
- Los documentos embebidos estarán disponibles para RAG Search

---

## 💬 SYSTEM PROMPT DEL AGENTE

### Del archivo JSON:

El archivo JSON contiene 4 configuraciones de "agentes" diferentes:

```json
"agents": {
  "generador_ejercicios": {
    "system_prompt": "Eres un experto en generar ejercicios matemáticos ICFES..."
  },
  "validador_codigo": {
    "system_prompt": "Eres un experto en validar archivos R-exams..."
  },
  ...
}
```

### ⚠️ IMPORTANTE:

En AnythingLLM, **NO se crean múltiples agentes separados**.

Solo hay **UN agente por workspace** con un **único System Prompt**.

### Dónde configurar en AnythingLLM:

**Ubicación**:

Workspace Settings → Chat Settings → System Prompt

**Pasos**:

1. Ve a Workspace Settings → **Chat Settings**
2. Busca el campo **"System Prompt"**
3. Pega el siguiente prompt unificado:

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

- SIEMPRE consulta los ejemplos funcionales en /Auxiliares/Ejemplos-Funcionales-Rmd/ antes de generar código
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
- Usa tus Flows personalizados para tareas específicas

COMPETENCIAS ICFES:

- interpretacion_representacion
- formulacion_ejecucion
- argumentacion

COMPONENTES ICFES:

- geometrico_metrico
- numerico_variacional
- aleatorio

NIVELES:

1 (básico) a 4 (superior)

FORMATO DE RESPUESTA:

- Responde siempre en español
- Sé preciso y técnico
- Proporciona código completo y funcional
- Explica decisiones técnicas importantes
- Sugiere mejoras cuando sea apropiado
```

4. Haz clic en **"Update workspace"**

**Nota**:

Este prompt unificado combina las capacidades de los 4 "agentes" del JSON en un solo agente versátil.

---

## 🔄 WORKFLOWS (FLOWS)

### Del archivo JSON:

```json
"workflows": {
  "generacion_completa": {
    "name": "Generar Ejercicio Completo",
    "steps": [...]
  },
  "validacion_correccion": {
    "name": "Validar y Corregir",
    "steps": [...]
  },
  "optimizacion_diversidad": {
    "name": "Optimizar Diversidad",
    "steps": [...]
  }
}
```

### Dónde configurar en AnythingLLM:

**Ubicación**:

Main Menu → Flows (o Workflows)

**Pasos**:

1. Haz clic en el menú principal (☰) en la esquina superior izquierda
2. Selecciona **"Flows"** o **"Workflows"**
3. Haz clic en **"New Flow"**
4. Sigue la guía completa en: `02-GUIA_Creacion_Workflows_AnythingLLM.md`

**Workflows a crear**:

1. **Workflow 1: Generación Completa desde Imagen**
   - Configuración completa en la guía de workflows (líneas 156-285)

2. **Workflow 2: Validación y Corrección**
   - Configuración completa en la guía de workflows (líneas 289-518)

3. **Workflow 3: Optimización de Diversidad**
   - Configuración completa en la guía de workflows (líneas 522-729)

**Después de crear los Flows**:

1. Ve a Workspace Settings → Agent Configuration
2. Haz clic en **"Configure Agent Skills"**
3. En la sección **"Custom Flows"**, activa los 3 flows que creaste
4. Haz clic en **"Save Configuration"**

Ahora el agente podrá usar estos flows como herramientas.

---

## 🎯 COMANDOS PERSONALIZADOS

### Del archivo JSON:

```json
"custom_commands": {
  "/generar-ejercicio": {...},
  "/validar": {...},
  "/corregir": {...},
  ...
}
```

### ⚠️ IMPORTANTE:

AnythingLLM **NO soporta comandos personalizados** como `/generar-ejercicio`.

Esta sección del JSON es solo **documentación de referencia** para saber qué comandos te gustaría tener.

### Alternativa:

En lugar de comandos personalizados, usa el agente con lenguaje natural:

| **Comando deseado** | **Alternativa en AnythingLLM** |
|---------------------|-------------------------------|
| `/generar-ejercicio [imagen] [competencia] [nivel]` | `@agent genera un ejercicio de [competencia] nivel [nivel] desde esta imagen` |
| `/validar [archivo.Rmd]` | `@agent valida el archivo [archivo.Rmd]` |
| `/corregir [archivo.Rmd]` | `@agent corrige los errores en [archivo.Rmd]` |
| `/compilar [archivo.Rmd] [formato]` | `@agent compila [archivo.Rmd] en formato [formato]` |
| `/optimizar-diversidad [archivo.Rmd]` | `@agent optimiza la diversidad de [archivo.Rmd] para 300+ versiones` |

---

## ✅ CHECKLIST DE CONFIGURACIÓN

Usa esta lista para verificar que todo está configurado correctamente:

### Información del Workspace

- [ ] Nombre del workspace: "ICFES R-Exams"
- [ ] Descripción configurada

### Configuración del LLM

- [ ] Provider: OpenAI
- [ ] Model: gpt-4-turbo-preview
- [ ] Temperature: 0.3
- [ ] Max Tokens: 4000
- [ ] Top P: 0.95

### Configuración de Embeddings

- [ ] Embedding Provider: OpenAI
- [ ] Embedding Model: text-embedding-3-small
- [ ] Chunk Size: 1000
- [ ] Chunk Overlap: 200

### Configuración del Agente

- [ ] Agente configurado con GPT-4 Turbo
- [ ] System Prompt personalizado aplicado
- [ ] Agent Skills configuradas

### Documentos Embebidos

- [ ] Documentos de prioridad alta subidos
- [ ] Documentos de prioridad media subidos
- [ ] Ejemplos funcionales subidos
- [ ] Documentación técnica subida (opcional)

### Workflows

- [ ] Workflow 1: Generación Completa creado
- [ ] Workflow 2: Validación y Corrección creado
- [ ] Workflow 3: Optimización de Diversidad creado
- [ ] Workflows activados como Agent Skills

### Pruebas

- [ ] Agente responde correctamente con `@agent hola`
- [ ] RAG Search funciona (consulta ejemplos funcionales)
- [ ] Workflows se ejecutan correctamente

---

## 📚 RECURSOS RELACIONADOS

- **Guía de Workflows**: `02-GUIA_Creacion_Workflows_AnythingLLM.md`
- **Guía de Agentes**: `03-GUIA_Creacion_Agentes_AnythingLLM.md`
- **README AnythingLLM**: `01-README_AnythingLLM_ICFES.md`
- **Archivo de referencia**: `02-workspace-config.json`

---

## 🎯 RESUMEN

El archivo `02-workspace-config.json` es una **referencia de configuración**, no un archivo importable.

Usa esta guía para aplicar manualmente cada configuración en la interfaz web de AnythingLLM.

Una vez completada la configuración, tu workspace "ICFES R-Exams" estará listo para generar ejercicios matemáticos de alta calidad.

---

**Versión**: 1.0.0\
**Fecha**: 2025-11-06\
**Ubicación**: `AnythingLLM-Config/04-GUIA_Aplicar_Configuracion_Workspace.md`\
**Autor**: Sistema ICFES R-Exams



