# 🤖 AnythingLLM para ICFES R-Exams

## 📋 ÍNDICE

1. [¿Qué es AnythingLLM?](#qué-es-anythingllm)
2. [Instalación y Configuración](#instalación-y-configuración)
3. [Configuración para el Proyecto](#configuración-para-el-proyecto)
4. [Agentes Especializados](#agentes-especializados)
5. [Workflows Automatizados](#workflows-automatizados)
6. [Integración con el Proyecto](#integración-con-el-proyecto)
7. [Casos de Uso Prácticos](#casos-de-uso-prácticos)

---

## 🎯 ¿QUÉ ES ANYTHINGLLM?

**AnythingLLM** es una plataforma de IA que permite:


✅ **Crear agentes personalizados** con conocimiento específico del proyecto\
✅ **Entrenar con documentación local** (archivos .Rmd, .md, .R, etc.)\
✅ **Ejecutar workflows automatizados** para tareas repetitivas\
✅ **Integrar múltiples LLMs** (GPT-4, Claude, Gemini, etc.)\
✅ **Mantener contexto persistente** del proyecto\

### Beneficios para ICFES R-Exams

- 🎓 **Agente experto** en generación de ejercicios matemáticos
- 📚 **Conocimiento del proyecto** embebido en el sistema
- 🔄 **Workflows automatizados** para validación y compilación
- 🎯 **Consistencia** en la calidad de ejercicios generados
- ⚡ **Velocidad** en el desarrollo

---

## 🚀 INSTALACIÓN Y CONFIGURACIÓN

### Opción 1: Docker (Recomendado)

```bash
# Descargar AnythingLLM
docker pull mintplexlabs/anythingllm

# Crear directorio de datos
mkdir -p ~/anythingllm-data

# Ejecutar AnythingLLM
docker run -d \
  --name anythingllm \
  -p 3001:3001 \
  -v ~/anythingllm-data:/app/server/storage \
  -v /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams:/workspace:ro \
  mintplexlabs/anythingllm
```

### Opción 2: Instalación Local

```bash
# Descargar desde https://anythingllm.com/download
# Instalar según tu sistema operativo (Linux/Windows/Mac)

# En Manjaro/Arch Linux
yay -S anythingllm-desktop
```

### Acceso

Una vez instalado, accede a:

- **URL**: http://localhost:3001
- **Usuario**: Configurar en primer inicio
- **Contraseña**: Configurar en primer inicio

---

## ⚙️ CONFIGURACIÓN PARA EL PROYECTO

### Paso 1: Crear Workspace "ICFES R-Exams"

1. Abrir AnythingLLM en http://localhost:3001
2. Crear nuevo workspace: **"ICFES R-Exams"**
3. Configurar LLM preferido (GPT-4, Claude, Gemini)

### Paso 2: Cargar Documentación del Proyecto

**Documentos a cargar**:


```
📁 Documentación Principal
├── .augment/rules/reglas-generales.md
├── .agent.md
├── Auxiliares/Agentes-IA/01-GUIA_COMPLETA_Agentes_Workflows_ICFES_R_Exams.md
├── Auxiliares/METODOLOGIA_Correccion_Errores_Recurrentes_ICFES_R_Exams.md
├── Auxiliares/BIBLIOTECA_Soluciones_Errores_Comunes.md
├── Auxiliares/CHECKLIST_Validacion_Archivos_Rmd.md
└── Auxiliares/README_AGENTE_TIKZ.md

📁 Ejemplos Funcionales
├── Auxiliares/Ejemplos-Funcionales-Rmd/*.Rmd (todos)

📁 Metadatos ICFES
├── Auxiliares/plantilla_metadatos_icfes.md
├── Auxiliares/matriz_alineacion_icfes.md
└── Auxiliares/guia_implementacion_icfes.md

📁 Documentación Técnica
├── Auxiliares/Python-Documentation/*.md
└── Auxiliares/TikZ-Documentation/*.md (si existe)
```

### Paso 3: Configurar Embeddings

**Configuración recomendada**:

- **Modelo de embeddings**: `text-embedding-3-small` (OpenAI) o `all-MiniLM-L6-v2` (local)
- **Chunk size**: 1000 tokens
- **Chunk overlap**: 200 tokens
- **Vector database**: ChromaDB (incluido)

### Paso 4: Configurar Agente del Workspace

**Configuración del agente**:


```yaml
Nombre: Experto ICFES R-Exams
Descripción: Especialista en generación de ejercicios matemáticos ICFES

Instrucciones del Sistema:

Eres un experto en generar ejercicios matemáticos para el sistema ICFES 
usando el framework R-exams. Tu conocimiento incluye:


1. Estructura completa de archivos .Rmd
2. Metadatos ICFES obligatorios
3. Metodologías TikZ avanzadas
4. Integración Python-R con reticulate
5. Sistema de validación y corrección de errores
6. Generación de 300+ versiones únicas por ejercicio

SIEMPRE consulta los ejemplos funcionales en tu base de conocimiento 
antes de generar código. NUNCA improvises configuraciones técnicas.

Modo: chat
Temperatura: 0.3 (para mayor precisión)
```

---

## 🤖 AGENTES ESPECIALIZADOS

### Agente 1: Generador de Ejercicios

**Configuración**:

```yaml
Nombre: Generador ICFES
Prompt del Sistema: |
  Especialista en generar ejercicios .Rmd completos a partir de imágenes.
  
  PROCESO OBLIGATORIO:

  1. Analizar imagen (Sistema Condicional Automático)
  2. Consultar ejemplos funcionales similares
  3. Identificar competencia ICFES apropiada
  4. Generar código .Rmd completo
  5. Validar estructura y metadatos
  
  RESTRICCIONES:

  - NUNCA usar set.seed() fijo
  - SIEMPRE incluir test de diversidad (300+ versiones)
  - OBLIGATORIO metadatos ICFES completos
  - REQUERIDO consultar ejemplos funcionales PRIMERO

Herramientas: 
  - Búsqueda en documentación
  - Generación de código
  - Validación de sintaxis
```

### Agente 2: Validador y Corrector

**Configuración**:

```yaml
Nombre: Validador ICFES
Prompt del Sistema: |
  Especialista en validar y corregir archivos .Rmd existentes.
  
  PROCESO:

  1. Leer archivo completo
  2. Identificar errores por categoría
  3. Consultar biblioteca de soluciones
  4. Aplicar correcciones validadas
  5. Re-validar y confirmar
  
  FUENTES DE REFERENCIA:

  - /Auxiliares/Ejemplos-Funcionales-Rmd/
  - /Auxiliares/BIBLIOTECA_Soluciones_Errores_Comunes.md
  - /Auxiliares/CHECKLIST_Validacion_Archivos_Rmd.md

Herramientas:

  - Búsqueda en documentación
  - Análisis de código
  - Corrección automática
```

### Agente 3: Experto en TikZ

**Configuración**:

```yaml
Nombre: Graficador TikZ
Prompt del Sistema: |
  Experto en generar código TikZ profesional desde imágenes.
  
  OBJETIVO: 98%+ fidelidad visual
  
  METODOLOGÍA:

  1. Analizar imagen (geometría, colores, texto)
  2. Consultar templates TikZ apropiados
  3. Generar código TikZ parametrizado
  4. Validar compilación
  5. Medir fidelidad visual
  
  TEMPLATES DISPONIBLES:

  - /Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/

Herramientas:

  - Análisis de imágenes
  - Generación de código TikZ
  - Validación de sintaxis
```

### Agente 4: Clasificador ICFES

**Configuración**:

```yaml
Nombre: Clasificador ICFES
Prompt del Sistema: |
  Especialista en clasificación de ejercicios según estándares ICFES.
  
  COMPETENCIAS:

  - interpretacion_representacion
  - formulacion_ejecucion
  - argumentacion
  
  COMPONENTES:

  - geometrico_metrico
  - numerico_variacional
  - aleatorio
  
  NIVELES: 1 (básico) a 4 (superior)
  
  PROCESO:

  1. Analizar contenido matemático
  2. Identificar tipo de razonamiento
  3. Determinar competencia principal
  4. Asignar nivel de dificultad
  5. Generar metadatos completos

Herramientas:

  - Análisis de contenido
  - Clasificación automática
  - Generación de metadatos
```

---

## 🔄 WORKFLOWS AUTOMATIZADOS

### Workflow 1: Generación Completa desde Imagen

**Configuración en AnythingLLM**:


```yaml
Nombre: Generar Ejercicio Completo
Trigger: Usuario sube imagen PNG

Pasos:

  1. Análisis de Imagen
     Agente: Generador ICFES
     Acción: Detectar contenido gráfico y concepto matemático

  2. Clasificación ICFES
     Agente: Clasificador ICFES
     Acción: Determinar competencia, nivel, componente

  3. Generación de Código
     Agente: Generador ICFES
     Acción: Crear archivo .Rmd completo

  4. Generación de Gráficos (si aplica)
     Agente: Graficador TikZ
     Acción: Generar código TikZ con 98%+ fidelidad

  5. Validación
     Agente: Validador ICFES
     Acción: Verificar estructura, sintaxis, metadatos

  6. Compilación de Prueba
     Acción: Compilar a HTML, PDF, Moodle

  7. Entrega
     Acción: Guardar archivo y presentar resultado
```

### Workflow 2: Validación y Corrección

**Configuración**:


```yaml
Nombre: Validar y Corregir Ejercicio
Trigger: Usuario proporciona archivo .Rmd

Pasos:

  1. Diagnóstico
     Agente: Validador ICFES
     Acción: Identificar errores por categoría

  2. Consulta de Soluciones
     Agente: Validador ICFES
     Acción: Buscar en biblioteca de soluciones

  3. Corrección
     Agente: Validador ICFES
     Acción: Aplicar correcciones validadas

  4. Re-validación
     Agente: Validador ICFES
     Acción: Verificar correcciones aplicadas

  5. Compilación
     Acción: Compilar en múltiples formatos

  6. Reporte
     Acción: Generar reporte de cambios
```

### Workflow 3: Optimización de Diversidad

**Configuración**:


```yaml
Nombre: Optimizar Diversidad de Versiones
Trigger: Usuario solicita optimización

Pasos:

  1. Análisis de generar_datos()
     Agente: Validador ICFES
     Acción: Identificar parámetros aleatorizables

  2. Ampliación de Rangos
     Agente: Generador ICFES
     Acción: Expandir rangos de variación

  3. Nuevos Contextos
     Agente: Generador ICFES
     Acción: Agregar contextos alternativos

  4. Colores Aleatorios
     Agente: Graficador TikZ
     Acción: Implementar paletas aleatorias

  5. Test de Diversidad
     Acción: Ejecutar test (objetivo: 300+ versiones)

  6. Validación
     Agente: Validador ICFES
     Acción: Confirmar funcionamiento
```

---

## 🔗 INTEGRACIÓN CON EL PROYECTO

### Configuración de Rutas

Crear archivo `AnythingLLM-Config/02-workspace-config.json`:


```json
{
  "workspace": {
    "name": "ICFES R-Exams",
    "description": "Sistema integral para ejercicios matemáticos ICFES",
    "slug": "icfes-r-exams"
  },

  "paths": {
    "project_root": "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams",
    "examples": "Auxiliares/Ejemplos-Funcionales-Rmd",
    "documentation": "Auxiliares",
    "lab": "Lab-Manjaro",
    "output": "salida"
  },

  "documents": {
    "priority_high": [
      ".augment/rules/reglas-generales.md",
      ".agent.md",
      "Auxiliares/Agentes-IA/01-GUIA_COMPLETA_Agentes_Workflows_ICFES_R_Exams.md"
    ],
    "priority_medium": [
      "Auxiliares/METODOLOGIA_Correccion_Errores_Recurrentes_ICFES_R_Exams.md",
      "Auxiliares/BIBLIOTECA_Soluciones_Errores_Comunes.md",
      "Auxiliares/CHECKLIST_Validacion_Archivos_Rmd.md"
    ],
    "examples": [
      "Auxiliares/Ejemplos-Funcionales-Rmd/*.Rmd"
    ]
  },

  "llm_config": {
    "provider": "openai",
    "model": "gpt-4-turbo-preview",
    "temperature": 0.3,
    "max_tokens": 4000,
    "top_p": 0.95
  },

  "embedding_config": {
    "provider": "openai",
    "model": "text-embedding-3-small",
    "chunk_size": 1000,
    "chunk_overlap": 200
  },

  "agent_config": {
    "mode": "chat",
    "memory": true,
    "search_provider": "vector",
    "top_k": 5
  }
}
```

### Comandos Personalizados

Crear archivo `AnythingLLM-Config/03-custom-commands.md`:


```markdown
# Comandos Personalizados para AnythingLLM

## Generación de Ejercicios

### Comando: /generar-ejercicio
**Sintaxis**: `/generar-ejercicio [imagen] [competencia] [nivel]`
**Ejemplo**: `/generar-ejercicio estadistica01.png interpretacion_representacion 2`

**Proceso**:

1. Analizar imagen
2. Aplicar sistema condicional
3. Generar código .Rmd completo
4. Validar y compilar

---

## Validación y Corrección

### Comando: /validar
**Sintaxis**: `/validar [archivo.Rmd]`
**Ejemplo**: `/validar ejercicio_estadistica.Rmd`

**Proceso**:

1. Leer archivo
2. Ejecutar validaciones
3. Identificar errores
4. Proporcionar reporte

### Comando: /corregir
**Sintaxis**: `/corregir [archivo.Rmd]`
**Ejemplo**: `/corregir ejercicio_estadistica.Rmd`

**Proceso**:

1. Validar archivo
2. Consultar soluciones
3. Aplicar correcciones
4. Re-validar

---

## Compilación

### Comando: /compilar
**Sintaxis**: `/compilar [archivo.Rmd] [formato]`
**Ejemplo**: `/compilar ejercicio.Rmd html`
**Formatos**: html, pdf, moodle, nops, todos

---

## Optimización

### Comando: /optimizar-diversidad
**Sintaxis**: `/optimizar-diversidad [archivo.Rmd]`
**Ejemplo**: `/optimizar-diversidad ejercicio.Rmd`

**Proceso**:

1. Analizar función generar_datos()
2. Ampliar parámetros
3. Ejecutar test de diversidad
4. Validar 300+ versiones

---

## Consultas

### Comando: /consultar-ejemplo
**Sintaxis**: `/consultar-ejemplo [tema]`
**Ejemplo**: `/consultar-ejemplo mediana estadística`

**Acción**: Buscar en ejemplos funcionales

### Comando: /consultar-error
**Sintaxis**: `/consultar-error [tipo-error]`
**Ejemplo**: `/consultar-error sintaxis TikZ`

**Acción**: Buscar en biblioteca de soluciones
```

---

## 💡 CASOS DE USO PRÁCTICOS

### Caso 1: Generar Ejercicio desde Imagen

**En AnythingLLM**:

```
Usuario: Genera un ejercicio de estadística a partir de esta imagen
[adjuntar imagen de gráfico de barras]

Competencia: interpretacion_representacion
Nivel: 2
```

**Respuesta del Agente**:

1. Analiza la imagen
2. Detecta gráfico de barras
3. Consulta ejemplos funcionales similares
4. Genera código .Rmd completo
5. Valida estructura y metadatos
6. Compila a HTML para verificación
7. Entrega archivo listo para usar

---

### Caso 2: Corregir Ejercicio con Errores

**En AnythingLLM**:

```
Usuario: Este ejercicio tiene errores de compilación
[adjuntar archivo .Rmd]

Por favor valida y corrige
```

**Respuesta del Agente**:

1. Lee el archivo completo
2. Identifica errores:

   - Falta configuración de locale
   - Sintaxis TikZ incorrecta
   - Metadatos ICFES incompletos
3. Consulta ejemplos funcionales
4. Aplica correcciones:

   - Agrega Sys.setlocale()
   - Corrige código TikZ
   - Completa metadatos
5. Re-valida
6. Compila exitosamente
7. Entrega archivo corregido

---

### Caso 3: Optimizar Diversidad de Versiones

**En AnythingLLM**:

```
Usuario: Este ejercicio solo genera 150 versiones únicas.
Necesito optimizarlo para 300+

[adjuntar archivo .Rmd]
```

**Respuesta del Agente**:

1. Analiza función generar_datos()
2. Identifica parámetros limitados
3. Propone mejoras:

   - Ampliar rangos numéricos
   - Agregar contextos alternativos
   - Implementar colores aleatorios
   - Diversificar nombres/objetos
4. Implementa cambios
5. Ejecuta test de diversidad
6. Confirma 300+ versiones únicas
7. Entrega archivo optimizado

---

## 📊 MÉTRICAS Y MONITOREO

### Dashboard de AnythingLLM

**Métricas a monitorear**:

- 📈 Ejercicios generados por día
- ✅ Tasa de éxito en compilación
- 🔄 Promedio de versiones únicas
- ⏱️ Tiempo promedio de generación
- 🎯 Fidelidad visual (gráficos TikZ)

### Configurar Reportes

En AnythingLLM, configurar reportes semanales:

```yaml
Reporte Semanal:

  - Total de ejercicios generados
  - Ejercicios validados exitosamente
  - Errores más comunes encontrados
  - Tiempo ahorrado vs. proceso manual
  - Recomendaciones de mejora
```

---

## 🔧 TROUBLESHOOTING

### Problema: AnythingLLM no encuentra documentos

**Solución**:

1. Verificar que los archivos estén en las rutas correctas
2. Re-indexar workspace
3. Verificar permisos de lectura

### Problema: Agente genera código incorrecto

**Solución**:

1. Verificar que ejemplos funcionales estén cargados
2. Ajustar temperatura del LLM (reducir a 0.2-0.3)
3. Mejorar prompt del sistema con restricciones más específicas

### Problema: Compilación falla

**Solución**:

1. Usar comando `/validar` primero
2. Verificar que todas las dependencias estén instaladas
3. Consultar logs de compilación

---

## 📚 RECURSOS ADICIONALES

### Documentación Oficial
- **AnythingLLM**: https://docs.anythingllm.com/
- **R-exams**: https://www.r-exams.org/
- **ICFES**: https://www.icfes.gov.co/

### Comunidad
- **GitHub AnythingLLM**: https://github.com/Mintplex-Labs/anything-llm
- **Discord**: https://discord.gg/anythingllm

---

## 🎯 PRÓXIMOS PASOS

1. ✅ Instalar AnythingLLM (Docker o local)
2. ✅ Crear workspace "ICFES R-Exams"
3. ✅ Cargar documentación del proyecto
4. ✅ Configurar agentes especializados
5. ✅ Probar con ejercicio simple
6. ✅ Iterar y mejorar configuración

---

**¡AnythingLLM está listo para potenciar tu proyecto ICFES R-Exams!** 🚀

**Versión**: 1.0.0\
**Fecha**: 2025-11-06\
**Ubicación**: `AnythingLLM-Config/01-README_AnythingLLM_ICFES.md`

