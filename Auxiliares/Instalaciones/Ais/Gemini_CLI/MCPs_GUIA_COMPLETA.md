# 🤖 GUÍA COMPLETA: MCPs PARA GEMINI-ICFES-OPTIMIZADO

## 🎯 INTRODUCCIÓN

Los MCPs (Model Context Protocols) extienden las capacidades de Gemini CLI permitiendo integración automática con herramientas externas. Esta configuración está optimizada específicamente para el proyecto ICFES R-exams.

## 📦 MCPs INSTALADOS Y CONFIGURADOS

### **🧠 Thinking MCP** ⭐ NUEVO
- **Función**: Análisis y razonamiento estructurado paso a paso
- **Activación automática**: "analizar", "razonamiento", "estructurado", "thinking"
- **Uso**: Análisis completo de problemas complejos de optimización de ejercicios
- **Herramientas**: `structured_thinking` - Análisis sistemático con contexto

### **🎭 Playwright MCP (Corregido)** ⭐ ACTUALIZADO
- **Función**: Testing automático de ejercicios web y capturas de pantalla
- **Activación automática**: "testing", "web", "captura", "validación"
- **Uso**: Testing de ejercicios R-exams compilados en HTML, validación visual
- **Herramientas**:
  - `test_web_exercise` - Testing automático de ejercicios HTML
  - `screenshot_exercise` - Capturas de pantalla para validación visual

### **📐 LaTeX Validator MCP** ⭐ NUEVO
- **Función**: Validación de código LaTeX/TikZ para ejercicios R-exams
- **Activación automática**: "latex", "tikz", "validar", "compilar"
- **Uso**: Verificación de sintaxis LaTeX/TikZ, detección de errores comunes
- **Herramientas**:
  - `validate_latex` - Validación de sintaxis LaTeX/TikZ
  - `compile_tikz` - Compilación de código TikZ

### **🖼️ Image Analysis MCP** ⭐ NUEVO
- **Función**: Análisis de imágenes matemáticas para replicación TikZ
- **Activación automática**: "imagen", "análisis", "metadatos", "tikz"
- **Uso**: Análisis de imágenes PNG para generar código TikZ con fidelidad 98%
- **Herramientas**:
  - `analyze_math_image` - Análisis de imágenes matemáticas
  - `extract_image_metadata` - Extracción de metadatos técnicos

### **🔍 Brave Search MCP**
- **Función**: Búsqueda web privada y actualizada
- **Activación automática**: "buscar", "investigar", "información actualizada"
- **Uso**: Investigación de estándares ICFES, documentación oficial MEN

### **📚 Context7 MCP**
- **Función**: Documentación de librerías y APIs
- **Activación automática**: "documentación", "librería", "API", "referencia"
- **Uso**: Consulta de R-exams, TikZ, matplotlib, referencias técnicas

### **💾 Memory MCP**
- **Función**: Gestión de memoria persistente
- **Activación automática**: "recordar", "memoria", "guardar", "persistir"
- **Uso**: Guardar mejores prácticas, configuraciones exitosas, patrones

### **📁 Filesystem MCP**
- **Función**: Acceso a archivos locales del proyecto
- **Activación automática**: "archivo", "directorio", "leer", "escribir"
- **Uso**: Acceso directo a ejemplos funcionales, templates, ejercicios

## 🚀 INSTALACIÓN Y CONFIGURACIÓN

### **Paso 1: Instalar MCPs**
```bash
# Ejecutar desde directorio del proyecto
bash Auxiliares/Instalaciones/Ais/Gemini_CLI/install-mcps.sh
```

### **Paso 2: Configurar Integración**
```bash
# Configurar MCPs con Gemini CLI
bash Auxiliares/Instalaciones/Ais/Gemini_CLI/configure-gemini-mcps.sh
```

### **Paso 3: Configurar Variables de Entorno**
```bash
# Cargar configuración de APIs
source Auxiliares/Instalaciones/Ais/Gemini_CLI/mcp-env-setup.sh

# Configurar APIs opcionales
export UPSTASH_REDIS_REST_URL='tu_url_redis'      # Para Context7
export BRAVE_API_KEY='tu_api_key_brave'           # Para Brave Search
```

### **Paso 4: Testing**
```bash
# Verificar instalación completa
bash Auxiliares/Instalaciones/Ais/Gemini_CLI/test-mcps.sh
```

## 🎮 USO PRÁCTICO

### **Comando Principal**
```bash
# Iniciar Gemini CLI con MCPs integrados
gemini-icfes-mcps

# Cargar contexto completo del proyecto
@Auxiliares/Instalaciones/Ais/Gemini_CLI/GEMINI.md
```

### **Comandos Automáticos por MCP**

#### **🔍 Brave Search - Investigación ICFES**
```
# Activación automática con palabras clave
"buscar información sobre competencias matemáticas ICFES 2025"
"investigar metodologías de evaluación argumentación"
"información actualizada sobre estándares MEN matemáticas"
```

#### **📚 Context7 - Documentación Técnica**
```
# Activación automática con referencias
"documentación de R-exams para ejercicios matemáticos"
"API de TikZ para gráficas geométricas"
"referencia de matplotlib para gráficos estadísticos"
```

#### **🌐 Playwright - Automatización Web**
```
# Activación automática con tareas web
"automatizar navegación web para buscar ejemplos ICFES"
"testing de compilación HTML de ejercicios R-exams"
"scraping de sitio MEN para documentación oficial"
```

#### **💾 Memory - Gestión de Conocimiento**
```
# Activación automática con persistencia
"recordar que este ejercicio evalúa competencia interpretación"
"guardar la configuración de tolerancias para ejercicios numéricos"
"persistir las mejores prácticas identificadas"
```

#### **🧠 Thinking - Análisis Estructurado** ⭐ NUEVO
```
# Activación automática con análisis
"analizar el problema de optimización de ejercicios R-exams ICFES"
"razonamiento estructurado para mejorar la aleatorización"
"thinking sobre la metodología TikZ para fidelidad 98%"
```

#### **🎭 Playwright - Testing Automático** ⭐ ACTUALIZADO
```
# Activación automática con testing
"testing automático del ejercicio compilado en HTML"
"captura de pantalla del gráfico TikZ generado"
"validación visual de la fidelidad del ejercicio"
```

#### **📐 LaTeX Validator - Validación de Código** ⭐ NUEVO
```
# Activación automática con validación
"validar código LaTeX del ejercicio R-exams"
"compilar código TikZ para verificar sintaxis"
"verificar estructura de metadatos YAML"
```

#### **🖼️ Image Analysis - Análisis de Imágenes** ⭐ NUEVO
```
# Activación automática con imágenes
"analizar imagen PNG para replicación TikZ"
"extraer metadatos de imagen matemática"
"análisis de geometría para código TikZ"
```

#### **📁 Filesystem - Acceso a Archivos**
```
# Activación automática con archivos
"leer archivo Auxiliares/Ejemplos-Funcionales-Rmd/ejercicio.Rmd"
"escribir ejercicio en Lab-Manjaro/01-S1-2024B/"
"listar archivos en Auxiliares/TikZ-Documentation/"
```

## 🎯 FLUJOS DE TRABAJO INTEGRADOS

### **Flujo 1: Crear Ejercicio desde Investigación**
```
1. "buscar información sobre competencia formulación ICFES 2025"
   → Brave Search investiga estándares oficiales

2. "documentación de R-exams para ejercicios de álgebra"
   → Context7 proporciona referencias técnicas

3. "leer archivo Auxiliares/Ejemplos-Funcionales-Rmd/algebra_ejemplo.Rmd"
   → Filesystem accede a ejemplos funcionales

4. "recordar las mejores prácticas identificadas"
   → Memory guarda conocimiento para uso futuro
```

### **Flujo 2: Optimizar Ejercicio Existente**
```
1. "leer archivo Lab-Manjaro/01-S1-2024B/ejercicio_actual.Rmd"
   → Filesystem carga ejercicio para análisis

2. "buscar información sobre errores comunes en ejercicios ICFES"
   → Brave Search investiga problemas frecuentes

3. "documentación de TikZ para mejorar gráficas"
   → Context7 proporciona referencias de mejora

4. "testing de compilación HTML del ejercicio optimizado"
   → Playwright verifica funcionamiento

5. "recordar las optimizaciones aplicadas exitosamente"
   → Memory persiste mejoras para futuros ejercicios
```

### **Flujo 3: Validar Estándares ICFES**
```
1. "buscar información actualizada sobre estándares ICFES 2025"
   → Brave Search obtiene documentación oficial

2. "documentación oficial MEN competencias matemáticas"
   → Context7 accede a referencias gubernamentales

3. "leer archivo con ejercicio a validar"
   → Filesystem carga ejercicio para revisión

4. "recordar criterios de validación identificados"
   → Memory guarda estándares para uso consistente
```

### **Flujo 4: Desarrollo Completo con Análisis Estructurado** ⭐ NUEVO
```
1. "analizar el problema de crear ejercicio de geometría ICFES"
   → Thinking MCP realiza análisis estructurado del problema

2. "buscar información sobre competencias geométricas ICFES"
   → Brave Search investiga estándares específicos

3. "documentación de TikZ para figuras geométricas"
   → Context7 proporciona referencias técnicas

4. "leer archivo con ejemplo similar de geometría"
   → Filesystem accede a templates funcionales

5. "validar código LaTeX del ejercicio desarrollado"
   → LaTeX Validator verifica sintaxis y estructura

6. "testing automático del ejercicio compilado"
   → Playwright valida funcionamiento web

7. "recordar metodología exitosa aplicada"
   → Memory persiste proceso para futuros ejercicios
```

### **Flujo 5: Replicación TikZ desde Imagen** ⭐ NUEVO
```
1. "analizar imagen PNG para identificar elementos matemáticos"
   → Image Analysis extrae información técnica

2. "thinking sobre estrategia de replicación TikZ"
   → Thinking MCP estructura plan de desarrollo

3. "documentación de TikZ para elementos identificados"
   → Context7 proporciona sintaxis específica

4. "validar código TikZ generado"
   → LaTeX Validator verifica sintaxis

5. "captura de pantalla del resultado compilado"
   → Playwright genera imagen para comparación

6. "recordar patrón exitoso de replicación"
   → Memory guarda metodología para casos similares
```

## ⚙️ CONFIGURACIÓN AVANZADA

### **Personalizar Activación Automática**
Editar `.gemini-mcp-config.json`:
```json
{
  "mcps": {
    "servers": {
      "brave-search": {
        "auto_trigger": ["buscar", "investigar", "actualizada", "oficial"],
        "custom_triggers": ["ICFES", "MEN", "estándares"]
      }
    }
  }
}
```

### **Configurar Directorios Prioritarios**
```json
{
  "icfes_integration": {
    "priority_directories": [
      "Auxiliares/Ejemplos-Funcionales-Rmd",
      "Auxiliares/TikZ-Documentation",
      "Lab-Manjaro/01-S1-2024B"
    ]
  }
}
```

## 🔧 SOLUCIÓN DE PROBLEMAS

### **Problema: MCP no se activa automáticamente**
```bash
# Verificar configuración
cat .gemini-mcp-config.json

# Usar activación manual
"usar brave-search para buscar información ICFES"
```

### **Problema: Context7 no funciona**
```bash
# Verificar variables de entorno
echo $UPSTASH_REDIS_REST_URL

# Configurar Redis gratuito en Upstash
# https://upstash.com/
```

### **Problema: Playwright falla**
```bash
# Reinstalar dependencias
cd .mcps/playwright-mcp
npm install
npx playwright install
```

### **Problema: Memory no persiste datos**
```bash
# Verificar permisos de escritura
ls -la .mcps/mcp-servers/src/memory/

# Crear directorio de datos si no existe
mkdir -p .mcps/memory-data
```

## 📊 MÉTRICAS Y MONITOREO

### **Verificar Estado de MCPs**
```bash
# Testing completo
bash test-mcps.sh

# Verificar logs de MCPs
tail -f ~/.gemini/mcp-logs/*.log
```

### **Estadísticas de Uso**
- **Brave Search**: Consultas de investigación ICFES
- **Context7**: Referencias técnicas consultadas
- **Playwright**: Tests de compilación ejecutados
- **Memory**: Conocimiento persistido
- **Filesystem**: Archivos accedidos

## 🎉 BENEFICIOS OBTENIDOS

### **Eficiencia Mejorada**
- ✅ **Investigación automática** - Brave Search integrado
- ✅ **Documentación instantánea** - Context7 siempre disponible
- ✅ **Testing automatizado** - Playwright para validación
- ✅ **Conocimiento persistente** - Memory para mejores prácticas
- ✅ **Acceso directo** - Filesystem para ejemplos funcionales

### **Calidad Garantizada**
- ✅ **Estándares actualizados** - Investigación web automática
- ✅ **Referencias técnicas** - Documentación oficial integrada
- ✅ **Validación continua** - Testing automatizado
- ✅ **Consistencia** - Memoria de mejores prácticas

### **Productividad Maximizada**
- ✅ **Flujos integrados** - MCPs trabajan en conjunto
- ✅ **Activación automática** - Sin comandos manuales
- ✅ **Contexto inteligente** - Información relevante siempre disponible
- ✅ **Escalabilidad** - Sistema crece con el proyecto

---

**🎯 Los MCPs transforman gemini-icfes-optimizado en una herramienta integral para crear ejercicios ICFES de máxima calidad con investigación automática, documentación integrada y validación continua.**
