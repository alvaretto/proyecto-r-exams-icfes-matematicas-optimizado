# Manual de Usuario: Gemini CLI para R-exams ICFES

**Versión:** 1.0 | **Fecha:** Agosto 2025 | **Proyecto:** RepositorioMatematicasICFES_R_Exams

---

## 📚 **ÍNDICE DE CONTENIDOS**

1. [**Introducción Rápida**](#-1-introducción-rápida) - Qué es y por qué usarlo
2. [**Requisitos Previos**](#-2-requisitos-previos) - Verificar configuración
3. [**Comandos Básicos Esenciales**](#-3-comandos-básicos-esenciales) - Los 5 comandos más importantes
4. [**Modo Avanzado con MCPs**](#-4-modo-avanzado-con-mcps) - Capacidades extendidas
5. [**Modo Experto con Extensión VSCode**](#-5-modo-experto-con-extensión-vscode) - Integración directa con IDE
6. [**Casos de Uso Frecuentes**](#-6-casos-de-uso-frecuentes) - Ejemplos paso a paso
7. [**Solución de Problemas**](#-7-solución-de-problemas-comunes) - Errores típicos y soluciones
8. [**Referencias Rápidas**](#-8-referencias-rápidas) - Tabla de comandos y atajos
9. [**Ejemplos Prácticos**](#-9-ejemplos-prácticos-paso-a-paso) - Casos completos
10. [**Consejos Avanzados**](#-10-consejos-avanzados) - Optimización y productividad
11. [**Checklist de Calidad**](#-11-checklist-de-calidad) - Verificación antes de usar
12. [**Soporte y Recursos**](#-12-soporte-y-recursos) - Ayuda adicional

**⏱️ Tiempo estimado de lectura:** 15-20 minutos | **🎯 Objetivo:** Uso productivo inmediato

---

## ⚡ **INICIO RÁPIDO (2 MINUTOS)**

**¿Tienes prisa? Estos comandos te permiten empezar inmediatamente:**

### **🚀 MODO RECOMENDADO (CON MCPs)**
```bash
# 1. Verificar que todo funciona
.gemini/scripts/verify_setup.sh

# 2. Iniciar Gemini CLI con capacidades extendidas
gemini-icfes --mcps

# 3. Comando inteligente con investigación automática
"buscar información sobre competencias matemáticas ICFES 2025"
```

### **📋 MODO BÁSICO (SIN MCPs)**
```bash
# 1. Verificar configuración
.gemini/scripts/verify_setup.sh

# 2. Iniciar modo básico
gemini-icfes --basic

# 3. Analizar con contexto local
gemini --context-file ".gemini/rules-gemini.md" "Explica las competencias ICFES matemáticas"
```

**Si los comandos funcionan, ¡ya puedes usar Gemini CLI!** Continúa leyendo para aprovechar todas las capacidades.

---

## 🎯 **1. INTRODUCCIÓN RÁPIDA**

### **¿Qué es Gemini CLI?**
Gemini CLI es una herramienta de inteligencia artificial que te permite analizar, generar y optimizar ejercicios matemáticos para el ICFES usando el poder de Google Gemini 2.5 Pro directamente desde tu terminal.

### **¿Por qué usarlo para R-exams ICFES?**
- 🧠 **Contexto masivo**: 1M tokens (5x mayor que otras herramientas IA)
- 🎨 **Generación TikZ**: Código TikZ con fidelidad visual del 98%
- 📊 **Validación ICFES**: Verifica automáticamente estándares y competencias
- ⚡ **Análisis profundo**: Evalúa ejercicios completos con metodologías del proyecto
- 🔍 **Análisis de imágenes**: Convierte gráficos matemáticos en código TikZ

### **Tiempo de aprendizaje:** 15-20 minutos para uso productivo

---

## ✅ **2. REQUISITOS PREVIOS**

### **Verificar Configuración Completa**

Antes de empezar, ejecuta el script de verificación:

```bash
# Desde el directorio raíz del proyecto
.gemini/scripts/verify_setup.sh
```

**Debes ver estos resultados:**

- ✅ Gemini CLI instalado
- ✅ API Key configurada
- ✅ Archivos de contexto creados
- ✅ Configuración Pro activa (1M tokens)

### **Si hay problemas:**

- Consulta el tutorial completo: `Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-cli-r-exams.md`
- Revisa la configuración: `~/.config/gemini/icfes-config.json`

---

## 🚀 **3. COMANDOS BÁSICOS ESENCIALES**

### **Comando 1: Iniciar Gemini CLI**
```bash
# Modo con MCPs (RECOMENDADO - capacidades extendidas)
gemini-icfes --mcps

# Modo básico (para empezar)
gemini-icfes --basic

# Modo optimizado (con verificaciones avanzadas)
gemini-icfes --optimized
```

### **Comando 2: Análisis con Contexto del Proyecto**
```bash
# Cargar contexto completo del proyecto
gemini --context-file "GEMINI.md" "Tu pregunta aquí"
```

### **Comando 3: Análisis de Ejercicio Específico**
```bash
# Analizar un ejercicio R-exams
gemini --context-file "ruta/ejercicio.Rmd" \
       --context-file ".gemini/rules-gemini.md" \
       "Analiza este ejercicio según estándares ICFES"
```

### **Comando 4: Análisis de Imagen para TikZ**
```bash
# Generar código TikZ desde imagen
gemini --image "ruta/imagen.png" \
       --context-file ".gemini/rules-gemini.md" \
       "Genera código TikZ con fidelidad 98%"
```

### **Comando 5: Ayuda y Comandos Disponibles**
```bash
# Ver ayuda del script unificado
gemini-icfes --help

# Ver ayuda de Gemini CLI
gemini --help
```

---

## 🚀 **4. MODO AVANZADO CON MCPs**

### **¿Qué son los MCPs?**
MCPs (Model Context Protocols) son extensiones que dotan a Gemini CLI de capacidades adicionales como investigación web, acceso a documentación técnica, gestión de memoria y automatización. **Son especialmente útiles para desarrollo de ejercicios R-exams ICFES.**

### **🎯 5 MCPs Integrados**

#### **🔍 Brave Search - Investigación Automática**
- **Función**: Búsqueda web privada y actualizada
- **Activación**: "buscar", "investigar", "información actualizada"
- **Uso ICFES**: Estándares actualizados, metodologías oficiales MEN

#### **📚 Context7 - Documentación Técnica**
- **Función**: Acceso a documentación de librerías y APIs
- **Activación**: "documentación", "librería", "API", "referencia"
- **Uso ICFES**: R-exams, TikZ, matplotlib, referencias técnicas

#### **🌐 Playwright - Testing Automático**
- **Función**: Automatización web y testing de ejercicios
- **Activación**: "testing", "compilar", "verificar funcionamiento"
- **Uso ICFES**: Validar compilación HTML/PDF de ejercicios

#### **💾 Memory - Gestión de Conocimiento**
- **Función**: Memoria persistente de mejores prácticas
- **Activación**: "recordar", "guardar", "persistir"
- **Uso ICFES**: Patrones exitosos, configuraciones optimizadas

#### **📁 Filesystem - Acceso a Archivos**
- **Función**: Lectura/escritura directa de archivos del proyecto
- **Activación**: "leer archivo", "escribir", "listar archivos"
- **Uso ICFES**: Acceso a ejemplos funcionales, templates, ejercicios

### **🔄 Comparación: Con vs Sin MCPs**

| **Aspecto** | **Sin MCPs** | **Con MCPs** |
|-------------|--------------|--------------|
| **Investigación** | Manual en navegador | Automática integrada |
| **Documentación** | Buscar referencias externas | Acceso directo integrado |
| **Testing** | Manual en R/terminal | Automatizado en Gemini CLI |
| **Memoria** | Notas externas | Persistencia automática |
| **Archivos** | Copiar/pegar contenido | Acceso directo por nombre |
| **Eficiencia** | Múltiples herramientas | Todo integrado |

### **⚡ Comandos MCPs Esenciales**

#### **Investigación ICFES**
```bash
# Iniciar con MCPs
gemini-icfes --mcps

# Comandos automáticos (dentro de Gemini CLI)
"buscar información sobre competencias matemáticas ICFES 2025"
"investigar metodologías de evaluación argumentación matemática"
"información actualizada sobre estándares MEN matemáticas"
```

#### **Documentación Técnica**
```bash
# Acceso a referencias (activación automática)
"documentación de R-exams para ejercicios matemáticos"
"API de TikZ para gráficas geométricas"
"referencia de matplotlib para gráficos estadísticos"
```

#### **Acceso a Archivos del Proyecto**
```bash
# Lectura directa de archivos
"leer archivo A-Produccion/Ejemplos-Funcionales-Rmd/ejercicio.Rmd"
"listar archivos en Auxiliares/TikZ-Documentation/"
"escribir ejercicio optimizado en Lab-Manjaro/01-S1-2024B/"
```

#### **Gestión de Conocimiento**
```bash
# Memoria persistente
"recordar que este ejercicio evalúa competencia interpretación"
"guardar la configuración de tolerancias para ejercicios numéricos"
"persistir las mejores prácticas identificadas"
```

#### **Testing Automático**
```bash
# Validación de ejercicios
"testing de compilación HTML del ejercicio optimizado"
"verificar funcionamiento de ejercicio en formato PDF"
"automatizar testing de múltiples versiones del ejercicio"
```

### **🎯 Flujo de Trabajo Típico con MCPs**

```
1. INVESTIGACIÓN → "buscar estándares ICFES actualizados"
         ↓
2. DOCUMENTACIÓN → "documentación de R-exams para álgebra"
         ↓
3. ACCESO ARCHIVOS → "leer archivo ejemplo_algebra.Rmd"
         ↓
4. DESARROLLO → Crear/optimizar ejercicio con contexto completo
         ↓
5. TESTING → "testing de compilación del ejercicio"
         ↓
6. MEMORIA → "recordar mejores prácticas aplicadas"
```

### **✅ Verificar MCPs Disponibles**
```bash
# Verificar que MCPs están configurados
.gemini/scripts/verify_setup.sh

# Testing específico de MCPs
bash Auxiliares/Instalaciones/Ais/Gemini_CLI/test-mcps.sh

# Iniciar con MCPs
gemini-icfes --mcps
```

---

## 🎯 **5. MODO EXPERTO CON EXTENSIÓN VSCODE**

### **¿Qué es la Extensión Gemini CLI Companion?**
La extensión oficial `google.gemini-cli-vscode-ide-companion` proporciona integración directa entre Gemini CLI y VSCode Insiders, permitiendo que Gemini "vea" y "edite" archivos directamente en tu IDE. **Es el nivel más avanzado de integración para desarrollo de ejercicios R-exams ICFES.**

### **🚀 Capacidades Exclusivas de la Extensión**

#### **👁️ Visibilidad Completa del Proyecto**
- **Acceso directo**: Gemini puede leer y editar archivos .Rmd, .Rnw, .tikz
- **Comprensión contextual**: Entiende la estructura completa del proyecto
- **Navegación inteligente**: Acceso automático a templates y ejemplos

#### **🧠 Memoria Semántica Automática**
- **Análisis del repositorio**: Comprende automáticamente metodologías y estructura
- **Generación de contexto**: Crea documentación automática del proyecto
- **Persistencia inteligente**: Memoria específica por tipo de ejercicio

#### **💬 Gestión Avanzada de Conversaciones**
- **Sesiones persistentes**: Guarda y recupera conversaciones sobre ejercicios específicos
- **Continuidad de trabajo**: Retoma desarrollo donde lo dejaste
- **Organización por proyecto**: Memoria separada para diferentes tipos de ejercicios

### **⚡ Comandos Específicos de la Extensión**

#### **Comando 1: Activación del Proyecto**
```bash
# Ejecutar en terminal integrado de VSCode Insiders
/id install
```
**Resultado**: Gemini CLI obtiene acceso completo al proyecto actual

#### **Comando 2: Inicialización Semántica**
```bash
# Construir memoria semántica automática del proyecto
/init
```
**Resultado**: Gemini comprende automáticamente:
- Estructura de ejercicios en `A-Produccion/Ejemplos-Funcionales-Rmd/`
- Metodologías en `Auxiliares/METODOLOGIA_*.md`
- Templates TikZ en `Auxiliares/TikZ-Documentation/`
- Configuraciones en `.gemini/`

#### **Comando 3: Gestión de Memoria Avanzada**
```bash
# Ver memorias disponibles del proyecto
/memory show

# Agregar contexto específico para R-exams ICFES
/memory add "Configuración exitosa para ejercicios de álgebra con aleatorización sample()"

# Actualizar memoria con cambios recientes
/memory refresh
```

#### **Comando 4: Gestión de Conversaciones**
```bash
# Guardar conversación actual
/chat save "desarrollo_ejercicio_geometria"

# Listar conversaciones guardadas
/chat list

# Reanudar conversación específica
/chat resume "desarrollo_ejercicio_geometria"

# Eliminar conversación antigua
/chat delete "sesion_antigua"
```

### **🔄 Workflow Completo con Extensión VSCode**

#### **Desarrollo de Ejercicio Nuevo (Método Experto)**
```
1. ACTIVACIÓN → /id install (en terminal VSCode)
2. INICIALIZACIÓN → /init (memoria semántica automática)
3. MEMORIA ESPECÍFICA → /memory add "contexto del ejercicio"
4. DESARROLLO → Edición directa en VSCode con asistencia Gemini
5. PERSISTENCIA → /chat save "nombre_ejercicio"
6. VALIDACIÓN → Testing integrado con feedback directo
```

#### **Continuación de Trabajo Previo**
```
1. RECUPERACIÓN → /chat resume "ejercicio_anterior"
2. ACTUALIZACIÓN → /memory refresh
3. DESARROLLO → Continuar donde se quedó
4. NUEVA MEMORIA → /memory add "mejoras aplicadas"
5. GUARDADO → /chat save "ejercicio_actualizado"
```

### **🎯 Casos de Uso Específicos para R-exams ICFES**

#### **Desarrollo de Ejercicio de Álgebra**
```bash
# 1. Activar proyecto
/id install

# 2. Inicializar memoria
/init

# 3. Agregar contexto específico
/memory add "Ejercicio álgebra: ecuaciones cuadráticas, competencia formulación-ejecución, nivel 2-3 ICFES"

# 4. Desarrollo con asistencia directa
# Gemini puede ahora editar directamente archivos .Rmd en VSCode

# 5. Guardar sesión
/chat save "algebra_ecuaciones_cuadraticas"
```

#### **Optimización de Gráfico TikZ**
```bash
# 1. Recuperar sesión de TikZ
/chat resume "desarrollo_tikz_funciones"

# 2. Agregar memoria de patrón exitoso
/memory add "TikZ exitoso: scale=1.0, font=\\bfseries\\itshape, fidelidad 98% alcanzada"

# 3. Aplicar optimizaciones directamente en archivo .tikz
# Gemini edita el código TikZ en tiempo real

# 4. Persistir mejoras
/memory add "Patrón optimizado para funciones cuadráticas guardado"
```

### **✅ Verificar Extensión Disponible**
```bash
# Verificar que la extensión está instalada
code-insiders --list-extensions | grep gemini

# Resultado esperado:
# google.gemini-cli-vscode-ide-companion
```

### **🔧 Configuración Recomendada**
La extensión ya está configurada en nuestro proyecto. Configuración actual en `.vscode/settings.json`:
```json
{
    "gemini.apiKey": "${env:GEMINI_API_KEY}",
    "gemini.model": "gemini-2.5-pro",
    "gemini.maxTokens": 1000000,
    "gemini.projectContext": {
        "name": "RepositorioMatematicasICFES_R_Exams",
        "type": "educational",
        "framework": "R-exams"
    }
}
```

---

## 📚 **5. CASOS DE USO FRECUENTES**

### **CASO 1: Analizar un Ejercicio R-exams Existente**

**Objetivo:** Evaluar la calidad y alineación ICFES de un ejercicio.

#### **🚀 MÉTODO AVANZADO (CON MCPs) - RECOMENDADO**
```bash
# 1. Iniciar con MCPs
gemini-icfes --mcps

# 2. Análisis integral con investigación automática (dentro de Gemini CLI)
"buscar información sobre estándares ICFES actualizados para competencia interpretación"
"leer archivo A-Produccion/Ejemplos-Funcionales-Rmd/ejercicio.Rmd"
"documentación de R-exams para validar sintaxis"
"recordar mejores prácticas identificadas en análisis previos"
```

#### **📋 MÉTODO BÁSICO (SIN MCPs)**
```bash
# 1. Navegar al directorio
cd A-Produccion/Ejemplos-Funcionales-Rmd/

# 2. Análisis con contexto local
gemini --context-file "ejercicio.Rmd" \
       --context-file "../.gemini/rules-gemini.md" \
       "Analiza este ejercicio R-exams: estructura técnica, competencia ICFES, nivel de dificultad y calidad de distractores"
```

**Resultado esperado:**
- ✅ Evaluación de sintaxis R-exams
- ✅ Verificación de competencia ICFES con estándares actualizados
- ✅ Análisis de nivel de dificultad
- ✅ Sugerencias de mejora basadas en mejores prácticas
- ✅ **Con MCPs**: Investigación automática + memoria persistente

### **CASO 2: Generar Código TikZ desde Imagen**

**Objetivo:** Convertir una imagen matemática en código TikZ reutilizable.

#### **🚀 MÉTODO AVANZADO (CON MCPs) - RECOMENDADO**
```bash
# 1. Iniciar con MCPs
gemini-icfes --mcps

# 2. Análisis integral con documentación automática (dentro de Gemini CLI)
"documentación de TikZ para gráficas matemáticas"
"leer archivo ruta/a/tu/imagen.png"
"buscar ejemplos similares de gráficas TikZ en ICFES"
"recordar patrones exitosos de replicación gráfica"
```

#### **📋 MÉTODO BÁSICO (SIN MCPs)**
```bash
# 1. Verificar imagen disponible
ls ruta/a/tu/imagen.png

# 2. Generar código TikZ
gemini --image "ruta/a/tu/imagen.png" \
       --context-file ".gemini/rules-gemini.md" \
       "Analiza esta imagen matemática y genera código TikZ equivalente con fidelidad 98%. Usa elementos en negrita cursiva y escala apropiada."
```

**Resultado esperado:**
- ✅ Código TikZ completo y compilable
- ✅ Fidelidad visual del 98%
- ✅ Elementos de texto en negrita cursiva
- ✅ Comentarios explicativos
- ✅ **Con MCPs**: Documentación TikZ integrada + ejemplos similares + memoria de patrones

### **CASO 3: Validar Estándares ICFES**

**Objetivo:** Verificar que un ejercicio cumple con los estándares ICFES.

**Pasos:**

1. Carga el contexto de competencias ICFES:
   ```bash
   gemini --context-file "GEMINI.md" \
          --context-file "ejercicio.Rmd" \
          "Valida este ejercicio contra los estándares ICFES matemáticas: competencia, nivel, contexto colombiano y distractores"
   ```

**Resultado esperado:**

- Verificación de competencia (interpretación/formulación/argumentación)
- Validación de nivel (1-4)
- Evaluación de contexto colombiano
- Análisis de calidad de distractores

### **CASO 4: Optimizar Código R-exams**

**Objetivo:** Mejorar la aleatorización y estructura de un ejercicio.

**Pasos:**

1. Analiza el ejercicio actual:
   ```bash
   gemini --context-file "ejercicio.Rmd" \
          --context-file ".gemini/rules-gemini.md" \
          "Optimiza este ejercicio R-exams: mejora la aleatorización para generar 300+ versiones únicas, optimiza el código R y sugiere mejoras en la estructura"
   ```

**Resultado esperado:**

- Código R optimizado
- Mejor aleatorización de parámetros
- Sugerencias de estructura
- Validación de 300+ versiones únicas

---

## 🔧 **5. SOLUCIÓN DE PROBLEMAS COMUNES**

### **Error: "API key not found"**
```bash
# Verificar variable de entorno
echo $GEMINI_API_KEY

# Si está vacía, configurar:
export GEMINI_API_KEY="tu_api_key_aqui"
```

### **Error: "Context too large"**
```bash
# Usar archivos de contexto más específicos
gemini --context-file ".gemini/rules-gemini.md" "Tu pregunta"

# En lugar de cargar todo el proyecto
```

### **Error: "Command not found: gemini-icfes"**
```bash
# Verificar enlaces simbólicos
ls -la ~/.local/bin/gemini-icfes

# Recrear si es necesario
ln -sf "$(pwd)/Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-icfes-unified.sh" ~/.local/bin/gemini-icfes
```

### **Respuestas inconsistentes**
```bash
# Verificar configuración de temperatura
cat ~/.config/gemini/icfes-config.json | grep temperature

# Debe ser 0.1 para máxima consistencia
```

### **Problemas con imágenes**
```bash
# Verificar que las imágenes estén incluidas en Git
git ls-files '*.png' '*.jpg' | head -5

# Verificar configuración de .gitignore
.gemini/scripts/verify_gitignore_images.sh
```

### **🚀 PROBLEMAS ESPECÍFICOS DE MCPs**

#### **Error: "MCPs not available"**
```bash
# Verificar instalación de MCPs
bash Auxiliares/Instalaciones/Ais/Gemini_CLI/test-mcps.sh

# Reinstalar MCPs si es necesario
bash Auxiliares/Instalaciones/Ais/Gemini_CLI/install-mcps.sh
```

#### **Brave Search no funciona**
```bash
# Verificar configuración
cat .gemini-mcp-config.json | grep brave

# Usar activación manual si es necesario
"usar brave-search para buscar información ICFES"
```

#### **Filesystem MCP no accede a archivos**
```bash
# Verificar permisos de archivos
ls -la A-Produccion/Ejemplos-Funcionales-Rmd/

# Usar rutas completas
"leer archivo ./A-Produccion/Ejemplos-Funcionales-Rmd/ejercicio.Rmd"
```

#### **Memory MCP no persiste datos**
```bash
# Verificar directorio de memoria
ls -la .mcps/memory-data/

# Crear directorio si no existe
mkdir -p .mcps/memory-data
```

#### **Context7 MCP falla**
```bash
# Verificar variables de entorno (opcional)
echo $UPSTASH_REDIS_REST_URL

# Usar sin configuración externa
"documentación de R-exams básica"
```

---

## 📋 **6. REFERENCIAS RÁPIDAS**

### **Tabla de Comandos Esenciales**

#### **🎯 COMANDOS EXTENSIÓN VSCODE (MODO EXPERTO)**
| **Acción** | **Comando** |
|------------|-------------|
| **Activar proyecto** | `/id install` (en terminal VSCode) |
| **Memoria semántica** | `/init` |
| **Ver memoria** | `/memory show` |
| **Agregar contexto** | `/memory add "contexto específico"` |
| **Guardar conversación** | `/chat save "nombre_sesion"` |
| **Reanudar conversación** | `/chat resume "nombre_sesion"` |

#### **🚀 COMANDOS CON MCPs (RECOMENDADOS)**
| **Acción** | **Comando** |
|------------|-------------|
| **Iniciar con MCPs** | `gemini-icfes --mcps` |
| **Investigar ICFES** | `"buscar información sobre competencias matemáticas ICFES 2025"` |
| **Acceder archivos** | `"leer archivo A-Produccion/Ejemplos-Funcionales-Rmd/ejercicio.Rmd"` |
| **Documentación técnica** | `"documentación de R-exams para ejercicios matemáticos"` |
| **Testing automático** | `"testing de compilación HTML del ejercicio"` |
| **Guardar conocimiento** | `"recordar mejores prácticas identificadas"` |

#### **📋 COMANDOS BÁSICOS (SIN MCPs)**
| **Acción** | **Comando** |
|------------|-------------|
| Iniciar modo básico | `gemini-icfes --basic` |
| Analizar ejercicio | `gemini --context-file "ejercicio.Rmd" --context-file ".gemini/rules-gemini.md" "Analiza este ejercicio"` |
| Generar TikZ | `gemini --image "imagen.png" "Genera código TikZ"` |
| Validar ICFES | `gemini --context-file "GEMINI.md" "Valida estándares ICFES"` |
| Verificar setup | `.gemini/scripts/verify_setup.sh` |
| Ayuda | `gemini-icfes --help` |

### **Archivos de Contexto Importantes**

| **Archivo** | **Propósito** |
|-------------|---------------|
| `GEMINI.md` | Contexto completo del proyecto |
| `.gemini/rules-gemini.md` | Reglas específicas para R-exams |
| `.gemini/task-list-gemini.md` | Lista de tareas del proyecto |

### **Atajos de Teclado en Gemini CLI**

| **Tecla** | **Acción** |
|-----------|------------|
| `Ctrl+C` | Salir de Gemini CLI |
| `Ctrl+G` | Ver archivos cargados |
| `@archivo.md` | Cargar archivo específico |
| `/help` | Ayuda dentro de la sesión |

### **Mejores Prácticas**

#### **🎯 MODO EXPERTO CON EXTENSIÓN VSCODE (MÁXIMO NIVEL)**
1. **Usa VSCode Insiders + Extensión**: Integración directa con archivos del proyecto
2. **Activa el proyecto**: `/id install` para acceso completo al repositorio
3. **Inicializa memoria semántica**: `/init` para comprensión automática del proyecto
4. **Gestiona memoria granular**: `/memory add` para contextos específicos por ejercicio
5. **Persiste sesiones**: `/chat save` para continuidad en desarrollo complejo
6. **Combina con MCPs**: Usar extensión VSCode + MCPs para máxima potencia

#### **🚀 CON MCPs (RECOMENDADO)**
1. **Usa el modo MCPs por defecto**: `gemini-icfes --mcps` para máxima funcionalidad
2. **Aprovecha la investigación automática**: "buscar información sobre..." para datos actualizados
3. **Accede directamente a archivos**: "leer archivo..." en lugar de copiar/pegar contenido
4. **Persiste conocimiento**: "recordar..." para guardar patrones exitosos
5. **Combina MCPs en workflows**: Investigación → Documentación → Archivos → Testing → Memoria

#### **📋 GENERALES (TODOS LOS MODOS)**
1. **Sé específico en tus preguntas**: "Analiza competencia ICFES interpretación" vs "Analiza este ejercicio"
2. **Usa ejemplos del proyecto**: Referencia ejercicios en `A-Produccion/Ejemplos-Funcionales-Rmd/`
3. **Verifica antes de implementar**: Siempre revisa el código generado antes de usarlo
4. **Documenta tus workflows**: Guarda comandos útiles para reutilizar
5. **Aprovecha la memoria persistente**: Con MCPs y extensión VSCode, el sistema recuerda mejores prácticas automáticamente

### **Recursos Adicionales**

- **Tutorial completo**: `Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-cli-r-exams.md`
- **Ejemplos funcionales**: `A-Produccion/Ejemplos-Funcionales-Rmd/`
- **Documentación TikZ**: `Auxiliares/TikZ-Documentation/`
- **Metodologías**: `Auxiliares/METODOLOGIA_*.md`

---

## 🎨 **7. EJEMPLOS PRÁCTICOS PASO A PASO**

### **EJEMPLO COMPLETO: Crear Ejercicio de Función Cuadrática**

**Escenario:** Necesitas crear un ejercicio sobre funciones cuadráticas con gráfico TikZ.

**Paso 1: Analizar imagen de referencia**
```bash
# Si tienes una imagen de función cuadrática
gemini --image "A-Produccion/Ejemplos-Funcionales-Rmd/funcion_cuadratica.png" \
       --context-file ".gemini/rules-gemini.md" \
       "Analiza esta función cuadrática y genera código TikZ equivalente"
```

**Paso 2: Crear ejercicio R-exams**
```bash
gemini --context-file "GEMINI.md" \
       --context-file ".gemini/rules-gemini.md" \
       "Crea un ejercicio R-exams sobre función cuadrática f(x)=ax²+bx+c con:
       - Aleatorización de parámetros a, b, c
       - 4 opciones de respuesta sobre vértice
       - Competencia: interpretación y representación
       - Nivel 2-3 ICFES
       - Contexto colombiano apropiado"
```

**Paso 3: Validar el ejercicio generado**
```bash
# Guardar el ejercicio generado como ejercicio_cuadratica.Rmd
gemini --context-file "ejercicio_cuadratica.Rmd" \
       --context-file ".gemini/rules-gemini.md" \
       "Valida este ejercicio: sintaxis R-exams, aleatorización, estándares ICFES"
```

### **FLUJO DE TRABAJO TÍPICO**

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   1. ANÁLISIS   │───▶│  2. GENERACIÓN  │───▶│ 3. VALIDACIÓN   │
│                 │    │                 │    │                 │
│ • Imagen/tema   │    │ • Código R-exams│    │ • Estándares    │
│ • Competencia   │    │ • Código TikZ   │    │ • Compilación   │
│ • Nivel ICFES   │    │ • Metadatos     │    │ • Optimización  │
└─────────────────┘    └─────────────────┘    └─────────────────┘
         │                       │                       │
         └───────────────────────┼───────────────────────┘
                                 ▼
                    ┌─────────────────┐
                    │ 4. REFINAMIENTO │
                    │                 │
                    │ • Iteraciones   │
                    │ • Mejoras       │
                    │ • Testing       │
                    └─────────────────┘
```

---

## 💡 **8. CONSEJOS AVANZADOS**

### **Optimización de Prompts**

**❌ Prompt genérico:**
```bash
gemini "Analiza este ejercicio"
```

**✅ Prompt específico:**
```bash
gemini --context-file "ejercicio.Rmd" \
       --context-file ".gemini/rules-gemini.md" \
       "Analiza este ejercicio R-exams enfocándote en:
       1. Sintaxis y compilación
       2. Competencia ICFES específica
       3. Calidad de distractores
       4. Aleatorización de parámetros
       5. Contexto colombiano apropiado"
```

### **Uso Eficiente del Contexto**

**Para análisis rápido:**
```bash
gemini --context-file ".gemini/rules-gemini.md" "Tu pregunta"
```

**Para análisis profundo:**
```bash
gemini --context-file "GEMINI.md" \
       --context-file ".gemini/rules-gemini.md" \
       --context-file "ejercicio.Rmd" \
       "Tu pregunta detallada"
```

### **Comandos de Productividad**

**Crear alias útiles:**
```bash
# Agregar a ~/.bashrc o ~/.zshrc
alias gicfes='gemini-icfes --basic'
alias ganalizar='gemini --context-file ".gemini/rules-gemini.md"'
alias gvalidar='gemini --context-file "GEMINI.md" --context-file ".gemini/rules-gemini.md"'
```

**Scripts personalizados:**
```bash
# Crear script para análisis rápido
cat > analizar_ejercicio.sh << 'EOF'
#!/bin/bash
if [ $# -eq 0 ]; then
    echo "Uso: ./analizar_ejercicio.sh archivo.Rmd"
    exit 1
fi
gemini --context-file "$1" \
       --context-file ".gemini/rules-gemini.md" \
       "Analiza este ejercicio R-exams: estructura, ICFES, optimizaciones"
EOF
chmod +x analizar_ejercicio.sh
```

---

## 🚨 **9. CHECKLIST DE CALIDAD**

### **Antes de Usar un Ejercicio Generado**

- [ ] **Compilación**: ¿El ejercicio compila sin errores?
- [ ] **Aleatorización**: ¿Genera 300+ versiones únicas?
- [ ] **Competencia ICFES**: ¿Está claramente alineado?
- [ ] **Nivel apropiado**: ¿Corresponde al nivel declarado?
- [ ] **Contexto colombiano**: ¿Es culturalmente apropiado?
- [ ] **Distractores**: ¿Son plausibles pero incorrectos?
- [ ] **Gráficos TikZ**: ¿Tienen fidelidad visual 98%+?
- [ ] **Metadatos**: ¿Están completos y correctos?

### **Comandos de Verificación Rápida**

```bash
# Verificar sintaxis R-exams
R -e "library(exams); exams2html('ejercicio.Rmd')"

# Verificar aleatorización
R -e "library(exams); set.seed(123); exams2html('ejercicio.Rmd', n=5)"

# Verificar metadatos
grep -E "^ex(name|type|solution|competencia|nivel):" ejercicio.Rmd
```

---

## 📞 **10. SOPORTE Y RECURSOS**

### **Si Necesitas Ayuda**

1. **Verificación automática**: `.gemini/scripts/verify_setup.sh`
2. **Tutorial completo**: `Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-cli-r-exams.md`
3. **Ejemplos funcionales**: `A-Produccion/Ejemplos-Funcionales-Rmd/`
4. **Configuración**: `~/.config/gemini/icfes-config.json`

### **Recursos de Aprendizaje**

- **Documentación R-exams**: https://www.r-exams.org/
- **Marco ICFES Matemáticas**: Consulta `GEMINI.md`
- **TikZ/PGF Manual**: `Auxiliares/TikZ-Documentation/`
- **Metodologías del proyecto**: `Auxiliares/METODOLOGIA_*.md`

### **Comunidad y Actualizaciones**

- **Repositorio GitHub**: `alvaretto/proyecto-r-exams-icfes-matematicas-optimizado`
- **Rama de desarrollo**: `experimentos-seguros`
- **Issues y mejoras**: Crear issues en GitHub para reportar problemas

---

**🎯 OBJETIVO ALCANZADO: Con este manual puedes usar Gemini CLI productivamente para crear ejercicios R-exams ICFES de alta calidad en 15-20 minutos.**

**🚀 CAPACIDADES HABILITADAS:**
- ✅ **Modo básico**: Análisis y generación con contexto local
- ✅ **Modo avanzado con MCPs**: Investigación automática + documentación integrada + testing + memoria persistente
- ✅ **Modo experto con extensión VSCode**: Integración directa con IDE + memoria semántica + gestión de conversaciones
- ✅ **Workflows optimizados**: Para desarrollo eficiente de ejercicios ICFES

**📈 PRÓXIMO NIVEL: Una vez domines estos comandos básicos, explora el tutorial completo para funcionalidades avanzadas y automatización de workflows.**

**🔗 RECURSOS ADICIONALES:**
- **Guía completa de MCPs**: `Auxiliares/Instalaciones/Ais/Gemini_CLI/MCPs_GUIA_COMPLETA.md`
- **Comandos de ejemplo**: `Auxiliares/Instalaciones/Ais/Gemini_CLI/comandos-ejemplo-gemini-cli.sh`
- **Tutorial técnico**: `Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-cli-r-exams.md`

---

*Manual creado por: Especialista en Integración IA Educativa*\
*Versión: 1.2 - Incluye información completa sobre MCPs + Extensión VSCode Companion*\
*Fecha: Agosto 2025*\
*Proyecto: RepositorioMatematicasICFES_R_Exams*\
*Última actualización: Agosto 24, 2025 - Integración completa VSCode Companion*
