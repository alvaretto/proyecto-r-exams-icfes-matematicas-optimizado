# 🔄 GUÍA COMPLETA: Creación de Workflows en AnythingLLM

## 📋 ÍNDICE

1. [¿Qué son los Workflows (Flows)?](#qué-son-los-workflows-flows)
2. [Acceso al Constructor de Flows](#acceso-al-constructor-de-flows)
3. [Bloques Básicos de un Flow](#bloques-básicos-de-un-flow)
4. [Bloques Disponibles](#bloques-disponibles)
5. [Tutorial: Crear tu Primer Flow](#tutorial-crear-tu-primer-flow)
6. [Workflows para ICFES R-Exams](#workflows-para-icfes-r-exams)
7. [Consejos y Mejores Prácticas](#consejos-y-mejores-prácticas)
8. [Solución de Problemas](#solución-de-problemas)

---

## 🎯 ¿QUÉ SON LOS WORKFLOWS (FLOWS)?

En AnythingLLM, los **Flows** (flujos de trabajo) son secuencias automatizadas de acciones que permiten:

✅ **Automatizar tareas repetitivas** (validación, compilación, generación)\
✅ **Encadenar múltiples operaciones** (scraping → análisis → generación)\
✅ **Integrar herramientas externas** (APIs, archivos, web scraping)\
✅ **Crear agentes especializados** con capacidades específicas\
✅ **Mantener consistencia** en procesos complejos

### Diferencia entre Agentes y Flows

| **Agentes** | **Flows** |
|-------------|-----------|
| Conversacionales | Automatizados |
| Responden a preguntas | Ejecutan secuencias |
| Usan RAG sobre documentos | Usan bloques de acción |
| Flexibles y adaptativos | Estructurados y predecibles |

**💡 Mejor práctica**: Usa **Agentes** para interacción y **Flows** para automatización.

---

## 🚀 ACCESO AL CONSTRUCTOR DE FLOWS

### Paso 1: Navegar a Agent Skills

1. Abre AnythingLLM en http://localhost:3001
2. Selecciona tu workspace **"ICFES R-Exams"**
3. Haz clic en el ícono de **configuración** (⚙️) del workspace
4. Ve a la sección **"Agent Skills"**

### Paso 2: Crear un Nuevo Flow

1. Haz clic en el botón **"Create Flow"**
2. Se abrirá el constructor de flows con un canvas en blanco
3. Verás 3 bloques básicos pre-configurados

![Interfaz de creación de flows](https://docs.anythingllm.com/agent-flows/getting-started)

---

## 🧩 BLOQUES BÁSICOS DE UN FLOW

Cada flow nuevo incluye automáticamente estos 3 bloques esenciales:

### 1️⃣ Flow Information Block

**Propósito**: Define el nombre y descripción del flow

**Campos**:
- **Name**: Nombre descriptivo del flow
- **Description**: Explicación detallada de qué hace el flow y cómo usarlo

**Ejemplo**:
```yaml
Name: Validador de Ejercicios ICFES
Description: |
  Este flow valida archivos .Rmd de ejercicios ICFES.
  
  Verifica:
  - Estructura YAML correcta
  - Metadatos ICFES completos
  - Sintaxis R y Python
  - Configuración de chunks
  
  Uso: "Valida el archivo ejercicio_estadistica.Rmd"
```

### 2️⃣ Flow Variables Block

**Propósito**: Define variables que se usarán en el flow

**Campos por variable**:
- **Name**: Nombre de la variable (sin espacios, usa camelCase)
- **Default Value**: Valor por defecto (puede estar vacío)

**Ejemplo**:
```yaml
Variables:
  - Name: archivoRmd
    Default Value: ""
  
  - Name: tipoValidacion
    Default Value: "completa"
  
  - Name: resultadoValidacion
    Default Value: ""
```

### 3️⃣ Flow Complete Block

**Propósito**: Marca el final del flow

**Nota**: Este bloque siempre debe estar al final de la secuencia.

---

## 🔧 BLOQUES DISPONIBLES

### 📥 Web Scraper

**Función**: Extrae contenido de una URL

**Configuración**:
- **URL to scrape**: URL completa o con variables `${variableName}`
- **Result Variable**: Variable donde se guardará el contenido

**Ejemplo**:
```yaml
URL to scrape: https://www.r-exams.org/intro/
Result Variable: documentacionRExams
```

### 🌐 API Call

**Función**: Realiza llamadas a APIs externas

**Configuración**:
- **Method**: GET, POST, PUT, DELETE
- **URL**: Endpoint de la API
- **Headers**: Encabezados HTTP (JSON)
- **Body**: Cuerpo de la petición (para POST/PUT)
- **Result Variable**: Variable para la respuesta

**Ejemplo**:
```yaml
Method: POST
URL: https://api.github.com/repos/alvaretto/proyecto-r-exams/issues
Headers: {"Authorization": "token ${githubToken}"}
Body: {"title": "Error en ${archivoRmd}", "body": "${errorDetalle}"}
Result Variable: issueCreado
```

### 🤖 LLM Instruction

**Función**: Procesa contenido usando el LLM configurado

**Configuración**:
- **Instructions**: Instrucciones para el LLM
- **Content**: Contenido a procesar (puede usar variables)
- **Result Variable**: Variable para la respuesta del LLM

**Ejemplo**:
```yaml
Instructions: |
  Analiza este código .Rmd y identifica todos los errores de sintaxis.
  Clasifica los errores en categorías: YAML, R, Python, TikZ, LaTeX.
  
Content: ${contenidoArchivoRmd}
Result Variable: erroresIdentificados
```

### 📄 Read File

**Función**: Lee contenido de un archivo local

**Configuración**:
- **File Path**: Ruta absoluta o relativa al archivo
- **Result Variable**: Variable donde se guardará el contenido

**Ejemplo**:
```yaml
File Path: /workspace/Lab-Manjaro/${archivoRmd}
Result Variable: contenidoArchivoRmd
```

### 💾 Write File

**Función**: Escribe contenido en un archivo

**Configuración**:
- **File Path**: Ruta donde guardar el archivo
- **Content**: Contenido a escribir (puede usar variables)
- **Overwrite**: Si sobrescribir archivo existente

**Ejemplo**:
```yaml
File Path: /workspace/salida/reporte_validacion_${timestamp}.md
Content: ${reporteValidacion}
Overwrite: true
```

---

## 📚 TUTORIAL: CREAR TU PRIMER FLOW

Vamos a crear un flow simple que valida la estructura básica de un archivo .Rmd.

### Paso 1: Crear el Flow

1. En Agent Skills, haz clic en **"Create Flow"**
2. Se abrirá el canvas con los 3 bloques básicos

### Paso 2: Configurar Flow Information

```yaml
Name: Validador Básico ICFES
Description: |
  Valida la estructura básica de un archivo .Rmd para ejercicios ICFES.
  
  Verifica:
  - Presencia de encabezado YAML
  - Secciones Question, Solution, Meta-information
  - Metadatos ICFES básicos
  
  Uso: "Valida el archivo [nombre_archivo.Rmd]"
```

### Paso 3: Configurar Flow Variables

Agrega estas variables:

```yaml
Variable 1:
  Name: nombreArchivo
  Default Value: ""

Variable 2:
  Name: contenidoArchivo
  Default Value: ""

Variable 3:
  Name: resultadoValidacion
  Default Value: ""
```

### Paso 4: Agregar Bloque Read File

1. Haz clic en **"Add Block"** debajo de Flow Variables
2. Selecciona **"Read File"**
3. Configura:

```yaml
File Path: /workspace/Lab-Manjaro/${nombreArchivo}
Result Variable: contenidoArchivo
```

### Paso 5: Agregar Bloque LLM Instruction

1. Haz clic en **"Add Block"** debajo de Read File
2. Selecciona **"LLM Instruction"**
3. Configura:

```yaml
Instructions: |
  Analiza este archivo .Rmd y verifica:
  
  1. ¿Tiene encabezado YAML válido (entre ---)?
  2. ¿Tiene sección Question?
  3. ¿Tiene sección Solution?
  4. ¿Tiene sección Meta-information?
  5. ¿Incluye metadatos ICFES (exname, extype, exsolution)?
  
  Responde en formato:
  ✅ [Aspecto]: OK
  ❌ [Aspecto]: FALTA - [Explicación]
  
Content: ${contenidoArchivo}
Result Variable: resultadoValidacion
```

### Paso 6: Guardar y Probar

1. Haz clic en **"Save"** en la esquina superior derecha
2. El flow se guardará automáticamente como **"Enabled"**
3. Prueba con: *"Valida el archivo ejercicio_test.Rmd"*

---

## 🎯 WORKFLOWS PARA ICFES R-EXAMS

A continuación, configuraciones detalladas de los 3 workflows principales para el proyecto.

### 🔄 WORKFLOW 1: Generación Completa desde Imagen

**Objetivo**: Generar un ejercicio .Rmd completo a partir de una imagen PNG

#### Configuración del Flow

**Flow Information**:
```yaml
Name: Generador Completo ICFES
Description: |
  Genera un ejercicio .Rmd completo a partir de una imagen PNG.

  Proceso:
  1. Analiza la imagen (detecta contenido gráfico)
  2. Identifica competencia ICFES apropiada
  3. Genera código .Rmd completo
  4. Valida estructura y metadatos
  5. Guarda archivo en ubicación apropiada

  Uso: "Genera un ejercicio de [competencia] nivel [1-4] desde esta imagen"

  Ejemplo: "Genera un ejercicio de interpretación nivel 2 desde estadistica01.png"
```

**Flow Variables**:
```yaml
Variables:
  - Name: rutaImagen
    Default Value: ""

  - Name: competenciaICFES
    Default Value: "interpretacion_representacion"

  - Name: nivelDificultad
    Default Value: "2"

  - Name: componenteICFES
    Default Value: ""

  - Name: analisisImagen
    Default Value: ""

  - Name: codigoRmdGenerado
    Default Value: ""

  - Name: nombreArchivoSalida
    Default Value: ""
```

#### Secuencia de Bloques

**Bloque 1: LLM Instruction - Análisis de Imagen**
```yaml
Instructions: |
  Analiza esta imagen matemática y determina:

  1. ¿Contiene gráficos/diagramas? (SÍ/NO)
  2. Concepto matemático principal
  3. Tipo de problema (álgebra, geometría, estadística)
  4. Componente ICFES apropiado (geometrico_metrico, numerico_variacional, aleatorio)
  5. Elementos visuales clave (tablas, gráficas, figuras)

  Responde en formato JSON:
  {
    "contiene_graficos": true/false,
    "concepto_principal": "...",
    "tipo_problema": "...",
    "componente_icfes": "...",
    "elementos_visuales": ["..."]
  }

Content: [Imagen adjunta por el usuario]
Result Variable: analisisImagen
```

**Bloque 2: LLM Instruction - Generación de Código .Rmd**
```yaml
Instructions: |
  Genera un archivo .Rmd completo para ejercicio ICFES siguiendo estas especificaciones:

  Análisis de imagen: ${analisisImagen}
  Competencia: ${competenciaICFES}
  Nivel: ${nivelDificultad}
  Componente: ${componenteICFES}

  OBLIGATORIO:
  1. Consultar ejemplos funcionales en /A-Produccion/Ejemplos-Funcionales-Rmd/
  2. Incluir encabezado YAML completo
  3. Chunk de configuración inicial con locale
  4. Función generar_datos() para 300+ versiones
  5. Test de diversidad de versiones
  6. Si hay gráficos: usar TikZ o Python/matplotlib
  7. Secciones Question, Solution, Meta-information
  8. Metadatos ICFES completos

  Genera el código .Rmd completo y funcional.

Content: ${analisisImagen}
Result Variable: codigoRmdGenerado
```

**Bloque 3: LLM Instruction - Generar Nombre de Archivo**
```yaml
Instructions: |
  Genera un nombre de archivo apropiado siguiendo el formato:

  [ejercicio]_[componente]_[competencia]_n[nivel]_v1.Rmd

  Basado en:
  - Análisis: ${analisisImagen}
  - Competencia: ${competenciaICFES}
  - Nivel: ${nivelDificultad}
  - Componente: ${componenteICFES}

  Responde SOLO con el nombre del archivo, sin explicaciones.

Content: ${analisisImagen}
Result Variable: nombreArchivoSalida
```

**Bloque 4: Write File - Guardar Archivo**
```yaml
File Path: /workspace/Lab-Manjaro/${nombreArchivoSalida}
Content: ${codigoRmdGenerado}
Overwrite: false
```

**Bloque 5: LLM Instruction - Reporte Final**
```yaml
Instructions: |
  Genera un reporte de confirmación con:

  ✅ Archivo generado: ${nombreArchivoSalida}
  ✅ Ubicación: /workspace/Lab-Manjaro/
  ✅ Competencia: ${competenciaICFES}
  ✅ Nivel: ${nivelDificultad}
  ✅ Componente: ${componenteICFES}

  Próximos pasos:
  1. Revisar el archivo generado
  2. Compilar en RStudio para verificar
  3. Ajustar si es necesario

  Formato markdown con emojis.

Content: "Archivo ${nombreArchivoSalida} generado exitosamente"
Result Variable: (dejar vacío para mostrar al usuario)
```

---

### ✅ WORKFLOW 2: Validación y Corrección

**Objetivo**: Validar y corregir archivos .Rmd existentes

#### Configuración del Flow

**Flow Information**:
```yaml
Name: Validador y Corrector ICFES
Description: |
  Valida y corrige archivos .Rmd de ejercicios ICFES.

  Proceso:
  1. Lee el archivo .Rmd
  2. Identifica errores por categoría
  3. Consulta biblioteca de soluciones
  4. Aplica correcciones validadas
  5. Re-valida y confirma
  6. Guarda archivo corregido

  Uso: "Valida y corrige el archivo [nombre.Rmd]"

  Ejemplo: "Valida y corrige ejercicio_estadistica_v1.Rmd"
```

**Flow Variables**:
```yaml
Variables:
  - Name: nombreArchivo
    Default Value: ""

  - Name: contenidoOriginal
    Default Value: ""

  - Name: erroresIdentificados
    Default Value: ""

  - Name: solucionesAplicables
    Default Value: ""

  - Name: contenidoCorregido
    Default Value: ""

  - Name: validacionFinal
    Default Value: ""
```

#### Secuencia de Bloques

**Bloque 1: Read File - Leer Archivo**
```yaml
File Path: /workspace/Lab-Manjaro/${nombreArchivo}
Result Variable: contenidoOriginal
```

**Bloque 2: LLM Instruction - Diagnóstico de Errores**
```yaml
Instructions: |
  Analiza este archivo .Rmd e identifica TODOS los errores por categoría:

  A) ERRORES GRAMATICALES/CONCORDANCIA
     - Concordancia de género/número
     - Ortografía

  B) ERRORES DE POSICIONAMIENTO
     - Orden de elementos (texto → tabla → pregunta)
     - Ubicación de chunks

  C) ERRORES DE GENERACIÓN DE DATOS
     - Opciones duplicadas
     - Falta de diversidad
     - Datos inconsistentes

  D) ERRORES DE COMPILACIÓN
     - Sintaxis LaTeX/TikZ
     - Configuración de chunks
     - Paquetes faltantes

  E) ERRORES DE ESTRUCTURA R-EXAMS
     - YAML incompleto
     - Metadatos ICFES faltantes
     - Secciones faltantes

  Responde en formato JSON:
  {
    "categoria_A": ["error1", "error2"],
    "categoria_B": [...],
    ...
  }

Content: ${contenidoOriginal}
Result Variable: erroresIdentificados
```

**Bloque 3: Read File - Consultar Biblioteca de Soluciones**
```yaml
File Path: /workspace/Auxiliares/BIBLIOTECA_Soluciones_Errores_Comunes.md
Result Variable: bibliotecaSoluciones
```

**Bloque 4: LLM Instruction - Identificar Soluciones**
```yaml
Instructions: |
  Basado en los errores identificados:
  ${erroresIdentificados}

  Consulta esta biblioteca de soluciones:
  ${bibliotecaSoluciones}

  Identifica las soluciones aplicables para cada error.

  Responde en formato JSON:
  {
    "error1": "solución específica",
    "error2": "solución específica",
    ...
  }

Content: ${erroresIdentificados}
Result Variable: solucionesAplicables
```

**Bloque 5: LLM Instruction - Aplicar Correcciones**
```yaml
Instructions: |
  Aplica las siguientes correcciones al archivo original:

  Errores: ${erroresIdentificados}
  Soluciones: ${solucionesAplicables}

  IMPORTANTE:
  1. Mantén la estructura general del archivo
  2. Solo corrige los errores identificados
  3. No agregues funcionalidad nueva
  4. Preserva comentarios y documentación
  5. Asegura que compile correctamente

  Genera el código .Rmd corregido completo.

Content: ${contenidoOriginal}
Result Variable: contenidoCorregido
```

**Bloque 6: LLM Instruction - Re-validación**
```yaml
Instructions: |
  Valida que el archivo corregido:

  1. No tenga errores de sintaxis
  2. Tenga estructura completa
  3. Incluya metadatos ICFES
  4. Compile correctamente

  Responde:
  ✅ VALIDACIÓN EXITOSA - [detalles]
  o
  ❌ ERRORES PENDIENTES - [lista de errores]

Content: ${contenidoCorregido}
Result Variable: validacionFinal
```

**Bloque 7: Write File - Guardar Archivo Corregido**
```yaml
File Path: /workspace/Lab-Manjaro/${nombreArchivo}.corregido.Rmd
Content: ${contenidoCorregido}
Overwrite: true
```

**Bloque 8: LLM Instruction - Reporte de Correcciones**
```yaml
Instructions: |
  Genera un reporte detallado de las correcciones aplicadas:

  📄 Archivo: ${nombreArchivo}

  🔍 Errores encontrados:
  ${erroresIdentificados}

  ✅ Correcciones aplicadas:
  ${solucionesAplicables}

  📊 Validación final:
  ${validacionFinal}

  💾 Archivo corregido guardado como:
  ${nombreArchivo}.corregido.Rmd

  Próximos pasos:
  1. Revisar el archivo corregido
  2. Compilar en RStudio
  3. Si todo está bien, renombrar eliminando ".corregido"

Content: "Correcciones completadas"
Result Variable: (dejar vacío)
```

---

### 🎨 WORKFLOW 3: Optimización de Diversidad

**Objetivo**: Optimizar ejercicios para generar 300+ versiones únicas

#### Configuración del Flow

**Flow Information**:
```yaml
Name: Optimizador de Diversidad ICFES
Description: |
  Optimiza la función generar_datos() para alcanzar 300+ versiones únicas.

  Proceso:
  1. Analiza función generar_datos() actual
  2. Identifica parámetros aleatorizables
  3. Amplía rangos de variación
  4. Agrega contextos alternativos
  5. Implementa colores aleatorios (si aplica)
  6. Ejecuta test de diversidad
  7. Valida 300+ versiones únicas

  Uso: "Optimiza la diversidad de [archivo.Rmd]"

  Ejemplo: "Optimiza la diversidad de ejercicio_mediana_v1.Rmd"
```

**Flow Variables**:
```yaml
Variables:
  - Name: nombreArchivo
    Default Value: ""

  - Name: contenidoArchivo
    Default Value: ""

  - Name: analisisDiversidad
    Default Value: ""

  - Name: mejoras Propuestas
    Default Value: ""

  - Name: contenidoOptimizado
    Default Value: ""

  - Name: resultadoTest
    Default Value: ""
```

#### Secuencia de Bloques

**Bloque 1: Read File**
```yaml
File Path: /workspace/Lab-Manjaro/${nombreArchivo}
Result Variable: contenidoArchivo
```

**Bloque 2: LLM Instruction - Análisis de Diversidad**
```yaml
Instructions: |
  Analiza la función generar_datos() en este archivo .Rmd:

  Identifica:
  1. Parámetros numéricos y sus rangos actuales
  2. Contextos/escenarios utilizados
  3. Colores/estilos (si hay gráficos)
  4. Nombres/objetos/variables
  5. Estructuras de datos

  Estima cuántas versiones únicas puede generar actualmente.

  Responde en formato JSON:
  {
    "parametros_numericos": {"param1": "rango actual", ...},
    "contextos": ["contexto1", ...],
    "colores": ["color1", ...],
    "estimacion_versiones": número,
    "limitaciones": ["limitación1", ...]
  }

Content: ${contenidoArchivo}
Result Variable: analisisDiversidad
```

**Bloque 3: LLM Instruction - Proponer Mejoras**
```yaml
Instructions: |
  Basado en este análisis:
  ${analisisDiversidad}

  Propón mejoras específicas para alcanzar 300+ versiones:

  1. RANGOS NUMÉRICOS
     - Ampliar rangos existentes
     - Agregar nuevos parámetros aleatorios

  2. CONTEXTOS
     - Agregar escenarios alternativos
     - Diversificar nombres/objetos

  3. COLORES (si aplica)
     - Implementar paletas aleatorias
     - Variar estilos visuales

  4. ESTRUCTURAS
     - Variar tamaños de datasets
     - Diversificar tipos de datos

  Responde en formato JSON con código específico para cada mejora.

Content: ${analisisDiversidad}
Result Variable: mejorasPropuestas
```

**Bloque 4: LLM Instruction - Implementar Optimizaciones**
```yaml
Instructions: |
  Implementa estas mejoras en el archivo:

  Mejoras propuestas: ${mejorasPropuestas}

  IMPORTANTE:
  1. Mantén la lógica matemática del ejercicio
  2. No cambies la competencia ICFES evaluada
  3. Asegura coherencia matemática
  4. Preserva validaciones existentes
  5. Actualiza el test de diversidad si es necesario

  Genera el código .Rmd optimizado completo.

Content: ${contenidoArchivo}
Result Variable: contenidoOptimizado
```

**Bloque 5: Write File - Guardar Versión Optimizada**
```yaml
File Path: /workspace/Lab-Manjaro/${nombreArchivo}.optimizado.Rmd
Content: ${contenidoOptimizado}
Overwrite: true
```

**Bloque 6: LLM Instruction - Simular Test de Diversidad**
```yaml
Instructions: |
  Analiza el código optimizado y estima:

  1. Número de versiones únicas esperadas
  2. Factores de variación implementados
  3. Probabilidad de duplicados

  Simula mentalmente el test de diversidad y responde:

  ✅ ESTIMACIÓN: [número] versiones únicas
  📊 FACTORES: [lista de factores]
  ⚠️ RIESGOS: [posibles problemas]

  ¿Alcanza el objetivo de 300+ versiones? SÍ/NO

Content: ${contenidoOptimizado}
Result Variable: resultadoTest
```

**Bloque 7: LLM Instruction - Reporte de Optimización**
```yaml
Instructions: |
  Genera un reporte de optimización:

  📄 Archivo: ${nombreArchivo}

  📊 Análisis inicial:
  ${analisisDiversidad}

  🔧 Mejoras implementadas:
  ${mejorasPropuestas}

  ✅ Resultado esperado:
  ${resultadoTest}

  💾 Archivo optimizado guardado como:
  ${nombreArchivo}.optimizado.Rmd

  Próximos pasos:
  1. Compilar en RStudio
  2. Ejecutar test de diversidad real
  3. Verificar que alcanza 300+ versiones
  4. Si todo está bien, renombrar eliminando ".optimizado"

Content: "Optimización completada"
Result Variable: (dejar vacío)
```

---

## 💡 CONSEJOS Y MEJORES PRÁCTICAS

### ✅ Diseño de Flows

1. **Mantén flows simples y enfocados**: Un flow = una tarea específica
2. **Usa nombres descriptivos**: Para variables y bloques
3. **Documenta en Flow Information**: Explica claramente qué hace y cómo usarlo
4. **Maneja errores**: Considera qué pasa si un bloque falla
5. **Prueba incrementalmente**: Agrega bloques de uno en uno y prueba

### ✅ Variables

1. **Usa camelCase**: `nombreArchivo`, no `nombre_archivo`
2. **Valores por defecto útiles**: Facilitan pruebas
3. **Variables intermedias**: Para resultados de bloques intermedios
4. **Variables de salida**: Deja vacío `Result Variable` en el último bloque para mostrar al usuario

### ✅ LLM Instructions

1. **Sé específico**: Instrucciones claras y detalladas
2. **Usa formato estructurado**: JSON, Markdown, listas
3. **Incluye ejemplos**: Cuando sea posible
4. **Valida salidas**: Pide formatos específicos para facilitar procesamiento

### ✅ Integración con el Proyecto

1. **Rutas absolutas**: Usa `/workspace/` como base
2. **Consulta ejemplos**: Referencia `/A-Produccion/Ejemplos-Funcionales-Rmd/`
3. **Mantén consistencia**: Con estructura de archivos del proyecto
4. **Guarda logs**: Para debugging y mejora continua

---

## 🔧 SOLUCIÓN DE PROBLEMAS

### ❌ El flow no se ejecuta

**Posibles causas**:
- Flow no está habilitado
- Otros agent skills interfieren
- Prompt del usuario no coincide con descripción

**Solución**:
1. Verifica que el flow esté "Enabled"
2. Deshabilita otros agent skills temporalmente
3. Usa prompts que coincidan con la descripción del flow

### ❌ Variables no se resuelven

**Posibles causas**:
- Nombre de variable incorrecto
- Sintaxis `${variable}` incorrecta
- Variable no definida en Flow Variables

**Solución**:
1. Verifica nombres exactos (case-sensitive)
2. Usa sintaxis `${nombreVariable}`
3. Define todas las variables en Flow Variables block

### ❌ Read/Write File falla

**Posibles causas**:
- Ruta incorrecta
- Permisos insuficientes
- Archivo no existe (Read)

**Solución**:
1. Usa rutas absolutas desde `/workspace/`
2. Verifica permisos del contenedor Docker
3. Valida existencia de archivos antes de leer

### ❌ LLM no sigue instrucciones

**Posibles causas**:
- Instrucciones ambiguas
- Temperatura muy alta
- Modelo LLM inadecuado

**Solución**:
1. Sé más específico en las instrucciones
2. Reduce temperatura a 0.2-0.3
3. Usa modelos más potentes (GPT-4, Claude 3.5)

### ❌ Flow es muy lento

**Posibles causas**:
- Demasiados bloques LLM
- Archivos muy grandes
- Modelo LLM lento

**Solución**:
1. Combina instrucciones LLM cuando sea posible
2. Procesa archivos en chunks
3. Usa modelos más rápidos para tareas simples

---

## 📚 RECURSOS ADICIONALES

### Documentación Oficial

- **Getting Started with Flows**: https://docs.anythingllm.com/agent-flows/getting-started
- **Tutorial HackerNews**: https://docs.anythingllm.com/agent-flows/tutorial-hackernews
- **All About Blocks**: https://docs.anythingllm.com/agent-flows/all-about-blocks
- **Debugging Flows**: https://docs.anythingllm.com/agent-flows/debugging-flows

### Archivos del Proyecto

- **Ejemplos funcionales**: `/A-Produccion/Ejemplos-Funcionales-Rmd/`
- **Biblioteca de soluciones**: `/Auxiliares/BIBLIOTECA_Soluciones_Errores_Comunes.md`
- **Checklist de validación**: `/Auxiliares/CHECKLIST_Validacion_Archivos_Rmd.md`
- **Guía de agentes**: `/Auxiliares/Agentes-IA/01-GUIA_COMPLETA_Agentes_Workflows_ICFES_R_Exams.md`

---

## 🎯 PRÓXIMOS PASOS

1. ✅ Crear tu primer flow siguiendo el tutorial
2. ✅ Implementar los 3 workflows principales para ICFES
3. ✅ Probar cada workflow con casos reales
4. ✅ Iterar y mejorar basado en resultados
5. ✅ Documentar flows personalizados adicionales

---

**¡Ahora estás listo para crear workflows automatizados en AnythingLLM!** 🚀

**Versión**: 1.0.0\
**Fecha**: 2025-11-06\
**Ubicación**: `AnythingLLM-Config/02-GUIA_Creacion_Workflows_AnythingLLM.md`\
**Autor**: Sistema ICFES R-Exams


