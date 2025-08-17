# 🎓 TUTORIAL COMPLETO: GEMINI CLI PARA EJERCICIOS ICFES R-EXAMS

## 🎯 INTRODUCCIÓN

Este tutorial te enseña cómo usar Gemini CLI de manera óptima para crear ejercicios matemáticos ICFES de alta calidad usando las configuraciones especializadas del proyecto.

## 🚀 CONFIGURACIÓN INICIAL

### 1. Verificar Instalación
```bash
# Verificar que Gemini CLI está instalado
gemini --version

# Verificar configuración del proyecto
bash Auxiliares/Instalaciones/Ais/Gemini_CLI/test-gemini-cli.sh
```

### 2. Configurar para Proyecto ICFES
```bash
# Ejecutar configuración especializada
bash Auxiliares/Instalaciones/Ais/Gemini_CLI/configurar-gemini.sh

# Verificar que se creó el comando global
which gemini-icfes
```

## 🎮 MÉTODOS DE USO

### MÉTODO 1: Comando Global (Recomendado)
```bash
# Iniciar Gemini CLI con configuración ICFES
gemini-icfes
```

### MÉTODO 2: Script Directo
```bash
# Desde el directorio del proyecto
bash Auxiliares/Instalaciones/Ais/Gemini_CLI/iniciar-gemini-icfes.sh
```

### MÉTODO 3: Gemini CLI Estándar
```bash
# Gemini CLI normal (requiere cargar contexto manualmente)
gemini
```

## 📋 FLUJO DE TRABAJO PASO A PASO

### PASO 1: Iniciar Sesión Optimizada
```bash
# Usar comando configurado
gemini-icfes
```

Al iniciar, verás:
```
🤖 INICIANDO GEMINI CLI PARA PROYECTO ICFES R-EXAMS
==================================================

📋 Configuración cargada:
  • Reglas: rules-gemini.md
  • Tareas: task-list-gemini.md
  • Contexto: GEMINI.md

🚀 Comandos rápidos disponibles...
```

### PASO 2: Cargar Contexto Completo
```
@Auxiliares/Instalaciones/Ais/Gemini_CLI/GEMINI.md
```

Esto carga:
- ✅ Reglas específicas del proyecto
- ✅ Metodologías integradas
- ✅ Comandos especializados
- ✅ Configuraciones críticas

### PASO 3: Elegir Flujo de Trabajo

#### FLUJO A: Crear Ejercicio desde Imagen
```
# 1. Subir imagen PNG al contexto
# 2. Ejecutar comando:
Analiza esta imagen y aplica el sistema condicional automático

# 3. Seguir las recomendaciones de Gemini
# 4. Validar resultado final
```

#### FLUJO B: Optimizar Ejercicio Existente
```
# 1. Cargar ejercicio:
Analiza este ejercicio: @path/to/ejercicio.Rmd

# 2. Solicitar análisis:
Identifica errores y oportunidades de mejora

# 3. Aplicar correcciones:
Aplica correcciones basadas en mejores prácticas del proyecto
```

#### FLUJO C: Investigación ICFES
```
# 1. Buscar información actualizada:
Busca información actualizada sobre competencia argumentación ICFES 2025

# 2. Validar ejercicio:
Valida que este ejercicio cumple estándares oficiales ICFES
```

## 🎯 COMANDOS ESPECIALIZADOS

### ANÁLISIS Y DIAGNÓSTICO
```
# Análisis completo de ejercicio
"Analiza este ejercicio: @Auxiliares/Ejemplos-Funcionales-Rmd/ejercicio.Rmd"

# Identificar competencia ICFES
"Identifica qué competencia ICFES evalúa mejor este problema"

# Revisar diversidad de versiones
"Verifica que este ejercicio genera 300+ versiones únicas"

# Diagnosticar problemas
"Identifica errores siguiendo la metodología de corrección del proyecto"
```

### CREACIÓN Y GENERACIÓN
```
# Crear desde imagen
"Crea un ejercicio ICFES completo desde esta imagen siguiendo FLUJO B"

# Generar distractores avanzados
"Genera distractores pedagógicos con errores conceptuales reales"

# Desarrollar función de aleatorización
"Crea una función generar_datos() optimizada para este problema"

# Construir diagrama TikZ
"Replica esta imagen con TikZ manteniendo 98% fidelidad visual"
```

### OPTIMIZACIÓN Y MEJORA
```
# Optimizar rendimiento
"Optimiza este código para mejor rendimiento manteniendo calidad pedagógica"

# Mejorar distractores
"Mejora la calidad pedagógica de estos distractores usando errores reales"

# Simplificar estructura
"Simplifica esta estructura sin perder funcionalidad educativa"

# Aumentar diversidad
"Modifica la función para generar 300+ versiones únicas verificadas"
```

### VALIDACIÓN Y TESTING
```
# Validar estándares ICFES
"Valida que este ejercicio cumple estándares ICFES oficiales"

# Probar compilación
"Compila este ejercicio en HTML, PDF y Word y reporta resultados"

# Verificar tolerancias
"Revisa la configuración de tolerancias para evaluación automática"

# Confirmar calidad
"Ejecuta validación completa de calidad técnica y pedagógica"
```

## 🔧 CONFIGURACIONES AVANZADAS

### Personalizar Sesión
```
# Cargar reglas específicas
@Auxiliares/Instalaciones/Ais/Gemini_CLI/rules-gemini.md

# Cargar plan de tareas
@Auxiliares/Instalaciones/Ais/Gemini_CLI/task-list-gemini.md

# Acceder a ejemplos funcionales
@Auxiliares/Ejemplos-Funcionales-Rmd/

# Consultar documentación TikZ
@Auxiliares/TikZ-Documentation/TikZ-ICFES-Guide.md
```

### Comandos de Contexto
```
# Analizar archivo específico
@path/to/archivo.Rmd

# Cargar múltiples archivos
@Auxiliares/Ejemplos-Funcionales-Rmd/ejercicio1.Rmd
@Auxiliares/Ejemplos-Funcionales-Rmd/ejercicio2.Rmd

# Acceder a documentación
@Auxiliares/rules_full/rules_full_v1.md
```

## 🎯 CASOS DE USO PRÁCTICOS

### CASO 1: Ejercicio de Geometría con Imagen
```
1. gemini-icfes
2. @Auxiliares/Instalaciones/Ais/Gemini_CLI/GEMINI.md
3. [Subir imagen PNG de figura geométrica]
4. "Analiza esta imagen y aplica el sistema condicional automático"
5. [Seguir recomendaciones para FLUJO B con TikZ]
6. "Valida que el TikZ tiene 98% fidelidad visual"
7. "Completa el ejercicio con competencia geometrico_metrico nivel 2"
```

### CASO 2: Optimización de Ejercicio Existente
```
1. gemini-icfes
2. @Auxiliares/Instalaciones/Ais/Gemini_CLI/GEMINI.md
3. "Analiza este ejercicio: @path/to/ejercicio_problematico.Rmd"
4. "Identifica errores de las 5 categorías de la metodología"
5. "Aplica correcciones basadas en ejemplos funcionales"
6. "Valida que genera 300+ versiones y compila correctamente"
```

### CASO 3: Investigación y Validación ICFES
```
1. gemini-icfes
2. @Auxiliares/Instalaciones/Ais/Gemini_CLI/GEMINI.md
3. "Busca información oficial sobre competencia formulacion_ejecucion ICFES 2025"
4. "Analiza si este ejercicio evalúa correctamente esa competencia"
5. "Sugiere ajustes para mejor alineación con estándares oficiales"
```

## ⚠️ MEJORES PRÁCTICAS

### DO (Hacer)
- ✅ Siempre cargar contexto completo al inicio
- ✅ Usar comandos específicos del proyecto
- ✅ Consultar ejemplos funcionales antes de generar
- ✅ Validar fidelidad visual en gráficos TikZ
- ✅ Verificar 300+ versiones únicas
- ✅ Probar compilación en múltiples formatos

### DON'T (No hacer)
- ❌ Usar Gemini CLI sin cargar contexto del proyecto
- ❌ Improvisar código sin consultar ejemplos
- ❌ Usar caracteres Unicode directos
- ❌ Configurar tolerancias incorrectamente
- ❌ Generar distractores superficiales
- ❌ Ignorar metodologías del proyecto

## 🔍 SOLUCIÓN DE PROBLEMAS

### Problema: Gemini no reconoce el contexto del proyecto
**Solución**: Cargar explícitamente el archivo GEMINI.md
```
@Auxiliares/Instalaciones/Ais/Gemini_CLI/GEMINI.md
```

### Problema: Errores en generación de TikZ
**Solución**: Consultar ejemplos funcionales
```
Consulta ejemplos funcionales en @Auxiliares/Ejemplos-Funcionales-Rmd/ antes de generar TikZ
```

### Problema: Ejercicio no cumple estándares ICFES
**Solución**: Investigar información oficial
```
Busca información actualizada sobre [competencia específica] ICFES 2025 y ajusta el ejercicio
```

## 📊 MÉTRICAS DE ÉXITO

### Técnicas
- ✅ Compilación exitosa en HTML, PDF, Word
- ✅ 300+ versiones únicas verificadas
- ✅ Tolerancias configuradas correctamente
- ✅ Sin errores de las 5 categorías

### Pedagógicas
- ✅ Competencia ICFES claramente evaluada
- ✅ Nivel de dificultad apropiado
- ✅ Distractores con errores conceptuales reales
- ✅ Contexto realista y relevante

### Visuales (si aplica)
- ✅ Fidelidad visual 98%+ con imagen original
- ✅ Proporciones y colores exactos
- ✅ Todos los elementos presentes

---

**🎯 OBJETIVO**: Dominar el uso de Gemini CLI para crear ejercicios ICFES matemáticos de máxima calidad de manera eficiente y sistemática.
