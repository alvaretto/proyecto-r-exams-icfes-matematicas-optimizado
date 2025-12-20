# 📋 Prompts Listos para Usar con Claude Code

Colección de prompts pre-configurados para usar directamente con Claude Code en el proyecto R-Exams ICFES.

---

## 🟢 Nivel 1: Validaciones Básicas

### Validación YAML
```
Ejecuta la validación YAML del archivo [ARCHIVO.Rmd] usando 
.claudecode/examples/basico/01-pre-commit-yaml-validator.sh
```

### Validación de Caracteres Especiales
```
Ejecuta la validación de caracteres especiales LaTeX del archivo [ARCHIVO.Rmd] 
usando .claudecode/examples/basico/02-latex-escape-validator.sh
```

### Validación de Metadatos ICFES
```
Ejecuta la validación de metadatos ICFES del archivo [ARCHIVO.Rmd] usando 
.claudecode/examples/basico/03-metadata-icfes-validator.sh
```

### Validación Completa Básica
```
Ejecuta las tres validaciones básicas (YAML, metadatos ICFES, caracteres 
especiales) sobre el archivo [ARCHIVO.Rmd] y muestra un resumen consolidado 
de todos los errores y advertencias encontradas.
```

---

## 🟡 Nivel 2: Validación con Análisis

### Validación Contra Guía de Estilo
```
Lee el archivo [ARCHIVO.Rmd] y valídalo completamente contra 
.claudedoc/guia_estilo_icfes.md. 

Para cada desviación encontrada, proporciona:
- Tipo de problema (ERROR/ADVERTENCIA/SUGERENCIA)
- Ubicación exacta (línea/sección)
- Descripción del problema
- Corrección sugerida con código
- Referencia a la sección de guia_estilo_icfes.md
```

### Comparación con Ejemplos Funcionales
```
Compara la estructura del archivo [ARCHIVO.Rmd] contra los ejemplos funcionales 
en A-Produccion/Ejemplos-Funcionales-Rmd/.

Identifica:
1. Chunks faltantes o en orden incorrecto
2. Configuraciones diferentes a los ejemplos validados
3. Patrones no seguidos
4. Recomendaciones específicas para alinear estructura

Muestra una tabla comparativa de estructura.
```

---

## 🔴 Nivel 3: Corrección Automática

### Corrección Básica Automática
```
Lee [ARCHIVO.Rmd] y aplica las siguientes correcciones automáticas según 
.claudedoc/guia_estilo_icfes.md:

1. Agregar campos faltantes en YAML (latex_engine, header-includes completos)
2. Corregir set.seed() fijo a set.seed(sample(1:100000, 1))
3. Agregar options(scipen = 999) y options(OutDec = ".") si faltan
4. Escapar caracteres especiales LaTeX (&, %, $, #, _, {, }) en texto markdown

Muestra el diff de cambios antes de aplicarlos.
```

### Corrección Interactiva
```
Analiza [ARCHIVO.Rmd] y para cada error encontrado:
1. Muestra el problema con contexto
2. Indica la severidad (CRÍTICO/ERROR/ADVERTENCIA)
3. Sugiere la corrección específica
4. Espera mi confirmación antes de aplicar cada corrección

Empezar con los errores CRÍTICOS primero.
```

---

## 🔵 Nivel 4: Validación con Renderizado

### Validación Completa con Renderizado
```
Ejecuta la validación completa del archivo [ARCHIVO.Rmd] incluyendo renderizado real:

1. Validaciones estáticas (YAML, metadatos, caracteres especiales)
2. Renderizado de prueba usando exams2html
3. Análisis de errores si el renderizado falla
4. Sugerencias de corrección basadas en errores de renderizado

Muestra resultados detallados de cada paso y un resumen final.
```

### Diagnóstico de Errores de Renderizado
```
El archivo [ARCHIVO.Rmd] falla al renderizar. Haz un diagnóstico completo:

1. Ejecuta .claudecode/examples/avanzado/07-skill-render-validator.sh
2. Analiza el log de error generado
3. Identifica el tipo de error (YAML_SYNTAX, LATEX_ERROR, TIKZ_ERROR, R_SYNTAX, etc.)
4. Consulta .claudedoc/guia_estilo_icfes.md para la solución específica
5. Proporciona corrección exacta con código antes/después
```

### Workflow Completo Iterativo
```
Ejecuta el workflow completo de validación iterativa para [ARCHIVO.Rmd]:

Ejecuta: .claudecode/workflows/validation_chain.sh [ARCHIVO.Rmd]

Muestra los resultados de cada iteración y el resumen final.
```

---

## 🟣 Nivel 5: Creación y Aprendizaje

### Crear Template desde Ejemplos
```
Quiero crear un nuevo ejercicio .Rmd. 

1. Analiza varios ejemplos funcionales de A-Produccion/Ejemplos-Funcionales-Rmd/
2. Identifica los patrones comunes de estructura
3. Crea un template nuevo siguiendo esos patrones
4. Valida el template contra .claudedoc/guia_estilo_icfes.md
5. Muestra el template completo listo para usar

El template debe ser para tipo [schoice/cloze] con [competencia] nivel [1-4].
```

### Validación de Múltiples Archivos
```
Valida todos los archivos .Rmd en el directorio [DIRECTORIO] usando:

1. Validaciones básicas (YAML, metadatos, caracteres especiales)
2. Comparación con ejemplos funcionales
3. Validación de renderizado (opcional, toma tiempo)

Genera un reporte consolidado con:
- Lista de archivos validados
- Errores encontrados por archivo
- Estadísticas generales (total errores, advertencias)
- Archivos que pasan todas las validaciones
```

---

## 🔧 Prompts de Utilidad

### Pre-Commit Check
```
Estoy a punto de hacer commit de [ARCHIVO.Rmd]. 

Ejecuta todas las validaciones básicas y dime si está listo para commit o 
qué correcciones críticas necesita antes de commit.
```

### Post-Edición Validation
```
Acabo de editar [ARCHIVO.Rmd]. Valida que los cambios no hayan introducido 
errores y que el archivo siga cumpliendo con .claudedoc/guia_estilo_icfes.md.
```

### Debugging Específico
```
El archivo [ARCHIVO.Rmd] tiene un error en la línea [NÚMERO]. 

1. Lee esa sección del archivo
2. Consulta .claudedoc/guia_estilo_icfes.md para ver qué podría estar mal
3. Compara con ejemplos funcionales similares
4. Proporciona solución específica
```

### Mejora de Archivo Existente
```
Mejora el archivo [ARCHIVO.Rmd] para que cumpla con todas las mejores prácticas:

1. Validar contra guia_estilo_icfes.md
2. Comparar con ejemplos funcionales
3. Aplicar mejoras sugeridas
4. Validar renderizado
5. Mostrar resumen de mejoras aplicadas
```

---

## 📚 Prompts de Aprendizaje

### Explicar Estándar Específico
```
Explica la sección [SECCIÓN] de .claudedoc/guia_estilo_icfes.md y muestra 
ejemplos concretos de cómo aplicarla en un archivo .Rmd.
```

### Diferencias Entre Tipos de Ejercicio
```
¿Cuáles son las diferencias en estructura y configuración entre ejercicios 
tipo 'schoice' y tipo 'cloze' según la guía de estilo? Muestra ejemplos de cada uno.
```

### Configuración de TikZ
```
Necesito usar TikZ en mi ejercicio .Rmd. 

1. Lee .claudedoc/guia_estilo_icfes.md sobre configuración TikZ
2. Busca ejemplos funcionales que usen TikZ
3. Muéstrame la configuración correcta y un ejemplo mínimo funcional
```

---

## 🎯 Prompts de Integración

### Configurar Pre-Commit Hook
```
Crea un script de pre-commit hook para git que ejecute las validaciones básicas 
sobre archivos .Rmd antes de cada commit. El hook debe:

1. Validar YAML y metadatos ICFES
2. Bloquear commit si hay errores críticos
3. Mostrar advertencias pero permitir commit si solo hay advertencias
4. Guardar reporte de validación
```

### Validación en Batch
```
Valida todos los archivos .Rmd modificados desde el último commit:

1. Lista archivos .Rmd modificados
2. Ejecuta validaciones básicas en cada uno
3. Genera reporte consolidado
4. Identifica archivos que necesitan corrección antes de push
```

---

## 💡 Tips para Usar Estos Prompts

1. **Reemplaza [ARCHIVO.Rmd]** con la ruta real de tu archivo
2. **Reemplaza [DIRECTORIO]** con la ruta real del directorio
3. **Combina prompts** para flujos más complejos
4. **Pide contexto adicional** si los resultados no son claros
5. **Itera** sobre los resultados para refinar las correcciones

---

## 🔄 Flujos Completos Recomendados

### Flujo para Nuevo Archivo
```
1. Crear template desde ejemplos
2. Validar contra guía de estilo
3. Renderizar y verificar
4. Comparar con ejemplos funcionales
```

### Flujo para Archivo Existente
```
1. Validación básica completa
2. Comparación con ejemplos funcionales
3. Aplicar correcciones automáticas
4. Validar renderizado
5. Verificación final
```

### Flujo para Debugging
```
1. Renderizar y capturar error
2. Analizar log de error
3. Consultar guía de estilo para solución
4. Comparar con ejemplos funcionales
5. Aplicar corrección específica
6. Re-renderizar y verificar
```

---

**Nota**: Reemplaza los placeholders [ARCHIVO.Rmd], [DIRECTORIO], etc. con valores reales cuando uses estos prompts.
