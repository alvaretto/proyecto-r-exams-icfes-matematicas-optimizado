# 📚 EJEMPLOS DE USO - PROMPT ENHANCER

**Ubicación del script**: `Auxiliares/Prompt-Enhancer/prompt-enhancer.sh`

## ✅ Alias Configurados

Los alias ya están configurados en tu sistema. Solo necesitas activarlos:

```bash
source ~/.bashrc  # o source ~/.zshrc
```

O simplemente abre una nueva terminal.

**Alias disponibles**:

- `pe` - Versión corta (recomendado)
- `pec` - Con copia al portapapeles
- `pei` - Modo interactivo

---

## 🎯 Casos de Uso Prácticos

### Ejemplo 1: Generar Ejercicio desde Producción

**Ubicación**: `A-Produccion/En-Produccion/`

```bash
cd A-Produccion/En-Produccion/

# Con alias (recomendado)
pe "Genera un ejercicio similar al de mediana pero con moda"

# O con ruta completa
../../Auxiliares/Prompt-Enhancer/prompt-enhancer.sh "Genera un ejercicio similar al de mediana pero con moda"
```

**Contexto detectado**: Producción
**Reglas incluidas**:

- Reglas de `.augment/rules/`
- Documentación de `.claude/`
- Guía de estilo de `.claudedoc/`

**Recomendaciones incluidas**:

- Consultar ejemplos funcionales antes de hacer cambios
- Mantener compatibilidad con sistema exams2*
- Validar cambios con auditorías completas

---

### Ejemplo 2: Desarrollar Nuevo Ejercicio

**Ubicación**: `A-Produccion/En-Desarrollo/`

```bash
cd A-Produccion/En-Desarrollo/

# Con alias (recomendado)
pe "Crea un ejercicio de probabilidad condicional con diagrama de árbol TikZ, nivel 3"

# O con ruta completa
../../Auxiliares/Prompt-Enhancer/prompt-enhancer.sh "Crea un ejercicio de probabilidad condicional con diagrama de árbol TikZ, nivel 3"
```

**Contexto detectado**: Desarrollo
**Reglas incluidas**: Todas las reglas de `.augment/`, `.claude/` y `.claudedoc/`
**Recomendaciones incluidas**:

- Consultar templates y plantillas
- Seguir metodologías establecidas (incluye metodología TikZ de `.claude/`)
- Documentar cambios y decisiones

---

### Ejemplo 3: Crear Script Auxiliar

**Ubicación**: `Auxiliares/Scripts/`

```bash
cd Auxiliares/Scripts/

# Con alias (recomendado)
pe "Necesito un script que valide metadatos ICFES en archivos .Rmd"

# O con ruta completa
../Prompt-Enhancer/prompt-enhancer.sh "Necesito un script que valide metadatos ICFES en archivos .Rmd"
```

**Contexto detectado**: Auxiliares
**Reglas incluidas**: Documentación técnica completa de `.claude/`
**Recomendaciones incluidas**:

- Mantener compatibilidad con scripts existentes
- Documentar funcionalidad claramente
- Probar en entorno real antes de integrar

---

### Ejemplo 4: Corregir Error TikZ

**Ubicación**: Cualquiera

```bash
./prompt-enhancer.sh "Tengo un error de compilación LaTeX en include_tikz(), ¿cómo lo corrijo?"
```

**Salida**: Prompt mejorado con:

- Reglas del proyecto sobre TikZ
- Referencias a ejemplos funcionales con TikZ
- Metodología de corrección de errores
- Ubicación de documentación TikZ

---

### Ejemplo 5: Modo Interactivo Completo

```bash
./prompt-enhancer.sh
```

**Interacción**:
```
🔍 Buscando raíz del proyecto...
✓ Raíz del proyecto encontrada: /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams

╔════════════════════════════════════════════════════════════════╗
║         MODO INTERACTIVO - PROMPT ENHANCER                     ║
╚════════════════════════════════════════════════════════════════╝

Ingresa tu prompt (presiona Ctrl+D cuando termines):
```

**Usuario ingresa**:
```
Necesito crear un ejercicio tipo Cloze que combine:

- Estadística descriptiva (media, mediana, moda)
- Gráfico de barras con Python/matplotlib
- Nivel 2, competencia interpretación y representación
- Contexto: ventas de una tienda
```

**Presiona Ctrl+D**

**Resultado**: Prompt mejorado con todo el contexto del proyecto

---

### Ejemplo 6: Guardar y Compartir

```bash
./prompt-enhancer.sh -f requisitos.txt -o prompt_para_ia.txt
```

**Archivo `requisitos.txt`**:
```
Genera un ejercicio de geometría analítica que incluya:

- Ecuación de la recta
- Pendiente e intercepto
- Gráfico con TikZ
- 4 opciones de respuesta tipo schoice
```

**Resultado**: Archivo `prompt_para_ia.txt` con prompt mejorado listo para usar con cualquier IA

---

### Ejemplo 7: Copiar al Portapapeles

```bash
./prompt-enhancer.sh "Corrige el error de tolerancias en este ejercicio Cloze" -c
```

**Resultado**: 

- Prompt mejorado mostrado en pantalla
- Copiado automáticamente al portapapeles
- Listo para pegar en Augment, Claude, ChatGPT, etc.

---

### Ejemplo 8: Pipeline de Trabajo

```bash
# Paso 1: Crear prompt mejorado
./prompt-enhancer.sh -f idea_inicial.txt -o prompt_mejorado.txt

# Paso 2: Usar con IA (manual o automatizado)
# [Copiar prompt_mejorado.txt a tu IA favorita]

# Paso 3: Guardar resultado
# [IA genera el código .Rmd]

# Paso 4: Validar
cd A-Produccion/En-Desarrollo/
# [Ejecutar validaciones]
```

---

## 🎨 Prompts de Ejemplo por Categoría

### Geometría

```bash
./prompt-enhancer.sh "Ejercicio de áreas de polígonos regulares con TikZ, nivel 2"
```

### Estadística

```bash
./prompt-enhancer.sh "Ejercicio de medidas de dispersión con gráfico de caja Python, nivel 3"
```

### Álgebra

```bash
./prompt-enhancer.sh "Sistema de ecuaciones 2x2 con contexto real, nivel 2, schoice"
```

### Probabilidad

```bash
./prompt-enhancer.sh "Probabilidad condicional con diagrama de árbol TikZ, nivel 3"
```

### Funciones

```bash
./prompt-enhancer.sh "Función cuadrática con gráfica y análisis de vértice, nivel 2"
```

---

## 🔧 Casos Especiales

### Desde Subcarpeta Profunda

```bash
cd A-Produccion/En-Produccion/Ejemplos-Funcionales-Rmd/Avances-Pedagogicos/
../../../../prompt-enhancer.sh "Adapta este ejercicio para nivel 3"
```

**Funciona perfectamente**: El script encuentra la raíz automáticamente

---

### Con Prompt Multilínea

```bash
./prompt-enhancer.sh "
Genera un ejercicio que:
1. Use estadística descriptiva
2. Incluya gráfico Python
3. Sea tipo Cloze
4. Nivel 2
5. Contexto familiar
"
```

---

### Integración con Git

```bash
# Antes de hacer cambios importantes
./prompt-enhancer.sh "Voy a modificar el sistema de tolerancias en ejercicios Cloze" -o plan_cambios.txt

# Revisar plan_cambios.txt
# Implementar cambios
# Commit con referencia al plan
```

---

## 📊 Comparación: Antes vs Después

### ❌ Prompt Original (Sin Contexto)

```
Genera un ejercicio de geometría
```

### ✅ Prompt Mejorado (Con Contexto)

```markdown
# PROMPT MEJORADO CON CONTEXTO DEL PROYECTO

## CONTEXTO DE UBICACIÓN
- Proyecto: RepositorioMatematicasICFES_R_Exams
- Ubicación actual: A-Produccion/En-Desarrollo
- Tipo de contexto: desarrollo

## REGLAS GENERALES DEL PROYECTO
[Reglas completas del proyecto]

## EJEMPLOS FUNCIONALES DISPONIBLES
- estadistica_diagramas_caja_interpretacion_representacion_Nivel2_v2_py.Rmd
- mediana_aleatorio_argumentacion_n2_v1.Rmd
[... más ejemplos]

## RECOMENDACIONES SEGÚN CONTEXTO
- Consultar templates y plantillas
- Seguir metodologías establecidas
- Documentar cambios y decisiones

## SOLICITUD DEL USUARIO
Genera un ejercicio de geometría
```

**Resultado**: La IA tiene mucho más contexto para generar un ejercicio apropiado

---

## 🚀 Tips y Trucos

### Tip 1: Alias Bash

Añade a tu `~/.bashrc`:

```bash
alias pe='/ruta/completa/prompt-enhancer.sh'
alias pec='/ruta/completa/prompt-enhancer.sh -c'
```

Uso:
```bash
pe "Mi prompt"
pec "Mi prompt con copia al portapapeles"
```

### Tip 2: Función Bash Avanzada

```bash
function prompt_ia() {
    local prompt="$1"
    /ruta/completa/prompt-enhancer.sh "$prompt" -o /tmp/prompt_temp.txt
    cat /tmp/prompt_temp.txt
    # Aquí podrías integrar con API de IA
}
```

### Tip 3: Integración con Editor

En VSCode, crea una tarea:

```json
{
    "label": "Mejorar Prompt",
    "type": "shell",
    "command": "${workspaceFolder}/prompt-enhancer.sh",
    "args": ["${input:userPrompt}", "-c"]
}
```

---

## 📝 Plantillas de Prompts

### Plantilla: Nuevo Ejercicio

```
Genera un ejercicio de [TEMA] que incluya:

- Competencia ICFES: [interpretacion_representacion|formulacion_ejecucion|argumentacion]
- Nivel: [1|2|3|4]
- Componente: [geometrico_metrico|numerico_variacional|aleatorio]
- Tipo: [schoice|cloze]
- Contexto: [familiar|laboral|comunitario|matematico]
- Elementos visuales: [TikZ|Python|R|ninguno]
```

### Plantilla: Corrección de Errores

```
Tengo un error en el archivo [NOMBRE_ARCHIVO.Rmd]:
- Error: [DESCRIPCIÓN DEL ERROR]
- Ubicación: [CHUNK O LÍNEA]
- Comportamiento esperado: [DESCRIPCIÓN]
- Comportamiento actual: [DESCRIPCIÓN]
```

### Plantilla: Adaptación de Ejercicio

```
Adapta el ejercicio [NOMBRE_EJERCICIO] para:
- Cambiar nivel de [NIVEL_ACTUAL] a [NIVEL_NUEVO]
- Modificar competencia a [COMPETENCIA]
- Añadir/Quitar: [ELEMENTOS]
- Mantener: [ELEMENTOS_A_CONSERVAR]
```

---

**Última actualización**: 2025-12-20  
**Versión**: 1.0.0

