# 📖 Guía Detallada: Uso de `pei` (Modo Interactivo)

**Alias**: `pei` = `prompt-enhancer.sh -i` (modo interactivo)  
**Versión**: 1.2.0+  
**Última actualización**: 2025-12-21

---

## 🎯 ¿Qué es `pei`?

`pei` es el alias para el **modo interactivo** del Prompt Enhancer. Permite ingresar prompts de múltiples líneas de forma cómoda, ideal para prompts largos o complejos.

---

## 🚀 Activación

### Paso 1: Activar alias (si no está activo)

```bash
# Para Bash
source ~/.bashrc

# Para Zsh
source ~/.zshrc
```

O simplemente abre una nueva terminal.

### Paso 2: Verificar que funciona

```bash
pei
```

Deberías ver:
```
🔍 Buscando raíz del proyecto...
✓ Raíz del proyecto encontrada: /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams

╔════════════════════════════════════════════════════════════════╗
║         MODO INTERACTIVO - PROMPT ENHANCER                     ║
╚════════════════════════════════════════════════════════════════╝

Ingresa tu prompt (presiona Ctrl+D cuando termines):
```

---

## 📝 Uso Básico

### Ejemplo 1: Prompt Simple

```bash
pei
```

**Interacción:**
```
Ingresa tu prompt (presiona Ctrl+D cuando termines):
Genera un ejercicio de geometría nivel 2
[Presiona Ctrl+D]
```

**Resultado**: Prompt mejorado mostrado en pantalla

---

### Ejemplo 2: Prompt Multilínea

```bash
pei
```

**Interacción:**
```
Ingresa tu prompt (presiona Ctrl+D cuando termines):
Necesito crear un ejercicio tipo Cloze que combine:

- Estadística descriptiva (media, mediana, moda)
- Gráfico de barras con Python/matplotlib
- Nivel 2, competencia interpretación y representación
- Contexto: ventas de una tienda
[Presiona Ctrl+D]
```

**Ventaja**: Puedes escribir múltiples líneas sin problemas

---

### Ejemplo 3: Prompt con Archivo .Rmd

```bash
pei
```

**Interacción:**
```
Ingresa tu prompt (presiona Ctrl+D cuando termines):
Tengo un error en recta_geometria_analitica_interpretacion_representacion_n2_v1.Rmd

El error es:
Error in abs(b_formateado): Argumento no numérico para una función matemática

¿Cómo lo corrijo?
[Presiona Ctrl+D]
```

**Resultado**: 
- Prompt mejorado con contexto
- **NUEVO**: Detección automática de errores en el archivo .Rmd
- **NUEVO**: Resumen de errores conocidos y soluciones

---

## 🎨 Características del Modo Interactivo

### ✅ Ventajas

1. **Múltiples líneas**: Escribe prompts largos cómodamente
2. **Sin comillas**: No necesitas escapar comillas especiales
3. **Edición fácil**: Puedes usar las flechas para editar
4. **Historial**: Usa las flechas arriba/abajo para navegar historial
5. **Detección automática**: Detecta archivos .Rmd y errores automáticamente

### 📋 Lo que incluye automáticamente

El prompt mejorado incluye:

1. **Contexto de ubicación**
   - Proyecto actual
   - Ubicación en el repositorio
   - Tipo de contexto (producción, desarrollo, etc.)

2. **Reglas del proyecto**
   - Reglas generales de `.claude/`
   - Documentación técnica
   - Guía de estilo ICFES

3. **Ejemplos funcionales**
   - Archivos .Rmd de ejemplo relevantes
   - Templates disponibles

4. **Recomendaciones contextuales**
   - Según tu ubicación actual
   - Mejores prácticas aplicables

5. **🆕 Detección de errores** (si mencionas archivos .Rmd)
   - Análisis automático del archivo
   - Advertencias sobre patrones problemáticos
   - Referencias a documentación de errores conocidos

6. **🆕 Resumen de errores conocidos** (si mencionas archivos .Rmd)
   - Errores documentados y sus soluciones
   - Referencias a casos resueltos

---

## 💡 Casos de Uso Prácticos

### Caso 1: Generar Ejercicio Completo

```bash
pei
```

**Prompt:**
```
Crea un ejercicio de geometría analítica que incluya:

- Ecuación de la recta
- Pendiente e intercepto
- Gráfico con TikZ
- 4 opciones de respuesta tipo schoice
- Nivel 2, competencia interpretación y representación
- Contexto: movimiento de un objeto
```

**Resultado**: Prompt mejorado con:
- Reglas de estilo ICFES
- Ejemplos de ejercicios similares
- Metodología TikZ
- Templates disponibles

---

### Caso 2: Corregir Error Específico

```bash
pei
```

**Prompt:**
```
Error en volumen_cilindro_geometrico_metrico_interpretacion_n2_v1.Rmd:

Package pdftex.def Error: File 'cilindro_vaso.png' not found

¿Cómo lo corrijo?
```

**Resultado**: Prompt mejorado con:
- **Detección automática**: Analiza el archivo y detecta el problema
- **Solución documentada**: Referencia a Error 1 (patrones-errores-conocidos.md)
- **Código de ejemplo**: Solución verificada con código antes/después
- **Checklist de corrección**: Pasos específicos a seguir

---

### Caso 3: Adaptar Ejercicio Existente

```bash
pei
```

**Prompt:**
```
Adapta el ejercicio de probabilidad condicional para:

- Cambiar el contexto (de canciones a deportes)
- Aumentar la dificultad a nivel 3
- Mantener el diagrama de árbol TikZ
- Archivo: seleccion_canciones_cd_diagrama_arbol_n2_v1.Rmd
```

**Resultado**: Prompt mejorado con:
- Ejemplo del archivo mencionado
- Reglas de adaptación
- Metodología de diagramas TikZ
- Criterios de nivel 3

---

### Caso 4: Crear Script o Herramienta

```bash
pei
```

**Prompt:**
```
Necesito un script que:

- Valide metadatos ICFES en archivos .Rmd
- Verifique que todos los campos obligatorios estén presentes
- Genere un reporte de errores
- Sea compatible con el sistema existente
```

**Resultado**: Prompt mejorado con:
- Documentación técnica del sistema
- Ejemplos de scripts existentes
- Reglas de compatibilidad
- Mejores prácticas

---

## 🔧 Opciones Combinadas

### `pei` con Guardado en Archivo

Aunque `pei` es interactivo, puedes redirigir la salida:

```bash
pei > prompt_mejorado.txt
```

**Interacción:**
```
Ingresa tu prompt (presiona Ctrl+D cuando termines):
Tu prompt aquí
[Presiona Ctrl+D]
```

**Resultado**: Prompt mejorado guardado en `prompt_mejorado.txt`

---

### `pei` con Copia al Portapapeles (Manual)

```bash
pei | xclip -selection clipboard  # Linux X11
pei | wl-copy                       # Linux Wayland
pei | pbcopy                        # macOS
```

O usa `pec` para modo directo con copia automática.

---

## 🆕 Nuevas Funcionalidades (v1.2.0+)

### Detección Automática de Errores

Cuando mencionas un archivo .Rmd, el script ahora:

1. **Busca el archivo** en el prompt o directorio actual
2. **Analiza el código** buscando patrones problemáticos:
   - `abs()` sobre variables formateadas
   - `round()`, `floor()`, `ceiling()` sobre variables formateadas
   - `include_tikz()` en chunks de generación
3. **Muestra advertencias** específicas con referencias a documentación
4. **Incluye resumen** de errores conocidos y soluciones

**Ejemplo:**
```bash
pei
```

**Prompt:**
```
Error en mi_archivo.Rmd con abs(b_formateado)
```

**Resultado incluye:**
```
## 🚨 ERRORES DETECTADOS EN ARCHIVO .RMD

⚠️  **Error detectado**: Uso de abs() sobre variable formateada
   Patrón problemático: abs(variable_formateada)
   Solución: Aplicar abs() sobre valor numérico, luego formatear
   Documentación: .claude/docs/patrones-errores-conocidos.md#error-2

## 🚨 ERRORES CONOCIDOS Y SOLUCIONES

**Referencia completa:** .claude/docs/patrones-errores-conocidos.md

### Error 1: Imagen PNG no encontrada en compilación PDF
### Error 2: Argumento no numérico para función matemática abs()
```

---

## 📊 Comparación de Modos

| Característica | `pe` | `pec` | `pei` |
|----------------|------|-------|-------|
| Modo | Directo | Directo + Portapapeles | Interactivo |
| Múltiples líneas | ❌ (requiere comillas) | ❌ (requiere comillas) | ✅ Fácil |
| Edición | ❌ | ❌ | ✅ |
| Historial | ✅ | ✅ | ✅ |
| Copia automática | ❌ | ✅ | ❌ |
| Detección errores | ✅ | ✅ | ✅ |
| Ideal para | Prompts cortos | Prompts cortos + copia | Prompts largos |

---

## 🎯 Mejores Prácticas

### ✅ Usa `pei` cuando:

- El prompt tiene múltiples líneas
- Necesitas describir un problema complejo
- Quieres incluir código o ejemplos en el prompt
- El prompt es muy largo (> 100 caracteres)
- Necesitas editar el prompt antes de enviarlo

### ❌ No uses `pei` cuando:

- El prompt es muy corto (< 20 caracteres)
- Necesitas copiar automáticamente al portapapeles (usa `pec`)
- Estás en un script automatizado (usa ruta completa)

---

## 🔍 Solución de Problemas

### Problema: "command not found: pei"

**Solución:**
```bash
source ~/.bashrc  # o source ~/.zshrc
```

### Problema: Ctrl+D no funciona

**Explicación**: Ctrl+D envía EOF (End of File) al script. Si no funciona:
- Asegúrate de estar en una terminal real (no un editor)
- Presiona Enter antes de Ctrl+D
- O usa Ctrl+C para cancelar

### Problema: No detecta archivos .Rmd

**Solución**: 
- Menciona explícitamente el archivo en el prompt: `"error en archivo.Rmd"`
- O coloca el archivo .Rmd en el directorio actual

### Problema: El prompt mejorado es muy largo

**Explicación**: Es normal, incluye todo el contexto del proyecto.

**Solución**: 
- Redirige a archivo: `pei > prompt.txt`
- O usa `pec` para copiar y pegar solo lo necesario

---

## 📚 Referencias

- **Documentación principal**: `README.md`
- **Ejemplos de uso**: `02-EJEMPLOS_USO_PROMPT_ENHANCER.md`
- **Configuración de alias**: `04-CONFIGURACION_ALIAS.md`
- **Análisis de mejoras**: `06-ANALISIS_MEJORAS_DETECCION_ERRORES.md`

---

## ✅ Resumen Rápido

```bash
# Activar alias
source ~/.bashrc  # o source ~/.zshrc

# Usar modo interactivo
pei

# Escribir prompt (múltiples líneas OK)
# Presionar Ctrl+D cuando termines

# Resultado: Prompt mejorado con todo el contexto
```

---

**Versión**: 1.2.0+  
**Última actualización**: 2025-12-21  
**Autor**: Sistema ICFES R-Exams

