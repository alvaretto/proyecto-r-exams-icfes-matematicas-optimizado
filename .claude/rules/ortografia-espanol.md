# Regla: Ortografía y Formato en Español

## Principio Fundamental

**Todos los archivos de documentación (.md) y ejercicios (.Rmd) DEBEN escribirse con ortografía correcta en español, incluyendo tildes y acentos.**

Esta regla aplica a:

- Archivos README.md y WALKTHROUGH.md
- Archivos .Rmd (secciones Question, Solution, comentarios)
- Documentación en `.claude/`
- Comentarios extensos en código R
- Cualquier texto en español destinado a usuarios

## Regla de Formato: Renglón Vacío Antes de Listados

**OBLIGATORIO**: Antes de todo listado (con o sin viñeta) se debe agregar un renglón vacío para mejorar legibilidad.

### Incorrecto

```markdown
Los requisitos son:
- Requisito 1
- Requisito 2
- Requisito 3

El proceso incluye:
1. Paso uno
2. Paso dos
3. Paso tres
```

### Correcto

```markdown
Los requisitos son:

- Requisito 1
- Requisito 2
- Requisito 3

El proceso incluye:

1. Paso uno
2. Paso dos
3. Paso tres
```

### Aplica también a listados sin viñeta

```markdown
Incorrecto:
Las variables son:
v0 = velocidad inicial
g = gravedad
theta = ángulo

Correcto:
Las variables son:

v0 = velocidad inicial
g = gravedad
theta = ángulo
```

## Palabras Frecuentes con Tilde

### Sustantivos y Adjetivos Comunes

| Incorrecto | Correcto |
|------------|----------|
| informacion | información |
| descripcion | descripción |
| explicacion | explicación |
| configuracion | configuración |
| solucion | solución |
| validacion | validación |
| clasificacion | clasificación |
| ecuacion | ecuación |
| dimension | dimensión |
| version | versión |
| seleccion | selección |
| seccion | sección |
| funcion | función |
| relacion | relación |
| distribucion | distribución |
| variacion | variación |
| dispersion | dispersión |
| combinacion | combinación |
| iteracion | iteración |
| compilacion | compilación |
| instalacion | instalación |
| documentacion | documentación |
| retroalimentacion | retroalimentación |

### Términos Técnicos

| Incorrecto | Correcto |
|------------|----------|
| grafica | gráfica |
| grafico | gráfico |
| matematico | matemático |
| estadistica | estadística |
| aleatorio | aleatorio (sin tilde) |
| cientifico | científico |
| parabolico | parabólico |
| geometrico | geométrico |
| numerico | numérico |
| teorico | teórico |
| unico | único |
| dinamico | dinámico |
| automatico | automático |
| semantico | semántico |

### Verbos y Formas Verbales

| Incorrecto | Correcto |
|------------|----------|
| codigo | código |
| proposito | propósito |
| analisis | análisis |
| numero | número |
| angulo | ángulo |
| calculo | cálculo |
| metodo | método |
| exito | éxito |
| patron | patrón |
| maximo | máximo |
| minimo | mínimo |

### Adverbios y Conectores

| Incorrecto | Correcto |
|------------|----------|
| mas | más |
| tambien | también |
| asi | así |
| aqui | aquí |
| ahi | ahí |
| despues | después |
| segun | según |
| solo (adverbio) | sólo/solo |

### Palabras con Diéresis

| Incorrecto | Correcto |
|------------|----------|
| linguistico | lingüístico |
| verguenza | vergüenza |
| pinguino | pingüino |
| bilinguismo | bilingüismo |

## Reglas de Acentuación

### Palabras Agudas (acento en última sílaba)

Llevan tilde si terminan en vocal, n o s:

- información, versión, función, además

### Palabras Graves (acento en penúltima sílaba)

Llevan tilde si NO terminan en vocal, n o s:

- fácil, difícil, útil, árbol

### Palabras Esdrújulas (acento en antepenúltima sílaba)

SIEMPRE llevan tilde:

- gráfico, matemático, estadística, parabólico

### Palabras Sobresdrújulas

SIEMPRE llevan tilde:

- explícamelo, dígaselo

## Verificación Automática

### Herramientas Recomendadas

1. **aspell** (línea de comandos):

```bash
aspell --lang=es check archivo.md
```

2. **hunspell**:

```bash
hunspell -d es_ES archivo.md
```

3. **Extensiones de editor**:

- VS Code: "Spanish - Code Spell Checker"
- Vim: spell con `set spelllang=es`

### Palabras Técnicas Permitidas sin Tilde

Estas palabras técnicas en inglés NO deben tildarse:

- TikZ, LaTeX, R-exams, Markdown, YAML
- HTML, PDF, DOCX, NOPS
- GitHub, Moodle, reticulate
- chunk, hash, framework

## Aplicación de la Regla

### Al crear archivos nuevos

- Escribir SIEMPRE con ortografía correcta desde el inicio
- Usar autocompletado de editor con diccionario español
- Agregar renglón vacío antes de cada listado

### Al editar archivos existentes

- Corregir errores ortográficos encontrados
- Agregar renglones vacíos faltantes antes de listados
- NO introducir nuevos errores

### Excepciones

- Código fuente (variables, funciones) mantienen nombres sin tildes por compatibilidad
- Nombres de archivos sin tildes por compatibilidad con sistemas de archivos
- Texto en inglés mantiene ortografía inglesa

## Aplicación en Archivos .Rmd

### Secciones que requieren ortografía correcta

- `Question` (enunciado del ejercicio)
- `Solution` (retroalimentación)
- Comentarios en chunks de R (`# comentario`)
- Texto en `Answerlist`

### Ejemplo en .Rmd

```r
# Incorrecto
# Este chunk genera la grafica de dispersion
# y calcula la solucion del ejercicio

# Correcto
# Este chunk genera la gráfica de dispersión
# y calcula la solución del ejercicio
```

```markdown
Incorrecto:
Question
========
En la grafica se muestran los resultados de un experimento.
Segun los datos:
- La relacion es lineal
- La dispersion aumenta

Correcto:
Question
========
En la gráfica se muestran los resultados de un experimento.
Según los datos:

- La relación es lineal
- La dispersión aumenta
```

## Fórmulas Matemáticas

Las expresiones matemáticas deben formatearse con LaTeX:

### Incorrecto

```markdown
R = (v0^2 * sin(2*theta)) / g
```

### Correcto

```markdown
$$R = \frac{v_0^2 \cdot \sin(2\theta)}{g}$$
```

---

**Fecha de creación**: 2025-12-30
**Versión**: 1.1
**Aplicación**: OBLIGATORIA para toda documentación y ejercicios en español

## Cambios v1.1

- Incluidos archivos .Rmd en el alcance de la regla
- Nueva regla: renglón vacío obligatorio antes de listados
- Ejemplos actualizados para .Rmd
