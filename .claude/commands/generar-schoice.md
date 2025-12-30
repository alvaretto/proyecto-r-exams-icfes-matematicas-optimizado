---
description: Genera ejercicio R-exams tipo SCHOICE (seleccion unica) - Despues requiere Ciclo de Validacion.
---

# Generador SCHOICE

**REGLAS CRITICAS**:
- @.claude/rules/flujo-b-obligatorio.md
- @.claude/rules/graficador-secuencial.md

Genera un archivo .Rmd de tipo **schoice** (seleccion unica) siguiendo la estructura
del proyecto.

## ⛔ BLOQUEO: Verificacion de Flujo B (OBLIGATORIO)

**ANTES de generar cualquier .Rmd, verificar:**

```
SI ejercicio tiene graficos detectados en /analizar-icfes:
    VERIFICAR que Flujo B fue completado:
    - workflow_state.json existe
    - tikz.usuario_aprobo == true
    - python.usuario_aprobo == true
    - r.usuario_aprobo == true
    - version_seleccionada != null

    SI Flujo B NO completado:
        BLOQUEAR generacion
        MOSTRAR mensaje de error
        REDIRIGIR a /auto-refinar-grafico tikz
```

### Mensaje de Bloqueo

```markdown
## ⛔ BLOQUEO: Flujo B Incompleto

Se detectaron graficos en este ejercicio pero el Flujo B (Graficador Experto)
no ha sido completado.

**Estado actual**:
- TikZ: [pendiente|en_iteracion|aprobado]
- Python: [pendiente|en_iteracion|aprobado]
- R: [pendiente|en_iteracion|aprobado]
- Version seleccionada: [ninguna]

**Accion requerida**:
Ejecutar `/auto-refinar-grafico tikz` para iniciar el proceso secuencial.

**NO SE PUEDE CONTINUAR SIN COMPLETAR FLUJO B**
```

## ⚡ IMPORTANTE: Despues de generar, ejecutar Ciclo de Validacion

```
Generación del archivo .Rmd
    │
    ▼
🔄 FASE 1: /validar-renderizado
    │
    ▼
🔍 FASE 2: /validar-coherencia
    │
    ▼
⚡ FASE 3: /diagnosticar-errores (si hay errores)
```

## Parámetros de entrada

- **$ARGUMENTS**: Ruta de imagen o descripción del ejercicio

## Ruta de generación
**Carpeta destino**: `/A-Produccion/En-Desarrollo/`

Una vez testeado, usar `/promover-ejercicio` para mover a `/A-Produccion/Nuevos-Ejercicios/`

## ⚠️ NOMENCLATURA OBLIGATORIA

**Todo archivo .Rmd DEBE seguir este formato:**

```
[ejercicio]_[componente]_[competencia]_n[nivel]_v[version].Rmd
```

### Componentes:

| Parte | Valores Permitidos |
|-------|-------------------|
| `[ejercicio]` | Nombre descriptivo en snake_case (ej: `series_temporales_poblacion`) |
| `[componente]` | `geometrico_metrico` \| `numerico_variacional` \| `aleatorio` |
| `[competencia]` | `interpretacion_representacion` \| `formulacion_ejecucion` \| `argumentacion` |
| `n[nivel]` | `n1` \| `n2` \| `n3` \| `n4` |
| `v[version]` | `v1`, `v2`, `v3`... |

### Ejemplo correcto:
```
series_temporales_poblacion_aleatorio_interpretacion_representacion_n2_v1.Rmd
```

### ❌ NUNCA usar nombres como:
- `poblacion_paises.Rmd` (incompleto)
- `ejercicio1.Rmd` (no descriptivo)
- `mi_ejercicio_v1.Rmd` (falta componente, competencia, nivel)

**Documentación completa:** `.claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md`

## Flujo de generación

### Paso 0: ⚠️ SELECCIÓN OBLIGATORIA DE VERSIÓN GRÁFICA

**Si el ejercicio incluye gráficos generados con el workflow del graficador:**

1. Verificar que existan las tres versiones (TikZ, Python, R)
2. **OBLIGATORIO: Preguntar al usuario cuál versión usar:**

```
Se han generado tres versiones del gráfico:

| Versión | Similitud | Integración R-exams |
|---------|-----------|---------------------|
| TikZ    | [X]%      | Imagen externa (.png/.pdf) |
| Python  | [Y]%      | Via reticulate |
| R       | [Z]%      | Nativo (RECOMENDADO) |

¿Cuál versión deseas usar para este ejercicio .Rmd?
```

3. **NO continuar hasta recibir respuesta del usuario**
4. Usar la versión seleccionada en el código del .Rmd

### Paso 1: Verificar clasificación
Confirma que el ejercicio fue clasificado con `/analizar-icfes`.

### Paso 2: Consultar ejemplos funcionales
```bash
# Ejemplos en producción
ls /A-Produccion/En-Produccion/*.Rmd | head -5

# Ejemplos en pre-desarrollo (también funcionales)
ls /A-Produccion/En-PreDesarrollo/**/*.Rmd | head -5
```

### Paso 3: Estructura obligatoria del .Rmd

1. **Encabezado YAML** con `output: pdf_document`, `header-includes` para TikZ/babel
2. **Chunk inicio**: Librerías (exams, tidyverse, knitr, reticulate)
3. **Chunk data_generation**: Función `generar_datos()` con aleatorización
4. **Chunk version_diversity_test**: Test de 300+ versiones únicas
5. **Sección Question**: Enunciado + Answerlist (4 opciones mínimo)
6. **Sección Solution**: Explicación detallada + Answerlist (Verdadero/Falso)
7. **Meta-information**:
   - `extype: schoice`
   - `exsolution: 1000` (posición de respuesta correcta)
   - `exshuffle: TRUE`

### Paso 4: Metadatos ICFES obligatorios
Incluir en comentarios YAML:
```yaml
# icfes:
#   competencia: [interpretacion_representacion|formulacion_ejecucion|argumentacion]
#   nivel_dificultad: [1|2|3|4]
#   componente: [geometrico_metrico|numerico_variacional|aleatorio]
```

### Paso 5: Crear CARPETA y Guardar con NOMENCLATURA OBLIGATORIA

**CRÍTICO:** Crear carpeta Y aplicar nomenclatura oficial ANTES de guardar.

```bash
# 1. Determinar componentes del nombre:
#    - ejercicio: descripción snake_case del contenido
#    - componente: geometrico_metrico | numerico_variacional | aleatorio
#    - competencia: interpretacion_representacion | formulacion_ejecucion | argumentacion
#    - nivel: n1 | n2 | n3 | n4
#    - version: v1 (primera versión)

# 2. Construir nombre completo:
NOMBRE="[ejercicio]_[componente]_[competencia]_n[nivel]_v[version]"

# 3. CREAR CARPETA con el mismo nombre:
mkdir -p outputs/$NOMBRE

# 4. Mover/copiar archivos relacionados a la carpeta:
mv outputs/output_tikz.tex outputs/$NOMBRE/
mv outputs/output_python.py outputs/$NOMBRE/
mv outputs/output_r.R outputs/$NOMBRE/
mv outputs/tikz_final.png outputs/$NOMBRE/
mv outputs/python_final.png outputs/$NOMBRE/
mv outputs/r_final.png outputs/$NOMBRE/
mv outputs/analisis_inicial.json outputs/$NOMBRE/
mv outputs/workflow_state.json outputs/$NOMBRE/
cp outputs/original.png outputs/$NOMBRE/  # Si existe

# 5. Guardar el .Rmd DENTRO de la carpeta:
# outputs/$NOMBRE/$NOMBRE.Rmd
```

**Ejemplo estructura final:**
```
outputs/series_temporales_poblacion_aleatorio_interpretacion_representacion_n2_v1/
├── series_temporales_poblacion_aleatorio_interpretacion_representacion_n2_v1.Rmd
├── output_tikz.tex
├── output_python.py
├── output_r.R
├── tikz_final.png
├── python_final.png
├── r_final.png
├── analisis_inicial.json
└── workflow_state.json
```

**IMPORTANTE:**
- El campo `exname` DEBE coincidir con el nombre del archivo (sin .Rmd)
- La carpeta DEBE tener el mismo nombre que el archivo .Rmd (sin extensión)

### Paso 6: Validación
Ejecutar skill `validar-diversidad-300` para confirmar aleatorización.

### Paso 7: Promoción (después de testear)
Una vez validado, usar `/promover-ejercicio [nombre.Rmd]` para mover a `/A-Produccion/Nuevos-Ejercicios/`

## ⚠️ ERRORES COMUNES DE COMPILACIÓN LATEX

### Error: Gráfico no se muestra en PDF/DOCX

**Causa**: R-exams no captura correctamente el output de chunks `{r grafico, ...}` con `print(p)`.

**Solución OBLIGATORIA**: Usar el patrón estándar de R-exams para figuras:

```r
# En el chunk data generation:
# 1. Crear el gráfico con ggplot2
p <- ggplot(...) + ...

# 2. Guardar como archivo PNG
ggsave("grafico.png", plot = p, width = 8, height = 5, dpi = 150)

# 3. Registrar como suplemento de R-exams
include_supplement("grafico.png")
```

```markdown
# En la sección Question:
# 4. Incluir con sintaxis Markdown
![](grafico.png)
```

**NUNCA** usar chunks separados `{r grafico, ...}` con `print(p)` - R-exams no los captura.

### Error: `\pandocbounded` undefined

**Causa**: Pandoc genera `\pandocbounded{}` cuando las imágenes no tienen tamaño especificado.

**Solución**: Los ejemplos funcionales en producción usan YAML header con configuración correcta:

```yaml
# ✓ CORRECTO - Seguir patrón de ejemplos funcionales
---
output:
  html_document: default
  word_document: default
  pdf_document:
    keep_tex: true
    extra_dependencies: ["graphicx", "float"]
icfes:
  competencia: interpretacion_representacion
  nivel_dificultad: 2
  componente: aleatorio
---
```

**Y en el chunk de gráfico**, usar `include_supplement()` + `![](imagen.png)`:

```r
ggsave("grafico.png", plot = p, width = 8, height = 5, dpi = 150)
include_supplement("grafico.png")
```

```markdown
![](grafico.png)
```

### Error: `\pandocbounded` undefined (sin YAML header)

**Causa**: Pandoc versiones recientes generan `\pandocbounded{}` cuando no se especifica tamaño de imagen.

**Solución**: Si usas chunks de figura (no recomendado), incluir `out.width`:

```r
# Solo si NO usas el patrón ggsave + include_supplement
```{r grafico, echo = FALSE, fig.height = 5, fig.width = 8, out.width = "90%"}
```

### Error: `big.mark` y `decimal.mark` ambiguos

**Causa**: Formato de números sin especificar ambos separadores.

**Solución**: Siempre especificar ambos para locale español:

```r
# ❌ INCORRECTO
format(x, big.mark = ".", scientific = FALSE)

# ✅ CORRECTO
format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
```

### Error: Unicode character not set up

**Causa**: Caracteres Unicode (emojis, símbolos especiales) en texto LaTeX.

**Solución**: Evitar emojis y usar solo ASCII en texto del ejercicio.

## ⛔ CONDICIONES CRÍTICAS

1. ✓ **SIEMPRE** consultar ejemplos funcionales ANTES de escribir código
2. ✓ **SIEMPRE** consultar ejemplos funcionales ANTES de corregir errores
3. ✓ **SIEMPRE** ejecutar Ciclo de Validación después de generar
4. ✓ **SIEMPRE** verificar VISUALMENTE cada gráfico después de renderizar
5. ✓ **SIEMPRE** incluir `out.width = "90%"` en chunks con gráficos
6. ✓ **Ejemplos funcionales** = Fuente de verdad ABSOLUTA
7. ❌ **NUNCA** promover sin completar validación
8. ❌ **NUNCA** asumir que lógica matemática correcta = visualización correcta

## ⚠️ COHERENCIA MATEMÁTICA EN GRÁFICOS

### Gráficos con cruces de líneas (intersecciones)

**PROBLEMA COMÚN**: El código calcula correctamente el punto de intersección, pero factores de escala o ajustes adicionales rompen el cruce visual.

**SOLUCIÓN OBLIGATORIA** (ver ejemplo funcional `poblaciones_paises_graficas_lineas_*.Rmd`):

```r
# 1. DEFINIR PRIMERO el punto de intersección (x, y)
x_interseccion <- (año_interseccion - 1960) / 5
y_interseccion <- sample(seq(2.5, 4.5, 0.1), 1)

# 2. GENERAR trayectorias que PASEN por ese punto
# País A: llega al punto desde abajo
y_inicial_a <- y_interseccion - (x_interseccion * tasa_a)
trayectoria_a <- y_inicial_a + (x * tasa_a)

# País B: llega al punto desde arriba
y_inicial_b <- y_interseccion - (x_interseccion * tasa_b)
trayectoria_b <- y_inicial_b + (x * tasa_b)

# 3. NO aplicar factores diferenciales después del cálculo
# ❌ INCORRECTO: pais_a * factor_escala * ajuste_extra
# ✓ CORRECTO: Ambos países usan los mismos factores
```

### Verificación visual OBLIGATORIA

Después de CADA renderización:

1. Abrir el PDF/DOCX generado
2. Verificar que el cruce ocurre en el año indicado
3. Verificar que las líneas son distinguibles
4. Si hay error visual → consultar ejemplo funcional → corregir

## Regla de Oro
**NUNCA improvises**. Consulta `/A-Produccion/En-Produccion/` antes de escribir o corregir.

## Referencias

- `/A-Produccion/Ejemplos-Funcionales-Rmd/` (FUENTE DE VERDAD)
- `.claude/Mermaid_Chart.txt` (diagrama de flujo oficial)

