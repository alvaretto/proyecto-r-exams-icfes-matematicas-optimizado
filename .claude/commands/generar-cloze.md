---
description: Genera ejercicio R-exams tipo CLOZE (pregunta compuesta) - Despues requiere Ciclo de Validacion.
---

# Generador CLOZE

**REGLAS CRITICAS**:
- @.claude/rules/flujo-b-obligatorio.md
- @.claude/rules/graficador-secuencial.md

Genera un archivo .Rmd de tipo **cloze** (pregunta compuesta con multiples gaps)
siguiendo la estructura del proyecto.

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

## ⚡ IMPORTANTE: Despues de generar, ejecutar Ciclo de Validacion Visual Iterativa

**Ver regla completa**: @.claude/rules/ciclo-validacion.md

```
Generación del archivo .Rmd
    │
    ▼
🔄 FASE 1: Renderizar (HTML, PDF, DOCX)
    │
    ▼
🔍 FASE 2: VALIDACIÓN VISUAL ITERATIVA (OBLIGATORIA)
    │
    ├── 2.1: Convertir PDF → PNG con magick
    ├── 2.2: MOSTRAR imagen al usuario (Read tool)
    ├── 2.3: Verificar 5 coherencias VISUALMENTE
    ├── 2.4: Documentar hallazgos con checklist
    └── 2.5: Comparar con imagen original (si aplica)
    │
    ▼
⚡ FASE 3: DECISIÓN
    │
    ├── SI problemas → Corregir → VOLVER A FASE 1
    │
    └── SI OK → Solicitar aprobación usuario → FIN
```

### ⚠️ PROHIBIDO (Antipatrones)

```markdown
# ❌ INCORRECTO - Validación "ciega"
"Los 3 formatos se generaron correctamente. Ejercicio completado."
# ← NO se mostró ninguna imagen, NO se verificaron coherencias

# ❌ INCORRECTO - Asumir éxito
"El PDF se generó sin errores."
# ← Solo verificó que no hubo error de compilación, no el contenido

# ❌ INCORRECTO - Saltarse comparación
"El gráfico se generó."
# ← Sin comparar con imagen original cuando aplica
```

### ✅ PATRÓN CORRECTO

```markdown
## Ciclo de Validación - Iteración 1

### FASE 1: Renderizado
- HTML: ✅ plain1.html (XXkb)
- PDF: ✅ plain1.pdf (XXkb)
- DOCX: ✅ pandoc1.docx (XXkb)

### FASE 2: Inspección Visual

[Imagen preview.png mostrada con Read tool]

#### Coherencias verificadas:
- [x] Semántica: Texto legible, tildes correctas
- [x] Visual-Texto: Gráfico coincide con enunciado
- [x] Matemática: Fórmulas correctas
- [x] Código: Elementos dinámicos funcionando
- [x] General: Legible, estilo ICFES

### FASE 3: Decisión
✅ Todas las coherencias OK

**¿Aprueba este ejercicio?**
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
| `[ejercicio]` | Nombre descriptivo en snake_case (ej: `probabilidad_condicional_dados`) |
| `[componente]` | `geometrico_metrico` \| `numerico_variacional` \| `aleatorio` |
| `[competencia]` | `interpretacion_representacion` \| `formulacion_ejecucion` \| `argumentacion` |
| `n[nivel]` | `n1` \| `n2` \| `n3` \| `n4` |
| `v[version]` | `v1`, `v2`, `v3`... |

### Ejemplo correcto:
```
probabilidad_condicional_dados_aleatorio_formulacion_ejecucion_n3_v1.Rmd
```

### ❌ NUNCA usar nombres como:
- `ejercicio_cloze.Rmd` (incompleto)
- `prob_cond_v1.Rmd` (abreviado, falta componente, competencia, nivel)

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

### Paso 2: Consultar ejemplos funcionales CLOZE
```bash
ls /06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/
```

### Paso 3: Estructura obligatoria del .Rmd CLOZE

1. **Encabezado YAML** con configuración completa
2. **Chunk inicio**: Librerías + configuración numérica
3. **Chunk data_generation**: 
   - Función `generar_datos()` con aleatorización completa
   - `options(scipen = 999)` para evitar notación científica
   - Funciones `formatear_entero()` y `formato_estandar()`
4. **Chunk version_diversity_test**: Test de 250+ versiones únicas (de 300 intentos)
5. **Sección Question**: 
   - Enunciado con gaps: `##ANSWER1##`, `##ANSWER2##`, etc.
   - Answerlist para cada gap
6. **Sección Solution**: Explicación detallada
7. **Meta-information CRÍTICA**:
   - `extype: cloze`
   - `exclozetype: schoice|num|string` (separados por `|`)
   - `extol: 0|1|0` (tolerancias: 0 para schoice, ≥1 para numéricos grandes)
   - `exsolution: 1000|42.5|texto`

### Paso 4: Configuración de tolerancias

- **schoice**: tolerancia = 0 (exactitud requerida)
- **num con valores grandes**: tolerancia ≥ 1
- **num con decimales pequeños**: tolerancia 0.01-0.1

### Paso 5: Metadatos ICFES obligatorios
```yaml
# icfes:
#   competencia: [interpretacion_representacion|formulacion_ejecucion|argumentacion]
#   nivel_dificultad: [1|2|3|4]
#   componente: [geometrico_metrico|numerico_variacional|aleatorio]
```

### Paso 6: Crear CARPETA y Guardar con NOMENCLATURA OBLIGATORIA

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
outputs/probabilidad_condicional_dados_aleatorio_formulacion_ejecucion_n3_v1/
├── probabilidad_condicional_dados_aleatorio_formulacion_ejecucion_n3_v1.Rmd
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

### Paso 7: Validación
Ejecutar skill `validar-diversidad` y `validar-metadatos-icfes`.

### Paso 8: Promoción (después de testear)
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

### OBLIGATORIO:
1. ✓ **SIEMPRE** consultar ejemplos funcionales ANTES de escribir código
2. ✓ **SIEMPRE** ejecutar Ciclo de Validación Visual Iterativa después de generar
3. ✓ **SIEMPRE** MOSTRAR preview.png al usuario con Read tool
4. ✓ **SIEMPRE** documentar las 5 coherencias con checklist explícito
5. ✓ **SIEMPRE** solicitar aprobación del usuario antes de marcar como completado
6. ✓ **SIEMPRE** comparar con imagen original cuando aplique
7. ✓ **SIEMPRE** configurar tolerancias apropiadas (0 para schoice, ≥1 para numéricos grandes)
8. ✓ **Ejemplos funcionales** = Fuente de verdad ABSOLUTA

### PROHIBIDO:
9. ❌ **NUNCA** promover sin completar validación visual
10. ❌ **NUNCA** marcar como "completado" sin inspección visual REAL
11. ❌ **NUNCA** decir "El PDF se generó correctamente" sin mostrar imagen
12. ❌ **NUNCA** asumir éxito solo porque no hubo errores de compilación

## Regla de Oro
**NUNCA improvises**. Consulta ejemplos funcionales en:

- `/A-Produccion/Ejemplos-Funcionales-Rmd/` (FUENTE DE VERDAD)

Para ejemplos CLOZE específicos también revisa:
`/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/`

## Referencias

- `/A-Produccion/Ejemplos-Funcionales-Rmd/` (FUENTE DE VERDAD)
- `.claude/Mermaid_Chart.txt` (diagrama de flujo oficial)

