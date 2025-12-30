# Regla: Workflow Secuencial del Graficador Experto

## Principio Fundamental

**Las tres versiones graficas (TikZ, Python, R) se generan e iteran SECUENCIALMENTE, no simultaneamente.**

Cada lenguaje debe completarse ANTES de pasar al siguiente.

## Orden Obligatorio

```
1. TikZ (dinamico desde R)
   ↓ Iterar hasta ≥95% + coherencias + aprobacion usuario
   ↓
2. Python (via reticulate)
   ↓ Iterar hasta ≥95% + coherencias + aprobacion usuario
   ↓
3. R (nativo ggplot2)
   ↓ Iterar hasta ≥95% + coherencias + aprobacion usuario
   ↓
4. Seleccion final por usuario
```

## Proceso Detallado por Lenguaje

### FASE 1: TikZ (Dinamico desde R)

**Paso 1.1: Generacion inicial**
```
- Analizar imagen original
- Generar codigo TikZ integrado con R (NO estatico)
- El codigo R debe generar coordenadas/datos dinamicamente
- TikZ se construye con paste0() interpolando variables R
```

**Paso 1.2: Renderizado**
```bash
# Compilar TikZ a PNG para comparacion
pdflatex output_tikz_v1.tex
magick convert -density 150 output_tikz_v1.pdf output_tikz_v1.png
```

**Paso 1.3: Comparacion visual**
```
- Comparar tikz_output_v1.png con imagen original
- Calcular similitud (%)
- SI similitud < 95%: Refinar y repetir
- SI similitud >= 95%: Continuar a verificaciones
```

**Paso 1.4: Verificacion de coherencias (TODAS obligatorias)**

```markdown
## Verificacion de Coherencias - TikZ v[N]

### 1. Coherencia Semantica (Gramatica)
- [ ] Etiquetas de ejes sin errores ortograficos
- [ ] Leyendas correctamente escritas
- [ ] Titulo (si existe) gramaticalmente correcto
- [ ] Numeros con formato apropiado (comas, puntos)

### 2. Coherencia Visual con Texto
- [ ] Grafico coincide con descripcion del enunciado
- [ ] Valores visuales coinciden con valores mencionados
- [ ] Colores/estilos coherentes con la descripcion
- [ ] Escalas apropiadas segun el contexto

### 3. Coherencia Matematica
- [ ] Formulas/ecuaciones correctas si aplica
- [ ] Proporciones geometricas correctas
- [ ] Relaciones matematicas preservadas
- [ ] Puntos de interseccion en posiciones correctas

### 4. Coherencia de Codigo
- [ ] Codigo genera grafico dinamicamente (no hardcoded)
- [ ] Variables R se interpolan correctamente en TikZ
- [ ] Compatible con include_tikz() de R-exams
- [ ] Diferentes semillas generan graficos validos

### 5. Coherencia General
- [ ] Grafico legible (tamano de fuente adecuado)
- [ ] Estilo visual apropiado para examen ICFES
- [ ] No hay elementos visuales confusos
- [ ] Calidad de imagen suficiente
```

**Paso 1.5: Aprobacion del usuario**

```markdown
## TikZ v[N] - Solicitud de Aprobacion

**Similitud alcanzada**: [X]%

[Mostrar imagen generada]

### Verificacion de Coherencias
- Semantica: OK/Pendiente
- Visual-Texto: OK/Pendiente
- Matematica: OK/Pendiente
- Codigo: OK/Pendiente
- General: OK/Pendiente

**¿Aprueba esta version de TikZ?**
- Si, aprobar y continuar con Python
- No, necesito ajustes: [describir]
- Continuar iterando automaticamente
```

**SOLO continuar a Python cuando usuario apruebe TikZ.**

### FASE 2: Python (via reticulate)

**Paso 2.1: Generacion inicial**
```
- Usar misma logica matematica que TikZ
- Generar codigo Python/matplotlib
- Asegurar compatibilidad con reticulate
```

**Paso 2.2-2.5: Repetir proceso de TikZ**
- Renderizado, comparacion, coherencias, aprobacion

**SOLO continuar a R cuando usuario apruebe Python.**

### FASE 3: R (nativo ggplot2)

**Paso 3.1: Generacion inicial**
```
- Usar misma logica matematica que versiones anteriores
- Generar codigo R/ggplot2 nativo
- Formato natural para R-exams
```

**Paso 3.2-3.5: Repetir proceso**
- Renderizado, comparacion, coherencias, aprobacion

**SOLO proceder a seleccion cuando usuario apruebe R.**

### FASE 4: Seleccion Final

```markdown
## Seleccion de Version para el .Rmd

Las tres versiones han sido validadas:

| Version | Similitud | Ventajas | Desventajas |
|---------|-----------|----------|-------------|
| TikZ    | [X]%      | Tipografia LaTeX, escalable | Requiere compilacion |
| Python  | [Y]%      | Flexible, familiar | Dependencia reticulate |
| R       | [Z]%      | Nativo R-exams | Menos control tipografico |

**¿Cual version desea usar para el archivo .Rmd final?**
```

## Estados del Workflow

```json
{
  "fase_actual": "tikz_iteracion|tikz_coherencias|tikz_aprobacion|python_iteracion|...|seleccion_final",
  "tikz": {
    "estado": "pendiente|en_iteracion|verificando|aprobado",
    "version_actual": 1,
    "similitud_actual": 0,
    "coherencias_verificadas": false,
    "usuario_aprobo": false
  },
  "python": {
    "estado": "bloqueado|pendiente|en_iteracion|verificando|aprobado",
    "version_actual": 0,
    "similitud_actual": 0,
    "coherencias_verificadas": false,
    "usuario_aprobo": false
  },
  "r": {
    "estado": "bloqueado|pendiente|en_iteracion|verificando|aprobado",
    "version_actual": 0,
    "similitud_actual": 0,
    "coherencias_verificadas": false,
    "usuario_aprobo": false
  }
}
```

## Reglas de Transicion

```
tikz.estado == "aprobado" → python.estado = "pendiente"
python.estado == "aprobado" → r.estado = "pendiente"
r.estado == "aprobado" → fase_actual = "seleccion_final"
```

## Prohibiciones

### PROHIBIDO: Generacion Simultanea
```
# ❌ INCORRECTO - NO HACER
generar_tikz() AND generar_python() AND generar_r()  # Simultaneo
```

### PROHIBIDO: Saltar Aprobaciones
```
# ❌ INCORRECTO - NO HACER
if similitud >= 95:
    marcar_validado()  # Sin pedir aprobacion
    pasar_siguiente()
```

### PROHIBIDO: Ignorar Coherencias
```
# ❌ INCORRECTO - NO HACER
if similitud >= 95:
    aprobar()  # Sin verificar coherencias
```

## Flujo Correcto Paso a Paso

```
1. /auto-refinar-grafico tikz 95
   - Iterar TikZ hasta >=95%
   - Verificar 5 coherencias
   - Mostrar resultado y ESPERAR aprobacion
   - Usuario aprueba

2. /auto-refinar-grafico python 95
   - Iterar Python hasta >=95%
   - Verificar 5 coherencias
   - Mostrar resultado y ESPERAR aprobacion
   - Usuario aprueba

3. /auto-refinar-grafico r 95
   - Iterar R hasta >=95%
   - Verificar 5 coherencias
   - Mostrar resultado y ESPERAR aprobacion
   - Usuario aprueba

4. Preguntar: ¿Cual version para el .Rmd?
   - Usuario selecciona
   - Registrar en workflow_state.json

5. Generar .Rmd con version seleccionada
```

## Integracion con R-exams

### TikZ Dinamico (NO estatico)

```r
# Generar datos en R
datos <- generar_datos()

# Construir codigo TikZ interpolando variables R
tikz_code <- paste0(
  "\\begin{tikzpicture}\n",
  "\\begin{axis}[\n",
  "    xlabel={", datos$xlabel, "},\n",
  "    ylabel={", datos$ylabel, "},\n",
  "]\n",
  "\\addplot coordinates {\n",
  paste0("(", datos$x, ",", datos$y, ")", collapse="\n"),
  "\n};\n",
  "\\end{axis}\n",
  "\\end{tikzpicture}"
)

# Usar include_tikz de R-exams
include_tikz(tikz_code, name="grafico", ...)
```

### Python via Reticulate

```r
library(reticulate)
py_run_string(codigo_python)
# O usar source_python("output_python.py")
```

### R Nativo

```r
p <- ggplot(datos, aes(x, y)) + geom_point()
ggsave("grafico.png", p)
include_supplement("grafico.png")
```

---

**Fecha de creacion**: 2025-12-30
**Version**: 1.0
**Autor**: Sistema automatizado
**Razon**: Garantizar proceso ordenado y aprobacion explicita del usuario
