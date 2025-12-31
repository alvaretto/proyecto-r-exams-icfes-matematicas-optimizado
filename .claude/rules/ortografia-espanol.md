# Ortografía Española - Diccionario de Referencia

**Copiar palabras TAL CUAL de aquí. No improvisar.**

## Palabras Frecuentes

```
más  según  así  después  también  además  aquí  ahí

ángulo  gráfica  gráfico  función  número  cálculo  método  código  propósito  patrón  máximo  mínimo  análisis  éxito

dispersión  solución  ecuación  relación  variación  descripción  información  configuración  clasificación  validación  explicación  distribución  combinación  iteración  sección  versión  dimensión  selección

matemático  estadística  científico  parabólico  geométrico  numérico  teórico  único  dinámico  automático  semántico
```

## Excepciones (SIN tilde)

### 1. Variables R
- `angulos`, `solucion`, `grafica`, `numero`, `codigo`, `metodo`
- Razón: Nombres de variables R deben ser ASCII para compatibilidad

### 2. Metadatos R-exams (OBLIGATORIO ASCII)
Los siguientes campos NUNCA llevan tildes porque R-exams los usa como identificadores:

```
exname: nombre_ejercicio_sin_tildes
exsection: Numerico-Variacional/Argumentacion
extype: schoice
exsolution: 1000
exshuffle: TRUE
extol: 0.01
exextra[Competencia]: Interpretacion
exextra[Componente]: Aleatorio
```

**Razón**: R-exams parsea estos campos como identificadores. Las tildes pueden causar errores de codificación en algunos sistemas.

### 3. Inglés técnico
- TikZ, LaTeX, R-exams, chunk, hash, reticulate, ggplot2

### 4. Pronombres demostrativos
- "Esta/Estas" (≠ "Está" verbo estar)
- "Este/Estos" (≠ "Esté" subjuntivo)

## Validación Automática

```bash
# Verificar errores (sin corregir)
Rscript .claude/scripts/corregir_ortografia_espanol.R archivo.Rmd

# Aplicar correcciones
Rscript .claude/scripts/corregir_ortografia_espanol.R archivo.Rmd --fix
```

El script excluye automáticamente:
1. Metadatos R-exams (exname, exsection, extype, etc.)
2. Nombres de variables R en contexto de código
3. Código inline `` `r variable` ``

## NUNCA usar --no-verify

Si el hook de ortografía detecta errores:
1. Verificar si son falsos positivos (metadatos R-exams → ya excluidos)
2. Si son errores reales → corregirlos con `--fix`
3. **NUNCA** usar `git commit --no-verify` para evadir el hook
