---
name: post-grafica-generada
trigger: PostToolUse
pattern: write.*Graficador-Experto/outputs/.*\.(tex|tikz)
description: Hook que detecta cuando se guarda código TikZ en Graficador-Experto/outputs/ y lo copia automáticamente al repositorio centralizado con metadata.
---

# Hook: Post-Gráfica Generada

## Trigger

Se activa automáticamente después de guardar archivos `.tex` o `.tikz` en:

```
Graficador-Experto/outputs/
```

## Propósito

Detectar cuando el Graficador-Experto genera nuevo código TikZ y:

1. Copiar automáticamente al repositorio centralizado
2. Solicitar metadata necesaria (categoría, tags, descripción)
3. Generar archivo JSON de metadata
4. Crear preview PNG si es posible
5. Actualizar índice del repositorio

## Comportamiento

### Detección de Archivo TikZ

Cuando se detecta guardado de `.tex` o `.tikz` en `Graficador-Experto/outputs/`:

```
✅ Detectado: Nuevo código TikZ generado
   Archivo: Graficador-Experto/outputs/output_tikz.tex
   
   ¿Deseas guardarlo en el repositorio centralizado? [Sí/No]
```

### Si Usuario Confirma: Sí

**Paso 1: Analizar código TikZ**

- Leer contenido del archivo
- Detectar parámetros (placeholders `%%PARAMETRO%%`)
- Identificar tipo de gráfica (geometría, estadística, probabilidad)
- Extraer información básica del código

**Paso 2: Solicitar Metadata**

```
📋 Información necesaria para guardar en repositorio:

1. Categoría: [geometria|estadistica|probabilidad]
2. Subcategoría: [cilindros|barras|arboles_decision|...]
3. Descripción: [Descripción breve de la gráfica]
4. Tags (separados por comas): [tag1, tag2, tag3]
5. Componente ICFES: [geometrico_metrico|aleatorio|...]

¿Continuar? [Sí/No]
```

**Paso 3: Generar Nombre Único**

- Formato: `[tipo]_[variante]_[numero].tikz`
- Verificar que no existe en repositorio
- Si existe, incrementar número

**Paso 4: Copiar Archivos**

```bash
# Copiar código TikZ
cp Graficador-Experto/outputs/output_tikz.tex \
   Repositorio-Graficas-TikZ/[categoria]/[subcategoria]/[nombre].tikz

# Si existe preview PNG, copiarlo también
if [ -f "Graficador-Experto/outputs/output_tikz.png" ]; then
  cp Graficador-Experto/outputs/output_tikz.png \
     Repositorio-Graficas-TikZ/[categoria]/[subcategoria]/[nombre].png
fi
```

**Paso 5: Generar Metadata JSON**

```json
{
  "id": "[nombre]",
  "categoria": "[categoria_proporcionada]",
  "subcategoria": "[subcategoria_proporcionada]",
  "descripcion": "[descripcion_proporcionada]",
  "tags": ["tag1", "tag2", "tag3"],
  "parametros": [
    // Extraídos automáticamente del código TikZ
  ],
  "componente_icfes": "[componente_proporcionado]",
  "fecha_creacion": "[YYYY-MM-DD]",
  "validado": true,
  "origen": "graficador-experto",
  "ruta_origen": "Graficador-Experto/outputs/output_tikz.tex"
}
```

**Paso 6: Crear Preview PNG (si no existe)**

Si no hay preview PNG disponible:

```bash
# Compilar TikZ a PDF
cd Repositorio-Graficas-TikZ/[categoria]/[subcategoria]/
pdflatex [nombre].tikz

# Convertir PDF a PNG
convert [nombre].pdf [nombre].png -density 150

# Limpiar archivos temporales
rm [nombre].aux [nombre].log [nombre].pdf
```

**Paso 7: Actualizar Índice**

Actualizar `Repositorio-Graficas-TikZ/indice.json`:

- Agregar entrada en array `graficas`
- Incrementar contadores
- Actualizar fecha

**Paso 8: Confirmación**

```
✅ Gráfica TikZ guardada exitosamente en repositorio

📁 Ubicación: Repositorio-Graficas-TikZ/[categoria]/[subcategoria]/[nombre].tikz
📊 Metadata: [nombre].json
🖼️ Preview: [nombre].png

La gráfica está disponible para uso en ejercicios.
```

### Si Usuario Confirma: No

```
⏭️ Saltando guardado en repositorio.
   El archivo permanece en Graficador-Experto/outputs/
   
   Puedes guardarlo manualmente más tarde usando:
   /generar-grafica-nueva [ruta/al/archivo.tikz]
```

## Casos Especiales

### Archivo Ya Existe en Repositorio

```
⚠️ Ya existe una gráfica con nombre similar: [nombre].tikz

Opciones:
1. Sobrescribir existente
2. Guardar con nuevo nombre: [nombre]_v2.tikz
3. Cancelar

¿Qué deseas hacer? [1/2/3]
```

### No Se Puede Determinar Categoría Automáticamente

```
❓ No se pudo determinar automáticamente la categoría de la gráfica.

Por favor, proporciona:
- Categoría: [geometria|estadistica|probabilidad]
- Subcategoría: [especificar]
```

### Error al Generar Preview

```
⚠️ No se pudo generar preview PNG automáticamente.

El código TikZ se guardó correctamente, pero el preview deberá
generarse manualmente más tarde.

¿Continuar? [Sí/No]
```

## Integración con Otros Hooks

- **pre-edit-rmd-validation**: No se activa (solo para .Rmd)
- **post-exams2-validation**: No relacionado

## Beneficios

1. **Automatización**: No requiere pasos manuales para guardar en repositorio
2. **Consistencia**: Metadata estructurada garantizada
3. **Inmediatez**: Gráficas disponibles inmediatamente después de generarse
4. **Trazabilidad**: Registro de origen en metadata

## Referencias

- `Repositorio-Graficas-TikZ/README.md` - Documentación del repositorio
- `.claude/commands/generar-grafica-nueva.md` - Comando manual de guardado
- `.claude/agents/graficador-tikz.md` - Agente generador de TikZ
- `Graficador-Experto/README.md` - Documentación del Graficador-Experto

## Notas Técnicas

- **Trigger pattern**: Detecta escritura de `.tex` o `.tikz` en `Graficador-Experto/outputs/`
- **Validación**: Verifica que el código TikZ es válido antes de copiar
- **Encoding**: Preserva encoding UTF-8 del archivo original
- **Dependencias**: Requiere `pdflatex` y `convert` (ImageMagick) para preview

