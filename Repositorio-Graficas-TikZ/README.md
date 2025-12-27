# Repositorio Centralizado de Gráficas TikZ

Repositorio de código TikZ validado y reutilizable para ejercicios R-Exams del proyecto ICFES Matemáticas.

## Propósito

Este repositorio centraliza gráficas TikZ validadas visualmente que pueden ser reutilizadas en múltiples ejercicios, garantizando:

- **Consistencia visual**: Estilo uniforme entre ejercicios
- **Calidad**: Solo código TikZ validado entra al repositorio
- **Eficiencia**: Evita regenerar gráficas desde cero
- **Reutilización**: Templates parametrizables para diferentes contextos

## Estructura

```
Repositorio-Graficas-TikZ/
├── geometria/          # Gráficas geométricas
│   ├── cilindros/      # Cilindros, volúmenes 3D
│   ├── rectas/         # Rectas, geometría analítica
│   └── parabolas/      # Parábolas, funciones cuadráticas
├── estadistica/        # Gráficas estadísticas
│   ├── barras/         # Gráficos de barras
│   ├── puntos/         # Diagramas de dispersión
│   └── histogramas/    # Histogramas
├── probabilidad/       # Gráficas probabilísticas
│   ├── arboles_decision/  # Árboles de decisión
│   └── diagramas_venn/    # Diagramas de Venn
├── indice.json         # Catálogo completo con metadata
└── README.md           # Este archivo
```

## Formato de Archivos

Cada gráfica TikZ incluye tres archivos:

1. **`[nombre].tikz`**: Código TikZ con placeholders parametrizables
2. **`[nombre].json`**: Metadata con información de la gráfica
3. **`[nombre].png`**: Preview visual de la gráfica

### Ejemplo de Metadata (JSON)

```json
{
  "id": "cilindro_basico_01",
  "categoria": "geometria",
  "subcategoria": "cilindros",
  "descripcion": "Cilindro básico con radio y altura variables",
  "tags": ["cilindro", "volumen", "3D", "geometrico_metrico"],
  "parametros": ["radio", "altura"],
  "componente_icfes": "geometrico_metrico",
  "fecha_creacion": "2025-12-25",
  "validado": true,
  "origen": "graficador-experto"
}
```

## Parametrización

Los archivos TikZ usan placeholders para valores dinámicos:

```latex
\def\radioValor{%%RADIO%%}
\def\alturaValor{%%ALTURA%%}
```

Estos placeholders son reemplazados por las skills de generación de ejercicios con valores aleatorios del ejercicio.

## Uso en Workflow

### Consulta Automática

Durante `/generar-schoice` o `/generar-cloze`, si se detecta necesidad de gráficas:

1. Se consulta automáticamente este repositorio
2. Se listan opciones disponibles según tags/categoría
3. El usuario selecciona una existente o genera nueva
4. El código TikZ se integra en el .Rmd generado

### Generación de Nueva Gráfica

Para agregar nuevas gráficas:

1. Usar `/generar-grafica-nueva` con imagen de referencia
2. El sistema genera código TikZ iterativo con validación visual
3. Se guarda automáticamente en el repositorio con metadata
4. Se actualiza `indice.json`

## Gestión del Repositorio

Usar el script de gestión:

```bash
# Listar gráficas por categoría
.claude/scripts/gestionar_repo_tikz.sh listar geometria

# Buscar por tags
.claude/scripts/gestionar_repo_tikz.sh buscar "cilindro volumen"

# Validar integridad
.claude/scripts/gestionar_repo_tikz.sh validar

# Regenerar índice
.claude/scripts/gestionar_repo_tikz.sh reindexar
```

## Integración con Graficador-Experto

Este repositorio es el destino final de las gráficas generadas por el **Graficador-Experto** (`/Graficador-Experto/outputs/`). 

Un hook automático (`post-grafica-generada`) detecta cuando se genera nuevo código TikZ y:

1. Copia el archivo al repositorio
2. Solicita categoría y tags al usuario
3. Genera metadata JSON automáticamente
4. Actualiza el índice central

## Fuentes de Verdad

1. **Primera fuente**: `/A-Produccion/Ejemplos-Funcionales-Rmd/` (ejemplos completos funcionales)
2. **Segunda fuente**: Este repositorio (gráficas TikZ reutilizables)

## Convenciones de Nomenclatura

- Formato: `[tipo]_[variante]_[numero].tikz`
- Ejemplos:
  - `cilindro_basico_01.tikz`
  - `grafica_barras_horizontal_02.tikz`
  - `arbol_decision_binario_01.tikz`

## Validación

Todas las gráficas en este repositorio deben:

- ✅ Compilar correctamente con `pdflatex` o `tinytex`
- ✅ Ser parametrizables (usar placeholders)
- ✅ Tener metadata JSON completa
- ✅ Incluir preview PNG
- ✅ Estar validadas visualmente (98%+ fidelidad)

## Referencias

- `.claude/skills/consultar-grafica-tikz/` - Skill para consultar repositorio
- `.claude/commands/generar-grafica-nueva.md` - Comando para generar nuevas gráficas
- `.claude/hooks/post-grafica-generada.md` - Hook de integración automática
- `Graficador-Experto/README.md` - Documentación del generador

---

**Última actualización**: Diciembre 2025  
**Mantenedor**: Equipo ICFES Matemáticas

