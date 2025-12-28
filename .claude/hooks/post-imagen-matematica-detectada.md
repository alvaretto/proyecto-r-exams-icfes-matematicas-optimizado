# Hook: post-imagen-matematica-detectada

## Descripcion

Hook que se activa automaticamente cuando se detecta una imagen con contenido matematico ICFES. Inicia el workflow del Graficador-Experto para generar codigo triple.

## Evento Trigger

Se activa cuando:
1. Usuario comparte una imagen en la conversacion
2. La imagen contiene elementos matematicos detectados por Claude Vision
3. El contexto indica que se esta trabajando en un ejercicio ICFES

## Acciones del Hook

### 1. Registro de Deteccion

```markdown
[HOOK] Imagen matematica detectada
- Timestamp: [fecha/hora]
- Tipo: [Geometria/Estadistica/Calculo/etc.]
- Elementos: [lista]
- Complejidad: [Baja/Media/Alta]
```

### 2. Activacion del Graficador-Experto

Ejecutar automaticamente:

```bash
# Cambiar contexto al Graficador-Experto
cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/Graficador-Experto

# El sistema usa los skills del Graficador-Experto:
# - analizar-imagen-matematica
# - generar-tikz
# - generar-python
# - generar-r
# - comparar-visual
```

### 3. Generacion Secuencial de Codigo

```
Paso 1: Analizar imagen con Claude Vision
Paso 2: Generar codigo TikZ/LaTeX
Paso 3: Compilar y validar TikZ
Paso 4: Generar codigo Python
Paso 5: Ejecutar y validar Python
Paso 6: Generar codigo R
Paso 7: Ejecutar y validar R
Paso 8: Comparar fidelidad visual de los 3
Paso 9: Presentar opciones al usuario
```

### 4. Notificacion al Usuario

```markdown
## Graficador-Experto Activado

He detectado elementos matematicos en la imagen que requieren replicacion.

**Iniciando generacion de codigo triple:**
- TikZ/LaTeX (para diagramas precisos)
- Python (matplotlib/numpy via reticulate)
- R (ggplot2 nativo)

Esto tomara unos momentos...
```

## Flujo de Integracion

```
                    Usuario comparte imagen
                            ↓
            [HOOK] post-imagen-matematica-detectada
                            ↓
                    Graficador-Experto
                            ↓
            ┌───────────────┼───────────────┐
            ↓               ↓               ↓
         TikZ           Python             R
            ↓               ↓               ↓
         Validar        Validar         Validar
            ↓               ↓               ↓
            └───────────────┼───────────────┘
                            ↓
                    Presentar opciones
                            ↓
                    Usuario selecciona
                            ↓
                    Integrar en .Rmd
                            ↓
            Continuar workflow ICFES normal
```

## Condiciones de Activacion

### Activar SI:
- Imagen contiene graficos matematicos
- Imagen contiene figuras geometricas
- Imagen contiene graficos estadisticos
- Imagen contiene funciones o curvas
- Imagen contiene ejes coordenados
- Imagen contiene notacion matematica visual

### NO Activar SI:
- Imagen es solo texto sin graficos
- Imagen es tabla simple de datos
- Imagen es captura de pantalla sin contenido matematico
- Usuario explicitamente indica que no requiere replicacion

## Ubicacion de Outputs

Los codigos generados se guardan en:

```
Graficador-Experto/outputs/
├── output_tikz.tex       # TikZ/LaTeX
├── output_python.py      # Python matplotlib
├── output_r.R            # R ggplot2
├── reporte_matematico.md # Reporte del proceso
└── renders/
    ├── render_tikz.png   # Render TikZ
    ├── render_python.png # Render Python
    └── render_r.png      # Render R
```

## Logs

El hook registra en `.claude/logs/`:

```
[2025-12-27 14:30:00] HOOK: post-imagen-matematica-detectada
[2025-12-27 14:30:00] Tipo detectado: Geometria - Triangulo con medidas
[2025-12-27 14:30:01] Iniciando Graficador-Experto
[2025-12-27 14:30:15] TikZ generado: fidelidad 96%
[2025-12-27 14:30:25] Python generado: fidelidad 94%
[2025-12-27 14:30:35] R generado: fidelidad 95%
[2025-12-27 14:30:36] Esperando seleccion del usuario
```

## Manejo de Errores

### Error de Compilacion TikZ
```markdown
Advertencia: Error al compilar TikZ
- Intentando correccion automatica...
- Si persiste, se omitira TikZ y se presentaran Python y R
```

### Error de Ejecucion Python
```markdown
Advertencia: Error al ejecutar Python
- Verificando dependencias...
- Si persiste, se omitira Python y se presentaran TikZ y R
```

### Fidelidad Baja (<90%)
```markdown
Advertencia: Fidelidad visual menor al 90%
- TikZ: XX%
- Python: XX%
- R: XX%

¿Deseas:
1. Continuar con el mejor codigo disponible
2. Refinar iterativamente
3. Generar manualmente
```

## Integracion con Workflow Principal

Despues de la seleccion del usuario:

1. Codigo seleccionado se copia a carpeta del ejercicio
2. Se integra en chunk .Rmd apropiado
3. Si es TikZ validado → guardar en Repositorio-Graficas-TikZ
4. Continuar con `/analizar-icfes` si no se ha ejecutado
5. Proceder con `/generar-schoice` o `/generar-cloze`

## Dependencias

- Claude Vision (analisis de imagenes)
- Graficador-Experto (generacion de codigo)
- LaTeX con TikZ (compilacion)
- Python 3.8+ con matplotlib, numpy
- R 4.0+ con ggplot2

---

**Version:** 1.0
**Fecha:** 2025-12-27
**Tipo:** Hook automatico
