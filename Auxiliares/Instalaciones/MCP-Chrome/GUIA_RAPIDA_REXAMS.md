# Guía Rápida: Chrome DevTools MCP para R/exams

## 🚀 Inicio Rápido

### 1. Genera tu examen con R
```r
library(exams)
exams2html("mi_examen.Rmd", dir = "out_html_cloze")
```

### 2. Usa el script de verificación
```bash
cd Auxiliares/Instalaciones/MCP-Chrome
./verificar_examen.sh
```

### 3. Copia el prompt generado y pégalo en Copilot (VS Code Insiders)

---

## 📋 Flujo de Trabajo Completo

```
┌─────────────────────────────────────────────────────────────┐
│ 1. GENERAR EXAMEN EN R                                      │
│    exams2html("examen.Rmd", dir = "out_html_cloze")        │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│ 2. EJECUTAR SCRIPT DE VERIFICACIÓN                          │
│    ./verificar_examen.sh                                    │
│    - Selecciona el examen de la lista                       │
│    - Elige tipo de verificación                             │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│ 3. COPIAR PROMPT GENERADO                                   │
│    El script genera el prompt completo                      │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│ 4. PEGAR EN COPILOT (VS CODE INSIDERS)                      │
│    Ctrl+Shift+I → Pegar prompt → Enter                      │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│ 5. REVISAR RESULTADOS                                        │
│    Copilot te mostrará:                                      │
│    - Errores encontrados                                     │
│    - Screenshots                                             │
│    - Reporte detallado                                       │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│ 6. CORREGIR PROBLEMAS (si hay)                               │
│    - Edita el archivo .Rmd                                   │
│    - Regenera el examen                                      │
│    - Vuelve al paso 2                                        │
└─────────────────────────────────────────────────────────────┘
```

---

## 🎯 Tipos de Verificación Disponibles

### 1️⃣ Verificación Básica
**Cuándo usar**: Después de generar cualquier examen
**Qué verifica**:
- ✅ Errores de JavaScript
- ✅ Imágenes rotas
- ✅ Screenshot general

**Comando**:
```bash
./verificar_examen.sh
# Opción 1
```

---

### 2️⃣ Análisis Completo
**Cuándo usar**: Antes de publicar un examen
**Qué verifica**:
- ✅ Errores y warnings
- ✅ Todas las imágenes
- ✅ MathJax
- ✅ Rendimiento
- ✅ Screenshots detallados

**Comando**:
```bash
./verificar_examen.sh
# Opción 2
```

---

### 3️⃣ Verificación Responsive
**Cuándo usar**: Para exámenes que se verán en diferentes dispositivos
**Qué verifica**:
- ✅ Vista móvil (375x667)
- ✅ Vista tablet (768x1024)
- ✅ Vista desktop (1920x1080)

**Comando**:
```bash
./verificar_examen.sh
# Opción 3
```

---

### 4️⃣ Verificación MathJax
**Cuándo usar**: Exámenes con muchas fórmulas matemáticas
**Qué verifica**:
- ✅ MathJax cargó correctamente
- ✅ No hay errores de renderizado
- ✅ Todas las fórmulas se ven bien

**Comando**:
```bash
./verificar_examen.sh
# Opción 4
```

---

### 5️⃣ Metadatos ICFES
**Cuándo usar**: Para validar metadatos de competencias
**Qué verifica**:
- ✅ Metadatos ICFES presentes
- ✅ Competencias asignadas
- ✅ Distribución de niveles

**Comando**:
```bash
./verificar_examen.sh
# Opción 5
```

---

## 💡 Ejemplos de Uso

### Ejemplo 1: Verificar examen recién generado

```bash
# 1. Generar examen en R
R -e "library(exams); exams2html('geometria_triangulos.Rmd', dir='out_html_cloze')"

# 2. Verificar
cd Auxiliares/Instalaciones/MCP-Chrome
./verificar_examen.sh

# 3. Seleccionar el archivo más reciente
# 4. Elegir opción 1 (Verificación Básica)
# 5. Copiar el prompt y pegarlo en Copilot
```

---

### Ejemplo 2: Análisis completo antes de publicar

```bash
# 1. Ya tienes el examen generado
cd Auxiliares/Instalaciones/MCP-Chrome
./verificar_examen.sh

# 2. Seleccionar el examen
# 3. Elegir opción 2 (Análisis Completo)
# 4. Copiar el prompt y pegarlo en Copilot
# 5. Revisar el reporte detallado
```

---

### Ejemplo 3: Verificar múltiples exámenes

```bash
# Generar todos los prompts de una vez
./verificar_examen.sh out_html_cloze/render11.html
# Opción 6 (Generar TODOS los prompts)

# Copiar cada prompt y ejecutarlo en Copilot uno por uno
```

---

## 🔧 Prompts Manuales (sin script)

Si prefieres escribir los prompts manualmente:

### Verificación Rápida
```
Abre file:///home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/out_html_cloze/render11.html y verifica errores
```

### Con Screenshot
```
Abre file:///.../out_html_cloze/render11.html, verifica errores y toma screenshot
```

### Verificar Imágenes
```
Abre file:///.../out_html_cloze/render11.html y lista todas las imágenes que no cargaron
```

---

## 🐛 Problemas Comunes y Soluciones

### Problema: "No se encontraron exámenes HTML"

**Solución**:
```bash
# Verifica que existan archivos HTML
ls -la out_html_cloze/*.html

# Si no hay archivos, genera uno primero en R
R -e "library(exams); exams2html('tu_examen.Rmd', dir='out_html_cloze')"
```

---

### Problema: "Copilot no reconoce el servidor MCP"

**Solución**:
```bash
# 1. Verifica la instalación
cd Auxiliares/Instalaciones/MCP-Chrome
./test_mcp.sh

# 2. Reinicia VS Code Insiders
killall code-insiders && code-insiders

# 3. Espera unos segundos y vuelve a intentar
```

---

### Problema: "Las imágenes no cargan en Chrome"

**Causa**: Rutas relativas en el HTML

**Solución**: Usa rutas absolutas en tus archivos .Rmd o verifica con:
```
Abre file:///.../out_html_cloze/render11.html y muéstrame las rutas de todas las imágenes
```

---

### Problema: "MathJax no renderiza"

**Solución**: Siempre espera unos segundos:
```
Abre file:///.../out_html_cloze/render11.html, espera 5 segundos, luego verifica MathJax
```

---

## 📊 Checklist de Calidad

Antes de publicar un examen, verifica:

- [ ] ✅ Sin errores de JavaScript
- [ ] ✅ Todas las imágenes cargan
- [ ] ✅ MathJax renderiza correctamente
- [ ] ✅ Se ve bien en móvil
- [ ] ✅ Se ve bien en tablet
- [ ] ✅ Se ve bien en desktop
- [ ] ✅ Metadatos ICFES correctos
- [ ] ✅ Tiempo de carga < 3 segundos

**Comando para verificar todo**:
```bash
./verificar_examen.sh
# Opción 6 (Generar TODOS los prompts)
```

---

## 🎓 Tips Avanzados

### 1. Crear alias para verificación rápida

Agrega a tu `~/.bashrc`:
```bash
alias verificar-examen='cd /ruta/a/MCP-Chrome && ./verificar_examen.sh'
```

Luego solo ejecuta:
```bash
verificar-examen
```

---

### 2. Integrar con tu flujo de trabajo R

Crea un script R que genere y verifique:
```r
# generar_y_verificar.R
library(exams)

# Generar examen
exams2html("mi_examen.Rmd", dir = "out_html_cloze")

# Mostrar mensaje
cat("\n✅ Examen generado!\n")
cat("Ahora ejecuta: ./Auxiliares/Instalaciones/MCP-Chrome/verificar_examen.sh\n")
```

---

### 3. Verificar batch de exámenes

Para verificar múltiples exámenes:
```bash
# Lista todos los exámenes
for file in out_html_cloze/*.html; do
    echo "Verificando: $file"
    ./verificar_examen.sh "$file"
    # Copiar y ejecutar cada prompt en Copilot
done
```

---

## 📚 Recursos

- [Guía completa de uso con R/exams](./USO_CON_REXAMS.md)
- [Documentación de instalación](./README_INSTALACION.md)
- [Ejemplos generales de MCP](./EJEMPLOS_USO.md)
- [Comandos útiles](./COMANDOS_UTILES.md)

---

## 🆘 Ayuda

Si tienes problemas:

1. **Ejecuta el script de ayuda**:
   ```bash
   ./ayuda.sh
   ```

2. **Verifica la instalación**:
   ```bash
   ./test_mcp.sh
   ```

3. **Consulta la documentación**:
   ```bash
   code-insiders USO_CON_REXAMS.md
   ```

---

**Recuerda**: Todos los prompts deben ejecutarse en **Copilot dentro de VS Code Insiders**, no en Augment Agent.

