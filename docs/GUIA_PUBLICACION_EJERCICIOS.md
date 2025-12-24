# 📘 Guía de Publicación de Nuevos Ejercicios en GitHub Pages

## 🎯 Objetivo
Esta guía explica el flujo completo para publicar un nuevo ejercicio matemático ICFES en el sitio web de GitHub Pages.

---

## 📋 Flujo de Trabajo Completo

### **PASO 1: Crear el Ejercicio .Rmd**

1. **Ubicación del archivo**:
   ```
   A-Produccion/En-Desarrollo/[nombre_ejercicio]/[nombre_ejercicio]_[componente]_[competencia]_n[nivel]_v1.Rmd
   ```

2. **Ejemplo**:
   ```
   A-Produccion/En-Desarrollo/consumo_telefonico_adicional/consumo_telefonico_adicional_n2_v1.Rmd
   ```

3. **Seguir la estructura obligatoria** definida en las reglas generales del proyecto

---

### **PASO 2: Compilar el Ejercicio en RStudio**

#### **2.1 Generar Versiones HTML Individuales (Demos)**

```r
# En RStudio, ejecutar:
library(exams)

# Generar 5 versiones HTML para demos
exams2html(
  "consumo_telefonico_adicional_n2_v1.Rmd",
  n = 5,
  name = "demo_consumo_telefonico",
  dir = "output_html",
  encoding = "UTF-8",
  solution = TRUE,
  mathjax = TRUE
)
```

**Resultado**: Se crean archivos `demo_consumo_telefonico_v1.html`, `v2.html`, etc.

#### **2.2 Generar PDF con Múltiples Versiones**

```r
# Generar PDF con 10 versiones
exams2pdf(
  "consumo_telefonico_adicional_n2_v1.Rmd",
  n = 10,
  name = "muestra_10_versiones_consumo_telefonico",
  dir = "output_pdf",
  encoding = "UTF-8",
  solution = TRUE
)
```

#### **2.3 Generar Archivo Moodle XML**

```r
# Generar archivo para importar a Moodle
exams2moodle(
  "consumo_telefonico_adicional_n2_v1.Rmd",
  n = 50,
  name = "consumo_telefonico_moodle",
  dir = "output_moodle",
  encoding = "UTF-8"
)
```

---

### **PASO 3: Organizar Archivos en la Carpeta `docs/`**

#### **3.1 Estructura de carpetas**

```
docs/
├── demos/                          # Versiones HTML individuales
│   ├── demo_[ejercicio]_v1.html
│   ├── demo_[ejercicio]_v2.html
│   └── ...
├── recursos/                       # Archivos descargables
│   ├── [ejercicio].Rmd            # Código fuente
│   ├── [ejercicio]_moodle.xml     # Archivo Moodle
│   └── muestra_10_versiones_[ejercicio].pdf
└── index.html                      # Página principal
```

#### **3.2 Copiar archivos generados**

```bash
# Desde el directorio del proyecto
cd A-Produccion/En-Desarrollo/[nombre_ejercicio]/

# Copiar demos HTML
cp output_html/demo_*.html ../../../docs/demos/

# Copiar PDF
cp output_pdf/muestra_10_versiones_*.pdf ../../../docs/recursos/

# Copiar XML de Moodle
cp output_moodle/*_moodle.xml ../../../docs/recursos/

# Copiar archivo .Rmd fuente
cp [nombre_ejercicio]_n*_v1.Rmd ../../../docs/recursos/
```

---

### **PASO 4: Actualizar `docs/index.html`**

#### **4.1 Agregar nueva sección de demos**

Editar `docs/index.html` y agregar:

```html
<h2 class="section-title">📊 [Nombre del Ejercicio] <span class="badge">NUEVO</span></h2>
<p style="margin-bottom: 15px;">Explora 5 versiones diferentes del ejercicio <strong>"[Nombre Descriptivo]"</strong> - Nivel ICFES [N]:</p>

<div class="demo-grid">
    <a href="demos/demo_[ejercicio]_v1.html" class="demo-link">
        📊 Demo Versión 1
    </a>
    <a href="demos/demo_[ejercicio]_v2.html" class="demo-link">
        📊 Demo Versión 2
    </a>
    <a href="demos/demo_[ejercicio]_v3.html" class="demo-link">
        📊 Demo Versión 3
    </a>
    <a href="demos/demo_[ejercicio]_v4.html" class="demo-link">
        📊 Demo Versión 4
    </a>
    <a href="demos/demo_[ejercicio]_v5.html" class="demo-link">
        📊 Demo Versión 5
    </a>
</div>
```

#### **4.2 Agregar recursos descargables**

```html
<h2 class="section-title">📥 Recursos Descargables - [Nombre Ejercicio]</h2>
<ul class="resource-list">
    <li>
        <a href="recursos/muestra_10_versiones_[ejercicio].pdf" target="_blank">
            📄 PDF con 10 Versiones del Ejercicio
        </a>
    </li>
    <li>
        <a href="recursos/[ejercicio]_moodle.xml" download>
            🎓 Archivo Moodle XML (Importar a LMS)
        </a>
    </li>
    <li>
        <a href="https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado/blob/main/A-Produccion/En-Desarrollo/[ejercicio]/[ejercicio]_n*_v1.Rmd" target="_blank">
            💻 Código Fuente (.Rmd) en GitHub
        </a>
    </li>
</ul>
```

---

### **PASO 5: Commit y Push a GitHub**

```bash
# Verificar cambios
git status

# Agregar todos los archivos nuevos
git add docs/

# Commit con mensaje descriptivo
git commit -m "Agregar nuevo ejercicio: [Nombre del Ejercicio] - Nivel ICFES [N]"

# Push a la rama principal
git push origin main
```

---

### **PASO 6: Actualizar rama `gh-pages` (si es necesaria)**

Si trabajas con rama separada para GitHub Pages:

```bash
# Cambiar a rama gh-pages
git checkout gh-pages

# Merge desde main
git merge main

# Push a gh-pages
git push origin gh-pages

# Volver a main
git checkout main
```

---

## ✅ Verificación Final

1. **Esperar 1-2 minutos** para que GitHub Actions compile el sitio
2. **Visitar**: https://alvaretto.github.io/proyecto-r-exams-icfes-matematicas-optimizado/
3. **Verificar**:
   - ✅ Los demos HTML se cargan correctamente
   - ✅ Los enlaces de descarga funcionan
   - ✅ El PDF se visualiza correctamente
   - ✅ El archivo Moodle XML se descarga
   - ✅ El enlace al código fuente en GitHub funciona

---

## 🎯 Checklist Rápido

- [ ] Ejercicio .Rmd creado y probado en RStudio
- [ ] Generadas 5 versiones HTML (demos)
- [ ] Generado PDF con 10 versiones
- [ ] Generado archivo Moodle XML
- [ ] Archivos copiados a `docs/demos/` y `docs/recursos/`
- [ ] `docs/index.html` actualizado con nueva sección
- [ ] Commit y push realizados
- [ ] Sitio verificado en GitHub Pages

---

## 📝 Notas Importantes

- **Nombres consistentes**: Usar el mismo nombre base para todos los archivos del ejercicio
- **Versiones HTML**: Siempre generar al menos 5 versiones para mostrar diversidad
- **PDF de muestra**: 10 versiones es un buen número para demostración
- **Moodle XML**: Generar 50+ versiones para uso real en LMS
- **Código fuente**: Siempre incluir el enlace al .Rmd en GitHub

---

## 🚀 Automatización Futura (Opcional)

Considera crear un script R que automatice todo el proceso:

```r
# publicar_ejercicio.R
publicar_ejercicio <- function(archivo_rmd, nombre_ejercicio, nivel_icfes) {
  # 1. Generar HTMLs
  # 2. Generar PDF
  # 3. Generar Moodle XML
  # 4. Copiar archivos a docs/
  # 5. Actualizar index.html automáticamente
}
```

---

**¡Listo!** Ahora tienes el flujo completo para publicar nuevos ejercicios en GitHub Pages. 🎉

