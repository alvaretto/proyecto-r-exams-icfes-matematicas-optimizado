# 🌐 CONFIGURACIÓN COMPLETA DE GITHUB PAGES

## 📋 OBJETIVO
Configurar GitHub Pages para hospedar los demos HTML interactivos con URLs directas y accesibles desde LinkedIn.

---

## 🎯 ESTRATEGIA DE HOSTING

### Opción Recomendada: Rama `gh-pages` con Carpeta `/docs`

**VENTAJAS:**

- ✅ URLs limpias y profesionales
- ✅ Separación clara entre código fuente y demos públicos
- ✅ Fácil actualización sin afectar rama principal
- ✅ Compatible con LinkedIn (sin redirecciones)
- ✅ Soporte completo para HTML/CSS/JS

**URL RESULTANTE:**
```
https://alvaretto.github.io/proyecto-r-exams-icfes-matematicas-optimizado/
```

---

## 🔧 PASO 1: PREPARAR ESTRUCTURA LOCAL

### 1.1 Crear Rama gh-pages

```bash
cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams

# Crear y cambiar a rama gh-pages
git checkout -b gh-pages

# Crear estructura de directorios
mkdir -p docs/demos
mkdir -p docs/recursos
mkdir -p docs/assets/css
mkdir -p docs/assets/js
mkdir -p docs/assets/img
```

### 1.2 Estructura de Directorios Resultante

```
docs/
├── index.html                    # Página principal
├── demos/
│   ├── demo_consumo_telefonico_v1.html
│   ├── demo_consumo_telefonico_v2.html
│   ├── demo_consumo_telefonico_v3.html
│   ├── demo_consumo_telefonico_v4.html
│   └── demo_consumo_telefonico_v5.html
├── recursos/
│   ├── muestra_10_versiones_consumo_telefonico.pdf
│   ├── consumo_telefonico_moodle.xml
│   └── consumo_telefonico_adicional_n2_v1.Rmd
├── assets/
│   ├── css/
│   │   └── estilos.css
│   ├── js/
│   │   └── analytics.js
│   └── img/
│       └── logo-proyecto.png
```

---

## 🎨 PASO 2: CREAR PÁGINA INDEX PROFESIONAL

### 2.1 Archivo `docs/index.html`

```html
<!DOCTYPE html>
<html lang="es">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta name="description" content="Demos interactivos de ejercicios matemáticos ICFES generados con R/exams">
    <meta name="keywords" content="ICFES, Matemáticas, R/exams, Educación, Ejercicios Dinámicos">
    <meta name="author" content="Proyecto ICFES R-Exams 2025">
    
    <!-- Open Graph para LinkedIn -->
    <meta property="og:title" content="Demos Interactivos - Matemáticas ICFES R/exams">
    <meta property="og:description" content="Explora ejercicios matemáticos dinámicos con 300+ versiones únicas">
    <meta property="og:type" content="website">
    <meta property="og:url" content="https://alvaretto.github.io/proyecto-r-exams-icfes-matematicas-optimizado/">
    
    <title>Demos Interactivos - Matemáticas ICFES R/exams</title>
    
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }
        
        body {
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            min-height: 100vh;
            padding: 20px;
        }
        
        .container {
            max-width: 900px;
            margin: 0 auto;
            background: white;
            border-radius: 20px;
            box-shadow: 0 20px 60px rgba(0,0,0,0.3);
            padding: 40px;
        }
        
        h1 {
            color: #2c3e50;
            text-align: center;
            margin-bottom: 10px;
            font-size: 2.5em;
        }
        
        .subtitle {
            text-align: center;
            color: #7f8c8d;
            margin-bottom: 30px;
            font-size: 1.1em;
        }
        
        .intro {
            background: #ecf0f1;
            padding: 20px;
            border-radius: 10px;
            margin-bottom: 30px;
            line-height: 1.6;
        }
        
        .section-title {
            color: #34495e;
            margin: 30px 0 15px 0;
            font-size: 1.5em;
            border-left: 4px solid #3498db;
            padding-left: 15px;
        }
        
        .demo-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
            gap: 15px;
            margin-bottom: 30px;
        }
        
        .demo-link {
            display: block;
            padding: 20px;
            background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
            color: white;
            text-decoration: none;
            border-radius: 10px;
            text-align: center;
            transition: transform 0.3s, box-shadow 0.3s;
            font-weight: bold;
        }
        
        .demo-link:hover {
            transform: translateY(-5px);
            box-shadow: 0 10px 25px rgba(52, 152, 219, 0.4);
        }
        
        .resource-list {
            list-style: none;
        }
        
        .resource-list li {
            margin: 10px 0;
        }
        
        .resource-list a {
            display: inline-block;
            padding: 12px 20px;
            background: #2ecc71;
            color: white;
            text-decoration: none;
            border-radius: 5px;
            transition: background 0.3s;
        }
        
        .resource-list a:hover {
            background: #27ae60;
        }
        
        .footer {
            text-align: center;
            margin-top: 40px;
            padding-top: 20px;
            border-top: 2px solid #ecf0f1;
            color: #7f8c8d;
        }
        
        .badge {
            display: inline-block;
            background: #e74c3c;
            color: white;
            padding: 5px 10px;
            border-radius: 15px;
            font-size: 0.8em;
            margin-left: 10px;
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>🎯 Demos Interactivos</h1>
        <p class="subtitle">Ejercicios Matemáticos ICFES con R/exams</p>
        
        <div class="intro">
            <p><strong>¿Qué hace especial a este sistema?</strong></p>
            <ul style="margin-left: 20px; margin-top: 10px;">
                <li>✅ <strong>300+ versiones únicas</strong> del mismo ejercicio</li>
                <li>✅ <strong>Verificación automática</strong> de respuestas</li>
                <li>✅ <strong>Explicaciones detalladas</strong> paso a paso</li>
                <li>✅ <strong>Gráficos dinámicos</strong> generados con Python</li>
                <li>✅ <strong>Compatible con Moodle</strong> y otros LMS</li>
            </ul>
        </div>
        
        <h2 class="section-title">📊 Demos Interactivos <span class="badge">NUEVO</span></h2>
        <p style="margin-bottom: 15px;">Explora 5 versiones diferentes del ejercicio "Consumo Telefónico Adicional":</p>
        
        <div class="demo-grid">
            <a href="demos/demo_consumo_telefonico_v1.html" class="demo-link">
                📊 Demo Versión 1
            </a>
            <a href="demos/demo_consumo_telefonico_v2.html" class="demo-link">
                📊 Demo Versión 2
            </a>
            <a href="demos/demo_consumo_telefonico_v3.html" class="demo-link">
                📊 Demo Versión 3
            </a>
            <a href="demos/demo_consumo_telefonico_v4.html" class="demo-link">
                📊 Demo Versión 4
            </a>
            <a href="demos/demo_consumo_telefonico_v5.html" class="demo-link">
                📊 Demo Versión 5
            </a>
        </div>
        
        <h2 class="section-title">📥 Recursos Descargables</h2>
        <ul class="resource-list">
            <li>
                <a href="recursos/muestra_10_versiones_consumo_telefonico.pdf" target="_blank">
                    📄 PDF con 10 Versiones del Ejercicio
                </a>
            </li>
            <li>
                <a href="recursos/consumo_telefonico_moodle.xml" download>
                    🎓 Archivo Moodle XML (Importar a LMS)
                </a>
            </li>
            <li>
                <a href="https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado/blob/main/A-Produccion/En-Desarrollo/consumo_telefonico_adicional/consumo_telefonico_adicional_n2_v1.Rmd" target="_blank">
                    💻 Código Fuente (.Rmd) en GitHub
                </a>
            </li>
        </ul>
        
        <div class="footer">
            <p><strong>Proyecto ICFES R-Exams 2025</strong></p>
            <p>Tecnología: R/exams + exams2forms + Python + TikZ</p>
            <p style="margin-top: 10px;">
                <a href="https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado" 
                   style="color: #3498db; text-decoration: none;">
                    🔗 Ver Repositorio Completo en GitHub
                </a>
            </p>
        </div>
    </div>
</body>
</html>
```

---

## 🚀 PASO 3: COPIAR ARCHIVOS GENERADOS

### 3.1 Script de Copia Automatizada

Crear archivo: `Estrategia-LinkedIn/scripts/copiar_a_docs.sh`

```bash
#!/bin/bash
# ============================================================================
# SCRIPT: Copiar demos y recursos a carpeta docs/ para GitHub Pages
# ============================================================================

echo "🚀 Iniciando copia de archivos a docs/..."

# Directorio base
BASE_DIR="/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/En-Desarrollo/consumo_telefonico_adicional"

# Copiar demos HTML
echo "📊 Copiando demos HTML..."
cp -v "$BASE_DIR/Estrategia-LinkedIn/demos-html"/*.html docs/demos/

# Copiar recursos descargables
echo "📥 Copiando recursos descargables..."
cp -v "$BASE_DIR/Estrategia-LinkedIn/recursos-descargables"/*.pdf docs/recursos/
cp -v "$BASE_DIR/Estrategia-LinkedIn/recursos-descargables"/*.xml docs/recursos/

# Copiar código fuente .Rmd
echo "💻 Copiando código fuente..."
cp -v "$BASE_DIR/consumo_telefonico_adicional_n2_v1.Rmd" docs/recursos/

echo "✅ Copia completada exitosamente"
echo "📁 Archivos listos en: docs/"
```

### 3.2 Ejecutar Script

```bash
chmod +x Estrategia-LinkedIn/scripts/copiar_a_docs.sh
./Estrategia-LinkedIn/scripts/copiar_a_docs.sh
```

---

## ⚙️ PASO 4: CONFIGURAR GITHUB PAGES EN EL REPOSITORIO

### 4.1 Commit y Push de la Rama gh-pages

```bash
# Asegurarse de estar en rama gh-pages
git checkout gh-pages

# Agregar archivos
git add docs/

# Commit
git commit -m "🚀 Configuración inicial GitHub Pages - Demos interactivos consumo telefónico"

# Push a GitHub
git push origin gh-pages
```

### 4.2 Activar GitHub Pages en el Repositorio

**PASOS EN GITHUB.COM:**

1. **Ir al repositorio**: `https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado`

2. **Settings** → **Pages** (menú lateral izquierdo)

3. **Source**:
   - Branch: `gh-pages`
   - Folder: `/docs`
   - Click **Save**

4. **Esperar deployment** (2-5 minutos)

5. **Verificar URL activa**:
   ```
   https://alvaretto.github.io/proyecto-r-exams-icfes-matematicas-optimizado/
   ```

---

## ✅ PASO 5: VERIFICACIÓN DE FUNCIONAMIENTO

### 5.1 Checklist de URLs

Verificar que las siguientes URLs funcionan correctamente:

```
✓ Página principal:
  https://alvaretto.github.io/proyecto-r-exams-icfes-matematicas-optimizado/

✓ Demos interactivos:
  https://alvaretto.github.io/proyecto-r-exams-icfes-matematicas-optimizado/demos/demo_consumo_telefonico_v1.html
  https://alvaretto.github.io/proyecto-r-exams-icfes-matematicas-optimizado/demos/demo_consumo_telefonico_v2.html
  https://alvaretto.github.io/proyecto-r-exams-icfes-matematicas-optimizado/demos/demo_consumo_telefonico_v3.html
  https://alvaretto.github.io/proyecto-r-exams-icfes-matematicas-optimizado/demos/demo_consumo_telefonico_v4.html
  https://alvaretto.github.io/proyecto-r-exams-icfes-matematicas-optimizado/demos/demo_consumo_telefonico_v5.html

✓ Recursos descargables:
  https://alvaretto.github.io/proyecto-r-exams-icfes-matematicas-optimizado/recursos/muestra_10_versiones_consumo_telefonico.pdf
  https://alvaretto.github.io/proyecto-r-exams-icfes-matematicas-optimizado/recursos/consumo_telefonico_moodle.xml
```

### 5.2 Pruebas de Compatibilidad

**NAVEGADORES:**

- ✅ Chrome/Edge (Desktop)
- ✅ Firefox (Desktop)
- ✅ Safari (Desktop)
- ✅ Chrome Mobile (Android)
- ✅ Safari Mobile (iOS)

**FUNCIONALIDADES:**

- ✅ Botones interactivos (✓, ?, ↺)
- ✅ Verificación de respuestas
- ✅ Renderizado de fórmulas MathJax
- ✅ Visualización de gráficos PNG
- ✅ Descarga de recursos PDF/XML

---

## 🔒 PASO 6: CONFIGURACIÓN DE DOMINIO PERSONALIZADO (OPCIONAL)

### Si deseas usar un dominio propio (ej: demos.matematicas-icfes.com)

1. **Comprar dominio** en proveedor (Namecheap, GoDaddy, etc.)

2. **Configurar DNS** con los siguientes registros:

```
Tipo: CNAME
Host: demos
Valor: alvaretto.github.io
```

3. **En GitHub Pages Settings**:
   - Custom domain: `demos.matematicas-icfes.com`
   - Enforce HTTPS: ✅ Activado

---

## 📊 ANALYTICS Y SEGUIMIENTO (OPCIONAL)

### Agregar Google Analytics

Crear archivo: `docs/assets/js/analytics.js`

```javascript
// Google Analytics 4
window.dataLayer = window.dataLayer || [];
function gtag(){dataLayer.push(arguments);}
gtag('js', new Date());
gtag('config', 'G-XXXXXXXXXX'); // Reemplazar con tu ID
```

Incluir en `index.html` antes de `</body>`:

```html
<!-- Google Analytics -->
<script async src="https://www.googletagmanager.com/gtag/js?id=G-XXXXXXXXXX"></script>
<script src="assets/js/analytics.js"></script>
```

---

## 🔄 ACTUALIZACIÓN DE CONTENIDO

### Workflow para Agregar Nuevos Demos

```bash
# 1. Generar nuevos demos con R
Rscript Estrategia-LinkedIn/scripts/generar_demos_individuales.R

# 2. Copiar a docs/
./Estrategia-LinkedIn/scripts/copiar_a_docs.sh

# 3. Commit y push
git add docs/
git commit -m "➕ Nuevos demos: [nombre del ejercicio]"
git push origin gh-pages

# 4. Esperar deployment automático (2-3 minutos)
```

---

## 🎯 MEJORES PRÁCTICAS

### SEO y Accesibilidad

- ✅ **Meta tags** completos en todas las páginas
- ✅ **Alt text** en todas las imágenes
- ✅ **Títulos descriptivos** en cada demo
- ✅ **URLs semánticas** (sin caracteres especiales)
- ✅ **Responsive design** para móviles

### Performance

- ✅ **Comprimir imágenes** (PNG optimizados)
- ✅ **Minificar CSS/JS** (opcional)
- ✅ **Lazy loading** para imágenes grandes
- ✅ **Cache headers** configurados por GitHub Pages

---

## 🆘 SOLUCIÓN DE PROBLEMAS

### Error 404 en GitHub Pages

**Causa**: Deployment no completado o configuración incorrecta

**Solución**:
```bash
# Verificar que docs/ existe en rama gh-pages
git checkout gh-pages
ls -la docs/

# Verificar configuración en GitHub Settings → Pages
```

### Demos no se visualizan correctamente

**Causa**: Rutas relativas incorrectas

**Solución**: Usar rutas absolutas en HTML:
```html
<link href="/proyecto-r-exams-icfes-matematicas-optimizado/assets/css/estilos.css" rel="stylesheet">
```

### Cambios no se reflejan

**Causa**: Cache del navegador

**Solución**:

- Ctrl + Shift + R (forzar recarga)
- Esperar 5-10 minutos para propagación CDN

---

## 📚 RECURSOS ADICIONALES

- **GitHub Pages Docs**: https://docs.github.com/en/pages
- **Custom Domain Setup**: https://docs.github.com/en/pages/configuring-a-custom-domain-for-your-github-pages-site
- **Troubleshooting**: https://docs.github.com/en/pages/getting-started-with-github-pages/troubleshooting-404-errors-for-github-pages-sites

---

## 🎯 PRÓXIMOS PASOS

1. ✅ Configurar GitHub Pages (esta guía)
2. ⏭️ Crear publicación LinkedIn → Ver `03-TEMPLATE_Publicacion_LinkedIn.md`
3. ⏭️ Automatizar publicaciones diarias → Ver `04-AUTOMATIZACION_Publicaciones_Diarias.md`

---

**FECHA DE CREACIÓN**: Diciembre 2025
**ÚLTIMA ACTUALIZACIÓN**: Diciembre 2025
**ESTADO**: ✅ Configuración completa y funcional

