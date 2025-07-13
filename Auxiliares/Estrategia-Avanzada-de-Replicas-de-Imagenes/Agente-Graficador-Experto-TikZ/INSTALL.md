# 🛠️ Guía de Instalación - Agente Graficador Experto TikZ

## 📋 Requisitos del Sistema

### Sistema Operativo
- **Linux:** Ubuntu 18.04+, Debian 10+, CentOS 7+
- **macOS:** 10.14+ (Mojave)
- **Windows:** 10+ (con WSL recomendado)

### Software Base Requerido
- **Python:** 3.8 o superior
- **LaTeX:** Distribución completa (TeX Live, MiKTeX)
- **ImageMagick:** Para conversión de imágenes
- **Git:** Para clonación del repositorio

## 🚀 Instalación Paso a Paso

### 1. Preparar el Sistema

#### Ubuntu/Debian
```bash
# Actualizar sistema
sudo apt update && sudo apt upgrade -y

# Instalar dependencias del sistema
sudo apt install -y python3 python3-pip python3-venv git

# Instalar LaTeX completo
sudo apt install -y texlive-full

# Instalar ImageMagick
sudo apt install -y imagemagick

# Verificar instalaciones
python3 --version
pdflatex --version
convert --version
```

#### macOS (con Homebrew)
```bash
# Instalar Homebrew si no está instalado
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Instalar dependencias
brew install python git imagemagick

# Instalar LaTeX (MacTeX)
brew install --cask mactex

# Actualizar PATH
echo 'export PATH="/usr/local/texlive/2023/bin/universal-darwin:$PATH"' >> ~/.zshrc
source ~/.zshrc
```

#### Windows (WSL)
```bash
# Instalar WSL2 con Ubuntu
wsl --install -d Ubuntu

# Dentro de WSL, seguir instrucciones de Ubuntu/Debian
```

### 2. Clonar el Repositorio

```bash
# Navegar al directorio deseado
cd /ruta/a/tu/proyecto

# Clonar o copiar el agente
# (Si está en un repositorio Git)
git clone <url-repositorio> Agente-Graficador-Experto-TikZ

# O copiar directamente los archivos
cp -r /ruta/origen/Agente-Graficador-Experto-TikZ ./
```

### 3. Configurar Entorno Python

```bash
# Navegar al directorio del agente
cd Agente-Graficador-Experto-TikZ

# Crear entorno virtual
python3 -m venv venv

# Activar entorno virtual
source venv/bin/activate  # Linux/macOS
# o
venv\Scripts\activate     # Windows

# Actualizar pip
pip install --upgrade pip

# Instalar dependencias
pip install -r requirements.txt
```

### 4. Verificar Instalación

```bash
# Ejecutar demo de verificación
python demo.py

# O verificación interactiva
python demo.py --interactivo
```

## 🔧 Configuración Avanzada

### Configurar ImageMagick (Importante)

ImageMagick puede tener restricciones de seguridad que impiden la conversión de PDFs:

```bash
# Editar política de ImageMagick
sudo nano /etc/ImageMagick-6/policy.xml

# Buscar la línea que contiene:
# <policy domain="coder" rights="none" pattern="PDF" />

# Cambiarla por:
# <policy domain="coder" rights="read|write" pattern="PDF" />

# Guardar y salir
```

### Configurar LaTeX para TikZ

```bash
# Verificar que los paquetes TikZ están instalados
kpsewhich tikz.sty
kpsewhich pgfplots.sty

# Si no están instalados, instalar manualmente
tlmgr install tikz pgfplots
```

### Configurar Qtikz/Ktikz (Opcional)

Para testing visual interactivo:

```bash
# Ubuntu/Debian
sudo apt install qtikz

# macOS
brew install --cask qtikz

# Verificar instalación
qtikz --version
```

## 🧪 Testing de la Instalación

### Test Básico

```python
# test_instalacion.py
from agente_core import AgenteTikZ
import sys

try:
    agente = AgenteTikZ()
    print("✅ Agente inicializado correctamente")
    
    # Test de generación básica
    analisis_test = {
        'exitoso': True,
        'tipo_detectado': 'funcion',
        'dimensiones': (800, 600),
        'ejes': {'detectados': True}
    }
    
    codigo = agente.generador.generar(analisis_test)
    if codigo:
        print("✅ Generación de código funcional")
    else:
        print("❌ Error en generación de código")
        sys.exit(1)
    
    print("🎉 Instalación verificada exitosamente")
    
except Exception as e:
    print(f"❌ Error en instalación: {e}")
    sys.exit(1)
```

```bash
# Ejecutar test
python test_instalacion.py
```

### Test de Compilación LaTeX

```bash
# Crear archivo de test
cat > test_latex.tex << 'EOF'
\documentclass{standalone}
\usepackage{tikz}
\usepackage{pgfplots}
\pgfplotsset{compat=1.18}

\begin{document}
\begin{tikzpicture}
\draw[->] (0,0) -- (2,0) node[right] {$x$};
\draw[->] (0,0) -- (0,2) node[above] {$y$};
\draw[blue, thick] (0,0) -- (1,1);
\end{tikzpicture}
\end{document}
EOF

# Compilar
pdflatex test_latex.tex

# Verificar que se generó el PDF
ls test_latex.pdf && echo "✅ LaTeX funcional" || echo "❌ Error en LaTeX"

# Limpiar
rm test_latex.*
```

### Test de ImageMagick

```bash
# Crear PDF simple y convertir a PNG
echo '\documentclass{standalone}\begin{document}Test\end{document}' > test.tex
pdflatex test.tex
convert test.pdf test.png

# Verificar conversión
ls test.png && echo "✅ ImageMagick funcional" || echo "❌ Error en ImageMagick"

# Limpiar
rm test.*
```

## 🐛 Solución de Problemas

### Error: "pdflatex not found"

```bash
# Verificar instalación de LaTeX
which pdflatex

# Si no está instalado
sudo apt install texlive-latex-extra  # Ubuntu/Debian
brew install mactex                    # macOS

# Actualizar PATH si es necesario
export PATH="/usr/local/texlive/2023/bin/x86_64-linux:$PATH"
```

### Error: "convert: not authorized"

```bash
# Problema de política de ImageMagick
sudo sed -i 's/rights="none" pattern="PDF"/rights="read|write" pattern="PDF"/' /etc/ImageMagick-6/policy.xml

# Reiniciar si es necesario
sudo systemctl restart imagemagick
```

### Error: "ModuleNotFoundError: No module named 'cv2'"

```bash
# Reinstalar OpenCV
pip uninstall opencv-python
pip install opencv-python

# O versión específica
pip install opencv-python==4.5.5.64
```

### Error: "Permission denied" en directorio temporal

```bash
# Crear directorio temporal con permisos
mkdir -p /tmp/agente_tikz_validacion
chmod 755 /tmp/agente_tikz_validacion

# O cambiar directorio temporal en config.json
{
  "directorio_temporal": "/home/usuario/tmp/agente_tikz"
}
```

### Problemas de Memoria en Imágenes Grandes

```bash
# Aumentar límites de ImageMagick
sudo nano /etc/ImageMagick-6/policy.xml

# Modificar:
<policy domain="resource" name="memory" value="2GiB"/>
<policy domain="resource" name="disk" value="8GiB"/>
```

## 📦 Instalación con Docker (Alternativa)

### Dockerfile

```dockerfile
FROM ubuntu:20.04

# Evitar prompts interactivos
ENV DEBIAN_FRONTEND=noninteractive

# Instalar dependencias del sistema
RUN apt-get update && apt-get install -y \
    python3 python3-pip python3-venv \
    texlive-full imagemagick \
    git curl wget \
    && rm -rf /var/lib/apt/lists/*

# Configurar ImageMagick
RUN sed -i 's/rights="none" pattern="PDF"/rights="read|write" pattern="PDF"/' /etc/ImageMagick-6/policy.xml

# Crear usuario no-root
RUN useradd -m -s /bin/bash agente
USER agente
WORKDIR /home/agente

# Copiar código del agente
COPY --chown=agente:agente . /home/agente/agente-tikz/

# Instalar dependencias Python
RUN cd agente-tikz && \
    python3 -m venv venv && \
    . venv/bin/activate && \
    pip install --upgrade pip && \
    pip install -r requirements.txt

# Punto de entrada
WORKDIR /home/agente/agente-tikz
CMD ["bash"]
```

### Construir y Usar Docker

```bash
# Construir imagen
docker build -t agente-tikz .

# Ejecutar contenedor
docker run -it --rm -v $(pwd)/imagenes:/home/agente/imagenes agente-tikz

# Dentro del contenedor
source venv/bin/activate
python demo.py
```

## ✅ Verificación Final

Después de completar la instalación, ejecuta:

```bash
# Test completo
python demo.py

# Debe mostrar:
# ✅ Agente inicializado correctamente
# ✅ Código generado
# ✅ Validación funcional
# 🎉 Todos los demos completados exitosamente!
```

Si todos los tests pasan, ¡el agente está listo para usar! 🎉

## 📞 Soporte

Si encuentras problemas durante la instalación:

1. **Revisar logs:** Verificar archivos en `logs/`
2. **Verificar dependencias:** Ejecutar `python demo.py`
3. **Consultar documentación:** Revisar `tutorial_uso.md`
4. **Reportar issues:** Crear issue con detalles del error

---

**¡Bienvenido al Agente Graficador Experto TikZ!** 🎨✨
