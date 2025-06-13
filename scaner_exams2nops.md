# 🖥️ Configuración de Software para exams2nops en Manjaro XFCE

## 🐧 **MANJARO XFCE - CONFIGURACIÓN ESPECÍFICA**

### **✅ Ventajas de Manjaro para R/exams:**
- **Arch-based:** Paquetes siempre actualizados
- **AUR:** Acceso a software especializado
- **Rendimiento:** Excelente para procesamiento masivo de PDFs
- **Estabilidad:** XFCE es ligero y confiable

---

## 📦 **INSTALACIÓN DE SOFTWARE EN MANJARO**

### **1. ACTUALIZAR SISTEMA**
```bash
sudo pacman -Syu
```

### **2. INSTALAR R Y DEPENDENCIAS BASE**
```bash
# R base y herramientas de desarrollo
sudo pacman -S r gcc-fortran

# Dependencias para paquetes R
sudo pacman -S curl openssl libxml2 git
```

### **3. INSTALAR LaTeX COMPLETO**
```bash
# TeX Live completo (recomendado para R/exams)
sudo pacman -S texlive-most texlive-lang

# O instalación mínima si tienes espacio limitado
sudo pacman -S texlive-core texlive-bin texlive-latexextra
```

### **4. HERRAMIENTAS PDF ESENCIALES**
```bash
# PDFtk para manipulación de PDFs
sudo pacman -S pdftk

# ImageMagick para conversión de imágenes
sudo pacman -S imagemagick

# Herramientas adicionales útiles
sudo pacman -S poppler ghostscript
```

### **5. SOFTWARE ADICIONAL RECOMENDADO**
```bash
# RStudio desde AUR (opcional pero recomendado)
yay -S rstudio-desktop-bin

# O usar RStudio desde repositorios oficiales
sudo pacman -S rstudio-desktop

# Visor PDF avanzado
sudo pacman -S okular

# Editor de texto avanzado (alternativa a RStudio)
sudo pacman -S code
```

---

## ⚙️ **CONFIGURACIÓN ESPECÍFICA MANJARO**

### **1. CONFIGURAR ImageMagick PARA PDFs**
```bash
# Editar política de seguridad
sudo nano /etc/ImageMagick-7/policy.xml

# Buscar y modificar estas líneas:
# Cambiar:
#   <policy domain="coder" rights="none" pattern="PDF" />
# Por:
#   <policy domain="coder" rights="read|write" pattern="PDF" />

# También cambiar:
#   <policy domain="resource" name="memory" value="256MiB"/>
# Por:
#   <policy domain="resource" name="memory" value="2GiB"/>
```

### **2. CONFIGURAR R EN MANJARO**
```bash
# Iniciar R como usuario normal (no sudo)
R

# Dentro de R, instalar paquetes esenciales:
```

```r
# Configurar repositorio CRAN
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Instalar paquetes esenciales
install.packages(c(
  "exams",
  "tidyverse", 
  "knitr",
  "rmarkdown",
  "tinytex",
  "devtools"
))

# Verificar instalación de exams
library(exams)
```

### **3. CONFIGURAR TINYTEX (ALTERNATIVA LIGERA)**
```r
# Si prefieres TinyTeX en lugar de TeX Live completo
tinytex::install_tinytex()

# Instalar paquetes LaTeX adicionales necesarios
tinytex::tlmgr_install(c(
  "babel-spanish",
  "hyphen-spanish", 
  "collection-fontsrecommended",
  "fancyhdr",
  "geometry"
))
```

---

## 🔧 **OPTIMIZACIONES ESPECÍFICAS MANJARO**

### **1. CONFIGURAR VARIABLES DE ENTORNO**
```bash
# Editar .bashrc o .zshrc
nano ~/.bashrc

# Agregar al final:
export R_LIBS_USER="~/R/library"
export PATH="/usr/bin:$PATH"
export TEXMFHOME="~/.texmf"

# Recargar configuración
source ~/.bashrc
```

### **2. CONFIGURAR MEMORIA PARA PROCESAMIENTO MASIVO**
```bash
# Crear archivo de configuración para R
mkdir -p ~/.R
nano ~/.R/Makevars

# Agregar:
CXXFLAGS=-O3 -march=native
CFLAGS=-O3 -march=native
```

### **3. SCRIPT DE VERIFICACIÓN MANJARO**
```bash
#!/bin/bash
# Guardar como check_rexams_manjaro.sh

echo "=== Verificación R/exams en Manjaro ==="

echo "1. Verificando R..."
R --version | head -1

echo "2. Verificando LaTeX..."
pdflatex --version | head -1

echo "3. Verificando PDFtk..."
pdftk --version

echo "4. Verificando ImageMagick..."
convert --version | head -1

echo "5. Verificando paquete exams en R..."
R -e "library(exams); cat('exams version:', packageVersion('exams'), '\n')"

echo "6. Test básico de generación PDF..."
R -e "
library(exams)
setwd(tempdir())
exams2pdf('tstat.Rnw', n=1, name='test')
if(file.exists('test1.pdf')) cat('✓ PDF generado correctamente\n') else cat('✗ Error en generación PDF\n')
"

echo "=== Verificación completada ==="
```

```bash
# Hacer ejecutable y correr
chmod +x check_rexams_manjaro.sh
./check_rexams_manjaro.sh
```

---

## 📊 **CONFIGURACIÓN DE HARDWARE MANJARO**

### **OPTIMIZAR RENDIMIENTO:**
```bash
# Verificar RAM disponible
free -h

# Verificar espacio en disco
df -h

# Optimizar swappiness para procesamiento intensivo
echo 'vm.swappiness=10' | sudo tee -a /etc/sysctl.conf

# Aplicar cambios
sudo sysctl -p
```

### **CONFIGURAR SSD (si aplica):**
```bash
# Verificar si tienes SSD
lsblk -d -o name,rota

# Habilitar TRIM para SSD
sudo systemctl enable fstrim.timer
```

---

## 🖨️ **CONFIGURACIÓN DE ESCÁNER EN MANJARO**

### **INSTALAR SOPORTE PARA ESCÁNERES:**
```bash
# SANE para escáneres
sudo pacman -S sane sane-airscan

# Interfaz gráfica para escaneo
sudo pacman -S simple-scan

# Para escáneres HP
sudo pacman -S hplip

# Para escáneres Canon
yay -S scangearmp2
```

### **CONFIGURAR ESCÁNER:**
```bash
# Detectar escáneres
scanimage -L

# Test de escaneo
scanimage --format=png > test_scan.png
```

---

## 📄 **PROCESO MANUAL DE ESCANEO - PASO A PASO**

### **🔍 1. PREPARACIÓN DE LAS HOJAS FÍSICAS**

#### **Antes del escaneo:**
```
📋 CHECKLIST PRE-ESCANEO:
□ Separar solo las HOJAS DE RESPUESTAS (primera página)
□ Verificar que todas tengan marcas de lápiz #2 o bolígrafo negro
□ Quitar grapas, clips o elementos metálicos
□ Enderezar hojas arrugadas o dobladas
□ Ordenar por ID de examen (opcional, pero recomendado)
```

#### **Inspección visual obligatoria:**
- **Marcas claras:** Los círculos deben estar completamente rellenados
- **Sin marcas dobles:** Solo una opción marcada por pregunta
- **Códigos legibles:** El ID del estudiante y del examen deben ser visibles
- **Sin daños:** Hojas sin roturas en las áreas de código de barras

### **🖨️ 2. CONFIGURACIÓN DEL ESCÁNER**

#### **Configuración física del escáner:**
```bash
# En Manjaro, configurar escáner:
sudo scanimage -L  # Detectar escáneres disponibles

# Configuración típica para OMR:
Resolución: 300 DPI (mínimo) - 600 DPI (óptimo)
Modo: Escala de grises o Blanco/Negro
Formato: PNG o PDF
Orientación: Retrato
Tamaño: A4 o Letter (según el examen)
```

#### **Configuración en el escáner físico:**
1. **Alimentador automático:** Activar si está disponible
2. **Detección de doble alimentación:** Activar para evitar hojas pegadas
3. **Enderezamiento automático:** Activar si está disponible
4. **Compresión:** Mínima para mantener calidad

### **📑 3. PROCESO MANUAL DE ALIMENTACIÓN**

#### **Preparación del lote:**
```
TAMAÑO DE LOTES RECOMENDADOS:
• Escáner doméstico: 10-20 hojas por lote
• Escáner de oficina: 30-50 hojas por lote
• Escáner profesional: 50-100 hojas por lote
```

#### **Orientación correcta:**
```
ORIENTACIÓN DE LAS HOJAS:
✅ CORRECTO: Encabezado hacia arriba, código de barras visible
✅ ALTERNATIVO: Si hay problemas, rotar 180° (usar rotate=TRUE en R)
❌ INCORRECTO: Hojas al revés o de lado
```

### **🔄 4. PROCESO DE ESCANEO MANUAL**

#### **Paso a paso:**

**PASO 1: Cargar el primer lote**
```
1. Colocar 20-50 hojas en el alimentador
2. Ajustar las guías laterales (sin apretar mucho)
3. Verificar que la primera hoja esté bien alineada
4. Presionar "Scan" o "Escanear"
```

**PASO 2: Monitorear el proceso**
```
Durante el escaneo:
• Observar que las hojas pasen una por una
• Verificar que no se atasquen
• Escuchar ruidos anormales
• Verificar que el contador avance correctamente
```

**PASO 3: Verificación inmediata**
```
Después de cada lote:
• Contar hojas escaneadas vs hojas físicas
• Verificar que no haya hojas en blanco en el resultado
• Revisar las primeras y últimas imágenes del lote
• Guardar con nombre descriptivo: "lote_01_hojas_001-050.pdf"
```

### **📁 5. ORGANIZACIÓN DE ARCHIVOS**

#### **Estructura de carpetas recomendada:**
```
escaneos_examen_2025-01-15/
├── lote_01_hojas_001-050.pdf
├── lote_02_hojas_051-100.pdf
├── lote_03_hojas_101-150.pdf
├── problematicas/
│   ├── hoja_dañada_estudiante_123.pdf
│   └── marca_doble_estudiante_456.pdf
└── verificacion/
    ├── muestra_lote_01.png
    └── muestra_lote_02.png
```

### **🚨 6. MANEJO DE PROBLEMAS COMUNES**

#### **Hoja atascada en el escáner:**
```
PROCEDIMIENTO:
1. PARAR inmediatamente el escaneo
2. Apagar el escáner
3. Abrir las cubiertas de acceso
4. Retirar cuidadosamente la hoja atascada
5. Verificar que no queden pedazos
6. Reiniciar y continuar desde la hoja siguiente
```

#### **Hojas con marcas problemáticas:**
```
CASOS ESPECIALES:
• Marca doble: Escanear por separado, marcar para revisión manual
• Marca muy tenue: Aumentar contraste en el escáner
• Hoja arrugada: Planchar suavemente antes de escanear
• Código ilegible: Escanear por separado, anotar ID manualmente
```

#### **Problemas de calidad:**
```
SOLUCIONES INMEDIATAS:
• Imagen muy clara: Reducir brillo, aumentar contraste
• Imagen muy oscura: Aumentar brillo
• Líneas o rayas: Limpiar cristal del escáner
• Imágenes torcidas: Verificar guías del alimentador
```

### **🔍 7. VERIFICACIÓN POST-ESCANEO**

#### **Control de calidad manual:**
```bash
# Verificar archivos generados
ls -la escaneos_examen_2025-01-15/
du -h escaneos_examen_2025-01-15/  # Verificar tamaños

# Abrir muestras aleatorias
okular lote_01_hojas_001-050.pdf &
okular lote_02_hojas_051-100.pdf &
```

#### **Checklist de verificación:**
```
□ Todas las hojas están escaneadas (contar total)
□ No hay páginas en blanco
□ Los códigos de barras son legibles
□ Las marcas están claramente visibles
□ No hay hojas duplicadas
□ Los archivos no están corruptos
□ Tamaño de archivo razonable (no demasiado grande/pequeño)
```

### **⚙️ 8. PROCESAMIENTO EN R**

#### **Una vez verificado el escaneo:**
```r
# Procesar todos los lotes escaneados
library(exams)

# Configurar directorio
setwd("~/escaneos_examen_2025-01-15/")

# Escanear todos los PDFs
nops_scan(dir = ".",
          rotate = FALSE,  # TRUE si rotaste las hojas 180°
          cores = 2)       # Usar múltiples cores si tienes muchas hojas

# Verificar resultado
list.files(pattern = "nops_scan_.*\\.zip")
```

### **📊 9. FLUJO COMPLETO VISUAL**

```
HOJAS FÍSICAS → PREPARACIÓN → CONFIGURACIÓN ESCÁNER
      ↓              ↓                ↓
   Separar        Quitar           300+ DPI
   Ordenar        grapas           Escala grises
   Inspeccionar   Enderezar        Formato PDF
      ↓              ↓                ↓
ESCANEO POR LOTES → VERIFICACIÓN → PROCESAMIENTO R
      ↓              ↓                ↓
   20-50 hojas    Contar hojas    nops_scan()
   Monitorear     Verificar       nops_eval()
   Guardar        calidad         Resultados
```

### **⏱️ 10. TIEMPOS ESTIMADOS**

```
TIEMPOS APROXIMADOS:
• Preparación: 2-3 minutos por cada 50 hojas
• Escaneo: 1-2 minutos por cada 50 hojas (escáner rápido)
• Verificación: 1 minuto por cada 50 hojas
• Procesamiento R: 30 segundos por cada 100 hojas

TOTAL: ~5 minutos por cada 50 hojas de examen
```

---

## 🚀 **FLUJO DE TRABAJO OPTIMIZADO MANJARO**

### **1. SCRIPT DE CONFIGURACIÓN INICIAL:**
```bash
#!/bin/bash
# setup_rexams_manjaro.sh

echo "Configurando R/exams en Manjaro..."

# Crear directorios de trabajo
mkdir -p ~/R-exams/{examenes,escaneos,resultados}

# Configurar permisos
chmod 755 ~/R-exams/*

# Crear script R de ejemplo
cat > ~/R-exams/ejemplo_basico.R << 'EOF'
library(exams)

# Configurar directorio de trabajo
setwd("~/R-exams/examenes")

# Ejemplo básico
myexam <- list(
  "tstat.Rnw",
  "ttest.Rnw", 
  "relfreq.Rnw"
)

# Generar examen
set.seed(123)
exams2nops(myexam, n = 5, 
           dir = "output",
           name = "examen_demo",
           date = Sys.Date())

cat("Examen generado en:", getwd(), "/output\n")
EOF

echo "Configuración completada. Ejecutar: Rscript ~/R-exams/ejemplo_basico.R"
```

### **2. AUTOMATIZACIÓN CON SYSTEMD (OPCIONAL):**
```bash
# Crear servicio para procesamiento automático
sudo nano /etc/systemd/user/rexams-processor.service

# Contenido:
[Unit]
Description=R/exams Processor
After=graphical-session.target

[Service]
Type=oneshot
ExecStart=/usr/bin/Rscript /home/tu_usuario/R-exams/process_scans.R
WorkingDirectory=/home/tu_usuario/R-exams

[Install]
WantedBy=default.target
```

---

## 🔍 **SOLUCIÓN DE PROBLEMAS MANJARO**

### **Error: Paquete R no se instala**
```bash
# Instalar herramientas de compilación
sudo pacman -S base-devel

# Limpiar cache de paquetes R
R -e "remove.packages(installed.packages()[,1])"
```

### **Error: LaTeX no encuentra paquetes**
```bash
# Actualizar base de datos TeX
sudo texhash

# Instalar paquetes faltantes
sudo tlmgr install babel-spanish
```

### **Error: ImageMagick permisos**
```bash
# Verificar configuración
identify -list policy

# Reiniciar después de cambios
sudo systemctl restart imagemagick
```

---

## 📋 **CHECKLIST MANJARO ESPECÍFICO**

- [ ] Manjaro actualizado (`sudo pacman -Syu`)
- [ ] R instalado (`sudo pacman -S r`)
- [ ] TeX Live instalado (`sudo pacman -S texlive-most`)
- [ ] PDFtk instalado (`sudo pacman -S pdftk`)
- [ ] ImageMagick configurado para PDFs
- [ ] Paquete `exams` instalado en R
- [ ] Variables de entorno configuradas
- [ ] Permisos de directorio verificados
- [ ] Script de verificación ejecutado exitosamente

---

## 🎯 **COMANDO RÁPIDO DE INSTALACIÓN MANJARO**

```bash
# Instalación completa en una línea
sudo pacman -Syu && sudo pacman -S r texlive-most pdftk imagemagick gcc-fortran curl openssl libxml2 && R -e "install.packages(c('exams','tidyverse','knitr'), repos='https://cran.rstudio.com/')"
```

**¡Tu sistema Manjaro XFCE está perfectamente preparado para R/exams!** La combinación de Arch Linux + XFCE te dará un rendimiento excelente para el procesamiento masivo de exámenes.

---

## 🔄 **FLUJO DE TRABAJO COMPLETO exams2nops**

### **1. GENERACIÓN DE EXÁMENES**
```r
library(exams)

# Definir ejercicios
myexam <- list(
  "ejercicio1.Rmd",
  "ejercicio2.Rmd",
  "ejercicio3.Rmd"
)

# Generar PDFs para imprimir
set.seed(123)
exams2nops(myexam, n = 100,
           dir = "nops_pdf",
           name = "examen_final",
           date = "2025-01-15",
           points = c(2, 3, 5),
           showpoints = TRUE)
```

### **2. IMPRESIÓN Y DISTRIBUCIÓN**
- Imprimir sin escalar ("Fit to printable area" = NO)
- Engrapar en esquina superior izquierda
- Distribuir a estudiantes

### **3. ESCANEO MANUAL DE RESPUESTAS (PROCESO FÍSICO)**
```bash
# Preparar directorio de escaneo
mkdir -p ~/escaneos_examen_2025-01-15/{lotes,problematicas,verificacion}

# Proceso manual:
# 1. Separar hojas de respuestas (solo primera página)
# 2. Quitar grapas y enderezar hojas
# 3. Configurar escáner: 300+ DPI, escala grises, PDF
# 4. Escanear por lotes de 20-50 hojas
# 5. Verificar calidad de cada lote
# 6. Organizar archivos: lote_01_hojas_001-050.pdf
```

### **4. PROCESAMIENTO AUTOMÁTICO**
```r
# Procesar escaneos con nops_scan()
library(exams)
setwd("~/escaneos_examen_2025-01-15/")

# Escanear todos los PDFs del directorio
nops_scan(dir = ".",
          rotate = FALSE,  # TRUE si rotaste hojas 180°
          cores = 2)

# Verificar archivo ZIP generado
list.files(pattern = "nops_scan_.*\\.zip")
```

### **5. EVALUACIÓN AUTOMÁTICA**
```r
# Crear archivo CSV con datos de estudiantes
estudiantes <- data.frame(
  registration = c("2021001", "2021002"),
  name = c("Juan Pérez", "María García"),
  id = c("juan.perez", "maria.garcia")
)
write.csv2(estudiantes, "estudiantes.csv", row.names = FALSE)

# Evaluar exámenes
resultados <- nops_eval(
  register = "estudiantes.csv",
  solutions = "nops_pdf/examen_final.rds",
  scans = Sys.glob("nops_scan_*.zip"),  # Archivos ZIP del escaneo
  eval = exams_eval(partial = FALSE, negative = FALSE),
  interactive = FALSE
)

# Ver resultados
print(resultados)
```

---

## 📋 **GUÍA RÁPIDA DE ESCANEO MANUAL**

### **🔧 PREPARACIÓN PREVIA (5 minutos)**
```
1. ✅ Separar SOLO hojas de respuestas (primera página)
2. ✅ Quitar grapas, clips, elementos metálicos
3. ✅ Enderezar hojas arrugadas
4. ✅ Verificar marcas claras y legibles
5. ✅ Ordenar por ID (opcional)
```

### **⚙️ CONFIGURACIÓN ESCÁNER**
```
Resolución: 300-600 DPI
Modo: Escala de grises
Formato: PDF
Orientación: Retrato
Alimentador: Automático (si disponible)
```

### **📑 PROCESO DE ESCANEO**
```
LOTE 1: Hojas 001-050 → lote_01_hojas_001-050.pdf
LOTE 2: Hojas 051-100 → lote_02_hojas_051-100.pdf
LOTE 3: Hojas 101-150 → lote_03_hojas_101-150.pdf
...

⏱️ Tiempo estimado: ~5 minutos por cada 50 hojas
```

### **🔍 VERIFICACIÓN INMEDIATA**
```
Después de cada lote:
□ Contar hojas físicas vs escaneadas
□ Verificar primera y última imagen
□ Comprobar códigos de barras legibles
□ Revisar marcas visibles
□ Guardar con nombre descriptivo
```

---

## 📱 **HERRAMIENTAS ADICIONALES MANJARO**

### **APLICACIONES GRÁFICAS ÚTILES:**
```bash
# Visor de PDFs avanzado
sudo pacman -S okular

# Editor de imágenes para ajustar escaneos
sudo pacman -S gimp

# Herramienta de captura de pantalla
sudo pacman -S flameshot

# Administrador de archivos con vista previa
sudo pacman -S thunar thunar-archive-plugin
```

### **EXTENSIONES XFCE RECOMENDADAS:**
```bash
# Panel de tareas mejorado
sudo pacman -S xfce4-taskmanager

# Monitor del sistema
sudo pacman -S xfce4-systemload-plugin

# Notas rápidas
sudo pacman -S xfce4-notes-plugin
```

---

## 🚨 **TROUBLESHOOTING ESPECÍFICO**

### **Problema: R no encuentra LaTeX**
```r
# Verificar instalación LaTeX
Sys.which("pdflatex")

# Si está vacío, agregar al PATH
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "/usr/bin", sep = ":"))
```

### **Problema: Permisos de escritura**
```bash
# Verificar permisos del directorio de trabajo
ls -la ~/R-exams/

# Corregir permisos si es necesario
chmod -R 755 ~/R-exams/
chown -R $USER:$USER ~/R-exams/
```

### **Problema: Memoria insuficiente para exámenes grandes**
```r
# Aumentar límite de memoria en R
options(java.parameters = "-Xmx4g")

# Procesar en lotes más pequeños
for(i in seq(1, 1000, 50)) {
  batch_end <- min(i + 49, 1000)
  exams2nops(myexam, n = batch_end - i + 1,
             dir = paste0("batch_", i))
}
```

---

## 📈 **OPTIMIZACIÓN DE RENDIMIENTO**

### **CONFIGURACIÓN AVANZADA R:**
```r
# Configurar múltiples cores
library(parallel)
options(mc.cores = detectCores() - 1)

# Optimizar memoria
options(expressions = 500000)
```

### **MONITOREO DEL SISTEMA:**
```bash
# Instalar herramientas de monitoreo
sudo pacman -S htop iotop

# Monitorear durante procesamiento
htop
```

---

## 🎓 **CASOS DE USO ESPECÍFICOS**

### **EXÁMENES PEQUEÑOS (< 50 estudiantes):**
- Usar escáner doméstico
- Procesamiento en tiempo real
- Resultados inmediatos

### **EXÁMENES MEDIANOS (50-200 estudiantes):**
- Escáner con alimentador automático
- Procesamiento por lotes
- Verificación manual de casos dudosos

### **EXÁMENES MASIVOS (> 200 estudiantes):**
- Escáner profesional
- Servidor dedicado
- Procesamiento distribuido
- Backup automático

---

## 📞 **SOPORTE Y RECURSOS**

### **DOCUMENTACIÓN OFICIAL:**
- [R/exams Website](https://www.r-exams.org/)
- [CRAN Package](https://cran.r-project.org/package=exams)
- [GitHub Repository](https://github.com/r-forge/exams)

### **COMUNIDAD MANJARO:**
- [Manjaro Forum](https://forum.manjaro.org/)
- [Arch Wiki](https://wiki.archlinux.org/)
- [AUR Packages](https://aur.archlinux.org/)

---

**Fecha de creación:** $(date)
**Sistema:** Manjaro XFCE
**Versión:** R/exams 2.4+
