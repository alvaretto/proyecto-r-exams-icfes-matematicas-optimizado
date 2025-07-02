# 🚀 Tutorial: Automatización de Estructura del Repositorio

## 📋 Descripción General

Este tutorial explica el uso completo del script `estructura-actual-repositorio.sh`, una herramienta automatizada para generar y mantener actualizada la documentación de estructura del repositorio ICFES R-Exams.

**Archivo del script**: `estructura-actual-repositorio.sh`  
**Archivo generado**: `Estructura_Repositorio.md`  
**Versión**: 1.0  
**Compatibilidad**: Linux/Unix, macOS, WSL

---

## 🎯 Objetivos del Script

### ✅ **Automatización Completa**
- Generar documentación de estructura sin intervención manual
- Mantener actualizada la información del repositorio
- Calcular estadísticas precisas en tiempo real
- Detectar automáticamente competencias implementadas

### 📊 **Información Generada**
- Estructura completa de directorios
- Estadísticas del repositorio (archivos, directorios, ejercicios)
- Competencias ICFES detectadas
- Convenciones de estructura
- Metadatos de generación

---

## 🔧 Instalación y Configuración

### **Paso 1: Verificar Dependencias**

El script requiere las siguientes herramientas:

```bash
# Verificar dependencias
which tree find wc

# Instalar tree si no está disponible (Ubuntu/Debian)
sudo apt-get update
sudo apt-get install tree

# Instalar tree (macOS con Homebrew)
brew install tree

# Instalar tree (CentOS/RHEL)
sudo yum install tree
```

### **Paso 2: Descargar y Configurar el Script**

```bash
# Hacer el script ejecutable
chmod +x estructura-actual-repositorio.sh

# Verificar permisos
ls -la estructura-actual-repositorio.sh
```

### **Paso 3: Ubicación Correcta**

⚠️ **IMPORTANTE**: Ejecutar el script desde el **directorio raíz** del repositorio ICFES R-Exams.

```bash
# Verificar ubicación correcta
pwd
ls -la | grep -E "(01-Numeros-Reales|02-Funciones|06-Estadística)"
```

---

## 🚀 Uso del Script

### **Ejecución Básica**

```bash
# Ejecución estándar
./estructura-actual-repositorio.sh
```

**Salida esperada**:
```
=============================================================================
  GENERADOR AUTOMÁTICO DE ESTRUCTURA DEL REPOSITORIO
  Proyecto: ICFES R-Exams Matemáticas
  Fecha: 2024-01-15 14:30:25
=============================================================================
[PASO] Verificando dependencias...
[ÉXITO] Todas las dependencias están disponibles
[PASO] Creando backup del archivo existente...
[ÉXITO] Backup creado: backups/Estructura_Repositorio_20240115_143025.md
[PASO] Generando estructura de directorios...
[ÉXITO] Estructura de directorios generada
[PASO] Calculando estadísticas del repositorio...
[ÉXITO] Estadísticas calculadas: 173 directorios, 15 archivos .Rmd
[PASO] Detectando competencias implementadas...
[ÉXITO] Competencias detectadas: 4
[PASO] Generando archivo Markdown...
[ÉXITO] Archivo Markdown generado: Estructura_Repositorio.md
[PASO] Limpiando archivos temporales...
[ÉXITO] Limpieza completada
```

### **Opciones Disponibles**

#### **Ver Ayuda**
```bash
./estructura-actual-repositorio.sh --help
# o
./estructura-actual-repositorio.sh -h
```

#### **Ejecutar sin Backup**
```bash
./estructura-actual-repositorio.sh --no-backup
```

---

## 📊 Análisis de Funcionalidades

### **🔍 Verificación de Dependencias**
```bash
check_dependencies() {
    # Verifica: tree, find, wc
    # Informa dependencias faltantes
    # Proporciona comandos de instalación
}
```

### **💾 Sistema de Backup Automático**
```bash
create_backup() {
    # Crea directorio backups/ si no existe
    # Genera backup con timestamp
    # Formato: Estructura_Repositorio_YYYYMMDD_HHMMSS.md
}
```

### **🌳 Generación de Estructura**
```bash
generate_tree_structure() {
    # Usa: tree -d -I 'exclusiones'
    # Excluye: __pycache__, *.pyc, .git, .DS_Store
    # Genera solo directorios (-d)
}
```

### **📈 Cálculo de Estadísticas**
```bash
count_statistics() {
    # Cuenta directorios totales
    # Cuenta archivos .Rmd y .R
    # Identifica directorios especiales (salida, ejercicios)
    # Exporta variables para plantilla
}
```

### **🧠 Detección de Competencias**
```bash
detect_competencias() {
    # Busca directorios principales:
    # - 01-Numeros-Reales → Pensamiento Numérico
    # - 02-Funciones → Pensamiento Variacional
    # - 03-Razones-Trigonometricas → Espacial y Métrico
    # - 05-Geometria-Analitica → Pensamiento Espacial
    # - 06-Estadística-Y-Probabilidad → Pensamiento Aleatorio
}
```

---

## 📄 Estructura del Archivo Generado

### **Secciones Principales**

1. **📁 Descripción General**
   - Información del repositorio
   - Fecha y hora de generación
   - Total de directorios

2. **📊 Estadísticas del Repositorio**
   - Tabla con métricas clave
   - Contadores automáticos
   - Competencias detectadas

3. **🌳 Estructura Completa**
   - Output completo de `tree -d`
   - Formato código con sintaxis

4. **🎯 Competencias ICFES Detectadas**
   - Lista de competencias encontradas
   - Estado de implementación

5. **📁 Convenciones de Estructura**
   - Directorios estándar
   - Tipos de ejercicios R-exams

6. **🔧 Información de Generación**
   - Metadatos del proceso
   - Trazabilidad completa

---

## 🔧 Casos de Uso Avanzados

### **Integración con Git Hooks**

```bash
# .git/hooks/post-commit
#!/bin/bash
cd "$(git rev-parse --show-toplevel)"
if [ -f "estructura-actual-repositorio.sh" ]; then
    ./estructura-actual-repositorio.sh --no-backup
    git add Estructura_Repositorio.md
    git commit -m "docs: actualizar estructura del repositorio [auto]"
fi
```

### **Automatización con Cron**

```bash
# Editar crontab
crontab -e

# Ejecutar diariamente a las 6:00 AM
0 6 * * * cd /ruta/al/repositorio && ./estructura-actual-repositorio.sh --no-backup
```

### **Integración en CI/CD**

```yaml
# .github/workflows/update-structure.yml
name: Update Repository Structure
on:
  push:
    branches: [ main ]
jobs:
  update-structure:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v2
    - name: Install tree
      run: sudo apt-get install tree
    - name: Update structure
      run: ./estructura-actual-repositorio.sh --no-backup
    - name: Commit changes
      run: |
        git config --local user.email "action@github.com"
        git config --local user.name "GitHub Action"
        git add Estructura_Repositorio.md
        git commit -m "docs: update repository structure" || exit 0
        git push
```

---

## 🚨 Solución de Problemas

### **Error: "tree: command not found"**
```bash
# Solución Ubuntu/Debian
sudo apt-get install tree

# Solución macOS
brew install tree

# Solución CentOS/RHEL
sudo yum install tree
```

### **Error: "Permission denied"**
```bash
# Dar permisos de ejecución
chmod +x estructura-actual-repositorio.sh
```

### **Advertencia: "No se detectaron directorios principales"**
```bash
# Verificar ubicación
pwd
ls -la | grep "01-Numeros-Reales"

# Navegar al directorio correcto
cd /ruta/al/repositorio/icfes-r-exams
```

### **Error: "No space left on device"**
```bash
# Limpiar backups antiguos
rm -rf backups/Estructura_Repositorio_202*

# Verificar espacio
df -h
```

---

## 📈 Métricas y Monitoreo

### **Estadísticas Típicas Esperadas**
- **Directorios**: 150-200
- **Archivos .Rmd**: 10-50
- **Competencias**: 4-5
- **Directorios de ejercicios**: 5-20

### **Indicadores de Calidad**
- ✅ **Bueno**: >10 archivos .Rmd, >3 competencias
- ⚠️ **Regular**: 5-10 archivos .Rmd, 2-3 competencias  
- ❌ **Mejorar**: <5 archivos .Rmd, <2 competencias

---

## 🔄 Mantenimiento y Actualizaciones

### **Frecuencia Recomendada**
- **Desarrollo activo**: Diario
- **Mantenimiento**: Semanal
- **Releases**: Antes de cada versión

### **Backup Management**
```bash
# Limpiar backups antiguos (>30 días)
find backups/ -name "*.md" -mtime +30 -delete

# Mantener solo últimos 10 backups
ls -t backups/ | tail -n +11 | xargs -I {} rm backups/{}
```

---

## 🎯 Mejores Prácticas

1. **📍 Ubicación**: Siempre ejecutar desde directorio raíz
2. **⏰ Frecuencia**: Ejecutar después de cambios significativos
3. **💾 Backups**: Mantener backups para recuperación
4. **🔍 Verificación**: Revisar output para detectar anomalías
5. **📝 Documentación**: Mantener este tutorial actualizado

---

*Tutorial generado para el proyecto ICFES R-Exams - Versión 1.0*
