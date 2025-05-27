#!/bin/bash

# Script para crear respaldo de configuración R + VSCode
# Ejecutar después de una instalación exitosa

echo "📦 Creando respaldo de configuración R + VSCode..."

# Crear directorio temporal
BACKUP_DIR="respaldo_r_vscode_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$BACKUP_DIR"

# Copiar archivos de configuración
echo "Copiando archivos de configuración..."

# Configuración VSCode
if [ -d ".vscode" ]; then
    cp -r .vscode "$BACKUP_DIR/"
    echo "✓ Configuración VSCode copiada"
fi

# Configuración R
if [ -f ".Rprofile" ]; then
    cp .Rprofile "$BACKUP_DIR/"
    echo "✓ .Rprofile copiado"
fi

if [ -f "$HOME/.Renviron" ]; then
    cp "$HOME/.Renviron" "$BACKUP_DIR/"
    echo "✓ .Renviron copiado"
fi

# Scripts de instalación y verificación
cp TUTORIAL_INSTALACION_R_VSCODE.md "$BACKUP_DIR/" 2>/dev/null || echo "⚠ Tutorial no encontrado"
cp instalar_r_vscode.sh "$BACKUP_DIR/" 2>/dev/null || echo "⚠ Script de instalación no encontrado"
cp README_INSTALACION_R.md "$BACKUP_DIR/" 2>/dev/null || echo "⚠ README no encontrado"
cp verificar_instalacion.R "$BACKUP_DIR/" 2>/dev/null || echo "⚠ Script de verificación no encontrado"

# Crear lista de paquetes R instalados
echo "Generando lista de paquetes R..."
R --slave -e "
.libPaths(c('~/R/library', .libPaths()))
installed_packages <- installed.packages()
write.csv(installed_packages, 'paquetes_r_instalados.csv', row.names=FALSE)
cat('Lista de paquetes R guardada\n')
" 2>/dev/null

if [ -f "paquetes_r_instalados.csv" ]; then
    mv paquetes_r_instalados.csv "$BACKUP_DIR/"
    echo "✓ Lista de paquetes R guardada"
fi

# Crear script de restauración
cat > "$BACKUP_DIR/restaurar_configuracion.sh" << 'EOF'
#!/bin/bash

echo "🔄 Restaurando configuración R + VSCode..."

# Restaurar configuración VSCode
if [ -d "vscode" ]; then
    mkdir -p .vscode
    cp -r vscode/* .vscode/
    echo "✓ Configuración VSCode restaurada"
fi

# Restaurar configuración R
if [ -f ".Rprofile" ]; then
    cp .Rprofile ./
    echo "✓ .Rprofile restaurado"
fi

if [ -f ".Renviron" ]; then
    cp .Renviron "$HOME/"
    echo "✓ .Renviron restaurado"
fi

# Crear biblioteca personal si no existe
mkdir -p ~/R/library

echo "✅ Configuración restaurada. Ejecuta instalar_r_vscode.sh para completar la instalación."
EOF

chmod +x "$BACKUP_DIR/restaurar_configuracion.sh"

# Crear archivo comprimido
tar -czf "${BACKUP_DIR}.tar.gz" "$BACKUP_DIR"
rm -rf "$BACKUP_DIR"

echo ""
echo "✅ Respaldo creado: ${BACKUP_DIR}.tar.gz"
echo ""
echo "📋 Para restaurar en otro sistema:"
echo "1. Copiar ${BACKUP_DIR}.tar.gz al nuevo sistema"
echo "2. Extraer: tar -xzf ${BACKUP_DIR}.tar.gz"
echo "3. Entrar al directorio: cd ${BACKUP_DIR}"
echo "4. Ejecutar: ./restaurar_configuracion.sh"
echo "5. Ejecutar: ./instalar_r_vscode.sh"
