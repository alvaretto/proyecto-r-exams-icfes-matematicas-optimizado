#!/bin/bash

# 💾 Script de backup N8N para ICFES Matemáticas
# Crea respaldos completos de configuración y workflows

echo "💾 Iniciando backup de N8N ICFES Matemáticas..."
echo "📅 Fecha: $(date)"

# Configurar directorios
BACKUP_DIR=~/n8n-workflows/RepositorioMatematicasICFES_R_Exams/backups
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_NAME="n8n_backup_${TIMESTAMP}"
BACKUP_PATH="${BACKUP_DIR}/${BACKUP_NAME}"

# Crear directorio de backup
mkdir -p "$BACKUP_PATH"

echo "📂 Directorio de backup: $BACKUP_PATH"

# 1. Backup de base de datos N8N
if [ -f ~/.n8n/database.sqlite ]; then
    echo "💾 Respaldando base de datos N8N..."
    cp ~/.n8n/database.sqlite "$BACKUP_PATH/database.sqlite"
    echo "✅ Base de datos respaldada"
else
    echo "⚠️  Base de datos N8N no encontrada"
fi

# 2. Backup de configuración
if [ -f ~/.n8n/config ]; then
    echo "⚙️  Respaldando configuración..."
    cp -r ~/.n8n/config "$BACKUP_PATH/"
    echo "✅ Configuración respaldada"
fi

# 3. Backup de workflows exportados
WORKFLOWS_SOURCE="/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/Auxiliares/Instalaciones/N8N/workflows"
if [ -d "$WORKFLOWS_SOURCE" ]; then
    echo "🔄 Respaldando workflows..."
    cp -r "$WORKFLOWS_SOURCE" "$BACKUP_PATH/workflows_export"
    echo "✅ Workflows respaldados"
fi

# 4. Backup del auto-corrector
CORRECTOR_SOURCE="/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/core/auto_corrector_icfes.py"
if [ -f "$CORRECTOR_SOURCE" ]; then
    echo "🔧 Respaldando auto-corrector..."
    cp "$CORRECTOR_SOURCE" "$BACKUP_PATH/auto_corrector_icfes.py"
    echo "✅ Auto-corrector respaldado"
fi

# 5. Crear archivo de información del backup
cat > "$BACKUP_PATH/backup_info.txt" << EOF
N8N ICFES Matemáticas - Información del Backup
==============================================

Fecha de creación: $(date)
Timestamp: $TIMESTAMP
Versión N8N: $(n8n --version 2>/dev/null || echo "No disponible")
Sistema: $(uname -a)

Contenido del backup:
- database.sqlite: Base de datos N8N completa
- workflows_export/: Workflows exportados en JSON
- auto_corrector_icfes.py: Auto-corrector v2.0
- config/: Configuración N8N (si existe)

Workflows incluidos:
- 🔧 Validador Automático ICFES
- 📊 Compilador Masivo Multi-formato  
- 🎨 Replicador TikZ PNG→TikZ
- 🚨 Monitor de Errores de Compilación

Para restaurar:
1. Detener N8N
2. Copiar database.sqlite a ~/.n8n/
3. Importar workflows desde workflows_export/
4. Copiar auto_corrector_icfes.py a core/
5. Reiniciar N8N

Ubicación original:
$BACKUP_PATH
EOF

# 6. Comprimir backup (opcional)
echo "🗜️  Comprimiendo backup..."
cd "$BACKUP_DIR"
tar -czf "${BACKUP_NAME}.tar.gz" "$BACKUP_NAME"

if [ $? -eq 0 ]; then
    echo "✅ Backup comprimido: ${BACKUP_NAME}.tar.gz"
    # Eliminar directorio sin comprimir para ahorrar espacio
    rm -rf "$BACKUP_NAME"
else
    echo "⚠️  Error al comprimir, manteniendo directorio sin comprimir"
fi

# 7. Limpiar backups antiguos (mantener últimos 10)
echo "🧹 Limpiando backups antiguos..."
cd "$BACKUP_DIR"
ls -t n8n_backup_*.tar.gz 2>/dev/null | tail -n +11 | xargs -r rm
echo "✅ Limpieza completada"

# 8. Mostrar resumen
echo ""
echo "📊 RESUMEN DEL BACKUP:"
echo "   Nombre: $BACKUP_NAME"
echo "   Ubicación: $BACKUP_DIR"
echo "   Tamaño: $(du -h "${BACKUP_DIR}/${BACKUP_NAME}.tar.gz" 2>/dev/null | cut -f1 || echo "N/A")"
echo "   Estado: ✅ Completado exitosamente"
echo ""
echo "📋 Para restaurar este backup:"
echo "   1. Ejecutar: tar -xzf ${BACKUP_NAME}.tar.gz"
echo "   2. Seguir instrucciones en backup_info.txt"
echo ""

echo "💾 Backup completado exitosamente!"
