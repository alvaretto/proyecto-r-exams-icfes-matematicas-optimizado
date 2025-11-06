#!/bin/bash

# Script para actualizar el repositorio RepositorioMatematicasICFES_R_Exams
# Configurado para usar token de GitHub sin solicitar credenciales

echo "=== ACTUALIZADOR DE REPOSITORIO ICFES R-EXAMS ==="
echo "Fecha: $(date)"
echo "Usuario: alvaretto"
echo "Repositorio: proyecto-r-exams-icfes-matematicas-optimizado"
echo "Rama: experimentos-seguros"
echo ""

# Verificar que estamos en el directorio correcto
if [ ! -d ".git" ]; then
    echo "❌ Error: No se encontró el directorio .git"
    echo "   Asegúrate de ejecutar este script desde el directorio raíz del repositorio"
    exit 1
fi

# Verificar la rama actual
RAMA_ACTUAL=$(git branch --show-current)
echo "📍 Rama actual: $RAMA_ACTUAL"

if [ "$RAMA_ACTUAL" != "experimentos-seguros" ]; then
    echo "⚠️  Advertencia: No estás en la rama 'experimentos-seguros'"
    echo "   ¿Deseas cambiar a la rama experimentos-seguros? (s/n)"
    read -r respuesta
    if [ "$respuesta" = "s" ] || [ "$respuesta" = "S" ]; then
        git checkout experimentos-seguros
        echo "✅ Cambiado a rama experimentos-seguros"
    else
        echo "❌ Cancelando actualización"
        exit 1
    fi
fi

# Verificar el estado del repositorio
echo ""
echo "🔍 Verificando estado del repositorio..."
git status --porcelain > /tmp/git_status.txt

if [ -s /tmp/git_status.txt ]; then
    echo "⚠️  Hay cambios sin confirmar en el repositorio:"
    git status --short
    echo ""
    echo "   ¿Deseas continuar? Los cambios locales podrían perderse (s/n)"
    read -r respuesta
    if [ "$respuesta" != "s" ] && [ "$respuesta" != "S" ]; then
        echo "❌ Actualización cancelada"
        exit 1
    fi
fi

# Realizar fetch
echo ""
echo "📥 Descargando cambios del repositorio remoto..."
if git fetch origin; then
    echo "✅ Fetch completado exitosamente"
else
    echo "❌ Error al hacer fetch del repositorio remoto"
    echo "   Verifica tu conexión a internet y el token de GitHub"
    exit 1
fi

# Verificar si hay cambios para actualizar
CAMBIOS_REMOTOS=$(git rev-list HEAD..origin/experimentos-seguros --count)
if [ "$CAMBIOS_REMOTOS" -eq 0 ]; then
    echo ""
    echo "✅ Tu repositorio ya está actualizado"
    echo "   No hay nuevos cambios en el repositorio remoto"
    exit 0
fi

echo ""
echo "📊 Se encontraron $CAMBIOS_REMOTOS nuevos commits en el repositorio remoto"

# Mostrar los commits que se van a descargar
echo ""
echo "📋 Nuevos commits a descargar:"
git log --oneline HEAD..origin/experimentos-seguros | head -10

if [ "$CAMBIOS_REMOTOS" -gt 10 ]; then
    echo "   ... y $(($CAMBIOS_REMOTOS - 10)) commits más"
fi

# Realizar merge
echo ""
echo "🔄 Actualizando repositorio local..."
if git merge origin/experimentos-seguros; then
    echo ""
    echo "🎉 ¡ACTUALIZACIÓN COMPLETADA EXITOSAMENTE!"
    echo ""
    echo "📈 Resumen de la actualización:"
    git diff --stat HEAD~$CAMBIOS_REMOTOS HEAD
else
    echo ""
    echo "❌ Error durante la actualización"
    echo "   Puede haber conflictos que requieren resolución manual"
    echo "   Ejecuta 'git status' para ver los detalles"
    exit 1
fi

echo ""
echo "✅ Tu repositorio está ahora sincronizado con la rama experimentos-seguros"
echo "   Última actualización: $(date)"
echo ""
echo "💡 Para futuras actualizaciones, simplemente ejecuta: ./actualizar_repositorio.sh"

# Limpiar archivos temporales
rm -f /tmp/git_status.txt

echo ""
echo "=== FIN DE LA ACTUALIZACIÓN ==="
