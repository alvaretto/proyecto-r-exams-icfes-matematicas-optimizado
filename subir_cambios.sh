#!/bin/bash

# Script para subir cambios al repositorio RepositorioMatematicasICFES_R_Exams
# Configurado para usar token de GitHub sin solicitar credenciales

echo "=== SUBIR CAMBIOS AL REPOSITORIO ICFES R-EXAMS ==="
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
        echo "❌ Cancelando subida de cambios"
        exit 1
    fi
fi

# Verificar si hay cambios para subir
echo ""
echo "🔍 Verificando cambios locales..."
git status --porcelain > /tmp/git_status.txt

if [ ! -s /tmp/git_status.txt ]; then
    # Verificar si hay commits locales sin subir
    COMMITS_LOCALES=$(git rev-list origin/experimentos-seguros..HEAD --count)
    if [ "$COMMITS_LOCALES" -eq 0 ]; then
        echo "✅ No hay cambios para subir"
        echo "   Tu repositorio está sincronizado con el remoto"
        exit 0
    else
        echo "📤 Se encontraron $COMMITS_LOCALES commits locales para subir"
        git log --oneline origin/experimentos-seguros..HEAD
    fi
else
    echo "📝 Cambios detectados:"
    git status --short
    echo ""
    
    # Mostrar los archivos modificados
    echo "📋 Archivos modificados:"
    git diff --name-only
    
    echo ""
    echo "¿Deseas ver los cambios detallados antes de continuar? (s/n)"
    read -r ver_cambios
    if [ "$ver_cambios" = "s" ] || [ "$ver_cambios" = "S" ]; then
        git diff
        echo ""
        echo "Presiona Enter para continuar..."
        read -r
    fi
    
    # Agregar archivos al staging
    echo ""
    echo "📦 Agregando archivos al staging..."
    git add .
    
    # Solicitar mensaje de commit
    echo ""
    echo "✏️  Ingresa un mensaje para el commit:"
    read -r mensaje_commit
    
    if [ -z "$mensaje_commit" ]; then
        mensaje_commit="Actualización automática - $(date '+%Y-%m-%d %H:%M:%S')"
        echo "   Usando mensaje por defecto: $mensaje_commit"
    fi
    
    # Hacer commit
    echo ""
    echo "💾 Creando commit..."
    if git commit -m "$mensaje_commit"; then
        echo "✅ Commit creado exitosamente"
    else
        echo "❌ Error al crear el commit"
        exit 1
    fi
fi

# Actualizar desde remoto antes de hacer push
echo ""
echo "🔄 Actualizando desde remoto antes de subir..."
if git fetch origin; then
    echo "✅ Fetch completado"
else
    echo "❌ Error al hacer fetch"
    exit 1
fi

# Verificar si hay conflictos
CAMBIOS_REMOTOS=$(git rev-list HEAD..origin/experimentos-seguros --count)
if [ "$CAMBIOS_REMOTOS" -gt 0 ]; then
    echo "⚠️  Hay $CAMBIOS_REMOTOS nuevos commits en el remoto"
    echo "   Es necesario hacer merge antes de subir"
    echo "   ¿Deseas continuar con el merge? (s/n)"
    read -r hacer_merge
    if [ "$hacer_merge" = "s" ] || [ "$hacer_merge" = "S" ]; then
        if git merge origin/experimentos-seguros; then
            echo "✅ Merge completado exitosamente"
        else
            echo "❌ Error durante el merge - hay conflictos"
            echo "   Resuelve los conflictos manualmente y ejecuta el script nuevamente"
            exit 1
        fi
    else
        echo "❌ Subida cancelada"
        exit 1
    fi
fi

# Hacer push
echo ""
echo "📤 Subiendo cambios al repositorio remoto..."
if git push origin experimentos-seguros; then
    echo ""
    echo "🎉 ¡CAMBIOS SUBIDOS EXITOSAMENTE!"
    echo ""
    echo "✅ Tus cambios están ahora en el repositorio remoto"
    echo "   Rama: experimentos-seguros"
    echo "   Fecha: $(date)"
else
    echo ""
    echo "❌ Error al subir los cambios"
    echo "   Verifica tu conexión a internet y el token de GitHub"
    exit 1
fi

# Limpiar archivos temporales
rm -f /tmp/git_status.txt

echo ""
echo "=== FIN DE LA SUBIDA ==="
