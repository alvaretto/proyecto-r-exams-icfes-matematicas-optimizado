#!/bin/bash

# Script de sincronización completa para proyecto R-exams ICFES
# Maneja fetch, merge, commit y push de forma inteligente
# Autor: Optimización de flujo de trabajo Git
# Fecha: Enero 2025

echo "🔄 SINCRONIZACIÓN COMPLETA - Proyecto R-exams ICFES"
echo "==================================================="

# Verificar que estamos en un repositorio Git
if ! git rev-parse --git-dir > /dev/null 2>&1; then
    echo "❌ Error: No estás en un repositorio Git"
    exit 1
fi

# Mostrar rama actual
rama_actual=$(git branch --show-current)
echo "📍 Rama actual: $rama_actual"

# Verificar estado inicial
echo ""
echo "📊 Estado inicial del repositorio:"
git status --short

# Verificar conexión remota
echo ""
echo "🌐 Verificando conexión con el repositorio remoto..."
if git ls-remote origin > /dev/null 2>&1; then
    echo "✅ Conexión con origin establecida"
else
    echo "❌ Error: No se puede conectar con el repositorio remoto"
    echo "💡 Verifica tu conexión a internet y configuración de Git"
    exit 1
fi

# Fetch cambios remotos
echo ""
echo "📥 Obteniendo cambios del repositorio remoto..."
git fetch origin

if [ $? -eq 0 ]; then
    echo "✅ Fetch completado"
else
    echo "❌ Error en fetch"
    exit 1
fi

# Verificar si hay cambios remotos
echo ""
echo "🔍 Verificando cambios remotos..."
if git merge-base --is-ancestor HEAD origin/experimentos-seguros; then
    echo "✅ Repositorio local actualizado con respecto al remoto"
    hay_cambios_remotos=false
else
    echo "⚠️  Hay cambios remotos que necesitan sincronizarse"
    hay_cambios_remotos=true
    
    # Mostrar diferencias
    echo ""
    echo "📋 Commits remotos nuevos:"
    git log --oneline HEAD..origin/experimentos-seguros | head -5
fi

# Verificar cambios locales
echo ""
echo "🔍 Verificando cambios locales..."
if git diff --quiet && git diff --cached --quiet; then
    echo "✅ No hay cambios locales pendientes"
    hay_cambios_locales=false
else
    echo "📝 Hay cambios locales pendientes"
    hay_cambios_locales=true
    
    # Mostrar archivos modificados
    echo ""
    echo "📁 Archivos modificados:"
    git status --short | head -10
fi

# Sincronizar cambios remotos si los hay
if [ "$hay_cambios_remotos" = true ]; then
    echo ""
    read -p "🔄 ¿Sincronizar cambios remotos? (y/N): " sync_remote
    
    if [[ $sync_remote =~ ^[Yy]$ ]]; then
        echo "🔄 Sincronizando cambios remotos..."
        
        if [ "$hay_cambios_locales" = true ]; then
            echo "💾 Guardando cambios locales temporalmente..."
            git stash push -m "Sync temporal - $(date)"
            cambios_stashed=true
        else
            cambios_stashed=false
        fi
        
        # Hacer merge o rebase
        echo "🔀 Integrando cambios remotos..."
        if git merge origin/experimentos-seguros; then
            echo "✅ Cambios remotos integrados exitosamente"
            
            # Restaurar cambios locales si los había
            if [ "$cambios_stashed" = true ]; then
                echo "📤 Restaurando cambios locales..."
                if git stash pop; then
                    echo "✅ Cambios locales restaurados"
                else
                    echo "⚠️  Conflictos al restaurar cambios locales"
                    echo "💡 Resuelve los conflictos manualmente"
                fi
            fi
        else
            echo "❌ Error al integrar cambios remotos"
            echo "💡 Resuelve los conflictos manualmente"
            exit 1
        fi
    fi
fi

# Manejar cambios locales
if [ "$hay_cambios_locales" = true ] || git diff --quiet && ! git diff --cached --quiet; then
    echo ""
    echo "📝 Procesando cambios locales..."
    
    # Preguntar si usar el script inteligente o hacer commit manual
    echo "¿Cómo quieres hacer el commit?"
    echo "1) Usar script inteligente (recomendado)"
    echo "2) Commit manual rápido"
    echo "3) Omitir commit por ahora"
    
    read -p "Selecciona (1-3): " opcion_commit
    
    case $opcion_commit in
        1)
            echo "🚀 Ejecutando script de commit inteligente..."
            if [ -f "./git_commit_smart.sh" ]; then
                ./git_commit_smart.sh
            else
                echo "❌ Script git_commit_smart.sh no encontrado"
                echo "💡 Haciendo commit manual..."
                git add .
                read -p "📝 Mensaje de commit: " mensaje
                git commit -m "$mensaje"
            fi
            ;;
        2)
            echo "📝 Commit manual rápido..."
            git add .
            read -p "📝 Mensaje de commit: " mensaje
            if [ -n "$mensaje" ]; then
                git commit -m "$mensaje"
                echo "✅ Commit realizado"
            else
                echo "❌ Mensaje vacío, commit cancelado"
            fi
            ;;
        3)
            echo "ℹ️  Commit omitido"
            ;;
        *)
            echo "⚠️  Opción inválida, omitiendo commit"
            ;;
    esac
fi

# Verificar si hay commits para hacer push
echo ""
echo "🔍 Verificando commits pendientes de push..."
commits_pendientes=$(git log origin/experimentos-seguros..HEAD --oneline 2>/dev/null | wc -l)

if [ "$commits_pendientes" -gt 0 ]; then
    echo "📤 Hay $commits_pendientes commit(s) pendiente(s) de push:"
    git log origin/experimentos-seguros..HEAD --oneline | head -5
    
    echo ""
    read -p "🚀 ¿Hacer push a experimentos-seguros? (y/N): " hacer_push
    
    if [[ $hacer_push =~ ^[Yy]$ ]]; then
        echo "🚀 Realizando push seguro..."
        
        # Verificar que estamos en la rama correcta
        if [ "$rama_actual" != "experimentos-seguros" ]; then
            echo "⚠️  Estás en la rama '$rama_actual', no en 'experimentos-seguros'"
            read -p "¿Cambiar a experimentos-seguros? (y/N): " cambiar_rama
            
            if [[ $cambiar_rama =~ ^[Yy]$ ]]; then
                git checkout experimentos-seguros
                if [ $? -eq 0 ]; then
                    echo "✅ Cambiado a rama experimentos-seguros"
                else
                    echo "❌ Error al cambiar de rama"
                    exit 1
                fi
            else
                echo "ℹ️  Manteniéndose en rama actual"
            fi
        fi
        
        # Realizar push
        if git push --force-with-lease origin experimentos-seguros; then
            echo "🎉 Push completado exitosamente"
        else
            echo "❌ Error en el push"
            echo "💡 Posibles causas:"
            echo "   - Hay cambios remotos nuevos (ejecuta el script de nuevo)"
            echo "   - Problemas de autenticación"
            echo "   - Problemas de conectividad"
        fi
    else
        echo "ℹ️  Push omitido. Commits guardados localmente."
    fi
else
    echo "✅ No hay commits pendientes de push"
fi

# Resumen final
echo ""
echo "📊 RESUMEN DE SINCRONIZACIÓN"
echo "============================"
echo "📍 Rama: $(git branch --show-current)"
echo "📝 Último commit: $(git log -1 --oneline)"
echo "🔄 Estado: $(git status --porcelain | wc -l) archivo(s) modificado(s)"

# Verificar estado final
if git diff --quiet && git diff --cached --quiet; then
    echo "✅ Repositorio limpio y sincronizado"
else
    echo "⚠️  Aún hay cambios pendientes"
    git status --short
fi

echo ""
echo "🎯 Sincronización completada"
echo ""
echo "💡 Comandos útiles:"
echo "   ./git_commit_smart.sh  - Commit inteligente"
echo "   ./setup_git_lfs.sh     - Configurar Git LFS"
echo "   ./sync_repo.sh         - Sincronización completa"