#!/bin/bash

# Script para aplicar configuración global a todos los modos de Roo Code
# Contexto principal: /home/pequeniomanjaro/Documentos/proyecto-r-exams-icfes-matematicas-optimizado/Auxiliares/rules_full/rules_full_v1.md

GLOBAL_CONTEXT_HEADER='## 🌟 CONTEXTO GLOBAL PRINCIPAL

**REFERENCIA OBLIGATORIA:** `/home/pequeniomanjaro/Documentos/proyecto-r-exams-icfes-matematicas-optimizado/Auxiliares/rules_full/rules_full_v1.md`

Todas las operaciones DEBEN:
1. **CONSULTAR PRIMERO** el archivo de contexto global
2. Seguir las estructuras y patrones ICFES definidos
3. Usar los ejemplos funcionales como referencia
4. Aplicar las validaciones y metadatos especificados
5. Mantener coherencia con el proyecto R-exams ICFES

⸻'

# Función para actualizar un archivo de reglas
update_mode_rules() {
    local file="$1"
    local temp_file="${file}.tmp"
    
    if [ -f "$file" ]; then
        echo "Actualizando: $file"
        
        # Crear archivo temporal con el nuevo contenido
        {
            head -n 1 "$file"  # Primera línea (Goal)
            echo ""
            echo "$GLOBAL_CONTEXT_HEADER"
            echo ""
            tail -n +2 "$file" | sed '/^0 · Onboarding/,$d'  # Contenido hasta Onboarding
            echo "0 · Onboarding"
            echo ""
            tail -n +2 "$file" | sed -n '/^0 · Onboarding/,$p' | tail -n +3  # Desde Onboarding en adelante
        } > "$temp_file"
        
        # Reemplazar archivo original
        mv "$temp_file" "$file"
        echo "✅ Actualizado: $file"
    else
        echo "❌ Archivo no encontrado: $file"
    fi
}

# Actualizar todos los modos
for mode_dir in .roo/rules-*/; do
    if [ -d "$mode_dir" ]; then
        rules_file="${mode_dir}rules.md"
        if [ -f "$rules_file" ]; then
            update_mode_rules "$rules_file"
        fi
    fi
done

echo ""
echo "🎯 Configuración global aplicada a todos los modos de Roo Code"
echo "📁 Contexto principal: Auxiliares/rules_full/rules_full_v1.md"