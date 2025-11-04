#!/bin/bash

echo "╔═══════════════════════════════════════════════════════════════════╗"
echo "║  VERIFICACIÓN DE CONSISTENCIA - NUEVA GENERACIÓN                  ║"
echo "╚═══════════════════════════════════════════════════════════════════╝"
echo ""

# Extraer escenarios
grep -n "Escenario" nueva_con_sol.txt | head -15 > escenarios_nueva_con.txt
grep -n "Escenario" nueva_sin_sol.txt | head -15 > escenarios_nueva_sin.txt

echo "📊 Total de escenarios encontrados:"
echo "   - CON soluciones: $(wc -l < escenarios_nueva_con.txt)"
echo "   - SIN soluciones: $(wc -l < escenarios_nueva_sin.txt)"
echo ""

echo "🔍 Verificando consistencia entre versiones..."
echo ""

diferencias=0
for i in {1..15}; do
    contexto_con=$(sed -n "${i}p" escenarios_nueva_con.txt | cut -d: -f2-)
    contexto_sin=$(sed -n "${i}p" escenarios_nueva_sin.txt | cut -d: -f2-)
    
    if [ "$contexto_con" = "$contexto_sin" ]; then
        echo "✅ Pregunta $i: IDÉNTICA"
    else
        echo "❌ Pregunta $i: DIFERENTE"
        diferencias=$((diferencias + 1))
    fi
done

echo ""
echo "═══════════════════════════════════════════════════════════════════"
if [ $diferencias -eq 0 ]; then
    echo "✅ RESULTADO: TODAS LAS 15 PREGUNTAS SON IDÉNTICAS"
    echo "   Las versiones CON y SIN soluciones son consistentes."
else
    echo "❌ RESULTADO: Se encontraron $diferencias diferencias"
fi
echo "═══════════════════════════════════════════════════════════════════"
echo ""

# Comparar con la generación anterior
echo "🔄 Comparando con la generación anterior..."
echo ""

if [ -f "con_soluciones_nuevo.txt" ]; then
    echo "Primeras 3 preguntas de la generación ANTERIOR:"
    grep "Escenario" con_soluciones_nuevo.txt | head -3 | nl
    echo ""
    echo "Primeras 3 preguntas de la generación ACTUAL:"
    grep "Escenario" nueva_con_sol.txt | head -3 | nl
    echo ""
    
    # Verificar si el orden cambió
    primera_anterior=$(grep "Escenario" con_soluciones_nuevo.txt | head -1)
    primera_actual=$(grep "Escenario" nueva_con_sol.txt | head -1)
    
    if [ "$primera_anterior" = "$primera_actual" ]; then
        echo "⚠️  ADVERTENCIA: El orden de las preguntas NO cambió"
        echo "   (La primera pregunta es la misma en ambas generaciones)"
    else
        echo "✅ CONFIRMADO: El orden de las preguntas SÍ cambió"
        echo "   (La aleatorización está funcionando correctamente)"
    fi
else
    echo "ℹ️  No hay generación anterior para comparar"
fi

echo ""
