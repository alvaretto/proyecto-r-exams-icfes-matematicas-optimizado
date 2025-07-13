#!/usr/bin/env python3
"""
🎨 Demo del Agente Graficador Experto TikZ
==========================================

Script de demostración que muestra las capacidades del agente
procesando ejemplos de diferentes tipos de gráficas matemáticas.

Autor: Agente IA Especializado
Fecha: 2025-01-13
Versión: 1.0.0
"""

import sys
import os
from pathlib import Path
import time

# Agregar directorio del agente al path
sys.path.insert(0, str(Path(__file__).parent))

try:
    from agente_core import AgenteTikZ
except ImportError as e:
    print(f"❌ Error importando agente: {e}")
    print("💡 Asegúrate de que todas las dependencias están instaladas:")
    print("   pip install -r requirements.txt")
    sys.exit(1)

def mostrar_banner():
    """Mostrar banner de bienvenida."""
    print("=" * 60)
    print("🎨 AGENTE GRAFICADOR EXPERTO TIKZ - DEMO")
    print("=" * 60)
    print("Convierte imágenes matemáticas en código TikZ profesional")
    print("Compatible con Qtikz/Ktikz")
    print()

def demo_basico():
    """Demostración básica del agente."""
    print("🚀 DEMO BÁSICO")
    print("-" * 30)
    
    try:
        # Inicializar agente
        print("📦 Inicializando agente...")
        agente = AgenteTikZ()
        print("✅ Agente inicializado correctamente")
        
        # Mostrar configuración
        print("\n⚙️ Configuración actual:")
        config_importante = {
            'max_iteraciones': agente.config.get('max_iteraciones'),
            'umbral_similitud': agente.config.get('umbral_similitud'),
            'escala_default': agente.config.get('escala_default'),
            'timeout_compilacion': agente.config.get('timeout_compilacion')
        }
        
        for clave, valor in config_importante.items():
            print(f"  • {clave}: {valor}")
        
        # Mostrar templates disponibles
        print(f"\n📋 Templates disponibles: {len(agente.generador.templates)}")
        for nombre in agente.generador.templates.keys():
            print(f"  • {nombre}")
        
        return agente
        
    except Exception as e:
        print(f"❌ Error en demo básico: {e}")
        return None

def demo_generacion_codigo():
    """Demostración de generación de código sin imagen."""
    print("\n🛠️ DEMO GENERACIÓN DE CÓDIGO")
    print("-" * 35)
    
    try:
        agente = AgenteTikZ()
        
        # Simular análisis de función
        analisis_simulado = {
            'exitoso': True,
            'tipo_detectado': 'funcion',
            'dimensiones': (800, 600),
            'ejes': {'detectados': True},
            'curvas': [
                {
                    'puntos': [[100, 400], [200, 300], [300, 200], [400, 150], [500, 100]],
                    'longitud': 500
                }
            ],
            'puntos_importantes': [[300, 200], [500, 100]],
            'cuadricula': {'presente': True},
            'colores_principales': [(255, 0, 0), (0, 0, 255)]
        }
        
        print("📊 Generando código TikZ para función simulada...")
        codigo = agente.generador.generar(analisis_simulado)
        
        print("✅ Código generado:")
        print("=" * 50)
        print(codigo)
        print("=" * 50)
        
        # Optimizar código
        print("\n⚡ Optimizando código...")
        codigo_optimizado = agente.generador.optimizar(codigo)
        
        print("✅ Código optimizado:")
        print("=" * 50)
        print(codigo_optimizado)
        print("=" * 50)
        
        return codigo_optimizado
        
    except Exception as e:
        print(f"❌ Error en generación de código: {e}")
        return None

def demo_validacion():
    """Demostración de validación de código."""
    print("\n🔍 DEMO VALIDACIÓN DE CÓDIGO")
    print("-" * 32)
    
    try:
        agente = AgenteTikZ()
        
        # Código TikZ de ejemplo
        codigo_ejemplo = """\\begin{tikzpicture}[scale=1.2]
% Ejes coordenados
\\draw[thick, ->] (0,0) -- (5,0) node[below right] {$x$};
\\draw[thick, ->] (0,0) -- (0,4) node[above left] {$y$};

% Función cuadrática
\\draw[blue, very thick] plot[domain=0:4, samples=50] (\\x, {0.25*\\x^2});

% Puntos importantes
\\fill[red] (2,1) circle (2pt);
\\fill[red] (4,4) circle (2pt);

% Origen
\\fill[black] (0,0) circle (1.5pt);
\\end{tikzpicture}"""
        
        print("📝 Validando código TikZ de ejemplo...")
        resultado_validacion = agente.validador.validar(codigo_ejemplo)
        
        print("📊 Resultados de validación:")
        print(f"  • Exitoso: {resultado_validacion['exitoso']}")
        print(f"  • Errores: {len(resultado_validacion['errores'])}")
        print(f"  • Advertencias: {len(resultado_validacion['advertencias'])}")
        print(f"  • Tiempo compilación: {resultado_validacion['tiempo_compilacion']:.2f}s")
        
        if resultado_validacion['errores']:
            print("\n❌ Errores encontrados:")
            for error in resultado_validacion['errores']:
                print(f"  • {error}")
        
        if resultado_validacion['advertencias']:
            print("\n⚠️ Advertencias:")
            for advertencia in resultado_validacion['advertencias']:
                print(f"  • {advertencia}")
        
        # Mostrar métricas
        metricas = resultado_validacion.get('metricas', {})
        if metricas:
            print("\n📈 Métricas de calidad:")
            for metrica, valor in metricas.items():
                print(f"  • {metrica}: {valor}")
        
        return resultado_validacion['exitoso']
        
    except Exception as e:
        print(f"❌ Error en validación: {e}")
        return False

def demo_templates():
    """Demostración de templates disponibles."""
    print("\n📋 DEMO TEMPLATES")
    print("-" * 20)
    
    try:
        agente = AgenteTikZ()
        
        print("📁 Templates disponibles:")
        
        for nombre, contenido in agente.generador.templates.items():
            print(f"\n🎨 Template: {nombre}")
            print("-" * (len(nombre) + 12))
            
            # Mostrar primeras líneas del template
            lineas = contenido.split('\n')[:10]
            for i, linea in enumerate(lineas, 1):
                print(f"{i:2d}: {linea}")
            
            if len(contenido.split('\n')) > 10:
                print("    ... (más líneas)")
            
            print(f"📊 Estadísticas:")
            print(f"  • Líneas totales: {len(contenido.split('\n'))}")
            print(f"  • Comandos \\draw: {contenido.count('\\draw')}")
            print(f"  • Comandos \\fill: {contenido.count('\\fill')}")
            print(f"  • Comandos \\node: {contenido.count('\\node')}")
        
        return True
        
    except Exception as e:
        print(f"❌ Error mostrando templates: {e}")
        return False

def demo_configuracion():
    """Demostración de configuración del agente."""
    print("\n⚙️ DEMO CONFIGURACIÓN")
    print("-" * 22)
    
    try:
        agente = AgenteTikZ()
        
        print("📋 Configuración completa:")
        for seccion, valores in agente.config.items():
            if isinstance(valores, dict):
                print(f"\n📁 {seccion}:")
                for clave, valor in valores.items():
                    print(f"  • {clave}: {valor}")
            else:
                print(f"• {seccion}: {valores}")
        
        # Demostrar modificación de configuración
        print("\n🔧 Modificando configuración...")
        config_original = agente.config['escala_default']
        agente.config['escala_default'] = 1.5
        
        print(f"  • escala_default: {config_original} → {agente.config['escala_default']}")
        
        # Restaurar configuración
        agente.config['escala_default'] = config_original
        print(f"  • Restaurado a: {agente.config['escala_default']}")
        
        return True
        
    except Exception as e:
        print(f"❌ Error en configuración: {e}")
        return False

def demo_completo():
    """Ejecutar demostración completa."""
    mostrar_banner()
    
    resultados = {}
    
    # Demo básico
    agente = demo_basico()
    resultados['basico'] = agente is not None
    
    if not resultados['basico']:
        print("❌ Demo básico falló. Abortando demos restantes.")
        return resultados
    
    # Demo generación
    codigo = demo_generacion_codigo()
    resultados['generacion'] = codigo is not None
    
    # Demo validación
    validacion_ok = demo_validacion()
    resultados['validacion'] = validacion_ok
    
    # Demo templates
    templates_ok = demo_templates()
    resultados['templates'] = templates_ok
    
    # Demo configuración
    config_ok = demo_configuracion()
    resultados['configuracion'] = config_ok
    
    # Resumen final
    print("\n" + "=" * 60)
    print("📊 RESUMEN DE DEMOS")
    print("=" * 60)
    
    total_demos = len(resultados)
    demos_exitosos = sum(resultados.values())
    
    for demo, exitoso in resultados.items():
        estado = "✅" if exitoso else "❌"
        print(f"{estado} {demo.capitalize()}")
    
    print(f"\n🎯 Resultado: {demos_exitosos}/{total_demos} demos exitosos")
    
    if demos_exitosos == total_demos:
        print("🎉 ¡Todos los demos completados exitosamente!")
        print("🚀 El agente está listo para usar.")
    else:
        print("⚠️ Algunos demos fallaron. Revisar configuración y dependencias.")
    
    print("\n💡 Para usar el agente:")
    print("   from agente_core import AgenteTikZ")
    print("   agente = AgenteTikZ()")
    print("   resultado = agente.procesar_imagen('mi_imagen.png')")
    
    return resultados

def menu_interactivo():
    """Menú interactivo para ejecutar demos específicos."""
    while True:
        print("\n" + "=" * 50)
        print("🎨 AGENTE TIKZ - MENÚ INTERACTIVO")
        print("=" * 50)
        print("1. Demo completo")
        print("2. Demo básico")
        print("3. Demo generación de código")
        print("4. Demo validación")
        print("5. Demo templates")
        print("6. Demo configuración")
        print("0. Salir")
        print()
        
        try:
            opcion = input("Selecciona una opción (0-6): ").strip()
            
            if opcion == "0":
                print("👋 ¡Hasta luego!")
                break
            elif opcion == "1":
                demo_completo()
            elif opcion == "2":
                demo_basico()
            elif opcion == "3":
                demo_generacion_codigo()
            elif opcion == "4":
                demo_validacion()
            elif opcion == "5":
                demo_templates()
            elif opcion == "6":
                demo_configuracion()
            else:
                print("❌ Opción inválida. Intenta de nuevo.")
                
        except KeyboardInterrupt:
            print("\n👋 ¡Hasta luego!")
            break
        except Exception as e:
            print(f"❌ Error: {e}")

if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "--interactivo":
        menu_interactivo()
    else:
        demo_completo()
