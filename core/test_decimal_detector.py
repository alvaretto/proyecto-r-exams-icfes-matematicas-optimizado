#!/usr/bin/env python3
"""
Script de prueba para el detector de errores de formato de decimales
"""

import sys
import os
import json
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from decimal_format_detector import DecimalFormatDetector

def test_decimal_format_detector():
    """Probar el detector de errores de formato de decimales"""
    
    test_file = "/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/Auxiliares/Ejemplos-Funcionales-Rmd/Plantillas/tex/Graficos_Estadisticos_Adopcion_Mascotas/I_1796473-Opc-A2v2_optimizado.Rmd"
    
    if not os.path.exists(test_file):
        print(f"❌ Archivo de prueba no encontrado: {test_file}")
        return False
    
    print("🔍 DETECTOR DE ERRORES DE FORMATO DE DECIMALES")
    print(f"📁 Archivo: {os.path.basename(test_file)}")
    print("=" * 80)
    
    try:
        # PASO 1: Detectar errores de formato de decimales
        print("\n🔍 PASO 1: ANÁLISIS DE FORMATO DE DECIMALES")
        detector = DecimalFormatDetector()
        results = detector.analyze_rmd_decimal_formatting(test_file)
        
        print(f"   ❌ Errores de formato: {results['summary']['total_decimal_errors']}")
        print(f"   🔧 Correcciones disponibles: {results['summary']['total_corrections_available']}")
        
        # PASO 2: Mostrar errores específicos encontrados
        print("\n❌ PASO 2: ERRORES ESPECÍFICOS DETECTADOS")
        if results['decimal_format_errors']:
            for i, error in enumerate(results['decimal_format_errors'], 1):
                print(f"\n   {i}. {error['type'].upper()}")
                print(f"      📝 Mensaje: {error['message']}")
                print(f"      📍 Línea: {error.get('line_start', 'N/A')}")
                print(f"      🔧 Sugerencia: {error.get('suggestion', 'N/A')}")
                if 'code' in error:
                    print(f"      💻 Código: {error['code'][:60]}...")
        else:
            print("   ✅ No se detectaron errores de formato de decimales")
        
        # PASO 3: Mostrar análisis de código
        print("\n📊 PASO 3: ANÁLISIS DE CÓDIGO R")
        code_analysis = results['code_analysis']
        
        if code_analysis['pie_chart_formatting']:
            print(f"   🥧 Gráficas de torta encontradas: {len(code_analysis['pie_chart_formatting'])}")
            for i, pie_code in enumerate(code_analysis['pie_chart_formatting'], 1):
                print(f"      {i}. {pie_code['code'][:50]}...")
        
        if code_analysis['percentage_formatting']:
            print(f"   📊 Formatos de porcentaje encontrados: {len(code_analysis['percentage_formatting'])}")
            for i, pct_code in enumerate(code_analysis['percentage_formatting'], 1):
                print(f"      {i}. {pct_code['code'][:50]}...")
        
        # PASO 4: Mostrar correcciones sugeridas
        print("\n🔧 PASO 4: CORRECCIONES SUGERIDAS")
        if results['corrections_suggested']:
            for i, correction in enumerate(results['corrections_suggested'], 1):
                print(f"\n   {i}. {correction['error_type'].upper()}")
                print(f"      📝 Explicación: {correction['explanation']}")
                print(f"      💻 Código original: {correction['original_code'][:50]}...")
                print(f"      ✅ Implementación: {correction['implementation']}")
        else:
            print("   ℹ️ No hay correcciones automáticas disponibles")
        
        # PASO 5: Mostrar desglose por severidad
        print("\n📊 PASO 5: DESGLOSE POR SEVERIDAD")
        severity = results['summary']['severity_breakdown']
        print(f"   🔴 Errores críticos: {severity['error']}")
        print(f"   🟡 Advertencias: {severity['warning']}")
        
        # PASO 6: Recomendaciones específicas
        print("\n💡 PASO 6: RECOMENDACIONES ESPECÍFICAS")
        print("   🎯 PROBLEMA IDENTIFICADO: Torta muestra decimales innecesarios (.0)")
        print("   ✅ SOLUCIÓN: Solo mostrar decimal si es distinto de cero")
        print("   📝 IMPLEMENTACIÓN:")
        print("      • Usar formato condicional en lugar de sprintf fijo")
        print("      • Verificar que 25.0 se muestre como '25%' y 25.3 como '25.3%'")
        
        # PASO 7: Guardar resultados
        results_file = "/tmp/decimal_format_analysis.json"
        with open(results_file, 'w', encoding='utf-8') as f:
            json.dump(results, f, indent=2, ensure_ascii=False)
        
        print(f"\n💾 Resultados guardados en: {results_file}")
        
        # PASO 8: Resumen final
        print("\n📋 RESUMEN FINAL:")
        print(f"   📁 Archivo analizado: {os.path.basename(test_file)}")
        print(f"   ❌ Errores encontrados: {results['summary']['total_decimal_errors']}")
        print(f"   🔧 Correcciones disponibles: {results['summary']['total_corrections_available']}")
        print(f"   🎯 Estado: {results['summary']['status'].upper()}")
        
        if results['summary']['total_decimal_errors'] > 0:
            print("\n🚨 ACCIÓN REQUERIDA:")
            print("   1. Revisar errores de formato de decimales detectados")
            print("   2. Implementar formato condicional para porcentajes")
            print("   3. Verificar que tortas no muestren decimales innecesarios")
        
        return True
        
    except Exception as e:
        print(f"❌ Error en la detección: {e}")
        import traceback
        traceback.print_exc()
        return False

if __name__ == "__main__":
    success = test_decimal_format_detector()
    sys.exit(0 if success else 1)
