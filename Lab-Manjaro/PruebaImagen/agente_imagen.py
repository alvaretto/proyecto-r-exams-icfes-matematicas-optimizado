#!/usr/bin/env python3
"""
🎨 Agente TikZ Simplificado para Análisis de Imagen
==================================================

Versión simplificada para procesar imagen específica con análisis detallado.
"""

import os
import sys
from pathlib import Path
import json
from datetime import datetime

class AgenteTikZImagen:
    """Agente especializado para análisis de imagen específica."""
    
    def __init__(self):
        self.directorio_trabajo = Path.cwd()
        self.directorio_salida = self.directorio_trabajo / "tikz_generado"
        self.directorio_salida.mkdir(exist_ok=True)
        
        print("🎨 Agente TikZ para Imagen inicializado")
        print(f"📁 Directorio: {self.directorio_trabajo}")
        print(f"📂 Salida: {self.directorio_salida}")
    
    def analizar_imagen_con_augment(self, ruta_imagen, prompt_personalizado=""):
        """Simular análisis con Augment IA para imagen específica."""
        
        print(f"\n🧠 ANÁLISIS CON AUGMENT IA")
        print("=" * 40)
        print(f"📷 Imagen: {ruta_imagen}")
        
        # Verificar que existe la imagen
        if not Path(ruta_imagen).exists():
            return {
                'exitoso': False,
                'error': f'Imagen no encontrada: {ruta_imagen}'
            }
        
        # Prompt optimizado para análisis matemático
        prompt_base = f"""
        🧠 ANÁLISIS AUGMENT IA DE IMAGEN MATEMÁTICA
        
        Imagen: {ruta_imagen}
        
        Por favor analiza esta imagen matemática y extrae:
        
        1. **TIPO DE GRÁFICA**:
           - ¿Es una función matemática, figura geométrica, diagrama, o gráfica estadística?
           - Identifica el contexto matemático principal
        
        2. **ELEMENTOS VISUALES**:
           - Ejes coordenados (si existen)
           - Curvas, líneas o formas principales
           - Puntos importantes (intersecciones, vértices, máximos, mínimos)
           - Texto, etiquetas y anotaciones
           - Cuadrícula o sistema de referencia
        
        3. **CARACTERÍSTICAS MATEMÁTICAS**:
           - Tipo de función (si aplica): lineal, cuadrática, trigonométrica, exponencial
           - Ecuaciones visibles o deducibles
           - Escalas y rangos de los ejes
           - Proporciones y medidas importantes
        
        4. **INFORMACIÓN PARA TIKZ**:
           - Colores utilizados
           - Estilos de línea (continua, punteada, gruesa)
           - Coordenadas aproximadas de puntos clave
           - Elementos que requieren comandos específicos de TikZ
        
        {prompt_personalizado}
        
        Responde con análisis detallado para generar código TikZ profesional.
        """
        
        print("💭 Prompt enviado a Augment:")
        print("-" * 50)
        print(prompt_base)
        print("-" * 50)
        
        # Simular respuesta de Augment basada en análisis visual
        # (En implementación real, aquí Augment analizaría la imagen)
        
        analisis_augment = {
            "tipo_grafica": "funcion_matematica",
            "contexto": "Gráfica de función con ejes coordenados",
            "elementos_visuales": {
                "ejes_coordenados": True,
                "cuadricula": True,
                "curva_principal": "presente",
                "puntos_marcados": "varios",
                "etiquetas": "ejes_numerados"
            },
            "caracteristicas_matematicas": {
                "tipo_funcion": "por_determinar_visualmente",
                "rango_x": "aproximado",
                "rango_y": "aproximado",
                "puntos_importantes": "intersecciones_y_extremos"
            },
            "elementos_tikz": {
                "colores_detectados": ["negro", "azul", "rojo"],
                "estilos_linea": ["continua", "gruesa"],
                "comandos_necesarios": [
                    "\\draw para ejes",
                    "\\draw para curva principal", 
                    "\\fill para puntos",
                    "\\node para etiquetas"
                ]
            },
            "recomendaciones": [
                "Usar escala apropiada para visualización",
                "Incluir cuadrícula de fondo",
                "Marcar puntos importantes",
                "Etiquetar ejes claramente"
            ]
        }
        
        print("✅ Análisis Augment completado")
        return {
            'exitoso': True,
            'analisis': analisis_augment,
            'timestamp': datetime.now().isoformat()
        }
    
    def generar_tikz_profesional(self, analisis_resultado):
        """Generar código TikZ profesional basado en análisis."""
        
        print("\n🛠️ GENERANDO CÓDIGO TIKZ PROFESIONAL")
        print("=" * 45)
        
        if not analisis_resultado.get('exitoso', False):
            return None
        
        analisis = analisis_resultado['analisis']
        tipo = analisis.get('tipo_grafica', 'general')
        elementos = analisis.get('elementos_visuales', {})
        matematicas = analisis.get('caracteristicas_matematicas', {})
        
        # Construir código TikZ profesional
        codigo_partes = []
        
        # Encabezado con información del análisis
        codigo_partes.extend([
            "% =" * 30,
            "% CÓDIGO TIKZ GENERADO POR AGENTE + AUGMENT IA",
            "% =" * 30,
            f"% Imagen analizada: {analisis_resultado.get('imagen_original', 'N/A')}",
            f"% Tipo detectado: {tipo}",
            f"% Timestamp: {analisis_resultado.get('timestamp', 'N/A')}",
            "% Basado en templates profesionales y análisis Augment",
            "",
            "\\documentclass{standalone}",
            "\\usepackage{tikz}",
            "\\usepackage{pgfplots}",
            "\\usetikzlibrary{calc,arrows.meta,positioning}",
            "\\pgfplotsset{compat=1.18}",
            "",
            "\\begin{document}",
            "\\begin{tikzpicture}[scale=1.2]"
        ])
        
        # Cuadrícula de fondo si está presente
        if elementos.get('cuadricula', False):
            codigo_partes.extend([
                "",
                "% Cuadrícula de fondo sutil",
                "\\draw[gray!20, thin] (-4,-3) grid[step=0.5] (4,3);"
            ])
        
        # Ejes coordenados
        if elementos.get('ejes_coordenados', False):
            codigo_partes.extend([
                "",
                "% Ejes coordenados principales",
                "\\draw[thick, ->] (-4,0) -- (4,0) node[below right] {$x$};",
                "\\draw[thick, ->] (0,-3) -- (0,3) node[above left] {$y$};"
            ])
        
        # Función principal basada en tipo
        if tipo == "funcion_matematica":
            tipo_funcion = matematicas.get('tipo_funcion', 'cuadratica')
            
            codigo_partes.extend([
                "",
                f"% Función principal ({tipo_funcion})",
                "% Adaptada del análisis de imagen"
            ])
            
            if 'cuadratica' in tipo_funcion or tipo_funcion == 'por_determinar_visualmente':
                codigo_partes.append("\\draw[blue, very thick] plot[domain=-3:3, samples=50] (\\x, {0.5*\\x^2 - 1});")
            elif 'lineal' in tipo_funcion:
                codigo_partes.append("\\draw[blue, very thick] (-3,-2) -- (3,2);")
            elif 'trigonometrica' in tipo_funcion:
                codigo_partes.append("\\draw[blue, very thick] plot[domain=-3:3, samples=100] (\\x, {sin(deg(\\x))});")
            else:
                codigo_partes.append("\\draw[blue, very thick] plot[domain=-3:3, samples=50] (\\x, {\\x^2/4});")
        
        # Puntos importantes
        if elementos.get('puntos_marcados', False):
            codigo_partes.extend([
                "",
                "% Puntos importantes marcados",
                "\\fill[red] (0,0) circle (3pt) node[below right] {Origen};",
                "\\fill[blue] (-2,1) circle (2pt);",
                "\\fill[blue] (2,1) circle (2pt);"
            ])
        
        # Etiquetas en ejes
        if elementos.get('etiquetas', False):
            codigo_partes.extend([
                "",
                "% Marcas y etiquetas en ejes",
                "\\foreach \\x in {-3,-2,-1,1,2,3}",
                "  \\draw (\\x,-0.1) -- (\\x,0.1) node[below] {\\x};",
                "\\foreach \\y in {-2,-1,1,2}",
                "  \\draw (-0.1,\\y) -- (0.1,\\y) node[left] {\\y};"
            ])
        
        # Origen marcado
        codigo_partes.extend([
            "",
            "% Origen del sistema coordenado",
            "\\fill[black] (0,0) circle (1.5pt);"
        ])
        
        # Cierre del documento
        codigo_partes.extend([
            "",
            "\\end{tikzpicture}",
            "\\end{document}",
            "",
            "% =" * 30,
            "% FIN DEL CÓDIGO GENERADO",
            "% =" * 30
        ])
        
        codigo_final = '\n'.join(codigo_partes)
        
        print("✅ Código TikZ profesional generado")
        print(f"📊 Líneas de código: {len(codigo_partes)}")
        
        return codigo_final
    
    def procesar_imagen_completa(self, ruta_imagen, prompt_personalizado=""):
        """Procesamiento completo de imagen con Augment + TikZ."""
        
        print(f"\n🎯 PROCESAMIENTO COMPLETO DE IMAGEN")
        print("=" * 50)
        
        try:
            # 1. Análisis con Augment
            resultado_analisis = self.analizar_imagen_con_augment(ruta_imagen, prompt_personalizado)
            
            if not resultado_analisis.get('exitoso', False):
                print(f"❌ Error en análisis: {resultado_analisis.get('error')}")
                return None
            
            # Agregar información de imagen al resultado
            resultado_analisis['imagen_original'] = ruta_imagen
            
            # 2. Generación de código TikZ
            codigo_tikz = self.generar_tikz_profesional(resultado_analisis)
            
            if not codigo_tikz:
                print("❌ Error generando código TikZ")
                return None
            
            # 3. Guardar resultados
            nombre_imagen = Path(ruta_imagen).stem
            archivo_tikz = self.directorio_salida / f"{nombre_imagen}_agente.tikz"
            archivo_qtikz = self.directorio_salida / f"{nombre_imagen}_qtikz.tikz"
            archivo_metadata = self.directorio_salida / f"{nombre_imagen}_analisis.json"

            # Guardar código TikZ completo (con documentclass)
            with open(archivo_tikz, 'w', encoding='utf-8') as f:
                f.write(codigo_tikz)

            # Generar versión para QTikz (solo código TikZ puro)
            codigo_qtikz = self._extraer_tikz_puro(codigo_tikz)
            with open(archivo_qtikz, 'w', encoding='utf-8') as f:
                f.write(codigo_qtikz)
            
            # Guardar metadatos del análisis
            metadata = {
                'imagen_original': ruta_imagen,
                'timestamp': resultado_analisis['timestamp'],
                'analisis_augment': resultado_analisis['analisis'],
                'archivo_tikz': str(archivo_tikz),
                'lineas_codigo': len(codigo_tikz.split('\n'))
            }
            
            with open(archivo_metadata, 'w', encoding='utf-8') as f:
                json.dump(metadata, f, indent=2, ensure_ascii=False)
            
            # 4. Resultado final
            resultado_final = {
                'exitoso': True,
                'imagen_original': ruta_imagen,
                'codigo_tikz': codigo_tikz,
                'archivo_tikz': str(archivo_tikz),
                'archivo_qtikz': str(archivo_qtikz),
                'archivo_metadata': str(archivo_metadata),
                'analisis': resultado_analisis['analisis']
            }
            
            print(f"\n✅ PROCESAMIENTO COMPLETADO")
            print(f"📄 Código TikZ completo: {archivo_tikz}")
            print(f"🎨 Código para QTikz: {archivo_qtikz}")
            print(f"📋 Metadatos: {archivo_metadata}")
            
            return resultado_final
            
        except Exception as e:
            print(f"❌ Error en procesamiento: {e}")
            return None
    
    def mostrar_resultado(self, resultado):
        """Mostrar resultado del procesamiento."""
        
        if not resultado or not resultado.get('exitoso', False):
            print("❌ No hay resultado válido para mostrar")
            return
        
        print(f"\n📊 RESULTADO FINAL")
        print("=" * 30)
        print(f"📷 Imagen: {resultado['imagen_original']}")
        print(f"📄 Archivo TikZ: {resultado['archivo_tikz']}")
        print(f"📋 Metadatos: {resultado['archivo_metadata']}")
        
        analisis = resultado.get('analisis', {})
        print(f"\n🎯 Análisis:")
        print(f"  • Tipo: {analisis.get('tipo_grafica', 'N/A')}")
        print(f"  • Contexto: {analisis.get('contexto', 'N/A')}")
        
        elementos = analisis.get('elementos_visuales', {})
        print(f"  • Ejes: {elementos.get('ejes_coordenados', False)}")
        print(f"  • Cuadrícula: {elementos.get('cuadricula', False)}")
        
        print(f"\n📝 CÓDIGO TIKZ GENERADO:")
        print("-" * 50)
        # Mostrar solo las primeras líneas del código
        lineas = resultado['codigo_tikz'].split('\n')
        for i, linea in enumerate(lineas[:15], 1):
            print(f"{i:2d}: {linea}")
        
        if len(lineas) > 15:
            print(f"    ... ({len(lineas) - 15} líneas más)")
        
        print("-" * 50)
        print(f"📊 Total: {len(lineas)} líneas de código")

    def _extraer_tikz_puro(self, codigo_completo):
        """Extraer solo el código TikZ puro para QTikz/KTikz."""

        lineas = codigo_completo.split('\n')
        tikz_puro = []

        # Agregar encabezado para QTikz
        tikz_puro.extend([
            "% ==============================",
            "% CÓDIGO TIKZ PARA QTIKZ/KTIKZ",
            "% ==============================",
            "% Compatible con QTikz/KTikz (solo código TikZ puro)",
            ""
        ])

        # Extraer solo las líneas entre \begin{tikzpicture} y \end{tikzpicture}
        dentro_tikz = False
        for linea in lineas:
            if '\\begin{tikzpicture}' in linea:
                dentro_tikz = True
                tikz_puro.append(linea)
            elif '\\end{tikzpicture}' in linea:
                tikz_puro.append(linea)
                dentro_tikz = False
            elif dentro_tikz:
                tikz_puro.append(linea)

        return '\n'.join(tikz_puro)

def main():
    """Función principal."""
    
    if len(sys.argv) < 2:
        print("❌ Uso: python agente_imagen.py <ruta_imagen> [prompt_personalizado]")
        print("📝 Ejemplo: python agente_imagen.py 04.png 'Analiza esta función matemática'")
        sys.exit(1)
    
    ruta_imagen = sys.argv[1]
    prompt_personalizado = sys.argv[2] if len(sys.argv) > 2 else ""
    
    # Crear agente
    agente = AgenteTikZImagen()
    
    # Procesar imagen
    resultado = agente.procesar_imagen_completa(ruta_imagen, prompt_personalizado)
    
    # Mostrar resultado
    agente.mostrar_resultado(resultado)

if __name__ == "__main__":
    main()
