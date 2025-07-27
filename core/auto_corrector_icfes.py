#!/usr/bin/env python3
"""
Auto-corrector ICFES v2.0 - Mejorado
Corrige errores específicos identificados en el testing
Integrado con N8N para automatización
"""

import os
import re
import sys
import glob
import argparse
from datetime import datetime

class AutoCorrectorICFES:
    def __init__(self, base_path):
        self.base_path = base_path
        self.errores_corregidos = 0
        self.archivos_procesados = 0
        self.tipos_errores = {
            'metadatos_faltantes': 0,
            'encoding_mal_ubicado': 0,
            'encoding_duplicado': 0,
            'answerlist_corrupto': 0
        }
        
    def corregir_metadatos_icfes(self, contenido):
        """Corrige metadatos ICFES faltantes o incorrectos"""
        correcciones = 0
        
        # Agregar extype si falta
        if 'extype:' not in contenido:
            contenido = re.sub(r'(---\n)', r'\1extype: schoice\n', contenido)
            correcciones += 1
            
        # Agregar exsolution si falta
        if 'exsolution:' not in contenido:
            contenido = re.sub(r'(---\n)', r'\1exsolution: 1000\n', contenido)
            correcciones += 1
            
        # Corregir encoding
        if 'encoding:' not in contenido:
            contenido = re.sub(r'(---\n)', r'\1encoding: UTF-8\n', contenido)
            correcciones += 1
            
        return contenido, correcciones

    def corregir_encoding_mal_ubicado(self, contenido):
        """Corrige encoding: UTF-8 mal ubicado (NUEVO v2.0)"""
        correcciones = 0

        # 1. Eliminar encoding duplicado después del header YAML
        pattern1 = r'(---\s*\n(?:[^-].*\n)*---\s*\n)encoding: UTF-8\s*\n'
        if re.search(pattern1, contenido, re.MULTILINE):
            contenido = re.sub(pattern1, r'\1', contenido, flags=re.MULTILINE)
            correcciones += 1
            self.tipos_errores['encoding_duplicado'] += 1

        # 2. Eliminar encoding dentro de chunks R
        pattern2 = r'(\n# [^\n]*\n)encoding: UTF-8\s*\n'
        matches = re.findall(pattern2, contenido)
        if matches:
            contenido = re.sub(pattern2, r'\1', contenido, flags=re.MULTILINE)
            correcciones += len(matches)
            self.tipos_errores['encoding_mal_ubicado'] += len(matches)

        # 3. Eliminar encoding en secciones Answerlist
        pattern3 = r'(Answerlist\s*\n-{10}\s*\n)encoding: UTF-8\s*\n'
        matches3 = re.findall(pattern3, contenido)
        if matches3:
            contenido = re.sub(pattern3, r'\1', contenido, flags=re.MULTILINE)
            correcciones += len(matches3)
            self.tipos_errores['answerlist_corrupto'] += len(matches3)

        # 4. Eliminar encoding suelto en líneas
        pattern4 = r'\nencoding: UTF-8\s*\n(?!-)'
        matches4 = re.findall(pattern4, contenido)
        if matches4:
            contenido = re.sub(pattern4, r'\n', contenido, flags=re.MULTILINE)
            correcciones += len(matches4)
            self.tipos_errores['encoding_mal_ubicado'] += len(matches4)

        return contenido, correcciones

    def corregir_tikz_errores(self, contenido):
        """Corrige errores comunes en código TikZ"""
        correcciones = 0
        
        # Corregir espaciado en TikZ
        contenido = re.sub(r'\\begin{tikzpicture}\s*\[', r'\\begin{tikzpicture}[', contenido)
        
        # Corregir puntos y comas en TikZ
        contenido = re.sub(r';\s*\\end{tikzpicture}', r';\n\\end{tikzpicture}', contenido)
        
        # Corregir coordenadas mal formateadas
        contenido = re.sub(r'\(\s*(\d+)\s*,\s*(\d+)\s*\)', r'(\1,\2)', contenido)
        
        return contenido, correcciones
    
    def corregir_latex_errores(self, contenido):
        """Corrige errores comunes de LaTeX"""
        correcciones = 0
        
        # Corregir comandos matemáticos
        contenido = re.sub(r'\\frac\s*{\s*([^}]+)\s*}\s*{\s*([^}]+)\s*}', r'\\frac{\1}{\2}', contenido)
        
        # Corregir espacios en comandos
        contenido = re.sub(r'\\sqrt\s*{\s*([^}]+)\s*}', r'\\sqrt{\1}', contenido)
        
        # Corregir referencias a figuras
        contenido = re.sub(r'\\includegraphics\s*\[\s*([^]]+)\s*\]\s*{\s*([^}]+)\s*}', 
                          r'\\includegraphics[\1]{\2}', contenido)
        
        return contenido, correcciones
    
    def procesar_archivo(self, archivo_path):
        """Procesa un archivo .Rmd individual"""
        try:
            with open(archivo_path, 'r', encoding='utf-8') as f:
                contenido = f.read()
            
            contenido_original = contenido
            correcciones_total = 0
            
            # Aplicar correcciones en orden específico (v2.0)
            contenido, corr1 = self.corregir_encoding_mal_ubicado(contenido)  # PRIMERO
            contenido, corr2 = self.corregir_metadatos_icfes(contenido)       # SEGUNDO
            contenido, corr3 = self.corregir_tikz_errores(contenido)          # TERCERO
            contenido, corr4 = self.corregir_latex_errores(contenido)         # CUARTO

            correcciones_total = corr1 + corr2 + corr3 + corr4
            
            # Guardar si hay cambios
            if contenido != contenido_original:
                with open(archivo_path, 'w', encoding='utf-8') as f:
                    f.write(contenido)
                
                self.errores_corregidos += correcciones_total
                print(f"✅ Corregido: {archivo_path} ({correcciones_total} errores)")
            
            self.archivos_procesados += 1
            return True
            
        except Exception as e:
            print(f"❌ Error procesando {archivo_path}: {e}")
            return False
    
    def escanear_recientes(self, horas=24):
        """Escanea archivos .Rmd modificados recientemente"""
        import time
        
        tiempo_limite = time.time() - (horas * 3600)
        archivos_recientes = []
        
        for root, dirs, files in os.walk(self.base_path):
            for file in files:
                if file.endswith('.Rmd'):
                    archivo_path = os.path.join(root, file)
                    if os.path.getmtime(archivo_path) > tiempo_limite:
                        archivos_recientes.append(archivo_path)
        
        return archivos_recientes
    
    def ejecutar_correccion(self, scan_recent=False):
        """Ejecuta el proceso de corrección"""
        print(f"🔧 Iniciando auto-corrección ICFES...")
        print(f"📁 Directorio base: {self.base_path}")
        
        if scan_recent:
            archivos = self.escanear_recientes()
            print(f"🔍 Archivos recientes encontrados: {len(archivos)}")
        else:
            archivos = glob.glob(os.path.join(self.base_path, "**/*.Rmd"), recursive=True)
            print(f"🔍 Total archivos .Rmd: {len(archivos)}")
        
        for archivo in archivos[:10]:  # Limitar a 10 para evitar sobrecarga
            self.procesar_archivo(archivo)
        
        print(f"\n📊 RESUMEN DETALLADO (v2.0):")
        print(f"✅ Archivos procesados: {self.archivos_procesados}")
        print(f"🔧 Errores corregidos: {self.errores_corregidos}")
        print(f"📋 Tipos de errores corregidos:")
        for tipo, cantidad in self.tipos_errores.items():
            if cantidad > 0:
                print(f"   - {tipo}: {cantidad}")

        # Salida JSON para N8N
        resultado = {
            "archivos_procesados": self.archivos_procesados,
            "errores_corregidos": self.errores_corregidos,
            "tipos_errores": self.tipos_errores,
            "timestamp": datetime.now().isoformat(),
            "version": "2.0_mejorado"
        }
        
        import json
        print(json.dumps(resultado))

def main():
    parser = argparse.ArgumentParser(description='Auto-corrector ICFES para N8N')
    parser.add_argument('--scan-recent', action='store_true', 
                       help='Escanear solo archivos recientes')
    parser.add_argument('--base-path', 
                       default='/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/Lab-Manjaro',
                       help='Directorio base del repositorio')
    
    args = parser.parse_args()
    
    corrector = AutoCorrectorICFES(args.base_path)
    corrector.ejecutar_correccion(scan_recent=args.scan_recent)

if __name__ == "__main__":
    main()
