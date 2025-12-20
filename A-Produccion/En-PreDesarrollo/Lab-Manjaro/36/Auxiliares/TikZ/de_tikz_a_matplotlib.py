#!/usr/bin/env python3
import re
import sys
import argparse

def parse_tikz_variables(tikz_code):
    """Extract variables defined with pgfmathsetmacro from TikZ code"""
    variables = {}
    var_pattern = r'\\pgfmathsetmacro{\\(\w+)}{([^}]*)}'
    
    for match in re.finditer(var_pattern, tikz_code):
        var_name = match.group(1)
        var_value = match.group(2)
        # Try to evaluate simple expressions
        try:
            value = eval(var_value, {"__builtins__": {}}, variables)
            variables[var_name] = value
        except:
            # If we can't evaluate, store as string
            variables[var_name] = var_value
    
    return variables

def parse_coordinates(tikz_code):
    """Extract coordinate definitions from TikZ code"""
    coordinates = {}
    coord_pattern = r'\\coordinate\s+\((\w+)\)\s+at\s+\(([^)]*)\);'
    
    for match in re.finditer(coord_pattern, tikz_code):
        coord_name = match.group(1)
        coord_value = match.group(2)
        coordinates[coord_name] = coord_value
    
    return coordinates

def tikz_to_matplotlib(tikz_code):
    """Convert TikZ code to matplotlib Python code"""
    # Parse variables and coordinates
    variables = parse_tikz_variables(tikz_code)
    
    # Extract title or use default
    title = "TikZ Diagram Visualization"
    title_match = re.search(r'\\begin{tikzpicture}\s*\[(.*)\]', tikz_code)
    if title_match:
        title = title_match.group(1).strip()
    
    # Begin building matplotlib code
    matplotlib_code = """import matplotlib.pyplot as plt
import numpy as np
from matplotlib.patches import Ellipse, Arc, FancyArrowPatch
from matplotlib.lines import Line2D

# Variables extracted from TikZ code
"""
    
    # Add variables
    for var_name, var_value in variables.items():
        if isinstance(var_value, (int, float)):
            matplotlib_code += f"{var_name} = {var_value}  # From TikZ\n"
        else:
            # If it's a calculated value from another variable
            if '\\' in str(var_value) and var_name in var_value:
                # Handle case like radioexterno = radiointerno + grosor
                val_parts = var_value.split('+')
                if len(val_parts) == 2:
                    var1 = val_parts[0].strip().replace('\\', '')
                    var2 = val_parts[1].strip().replace('\\', '')
                    matplotlib_code += f"{var_name} = {var1} + {var2}  # From TikZ\n"
                else:
                    matplotlib_code += f"# Couldn't parse complex expression: {var_name} = {var_value}\n"
                    matplotlib_code += f"{var_name} = 1  # Default value, PLEASE ADJUST\n"
            else:
                matplotlib_code += f"{var_name} = {var_value}  # From TikZ\n"
    
    # Add figure setup
    matplotlib_code += """
# Create figure
fig, ax = plt.subplots(figsize=(10, 12))

# Set equal aspect ratio
ax.set_aspect('equal')

"""

    # Look for coordinate definitions
    coordinates = parse_coordinates(tikz_code)
    if coordinates:
        matplotlib_code += "# Define coordinates from TikZ\n"
        for coord_name, coord_value in coordinates.items():
            # Convert TikZ coordinates to Python tuples
            coord_value = coord_value.replace('\\altura', 'altura')
            coord_value = coord_value.replace('\\radiointerno', 'radiointerno')
            coord_value = coord_value.replace('\\radioexterno', 'radioexterno')
            
            if '+' in coord_value or '-' in coord_value:
                # Handle complex coordinate expressions
                matplotlib_code += f"# {coord_name} at ({coord_value}) - complex expression\n"
                if 'centro' in coord_name.lower():
                    # Common center points
                    if 'inferior' in coord_name.lower():
                        matplotlib_code += f"{coord_name} = (0, 0)\n"
                    elif 'superior' in coord_name.lower():
                        matplotlib_code += f"{coord_name} = (0, altura)\n"
                else:
                    # Default handling for complex coordinates
                    matplotlib_code += f"{coord_name} = (0, 0)  # PLEASE ADJUST this default value\n"
            else:
                coord_parts = coord_value.split(',')
                if len(coord_parts) == 2:
                    x, y = coord_parts
                    matplotlib_code += f"{coord_name} = ({x.strip()}, {y.strip()})\n"
                else:
                    matplotlib_code += f"# Couldn't parse coordinate: {coord_name} at ({coord_value})\n"
                    matplotlib_code += f"{coord_name} = (0, 0)  # Default value, PLEASE ADJUST\n"
    
    # Look for drawing elements
    # Detect ellipses
    ellipse_pattern = r'\\draw\s+\(([^)]*)\)\s+ellipse\s+\(([^)]*)\);'
    for match in re.finditer(ellipse_pattern, tikz_code):
        center = match.group(1)
        dims = match.group(2)
        
        # Convert TikZ ellipse dimensions to matplotlib
        center = center.replace('\\altura', 'altura')
        dims = dims.replace('\\radioexterno', 'radioexterno')
        dims = dims.replace('\\radiointerno', 'radiointerno')
        dims = dims.replace('\\vradioexternotop', 'vradioexternotop')
        dims = dims.replace('\\vradiointernotop', 'vradiointernotop')
        
        if 'centro' in center:
            if 'superior' in center:
                center_code = 'centro_superior'
            else:
                center_code = 'centro_inferior'
        else:
            center_code = f"({center})"
        
        # Parse dims which is typically like "{radioexterno} and {vradioexternotop}"
        dims_parts = dims.split('and')
        if len(dims_parts) == 2:
            width = dims_parts[0].strip().replace('{', '').replace('}', '')
            height = dims_parts[1].strip().replace('{', '').replace('}', '')
            
            ellipse_name = f"ellipse_{width}_{height}".replace('*', '').replace('+', '_').replace(' ', '')
            matplotlib_code += f"""
# Ellipse: center at {center}, dims {dims}
{ellipse_name} = Ellipse({center_code}, 2*{width}, 2*{height}, fill=False)
ax.add_patch({ellipse_name})
"""
    
    # Detect arcs
    arc_pattern = r'\\draw(\[.*?\])?\s+\(([^)]*)\)\s+arc\s+\(([^)]*)\);'
    for match in re.finditer(arc_pattern, tikz_code):
        style = match.group(1) if match.group(1) else ""
        start_point = match.group(2)
        arc_params = match.group(3)
        
        # Parse arc parameters
        arc_parts = arc_params.split(':')
        if len(arc_parts) == 2:
            start_angle = arc_parts[0].strip()
            end_angle = arc_parts[1].strip()
            
            # Determine if it's dashed
            linestyle = "dashed" if "dashed" in style else ""
            
            # Create unique name for arc
            arc_name = f"arc_{start_angle.replace('-', 'neg')}_{end_angle.replace('-', 'neg')}"
            if linestyle:
                arc_name = f"dashed_{arc_name}"
            
            matplotlib_code += f"""
# Arc from {start_point} with angles {arc_params}
{arc_name} = Arc((0, 0), 2*radioexterno if '{start_point}' == 'R0b' else 2*radiointerno, 
                2*vradioexternobot if '{start_point}' == 'R0b' else 2*vradiointernobot,
                theta1={start_angle}, theta2={end_angle}{', linestyle="dashed"' if linestyle else ''})
ax.add_patch({arc_name})
"""
    
    # Detect lines
    line_pattern = r'\\draw(\[.*?\])?\s+\(([^)]*)\)\s+--\s+\(([^)]*)\);'
    for match in re.finditer(line_pattern, tikz_code):
        style = match.group(1) if match.group(1) else ""
        point1 = match.group(2)
        point2 = match.group(3)
        
        # Determine if it's dashed
        linestyle = "dashed" if "dashed" in style else ""
        
        # Use coordinate variables if they exist
        if point1 in coordinates:
            p1_code = point1
        else:
            p1_code = f"({point1})"
        
        if point2 in coordinates:
            p2_code = point2
        else:
            p2_code = f"({point2})"
        
        matplotlib_code += f"""
# Line from {point1} to {point2}
ax.add_line(Line2D([{p1_code}[0], {p2_code}[0]], [{p1_code}[1], {p2_code}[1]]{', linestyle="dashed"' if linestyle else ''}))
"""
    
    # Add standard finishing code
    matplotlib_code += """
# Setting the limits
ax.set_xlim(-radioexterno - 1, radioexterno + 1)
ax.set_ylim(-0.5, altura + 1)
ax.axis('off')

# List for image titles
_mfajlsdf98q21_image_title_list = ["{0}"]

plt.tight_layout()
plt.show()
""".format(title)
    
    return matplotlib_code

def process_file(tikz_file_path):
    """Process a single TikZ file and convert it to Python"""
    # Get output path by replacing .tikz extension with .py
    output_file_path = tikz_file_path.replace('.tikz', '.py')
    if output_file_path == tikz_file_path:
        output_file_path = tikz_file_path + '.py'
    
    try:
        with open(tikz_file_path, 'r') as tikz_file:
            tikz_code = tikz_file.read()
            matplotlib_code = tikz_to_matplotlib(tikz_code)
        
        with open(output_file_path, 'w') as output_file:
            output_file.write(matplotlib_code)
            
        print(f"Converted {tikz_file_path} to {output_file_path}")
        return True
    except Exception as e:
        print(f"Error processing {tikz_file_path}: {str(e)}")
        return False

def main():
    parser = argparse.ArgumentParser(description='Convert TikZ code to matplotlib Python code')
    parser.add_argument('tikz_files', nargs='*', 
                        help='TikZ input files (or use stdin if none provided)')
    parser.add_argument('-o', '--output', type=str,
                        help='Output Python file (only used when converting a single file)')
    
    args = parser.parse_args()
    
    if not args.tikz_files:
        # No files provided, read from stdin
        tikz_code = sys.stdin.read()
        matplotlib_code = tikz_to_matplotlib(tikz_code)
        
        if args.output:
            with open(args.output, 'w') as output_file:
                output_file.write(matplotlib_code)
        else:
            sys.stdout.write(matplotlib_code)
    elif len(args.tikz_files) == 1 and args.output:
        # Single file with specified output
        with open(args.tikz_files[0], 'r') as tikz_file:
            tikz_code = tikz_file.read()
            matplotlib_code = tikz_to_matplotlib(tikz_code)
        
        with open(args.output, 'w') as output_file:
            output_file.write(matplotlib_code)
        print(f"Converted {args.tikz_files[0]} to {args.output}")
    else:
        # Process all files, auto-generating output filenames
        import glob
        
        # If the arguments contain wildcards, expand them
        all_files = []
        for pattern in args.tikz_files:
            if '*' in pattern or '?' in pattern or '[' in pattern:
                expanded = glob.glob(pattern)
                if expanded:
                    all_files.extend(expanded)
                else:
                    print(f"Warning: No files match pattern '{pattern}'")
            else:
                all_files.append(pattern)
        
        if not all_files:
            print("No files found to process")
            return
            
        success_count = 0
        for tikz_file in all_files:
            if process_file(tikz_file):
                success_count += 1
                
        print(f"Processed {success_count} of {len(all_files)} files")

if __name__ == "__main__":
    main()