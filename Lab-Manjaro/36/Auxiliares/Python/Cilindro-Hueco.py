#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.patches import Ellipse, FancyArrowPatch, ConnectionPatch
import matplotlib.patheffects as path_effects
from matplotlib.patches import Rectangle

def create_hollow_cylinder():
    # Create figure with the right proportions
    fig, ax = plt.subplots(figsize=(8, 8))
    
    # Define variables (equivalent to TikZ \pgfmathsetmacro)
    altura = 3  # Height of the cylinder
    radiointerno = 1  # Internal radius
    grosor = 0.5  # Wall thickness
    radioexterno = radiointerno + grosor  # External radius = 1 + 0.5 = 1.5
    
    # Vertical radii for perspective (ellipses)
    vradioexternobot = 0.45  # Vertical radius of external bottom ellipse
    vradiointernobot = 0.3   # Vertical radius of internal bottom ellipse
    vradioexternotop = 0.6   # Vertical radius of external top ellipse
    vradiointernotop = 0.4   # Vertical radius of internal top ellipse
    
    # Define custom colors (equivalent to TikZ \definecolor)
    cilindroColor = '#3C78B4'  # Blue for the cylinder (RGB 60, 120, 180)
    lineaOculta = '#969696'    # Gray for hidden lines (RGB 150, 150, 150)
    etiquetaColor = '#323232'  # Dark gray for labels (RGB 50, 50, 50)
    
    # Coordinates of the centers of the bases
    centroinferior = (0, 0)
    centrosuperior = (0, altura)
    
    # Key points on the bases (equivalent to TikZ \coordinate)
    # Bottom base
    R0b = (radioexterno, 0)  # Bottom external right point
    neg_R0b = (-radioexterno, 0)  # Bottom external left point
    r0b = (radiointerno, 0)  # Bottom internal right point
    neg_r0b = (-radiointerno, 0)  # Bottom internal left point
    
    # Top base
    R0t = (radioexterno, altura)  # Top external right point
    neg_R0t = (-radioexterno, altura)  # Top external left point
    r0t = (radiointerno, altura)  # Top internal right point
    neg_r0t = (-radiointerno, altura)  # Top internal left point
    
    # --- Drawing hidden parts (dashed) ---
    # Bottom external back arc
    theta = np.linspace(0, np.pi, 100)
    x_ext_back = radioexterno * np.cos(theta)
    y_ext_back = vradioexternobot * np.sin(theta)
    ax.plot(x_ext_back, y_ext_back, linestyle='--', color=lineaOculta, linewidth=0.8)
    
    # Bottom internal back arc
    x_int_back = radiointerno * np.cos(theta)
    y_int_back = vradiointernobot * np.sin(theta)
    ax.plot(x_int_back, y_int_back, linestyle='--', color=lineaOculta, linewidth=0.8)
    
    # Internal vertical lines (hidden)
    ax.plot([neg_r0b[0], neg_r0t[0]], [neg_r0b[1], neg_r0t[1]], 
            linestyle='--', color=lineaOculta, linewidth=0.8)
    ax.plot([r0b[0], r0t[0]], [r0b[1], r0t[1]], 
            linestyle='--', color=lineaOculta, linewidth=0.8)
    
    # --- Drawing visible parts ---
    # External vertical lines (visible)
    ax.plot([neg_R0b[0], neg_R0t[0]], [neg_R0b[1], neg_R0t[1]], 
            color=cilindroColor, linewidth=1.2)
    ax.plot([R0b[0], R0t[0]], [R0b[1], R0t[1]], 
            color=cilindroColor, linewidth=1.2)
    
    # Bottom external front arc
    theta = np.linspace(0, -np.pi, 100)
    x_ext_front = radioexterno * np.cos(theta)
    y_ext_front = vradioexternobot * np.sin(theta)
    ax.plot(x_ext_front, y_ext_front, color=cilindroColor, linewidth=1.2)
    
    # Bottom internal front arc
    x_int_front = radiointerno * np.cos(theta)
    y_int_front = vradiointernobot * np.sin(theta)
    ax.plot(x_int_front, y_int_front, color=cilindroColor, linewidth=1.2)
    
    # Top external ellipse (visible)
    top_ext_ellipse = Ellipse(centrosuperior, 2*radioexterno, 2*vradioexternotop, 
                             fill=False, edgecolor=cilindroColor, linewidth=1.2)
    ax.add_patch(top_ext_ellipse)
    
    # Top internal ellipse (visible)
    top_int_ellipse = Ellipse(centrosuperior, 2*radiointerno, 2*vradiointernotop, 
                             fill=False, edgecolor=cilindroColor, linewidth=1.2)
    ax.add_patch(top_int_ellipse)
    
    # Subtle shading for depth (equivalent to TikZ transparency group)
    # Create a polygon for the cylinder's visible side
    theta_side = np.linspace(0, -np.pi, 50)
    x_side_bottom = radioexterno * np.cos(theta_side)
    y_side_bottom = vradioexternobot * np.sin(theta_side)
    
    theta_side_top = np.linspace(np.pi, 0, 50)
    x_side_top = radioexterno * np.cos(theta_side_top) 
    y_side_top = altura + vradioexternotop * np.sin(theta_side_top)
    
    x_side = np.concatenate([x_side_bottom, x_side_top[::-1]])
    y_side = np.concatenate([y_side_bottom, y_side_top[::-1]])
    
    ax.fill(x_side, y_side, color=cilindroColor, alpha=0.1)
    
    # Center points (optional)
    ax.plot(centroinferior[0], centroinferior[1], 'o', color=cilindroColor, markersize=3)
    ax.plot(centrosuperior[0], centrosuperior[1], 'o', color=cilindroColor, markersize=3)
    
    # --- Dimensions and labels ---
    # Height
    altura_inicio = (R0b[0] + 0.3, R0b[1])
    altura_fin = (R0t[0] + 0.3, R0t[1])
    
    # Draw height measurement line with arrows
    height_line = FancyArrowPatch(altura_inicio, altura_fin, 
                                 arrowstyle='<->', linewidth=0.9,
                                 color=etiquetaColor, mutation_scale=10)
    ax.add_patch(height_line)
    
    # Height label
    altura_label_pos = ((altura_inicio[0] + altura_fin[0])/2 + 0.2, 
                        (altura_inicio[1] + altura_fin[1])/2)
    ax.text(altura_label_pos[0], altura_label_pos[1], 'Altura', 
            color=etiquetaColor, fontweight='bold', ha='left', va='center')
    
    # Internal radius
    # Draw internal radius line
    internal_radius_line = FancyArrowPatch(centrosuperior, r0t, 
                                          arrowstyle='->', linewidth=0.9,
                                          color=cilindroColor, alpha=0.8, mutation_scale=10)
    ax.add_patch(internal_radius_line)
    
    # Internal radius label with indicator line
    etiqueta_radio_interno = (centrosuperior[0] + 3.0, centrosuperior[1] + 1.3)
    punto_control_radio_interno = (etiqueta_radio_interno[0] - 1.0, etiqueta_radio_interno[1] - 0.6)
    punto_final_radio_interno = (centrosuperior[0] + 0.65*(r0t[0]-centrosuperior[0]) + 0.1, 
                                centrosuperior[1] + 0.65*(r0t[1]-centrosuperior[1]) + 0.1)
    
    # Create a path for the indicator line with a control point
    radio_interno_path = np.array([
        [etiqueta_radio_interno[0], etiqueta_radio_interno[1]],
        [punto_control_radio_interno[0], punto_control_radio_interno[1]],
        [punto_final_radio_interno[0], punto_final_radio_interno[1]]
    ])
    
    # Draw the path
    ax.plot(radio_interno_path[:, 0], radio_interno_path[:, 1], 
            '-', color=etiquetaColor, alpha=0.8, linewidth=0.9)
    
    # Add arrowhead to the end of the path
    arrow_head = FancyArrowPatch(radio_interno_path[-2], radio_interno_path[-1], 
                                arrowstyle='->', linewidth=0.9,
                                color=etiquetaColor, alpha=0.8, mutation_scale=10)
    ax.add_patch(arrow_head)
    
    # Add label with white background
    ax.text(etiqueta_radio_interno[0], etiqueta_radio_interno[1], 
            f'Radio interno = {radiointerno} m', color=etiquetaColor, 
            fontweight='bold', ha='center', va='center', 
            bbox=dict(facecolor='white', edgecolor='none', boxstyle='round,pad=0.3'))
    
    # Wall thickness (grosor)
    # Draw thickness measurement line
    thickness_line = FancyArrowPatch(neg_r0t, neg_R0t, 
                                    arrowstyle='<->', linewidth=0.9,
                                    color=etiquetaColor, mutation_scale=10)
    ax.add_patch(thickness_line)
    
    # Thickness label with indicator line
    etiqueta_grosor = (neg_R0t[0] - 2.0, neg_R0t[1] + 1.4)
    punto_control_grosor = (etiqueta_grosor[0] + 0.7, etiqueta_grosor[1] - 0.7)
    punto_final_grosor = (neg_r0t[0] + 0.5*(neg_R0t[0]-neg_r0t[0]) - 0.1, 
                         neg_r0t[1] + 0.5*(neg_R0t[1]-neg_r0t[1]) + 0.1)
    
    # Create a path for the indicator line with a control point
    grosor_path = np.array([
        [etiqueta_grosor[0], etiqueta_grosor[1]],
        [punto_control_grosor[0], punto_control_grosor[1]],
        [punto_final_grosor[0], punto_final_grosor[1]]
    ])
    
    # Draw the path
    ax.plot(grosor_path[:, 0], grosor_path[:, 1], 
            '-', color=etiquetaColor, alpha=0.8, linewidth=0.9)
    
    # Add arrowhead to the end of the path
    arrow_head = FancyArrowPatch(grosor_path[-2], grosor_path[-1], 
                                arrowstyle='->', linewidth=0.9,
                                color=etiquetaColor, alpha=0.8, mutation_scale=10)
    ax.add_patch(arrow_head)
    
    # Add label with white background
    ax.text(etiqueta_grosor[0], etiqueta_grosor[1], 
            f'Grosor = {grosor} m', color=etiquetaColor, 
            fontweight='bold', ha='center', va='center', 
            bbox=dict(facecolor='white', edgecolor='none', boxstyle='round,pad=0.3'))
    
    # External diameter
    diam_ext_izq = (neg_R0t[0], neg_R0t[1] + 0.75)
    diam_ext_der = (R0t[0], R0t[1] + 0.75)
    
    # Draw diameter measurement line
    diameter_line = FancyArrowPatch(diam_ext_izq, diam_ext_der, 
                                   arrowstyle='<->', linewidth=0.9,
                                   color=etiquetaColor, mutation_scale=10)
    ax.add_patch(diameter_line)
    
    # Diameter label with indicator line
    etiqueta_diametro = ((diam_ext_izq[0] + diam_ext_der[0])/2, diam_ext_izq[1] + 1.3)
    punto_control_diametro = (etiqueta_diametro[0], etiqueta_diametro[1] - 0.6)
    punto_final_diametro = ((diam_ext_izq[0] + diam_ext_der[0])/2, diam_ext_izq[1] + 0.1)
    
    # Create a path for the indicator line with a control point
    diametro_path = np.array([
        [etiqueta_diametro[0], etiqueta_diametro[1]],
        [punto_control_diametro[0], punto_control_diametro[1]],
        [punto_final_diametro[0], punto_final_diametro[1]]
    ])
    
    # Draw the path
    ax.plot(diametro_path[:, 0], diametro_path[:, 1], 
            '-', color=etiquetaColor, alpha=0.8, linewidth=0.9)
    
    # Add arrowhead to the end of the path
    arrow_head = FancyArrowPatch(diametro_path[-2], diametro_path[-1], 
                                arrowstyle='->', linewidth=0.9,
                                color=etiquetaColor, alpha=0.8, mutation_scale=10)
    ax.add_patch(arrow_head)
    
    # Add label with white background
    ax.text(etiqueta_diametro[0], etiqueta_diametro[1], 
            'Diámetro externo', color=etiquetaColor, 
            fontweight='bold', ha='center', va='center', 
            bbox=dict(facecolor='white', edgecolor='none', boxstyle='round,pad=0.3'))
    
    # External radius
    # Draw external radius line
    external_radius_line = FancyArrowPatch(centroinferior, R0b, 
                                          arrowstyle='->', linewidth=0.9,
                                          color=cilindroColor, alpha=0.8, mutation_scale=10)
    ax.add_patch(external_radius_line)
    
    # External radius label with indicator line
    etiqueta_radio_externo = (centroinferior[0] + 3.0, centroinferior[1] - 1.2)
    punto_control_radio_externo = (etiqueta_radio_externo[0] - 1.0, etiqueta_radio_externo[1] + 0.4)
    punto_final_radio_externo = (centroinferior[0] + 0.7*(R0b[0]-centroinferior[0]) + 0.1, 
                                centroinferior[1] + 0.7*(R0b[1]-centroinferior[1]) - 0.1)
    
    # Create a path for the indicator line with a control point
    radio_externo_path = np.array([
        [etiqueta_radio_externo[0], etiqueta_radio_externo[1]],
        [punto_control_radio_externo[0], punto_control_radio_externo[1]],
        [punto_final_radio_externo[0], punto_final_radio_externo[1]]
    ])
    
    # Draw the path
    ax.plot(radio_externo_path[:, 0], radio_externo_path[:, 1], 
            '-', color=etiquetaColor, alpha=0.8, linewidth=0.9)
    
    # Add arrowhead to the end of the path
    arrow_head = FancyArrowPatch(radio_externo_path[-2], radio_externo_path[-1], 
                                arrowstyle='->', linewidth=0.9,
                                color=etiquetaColor, alpha=0.8, mutation_scale=10)
    ax.add_patch(arrow_head)
    
    # Add label with white background
    ax.text(etiqueta_radio_externo[0], etiqueta_radio_externo[1], 
            'Radio externo', color=etiquetaColor, 
            fontweight='bold', ha='center', va='center', 
            bbox=dict(facecolor='white', edgecolor='none', boxstyle='round,pad=0.3'))
    
    # Set equal aspect ratio and limits
    ax.set_aspect('equal')
    ax.set_xlim(-4, 4)
    ax.set_ylim(-2, 5)
    
    # Remove axes
    ax.axis('off')
    
    # Apply the scale factor (equivalent to TikZ scale=1.1)
    # This is handled by the figure size in matplotlib
    
    # Save the figure
    plt.tight_layout()
    plt.savefig('Cilindro-Hueco.png', dpi=300, bbox_inches='tight', transparent=True)
    plt.close()

if __name__ == "__main__":
    create_hollow_cylinder()
    print("Hollow cylinder visualization created successfully.")
