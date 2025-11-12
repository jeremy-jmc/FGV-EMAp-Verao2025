import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import numpy as np
import pandas as pd
from typing import List, Dict
from dataclasses import dataclass


# -----------------------------------------------------------------------------
# Data Models
# -----------------------------------------------------------------------------

@dataclass
class Cliente:
    """Representa un cliente con sus características"""
    id: int
    x: float
    y: float
    demanda_gasohol: float
    demanda_diesel: float
    ventana_inicio: int  # minutos desde 04:00
    ventana_fin: int
    angulo: float
    radio: float


@dataclass
class Cisterna:
    """Representa una cisterna con sus características"""
    tipo: int
    cap_gasohol: float
    cap_diesel: float
    costo_fijo: float
    costo_km: float


@dataclass
class Ruta:
    """Representa una ruta de entrega"""
    cisterna: Cisterna
    clientes: List[int]
    carga_gasohol: float
    carga_diesel: float
    distancia_total: float
    tiempo_total: float
    costo_total: float
    factible: bool
    productos_entregados: Dict[int, List[str]]  # cliente_id -> ['G', 'D']
    tiempos_llegada: List[float]  # Tiempo de llegada a cada cliente (en minutos desde 04:00)


class ProblemInstance:
    """Encapsula los datos y parámetros de una instancia del problema."""
    
    def __init__(self, df: pd.DataFrame, tipos_cisternas: Dict, 
                 num_vehiculos_por_tipo: int = 20,
                 velocidad: float = 60,     # km/h
                 tiempo_descarga: float = 5,    # minutos por producto
                 M: float = 10000):
        """
        Inicializa una instancia del problema.
        
        Args:
            df: DataFrame con información de clientes (ya ordenado por ángulo)
            tipos_cisternas: Diccionario con características de las cisternas
            num_vehiculos_por_tipo: Número de vehículos disponibles por tipo
            velocidad: Velocidad en km/h
            tiempo_descarga: Tiempo de descarga por producto en minutos
            M: Big M para restricciones
        """
        self.df = df
        self.tipos_cisternas = tipos_cisternas
        self.num_vehiculos_por_tipo = num_vehiculos_por_tipo
        self.velocidad = velocidad
        self.tiempo_descarga = tiempo_descarga
        self.M = M
        
        # Crear objetos Cliente
        self.depot = Cliente(
            id=0,
            x=df.loc[0, 'x'],
            y=df.loc[0, 'y'],
            demanda_gasohol=0,
            demanda_diesel=0,
            ventana_inicio=df.loc[0, 'E'],
            ventana_fin=df.loc[0, 'L'],
            angulo=0,
            radio=0
        )
        
        self.clientes = []
        for idx, row in df.iloc[1:].iterrows():
            self.clientes.append(Cliente(
                id=int(row['index']),
                x=row['x'],
                y=row['y'],
                demanda_gasohol=row['demanda_gasohol'],
                demanda_diesel=row['demanda_diesel'],
                ventana_inicio=row['E'],
                ventana_fin=row['L'],
                angulo=row['AN'],
                radio=row['R']
            ))
        
        self.n = len(self.clientes)
        
        # Calcular matriz de distancias
        coords = df[['x', 'y']].values
        self.D = np.sqrt(((coords[:, None] - coords[None, :]) ** 2).sum(axis=2))
        
        # Crear objetos Cisterna
        self.cisternas_disponibles = []
        for tipo, params in tipos_cisternas.items():
            self.cisternas_disponibles.append(Cisterna(
                tipo=tipo,
                cap_gasohol=params['cap_gasohol'],
                cap_diesel=params['cap_diesel'],
                costo_fijo=params['costo_fijo'],
                costo_km=params['costo_km']
            ))
    
    def distancia(self, i: int, j: int) -> float:
        """Retorna la distancia entre los nodos i y j."""
        return self.D[i, j]
    
    def tiempo_viaje(self, i: int, j: int) -> float:
        """Retorna el tiempo de viaje en minutos entre los nodos i y j."""
        return (self.distancia(i, j) / self.velocidad) * 60
    
    def cliente_por_id(self, cliente_id: int) -> Cliente:
        """Retorna el objeto Cliente dado su ID."""
        if cliente_id == 0:
            return self.depot
        return self.clientes[cliente_id - 1]


# -----------------------------------------------------------------------------
# Solution Visualization
# -----------------------------------------------------------------------------

class SolutionVisualizer:
    """Visualiza y reporta soluciones."""
    
    def __init__(self, instance: ProblemInstance):
        self.instance = instance
    
    def imprimir_solucion(self, rutas: List[Ruta], verbosity: int = 1):
        """Imprime la solución de forma legible."""
        print("\n" + "=" * 80)
        print("SOLUCIÓN")
        print("=" * 80)
        
        costo_total = sum(r.costo_total for r in rutas)
        distancia_total = sum(r.distancia_total for r in rutas)
        
        vehiculos_tipo_1 = sum(1 for r in rutas if r.cisterna.tipo == 1)
        vehiculos_tipo_2 = sum(1 for r in rutas if r.cisterna.tipo == 2)
        
        if verbosity >= 1:
            print(f"\n>>> RESUMEN GENERAL:")
            print(f"  * Número de rutas: {len(rutas)}")
            print(f"  * Costo total: ${costo_total:,.2f}")
            print(f"  * Distancia total: {distancia_total:.2f} km")
            print(f"  * Cisternas Tipo 1: {vehiculos_tipo_1}/{self.instance.num_vehiculos_por_tipo}", end="")
            if vehiculos_tipo_1 > self.instance.num_vehiculos_por_tipo:
                print(" !!! EXCEDE LÍMITE", end="")
            print()
            print(f"  * Cisternas Tipo 2: {vehiculos_tipo_2}/{self.instance.num_vehiculos_por_tipo}", end="")
            if vehiculos_tipo_2 > self.instance.num_vehiculos_por_tipo:
                print(" !!! EXCEDE LÍMITE", end="")
            print()

        if verbosity >= 2:    
            print(f"\n>>> DETALLE DE RUTAS:")
            for idx, ruta in enumerate(rutas, 1):
                print(f"\n  Ruta #{idx}:")
                print(f"    Cisterna: Tipo {ruta.cisterna.tipo}")
                print(f"    Secuencia: Depot → {' → '.join(map(str, ruta.clientes))} → Depot")
                print(f"    Carga: Gasohol={ruta.carga_gasohol:.0f}gal ({ruta.carga_gasohol/ruta.cisterna.cap_gasohol*100:.1f}%), "
                    f"Diésel={ruta.carga_diesel:.0f}gal ({ruta.carga_diesel/ruta.cisterna.cap_diesel*100:.1f}%)")
                print(f"    Distancia: {ruta.distancia_total:.2f} km")
                print(f"    Tiempo: {ruta.tiempo_total:.1f} min")
                print(f"    Costo: ${ruta.costo_total:,.2f}")
                print(f"    Tiempos de llegada (min desde 04:00):")
                for cliente_id, tiempo_llegada in zip(ruta.clientes, ruta.tiempos_llegada):
                    hora = 4 + tiempo_llegada // 60
                    minuto = tiempo_llegada % 60
                    cliente = self.instance.cliente_por_id(cliente_id)
                    ventana_str = f"[{4 + cliente.ventana_inicio//60:02.0f}:{cliente.ventana_inicio%60:02.0f} - {4 + cliente.ventana_fin//60:02.0f}:{cliente.ventana_fin%60:02.0f}]"
                    print(f"      - Cliente {cliente_id}: {tiempo_llegada:.1f} min ({hora:02.0f}:{minuto:02.0f}) | Ventana: {ventana_str}")
                print(f"    Entregas por cliente:")
                for cliente_id, productos in ruta.productos_entregados.items():
                    prods_str = ", ".join(productos)
                    print(f"      - Cliente {cliente_id}: {prods_str}")
            
        print("\n" + "="*80)

    def visualizar_rutas(self, rutas: List[Ruta], caption: str = ""):
        """Visualiza las rutas en un mapa 2D."""
        colores_base = [
            '#FF6B6B', '#FFA07A', '#98D8C8', '#F7DC6F', 
            '#45B7D1', '#BB8FCE', '#F8B739', '#85C1E2',
            '#E63946', '#06FFA5', '#A8DADC', '#FF006E', 
            '#FB5607', '#3A86FF', '#FFBE0B', '#06D6A0', 
            '#EF476F', '#8338EC', '#4ECDC4', '#52B788',
            '#118AB2', '#FFD166', '#D62828', '#F77F00',
            '#2EC4B6', '#E71D36', '#011627', '#C9ADA7'
        ]
        
        costo_total = sum(r.costo_total for r in rutas)
        fig, ax = plt.subplots(figsize=(12, 9))
        
        # Dibujar depot
        ax.scatter(self.instance.depot.x, self.instance.depot.y, s=300, c='red', marker='s', 
                  label='Depósito', zorder=5, edgecolors='black', linewidth=2)
        ax.text(self.instance.depot.x + 0.75, self.instance.depot.y + 0.75, 'DEPOT', 
               fontsize=10, fontweight='bold')
        
        # Dibujar clientes
        clientes_x = [c.x for c in self.instance.clientes]
        clientes_y = [c.y for c in self.instance.clientes]
        ax.scatter(clientes_x, clientes_y, s=150, c='lightblue', 
                  label='Clientes', zorder=4, edgecolors='black', linewidth=1)
        
        for cliente in self.instance.clientes:
            ax.text(cliente.x + 0.75, cliente.y + 0.75, str(cliente.id), fontsize=9)
        
        # Dibujar rutas
        legend_elements = []
        
        for idx, ruta in enumerate(rutas):
            color = colores_base[idx % len(colores_base)]
            secuencia = [0] + ruta.clientes + [0]
            
            x_coords = []
            y_coords = []
            for nodo in secuencia:
                cliente = self.instance.cliente_por_id(nodo)
                x_coords.append(cliente.x)
                y_coords.append(cliente.y)
            
            ax.plot(x_coords, y_coords, color=color, linewidth=2, alpha=0.7, zorder=3)
            
            # Flechas de dirección
            for i in range(len(x_coords) - 1):
                dx = x_coords[i+1] - x_coords[i]
                dy = y_coords[i+1] - y_coords[i]
                mid_x = x_coords[i] + dx * 0.5
                mid_y = y_coords[i] + dy * 0.5
                ax.annotate('', xy=(mid_x + dx*0.1, mid_y + dy*0.1), 
                          xytext=(mid_x - dx*0.1, mid_y - dy*0.1),
                          arrowprops=dict(arrowstyle='->', color=color, lw=1.5, alpha=0.8))
            
            tipo_str = f"Tipo {ruta.cisterna.tipo}"
            costo_str = f"${ruta.costo_total:.0f}"
            dist_str = f"{ruta.distancia_total:.1f}km"
            legend_elements.append(
                mpatches.Patch(color=color, 
                              label=f"Ruta {idx+1}: {tipo_str} | {dist_str} | {costo_str}")
            )

        if caption:
            caption = " - " + caption      
        ax.set_xlabel('Coordenada X (km)', fontsize=11)
        ax.set_ylabel('Coordenada Y (km)', fontsize=11)
        ax.set_title('Visualización de Rutas' + caption + f"| CT: ${costo_total:,.2f}", 
                    fontsize=14, fontweight='bold')
        ax.grid(True, alpha=0.3)
        ax.set_aspect('equal', adjustable='box')
        ax.legend(handles=legend_elements, loc='upper left', 
                 bbox_to_anchor=(1.02, 1), fontsize=9, framealpha=0.9)
        
        plt.tight_layout()
        plt.show()

