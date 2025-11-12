import pandas as pd
import numpy as np
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
