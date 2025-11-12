"""
Problema de ruteo de vehiculos con multiples compartimento, flota heterogenea y ventanas de tiempo.

0. Contexto:
    PetroAndes Logistics S.A.C. es una empresa dedicada a la distribución de combustibles líquidos, principalmente gasohol 90 y diésel B5, hacia estaciones de servicio y clientes industriales en el sur del país. 
    
    Cada día la empresa alquila cisternas a transportistas con los que mantiene contratos que establecen tarifas diferenciadas según el tamaño del vehículo y el costo por kilómetro recorrido. 
    
    Al inicio de cada jornada se reciben los pedidos de los clientes, se consolida la demanda total y se debe decidir qué cisternas alquilar, cuánto producto cargará cada una y en qué orden se realizarán las entregas.

    Las cisternas utilizadas son vehículos con dos compartimentos, diseñados para transportar simultáneamente gasohol y diésel sin riesgo de mezcla entre productos. 
    
    La flota está compuesta por dos tipos de unidades, y cada tipo cuenta con un volumen específico asignado a cada producto. 

    Cada unidad realiza una única ruta por jornada, la cual comienza y termina en la almacen de despacho. 
    
    Una vez que un vehículo completa su recorrido y retorna al almacen, no puede volver a salir durante el mismo día.

    Un aspecto operativo crítico son los horarios de atención de los clientes. 
    Las estaciones no pueden recibir combustible en cualquier momento del día, estas ventanas de tiempo son propias de cada cliente y responden a regulaciones de seguridad o políticas internas de cada negocio. 
    Si una cisterna arriba antes del inicio de la ventana a un cliente, puede esperar hasta el horario permitido para iniciar la descarga, sin embargo, si llega después del límite establecido, no puede iniciar ese proceso.

    Cada cliente demanda cantidades específicas de gasohol y diésel, que deben entregarse en una sola visita por producto. 
    
    Un cliente puede ser atendido una única vez si una cisterna entrega ambos combustibles en la misma parada, o con dos cisternas distintas si cada producto se despacha por separado. No está permitido que más de una cisterna atienda el mismo tipo de producto en un mismo cliente. 
    
    Cada entrega tiene un tiempo de descarga definido, y cuando ambos productos se entregan en la misma visita, el tiempo total corresponde a la suma de los tiempos de descarga de cada combustible. 
    
    Los compartimentos de las cisternas son exclusivos para un solo tipo de producto.

    El crecimiento del número de clientes y la variabilidad diaria de la demanda han hecho que la planificación manual resulte insuficiente. Cada error en la asignación de cisternas puede traducirse en retrasos, sobrecostos o incluso en la pérdida de clientes por falta de abastecimiento oportuno. Contar con un programa que asista al gerente de operaciones en la toma de decisiones diarias se ha vuelto crucial para evitar pérdidas y garantizar la continuidad del servicio. 

    INSTANCIAS:

        La flota disponible es fija en todas las instancias y está compuesta por dos tipos de cisternas, con 20 unidades de cada tipo. 
        Los parámetros asociados a cada tipo se detallan a continuación:

        Tipo | Cap. Gasohol | Cap. Diésel | Costo fijo | Costo por km
        -----|--------------|------------|-----------|-------------
        1    | 5800         | 5200       | 450       | 2
        2    | 4000         | 4000       | 370       | 2

        Se asume una velocidad promedio constante de 60 km/h para todos los desplazamientos, sin variaciones horarias. 
        Las capacidades de los compartimentos están expresadas en galones y todos los costos se encuentran en dólares estadounidenses (US$). 
        Además, se considera que el tiempo de servicio en cada cliente es fijo e igual a 5 minutos, independientemente de la cantidad entregada. Si ambos combustibles se entregan en la misma visita, el tiempo total será de 10 minutos.

    EJEMPLO DE FORMATO DE ARCHIVO DE INSTANCIA (.csv):

            id, x,    y,    Gasohol,  Diésel,   Inicio,  Fin
            0,  35.0, 35.0, 0,        0,        04:00,   09:00
            36, 2.0,  60.0, 810,      790,      04:40,   06:00
            68, 56.0, 39.0, 760,      780,      05:50,   07:00
            22, 45.0, 10.0, 810,      850,      05:10,   06:10
            3,  55.0, 45.0, 800,      810,      04:50,   07:10
            31, 31.0, 52.0, 710,      840,      04:10,   07:20
            42, 24.0, 12.0, 810,      800,      04:20,   05:40
            8,  10.0, 10.0, 770,      790,      05:20,   06:00
            24, 65.0, 35.0, 840,      800,      04:50,   07:10
            53, 37.0, 31.0, 810,      850,      05:20,   06:00
            63, 27.0, 69.0, 860,      810,      04:30,   07:00

        Las distancias entre el almacén y los clientes, así como entre clientes, se calculan utilizando la distancia euclidiana a partir de sus coordenadas (x, y) expresadas en kilómetros. 
        Además, se establece que las cisternas pueden iniciar sus rutas a partir de las 04:00 horas y deben retornar al almacén antes de las 09:00 horas del mismo día.

Funcion Objetivo: Minimizar el costo diario de operacion, considerando el alquiler de cisternas y la distancia recorrida por los vehiculos, garantizando la atención completa de todos los clientes dentro de sus ventanas de tiempo.
"""

import os, sys
os.chdir(os.path.dirname(os.path.abspath(__file__)))

import pandas as pd
import numpy as np
from IPython.display import display
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from typing import List, Dict, Tuple, Optional
from dataclasses import dataclass
import copy
import math
import random

pd.set_option('display.float_format', lambda x: '%.2f' % x)
np.set_printoptions(precision=2, suppress=True)

SEED = 42
np.random.seed(SEED)
random.seed(SEED)


# Convertir ventanas de tiempo a minutos desde 04:00
def time_to_minutes(time_str):
    h, m = map(int, time_str.split(':'))
    return (h - 4) * 60 + m


def polar_coordinate_angle(x, y, depot_x, depot_y):
    """
    Calculate the angle of a point (x, y) with respect to a depot (depot_x, depot_y)
    in polar coordinates.
    """
    delta_x = x - depot_x
    delta_y = y - depot_y
    angle = np.arctan2(delta_y, delta_x)
    return angle if angle >= 0 else angle + 2 * np.pi

def clockwise_angle(angle_ccw):
    """
    Convierte un ángulo CCW a su equivalente horario en [0, 2π).
    """
    return (-angle_ccw) % (2 * np.pi)

def radius(x, y, depot_x, depot_y):
    """
    Calculate the radius of a point (x, y) with respect to a depot (depot_x, depot_y)
    in polar coordinates.
    """
    return np.sqrt((x - depot_x) ** 2 + (y - depot_y) ** 2)


df = (
    pd.read_csv('./instancias/100_clientes.csv')
    .reset_index(drop=False)
)

df['E'] = df['ventana_inicio'].apply(time_to_minutes)
df['L'] = df['ventana_fin'].apply(time_to_minutes)

df['AN'] = df.apply(
    lambda row: polar_coordinate_angle(row['x'], row['y'], df.loc[0, 'x'], df.loc[0, 'y']), axis=1
)

df['R'] = df.apply(
    lambda row: radius(row['x'], row['y'], df.loc[0, 'x'], df.loc[0, 'y']), axis=1
)


# Reorder the depots according to their polar-coordinate angle or the distance w.r.t. the deposit. 
# Depot is the first row (index 0)
df = (
    df.sort_values(by=['AN', 'R'])
    .rename(columns={'index': 'old_index'})
    .reset_index(drop=True).reset_index(drop=False)
)
display(df)

def split_client_demands(df: pd.DataFrame) -> pd.DataFrame:
    """
    Split demands into two rows: one for gasohol and another for diesel.
    """
    # Split demands into two rows: one for gasohol and another for diesel
    gasohol_df = df.copy()
    gasohol_df["tipo_combustible"] = "gasohol"
    gasohol_df["demanda_diesel"] = 0

    diesel_df = df.copy()
    diesel_df["tipo_combustible"] = "diesel"
    diesel_df["demanda_gasohol"] = 0

    df_split_demands = (
        pd.concat([gasohol_df, diesel_df], ignore_index=True)
        .sort_values(by=['AN', 'R'])
        .drop_duplicates(subset=['index', 'demanda_gasohol', 'demanda_diesel'], keep='first')
        .assign(
            old_index=lambda x: x.index
        ).drop(columns=['index'])
        .reset_index(drop=True).reset_index(drop=False)
    )
    
    return df_split_demands


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

# -----------------------------------------------------------------------------
# Parámetros del problema
# -----------------------------------------------------------------------------

tipos_cisternas = {
    1: {'cap_gasohol': 5800, 'cap_diesel': 5200, 'costo_fijo': 450, 'costo_km': 2},
    2: {'cap_gasohol': 4000, 'cap_diesel': 4000, 'costo_fijo': 370, 'costo_km': 2}
}

# -----------------------------------------------------------------------------
# Problem Instance
# -----------------------------------------------------------------------------

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
# Route Evaluation and Utilities
# -----------------------------------------------------------------------------

class RouteEvaluator:
    """Evalúa y valida rutas."""
    
    def __init__(self, instance: ProblemInstance):
        self.instance = instance
    
    def calcular_tiempo_servicio(self, productos: List[str]) -> float:
        """
        Calcula el tiempo de servicio en un cliente según los productos entregados.
        
        Args:
            productos: Lista de productos entregados ['G'] o ['D'] o ['G', 'D']
        
        Returns:
            Tiempo total de servicio en minutos
        """
        return len(productos) * self.instance.tiempo_descarga
    
    def verificar_factibilidad_ruta(self, ruta: List[int], cisterna: Cisterna,
                                   productos_por_cliente: Dict[int, List[str]]) -> Tuple[bool, float, Dict]:
        """
        Verifica si una ruta es factible respecto a capacidad y ventanas de tiempo.
        
        Args:
            ruta: Lista de IDs de clientes en orden de visita
            cisterna: Cisterna asignada a la ruta
            productos_por_cliente: Dict {cliente_id: ['G', 'D', ...]}
        
        Returns:
            (factible, tiempo_total, info_detallada)
        """
        # Verificar capacidad
        carga_gasohol = 0
        carga_diesel = 0
        
        for cliente_id in ruta:
            cliente = self.instance.cliente_por_id(cliente_id)
            if 'G' in productos_por_cliente[cliente_id]:
                carga_gasohol += cliente.demanda_gasohol
            if 'D' in productos_por_cliente[cliente_id]:
                carga_diesel += cliente.demanda_diesel
        
        if carga_gasohol > cisterna.cap_gasohol or carga_diesel > cisterna.cap_diesel:
            return False, 0, {'razon': 'capacidad_excedida'}
        
        # Verificar ventanas de tiempo
        tiempo_actual = self.instance.depot.ventana_inicio
        nodo_actual = 0  # Depot
        
        tiempos_llegada = {}
        
        for cliente_id in ruta:
            cliente = self.instance.cliente_por_id(cliente_id)
            
            # Tiempo de viaje al siguiente cliente
            tiempo_viaje = self.instance.tiempo_viaje(nodo_actual, cliente_id)
            tiempo_llegada = tiempo_actual + tiempo_viaje
            
            # Si llegamos antes de la ventana, esperamos
            if tiempo_llegada < cliente.ventana_inicio:
                tiempo_llegada = cliente.ventana_inicio
            
            # Si llegamos después de la ventana, no es factible
            if tiempo_llegada > cliente.ventana_fin:
                return False, 0, {'razon': 'ventana_tiempo_violada', 'cliente': cliente_id}
            
            tiempos_llegada[cliente_id] = tiempo_llegada
            
            # Tiempo de servicio
            tiempo_servicio = self.calcular_tiempo_servicio(productos_por_cliente[cliente_id])
            
            tiempo_actual = tiempo_llegada + tiempo_servicio
            nodo_actual = cliente_id
        
        # Retorno al depot
        tiempo_viaje_retorno = self.instance.tiempo_viaje(nodo_actual, 0)
        tiempo_retorno = tiempo_actual + tiempo_viaje_retorno
        
        if tiempo_retorno > self.instance.depot.ventana_fin:
            return False, 0, {'razon': 'retorno_tardio'}
        
        return True, tiempo_retorno, {
            'carga_gasohol': carga_gasohol,
            'carga_diesel': carga_diesel,
            'tiempos_llegada': tiempos_llegada,
            'tiempo_retorno': tiempo_retorno
        }
    
    def calcular_distancia_ruta(self, ruta: List[int]) -> float:
        """Calcula la distancia total de una ruta incluyendo ida y vuelta al depot."""
        if len(ruta) == 0:
            return 0
        
        distancia = self.instance.distancia(0, ruta[0])  # Depot al primer cliente
        
        for i in range(len(ruta) - 1):
            distancia += self.instance.distancia(ruta[i], ruta[i + 1])
        
        distancia += self.instance.distancia(ruta[-1], 0)  # Último cliente al depot
        
        return distancia
    
    def seleccionar_mejor_cisterna(self, ruta: List[int], 
                                   productos_por_cliente: Dict[int, List[str]],
                                   vehiculos_usados: Dict[int, int]) -> Optional[Cisterna]:
        """
        Selecciona la cisterna de menor costo que puede satisfacer la ruta.
        
        Returns:
            Cisterna seleccionada o None si ninguna es factible
        """
        cisternas_factibles = []
        
        for cisterna in self.instance.cisternas_disponibles:
            # Verificar si hay vehículos disponibles de este tipo
            if vehiculos_usados.get(cisterna.tipo, 0) >= self.instance.num_vehiculos_por_tipo:
                continue
            
            factible, _, _ = self.verificar_factibilidad_ruta(ruta, cisterna, productos_por_cliente)
            if factible:
                distancia = self.calcular_distancia_ruta(ruta)
                costo = cisterna.costo_fijo + cisterna.costo_km * distancia
                cisternas_factibles.append((cisterna, costo))
        
        if not cisternas_factibles:
            return None
        
        # Ordenar por costo y retornar la más económica
        cisternas_factibles.sort(key=lambda x: x[1])
        return cisternas_factibles[0][0]
    
    def crear_ruta_objeto(self, clientes: List[int], cisterna: Cisterna, 
                         productos_por_cliente: Dict[int, List[str]]) -> Ruta:
        """Crea un objeto Ruta completo con todos sus atributos calculados."""
        factible, tiempo_total, info = self.verificar_factibilidad_ruta(
            clientes, cisterna, productos_por_cliente
        )
        distancia = self.calcular_distancia_ruta(clientes)
        costo = cisterna.costo_fijo + cisterna.costo_km * distancia
        
        tiempos_llegada = [info['tiempos_llegada'][cid] for cid in clientes]
        
        return Ruta(
            cisterna=cisterna,
            clientes=clientes,
            carga_gasohol=info['carga_gasohol'],
            carga_diesel=info['carga_diesel'],
            distancia_total=distancia,
            tiempo_total=tiempo_total,
            costo_total=costo,
            factible=factible,
            productos_entregados=productos_por_cliente,
            tiempos_llegada=tiempos_llegada
        )


# -----------------------------------------------------------------------------
# Utility Functions
# -----------------------------------------------------------------------------

def contar_vehiculos(rutas: List[Ruta]) -> Dict[int, int]:
    """Cuenta el número de vehículos usados por tipo."""
    contador = {1: 0, 2: 0}
    for ruta in rutas:
        contador[ruta.cisterna.tipo] += 1
    return contador


def build_product_map(clientes_list: List[int], route_k: Ruta, route_k_plus_1: Ruta) -> Dict[int, List[str]]:
    """
    Build a mapping of customer IDs to their delivered products from two routes.
    """
    m: Dict[int, List[str]] = {}
    for cid in clientes_list:
        if cid in route_k.productos_entregados:
            m[cid] = route_k.productos_entregados[cid]
        elif cid in route_k_plus_1.productos_entregados:
            m[cid] = route_k_plus_1.productos_entregados[cid]
    return m


# -----------------------------------------------------------------------------
# Sweep Algorithm for Initial Solution Construction
# -----------------------------------------------------------------------------

class SweepAlgorithm:
    """
    Implements the Angular Sweep Algorithm for initial solution construction.
    Based on Gillett & Miller (1974) - 'A Heuristic Algorithm for the Vehicle-Dispatch Problem'
    """
    
    def __init__(self, instance: ProblemInstance):
        """
        Inicializa el algoritmo con una instancia del problema.
        
        Args:
            instance: Instancia del problema con todos los datos necesarios
        """
        self.instance = instance
        self.evaluator = RouteEvaluator(instance)
    
    def forward_sweep(self) -> List[Ruta]:
        """
        Algoritmo Forward Sweep.
        Particiona los clientes en rutas comenzando desde el cliente con menor ángulo.
        
        Implementación del algoritmo Angular Sweep de Gillett & Miller (1974) adaptado para VRP con múltiples compartimentos y ventanas de tiempo.
            'A Heuristic Algorithm for the Vehicle-Dispatch Problem'

                The problem is to determine the number of routes and the paths in each rout that will minimize the total distance traveled by all vehicles in supplying all demands, subject to the lead and distance constraints on each vehicle.

                The distance, constraint could be replaced by a time constraint without changing the problem.

                The sweep algorithm divides the locations into a number of routes and then operates on the individual routes unitl an optimum or near-optimum solution is obtained.

                The vehicle-dispatch problem is to minimize the total distance traveled supplying all demands while satisfying all constraints

                The sweep algorithm consists of two parts: a forward sweep and a backward sweep.

                    In the forward-sweep algorithm, the locations are partitioned into routes beginning with the location that has the smallest angle, namely, location 1. 
                    Recall that the locations were renumbered according to the size of their polar-coordinate angle and the depot is location 0. 
                    The first route consists of locations 1, 2, ..., I, where I is the last location that can be added without exceeding the vehicle capacity or distance constraint. 
                    The second route contains locations J+1, J+2, ..., L, where L is the last location that can be added to the second route without exceeding the vehicle capacity or distance constraint. 
                    The remaining routes are formed in the same manner. 
                    The total distance traveled then is just the sum of distances for each route.

                Then, a 2-opt type of improvement procedure is applied to the set of routes obtained from the forward sweep. 
                The procedure to modify consider replacing one location in route K with one or more locations in route K + 1
                This improvement process is continued both in the clockwise and counterclockwise directions until no further improvement is possible.

                The backward-sweep algorithm is similar to the forward-sweep algorithm except that the locations are considered in the reverse order.

        Returns:
            Lista de rutas generadas
        """
        rutas = []
        clientes_no_asignados = set(range(1, self.instance.n + 1))
        demandas_pendientes = {
            c.id: {'G': c.demanda_gasohol > 0, 'D': c.demanda_diesel > 0} 
            for c in self.instance.clientes
        }
        vehiculos_usados = {1: 0, 2: 0}
        
        while clientes_no_asignados:
            ruta_actual = []
            productos_ruta = {}
            
            for cliente_id in sorted(clientes_no_asignados, 
                                    key=lambda c: self.instance.cliente_por_id(c).angulo):
                
                cliente = self.instance.cliente_por_id(cliente_id)
                
                # Determinar qué productos entregar
                productos = []
                if demandas_pendientes[cliente_id]['G']:
                    productos.append('G')
                if demandas_pendientes[cliente_id]['D']:
                    productos.append('D')
                
                if not productos:
                    continue
                
                # Intentar agregar cliente a la ruta
                ruta_tentativa = ruta_actual + [cliente_id]
                productos_tentativa = productos_ruta.copy()
                productos_tentativa[cliente_id] = productos
                
                cisterna_factible = self.evaluator.seleccionar_mejor_cisterna(
                    ruta_tentativa, productos_tentativa, vehiculos_usados
                )
                
                if cisterna_factible:
                    ruta_actual = ruta_tentativa
                    productos_ruta = productos_tentativa
                else:
                    # Intentar solo con un producto si es posible
                    if len(productos) == 2:
                        for prod in ['G', 'D']:
                            productos_tentativa[cliente_id] = [prod]
                            cisterna = self.evaluator.seleccionar_mejor_cisterna(
                                ruta_tentativa, productos_tentativa, vehiculos_usados
                            )
                            if cisterna:
                                ruta_actual = ruta_tentativa
                                productos_ruta = productos_tentativa
                                break
                        else:
                            break  # No cabe ni con un solo producto
                    else:
                        break  # No se puede agregar más
            
            # Crear objeto Ruta
            if ruta_actual:
                cisterna = self.evaluator.seleccionar_mejor_cisterna(
                    ruta_actual, productos_ruta, vehiculos_usados
                )
                
                if cisterna is None:
                    raise ValueError(
                        f"No hay vehículos disponibles. Uso: Tipo 1: {vehiculos_usados[1]}/{self.instance.num_vehiculos_por_tipo}, "
                        f"Tipo 2: {vehiculos_usados[2]}/{self.instance.num_vehiculos_por_tipo}"
                    )
                
                ruta_obj = self.evaluator.crear_ruta_objeto(ruta_actual, cisterna, productos_ruta)
                rutas.append(ruta_obj)
                vehiculos_usados[cisterna.tipo] += 1
                
                # Actualizar estado
                for cliente_id, productos in productos_ruta.items():
                    if 'G' in productos:
                        demandas_pendientes[cliente_id]['G'] = False
                    if 'D' in productos:
                        demandas_pendientes[cliente_id]['D'] = False
                    
                    if not any(demandas_pendientes[cliente_id].values()):
                        clientes_no_asignados.discard(cliente_id)
            else:
                raise ValueError(f"No se pudo asignar clientes restantes: {clientes_no_asignados}")
        
        print(f"\n>>> Vehículos utilizados en Forward Sweep:")
        print(f"  * Tipo 1: {vehiculos_usados[1]}/{self.instance.num_vehiculos_por_tipo}")
        print(f"  * Tipo 2: {vehiculos_usados[2]}/{self.instance.num_vehiculos_por_tipo}")
        
        return rutas
    
    def _calcular_score_eliminacion(self, cliente_id: int, avg_radius: float) -> float:
        """Score para seleccionar cliente a eliminar: R(I) + An(I) * AVR."""
        cliente = self.instance.cliente_por_id(cliente_id)
        return cliente.radio + cliente.angulo * avg_radius
    
    def _seleccionar_cliente_a_eliminar(self, ruta: Ruta, avg_radius: float) -> Optional[int]:
        """Selecciona el cliente de la ruta que minimiza el score de eliminación."""
        if not ruta.clientes:
            return None
        
        return min(ruta.clientes, 
                  key=lambda cid: self._calcular_score_eliminacion(cid, avg_radius))
    
    def _obtener_candidatos_ordenados(self, ruta_k: Ruta, ruta_k_plus_1: Ruta) -> List[int]:
        """Obtiene clientes de K+1 ordenados por cercanía al último cliente de K."""
        if not ruta_k.clientes or not ruta_k_plus_1.clientes:
            return []
        
        ultimo_cliente_k = ruta_k.clientes[-1]
        return sorted(ruta_k_plus_1.clientes,
                     key=lambda c: self.instance.distancia(ultimo_cliente_k, c))
    
    def _intentar_insercion_greedy(self, base_clientes: List[int], 
                                  clientes_a_insertar: List[int], 
                                  ruta_k: Ruta, ruta_k_plus_1: Ruta, 
                                  vehiculos_disponibles: Dict[int, int]) -> Optional[Tuple[List[int], Cisterna, Dict, float]]:
        """Inserta clientes en la ruta base usando estrategia greedy."""
        ruta_actual = base_clientes[:]
        cisterna_actual = None
        info_actual = None
        costo_actual = None
        
        for cliente_id in clientes_a_insertar:
            mejor_insercion = self._encontrar_mejor_posicion_insercion(
                ruta_actual, cliente_id, ruta_k, ruta_k_plus_1, vehiculos_disponibles
            )
            
            if mejor_insercion is None:
                return None
            
            pos, cisterna, info, costo = mejor_insercion
            ruta_actual = ruta_actual[:pos] + [cliente_id] + ruta_actual[pos:]
            cisterna_actual = cisterna
            info_actual = info
            costo_actual = costo
        
        return (ruta_actual, cisterna_actual, info_actual, costo_actual)
    
    def _encontrar_mejor_posicion_insercion(self, ruta_actual: List[int], 
                                           cliente_id: int, 
                                           ruta_k: Ruta, ruta_k_plus_1: Ruta, 
                                           vehiculos_disponibles: Dict[int, int]) -> Optional[Tuple[int, Cisterna, Dict, float]]:
        """Encuentra la mejor posición para insertar un cliente en una ruta."""
        mejor_pos, mejor_costo = None, float('inf')
        mejor_cisterna, mejor_info = None, None
        
        for pos in range(len(ruta_actual) + 1):
            tentativa = ruta_actual[:pos] + [cliente_id] + ruta_actual[pos:]
            productos = build_product_map(tentativa, ruta_k, ruta_k_plus_1)
            
            cisterna = self.evaluator.seleccionar_mejor_cisterna(
                tentativa, productos, vehiculos_disponibles
            )
            if cisterna is None:
                continue
            
            factible, _, info = self.evaluator.verificar_factibilidad_ruta(
                tentativa, cisterna, productos
            )
            if not factible:
                continue
            
            distancia = self.evaluator.calcular_distancia_ruta(tentativa)
            costo = cisterna.costo_fijo + cisterna.costo_km * distancia
            
            if costo < mejor_costo:
                mejor_pos = pos
                mejor_costo = costo
                mejor_cisterna = cisterna
                mejor_info = info
        
        return (mejor_pos, mejor_cisterna, mejor_info, mejor_costo) if mejor_pos is not None else None
    
    def _reconstruir_ruta_k_plus_1(self, base_clientes: List[int], 
                                   cliente_a_insertar: int, 
                                   ruta_k: Ruta, ruta_k_plus_1: Ruta, 
                                   vehiculos_disponibles: Dict[int, int]) -> Optional[Tuple[List[int], Cisterna, Dict, float]]:
        """Reconstruye la ruta K+1 insertando un cliente eliminado de K."""
        mejor_pos, mejor_costo = None, float('inf')
        mejor_cisterna, mejor_info, mejor_ruta = None, None, None
        
        for pos in range(len(base_clientes) + 1):
            tentativa = base_clientes[:pos] + [cliente_a_insertar] + base_clientes[pos:]
            productos = build_product_map(tentativa, ruta_k, ruta_k_plus_1)
            
            cisterna = self.evaluator.seleccionar_mejor_cisterna(
                tentativa, productos, vehiculos_disponibles
            )
            if cisterna is None:
                continue
            
            factible, _, info = self.evaluator.verificar_factibilidad_ruta(
                tentativa, cisterna, productos
            )
            if not factible:
                continue
            
            distancia = self.evaluator.calcular_distancia_ruta(tentativa)
            costo = cisterna.costo_fijo + cisterna.costo_km * distancia
            
            if costo < mejor_costo:
                mejor_pos = pos
                mejor_costo = costo
                mejor_cisterna = cisterna
                mejor_info = info
                mejor_ruta = tentativa
        
        return (mejor_ruta, mejor_cisterna, mejor_info, mejor_costo) if mejor_pos is not None else None
    
    def _buscar_mejor_intercambio(self, ruta_k: Ruta, ruta_k_plus_1: Ruta, 
                                  cliente_eliminar: int, candidatos: List[int], 
                                  vehiculos_disponibles: Dict[int, int]) -> Optional[Tuple[List[int], Cisterna, Dict, List[int], Cisterna, Dict, float]]:
        """Busca el mejor intercambio de clientes entre dos rutas consecutivas."""
        base_k = [c for c in ruta_k.clientes if c != cliente_eliminar]
        costo_actual = ruta_k.costo_total + ruta_k_plus_1.costo_total
        
        # Liberar temporalmente vehículos de ambas rutas
        veh_temp = vehiculos_disponibles.copy()
        veh_temp[ruta_k.cisterna.tipo] -= 1
        veh_temp[ruta_k_plus_1.cisterna.tipo] -= 1
        
        # Intentar con prefijos crecientes de candidatos
        for m in range(1, len(candidatos) + 1):
            prefijo = candidatos[:m]
            
            resultado_k = self._intentar_insercion_greedy(
                base_k, prefijo, ruta_k, ruta_k_plus_1, veh_temp
            )
            
            if resultado_k is None:
                continue
            
            nueva_k, cisterna_k, info_k, costo_k = resultado_k
            
            veh_temp2 = veh_temp.copy()
            veh_temp2[cisterna_k.tipo] += 1
            
            base_k1 = [c for c in ruta_k_plus_1.clientes if c not in prefijo]
            resultado_k1 = self._reconstruir_ruta_k_plus_1(
                base_k1, cliente_eliminar, ruta_k, ruta_k_plus_1, veh_temp2
            )
            
            if resultado_k1 is None:
                continue
            
            nueva_k1, cisterna_k1, info_k1, costo_k1 = resultado_k1
            nuevo_costo = costo_k + costo_k1
            
            if nuevo_costo < costo_actual:
                return (nueva_k, cisterna_k, info_k, nueva_k1, cisterna_k1, info_k1, nuevo_costo)
        
        return None
    
    def _crear_ruta_desde_swap(self, clientes: List[int], cisterna: Cisterna, 
                               info: Dict, ruta_original_k: Ruta, 
                               ruta_original_k_plus_1: Ruta) -> Ruta:
        """Crea un objeto Ruta a partir del resultado de un intercambio."""
        productos = build_product_map(clientes, ruta_original_k, ruta_original_k_plus_1)
        return self.evaluator.crear_ruta_objeto(clientes, cisterna, productos)

    def improving_sweep(self, rutas_candidatas: List[Ruta], 
                       clockwise: bool = False) -> Tuple[List[Ruta], bool]:
        """
        Implementa el algoritmo de mejora del Forward Angular Sweep.

        Considera reemplazar un cliente en la ruta K con uno o más clientes de la ruta K+1.
        Un reemplazo se realiza solo si reduce el costo y ambas rutas permanecen factibles.

        Args:
            rutas_iniciales: Lista de rutas iniciales
            clockwise: Si True, procesa las rutas en orden inverso (sentido horario)
        
        Returns:
            (rutas_mejoradas, hubo_mejora): Tupla con las rutas mejoradas y un flag indicando si hubo mejora
        """
        avg_radius = np.mean([c.radio for c in self.instance.clientes])
        
        rutas_mejoradas = copy.deepcopy(rutas_candidatas)
        if clockwise:
            rutas_mejoradas = list(reversed(rutas_mejoradas))
        
        vehiculos_usados = contar_vehiculos(rutas_mejoradas)
        iteracion = 0
        hubo_alguna_mejora = False
        seguir_mejorando = True
        
        while seguir_mejorando:
            seguir_mejorando = False
            iteracion += 1

            for k in range(len(rutas_mejoradas) - 1):
                mejora_local = True
                
                while mejora_local:
                    mejora_local = False
                    
                    ruta_k = rutas_mejoradas[k]
                    ruta_k_plus_1 = rutas_mejoradas[k + 1]
                    costo_actual = ruta_k.costo_total + ruta_k_plus_1.costo_total

                    cliente_eliminar = self._seleccionar_cliente_a_eliminar(ruta_k, avg_radius)
                    if cliente_eliminar is None:
                        break

                    candidatos = self._obtener_candidatos_ordenados(ruta_k, ruta_k_plus_1)
                    if not candidatos:
                        break

                    mejor_swap = self._buscar_mejor_intercambio(
                        ruta_k, ruta_k_plus_1, cliente_eliminar, candidatos, vehiculos_usados
                    )

                    if mejor_swap is None:
                        break

                    nueva_k, cisterna_k, info_k, nueva_k1, cisterna_k1, info_k1, nuevo_costo = mejor_swap

                    nueva_ruta_k = self._crear_ruta_desde_swap(nueva_k, cisterna_k, info_k, ruta_k, ruta_k_plus_1)
                    nueva_ruta_k_plus_1 = self._crear_ruta_desde_swap(nueva_k1, cisterna_k1, info_k1, ruta_k, ruta_k_plus_1)

                    # Actualizar contador de vehículos
                    vehiculos_usados[ruta_k.cisterna.tipo] -= 1
                    vehiculos_usados[ruta_k_plus_1.cisterna.tipo] -= 1
                    vehiculos_usados[cisterna_k.tipo] += 1
                    vehiculos_usados[cisterna_k1.tipo] += 1

                    rutas_mejoradas[k] = nueva_ruta_k
                    rutas_mejoradas[k + 1] = nueva_ruta_k_plus_1

                    mejora_local = True
                    seguir_mejorando = True
                    hubo_alguna_mejora = True
                    
                    direccion_str = "horario" if clockwise else "antihorario"
                    ahorro = costo_actual - nuevo_costo
                    print(f"  [Iteración {iteracion} - {direccion_str}] Mejora rutas {k}-{k+1}: "
                          f"${costo_actual:.2f} → ${nuevo_costo:.2f} (ahorro: ${ahorro:.2f})")

        direccion_str = "horario" if clockwise else "antihorario"
        print(f"\n>>> Proceso de mejora ({direccion_str}) completado en {iteracion} iteraciones.")
        print(f">>> Vehículos utilizados:")
        print(f"  * Tipo 1: {vehiculos_usados[1]}/{self.instance.num_vehiculos_por_tipo}")
        print(f"  * Tipo 2: {vehiculos_usados[2]}/{self.instance.num_vehiculos_por_tipo}")

        if clockwise:
            rutas_mejoradas = list(reversed(rutas_mejoradas))

        return rutas_mejoradas, hubo_alguna_mejora

    def iterative_improving_sweep(self, rutas_candidatas: List[Ruta]) -> List[Ruta]:
        """
        Ejecuta improving_sweep alternando entre sentido antihorario y horario hasta que ambas direcciones no produzcan mejoras.
        
        Según Gillett & Miller (1974): "The X and Y axes are then rotated counterclockwise 
        or in the first location (counterclockwise is to the left). The procedure is then 
        repeated. The process of rotating the X and Y axes is continued until all 
        possibilities have been exhausted."
        
        Args:
            rutas_iniciales: Lista de rutas iniciales del forward_sweep
        
        Returns:
            Lista de rutas mejoradas después de agotar ambas direcciones
        """

        rutas_actuales = rutas_candidatas
        mejora_global = True
        iteracion_global = 0

        print("\n" + "="*80)
        print("INICIANDO MEJORA ITERATIVA (CLOCKWISE Y COUNTERCLOCKWISE)")
        print("="*80)
        
        while mejora_global:
            iteracion_global += 1
            mejora_global = False
            
            print(f"\n>>> Iteración global {iteracion_global}")
            
            # Sentido antihorario
            print("\n  >>> Intentando mejora en sentido ANTIHORARIO...")
            rutas_ccw, mejora_ccw = self.improving_sweep(rutas_actuales, clockwise=False)
            
            if mejora_ccw:
                rutas_actuales = rutas_ccw
                mejora_global = True
                costo_actual = sum(r.costo_total for r in rutas_actuales)
                print(f"  >>> Mejora encontrada. Costo actual: ${costo_actual:,.2f}")
            else:
                print(f"  >>> Sin mejoras")
            
            # Sentido horario
            print("\n  >>> Intentando mejora en sentido HORARIO...")
            rutas_cw, mejora_cw = self.improving_sweep(rutas_actuales, clockwise=True)
            
            if mejora_cw:
                rutas_actuales = rutas_cw
                mejora_global = True
                costo_actual = sum(r.costo_total for r in rutas_actuales)
                print(f"  >>> Mejora encontrada. Costo actual: ${costo_actual:,.2f}")
            else:
                print(f"  >>> Sin mejoras")
            
            if not mejora_ccw and not mejora_cw:
                print(f"\n>>> No se encontraron más mejoras en ninguna dirección.")
                mejora_global = False
        
        print("\n" + "="*80)
        print(f"MEJORA ITERATIVA COMPLETADA EN {iteracion_global} ITERACIONES GLOBALES")
        print("="*80)
        
        return rutas_actuales


class SolverTabuSearchMCVRPTW:
    """
    Solver for Multi-Compartment Vehicle Routing Problem with Time Windows using Tabu Search.
    Based on Silvestrin (2017) - 'An Iterated Tabu Search for Multi-Compartment Vehicle Routing Problem'
    """
    
    def __init__(self, sweep_solver: SweepAlgorithm):
        """
        Inicializa el solver de Tabu Search.
        
        Args:
            sweep_solver: Instancia de SweepAlgorithm para reutilizar funciones auxiliares
        """
        self.sweep = sweep_solver
    
    def perturbation(self, rutas: List[Ruta]) -> List[Ruta]:
        """
        To perturb a solution a random client is chosen and removed from its route, together its the `pi` nearest neighbors clientes
        `pi` is randomly chosen in [0, sqrt(n)], where n is the number of clients in the solution.

        The removed clients are then reinserted in the solution using a greedy insertion heuristic.
        Each clients is inserted into the route which minimizes the increase in the total routing cost (having into account the schedule, vehicle, and capacity constraints). [Parallelize operations for speedup]

        We use the Generalized Insertion Procedure (GENI) to insert visits into routes or remove visits from routes. 
        Together with the insertion or removal of a vertex, GENI applies a subset of 3-opt and 4-opt moves to the route.
        """
        pi = random.uniform(0, math.sqrt(self.sweep.n))
        return rutas
    
    def tabu_search(self, rutas: List[Ruta], iteration: int, alpha: float = 1.0, beta: float = 1.0, gamma: float = 1.0) -> List[Ruta]:
        """
        The performance of the tabu search depends on the neighborhood structure, the handling of infeasible solutions, and the design of the short-term memory.

        The proposed implementation startas from some initial solution and repeatedly moves to the best non-tabu neighbor. 
        The current solution may exceed, the capacity, lenght, or time window constraints.

        
        For a set of routes $s = {R_1, ldots, R_tau}$ we define its time (or distance) excess as:

        $$D^+(s) = sum_{R in s} max{d(R) - D, 0}$$

        and its capacity excess as:

        $$C^+ = sum_{R in s} max{max_{i in [m]} Delta c_i, 0}$$

        for $Delta c = (Delta c_1, ldots, Delta c_m) = c(R) - C$.

        The objective value of solution $s$ is then:

        $$F(s) = d(s) + alpha C^+(s) + beta D^+(s)$$

        where $alpha$ and $beta$ are penalties for each unit of capacity and time excess respectively. Initially, $alpha = beta = 0$.
        We will set $gamma$ as the penalty for each unit of time window violation.

        The penalties are then updated to allow strategic oscillation between feasible and infeasible solutions.
        Every time the curren solution exceeds the capacity, lenght, or time window constraints, the corresponding penalty is increased by a factor of $(1 + delta)$, with $delta > 0$; otherwise, it is decreased by a factor of $(1 + delta)$.
        """

        return rutas


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
        print("SOLUCIÓN - ANGULAR SWEEP ALGORITHM")
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

    def visualizar_rutas(self, rutas: List[Ruta]):
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
        
        ax.set_xlabel('Coordenada X (km)', fontsize=11)
        ax.set_ylabel('Coordenada Y (km)', fontsize=11)
        ax.set_title('Visualización de Rutas - Angular Sweep Algorithm', 
                    fontsize=14, fontweight='bold')
        ax.grid(True, alpha=0.3)
        ax.set_aspect('equal', adjustable='box')
        ax.legend(handles=legend_elements, loc='upper left', 
                 bbox_to_anchor=(1.02, 1), fontsize=9, framealpha=0.9)
        
        plt.tight_layout()
        plt.show()


def tabu_search_mcvrptw(data: pd.DataFrame, tipos_cisternas: Dict, 
                       clockwise: bool = False, I: int = 1000, 
                       split_demands: bool = False,
                       num_vehiculos_por_tipo: int = 20,
                       velocidad: float = 60, 
                       tiempo_descarga: float = 5) -> List[Ruta]:
    """
    Iterated Tabu Search para MCVRPTW.
    
    Basado en Silvestrin (2017) - 'An Iterated Tabu Search for Multi-Compartment Vehicle Routing Problem'
    
    Pseudo-código:
        function ITS()
            s <- SweepConstruction()
            s <- TabuSearch(s)
            for i = 1, ..., I iterations do
                s' <- Perturb(s)
                s <- TabuSearch(s')
                with probability (i / I)^2: s <- s'
            end for
            return the best solution s' found during search
        end function
    """
    df_input = data.copy()
    if clockwise:
        df_input['AN'] = df_input['AN'].apply(clockwise_angle)
    if split_demands:
        df_input = split_client_demands(df_input)

    instance = ProblemInstance(
        df_input, tipos_cisternas, num_vehiculos_por_tipo, 
        velocidad, tiempo_descarga
    )
    visualizer = SolutionVisualizer(instance)
    
    sweep_solver = SweepAlgorithm(instance)
    
    # Construcción inicial con Angular Sweep
    rutas = sweep_solver.forward_sweep()
    visualizer.imprimir_solucion(rutas, 1)
    visualizer.visualizar_rutas(rutas)

    # Mejora con "2-opt" bidireccional
    rutas = sweep_solver.iterative_improving_sweep(rutas)
    visualizer.imprimir_solucion(rutas, 1)
    visualizer.visualizar_rutas(rutas)

    # Iterated Tabu Search
    tabu_solver = SolverTabuSearchMCVRPTW(instance)
    best_solution = rutas
    best_cost = sum(r.costo_total for r in best_solution)

    for iteration in range(1, I + 1):
        rutas_perturbadas = tabu_solver.perturbation(rutas)
        rutas = tabu_solver.tabu_search(rutas_perturbadas, iteration)

        prob = (iteration / I) ** 2
        if random.random() < prob:
            rutas = rutas_perturbadas
        
        current_cost = sum(r.costo_total for r in rutas)
        if current_cost < best_cost:
            best_solution = rutas
            best_cost = current_cost
            print(f"  [Iteración {iteration}] Nueva mejor solución: ${best_cost:,.2f}")
    
    return best_solution


rutas_solucion = tabu_search_mcvrptw(df, tipos_cisternas, False)

