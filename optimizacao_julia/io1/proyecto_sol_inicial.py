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

1. Conjuntos
    - C: Conjunto de clientes, indexado por i y j. C = {1, ..., n} donde n es el número total de clientes en la instancia.
    - D: Conjunto del depósito o almacén, representado por el nodo 0, desde donde inician y terminan las rutas de las cisternas.
    - K: Conjunto de tipos de cisternas, indexado por k. K = {1, 2} donde: 1 = Tipo 1, 2 = Tipo 2.
    - V: Conjunto de vehículos disponibles, indexado por v. V = {1, ..., 20} para cada tipo de cisterna.
    - P: Conjunto de productos (Gasohol, Diésel), indexado por p. P = {G, D} donde: G = Gasohol, D = Diésel.

2. Parametros
    - d_ij: Distancia en kilómetros entre el cliente i y el cliente j, calculada usando la distancia euclidiana.
    - Q_kp: Capacidad del compartimento para el producto p en una cisterna de tipo k (en galones).
    - F_k: Costo fijo de alquiler de una cisterna de tipo k (en US$).
    - C_k: Costo por kilómetro recorrido por una cisterna de tipo k (en US$ por km).
    - S_ip: Demanda del cliente i para cada producto p (en galones).
    - [E_i, L_i]: Ventana de tiempo del cliente i, donde E_i es el tiempo de inicio y L_i es el tiempo límite para la entrega (en minutos desde las 04:00 horas).
    - T_p: Tiempo de descarga para el producto p en un cliente (en minutos). T_G = 5 minutos, T_D = 5 minutos, si ambos productos se entregan en la misma visita, el tiempo total es T_G + T_D = 10 minutos.
    - V_max: Velocidad máxima constante de las cisternas (60 km/h).

3. Variables de Decision:
    - x_ijkv: Variable binaria que indica si la cisterna v de tipo k viaja directamente del cliente i al cliente j.
    - y_ikvp: Variable binaria que indica si la cisterna v de tipo k atiende al cliente i con el producto p.
    - t_ikv: Tiempo de llegada de la cisterna v de tipo k al cliente i (en minutos desde las 04:00 horas).
    - q_kvp: Cantidad de producto p cargada en la cisterna v de tipo k (en galones).

4. Funcion Objetivo: Minimizar el costo diario de operacion, considerando el alquiler de cisternas y la distancia recorrida por los vehiculos, garantizando la atención completa de todos los clientes dentro de sus ventanas de tiempo.
    Min Z =    Σ (F_k * Σ x_0jkv)    +    Σ (C_k * d_ij * x_ijkv)               for all k in K, v in V, i,j in C U {0}
            (COSTO FIJO DE ALQUILER)   (COSTO VARIABLE POR DISTANCIA)
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

pd.set_option('display.float_format', lambda x: '%.2f' % x)
np.set_printoptions(precision=2, suppress=True)


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
    pd.read_csv('./instancias/50_clientes.csv')
    .reset_index(drop=False)
)

df['E'] = df['ventana_inicio'].apply(time_to_minutes)
df['L'] = df['ventana_fin'].apply(time_to_minutes)


clockwise = False
df['AN'] = df.apply(
    lambda row: polar_coordinate_angle(row['x'], row['y'], df.loc[0, 'x'], df.loc[0, 'y']), axis=1
)
if clockwise:
    df['AN'] = df['AN'].apply(clockwise_angle)

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


# Distancias euclidianas
coords = df[['x', 'y']].values
D = np.sqrt(((coords[:, None] - coords[None, :]) ** 2).sum(axis=2))

display(df)


# -----------------------------------------------------------------------------
# Parámetros del problema
# -----------------------------------------------------------------------------

# Parámetros de la flota
N = df.shape[0]  # Número total de nodos (depósito + clientes)
tipos_cisternas = {
    1: {'cap_gasohol': 5800, 'cap_diesel': 5200, 'costo_fijo': 450, 'costo_km': 2},
    2: {'cap_gasohol': 4000, 'cap_diesel': 4000, 'costo_fijo': 370, 'costo_km': 2}
}
num_vehiculos_por_tipo = 20
velocidad = 60  # km/h
tiempo_descarga = 5  # minutos por producto
M = 10000  # Big M


# -----------------------------------------------------------------------------
# Initial Solution Construction Algorithms
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


# Calcular vehículos usados basándose en las rutas iniciales
def contar_vehiculos(rutas: List[Ruta]) -> Dict[int, int]:
    contador = {1: 0, 2: 0}
    for ruta in rutas:
        contador[ruta.cisterna.tipo] += 1
    return contador


class SweepAlgorithm:
    """
    Implementación del algoritmo Angular Sweep de Gillett & Miller (1974)
    adaptado para VRP con múltiples compartimentos y ventanas de tiempo.
    """
    
    def __init__(self, df: pd.DataFrame, tipos_cisternas: Dict, velocidad: float = 60, tiempo_descarga: float = 5, M: float = 10000):
        """
        Inicializa el algoritmo con los datos del problema.
        
        Args:
            df: DataFrame con información de clientes (ya ordenado por ángulo)
            tipos_cisternas: Diccionario con características de las cisternas
            velocidad: Velocidad en km/h
            tiempo_descarga: Tiempo de descarga por producto en minutos
            M: Big M para restricciones
        """
        self.df = df
        self.tipos_cisternas = tipos_cisternas
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
    
    def calcular_tiempo_servicio(self, cliente_id: int, productos: List[str]) -> float:
        """
        Calcula el tiempo de servicio en un cliente según los productos entregados.
        
        Args:
            cliente_id: ID del cliente
            productos: Lista de productos entregados ['G'] o ['D'] o ['G', 'D']
        
        Returns:
            Tiempo total de servicio en minutos
        """
        return len(productos) * self.tiempo_descarga
    
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
            cliente = self.clientes[cliente_id - 1]  # -1 porque clientes_id empieza en 1
            if 'G' in productos_por_cliente[cliente_id]:
                carga_gasohol += cliente.demanda_gasohol
            if 'D' in productos_por_cliente[cliente_id]:
                carga_diesel += cliente.demanda_diesel
        
        if carga_gasohol > cisterna.cap_gasohol or carga_diesel > cisterna.cap_diesel:
            return False, 0, {'razon': 'capacidad_excedida'}
        
        # Verificar ventanas de tiempo
        tiempo_actual = self.depot.ventana_inicio
        nodo_actual = 0  # Depot
        
        tiempos_llegada = {}
        
        for cliente_id in ruta:
            cliente = self.clientes[cliente_id - 1]
            
            # Tiempo de viaje al siguiente cliente
            tiempo_viaje = self.tiempo_viaje(nodo_actual, cliente_id)
            tiempo_llegada = tiempo_actual + tiempo_viaje
            
            # Si llegamos antes de la ventana, esperamos
            if tiempo_llegada < cliente.ventana_inicio:
                tiempo_llegada = cliente.ventana_inicio
            
            # Si llegamos después de la ventana, no es factible
            if tiempo_llegada > cliente.ventana_fin:
                return False, 0, {'razon': 'ventana_tiempo_violada', 'cliente': cliente_id}
            
            tiempos_llegada[cliente_id] = tiempo_llegada
            
            # Tiempo de servicio
            tiempo_servicio = self.calcular_tiempo_servicio(
                cliente_id, 
                productos_por_cliente[cliente_id]
            )
            
            tiempo_actual = tiempo_llegada + tiempo_servicio
            nodo_actual = cliente_id
        
        # Retorno al depot
        tiempo_viaje_retorno = self.tiempo_viaje(nodo_actual, 0)
        tiempo_retorno = tiempo_actual + tiempo_viaje_retorno
        
        if tiempo_retorno > self.depot.ventana_fin:
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
        
        distancia = self.distancia(0, ruta[0])  # Depot al primer cliente
        
        for i in range(len(ruta) - 1):
            distancia += self.distancia(ruta[i], ruta[i + 1])
        
        distancia += self.distancia(ruta[-1], 0)  # Último cliente al depot
        
        return distancia
    
    def seleccionar_mejor_cisterna(self, ruta: List[int], 
                                   productos_por_cliente: Dict[int, List[str]],
                                   vehiculos_usados: Dict[int, int]) -> Optional[Cisterna]:
        """
        Selecciona la cisterna de menor costo que puede satisfacer la ruta, constraints de distancia, capacidad y alquiler de vehiculos
        
        Returns:
            Cisterna seleccionada o None si ninguna es factible
        """
        cisternas_factibles = []
        
        for cisterna in self.cisternas_disponibles:
            # Verificar si hay vehículos disponibles de este tipo
            if vehiculos_usados.get(cisterna.tipo, 0) >= num_vehiculos_por_tipo:
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
    
    def forward_sweep(self) -> List[Ruta]:
        """
        Implementa el algoritmo Forward Sweep.
        
        Particiona los clientes en rutas comenzando desde el cliente con menor ángulo,
        agregando clientes mientras se respeten las restricciones de capacidad y tiempo.
        
        Returns:
            Lista de rutas generadas
        """
        rutas = []
        clientes_no_asignados = set(range(1, len(self.clientes) + 1))
        demandas_pendientes = {
            c.id: {'G': c.demanda_gasohol > 0, 'D': c.demanda_diesel > 0} 
            for c in self.clientes
        }
        vehiculos_usados = {1: 0, 2: 0}  # Contador de vehículos por tipo
        
        while clientes_no_asignados:
            # Iniciar nueva ruta
            ruta_actual = []
            productos_ruta = {}  # {cliente_id: ['G', 'D']}
            
            # Intentar construir ruta
            for cliente_id in sorted(clientes_no_asignados, key=lambda c: self.clientes[c-1].angulo):
                
                cliente = self.clientes[cliente_id - 1]
                
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
                
                # Intentar con ambos tipos de cisterna
                cisterna_factible = self.seleccionar_mejor_cisterna(
                    ruta_tentativa, productos_tentativa, vehiculos_usados
                )
                
                if cisterna_factible:
                    # El cliente cabe en la ruta
                    ruta_actual = ruta_tentativa
                    productos_ruta = productos_tentativa
                else:
                    # No cabe, intentar solo con un producto
                    if len(productos) == 2:
                        # Intentar solo con gasohol
                        productos_tentativa[cliente_id] = ['G']
                        cisterna_g = self.seleccionar_mejor_cisterna(
                            ruta_tentativa, productos_tentativa, vehiculos_usados
                        )
                        
                        if cisterna_g:
                            ruta_actual = ruta_tentativa
                            productos_ruta = productos_tentativa
                            continue
                        
                        # Intentar solo con diesel
                        productos_tentativa[cliente_id] = ['D']
                        cisterna_d = self.seleccionar_mejor_cisterna(
                            ruta_tentativa, productos_tentativa, vehiculos_usados
                        )
                        
                        if cisterna_d:
                            ruta_actual = ruta_tentativa
                            productos_ruta = productos_tentativa
                            continue
                    
                    # No se puede agregar más, cerrar ruta
                    break
            
            # Crear objeto Ruta
            if ruta_actual:
                cisterna = self.seleccionar_mejor_cisterna(ruta_actual, productos_ruta, vehiculos_usados)
                
                if cisterna is None:
                    raise ValueError(
                        f"No hay vehículos disponibles. Uso actual: Tipo 1: {vehiculos_usados[1]}/{num_vehiculos_por_tipo}, "
                        f"Tipo 2: {vehiculos_usados[2]}/{num_vehiculos_por_tipo}. "
                        f"Clientes no asignados: {clientes_no_asignados}"
                    )
                
                factible, tiempo_total, info = self.verificar_factibilidad_ruta(
                    ruta_actual, cisterna, productos_ruta
                )
                distancia = self.calcular_distancia_ruta(ruta_actual)
                costo = cisterna.costo_fijo + cisterna.costo_km * distancia
                
                # Extraer tiempos de llegada en orden de la ruta
                tiempos_llegada = [info['tiempos_llegada'][cliente_id] for cliente_id in ruta_actual]
                
                ruta_obj = Ruta(
                    cisterna=cisterna,
                    clientes=ruta_actual,
                    carga_gasohol=info['carga_gasohol'],
                    carga_diesel=info['carga_diesel'],
                    distancia_total=distancia,
                    tiempo_total=tiempo_total,
                    costo_total=costo,
                    factible=factible,
                    productos_entregados=productos_ruta,
                    tiempos_llegada=tiempos_llegada
                )
                
                rutas.append(ruta_obj)
                # Incrementar contador de vehículos usados
                vehiculos_usados[cisterna.tipo] += 1
                
                # Actualizar clientes no asignados y demandas pendientes
                for cliente_id, productos in productos_ruta.items():
                    if 'G' in productos:
                        demandas_pendientes[cliente_id]['G'] = False
                    if 'D' in productos:
                        demandas_pendientes[cliente_id]['D'] = False
                    
                    # Si el cliente no tiene más demandas, removerlo
                    if not demandas_pendientes[cliente_id]['G'] and \
                       not demandas_pendientes[cliente_id]['D']:
                        clientes_no_asignados.discard(cliente_id)
            else:
                # No se pudo construir ruta, problema infactible
                raise ValueError(
                    f"No se pudo asignar clientes restantes: {clientes_no_asignados}. "
                    f"Vehículos usados: Tipo 1: {vehiculos_usados[1]}/{num_vehiculos_por_tipo}, "
                    f"Tipo 2: {vehiculos_usados[2]}/{num_vehiculos_por_tipo}"
                )
        
        print(f"\n>>> Vehículos utilizados en Forward Sweep:")
        print(f"  * Tipo 1: {vehiculos_usados[1]}/{num_vehiculos_por_tipo}")
        print(f"  * Tipo 2: {vehiculos_usados[2]}/{num_vehiculos_por_tipo}")
        
        return rutas
    
    def improving_sweep(self, rutas_iniciales: List[Ruta]) -> List[Ruta]:
        """
        Implementa el algoritmo de mejora del Forward Angular Sweep.

        The procedure to modify consider replacing one location in route K with one or more locations in route K + 1 for K = 1, 2, ..., m - 1, where m is the number of routes formed.
        A replacement is made only if the cost of the two routes after the replacement is less than the cost before the replacement and both routes remain feasible after the replacement.

        The location to be deleted from route K is obtained by minimizing a function of the radius R(I) and the angle An(I) of each location in route K.
        This provides a location that is close to the depot and also close to the next route. A function that works very well is R(I) + An(I) * AVR (Average Radius among all locations).

        The first location, say location p, that is considered for inclusion in route K is the location in route K + 1 that is nearest to the last location that was added to route K. 
        The second location considered for inclusion in route K is the location in route K + 1 that is nearest to location p.
        If one or more locations are added to route K by this scheme, then the next location in route K + 1 is also checked to see if it can be included in route K.
        
        The process of adding one or more locations to route K and deleting another location continues until no further improvement is found. 
        The X and Y are then rotated counterclockwise, and the entire process is repeated until all possibilities have been exhausted.
        """
        
        # Calcular el radio promedio de todos los clientes
        avg_radius = np.mean([c.radio for c in self.clientes])
        
        rutas_mejoradas = copy.deepcopy(rutas_iniciales)
        vehiculos_usados = contar_vehiculos(rutas_mejoradas)
        
        mejora_global = True
        iteracion = 0
        
        while mejora_global:
            mejora_global = False
            iteracion += 1
            
            # Intentar mejorar entre cada par de rutas consecutivas
            for k in range(len(rutas_mejoradas) - 1):
                mejora_local = True

                while mejora_local:
                    mejora_local = False
                    
                    ruta_k = rutas_mejoradas[k]
                    ruta_k_plus_1 = rutas_mejoradas[k + 1]
                    
                    # Costo actual de ambas rutas
                    costo_actual = ruta_k.costo_total + ruta_k_plus_1.costo_total
                    
                    # 1. Seleccionar ubicación a eliminar de ruta K
                    # Minimizar R(I) + An(I) * AVR
                    mejor_cliente_eliminar, minimum_score = None, float('inf')
                    
                    for cliente_id in ruta_k.clientes:
                        cliente = self.clientes[cliente_id - 1]
                        score = cliente.radio + cliente.angulo * avg_radius
                        if score < minimum_score:
                            minimum_score = score
                            mejor_cliente_eliminar = cliente_id
                    
                    if mejor_cliente_eliminar is None:
                        break
                    
                    # 2. Seleccionar ubicaciones a agregar de ruta K+1
                    # Comenzar con la ubicación más cercana al último cliente agregado en ruta K
                    ultimo_cliente_k = ruta_k.clientes[-1]
                    
                    # Encontrar cliente más cercano en ruta K+1
                    clientes_a_agregar = []
                    distancias_candidatos = []
                    
                    for cliente_id in ruta_k_plus_1.clientes:
                        dist = self.distancia(ultimo_cliente_k, cliente_id)
                        distancias_candidatos.append((cliente_id, dist))
                    
                    if not distancias_candidatos:
                        break
                    
                    # Ordenar por distancia
                    distancias_candidatos.sort(key=lambda x: x[1])
                    
                    # Primer candidato (p): el más cercano al último de ruta K
                    primer_candidato_id = distancias_candidatos[0][0]
                    clientes_a_agregar.append(primer_candidato_id)
                    
                    # Segundo candidato: el más cercano a p en ruta K+1
                    if len(ruta_k_plus_1.clientes) > 1:
                        distancias_desde_p = []
                        for cliente_id in ruta_k_plus_1.clientes:
                            if cliente_id != primer_candidato_id:
                                dist = self.distancia(primer_candidato_id, cliente_id)
                                distancias_desde_p.append((cliente_id, dist))
                        
                        if distancias_desde_p:
                            distancias_desde_p.sort(key=lambda x: x[1])
                            segundo_candidato_id = distancias_desde_p[0][0]
                            clientes_a_agregar.append(segundo_candidato_id)
                    
                    # 3. Intentar intercambio con diferentes combinaciones
                    # Probar agregando 1, 2, ... clientes de K+1 y eliminando 1 de K
                    for num_agregar in range(1, len(clientes_a_agregar) + 1):
                        clientes_seleccionados = clientes_a_agregar[:num_agregar]
                        
                        # Crear nueva ruta K sin el cliente a eliminar
                        nueva_ruta_k_clientes = [c for c in ruta_k.clientes if c != mejor_cliente_eliminar]
                        
                        # TODO: lo que se debe minimizar a la hora de insertar un nuevo elemento es el costo, no la distancia de la ruta

                        # Agregar los clientes seleccionados de K+1 a K
                        # Insertarlos en la posición que minimice la distancia
                        for cliente_agregar in clientes_seleccionados:
                            mejor_posicion = 0
                            menor_incremento = float('inf')
                            
                            for pos in range(len(nueva_ruta_k_clientes) + 1):
                                ruta_temp = nueva_ruta_k_clientes[:pos] + [cliente_agregar] + nueva_ruta_k_clientes[pos:]
                                dist_temp = self.calcular_distancia_ruta(ruta_temp)
                                if dist_temp < menor_incremento:
                                    menor_incremento = dist_temp
                                    mejor_posicion = pos
                            
                            nueva_ruta_k_clientes.insert(mejor_posicion, cliente_agregar)
                        
                        # Crear nueva ruta K+1 sin los clientes agregados a K
                        nueva_ruta_k_plus_1_clientes = [c for c in ruta_k_plus_1.clientes if c not in clientes_seleccionados]
                        
                        # Agregar el cliente eliminado de K a K+1 en la mejor posición
                        if nueva_ruta_k_plus_1_clientes:
                            mejor_posicion = 0
                            menor_incremento = float('inf')
                            
                            for pos in range(len(nueva_ruta_k_plus_1_clientes) + 1):
                                ruta_temp = nueva_ruta_k_plus_1_clientes[:pos] + [mejor_cliente_eliminar] + nueva_ruta_k_plus_1_clientes[pos:]
                                dist_temp = self.calcular_distancia_ruta(ruta_temp)
                                if dist_temp < menor_incremento:
                                    menor_incremento = dist_temp
                                    mejor_posicion = pos
                            
                            nueva_ruta_k_plus_1_clientes.insert(mejor_posicion, mejor_cliente_eliminar)
                        else:
                            # Si K+1 queda vacía, el cliente va solo
                            nueva_ruta_k_plus_1_clientes = [mejor_cliente_eliminar]
                        
                        # Reconstruir productos entregados para ambas rutas
                        productos_k = {}
                        for cliente_id in nueva_ruta_k_clientes:
                            if cliente_id in ruta_k.productos_entregados:
                                productos_k[cliente_id] = ruta_k.productos_entregados[cliente_id]
                            elif cliente_id in ruta_k_plus_1.productos_entregados:
                                productos_k[cliente_id] = ruta_k_plus_1.productos_entregados[cliente_id]
                        
                        productos_k_plus_1 = {}
                        for cliente_id in nueva_ruta_k_plus_1_clientes:
                            if cliente_id in ruta_k.productos_entregados:
                                productos_k_plus_1[cliente_id] = ruta_k.productos_entregados[cliente_id]
                            elif cliente_id in ruta_k_plus_1.productos_entregados:
                                productos_k_plus_1[cliente_id] = ruta_k_plus_1.productos_entregados[cliente_id]
                        
                        # Crear contador temporal de vehículos
                        vehiculos_temp = vehiculos_usados.copy()
                        # Liberar los vehículos de las rutas K y K+1
                        vehiculos_temp[ruta_k.cisterna.tipo] -= 1
                        vehiculos_temp[ruta_k_plus_1.cisterna.tipo] -= 1
                        
                        # Verificar factibilidad y calcular costos
                        cisterna_k = self.seleccionar_mejor_cisterna(nueva_ruta_k_clientes, productos_k, vehiculos_temp)
                        if cisterna_k is None:
                            continue
                        
                        # Incrementar temporalmente para la segunda ruta
                        vehiculos_temp[cisterna_k.tipo] += 1
                        cisterna_k_plus_1 = self.seleccionar_mejor_cisterna(nueva_ruta_k_plus_1_clientes, productos_k_plus_1, vehiculos_temp)
                        
                        if cisterna_k_plus_1 is None:
                            continue
                        
                        # Verificar factibilidad completa
                        factible_k, tiempo_k, info_k = self.verificar_factibilidad_ruta(
                            nueva_ruta_k_clientes, cisterna_k, productos_k
                        )
                        factible_k_plus_1, tiempo_k_plus_1, info_k_plus_1 = self.verificar_factibilidad_ruta(
                            nueva_ruta_k_plus_1_clientes, cisterna_k_plus_1, productos_k_plus_1
                        )
                        
                        if not factible_k or not factible_k_plus_1:
                            continue
                        
                        # Calcular nuevo costo
                        dist_k = self.calcular_distancia_ruta(nueva_ruta_k_clientes)
                        dist_k_plus_1 = self.calcular_distancia_ruta(nueva_ruta_k_plus_1_clientes)
                        
                        costo_k = cisterna_k.costo_fijo + cisterna_k.costo_km * dist_k
                        costo_k_plus_1 = cisterna_k_plus_1.costo_fijo + cisterna_k_plus_1.costo_km * dist_k_plus_1
                        
                        nuevo_costo = costo_k + costo_k_plus_1
                        
                        # Si hay mejora, actualizar
                        if nuevo_costo < costo_actual:
                            # Crear nuevas rutas
                            tiempos_llegada_k = [info_k['tiempos_llegada'][cid] for cid in nueva_ruta_k_clientes]
                            tiempos_llegada_k_plus_1 = [info_k_plus_1['tiempos_llegada'][cid] for cid in nueva_ruta_k_plus_1_clientes]
                            
                            nueva_ruta_k = Ruta(
                                cisterna=cisterna_k,
                                clientes=nueva_ruta_k_clientes,
                                carga_gasohol=info_k['carga_gasohol'],
                                carga_diesel=info_k['carga_diesel'],
                                distancia_total=dist_k,
                                tiempo_total=tiempo_k,
                                costo_total=costo_k,
                                factible=factible_k,
                                productos_entregados=productos_k,
                                tiempos_llegada=tiempos_llegada_k
                            )
                            
                            nueva_ruta_k_plus_1 = Ruta(
                                cisterna=cisterna_k_plus_1,
                                clientes=nueva_ruta_k_plus_1_clientes,
                                carga_gasohol=info_k_plus_1['carga_gasohol'],
                                carga_diesel=info_k_plus_1['carga_diesel'],
                                distancia_total=dist_k_plus_1,
                                tiempo_total=tiempo_k_plus_1,
                                costo_total=costo_k_plus_1,
                                factible=factible_k_plus_1,
                                productos_entregados=productos_k_plus_1,
                                tiempos_llegada=tiempos_llegada_k_plus_1
                            )
                            
                            # Actualizar contador de vehículos
                            vehiculos_usados[ruta_k.cisterna.tipo] -= 1
                            vehiculos_usados[ruta_k_plus_1.cisterna.tipo] -= 1
                            vehiculos_usados[cisterna_k.tipo] += 1
                            vehiculos_usados[cisterna_k_plus_1.tipo] += 1
                            
                            # Actualizar rutas
                            rutas_mejoradas[k] = nueva_ruta_k
                            rutas_mejoradas[k + 1] = nueva_ruta_k_plus_1
                            
                            mejora_local = True
                            mejora_global = True
                            
                            print(f"  [Iteración {iteracion}] Mejora en rutas {k+1} y {k+2}: ${costo_actual:.2f} → ${nuevo_costo:.2f} (ahorro: ${costo_actual - nuevo_costo:.2f})")
                            break  # Probar siguiente par de rutas
                    
                    if mejora_local:
                        break  # Reintentar con las rutas actualizadas
        
        print(f"\n>>> Proceso de mejora completado en {iteracion} iteraciones.")
        print(f">>> Vehículos utilizados en Improving Sweep:")
        print(f"  * Tipo 1: {vehiculos_usados[1]}/{num_vehiculos_por_tipo}")
        print(f"  * Tipo 2: {vehiculos_usados[2]}/{num_vehiculos_por_tipo}")
        
        return rutas_mejoradas

    def imprimir_solucion(self, rutas: List[Ruta], verbosity: int = 1):
        """Imprime la solución de forma legible."""
        print("\n" + "=" * 80)
        print("SOLUCIÓN - ANGULAR SWEEP ALGORITHM")
        print("=" * 80)
        
        costo_total = sum(r.costo_total for r in rutas)
        distancia_total = sum(r.distancia_total for r in rutas)
        
        # Contar vehículos por tipo
        vehiculos_tipo_1 = sum(1 for r in rutas if r.cisterna.tipo == 1)
        vehiculos_tipo_2 = sum(1 for r in rutas if r.cisterna.tipo == 2)
        
        if verbosity >= 1:
            print(f"\n>>> RESUMEN GENERAL:")
            print(f"  * Número de rutas: {len(rutas)}")
            print(f"  * Costo total: ${costo_total:,.2f}")
            print(f"  * Distancia total: {distancia_total:.2f} km")
            print(f"  * Cisternas Tipo 1: {vehiculos_tipo_1}/{num_vehiculos_por_tipo}", end="")
            if vehiculos_tipo_1 > num_vehiculos_por_tipo:
                print(" !!! EXCEDE LÍMITE", end="")
            print()
            print(f"  * Cisternas Tipo 2: {vehiculos_tipo_2}/{num_vehiculos_por_tipo}", end="")
            if vehiculos_tipo_2 > num_vehiculos_por_tipo:
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
                    cliente = self.clientes[cliente_id - 1]
                    ventana_str = f"[{4 + cliente.ventana_inicio//60:02.0f}:{cliente.ventana_inicio%60:02.0f} - {4 + cliente.ventana_fin//60:02.0f}:{cliente.ventana_fin%60:02.0f}]"
                    print(f"      - Cliente {cliente_id}: {tiempo_llegada:.1f} min ({hora:02.0f}:{minuto:02.0f}) | Ventana: {ventana_str}")
                print(f"    Entregas por cliente:")
                for cliente_id, productos in ruta.productos_entregados.items():
                    prods_str = ", ".join(productos)
                    print(f"      - Cliente {cliente_id}: {prods_str}")
            
        print("\n" + "="*80)

    def visualizar_rutas(self, rutas: List[Ruta]):
        """
        Visualiza las rutas generadas en un mapa.
        
        Args:
            rutas: Lista de rutas a visualizar
        """
        # Definir colores para las rutas (ciclo de colores si hay más de 10 rutas)
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
        ax.scatter(self.depot.x, self.depot.y, s=300, c='red', marker='s', 
                  label='Depósito', zorder=5, edgecolors='black', linewidth=2)
        ax.text(self.depot.x + 0.75, self.depot.y + 0.75, 'DEPOT', 
               fontsize=10, fontweight='bold')
        
        # Dibujar clientes
        clientes_x = [c.x for c in self.clientes]
        clientes_y = [c.y for c in self.clientes]
        ax.scatter(clientes_x, clientes_y, s=150, c='lightblue', 
                  label='Clientes', zorder=4, edgecolors='black', linewidth=1)
        
        # Etiquetar clientes
        for cliente in self.clientes:
            ax.text(cliente.x + 0.75, cliente.y + 0.75, str(cliente.id), 
                   fontsize=9)
        
        # Dibujar rutas
        legend_elements = []
        
        for idx, ruta in enumerate(rutas):
            color = colores_base[idx % len(colores_base)]
            
            # Construir la secuencia completa: Depot -> clientes -> Depot
            secuencia = [0] + ruta.clientes + [0]
            
            # Obtener coordenadas
            x_coords = []
            y_coords = []
            for nodo in secuencia:
                if nodo == 0:
                    x_coords.append(self.depot.x)
                    y_coords.append(self.depot.y)
                else:
                    cliente = self.clientes[nodo - 1]
                    x_coords.append(cliente.x)
                    y_coords.append(cliente.y)
            
            # Dibujar la ruta
            ax.plot(x_coords, y_coords, color=color, linewidth=2, 
                   alpha=0.7, zorder=3)
            
            # Agregar flechas para indicar dirección
            for i in range(len(x_coords) - 1):
                dx = x_coords[i+1] - x_coords[i]
                dy = y_coords[i+1] - y_coords[i]
                # Dibujar flecha en el punto medio de cada segmento
                mid_x = x_coords[i] + dx * 0.5
                mid_y = y_coords[i] + dy * 0.5
                ax.annotate('', xy=(mid_x + dx*0.1, mid_y + dy*0.1), 
                          xytext=(mid_x - dx*0.1, mid_y - dy*0.1),
                          arrowprops=dict(arrowstyle='->', color=color, 
                                        lw=1.5, alpha=0.8))
            
            # Crear etiqueta para la leyenda
            tipo_str = f"Tipo {ruta.cisterna.tipo}"
            costo_str = f"${ruta.costo_total:.0f}"
            dist_str = f"{ruta.distancia_total:.1f}km"
            legend_elements.append(
                mpatches.Patch(color=color, 
                              label=f"Ruta {idx+1}: {tipo_str} | {dist_str} | {costo_str}")
            )
        
        # Configurar el gráfico
        ax.set_xlabel('Coordenada X (km)', fontsize=11)
        ax.set_ylabel('Coordenada Y (km)', fontsize=11)
        ax.set_title('Visualización de Rutas - Angular Sweep Algorithm', 
                    fontsize=14, fontweight='bold')
        ax.grid(True, alpha=0.3)
        ax.set_aspect('equal', adjustable='box')
        
        # Agregar leyenda
        ax.legend(handles=legend_elements, loc='upper left', 
                 bbox_to_anchor=(1.02, 1), fontsize=9, framealpha=0.9)
        
        plt.tight_layout()
        plt.show()



def angular_sweep_algorithm(
        data: pd.DataFrame, 
        tipos_cisternas: Dict,  
        velocidad: float = 60, 
        tiempo_descarga: float = 5
):
    """
    Implementing the algorithm/ideas of Gillet & Miller (1971) - 'A Heuristic Algorithm for the Vehicle-Dispatch Problem'

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

    """

    solver = SweepAlgorithm(data, tipos_cisternas, velocidad, tiempo_descarga)
    
    rutas = solver.forward_sweep()
    solver.imprimir_solucion(rutas, 1)
    solver.visualizar_rutas(rutas)

    rutas = solver.improving_sweep(rutas)
    solver.imprimir_solucion(rutas, 2)
    solver.visualizar_rutas(rutas)
    
    return rutas


rutas_solucion = angular_sweep_algorithm(df, tipos_cisternas)

