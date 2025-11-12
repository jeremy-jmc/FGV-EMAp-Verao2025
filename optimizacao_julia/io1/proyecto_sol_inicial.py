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
from typing import List, Dict
import random

from models import ProblemInstance, Ruta, SolutionVisualizer
from solvers import SweepAlgorithm, SolverTabuSearchMCVRPTW

pd.set_option('display.float_format', lambda x: '%.2f' % x)
np.set_printoptions(precision=2, suppress=True)

SEED = 42
np.random.seed(SEED)
random.seed(SEED)


# -----------------------------------------------------------------------------
# Utility Functions for Data Preprocessing
# -----------------------------------------------------------------------------

def time_to_minutes(time_str):
    """Convertir ventanas de tiempo a minutos desde 04:00"""
    h, m = map(int, time_str.split(':'))
    return (h - 4) * 60 + m


def polar_coordinate_angle(x, y, depot_x, depot_y):
    """Calculate the angle of a point (x, y) with respect to a depot in polar coordinates."""
    delta_x = x - depot_x
    delta_y = y - depot_y
    angle = np.arctan2(delta_y, delta_x)
    return angle if angle >= 0 else angle + 2 * np.pi


def clockwise_angle(angle_ccw):
    """Convierte un ángulo CCW a su equivalente horario en [0, 2π)."""
    return (-angle_ccw) % (2 * np.pi)


def radius(x, y, depot_x, depot_y):
    """
    Calculate the radius of a point (x, y) with respect to a depot (depot_x, depot_y)
    in polar coordinates.
    """
    return np.sqrt((x - depot_x) ** 2 + (y - depot_y) ** 2)


def split_client_demands(df: pd.DataFrame) -> pd.DataFrame:
    """Split demands into two rows: one for gasohol and another for diesel."""
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
        .assign(old_index=lambda x: x.index)
        .drop(columns=['index'])
        .reset_index(drop=True).reset_index(drop=False)
    )
    
    return df_split_demands


# -----------------------------------------------------------------------------
# Load and Preprocess Data
# -----------------------------------------------------------------------------

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

# Reorder according to polar-coordinate angle and distance
df = (
    df.sort_values(by=['AN', 'R'])
    .rename(columns={'index': 'old_index'})
    .reset_index(drop=True).reset_index(drop=False)
)
display(df)


# -----------------------------------------------------------------------------
# Problem Parameters
# -----------------------------------------------------------------------------

tipos_cisternas = {
    1: {'cap_gasohol': 5800, 'cap_diesel': 5200, 'costo_fijo': 450, 'costo_km': 2},
    2: {'cap_gasohol': 4000, 'cap_diesel': 4000, 'costo_fijo': 370, 'costo_km': 2}
}

# -----------------------------------------------------------------------------
# Main Algorithm
# -----------------------------------------------------------------------------

def tabu_search_mcvrptw(data: pd.DataFrame, tipos_cisternas: Dict, 
                       clockwise: bool = False, I: int = 1000, 
                       split_demands: bool = False,
                       num_vehiculos_por_tipo: int = 20,
                       velocidad: float = 60, 
                       tiempo_descarga: float = 5):
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
    visualizer.visualizar_rutas(rutas, "Forward Sweep")

    # Mejora con "2-opt" bidireccional
    rutas = sweep_solver.iterative_improving_sweep(rutas)
    visualizer.imprimir_solucion(rutas, 1)
    visualizer.visualizar_rutas(rutas, "Forward Sweep + 2-opt bidirectional")

    # Iterated Tabu Search
    tabu_solver = SolverTabuSearchMCVRPTW(sweep_solver)
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
    
    visualizer.imprimir_solucion(best_solution, 2)
    visualizer.visualizar_rutas(best_solution, "Perturbation Only")
    return best_solution, best_cost


# -----------------------------------------------------------------------------
# Execute
# -----------------------------------------------------------------------------

best_cost_solution = float('inf')
best_config = {}
for cc in [True, False]:
    for sd in [True, False]:
        print(f"\n--- Clockwise: {cc} | Split Demands: {sd} ---")
        rutas_solucion, costo = tabu_search_mcvrptw(df, tipos_cisternas, clockwise=False, split_demands=False)
        print(f"Solución final con costo total: ${costo:,.2f}")
        if costo < best_cost_solution:
            best_cost_solution = costo
            best_config = {'clockwise': cc, 'split_demands': sd}

print(f"\n=== Mejor configuración encontrada: Clockwise={best_config['clockwise']}, Split Demands={best_config['split_demands']} con costo total: ${best_cost_solution:,.2f} ===")
