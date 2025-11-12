
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
    - D: Conjunto del depósito, representado por el nodo 0, desde donde inician y terminan las rutas de las cisternas.
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

5. Restricciones
    a. "Cada unidad realiza una única ruta por jornada, la cual comienza y termina en la almacen de despacho."
        - Las rutas de las cisternas deben comenzar y terminar en el depósito.
            Σ x_0jkv ≤ 1                                    ∀ k ∈ K, v ∈ V
            j∈C
            
            Σ x_i0kv ≤ 1                                    ∀ k ∈ K, v ∈ V
            i∈C
            
        - Cada cisterna puede realizar una única ruta por día y no puede volver a salir una vez que retorna al depósito.
            Σ x_0jkv = Σ x_i0kv                             ∀ k ∈ K, v ∈ V
            j∈C       i∈C
            
        - Conservación de flujo: si una cisterna llega a un cliente, debe salir de ese cliente.
            Σ x_ijkv = Σ x_jikv                             ∀ j ∈ C, k ∈ K, v ∈ V
            i∈C∪{0}    i∈C∪{0}
    
    b. Cada cliente debe ser atendido exactamente una vez por cada producto que demanda. No está permitido que más de una cisterna atienda el mismo tipo de producto en un mismo cliente. 
        Σ y_ikvp = 1                                     ∀ i ∈ C, p ∈ P
        k∈K, v∈V
        
        y_ikvp ≤ Σ x_jikv                               ∀ i ∈ C, k ∈ K, v ∈ V, p ∈ P
                    j∈C∪{0}
    
    c. La cantidad de producto entregada por cada cisterna no debe exceder la capacidad de sus compartimentos.
        Σ S_ip * y_ikvp ≤ Q_kp                         ∀ k ∈ K, v ∈ V, p ∈ P
        i∈C
    
    d. Las ventanas de tiempo de los clientes deben ser respetadas. 
        E_i ≤ t_ikv ≤ L_i                              ∀ i ∈ C, k ∈ K, v ∈ V

    e. El tiempo de viaje entre clientes debe ser considerado para calcular los tiempos de llegada.
        t_jkv ≥ t_ikv + (d_ij / V_max) * 60 + Σ(T_p * y_ikvp) - M * (1 - x_ijkv)   ∀ i, j ∈ C, i ≠ j, k ∈ K, v ∈ V
                                               p∈P
    
    f. Vínculo entre variables de ruta y tiempo**
        t_ikv ≤ M * Σ x_jikv                           ∀ i ∈ C, k ∈ K, v ∈ V
                    j∈C∪{0}
    
    g. Restricción de retorno al depósito antes de las 09:00**
        t_0kv ≤ 300  (300 min desde las 04:00 = 09:00)  ∀ k ∈ K, v ∈ V
    
    h. No visitar el mismo cliente dos veces con el mismo vehículo**
        Σ x_ijkv ≤ 1                                    ∀ i ∈ C, k ∈ K, v ∈ V
        j∈C∪{0}

    i. Tiempo de inicio en el depósito (04:00 = 0 minutos):
        t_0kv ≥ 0                                       ∀ k ∈ K, v ∈ V
        t_ikv ≥ (d_0i / V_max) * 60                    ∀ i ∈ C, k ∈ K, v ∈ V (si x_0ikv = 1)
    
    j. No autobucles:
        x_iikv = 0                                      ∀ i ∈ C∪{0}, k ∈ K, v ∈ V
    
    k. Vínculo entre carga q_kvp y entregas y_ikvp:
        q_kvp = Σ S_ip * y_ikvp                        ∀ k ∈ K, v ∈ V, p ∈ P
                i∈C
        (Esta restricción asegura consistencia entre la carga y las entregas)

    l. **No entregar productos si no hay demanda:**
        y_ikvp ≤ 1   si S_ip > 0                       ∀ i ∈ C, k ∈ K, v ∈ V, p ∈ P
        y_ikvp = 0   si S_ip = 0                       ∀ i ∈ C, k ∈ K, v ∈ V, p ∈ P

    m. Restricciones de no negatividad y binaridad:
        x_ijkv ∈ {0, 1}                                 ∀ i,j ∈ C∪{0}, k ∈ K, v ∈ V
        y_ikvp ∈ {0, 1}                                 ∀ i ∈ C, k ∈ K, v ∈ V, p ∈ P
        t_ikv ≥ 0                                       ∀ i ∈ C∪{0}, k ∈ K, v ∈ V
        q_kvp ≥ 0                                       ∀ k ∈ K, v ∈ V, p ∈ P

"""

import os, sys
os.chdir(os.path.dirname(os.path.abspath(__file__)))
import pulp as lp
import pandas as pd
import numpy as np

pd.set_option('display.float_format', lambda x: '%.2f' % x)
np.set_printoptions(precision=2, suppress=True)


df = pd.read_csv('./instancias/10_clientes_1.csv').reset_index(drop=False)

# Parámetros de la flota
tipos_cisternas = {
    1: {'cap_gasohol': 5800, 'cap_diesel': 5200, 'costo_fijo': 450, 'costo_km': 2},
    2: {'cap_gasohol': 4000, 'cap_diesel': 4000, 'costo_fijo': 370, 'costo_km': 2}
}
num_vehiculos = 20
velocidad = 60  # km/h
tiempo_descarga = 5  # minutos por producto
M = 10000  # Big M

# Convertir ventanas de tiempo a minutos desde 04:00
def time_to_minutes(time_str):
    h, m = map(int, time_str.split(':'))
    return (h - 4) * 60 + m

df['E'] = df['ventana_inicio'].apply(time_to_minutes)
df['L'] = df['ventana_fin'].apply(time_to_minutes)


# Conjuntos
C = df[df['index'] != 0]['index'].tolist()  # Clientes
depot = df[df['index'] == 0]['index'][0]

K = [1, 2]  # Tipos de cisternas
V = range(1, num_vehiculos + 1)  # Vehículos
P = ['G', 'D']  # Productos

# Distancias euclidianas
coords = df[['x', 'y']].values
dist = np.sqrt(((coords[:, None] - coords[None, :]) ** 2).sum(axis=2))

# Modelo
model = lp.LpProblem("VRP_Combustibles", lp.LpMinimize)

# Variables
x = lp.LpVariable.dicts("x", [(i,j,k,v) for i in [depot] + C for j in [depot] + C for k in K for v in V], cat='Binary')
y = lp.LpVariable.dicts("y", [(i,k,v,p) for i in C for k in K for v in V for p in P], cat='Binary')
t = lp.LpVariable.dicts("t", [(i,k,v) for i in [depot] + C for k in K for v in V], lowBound=0)

# Función objetivo
model += (
    lp.lpSum(tipos_cisternas[k]['costo_fijo'] * x[depot, j, k, v] for j in C for k in K for v in V) +
    lp.lpSum(tipos_cisternas[k]['costo_km'] * dist[i,j] * x[i,j,k,v] 
             for i in [depot] + C for j in [depot] + C for k in K for v in V)
)


# Restricciones
for k in K:
    for v in V:
        # Sale y entra al depósito
        model += lp.lpSum(x[depot,j,k,v] for j in C) <= 1
        model += lp.lpSum(x[i,depot,k,v] for i in C) <= 1
        model += lp.lpSum(x[depot,j,k,v] for j in C) == lp.lpSum(x[i,depot,k,v] for i in C)
        
        # Conservación de flujo
        for j in C:
            model += lp.lpSum(x[i,j,k,v] for i in [depot] + C if i!=j) == lp.lpSum(x[j,i,k,v] for i in [depot] + C if i!=j)
            model += lp.lpSum(x[j,i,k,v] for i in [depot] + C) <= 1  # No visitar dos veces
        
        # Capacidad
        model += lp.lpSum(df.loc[i,'demanda_gasohol'] * y[i,k,v,'G'] for i in C) <= tipos_cisternas[k]['cap_gasohol']
        model += lp.lpSum(df.loc[i,'demanda_diesel'] * y[i,k,v,'D'] for i in C) <= tipos_cisternas[k]['cap_diesel']

# Cada cliente atendido una vez por producto
for i in C:
    for p in P:
        col = 'demanda_gasohol' if p == 'G' else 'demanda_diesel'
        if df.loc[i, col] > 0:
            model += lp.lpSum(y[i,k,v,p] for k in K for v in V) == 1
        else:
            for k in K:
                for v in V:
                    model += y[i,k,v,p] == 0

# Vínculo entre y y x
for i in C:
    for k in K:
        for v in V:
            for p in P:
                model += y[i,k,v,p] <= lp.lpSum(x[j,i,k,v] for j in [depot] + C if j!=i)

# Ventanas de tiempo
for i in C:
    for k in K:
        for v in V:
            model += t[i,k,v] >= df.loc[i,'E'] - M * (1 - lp.lpSum(x[j,i,k,v] for j in [depot] + C if j!=i))
            model += t[i,k,v] <= df.loc[i,'L'] + M * (1 - lp.lpSum(x[j,i,k,v] for j in [depot] + C if j!=i))

# Tiempo de viaje
for i in C:
    for j in [depot] + C:
        if i != j:
            for k in K:
                for v in V:
                    tiempo_viaje = (dist[i,j] / velocidad) * 60
                    tiempo_srv = tiempo_descarga * lp.lpSum(y[i,k,v,p] for p in P)
                    model += t[j,k,v] >= t[i,k,v] + tiempo_viaje + tiempo_srv - M * (1 - x[i,j,k,v])

# Retorno antes de 09:00
for k in K:
    for v in V:
        model += t[depot,k,v] <= 300

# No autobucles
for i in [depot] + C:
    for k in K:
        for v in V:
            model += x[i,i,k,v] == 0

# Resolver
model.solve(lp.PULP_CBC_CMD(msg=1))

print(f"Estado: {lp.LpStatus[model.status]}")
print(f"Costo total: ${lp.value(model.objective):.2f}")


"""
https://www.sciencedirect.com/science/article/pii/S1110016822005956
"""